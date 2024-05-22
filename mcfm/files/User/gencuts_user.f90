!
!  SPDX-License-Identifier: GPL-3.0-or-later
!  Copyright (C) 2019-2022, respective authors of MCFM.
!

submodule (m_gencuts) m_gencuts_user
    implicit none

    contains

    module function reweight_user(pjet)
        use types
        implicit none
        include 'mxpart.f'
        include 'kprocess.f'
        include 'energy.f'
        real(dp) :: reweight_user
        real(dp), intent(in) :: pjet(mxpart,4)

        real(dp) :: ptpure,dot

        reweight_user = 1._dp

! Reweighting to help capture the important high-pt tail for these processes
        if (kcase == kWgajew) then
          reweight_user=(ptpure(6,pjet)/100._dp)**2
        endif
        if (kcase == ktt_mix) then
          reweight_user=(2._dp*dot(pjet,1,2)/(sqrts*sqrts))**2
        endif

    end function

    module function gencuts_user(pjet, njets)
      use types
      implicit none
      include 'mxpart.f'
      include 'runstring.f'
      include 'constants.f'
      include 'nf.f'
      include 'cplx.h'
      include 'leptcuts.f'
      include 'jetcuts.f'
      include 'taucut.f'! for usescet
      include 'jetlabel.f'! correct njets ("jets")

      logical :: gencuts_user
      real(dp), intent(in) :: pjet(mxpart,4)
      integer, intent(in) :: njets

      real(dp) :: etarappure, yrap, yraptwo, ayrap
      real(dp) :: puremass
      real(dp) :: mll, mllga
      real(dp) :: eta3, eta4, eta5
      real(dp) :: pt34
      ! implement your own cuts here
      real(dp) :: etall,yll, ptll, pttwo

      real(dp) :: pt, deltarlepjet, mindeltarlepjet, etarap, aetarap
      logical :: is_inclusive, is_inclusi2j, is_collinear, is_back2back, is_lepton, is_neutrino
      integer :: countjet, countlept, countneutrino, jetindex(mxpart), leptindex(mxpart), neutrinoindex(mxpart)
      integer :: j,ijet

      ! implement your own cuts here

      is_inclusive = index(runstring, "inclusive") /= 0
      is_inclusi2j = index(runstring, "inclusi2j") /= 0
      is_collinear = index(runstring, "collinear") /= 0
      is_back2back = index(runstring, "backtoback") /= 0

      if (any((/is_inclusive, is_inclusi2j, is_collinear, is_back2back/))) then

        ! identify the leptons, jets, neutrinos
        countjet=0
        countlept=0
        countneutrino=0
        do j=3,mxpart
          if (is_lepton(j)) then
            countlept=countlept+1
            leptindex(countlept)=j
          elseif (is_neutrino(j)) then
            countneutrino=countneutrino+1
            neutrinoindex(countneutrino)=j
          endif
        enddo

        ! number of leptons
        if(countlept /= 1) then
          gencuts_user=.true.
          return
        endif

        ! filter out jets that are overlapping with the lepton
        ! angular separation of leptons and jets (prefer lepton)
        ! Starting at index 5 (which is the first "is_hadronic" number):
        !   - if jets is 1, p(5,:) is the jet.
        !   - if jets is 2, p(5,:) is the pt-leading jet, p(6,:) the subleading
        do j=1,jets
          if(deltarlepjet(leptindex(1),j+4,pjet) < 0.4) cycle
          countjet=countjet+1
          jetindex(countjet)=j
        enddo

        write (*,*) "GIORDON: jets, count(lep,jet): ", jets, ",", countlept, ",", countjet, &
                    "; leptindex: ", leptindex(1), "ptlep1: ", pt(leptindex(1), pjet), "; etalep1: ", etarap(leptindex(1), pjet), &
                    "; jet1index: ", jetindex(1), "; ptjet1: ", pt(jetindex(1), pjet), "; etajet1: ", etarap(jetindex(1), pjet), &
                    "; jet2index: ", jetindex(2), "; ptjet2: ", pt(jetindex(2), pjet), "; etajet2: ", etarap(jetindex(2), pjet), &
                    "; jet3index: ", jetindex(3), "; ptjet3: ", pt(jetindex(3), pjet), "; etajet3: ", etarap(jetindex(3), pjet)

        ! number of jets
        if (countjet == 0) then
          gencuts_user=.true.
          return
        endif

        ! leading jet transverse momentum
        if (pt(jetindex(1),pjet) < 500.0 .or. aetarap(jetindex(1),pjet) > 2.5) then
          gencuts_user=.true.
          return
        endif

        ! inclusive-2j selection
        if(is_inclusi2j .and. countjet < 2) then
          gencuts_user=.true.
          return
        endif

        ! collinear/backtoback selection
        if(is_collinear .or. is_back2back) then
          mindeltarlepjet = 100._dp
          do ijet=1,countjet
            if(pt(jetindex(ijet), pjet) < 100.0) cycle
            mindeltarlepjet = min(deltarlepjet(leptindex(1),jetindex(ijet),pjet), mindeltarlepjet)
          enddo

          if(is_collinear .and. mindeltarlepjet > 2.6) then
            gencuts_user=.true.
            return
          elseif(is_back2back .and. mindeltarlepjet <= 2.6) then
            gencuts_user=.true.
            return
          endif
        endif

      endif ! runstring in ["inclusive", "inclusi2j", "collinear", "backtoback"]

      gencuts_user = .false.
      return

    end function

end submodule
