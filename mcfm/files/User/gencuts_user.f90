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

      logical :: gencuts_user
      real(dp), intent(in) :: pjet(mxpart,4)
      integer, intent(in) :: njets

      real(dp) :: etarappure, yrap, yraptwo, ayrap
      real(dp) :: ptpure, puremass
      real(dp) :: mll, mllga
      real(dp) :: eta3, eta4, eta5
      real(dp) :: pt34
      ! implement your own cuts here
      real(dp) :: etall,yll, ptll, pttwo

      real(dp) :: pt, deltarlepjet, mindeltarlepjet, aetarap
      logical :: is_inclusive, is_inclusive2j, is_collinear, is_lepton, is_hadronic, is_neutrino
      integer :: countjet, countlept, countneutrino, jetindex(mxpart), leptindex(mxpart), neutrinoindex(mxpart)
      integer :: j,ijet
      real(dp) :: deltarwjet, mindeltarwjet, value_deltarwjet, ptratio, ptpure, wpt
      integer :: ijetmindeltar
      real(dp) :: wcandidate(4)

      ! implement your own cuts here

      is_inclusive2j = index(runstring, "inclusive2j") /= 0
      is_inclusive = index(runstring, "inclusive") /= 0 .and. .not. is_inclusive2j
      is_collinear = index(runstring, "collinear") /= 0

      if (any((/is_inclusive, is_inclusive2j, is_collinear/))) then

        ! identify the leptons, jets, neutrinos
        countjet=0
        countlept=0
        countneutrino=0
        do j=3,mxpart
          if (is_lepton(j)) then
            countlept=countlept+1
            leptindex(countlept)=j
          elseif (is_hadronic(j)) then
            countjet=countjet+1
            jetindex(countjet)=j
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

        ! lepton momentum
        ! handled by existing config: ptleptmin
        if(pt(leptindex(1),pjet) < leptptmin) then
          gencuts_user=.true.
          return
        endif

        ! lepton pseudorapidity
        ! handled by existing config: etaleptmax
        if (aetarap(leptindex(1),pjet) > 2.4) then
          gencuts_user=.true.
          return
        endif

        ! number of jets
        if (njets == 0) then
          gencuts_user=.true.
          return
        endif

        ! leading jet transverse momentum
        if (pt(jetindex(1),pjet) < 500.0 .or. aetarap(jetindex(1),pjet) > 2.5) then
          gencuts_user=.true.
          return
        endif

        ! angular separation of leptons and jets
        do ijet=1,njets
          if(deltarlepjet(leptindex(1),jetindex(ijet),pjet) < 0.4) then
            gencuts_user=.true.
            return
          endif
        enddo

        wcandidate(:) = pjet(neutrinoindex(1),:) + pjet(leptindex(1),:)
        wpt = ptpure(wcandidate)

        ptratio = 0._dp
        mindeltarwjet = 100._dp
        do ijet=1,njets
          if(pt(jetindex(ijet), pjet) < 100.0) cycle
          value_deltarwjet = deltarwjet(wcandidate,jetindex(ijet),pjet)
          if (value_deltarwjet < mindeltarwjet) then
              ijetmindeltar = ijet
              mindeltarwjet = value_deltarwjet
          endif
        enddo
        ptratio = wpt / pt(jetindex(ijetmindeltar), pjet)

        ! inclusive-2j selection
        if(is_inclusive2j) then
          if(njets < 2) then
            gencuts_user=.true.
            return
          else if(pt(jetindex(2),pjet) < 30.0 .or. aetarap(jetindex(2),pjet) > 2.5)  then
            gencuts_user=.true.
            return
          endif
        endif

        ! collinear selection
        if(is_collinear) then
          mindeltarlepjet = 100._dp
          do ijet=1,njets
            if(pt(jetindex(ijet), pjet) < 100.0) cycle
            mindeltarlepjet = min(deltarlepjet(leptindex(1),jetindex(ijet),pjet), mindeltarlepjet)
          enddo

          if(mindeltarlepjet > 2.6) then
            gencuts_user=.true.
            return
          endif
        endif ! collinear

      endif ! runstring in ["inclusive", "inclusive2j", "collinear"]

      ! this performs cuts based on cuts%y34min and cuts%y34max from the input file
      yll = abs(yraptwo(3,4,pjet))
      if (yll < y34min .or. yll > y34max) then
          gencuts_user = .true.
          return
      endif

      gencuts_user = .false.
      return

      ! Example for a rapidity cut on the particle 3,4 system:

      !yll = abs(yraptwo(3,4,pjet))
      !if (yll < y34min .or. yll > y34max) then
          !gencuts_user = .true.
          !return
      !endif

      ! example cuts which are enabled based on the input file runstring

!     if (trim(runstring) == "Z_atlas_8tev") then
!         yll = yraptwo(3,4,pjet)
!         if (yll < -2.4d0 .or. yll > 2.4d0) then
!             gencuts_user = .true.
!             return
!         endif
!     elseif (trim(runstring) == "Z_cms_7tev") then
!         return
!     elseif (trim(runstring) == "Z_cms_13tev") then
!         yll = yraptwo(3,4,pjet)
!         if (yll < -2.4d0 .or. yll > 2.4d0) then
!             gencuts_user = .true.
!             return
!         endif
!     elseif (trim(runstring) == "Higgs") then
!         yll = yraptwo(3,4,pjet)
!         if (yll < -2.4d0 .or. yll > 2.4d0) then
!             gencuts_user = .true.
!             return
!         endif
!     elseif (trim(runstring) == "ZZ") then
!             mll = puremass(pjet(3,:) + pjet(4,:))
!             mllga = puremass(pjet(3,:) + pjet(4,:) + pjet(5,:) + pjet(6,:))

!             !if ((mllga > 185d0) .or. (mllga < 179d0)) then
!                 !gencuts_user = .true.
!                 !return
!             !endif

!     elseif (trim(runstring) == "Wm_cms_8tev" .or. trim(runstring) == "Wp_cms_8tev") then
!         !pt34 = ptpure(pjet(3,:)+pjet(4,:))
!         !if (pt34 < 30d0) then
!             !gencuts_user = .true.
!             !return
!         !endif
!     elseif (trim(runstring) == "1211.1913") then
!         eta3 = ayrap(3,pjet)
!         eta4 = ayrap(4,pjet)

!         if (eta3 > 1.37d0 .and. eta3 < 1.52d0) then
!             gencuts_user = .true.
!             return
!         endif
!         if (eta4 > 1.37d0 .and. eta4 < 1.52d0) then
!             gencuts_user = .true.
!             return
!         endif

!     elseif (trim(runstring) == "Zga_atlas_13TeV") then
!             mll = puremass(pjet(3,:) + pjet(4,:))
!             mllga = puremass(pjet(3,:) + pjet(4,:) + pjet(5,:))

!             if (mll < 40d0) then
!                 gencuts_user = .true.
!                 return
!             endif

!             if (mll + mllga < 182d0) then
!                 gencuts_user = .true.
!                 return
!             endif

!             mycuts: block
!                 real(dp) :: delphi, etarap
!
!                 real(dp) :: delphi345, phiacop, costhetastar, phistar
!
!                 delphi345 = delphi(pjet(3,:)+pjet(4,:), pjet(5,:))
!
!                 if (delphi345 > 0.10d0) then
!                     gencuts_user = .true.
!                     return
!                 endif
!
!             end block mycuts

!             eta3 = abs(etarappure(pjet(3,:)))
!             eta4 = abs(etarappure(pjet(4,:)))
!             eta5 = abs(etarappure(pjet(5,:)))

!             if (eta3 > 1.37d0 .and. eta3 < 1.52d0) then
!                 gencuts_user = .true.
!                 return
!             endif

!             if (eta4 > 1.37d0 .and. eta4 < 1.52d0) then
!                 gencuts_user = .true.
!                 return
!             endif

!             if (eta5 > 1.37d0 .and. eta5 < 1.52d0) then
!                 gencuts_user = .true.
!                 return
!             endif
!     else
!         continue
!         !error stop __FILE__//": gencuts_user unset cuts"
!     endif

    end function

end submodule
