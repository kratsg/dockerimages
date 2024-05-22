!
!  SPDX-License-Identifier: GPL-3.0-or-later
!  Copyright (C) 2019-2022, respective authors of MCFM.
!

module nplotter_W
      use types
      use MCFMPlotting
      use ResummationTransition, only: transition
      use qtResummation_params, only: transitionSwitch
      implicit none

      public :: setup, book
      private

      integer, save, allocatable :: histos(:)

      contains

      subroutine setup()
          use types
          use parseinput
          implicit none

          include 'mpicommon.f'

          allocate(histos(11))

          if (rank == 0) then
              write (*,*) "RESUMMATION: Using transition with switch ", transitionSwitch
          endif

          histos(1) = plot_setup_custom([0.0010d0,0.0013d0,0.0016d0,0.0020d0, &
                0.0025d0,0.0032d0,0.0040d0,0.0050d0,0.0063d0,0.0079d0, &
                0.0100d0,0.0126d0,0.0158d0,0.0200d0,0.0251d0,0.0316d0, &
                0.0398d0,0.0501d0,0.0631d0,0.0794d0,0.1000d0,0.1259d0, &
                0.1585d0,0.1995d0,0.2512d0,0.3162d0,0.3981d0,0.5012d0, &
                0.6310d0,0.7943d0,1.0000d0,1.2589d0,1.5849d0,1.9953d0, &
                2.5119d0,3.1623d0,3.9811d0,5.0119d0,6.3096d0,7.9433d0, &
                10.0000d0,12.5893d0,15.8489d0,19.9526d0,25.1189d0, &
                31.6228d0,39.8107d0,50.1187d0,63.0957d0,79.4328d0,100.0000d0], &
                'pt34_fine')
          histos(2) = plot_setup_uniform(0.0_dp,60._dp,1.0_dp,'pt34')

          histos(3) = plot_setup_custom([0d0,7.5d0,12.5d0,17.5d0, &
              24d0,30d0,40d0,50d0,70d0,110d0,150d0,190d0,250d0,600d0],'pt34_cms')

          histos(4) = plot_setup_custom([0d0,0.004d0,0.008d0,0.012d0, &
              0.016d0,0.02d0,0.024d0,0.029d0,0.034d0,0.039d0,0.045d0, &
              0.051d0,0.057d0,0.064d0,0.072d0,0.081d0,0.091d0,0.102d0, &
              0.114d0,0.128d0,0.145d0,0.165d0,0.189d0,0.219d0,0.258d0, &
              0.312d0,0.391d0,0.524d0,0.695d0,0.918d0,1.153d0,1.496d0, &
              1.947d0,2.522d0,3.277d0,5d0,10d0],'phistar_atlas')

          ! Number of jets with pT>30 GeV
          histos(5) = plot_setup_uniform(-0.5_dp,6.5_dp,1.0_dp,'njets')

          ! Leading jet transverse momentum
          histos(6) = plot_setup_custom([500.0_dp,550.0_dp,600.0_dp,700.0_dp, &
                                         800.0_dp,900.0_dp,1000.0_dp,1250.0_dp,1500.0_dp,1750.0_dp],'ptj1')

          ! Scalar sum of all jet momenta
          histos(7) = plot_setup_custom([500.0_dp,600.0_dp,700.0_dp,800.0_dp,900.0_dp,1000.0_dp,&
                                        1100.0_dp,1200.0_dp,1400.0_dp,1600.0_dp,1800.0_dp,2000.0_dp, &
                                        2200.0_dp,2400.0_dp,2600.0_dp,2800.0_dp,3000.0_dp] ,'ht')

          ! Minimum separation between jet (pt>100) and lepton
          histos(8) = plot_setup_uniform(0.4_dp,4.18_dp,0.18_dp,'drjetlep')

          ! Lepton + neutrino transverse momentum
          histos(9) = plot_setup_custom([0.0_dp,100.0_dp,200.0_dp,300.0_dp,400.0_dp,500.0_dp, &
                                         600.0_dp,700.0_dp,800.0_dp,1000.0_dp,1500.0_dp,2000.0_dp],'wpt')

          ! Leading and sub-leading jet mass
          histos(10) = plot_setup_custom([0.0_dp,200.0_dp,400.0_dp,600.0_dp,800.0_dp,1000.0_dp, &
                                         1200.0_dp,1400.0_dp,1600.0_dp,1800.0_dp,2000.0_dp, &
                                         2250.0_dp,2500.0_dp,3000.0_dp,3500.0_dp,4000.0_dp,4500.0_dp], 'mjj')

          ! ratio of: Wpt / closest jet pT
          histos(11) = plot_setup_custom([0.00_dp,0.08_dp,0.16_dp,0.24_dp,0.32_dp, &
                                          0.40_dp,0.48_dp,0.56_dp,0.64_dp,0.72_dp, &
                                          0.80_dp,0.84_dp,0.88_dp,0.92_dp,0.96_dp, &
                                          1.00_dp,1.04_dp,1.08_dp,1.12_dp,1.16_dp, &
                                          1.20_dp,1.28_dp,1.36_dp,1.44_dp,1.52_dp, &
                                          1.60_dp,1.68_dp,1.76_dp,1.84_dp,1.92_dp,2.0_dp], 'ptratio')

      end subroutine

      subroutine book(p,wt,ids,vals,wts)
          use types
          use ResummationTransition
          use ieee_arithmetic
          implicit none
          include 'mxpart.f'
          include 'kpart.f'
          include 'runstring.f'
          include 'constants.f'
          include 'nf.f'
          include 'cplx.h'
          include 'leptcuts.f'
          include 'jetcuts.f'
          include 'taucut.f'! abovecut
          include 'jetlabel.f'! correct njets ("jets")

          logical:: is_lepton, is_neutrino

          real(dp), intent(in) :: p(mxpart,4)
          real(dp), intent(in) :: wt

          integer, allocatable, intent(out) :: ids(:)
          real(dp), allocatable, intent(out) :: vals(:)
          real(dp), allocatable, intent(out) :: wts(:)

          real(dp) :: pt, pttwo, puremass, twomass, delphi, etarap, ptpure, deltarlepjet, deltaphiwjet, aetarap

          real(dp) :: pt34, ptj1, drjetlep, wpt, ht, mjj, mindeltarlepjet, value_deltaphiwjet, mindeltaphiwjet, ptratio, trans
          real(dp) :: phistar, phiacop, costhetastar, delphi34

          integer :: countjet, countlept, countneutrino, jetindex(mxpart), leptindex(mxpart),neutrinoindex(mxpart)
          integer :: j,njets,ijet,ijetmindeltar
          real(dp) :: pjk(4), wcandidate(4)

          pt34 = pttwo(3,4,p)
          delphi34 = delphi(p(3,:),p(4,:))
          phiacop = 2._dp*atan(sqrt((1._dp+cos(delphi34))/(1._dp-cos(delphi34))))
          costhetastar = tanh((etarap(3,p)-etarap(4,p))/2._dp)
          phistar = tan(phiacop/2._dp)*sin(acos(costhetastar))

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

          ! filter out jets that are overlapping with the lepton
          ! angular separation of leptons and jets (prefer lepton)
          ! Starting at index 5 (which is the first "is_hadronic" number):
          !   - if jets is 1, p(5,:) is the jet.
          !   - if jets is 2, p(5,:) is the pt-leading jet, p(6,:) the subleading
          do j=1,jets
            if(deltarlepjet(leptindex(1),j+4,p) < 0.4) cycle
            countjet=countjet+1
            jetindex(countjet)=j
          enddo

          wcandidate(:) = p(neutrinoindex(1),:) + p(leptindex(1),:)
          wpt = ptpure(wcandidate)

          ptj1 = pt(jetindex(1),p)

          mjj = -100._dp
          if(countjet > 1) then
            pjk(:) = p(jetindex(1),:) + p(jetindex(2),:)
            mjj = puremass(pjk)
          endif

          ht = 0._dp
          ptratio = -100._dp
          mindeltarlepjet = 100._dp
          mindeltaphiwjet = 100._dp
          do ijet=1,countjet
            if(pt(jetindex(ijet), p) < 30.0) cycle
            ht = ht + pt(jetindex(ijet),p)
            if(pt(jetindex(ijet), p) < 100.0) cycle
            mindeltarlepjet = min(deltarlepjet(leptindex(1),jetindex(ijet),p), mindeltarlepjet)

            value_deltaphiwjet = deltaphiwjet(wcandidate,jetindex(ijet),p)
            if (value_deltaphiwjet < mindeltaphiwjet) then
                ijetmindeltar = ijet
                mindeltaphiwjet = value_deltaphiwjet
            endif
          enddo

          drjetlep = mindeltarlepjet
          ptratio = wpt / pt(jetindex(ijetmindeltar), p)

          write (*,*) "GIORDON: jets, count(lep,jet): ", jets, ",", countlept, ",", countjet, &
                      "; drjetlep: ", drjetlep, "; mjj: ", mjj, "; ht: ", ht, "ptratio: ", ptratio, &
                      "; leptindex: ", leptindex(1), "ptlep1: ", pt(leptindex(1), p), "; etalep1: ", etarap(leptindex(1), p), &
                      "; jet1index: ", jetindex(1), "; ptjet1: ", pt(jetindex(1), p), "; etajet1: ", etarap(jetindex(1), p), &
                      "; jet2index: ", jetindex(2), "; ptjet2: ", pt(jetindex(2), p), "; etajet2: ", etarap(jetindex(2), p), &
                      "; jet3index: ", jetindex(3), "; ptjet3: ", pt(jetindex(3), p), "; etajet3: ", etarap(jetindex(3), p)

          if (origKpart == kresummed) then
              if (abovecut .eqv. .false.) then
                  trans = transition((pt34/twomass(3,4,p))**2d0,0.001d0,transitionSwitch,0.001d0)
              else
                  ! fo piece without transition
                  trans = 1._dp
              endif
          else
              trans = 1._dp
          endif

          if (ieee_is_nan(pt34)) then
              pt34 = -1._dp
          endif

          if (ieee_is_nan(phistar)) then
              phistar = -1._dp
          endif

          if (ieee_is_nan(mjj)) then
              mjj = -1._dp
          endif

          ids = histos
          vals = [pt34,pt34,pt34,phistar,real(countjet,dp),ptj1,ht,drjetlep,wpt,mjj,ptratio]
          wts = [wt*trans,wt*trans,wt*trans,wt*trans,wt*trans,wt*trans,wt*trans,wt*trans,wt*trans,wt*trans,wt*trans]

      end subroutine

end module
