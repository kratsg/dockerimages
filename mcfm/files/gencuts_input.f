!
!  SPDX-License-Identifier: GPL-3.0-or-later
!  Copyright (C) 2019-2022, respective authors of MCFM.
!

      function gencuts_input(pjet_in,njets)
      implicit none
      include 'types.f'
      include 'mpicommon.f'
      logical:: gencuts_input
c***********************************************************************
c   Author: J.M. Campbell, 5th December 2001                           *
c                                                                      *
c   This routine imposes a generic set of cuts that can be applied     *
c   to all processes in process.DAT, using the parton momenta in pjet  *
c   which have already passed through the jet clustering algorithm     *
c                                                                      *
c   Only a basic set of variables is tested:                           *
c     pt(lepton) > leptpt, eta(lepton) < leptrap, missing Et > misspt  *
c                                                                      *
c   The  second-leading pt one must instead satisfy:                   *
c     pt(lepton) > leptpt2, eta(lepton) < leptrap2                     *
c                                                                      *
c   If leptpt3 or leptrap3 is not zero, then any leptons beyond the    *
c    second-leading pt one must instead satisfy:                       *
c     pt(lepton) > leptpt3, eta(lepton) < leptrap3                     *
c                                                                      *
c   If these are zero, cuts use leptpt2 and leptrap2 instead           *
c                                                                      *
c   For processes where one would like to apply jet-like cuts, but     *
c   no clustering has been performed, additional cuts apply:           *
c     pt(jet) > jetpt, eta(jet) < jetrap, R(jet1,jet2) > Rcut          *
c                                                                      *
c   Finally, if further process-specific cuts are necessary,           *
c   an appropriate second routine may be called                        *
c                                                                      *
c   Return TRUE if this point FAILS the cuts                           *
c                                                                      *
c***********************************************************************

      include 'bbproc.f'
      include 'constants.f'
      include 'nf.f'
      include 'mxpart.f'
      include 'cplx.h'
      include 'jetcuts.f'
      include 'leptcuts.f'
      include 'kprocess.f'
      include 'plabel.f'
      include 'masses.f'
      include 'jetlabel.f'
      include 'notag.f'
      include 'taucut.f'
      include 'first.f'
      include 'interference.f'
      include 'nwz.f'
      logical:: passedlept,is_lepton,is_neutrino,
     & is_hadronic,is_electron,is_muon
      integer:: njets,j,k,jindex,kindex,ib1,ib2
      integer:: countjet,jetindex(mxpart),pntr,jmin
      real(dp):: pjet(mxpart,4),etvec(4),pZj(4),mZj,phimin,phicheck,delphi
      real(dp):: pt,pttwo,etarap,etmiss,evtmisset,R,Rcut,etaj,etak,
     & etalept,mll,jetpt,jetrap,mnul,etabuffer,ptvals(mxpart)
      real(dp):: pjet_in(mxpart,4),m34sq,m56sq,m36sq,m45sq,m3lsq,P1,P2
      logical:: hwwjetcuts
      real(dp):: ht,qeta,mlbnu,merecon,reconcorr
      real(dp):: dphi_ll,m_ll,mtrans,scut1,scut2
      real(dp):: phill,phillcut,etajet2cut,mllcut,mnulcut
      real(dp):: mZjcut,ptcheck,aetacheck
      integer:: ilep,igam,inu,countljet,countbjet,reconstr_top,id(mxpart)
      real(dp):: pljet(mxpart,4),pbjet(mxpart,4),pthad
      common/stopvars/ht,qeta,mlbnu,merecon,reconcorr
      common/hwwvars/dphi_ll,m_ll,mtrans,scut1,scut2
      common/rcut/Rcut
c***********************************************************************
c     Set-up the jet-like cut parameters here                          *
      parameter (jetpt=15._dp,jetrap=2._dp)
c***********************************************************************
      parameter(phillcut=1.2_dp,etajet2cut=2.5_dp,mllcut=15._dp)
      parameter(mZjcut=40._dp,mnulcut=30._dp)
      integer, save :: countlept,countnu,countb,countel,countmu,
     &   leptindex(mxpart),nuindex(mxpart),bindex(mxpart),
     &   elindex(mxpart),muindex(mxpart)
!$omp threadprivate(countlept,countnu,countb,countel,countmu)
!$omp threadprivate(leptindex,nuindex,bindex,elindex,muindex)

      pjet(:,:)=pjet_in(:,:)

      gencuts_input=.false.

      hwwjetcuts=.false.

      if (first) then
      first=.false.
c--- initialize counters and arrays that will be used to perform cuts
      countlept=0
      countnu=0
      countel=0
      countmu=0
c--- lepton pt and rapidity cuts
      do j=3,mxpart
        if (is_lepton(j)) then
          countlept=countlept+1
          leptindex(countlept)=j
        endif
        if (is_neutrino(j)) then
           countnu=countnu+1
           nuindex(countnu)=j
        endif
        if (is_electron(j)) then
          countel=countel+1
          elindex(countel)=j
        endif
        if (is_muon(j)) then
          countmu=countmu+1
          muindex(countmu)=j
        endif
      enddo
c--- Look for particles that should be treated as jets,
c--- so far only b decays from Z-bosons and
c--- hadronic decay of the W in diboson processes
c--- If these are present, we will do additional cuts
      countb=0
      do j=3,mxpart
         if ((plabel(j) == 'qb') .or. (plabel(j) == 'ab')
     &  .or. (plabel(j) == 'qq') .or. (plabel(j) == 'qa')) then
           countb=countb+1
           bindex(countb)=j
         endif
      enddo
c--- write-out the cuts we are using
!$omp master
      if (rank == 0) then
      write(6,*)
      write(6,*)  '****************** Generic cuts ********************'
      write(6,*)  '*                                                  *'
      if (leptptmax > 0.99e6_dp) then
        write(6,99) '*        pt(lepton)      >   ',leptptmin,
     &                  ' GeV            *'
      else
        write(6,98) leptptmin,'    pt(lepton)     ',leptptmax,'GeV'
      endif
      write(6,98) leptrapmin,'   |eta(lepton)|   ',leptrapmax,'   '
      write(6,99) '*       pt(missing)      >   ',misspt,
     &                ' GeV            *'
      if ((leptpt2min  /=  zip) .or. (leptrap2max  /=  zip)) then
      if (leptpt2max > 0.99e6_dp) then
        write(6,99) '*        pt(2nd lepton)      >   ',leptpt2min,
     &                  ' GeV            *'
      else
        write(6,98) leptpt2min,'  pt(2nd lepton)   ',leptpt2max,'GeV'
      endif
      write(6,98) leptrap2min,' |eta(2nd lepton)| ',leptrap2max,'   '
      endif
      if ((leptpt3min  /=  zip) .or. (leptrap3max  /=  zip)) then
      if (leptpt3max > 0.99e6_dp) then
        write(6,99) '*        pt(3rd lepton)      >   ',leptpt3min,
     &                  ' GeV            *'
      else
        write(6,98) leptpt3min,'  pt(3rd lepton)   ',leptpt3max,'GeV'
      endif
      write(6,98) leptrap3min,' |eta(3rd lepton)| ',leptrap3max,'   '
      endif
      if     (kcase==kWgamma) then
      if (mtrans34cut < zip) then
      write(6,99) '*   (3,4,5) trans. mass  >   ',abs(mtrans34cut),
     &                ' GeV            *'
      else
      write(6,99) '* (e-gam,nu) trans. mass >   ',mtrans34cut,
     &                ' GeV            *'
      endif
      elseif (kcase==kZgamma) then
      write(6,99) '*    (3,4,5) inv. mass   >   ',mtrans34cut,
     &                ' GeV            *'
      elseif ( kcase==kZ_tjet) then
         write(6,99) '*      R(jet,lepton)     >   ',Rjlmin,
     &        '                *'
         write(6,99) '*    |mll-mz| <   ',mllcut,
     &                ' GeV            *'
         write(6,99) '*    |mZj-mt| <   ',mZjcut,
     &                ' GeV            *'

      else
      write(6,99) '*  (3,4) transverse mass >   ',mtrans34cut,
     &                ' GeV            *'
      endif
      write(6,99) '*      R(jet,lepton)     >   ',Rjlmin,
     &                '                *'
      write(6,99) '*     R(lepton,lepton)   >   ',Rllmin,
     &                '                *'
      write(6,99) '* |eta(jet1)-eta(jet2)|  >   ',delyjjmin,
     &                '                *'
      if (jetsopphem) then
      write(6,*) '*           eta(jet1) . eta(jet2)  <  0            *'
      endif
      if     (lbjscheme == 1) then
      write(6,*) '*        eta(jet1)  <  eta(lept)  <  eta(jet2)     *'
      elseif (lbjscheme >= 2) then
      write(6,*) '*  eta(jet1)+Rcut  <  eta(lept)  <  eta(jet2)-Rcut *'
      lbjscheme=2
      endif
      if (countb > 0) then
      write(6,*)  '*                                                  *'
      write(6,99) '*      pt(jet)       >   ',jetpt,
     &                ' GeV                *'
      write(6,99) '*    |eta(jet)|      <   ',jetrap,
     &                '                    *'
      write(6,99) '*   R(jet1,jet2)     >   ',Rcut,
     &                '                    *'
      endif
      if (hwwjetcuts) then
      write(6,99) '*   phi(lepton,lepton)   <   ',phillcut,
     &                '                *'
      write(6,99) '*  |eta(2nd jet)|        >   ',etajet2cut,
     &                '                *'
      write(6,99) '*  m(lepton,lepton)      <   ',mllcut,
     &                '                *'
      endif
      write(6,*)  '****************************************************'
      endif
!$omp end master
      endif

      if ((kcase == kWZbbar) .and. (interference)) then
c--- arrange lepton entries in pjet such that the muon cuts are
c--- associated with the Z decay (5 and 6) and the electron cuts to
c--- the W decay (3 or 4); discriminator is ATLAS resonant-shape procedure
        m34sq=(pjet(3,4)+pjet(4,4))**2-(pjet(3,1)+pjet(4,1))**2
     &       -(pjet(3,2)+pjet(4,2))**2-(pjet(3,3)+pjet(4,3))**2
        m56sq=(pjet(5,4)+pjet(6,4))**2-(pjet(5,1)+pjet(6,1))**2
     &       -(pjet(5,2)+pjet(6,2))**2-(pjet(5,3)+pjet(6,3))**2
        m36sq=(pjet(3,4)+pjet(6,4))**2-(pjet(3,1)+pjet(6,1))**2
     &       -(pjet(3,2)+pjet(6,2))**2-(pjet(3,3)+pjet(6,3))**2
        m45sq=(pjet(5,4)+pjet(4,4))**2-(pjet(5,1)+pjet(4,1))**2
     &       -(pjet(5,2)+pjet(4,2))**2-(pjet(5,3)+pjet(4,3))**2
        P1=abs(m34sq-wmass**2+im*wmass*wwidth)**2*abs(m56sq-zmass**2+im*zmass*zwidth)**2
        if (nwz == +1) then
          P2=abs(m36sq-wmass**2+im*wmass*wwidth)**2*abs(m45sq-zmass**2+im*zmass*zwidth)**2
          if (P2 < P1) then
            pjet(4,:)=pjet_in(6,:)
            pjet(6,:)=pjet_in(4,:)
          endif
        else
          P2=abs(m45sq-wmass**2+im*wmass*wwidth)**2*abs(m36sq-zmass**2+im*zmass*zwidth)**2
          if (P2 < P1) then
            pjet(3,:)=pjet_in(5,:)
            pjet(5,:)=pjet_in(3,:)
          endif
        endif
      endif

c     Basic pt and rapidity cuts for lepton
      if     (countlept == 1) then
          ptcheck=pt(leptindex(1),pjet)
          aetacheck=abs(etarap(leptindex(1),pjet))
          if ((ptcheck < leptptmin) .or. (ptcheck > leptptmax)
     &   .or. (aetacheck > leptrapmax) .or. (aetacheck < leptrapmin)) then
            gencuts_input=.true.
            return
          endif
          if ((aetacheck > leptveto1min) .and.
     &        (aetacheck < leptveto1max)) then
            gencuts_input=.true.
            return
          endif

      elseif (countlept > 1) then
          do j=1,countlept
            ptvals(j)=pt(leptindex(j),pjet)
          enddo
          call arraysort(countlept,ptvals,id)

! lepton 1
          ptcheck=pt(leptindex(id(1)),pjet)
          aetacheck=abs(etarap(leptindex(id(1)),pjet))
          if ((ptcheck < leptptmin) .or. (ptcheck > leptptmax)
     &   .or. (aetacheck > leptrapmax) .or. (aetacheck < leptrapmin)) then
            gencuts_input=.true.
            return
          endif
          if ((aetacheck > leptveto1min) .and.
     &        (aetacheck < leptveto1max)) then
            gencuts_input=.true.
            return
          endif

! lepton 2
          ptcheck=pt(leptindex(id(2)),pjet)
          aetacheck=abs(etarap(leptindex(id(2)),pjet))
          if ((ptcheck < leptpt2min) .or. (ptcheck > leptpt2max)
     &   .or. (aetacheck > leptrap2max) .or. (aetacheck < leptrap2min)) then
            gencuts_input=.true.
            return
          endif
          if ((aetacheck > leptveto2min) .and.
     &        (aetacheck < leptveto2max)) then
            gencuts_input=.true.
            return
          endif

          if (countlept > 2) then
! leptons 3 and beyond
          do j=3,countlept
          ptcheck=pt(leptindex(id(j)),pjet)
          aetacheck=abs(etarap(leptindex(id(j)),pjet))
          if ((ptcheck < leptpt3min) .or. (ptcheck > leptpt3max)
     &   .or. (aetacheck > leptrap3max) .or. (aetacheck < leptrap3min)) then
            gencuts_input=.true.
            return
          endif
          if ((aetacheck > leptveto2min) .and.
     &        (aetacheck < leptveto2max)) then
            gencuts_input=.true.
            return
          endif
          enddo
          endif

!---c--- loop over all the lepton possibilities for lepton 1 (j)
!---          j=0
!---  77      continue
!---          j=j+1
!---          passedlept=.true.
!---          ptcheck=pt(leptindex(j),pjet)
!---          aetacheck=abs(etarap(leptindex(j),pjet))
!---          if ((ptcheck < leptptmin) .or. (ptcheck > leptptmax)
!---     &   .or. (aetacheck > leptrapmax) .or. (aetacheck < leptrapmin)) then
!---            passedlept=.false.
!---            goto 78
!---          endif
!---          if ((aetacheck > leptveto1min) .and.
!---     &        (aetacheck < leptveto1max)) then
!---            passedlept=.false.
!---            goto 78
!---          endif
!---          do k=1,countlept
!---            if (k  /=  j) then
!---             ptcheck=pt(leptindex(k),pjet)
!---             aetacheck=abs(etarap(leptindex(k),pjet))
!---             if ((ptcheck < leptpt2min) .or. (ptcheck > leptpt2max)
!---     &      .or. (aetacheck > leptrap2max) .or. (aetacheck < leptrap2min)) then
!---                passedlept=.false.
!---              endif
!---              if ((aetacheck > leptveto2min) .and.
!---     &            (aetacheck < leptveto2max)) then
!---                passedlept=.false.
!---              endif
!---            endif
!---          enddo
!---  78      continue
!---c--- return to beginning if we failed and there are more leptons to try
!---          if ((passedlept .eqv. .false.).and.(j < countlept)) goto 77
!---          gencuts_input=.not.(passedlept)
!---          if (gencuts_input) return
      endif

! 3-lepton system mass cut
      if (countlept == 3) then
        m3lsq=(pjet(leptindex(1),4)+pjet(leptindex(2),4)+pjet(leptindex(3),4))**2
     &       -(pjet(leptindex(1),1)+pjet(leptindex(2),1)+pjet(leptindex(3),1))**2
     &       -(pjet(leptindex(1),2)+pjet(leptindex(2),2)+pjet(leptindex(3),2))**2
     &       -(pjet(leptindex(1),3)+pjet(leptindex(2),3)+pjet(leptindex(3),3))**2
        if (m3lsq < m3lmin**2) then
          gencuts_input=.true.
          return
        endif
      endif

c--- electron and muon specific cuts (pt and R)
      if (countel > 0) then
        do k=1,countel
          if (pt(elindex(k),pjet) < elptmin) then
            gencuts_input=.true.
            return
          endif
          aetacheck=abs(etarap(elindex(k),pjet))
          if (((aetacheck > elvetomin) .and. (aetacheck < elvetomax))
     &       .or. (aetacheck > elrapmax)) then
            gencuts_input=.true.
            return
          endif
        enddo
      endif

      if (countmu > 0) then
        do k=1,countmu
          if (pt(muindex(k),pjet) < muptmin) then
            gencuts_input=.true.
            return
          endif
          aetacheck=abs(etarap(muindex(k),pjet))
          if (((aetacheck > muvetomin) .and. (aetacheck < muvetomax))
     &       .or. (aetacheck > murapmax)) then
            gencuts_input=.true.
            return
          endif
        enddo
      endif

      if (countel > 1) then
        do j=1,countel
        do k=j+1,countel
          if (R(pjet,elindex(j),elindex(k)) < Relelmin) then
            gencuts_input=.true.
            return
          endif
        enddo
        enddo
      endif

      if ((countel > 0) .and. (countmu > 0)) then
        do j=1,countel
        do k=1,countmu
          if (R(pjet,elindex(j),muindex(k)) < Relmumin) then
            gencuts_input=.true.
            return
          endif
        enddo
        enddo
      endif

      if (countmu > 1) then
        do j=1,countmu
        do k=j+1,countmu
          if (R(pjet,muindex(j),muindex(k)) < Rmumumin) then
            gencuts_input=.true.
            return
          endif
        enddo
        enddo
      endif

c--- missing energy cut
      evtmisset=etmiss(pjet,etvec)
      if ((evtmisset < misspt) .and. (evtmisset  /=  zip)) then
        gencuts_input=.true.
        return
      endif

c--- pt(miss,rel) cut
      if (missrelpt > zip) then
        phimin=99d0
        jmin=-1
        do j=1,countlept
        phicheck=abs(delphi(pjet(leptindex(j),:),etvec))
        if (phicheck < phimin) then
          jmin=j
          phimin=phicheck
        endif
        enddo
        if (jmin == -1) then
          write(6,*) 'Invalid cut: missrelpt with no leptons!'
          stop
        endif
        phill=min(phimin,pi/two)
        if (evtmisset*sin(abs(phill)) < missrelpt) then
          gencuts_input=.true.
          return
        endif

      endif

c--- mtrans34cut is used for three roles:
c---  1) Wgamma    --> mtrans34cut<0: transverse mass cut on (3,4,5) system
c---               --> mtrans34cut<0: transverse mass cut on (e-gam,nu) system
c---  2) Zgamma    --> invariant mass cut on (3,4,5) system
c---  3) otherwise --> transverse mass cut on (3,4) system
c---
      if ( (kcase==kWgamma) .or. (kcase==kWgajet) .or. (kcase==kWgaj_a)
     &.or. (kcase==kWga_ew) .or. (kcase==kWgajew) .or. (kcase==kWgajja)) then
c--- cut on transverse mass of (3,4,5) system for Wgamma
        if (mtrans34cut < zip) then
        mtrans=zip
        do j=3,5
           mtrans=mtrans+sqrt(pjet(j,1)**2+pjet(j,2)**2)
        enddo
        mtrans=mtrans**2
        do j=1,2
           mtrans=mtrans-(pjet(3,j)+pjet(4,j)+pjet(5,j))**2
        enddo
        mtrans=sqrt(max(mtrans,zip))
        if (mtrans < abs(mtrans34cut)) then
           gencuts_input=.true.
           return
        endif
        else
c--- cut on (e-gam,nu) transverse mass for Wgamma
        if (is_neutrino(3)) then
          inu=3
          ilep=4
        else
          inu=4
          ilep=3
        endif
        igam=5
        if (kcase == kWga_ew) then
          if (pjet(6,4) > 1.e-8_dp) then
            if (pt(6,pjet) > pt(5,pjet)) then
              igam=6
            endif
          endif
        endif
        mtrans=(pjet(ilep,4)+pjet(igam,4))**2
     &        -(pjet(ilep,1)+pjet(igam,1))**2
     &        -(pjet(ilep,2)+pjet(igam,2))**2
     &        -(pjet(ilep,3)+pjet(igam,3))**2
        mtrans=mtrans+(pjet(ilep,1)+pjet(igam,1))**2
     &               +(pjet(ilep,2)+pjet(igam,2))**2
        mtrans=sqrt(max(mtrans,zip))
     &        +sqrt(pjet(inu,1)**2+pjet(inu,2)**2)
        mtrans=mtrans**2
        do j=1,2
           mtrans=mtrans-(pjet(3,j)+pjet(4,j)+pjet(5,j))**2
        enddo
        mtrans=sqrt(max(mtrans,zip))
        if (mtrans < mtrans34cut) then
           gencuts_input=.true.
           return
        endif
        endif
c--- cut on invariant mass of (3,4,5) system for Zgamma
      elseif (kcase==kZgamma) then
        mtrans=(pjet(3,4)+pjet(4,4)+pjet(5,4))**2
        do j=1,3
          mtrans=mtrans-(pjet(3,j)+pjet(4,j)+pjet(5,j))**2
        enddo
        mtrans=sqrt(max(mtrans,zip))
        if(mtrans<mtrans34cut) then
           gencuts_input=.true.
           return
        endif
      else
c--- cut on transverse mass of (3,4) pair otherwise
        mtrans=
     &   (pjet(3,1)*pjet(4,1)+pjet(3,2)*pjet(4,2))
     &   /sqrt((pjet(3,1)**2+pjet(3,2)**2)
     &         *(pjet(4,1)**2+pjet(4,2)**2))
        mtrans=2._dp*sqrt(pjet(3,1)**2+pjet(3,2)**2)
     &   *sqrt(pjet(4,1)**2+pjet(4,2)**2)*(1._dp-mtrans)
        mtrans=sqrt(max(mtrans,zip))
        if (mtrans < mtrans34cut) then
          gencuts_input=.true.
        return
        endif
      endif

c--- lepton-lepton DeltaR and invariant mass cuts (if there are 2 or more leptons)
      if ((countlept > 1)) then
        do j=1,countlept
        do k=j+1,countlept
          if (R(pjet,leptindex(j),leptindex(k)) < Rllmin) then
            gencuts_input=.true.
            return
          endif
          if (pttwo(leptindex(j),leptindex(k),pjet) < ptllmin) then
            gencuts_input=.true.
            return
          endif
          mtrans=(pjet(leptindex(j),4)+pjet(leptindex(k),4))**2
     &          -(pjet(leptindex(j),1)+pjet(leptindex(k),1))**2
     &          -(pjet(leptindex(j),2)+pjet(leptindex(k),2))**2
     &          -(pjet(leptindex(j),3)+pjet(leptindex(k),3))**2
          if ((mtrans < mllmin**2) .or. (mtrans > mllmax**2)) then
            gencuts_input=.true.
            return
          endif
        enddo
        enddo
c--- extra cut on phi(lept,lept) for H(->WW)+jet search
        if (hwwjetcuts) then
          phill=
     &       (pjet(leptindex(1),1)*pjet(leptindex(2),1)
     &       +pjet(leptindex(1),2)*pjet(leptindex(2),2))
     &       /sqrt((pjet(leptindex(1),1)**2+pjet(leptindex(1),2)**2)
     &             *(pjet(leptindex(2),1)**2+pjet(leptindex(2),2)**2))
          if (phill < -0.999999999_dp) phill=-1._dp
          phill=acos(phill)
          if (phill > phillcut) then
            gencuts_input=.true.
            return
          endif
        endif
c--- extra cut on m(lept,lept) for H(->WW)+jet search
        if (hwwjetcuts) then
          mll=sqrt(2._dp*(
     &       +pjet(leptindex(1),4)*pjet(leptindex(2),4)
     &       -pjet(leptindex(1),1)*pjet(leptindex(2),1)
     &       -pjet(leptindex(1),2)*pjet(leptindex(2),2)
     &       -pjet(leptindex(1),3)*pjet(leptindex(2),3)))
          if (mll > mllcut) then
            gencuts_input=.true.
            return
          endif
        endif
      endif

c -- mll lepton cut for Ztjet production
      if (kcase==kZ_tjet .or. kcase==kZ_tdkj) then
         mll=sqrt(2._dp*(
     &        +pjet(leptindex(1),4)*pjet(leptindex(2),4)
     &        -pjet(leptindex(1),1)*pjet(leptindex(2),1)
     &        -pjet(leptindex(1),2)*pjet(leptindex(2),2)
     &        -pjet(leptindex(1),3)*pjet(leptindex(2),3)))
c         if ( abs(mll-zmass) >= mllcut) then
c            gencuts_input=.true.
c            return
c         endif
      endif
      if (kcase==kZ_tdkj) then
         mnul=sqrt(2._dp*(
     &        +pjet(nuindex(1),4)*pjet(leptindex(3),4)
     &        -pjet(nuindex(1),1)*pjet(leptindex(3),1)
     &        -pjet(nuindex(1),2)*pjet(leptindex(3),2)
     &        -pjet(nuindex(1),3)*pjet(leptindex(3),3)))
c         if ( abs(mnul-wmass) >= mnulcut) then
c            gencuts_input=.true.
c            return
c         endif
      endif

c--- if there are no cuts on the jets - or no jets - we are done
      if ((Rjlmin <= zip) .and. (delyjjmin <= zip) .and. (Rlepiso <=zip)) return
!      if ((njets == 0) .and. (countb == 0)) return

c--- get rid of 0-jet contribution
      if (njets == 0) then
        gencuts_input=.true.
        return
      endif

c--- identify the jets
      countjet=0
      do j=3,mxpart
        if (is_hadronic(j)) then
          countjet=countjet+1
          jetindex(countjet)=j
        endif
      enddo

c--- countjet will pick up the extra 'pp' needed for the real piece,
c--- therefore we should subtract 1 from this number
      if (countjet > njets) countjet=countjet-1
c--- for SCET case, there can be up to two extra jets present
      if (usescet .and. (countjet > njets)) countjet=countjet-1

      if ((njets  /=  countjet) .and. (notag == 0)) then
        write(6,*) 'Something is wrong in gencuts_input.f -'
        write(6,*) 'countjet = ',countjet,' BUT njets = ',njets
        stop
      endif

c -- identify b-jets
      call idjet(pjet,jetindex,countljet,countbjet,
     &     pljet,pbjet)

c--- extra cut on eta(2nd jet) for H(->WW)+jet search
      if ((hwwjetcuts) .and. (countjet >= 2)) then
        if (pt(jetindex(1),pjet) > pt(jetindex(2),pjet)) then
          if (abs(etarap(jetindex(2),pjet)) < etajet2cut) then
            gencuts_input=.true.
            return
          endif
        else
          if (abs(etarap(jetindex(1),pjet)) < etajet2cut) then
            gencuts_input=.true.
            return
          endif
        endif
      endif
c--- jet-lepton separation (if there are 1 or more jets and leptons)
c -- applies to all jets
      if ((njets > 0) .and. (countlept > 0)) then
        do j=1,countlept
        do k=1,njets
           if (R(pjet,leptindex(j),jetindex(k)) < Rjlmin) then
            gencuts_input=.true.
            return
          endif
        enddo
        enddo
      endif

c--- lepton isolation (if there are 1 or more jets and leptons)
      if ((njets > 0) .and. (countlept > 0)) then
        do j=1,countlept
          pthad=0._dp
          do k=1,njets
             if (R(pjet,leptindex(j),jetindex(k)) < Rlepiso) then
               pthad=pthad+pt(jetindex(k),pjet)
             endif
          enddo
          if (pthad > fraclepiso*pt(leptindex(j),pjet)) then
            gencuts_input=.true.
            return
          endif
        enddo
      endif

c -- cut on Zj mass -- use light jet only
c -- only one light jet needs to be able to construct the top mass
      if (kcase==kZ_tjet .or. kcase==kZ_tdkj) then
         reconstr_top=0
         do j=1,countljet
            pZj(:)=pljet(j,:)+pjet(leptindex(1),:)
     &           +pjet(leptindex(2),:)
            mZj=sqrt(pZj(4)**2-pZj(1)**2-pZj(2)**2-pZj(3)**2)

            if ( abs(mZj-mt) <= mZjcut) then
               reconstr_top=reconstr_top+1
            endif
         enddo
         if (reconstr_top == 0) then
            gencuts_input=.true.
            return
         endif
      endif

c--- Cut on the leading jet pt

      if(njets > 1) then
        write(6,*) 'pt(jetindex(1), pjet)=', pt(jetindex(1),pjet)
        write(6,*) 'pt(jetindex(2), pjet)=', pt(jetindex(2),pjet)
      endif
      if(njets > 2)then
        write(6,*) 'pt(jetindex(3), pjet)=', pt(jetindex(3),pjet)
      endif
      if (pt(jetindex(1),pjet) <= 500.0) then
        gencuts_input=.true.
        return
      endif

c--- WBF-style cuts (if there are 2 or more jets)
      if ((njets > 1)) then
c--- jet-jet rapidity separation
c--- j and k point to the two highest pt ('tagging') jets
        j=1
        k=2
        if (njets == 3) then
          if     ( pt(jetindex(1),pjet) <
     &      min(pt(jetindex(2),pjet),pt(jetindex(3),pjet)) ) then
            j=2
            k=3
          elseif ( pt(jetindex(2),pjet) <
     &      min(pt(jetindex(1),pjet),pt(jetindex(3),pjet)) ) then
            j=1
            k=3
          endif
        endif
        if (abs(etarap(jetindex(j),pjet)-etarap(jetindex(k),pjet))
     &         < delyjjmin) then
          gencuts_input=.true.
          return
        endif

c--- Requirement that jets be in opposite hemispheres
        if (jetsopphem) then
          if(etarap(jetindex(j),pjet)*etarap(jetindex(k),pjet) > zip)
     &       then
            gencuts_input=.true.
            return
          endif
        endif

        if (lbjscheme > 0) then
c--- Cut to require lepton to be between jets
          etabuffer=real(lbjscheme-1,dp)*Rcut
          etaj=etarap(jetindex(j),pjet)
          etak=etarap(jetindex(k),pjet)
          do pntr=1,countlept
            etalept=etarap(leptindex(pntr),pjet)
            if ( (etalept < min(etaj,etak)+etabuffer) .or.
     &           (etalept > max(etaj,etak)-etabuffer) ) then
              gencuts_input=.true.
              return
            endif
          enddo
        endif

      endif

c-- cuts on b-quarks
      if (bbproc) then
        call getbs(pjet,ib1,ib2)
        if ( (abs(etarap(ib1,pjet)) > etabjetmax)
     &  .or. (abs(etarap(ib1,pjet)) < etabjetmin)
     &  .or. (pt(ib1,pjet) < ptbjetmin)
     &  .or. (pt(ib1,pjet) > ptbjetmax) ) gencuts_input=.true.
        if ( (abs(etarap(ib2,pjet)) > etabjetmax)
     &  .or. (abs(etarap(ib2,pjet)) < etabjetmin)
     &  .or. (pt(ib2,pjet) < ptbjetmin)
     &  .or. (pt(ib2,pjet) > ptbjetmax) ) gencuts_input=.true.
      endif

c--- completed basic cuts
c--- if there are jet-like particles (see above), do more cuts
      if (countb > 0) then
        do j=1,countb
          jindex=bindex(j)
          if (          (pt(jindex,pjet) < jetpt) .or.
     &           (abs(etarap(jindex,pjet)) > jetrap)) then
            gencuts_input=.true.
            return
          endif
          do k=j+1,countb
            kindex=bindex(k)
            if ((r(pjet,jindex,kindex) < rcut)) then
              gencuts_input=.true.
              return
            endif
          enddo
        enddo
      endif

      return

   98 format(' *  ',f9.3,' < ',a18,' < ',f9.3,a4,'  *')
   99 format(1x,a29,f6.2,a17)


      end
