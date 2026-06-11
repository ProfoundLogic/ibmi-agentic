     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H*********************************************************************
     H/TITLE W0093R1 - Third Party Billing Data Update
     H*********************************************************************
     H* CREATED: 02/03/09  C:33  PROJECT 146WBW                           *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 12/31/20 C:136 Support: 92607                                     *
     H*                Retro premium history records caused pgm to        *
     H*                write estimated figures to premium history         *
     H*                even if policy was audited.                        *
     H* 06/20/11 C:136 Support: WA146 77093                               *
     H*                Prevent cash transactions from being written to    *
     H*                Live                                               *
     H*                                                                   *
     H* 03/28/11 C:136 WA146 74563                                        *
     H*                Exclude cash records in A/R from 3rd party         *
     H*                                                                   *
     H* 06/21/10 C:136 WA146 71214                                        *
     H*                Update EHTRDT field when writing Premium History   *
     H*                to keep CEU straight                               *
     H*                                                                   *
     H*********************************************************************
     Fwdell     uf   e           k disk    rename(pel01:pel00)
     Fwmeal     uf a e           k disk    rename(pea01:pea00)
     Fwae3l     uf a e           k disk    rename(pel01:pe300)
     Fwae4l     uf a e           k disk    rename(pea01:pe400)
     Fwabll1    uf   e           k disk
     Fwdehl12   if a e           k disk    rename(peh01:peh12)
      *
     D @@Co#           s                   like(ELCO#)
     D @@Fnd           s                   like(ELFND)
     D @@Emp#          s                   like(ELEMP#)
     D @@Div           s                   like(ELDIV)
     D @@Fyr           s                   like(ELFYR)
     D @@Mode          s              1
     d EQWMPR          s                   like(EHWMPR)
     d EQWSPR          s                   like(EHWSPR)
     d EQWNMP          s                   like(EHWNMP)
     d EQWAPR          s                   like(EHWAPR)
     d EQWDC1          s                   like(EHWDC1)
     d EQWDC2          s                   like(EHWDC2)
     d EQWDC3          s                   like(EHWDC3)
     d EQWXP1          s                   like(EHWXP1)
     d EQWXP2          s                   like(EHWXP2)
     d EQWXP3          s                   like(EHWXP3)
     d EQWIN1          s                   like(EHWIN1)
     d EQWIN2          s                   like(EHWIN2)
     d EQWDCT          s                   like(EHWDCT)
     d EQWDD2          s                   like(EHWDD2)
     d EQWDCM          s                   like(EHWDCM)
     d EQWDCS          s                   like(EHWDCS)
     d EQWDCR          s                   like(EHWDCR)
     d EQWDC           s                   like(EHWDC)
     d EQWSBJ          s                   like(EHWSBJ)
     d EQWMPM          s                   like(EHWMPM)
     d EQWFRS          s                   like(EHWFRS)
     d EQWDV2          s                   like(EHWDV2)
     d EQIMPR          s                   like(EHIMPR)
     d EQISPR          s                   like(EHISPR)
     d EQINMP          s                   like(EHINMP)
     d EQIAPR          s                   like(EHIAPR)
     d EQIDC1          s                   like(EHIDC1)
     d EQIDC2          s                   like(EHIDC2)
     d EQIDC3          s                   like(EHIDC3)
     d EQIXP1          s                   like(EHIXP1)
     d EQIXP2          s                   like(EHIXP2)
     d EQIXP3          s                   like(EHIXP3)
     d EQIIN1          s                   like(EHIIN1)
     d EQIIN2          s                   like(EHIIN2)
     d EQIDCT          s                   like(EHIDCT)
     d EQIDD2          s                   like(EHIDD2)
     d EQIDCM          s                   like(EHIDCM)
     d EQIDCS          s                   like(EHIDCS)
     d EQIDCR          s                   like(EHIDCR)
     d EQIDC           s                   like(EHIDC)
     d EQISBJ          s                   like(EHISBJ)
     d EQIMPM          s                   like(EHIMPM)
     d EQIFRS          s                   like(EHIFRS)
     d EQIDV2          s                   like(EHIDV2)
     d EQWADL          s                   like(EHWADL)
     d EQIADL          s                   like(EHIADL)
     d EQWTER          s                   like(EHWTER)
     d EQITER          s                   like(EHITER)
     d EQWDTR          s                   like(EHWDTR)
     d EQIDTR          s                   like(EHIDTR)
     d EQWHXM          s                   like(EHWHXM)
     d EQIHXM          s                   like(EHIHXM)
     d EQWVLC          s                   like(EHWVLC)
     d EQIVLC          s                   like(EHIVLC)
     d EQWRP1          s                   like(EHWRP1)
     d EQWRP2          s                   like(EHWRP2)
     d EQWRP3          s                   like(EHWRP3)
     d EQIRP1          s                   like(EHIRP1)
     d EQIRP2          s                   like(EHIRP2)
     d EQIRP3          s                   like(EHIRP3)
     d EQWARC          s                   like(EHWARC)
     d EQIARC          s                   like(EHIARC)
     d EQWLOS          s                   like(EHWLOS)
     d EQILOS          s                   like(EHILOS)
      *=================================================================================
      *   Arrays and data structures
      *=================================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=================================================================================
      *   Procedure Prototpes
      *=================================================================================
     D clearAR         s              1n   inz
     D clearPrem       s              1n   inz
     D/copy sprdate
      *=================================================================================
      *   KLISTS's
      *=================================================================================
     C     key1          klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C                   kfld                    @@Fyr
     C*
     C     key2          klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Fyr
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div

     C     *entry        plist
     C                   parm                    @@Co#
     C                   parm                    @@Fnd
     C                   parm                    @@Emp#
     C                   parm                    @@Div
     C                   parm                    @@Fyr
     C                   parm                    @@Mode
     C*
     C                   select
     C* Copy from Live to hold files
     C                   when      @@Mode = '1'
     C                   exsr      FromLive
     C* Copy from hold files to Live
     C                   when      @@Mode = '2'
     C                   exsr      ToLive
     C* Reset Premium hold files from Live
     C                   when      @@Mode = '3'
     C                   eval      clearPrem = *on
     C                   exsr      ToLive
     C* Reset A/R hold files from Live
     C                   when      @@Mode = '4'
     C                   eval      clearAR = *on
     C                   exsr      ToLive
     C* Clear hold files
     C                   when      @@Mode = '5'
     C                   eval      clearAR   = *on
     C                   eval      clearPrem = *on
     C                   exsr      ClearHold
     C                   endsl
     C*
     C                   eval      *inlr = *on
     C*====================================================================
     C* Clear holding files for employer
     C*====================================================================
     C     ClearHold     begsr
     C*
     C                   if        clearAR
     C                   dou       not %found(WAE4L)
     C     key1          delete    WAE4L
     C                   enddo
     C                   endif
     C*
     C                   if        clearPrem
     C                   dou       not %found(WAE3L)
     C     key1          delete    WAE3L
     C                   enddo
     C                   endif
     C*
     C                   eval      clearAR   = *off
     C                   eval      clearPrem = *off
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Clear A/R live files
     C*====================================================================
     C     ClearLiveAR   begsr
     C*
      *** beg chg ***** 03/28/11 ******************************************
     C***********        dou       not %found(WMEAL)
     C*****key1**        delete    WMEAL
     C***********        enddo
     c     key1          setll     WMEAL
     c     key1          reade     WMEAL
     c                   dow       not(%eof)
     c                   if        EAARTY = 'A' or
     c                             EAARTY = 'B' or
     c                             EAARTY = 'D' or
     c                             EAARTY = 'F' or
     c                             EAARTY = 'H'
     c                   delete    WMEAL
     c                   endif
     c     key1          reade     WMEAL
     c                   enddo
      *** end chg ***** 03/28/11 ******************************************
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Load values from Live to holding files
     C*====================================================================
     C     FromLive      begsr
     C*
     C                   if        clearAR or clearPrem
     C                   exsr      ClearHold
     C                   endif
     C*
     C     key1          chain(n)  WAE3L
     C                   if        not %found
     C     key1          chain     WDELL
     C                   write     pe300
     C                   endif
     C*
     C     key1          chain     WAE4L
     C                   if        not %found
     C     key1          setll     WMEAL
     C     key1          reade(n)  WMEAL
     C                   dow       not %eof
      *** beg chg ***** 03/28/11 ***********************************************
     C***********        write     pe400
     c                   if        EAARTY = 'A' or
     c                             EAARTY = 'B' or
     c                             EAARTY = 'D' or
     c                             EAARTY = 'F' or
     c                             EAARTY = 'H'
     c                   write     pe400
     c                   endif
      *** end chg ***** 03/28/11 ***********************************************
     C     key1          reade(n)  WMEAL
     C                   enddo
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Load values from holding files back to Live
     C*====================================================================
     C     ToLive        begsr
     C*
     C     key1          chain     WDELL
     C     key1          chain(n)  WAE3L
     C                   update    pel00
     C*
     C     key1          chain(n)  WAE4L
     C                   if        %found
     C                   exsr      ClearLiveAR
     C     key1          setll     WAE4L
     C     key1          reade(n)  WAE4L
     C                   dow       not %eof
      *** beg chg ***** 06/20/11 *************************************
     C***********        write     pea00
     C                   if        EAARTY = 'A' or
     C                             EAARTY = 'B' or
     C                             EAARTY = 'D' or
     C                             EAARTY = 'F' or
     C                             EAARTY = 'H'
     C                   write     pea00
     C                   endif
      *** end chg ***** 06/20/11 *************************************
     C     key1          reade(n)  WAE4L
     C                   enddo
     C                   endif
     C*
     C                   exsr      WrtPremHist
     C                   exsr      UpdtBillWrk
     C*
     C                   eval      clearAR   = *on
     C                   eval      clearPrem = *on
     C                   exsr      ClearHold
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Update the Billing Workfile to ensure the employer will not run
     C*====================================================================
     C     UpdtBillWrk   begsr
     C*
     C     key1          chain     WABLL1
     C                   if        %found
     C                   delete    pbl01
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Write Premium History record
     C*====================================================================
     C     WrtPremHist   begsr
     C*
     C     KEY1          SETGT     PEH12
     C     KEY1          READPE    PEH12                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EHWMPR        EQWMPR
     C                   Z-ADD     EHWSPR        EQWSPR
     C                   Z-ADD     EHWNMP        EQWNMP
     C                   Z-ADD     EHWAPR        EQWAPR
     C                   Z-ADD     EHWDC1        EQWDC1
     C                   Z-ADD     EHWDC2        EQWDC2
     C                   Z-ADD     EHWDC3        EQWDC3
     C                   Z-ADD     EHWXP1        EQWXP1
     C                   Z-ADD     EHWXP2        EQWXP2
     C                   Z-ADD     EHWXP3        EQWXP3
     C                   Z-ADD     EHWIN1        EQWIN1
     C                   Z-ADD     EHWIN2        EQWIN2
     C                   Z-ADD     EHWDCT        EQWDCT
     C                   Z-ADD     EHWDD2        EQWDD2
     C                   Z-ADD     EHWDCM        EQWDCM
     C                   Z-ADD     EHWDCS        EQWDCS
     C                   Z-ADD     EHWDCR        EQWDCR
     C                   Z-ADD     EHWDC         EQWDC
     C                   Z-ADD     EHWSBJ        EQWSBJ
     C                   Z-ADD     EHWMPM        EQWMPM
     C                   Z-ADD     EHWFRS        EQWFRS
     C                   Z-ADD     EHWDV2        EQWDV2
     C                   Z-ADD     EHIMPR        EQIMPR
     C                   Z-ADD     EHISPR        EQISPR
     C                   Z-ADD     EHINMP        EQINMP
     C                   Z-ADD     EHIAPR        EQIAPR
     C                   Z-ADD     EHIDC1        EQIDC1
     C                   Z-ADD     EHIDC2        EQIDC2
     C                   Z-ADD     EHIDC3        EQIDC3
     C                   Z-ADD     EHIXP1        EQIXP1
     C                   Z-ADD     EHIXP2        EQIXP2
     C                   Z-ADD     EHIXP3        EQIXP3
     C                   Z-ADD     EHIIN1        EQIIN1
     C                   Z-ADD     EHIIN2        EQIIN2
     C                   Z-ADD     EHIDCT        EQIDCT
     C                   Z-ADD     EHIDD2        EQIDD2
     C                   Z-ADD     EHIDCM        EQIDCM
     C                   Z-ADD     EHIDCS        EQIDCS
     C                   Z-ADD     EHIDCR        EQIDCR
     C                   Z-ADD     EHIDC         EQIDC
     C                   Z-ADD     EHISBJ        EQISBJ
     C                   Z-ADD     EHIMPM        EQIMPM
     C                   Z-ADD     EHIFRS        EQIFRS
     C                   Z-ADD     EHIDV2        EQIDV2
     C                   Z-ADD     EHWADL        EQWADL
     C                   Z-ADD     EHIADL        EQIADL
     C                   Z-ADD     EHWLOS        EQWLOS
     C                   Z-ADD     EHILOS        EQILOS
     C                   Z-ADD     EHWTER        EQWTER
     C                   Z-ADD     EHITER        EQITER
     C                   Z-ADD     EHWDTR        EQWDTR
     C                   Z-ADD     EHIDTR        EQIDTR
     C                   Z-ADD     EHWHXM        EQWHXM
     C                   Z-ADD     EHIHXM        EQIHXM
     C                   Z-ADD     EHWVLC        EQWVLC
     C                   Z-ADD     EHIVLC        EQIVLC
     C                   Z-ADD     EHWRP1        EQWRP1
     C                   Z-ADD     EHWRP2        EQWRP2
     C                   Z-ADD     EHWRP3        EQWRP3
     C                   Z-ADD     EHIRP1        EQIRP1
     C                   Z-ADD     EHIRP2        EQIRP2
     C                   Z-ADD     EHIRP3        EQIRP3
     C                   Z-ADD     EHWARC        EQWARC
     C                   Z-ADD     EHIARC        EQIARC
     C                   ENDIF
     C*
     C  n60              SELECT
      *** beg chg ***** 12/31/20 ********************
     C***                WHEN      EHFAF <> 'A'
     C                   WHEN      ELFAF <> 'A'
      *** end chg ***** 12/31/20 ********************
     C                   MOVE(P)   ELCO#         EHCO#
     C                   MOVE(P)   ELFND         EHFND
     C                   MOVE(P)   ELFYR         EHFYR
     C                   MOVE(P)   ELEMP#        EHEMP#
     C                   MOVE(P)   ELDIV         EHDIV
     C                   MOVE(P)   ELBSCD        EHBSCD
     C                   MOVE(P)   ELPFDT        EHPFDT
     C                   MOVE(P)   ELPTDT        EHPTDT
     C                   MOVE(P)   ELANDT        EHANDT
     C                   MOVE(P)   ELPROR        EHPROR
     C                   MOVE(P)   ELMAPR        EHWMPR
     C                   MOVE(P)   ELSTPR        EHWSPR
     C                   MOVE(P)   ELENMP        EHWNMP
     C                   MOVE(P)   ELANPR        EHWAPR
     C                   MOVE(P)   ELPRTD        EHPRTD
     C                   MOVE(P)   ELPRTF        EHPRTF
     C                   MOVE(P)   ELWVFL        EHWVFL
     C                   MOVE(P)   ELASP1        EHASP1
     C                   MOVE(P)   ELCREF        EHCREF
     C                   MOVE(P)   ELCREX        EHCREX
     C                   MOVE(P)   ELEDC1        EHWDC1
     C                   MOVE(P)   ELASP2        EHASP2
     C                   MOVE(P)   ELDVEF        EHDVEF
     C                   MOVE(P)   ELDVEX        EHDVEX
     C                   MOVE(P)   ELEDC2        EHWDC2
     C                   MOVE(P)   ELASP3        EHASP3
     C                   MOVE(P)   ELDISC        EHDISC
     C                   MOVE(P)   ELEDC3        EHWDC3
     C                   MOVE(P)   ELEXM1        EHEXM1
     C                   MOVE(P)   ELEMD1        EHEMD1
     C                   MOVE(P)   ELEXP1        EHWXP1
     C                   MOVE(P)   ELEXM2        EHEXM2
     C                   MOVE(P)   ELEMD2        EHEMD2
     C                   MOVE(P)   ELEXP2        EHWXP2
     C                   MOVE(P)   ELEXM3        EHEXM3
     C                   MOVE(P)   ELEMD3        EHEMD3
     C                   MOVE(P)   ELEXP3        EHWXP3
     C                   MOVE(P)   ELILCD        EHILCD
     C                   MOVE(P)   ELILEF        EHILEF
     C                   MOVE(P)   ELILEX        EHILEX
     C                   MOVE(P)   ELEIN1        EHWIN1
     C                   MOVE(P)   ELINL2        EHI2CD
     C                   MOVE(P)   ELI2EF        EHI2EF
     C                   MOVE(P)   ELI2EX        EHI2EX
     C                   MOVE(P)   ELEIN2        EHWIN2
     C                   MOVE(P)   ELDCDE        EHDCDE
     C                   MOVE(P)   ELDDCT        EHDDCT
     C                   MOVE(P)   ELCXBL        EHCXBL
     C                   MOVE(P)   ELDCEF        EHDCEF
     C                   MOVE(P)   ELDCEX        EHDCEX
     C                   MOVE(P)   ELEDCT        EHWDCT
     C                   MOVE(P)   ELDCD2        EHDCD2
     C                   MOVE(P)   ELDD2P        EHDD2P
     C                   MOVE(P)   EL2DEF        EH2DEF
     C                   MOVE(P)   EL2DEX        EH2DEX
     C                   MOVE(P)   ELEDD2        EHWDD2
     C                   MOVE(P)   ELASPM        EHASPM
     C                   MOVE(P)   ELMCEF        EHMCEF
     C                   MOVE(P)   ELMCEX        EHMCEX
     C                   MOVE(P)   ELEDCM        EHWDCM
     C                   MOVE(P)   ELASPS        EHASPS
     C                   MOVE(P)   ELSCEF        EHSCEF
     C                   MOVE(P)   ELSCEX        EHSCEX
     C                   MOVE(P)   ELEDCS        EHWDCS
     C                   MOVE(P)   ELRTCD        EHRTCD
     C                   MOVE(P)   ELSTAT        EHSTAT
     C                   MOVE(P)   Q1USER        EHUSER
     C                   MOVE(P)   Q1USER        EHIUSR
     C                   MOVE(P)   Q1PGM         EHPGM
     C                   eval      EHTIME = TimeTo6(SystemTime)
     C                   eval      EHDT   = DateTo7(SystemDate)
     C                   eval      EHIDT  = EHDT
      *** beg add ***** 06/21/10 *******************************************************
     C                   eval      EHTRDT = EHDT
      *** end add ***** 06/21/10 *******************************************************
     C                   MOVE(P)   ELDEV2        EHDEV2
     C                   MOVE(P)   ELEDV2        EHWDV2
     C                   MOVE(P)   ELD2EF        EHD2EF
     C                   MOVE(P)   ELD2EX        EHD2EX
     C                   MOVE(P)   ELDBCR        EHDBCR
     C                   MOVE(P)   ELCMUL        EHCMUL
     C                   MOVE(P)   ELEDCR        EHWDCR
     C                   MOVE(P)   ELEDC         EHWDC
     C                   MOVE(P)   ELESBJ        EHWSBJ
     C                   MOVE(P)   ELEMPM        EHWMPM
     C                   MOVE(P)   ELEFRS        EHWFRS
     C                   MOVE(P)   ELTXPR        EHTXPR
     C                   MOVE(P)   ELLFEF        EHLFEF
     C                   MOVE(P)   ELAVCC        EHAVCC
     C                   MOVE(P)   ELEIN2        EHWIN2
     C                   MOVE(P)   ELSDEF        EHSDEF
     C                   MOVE(P)   ELSDEX        EHSDEX
     C                   MOVE(P)   ELEDCR        EHWDCR
     C                   Z-ADD     ELEWAV        EHWWAV
     C                   MOVE(P)   ELNWRW        EHNWRW
     C                   Z-ADD     ELLOSS        EHWLOS
     C                   Z-ADD     ELETER        EHWTER
     C                   Z-ADD     ELEDTR        EHWDTR
     C                   Z-ADD     ELRPE1        EHWRP1
     C                   Z-ADD     ELRPE2        EHWRP2
     C                   Z-ADD     ELRPE3        EHWRP3
     C                   Z-ADD     ELVLCA        EHWVLC
     C                   Z-ADD     ELHXMN        EHWHXM
     C                   Z-ADD     ELARCA        EHARCA
     C                   Z-ADD     ELARCP        EHARCP
     C                   Z-ADD     ELAREF        EHAREF
     C                   Z-ADD     ELAREX        EHAREX
     C                   Z-ADD     ELEARC        EHWARC
     C                   MOVE      'R'           EHFAF
     C                   MOVE      'E'           EHFAF2
      *** beg chg ***** 12/31/20 ********************
     C***                WHEN      EHFAF = 'A'
     C                   When      ELfaf = 'A'
     C                   Eval      EHfaf = 'A'
      *** end chg ***** 12/31/20 ********************
     C                   MOVE(P)   ELCO#         EHCO#
     C                   MOVE(P)   ELFND         EHFND
     C                   MOVE(P)   ELFYR         EHFYR
     C                   MOVE(P)   ELEMP#        EHEMP#
     C                   MOVE(P)   ELDIV         EHDIV
     C                   MOVE(P)   ELBSCD        EHBSCD
     C                   MOVE(P)   ELPFDT        EHPFDT
     C                   MOVE(P)   ELPTDT        EHPTDT
     C                   MOVE(P)   ELANDT        EHANDT
     C                   MOVE(P)   ELPROR        EHPROR
     C                   MOVE(P)   ELAMPR        EHWMPR
     C                   MOVE(P)   ELSRAM        EHWSRA
     C                   MOVE(P)   ELASPR        EHWSPR
     C                   MOVE(P)   ELANMP        EHWNMP
     C                   MOVE(P)   ELAAPR        EHWAPR
     C                   MOVE(P)   ELAMNP        EHWMNP
     C                   MOVE(P)   ELPRTD        EHPRTD
     C                   MOVE(P)   ELPRTF        EHPRTF
     C                   MOVE(P)   ELWVFL        EHWVFL
     C                   MOVE(P)   ELASP1        EHASP1
     C                   MOVE(P)   ELCREF        EHCREF
     C                   MOVE(P)   ELCREX        EHCREX
     C                   MOVE(P)   ELADC1        EHWDC1
     C                   MOVE(P)   ELASP2        EHASP2
     C                   MOVE(P)   ELDVEF        EHDVEF
     C                   MOVE(P)   ELDVEX        EHDVEX
     C                   MOVE(P)   ELADC2        EHWDC2
     C                   MOVE(P)   ELASP3        EHASP3
     C                   MOVE(P)   ELDISC        EHDISC
     C                   MOVE(P)   ELADC3        EHWDC3
     C                   MOVE(P)   ELEXM1        EHEXM1
     C                   MOVE(P)   ELEMD1        EHEMD1
     C                   MOVE(P)   ELAXP1        EHWXP1
     C                   MOVE(P)   ELEXM2        EHEXM2
     C                   MOVE(P)   ELEMD2        EHEMD2
     C                   MOVE(P)   ELAXP2        EHWXP2
     C                   MOVE(P)   ELEXM3        EHEXM3
     C                   MOVE(P)   ELEMD3        EHEMD3
     C                   MOVE(P)   ELAXP3        EHWXP3
     C                   MOVE(P)   ELILCD        EHILCD
     C                   MOVE(P)   ELILEF        EHILEF
     C                   MOVE(P)   ELILEX        EHILEX
     C                   MOVE(P)   ELAIN1        EHWIN1
     C                   MOVE(P)   ELINL2        EHI2CD
     C                   MOVE(P)   ELI2EF        EHI2EF
     C                   MOVE(P)   ELI2EX        EHI2EX
     C                   MOVE(P)   ELAIN2        EHWIN2
     C                   MOVE(P)   ELDCDE        EHDCDE
     C                   MOVE(P)   ELDDCT        EHDDCT
     C                   MOVE(P)   ELCXBL        EHCXBL
     C                   MOVE(P)   ELDCEF        EHDCEF
     C                   MOVE(P)   ELDCEX        EHDCEX
     C                   MOVE(P)   ELADCT        EHWDCT
     C                   MOVE(P)   ELDCD2        EHDCD2
     C                   MOVE(P)   ELDD2P        EHDD2P
     C                   MOVE(P)   EL2DEF        EH2DEF
     C                   MOVE(P)   EL2DEX        EH2DEX
     C                   MOVE(P)   ELADD2        EHWDD2
     C                   MOVE(P)   ELASPM        EHASPM
     C                   MOVE(P)   ELMCEF        EHMCEF
     C                   MOVE(P)   ELMCEX        EHMCEX
     C                   MOVE(P)   ELADCM        EHWDCM
     C                   MOVE(P)   ELASPS        EHASPS
     C                   MOVE(P)   ELSCEF        EHSCEF
     C                   MOVE(P)   ELSCEX        EHSCEX
     C                   MOVE(P)   ELADCS        EHWDCS
     C                   MOVE(P)   ELRTCD        EHRTCD
     C                   MOVE(P)   ELSTAT        EHSTAT
     C                   MOVE(P)   Q1USER        EHUSER
     C                   MOVE(P)   Q1USER        EHIUSR
     C                   MOVE(P)   Q1PGM         EHPGM
     C                   eval      EHTIME = TimeTo6(SystemTime)
     C                   eval      EHDT   = DateTo7(SystemDate)
     C                   MOVE(P)   EHDT          EHIDT
      *** beg add ***** 06/21/10 *******************************************************
     C                   eval      EHTRDT = EHDT
      *** end add ***** 06/21/10 *******************************************************
     C                   MOVE(P)   ELDEV2        EHDEV2
     C                   MOVE(P)   ELADV2        EHWDV2
     C                   MOVE(P)   ELD2EF        EHD2EF
     C                   MOVE(P)   ELD2EX        EHD2EX
     C                   MOVE(P)   ELDBCR        EHDBCR
     C                   MOVE(P)   ELCMUL        EHCMUL
     C                   MOVE(P)   ELADCR        EHWDCR
     C                   MOVE(P)   ELADC         EHWDC
     C                   MOVE(P)   ELASBJ        EHWSBJ
     C                   MOVE(P)   ELAMPM        EHWMPM
     C                   MOVE(P)   ELAFRS        EHWFRS
     C                   MOVE(P)   ELTXPR        EHTXPR
     C                   MOVE(P)   ELAVCC        EHAVCC
     C                   MOVE(P)   ELSDEX        EHSDEX
     C                   MOVE(P)   ELSDEF        EHSDEF
     C                   MOVE(P)   ELLFEF        EHLFEF
     C                   Z-ADD     ELAWAV        EHWWAV
     C                   MOVE(P)   ELNWRW        EHNWRW
     C                   MOVE(P)   ELLOSS        EHWLOS
     C                   Z-ADD     ELATER        EHWTER
     C                   Z-ADD     ELADTR        EHWDTR
     C                   Z-ADD     ELVLCA        EHWVLC
     C                   Z-ADD     ELHXMN        EHWHXM
     C                   Z-ADD     ELRPA1        EHWRP1
     C                   Z-ADD     ELRPA2        EHWRP2
     C                   Z-ADD     ELRPA3        EHWRP3
     C                   Z-ADD     ELARCA        EHARCA
     C                   Z-ADD     ELARCP        EHARCP
     C                   Z-ADD     ELAREF        EHAREF
     C                   Z-ADD     ELAREX        EHAREX
     C                   Z-ADD     ELAARC        EHWARC
     C                   ENDSL
     c                   if        not(*in60)
     C     EHWMPR        SUB       EQWMPR        EHXMPR
     C     EHWSPR        SUB       EQWSPR        EHXSPR
     C     EHWNMP        SUB       EQWNMP        EHXNMP
     C     EHWAPR        SUB       EQWAPR        EHXAPR
     C     EHWDC1        SUB       EQWDC1        EHXDC1
     C     EHWDC2        SUB       EQWDC2        EHXDC2
     C     EHWDC3        SUB       EQWDC3        EHXDC3
     C     EHWXP1        SUB       EQWXP1        EHXXP1
     C     EHWXP2        SUB       EQWXP2        EHXXP2
     C     EHWXP3        SUB       EQWXP3        EHXXP3
     C     EHWIN1        SUB       EQWIN1        EHXIN1
     C     EHWIN2        SUB       EQWIN2        EHXIN2
     C     EHWDCT        SUB       EQWDCT        EHXDCT
     C     EHWDD2        SUB       EQWDD2        EHXDD2
     C     EHWDCM        SUB       EQWDCM        EHXDCM
     C     EHWDCS        SUB       EQWDCS        EHXDCS
     C     EHWDCR        SUB       EQWDCR        EHXDCR
     C     EHWDC         SUB       EQWDC         EHXDC
     C     EHWSBJ        SUB       EQWSBJ        EHXSBJ
     C     EHWMPM        SUB       EQWMPM        EHXMPM
     C     EHWFRS        SUB       EQWFRS        EHXFRS
     C     EHWDV2        SUB       EQWDV2        EHXDV2
     C     EHIMPR        SUB       EQIMPR        EHJMPR
     C     EHISPR        SUB       EQISPR        EHJSPR
     C     EHINMP        SUB       EQINMP        EHJNMP
     C     EHIAPR        SUB       EQIAPR        EHJAPR
     C     EHIDC1        SUB       EQIDC1        EHJDC1
     C     EHIDC2        SUB       EQIDC2        EHJDC2
     C     EHIDC3        SUB       EQIDC3        EHJDC3
     C     EHIXP1        SUB       EQIXP1        EHJXP1
     C     EHIXP2        SUB       EQIXP2        EHJXP2
     C     EHIXP3        SUB       EQIXP3        EHJXP3
     C     EHIIN1        SUB       EQIIN1        EHJIN1
     C     EHIIN2        SUB       EQIIN2        EHJIN2
     C     EHIDCT        SUB       EQIDCT        EHJDCT
     C     EHIDD2        SUB       EQIDD2        EHJDD2
     C     EHIDCM        SUB       EQIDCM        EHJDCM
     C     EHIDCS        SUB       EQIDCS        EHJDCS
     C     EHIDCR        SUB       EQIDCR        EHJDCR
     C     EHIDC         SUB       EQIDC         EHJDC
     C     EHISBJ        SUB       EQISBJ        EHJSBJ
     C     EHIMPM        SUB       EQIMPM        EHJMPM
     C     EHIFRS        SUB       EQIFRS        EHJFRS
     C     EHIDV2        SUB       EQIDV2        EHJDV2
     C     EHWADL        SUB       EQWADL        EHXADL
     C     EHIADL        SUB       EQIADL        EHJADL
     C     EHWLOS        SUB       EQWLOS        EHXLOS
     C     EHILOS        SUB       EQILOS        EHJLOS
     C     EHWTER        SUB       EQWTER        EHXTER
     C     EHITER        SUB       EQITER        EHJTER
     C     EHWDTR        SUB       EQWDTR        EHXDTR
     C     EHIDTR        SUB       EQIDTR        EHJDTR
     C     EHWRP1        SUB       EQWRP1        EHXRP1
     C     EHWRP2        SUB       EQWRP2        EHXRP2
     C     EHWRP3        SUB       EQWRP3        EHXRP3
     C     EHIRP1        SUB       EQIRP1        EHJRP1
     C     EHIRP2        SUB       EQIRP2        EHJRP2
     C     EHIRP3        SUB       EQIRP3        EHJRP3
     C     EHWVLC        SUB       EQWVLC        EHXVLC
     C     EHIVLC        SUB       EQIVLC        EHJVLC
     C     EHWHXM        SUB       EQWHXM        EHXHXM
     C     EHIHXM        SUB       EQIHXM        EHJHXM
     C     EHWARC        SUB       EQWARC        EHXARC
     C     EHIARC        SUB       EQIARC        EHJARC
     C                   WRITE     PEH12
     c                   endif

     C                   endsr
