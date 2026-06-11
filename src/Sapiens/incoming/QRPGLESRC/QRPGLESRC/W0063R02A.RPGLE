     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/TITLE W0063R02 - Fee Information
     H*********************************************************************
     H*  CREATED: 01/27/09  PROGRAMMER: C:190 PROJECT: WC1851             *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 02/11/11  C:33  Project: WC1851                                   *
     H*                 Re-write.  No comments.                           *
     H*                                                                   *
     H* 08/11/10  C:61  Project: WC1851  priss 63
     H*               - Do not allow fee entry if no A/R codes specified.
     H*                 (WMEXP: EXCFBA, EXCOBA).                          *
     H*               - Do not copy WDCFP record if no A/R Codes
     H*                 specified in destination group.
     H*                                                                   *
     H* 05/26/10  C:61  Project: WC1851                                   *
     H*                 Fix.  No comments.
     H*                                                                   *
     H* 02/24/09  C:33  Project: WC1851   25WQL Priss# 8                  *
     H*                 Re-write.  No comments.                           *
     H*********************************************************************
     Fw0063f02a cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtfip     uf a e             disk    rename(pfi01:dbrcdf)
     Fwtfil     if   e           k disk    rename(pfi01:pfi00) infds(fifds)
     Fwtfil2    if   e           k disk    rename(pfi01:pfi02) infds(fifds1)
     Fststl3    if   e           k disk
     Fwtftp     if   e             disk    rename(pft01:pft0p)
     Fwtftl     if   e           k disk    rename(pft01:pft00)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     Fwdf1l1    if   e           k disk
     Fwmfnl     if   e           k disk
     Fwdfcl2    if   e           k disk    rename(pfc01:pfc02)


    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D comapi          c                   'WA001R'
     D #ofScr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D optNm           s             40
     D xxTbId          s                   like(GTTBID) inz('FEES')
     D xxgroup         s                   like(FIFND)
     D xxFnd           s                   like(FIFND)
     D xxst            s                   like(FIST)
     D x7efdt          s                   like(FIEFDT)
     D kkefdt          s                   like(FIEFDT)
     D xxRRN           s                   like(firrn)
     d addMode         s              1n
     d cpyMode         s              1n
     d dspMode         s              1n
     d updMode         s              1n
     d cpyModeMass     s              1n
     d singleCopy      s              1n
     d Yes_to_all      s              1n
     d xxwrite         s              1a
     D ##FR_grp        s              3s 0
     D ##FR_st         s              2a
     D ##FR_efdt       s              7s 0
     D ##TO_grp        s              3s 0
     D ##TO_st         s              2a
     D ##TO_efdt       s              7s 0

     D fids01        e ds                  extname(szq2p)
     D fifds           ds
     D firrn                 397    400b 0
     D fifds1          ds
     D firrn1                397    400b 0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DNextPgm          pr             1n
     D inKc                           1n
     D inKl                           1n
      *
     DCopyRecord       pr
     D FRgrp                          3s 0
     D FRstate                        2a
     D FRefdt                         7s 0
     D TOgrp                          3s 0
     D TOstate                        2a
     D TOefdt                         7s 0
     D*
     DW0063R10         pr                  extPgm('W0063R10')
     D outKc                          1n
     D outKl                          1n
     D outMode                        1
     D outRRN                         9  0
     D*
     DW0063R11         pr                  extPgm('W0063R11')
     D outKc                          1n
     D outKl                          1n
     D outMode                        1
     D outRRN                         9  0
     D*
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrGroup
    C*
     C/copy sCopyMT41c
    C*
      *

    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     nextPgmPl     plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##Mode
     C                   parm                    ##rrn


    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     exky          klist
     C                   kfld                    ##co#
     C                   kfld                    scFnd

     C     kkey3         klist
     C                   kfld                    xxKey            10

     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    scNewGrp

     C     fnky1         klist
     C                   kfld                    ##co#

     C     fnky3         klist
     C                   kfld                    ##co#
     C                   kfld                    scFnd

      * KEY LIST FOR THE GENERIC TABLE FILE

     C     keyWTGTL      klist
     c                   kfld                    ##co#
     c                   kfld                    xxTbId
     c                   kfld                    xxKey
     c*
     c     key1WTFIL     klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scType
     c                   kfld                    scDesc
     c                   kfld                    scst
     c                   kfld                    x7EfDt

     c     WTFIkey       klist
     c                   kfld                    scCo#
     c                   kfld                    scoldgrp

     c     keyWTFIL      klist
     c                   kfld                    scCo#
     c                   kfld                    FIFND
     c                   kfld                    scType
     c                   kfld                    scDesc
     c                   kfld                    FIST
     c                   kfld                    FIEFDT

     c     keyWDF1L1     klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    x7EfDt
     C*
     c     key1WDF1L1    klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd

      *
    C*====================================================================
    C* CopyRoutine - Process copy(s)
    C*====================================================================
     C     CopyRoutine   begsr
      * Mass Copy
     c                   if        @@mode = 'M'
     c                   eval      Yes_to_all = *off
     c                   eval      *in50 = *off
     c                   eval      singleCopy = *off
     c                   exsr      ProcCopy
     c                   eval      *inlr = '1'
     c                   return
     c                   endif                                                  @@mode = 'M'

      * Single Copy
     c                   if        @@Mode = 'P'
     c                   eval      Yes_to_all = *off
     c                   eval      *in50 = *on
     c                   eval      singleCopy = *on
     c     @@rrn         chain     dbrcdf
     c                   eval      scoldgrp  = FIFND
     c                   eval      scoldst   = FIST
     c                   eval      scoldefdt = DtMDYY(FIEFDT)
     c                   exsr      ProcCopy
     c                   eval      *inlr = '1'
     c                   return
     c                   endif
     C*
     C* * *@@rrn         chain     dbRcdF
     C*
     C* * *              eval      scFromGrp = FTFND
     C*
     C* * *              exsr      CopyWindow
     C*
     C* * *              if        *inkl
     C* * *              leavesr
     C* * *              endif
     C*
     C                   select
     C* Mass copy
     C                   when      @@Mode = 'M'
     C* * *              callp     MassCopyFee(scFromGrp:scToGrp)
     C* Single copy
     C                   when      @@Mode = 'P'
     C* * *              callp     CopyFee(FTTYPE:FTDESC:scFromGrp:scToGrp)
     C*
     C                   endsl
     C*
     C                   endsr
     C*

      *===============================================================*
      * ProcCopy  - Process copy
      *===============================================================*
     C     ProcCopy      begsr

     c                   exsr      ProcessCpyWdw                                Disp Mass cpy scr

     c                   if        *in90 or *inkl
     c                   eval      *inkl = *off
     c                   leavesr
     c                   endif

     c                   eval      ##FR_grp = scoldgrp
     c     wtfikey       SetLL     pfi00
     c     wtfikey       ReadE     pfi00

     c                   DoW       NOT %eof

     c                   if        (scoldst = *blanks)
     c                             or (FIST = scoldst)
     c                   eval      ##FR_st  = FIST

     c                   if        (scoldefdt = 0)
     c                             or (FIEFDT = DtCYMD(scoldefdt))
     c                   eval      ##FR_efdt = FIEFDT
     c     fnky1         setll     pfn01
     c     fnky1         reade     pfn01

     c                   doW       not(%eof)

     c                   if        (scnewgrp = 0)
     c                             or (fnfnd = scnewgrp)
     c                   eval      ##TO_grp = fnfnd

     c                   if        (scnewst <> *blanks)
     c                   eval      ##TO_st = scnewst
     c                   else
     c                   eval      ##TO_st = FIST
     c                   endif

     c                   if        (scnewefdt <> 0)
     c                   eval      ##TO_efdt = DtCYMD(scnewefdt)
     c                   else
     c                   eval      ##TO_efdt = FIEFDT
     c                   endif

     c                   callp     CopyRecord(##FR_grp :
     c                                        ##FR_st :
     c                                        ##FR_efdt :
     c                                        ##TO_grp :
     c                                        ##TO_st :
     c                                        ##TO_efdt)

     c                   if        *inkl
     c                   eval      *inkl = *off
     c                   leavesr
     c                   endif

     c                   endif

     c     fnky1         reade     pfn01
     c                   enddo

     c                   endif

     c                   endif

     c     wtfikey       ReadE     pfi00
     c                   Enddo

     C                   endsr


    C*================================================================
    C* Delete record so it can be re-written, if needed
    C*================================================================
     C     DeleteR       begsr

     C     xxRRN         delete    dbrcdf                             99

     C                   endsr


    C*================================================================
    C* DltPre - Pre-processing before delete occurs
    C*================================================================
     C     DltPre        begsr
     C                   endsr


    C*================================================================
    C* DsplyR - Processing after displaying information
    C*================================================================
     C     DsplyR        begsr

     C                   exsr      NextScreen

     C                   endsr


    C*================================================================
    C* ErrCk1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     ErrCk1        begsr

     C                   MOVEA     '00000000'    *IN(25)
     C                   MOVEA     '0000000'     *IN(33)


     c*  IF ADD mode or COPY mode

     c                   if        addMode or cpyMode

     c*  To Group validation, only in copy mode
     c                   if        cpyMode and scFnd <> *zero
     c                              and ValidGrp(scFnd) = *off
     c                   seton                                        89  90
     c                   leavesr
     c                   endif

     c*  Populate Group name.
     c                   if        scFnd  <> *zeros
     c                   eval      scFNam = GetGrpName(scFnd)
     c                   endif

     C*  State must be valid
     c                   eval      xxKey = scSt
     c     kkey3         chain     pst01                              60
     c                   if        not %found
     c                   seton                                        81  90
     c                   leavesr
     c                   endif

     c*  Effective date must be valid.
     c                   if        DtCheck(scEfDt) = *zero
     c                   seton                                        82  90
     c                   leavesr
     c                   endif                                                  (scst <> *blanks)
     c                   eval      scEfDt = DtCheck(scEfDt)

     C*  Group Policy Year - must exist
     c                   eval      x7EfDt = DtCYMD(scEfDt)
     c     keyWDF1L1     setgt     pf101
     c     key1WDF1L1    readpe    pf101
     c                   if        not %found
     c                   seton                                        85  90
     c                   leavesr
     c                   endif

     c*  Record must not already exist for  ST/EFDT
     c     key1WTFIL     chain     pfi00                              60
     c                   if        %found
     c                   seton                                        86  90
     c                   leavesr
     c                   endif

     c                   endif                                                  Add or Copy

     c*  End of "Add mode" or "Copy mode"


     c*  Expiration date must be valid, if entered.
     c                   if        (scexdt <> *zeros)
     c                              and DtCheck(scExDt) = *zero
     c                   seton                                        83  90
     c                   leavesr
     c                   endif
     c                   eval      scExDt = DtCheck(scExDt)

     C*  Dates must be ascending order
      *
     c                   if        CvtToDate(scExDt) <= CvtToDate(scEfDt)
     c                              and scExDt <> *zeros
     c                   seton                                        84  90
     c                   leavesr
     c                   endif                                                  xxdat2 <> *zeros

     C                   endsr


    C*================================================================
    C* ErrCk2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     c     ErrCk2        begsr
     C                   endsr


      *================================================================
      * ErrCk3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ErrCk3        begsr
     C                   endsr


      *===============================================================*
      * Error checking for copy window
      *===============================================================*
     c     ErrChkWdwCpy  begsr

     c** If FROM State is blanks, then TO State must be blanks.             y
     c                   if        (scoldst = *blanks)
     c                             and (scnewst <> *blanks)
     C                   seton                                        7190
     c                   leavesr
     c                   endif

     c** If FROM state entered, then TO state must be entered.              y
     c                   if        (scoldst <> *blanks)
     c                             and (scnewst = *blanks)
     C                   seton                                        7290
     c                   leavesr
     c                   endif

     c** If FROM eff date left blank, then TO eff date must be blank.       y
     c                   if        (scoldefdt = *zero)
     c                             and (scnewefdt <> *zero)
     C                   seton                                        7390
     c                   leavesr
     c                   endif

     c** From Group Fee state.
     c                   if        not(singlecopy) and (scoldst <> *blanks)
     c                   eval      xxKey = scOldSt
     c     kkey3         chain     pst01                              60
     c                   if        not %found
     c                   seton                                        7590
     c                   leavesr
     c                   endif
     c                   endif

     c** From Group Effective date
     c                   if        not(singlecopy) and (scoldefdt <> *zeros)
     c                              and DtCheck(scoldefdt) = *zeros
     C                   seton                                        7690
     c                   leavesr
     c                   endif                                                  scoldefdt <> *zeros

     c** To Group validation
     c                   if        scNewGrp <> *zeros
     c                              and ValidGrp(scNewGrp) = *off
     c                   seton                                        7790
     c                   leavesr
     c                   endif

     c** To Group New Fee state
     c                   if        (scNewSt <> *blanks)
     c                   eval      xxKey = scNewSt
     c     kkey3         chain     pst01                              60
     C                   if        not %found
     c                   seton                                        7890
     c                   leavesr
     c                   endif
     c                   endif

     c** To Group New Effective date
     c                   if        (scNewEfDt <> *zeros)
     c                              and DtCheck(scNewEfDt) = *zero
     c                   seton                                        7990
     c                   leavesr
     c                   endif

     C                   endsr


     C*===============================================================
     C* KjDo - SUBROUTINE FOR F10
     C*===============================================================
     C     KjDo          begsr

     C                   select
     C*  Group search
     C                   when      hfld = 'SCNEWGRP'
     C                   eval      xxRRN = GrpSrch(*inkc:*inkl:xxFnd:##Co#)
     C                   select
     C                   when      xxFnd <> *zero
     C                   eval      scNewGrp = xxFnd
     C                   eval      *in89 = *on
     C                   other
     C                   eval      *in88 = *on
     C                   endsl
     C*
     C                   endsl

     C                   endsr


      *================================================================
      * Process follow-on screens
      *================================================================
     C     NextScreen    begsr

     C* Do not process until last screen processed
     C                   if        %subst(q2recf:5:1) <> %editc(#ofScr:'X')
     C                   leavesr
     C                   endif

     C     keyWTFIL      chain     wtfil
     C                   eval      xxRRN = firrn
     C                   callp     NextPgm(*inkc:*inkl)
     C                   eval      *in90 = *inkl

     C                   if        addMode
     C                              and (*inkc or *inkl)
     C                   exsr      DeleteR
     C                   endif

     C                   endsr


    C*================================================================
    C* Load Maintenance Fields
    C*================================================================
     C     MaintFlds     begsr

     c                   eval      FIDT   = DateTo7(SystemDate)
     c                   eval      FITIME = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      FIUSER = q1user                              Maintaince user
     c                   eval      FIPGM  = q1pgm                               Maintaince Pgm

     C                   endsr


    C*================================================================
    C* Load Initial Maintenance Fields
    C*================================================================
     C     MaintFldsInit begsr

     C                   exsr      MaintFlds

     c                   eval      FIIDT  = FIDT                                Maintaince date
     c                   eval      FIITM  = FITIME                              Initial Time
     c                   eval      FIIUSR = FIUSER                              Initial User
     c                   eval      FIIPGM = FIPGM                               Initial pgm.

     C                   endsr


    C*================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*================================================================
     C     MvDbSc        begsr

     c* Screen 1
     c                   eval      scCo#  = FTCO#
     c                   eval      scType = FTTYPE
     c                   eval      scDesc = FTDESC
     c                   eval      scFnd  = FTFND
     c                   eval      scFNam = GetGrpName(scFnd)
     c                   eval      scSt   = FIST
     c                   eval      scEfDt = DtMDYY(FIEFDT)
     c                   eval      scExDt = DtMDYY(FIEXDT)

     C                   endsr


    C*================================================================
    C* MvScDb - Move screen fields to data base fields
    C*================================================================
     c     MvScDb        begsr

     c** Screen 1
     C                   eval      FICO#  = scCo#
     C                   eval      FITYPE = scType
     C                   eval      FIDESC = scDesc
     C                   eval      FIFND  = scFnd
     C                   eval      FIST   = scSt
     C                   eval      FIEFDT = DtCYMD(scEfDt)
     C                   eval      FIEXDT = DtCYMD(scExDt)

     C                   endsr


    C*================================================================
    C* OtherR - Processing for other modes
    C*================================================================
     C     OtherR        begsr
     C                   endsr


    C*================================================================
    C* OthFks - Processing for other functions keys
    C*================================================================
     C     OthFks        begsr
     C*
     C     *inkj         caseq     *on           KjDo                     90
     C                   endcs

     C                   endsr


    C*================================================================
    C* PgmEnd - End of program processing
    C*================================================================
     C     PgmEnd        begsr
     C                   eval      *inlr = *on
     C                   endsr


    C*================================================================
    C* PgmStr - Start of program processing
    C*================================================================
     C     PgmStr        begsr

     C                   eval      addMode = @@Mode = 'A'
     C                   eval      cpyMode = @@Mode = 'P'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      updMode = @@Mode = 'C'
     C                   eval      cpyModeMass = @@Mode = 'M'
     C*
     C                   eval      *in40 = addMode
     C                   eval      *in09 = dspMode
     C*
     C     @@rrn2        chain     WTFTP
     c                   eval      scFnd  = FTFND
     c                   eval      scFNam = GetGrpName(FTFND)
     C*
     C                   eval      optNm = %trim(FTDESC) + ' State'
     C                   exsr      GetPgT
     C*
     C                   select
     C                   when      @@Mode = 'M'
     C                              or @@Mode = 'P'
     C                   exsr      CopyRoutine
     C                   eval      *inlr = *on
     C                   return
     C                   endsl
     C*
     C                   endsr


    C*================================================================
    C* PreOth - Pre-processing for other modes
    C*================================================================
     C     PreOth        begsr
     C                   endsr


      *===============================================================*
      * Display Mass copy screen.
      *===============================================================*
     C     ProcessCpyWdw begsr

     c  n50              eval      scoldgrp =  scFnd

     C     *in90         doueq     *off
     c     *inkl         oreq      *on
     C                   eval      *in90 = *off
     c                   exfmt     WdwCpy
     c                   exsr      OthFks

     c                   if        not *in90
     c                   exsr      ErrChkWdwCpy                                 Errchk for Mass cpy
     c                   endif

     c                   enddo                                                  doweq

     c                   endsr                                                  ProcessCpyWdw

    C*================================================================
    C* UpdatR - Update data base record and after processing
    C*================================================================
     C     UpdatR        begsr

     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      MvScDb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif

     C                   exsr      NextScreen

     C                   endsr


    C*================================================================
    C* WriteR - Write data base record and after processing
    C*================================================================
     C     WriteR        begsr

     C                   clear                   dbrcdf
     C                   exsr      MvScDb
     C                   exsr      MaintFldsInit
     C                   write     dbrcdf
     C                   feod      wtfip

     C                   select
     C                   when      addMode
     C                   exsr      NextScreen
     C                   when      cpyMode
     C                   eval      outFlg = *on
     C                   endsl

     C                   endsr





     *================================================================
     * Copy Single Record
     *================================================================
     PCopyRecord       b
     DCopyRecord       pi
     D FRgrp                          3s 0
     D FRstate                        2a
     D FRefdt                         7s 0
     D TOgrp                          3s 0
     D TOstate                        2a
     D TOefdt                         7s 0

     c     fromWTFIL2    klist
     c                   kfld                    scCo#
     c                   kfld                    scType
     c                   kfld                    scDesc
     c                   kfld                    FRgrp
     c                   kfld                    FRefdt
     c                   kfld                    FRstate

     c     toWTFIL2      klist
     c                   kfld                    scCo#
     c                   kfld                    scType
     c                   kfld                    scDesc
     c                   kfld                    TOgrp
     c                   kfld                    TOefdt
     c                   kfld                    TOstate

     c     toWTFTL       klist
     c                   kfld                    scCo#
     c                   kfld                    TOgrp
     c                   kfld                    scType
     c                   kfld                    scDesc

     c                   eval      xxwrite = 'Y'

      * If FROM and TO record are same key and Mass copy
      * Then do nothing

     c                   if        (FRgrp   = TOgrp)
     c                             and (FRstate = TOstate)
     c                             and (FRefdt = TOefdt)
     c                             and not(singlecopy)
     c                   return
     c                   endif

      * Check to see Default A/R codes entered for destination Group

     c                   if        (xxwrite = 'Y')
     c     toWTFTL       chain     pft00

      * Check that Fee Type infomation is set up in destination group

     c                   if        not %found
     c                   eval      sctgrp = TOgrp
     c                   exfmt     wdw02
     c                   eval      xxwrite = 'N'
     c                   return
     c                   endif

     c                   endif

      * if TO record already exists

     c     toWTFIL2      chain     pfi02

     c                   if        %found
     c                   eval      xxwrite = 'N'

      * display information window

     c                   if        not(Yes_to_all)
     c                   eval      scyorn = *blank

     c                   dou       (scyorn = 'Y')
     c                             or (scyorn = 'A')
     c                             or (scyorn = 'N')
     c                             or (*inkl)
     c                   eval      sctgrp = TOgrp
     c                   eval      sctst  = TOstate
     c                   eval      scefd8 = DtMDYY(TOefdt)
     c                   exfmt     wdw01
     c                   enddo

      * If FROM and TO record are same key and processing single copy
      * then do nothing.

     c                   if        (FRgrp   = TOgrp)
     c                             and (FRstate = TOstate)
     c                             and (FRefdt = TOefdt)
     c                             and (singlecopy)
     c                   return
     c                   endif

      * F12 = Cancel
     c   KL              return

     c                   if        scyorn = 'A'
     c                   eval      Yes_to_All = *on
     c                   endif

     c                   endif

     c                   if        (Yes_to_all)
     c                             or (SCYORN = 'Y')
     c     firrn1        delete    dbrcdf
     c                   eval      xxwrite = 'Y'
     c                   endif

     c                   endif

      * Write new record

     c                   if        (xxwrite = 'Y')
     c     fromWTFIL2    chain     pfi02

     c                   if        %found
     c                   eval      FIFND  = TOgrp
     c                   eval      FIST   = TOstate
     c                   eval      FIEFDT = TOefdt

     c                   eval      FIDT   = DateTo7(SystemDate)
     c                   eval      FITIME = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      FIUSER = q1user                              Maintaince user
     c                   eval      FIPGM  = q1pgm                               Maintaince Pgm
     c                   eval      FIIDT  = FIDT                                Maintaince date
     c                   eval      FIITM  = FITIME                              Initial Time
     c                   eval      FIIUSR = FIUSER                              Initial User
     c                   eval      FIIPGM = FIPGM                               Initial pgm.
     c                   write     dbrcdf
     c                   endif

     c                   endif

     PCopyRecord       e





     *================================================================
     * Process follow-on screens
     *================================================================
     PNextPgm          b
     DNextPgm          pi             1n
     D F3                             1n
     D F12                            1n
     D*
     D getOut          s              1n   inz
     D inkc            s              1n
     D inkl            s              1n
     D xzTbId          s                   like(GTTBID) inz('FEES')

     c     keyWTGTL      klist
     c                   kfld                    ##co#
     c                   kfld                    xzTbId
     c                   kfld                    xxKey

     C                   eval      ##rrn  = firrn
     C                   eval      ##Mode = @@Mode

     C                   eval      xxKey = scType
     C                   clear                   GTCF01
     C     keyWTGTL      chain     pgt00
     C                   call(e)   GTCF01        nextPgmPl
     C                   if        %error
     C                   eval      *inkl = *on
     C                   endif
     C*
     C                   eval      inkc  = *inkc
     C                   eval      inkl  = *inkl
     C                   eval      *inkc = *off
     C                   eval      *inkl = *off
     C                   eval      F3    = inkc
     C                   eval      F12   = inkl

     C                   return    getOut

     *================================================================
     * Finishing processes
     *================================================================
     C     FinishNext    begsr

     C                   eval      getOut = *on

     C                   endsr

     PNextPgm          e
