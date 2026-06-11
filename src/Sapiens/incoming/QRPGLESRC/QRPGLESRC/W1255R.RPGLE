     H* (c) 2013 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
    H**************************************************************************************
    H/TITLE EFT A/R Batch Entry
    H**************************************************************************************
    H*  Description: Prompts user for info for Direct Debit Entry.                        *
    H*                                                                                    *
    H*  Called from: Menu                                                                 *
    H*                                                                                    *
    H*  Calls to:                                                                         *
    h*                                                                                    *
    h*  Created: 04/11/03      C:136     Project: WC1116                                  *
    h**************************************************************************************
    h*  Revisions:                                                                        *
     h*                                                                                    *
     h*  02/07/13 C:136 Support: WA102 81657 (WC2044)
     h*                 Update call to WA023R                                              *
     h*                                                                                    *
     h*  04/17/12 C:255 Project: WC2044                                                    *
     h*                 Replace "Direct Debit" with EFT.                                   *
     h*                 Call GetBankAcc in place of WA023R.                                *
     h*                                                                                    *
     h*  09/01/10 C:255 Project: WC1806(PRISS#77)                                          *
     h*                 Changed screen title from 'Direct Debit A/R Batch Entry'           *
     h*                 to 'EFT A/R Batch Entry'                                           *
     h*                                                                                    *
     h*  05/24/10 C:255 Project: WC1806                                                    *
     h*                 Comment files WMDCL & WMDBL. CALL API WA023R to check Direct       *
     h*                 Debit group.                                                       *
     h*                                                                                    *
     h*  03/28/08 C:65  Support: CI019                                                     *
     h*                 Prevent program from executing if ACH module is not installed.     *
     h*                                                                                    *
     h*  06/12/03 C:136 Support: WC1116                                                    *
     h*                 Check right blank of SCDATE needed.                                *
     h*                                                                                    *
    h**************************************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     fw1255f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     fwmfnl     if   e           k disk
     f*** begin del ***  05/24/10  *****************************************
     f***************wmdbl     if   e           k disk
     f***************wmdcl     if   e           k disk
     f*** end   del ***  05/24/10  *****************************************
     fwmupl     if   e           k disk
     f*** begin add ***  03/28/08  *****************************************
     fssylf     if   e           k disk
     f*** end add *****  03/28/08  *****************************************
    
     d/copy scopypr01d
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d*** begin add ***  09/01/10  *****************************************
     d***** optnm           c                   const('Direct Debit A/R Batch Entry')
     d optnm           c                   const('EFT A/R Batch Entry')
     d*** end   add ***  09/01/10  *****************************************
     d comapi          c                   const('WA001R')
     d @@call          s              1
     d @@kc            s              1
     d @@kl            s              1
     d xxcall          s                   like(@@call)
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##kc            s              1
     d ##kl            s              1
     d ##call          s              1
     d ##rrn           s              9  0
     d qqco#           s              3
     d qqfnd           s              3
     d qqrndt          s              8
     d qqoutq          s             10
     d qqkc            s              1
     d qqkl            s              1
     d qqcall          s              1
     d qqfyr           s              3
     d qqview          s              1  0
     d kkgrp           s              3  0
     d runDate         s              8  0
     d toPODT8         s              8  0
     d svgrp           s              3  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     d/copy SPRDATE
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d @@co            s              3
     d @@fnd           s              3
     d @@appl          s              2
     d @@srdt          s              8
     d @@acct          s             20
     d @@hdcy          s              1
     d @@chkq          s             10
     d @@ftyp          s             10
     d @@rtcd          s              1
     d @@efdt          s              7
     d @@cpyq          s             10
     d @@cktp          s             10
     d @@eftc          s              1
     d @@coid          s             20
      *******End Add********04/17/12**********************************
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm                    @@call
    
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     fnky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkgrp

     c     upky          klist
     c                   kfld                    Q1USER
      **** BEGIN ADD **** 03/28/08 *********************************************
      *
      * KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     c     syky          klist
     c                   kfld                    xxsyk             2
      **** END   ADD **** 03/28/08 *********************************************

     c/copy scopypr01c
    c*====================================================================
    c* DSCAPI - Call API for getting descriptions (use 91 on error)
    c*====================================================================
     c     DSCAPI        begsr
     c                   call      comapi        compl                  91
     c                   endsr                                                  DSCAPI
    c*====================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*====================================================================
     c     ERRCK1        begsr
     c                   exsr      othfks
     *
      * Validate run date
      *
     c                   eval      srdti = scdate
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scdate
     c                   if        srerr = '1'
     c                   seton                                        71  90
     c                   endif
     *
      * Validate group
      *
     c                   if        scgrp <> *zeros
      *
      * Group not valid
      *
     c                   eval      kkgrp = scgrp
     c     fnky          chain     pfn01
     c                   if        not %found or FNTEST = 'Y'
     c                   seton                                        72  90
     c                   endif                                                  not %found
      *
      * Group not set up to be a Direct Debit group
      *
     c*** begin chg ***  05/24/10  *****************************************
     c**** fnky          chain     pdc01
     c****               if        not %found
     c****               seton                                        73  90
     c****               endif                                                  not %found
     c*
     c                   Move      ##co#         @@co
     c                   Move      scgrp         @@fnd
     c                   Move      'WP'          @@appl
     c                   Move      scdate        @@srdt
     c
      *******Beg Chg********04/17/12**********************************
     c******             CALL      'WA023R'
     c******             parm                    @@co              3
     c******             parm                    @@fnd             3
     c******             parm                    @@appl            2
     c******             parm                    @@srdt            8            mmddccyy
     c******
     c******             parm      *blanks       @@acct           20
     c******             parm      *blanks       @@call            1
     c******             parm      *blanks       @@hdcy            1
     c******             parm      *blanks       @@chkq           10
     c******             parm      *blanks       @@ftyp           10
     c******             parm      *blanks       @@rtcd            1
     c******             parm      *zeros        @@efdt            7 0
     c******             parm      *blanks       @@cpyq           10
     c******             parm      'EFT'         @@cktp           10
      /Free
       @@acct = *Blanks;
       @@call = *Blanks;
       @@hdcy = *Blanks;
       @@chkq = *Blanks;
       @@ftyp = *Blanks;
       @@rtcd = *Blanks;
       @@efdt = *Zeros ;
       @@cpyq = *Zeros ;
       @@cktp = *Blanks;
       @@coid = *Blanks;
       @@eftc = 'E'    ;

       @@acct= GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                @@cktp: @@eftc: @@coid: @@rtcd);
      /End-Free
      *******End Add********04/17/12**********************************
     c*
     c                   if        @@rtcd='1'
     c                   seton                                        73  90
     c                   endif                                                  not %found
     c*** end   chg ***  05/24/10  *****************************************

     c                   endif                                                  scgrp <> *zeros

     c                   endsr                                                  ERRCK1
    c*====================================================================
    c* KJDO   - Processing for F10
    c*====================================================================
     c     KJDO          begsr

     c                   if        hfld = 'SCGRP'                               search on group
     c                   move(p)   ##co#         qqco#
     c                   call      'W0010R'
     c                   parm                    qqkc
     c                   parm                    qqkl
     c                   parm      '2'           qqcall
     c                   parm                    qqco#
     c                   parm      *zero         qqfnd
     c                   parm                    qqfyr
     c                   parm      1             qqview

     c                   if        qqfnd <> *zero
     c                   move(p)   qqfnd         scgrp
     c                   else
     c                   seton                                        80  90
     c                   endif                                                  qqfnd <> 0

     c                   endif                                                  hfld = 'SCGRP'

     c                   endsr                                                  KJDO
    c*====================================================================
    c* KMDO   - Processing for F13=Change user parms
    c*====================================================================
     c     KMDO          begsr
     c                   call      'W0006R'                             99
     c                   exsr      getcof
     c                   endsr                                                  KMDO
    c*====================================================================
    c* KRDO   - Processing for F17 - Transmission History
    c*====================================================================
     c     KRDO          begsr

     c                   call      'W1257R'
     c                   parm                    ##kc
     c                   parm                    ##kl
     c                   parm      ' '           ##call
     c                   parm      *zero         ##rrn

     c                   endsr                                                  KJDO
    c*====================================================================
    c* OTHFKS - Processing for other functions keys
    c*====================================================================
     c     OTHFKS        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c     *inkm         caseq     *on           kmdo                     90
     c     *inkr         caseq     *on           krdo                     90
     c                   endcs
     c                   endsr                                                  OTHFKS
    c*====================================================================
    c* PGMEND - End of program processing
    c*====================================================================
     c     PGMEND        begsr
     c                   seton                                        lr
     c                   endsr                                                  PGMEND
    c*====================================================================
    c* PGMSTR - Used for any start of program processing (one-time)
    c*====================================================================
     c     PGMSTR        begsr
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
      **** begin add **** 03/28/08 *********************************************
     c     syky          chain     swc01                              60
     c                   if        wcfl24 <> 'Y'
     c                   exsr      pgmend
     c                   endif
      **** end add ****** 03/28/08 *********************************************
     c                   endsr                                                  PGMSTR
    c*====================================================================
    c* PREDSP - Pre-processing before displaying (every time)
    c*====================================================================
     c     PREDSP        begsr

     c                   time                    dsdttm
     c                   call      'SRPAST8'
     c                   parm      dsdate        datein            8 0
     c                   parm      1             numdays           5 0
     c                   parm                    err               1

     c                   eval      scdate = datein

     c                   endsr                                                  PREDSP
    c*====================================================================
    c* PRSCTL - Process control after no errors found (Call Submitter)
    c*====================================================================
     c     PRSCTL        begsr

     c     upky          chain     pup01
     c                   move      ##co#         qqco#
     c                   move      scgrp         qqfnd
     c                   move      scdate        qqrndt

     c                   call      'W1255C'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqrndt
     c                   parm      UPOUTQ        qqoutq

     c                   endsr                                                  PRSCTL
