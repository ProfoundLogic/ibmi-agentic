     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE W7295R - AGENT ACCOUNT CURRENT AGING REPORT
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    This program reads the Account Current Summary file (WSASP) and*
    H*    list agents, with a balance due within the selected aging      *
    H*    period.                                                        *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W7295C                                                         *
    H*  CALLS TO:                                                        *
    H*    NONE                                                           *
    H*                                                                   *
    H*  CREATED: 06/01/99     C:33    PROJECT: 96WAD                     *
    H*  IN BASE: 07/19/99     C:33    PROJECT: WC899                     *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* 08/26/14 C:307 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
     h*  01/23/14  C:283 Support: WA089 84611                             *
     h*                  The records with Fund zero are not being         *
     h*                  processed. They should not be skipped.           *
     H*                                                                   *
    h*  01/28/10  C:249 Project: SY215                                   *
    h*            C:189 Retrieve brand Name                              *
    H*                                                                   *
    H*  11/07/07 C:136 Support: WA132 60936                              *
    H*                 Correct logic for determing if group is a         *
    H*                 test group or not.                                *
    H*                                                                   *
     H*  06/13/07  C:190 SEQ#: WA132/58590                                *
     H*                           Change file reference for main program  *
    H*                           loop & change in logic of balance SR    *
    H*                                                                   *
     H*  11/05/04  C:132 PROJECT: WA132  44359                            *
     H*                           Check for FNTEST at PROCESS subroutine  *
    H*                           for test group, bypass if is a "Y"      *
    H*                                                                   *
     H*  06/22/00  C:58  PROJECT: WC993  Add fund master, use FNTEST flag,
     H*                                  instead of Grp 999 for test.
    H*                                                                   *
     H* 03/08/00  C:33  SEQ#: WA096/22085                                 *
     H*                 PROPERLY CALCULATE PAYMENTS                       *
    H*********************************************************************
     Fsmorl     if   e           k disk
     Fsmcol     if   e           k disk
     Fwmupl     if   e           k disk
     Fwsasl1    if   e           k disk
     F***  BEG ADD  ***  06/13/07  **************************************
     Fwsasl2    if   e           k disk    rename(pas01:pas02)
     F***  END ADD  ***  06/13/07  **************************************
     Fwdabl4    if   e           k disk
     Fwmaal5    if   e           k disk
     F***  BEGIN ADD  ***  06/22/00  **************************************
     FWMFNL     IF   E           K DISK
     F***  END ADD ******  06/22/00  **************************************
     Fqsysprt   o    f  198        printer oflind(*inoa)
    D*
     D x0titl          c                   const('Agent Account Current Aging -
     D                                     Report')
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D psds          esds                  extname(szq1p)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@stdt          s              6
     D @@agt#          s              5
     D @@que           s             10
     D xxco#           s                   like(ASCO#)
     D prtdt           s              6  0
     D xxagt#          s                   like(ASAGT#)
     D xxque           s                   like(@@que)
     D stmt_dt         s                   like(ASSTDT)
     D stdt_0_30       s                   like(ASSTDT)
     D stdt_31_60      s                   like(ASSTDT)
     D stdt_61_90      s                   like(ASSTDT)
     D stdt_91         s                   like(ASSTDT)
     D stdt_sub        s                   like(ASSTDT)
     D stdt_mth        s              2  0
     D prt_0_30        s                   like(prtdt)
     D prt_31_60       s                   like(prtdt)
     D prt_61_90       s                   like(prtdt)
     D prt_91          s                   like(prtdt)
     D ##data          s             80
     D ##length        s              3  0
     D x               s              3  0
     D xxprt           s                   like(ORFIL#)
     D bucket_bal      s             13  2
     D agt_0_30        s             13  2
     D agt_31_60       s             13  2
     D agt_61_90       s             13  2
     D agt_91          s             13  2
     D agt_total       s             13  2
     D ttl_0_30        s             15  2
     D ttl_31_60       s             15  2
     D ttl_61_90       s             15  2
     D ttl_91          s             15  2
     D total           s             15  2
     D valid           s                   like(*in60)
     D one_agent       s                   like(*in71)
     D one_record      s                   like(*in60) inz('0')
     D sav_group       s                   like(AAFND)
      *** beg add ***** 11/07/07 *******************************************
     D lengthTestFund  c                   999
     D testFund        s              1    dim(lengthTestFund)
      *** end add ***** 11/07/07 *******************************************
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C/copy SCOPYDTPL
     C*
      ***** BEG ADD ***** 11/05/04 ******************************************
      * Group Master Key
     C     fnky2         klist
     C                   kfld                    xxco#
     C                   kfld                    abfnd
      ***** END ADD ***** 11/05/04 ******************************************
     C*
     C     ky            klist
     C                   kfld                    xxco#
     C                   kfld                    xxagt#
     C*
     C     ky_0_30       klist
     C                   kfld                    xxco#
     C                   kfld                    xxagt#
     C                   kfld                    stdt_0_30
     C*
     C     ky_31_60      klist
     C                   kfld                    xxco#
     C                   kfld                    xxagt#
     C                   kfld                    stdt_31_60
     C*
     C     ky_61_90      klist
     C                   kfld                    xxco#
     C                   kfld                    xxagt#
     C                   kfld                    stdt_61_90
     C*
     C     ky_91         klist
     C                   kfld                    xxco#
     C                   kfld                    xxagt#
     C                   kfld                    stdt_91
     C*
     C     orky          klist
     C                   kfld                    Q1PGM
     C                   kfld                    xxprt
    C*
     C     aaky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxagt#
     C***  BEGIN ADD  ***  06/22/00  **************************************
     C*  FUND MASTER FILE
     C*
     C     FNKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    AAFND
     C***  END ADD ******  06/22/00  **************************************
    C*____________________________________________________________________
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@stdt
     C                   parm                    @@agt#
     C                   parm                    @@que
    C*____________________________________________________________________
     C*
     C*** beg add ***** 11/07/07 ****************************************
     c                   read      pfn01
     c                   dow       not(%eof)
     c                   if        FNTEST = 'Y' and
     c                             FNFND <> 0
     c                   eval      testFund(FNFND) = 'Y'
     c                   endif
     c                   read      pfn01
     c                   enddo
     C*** end add ***** 11/07/07 ****************************************
     C*
     C                   if        one_agent = *on
     C***  BEG CHG  ***  06/13/07  **************************************
     C**** ky*****       chain     pas01                              60
     C     ky            chain     pas02                              60
     C***  END CHG  ***  06/13/07  **************************************
     C                   else
     F***  BEG CHG  ***  06/13/07  **************************************
     C*****xxco#**       chain     pas01                              60
     C     xxco#         chain     pas02                              60
     F***  END CHG  ***  06/13/07  **************************************
     C                   endif
     C*
     C                   dow       *in60 = *off
     C*
     C                   eval      xxagt# = ASAGT#
     C*
     C                   exsr      valid_agt
     C*
     C                   if        valid = *on
     C                   exsr      process
     C                   endif
     C*
     C                   if        one_agent = *on
     C                   eval      *in60 = *on
     C                   else
     F***  BEG CHG  ***  06/13/07  **************************************
     C*****ky*****       setgt     pas01
     C*****xxco#**       reade     pas01                                  60
     C     ky            setgt     pas02
     C     xxco#         reade     pas02                                  60
     F***  END CHG  ***  06/13/07  **************************************
     C                   endif
     C*
     C                   enddo
     C*
     C                   if        *inoa = *on
     C                   except    head
     C                   endif
     C*
     C                   if        one_record = *off
     C                   except    no_records
     C                   else
     C                   except    totals
     C                   endif
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   move      *on           *inlr
    C*____________________________________________________________________
    C*
     C*====================================================================
     C* *INZSR  - Initialization subroutine
     C*====================================================================
    C*
     C     *INZSR        begsr
    C*
     C                   time                    dsdttm
    C*
     C                   move      @@co#         xxco#
     C                   move      @@stdt        prtdt
    C*
     C                   move(p)   @@stdt        srdti
     C                   call      'SRCVTMY'     dtcymd
     C                   movel     srdto         stmt_dt
    C*
     C                   move      @@agt#        xxagt#
     C*
     C                   if        Q1PRMS >= 4
     C                   eval      xxque = @@que
     C                   else
     C                   clear                   xxque
     C                   endif
    C*
     C                   eval      *in09 = xxque <> ' '
     C                   eval      *in08 = xxque = ' '
     C                   eval      one_agent = xxagt# <> *zero
     C                   eval      *in71 = one_agent
     C*
     C*  CHAIN TO THE USER PARAMETER FILE
     C*
     C     q1user        chain     pup01                              60
     C*
     C*  CHAIN TO THE COMPANY FILE
     C*
     C     xxco#         chain     pco01                              60
     C*
     C                   if        *in60 = *on
     C                   clear                   CONAME
     C                   endif
     C*
    C*  CENTER COMPANY NAME IN FIELD
     C*
     C                   movel(p)  CONAME        ##data
     C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##length
     C*
     C                   movel     ##data        CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   eval      xxprt = '0'
     C*
     C     orky          chain     por01                              60
     C*
     C                   if        ORTITL <> *blanks
    C*
    C*  LEFT JUSTIFY REPORT TITLE
    C*
     C     ' '           check     ORTITL        x
    C*
     C                   if        x <> 0
     C                   subst(p)  ORTITL:x      ORTITL
     C                   endif
    C*
     C                   else
    C*
     C                   movel(p)  x0titl        ORTITL
    C*
     C                   endif
    C*
     C                   if        *in60 = *off
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
    C*
     C                   movel(p)  ORTITL        ##data
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      60            ##length
    C*
     C                   movel(p)  ##data        ORTITL
     C*
     C                   exsr      CALC_STDT
     C*
     C                   except    head
     C*
     C                   endsr
    C*
     C*====================================================================
     C* AGT_NAME - Get agent name
     C*====================================================================
    C*
     C     AGT_NAME      begsr
     C*
     C                   clear                   AANAME
     C*
     C     aaky          chain     paa01                              60
     C*
     C                   dow       *in60 = *off
     C*
     C***  BEGIN DLT  ***  06/22/00  **************************************
     C*
     C****************   if        AASTAT <> 'I'
     C****************               and AAFND <> 999
     C***  END DELETE ***  06/22/00  **************************************
     C***  BEGIN ADD  ***  06/22/00  **************************************
     C                   if        sav_group <> AAFND
     C     FNKY          chain     PFN01                              67
     C                   if        *in67 = *ON
     C                   eval      FNTEST = 'Y'
     C                   endif
     C                   z-add     AAFND         sav_group
     C                   endif
     C*
     C                   if        AASTAT <> 'I' and
     C                             FNTEST <> 'Y'
     C***  END ADD ******  06/22/00  **************************************
     C                   leave
     C                   endif                                              999
     C*
     C     aaky          reade     paa01                                  60
     C*
     C                   enddo
     C*
     C                   endsr
    C*
     C*====================================================================
     C* BALANCE - Calculate the balance
     C*====================================================================
    C*
     C     BALANCE       begsr
     C*
     C                   select
     C                   when      ABARTY = 'P'
     C***  BEGIN CHG  ***  03/08/00  **************************************
     C**********                    or ABARTY = 'V'
     C**********         eval      bucket_bal = bucket_bal - ABNET
     C                              or ABARTY = 'I'
     C                              or ABARTY = 'Q'
     C***  BEGIN ADD  ***  06/13/07  **************************************
     C                              or ABARTY = 'T'
     C***  END   ADD  ***  06/13/07  **************************************
     C                   eval      bucket_bal = bucket_bal - ABAMT
     C***  END   CHG  ***  03/08/00  **************************************
     C***  BEGIN ADD  ***  03/08/00  **************************************
     C                   when      ABARTY = 'V'
     C                              or ABARTY = 'R'
     C                   eval      bucket_bal = bucket_bal + ABAMT
     C***  END   ADD  ***  03/08/00  **************************************
     C                   other
     C                   eval      bucket_bal = bucket_bal + ABNET
     C                   endsl
     C*
     C                   endsr
    C*
     C*====================================================================
     C* CALC_STDT - Calculate the statement dates needed for buckets
     C*====================================================================
    C*
     C     CALC_STDT     begsr
     C*
     C                   eval      stdt_0_30  = stmt_dt
     C*
     C                   eval      stdt_sub   = stmt_dt
     C                   exsr      SUB_MONTH
     C                   eval      stdt_31_60 = stdt_sub
     C*
     C                   exsr      SUB_MONTH
     C                   eval      stdt_61_90 = stdt_sub
     C*
     C                   exsr      SUB_MONTH
     C                   eval      stdt_91    = stdt_sub
    C*
     C                   move(p)   stdt_0_30     srdti
     C                   call      'SRCVTMY'     dtmdyy
     C                   movel     srdto         prt_0_30
    C*
     C                   move(p)   stdt_31_60    srdti
     C                   call      'SRCVTMY'     dtmdyy
     C                   movel     srdto         prt_31_60
    C*
     C                   move(p)   stdt_61_90    srdti
     C                   call      'SRCVTMY'     dtmdyy
     C                   movel     srdto         prt_61_90
    C*
     C                   move(p)   stdt_91       srdti
     C                   call      'SRCVTMY'     dtmdyy
     C                   movel     srdto         prt_91
     C*
     C                   endsr
    C*
     C*====================================================================
     C* PROCESS - Process valid agents
     C*====================================================================
    C*
     C     PROCESS       begsr
     C*____________________________________________________________________
     C*
     C*  Calculate 0 - 30 days bucket
     C*
     C                   eval      bucket_bal = 0
     C*
     C     ky_0_30       chain     pab01                              60
      *
      ***** BEG ADD ***** 11/05/04 ******************************************
      *** beg del ***** 11/07/07 ********************************************
     C* Check for Test Group, bypass if Flag FNTEST = "Y"
     C*****FNKY2         chain     PFN01                              67
     C**********         if        *in67 = *ON
     C**********         eval      FNTEST = 'Y'
     C**********         endif
     C**********         If        FNTEST = 'Y'
     C**********         goto      Process_end
     C**********         endif                                              999
      *** end del ***** 11/07/07 ********************************************
      ***** END ADD ***** 11/05/04 ******************************************
     C*
     C                   dow       *in60 = *off
     C*
      *** beg chg ***** 11/07/07 ********************************************
     C***********        exsr      BALANCE
      *** beg chg ***** 01/23/14 ********************************************
     c**************     if        ABFND <> 0 and
     c**************               testFund(ABFND) <> 'Y'
     c                   if        ABFND = 0 or
     c                             testFund(ABFND) <> 'Y'
      *** end chg ***** 01/23/14 ********************************************
     c                   exsr      BALANCE
     c                   endif
      *** end chg ***** 11/07/07 ********************************************
     C*
      *
     C     ky_0_30       reade     pab01                                  60
     C*
     C                   enddo
     C*
     C                   eval      agt_0_30 = bucket_bal
     C*
     C*  Calculate 31 - 60 days bucket
     C*
     C                   eval      bucket_bal = 0
     C*
     C     ky_31_60      chain     pab01                              60
     C*
     C                   dow       *in60 = *off
     C*
      *** beg chg ***** 11/07/07 ********************************************
     C***********        exsr      BALANCE
      *** beg chg ***** 01/23/14 ********************************************
     c***************    if        ABFND <> 0 and
     c***************              testFund(ABFND) <> 'Y'
     c                   if        ABFND = 0 or
     c                             testFund(ABFND) <> 'Y'
      *** end chg ***** 01/23/14 ********************************************
     c                   exsr      BALANCE
     c                   endif
      *** end chg ***** 11/07/07 ********************************************
     C*
     C     ky_31_60      reade     pab01                                  60
     C*
     C                   enddo
     C*
     C                   eval      agt_31_60 = bucket_bal
     C*
     C*  Calculate 61 - 90 days bucket
     C*
     C                   eval      bucket_bal = 0
     C*
     C     ky_61_90      chain     pab01                              60
     C*
     C                   dow       *in60 = *off
     C*
      *** beg chg ***** 11/07/07 ********************************************
     C***********        exsr      BALANCE
      *** beg chg ***** 01/23/14 ********************************************
     c***************    if        ABFND <> 0 and
     c***************              testFund(ABFND) <> 'Y'
     c                   if        ABFND = 0 or
     c                             testFund(ABFND) <> 'Y'
      *** end chg ***** 01/23/14 ********************************************
     c                   exsr      BALANCE
     c                   endif
      *** end chg ***** 11/07/07 ********************************************
     C*
     C     ky_61_90      reade     pab01                                  60
     C*
     C                   enddo
     C*
     C                   eval      agt_61_90 = bucket_bal
     C*
     C*  Calculate 91+ days bucket
     C*
     C                   eval      bucket_bal = 0
     C*
     C     ky_91         setgt     pab01
     C     ky            readpe    pab01                                  60
     C*
     C                   dow       *in60 = *off
     C*
      *** beg chg ***** 11/07/07 ********************************************
     C***********        exsr      BALANCE
      *** beg chg ***** 01/23/14 ********************************************
     c**************     if        ABFND <> 0 and
     c**************               testFund(ABFND) <> 'Y'
     c                   if        ABFND = 0 or
     c                             testFund(ABFND) <> 'Y'
      *** end chg ***** 01/23/14 ********************************************
     c                   exsr      BALANCE
     c                   endif
      *** end chg ***** 11/07/07 ********************************************
     C*
     C     ky            readpe    pab01                                  60
     C*
     C                   enddo
     C*
     C                   eval      agt_91 = bucket_bal
     C*____________________________________________________________________
     C*
     C                   exsr      AGT_NAME
     C*
     C                   eval      agt_total = agt_0_30 + agt_31_60
     C                               + agt_61_90 + agt_91
     C*
     C                   if        *inoa = *on
     C                   except    head
     C                   eval      *inoa = *off
     C                   endif
     C*
     C                   except    agt_totals
     C*
     C                   eval      ttl_0_30    = ttl_0_30    + agt_0_30
     C                   eval      ttl_31_60   = ttl_31_60   + agt_31_60
     C                   eval      ttl_61_90   = ttl_61_90   + agt_61_90
     C                   eval      ttl_91      = ttl_91      + agt_91
     C                   eval      total       = total       + agt_total
     C*
      ***** BEG ADD ***** 11/05/04 ******************************************
      *** beg del ***** 11/07/07 ********************************************
     C*****Process_End   Tag                                                999
      *** end del ***** 11/07/07 ********************************************
      ***** END ADD ***** 11/05/04 ******************************************
     C*
     C                   endsr
    C*
     C*====================================================================
     C* SUB_MONTH - Sutract one month for buckets
     C*====================================================================
    C*
     C     SUB_MONTH     begsr
     C*
     C                   move      stdt_sub      stdt_mth
     C*
     C                   if        stdt_mth = 1
     C                   eval      stdt_sub = stdt_sub - 89
     C                   else
     C                   eval      stdt_sub = stdt_sub - 1
     C                   endif
     C*
     C                   endsr
    C*
     C*====================================================================
     C* VALID_AGT - Determine if the agent should be printed
     C*====================================================================
    C*
     C     VALID_AGT     begsr
     C*
     C                   eval      valid = *off
     C*
     C     ky_0_30       chain     pas01                              60
     C*
     C                   if        *in60 = *off
     C                               and ASBAL <> *zero
     C                   eval      valid = *on
     C                   endif
     C*
     C     ky_31_60      chain     pas01                              60
     C*
     C                   if        *in60 = *off
     C                               and ASBAL <> *zero
     C                   eval      valid = *on
     C                   endif
     C*
     C     ky_61_90      chain     pas01                              60
     C*
     C                   if        *in60 = *off
     C                               and ASBAL <> *zero
     C                   eval      valid = *on
     C                   endif
     C*
     C     ky_91         setgt     pas01
     C     ky            readpe    pas01                                  60
     C*
     C                   dow       *in60 = *off
     C*
     C                   if        ASBAL <> *zero
     C                   eval      valid = *on
     C                   endif
     C*
     C     ky            readpe    pas01                                  60
     C*
     C                   enddo
     C*
     C                   if        valid = *on
     C                   eval      one_record = *on
     C                   endif
     C*
     C                   endsr
     C*___________________________________________________________________
     C*
     O/copy SCOPYROUT
     O          e            route          2
     O                                           32 'Company . . . . . . . . . '
     O                                         +000 '. . :'
     O                       xxco#         z   +002
     O          e            route          1
     O                                           36 '30, 60, 90, 90+ aging sta+
     O                                              rting'
     O          e            route          2
     O                                           32 '  with statment date  . . '
     O                                         +000 '. . :'
     O                       prtdt             +002 '  /    '
     O          e            route          2
     O                                           31 'Agent # . . . . . . . . . '
     O                                         +000 '. . :'
     O               71      xxagt#        z   +002
     O              N71                          42 '*All'
     O*** BEG CHG *****  01/28/10  ************************************
     O***       e            route          1 60
     O***                                           '(C) Copyright Progressive'
     O***                                      +001 'Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
     O*___________________________________________________________________
     O*
     O          e            head           1 06
     O                       dstime              08 '  :  :  '
     O                       CONAME             118
     O                       dsdate             196 '  /  /    '
     O          e            head           2
     O                       Q1PGM               10
     O                       ORTITL             128
     O                                          191 'PAGE:'
     O                       page          z    196
     O          e            head           1
     O                       prt_0_30            63 '  /    '
     O                       prt_31_60           86 '  /    '
     O                       prt_61_90          109 '  /    '
     O                       prt_91             126 '  /    '
     O                                         +001 'and prior'
     O          e            head           2
     O                                              'Agent'
     O                                           65 '0 - 30 days'
     O                                           88 '31 - 60 days'
     O                                          111 '61 - 90 days'
     O                                          134 'over 90 days'
     O                                          156 'Total'
     O          e            agt_totals     1
     O                       xxagt#        z
     O                       AANAME            +001
     O                       agt_0_30      j     68
     O                       agt_31_60     j     91
     O                       agt_61_90     j    114
     O                       agt_91        j    137
     O                       agt_total     j    160
     O          e            totals      1  0
     O                                           46 'Totals:'
     O                       ttl_0_30      j     68
     O                       ttl_31_60     j     91
     O                       ttl_61_90     j    114
     O                       ttl_91        j    137
     O                       total         j    160
     O          e            no_records  1  0
     O                                              '* * * No records meet'
     O                                         +001 'selected criteria * * *'
