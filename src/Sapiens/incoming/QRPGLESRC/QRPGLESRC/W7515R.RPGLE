     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/Title - Return Premium Check Register
      *=========================================================================
      *                                                                        *
      *  Called from: W6530C (Return Premium Batch)                            *
      *               W7514R (Return Premium Check Register Prompt)            *
      *                                                                        *
      *  Calls to   : None                                                     *
      *                                                                        *
      *  WRITTEN:  02/13/2004   C:136   PROJECT: WC1033                        *
      *=========================================================================
      * REVISIONS:                                                             *
      *                                                                        *
      * 08/05/15 C:307 Project: SY354                                          *
      *                -Adding 'Route To:'                                     *
      *                                                                        *
      * 09/01/15 C:306 Support: 35383                                          *
      *                - Added Division# column in return Premium Check        *
      *                  Register report.                                      *
      *                                                                        *
      * 08/26/14 C:307 Project: SY354                                          *
      *                -Implement Routing Page Changes.                        *
      *                                                                        *
      * 01/28/10  C:249  Project: SY215                                        *
      *           C:189  Retrieve brand Name                                   *
      *                                                                        *
      * 07/18/07 C:136 Project: WC1491                                        *
      *                Implement Ind Cash and Return Deposit                   *
      *                                                                        *
      * 08/24/05 C:136 Support: WA081 48656                                    *
      *                - Fix date range on routing. Add selection criteria to  *
      *                  routing                                               *
      *                - Do not include void or stop pays in total if running  *
      *                  the report to include all types of checks.            *
      *                - Show void or stop pay status of check.                *
      *=========================================================================
     Fsmcol     if   e           k disk
     Fwmupl     if   e           k disk
     Fsmorl     if   e           k disk
     Fwar4l7    if   e           k disk    rename(Pr401:Pr4l7)
     Fwar4l8    if   e           k disk    rename(Pr401:Pr4l8)
      *** beg chg ***** 09/01/15 *******************************************************
     F****qsysprt   o    f  138        printer oflind(*inoa)
     F****qsysprt1  o    f  138        printer oflind(*inob)
     Fqsysprt   o    f  141        printer oflind(*inoa)
     Fqsysprt1  o    f  141        printer oflind(*inob)
      *** end chg ***** 09/01/15 *******************************************************
      *=========================================================================
      * Definitions
      *=========================================================================
     D kkco#           S                   like(r4co#)
     D chkDate         S              8  0
     D totamt          S             13  2
     D totcnt          S             11  0
     D title           S             60
     D frDate7         S                   like(r4date)
     D toDate7         S                   like(r4date)
     D frDate          S              8  0
     D toDate          S              8  0
     D frGrp           S                   like(r4fnd)
     D toGrp           S                   like(r4fnd)
     D xxque           S                   like(qqque)
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
      *** beg chg ***** 09/01/15 *******************************************************
     D**** line            s            138
     D line            s            141
      *** end chg ***** 09/01/15 *******************************************************
     D ortitl1         s                   like(ortitl)
      *** beg add ***** 08/24/05 ***********************************************
     D xxptnm          s             35
     D xxtype          s              9
      *** end add ***** 08/24/05 ***********************************************
      *** beg add ***** 09/01/15 ***********************************************
     D prAcctnum       s             15    inz(' ')
      *** end add ***** 09/01/15 ***********************************************
     D psds          esds                  extname(szq1p)
     D/copy sprdate
     D/copy sprstring
     D selected        pr              n
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *=========================================================================
      * klists
      *=========================================================================
     C     coky          klist
     C                   kfld                    kkco#
      *
     C     upky          klist
     C                   kfld                    q1user
      *
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt             1
      *=========================================================================
      * plists
      *=========================================================================
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqffnd            3
     C                   parm                    qqtfnd            3
     C                   parm                    qqfdat            8
     C                   parm                    qqtdat            8
     C                   parm                    qqacct           20
     C                   parm                    qqttyp            1
     C                   parm                    qqsort            1
     C                   parm                    qqque            10
      *** beg add ***** 07/18/07 ***********************************************
     C                   parm                    qqbtyp            1
      *** end add ***** 07/18/07 ***********************************************
      *=========================================================================
      * routing
      *=========================================================================
     C     qqque         comp      *blank                             090908
      *** beg chg ***** 08/24/05 ***********************************************
     C*****qqffnd*****   comp      *blank                             6262
     C*****qqfdat*****   comp      *blank                             6363
     c                   if        qqffnd = *zeros and
     c                             qqtfnd = *zeros
     c                   move      *on           *in62
     c                   endif

     c                   if        qqfdat = *zeros and
     c                             qqtdat = *zeros
     c                   move      *on           *in63
     c                   endif

     c                   select
     c                   when      qqttyp = *blank
     c                   eval      xxtype = '*ALL     '
     c                   when      qqttyp = 'S'
     c                   eval      xxtype = 'STOP PAYS'
     c                   when      qqttyp = 'V'
     c                   eval      xxtype = 'VOIDS    '
     c                   endsl
      *** end chg ***** 08/24/05 ***********************************************

     C                   if        qqacct = *blanks
     C                   seton                                        61
     C                   endif
     C                   eval      xxque = qqque
     C     *hms          move      q1time        sys_time
     C     *usa          move      *date         sys_date
     C                   move      qqco#         kkco#
     C                   move      qqffnd        frGrp
     C                   move      qqtfnd        toGrp
     C                   move      qqfdat        frDate
     C                   move      qqtdat        toDate
     C                   move      *all'_'       line
      *
     C     coky          chain     pco01
     C                   eval      coname  = centerFld(coname)
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
     C     upky          chain     pup01
      *
     C                   if        qqsort = 'G'
     C                   move      '0'           xxprt
     C                   else
     C                   move      '1'           xxprt
     C                   endif
      *
     C     orky          chain     por01
     C                   if        %found
     C                   eval      title = centerFld(ortitl)
      *
     C                   if        qqsort = 'G'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   else
     C                   eval      ortitl1 = ortitl
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route1
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
      *
     C                   else
     C                   eval      title = 'RETURN FUND CHECK REGISTER'
     C                   eval      title = centerFld(title)
     C                   endif
      *=========================================================================
      * main
      *=========================================================================
     C                   if        qqsort = 'G'
     C                   except    head
     C                   else
     C                   eval      ortitl1 = ortitl
     C                   except    head1
     C                   endif
      *
     C                   if        qqsort= 'G'
     C     *loval        setll     pr4l8
     C                   else
     C     *loval        setll     pr4l7
     C                   endif
      *
     C                   dou       %eof
      *
     C                   if        qqsort= 'G'
     C                   read      pr4l8
     C                   else
     C                   read      pr4l7
     C                   endif
      *
      *** beg add ***** 07/18/07 ***********************************************
     c                   if        qqbtyp = *blank or
     c                             qqbtyp = R4BTYP
      *** end add ***** 07/18/07 ***********************************************
      *
     C                   if        %eof
     C                   leave
     C                   endif
      *
     C                   if        not selected
     C                   iter
     C                   endif
      *
     C                   eval      chkDate = dtMDYY(R4ckdt)
      *** beg add ***** 08/24/05 ***********************************************
      *** beg add ***** 09/01/15 ***********************************************
     C*****              eval      xxptnm = R4PTNM
     C                   eval      xxptnm = %trim(R4PTNM)
     C                   eval      prAcctnum = %trim(R4ACCT)
      *** end add ***** 09/01/15 ***********************************************
      *** end add ***** 08/24/05 ***********************************************
      *
     C                   if        qqsort= 'G'
     C                   except    detail
     C                   else
     C                   except    detail1
     C                   endif
      *
      *** beg add ***** 08/24/05 ***********************************************
     C                   if        qqttyp = *blank and
     C                             R4VDSP = *blank or
     c                             qqttyp <> *blank
      *** end add ***** 08/24/05 ***********************************************
     C                   eval      totamt = totamt + r4amt
      *** beg add ***** 08/24/05 ***********************************************
     C                   endif
      *** end add ***** 08/24/05 ***********************************************
      *
     C                   eval      totcnt = totcnt + 1
      *
     C                   if        *inoa
     C                   except    head
     C                   endif
      *
     C                   if        *inob
     C                   except    head1
     C                   endif
      *
      *** beg add ***** 07/18/07 ***********************************************
     C                   endif
      *** end add ***** 07/18/07 ***********************************************
      *
     C                   enddo
      *
     C                   if        totcnt > *zero
      *
     C                   if        qqsort= 'G'
     C                   except    total
     C                   else
     C                   except    total1
     C                   endif
      *
     C                   else
      *
     C                   if        qqsort= 'G'
     C                   except    errmsg
     C                   else
     C                   except    errmsg1
     C                   endif
      *
     C                   endif
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If QQSORT = 'G';
          xxprt = '0';
       Else;
          xxprt= '1';
       EndIf;
       Chain orky POR01;
       If %found;
          If qqsort = 'G';
             Except Route;
          Else;
             Except Route1;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   eval      *inlr = *on
      *=========================================================================
      * print for Group sort
      *=========================================================================
     O/copy scopyrout
      *
     O          E            route          2
     O                                         +  6 'BANK ACCOUNT #    :'
     O              n61      qqacct            +  1
     O               61                          31 '*ALL'
      *
     O          E            route          2
     O                                         +  6 'BATCH TYPE  . . . :'
     O                       qqbtyp            +  1
     O          E            route          2
     O                                         +  6 'GROUP RANGE . . . :'
     O              n62      frGrp         4   +  1
     O              n62                        +  1 '-'
     O              n62      toGrp         4   +  1
     O               62                          31 '*ALL'
     O          E            route          2
     O                                         +  6 'DATE RANGE  . . . :'
     O              n63      frDate        Y   +  1
     O              n63                        +  1 '-'
     O              n63      toDate        Y
     O               63                          31 '*ALL'
      *** beg add ***** 08/24/05 *******************************************************
     O          E            route          2
     O                                         +  6 'TRANSACTION TYPE  :'
     O                       xxtype            +  2
      *** end add ***** 08/24/05 *******************************************************
     O*** Beg Chg *****  01/28/10  ************************************
     O***       e            route          1 60
     O***                                        23 '(C) COPYRIGHT FISERV IN'
     O***                                        46 'SURANCE SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
     O          e            head           1 06
     O                       sys_time
     O                       coname              89
     O                       sys_date           138
     O          e            head           2
     O                       q1pgm
     O                       title               99
     O                                          132 'PAGE:'
     O                       page          Z    138
      *
     O          e            head           0
     O                       line
     O          e            head           2
     O                                            5 'GROUP'
     O                                           17 'EMPLOYER#'
      *** beg chg ***** 09/01/15 *******************************************************
     O****                                       24 'YEAR'
     O****                                       33 'AGENT'
     O****                                       47 'BANK ACCOUNT'
     O****                                       69 'PAID TO NAME'
     O                                           28 'DIVISION'
     O                                           34 'YEAR'
     O                                           43 'AGENT'
     O                                           57 'BANK ACCOUNT'
     O                                           72 'PAID TO NAME'
      *** end chg ***** 09/01/15 *******************************************************
      *** beg chg ***** 08/24/05 *******************************************************
     O***********                               112 'DATE'
     O***********                               121 'CHECK#'
      *** beg chg ***** 09/01/15 *******************************************************
     O****                                      109 'DATE'
     O****                                      118 'CHECK#'
     O                                          111 'DATE'
     O                                          121 'CHECK#'
      *** end chg ***** 09/01/15 *******************************************************
      *** end chg ***** 08/24/05 *******************************************************
      *** beg chg ***** 09/01/15 *******************************************************
     O****                                      136 'AMOUNT'
     O                                          138 'AMOUNT'
      *** end chg ***** 09/01/15 *******************************************************
     O          e            detail      1  0
     O                       r4fnd                5
     O                       r4emp#        Z     17
      *** beg chg ***** 09/01/15 *******************************************************
     O*****                  r4fyr         Z     24
     O*****                  r4agt#        Z     33
     O*****                  r4acct              55
     O                       r4div               28
     O                       r4fyr         Z     34
     O                       r4agt#        Z     43
     O                       prAcctnum           60
      *** end chg ***** 09/01/15 *******************************************************
      *** beg chg ***** 08/24/05 *******************************************************
     O***********            r4ptnm              97
     O***********            chkDate            112
     O***********            r4chk#        Z    121
      *** beg chg ***** 09/01/15 *******************************************************
     O****                   xxptnm              92
     O****                   chkDate            109 '  /  /   0'
     O****                   r4chk#        Z    118
     O****                   r4vdsp             120
     O                       xxptnm              95
     O                       chkDate            111 '  /  /   0'
     O                       r4chk#        Z    121
     O                       r4vdsp             123
      *** end chg ***** 09/01/15 *******************************************************
      *** end chg ***** 08/24/05 *******************************************************
      *** beg chg ***** 09/01/15 *******************************************************
     O*******                r4amt         J    137
     O                       r4amt         J    139
      *** end chg ***** 09/01/15 *******************************************************
     O          e            total       2  0
     O                                              'Record Count:'
     O                       totcnt        J   +  2
      *** beg chg ***** 09/01/15 *******************************************************
     O*****                  totamt        J    137
     O                       totamt        J    139
      *** end chg ***** 09/01/15 *******************************************************
      *
     O          e            errmsg      3
     O                                           68 '*** NO RECORDS MEET '
     O                                           89 'SELECTED CRITERIA ***'
      *=========================================================================
      * print for Bank Account sort
      *=========================================================================
     O/copy scopyrout1
      *
     O          E            route1         2
     O                                         +  6 'BANK ACCOUNT #    :'
     O              n61      qqacct            +  1
     O               61                          31 '*ALL'
      *
     O          E            route1         2
     O                                         +  6 'GROUP RANGE . . . :'
     O              n62      frGrp         2   +  1
     O              n62                        +  1 '-'
     O              n62      toGrp         2   +  1
     O               62                          31 '*ALL'
     O          E            route1         2
     O                                         +  6 'DATE RANGE  . . . :'
     O              n63      frDate        Y   +  1
     O              n63                        +  1 '-'
     O              n63      toDate        Y
     O               63                          31 '*ALL'
      *** beg add ***** 08/24/05 *******************************************************
     O          E            route1         2
     O                                         +  6 'TRANSACTION TYPE  :'
     O                       xxtype            +  2
      *** end add ***** 08/24/05 *******************************************************
     O*** Beg Chg *****  01/28/10  ************************************
     O****      e            route1         1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                        46 'VE DATA SOLUTIONS. INC,'
     O          e            route1         1 59
     O                       #brnd1
     O          e            route1         1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
     O          e            head1          1 06
     O                       sys_time
     O                       coname              89
     O                       sys_date           138
     O          e            head1          2
     O                       q1pgm
     O                       title               99
     O                                          132 'PAGE:'
     O                       page          Z    138
      *
     O          e            head1          0
     O                       line
     O          e            head1          2
     O                                              'BANK ACCOUNT'
     O                                           22 'GRP'
     O                                           34 'EMPLOYER#'
      *** beg chg ***** 09/01/15 *******************************************************
     O******                                     46 'YEAR'
     O******                                     55 'AGENT'
     O******                                     69 'PAID TO NAME'
     O                                           44 'DIVISION'
     O                                           51 'YEAR'
     O                                           60 'AGENT'
     O                                           74 'PAID TO NAME'
      *** end chg ***** 09/01/15 *******************************************************
      *** beg chg ***** 08/24/05 *******************************************************
     O***********                               112 'DATE'
     O***********                               121 'CHECK#'
      *** beg chg ***** 09/01/15 *******************************************************
     O********                                  109 'DATE'
     O********                                  118 'CHECK#'
     O                                          114 'DATE'
     O                                          123 'CHECK#'
      *** end chg ***** 09/01/15 *******************************************************
      *** end chg ***** 08/24/05 *******************************************************
      *** beg chg ***** 09/01/15 *******************************************************
     O********                                  136 'AMOUNT'
     O                                          138 'AMOUNT'
      *** end chg ***** 09/01/15 *******************************************************
     O          e            detail1     1  0
     O                       r4acct
     O                       r4fnd               22
     O                       r4emp#        Z     34
      *** beg chg ***** 09/01/15 *******************************************************
     O*******                r4fyr         Z     46
     O*******                r4agt#        Z     55
     O                       r4div               44
     O                       r4fyr         Z     51
     O                       r4agt#        Z     60
      *** end chg ***** 09/01/15 *******************************************************
      *** beg chg ***** 08/24/05 *******************************************************
     O***********            r4ptnm              97
     O***********            chkDate            112
     O***********            r4chk#        Z    121
      *** beg chg ***** 09/01/15 *******************************************************
     O********               xxptnm              92
     O********               chkDate            109 '  /  /   0'
     O********               r4chk#        Z    118
     O********               r4vdsp             120
     O                       xxptnm              97
     O                       chkDate            114 '  /  /   0'
     O                       r4chk#        Z    123
     O                       r4vdsp             125
      *** end chg ***** 09/01/15 *******************************************************
      *** end chg ***** 08/24/05 *******************************************************
      *** beg chg ***** 09/01/15 *******************************************************
     O********               r4amt         J    137
     O                       r4amt         J    138
      *** end chg ***** 09/01/15 *******************************************************
     O          e            total1      2  0
     O                                              'Record Count:'
     O                       totcnt        J   +  2
     O                                          119 'Total:'
      *** beg chg ***** 09/01/15 *******************************************************
     O********               totamt        J    137
     O                       totamt        J    138
      *** end chg ***** 09/01/15 *******************************************************
      *
     O          e            errmsg1     3
     O                                           68 '*** NO RECORDS MEET '
     O                                           89 'SELECTED CRITERIA ***'
     *====================================================================
     * selected  - Check record against selection criteria
     *====================================================================
     Pselected         b

     D selected        pi              n

     C                   if        frGrp <> 0 and
     C                             r4fnd < frGrp
     C                   return    *off
     C                   endif
      *
     C                   if        toGrp <> 0 and
     C                             r4fnd > toGrp
     C                   return    *off
     C                   endif
      *
     C                   if        qqacct <> *blanks and
     C                             qqacct <> r4acct
     C                   return    *off
     C                   endif
      *
     C                   eval      frDate7 = dtCYMD(frDate)
     C                   if        frDate7 <> 0 and
     C                             r4ckdt < frDate7
     C                   return    *off
     C                   endif
      *
     C                   eval      toDate7 = dtCYMD(toDate)
     C                   if        toDate7 <> 0 and
     C                             r4ckdt > toDate7
     C                   return    *off
     C                   endif
      *
     C                   if        qqttyp <> *blanks and
     C                             qqttyp <> r4vdsp
     C                   return    *off
     C                   endif
      *
     C                   return    *on

     Pselected         e
