     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/Title - Return Fund Batch Edit
      *========================================================================*
      *  W0500R18 - Return Fund Batch Edit                                     *
      *                                                                        *
      *  Called from: W0501C18 (Override - Return Premium Batch Edit)          *
      *                                                                        *
      *  Calls to   : None                                                     *
      *                                                                        *
      *  CREATED: 03/12/2003  C:136/C:142 WC1033                               *
      *========================================================================*
      * REVISIONS:                                                             *
      *                                                                        *
     F* 08/05/15 C:307 Project: SY354                                          *
     F*                -Adding 'Route To:'                                     *
     F*                                                                        *
      * 08/26/14 C:298 Project: SY354                                          *
      *                -Implement Routing Page Changes.                        *
      *                                                                        *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *                                                               *
      *  11/26/08 C:220 Project: 81WZY                                         *
      *                 Return Funds - Batch Edit Sort User Defined            *
      *                 Controls.                                              *
      *                                                                        *
      *  07/18/07 C:136 Project: WC1491                                        *
      *                 Implement Cash Dist and Return Deposit                 *
      *                                                                        *
      *  11/10/06 C:136  Support: WA109 52100                                  *
      *                  Sort by employer name instead of by date.             *
      *                                                                        *
      *  11/05/04  C:33  Project: WC1033                                       *
      *                  Include calculations for Agency Billing.              *
      *========================================================================*
     FSmcol     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FSmorl     IF   E           K Disk
     FWmfnl     IF   E           K Disk
      *** beg chg ***** 11/10/06 **********************************************
     F********** Wxrtl     IF   E           K Disk
      *** Beg Chg ***** 11/26/08 **********************************************
     F********** Wxrtl2    IF   E           K Disk
     FWxrtl2    IF   E           K Disk    USROPN
      *** End Chg ***** 11/26/08 **********************************************
      *** end chg ***** 11/10/06 **********************************************
     FWxrnl     IF   E           K Disk
      *** Beg Add ***** 11/26/08  **************************************
     FWxrtl     IF   E           K Disk    USROPN
     F                                     Rename(PRT01:PRT03)
     FWxrtl1    IF   E           K Disk    USROPN
     F                                     Rename(PRT01:PRT02)
      *** End Add ***** 11/26/08  **************************************
     FQsysprt   O    F  138        Printer OFLIND(*INOF)
      *
     D***  Begin Add  ***  11/05/04  **************************************
     D/copy sprstring
     D*
     D prtRoutePg      S              1n
     D***  End   Add  ***  11/05/04  **************************************
     D First           S              1n
     D Print           S              1n
     D***  Begin Chg  ***  11/05/04  **************************************
     D*Line*****       S            138
     D Line            S            132    inz(*all'_')
     D***  End   Chg  ***  11/05/04  **************************************
     D title           C                   'RETURN FUND BATCH EDIT'
     D Datusa          S               D   DATFMT(*USA)
     D Dtfaud          S               D   DATFMT(*USA)
     D Dttaud          S               D   DATFMT(*USA)
     D Emptot          S             13  2
     D Acrtot          S             13  2
     D Lrtot           S             13  2
      *
     D*** Beg Add ***** 11/26/08  **************************************
     D Xcorfbs         S              1a
     D*** End Add ***** 11/26/08  **************************************
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
      *  Read work file input
      *
     C     Wxrnl_Key     Chain     Wxrnl
      *
     C                   If        %Found
     C*** beg chg ***** 07/18/07 ******************************************
     C**** *Ymd********  Move      Rnfaud        Dtfaud
     C**** *Ymd********  Move      Rntaud        Dttaud
     C                   if        RNFAUD <> 0
     C     *ymd          move      RNFAUD        dtfaud
     c                   move      dtfaud        xxfaud           10
     c                   eval      *in61  = *on
     C                   endif
     C                   if        RNTAUD <> 0
     C     *ymd          move      RNTAUD        dttaud
     c                   move      dttaud        xxtaud           10
     C                   endif
     C                   if        RNFGRP = 0
     c                   eval      *in62  = *on
     c                   endif
     C                   if        RNMIN  = 0
     c                   eval      *in63  = *on
     c                   endif
     C*** end chg ***** 07/18/07 ******************************************
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         Except    Route
     C**********         Except    Head
     C                   Exsr      routing
     C***  End   Chg  ***  11/05/04  **************************************
     C***********        Exsr      Proc_Y
     C                   Exsr      Proc_N
     C***  Begin Add  ***  11/05/04  **************************************
     C                   Exsr      Proc_Y
     C***  End   Add  ***  11/05/04  **************************************
     C                   Eval      Lrtot = Emptot + Acrtot
     C                   Except    Lr_Total
      *
     C                   Endif
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Smorl_Key Smorl;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   Eval      *Inlr = *On
      *========================================================================*
      * Read the detail file for batch passed in parm with 'N' (Employer)
      *========================================================================*
      *
     C     Proc_N        Begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   Except    head
     C                   Except    heado
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   Eval      Print = *Off
      *** beg chg ***** 11/10/06 **********************************************
     C*****Wxrtl_Key     Setll     Wxrtl
     C**************     Dou       %Eof(Wxrtl)
     C*****Wxrtl_Key     Reade     Wxrtl
     C**************     If        %Eof(Wxrtl)
     C*** Beg Add ***** 11/26/08  **************************************
     C   52              Open      Wxrtl2
     C   51              Open      Wxrtl1
     C   53              Open      Wxrtl
     C   52Wxrtl_Key     Setll     Wxrtl2
     C   51Wxrtl_Key     Setll     Wxrtl1
     C   53Wxrtl_Key     Setll     Wxrtl
     C*** End Add ***** 11/26/08  **************************************
     C*** Beg Del ***** 11/26/08  **************************************
     C*****Wxrtl_Key     Setll     Wxrtl2
     C*** End Del ***** 11/26/08  **************************************
     C*** Beg Chg ***** 11/26/08  **************************************
     C***********        Dou       %Eof(Wxrtl2)
     C                   Dou       %Eof(Wxrtl2) or %Eof(Wxrtl1) or %Eof(Wxrtl)
      *
     C*****Wxrtl_Key     Reade     Wxrtl2
     C   52Wxrtl_Key     Reade     Wxrtl2
     C*** End Chg ***** 11/26/08  **************************************
      *
     C*** Beg Add ***** 11/26/08  **************************************
     C   51Wxrtl_Key     Reade     Wxrtl1
     C   53Wxrtl_Key     Reade     Wxrtl
     C*** End Add ***** 11/26/08  **************************************
      *
     C*** Beg Chg ***** 11/26/08  **************************************
     C**************     If        %Eof(Wxrtl2)
     C                   If        %Eof
     C*** End Chg ***** 11/26/08  **************************************
      *
     C*** Beg Add ***** 11/26/08  **************************************
     C   52              Close     Wxrtl2
     C   51              Close     Wxrtl1
     C   53              Close     Wxrtl
     C*** End Add ***** 11/26/08  **************************************
      *** end chg ***** 11/10/06 **********************************************
     C                   Leave
     C                   Endif
      *
     C***  Begin Chg  11/05/04  *******************************************
     C**********         If        Rtacfl = 'N' and
     C                   If        RTACFL = 'Y' or
     C***  End   Chg  11/05/04  *******************************************
     C                             Rtselc = 'N'
     C                   Iter
     C                   Endif
      *
     C                   Eval      Emptot = Emptot + Rtamt
     C***  Begin Chg  11/05/04  *******************************************
     C**********         Except    Detail
     C                   Except    detailo
     C***  End   Chg  11/05/04  *******************************************
     C                   Eval      Print = *On
      *
     C                   If        *Inof
     C                   Except    Head
     C***  Begin Add  11/05/04  *******************************************
     C                   Except    heado
     C***  End   Add  11/05/04  *******************************************
     C                   Endif
      *
     C                   Enddo
      *
     C                   If        Print
     C                   Except    Emp_Total
     C                   Endif
      *
     C                   Endsr
      *========================================================================*
      * Read the detail file for batch passed in parm with 'Y' (Account Cur)
      *========================================================================*
     C     Proc_Y        Begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   Except    head
     C                   Except    heada
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   Eval      Print = *Off
      *** beg chg ***** 11/10/06 **********************************************
     C*****Wxrtl_Key     Setll     Wxrtl
     C**************     Dou       %Eof(Wxrtl)
     C*****Wxrtl_Key     Reade     Wxrtl
     C**************     If        %Eof(Wxrtl)
     C*** Beg Add ***** 11/26/08  **************************************
     C   52              Open      Wxrtl2
     C   51              Open      Wxrtl1
     C   53              Open      Wxrtl
     C   52Wxrtl_Key     Setll     Wxrtl2
     C   51Wxrtl_Key     Setll     Wxrtl1
     C   53Wxrtl_Key     Setll     Wxrtl
     C*** End Add ***** 11/26/08  **************************************
      *
     C*** Beg Del ***** 11/26/08  **************************************
     C*****Wxrtl_Key     Setll     Wxrtl2
     C*** End Del ***** 11/26/08  **************************************
      *
     C*** Beg Chg ***** 11/26/08  **************************************
     C***********        Dou       %Eof(Wxrtl2)
     C                   Dou       %Eof(Wxrtl2) or %Eof(Wxrtl1) or %Eof(Wxrtl)
      *
     C*****Wxrtl_Key     Reade     Wxrtl2
     C   52Wxrtl_Key     Reade     Wxrtl2
     C*** End Chg ***** 11/26/08  **************************************
      *
     C*** Beg Add ***** 11/26/08  **************************************
     C   51Wxrtl_Key     Reade     Wxrtl1
     C   53Wxrtl_Key     Reade     Wxrtl
     C*** End Add ***** 11/26/08  **************************************
      *
     C*** Beg Chg ***** 11/26/08  **************************************
     C**************     If        %Eof(Wxrtl2)
     C                   If        %Eof
     C*** Beg Chg ***** 11/26/08  **************************************
      *
     C*** Beg Add ***** 11/26/08  **************************************
     C   52              Close     Wxrtl2
     C   51              Close     Wxrtl1
     C   53              Close     Wxrtl
     C*** End Add ***** 11/26/08  **************************************
      *
      *** end chg ***** 11/10/06 **********************************************
     C                   Leave
     C                   Endif
      *
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         If        Rtacfl = 'Y' and
     C**********                   Rtselc = 'Y'
     C                   If        RTACFL <> 'Y' or
     C                             RTSELC = 'N'
     C***  End   Chg  ***  11/05/04  **************************************
     C                   Iter
     C                   Endif
      *
     C                   Eval      Acrtot = Acrtot + Rtamt
      *
     C***  Begin Chg  11/05/04  *******************************************
     C**********         Except    Detail
     C                   Except    detailA
     C***  End   Chg  11/05/04  *******************************************
     C                   Eval      Print = *On
      *
     C                   If        *Inof
     C                   Except    Head
     C***  Begin Add  11/05/04  *******************************************
     C                   Except    heada
     C***  End   Add  11/05/04  *******************************************
     C                   Endif
      *
     C                   Enddo
      *
     C                   If        Print
     C                   Except    Acr_Total
     C                   Endif
      *
     C                   Endsr
     C***  Begin Add  11/05/04  *******************************************
     C*====================================================================
     C* Process routing page info
     C*====================================================================
     C     routing       begsr
     C*
     C                   if        ORTITL = *blanks
     C                   eval      ORTITL = title
     C                   endif
     C*
     C                   eval      ORTITL = LeftJstify(ORTITL)
     C*
     C                   if        prtRoutePg
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
     C*
     C                   eval      ORTITL = CenterFld(ORTITL:60)
     C*
     C                   endsr
     C***  End   Add  11/05/04  *******************************************
     C*
      *========================================================================*
      *========================================================================*
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *========================================================================*
     C     *Inzsr        Begsr
      *
     C     Smcol_Key     Klist
     C                   Kfld                    Xxco#
      *
     C     Wmupl_Key     Klist
     C                   Kfld                    Q1user
      *
     C     Smorl_Key     Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
     C     Wxrtl_Key     Klist
     C                   Kfld                    XXco#
     C                   Kfld                    XXbat#
      *
     C*** Beg Add ***** 11/26/08  *******************************************
      *
     C     Wxrnl_Key     Klist
     C                   Kfld                    XXco#
     C                   Kfld                    XXbat#
      *
     C     *Entry        Plist
     C                   Parm                    QQco#             3
     C                   Parm                    QQbat#            9
     C                   Parm                    QQoutq           10
      *
     C                   Move      QQco#         XXco#             3 0
     C                   Move      QQbat#        XXbat#            9 0
     C     qqoutq        comp      *blank                             090908
     C                   Time                    Dsdttm
      *
     C     Wmupl_Key     Chain     Wmupl                              60
     C     Smcol_Key     Chain     Smcol
      *
     C*** Beg Add ***** 11/26/08  *******************************************
      *
     C                   Move      Corfbs        Xcorfbs
     C                   Move      *Off          *In51
     C                   Move      *Off          *In52
     C                   Move      *Off          *In53
      *
     C                   Select
     C                   When      Xcorfbs = 'N'
     C                   Move      *On           *In51
     C                   When      Xcorfbs = 'E'
     C                   Move      *On           *In52
     C                   When      Xcorfbs = ' '
     C                   Move      *On           *In53
     C                   Endsl                                                  End select Flag
     C*** End Add ***** 11/26/08  *******************************************
      *
     C                   call      'SRCENTER'
     C     coname        parm      coname        ##str            80
     C                   parm      40            ##len             3 0
     C                   Move      '0'           XXPRT
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
      *
     C***  Begin Add  11/05/04  *******************************************
     C                   clear                   ORTITL
     C***  End   Add  11/05/04  *******************************************
     C     Smorl_Key     Chain     Smorl
     C***  Begin Chg  11/05/04  *******************************************
     C**********         if        %found
     C**********         call      'SRCENTER'
     C*****ortitl        parm      ortitl        ##str
     C**********         parm      60            ##len
     C**********         else
     C**********         call      'SRCENTER'
     C*****ortitl        parm      title         ##str
     C**********         parm      60            ##len
     C**********         endif
     C                   eval      prtRoutePg = %found
     C***  End   Chg  11/05/04  *******************************************
      *
     C***  Begin Del  ***  11/05/04  **************************************
     C**********         Move      *all'_'       Line
     C***  End   Del  ***  11/05/04  **************************************
      *
     C                   Endsr
      *========================================================================*
     OQSYSPRT   E            Route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            Route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            Route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            Route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            Route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            Route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            Route          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********     09      UPUSNM              67
     O**********E            Route          1
     O**********     08      ORDIS2              60
     O**********E            Route          1
     O**********     08      ORDIS3              60
     O**********E            Route          1
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            Route          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O               09      UPUSNM              67
     O          E            Route          1
     O               08      ORDIS2              60
     O          E            Route          1
     O               08      ORDIS3              60
     O          E            Route          1
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            Route          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            Route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            Route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      **********
      ***** End Del ***** 08/26/14 ************************************
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
     O          E            Route          2
     O                                         +  6 'GROUP RANGE. . . . . . . :'
     O              n62      Rnfgrp            +  2
     O              n62                        +  2 '-'
     O              n62      Rntgrp            +  2
     O          E            Route          2
     O                                         +  6 'COMMENT LINE 1 . . . . . :'
     O                       RNCOM1            +  2
     O          E            Route          2
     O                                         +  6 'COMMENT LINE 2 . . . . . :'
     O                       RNCOM2            +  2
     O          E            Route          2
     O                                         +  6 'COMMENT LINE 3 . . . . . :'
     O                       RNCOM3            +  2
     O          E            Route          2
     O              n61                        +  6 'FROM DATE  . . . . . . . :'
     O              n61      xxfaud            +  2
     O          E   n61      Route          2
     O              n61                        +  6 'TO DATE  . . . . . . . . :'
     O              n61      xxtaud            +  2
     O          E            Route          2
     O              n63                        +  6 'MIN RETRUN AMOUNT  . . . :'
     O              n63      RNMIN         J   +  2
     O          E            Route          2
     O                                         +  6 'BATCH PROC FLAG. . . . . :'
     O                       RNBPF             +  2
      *** beg add ***** 07/18/07 ******************************************************
     O          E            Route          2
     O                                         +  6 'BATCH TYPE . . . . . . . :'
     O                       RNBTYP            +  2
      *** end add ***** 07/18/07 ******************************************************
      *
     O*** BEGIN CHG ***  01/28/10  ************************************
     O****      E            Route          1 60
     O***                                        23 '(C) COPYRIGHT FISERV IN'
     O***                                        46 'SURANCE SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
      *
      * MAIN PAGE HEADING
      *
     O          E            Head           1 06
     O                       Dstime                 '  :  :  '
     O                       Coname              89
     O***  Begin Chg ***  11/05/04  ***************************************
     O**********             Dsdate        Y    138
     O                       Dsdate        Y    132
     O***  End   Chg ***  11/05/04  ***************************************
     O          E            Head           2
     O                       Q1pgm
     O                       Ortitl              99
     O***  Begin Chg ***  11/05/04  ***************************************
     O**********                                132 'PAGE:'
     O**********             Page          Z    138
     O                                          126 'PAGE:'
     O                       Page          Z    132
     O***  End   Chg ***  11/05/04  ***************************************
      *
     O          E            HEAD           2
     O                                              'BATCH:'
     O                       XXbat#        Z
     O                                         +  7 'GROUP RANGE:'
     O              n62      Rnfgrp            +  1
     O              n62                        +  1 '-'
     O              n62      Rntgrp            +  1
     O              n61                        +  7 'DATE RANGE:'
     O              n61      xxfaud            +  1
     O              n61                        +  1 '-'
     O              n61      xxtaud            +  1
      *** beg add ***** 07/18/07 ******************************************
     O                                              'BATCH TYPE:'
     O                       RNBTYP            +  1
      *** end add ***** 07/18/07 ******************************************
     O          E            HEAD           0
     O                       line
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********E            HEAD           2
     O          E            HEADo          2
     O***  End   Chg  ***  11/05/04  **************************************
     O***  beg chg ***** 11/10/06 *****************************************
     O**********                                  5 'Group'
     O**********                                 15 'Emp #'
     O**********                                 21 'Div'
     O**********                                 37 'Employer Name'
     O                                           13 'Employer Name'
     O                                           45 'Grp'
     O                                           56 'Emp #'
     O                                           63 'Div'
     O***  end chg ***** 11/10/06 *****************************************
     O                                           68 'Fyr'
     O                                           95 'Return Amt'
     O                                          102 'Slctd'
     O                                          112 'A/C Flag'
     O                                          122 'Pd To'
     O                                          132 'Prem/Fin'
     O***  Begin Add  ***  11/05/04  **************************************
     O          e            heada          2
     O                                           10 'Agt Fein'
     O                                          + 2 'Mod'
     O                                           34 'Agent Name'
     O                                           95 'Return Amt'
     O                                          102 'Slctd'
     O***  End   Add  ***  11/05/04  **************************************
      *
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********E            Detail         1
     O          E            Detailo        1
     O***  End   Chg  ***  11/05/04  **************************************
     O***  beg chg ***** 11/10/06 *****************************************
     O************           RTFND                4
     O************           RTEMP#        Z     15
     O************           RTDIV         Z     21
     O************           RTEMPN              64
     O                       RTEMPN              40
     O                       RTFND               45
     O                       RTEMP#        Z     56
     O                       RTDIV         Z     63
     O***  end chg ***** 11/10/06 *****************************************
     O                       RTFYR               68
     O                       RTAMT         J     96
     O                       RTSELC             100
     O***  Begin Del  ***  11/05/04  **************************************
     O**********             RTACFL             110
     O***  End   Del  ***  11/05/04  **************************************
     O                       RTPTFL             120
     O                       RTPFIN             131
     O***  Begin Add  ***  11/05/04  **************************************
     O          E            detaila        1
     O                       RTFEIN        3     10
     O                       RTMOD         3    + 2
     O                       RTAGTN              64
     O                       RTAMT         J     96
     O                       RTSELC             100
     O***  End   Add  ***  11/05/04  **************************************
      *
     O          E            Emp_Total   1  1
     O                                              'EMPLOYER TOTAL:'
     O                       Emptot        J     96
      *
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********E            Acr_Total      1
     O          E            Acr_Total   1  1
     O                                              'AGENT TOTAL   :'
     O***  End   Chg  ***  11/05/04  **************************************
     O                       Acrtot        J     96
      *
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********E            Lr_Total       1
     O          E            Lr_Total    2  1
     O***  End   Chg  ***  11/05/04  **************************************
     O                                              'GRAND TOTAL   :'
     O                       Lrtot         J     96
