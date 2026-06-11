     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================
     H/TITLE - RETURN PREMIUM CHECKS TO BE WRITTEN REPORT                      *
      *                                                                        *
      *  DESCRIPTION: Reports checks that haven't been written.                *
      *                                                                        *
      *  CREATED: 03/12/2003  JOHN STONE                                       *
      *=========================================================================
      * REVISIONS:                                                             *
      *                                                                        *
      * 08/05/15 C:307 Project: SY354                                          *
      *                -Adding 'Route To:'                                     *
      *                                                                        *
      * 08/26/14 C:298 Project: SY354                                          *
      *                -Implement Routing Page Changes.                        *
      *                                                                        *
      * 01/28/10  C:249  Project: SY215                                        *
      *           C:189  Retrieve brand Name                                   *
      *                                                                        *
      * 07/18/07 C:136 Project: WC1491                                         *
      *                Implement Ind Cash and Return Deposit                   *
      *                                                                        *
      *=========================================================================
     FSmcol     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FSmorl     IF   E           K Disk
     FWmfnl     IF   E           K Disk
     FWar4l2    IF   E           K Disk
     FQsysprt   O    F  115        Printer OFLIND(*INOF)
      *=========================================================================
     D First           S              1n
     D Line            S            115
     D xxptfl          S              8
     D title           C                   'RETURN FUND CHECKS TO BE WRITTEN'
     D Relamt          S             15  2
     D Hldamt          S             15  2
     D Totamt          S             15  2
     D Relamtac        S             15  2
     D Hldamtac        S             15  2
     D Totamtac        S             15  2
     D Datusa          S               D   DATFMT(*USA)
      *
     D Svco#           S                   Like(R4co#)
     D Svfnd           S                   Like(R4fnd)
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
      *  Loop through War4p
      *
     C     War4l2_Key    Setll     War4l2
     C                   Eval      First = *On
      *
     C                   Dou       %Eof(War4l2)
      *
     C     War4l2_Key    Reade     War4l2
      *
     C                   If        Not %Eof
     C                   If        (Qqhrb = 'R' and R4selc = 'Y') or
     C                             (Qqhrb = 'H' and R4selc = 'N') or
     C                             (Qqhrb = 'B')
      *
      *** beg add ***** 07/18/07 ***********************************************
     c                   if        qqbtyp = *blank or
     c                             qqbtyp = R4BTYP
      *** end add ***** 07/18/07 ***********************************************
      *
     C                   If        First
     C                   Move      R4co#         Svco#
     C                   Move      R4fnd         Svfnd
     C                   Eval      First = *Off
     C                   Endif
      *
     C                   If        R4fnd <> Svfnd
     C                   Select
     C                   When      Qqhrb = 'R'
     C                   Except    relFnd
     C                   Clear                   Relamtac
     C                   When      Qqhrb = 'H'
     C                   Except    hldFnd
     C                   Clear                   Hldamtac
     C                   Other
     C                   Except    relFnd
     C                   Clear                   Relamtac
     C                   Except    hldFnd
     C                   Clear                   Hldamtac
     C                   Except    totFnd
     C                   Clear                   Totamtac
      *
     C                   Except    Head
      *
     C                   Endsl
     C                   Move      R4co#         Svco#
     C                   Move      R4fnd         Svfnd
     C                   Endif
      *
     C                   Select
     C                   When      R4selc = 'Y'
     C                   Eval      Relamt = R4amt + Relamt
     C                   Eval      Relamtac = R4amt + Relamtac
     C                   When      R4selc = 'N'
     C                   Eval      Hldamt = R4amt + Hldamt
     C                   Eval      Hldamtac = R4amt + Hldamtac
     C                   Endsl
      *
     C                   select
     C                   when      r4ptfl = 'A'
     C                   eval      xxptfl = 'Agent'
     C                   when      r4ptfl = 'E'
     C                   eval      xxptfl = 'Employer'
     C                   when      r4ptfl = 'G'
     C                   eval      xxptfl = 'Group'
     C                   when      r4ptfl = 'P'
     C                   eval      xxptfl = 'P/F Co'
     C                   endsl
      *
     C                   Eval      Totamt = R4amt + Totamt
     C                   Eval      Totamtac = R4amt + Totamtac
      *
     C                   Except    Detail
      *
     C                   If        *Inof
     C                   Except    Head
     C                   Endif
      *
      *** beg add ***** 07/18/07 ***********************************************
     C                   endif
      *** end add ***** 07/18/07 ***********************************************
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Select
     C                   When      Qqhrb = 'R'
     c                   Except    relFnd
     C                   Except    Reltot
     C                   When      Qqhrb = 'H'
     C                   Except    hldFnd
     C                   Except    Hldtot
     C                   Other
      *
     c                   Except    relFnd
     C                   Except    hldFnd
     C                   Except    totFnd
      *
     C                   Except    Reltot
     C                   Except    Hldtot
     C                   Except    Lrtot
      *
     C                   Endsl
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   Eval      *Inlr = *On
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
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
     C     War4l2_Key    Klist
     C                   Kfld                    XXco#
      *
     C     *Entry        Plist
     C                   Parm                    QQco#             3
     C                   Parm                    QQhrb             1
     C                   Parm                    qqque            10
      *** beg add ***** 07/18/07 ***********************************************
     C                   parm                    qqbtyp            1
      *** end add ***** 07/18/07 ***********************************************
      *
     C                   Move      QQco#         XXco#             3 0
     C                   Time                    Dsdttm
      *
     C     Wmupl_Key     Chain     Wmupl
     C     Smcol_Key     Chain     Smcol
      *
     C                   call      'SRCENTER'
     C     coname        parm      coname        ##str            80
     C                   parm      40            ##len             3 0
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     C     qqque         comp      *blank                             090908
     C                   Move      '0'           XXPRT
     C     Smorl_Key     Chain     Smorl
      *
      * OPEN PRINT FILE
      *
     C                   if        %found
     C                   call      'SRCENTER'
     C     ortitl        parm      ortitl        ##str
     C                   parm      60            ##len
     C                   else
     C                   call      'SRCENTER'
     C     ortitl        parm      title         ##str
     C                   parm      60            ##len
     C                   endif
      *
     C                   Move      *all'_'       Line
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         Except    Route
      ***** End Del ***** 08/26/14 ************************************
     C                   Except    Head
      *
     C                   Endsr
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
     O**********E            Route          2
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
     O          E            Route          2
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
      ***** End Del ***** 08/26/14 ************************************
      *
      * CHANGED ROUTING PAGE TO COMPLY WITH VERSION 3 STANDARDS.
      *
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
     O          E            Route          2
     O                                         +  6 'REPORT SELECTION . . . . :'
     O                       QQHRB             +  1
      *** beg add ***** 07/18/07 *******************************************************
     O          e            route          2
     O                                         +  6 'BATCH TYPE . . . . . . . :'
     O                       qqbtyp            +  1
      *** end add ***** 07/18/07 *******************************************************
      *
     O*** Beg Chg *****  01/28/10  ************************************
     O****      E            Route          1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                        46 'VE DATA SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
      *
      * MAIN PAGE HEADING
      *
     O          E            Head           1 06
     O                       Dstime                 '  :  :  '
     O                       Coname              78
     O                       Dsdate        Y    115
     O          E            Head           2
     O                       Q1pgm
     O                       Ortitl              88
     O                                          110 'PAGE:'
     O                       Page          Z    115
      *
     O          E            HEAD           0
     O                       Line
     O          E            HEAD           2
     O                                            3 'GRP'
     O                                           15 'EMPLOYER#'
     O                                           22 'DIV'
     O                                           27 'FYR'
     O                                           36 'PAID TO'
     O                                           52 'PAID TO NAME'
     O                                           97 'CHECK AMOUNT'
     O                                          111 'SELECTED'
      *
     O          E            Detail         1
     O                       R4FND         Z      3
     O                       R4EMP#        Z     15
     O                       R4DIV         Z     22
     O                       R4FYR         Z     27
     O                       XXPTFL              37
     O                       R4PTNM              80
     O                       R4AMT         J     98
     O                       R4SELC             107
      *
     O          E            relFnd      2  1
     O                                              'GROUP RELEASED TOTAL'
     O                       Relamtac      J     98
      *
     O          E            hldFnd         1
     O                                              'GROUP HELD TOTAL'
     O                       Hldamtac      J     98
      *
     O          E            totFnd         1
     O                                              'GROUP TOTAL'
     O                       Totamtac      J     98
      *
     O          E            Reltot      2  1
     O                                              'RELEASED TOTAL'
     O                       Relamt        J     98
      *
     O          E            Hldtot         1
     O                                              'HELD TOTAL'
     O                       Hldamt        J     98
      *
     O          E            Lrtot          1
     O                                              'GRAND TOTAL'
     O                       Totamt        J     98
      *
