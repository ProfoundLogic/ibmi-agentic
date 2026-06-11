     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================
     H/TITLE -  Update All Error Report Print Program                          *
      *                                                                        *
      *  DESCRIPTION: This Program wll print a report of all error listings for*
      *  the current policy being shown.                                       *
      *                                                                        *
      *                                                                        *
      *   CALLED BY: W0051C3                                                   *
      *                                                                        *
      *   CALLS TO :                                                           *
      *                                                                        *
      *  CREATED: 07/27/2007  C:189 PROJECT WC1454                             *
      *=========================================================================
      **************************************************************************
      * REVISIONS:
      *
     H* 08/05/15 C:307 Project: SY354                                          *
     H*                -Adding 'Route To:'                                     *
     H*                                                                        *
      * 08/26/14 C:298 Project: SY354                                          *
      *                -Implement Routing Page Changes.                        *
      *                                                                        *
      ***************************************************************************
     FWxual     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FSmorl     IF   E           K Disk
     FSMCOL     IF   E           K Disk
     F***wmrsl     if   e           k disk
     Fwmeml     if   e           k disk
    D*
     FQsysprt   O    F  115        Printer OFLIND(*INOF)
      *=========================================================================
     D First           S              1n
     D Line            S            115
      *
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
      *  Loop through War4p
      *
     C     War4l2_Key    Setll     Wxual
     C     War4l2_Key    Reade     Wxual
     C                   Eval      First = *On
      *
     C                   Dow       not %Eof(Wxual)
      *
     C                   If        UAST <> *Blanks
      *
     C                   If        First
     C*
     C                   Z-ADD     qqdate        SRDTI
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdate            8 0
    C*
     C                   Except    Head
     C                   Except    Head1
     C                   Except    Head11
     C                   Except    Headdat
     C                   Except    Head12
     C                   Except    Head111
     C                   Except    Head2
     C                   Eval      First = *Off
     C                   Eval      *inof = *Off
     C                   Endif
     C     k_Wmeml1      Chain     Wmeml
     C                   Except    Detail
      *
     C                   If        *Inof  = *on
     C                   Except    Head
     C                   Except    Head2
     C                   Endif
      *
     C                   Endif
     C     War4l2_Key    Reade     Wxual
     C                   Enddo
      *
     C                   Except    summery
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
     C                   Eval      *Inlr = *On
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
     C     *Inzsr        Begsr
      *
     C     Smcol_Key     Klist
     C                   Kfld                    Xxco#             3 0
      *
     C     Wmupl_Key     Klist
     C                   Kfld                    Q1user
      *
     C     Smorl_Key     Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
     C     K_Wmeml       Klist
     C                   Kfld                    XXco#
     C                   Kfld                    XXfnd
     C                   Kfld                    XXemp#
      *
     C     K_Wmeml1      Klist
     C                   Kfld                    uaco#
     C                   Kfld                    uafnd
     C                   Kfld                    uaemp#
      *
     C     War4l2_Key    Klist
     C                   Kfld                    QQuser
     C                   Kfld                    QQdate
     C                   Kfld                    qqtime
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     *Entry        Plist
     C                   Parm                    QQuser           10
     C                   Parm                    QQdate            7 0
     C                   Parm                    qqtime            6 0
     C*****              Parm                    qqque            10
     C                   parm                    QQname           40
     C                   parm                    QQedn            44
     C                   parm                    QQ#div            5 0
     C                   parm                    QQcanc            1
     C                   parm                    QQmpo             1
     C                   parm                    QQntupd           5 0
      *
     C                   Time                    Dsdttm
      *
     C*
     C                   If        QQmpo =  'M'
     C                   Move      *on           *in67
     C                   Else
     C                   Move      *off          *in67
     C                   Endif
     C*
      * To populate the company name on the report
      *
     C     War4l2_Key    Setll     Wxual
     C     War4l2_Key    Reade     Wxual
     C                   Move      uaco#         XXco#             3 0
     C                   Move      uafnd         XXfnd             3 0
     C                   Move      uaemp#        XXemp#            9 0
     C     K_Wmeml       Chain     Wmeml
     C                   if        %found(Wmeml)
     C                   Move      emname        XXname           40
     C                   endif
      *
     C                   Move      uafnd         xxfnd#            3 0
     C                   Move      uafyr         xxfyr             3 0
      *
     C     Wmupl_Key     Chain     Wmupl
     C     Smcol_Key     Chain     Smcol
      *
     C                   call      'SRCENTER'
     C     coname        parm      coname        ##str            80
     C                   parm      40            ##len             3 0
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     C                   Move      '0'           XXPRT
     C     Smorl_Key     Chain     Smorl
      *
      * OPEN PRINT FILE
      *
     C                   Move      *all'_'       Line
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         Except    Route
      ***** End Del ***** 08/26/14 ************************************
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
     O**********             ORDIS1              60
     O**********             UPUSNM              67
     O**********E            Route          1
     O**********             ORDIS2              60
     O**********E            Route          1
     O**********             ORDIS3              60
     O**********E            Route          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            Route          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O                       UPUSNM              67
     O          E            Route          1
     O                       ORDIS2              60
     O          E            Route          1
     O                       ORDIS3              60
     O          E            Route          2
     O                       ORDIS4              60
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
     O
      *
      * CHANGED ROUTING PAGE TO COMPLY WITH VERSION 3 STANDARDS.
      *
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
     O          E            Route          2
     O                                         +  6 'REPORT SELECTION . . .   :'
     O**********             QQHRB             +  1
      *
      *
     O          E            Route          1 60
     O                                           23 '(C) COPYRIGHT PROGRESSI'
     O                                           46 'VE DATA SOLUTIONS. INC,'
     O                       *YEAR             +  1
      *
      * MAIN PAGE HEADING
      *
     O          E            Head           1 06
     O                       Dstime                 '  :  :  '
     O                       Coname              78
     O                       Dsdate        Y    115
     O          E            Head           2
     O                       Q1pgm
     O                       Ortitl              97
     O                                          110 'PAGE:'
     O                       Page          Z    115
      *
      * MAIN PAGE HEADING
     O*
     O          E            Head1       2  1
     O                                            7 'Group :'
     O                       xxfnd               11
     O                       qqname              52
     O*
     O          E            Head11
     O                                           25 'Employer/div of original '
     O                                              'update:'
     O                       qqedn               77
     O*
     O*
     O          E            Headdat
     O                                           25 'Update for policy year   '
     O                                              '      :'
     O                       xxfyr               36
     O*
     O          E            Head111
     O                                           26 'Employer detail updates re'
     O                                              'quested by '
     O                       QQuser              47
     O                                           50 'on'
     O                       xxdate        Y     61
     O                                           64 'at'
     O                       QQtime              73 '  :  :  '
     O*
     O          E            Head12         1
     O                    N67                    26 'Number of other divisions '
     O                    N67                       'requested for update:'
     O                     67                    25 'Number of other policies/'
     O                     67                       'divisions requested for up'
     O                     67                       'date:'
     O                    N67QQ#DIV        Z     53
     O                     67QQ#DIV        Z     62
     O                    N67                    74 'Cx div included?:'
     O                     67                    91 'Cx policies/div included?'
     O                     67                       ':'
     O                    N67QQCANC              76
     O                     67QQCANC              94
      *
     O          E            Head2       1  1
     O                                           09 'Employer#'
     O                                           17 'Div'
     O                                           23 'Name'
     O                                           73 'Rating State'
     O                                          100 'Field not updated due to '
     O                                              'error'
     O          E            Detail         1
     O                       uaemp#        Z     09
     O                       uadiv         Z     17
     O                       emname              59
     O                       uast                68
     O                       uadata             115
     O          E            summery     1  1
     O                     67                    26 'Number of policies/divisio'
     O                     67                       'ns not updated due to '
     O                     67                       'error:'
     O                    N67                    17 'Number of divisio'
     O                    N67                       'ns not updated due to'
     O                    N67                       ' error:'
     O                     67QQNTUPD       Z     60
     O                    N67QQNTUPD       Z     51
