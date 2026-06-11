     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/title Query Response File Error Report
     H*====================================================================
     H*  Created: 08/14/09   C:232  Project: WC1819 Phase 5.
     H*====================================================================
    H*  Revisions:
     H*                                                               *
     H*  03/06/25 C:333  Project WC2998                               *
     H*                  Changes to CMS Settlement Reporting.         *
     H*                  - Recompiled.                                *
     H*
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H*  03/25/11   C:198  Support: 75774
     H*                    Changed the desc of counters at the bottom of report
     H*                    and added more.
     H*
     H*  12/13/10   C:198  Project: WC1963
     H*                    Display on those claims that are received in WXM1P.
     H*
     H*  08/06/10   C:198  Project: WC1879 Priss 42 and 44
     H*                    Fixed the issues of multiple blank lines and
     H*                    no names claimants.
     H*
     H*  05/18/10   C:198  Project: WC1879
     H*                    Fixed the issue where header was printing
     H*                    for no reasons.
     H*
     H*====================================================================

    F*  Physical file definition (for maintaining record)
     FWDD1L1    if   e           k disk
     F*** Beg Chg *** 08/06/10 *********************************************
     F*********WMCMLH    if   e           k disk
     FWMCML1    if   e           k disk
     F*** End Chg *** 08/06/10 *********************************************
     F*** Beg Add *** 12/13/10 *********************************************
     FWXM1L     if   e           k disk
     F*** End Add *** 12/13/10 *********************************************
     FSMORL     IF   E           K DISK
     FQsysprt   o    f  132        printer oflind(*inof)

    D*================================================================
    D*  External Data Structure
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------

     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     d                 ds
     d  grp#                          3    inz
     d  year#                         3    inz
     d  case#                         7    inz

     D @@rcdcnt        s              9  0 inz
     D CO#             S              3  0
     D QUE             S             10
     D rcd_found       S               n   inz
     D xxrsdt          S              7  0
     D rspdte          S              8
     D ##CO#           S              3  0 inz
     D ##FND           S              3  0 inz
     D ##CONM          S             40    inz
     D ##FNAM          S             40    inz
     D Count_01        S              9  0 inz
     D Count_51        S              9  0 inz
     D*** Beg Add *** 03/25/11 ***********************************
     D   Tot_01        S              9  0 inz
     D   Tot_51        S              9  0 inz
     D*** End Add *** 03/25/11 ***********************************
     D GrYrCs          S             15    inz
     D ClmntNm         S             50    inz
     D d1code_sv       S                   like(d1code) inz
     D desc01          c                   'Medicare beneficiary'
     D desc51          c                   'Not Identified Med Benf.'
     D xxmbs           S                   like(cmmbs)
     D svfnd           S                   like(d1fnd)
     d* Route Information
     d routeCo#        s              3
     d routeDate       s               d   datfmt(*usa)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d companynm       s             60
     D comapi          c                   const('WA001R')
     C*
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    QUE
     C                   PARM                    RSPDTE

     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM
     C                   PARM                    ##FNAM

     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C

    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *  KEY LIST FOR WDD1L1
     C     D1ky          KLIST
     C                   KFLD                    xxrsdt
     C                   KFLD                    CO#

     C     D2ky          KLIST
     C                   KFLD                    xxrsdt
     C                   KFLD                    D1CO#
     C                   KFLD                    D1FND

     C     cmky          KLIST
     C                   KFLD                    co#
     C                   KFLD                    d1fnd
     C                   KFLD                    d1fyr
     C                   KFLD                    d1case
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
    C*================================================================
    C*                  M A I N L I N E
    C*================================================================

     C                   exsr      initialize
     C*
     C                   Move      Rspdte        srdti
     C                   Move      rspdte        ##rsdt            8 0
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         xxrsdt


      * Read through WDD1L1 and populate the report data
     C     D1ky          Setll     PD101
     C     D1ky          ReadE     PD101                                  90
     C                   except    Header
     C*** Beg Del *** 08/06/10 **************************************
     C********           except    space#
     C********           eval      d1code_sv = d1code
     C*** End Del *** 08/06/10 **************************************

     C                   eval      svfnd = d1fnd
     C*
     C                   Dow       *in90 = *off
     C                   If        D1type = 'Q'
     C*** Beg Add *** 12/13/10 **************************************
     C     cmky          chain     pm101                              91
     C                   if        *in91 = *on
     C     D1ky          ReadE     PD101                                  90
     C                   iter
     C                   endif
     C*** End Add *** 12/13/10 **************************************

     C   of              except    Header
     C*** Beg Add *** 05/18/10 **************************************
     C*
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*** End Add *** 05/18/10 **************************************

     C*** Beg Add *** 08/06/10 *************************************************
     C                   move      *blanks       clmntnm
     C*** End Add *** 08/06/10 *************************************************
     C     cmky          setll     pcm01
     C     cmky          reade     pcm01                                  90
     C                   dow       *in90 = *off
     C                   eval      xxmbs = cmmbs
     C                   eval      clmntnm = %trim(cmlsn) + ', '
     C                                        + %trim(cmfsn)
     C                                        + ' ' + cmmid
     C     cmky          reade     pcm01                                  90
     C                   enddo
     C*
     C*** Beg Add *** 03/25/11 *************************************************
     C                   Select
     C                   When      D1Code = '01'
     C                   eval      tot_01 = tot_01 + 1
     C                   When      D1Code = '51'
     C                   eval      tot_51 = tot_51 + 1
     C                   endsl
     C*** End Add *** 03/25/11 *************************************************
     C                   Select
     C                   When      D1Code = '01' and xxmbs = ' '
     C                   eval      count_01 = count_01 + 1
     C*** Beg Add *** 08/06/10 *************************************************
     C                   if        d1code_sv <> d1code
     C                             and d1code_sv <>  ' '
     C                   eval      d1code_sv = d1code
     C                   except    space#
     C                   endif
     C*** End Add *** 08/06/10 *************************************************
     C                   exsr      prntdata
     C*** Beg Add *** 06/08/10 *************************************************
     C                   eval      d1code_sv = d1code
     C*** End Add *** 06/08/10 *************************************************
     C                   When      D1Code = '51' and xxmbs = 'Y'
     C                   eval      count_51 = count_51 + 1
     C*** Beg Add *** 08/06/10 *************************************************
     C                   if        d1code_sv <> d1code
     C                             and d1code_sv <>  ' '
     C                   eval      d1code_sv = d1code
     C                   except    space#
     C                   endif
     C*** End Add *** 08/06/10 *************************************************
     C                   exsr      prntdata
     C*** Beg Add *** 06/08/10 *************************************************
     C                   eval      d1code_sv = d1code
     C*** End Add *** 06/08/10 *************************************************
     C                   Endsl

     C                   Endif                                                  d1type = 'Q'
     C     D1ky          ReadE     PD101                                  90
     C                   If        d1fnd <> svfnd
     C*
     C                   If        not rcd_found
     C                   except    fot2
     C                   Endif
      * Group# Changed..
     C                   except    skip#
     C                   except    Header
     C*** Beg Del *** 08/06/10 **************************************
     C*******            except    space#
     C*** End Del *** 08/06/10 **************************************
     C                   eval      svfnd = d1fnd
     C                   Endif

     C*** Beg Del *** 08/06/10 **************************************
     C*******            if        d1code_sv <> d1code
     C*******            eval      d1code_sv = d1code
     C*******            except    space#
     C*******            endif
     C*** End Del *** 08/06/10 **************************************

     C                   Enddo

     C                   If        rcd_found
     C                   exsr      prntsummry
     C                   except    Space#
     C                   except    fot1
     C                   Endif

     C                   If        not rcd_found
     C                   except    fot2
     C                   except    fot1
     C                   Endif

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfile = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   Eval      *inlr = *on
     C                   Return
     *===============================================================*
     C     PrntData      Begsr
     C                   eval      rcd_found = *on

     C                   move      D1FND         grp#
     C                   move      D1FYR         year#
     C                   move      D1CASE        case#
     C                   eval      GrYrCs = grp# + '/' + year#
     C                                      + '/' + case#

     C                   except    prtdata

     C                   Endsr
     *===============================================================*
     C     PrntSummry    Begsr

     C                   except    Header1
     C                   except    Disp01
     C                   except    Disp51
     C*** Beg Add 03/25/11 ********************************************
     C                   except    Mism01
     C                   except    Mism51
     C*** End Add 03/25/11 ********************************************

     C                   Endsr
     *===============================================================*
     C     initialize    Begsr

     C* Time & date
     C                   movel     @@co#         co#
     C                   Time                    dsdttm
     C* Company name
     c                   call      comapi        compl
     c                   move      ##conm        CompanyNm
     C* Printer file info
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF

     C                   Endsr
     *===============================================================*
     O*  Query Response File Error Report
     OQsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********
     O**********E            ROUTE          1
     O**********                                 15 'Route to:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
     O**********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'Route to:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'Report Title:'
     O                                           61 'Query Response File Report'
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'Program Name:'
     O**********             ORPNAM              30
     O**********                                 36 'File:'
     O**********             ORFIL#              38
     O**********                                 46 'Copies:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'Hold:'
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'Output queue:'
     O**********             OROUTQ              30
     O**********                                 37 'Forms:'
     O**********             ORFORM              48
     O**********                                 56 'Save:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     o*******   e            route            58
     o          e            route            54
     o                                         +001 '(c)'
     o                       *year             +001
     o                                         +001 'StoneRiver, Inc.'
     o                                         +001 'All rights reserved.'
     o          e            route       1
     o                                         +001 'StoneRiver is a'
     o                                         +001 'trademark of'
     o                                         +001 'StoneRiver, Inc.'
     O********  E            route          1 06
     O          E            route       1  1
     O                       dstime              09 '  :  :  '
     O                       CompanyNm           80
     O                       dsdate             130 '  /  /    '
     O*
     O*** Beg Chg *** 08/06/10 ****************************************
     O*******   e            Header      1  3
     O          e            Header      1  2
     O*** End Chg *** 08/06/10 ****************************************
     o                       q1pgm               11
     O                                           80 'Query Response File Report'
     O                                          126 'Page:'
     O                       page          Z    130
     O          e            Header         2
     O                                           15 'Response Date:'
     O                       ##rsdt              26 '  /  /    '
     O          e            Header         2
     O                                            7 'Group:'
     O                       d1fnd               11
     O          e            Header         1
     O                                           12 'Grp/Yr/Case'
     O                                          +12 'Claimant'
     O                                          +50 'Disposition'
     O          e            Header         1
     O                                           28 'Name'
     O                                           86 'Code'
     O          e            prtdata        1
     O                       GrYrCs              16
     O                       ClmntNm             +8
     O                       D1CODE              86
     O          e            Header1        4  1
     O                                           80 'Query Response File Summa'
     O                                           +0 'ry'
     O          e            Disp01         2
     O                                           17 'Disposition Code'
     O                                          +10 'Description'
     O                                          +25 'Total# of Claims'
     O          e            Disp01         4
     O                                            8 '01'
     O                       desc01              42
     O*** Beg Chg *** 03/25/11 ************************************************
     O***********            count_01      J     75
     O                       Tot_01        J     75
     O*** End Chg *** 03/25/11 ************************************************
     O          e            Disp51         2
     O                                           17 'Disposition Code'
     O                                          +10 'Description'
     O                                          +25 'Total# of Claims'
     O          e            Disp51         4
     O                                            8 '51'
     O                       desc51              47
     O*** Beg Chg *** 03/25/11 ************************************************
     O********               count_51      J     75
     O                       Tot_51        J     75
     O*** End Chg *** 03/25/11 ************************************************
     O*** Beg Add *** 03/25/11 ************************************************
     O          e            Mism01         2
     O                                            9 'Mismatch'
     O                                           +4 'CMS identified as 01 but '
     O                                              'WCMS Med Ben Flag <> Y'
     O                                           79 'Total# of Claims'
     O          e            Mism01         4
     O                       count_01      J     75
     O          e            Mism51         2
     O                                            9 'Mismatch'
     O                                           +4 'CMS identified as 51 but '
     O                                              'WCMS Med Ben Flag = Y'
     O                                           79 'Total# of Claims'
     O          e            Mism51         4
     O                       count_51      J     75
     O*** End Add *** 03/25/11 ************************************************
     O          e            Fot1           1
     O                                          +35 ' *** End  Of  Report *** '
     O          e            Fot2           1
     O                                          +35 ' *** No Records Found *** '
     O          e            Space#         1
     O          e            Skip#                1
