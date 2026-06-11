     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E7523R - Expected Losses & Discount Ratio
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* Prints Expected Losses & Discount Ratio                      *
     H*                                                               *
     H* CALLED BY: E7523C                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 07/02/2001                                           *
     H*                                                               *
     H* 07/02/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Exapand Expected Loss Rate to 8,3.             *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*****************************************************************
     H**                                                             **
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  80       XXTO AND XXFROM = BLANKS, PRINT 'ALL'               *
     H*  81       @@EFDT = BLANKS, PRINT 'ALL'                        *
     H*                                                               *
     H*****************************************************************
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * System User Parameters
     FSMUPL     IF   E           K DISK
      *
      * Logical file by co#, state, efdt, and class
     FEMEXL4    IF   E           K DISK
      *
      * State Table
     FSTABL     IF   E           K DISK
      *
      * Output
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

      *** Beg Add ***** 06/19/24 ***************************************
     D xxLsRt          s              8  5
      *** End Add ***** 06/19/24 ***************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *===============================================================
      * PLISTS
      *===============================================================
      *
      *  Company
      *

     C     COKY          KLIST
     C                   KFLD                    KKCO#
      *
      *  Output Routing
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *  User Paramter
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  State
      *
     C     STKY          KLIST
     C                   KFLD                    KKST
      *
      *
      *
      *===============================================================
      * *LIKE DEFN
      *===============================================================
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    EXEFDT        SSEFDT
     C     *LIKE         DEFINE    COCO#         KKCO#
      *
      *
      *
      *===============================================================
      * PLISTS
      *===============================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FROM            5
     C                   PARM                    @@TO              5
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ST              2
     C                   PARM                    @@EFDT            7
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *
      *===============================================================
      * BEGIN
      *===============================================================
      *
     C                   MOVEL     @@FROM        SSFRCL            5 0
     C                   MOVEL     @@TO          SSTOCL            5 0
     C                   MOVEL     @@FROM        X1FRCL            5 0
     C                   MOVEL     @@TO          X1TOCL            5 0
     C                   MOVEL(P)  @@ST          SSST              2
     C                   MOVEL(P)  @@ST          KKST
     C                   MOVEL     @@CO#         KKCO#
     C                   MOVEL     @@EFDT        SSEFDT
      *
     C                   MOVE      @@EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
      *
     C     X1EFDT        COMP      *ZERO                                  72
     C     SSTOCL        COMP      *ZERO                                  73
     C     SSST          COMP      *BLANKS                                74
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE             80
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *  Get state information
      *
     C     STKY          CHAIN     PST01                              60
      *
      *
      *  Print Routing Page
      *
     C                   EXSR      SRRTE
      *
      *  Begin reading file
      *
     C                   SETON                                        OA
     C                   READ      PEX01                                  91
      *
     C     *IN91         DOWEQ     '0'
      *
      *  If subset criteria met
      *
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
     C     MEETSS        IFEQ      *ON
      *
      *  Write detail record
      *
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
      *
     C                   ADD       1             LRCNT             9 0
     C                   ENDIF                                                  MEETSS
      *** Beg Add ***** 06/19/24 **********************************************
       xxLsRt = EXLsRt * .01;
      *** End Add ***** 06/19/24 **********************************************
      *
      *  Read next record
      *
     C                   READ      PEX01                                  91
     C                   ENDDO                                                  WRK RECORD
      *
      *  Message if no records printed
      *
     C     MEETPR        IFEQ      *ON
     C     LRCNT         OREQ      *ZERO
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF                                                  MEETPR
      *
      *   End Program
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * SUBSET - Check if record meets selection criteria
      *================================================================
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
     C     SSEFDT        IFNE      *ZERO
     C     SSEFDT        ANDNE     EXEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEFDT
      *
     C     SSST          IFNE      *BLANKS
     C     SSST          ANDNE     EXST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND
      *
     C     SSFRCL        IFNE      *ZEROS
     C     SSFRCL        ANDGT     EXCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFRCL
      *
     C     SSTOCL        IFNE      *ZEROS
     C     SSTOCL        ANDLT     EXCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  TOFRCL
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
      *================================================================
      * SRDET - Write Detail Record
      *================================================================
      *
     CSR   SRDET         BEGSR
      *
     C                   Z-ADD     EXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2EFDT            8 0
      *
     C     SRERR         COMP      *ON                                    71
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
      *
     C                   ENDSR                                                  SRDET
      *
      *
      *
      *
      *================================================================
      * SRRTE - Print routing page
      *================================================================
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *  User Name if no Distribution specified in routing
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
      ***** Beg Del ***** 08/26/14 ********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ********************************************
     C                   ENDIF
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ********************************************
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ********************************************
      *
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
      *
      *
     O          E            ROUTE          2
     O                                           26 'STATE . . . . . :'
     O              N74      @@ST              +001
     O               74                          31 '*ALL'
      *
     O          E            ROUTE          2
     O                                           26 'EFFECTIVE DATE  :'
     O              N72      X1EFDT            +001 '  /  /    '
     O               72                          31 '*ALL'
      *
     O          E            ROUTE          2
     O                                           26 'PAYROLL CLASS . :'
     O                                         +001 'FROM'
     O              N73      X1FRCL              37
     O               73                          37 '*ALL'
     O                                         +001 'TO'
     O              N73      X1TOCL              46
     O               73                          46 '*ALL'
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS. INC.,'
     O***                      *YEAR   +001
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           49 'TABLE FILE LISTING'
     O                                           75 'PAGE:'
     O                       PAGE          Z     80
      *
     O          E            HEAD           1
     O                                           36 'EXPECTED LOSSES'
     O                                         +  1 '& DISCOUNT RATIO REPORT'
      *
      *
     O          E   N92      HEAD           2
     O                                         + 30 'PAYROLL'
     O                                         +  5 'EXPECTED'
     O                                         +  4 'DISCOUNT'
      *
     O          E            HEAD           0
     O                       LINE              +000
      *
     O          E   N92      HEAD           2
     O                                         +  0 'CO#'
     O                                         +  4 'STATE'
     O                                         +  6 'EFF DATE'
     O                                         +  6 'CLASS'
     O                                         +  4 'LOSS RATE'
     O                                         +  8 'RATE'
     O                                         +  4 'STATUS'
      *
     O          E            DET            1
     O                       EXCO#          B  +  0
     O                       EXST           B  +  4
     O              N71      X2EFDT         B  +  7 '  /  /    '
     O                       EXCLAS        JB  +  5
      *** Beg Chg ***** 06/19/24 *********************************************
     O***                    EXLSRT         B  +  6 '0.    '
     O                       XXLSRT        4B  +  6
      *** End Chg ***** 06/19/24 *********************************************
     O                       EXDSRT         B  +  8 '0.  '
     O                       EXSTAT         B  +  6
      *
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
