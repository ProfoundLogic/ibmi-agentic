     H* (c) 2015 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W3036R - COMMISSION CASH REQUIREMENTS REPORT
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  04/17/12 C:255 Project: WC2044                               *
     H*                 Modify program to use EFT flag.               *
     H*                 Change existing keylist R2KY2 to use the flag.*
     H*                 Use EFT flag instead of  Bank account code    *
     H*                 (__CKTP)                                      *
     H*                                                               *
     H* 05/10/11 C:136 SUPPORT: WA071 76626                           *
     H*                CORRECT DISPLAY OF EFT COUNT AND AMOUNT        *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  09/20/07  C:136 PROJECT: WC997                               *
     H*                  SEPARATE EFT FROM MICR TRANSACTIONS          *
     H*                                                               *
     H*  04/10/00  C:118  PROJECT: WC987                              *
     H*                   USE ALTERNATE COMPANY NAME FROM WMFNP IF    *
     H*                   AVAILABLE.                                  *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWAR2L2    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D TITLE           C                   CONST('Commission Cash -
     D                                     Requirement Report')
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *******Beg Add********04/17/12**********************************
     D xxeftc          s                   like(R2eftc)
      *******End Add********04/17/12**********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQQUE            10
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE GROUP
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR REQUEST COMMISSION PAYMENT FILE
     C*
      *******Beg Chg********04/17/12**********************************
     C*****R2KEY         KLIST
     C*****              KFLD                    XXCO#
     C*****              KFLD                    XXFND
     C*****              KFLD                    XXCKTP
     C*
     C*
     C     r2key         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxeftc
      *******End Chg********04/17/12**********************************
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    R2AGT#
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
      *******Beg Del********04/17/12**********************************
     C******LIKE         DEFINE    R2CKTP        XXCKTP
      *******End Del********04/17/12**********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*
     C*   CHAIN TO THE GROUP FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C*  Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFEQ      *BLANKS                                      FIELD
     C                   MOVEL     TITLE         ORTITL
     C                   ENDIF                                                  ORTITL=*BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C*  CENTER TITLE IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C                   MOVE      '1'           *INOA
     C*
     C*   INITIALIZE TOTALS
     C*
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     0             XXCHK             6 0
     C                   Z-ADD     0             XXECNT            6 0
      *******Beg Chg********04/17/12**********************************
     C*****              CLEAR                   XXCKTP
      /Free
       xxeftc = *Blanks;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C*   START READING THE REQUEST FOR COMMISSION PAYMENT FILE
     C*
     C     R2KEY         CHAIN     PR201                              60
     C*
     C*
     C* IF NO MICR TRANSACTION EXIST, LOOK FOR EFT TRANSACTIONS
     C*
     C     *IN60         IFEQ      *ON
      *******Beg Chg********04/17/12**********************************
     C*****              MOVEL(P)  'EFT'         XXCKTP
      /Free
       xxeftc = 'E';
      /End-Free
      *******End Chg********04/17/12**********************************
     C     R2KEY         CHAIN     PR201                              60
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*   GET AGENT NAME
     C*
     C                   MOVE      *BLANKS       AGNAME
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       AGNAME
     C                   END
     C*
     C                   END
     C*
     C*   ACCUMULATE TOTAL COUNT OF CHECKS & TOTAL DOLLAR AMOUNT
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              SELECT
     C*****XXCKTP        WHENEQ    *BLANK
     C*****              ADD       1             XXCHK
     C*****XXCKTP        WHENEQ    'EFT'
     C*****              ADD       1             XXECNT
     C*****              ENDSL
      /Free
       Select;
          When xxeftc = ' ' ;
             xxchk = xxchk + 1;

          When xxeftc = 'E' ;
             xxecnt = xxecnt + 1;
       EndSl;
      /End-Free
      *******Beg Chg********04/17/12**********************************
     C                   ADD       R2AMT         XXAMT
     C                   Z-ADD     R2PODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              SELECT
     C*****R2CKTP        WHENEQ    'EFT'
     C*****              EXCEPT    HEADE
     C*****              ENDSL
      /Free
       Select;
          When R2eftc = 'E' ;
             Except HEADE;
       EndSl;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C                   EXCEPT    HEAD1
     C*
     C                   ENDIF                                                  *INOA=*ON
     C*
     C                   MOVE      '0'           *INOA
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              SELECT
     C*****R2CKTP        WHENEQ    'EFT'
     C*****R2AMT         ANDEQ     *ZERO
     C*****              EXCEPT    PRENOT
     C*****              ENDSL
      /Free
       Select;
          When R2eftc = 'E'  And R2amt = *Zeros;
             Except PRENOT;
       EndSl;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C                   EXCEPT    DETAIL
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C     R2KEY         READE     PR201                                  60
     C*
     C                   EXSR      CHKEFT
     C*
     C                   END
     C*
     C*   PRINT TOTALS
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C*
     C*  PRINT EITHER CHECK TOTAL OR EFT TOTAL LINES
     C     *IN61         IFEQ      *ON
     C     *IN69         IFEQ      *ON
     C                   Z-ADD     XXCHK         XXCHKG            6 0
     C                   ADD       XXECNT        XXCHKG
     C                   ADD       XXAMT         XXAMTG           11 2
     C                   EXCEPT    TOTLE
     C                   EXCEPT    TOTLG
     C                   ELSE
     C                   EXCEPT    TOTAL
     C                   ENDIF
     C                   ENDIF
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C     *IN61         IFEQ      *OFF
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C* CHKEFT - CHECK FOR EFT TRANSACTIONS
     C*===============================================================
     C*
     C     CHKEFT        BEGSR
     C*
      * IF 60 IS ON, CHG KEY AND CHAIN AGAIN TO FILE FOR EFT TRANSACTINS
     C                   SELECT
      *******Beg Chg********04/17/12**********************************
     C******IN60         WHENEQ    *ON
     C*****XXCKTP        ANDEQ     *BLANKS
     C*****              MOVEL(P)  'EFT'         XXCKTP
      /Free
       When *In60 = *On And xxeftc = ' ' ;
          xxeftc = 'E';
      /End-Free
      *******End Chg********04/17/12**********************************
     C     R2KEY         CHAIN     PR201                              60
      * IF HAVE EFT TRANSACTIONS, PRINT TOTALS, CHECK OVERFLOW & PRT HEADS
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *INOA
     C                   Z-ADD     XXCHK         XXCHKG
     C                   Z-ADD     XXAMT         XXAMTG
     C                   EXCEPT    TOTAL
     C                   Z-ADD     0             XXAMT
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDSR                                                  CHKEFT
     C*
     OQSYSPRT   E            ROUTE          1 06
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          3
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*  SELECTION CRITERIA ADDED TO ROUTING PAGE
     O*
     O          E            ROUTE          2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           12 'GROUP:'
     O                       FNFND         Z     16
     O                       FNNAME            +  2
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       CONAME              89
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
      *
     O          E            HEADE          2
     O                                         + 44 'EFT Transactions'
      *
     O          E            HEAD1          1
     O                                           70 'POSTING'
     O          E            HEAD1          2
     O                                           24 'AGENT#     NAME'
     O                                           82 'DATE        AMOUNT'
     O*
     O          E            PRENOT         0
     O                                         + 85 'Prenote'
     O*
     O          E            DETAIL         1
     O                       R2AGT#        Z     15
     O                       AGNAME              60
     O                       XXPODT        Y     70
     O                       R2AMT         J     83
     O*
     O          E            TOTAL       1  1
     O                                           32 'TOTAL COUNT OF CHECKS:'
     O                       XXCHK         J     40
     O                                           67 'TOTAL AMOUNT:'
     O                       XXAMT         J     83
     O*
     O          E            TOTLE       1  2
     O                                           32 'TOTAL COUNT OF EFTS:'
     O                       XXECNT        JB    40
     O                                           67 'TOTAL AMOUNT:'
     O                       XXAMT         JB    83
     O          E            TOTLG          1
     O                                           32 'CHECK AND EFT COUNT:'
     O                       XXCHKG        JB    40
     O                                           67 'GRAND TOTALS:'
     O                       XXAMTG        JB    83
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC          1
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
