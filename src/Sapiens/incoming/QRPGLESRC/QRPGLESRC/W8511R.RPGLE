     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8511R - CONTRACTORS CLASS REPORT
     H*
     H*****************************************************************
     H*  REVISIONS
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00  C:118  PROJECT: WC987
     H*                   LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*
     H*  11/23/99  C:118  PROJECT: WC577
     H*                   LEFT JUSTIFY REPORT TITLE ON ROUTING PAGE.
     H*
     H*****************************************************************
     FWMCCL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D                 DS
     D  EMMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR CONTRACTORS CLASS FILE
     C*
     C     CCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*                                                    MLS 1/20/88
     C*   KEY LIST FOR THE USER PARAMETERS FILE                 "
     C*                                                         "
     C     UPKEY         KLIST                                                       "
     C                   KFLD                    Q1USER                              "
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
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQQUE            10            MLS 1/20/88
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   TIME                    DSDTTM
     C*                                                    MLS 1/20/88
     C*   SET UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*                                                         "
     C*   CHAIN TO USER PARAMETER FILE                          "
     C*                                                         "
     C     UPKEY         CHAIN     PUP01                              60             "
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE GROUP FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 11/23/99 ***********************************
     C*
     C*  LEFT JUSTIFY TITLE FOR ROUTING PAGE
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL           60
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>BLK
     C***** END   ADD **** 11/23/99 ***********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
     C*
     C                   MOVE      '0'           *INOA
     C     XXEMP#        COMP      0                                      20
     C                   MOVE      '0'           *IN61
     C*
     C*   START READING THE CONTRACTORS CLASS FILE: IF EMP# = 0 CHAIN
     C*   WITH CO#, FND, & FYR; ELSE CHAIN USING EMP# & DIV
     C*
     C   20CCKY2         CHAIN     PCC01                              60
     C  N20CCKEY         CHAIN     PCC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     *IN61         DOWEQ     '0'                                          READ LOOP IF EMP# =
     C   20              Z-ADD     CCEMP#        XXEMP#
     C   20              Z-ADD     CCDIV         XXDIV
     C                   Z-ADD     0             XXMTOT           11 2
     C                   Z-ADD     0             XXCTOT            9 2
     C                   Z-ADD     0             XXPLCY            5 2
     C                   Z-ADD     0             XXPCNT            5 4
     C                   Z-ADD     0             PAGE
     C*
     C*   CHAIN TO EMPLOYER MASTER FILE - GET EMPLOYER ADDRESS
     C*
     C     EMKEY         CHAIN     PEM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANKS       XXADR1           40
     C                   MOVE      *BLANKS       XXADR2           40
     C                   MOVE      *BLANKS       XXCSZ            40
     C     EMMPFL        IFEQ      'M'
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      EMMST         XXSTAT            2
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4 0
     C                   MOVE      EMMCTY        XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXCSZ
     C                   MOVE      EMMAD1        XXADR1
     C                   MOVE      EMMAD2        XXADR2
     C*
     C                   ELSE
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
     C*
     C                   MOVE      EMPST         XXSTAT            2
     C                   MOVE      XXZPP1        XXZIP1            5
     C                   MOVE      XXZPP2        XXZIP2            4 0
     C                   MOVE      EMPCTY        XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXCSZ
     C                   MOVE      EMPAD1        XXADR1
     C                   MOVE      EMPAD2        XXADR2
     C*
     C                   END
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        63
     C*
     C                   END
     C     XXADR2        COMP      *BLANKS                                25
     C     CCCCFL        COMP      'N'                                    30
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C*
     C*
     C     *IN60         DOWEQ     '0'                                          READ LOOP FOR EACH
     C*
     C     CCPAMT        IFNE      0
     C*                                                         EMP# & DIV
     C*   ACCUMULATE TOTALS
     C*
     C                   ADD       CCMNPR        XXMTOT
     C                   ADD       CCCAMT        XXCTOT
     C*
     C*   CHECK IF CONTRACTORS CLASS CHANGED FROM "N" TO "Y"
     C*
     C     *IN30         IFEQ      '1'
     C     CCCCFL        ANDEQ     'Y'
     C                   MOVE      '0'           *IN30
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C*   PRINT DETAIL LINES
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DETAIL
     C*
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     *IN20         IFEQ      '1'
     C     CCKY2         READE     PCC01                                  62
     C     CCEMP#        IFNE      XXEMP#
     C     CCDIV         ORNE      XXDIV
     C     *IN62         ORNE      '0'
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C  N20CCKEY         READE     PCC01                                  60
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        63
     C*
     C                   END
     C*
     C*   CALCULATE CREDIT PERCENTAGE AND PRINT TOTAL LINE
     C*
     C     XXMTOT        IFNE      0
     C     XXCTOT        DIV(H)    XXMTOT        XXPCNT
     C     XXPCNT        MULT      100           XXPLCY
     C                   MOVE      XXPLCY        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXPLCY
     C                   END
     C                   MOVE      '00'          XXPLCY
     C                   ELSE
     C                   Z-ADD     0             XXPLCY
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTAL
     C*
     C*   IF EMP# & DIV WAS INITIALLY = 0 THEN CONTINUE READING
     C*
     C  N20              MOVE      '1'           *IN61
     C   20*IN62         IFEQ      '1'
     C                   MOVE      '1'           *IN61
     C                   ELSE
     C                   MOVE      '0'           *IN60
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN63         IFEQ      '0'
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*   END OF PROGRAM
     C                   MOVE      '1'           *INLR
     C******************************************************
     C*    -SUBROUTINE TO LOAD THE CITY, STATE, AND ZIP    *
     C******************************************************
     CSR   SR2ND         BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSTZ           40
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
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
     O*  MLS 1/20/88  ADD INDICATORS
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/20/88  ADD INDICATORS
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
     O***** BEGIN CHG **** 11/23/99 ***********************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END   CHG **** 11/23/99 ***********************************
      ***** Beg Del ***** 08/26/14 *************************************************
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
     O*  MLS 1/20/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O*
     O*  SELECTION CRITERIA ADDED TO ROUTING PAGE
     O*
     O          E            ROUTE          2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           22 'GROUP  . . . . :'
     O                       FNFND         Z     26
     O                       FNNAME            +  2
     O          E            ROUTE          1
     O                                           22 'POLICY YEAR    :'
     O                       XXFYR               26
     O          E            ROUTE          1
     O                                           22 'EMPLOYER NUMBER:'
     O                       XXEMP#        Z     32
     O          E            ROUTE          1
     O                                           22 'DIVISION NUMBER:'
     O                       XXDIV         Z     32
     O*
     O*
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              10 '  :  :  '
     O                       CONAME              90
     O                       DSDATE        Y    125
     O          E            HEAD           1
     O                       Q1PGM               12
     O                       ORTITL              92
     O                                          114 'PAGE:'
     O                       PAGE          Z    125
     O          E            HEAD           2
     O                                           08 'GROUP:'
     O                       FNFND         Z     12
     O                       FNNAME              74
     O                                          121 'POLICY YEAR:'
     O                       XXFYR              125
     O          E            HEAD           1
     O                                            8 'EMPLR:'
     O                       XXEMP#        Z     18
     O                       XXDIV         Z     24
     O                       EMNAME              59
     O          E            HEAD           1
     O                       XXADR1              59
     O          E   N25      HEAD           1
     O              N25      XXADR2              59
     O          E            HEAD           2
     O                       XXCSZ               59
     O          E            HEAD           1
     O                                           88 'AVERAGE  CREDIT'
     O                                          124 'POLICY'
     O          E            HEAD           1
     O                                           29 'CLASS   QTRLY PAYROLL'
     O                                           38 'RATE'
     O                                           55 'MANUAL PREM'
     O                                           70 'HOURS WORKED'
     O                                           86 'HRLY WAGE    %'
     O                                          103 'CREDIT AMT'
     O                                          124 'CREDIT'
     O          E    30      HEAD2       1  2
     O               30                          23 'NON-CONTRACTING'
     O               30                          39 'CLASSIFICATIONS'
     O          E   N30      HEAD2       1  2
     O              N30                          19 'CONTRACTING'
     O              N30                          35 'CLASSIFICATIONS'
     O          E            TOTAL       1  1
     O                                           25 'TOTAL MAN PREM'
     O                                           40 '(ALL CLASSES):'
     O                       XXMTOT        J     56
     O                                           90 'TOTAL CREDIT:'
     O                       XXCTOT        J    104
     O                                          117 'PLCY CREDIT:'
     O                       XXPLCY        J    125
     O          E            DETAIL         1
     O                       CCCLAS        Z     13
     O                       CCPAMT        J     30
     O                       CCRT          J     39
     O                       CCMNPR        J     56
     O                       CCHRS         J     71
     O                       CCAHW         J     80
     O                       CCCRPC        J     89
     O                       CCCAMT        J    104
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC          1
     O                                           21 '*** NO RECORDS MEET'
     O                                           44 'SELECTION CRITERIA ***'
     O*
