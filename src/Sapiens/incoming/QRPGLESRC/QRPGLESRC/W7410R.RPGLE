     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7410R - CERTIFICATE OF SELF-INSURANCE
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                PRINT EXTENDED EMPLR NAME FIELDS.              *
     H*                                                               *
     H* 01/24/01 C:61  SUPPORT WA113 26019                            *
     H*                Reverse change made 03/26/98.  Use mailing     *
     H*                address if physical address not entered.       *
     H*                                                               *
     H* 02/04/00  C:35   PROJECT: SUPPORT WA109 #21738                *
     H*                  PRINT CARRIER POLICY NUMBER IF IT EXISTS.    *
     H*                                                               *
     H* 12/14/99  C:35   PROJECT: SUPPORT WA033 #21006                *
     H*           C:118  MOVE ARRAY VALUE BEFORE INCREASING INDEX.    *
     H*                                                               *
     H* 03/29/99  C:35   PROJECT: SUPPORT WA051 #17148                *
     H*                  PRINT LATEST POLICY YEAR AS PART OF POLICY   *
     H*                  NUMBER.                                      *
     H*                                                               *
     H* 08/10/98  C:65  PROJECT: WC577                                *
     H*                 PRINT OWNERS ON ADDITIONAL PAGE ONLY SINCE    *
     H*                 IT IS POSSIBLE THEY CAN'T FIT ON THE CERT.    *
     H*                                                               *
     H* 08/06/98  C:61  PROJECT: 48WLG                                *
     H*               - Print alternate company name/address if it    *
     H*                 is entered in the group master, otherwise     *
     H*                 print the company physical address.           *
     H*                                                               *
     H* 03/26/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE TO ALWAYS PRINT PHYSICAL ADDRESS.      *
     H*                                                               *
     H* 03/09/98  C:65  PROJECT: V3                                   *
     H*                 GENERAL CLEANUP                               *
     H*                                                               *
     H*****************************************************************
      *
      *
     FWMEML     IF   E           K DISK
     FWMOOL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)
     FWMFNL     IF   E           K DISK
      **** BEGIN ADD **** 03/29/99 *********************************************
     FWDELL     IF   E           K DISK
      **** END   ADD **** 03/29/99 *********************************************
      **** BEGIN ADD **** 02/04/00 *********************************************
     FWMRPL     IF   E           K DISK
      **** END   ADD **** 02/04/00 *********************************************

     D*
     D*** BEGIN DEL ***  08/10/98  ************************************
     D*****               OL          7 30  A            LAST NAME
     D*****               OF          7 15  A            FIRST NAME
     D*****               OP          7  5 2A            PERCENTAGE
     D*** END DEL *****  08/10/98  ************************************
     D ZIP             S              1    DIM(10) ASCEND                       ZIP CODE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D                 DS
     D  EMMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD5               001    007  0
     D  EMEFD4               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD2               022    028  0
     D  EMEFD1               029    035  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  KEY LIST FOR EMPLOYER MASTER FILE
      *

     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
      *  KEY LIST FOR OWNER/OFFICER FILE
      *
     C     OOKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXELID
      *
      *  KEY LIST FOR COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *  KEY LIST FOR FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** BEGIN ADD **** 03/29/99 *********************************************
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      **** END   ADD **** 03/29/99 *********************************************
      **** BEGIN ADD **** 02/04/00 *********************************************
      *
      * CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    ELFYR
      **** END   ADD **** 02/04/00 *********************************************
      *
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXDTE
     C     *LIKE         DEFINE    EMDT          XXCDAT
     C     *LIKE         DEFINE    EMDT          XXEFFD
     C     *LIKE         DEFINE    OOELID        XXELID
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      *BLANKS       XXELID
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *ZEROS        XXCDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****                MOVE *ALL'0'   OP
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C*** END CHG *****  08/10/98  ************************************
     C                   MOVE      *ALL'0'       ED
      *
      *   CHAIN TO COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      ***** BEG ADD ******* 08/06/98 **********************************
      *
      *   Use alternate company name if entered in fund master
      *
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C     FNANAM        ANDNE     *BLANKS
     C     FNACTY        ANDNE     *BLANKS
      *
     C                   MOVEL(P)  FNANAM        XXCONM           24
     C                   MOVEL(P)  FNACTY        XXLAST
     C                   MOVEL(P)  FNAST         XXFRST
      *
     C                   ELSE
      *
      ***** END ADD ******* 08/06/98 **********************************
      *
      *   REMOVE LEADING BLANKS FROM COMPANY NAME
      *
     C                   Z-ADD     1             Z                 2 0
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM           24                  "
      *                                                          "
      *  Combine the company city and state                      "
      *                                                          "
     C                   MOVE      *BLANKS       XXLAST                               "
     C                   MOVE      *BLANKS       XXFRST                               "
     C                   MOVEL     COCTY         XXLAST                               "
     C                   MOVEL     COST          XXFRST                               "
      ***** BEG ADD ******* 08/06/98 **********************************
      *
     C                   ENDIF                                                  COMPANY NAME
      *
      ***** END ADD ******* 08/06/98 **********************************
     C                   MOVE      '1'           XXTERM            1                  "
     C                   CALL      'SRNAME'                                           "
     C                   PARM                    XXLAST                               "
     C                   PARM                    XXFRST                               "
     C                   PARM                    XXTERM                               "
     C                   MOVEL     XXLAST        XXCOCT           25                  "
      *
      *   START READING EMPLOYER MASTER FILE
      *
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
      **** BEGIN ADD **** 03/29/99 *********************************************
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  61
      **** END   ADD **** 03/29/99 *********************************************
      **** BEGIN ADD **** 02/04/00 *********************************************
     C     *IN61         IFEQ      *OFF
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C     RPKY          CHAIN     PRP01                              62
      *
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDEQ     *BLANKS
      *
     C                   MOVE      *ON           *IN62
      *
     C                   ENDIF                                                  *IN62 IFEQ *OFF
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN62
      *
     C                   ENDIF                                                  *IN61 IFEQ *OFF
      **** END   ADD **** 02/04/00 *********************************************
     C*** BEGIN ADD ***  03/07/05  ************************************
     C                   MOVEL(P)  EMNAME        XXNAM1           50
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXNAM1
     C                   MOVE(P)   EMNAM2        XXNAM2           50
     C*** END ADD *****  03/07/05  ************************************
      *
      *   CHECK WHICH ADDRESS TO USE
      *
     C                   MOVE      *BLANKS       XXZIP1            5
     C                   Z-ADD     0             XXZIP2            4 0
     C                   MOVE      *BLANKS       XXADR1           40
     C                   MOVE      *BLANKS       XXSTAT            2
     C                   MOVE      *BLANKS       XXCITY           23
      **** BEGIN DEL **** 03/26/98 *********************************************
      ********** EMMPFL    IFEQ 'M'
      **********
      ******************** MOVE EMMAD1    XXADR1
      ******************** MOVE EMMST     XXSTAT  2
      ******************** MOVE XXZPM1    XXZIP1  5
      ******************** MOVE XXZPM2    XXZIP2  40
      ******************** MOVELEMMCTY    XXCITY 23
      ********************
      ******************** ELSE
      **** END   DEL **** 03/26/98 *********************************************
      ***** BEG ADD ******* 01/24/01 **********************************
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVE      EMMAD1        XXADR1
     C                   MOVE      EMMST         XXSTAT            2
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4 0
     C                   MOVEL     EMMCTY        XXCITY           23
      *
     C                   ELSE
      ***** END ADD ******* 01/24/01 **********************************
      *
     C                   MOVE      EMPAD1        XXADR1
     C                   MOVE      EMPST         XXSTAT            2
     C                   MOVE      XXZPP1        XXZIP1            5
     C                   MOVE      XXZPP2        XXZIP2            4 0
     C                   MOVEL     EMPCTY        XXCITY           23
      *
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   ENDIF                                                  EMPAD1
      ***** END ADD ******* 01/24/01 **********************************
      **** BEGIN DEL **** 03/26/98 *********************************************
      ******************** END
      **** END   DEL **** 03/26/98 *********************************************
     C                   MOVE      *BLANKS       ZIP
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXZIP1        XXZP1            10
     C                   MOVEA     XXZP1         ZIP
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           ZIP(6)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         ZIP(7)
     C                   END
      *
     C                   MOVE      *BLANKS       XXZP             10
     C                   MOVEA     ZIP           XXZP
      *
      *   GET THE EFFECTIVE DATE
      *
     C                   Z-ADD     1             X                 1 0
     C     *ZERO         LOOKUP    ED(X)                              60
     C                   MOVE      ED(X)         XXEFFD
     C     X             IFNE      5                                            SAJ 4-22-88
     C     X             DOWNE     6
     C     XXCDAT        COMP      ED(X)                              60  60
      **** BEGIN CHG **** 12/14/99 *********************************************
     C******* N60          ADD  1         X
     C******* N60          MOVE ED,X      XXEFFD
     C  N60              MOVE      ED(X)         XXEFFD
     C  N60              ADD       1             X
      **** END   CHG **** 12/14/99 *********************************************
     C  N60              END
     C                   END                                                    SAJ 4-22-88
      *
     C                   Z-ADD     0             XXEDTE            8 0
     C                   Z-ADD     XXEFFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEDTE
     C                   EXCEPT    CERT
      *
      *  Get Fund Name & Address                           LJQ 6-27-88
      *                                                          "
     C                   MOVE      *BLANKS       XXFAD1           40                  "
     C                   MOVE      *BLANKS       XXFCTY           52                  "
     C                   MOVE      *BLANKS       XXLAST                               "
     C                   MOVE      *BLANKS       XXFRST                               "
     C                   MOVE      *BLANKS       XXSTZP           13                  "
     C                   MOVE      *BLANKS       XXZIP4            4                  "
     C                   MOVE      *BLANKS       XX5               5                  "
     C                   MOVE      *BLANKS       XXST10           10                  "
      *                                                          "
     C     FNKEY         CHAIN     PFN01                              60              "
     C     *IN60         IFEQ      '0'                                                "
     C                   MOVEL     FNMAD1        XXFAD1                               "
     C                   MOVEL     FNMCTY        XXLAST                               "
     C                   MOVEL     FNMST         XXSTZP                               "
     C                   MOVE      FNMZIP        XXSTZP                               "
     C                   MOVE      XXSTZP        XXZIP4                               "
     C     XXZIP4        IFEQ      *ZERO                                              "
     C                   MOVE      '    '        XXSTZP                               "
     C                   ELSE                                                         "
     C                   MOVEL     '-'           XX5                                  "
     C                   MOVE      XXZIP4        XX5                                  "
     C                   MOVEL     FNMZIP        XXST10                               "
     C                   MOVE      XX5           XXST10                               "
     C                   MOVE      XXST10        XXSTZP                               "
     C                   END                                                          "
      *                                                          "
     C                   MOVEL     XXSTZP        XXFRST                               "
     C                   MOVE      '1'           XXTERM                               "
     C                   CALL      'SRNAME'                                           "
     C                   PARM                    XXLAST                               "
     C                   PARM                    XXFRST                               "
     C                   PARM                    XXTERM                               "
     C                   MOVEL     XXLAST        XXFCTY                               "
     C                   END                                                          "
     C                   EXCEPT    CERT1                                              "
      *
      *   MOVE TYPE OF BUSINESS TO OUTPUT FIELD
      *
     C     EMTPOB        IFEQ      '1'
     C                   EXCEPT    CERT2
     C                   ELSE
     C     EMTPOB        IFEQ      '2'
     C                   EXCEPT    CERT3
     C                   ELSE
     C     EMTPOB        IFEQ      '3'
     C                   EXCEPT    CERT4
     C                   ELSE
     C                   EXCEPT    CERT5
     C                   END
     C                   END
     C                   END
      *
      *   GET FIRST AND LAST NAMES OF LEGAL OWNERS
      *
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****      OOKY      SETLLPOO01
      *
     C*****                Z-ADD1         Y       10
     C*****      Y         DOWNE8
      *
     C**N50      OOKY      READEPOO01                    50
     C*****      *IN50     IFEQ *ON
     C*****                MOVE *BLANKS   OOOLSN
     C*****                MOVE *BLANKS   OOOFSN
     C*****                Z-ADD0         OOOFPC
     C*****                ENDIF
      *
     C*****      OOOFPC    IFNE *ZEROS
     C*****      OOOLSN    ANDNE*BLANKS
     C*****      OOOFSN    ANDNE*BLANKS
      *
     C*****                MOVE *BLANKS   XXLAST 56
     C*****                MOVE *BLANKS   XXFRST 15
     C*****                MOVELOOOLSN    XXLAST
     C*****                MOVELOOOFSN    XXFRST
     C*****                MOVE '1'       XXTERM  1
     C*****                CALL 'SRNAME'
     C*****                PARM           XXLAST
     C*****                PARM           XXFRST
     C*****                PARM           XXTERM
     C*****                MOVELXXLAST    XXOFNM 30
      *
     C*****                EXCPTCERT6
     C*****                END
      *
     C*****                ADD  1         Y
     C*****                END
     C     OOKY          CHAIN     POO01                              50
     C     *IN50         IFEQ      *OFF
     C                   EXCEPT    CERT7
     C                   ENDIF                                                  *IN50 IFEQ
     C*** END CHG *****  08/10/98  ************************************
      *
      *  CHECK IF MORE THAN ONE DIVISION EXISTS
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
      *
     C     EMDIV         IFNE      XXDIV
     C                   EXCEPT    ATTCHD
     C                   GOTO      TERM
     C                   END
      *
     C     EMKY1         READE     PEM01                                  60
     C                   END
      *
     C                   END
      *
     C     TERM          TAG                                                    ADD 09/02/93
     C                   MOVE      '1'           *INLR
      *
     O*** BEGIN CHG ***  03/07/05  ************************************
     O******QSYSPRT E  311           CERT
     O******                         EMNAME    44
     OQSYSPRT   E            CERT           1 11
     O                       XXNAM1              54
     O          E            CERT           3
     O                       XXNAM2              54
     O*** END CHG *****  03/07/05  ************************************
     O          E            CERT           2
     O                       EMFEIN              13
     O          E            CERT           2
     O                       XXADR1              44
     O                       XXCITY              80
     O          E            CERT           2
     O                       XXSTAT              06
     O                       XXZP                67
     O          E            CERT           1
     O                       XXEDTE              41 '  /  /    '
     O          E            CERT        1  0
     O                                           45 'X'
     O          E            CERT           2
      **** BEGIN CHG **** 02/04/00 *********************************************
     O************************ EMFND     07
      **** BEGIN CHG **** 03/29/99 *********************************************
     O********************************** 08 '-'
     O************************ EMEMP#    17
     O************************ EMEMP#    16
     O*************** N61      ELFYR     19
      **** END   CHG **** 03/29/99 *********************************************
     O               62      EMFND               07
     O               62      EMEMP#              16
     O               62N61   ELFYR               19
     O              N62      RPCAP#              29
      **** END   CHG **** 02/04/00 *********************************************
     O          E            CERT1          1
     O                       FNNAME              64
     O          E            CERT1          1
     O                       XXFAD1              57
     O          E            CERT1          2
     O                       XXFCTY              57
     O          E            CERT1          2
     O                       XXCONM              54
     O                       XXCOCT              80
     O          E            CERT1          1
     O                       EMNAOB              71
     O*
     O*** BEGIN CHG ***  03/07/05  ************************************
     O******* E   3236         CERT2
     O          E            CERT2            33 37
     O*** END CHG *****  03/07/05  ************************************
     O                                           20 'X'
     O                                           21 'X'
     O*** BEGIN CHG ***  03/07/05  ************************************
     O******* E   3236         CERT3
     O          E            CERT3            33 37
     O*** END CHG *****  03/07/05  ************************************
     O                                           37 'X'
     O                                           38 'X'
     O*** BEGIN CHG ***  03/07/05  ************************************
     O******* E   3236         CERT4
     O          E            CERT4            33 37
     O*** END CHG *****  03/07/05  ************************************
     O                                           56 'X'
     O                                           57 'X'
     O*** BEGIN CHG ***  03/07/05  ************************************
     O******* E   3236         CERT5
     O          E            CERT5            33 37
     O*** END CHG *****  03/07/05  ************************************
     O                                           74 'X'
     O                                           75 'X'
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****   E  1             CERT6
     O*****                    XXOFNM    35
     O*****                    OOOFPC4   42
     O*****                              44 '%'
     O          E            CERT7          1
     O                                           40 'SEE ADDITIONAL A'
     O                                           56 'TTACHED SHEET(S)'
     O*** END CHG *****  08/10/98  ************************************
     O*** BEGIN CHG ***  03/07/05  ************************************
     O******* E   58           ATTCHD
     O          E            ATTCHD           59
     O*** END CHG *****  03/07/05  ************************************
     O                                           40 'SEE ADDITIONAL A'
     O                                           56 'TTACHED SHEET(S)'
