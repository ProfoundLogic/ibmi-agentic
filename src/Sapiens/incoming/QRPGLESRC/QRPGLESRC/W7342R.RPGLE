     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7342R - AGENT LIST BY FUND - ALPHA SEQUENCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:      /  /               PROJECT:                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 10/01/98  C:35   PROJECT: SUPPORT WA072 13431                 *
     H*                  CHANGED SO CITY SELECTION IS NOT CASE-       *
     H*                  SENSITIVE.                                   *
     H*                                                               *
     H******************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   21  SELECT BY FIRST 3 POSITIONS OF ZIP CODE                 *
     H*   22  SELECT BY CITY NAME                                     *
     H*   23  SELECT ALL                                              *
     H*   24  SELECT ACTIVE AGENT ONLY                                *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMAGL1    IF   E           K DISK
     FWMAAL2    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AGPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  AGPHN1                 1      7  0
     D  XXPH11                 1      3  0
     D  XXPH12                 4      7  0
     D                 DS
     D  AGPHN2                 1      7  0
     D  XXPH21                 1      3  0
     D  XXPH22                 4      7  0
     D                 DS
     D  AGFAX#                 1      7  0
     D  ZZFAX1                 1      3  0
     D  ZZFAX2                 4      7  0
     D                 DS
     D  ZZRCID                 1     10
     D  ZZFND                  1      3
     D  ZZAGT#                 4      8
     D                 DS
     D  FRFPH#                 1     32
     D*
      **** BEGIN ADD **** 10/01/98 ************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      **** END   ADD **** 10/01/98 ************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQZIP             3
     C                   PARM                    QQQUE            10            *MLS 01-14-88
     C                   PARM                    QQMODE            1            *MLS 01-14-88
     C                   PARM                    QQSEL             1            REPORT SELECTION
     C                   PARM                    QQCITY           25            CITY NAME
     C                   PARM                    QQAAO             1            A, I OR B
     C                   PARM                    QQCAA             1            ACTIVE AGT ONLY
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
     C                   MOVE      QQZIP         XXZIP             3 0
     C                   MOVE      QQMODE        XXMODE            1            *MLS 01-14-88
     C*
     C*
     C*   PRINT ACTIVE AGENTS ONLY FLAG
     C*
     C     QQAAO         IFEQ      'Y'                                                      8
     C                   SETON                                        24
     C                   ELSE
     C                   SETOFF                                       24
     C                   END
     C*
     C*                                                          "
     C*   KEY LIST FOR THE USER PARAMETER FILE                   "
     C*                                                          "
     C     UPKEY         KLIST                                                        "
     C                   KFLD                    Q1USER                               "
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AGFEIN
     C                   KFLD                    AGMOD
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C                   TIME                    DSDTTM
     C*
     C     XXMODE        IFEQ      'D'                                           MLS 01-14-88
     C                   SETON                                        09         DEMAND MODE
     C                   ELSE                                                         "
     C                   SETON                                        08         BATCH MODE
     C                   END                                                          "
     C*
     C*   INDICATE REPORT SELECTION
     C*
     C     QQSEL         COMP      '1'                                    21     BY ZIP CODE
     C     QQSEL         COMP      '2'                                    22     BY CITY NAME
     C     QQSEL         COMP      *BLANK                                 23     ALL
     C                   Z-ADD     *ZERO         XXCNT             9 0           RECORD COUNT
     C*
     C*  KEY CITY NAME = THE CITY NAME UP TO THE LAST TYPED CHARACTER
     C*  PP = LENGTH OF KEY CITY NAME
     C*
     C   22              MOVE      *BLANK        XXCTY
     C   22' '           CHECKR    QQCITY        PP                2 0
     C   22PP            SUBST     QQCITY        XXCTY            25
      **** BEGIN ADD **** 10/01/98 *********************************************
     C     LO:UP         XLATE(P)  XXCTY         XXCTY
      **** END   ADD **** 10/01/98 *********************************************
     C*                                                          "
     C*   CHAIN TO THE USER PARAMETER FILE                       "
     C*                                                          "
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
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      DSDATE        XXDAT1            6 0
     C*
     C                   Z-ADD     XXDAT1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C* CENTER TEXT IN FIELD CONAME
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXNAME           40
     C*
     C* CENTER TEXT IN FIELD ORTITL
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C                   SELECT
     C*
     C     QQAAO         WHENEQ    'B'
     C*
     C                   MOVE      *ON           *IN53
     C*
     C     QQAAO         WHENEQ    'A'                                                      B'
     C*
     C                   MOVE      *ON           *IN54
     C*
     C     QQAAO         WHENEQ    'I'                                                      B'
     C*
     C                   MOVE      *ON           *IN55
     C*
     C                   ENDSL
     C*
     C*
      ***** Beg Del ***** 08/26/14 ****************************************************
     C*************      EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ****************************************************
     C                   END
     C*
     C                   MOVE      '1'           *INOA
     C*
     C*   START READING THE AGENT MASTER FILE
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   GET THE AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   CHECK SELECTION CRITERIA
     C*
     C                   MOVE      'Y'           XPRINT            1
     C     QQSEL         IFEQ      '1'                                          BY ZIP CODE
     C*
     C*   CHECK THE FIRST THREE DIGITS IN ZIP CODE
     C*
     C                   MOVEL     XXZPP1        XXZPP             3 0
     C                   MOVEL     XXZPM1        XXZPM             3 0
     C     XXZIP         IFEQ      XXZPP
     C     XXZIP         OREQ      XXZPM
     C     XXZIP         OREQ      0
     C                   MOVE      'Y'           XPRINT
     C                   ELSE
     C                   MOVE      'N'           XPRINT
     C                   ENDIF                                                  IF XXZIP = XXZPP
     C                   ENDIF                                                  IF QQSEL = 1
     C*
     C* IF SELECT BY CITY, ONLY SELECT CITIES WITH FIRST PART = KEY CITY NAME
     C*
     C     QQSEL         IFEQ      '2'                                          BY CITY NAME
     C                   MOVE      *BLANK        XXMCTY
     C                   MOVE      *BLANK        XXPCTY
     C     PP            SUBST     AGMCTY        XXMCTY           25
     C     PP            SUBST     AGPCTY        XXPCTY           25
      **** BEGIN ADD **** 10/01/98 *********************************************
     C     LO:UP         XLATE(P)  XXMCTY        XXMCTY           25
     C     LO:UP         XLATE(P)  XXPCTY        XXPCTY           25
      **** END   ADD **** 10/01/98 *********************************************
     C     XXMCTY        IFNE      XXCTY
     C     XXPCTY        ANDNE     XXCTY
     C                   MOVE      'N'           XPRINT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  ONLY PRINT SELECTED AGENT
     C*
     C                   MOVE      'N'           XPRT1             1
     C                   MOVE      *OFF          *IN26
     C*
     C     QQCAA         IFEQ      'N'
     C     AACNDT        ANDNE     *ZERO
     C*
     C                   MOVE      *ON           *IN26
     C*
     C                   ENDIF                                                  QQCAA IFEQ 'N'
     C*
     C     XPRINT        IFEQ      'Y'
     C     *IN26         ANDEQ     *OFF
     C*
     C                   SELECT
     C*
     C     QQAAO         WHENEQ    'B'
     C*
     C                   MOVE      'Y'           XPRT1             1
     C                   ADD       1             XXCNT
     C*
     C     QQAAO         WHENEQ    'I'
     C*
     C     AASTAT        IFEQ      'I'
     C*
     C                   MOVE      'Y'           XPRT1             1
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  AASTAT IFEQ 'I'
     C*
     C     QQAAO         WHENEQ    'A'
     C*
     C     AASTAT        IFEQ      'A'
     C*
     C                   MOVE      'Y'           XPRT1             1
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  AASTAT IFEQ 'A'
     C*
     C                   ENDSL
      *
      *                    *** IF PRINT AGENT
     C     XPRT1         IFEQ      'Y'
      *
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGMST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGMCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXMCSZ           40
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGPST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGPCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXPCSZ           40
     C*
     C                   MOVE      AAFND         ZZFND
     C                   MOVE      AAAGT#        ZZAGT#
     C                   MOVE      '0'           *IN61
     C     AACNDT        IFEQ      *ZERO
     C                   Z-ADD     *ZERO         XXCNDT            6 0    61    IND. TO STP PRT
     C                   ELSE
     C                   Z-ADD     AACNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCNDT            6 0
     C                   END
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C                   ENDIF                                                  IF PRINT AGENT
     C                   END
     C                   END
     C     AGKEY         READE     PAG01                                  60
     C                   END
     C*
     C*   PRINT ERROR MESSAGE IF NO RECORD FOUND FOR SELECTION CRITERIA.
     C*
     C     XXCNT         IFEQ      *ZERO
     C                   EXCEPT    ERRMSG
     C                   END
      /Free
       // ***** Beg Add ***** 08/26/14 ************************************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ************************************************
      /End-free
     C*
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ****************************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ****************************************************
      ***** Beg Add ***** 08/05/15 **********************************
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
      ***** End Add ***** 08/05/15 **********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ****************************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ****************************************************
     O          E            ROUTE       2  2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          2
     O                                           14 'COMPANY:'
     O                       XXCO#               35
     O          E            ROUTE          2
     O                                           14 'GROUP:  '
     O                       XXFND               35
     O          E            ROUTE          2
     O                                           16 'SELECT BY:'
     O               21                          53 'FIRST 3 POSITIONS OF '
     O               21                          62 'ZIP CODE:'
     O               21      QQZIP               66
     O               22                          42 'CITY NAME:'
     O               22      QQCITY              68
     O               23                          37 '* ALL'
     O          E            ROUTE          2
     O                                           17 'SORT ORDER:'
     O                                           42 'ALPHABETIC'
     O          E            ROUTE          2
     O                                           19 'AGENT STATUS:'
     O               53                          56 'BOTH ACTIVE AND INACTIVE'
     O               54                          43 'ACTIVE ONLY'
     O               55                          45 'INACTIVE ONLY'
     O          E            ROUTE          2
     O                                           23 'INCLUDE CANCELLED'
     O                                           31 ' AGENTS:'
     O                       QQCAA               33
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                      *YEAR     51
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXNAME              86
     O                       XXDATE             132 '  /  /    '
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            7 'SELECT:'
     O               21                          31 'BY FIRST 3 POSITIONS OF'
     O               21                          41 'ZIP CODE:'
     O               21      QQZIP               45
     O               22                          21 'BY CITY NAME:'
     O               22      QQCITY              47
     O               23                          13 '* ALL'
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     09
     O                       FNNAME              70
     O          E            HEAD           2
     O                                           05 'AGT#'
     O                                           11 'AGENT'
     O                                           16 'NAME'
     O                                           55 'MAILING'
     O                                           63 'ADDRESS'
     O                                           99 'PHYSICAL'
     O                                          107 'ADDRESS'
     O                                          119 'STS'
     O                                          123 'CX'
     O                                          128 ' DATE'
     O          E            DET            1
     O                       AAAGT#        Z     05
     O                       AGNAME              46
     O                       AGMAD1              88
     O                       AGPAD1             131
     O          E            DET            1
     O                                           22 'FEDERAL TAX ID:'
     O                       AGFEIN              32
     O                       XXMCSZ              88
     O                       XXPCSZ             131
     O          E            DET            1
     O                                           12 'PHONE'
     O                                           22 'NUMBER 1:'
     O                       AGARE1              28 '0(   )'
     O                       XXPH11              33
     O                                           34 '-'
     O                       XXPH12              38
     O                                           53 'PHONE'
     O                                           63 'NUMBER 2:'
     O                       AGARE2              69 '0(   )'
     O                       XXPH21              73
     O                                           74 '-'
     O                       XXPH22              79
     O                       AASTAT             118
     O                       XXCNDT             128 '  /  /  '
     O          E            DET            2
     O                                           10 'FAX'
     O                                           22 'NUMBER    :'
     O                       AGFAXA              28 '0(   )'
     O                       ZZFAX1              33
     O                                           34 '-'
     O                       ZZFAX2              38
     O          E            ERRMSG      3  1
     O                                           33 '*** NO RECORDS MEET THE'
     O                                           56 'SELECTED CRITERIA. ***'
