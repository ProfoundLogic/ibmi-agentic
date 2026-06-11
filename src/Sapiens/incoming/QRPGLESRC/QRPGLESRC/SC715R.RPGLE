     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H* TITLE SC715R - REMARKS PRINT, DIARY ITEMS ONLY - PRINT PROGRAM*
     H*  02/08/92 LJD   PROJ: SC088 - Revamp of Diary Report SC715R   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  03/23/92 WLD   Proj: AJP                                     *
     H*                 1.  Changed 2A Remark Code to 5A.             * ARPNAM
     H*                                                               *
     H*  06/22/92 KRR   PROJ:                                         *
     H*                 1.  CHANGE TO USE THE OUTPUT ROUTING RECORD   *
     H*                                                               *
     H*  05/11/93 C:12  PROJ:                                         *
     H*                 1.  FOR ALL USERS,                            *
     H*                     IF SORT SEQUENCE IS IN USER ORDER         *
     H*                     THE REPORT NOW BREAKS AND                 *
     H*                     PRINTS THE USER NAME AT THE TOP           *
     H*                                                               *
     H*  12/21/93 C:20  PROJ: DRMNT                                   *
     H*                 1.  REVISE PRINTING TO PRINT FULL WCMC KEY    *
     H*                                                               *
     H*  02/01/94 C:25  PROJ: DR002                                   *
     H*                 1.  ADDED SUMMARY CODE TO ROUTING SHEET       *
     H*                                                               *
     H*  11/14/95 C:33  PROJ: DRMNT                                   *
     H*                 FIX OVERFLOW PROBLEM WITH ADDITIONAL REMARKS  *
     H*                                                               *
     H*  02/19/96 C:03  PROJ: SUPPORT                                 *
     H*                 PRINT NAME FROM SRKCVT1 (*IN71 ON OUTPUT RMVD)*
     H*                                                               *
     H*  07/22/96 C:53  PROJ: SUPPORT DR053 #3824                     *
     H*                 CHANGE TRUNCATION                             *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*       70      CONDITIONAL OUTPUT ON USER                      *
     H*       71      CONDITIONAL OUTPUT ON KYDF FIELD (I.E. NAME)    *
      *****************************************************************
     FSDCML9    IF   E           K DISK
     FSMARL     IF   E           K DISK
     FSMSLL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FQSYSPRT   O    F   95        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

     D SRT1            S              2    DIM(8) CTDATA PERRCD(1)              SORT SEQUENCE TITLES
     D SRT2            S             25    DIM(8) ALT(SRT1)
     D DIT1            S              1    DIM(3) CTDATA PERRCD(1)              DIARY TYPE TITLES
     D DIT2            S             50    DIM(3) ALT(DIT1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  ADDITIONAL REMARKS FILE
     C     ARKY          KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C                   KFLD                    HLASQ#
     C*
     C     ARKY2         KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C*
     C*   SECURITY FILE
     C     SLKY          KLIST
     C                   KFLD                    $$USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQUSER           10
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C                   PARM                    QQSMCD            4
     C                   PARM                    QQRMTY            5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDITY            1
     C                   PARM                    QQSORT            2
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    CMKEY         RSKEY
     C     *LIKE         DEFINE    CMUSER        RSUSER
     C     *LIKE         DEFINE    CMUSER        XXUSER
     C     *LIKE         DEFINE    CMDCID        RSDCID
     C     *LIKE         DEFINE    ARKEY         HLKEY
     C     *LIKE         DEFINE    ARSEQ#        HLSEQ#
     C     *LIKE         DEFINE    ARASQ#        HLASQ#
     C     *LIKE         DEFINE    ARCM75        XXCM75
     C     *LIKE         DEFINE    Q1USER        $$USER
     C     *LIKE         DEFINE    CMUSER        SVUSER
     C                   MOVE      *BLANKS       XXKEY            57            CHG 07/22/96
     C*
     C                   MOVE      *BLANKS       XXSORT            1
     C                   MOVEL     QQSORT        XXSORT
     C     QQUSER        IFNE      *BLANKS
     C                   MOVEL     QQUSER        XXUSER
     C                   MOVE      '1'           *IN70
     C                   ELSE                                                   ELS QQUSER NE *BLANK
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVE      '0'           *IN70
     C                   END                                                    END QQUSER NE *BLANK
     C*
     C* Establish Routing Sheet Information
     C                   MOVE      *BLANK        RSKEY
     C                   MOVE      *BLANK        RSKYDF
     C                   MOVE      *BLANK        RSUSER
     C                   MOVE      *BLANK        RSFRDT
     C                   MOVE      *BLANK        RSTODT
     C                   MOVE      *BLANK        RSSMCD            4
     C                   MOVE      *BLANK        RSRMTY            5
     C                   MOVE      *BLANK        RSDCID
     C                   MOVE      *BLANK        RSDITY           50
     C                   MOVE      *BLANK        RSSORT           25
     C     QQKEY         IFNE      *BLANK
     C                   MOVEL     QQKEY         RSKEY
     C                   MOVE      '1'           *IN71
     C                   ELSE
     C                   MOVEL     '*ALL'        RSKEY
     C                   MOVE      '0'           *IN71
     C                   END
     C     QQUSER        IFNE      *BLANK
     C                   MOVEL     QQUSER        RSUSER
     C                   ELSE
     C                   MOVEL     '*ALL'        RSUSER
     C                   END
     C     QQFRDT        IFEQ      '0000101'
     C     QQTODT        ANDEQ     '1991231'
     C                   MOVE      '1'           *IN71
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   MOVEL     QQFRDT        RSFRDT            7 0
     C*
     C                   Z-ADD     RSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RXFRDT            8 0
     C*
     C                   MOVEL     QQTODT        RSTODT            7 0
     C*
     C                   Z-ADD     RSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RXTODT            8 0
     C                   END
     C*
     C     QQSMCD        IFNE      *BLANK
     C                   MOVEL     QQSMCD        RSSMCD
     C                   ELSE
     C                   MOVEL     '*ALL'        RSSMCD
     C                   END
     C     QQRMTY        IFNE      *BLANK
     C                   MOVEL     QQRMTY        RSRMTY
     C                   ELSE
     C                   MOVEL     '*ALL'        RSRMTY
     C                   END
     C     QQDCID        IFNE      *BLANK
     C                   MOVEL     QQDCID        RSDCID
     C                   ELSE
     C                   MOVEL     '*ALL'        RSDCID
     C                   END
     C                   Z-ADD     1             X                 5 0
     C     QQSORT        LOOKUP    SRT1(X)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     SRT2(X)       RSSORT
     C                   END
     C                   Z-ADD     1             X                 5 0
     C     QQDITY        LOOKUP    DIT1(X)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     DIT2(X)       RSDITY
     C                   END
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   READ      PCM01                                  50
     C*
     C*** Build Key Definition here
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      *BLANKS       X#NAME
     C                   MOVE      ' '           X#RTCD
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#NAME           79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY
     C     RSKEY         IFNE      '*ALL'
     C                   MOVEL     X#KEY         RSKEY
     C                   END
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*
     C                   MOVEL     X#NAME        XXNAME           40
     C     RSKEY         IFNE      '*ALL'
     C                   MOVEL     X#NAME        RSKYDF           40
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C********           MOVE      '0'           XXPRT
     C********      ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C********     *IN60         IFEQ      '0'
     C********     ORSTAT        ANDEQ     'A'
     C********     Q1USER        CHAIN     PUP01                              60
     C********           EXCEPT    ROUTE
     C********           END
      ***** End Del ***** 08/26/14 ************************************
      * DON'T PRINT THE HEAD IF REPORT IS IN USER SEQUENCE AND
      * QQUSER IS *BLANKS.
     C     XXSORT        IFNE      'U'
     C     QQUSER        ORNE      *BLANKS
      *
     C                   EXCEPT    HEAD
     C                   ENDIF
     C*
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      *BLANKS       SVUSER
     C     *IN50         DOWEQ     '0'
     C*
     C* ================================================================
     C* Only print remarks that this user has authority to
     C                   MOVEL     Q1USER        $$USER
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       $$USER
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END                                                    END 1 *IN60 - *BLANK
     C                   END                                                    END 1 *IN60 - Q1USER
     C*
     C     CMSECL        IFLE      SLSECL
     C* ================================================================
     C* Security passed - continue with remark
     C*
     C*** Build Key Definition here
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      *BLANKS       X#NAME
     C                   MOVE      ' '           X#RTCD
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#NAME           79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*
     C                   MOVEL     X#NAME        XXNAME           40
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCDAT            6 0
     C*
     C                   Z-ADD     *ZEROS        XXDCDT
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDCDT            6 0
     C*
     C                   MOVE      *BLANKS       X1COMM
     C                   MOVE      *BLANKS       X2COMM
     C                   MOVE      *BLANKS       XXCM1
     C                   MOVE      *BLANKS       XXCM2
     C                   MOVE      *BLANKS       XXCM3
     C                   MOVE      *BLANKS       XXCM4
     C*
     C                   MOVEL     CMCOMM        X1COMM          150
     C                   MOVE      CMCOMM        X2COMM          125
     C*
     C                   MOVEL     X1COMM        XXCM1            75
     C                   MOVE      X1COMM        XXCM2            75
     C*
     C                   MOVEL     X2COMM        XXCM3            75
     C                   MOVE      X2COMM        XXCM4            50
     C*
      * PRINT THE HEAD NOW IF REPORT IS IN USER SEQUENCE AND
      * QQUSER IS *BLANKS.
     C     XXSORT        IFEQ      'U'
     C     QQUSER        ANDEQ     *BLANKS
     C     SVUSER        ANDNE     CMUSER
     C                   MOVE      *BLANKS       SVUSER
     C                   MOVE      CMUSER        SVUSER
     C                   Z-ADD     0             PAGE
      *
     C                   EXCEPT    HEAD
     C                   ENDIF
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET1
     C                   EXCEPT    DET2
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C     XXCM2         IFNE      *BLANKS
     C                   EXCEPT    DET3
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C     XXCM3         IFNE      *BLANKS
     C                   EXCEPT    DET4
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C     XXCM4         IFNE      *BLANKS
     C                   EXCEPT    DET5
     C                   END
     C*
     C* Get Additional Remarks and Print them
     C*
     C                   MOVE      '0'           XXONE
     C                   MOVE      *BLANKS       HLKEY
     C                   MOVE      *ZEROS        HLSEQ#
     C                   MOVE      *ZEROS        HLASQ#
     C                   MOVEL     CMKEY         HLKEY
     C                   MOVE      CMSEQ#        HLSEQ#
     C     ARKY          SETLL     PAR01
     C     ARKY2         READE     PAR01                                  61
     C     *IN61         DOWEQ     '0'
     C*
     C     XXONE         IFEQ      '0'
     C     ARCM75        ANDNE     *BLANKS
     C                   MOVE      '1'           XXONE             1
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET6
     C                   ELSE                                                    IFEQXXONE
     C*
     C     ARCM75        IFNE      *BLANKS
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END                                                    IFEQ*INOF
     C                   EXCEPT    DET7
     C*
     C                   END                                                    IFEQXXCM75
     C                   END                                                    IFEQXXONE
     C*
     C     ARKY2         READE     PAR01                                  61
     C                   END                                                    *IN61DOWEQ
     C*
     C                   END                                                    END CMSECL LE SLSECL
     C*
     C                   READ      PCM01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found and orStat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O*************************************************************************
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
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          1
     O                                           20 'KEY          :'
     O                       RSKEY               61
     O          E            ROUTE          2
     O                       RSKYDF              61
     O*
     O          E            ROUTE          2
     O                                           20 'USER         :'
     O                       RSUSER              31
     O*
     O          E            ROUTE          2
     O               71                          25 '*ALL'
     O                                           20 'DATE         :'
     O              N71      RXFRDT              31 '  /  /    '
     O              N71                          34 'TO'
     O              N71      RXTODT              45 '  /  /    '
     O*
     O          E            ROUTE          2
     O                                           20 'SUMMARY CODE :'
     O                       RSSMCD              25
     O          E            ROUTE          2
     O                                           20 'REMARK TYPE  :'
     O                       RSRMTY              26
     O*
     O          E            ROUTE          2
     O                                           20 'DOCUMENT ID  :'
     O                       RSDCID              33
     O*
     O          E            ROUTE          2
     O                                           20 'DIARY REMARKS:'
     O                       RSDITY              71
     O*
     O          E            ROUTE          1
     O                                           20 'SORT SEQUENCE:'
     O                       RSSORT              46
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                             +  1 'INC. 1992 - 1996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE        Y      8
     O                                           43 'DATABASE AND REMARKS'
     O                                           59 'TRACKING SYSTEM'
     O                       DSTIME              80 '  :  :  '
     O          E            HEAD           2
     O                                            6 'SC715R'
     O                                           40 'REMARKS LISTING'
     O                                           54 '- DIARY ITEMS'
     O                                           75 'PAGE:'
     O                       PAGE                80
     O          E            HEAD           2
     O                                            5 'USER:'
     O               70      QQUSER              16
     O              N70      CMUSER              16
      *
     O          E            HEAD           1
     O                                           07 'DIARY'
     O                                           75 'DIARY'
     O                                           80 'RMK'
     O          E            HEAD           1
     O                                           13 'KEY'
     O                                           06 'DATE'
     O                                           75 'CMPLTD'
     O                                           81 'TYPE'
     O                                           94 'DOCUMENT ID'
     O          E            DET1        1  1
     O                       XXCDAT              08 '  /  /  '
     O                       XXKEY               67
     O                       XXDCDT              76 '  /  /  '
     O                       CMRMCD              82
     O                       CMDCID              95
     O          E            DET1           1
     O                       XXNAME              51
     O          E            DET2           1
     O                       XXCM1               80
     O          E            DET3           1
     O                       XXCM2               80
     O          E            DET4           1
     O                       XXCM3               80
     O          E            DET5           1
     O                       XXCM4               55
     O          E            DET6           1
     O                       XXCM75              80
     O          E            DET7           1
     O                       XXCM75              80
     O*
     O**** ERROR MESSAGE, IF NO RECORDS TO BE PROCESSED *************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
     O**** Error Message, If user is not enrolled in DARTS Security ****
     O          E            ERRMS2      3  1
     O                                           23 '** YOU ARE NOT ENROLLED'
     O                                           47 'IN DARTS SECURITY FILE.'
     O          E            ERRMS2         1
     O                                           27 'SEE SYSTEM ADMINISTRATOR'
     O                                           52 'FOR FURTHER INFORMATION.'
**  SRT1/SRT2 - SORT SEQUENCE TITLES
KYKY - KEY
KDKD - KEY/DATE
KRKR - KEY/REMARK TYPE
KOKO - KEY/DOCUMENT ID
UKUK - USER/KEY
UDUD - USER/DATE
URUR - USER/REMARK TYPE
UOUO - USER/DOCUMENT ID
**  DIT1/DIT2 - DIARY TYPE TITLES
NN - NONCOMPLETED DIARY REMARKS
CC - COMPLETED DIARY REMARKS
BB - BOTH COMPLETED & NONCOMPLETED DIARY REMARKS
