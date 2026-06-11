     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H**
     H/TITLE E7560R - BALLAST VALUE LISTING
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINT A LIST OF THE                        **
     H**     EXPECTED LOSS &  BALLAST VALUE                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7560C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: T ANZALONE                                    **
     H**   CREATED:    02/03/92     PROJECT: MD054                   **
     H**   REASON:     TO SEPARATE WEIGHTING AND BALLAST VALUES      **
     H**               INTO 2 SEPARATE FILES                         **
     H**   CLIENT:     #0025                                         **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**08/05/15 C:307 Project: SY354                                **
     H**               -Adding 'Route To:'                           **
     H**                                                             **
     H**  08/26/14 C:298 Project: SY354                              **
     H**               -Implement Routing Page Changes.              **
     H**                                                             **
     H**  01/28/10  C:XXX  Project: SY215                            **
     H**            C:189  Retrieve brand Name                       **
     H**                                                             **
     H** 08/08/01 C:136 PROJECT: WC577                               **
     H**                Emp Exp Mod Rewrite - Enhance print rept     **
     H**                by adding efdt selections and printing *all  **
     H**                if no state or efdt selections are made      **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT: WC853                             **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add field State ID Code.                         **
     H**                                                             **
     H**     08-02-94 C:28 PROJ#: WC356                              **
     H**               CHANGED THE BALOSS AND WELOSS FIELDS SIZE     **
     H**               FROM 8/0 TO 11/0                              **
     H**                                                             **
     H**     07-16-92  LJQ PROJ#: WC153                              **
     H**               ADDED EFFECTIVE DATE TO FILE.  USE EFFECTIVE  **
     H**               DATE OF MOD TO DETERMINE WHICH VALUE TO USE.  **
     H**                                                             **
     H**               MOVED FROM CLIENT #25 TO BASE EXP MOD         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**     *IN                                                     **
     H**      08     DEMAND MODE                                     **
     H**      09     BATCH  MODE                                     **
     H**      50     DOWEQ    EMBAL4                                 **
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    SMORL (POR01): OVERRIDE FILE                             **
     H**    EMUPL (PUP01): USER FILE                                 **
     H**    EMBAL4(PBA01): CO/STATE/EFF DT/LODD SELECT STAT <> I     **
     H*****************************************************************
     H*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FEMUPL     IF   E           K DISK
     FEMBAL4    IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   BALLAST TABLE
     C*
     C     BAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQST              2
      *** BEG ADD *** 08/08/01 *************************************************
     C                   PARM                    QQEFDT            7
      *** END ADD *** 08/08/01 *************************************************
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    BALOSS        XXLOSS
     C     *LIKE         DEFINE    BAST          XXST
     C*
     C                   MOVE      *BLANKS       XXST
     C*
     C                   MOVE      *BLANKS       XXBALL            9 2
     C                   MOVE      *BLANKS       XXLOS            13 2
     C                   MOVE      *BLANKS       XXBALL
     C*
     C*
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     QQST          IFEQ      *BLANK
     C                   MOVE      *ON           *IN55                          READ ALL STATES
     C                   END
     C*
      *** BEG ADD *** 08/08/01 *************************************************
     C     QQEFDT        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN56                          PRINT *ALL
     C                   ELSE
     C                   MOVEL     QQEFDT        SSEFDT            7 0
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
     C                   ENDIF
      *** END ADD *** 08/08/01 *************************************************
     C*    CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *** BEG ADD *** 08/08/01 *************************************************
     C                   MOVEL(P)  CONAME        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
      *** END ADD *** 08/08/01 *************************************************
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
      *** BEG ADD *** 08/08/01 *************************************************
     C                   MOVEL(P)  ORTITL        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
      *** END ADD *** 08/08/01 *************************************************
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** Beg Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C*   PRINT PAGE HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C     BAKEY         SETLL     PBA01
     C*
     C   55              READ      PBA01                                  50
     C  N55BAKEY         READE     PBA01                                  50
     C*
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*
     C                   Z-ADD     BALOSS        XXLOS
     C                   Z-ADD     BABALL        XXBALL
     C*
     C                   Z-ADD     BAEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   MOVE      BAST          XXST
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
      *** BEG ADD *** 08/08/01 *************************************************
     C                   MOVE      *ON           MEETSS            1
     C     QQEFDT        IFNE      *BLANKS
     C                   EXSR      SUBSET
     C                   ENDIF
      *
     C     MEETSS        IFEQ      *ON
      *** END ADD *** 08/08/01 *************************************************
     C                   EXCEPT    DET1
      *** BEG ADD *** 08/08/01 *************************************************
     C                   ENDIF
      *** END ADD *** 08/08/01 *************************************************
     C*
     C   55              READ      PBA01                                  50
     C  N55BAKEY         READE     PBA01                                  50
     C*
     C                   END
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
     C                   MOVE      '1'           *INLR
      *** BEG ADD *** 08/08/01 *************************************************
      *=========================================================================
      * SUBSET SUBROUTINE
      *=========================================================================
     C     SUBSET        BEGSR
     C     SSEFDT        IFNE      BAEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSR
      *** END ADD *** 08/08/01 *************************************************
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
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
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
      *** BEG ADD *** 08/08/01 *************************************************
     O          E            ROUTE          2
     O                                           19 'DATE RANGE:'
     O              N56      X1EFDT        Y     29
     O               56                          25 '*ALL'
     O          E            ROUTE          2
     O                                           19 'STATE:'
     O              N55      QQST                23
     O               55                          25 '*ALL'
      *** END ADD *** 08/08/01 *************************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
     O***                                56 'SOLUTIONS, INC. 1997'
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              70
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                            8 'STATE'
     O                                           28 'EXPECTED LOSSES'
     O                                           51 'BALLAST VALUE'
     O                                           76 'EFFECTIVE DATE'
     O          E            DET1           1
     O                       XXST                 7
     O                       XXLOS               28 '  ,   ,   , 0 .  '
     O                       XXBALL              51 '  ,   , 0 .  '
     O                       XXEFDT        Y     76
