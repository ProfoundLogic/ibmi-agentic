     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H**
     H/TITLE E7525R - EXPECTED LOSS RATES & (D) RATIO LISTING
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINT A LIST OF THE                        **
     H**     EXPECTED LOSS &1DISCOUNT RATIO FILE                     **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7525C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    02/13/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**06/19/24 C:291 Project: WC2952                               **
     H**               Exapand Expected Loss Rate to 8,3.            **
     H**                                                             **
     H**08/05/15 C:307 Project: SY354                                **
     H**               -Adding 'Route To:'                           **
     H**                                                             **
     H**08/26/14 C:298 Project: SY354                                **
     H**               -Implement Routing Page Changes.              **
     H**                                                             **
     H**                                                             **
     H**  01/28/10  C:249  Project: SY215                            **
     H**            C:189  Retrieve brand Name                       **
     H**                                                             **
     H**  09/05/01 C:136  PROJECT: WC577                             **
     H**           EMP_EXP_MOD REWRITE - ADDED PRINT SELECTIONS FOR  **
     H**           EFF DATE AND PAYROLL CLASS RANGE                  **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT:  WC853                            **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add field State ID Code.                         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**     *IN                                                     **
     H**      08     DEMAND MODE                                     **
     H**      09     BATCH  MODE                                     **
     H**      50     DOWEQ    EMEXL5                                 **
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    SMORL (POR01): OVERRIDE FILE                             **
     H**    SMUPL (PUP01): USER FILE                                 **
     H**    EMEXL5(PEX01): EXPECTED LOSS DSCNT RATIO FILE            **
     H**                   CO#/STATE/CLAS                            **
     H*****************************************************************
     H*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
      *** BEG ADD ***** 09/05/01 ***********************************************
     F****************************EMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
      *** END ADD ***** 09/05/01 ***********************************************
     FEMEXL5    IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***** 06/19/24 ***************************************
     D xxLsRt          s              8  5
      *** End Add ***** 06/19/24 ***************************************

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
     C*   KEY LIST FOR THE EMPLOYER BATCH FILE
     C*   ALL EMP.
     C*
     C     EXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQST              2
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      *** BEG ADD *** 09/05/01 ************************************************
     C                   PARM                    QQDATE            7
     C                   PARM                    QQFRCL            5
     C                   PARM                    QQTOCL            5
      *** END ADD *** 09/05/01 ************************************************
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
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    EXCLAS        XXCLAS
     C     *LIKE         DEFINE    EXCLAS        XXCOD
     C     *LIKE         DEFINE    EXST          XXST
      *** BEG ADD *** 09/05/01 ************************************************
     C     *LIKE         DEFINE    EXEFDT        SSEFDT
      *** END ADD *** 09/05/01 ************************************************
     C                   MOVE      *BLANKS       XXST
     C                   MOVE      *BLANKS       XXCLAS
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQMODE        XXMODE            1
      ************** BEG ADD 09/05/01 ****************************************
     C                   MOVEL(P)  QQFRCL        SSFRCL            5 0
     C                   MOVEL(P)  QQTOCL        SSTOCL            5 0
     C                   MOVEL(P)  QQFRCL        X1FRCL            5 0
     C                   MOVEL(P)  QQTOCL        X1TOCL            5 0
     C                   MOVEL(P)  QQST          SSST              2
     C                   MOVEL(P)  QQDATE        SSEFDT
      *
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
      *
     C                   MOVE      *ALL'_'       LINE            132
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
     C     X1EFDT        COMP      *ZERO                                  72
     C     SSTOCL        COMP      *ZERO                                  73
     C     SSST          COMP      *BLANKS                                74
      ************** END ADD 09/05/01 ****************************************
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
      *
     C*    CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *** BEG ADD ********* 09/05/01 *******************************************
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *** END ADD ********* 09/05/01 *******************************************
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*******            MOVE      '0'           XXPRT
     C*******ORKEY         CHAIN     POR01                              60
     C*******     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C*******            EXCEPT    ROUTE
     C*******            END
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*   PRINT PAGE HEADING
     C*
      *** BEG ADD ********* 09/05/01 *******************************************
      *
      * Center report title
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *** END ADD ********* 09/05/01 *******************************************
     C                   EXCEPT    HEAD
     C*
     C****   READ EXPECTED LOSS  FILE
     C*
     C     EXKEY         SETLL     PEX01
     C*
     C   55              READ      PEX01                                  50
     C  N55EXKEY         READE     PEX01                                  50
     C*
     C     *IN50         DOWEQ     '0'
      *** BEG ADD *** 09/05/01 *************************************************
      *  If subset criteria met
      *
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
     C     MEETSS        IFEQ      *ON
      *** END ADD 09/05/01 *****************************************************
     C*
     C                   MOVE      *BLANKS       XXLSRT
     C                   MOVE      *BLANKS       XXDSRT
     C*
     C                   Z-ADD     EXCLAS        XXCOD
      *** Beg Chg ***** 06/19/24 **********************************************
     C***  EXLSRT        MULT      .0001         XXLSRT            5 4
       xxLsRt = EXLsRt * .01;
      *** End Chg ***** 06/19/24 **********************************************
     C     EXDSRT        MULT      .01           XXDSRT            3 2
     C                   MOVE      EXST          XXST
     C*
     C                   Z-ADD     EXEFDT        SRDTI
      *** BEG CHG *** 09/05/01 *************************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXEFDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
      *** END CHG *** 09/05/01 *************************************************
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET1
      *** BEG ADD *** 09/05/01 *************************************************
     C                   ENDIF                                                  MEETSS
      *** END ADD *** 09/05/01 *************************************************
     C*
     C*
     C   55              READ      PEX01                                  50
     C  N55EXKEY         READE     PEX01                                  50
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
      *** BEG ADD *** 09/05/01 ***********************************************
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
     C                   ENDIF                                                  SSST
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
      *** BEG ADD *** 09/05/01 ***********************************************
      *
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
      *** BEG ADD ***** 09/05/01 *****************************************
     O          E            ROUTE          2
     O                                           26 'STATE . . . . . :'
     O              N74      QQST              +001
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
     O              N73      X1FRCL        4     37
     O               73                          37 '*ALL'
     O                                         +001 'TO'
     O              N73      X1TOCL        4     46
     O               73                          46 '*ALL'
      *** BEG ADD ***** 09/05/01 *****************************************
      *
      ****** BEG DLT ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
      ****** END DLT ***** 01/28/10 **********************************
      *** BEG CHG *** 09/05/01 ************************************************
     O***********************************56 'SOLUTIONS, INC. 1997'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                51 'SOLUTIONS, INC.'
     O***                      *YEAR     56
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *** END CHG *** 09/05/01 ************************************************
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
      *** BEG CHG *** 09/05/01 *************************************************
     O***********************************19 'EFFECTIVE'
     O********************************** 25 'CLASS'
     O                                           19 'EFF DATE'
     O                                           28 'CLASS'
      *** END CHG *** 09/05/01 *************************************************
     O                                           48 'EXPECTED LOSS RATE'
     O                                           69 'DISCOUNT RATIO'
     O          E            DET1           1
     O                       XXST                 7
     O                       XXEFDT        Y     19
      *** BEG CHG *** 06/05/01 *************************************************
     O***********************  XXCOD 4   25
     O                       XXCOD         4     28
      *** END CHG *** 09/05/01 *************************************************
      *** Beg Chg ***** 06/19/24 *************************************************
     O***                    XXLSRT              43 '0 .    '
     O                       XXLSRT        4     43
      *** ENd Chg ***** 06/19/24 *************************************************
     O                       XXDSRT              65 '0 .  '
