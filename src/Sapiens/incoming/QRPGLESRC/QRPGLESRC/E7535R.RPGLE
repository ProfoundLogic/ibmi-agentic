     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H**
     H/TITLE E7535R - MANUAL RATE TABLE VALUE LISTING
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINT A LIST OF THE                        **
     H**     MANUAL RATE TABLE VALUE                                 **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7535C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    02/07/91     PROJECT: EXP MOD                 **
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
     H**  08/26/14 C:298 Project: SY354                              **
     H**                -Implement Routing Page Changes.             **
     H**                                                             **
     H**  01/28/10  C:XXX  Project: SY215                            **
     H**            C:189  Retrieve brand Name                       **
     H**                                                             **
     H** 10/18/02 SUPPORT: EX093 34579                               **
     H**          Expand rate field to 7.0                           **
     H**                                                             **
     H** 08/10/01  C:136 PROJECT:  WC577                             **
     H**           EMP_EXP_MOD REWRITE - ADDED EFDT, FRCL, AND       **
     H**           TOCL PARMS TO ENHANCE PRINT PROGRAM               **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT:  WC853                            **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add field Rate Set Code.                         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**     *IN                                                     **
     H**      08     DEMAND MODE                                     **
     H**      09     BATCH  MODE                                     **
     H**      50     DOWEQ    EMEXL                                  **
     H**      55     READ ALL RSC                                    **
     H**      56     PRINT *ALL IF EFDT IS BLANK                     **
     H**      57     PRINT *ALL IF FRCL AND TOCL IS BLANK            **
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    SMORL (POR01): OVERRIDE FILE                             **
     H**    EMUPL (PUP01): USER FILE                                 **
     H**    EMMRL2(PMR01): MANUAL RATE   CO#/RATECODE/MRCLAS/MREFDT  **
     H*****************************************************************
     H*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FEMUPL     IF   E           K DISK
     FEMMRL2    IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***** 06/19/24 ***************************************
     D xxRate          s              8  5
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
     C     MRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQRSC
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQRSC             3
      *** BEG ADD *** 08/10/01 ************************************************
     C                   PARM                    QQEFDT            7
      *** END ADD *** 08/10/01 ************************************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      *** BEG ADD *** 08/10/01 ************************************************
     C                   PARM                    QQFRCL            5
     C                   PARM                    QQTOCL            5
      *** END ADD *** 08/10/01 ************************************************
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
     C     *LIKE         DEFINE    MRCLAS        XXCLAS
     C     *LIKE         DEFINE    MRDRSC        XXDRSC
     C*
     C                   MOVE      *BLANKS       XXDRSC
     C                   MOVE      *BLANKS       XXCLAS
     C                   Z-ADD     *ZERO         XXEFDT            6 0
      *** BEG CHG **** 10/18/02 ******************************************
     C******************** MOVE *BLANKS   XXRATE  54
      *** Beg Del ***** 06/19/24 *****************************************
     C***                MOVE      *BLANKS       XXRATE            7 4
      *** End Del ***** 06/19/24 *****************************************
      *** END CHG **** 10/18/02 ******************************************
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
     C     QQRSC         IFEQ      *BLANK
     C                   MOVE      *ON           *IN55                          READ ALL RSC
     C                   END
      *** BEG ADD 08/10/01 *****************************************************
     C     QQEFDT        IFEQ      *BLANK
     C                   MOVE      *ON           *IN56                          PRINT *ALL FOR EFDT
     C                   ELSE
     C                   MOVE      QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
     C                   END
     C*
     C     QQFRCL        IFEQ      *ZEROS
     C     QQTOCL        ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN57                          PRINT *ALL FOR FRCL
     C                   END                                                    AND TOCL
      *** END ADD 08/10/01 *****************************************************
     C*
     C*    CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *** BEG ADD 08/10/01 **************************************************
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     ##DATA        CONAME
      *** BEG ADD 08/10/01 **************************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *** BEG ADD 08/10/01 **************************************************
     C                   MOVEL(P)  ORTITL        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     ##DATA        ORTITL
      *** BEG ADD 08/10/01 **************************************************
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *******************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *******************************************
     C                   END
     C*
     C*   PRINT PAGE HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C****   READ EXPECTED LOSS  FILE
     C*
     C     MRKEY         SETLL     PMR01
     C*
     C   55              READ      PMR01                                  50
     C  N55MRKEY         READE     PMR01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C                   Z-ADD     MREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT
     C*
      *** Beg Chg ***** 06/19/24 *********************************************
     C     MRRATE        MULT      .0001         XXRATE
        xxRate = MRRate * .01;
      *** End Chg ***** 06/19/24 *********************************************
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVE      *BLANKS       XXCLAS
     C                   END
      *** BEG ADD *** 08/10/01 ************************************************
     C                   MOVE      *ON           MEETSS            1
     C     QQEFDT        IFNE      *BLANKS
     C     QQFRCL        ORNE      *BLANKS
     C     QQTOCL        ORNE      *BLANKS
     C                   EXSR      SUBSET
     C                   ENDIF                                                  QQEFDT=BLANKS
     C     MEETSS        IFEQ      *ON
      *** END ADD *** 08/10/01 ************************************************
     C     MRCLAS        IFNE      XXCLAS
     C     MRDRSC        ORNE      XXDRSC
     C                   MOVE      MRDRSC        XXDRSC
     C                   Z-ADD     MRCLAS        XXCLAS
     C                   EXCEPT    DET1
     C                   ELSE
     C                   EXCEPT    DET2
     C                   END                                                    MRCLAS<>XXCLAS
      *** BEG ADD *** 08/10/01 ************************************************
     C                   ENDIF                                                  MEETSS=ON
      *** END ADD *** 08/10/01 ************************************************
     C*
     C   55              READ      PMR01                                  50
     C  N55MRKEY         READE     PMR01                                  50
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
      *** BEG ADD *** 08/10/01 ************************************************
      *=========================================================================
      * SUBSET SUBROUTINE
      *=========================================================================
     C     SUBSET        BEGSR
      *
     C                   MOVEL     QQFRCL        SSFRCL            5 0
     C                   MOVEL     QQTOCL        SSTOCL            5 0
     C                   MOVEL     QQEFDT        SSEFDT            7 0
      *
     C     SSEFDT        IFNE      *ZEROS
     C     MREFDT        ANDNE     SSEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSFRCL        IFNE      *ZEROS
     C     SSFRCL        ANDGT     MRCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSTOCL        IFNE      *ZEROS
     C     SSTOCL        ANDLT     MRCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
      *** END ADD *** 08/10/01 ************************************************
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *******************************************
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
      ***** End Del ***** 08/26/14 *******************************************
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
      ***** Beg Del ***** 08/26/14 *******************************************
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
      ***** End Del ***** 08/26/14 *******************************************
      *** BEG ADD 08/10/01 *****************************************************
     O          E            ROUTE          2
     O                                           20 'RATE SET CODE:'
     O              N55      QQRSC               24
     O               55                          25 '*ALL'
     O          E            ROUTE          2
     O                                           20 'EFF DATE . . :'
     O              N56      X1EFDT        Y     31
     O               56                          25 '*ALL'
     O          E            ROUTE          2
     O                                           20 'CLASS CODE . :'
     O              N57      SSFRCL        Z     26
     O               57                          25 '*ALL'
     O              N57                          31 'TO:'
     O              N57      SSTOCL        Z     38
      *** END ADD 08/10/01 *****************************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
      ****** END DLT ***** 01/28/10 **********************************
      *** BEG CHG 08/10/01 *****************************************************
     O********************************** 56 'SOLUTIONS, INC. 1991'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                51 'SOLUTIONS, INC.'
     O***                      *YEAR     56
      ****** END DLT ***** 01/28/10 **********************************
      *** END CHG 08/10/01 *****************************************************
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
     O          E            HEAD           1
     O                                           10 'RATE CODE'
     O                                           28 'PAYROLL CLASS'
     O                                           50 'EFFECTIVE DATE'
     O                                           69 'MANUAL RATE'
     O          E            DET1        1  1
     O                       XXDRSC               7
     O                       XXCLAS        4     21
     O                       XXEFDT        Y     47
      *** BEG CHG **** 10/18/02 **********************************************
     O************************ XXRATE4   66
     O                       XXRATE        4     68
      *** END CHG **** 10/18/02 **********************************************
     O          E            DET2           1
     O                       XXEFDT        Y     47
     O                       XXRATE        4     66
