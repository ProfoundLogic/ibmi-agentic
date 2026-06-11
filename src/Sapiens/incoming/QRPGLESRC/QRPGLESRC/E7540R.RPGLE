     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS A LIST OF EMPLOYERS WITH THEIR      **
     H**     BATCH YEARS WITH POLICY FROM AND TO DATES.              **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7541C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    02/13/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**08/05/15 C:307 Project: SY354                                **
     H**               -Adding 'Route To:'                           **
     H**                                                             **
     H**  08/26/14 C:298 Project: SY354                              **
     H**                -Implement Routing Page Changes.             **
     H**                                                             **
     H**  01/28/10  C:249  Project: SY215                            **
     H**            C:189  Retrieve brand Name                       **
     H**                                                             **
     H** 08/14/01 C:136 PROJECT WC577                                **
     H**          EMP_MOD_REWRITE - STRAIGHTENED UP COLUMNS ON PRINT **
     H**          ADDED SELECTIONS FOR  BATCH YEAR                   **
     H**                                                             **
     H** 08/12/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION TO STANDARDS                  **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**                                                             **
     H**     *IN                                                     **
     H**      08     DEMAND MODE                                     **
     H**      09     BATCH  MODE                                     **
     H**      50     DOWEQ    EMEBL                                  **
     H**      55     1 = RUN FOR ALL EMP#  0 = 1 EMP#                **
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E0535F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    SMORL (POR01): OVERRIDE FILE                             **
     H**    SMUPL (PUP01): USER FILE                                 **
     H**    EMEBL (PEB01): EMP# BATCH FILE (FND/EMP#/YEAR)           **
     H**    EBKEY=FND  EBKEY1=FND,EMP#                               **
     H**    EMEML (PEM01): EMP. MASTER FILE (FUD/EMP#)               **
     H*****************************************************************
     H*
     H/TITLE E7540R - EMPLOYER BATCH NUMBER LISTING - REPORT
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FEMUPL     IF   E           K DISK
     FEMEBL     IF   E           K DISK
     FEMEML     IF   E           K DISK
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
     C*** BEG DEL *** 08/14/01 *************************************************
     C*   KEY LIST FOR THE EMPLOYER BATCH FILE
     C*   ALL EMP.
     C*
     C***********EBKEY     KLIST
     C******************** KFLD           XXCO#
     C******************** KFLD           XXFND
     C***********EBKEY1    KLIST
     C******************** KFLD           XXCO#
     C******************** KFLD           XXFND
     C******************** KFLD           XXEMP#
     C*** END DEL *** 08/14/01 *************************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C*** BEG CHG *** 08/14/01 *************************************************
     C*******************  KFLD           XXCO#
     C                   KFLD                    EBCO#
     C*** END CHG *** 08/14/01 *************************************************
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*** BEG ADD *** 08/14/01 *************************************************
     C                   PARM                    QQFRBY            3
     C                   PARM                    QQTOBY            3
     C*** END ADD *** 08/14/01 *************************************************
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
     C     *LIKE         DEFINE    EBFND         XXFND
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    EBEMP#        XXEMP
     C     *LIKE         DEFINE    EBYEAR        XXYEAR
     C*
      *** BEG ADD ****** 08/14/01 **********************************************
     C                   MOVE      *BLANKS       XXCO#
      *** END ADD ****** 08/14/01 **********************************************
     C                   MOVE      *BLANKS       XXFND
     C                   MOVE      *BLANKS       XXEMP#
     C                   MOVE      *BLANKS       XXEMP
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQMODE        XXMODE            1
      *** BEG ADD ****** 08/14/01 **********************************************
     C                   Z-ADD     XXFND         SSFND             3 0
     C                   Z-ADD     XXEMP#        SSEMP#            9 0
     C                   MOVE      QQTOBY        SSTOBY            3 0
     C                   MOVE      QQFRBY        SSFRBY            3 0
      *** END ADD ****** 08/14/01 **********************************************
     C*
     C                   TIME                    DSDTTM
      *
      *   SET-UP AND INITIALIZE
      *
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *    CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      *** BEG ADD ****** 08/14/01 **********************************************
     C                   MOVEL     CONAME        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
      *** END ADD ****** 08/14/01 **********************************************
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   GET THE DISTRIBUTION INFORMATION
      *
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *
      *** BEG ADD ****** 08/14/01 **********************************************
     C                   MOVEL     ORTITL        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
      *** END ADD ****** 08/14/01 **********************************************
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      *** BEG ADD ****** 08/14/01 **********************************************
     C     QQEMP#        COMP      *ZERO                                  71     PRT *ALL IF ON
     C     QQTOBY        COMP      *ZERO                                  72     PRT *ALL IF ON
      *** END ADD ****** 08/14/01 **********************************************
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C*   PRINT PAGE HEADING
     C*
     C                   EXCEPT    HEAD
     C*** BEG DEL ****** 08/14/01 **********************************************
     C*   CHAIN TO EMPLOYER BATCH FILE
     C*
     C********** QQEMP#    IFEQ *ZERO
     C**********           MOVE '1'       *IN55
     C**********           END
     C***55***** EBKEY     CHAINPEB01                50    ALL EMP.
     C**N55***** EBKEY1    CHAINPEB01                50    1 EMP. ONLY
     C*** END DEL ****** 08/14/01 **********************************************
     C                   READ      PEB01                                  50
     C     *IN50         DOWEQ     '0'
     C*** BEG ADD ****** 08/14/01 **********************************************
     C                   MOVE      *ON           MEETSS            1              TO PRINT OR
     C                   EXSR      SUBSET                                         NOT TO PRINT?
     C     MEETSS        IFEQ      *ON
     C*** END ADD ****** 08/14/01 **********************************************
     C*   CHAIN TO EMPLOYER MASTER FILE
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       XXNAME           30
     C                   END
     C                   MOVEL     EMNAME        XXNAME
     C*
     C                   Z-ADD     EBYEAR        XXYEAR
     C                   Z-ADD     EBEMP#        XXEMP#
     C*
     C*   CHANGE POSTING FROM DATE FROM YYMMDD TO MMDDYY
     C*
     C                   Z-ADD     EBFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C*   CHANGE POSTING TO DATE FROM YYMMDD TO MMDDYY
     C*
     C                   Z-ADD     EBTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     EBEMP#        IFEQ      XXEMP
     C                   EXCEPT    DET2
     C                   ELSE
     C                   MOVE      EBEMP#        XXEMP
     C                   EXCEPT    DET1
     C                   END
     C*** BEG ADD ****** 08/14/01 **********************************************
     C                   ENDIF                                                  MEETSS
     C*** END ADD ****** 08/14/01 **********************************************
     C*** BEG CHG ****** 08/14/01 **********************************************
     C***55******EBKEY     READEPEB01                    50
     C**N55******EBKEY1    READEPEB01                    50
     C                   READ      PEB01                                  50
     C*** END CHG ****** 08/14/01 **********************************************
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
     C*
     C*** BEG ADD ****** 08/14/01 **********************************************
      *================================================================
      * SUBSET - Check if record meets selection criteria
      *================================================================
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
     C     SSFND         IFNE      *ZERO
     C     SSFND         ANDNE     EBFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND
      *
     C     SSEMP#        IFNE      *ZERO
     C     SSEMP#        ANDNE     EBEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEMP#
      *
     C     SSFRBY        IFNE      *ZEROS
     C     SSFRBY        ANDGT     EBYEAR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFRBY
      *
     C     SSTOBY        IFNE      *ZEROS
     C     SSTOBY        ANDLT     EBYEAR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSTOBY
      *
     C                   ENDSR                                                  SUBSET
     C*** END ADD ****** 08/14/01 **********************************************
      *
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
      *** BEG ADD ***** 08/14/01 *****************************************
     O          E            ROUTE          2
     O                                           26 'COMPANY . . . . :'
     O                       XXCO#         4   +001
      *
     O          E            ROUTE          2
     O                                           26 'GROUP . . . . . :'
     O                       XXFND         4   +001
      *
     O          E            ROUTE          2
     O                                           26 'EMPLOYER #  . . :'
     O              N71      XXEMP#        4   +001
     O               71                          31 '*ALL'
     O          E            ROUTE          2
     O                                           26 'BATCH YEAR  . . :'
     O                                         +001 'FROM'
     O              N72      SSFRBY        4     37
     O               72                          37 '*ALL'
     O                                         +001 'TO'
     O              N72      SSTOBY        4     46
     O               72                          46 '*ALL'
      *** END ADD ***** 09/05/01 *****************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
      ****** END DLT ***** 01/28/10 **********************************
      *** BEG CHG *** 08/14/01 ************************************************
     O***********************************56 'SOLUTIONS, INC. 1997'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                51 'SOLUTIONS, INC.'
     O***                      *YEAR     56
      ****** END DLT ***** 01/28/10 **********************************
      *** END CHG *** 08/14/01 ************************************************
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
      *** BEG CHG ******* 08/14/01 ********************************************
     O******* E  1             HEAD
     O***********************            07 'FUND'
     O***********************            18 'ACCOUNT'
     O***********************            28 'EMPLOYER'
     O***********************            33 'NAME'
     O***********************            55 'BATCH #'
     O***********************            71 'POLICY DATE'
     O******* E  2             HEAD
     O***********************            63 'FROM'
     O***********************            75 'TO'
     O          E            HEAD           1
     O                                           07 'GRP'
     O                                           19 'ACCOUNT'
     O                                           36 'EMPLOYER NAME'
     O                                           59 'BATCH#'
     O                                           75 'POLICY DATE'
     O          E            HEAD           2
     O                                           67 'FROM'
     O                                           77 'TO'
      *** END CHG ******* 08/14/01 ********************************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       ORTITL              70
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
      *** BEG CHG ******* 08/14/01 ********************************************
     O******* E  1             HEAD1
     O***********************            07 'FUND'
     O***********************            18 'ACCOUNT'
     O***********************            28 'EMPLOYER'
     O***********************            33 'NAME'
     O***********************            55 'BATCH #'
     O***********************            71 'POLICY DATE'
     O******* E  2             HEAD1
     O***********************            63 'FROM'
     O***********************            75 'TO'
     O          E            HEAD1          1
     O                                           07 'GRP'
     O                                           19 'ACCOUNT'
     O                                           36 'EMPLOYER NAME'
     O                                           59 'BATCH#'
     O                                           76 'POLICY DATE'
     O          E            HEAD1          2
     O                                           67 'FROM'
     O                                           77 'TO'
      *** END CHG ******* 08/14/01 ********************************************
     O          E            HEAD1          1
     O                       XXFND         4     07
     O                       XXEMP#        4     19
     O                       XXNAME              53
     O          E            DET1        1  1
     O                       XXFND         4     07
     O                       XXEMP#        4     19
     O                       XXNAME              53
      *** BEG CHG ******* 08/14/01 ********************************************
     O************************ XXYEAR4   59
     O                       XXYEAR        4     57
      *** END CHG ******* 08/14/01 ********************************************
     O                       XXFRDT        Y     69
     O                       XXTODT        Y     80
     O          E            DET2           1
      *** BEG CHG ******* 08/14/01 ********************************************
     O***********************  XXYEAR4   55
     O***********************  XXFRDTY   65
     O***********************  XXTODTY   76
     O                       XXYEAR        4     57
     O                       XXFRDT        Y     69
     O                       XXTODT        Y     80
      *** END CHG ******* 08/14/01 ********************************************
