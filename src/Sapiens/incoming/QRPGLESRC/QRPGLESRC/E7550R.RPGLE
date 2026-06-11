     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*
     H/TITLE E7545R - EMPLOYER PAYROLL LISTING - REPORT
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS A LIST OF EMPLOYERS BY THEIR        **
     H**     BATCH YEARS .                                           **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7550C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    03/12/91     PROJECT: EXP MOD                 **
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
     H**  09/10/01  C:136 PROJECT: WC577                             **
     H**            Emp_Exp_Mod Rewrite - Added subset routine to en-**
     H**            hance selection flexibilty. Centered report      **
     H**            and company info in print report. Aligned column **
     H**            headings.                                        **
     H**                                                             **
     H** 08/12/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION 3 STANDARDS                   **
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
     H**      50     READ                                            **
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    SMORL (POR01): OVERRIDE FILE                             **
     H**    SMUPL (PUP01): USER FILE                                 **
     H**    EMEML (PEM01): EMPLOYER MASTER FILE                      **
     H**    EMEEL (PEE01): EXCESS LOSS FILE (CO#/FND/EMP#/YEAR)      **
     H**    EMEEL1(PEE02): EXCESS LOSS FILE (CO#/FND/YEAR )          **
     H**    EEKEY= CO#/FUND/EMP#/YEAR                                **
     H**    EEKEY1= CO#/FUND/EMP#                                    **
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
      *** BEG CHG *** 09/10/01 ************************************************
     F******************************EMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
      *** END CHG *** 09/10/01 ************************************************
     FEMEML     IF   E           K DISK
     FEMEEL     IF   E           K DISK
      *** BEG DEL *** 09/10/01 ************************************************
     F************EMEEL1  IF  E           K        DISK
     F************PEE01                             KRENAMEPEE02
      *** END DEL *** 09/10/01 ************************************************
     FQSYSPRT   O    F  120        PRINTER OFLIND(*INOA)

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
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*    FUND/EMP#/YEAR
     C*
     C     EEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYPOLL FILE
     C*   FUND / EMP#
     C*
     C     EEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*   FUYND/YEAR
     C     EEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYEAR
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EEFND
     C                   KFLD                    EEEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQYEAR            3
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
     C     *LIKE         DEFINE    EEFND         XXFND
     C     *LIKE         DEFINE    EEEMP#        XXEMP#
     C     *LIKE         DEFINE    EEYEAR        XXYEAR
     C     *LIKE         DEFINE    EECASE        XXCASE
     C     *LIKE         DEFINE    EETLCA        XXTLCA
     C*
     C                   MOVE      *ZERO         XXFND
     C                   MOVE      *ZERO         XXEMP#
     C                   MOVE      *BLANKS       XXEMP
     C                   MOVE      *ZERO         XXYEAR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQYEAR        XXYEAR
     C                   MOVE      QQMODE        XXMODE            1
      *** BEG ADD ****** 09/10/01 **********************************************
     C                   Z-ADD     XXFND         SSFND             3 0          SUBSET VARIABLE
     C                   Z-ADD     XXEMP#        SSEMP#            9 0          SUBSET VARIABLE
     C                   Z-ADD     XXYEAR        SSYEAR            3 0          SUBSET VARIABLE
      *** END ADD ****** 09/10/01 **********************************************
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
     C*** BEG DEL *** 09/10/01 ********************************************
     C********** XXEMP#    IFEQ *ZERO
     C**********           MOVE '1'       *IN55            READ ALL EMP#
     C**********           END
     C********** QQYEAR    IFEQ '000'
     C**********           MOVE '1'       *IN56            READ ALL YR.
     C**********           END
     C*** END DEL *** 09/10/01 ********************************************
     C*
     C*    CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *** BEG ADD ****** 09/10/01 **********************************************
     C                   MOVEL     CONAME        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
      *** END ADD ****** 09/10/01 **********************************************
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
      *** BEG ADD ****** 09/10/01 **********************************************
     C                   MOVEL     ORTITL        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
      *** END ADD ****** 09/10/01 **********************************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      *** BEG ADD ****** 09/10/01 **********************************************
     C     QQEMP#        COMP      *ZERO                                  71     PRT *ALL IF ON
     C     QQYEAR        COMP      *ZERO                                  72     PRT *ALL IF ON
      *** END ADD ****** 09/10/01 **********************************************
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******            EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C*   PRINT PAGE HEADING
     C*
     C                   EXCEPT    HEAD
     C*                                           e
     C*   CHAIN TO EXCESS FILE
     C*
      *** BEG CHG ****** 09/10/01 **********************************************
     C**********  N55N56   EEKEY     CHAINPEE01                50    1 EMP-1 YR
     C**********   56      EEKEY1    CHAINPEE01                50    ALL YR
     C**********   55      EEKEY2    CHAINPEE02                50    ALL EMP
     C                   READ      PEE01                                  50
      *** END CHG ****** 09/10/01 **********************************************
     C     *IN50         DOWEQ     '0'
     C*** BEG ADD ****** 09/10/01 **********************************************
     C                   MOVE      *ON           MEETSS            1              TO PRINT OR
     C                   EXSR      SUBSET                                         NOT TO PRINT?
     C     MEETSS        IFEQ      *ON
     C*** END ADD ****** 09/10/01 **********************************************
     C*   CHAIN TO EMPLOYER MASTER FILE
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAME           36
     C                   END
     C                   MOVEL     EMNAME        XXNAME
     C*
     C                   Z-ADD     EEACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*   IF EXCESS CLAIM
     C     EEACDT        IFNE      0
     C*
     C     EESTAT        IFEQ      'F'
     C                   MOVEL     'FINAL'       XXSTAT            5
     C                   ELSE
     C                   MOVEL     'OPEN '       XXSTAT
     C                   END
     C*
     C     EEMULT        IFEQ      'Y'
     C                   MOVEL     'YES'         XXMULT            3
     C                   ELSE
     C                   MOVEL     'NO '         XXMULT
     C                   END
     C*
     C                   Z-ADD     0             XXTLCA
     C                   MOVE      *BLANKS       XXCASE
     C                   MOVE      EECASE        XXCASE
     C                   ELSE
     C*  NON-EXCESS CLAIM
     C                   MOVE      *BLANKS       XXCASE
     C                   MOVE      *BLANKS       XXSTAT
     C                   MOVE      *BLANKS       XXMULT
     C                   Z-ADD     EETLCA        XXTLCA
     C                   END
     C*
     C*
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
      *** BEG CHG *** 09/10/01 *************************************************
     C******************** MOVE *BLANKS   XXEMP   50
     C                   MOVE      *BLANKS       XXEMP             9 0
      *** END CHG *** 09/10/01 *************************************************
     C                   END
     C*
     C     EEEMP#        IFEQ      XXEMP
     C                   EXCEPT    DET2
     C                   ELSE
     C                   MOVE      EEEMP#        XXEMP
     C                   EXCEPT    DET1
     C                   END
     C*** BEG ADD ****** 09/10/01 **********************************************
     C                   ENDIF                                                  MEETSS
     C*** END ADD ****** 09/10/01 **********************************************
     C*** BEG CHG ****** 09/10/01 **********************************************
     C*********************  N55N56   EEKEY     READEPEE01                    50
     C*********************   56      EEKEY1    READEPEE01                    50
     C*********************   55      EEKEY2    READEPEE02                    50
     C                   READ      PEE01                                  50
     C*** END CHG ****** 09/10/01 **********************************************
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
      *** BEG ADD *** 09/10/01 *************************************************
      *================================================================
      * SUBSET - Check if record meets selection criteria
      *================================================================
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
     C     SSFND         IFNE      *ZERO
     C     SSFND         ANDNE     EEFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND
      *
     C     SSEMP#        IFNE      *ZERO
     C     SSEMP#        ANDNE     EEEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEMP#
      *
     C     SSYEAR        IFNE      *ZEROS
     C     SSYEAR        ANDNE     EEYEAR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSYEAR
      *
     C                   ENDSR                                                  SUBSET
      *** END ADD *** 09/10/01 *************************************************
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
      *** BEG ADD ***** 09/10/01 *****************************************
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
     O              N72      SSYEAR        4   +001
     O               72                          31 '*ALL'
      *** END ADD ***** 09/10/01 *****************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
      ****** END DLT ***** 01/28/10 **********************************
      *** BEG CHG *** 09/10/01 ************************************************
     O***********************************56 'SOLUTIONS, INC. 1997'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                51 'SOLUTIONS, INC.'
     O***                      *YEAR     56
      ****** END DLT ***** 01/28/10 **********************************
      *** END CHG *** 09/10/01 ************************************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    120
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          114 'PAGE:'
     O                       PAGE          Z    118
     O          E            HEAD           1
      *** BEG CHG *** 09/10/01 *************************************************
     O*********************************  08 'FUND:'
     O                                           08 'GROUP:'
      *** END CHG *** 09/10/01 *************************************************
     O                       XXFND               12
      *** BEG DEL *** 09/10/01 *************************************************
     O***************  56                30 'EMPLOYER NO.:'
     O***************  56      XXEMP#Z   40
     O***************  55                22 'YEAR:'
     O***************  55      XXYEARZ   26
     O*************** N55N56             30 'EMPLOYER NO.:'
     O*************** N55N56   XXEMP#Z   36
     O*************** N55N56             47 'YEAR:'
     O*************** N55N56   XXYEARZ   51
      *** END DEL *** 09/10/01 *************************************************
     O          E            HEAD           1
      *** BEG CHG *** 09/10/01 *************************************************
     O******************************     05 'EMP#'
     O**************** 56 **********     11 'YEAR'
     O******************************     22 'ACC.DT.'
     O******************************     33 'CASE NO.'
     O******************************     46 'STAT'
     O******************************     53 'MULT'
     O******************************     70 'AMOUNT'
     O******************************     78 '#CLAMS'
     O******************************     93 'EMPLOYER NAME'
     O                                           09 'EMPLOYER#'
     O                                           15 'YEAR'
     O                                           26 'ACC.DT.'
     O                                           43 'CASE NO.'
     O                                           51 'STAT'
     O                                           57 'MULT'
     O                                           74 'AMOUNT'
     O                                           82 '#CLAMS'
     O                                         +003 'EMPLOYER NAME'
      *** END CHG *** 09/10/01 *************************************************
     O          E            DET1        1  1
     O                       EEEMP#        Z     09
      *** BEG CHG *** 09/10/01 *************************************************
     O**************** 56      EEYEARZ   15
     O                       EEYEAR        Z     15
      *** END CHG *** 09/10/01 *************************************************
     O                       XXACDT              26 '  /  /  '
     O                       XXCASE              43
     O                       XXSTAT              51
     O                       XXMULT              57
     O                       EEAMT         1     71
     O                                           74 '.00'
     O                       XXTLCA        Z     82
     O                       XXNAME             120
     O          E            DET2           1
      *** BEG CHG *** 09/10/01 *************************************************
     O***************  56      EEYEARZ   11
     O***************          XXACDT    22 '  /  /  '
     O***************          XXCASE    39
     O***************          XXSTAT    47
     O***************          XXMULT    53
     O***************          EEAMT 1   67
     O***************                    70 '.00'
     O***************          XXTLCAZ   78
     O                       EEYEAR        Z     15
     O                       XXACDT              26 '  /  /  '
     O                       XXCASE              43
     O                       XXSTAT              51
     O                       XXMULT              57
     O                       EEAMT         1     71
     O                                           74 '.00'
     O                       XXTLCA        Z     82
      *** END CHG *** 09/10/01 *************************************************
