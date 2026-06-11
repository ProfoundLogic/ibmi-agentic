     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E7520R - EMPLOYER MASTER LISTING - REPORT
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS A LIST OF EMPLOYERS WITH THEIR      **
     H**     ACCOUNT#, EMP#, AND NAME & ADR.                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7520C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    03/08/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** REVISIONS:                                                  **
     H**                                                             **
     H**08/05/15 C:307 Project: SY354                                **
     H**               -Adding 'Route To:'                           **
     H**                                                             **
     H**08/26/14 C:298 Project: SY354                                **
     H**               -Implement Routing Page Changes.              **
     H**                                                             **
     H**  01/28/10  C:249  Project: SY215                            **
     H**            C:189  Retrieve brand Name                       **
     H**                                                             **
     H**  07/05/01 C:136  PROJECT WC577                              **
     H**            EMP_EXP_MOD REWRITE -                            **
     H**                                                             **
     H**  08/12/98  C:90  PROJ: WC853                                **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/06/91 LPK  PROJECT: MD006                               **
     H**                1. IF STATUS = 'I' OR EXP MOD FLAG = 'N',    **
     H**                DO NOT PRINT EMPLOYER ON LISTING.            **
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
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    SMORL (POR01): OVERRIDE FILE                             **
     H**    EMUPL (PUP01): USER FILE                                 **
     H**    EMEML (PEM01): EMP# MASTEE FILE (EMFND/EMEMP#)           **
     H**    EMFNL (PFN01): EMFND
     H**    EMKEY=CO/FND                                             **
     H**    FNKEY=CO/FND                                             **
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FEMUPL     IF   E           K DISK
     FEMEML     IF   E           K DISK
     FEMFNL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

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
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND  MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP
     C     *LIKE         DEFINE    EMSEMP        XXSEMP
     C     *LIKE         DEFINE    EMMZIP        XXZIP
     C     *LIKE         DEFINE    EMMST         XXST
     C     *LIKE         DEFINE    EMSTAT        XXSTAT
     C                   MOVE      *BLANKS       XXFND
     C                   MOVE      *BLANKS       XXEMP
      *** BEG CHG ****** 07/05/01 **********************************************
     C*********************MOVE *BLANKS   XXNAME 26
     C*********************MOVE *BLANKS   XXADR  30
     C                   MOVE      *BLANKS       XXNAME           25
     C                   MOVE      *BLANKS       XXADR            25
      *** END CHG ****** 07/05/01 **********************************************
     C                   MOVE      *BLANKS       XXCTY            10
     C                   MOVE      *BLANKS       XXST
     C                   MOVE      *BLANKS       XXZIP
     C                   MOVE      *BLANKS       XXEFDT
     C                   MOVE      *BLANKS       XXSEMP
     C                   MOVE      *BLANKS       XXSTAT
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQMODE        XXMODE            1
      *
      ************** BEG ADD 07/05/01 ****************************************
     C                   MOVE      *ALL'_'       LINE            132
     C                   MOVE      *ON           MEETPR            1            PRINTED
      ************** END ADD 07/05/01 ****************************************
      *
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*    CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *******************************************
     C********           MOVE      '0'           XXPRT
     C********     ORKEY         CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C********           EXCEPT    ROUTE
     C********                   END
      ***** End Del ***** 08/26/14 *******************************************
     C*
     C*   CHAIN TO FUND MASTER FILE
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXFNAM
     C                   ELSE
     C                   MOVEL     FNNAME        XXFNAM
     C                   END
     C*   PRINT PAGE HEADING
      *** BEG ADD ********** 07/05/01 ****************************************
      *
     C*   PRINT PAGE HEADING
      *
      * Center report name in display field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *** END ADD ********** 07/05/01 ****************************************
      *
     C                   EXCEPT    HEAD
      *
     C*   Begin read of employer file
      ************** BEG CHG 07/05/01 ****************************************
     C********** EMKEY     CHAINPEM01                50
     C                   READ      PEM01                                  50
      ************** END CHG 07/05/01 ****************************************
     C     *IN50         DOWEQ     '0'
      ************** BEG ADD 07/05/01 ****************************************
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
     C     MEETSS        IFEQ      *ON
     C                   MOVE      *OFF          MEETPR
      ************** END ADD 07/05/01 ****************************************
      ************** BEG DEL 07/05/01 ****************************************
     C********** EMSTAT    IFNE 'I'
     C********** EMMOD     ANDNE'N'
      ************** END DEL 07/05/01 ****************************************
     C                   Z-ADD     EMEMP#        XXEMP
     C                   MOVEL     EMNAME        XXNAME
     C                   MOVEL     EMMAD1        XXADR
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C*
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   MOVEL(P)  EMSEMP        XXSEMP
     C                   MOVE      EMSTAT        XXSTAT
     C*
     C*   PRINT THE LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C                   END
     C*
      ************** BEG CHG 07/05/01 ****************************************
     C***********EMKEY     READEPEM01                    50
     C                   READ      PEM01                                  50
      ************** END CHG 07/05/01 ****************************************
     C                   END
      ************** BEG ADD 07/05/01 ****************************************
     C     MEETPR        IFEQ      *ON
     C                   EXCEPT    NOREC
     C                   ENDIF                                                  MEETPR
      ************** END ADD 07/05/01 ****************************************
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
      ************** BEG ADD 07/05/01 ****************************************
      *=======================================================================
      *   SUBSET SUBROUTINE
      *=======================================================================
     CSR   SUBSET        BEGSR
      *
     C     EMSTAT        IFEQ      'I'
     C     EMMOD         ANDEQ     'N'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     XXFND         IFNE      *ZEROS
     C     XXFND         ANDNE     EMFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
      ************** END ADD 07/05/01 ****************************************
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
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
      ****** END DLT ***** 01/28/10 **********************************
      *** BEG CHG ******** 07/05/01 **********************************
     O********************               56 'SOLUTIONS, INC. 1997'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                51 'SOLUTIONS, INC.'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *** END CHG ******** 07/05/01 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
      *** BEG CHG ******** 07/05/01 **********************************
     O************************ CONAME    75
     O************************ DSDATEY  120
     O                       CONAME              86
     O                       DSDATE        Y    132
      *** END CHG ******** 07/05/01 **********************************
     O          E            HEAD           2
     O                       Q1PGM               10
      *** BEG CHG ******** 07/05/01 **********************************
     O*********************    ORTITL    80
     O*********************             116 'PAGE:'
     O*********************    PAGE  Z  120
     O******* E  1             HEAD
     O*********************               5 'FUND:'
     O*********************    XXFND Z   10
     O*********************    XXFNAM    71
     O******* E  1             HEAD
     O*********************              06 'EMP#'
     O*********************              12 'NAME'
     O*********************              47 'ADDRESS'
     O*********************              75 'CITY'
     O*********************              84 'ST'
     O*********************              94 'ZIP CODE'
     O*********************             104 'ST.EMP#'
     O*********************             114 'EF.DATE'
     O*********************             120 'STAT'
     O******* E 11             DET
     O*********************    XXEMP 4   09
     O*********************    XXNAME    38
     O*********************    XXADR     70
     O*********************    XXCTY     81
     O*********************    XXST      84
     O*********************    XXZIP     95 '     -    '
     O*********************    XXSEMP   113
     O*********************    XXEFDTY  123
     O*********************    XXSTAT   127
     O                       ORTITL              96
     O                                          127 'PAGE:'
     O                       PAGE          Z   +001
     O          E            HEAD           1
     O                                              'GRP'
     O                                         +007 'EMP#'
     O                                         +002 'NAME'
     O                                         +023 'ADDRESS'
     O                                         +020 'CITY'
     O                                         +008 'ST'
     O                                         +002 'ZIP CODE'
     O                                         +004 'ST.EMP#'
     O                                         +011 'EF.DATE'
     O                                         +002 'STAT'
     O          E            DET         1  1
     O                       EMFND
     O                       XXEMP         4   +002
     O                       XXNAME            +002
     O                       XXADR             +002
     O                       XXCTY             +002
     O                       XXST              +002
     O                       XXZIP             +002 '     -    '
     O                       XXSEMP            +002
     O                       XXEFDT        Y   +002
     O                       XXSTAT            +004
     O          E            NOREC          2
     O                                           67 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
      *** END CHG ******** 07/05/01 **********************************
