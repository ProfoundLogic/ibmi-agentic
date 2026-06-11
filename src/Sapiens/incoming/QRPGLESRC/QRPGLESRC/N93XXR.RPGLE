     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE N93XXR - PRINT BILLS
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**   PROGRAM: N93XXR                                           **
     H**   DESCRIPTION: PRINTS NETWORK BILLS ONLY ...                **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** REVISIONS:                                                  **
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                              **
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*
     H** 8/08/89  CONNIE TERRY - CHANGE HARD CODED TEXT - WAS 10     **
     H**          DAYS FOR PAYMENTS, NOW 30 DAYS.                    **
     H**          PROJECT : I0016                                    **
     H**                                                             **
     H** 8/14/89  CONNIE TERRY - RE-ALIGN DETAIL LINES -- PRINTING   **
     H**          SO CLOSE TO MARGINS IT IS DIFFICULT TO LINE UP.    **
     H**          PROJECT : I0007                                    **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*
     FQSYSPRT2  O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     F                                     USROPN
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D isopen          S              1    inz
     D xxopn           S              1  0

     d***** Beg Add *** 06/09/20 *******************************************
     d xxss#           s             11    inz
     d***** End Add *** 06/09/20 *******************************************
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  SMZIP                  1      9  0
     D  SMZPP1                 1      5  0
     D  SMZPP2                 6      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D***
     D                 DS
     D  XFNUM                  1     15
     D  XXFND                  1      3  0
     D  XFHYP1                 4      4
     D  XXFYR                  5      7  0
     D  XFHYP2                 8      8
     D  XXCASE                 9     15  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PR1DS           DS
     D  FILL3                  1    200
     D  FILL4                201    366
     D  X1LINE               367    368B 0
     D  X1PAGE               369    372B 0
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  PAYEE DEMOGRAPHICS
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  USER PARMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     *ENTRY        PLIST
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             PARM           QQINV#  5
     C                   PARM                    QQINV#            9
     C*** END CHG *** 01/04/99 ****************************************
     C                   PARM                    QQINDT            8
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQPRCH            9
     C                   PARM                    QQSBSV            9
     C                   PARM                    QQAFEE            9
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
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             MOVE QQINV#    XXINV#  50
     C                   MOVE      QQINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVE      QQINDT        X1INDT            8 0
     C*
     C                   Z-ADD     X1INDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXINDT            6 0
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQPRCH        XXPRCH            9 2
     C                   MOVE      QQSBSV        XXSBSV            9 2
     C                   MOVE      QQAFEE        XXAFEE            9 2
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  OPEN FILE IF FIRST TIME THROUGH
     C*
     C                   eval      isopen = *off
     C     XXOPN         IFNE      1
     C                   OPEN      QSYSPRT2
     C*
     C*** GET ROUTING INFORMATION FOR BILLS
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*******            MOVE      '0'           XXPRT
     C*****ORKEY         CHAIN     POR01                              60
     C*****IN60          IFEQ      '0'
     C**********         EXCEPT    ROUTE
     C*******            END                                                    END IFEQ '0'
     C*******            MOVE      '1'           XXOPN             1
      ***** End Del ***** 08/26/14 ************************************
     C                   END                                                    NOT YET OPEN
     C*
     C*   GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   MOVE      *BLANKS       XXSAD1           40
     C                   MOVE      *BLANKS       XXSAD2           40
     C                   MOVE      *BLANKS       XXSCSZ           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   MOVEL     SMADR1        XXSAD1           40
     C                   MOVEL     SMADR2        XXSAD2           40
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     SMST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      SMZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     SMZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      SMZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     SMCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXSCSZ           40
     C                   END
     C     XXSAD2        COMP      *BLANKS                            9494
     C*
     C*  GET TAX ID FROM CO AUXILLARY FILE
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      C1FEIN        XXTXID            9 0
     C                   END
     C*
     C*
     C* * * GET CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         XXSS#             9 0
      /Free
       xxSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C     CMSS#         CHAIN     PCD01                              52
     C     *IN52         IFEQ      '0'
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDOB             6 0
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXCLNM           56
     C                   ELSE
     C                   MOVEL     *BLANKS       XXCLNM           56
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT            6 0
     C                   Z-ADD     0             XXDOB             6 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      *ZERO         XXSS#             9 0
     C                   MOVE      *blanks       XXSS#
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL     *BLANKS       XXCLNM           56
     C                   END
     C*
     C* * * GET PROVIDER INFORMATION.
     C*
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXPRNM           56
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM           56
     C                   END
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN98                          NOT BLANK
     C                   ELSE
     C                   MOVE      '0'           *IN98                          BLANK
     C                   END
     C*
     C*
     C***  OUTPUT BILL - DETAIL LINE
     C*
     C                   EXCEPT    HEAD
     C*** END OF PROGRAM
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If isopen = *off;
       // Open qsysprt2;
       // Isopen = *on;
          xxprt = '0';
          Chain orkey POR01;
          If %found;
             Except Route;
          Endif;
          Isopen = *on;
       EndIf;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   RETURN
     C*
     C****
     O/EJECT
     OQSYSPRT2  E            ROUTE          1 06
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT2  E            HEAD           3 03
     O                                           59 'INVOICE FOR'
     O                                           76 'NETWORK SERVICES'
     O*
     O          E            HEAD           1
     O                                           23 'SUBSCRIBER:'
     O                       XXSBNM              64
     O                                          104 'INVOICE #'
     O                       XXINV#             116
     O          E            HEAD           1
     O                       XXSAD1              64
     O                                          107 'INVOICE DATE'
     O                       XXINDT             116 '  /  /  '
     O          E    94      HEAD           1
     O                       XXSAD2              64
     O          E            HEAD           3
     O                       XXSCSZ              64
     O          E            HEAD           2
     O          E            HEAD           1
     O                                           21 'CLAIMANT:'
     O                       XXCLNM              84
     O          E            HEAD           1
     O                                           17 'FILE:'
     O                       XFNUM               43
     O          E            HEAD           1
     O                                           16 'SS#:'
     O***** Beg Chg *** 06/09/20 *******************************************
     O***                    XXSS#               39 '   -  -    '
     O                       XXSS#               39
     O***** End Chg *** 06/09/20 *******************************************
     O          E            HEAD           1
     O                                           26 'DATE OF BIRTH:'
     O                       XXDOB               36 '  /  /  '
     O          E            HEAD           3
     O                                           27 'DATE OF INJURY:'
     O                       XXACDT              36 '  /  /  '
     O          E            HEAD           2
     O          E            HEAD           1
     O                                           21 'PROVIDER:'
     O                       XXPRNM              78
     O          E            HEAD           3
     O                                           34 'DATE SERVICE PROVIDED:'
     O                       XXFRDT              43 '  /  /  '
     O                                           45 '-'
     O                       XXTODT              55 '  /  /  '
     O*
     O          E            HEAD           3
     O          E            HEAD           2
     O          E            HEAD           2
     O                                           65 'TOTAL PROVIDER CHARGES:'
     O                       XXPRCH              86 '$ ,   ,  0.  '
     O          E            HEAD           2
     O                                           62 'SUBSCRIBER''S NETWORK'
     O                                           71 'SAVINGS:'
     O                       XXSBSV              86 '$ ,   ,  0.  '
     O          E            HEAD           3
     O                                           66 'REVIEWER ADMIN FEE:'
     O                       XXAFEE              86 '$ ,   ,  0.  '
     O          E            HEAD           3
     O          E            HEAD           1
     O          E            HEAD           3
     O                                           42 'PLEASE MAKE ALL'
     O                                           65 'REMITTANCES PAYABLE TO'
     O                                           76 'REVIEWER: TAX'
     O                                           94 'IDENTIFICATION #'
     O                       XXTXID             104
     O          E            HEAD           3
     O          E            HEAD           1
     O                                           56 'PAYMENT IN FULL DUE'
     O                                           74 'WITHIN 30 DAYS OF'
     O                                           87 'INVOICE DATE'
     O**
