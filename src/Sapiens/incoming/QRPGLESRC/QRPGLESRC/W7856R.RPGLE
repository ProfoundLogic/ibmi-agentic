     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7856R - CLAIMS WITH NO ACTIVITY REPORT - PRINT PGM
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS BY FYR/CASE, ALL OPEN CLAIMS WHICH  **
     H**     DID NOT HAVE ACTIVITY AFTER THE DATE ENTERED.  ACTIVITY **
     H**     BEGIN TRANSACTIONS IN WMRHP OR WDPAP.                   **
     H**   CALLED FROM:                                              **
     H**     W7856C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: C:10                                          **
     H**   CREATED:    9/23/93      PROJECT: WC220                   **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*    05/25/07 C:191 PROJECT: WC1515                             *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H**                                                             **
     H**   04/10/00  C:118  PROJECT: WC987                           **
     H**                    LOAD ALTERNATE COMPANY NAME FROM FUND    **
     H**                    MASTER                                   **
     H**                                                             **
     H**   11/19/93  C:10   PROJECT: ----                            **
     H**                    FIXED PROBLEM - IF ACTIVITY EXISTED IN   **
     H**                    EITHER THE RESERVE HISTORY FILE OR THE   **
     H**                    PAYMENT DETAIL FILE, BUT ACTIVITY DID    **
     H**                    NOT OCCUR IN THE OTHER FILE ... CASE WAS **
     H**                    BEING PRINTED WHEN IT SHOULD NOT BE.     **
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      08      Batch Mode                                     **
     H**      09      Demand Mode                                    **
     H**      40      Run Report for ALL fyrs                        **
     H**     N40      Run Report for ONE fyrs                        **
     H**      43      A record was written to the report             **
     H**     N43      NO records meet selected criteria              **
     H**      50      Used for CHAINing to and READing Claim Master  **
     H**      60      General Usage (CHAIN)                          **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO#                                       **
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO#/FND                                   **
     H**    WMCML1(PCM01): CO#/FND/FYR/CASE                          **
     H**    WMRHL (PRH01): CO#/FND/FYR/CASE/EFDT                     **
     H**    WDPAL5(PPA01): CO#/FND/FYR/CASE/TRDT                     **
     H**    WMCDL (PCD01): SS#                                       **
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMRHL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*   DEFINITION OF VARIABLES
     C*
     C                   Z-ADD     *ZEROS        XXPAID           11 2
     C                   Z-ADD     *ZEROS        XXRES            11 2
     C                   Z-ADD     *ZEROS        XXTOTI           11 2
     C                   Z-ADD     *ZEROS        FTPAID           11 2
     C                   Z-ADD     *ZEROS        FTRES            11 2
     C                   Z-ADD     *ZEROS        FTTOTI           11 2
     C                   Z-ADD     *ZEROS        GTPAID           11 2
     C                   Z-ADD     *ZEROS        GTRES            11 2
     C                   Z-ADD     *ZEROS        GTTOTI           11 2
     C                   MOVE      *BLANKS       XXYEAR            4
     C                   Z-ADD     *ZEROS        FTCNT             5 0
     C                   Z-ADD     *ZEROS        GTCNT             5 0
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE GROUP
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXEFDT
     C*
     C*   KEY LIST FOR THE PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXTRDT
     C*
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    CMFYR         SVFYR
     C     *LIKE         DEFINE    CMACDT        XXNADT
     C     *LIKE         DEFINE    CMACDT        XXTRDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQNADT            8
     C                   PARM                    QQQUE            10
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQNADT        R1NADT            8 0
     C*
     C                   Z-ADD     R1NADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXNADT
     C*
     C                   Z-ADD     R1NADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RSNADT            6 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZERO         SVFYR
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   CHAIN TO THE GROUP FILE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*  CHECK IF RUNNING THIS REPORT FOR ONE FYR OR ALL FYRS
     C*
     C     XXFYR         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN40
     C                   ELSE
     C                   MOVE      '0'           *IN40
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*******            MOVE      '0'           XXPRT             1
     C*******  ORKY          CHAIN     POR01                              60
     C********IN60         IFEQ      '0'
     C*******            EXCEPT    ROUTE
     C*******            END
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INOA
     C*
     C                   MOVE      '0'           *IN43
     C*
     C*  CHAIN TO CLAIM MASTER FILE FOR ONE FYR OR ALL FYRS
     C*
     C   40CMKY          CHAIN     PCM01                              50
     C  N40CMKY1         CHAIN     PCM01                              50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  ONLY PROCESS OPEN CLAIMS
     C*
     C     CMSTAT        IFEQ      'O'
     C*
     C*  GET LAST RESERVE HISTORY RECORD
     C*
     C                   Z-ADD     *HIVAL        XXEFDT
     C     RHKY          SETGT     PRH01
     C                   READP     PRH01                                  60
     C  N60XXCO#         COMP      RHCO#                              6060
     C  N60XXFND         COMP      RHFND                              6060
     C  N60CMFYR         COMP      RHFYR                              6060
     C  N60CMCASE        COMP      RHCASE                             6060
     C*
     C                   Z-ADD     *ZERO         XXEFDT
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN41
     C                   ELSE
     C     RHEFDT        IFLE      XXNADT
     C                   Z-ADD     RHEFDT        XXEFDT
     C                   MOVE      '1'           *IN41
     C                   ELSE
     C                   MOVE      '0'           *IN41
     C                   END
     C                   END
     C*
     C*  GET LAST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     *HIVAL        XXTRDT
     C     PAKY          SETGT     PPA01
     C                   READP     PPA01                                  60
     C  N60XXCO#         COMP      PACO#                              6060
     C  N60XXFND         COMP      PAFND                              6060
     C  N60CMFYR         COMP      PAFYR                              6060
     C  N60CMCASE        COMP      PACASE                             6060
     C*
     C                   Z-ADD     *ZERO         XXTRDT
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN42
     C                   ELSE
     C     PATRDT        IFLE      XXNADT
     C                   Z-ADD     PATRDT        XXTRDT
     C                   MOVE      '1'           *IN42
     C                   ELSE
     C                   MOVE      '0'           *IN42
     C                   END
     C                   END
     C*
     C*  DETERMINE DATE OF LAST ACTIVITY
     C*
     C     *IN41         IFEQ      '1'
     C     *IN42         ANDEQ     '1'                                          ADD 11/19/93
     C     XXEFDT        IFLE      XXTRDT
     C                   Z-ADD     XXTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXLADT            6 0
     C                   ELSE
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXLADT
     C                   END                                                    XXEFDT
     C*
     C*  CHECK FOR GROUP YEAR BREAK
     C*
     C     CMFYR         IFNE      SVFYR
     C     SVFYR         ANDNE     *ZERO
     C                   EXSR      FYRTTL
     C                   END
     C*
     C                   EXSR      PRTCAS
     C*
     C                   MOVE      '1'           *IN43
     C*
     C                   END                                                    *IN41 = '1'
     C*
     C                   END                                                    CMSTAT='O'
     C*
     C   40CMKY          READE     PCM01                                  50
     C  N40CMKY1         READE     PCM01                                  50
     C                   END                                                    *IN60 DOWEQ0
     C*
     C*  PRINT GROUP TOTALS AND GRAND TOTALS
     C*
     C     *IN43         IFEQ      '1'
     C                   EXSR      FYRTTL
     C                   EXCEPT    GTTL
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*===============================================================*
     C* SUBROUTINE PRTCAS - PRINT CASE                                *
     C*===============================================================*
     C     PRTCAS        BEGSR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*   ACCUMULATE TOTAL PAID & INCURRED, AND TOTAL REMAINING RSVS
     C*
     C                   XFOOT     CMA           XXPAID
     C                   XFOOT     CMR           XXRES
     C     XXPAID        ADD       XXRES         XXTOTI
     C*
     C*   GET CLAIMANT NAME
     C*
     C                   MOVE      *BLANKS       XXNAME           24
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
     C                   END
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL     XXATTX        XXNAME
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   EXCEPT    DET
     C*
     C*   ACCUMULATE GROUP YEAR TOTALS
     C*
     C                   ADD       XXPAID        FTPAID
     C                   ADD       XXRES         FTRES
     C                   ADD       XXTOTI        FTTOTI
     C                   ADD       1             FTCNT
     C*
     C                   Z-ADD     *ZEROS        XXPAID
     C                   Z-ADD     *ZEROS        XXRES
     C                   Z-ADD     *ZEROS        XXTOTI
     C*
     C                   Z-ADD     CMFYR         SVFYR
     C*
     C                   ENDSR
     C*===============================================================*
     C* SUBROUTINE FNDTTL - GROUP TOTALS                              *
     C*===============================================================*
     C     FYRTTL        BEGSR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    FTTL
     C                   MOVE      '1'           *INOA
     C*
     C*   ACCUMULATE GRAND TOTALS
     C*
     C                   ADD       FTPAID        GTPAID
     C                   ADD       FTRES         GTRES
     C                   ADD       FTTOTI        GTTOTI
     C                   ADD       FTCNT         GTCNT
     C*
     C                   Z-ADD     *ZEROS        FTPAID
     C                   Z-ADD     *ZEROS        FTRES
     C                   Z-ADD     *ZEROS        FTTOTI
     C                   Z-ADD     *ZEROS        FTCNT
     C*
     C                   ENDSR
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
      ***** Beg Add ***** 08/05/15 ************************************
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
     O**********E            ROUTE          3
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**
     O**   SELECTION CRITERIA
     O**
     O          E            ROUTE          1
     O                                           24 'POLICY YEAR      :'
     O               40                          34 '*ALL'
     O              N40      XXFYR         Z     34
     O*
     O          E            ROUTE          2
     O                                           24 'NO ACTIVITY SINCE:'
     O                       RSNADT              34 '  /  /  '
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  1
     O          E            HEAD           1
     O                                              'POLICY YEAR:'
     O                       CMFYR         Z   +  1
     O                                           70 'LOST'
     O                                          132 'DATE OF'
     O          E            HEAD           1
     O                                           45 'ACCIDENT'
     O                                           70 'TIME'
     O                                           86 'REMAINING'
     O                                          102 'TOTAL'
     O                                          119 'TOTAL'
     O                                          130 'LAST'
     O          E            HEAD           2
     O                                           07 'CASE'
     O                                           23 'CLAIMANT NAME'
     O                                           43 'DATE'
     O                                           57 'EMPLOYER'
     O                                           63 'DIV'
     O                                           70 'FLAG'
     O                                           86 'RESERVES'
     O                                          103 'PAYMENTS'
     O                                          120 'INCURRED'
     O                                          132 'ACTIVITY'
     O          E            DET            1
     O                       CMCASE        Z     07
     O                       XXNAME              34
     O                       XXACDT              45 '  /  /  '
     O                       CMEMP#        Z     57
     O                       CMDIV         Z     63
     O                       CMLTFL              69
     O                       XXRES         J     87
     O                       XXPAID        J    104
     O                       XXTOTI        J    121
     O                       XXLADT             132 '  /  /  '
     O          E            FTTL        1  1
     O                                              'TOTAL # OF CASES'
     O                                         +  1 'FOR POLICY YEAR:'
     O                       FTCNT         Z     39
     O                                           69 'POLICY YEAR TOTALS:'
     O                       FTRES         J     87
     O                       FTPAID        J    104
     O                       FTTOTI        J    121
     O          E            GTTL        1  1
     O                                           22 'TOTAL # OF CASES'
     O                                         +  1 'FOR GROUP:'
     O                       GTCNT         Z     39
     O                                           69 'GRAND TOTALS:'
     O                       GTRES         J     87
     O                       GTPAID        J    104
     O                       GTTOTI        J    121
     O          E            NOREC       1
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 ' SELECTED CRITERIA ***'
