     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8331R - EXCESS CLAIMS DIARY DATE REPORT
     H****************************************************************
     H** PROGRAM DESCRIPTION:                                       **
     H**  This program lists all claims that have an excess claims  **
     H**    diary date equal to the specified run date. The program **
     H**    can be run for one or all funds and breaks on fund.     **
     H** CALLED FROM: W8831C                                        **
     H** CALLS TO:                                                  **
     H**    NONE                                                    **
     H** PROGRAMMER:  Kenneth Kennedy                               **
     H** CREATED:     08/29/90        PROJECT:  WCMS-Client 0003    **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/02 C:15 Support: WC065/31867                           *
     H*                Chgd pgm to prevent the routing page from      *
     H*                printing twice.                                *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR     *
     H*                 'BASE' FILES (XXCASE) - CASE NUMBER REMAINS   *
     H*                 14 ALPHA FOR REINSURANCE FILES (X@CASE)       *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                 CHANGED CASE NUMBER FROM 7 NUMERIC TO 14      *
     H*                 ALPHA FIELD.                                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H**                                                            **
     H** INDICATOR USAGE:                                           **
     H**   Indicator               Function Description             **
     H**                                                            **
     H**      08                     Demand Mode                    **
     H**      09                     Batch Mode                     **
     H**      50                     Read File Indicator            **
     H**      60                     File Chaining Indicator        **
     H**      OA                     OverFlow Indicator             **
     H**      LR                     Last Record - End Program      **
     H****************************************************************
     H**  FILE USAGE:                                               **
     H**    SMORL  (POR01): CO#/Q1PGM/XXPRT                         **
     H**    SMCOL  (PCA01): CO#                                     **
     H**    WMUPL  (PUP01): Q1USER                                  **
     H**    WMUPL  (PUP01): RENAMED TO CHAIN FOR UMAN NAME          **
     H**    WMCML1 (PCM01): CO#/FND/FYR/CASE                        **
     H**    WMFNL  (PFN01): CO#/XXKEY                               **
     H**    WMCHL  (PCH01): CO#/FND/FYR/CASE                        **
     H**    WMEDL  (PED01): CO#/FND/FYR/CASE                        **
     H**    QSYSPRT:        Printer  Only                           **
     H****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEDL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Excess Claims Diary -
     D                                     Date Report')
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR FUND NAME
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EDFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR CLAIM HISTROY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EDFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR CLAIM DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE EXCESS CLAIMS FILE
     C*
     C     EDKEY         KLIST
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
     C*  DEFINE ALL VARIABLES
     C*
     C     *LIKE         DEFINE    EDCO#         XXCO#
     C     *LIKE         DEFINE    EDFND         XXFND
     C     *LIKE         DEFINE    CHOCST        XXOCST
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            8
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
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   MOVE      QQDATE        X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXRNDT            6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C     QQFND         COMP      *ZEROS                                 12
     C*
     C* MOVE ENTRY PARAMETERS INTO REPORT SELECTION FIELDS
     C*
     C     QQCO#         IFEQ      *ZEROS
     C                   MOVEL     'ALL'         RSCO#             3
     C                   ELSE
     C                   MOVEL     QQCO#         RSCO#
     C                   END
     C     QQFND         IFEQ      *ZEROS
     C                   MOVEL     'ALL'         RSFND             3
     C                   ELSE
     C                   MOVEL     QQFND         RSFND
     C                   END
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C                   Z-ADD     0             XXFCNT            4 0          FUND COUNT
     C                   Z-ADD     0             XXACNT            4 0          ALL CASES
     C*
     C*   GET THE USER PARAMETER INFORMATION
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C*** Begin dlt *** 04/10/02 *************************************
     C******************** EXCPTROUTE
     C*** End dlt ***** 04/10/02 *************************************
     C                   END
     C*
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C*
     C*  CENTER PROGRAM TITLE
     C*
     C*** Begin add *** 04/10/02 *************************************
     C     ORTITL        IFEQ      *BLANKS
     C*** End add ***** 04/10/02 *************************************
     C                   MOVEL(P)  OPTNM         XXTITL           40
     C*** Begin add *** 04/10/02 *************************************
     C                   ELSE
     C                   MOVEL(P)  ORTITL        XXTITL
     C                   ENDIF
     C*** End add ***** 04/10/02 *************************************
     C*
     C                   MOVEL(P)  XXTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        XXTITL
     C*                                                               *
     C*** END   ADD *** 01/28/98 ***********************************************
     C*
     C*** Begin add *** 04/10/02 *************************************
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C     *IN60         IFEQ      '0'
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   ENDIF
     C*** End add ***** 04/10/02 *************************************
     C*
     C****************************************************************
     C*   READ THE EXCESS CLAIMS DIARY DATE WORK FILE
     C****************************************************************
     C*
     C     EDKEY         CHAIN     PED01                              50
     C*
     C     *IN50         IFEQ      '1'
     C*
     C*** Begin dlt *** 04/10/02 *************************************
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C******************** EXCPTROUTE
     C*** END   ADD *** 01/28/98 ***********************************************
     C*** End dlt ***** 04/10/02 *************************************
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C                   Z-ADD     EDCO#         XXCO#
     C                   Z-ADD     EDFND         XXFND
     C*
     C                   EXSR      SRCONM
     C                   EXSR      SRFNNM
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    FHEAD
     C*
     C*   DO LOOP
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*   CHECK FOR BREAKS AND DO APPROPIATE UPDATES
     C*
     C     EDFND         IFNE      XXFND
     C                   EXSR      SRFUND
     C*
     C                   Z-ADD     EDFND         XXFND
     C                   EXSR      SRFNNM
     C*
     C                   EXCEPT    FHEAD
     C                   END
     C*
     C*  END OF BREAK CHECKS
     C*
     C                   EXSR      SRCASE
     C*
     C*   GET NEXT RECORD
     C*
     C     EDKEY         READE     PED01                                  50
     C*
     C*   END DO LOOP FOR UNIT MANAGER DIARY DATE WORK FILE
     C*
     C                   END                                                    END *IN50
     C*
     C*  PRINT FUND TOTAL
     C*
     C                   EXSR      SRFUND
     C*
     C*  PRINT TOTALS FOR ALL UNIT MANAGERS IF QQUMAN = *BLANK
     C*
     C   12              EXSR      SRALTT
     C*
     C*   END PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************
     C*   SUBROUTINE TO PRINT CASE TOTALS                            **
     C*****************************************************************
     C*
     C     SRCASE        BEGSR
     C*
     C*   RETRIEVE CLAIM STATUS HISTORY TO DETERMINE IF OPEN OR CLOSED
     C*
     C                   MOVE      EDCASE        XXCASE
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHCO#         ANDEQ     XXCO#
     C     CHFND         ANDEQ     XXFND
     C     CHFYR         ANDEQ     EDFYR
     C     CHCASE        ANDEQ     XXCASE
     C                   MOVEL     CHOCST        XXOCST
     C                   ELSE
     C                   MOVE      *BLANKS       XXOCST
     C                   END
     C*
     C*  ONLY PROCESS OPEN CLAIMS
     C*
     C*   ADD TO FUND COUNTER
     C*
     C                   ADD       1             XXFCNT
     C*
     C*  CHAIN TO CLAIM MASTER TO GET  ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT
     C                   END
     C*
     C*  CHAIN TO CLAIM DEMOGRAPHIC FILE TO GET CLAIMANT NAME
     C*
     C                   MOVE      *BLANKS       XXNAME           30
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       QQFNAM           15
     C                   MOVEL     QQMID         QQFNAM
     C*
     C                   MOVE      '1'           QQTERM
     C                   MOVE      *BLANKS       QQLNAM
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
     C                   END
     C*
     C                   Z-ADD     EDDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   MOVE      *BLANKS       XXCOMM           50
     C                   MOVEL     EDCOMM        XXCOMM           50
     C*
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FHEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   SUBROUTINE TO ADD COUNTER TO UNIT MANAGER TOTAL              **
     C*****************************************************************
     C*
     C     SRFUND        BEGSR
     C*
     C                   ADD       XXFCNT        XXACNT
     C*
     C*  PRINT FUND TOTALS
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FHEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    FNTOT
     C*
     C                   Z-ADD     0             XXFCNT
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*   ALL UNIT MANAGERS SUBROUTINE
     C*************************************************************
     C     SRALTT        BEGSR
     C*
     C                   MOVE      '1'           *INLR                          LAST REC READ
     C                   EXCEPT    HEAD
     C*
     C                   EXCEPT    AUMTT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   SUBROUTINE TO GET COMPANY NAME                             **
     C*****************************************************************
     C*
     C     SRCONM        BEGSR
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C*** END   ADD *** 01/28/98 ***********************************************
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   SUBROUTINE TO GET FUND NAME                                **
     C*****************************************************************
     C*
     C     SRFNNM        BEGSR
     C*
     C                   MOVE      *BLANKS       XXFNNM           70
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     O*                                                               *
     O*  BEGIN OUTPUT SPECIFICATIONS                                  *
     O*                                                               *
     O*****************************************************************
     O*   ROUTING INFORMATION                                         *
     O*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
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
     O*** CHG 01/28/98 ***     ORTITL    80
     O                       XXTITL              59
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
     O*
     O*** BEGIN CHG *** 01/28/98 ***********************************************
     O****CHG E  233           ROUTE
     O*** CHG 01/28/98 ***               18 'REPORT SELECTIONS:'
     O          E            ROUTE       1  2
     O                                           23 'REPORT SELECTIONS'
     O*** END   CHG *** 01/28/98 ***********************************************
     O*
     O          E            ROUTE          2
     O*** CHG 01/28/98 ***
     O*** CHG 01/28/98 ***               23 'COMPANY        :'
     O                                           23 'COMPANY . . . . :'
     O*** CHG 01/28/98 ***     RSCO#     33
     O                       RSCO#               27
     O          E            ROUTE          2
     O*** CHG 01/28/98 ***               26 'FUND           :'
     O                                           23 'GROUP . . . . . :'
     O*** CHG 01/28/98 ***     RSFND     33
     O                       RSFND               27
     O          E            ROUTE          2
     O*** CHG 01/28/98 ***               26 'RUN DATE       :'
     O                                           23 'RUN DATE  . . . :'
     O*** CHG 01/28/98 ***     XXRNDT    38 '  /  /  '
     O                       XXRNDT              32 '  /  /  '
     O*
     O*** CHG 01/28/98 ***               23 '(C) COPYRIGHT PROGRESSI'
     O*** CHG 01/28/98 ***               46 'VE DATA SOLUTIONS. INC,'
     O*** CHG 01/28/98 ***               51 '.1997'
     O*
     O*** BEGIN CHG *** 01/28/10 **************************************
     O****    E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O*****************************************************************
     O*      STANDARD HEADING                                         *
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O*** CHG 01/28/98 ***     ORTITL    96
     O                       XXTITL              87
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                          123 'RUN DATE:'
     O                       XXRNDT             132 '  /  /  '
     O          E            HEAD        1  2
     O              NLR                           4 'YEAR'
     O              NLR                          11 'CLAIM#'
     O              NLR                          30 'CLAIMANT'
     O              NLR                          61 'ACC DATE'
     O              NLR                          66 'STS'
     O              NLR                          79 'EXC CLM DATE'
     O              NLR                          89 'COMMENTS'
     O*****************************************************************
     O*      FUND NUMBER AND NAME HEADING                             *
     O*****************************************************************
     O          E            FHEAD       1  2
     O*** CHG 01/28/98 ***               08 'FUND:'
     O                                           08 'GROUP:'
     O                       XXFND         Z     12
     O                       XXFNNM              83
     O*****************************************************************
     O*      DETAIL INFORMATION                                       *
     O*****************************************************************
     O          E            DET            1
     O                       EDFYR         Z      3
     O                       XXCASE              17
     O                       XXNAME              52
     O                       XXACDT              61 '  /  /  '
     O                       XXOCST              65
     O                       XXDATE              77 '  /  /  '
     O                       XXCOMM             131
     O*****************************************************************
     O*      FUND TOTALS                                              *
     O*****************************************************************
     O          E            FNTOT       2  2
     O*** CHG 01/28/98 ***               34 'NUMBER OF CASES FOR FUND'
     O                                           34 'NUMBER OF CASES FOR GROU'
     O                                           35 'P'
     O*** CHG 01/28/98 ***     XXFND Z   38
     O                       XXFND         Z     39
     O*** CHG 01/28/98 ***               39 ':'
     O                                           40 ':'
     O*** CHG 01/28/98 ***     XXFCNTZ   44
     O                       XXFCNT        Z     45
     O*****************************************************************
     O*   TOTAL FOR ALL FUNDS IF ALL FUNDS ARE SELECTED               *
     O*****************************************************************
     O          E            AUMTT          2
     O                                           31 'TOTAL NUMBER OF CASES'
     O*** CHG 01/28/98 ***               47 'FOR ALL FUNDS :'
     O                                           47 'FOR ALL GROUPS:'
     O                       XXACNT        Z     52
     O*****************************************************************
     O*   NO RECORD ERROR MESSAGE   SUMMARY                           *
     O*****************************************************************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O*** CHG 01/28/98 ***               45 'SELECTED CRITERIA ***'
     O                                           41 'SELECTED CRITERIA ***'
     O*
