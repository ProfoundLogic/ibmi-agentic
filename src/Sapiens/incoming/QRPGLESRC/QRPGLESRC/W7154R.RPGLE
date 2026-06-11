     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W7154R - NAME CHANGE TRACKING REPORT
     H* ***************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM PRINTS A TRACKING REPORT ON A DAILY BASIS WHICH  *
     H* WILL INDICATE CLAIMS WHOSE NAME HAS CHANGED.                  *
     H*                                                               *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7154C - DAILY NAME CHANGE TRACKING - OVERRIDE CL           *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* CREATED: 09/22/94   C:28        PROJECT: 03BBB                *
     H*                                                               *
     H* ***************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00  C:118  PROJECT: WC987                              *
     H*                   LOAD ALTERNATE COMPANY NAME FROM FUND MASTER*
     H*                   IF AVAILABLE.                               *
     H*                                                               *
     H* ***************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   11  INITIAL SET UP SUBROUTINE                               *
     H*   40  READ CLAIMANT NAME CHANGE TRACKING FILE                 *
     H*   58  READ CLAIM DETAIL PAYMENT FILE                          *
     H*   60  GENERAL CHAIN AND LOKUP INDICATOR                       *
     H*   61  PREVIOUS NAME CHANGES                                   *
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    WDNTL: QUERY SELECTION: CHANGE TYPE "N" OR "B".          **
     H**    WDNTL (PNT01): CO#/FND/FYR/CASE                          **
     H**    WDNTL1(PNT02): CO#/FND/FYR/CASE/TRDT/TIME  OMIT TYPE-'A' **
     H**    WDPAL5(PPA01): CO#/FND/FYR/CASE/TRDT/SEQ#                **
     H**    SMCOL (PCO01): CO#                                       **
     H*     SMORL  (POR01) ORPNAM,ORFIL#                              *
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO#/FND                                   **
     H************************************************************************
     FWDNTL     IF   E           K DISK
     F* * * Claimant Name change Tracking File
     FWDNTL1    IF   E           K DISK
     F                                     RENAME(PNT01:PNT02)
     F* * * Claimant Name change Tracking File
     FWDPAL5    IF   E           K DISK
     F* * * Claim Payment Detail
     FSMCOL     IF   E           K DISK
     F* * * Company File
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D  DSYEAR                11     14  0
     D                 DS
     D  CO                     1     40
     D  CONAME                 1     40
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPNT02
     I              NTCO#                       N2CO#
     I              NTFND                       N2FND
     I              NTFYR                       N2FYR
     I              NTCASE                      N2CASE
     I              NTNNAM                      N2NNAM
     I              NTONAM                      N2ONAM
     I              NTTRDT                      N2TRDT
     I              NTTYPE                      N2TYPE
     I              NTUSER                      N2USER
     I              NTTIME                      N2TIME
     I              NTPGM                       N2PGM
     I              NTDT                        N2DT

     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    NTFND
     C*
     C*   KEY LIST FOR THE CLAIMANT NAME CHANGE TRACKING FILE
     C*
     C     NTKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     NTKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C                   KFLD                    SVTRDT
     C                   KFLD                    SVTIME
     C*
     C     NTKY2B        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C*
     C*   KEY LIST FOR THE CLAIM PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C*
     C*****      *LIKE     DEFN NTTRDT    XXDATE
     C     *LIKE         DEFINE    NTCASE        SVCASE
     C     *LIKE         DEFINE    NTTRDT        SVTRDT
     C     *LIKE         DEFINE    NTTIME        SVTIME
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    NTFYR         SVFYR
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C****       *LIKE     DEFN PATRDT    XPATDT
     C     *LIKE         DEFINE    NTFYR         XXFYR
     C     *LIKE         DEFINE    NTUSER        SVUSER
     C     *LIKE         DEFINE    NTFND         SVFND
     C     *LIKE         DEFINE    NTPGM         SVPGM
     C***        *LIKE     DEFN NTTRDT    PRTRDT
     C     *LIKE         DEFINE    NTONAM        PRONAM
     C     *LIKE         DEFINE    NTONAM        SVONAM
     C     *LIKE         DEFINE    NTNNAM        SVNNAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C  N11              EXSR      SR1ST
     C*
     C** GET THE CLAIMANT NAME CHANGE TRACKING INFO
     C*
     C     NTKY          CHAIN     PNT01                              40
     C*
     C     *IN40         IFEQ      '0'
     C                   OPEN      QSYSPRT
     C                   MOVEL     'Y'           XXQPRT
     C                   MOVEL     'Y'           XXIN40
     C*
     C     XXROUT        IFEQ      'Y'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      NTCASE        SVCASE
     C                   MOVE      NTTRDT        SVTRDT
     C                   MOVE      NTTIME        SVTIME
     C                   MOVE      NTFYR         SVFYR
     C                   MOVEL     NTONAM        SVONAM
     C                   MOVEL     NTNNAM        SVNNAM
     C                   MOVEL     NTPGM         SVPGM
     C                   MOVEL     NTUSER        SVUSER
     C*
     C*  GET FUND DESCRIPTION FIRST TIME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           55
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C                   MOVE      NTFND         XXFND
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C                   EXCEPT    HEAD
     C                   END
     C*
     C     *IN40         DOWEQ     '0'
     C*
     C*  GET FUND DESCRIPTION WHEN CHANGE
     C*
     C     NTFND         IFNE      XXFND
     C     NTCASE        IFNE      SVCASE
     C                   EXSR      SR2ND
     C                   END
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C                   MOVE      NTFND         XXFND
     C                   EXCEPT    HEAD
     C                   END
     C*
     C     NTCASE        IFNE      SVCASE
     C                   EXSR      SR2ND
     C                   END
     C*
     C                   READ      PNT01                                  40
     C                   ENDDO
     C*
     C     XXIN40        IFEQ      'Y'
     C                   EXSR      SR2ND
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Chain NtKy PNT01;
       If %found and XXROUT = 'Y';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      *BLANK        XXQPRT            1
     C                   MOVE      *BLANK        XXIN40            1
     C*
     C* * * INITIALIZE FIELDS
     C*
     C                   TIME                    DSDTTM
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     COKY          CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      *BLANK        XXROUT            1
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C***                  EXCPTROUTE
     C                   MOVEL     'Y'           XXROUT
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   SELECT THE RECORDS TO PRINT      ** Subroutine **   S R 2 N D  *
      *==================================================================*
     C*
     CSR   SR2ND         BEGSR
     C*
     C                   MOVE      NTFND         SVFND
     C                   EXSR      SR3TH
     C*
     C**   PREVIOUS CHANGES TO PRINT
     C*
     C     NTKY2         CHAIN     PNT02                              61
     C     NTKY2B        READPE    PNT02                                  61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     N2ONAM        PRONAM
     C                   Z-ADD     N2TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PRTRDT            8 0
     C                   ELSE
     C                   MOVE      *BLANKS       PRONAM
     C                   MOVE      *BLANKS       PRTRDT
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETAIL
     C*
     C                   MOVE      NTCASE        SVCASE
     C                   MOVE      NTTRDT        SVTRDT
     C                   MOVE      NTTIME        SVTIME
     C                   MOVE      NTFYR         SVFYR
     C                   MOVE      NTONAM        SVONAM
     C                   MOVE      NTNNAM        SVNNAM
     C                   MOVE      NTPGM         SVPGM
     C                   MOVE      NTUSER        SVUSER
     C                   MOVE      SVFND         NTFND
     C                   ENDSR
      *==================================================================*
      *   GET THE LAST COMPENSATION PAYMENT DATE   SBR   **   S R 3 T H  *
      *==================================================================*
     C*
     CSR   SR3TH         BEGSR
     C*
     C                   MOVE      '0'           *IN57
     C     PAKY          SETGT     PPA01                              58
     C     *IN58         DOWEQ     '0'
     C                   READP     PPA01                                  58
     C     PARETP        COMP      'C'                                    57
     C  N57              END
     C                   MOVE      *BLANKS       XPATDT
     C     *IN57         IFEQ      '1'
     C     PACO#         COMP      XXCO#                                  58
     C   58PAFND         COMP      XXFND                                  58
     C   58PAFYR         COMP      SVFYR                                  58
     C   58PACASE        COMP      SVCASE                                 58
     C   58PARETP        COMP      'C'                                    58
     C     *IN58         IFEQ      '1'
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XPATDT            8 0
     C                   END
     C                   END
     C                   ENDSR
      * ===============================================================
     OQSYSPRT   E            ROUTE          1 02
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                 11 'XXXXXXXX'
     O**********                                 23 'XXXXXX'
     O**********                                 37 'XX      XX'
     O**********                                 48 'XXXXXXXXXX'
     O**********                                 59 'XXXXXXXXXX'
     O**********                                 70 'XXX     XX'
     O**********                                 80 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 12 'XXXXXXXXX'
     O**********                                 24 'XXXXXXXX'
     O**********                                 37 'XX      XX'
     O**********                                 48 'XXXXXXXXXX'
     O**********                                 59 'XXXXXXXXXX'
     O**********                                 70 'XXXX    XX'
     O**********                                 81 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 13 'XX      XX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XXXX    XX'
     O**********                                 82 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 13 'XX      XX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX XX   XX'
     O**********                                 74 'XX'
     O**********E            ROUTE          1
     O**********                                 12 'XXXXXXXXX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX XX   XX'
     O**********                                 74 'XX'
     O**********E            ROUTE          1
     O**********                                 11 'XXXXXXXX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX  XX  XX'
     O**********                                 74 'XX'
     O**********E            ROUTE          1
     O**********                                  7 'XXXX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX  XX  XX'
     O**********                                 82 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX   XX XX'
     O**********                                 82 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XX XXX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX   XX XX'
     O**********                                 82 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX  XXX'
     O**********                                 25 'XX      XX'
     O**********                                 37 'XX      XX'
     O**********                                 44 'XX'
     O**********                                 55 'XX'
     O**********                                 70 'XX    XXXX'
     O**********                                 82 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 11 'XX   XXX'
     O**********                                 24 'XXXXXXXX'
     O**********                                 36 'XXXXXXXX'
     O**********                                 44 'XX'
     O**********                                 59 'XXXXXXXXXX'
     O**********                                 70 'XX    XXXX'
     O**********                                 81 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                 12 'XX    XXX'
     O**********                                 23 'XXXXXX'
     O**********                                 35 'XXXXXX'
     O**********                                 44 'XX'
     O**********                                 59 'XXXXXXXXXX'
     O**********                                 70 'XX     XXX'
     O**********                                 80 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              81
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** Beg Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               27 '(C) PROGRESSIVE DATA'
     O****                               43 'SOLUTIONS, INC.'
     O****                     DSYEAR    48
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END CHG ****** 01/28/10 ***********************************
     O*
     O* Report Heading
     O*
     O          E            HEAD           1 06
     O                       Q1PGM               10
     O                       CONAME             114
     O                                          178 'PAGE:'
     O                       PAGE          Z    190
     O          E            HEAD           1
     O                                          178 'RUN DATE:'
     O                       DSDATE             190 '  /  /    '
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       XFNAME              71
     O                                          178 'TIME:'
     O                       DSTIME             190 '  :  :  '
     O          E            HEAD           1
     O                       ORTITL             125
     O          E            HEAD           2
     O                                           94 'BATCH DATE:'
     O                       XXDATE             105 '  /  /    '
     O          E            HEAD           1
     O                                          107 'LAST DT '
     O                                          141 'PREVIOUS'
     O                                          151 'PREVIOUS'
     O          E            HEAD           2
     O                                           11 ' YR - CLAIM'
     O                                           21 'OLD NAME'
     O                                           63 'NEW NAME'
     O                                          106 'COMP PMT'
     O                                          112 'USER'
     O                                          127 'PGM NAME'
     O                                          141 'DATE CHG'
     O                                          151 'OLD NAME'
     O          E            DETAIL         1
     O                       SVFYR         Z      3
     O                       SVCASE        Z     11
     O                       SVONAM              53
     O                       SVNNAM              95
     O                       XPATDT             105 '  /  /    '
     O                       SVUSER             118
     O                       SVPGM              130
     O                       PRTRDT             141 '  /  /    '
     O                       PRONAM             183
