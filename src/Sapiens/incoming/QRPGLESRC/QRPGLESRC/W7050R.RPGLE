     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W7050R - CLAIMS HISTORY FILE EDIT LISTING
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                IF AVAILABLE.                                  *
     H*                                                               *
     H* 08/05/99 C:118 PROJECT: WC577                                 *
     H*                Verbiage changes.                              *
     H*                                                               *
     H* 05/28/98 C:15 Support: WC078/11410                            *
     H*              -Fixed pgm to read only records in the current   *
     H*               month.                                          *
     H*****************************************************************
     F*
     F* * * Reserve History File
     FWMCHL2    IF   E           K DISK
     F*
     F* * * Batch Routing File
     FSMORL     IF   E           K DISK
     F*
     F* * * Company File
     FSMCOL     IF   E           K DISK
     F*
     F* * * Fund File
     FWMFNL     IF   E           K DISK
     F*
     F* * * CLAIMANT DEMOGRAPHICS FILE
     FWMCDL     IF   E           K DISK
     F* * * CLAIM FILE
     FWMCML1    IF   E           K DISK
     F* * * User Parameter File
     FWMUPL     IF   E           K DISK
     F*
     F* * * Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CHFND
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE CLAIM FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CHFND
     C                   KFLD                    CHFYR
     C                   KFLD                    CHCASE
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    CHUSER
     C*
     C*  CLAIMS HISTORY FILE KEY
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   KFLD                    CHFND
     C                   KFLD                    CHUSER
     C*** END ADD ***** 05/28/98 **************************************
     C                   KFLD                    XXDATE
     C*
     C     CHKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C     CHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CHFND
     C                   KFLD                    CHUSER
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C/EJECT
     C*
     C     *LIKE         DEFINE    CHCO#         XXCO#
     C     *LIKE         DEFINE    CHDT          XXDATE
     C*** BEGIN ADD *** 05/28/98 **************************************
     C     *LIKE         DEFINE    CHDT          XXTDAT
     C*** END ADD ***** 05/28/98 **************************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   MOVE      '01'          XXDATE
     C                   MOVE      XXDATE        XXTDAT
     C                   MOVE      '31'          XXTDAT
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*  GET THE COMPANY RECORD.
     C*
     C     COKY          CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C                   TIME                    DSDTTM
     C**********************
     C*  BEGIN MAIN LOOP
     C**********************
     C*** BEGIN DLT *** 05/28/98 **************************************
     C********** CHKY      SETLLPCH01
     C********** CHKY1     CHAINPCH01                40
     C**********           MOVE '31'      XXDATE
     C*** END DLT ***** 05/28/98 **************************************
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C*  GET THE FIRST CLAIM HISTORY RECORD
     C*
     C     CHKY1         CHAIN     PCH01                              40
     C*
     C     NXTCLM        TAG
     C     CHKY          SETLL     PCH01
     C     CHKY2         READE     PCH01                                  40
     C*** END ADD ***** 05/28/98 **************************************
     C*
B01  C     *IN40         DOWEQ     '0'
     C*** BEGIN DLT *** 05/28/98 **************************************
     C********** CHDT      ANDLTXXDATE
     C*** END DLT ***** 05/28/98 **************************************
     C*** BEGIN ADD *** 05/28/98 **************************************
     C     CHEFDT        ANDLE     XXTDAT
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C* * * INITAIL SET-UP
     C  N11              EXSR      SR1ST
     C*
     C* * * CHECK FOR LEVEL BREAKS
     C     CHFND         CASNE     XHFND         SRFBRK
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   ENDCS
     C*** END ADD ***** 05/28/98 **************************************
     C     CHUSER        CASNE     XHUSER        SRUBRK
     C                   ENDCS
     C*
     C* * *  DETAIL ROUTINE
     C*
     C* * * CHECK TO SEE IF OVERFLOW OCCURED ON LAST OUTPUT
B02  C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    UHEAD
     C                   MOVE      '0'           *INOF
E02  C                   END
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C*** BEGIN DLT *** 05/28/98 **************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXEFDT  60
     C*** END DLT ***** 05/28/98 **************************************
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*   GET THE CLAIMANT'S NAME.
     C*
     C     CMKY          CHAIN     PCM01                              60
B02  C     *IN60         IFEQ      '0'
     C*
     C     CDKY          CHAIN     PCD01                              60
B03  C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XXNAME           30
X03  C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME
E03  C                   END
     C*
X02  C                   ELSE
     C                   MOVE      *BLANK        XXNAME
E02  C                   END
     C*
     C* * *  ACCRUE FUND TOTALS AND GRAND TOTALS HERE
B02  C     CHOCST        IFEQ      'C'
     C                   ADD       1             XFCTOT            5 0
X02  C                   ELSE
     C                   ADD       1             XFOTOT            5 0
E02  C                   END
     C*
     C* * * PRINT DETAIL LINES
     C                   EXCEPT    DETL
     C*
     C*** BEGIN DLT *** 05/28/98 **************************************
     C********** CHKY1     READEPCH01                    40
     C*** END DLT ***** 05/28/98 **************************************
     C*** BEGIN ADD *** 05/28/98 **************************************
     C     CHKY2         READE     PCH01                                  40
     C*** END ADD ***** 05/28/98 **************************************
     C*
E01  C                   ENDDO
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C*  Get the next claim history record which is within the current
     C*    month.
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READE     PCH01                                  40
     C     *IN40         IFEQ      '0'
     C                   GOTO      NXTCLM
     C                   ENDIF
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C* * * AT END PRINT FINAL FUND TOTAL AND GRAND TOTAL
     C   11              EXSR      SRFPRT
     C*
     C*  END OF PROGRAM
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain orKy POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C/EJECT
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      CHFND         XHFND             3 0
     C                   MOVE      CHUSER        XHUSER           10
     C*
     C* * * GET FUND NAME FROM FUND MASTER FILE
     C     FNKY          CHAIN     PFN01                              42
B01  C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
X01  C                   ELSE
     C                   MOVEL     *BLANKS       XFNAME
E01  C                   END
     C*
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKY          CHAIN     PUP01                              43
B01  C     *IN43         IFEQ      '0'
     C                   MOVEL     UPUSNM        XUNAME           50
X01  C                   ELSE
     C                   MOVEL     *BLANKS       XUNAME
E01  C                   END
     C*
     C* * * OPEN PRINT FILE
     C                   OPEN      QSYSPRT
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
B01  C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
E01  C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    UHEAD
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Level Break on Fund #             ** Subroutine **  S R F B R K *
      *==================================================================*
     C*
     CSR   SRFBRK        BEGSR
     C*
     C* * * PRINT FUND TOTALS
     C                   EXSR      SRFPRT
     C*
     C* * * CLEAR FUND TOTALS
     C                   MOVE      *ZEROS        XFOTOT
     C                   MOVE      *ZEROS        XFCTOT
     C*
     C* * * GET NEW FUND INFORMATION
     C     FNKY          CHAIN     PFN01                              42
B01  C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME
X01  C                   ELSE
     C                   MOVEL     *BLANKS       XFNAME
E01  C                   END
     C*
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      CHFND         XHFND
     C*
     C* * * FORCE USER BREAK
     C                   MOVE      *BLANKS       XHUSER
     C*
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Print Fund Totals                 ** Subroutine **  S R F P R T *
      *==================================================================*
     C*
     CSR   SRFPRT        BEGSR
     C*
     C                   EXCEPT    FTOTL
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Level Break on User               ** Subroutine **  S R U B R K *
      *==================================================================*
     C*
     CSR   SRUBRK        BEGSR
     C*
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKY          CHAIN     PUP01                              43
B01  C     *IN43         IFEQ      '0'
     C                   MOVEL     UPUSNM        XUNAME           50
X01  C                   ELSE
     C                   MOVEL     *BLANKS       XUNAME
E01  C                   END
     C*
     C* * * MOVE USER NAME TO HOLD FIELD
     C                   MOVEL     CHUSER        XHUSER
     C*
     C* * * PRINT NEW USER HEADING
     C                   EXCEPT    UHEAD
     C*
     C                   ENDSR
     C**************************************************************************
     O/EJECT
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
      *----------- REPORT OUTPUT STARTS HERE ------------------------*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********                         05 'FUND:'
     O                                           06 'GROUP:'
     O**********               CHFND Z   09
     O                       CHFND         Z     10
     O***** END   CHG **** 08/05/99 ***********************************
     O                       XFNAME              70
     O          E            HEAD           1
     O                                            3 'YR'
     O                                           11 'CASE'
     O                                           26 'CLAIMANT NAME'
     O                                           54 'EFF DATE'
     O                                           79 'EXPLANATION FOR CHANGE'
     O                                          129 'OPEN/CLOSE'
     O          E            UHEAD       1  2
     O                       CHUSER              10
     O                       XUNAME              62
     O          E            DETL           1
     O                       CHFYR         4      3
     O                       CHCASE        4     11
     O                       XXNAME              43
     O                       XXEFDT              54 '  /  /    '
     O                       CHEXPL             116
     O                       CHOCST             124
     O          E            FTOTL       3
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********                         10 'FUND TOTAL'
     O                                           11 'GROUP TOTAL'
     O***** END   CHG **** 08/05/99 ***********************************
     O                                           35 'NUMBER OF OPEN:'
     O                       XFOTOT        Z     41
     O                                           67 'NUMBER OF CLOSED:'
     O                       XFCTOT        Z     73
