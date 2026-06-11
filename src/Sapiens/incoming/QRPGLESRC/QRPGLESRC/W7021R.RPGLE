     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7021R - DELETED PAYMENT REGISTER
     H*****************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM CREATES A REPORT SHOWING    *
     H*    ALL PAYMENTS THAT WERE DELETED.                            *
     H* CALLED FROM:  W7021C                                          *
     H* CALLS TO:  NONE                                               *
     H* PROGRAMMER:  LARRY KORZON                                     *
     H* CREATED:  01/05/93            PROJECT: ASO                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H* 02/10/10 C:246 PROJECT: C133WIJ                               *
     H*               ADD Jurisdiction State in the report.           *
     H*                                                               *
     H* 04/10/00  C:118 PROJECT: WC987                                *
     H*                 Load alternate company name from fund master  *
     H*                 if available.                                 *
     H*                                                               *
     H* 08/20/99  C:15  Support: WC025/18690                          *
     H*                 When printing pmts deleted by pgm W7004R,     *
     H*                 print 'W7004R' in the 'User who deleted       *
     H*                 pmt' field.                                   *
     H*                                                               *
     H* 06/17/98  C:35  PROJECT: SUPPORT WA033 12200                  *
     H*                 CORRECT PRINTING OF DATES.                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*  *IN60  -  GENERAL USAGE (CHAIN)                              *
     H*  *IN61  -  END OF FILE (WACPP)                                *
     H*  *IN62  -  END OF FILE (WARPP)                                *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWACPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTPSL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*                                                               *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE PAYMENT STATUS FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C*
     C*  DATE ROUTINE PARM LISTS
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
     C     *LIKE         DEFINE    RPCO#         XXCO#
      **** BEGIN CHG **** 06/17/98 ************************************
     C********** *LIKE     DEFN RPTRDT    XXDATE
     C                   Z-ADD     0             XXDATE            8 0
      **** END   CHG **** 06/17/98 ************************************
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C     *LIKE         DEFINE    PSKEY         XXKEY
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   TIME                    DSDTTM
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
     C*   START READING THE CHECK PROCESSING FILE
     C*
     C     CPKY          CHAIN     PCP01                              61
     C     *IN61         DOWEQ     '0'
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INOA
     C*
     C*   START READING THE REQUEST PAYMENT FILE
     C*
     C     RPKY          CHAIN     PRP01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C*   CHECK FOR DELETED PAYMENTS
     C*
     C     RPSTAT        IFEQ      'D'
     C*
     C*   FIRST TIME THROUGH, SAVE THE FUND YEAR
     C*
     C     *IN80         IFEQ      '0'
     C                   MOVE      RPFYR         XXFYR             3 0
     C                   MOVE      '1'           *IN80
     C*
     C*   GET THE FUND RECORD
     C*
     C                   MOVE      *BLANK        FNNAME
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C                   END
     C*
     C*   IF THE FUND YEAR HAS CHANGED, THEN PRINT THE FUND YEAR
     C*   TOTALS
     C*
     C     XXFYR         IFNE      RPFYR
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    FYRT
     C                   Z-ADD     RPFYR         XXFYR
     C                   MOVE      *ZERO         XXFYRT
     C                   MOVE      *ZERO         XXFYCT
     C*
     C                   END
     C*
     C*   CHAIN TO THE CLAIM MASTER FILE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C*   CHAIN TO THE DEMOGRAPHICS FILE
     C*
     C*** BEG ADD ****** 02/10/10 ************************************
     C                   MOVE      CMJRST        XXST              2
     C*** BEG END ****** 02/10/10 ************************************
     C                   MOVE      *BLANK        XXLSN            20
     C  N60CDKY          CHAIN     PCD01                              60
     C  N60              MOVEL     CDLSN         XXLSN
     C*
     C*   GET THE PEND STATUS CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXDESC           20
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPSTS         XXKEY
     C     PSKY          CHAIN     PPS01                              60
     C  N60              MOVEL     PSDESC        XXDESC
     C*
     C                   Z-ADD     RPRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      **** BEGIN CHG **** 06/17/98 ************************************
     C******************** MOVELSRDTO     XXRCVD  60
     C                   MOVEL     SRDTO         XXRCVD            8 0
      **** END   CHG **** 06/17/98 ************************************
     C                   Z-ADD     RPDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      **** BEGIN CHG **** 06/17/98 ************************************
     C******************** MOVELSRDTO     XXDUE   60
     C                   MOVEL     SRDTO         XXDUE             8 0
      **** END   CHG **** 06/17/98 ************************************
     C*
     C*** BEGIN ADD *** 08/20/99 **************************************
     C*  If the pmt was deleted by W7004R, replace the 'User who
     C*    deleted pmt' field.
     C*
     C     RPPGM         IFEQ      'W7004R'
     C                   MOVEL(P)  'W7004R'      RPUSER
     C                   END
     C*** END ADD ***** 08/20/99 **************************************
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C*   ADD TO THE TOTALS
     C*
     C                   ADD       RPAMT         XXFYRT            9 2
     C                   ADD       RPAMT         XXFNDT            9 2
     C                   ADD       1             XXFYCT            5 0
     C                   ADD       1             XXFNCT            5 0
     C                   END
     C*
     C     RPKY          READE     PRP01                                  62
     C                   END
     C*
     C*  PRINT THE FUND YEAR AND FUND TOTALS IF ANY CHECKS WERE DELETED
     C*
     C     *IN80         IFEQ      '1'
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    FYRT
     C                   EXCEPT    FNDTT
     C                   MOVE      *ZERO         XXFYRT
     C                   MOVE      *ZERO         XXFYCT
     C                   Z-ADD     0             XXFNCT
     C                   Z-ADD     0             XXFNDT
     C                   Z-ADD     0             PAGE
     C                   MOVE      '0'           *IN80
     C                   ELSE
     C                   MOVE      *BLANK        FNNAME
     C     FNKY          CHAIN     PFN01                              60
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   END
     C*
     C     CPKY          READE     PCP01                                  61
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
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
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
      **** BEGIN CHG **** 06/17/98 ************************************
     O************************ XXDATEY  198
     O                       XXDATE             198 '  /  /    '
      **** END   CHG **** 06/17/98 ************************************
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             128
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           05 'FUND:'
     O                       FNFND         Z     09
     O                       FNNAME              70
     O          E            HEAD           1
     O**** BEG CHG 02/10/10******************************************
     O**                                104 'DATE'
     O**                                128 'STATUS'
     O                                           21 'JURISCDITION'
     O                                          113 'DATE'
     O                                          137 'STATUS'
     O**** END CHG 02/10/10******************************************
     O          E            HEAD           2
     O                                           03 'YR'
     O                                           09 'CASE'
     O**** BEG CHG 02/10/10******************************************
     O**                                 20 'CLAIMANT'
     O**                                 37 'PMT CD'
     O**                                 44 'PAYEE'
     O**                                 52 'MOD'
     O**                                 65 'PAYABLE TO:'
     O**                                106 'RECEIVED'
     O**                                118 'DUE DATE'
     O**                                127 'CODE'
     O**                                140 'DESCRIPTION'
     O**                                173 'USER WHO DELETED PMT'
     O**                                189 'AMOUNT'
     O                                           17 'STATE'
     O                                           30 'CLAIMANT'
     O                                           47 'PMT CD'
     O                                           53 'PAYEE'
     O                                           62 'MOD'
     O                                           75 'PAYABLE TO:'
     O                                          115 'RECEIVED'
     O                                          127 'DUE DATE'
     O                                          136 'CODE'
     O                                          149 'DESCRIPTION'
     O                                          185 'USER WHO DELETED PMT'
     O                                          196 'AMOUNT'
     O****END CHG  02/10/10******************************************
     O          E            DET            1
     O                       RPFYR         Z     03
     O                       RPCASE        Z     11
     O**** BEG CHG 02/10/10******************************************
     O**                       XXLSN     32
     O**                       RPTRCD    35
     O**                       RPFEINZ   48
     O**                       RPMOD Z   52
     O**                       RPPNAM    94
     O**                       XXRCVD   106 '  /  /    '
     O**                       XXDUE    118 '  /  /    '
     O**                       RPSTS    126
     O**                       XXDESC   149
     O**                       RPUSER   168
     O**                       RPAMT J  190
     O                       RPCASE        Z     11
     O                       XXST                16
     O                       XXLSN               41
     O                       RPTRCD              45
     O                       RPFEIN        Z     57
     O                       RPMOD         Z     62
     O                       RPPNAM             107
     O                       XXRCVD             117 '  /  /    '
     O                       XXDUE              129 '  /  /    '
     O                       RPSTS              135
     O                       XXDESC             157
     O                       RPUSER             181
     O                       RPAMT         J    196
     O**** END CHG 02/10/10******************************************
     O          E            FYRT        1  2
     O**** BEG CHG 02/10/10******************************************
     O**                                139 'FUND YEAR:'
     O**                       XXFYR Z  144
     O**                                156 'COUNT:'
     O**                       XXFYCTZ  162
     O**                                177 'TOTAL:'
     O**                       XXFYRTJ  190
     O                                          150 'FUND YEAR:'
     O                       XXFYR         Z    157
     O                                          162 'COUNT:'
     O                       XXFYCT        Z    168
     O                                          180 'TOTAL:'
     O                       XXFYRT        J    196
     O***** END CHG 02/10/10************
     O          E            FNDTT       1  2
     O***** BEG CHG 02/10/10************
     O**                                134 'FUND:'
     O**                       CPFND Z  139
     O**                                156 'FINAL COUNT:'
     O**                       XXFNCTZ  162
     O**                                177 'FINAL TOTAL:'
     O**                       XXFNDTJ  190
     O                                          131 'FUND:'
     O                       CPFND         Z    135
     O                                          150 'FINAL COUNT:'
     O                       XXFNCT        Z    155
     O                                          180 'FINAL TOTAL:'
     O                       XXFNDT        J    196
     O***** END CHG 02/10/10************
     O**** ERROR MESSAGE, IF NO RECORDS ARE PROCESSED *****************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
