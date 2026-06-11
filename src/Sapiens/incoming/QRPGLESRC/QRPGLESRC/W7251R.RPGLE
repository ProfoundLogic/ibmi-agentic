     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7251R - PRINT REQUEST FOR REFUND REPORT
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 11/25/13 C:282   PROJECT: 129WSC
     H*                  ADD REFUND STATUS CODE AND DESCRIPTION
     H*                  in QSYSPRT.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00 C:118  PROJECT: WC987
     H*                  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*                  IF AVAILABLE.
     H*
     H*  04/05/99 C:118  PROJECT: WC577
     H*                  UPDATED ROUTING PAGE TO INCLUDE GROUP AND LEFT
     H*                  JUSTIFY REPORT TITLE.  UPDATED TITLE ON REPORT
     H*                  TO CENTER UNDER GROUP.
     H*
     F*****************************************************************
     FWMRRL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*** BEG ADD *** 11/25/13 *****************************
     FWTABL     IF   E           K DISK
     F                                     INCLUDE(PRS01)
     F*** END ADD *** 11/25/13 *****************************
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR REQUEST REFUND FILE
     C*
     C     RRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMO FILE - TO GET CLAIMANT NAME
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE CLAIMS FILE - TO GET ACCIDENT DATE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RRFYR
     C                   KFLD                    RRCASE
     C*
     C*   KEY LIST FOR THE FUND FILE - TO GET FUND NAME
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
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
     C*   KEY LIST FOR THE COMPANY FILE - TO GET COMPANY NAME
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEG ADD *** 11/25/13 *****************************
     C*  TABLE FILE KEY
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*** END ADD *** 11/25/13 *****************************
     C*
     C*
     C*   ENTRY PARMS
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQMODE        XXMODE            1
     C* FIRST CYCLE PROCESSING
     C*
     C                   EXSR      SR1ST
     C*
     C*  START READING REQUEST REFUND FILE
     C*
     C     RRKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  CHECK FOR FUND YEAR BREAK
     C*
     C     RRFYR         IFNE      XXHYR
     C     XXHYR         ANDNE     0
     C     XXDETL        ANDEQ     'Y'
     C                   MOVE      'Y'           XXHEAD            1
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   ENDIF                                                  *INOA IFEQ
     C                   EXCEPT    TOTLY
     C*
     C*  CLEAR FUND YEAR TOTALS
     C*
     C                   ADD       XXYREQ        XXTREQ
     C                   ADD       XXYREC        XXTREC
     C                   Z-ADD     0             XXYREQ
     C                   Z-ADD     0             XXYREC
     C                   MOVE      ' '           XXDETL
     C                   ENDIF                                                  RRFYR IFEQ
     C*
     C*   SEE IF THERE ARE ANY REQUESTED REFUND AMOUNTS
     C*
     C     RRRAMT        IFNE      0
     C*
     C*   IF THIS IS THE 1ST RECORD OF A FUND YR BRK PRINT HEADINGS
     C*
     C     XXHEAD        IFEQ      'Y'
     C                   EXCEPT    HEAD
     C                   MOVE      ' '           XXHEAD
     C                   ENDIF                                                  XXHEAD IFEQ
     C*
     C*  GET ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     RRDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTRQ            6 0
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*** BEG CHG *** 11/25/13 ****************************
     C*****              MOVEL     XXLAST        XXNAME           40
     C                   MOVEL     XXLAST        XXNAME           34
     C*** END CHG *** 11/25/13 ****************************
     C                   ELSE
     C                   MOVE      *BLANK        XXNAME
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  CALCULATE FUND YEAR TOTALS
     C*
     C                   ADD       RRRAMT        XXYREQ
     C                   ADD       RRRCVD        XXYREC
     C*** BEG ADD *** 11/25/13 *****************************
     C                   MOVEL(P)  RRSTCD        XXKEY
     C     TBKY          CHAIN     PRS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  RSDESC        XXSTDE           20
     C                   ELSE
     C                   MOVE      *BLANKS       XXSTDE
     C                   ENDIF
     C*** END ADD *** 11/25/13 *****************************
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C                   MOVE      'Y'           XXDETL            1
     C                   Z-ADD     RRFYR         XXHYR
     C                   Z-ADD     RRCASE        XXCASE
     C                   ENDIF                                                  RRRAMT IFEQ
     C*
     C     RRKEY         READE     PRR01                                  60
     C                   END
     C*
     C*  PRINT TOTALS FOR YR AND FUND
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   ENDIF                                                  *INOA IFEQ
     C     XXDETL        IFEQ      'Y'
     C                   EXCEPT    TOTLY
     C                   ENDIF                                                  XXDETL IFEQ
     C*    ACCUMULATE FUND TOTALS
     C                   ADD       XXYREQ        XXTREQ
     C                   ADD       XXYREC        XXTREC
     C*
     C                   MOVE      '1'           *IN12
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTLF
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
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     C     SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'                                          DEMAND MODE
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF                                                  XXMODE IFEQ
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      *BLANKS       XFNAME
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME           60
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL           60
     C***** BEGIN ADD **** 04/05/99 ***********************************
     C*   LEFT JUSTIFY TITLE IN FIELD
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF                                                  N IFLE
     C                   ENDIF                                                  ORTITL IFNE
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*******            EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C***** END ADD ****** 04/05/99 ***********************************
     C*   INITIALIZE FUND YEAR AND FUND TOTALS
     C*
     C                   Z-ADD     0             XXYREQ           11 2
     C                   Z-ADD     0             XXYREC           11 2
     C                   Z-ADD     0             XXTREQ           11 2
     C                   Z-ADD     0             XXTREC           11 2
     C*
     C*   SET FYR FIELD TO 0 & PRINT HEADING
     C*
     C                   Z-ADD     0             XXHYR             3 0
     C                   Z-ADD     0             XXCASE            7 0
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
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
     O                       XXTITL              80
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
     O***** BEGIN ADD **** 04/05/99 ***********************************
     O          E            ROUTE          2
     O                                           20 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           25 'GROUP:'
     O                       XXFND               30
     O***** END ADD ****** 04/05/99 ***********************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O                       *YEAR             +  1
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
      *** BEG CHG *** 11/25/13 *****************************
     O*****                  CONAME              87
     O*****                  DSDATE        Y    132
     O                       CONAME              97
     O                       DSDATE        Y    152
      *** END CHG *** 11/25/13 *****************************
     O          E            HEAD           2
     O                       Q1PGM               10
      *** BEG CHG *** 11/25/13 *****************************
     O*****                  ORTITL              92
     O*****                                     126 'PAGE:'
     O*****                  PAGE          Z    132
     O                       ORTITL             102
     O                                          146 'PAGE:'
     O                       PAGE          Z    152
      *** END CHG *** 11/25/13 *****************************
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O          E            HEAD           1
     O                 N12                          'POLICY'
     O                 N12                       25 'REQUEST'
      *** BEG CHG *** 11/25/13 *****************************
     O*****            N12                       82 'ACCIDENT'
     O*****            N12                       99 'REFUND REQUEST'
     O*****                                     112 'AMOUNT'
     O*****                                     130 'AMOUNT'
     O                 N12                       76 'ACCIDENT'
     O                 N12                       85 'REFUND'
     O                 N12                      121 'REFUND REQUEST'
     O                                          133 'AMOUNT'
     O                                          150 'AMOUNT'
      *** END CHG *** 11/25/13 *****************************
     O          E            HEAD           2
     O                 N12                       14 'YEAR     CASE'
     O                 N12                       24 'NUMBER'
     O                 N12                       41 'CLAIMANT NAME'
      *** BEG CHG *** 11/25/13 *****************************
     O*****            N12                       80 'DATE'
     O*****            N12                       94 'DATE'
     O*****                                     114 'REQUESTED'
     O*****                                     132 'RECOVERED'
     O                 N12                       74 'DATE'
     O                 N12                       85 'STS CD'
     O                 N12                       98 'DESCRIPTION'
     O                 N12                      116 'DATE'
     O                                          135 'REQUESTED'
     O                                          152 'RECOVERED'
      *** END CHG *** 11/25/13 *****************************
     O          E            DET            1
     O                       RRFYR         Z      4
     O                       RRCASE        Z     14
     O                       RRREQ#        Z     25
      *** BEG CHG *** 11/25/13 *****************************
     O*****                  XXNAME              68
     O*****                  XXACDT        Y     82
     O*****                  XXDTRQ        Y     96
     O*****                  RRRAMT        J    115
     O*****                  RRRCVD        J    132
     O                       XXNAME              63
     O                       XXACDT        Y     76
     O                       RRSTCD              83
     O                       XXSTDE             107
     O                       XXDTRQ        Y    118
     O                       RRRAMT        J    136
     O                       RRRCVD        J    152
      *** END CHG *** 11/25/13 *****************************
     O          E            TOTLY       1  2
      *** BEG CHG *** 11/25/13 *****************************
     O*****                                      83 'POLICY YEAR TOTALS:'
     O*****                  XXYREQ        J    115
     O*****                  XXYREC        J    132
     O                                          103 'POLICY YEAR TOTALS:'
     O                       XXYREQ        J    136
     O                       XXYREC        J    152
      *** END CHG *** 11/25/13 *****************************
     O          E            TOTLF          2
      *** BEG CHG *** 11/25/13 *****************************
     O*****                                      83 'GROUP TOTALS:'
     O*****                  XXTREQ        J    115
     O*****                  XXTREC        J    132
     O                                          103 'GROUP TOTALS:'
     O                       XXTREQ        J    136
     O                       XXTREC        J    152
      *** END CHG *** 11/25/13 *****************************
