     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7851R - CLAIM HISTORY REPORT
     H*
     H*================================================================
     H*  REVISIONS
     H*                                                               *
     H* 11/23/20 C:062 Project: WC2749                                *
     H*                Change check# heading to REF #                 *
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
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Use PARQDT instead of PAPODT if PARQDT not zero*
     H*
     H*  04/10/00  C:118  PROJECT: WC987
     H*                   LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*
     H*================================================================
     FSMCOL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************

     I*
     I*

     C*
     C     *LIKE         DEFINE    FNDT          XXFRDT
     C     *LIKE         DEFINE    FNDT          XXTODT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C*
     C                   MOVE      QQFRDT        XXFROM            8 0
     C                   MOVE      QQTODT        XXTO              8 0
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1FROM            6 0
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1TO              6 0
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE FUND
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
     C     UPKEY2        KLIST
     C                   KFLD                    CMSUPR
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR CLAIM HISTORY
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     CHKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C                   KFLD                    XXFRDT
     C*
     C*   KEY LIST FOR THE PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
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
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
      ***** Beg Del ***** 08/26/14 *************************************
     C********           MOVE      '0'           XXPRT             1
     C********     ORKEY         CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C********           EXCEPT    ROUTE
     C********           END
      ***** End Del ***** 08/26/14 *************************************
     C                   MOVE      '1'           *INOA
     C*
     C*  INITIALIZE REC INDICATOR TO ZERO
     C*
     C                   Z-ADD     0             REC               1 0
     C*
     C*  START READING CLAIM HISTORY FILE
     C*
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  IF THERE IS A REOPEN WITHIN THE TIME PERIOD, PRINT
     C*
     C                   MOVE      '1'           *IN78
     C                   MOVE      'N'           XXPRNT            1
     C                   Z-ADD     CHCASE        XXCASE            7 0
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CHOCST        IFEQ      'C'
     C     CHKEY2        READE     PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHOCST        ANDEQ     'O'
     C     CHEFDT        ANDGE     XXFRDT
     C     CHEFDT        ANDLE     XXTODT
     C                   MOVE      'Y'           XXPRNT
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C*
     C  N60CHKEY2        READE     PCH01                                  60
     C                   END
     C*
     C     XXPRNT        IFEQ      'Y'
     C*
     C* SET REC INDICATOR TO ONE TO INDICATE A RECORD WAS PRINTED.
     C*
     C                   Z-ADD     1             REC
     C*
     C*   GET CLAIMANT NAME & ACCUM TOTAL PAID & INCURRED
     C*
     C                   MOVE      *BLANKS       XXNAME           25
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   XFOOT     CMA           XXPAID           11 2
     C                   XFOOT     CMR           XXRES            11 2
     C     XXPAID        ADD       XXRES         XXTOTI           11 2
     C*
     C     CDKEY         CHAIN     PCD01                              60
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
     C*   GET SUPERVISOR NAME
     C*
     C                   MOVE      *BLANKS       XXSUPR           15
     C     UPKEY2        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXSUPR
     C                   END
     C*
     C     CHKEY2        CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   MOVE      *BLANKS       XXEXPL           25
     C                   MOVEL     CHEXPL        XXEXPL
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '1'           *IN78
     C                   END
     C                   EXCEPT    DET
     C   78              MOVE      '0'           *IN78
     C*
     C     CHKEY2        READE     PCH01                                  60
     C                   END
     C*
     C*   PRINT PAYMENT HISTORY INFO
     C*
     C     PAKEY1        SETLL     PPA01
     C     PAKEY         READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        IFGE      XXFRDT
     C     PATRDT        ANDLE     XXTODT
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C************         Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXSFRM            6 0
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXSTO             6 0
     C*
     C                   MOVE      *BLANKS       XXPNAM           25
     C                   MOVE      PAFEIN        XXFEIN            9
     C     PAFEIN        IFEQ      0
     C     CMSS#         OREQ      XXFEIN
     C                   MOVE      XXNAME        XXPNAM
     C                   ELSE
     C     PDKEY         CHAIN     PPD01                              61
     C                   MOVEL     PDNAME        XXPNAM
     C                   END
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    PAYH
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN60
     C*
     C                   END
     C*
     C  N60PAKEY         READE     PPA01                                  60
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*   GET NEXT CASE
     C*
     C     CHKEY2        SETGT     PCH01
     C     CHKEY         READE     PCH01                                  60
     C                   END
     C*
     C*  NOTIFIES IN OUTPUT WHEN NO RECORDS WERE SELECTED
     C*
     C     REC           IFEQ      *ZERO
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOSEL
     C*
     C                   ENDIF                                                  REC EQ 0
     C*
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
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************
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
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
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
      ***** Beg Del ***** 08/26/14 *************************************
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
      ***** End Del ***** 08/26/14 *************************************
     O          E            ROUTE          2 35
     O                                           49 'SELECTION CRITERIA:'
     O          E            ROUTE          2
     O                                           11 'YEAR  . . :'
     O                       XXFYR         X     15
     O          E            ROUTE          2
     O                                           11 'DATE RANGE:'
     O                       X1FROM              20 '0  /  /  '
     O                                           22 '-'
     O                       X1TO                31 '0  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O****                     *YEAR     51
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
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O                                           94 'POLICY YEAR:'
     O                       XXFYR         Z     98
     O                                          112 'DATE RANGE:'
     O                       X1FROM             121 '0  /  /  '
     O                                          123 '-'
     O                       X1TO               132 '0  /  /  '
     O          E            HEAD           2
     O                                           21 'CASE# CLAIMANT NAME'
     O                                           57 'ACC DATE     TOTAL PAID'
     O                                           73 'TOTAL INCURRED'
     O                                           97 'SUPERVISOR      DATE &'
     O                                          116 'STATUS EXPLANATION'
     O          E            HEAD           1
     O                                           44 'TRANS  RSV'
     O                                           78 'SERVICE'
     O          E            HEAD           1
     O                                           45 'CD TP CD TP'
     O                                           71 'TRAN DT  POST DT    FROM'
     o*** Begin Chg *** 11/23/20 *******************************************
     O**********                                 96 'TO    CHECK#  PAYEE'
     O                                           96 'TO     REF #  PAYEE'
     o*** End   Chg *** 11/23/20 *******************************************
     O                                          129 'AMOUNT'
     O          E    78      DET         1
     O          E            DET         1
     O               78      CHCASE        Z     07
     O               78      XXNAME              33
     O               78      XXACDT        Y     42
     O               78      XXPAID        J     58
     O               78      XXTOTI        J     74
     O               78      XXSUPR              90
     O                       XXEFDT        Y     99
     O                       CHOCST             102
     O                       XXEXPL             130
     O          E            PAYH        1
     O                       PATRCD              36
     O                       PATYPE              38
     O                       PARECD              42
     O                       PARETP              44
     O                       XXTRDT              54 '  /  /  '
     O                       XXPODT              63 '  /  /  '
     O                       XXSFRM              73 '  /  /  '
     O                       XXSTO               82 '  /  /  '
     O                       PACHK#        Z     90
     O                       XXPNAM             116
     O                       PAAMT         J    132
     O          E            NOSEL       1
     O                                           18 '***NO RECORDS MEET'
     O                                           40 'SELECTION CRITERIA***'
