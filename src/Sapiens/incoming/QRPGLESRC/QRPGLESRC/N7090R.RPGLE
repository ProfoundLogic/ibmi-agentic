     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE N7090R - ALL UNPAID CHARGES REPORT BY CASE - REPORT PGM
     H***************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H***************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFDCAL2    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXCAS#                 1     10
     D  XZFYR                  1      2  0
     D  XXHYP1                 3      3
     D  XXCASE                 4     10  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     d***** Beg Add *** 06/09/20 *******************************************
     d xxss#           s              9    inz
     d***** End Add *** 06/09/20 *******************************************

     C*
     C*   KEY LISTS FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     CAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     CAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     CAKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    CAFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS MASTER FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE TABLE FILES
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
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
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C                   MOVEA     '000'         *IN(40)
     C     XXFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN40
     C                   END
     C     XXFYR         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN41
     C                   END
     C     XXCASE        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C*
     C                   Z-ADD     *ZERO         XXCCNT            4 0          # CHRGS
     C                   Z-ADD     *ZERO         XXCAU             7 2          ANESTH UNITS
     C                   Z-ADD     *ZERO         XXCUC             7 2          UNITS CHRG
     C                   Z-ADD     *ZERO         XXCTC            10 2          CASE CHRGS
     C                   Z-ADD     *ZERO         XXCTA            10 2               ALLOW
     C                   Z-ADD     *ZERO         XXCTD            10 2               DISALLW
     C                   Z-ADD     *ZERO         XXCTP            10 2               PAID
     C*
     C                   Z-ADD     *ZERO         XXYTC            11 2          FYR  CHRGS
     C                   Z-ADD     *ZERO         XXYTA            11 2               ALLOW
     C                   Z-ADD     *ZERO         XXYTD            11 2               DISALLW
     C                   Z-ADD     *ZERO         XXYTP            11 2               PAID
     C*
     C                   Z-ADD     *ZERO         XXFTC            12 2          FND  CHRGS
     C                   Z-ADD     *ZERO         XXFTA            12 2               ALLOW
     C                   Z-ADD     *ZERO         XXFTD            12 2               DISALLW
     C                   Z-ADD     *ZERO         XXFTP            12 2               PAID
     C*
     C                   Z-ADD     *ZERO         XXTTC            13 2          ALL  CHRGS
     C                   Z-ADD     *ZERO         XXTTA            13 2               ALLOW
     C                   Z-ADD     *ZERO         XXTTD            13 2               DISALLW
     C                   Z-ADD     *ZERO         XXTTP            13 2               PAID
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET THE USER PARAMETER INFORMATION
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   GET THE COMPANY INFORMATION
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
     C*   GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           MOVE      '0'           XXPRT
     C********     ORKEY         CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C*********          EXCEPT    ROUTE
     C*********          END
      ***** End Del ***** 08/26/14 *********************************************
     C****************************************************************
     C*   CHAIN TO THE CASE DETAIL FILE FOR ALL FUNDS
     C****************************************************************
     C   40CAKEY         CHAIN     PCA01                              50
     C*
     C****************************************************************
     C*   CHAIN TO THE CASE DETAIL FILE FOR ONE FUND
     C****************************************************************
     C  N40
     CAN 41CAKEY1        CHAIN     PCA01                              50
     C*
     C****************************************************************
     C*   CHAIN TO THE CASE DETAIL FILE FOR ONE FUND; ONE FUND YEAR
     C****************************************************************
     C  N40
     CANN41
     CAN 42CAKEY2        CHAIN     PCA01                              50
     C*
     C****************************************************************
     C*   CHAIN TO THE CASE DETAIL FILE FOR ONE CASE
     C****************************************************************
     C  N40
     CANN41
     CANN42CAKEY3        CHAIN     PCA01                              50
     C*
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN80
     C                   MOVE      CAFND         XXFND
     C                   MOVE      CAFYR         XXFYR
     C                   MOVE      CACASE        XXCASE
     C                   EXSR      GETNAM
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END
     C*
     C*   DO LOOP FOR CASE DETAIL FILE
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C                   ADD       CAANMN        XXCAU
     C                   ADD       CAUNTS        XXCUC
     C                   ADD       CATOTC        XXCTC
     C                   ADD       CATOTA        XXCTA
     C                   ADD       CATOTD        XXCTD
     C                   ADD       CATOTP        XXCTP
     C                   ADD       1             XXCCNT
     C*
     C*   CHAIN TO CLAIM STATUS TABLE FILE TO SEE IF OVERRIDE
     C*
     C                   MOVEL     CACSCD        XXKEY
     C     TBKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSOVRD        IFEQ      'Y'
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C*   CONVERT DATES
     C*
     C*
     C                   Z-ADD     CARDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRDAT            6 0
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFDAT            6 0
     C*
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTDAT            6 0
     C*
     C*   PRINT DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     CAEOB#        IFEQ      0
     C                   MOVE      '1'           *IN68                          NOT REQUESTD
     C                   ELSE
     C                   MOVE      '0'           *IN68
     C                   END
     C*
     C     CAMOD         IFNE      0
     C                   MOVE      '1'           *IN67
     C                   ELSE
     C                   MOVE      '0'           *IN67
     C                   END
     C*
     C     CAPMOD        IFNE      *BLANKS
     C                   MOVE      '1'           *IN66
     C                   ELSE
     C                   MOVE      '0'           *IN66
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C*   GET NEXT FUND / FUND YEAR / CASE IF NECESSARY
     C*
     C   40CAKEY         READE     PCA01                                  50
     C  N40
     CAN 41CAKEY1        READE     PCA01                                  50
     C  N40
     CANN41
     CAN 42CAKEY2        READE     PCA01                                  50
     C  N40
     CANN41
     CANN42CAKEY3        READE     PCA01                                  50
     C*
     C*   IF WE GET A HIT, CHECK FOR A CASE/FUND YEAR/FUND BREAK
     C*
     C     *IN50         IFEQ      '0'
     C*
     C                   MOVE      '0'           XXNEXT            1
     C*
     C*   CHECK FOR A CASE CHANGE
     C*
     C     CACASE        IFNE      XXCASE
     C                   EXSR      SRCASE
     C                   Z-ADD     0             XXCCNT
     C                   MOVE      CACASE        XXCASE
     C                   MOVE      '1'           XXNEXT
     C                   END                                                    END NE XXCASE
     C*
     C*   CHECK FOR A FUND YEAR CHANGE
     C*
     C     CAFYR         IFNE      XXFYR
     C                   EXSR      SRFYR
     C                   MOVE      '1'           XXNEXT
     C                   END                                                    END NE XXFYR
     C*
     C*   CHECK FOR A FUND CHANGE
     C*
     C     CAFND         IFNE      XXFND
     C                   EXSR      SRFUND
     C                   MOVE      '1'           XXNEXT
     C                   END                                                    END NE XXFND
     C*
     C*   IF LEVEL BREAK OCCURRED, PRINT NEXT CASE HEADING INFORMATION
     C*
     C     XXNEXT        IFEQ      '1'                                          LEVEL BREAK TA
     C                   EXSR      GETNAM
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           XXNEXT
     C                   END                                                    END IF XXNEX1
     C*
     C                   END                                                    END IF *IN501
     C*
     C*   END DO LOOP FOR CASE DETAIL FILE
     C*
     C                   END                                                    DO LOOP PCA01
     C***************************************
     C*     "LAST RECORD" PROCESSING        *
     C***************************************
     C     *IN80         IFEQ      '1'                                          ANY VALID DATA
     C*
     C*   PRINT LAST CASE INFORMATION
     C*
     C                   EXSR      SRCASE
     C     *IN42         IFEQ      '0'
     C                   GOTO      TERM
     C                   END                                                    END NR *IN42
     C*
     C*   PRINT LAST FUND YEAR INFORMATION
     C*
     C                   EXSR      SRFYR
     C     *IN41         CABEQ     '0'           TERM
     C*
     C*   PRINT LAST FUND INFORMATION
     C*
     C                   EXSR      SRFUND
     C     *IN40         CABEQ     '0'           TERM
     C*
     C*   PRINT ALL FUND INFORMATION, IF APPLICABLE
     C*
     C                   EXCEPT    FINTOT
     C*
     C                   ELSE                                                   ELSE *IN80
     C*
     C*   PRINT "0 RECORDS PROCESSED" IF INDICATOR NOT SET
     C*
     C                   EXCEPT    NOREC
     C*
     C                   END                                                    END IF *IN80
     C*
     C*   END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************
     C*   GET CLIENT NAME FROM THE CASE MASTER FILE
     C*****************************************************************
     C*
     C     GETNAM        BEGSR
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXFNNM           40
     C                   ELSE
     C                   MOVEL     FNNAME        XXFNNM
     C                   END
     C*
     C     CMKEY         CHAIN     PCM01                              45
     C     *IN45         IFEQ      '0'
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       xxSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C     CDKEY         CHAIN     PCD01                              45
     C     *IN45         IFEQ      '0'
     C                   MOVEL     *BLANKS       XXCMNM           56
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        XXCMNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXCMNM
     C                   END
     C*
     C                   END
     C                   Z-ADD     XXFYR         XZFYR             2 0
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   CASE LEVEL BREAK SUBROUTINE
     C*****************************************************************
     C     SRCASE        BEGSR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    CASTOT
     C*
     C                   ADD       XXCTC         XXYTC
     C                   ADD       XXCTA         XXYTA
     C                   ADD       XXCTD         XXYTD
     C                   ADD       XXCTP         XXYTP
     C*
     C                   ADD       XXCTC         XXFTC
     C                   ADD       XXCTA         XXFTA
     C                   ADD       XXCTD         XXFTD
     C                   ADD       XXCTP         XXFTP
     C*
     C                   Z-ADD     0             XXCAU
     C                   Z-ADD     0             XXCUC
     C                   Z-ADD     0             XXCTC
     C                   Z-ADD     0             XXCTA
     C                   Z-ADD     0             XXCTD
     C                   Z-ADD     0             XXCTP
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   FUND YEAR LEVEL BREAK SUBROUTINE
     C*****************************************************************
     C     SRFYR         BEGSR
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    FYRTOT
     C*
     C                   Z-ADD     0             XXYTC
     C                   Z-ADD     0             XXYTA
     C                   Z-ADD     0             XXYTD
     C                   Z-ADD     0             XXYTP
     C*
     C                   MOVE      CAFYR         XXFYR
     C                   MOVE      CACASE        XXCASE
     C*
     C                   ENDSR
     C***
     C*
     C*****************************************************************
     C*   FUND LEVEL BREAK SUBROUTINE
     C*****************************************************************
     C     SRFUND        BEGSR
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDTOT
     C*
     C                   ADD       XXFTC         XXTTC
     C                   ADD       XXFTA         XXTTA
     C                   ADD       XXFTD         XXTTD
     C                   ADD       XXFTP         XXTTP
     C*
     C                   Z-ADD     0             XXFTC
     C                   Z-ADD     0             XXFTA
     C                   Z-ADD     0             XXFTD
     C                   Z-ADD     0             XXFTP
     C*
     C                   MOVE      CAFND         XXFND
     C                   MOVE      CAFYR         XXFYR
     C                   MOVE      CACASE        XXCASE
     C*
     C*
     C                   ENDSR
     C****
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 1992'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD1          1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                       XXSBNM              60
     O          E            HEAD1          1
     O                                           11 'GROUP:'
     O                       XXFND               18
     O                       XXFNNM              60
     O          E            HEAD1          2
     O                                           06 'CASE:'
     O                       XXCAS#              18
     O                                           32 'CLAIMANT:'
      ***** Beg Chg *** 06/09/20 *******************************************
     O***                    CMSS#               43
     O                       XXSS#               43
      ***** End Chg *** 06/09/20 *******************************************
     O                       XXCMNM             101
     O                                          110 'ACDT:'
     O                       XXACDT        Y    119
     O          E            HEAD1          1
     O                                           06 'CLAIM'
     O                                           16 'PROVIDER'
     O                                           36 'DATE'
     O                                           47 'FROM'
     O                                           57 'TO'
     O                                           79 'SERV'
     O                                           86 'CPT-4'
     O                                          102 'ICD-9'
     O                                          108 'EOMB'
     O                                          114 'PART'
     O                                          122 'ANESTH'
     O                                          132 'UNITS'
     O                                          148 'TOTAL'
     O                                          164 'TOTAL'
     O                                          180 'TOTAL'
     O                                          196 'TOTAL'
     O          E            HEAD1          2
     O                                           06 'STAT'
     O                                           14 'FEIN'
     O                                           28 'TYPE'
     O                                           38 'RECEIVED'
     O                                           47 'DATE'
     O                                           58 'DATE'
     O                                           65 'POS'
     O                                           72 'EOB #'
     O                                           79 'CODE'
     O                                           85 'CODE'
     O                                          102 'CODE'
     O                                          108 'CODE'
     O                                          113 'CD'
     O                                          122 'UNITS'
     O                                          132 'CHARGE'
     O                                          148 'CHARGES'
     O                                          164 'ALLOWED'
     O                                          180 'DIS-ALLOWED'
     O                                          196 'PAID'
     O          E            DET            1
     O                       CACSCD              05
     O               20                          06 '*'
     O                       CAFEIN              17
     O               67                          18 '-'
     O                       CAMOD         Z     21
     O                       CAPRTY              27
     O                       XXRDAT              38 '  /  /  '
     O                       XXFDAT              49 '  /  /  '
     O                       XXTDAT              60 '  /  /  '
     O                       CAIPOP              64
     O              N68      CAEOB#        Z     72
     O               68                          72 '-----'
     O                       CASVCD        Z     79
     O                       CACPT4              86
     O               66                          87 '-'
     O                       CAPMOD              89
     O                       CAICD9             102
     O                       CAEOMB             108
     O                       CAPART             113
     O                       CAANMN        K    123
     O                       CAUNTS        K    133
     O                       CATOTC        K    149
     O                       CATOTA        K    165
     O                       CATOTD        K    181
     O                       CATOTP        K    197
     O          E            CASTOT      1  2
     O                                           73 'CASE'
     O                       XXCAS#              84
     O                                           92 'TOTALS:'
     O                                          106 '# OF CHARGES:'
     O                       XXCCNT        Z    111
     O                       XXCAU         K    123
     O                       XXCUC         K    133
     O                       XXCTC         K    149
     O                       XXCTA         K    165
     O                       XXCTD         K    181
     O                       XXCTP         K    197
     O          E            FYRTOT         2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                       XXSBNM              60
     O          E            FYRTOT      2  2
     O                                           59 'TOTALS FOR GROUP'
     O                       XXFND               63
     O                                           70 '- YEAR'
     O                       XXFYR         Z     73
     O                                           74 ':'
     O          E            FYRTOT         2
     O                                           94 'CHARGES:'
     O                       XXYTC         K    113
     O          E            FYRTOT         2
     O                                           94 'ALLOWED:'
     O                       XXYTA         K    113
     O          E            FYRTOT         2
     O                                           94 'DIS-ALLOWED:'
     O                       XXYTD         K    113
     O          E            FYRTOT         2
     O                                           94 'PAID:'
     O                       XXYTP         K    113
     O          E            FNDTOT         2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                       XXSBNM              60
     O          E            FNDTOT      2  2
     O                                           69 'TOTALS FOR GROUP'
     O                       XXFND               73
     O                                           74 ':'
     O          E            FNDTOT         2
     O                                           94 'CHARGES:'
     O                       XXFTC         K    113
     O          E            FNDTOT         2
     O                                           94 'ALLOWED:'
     O                       XXFTA         K    113
     O          E            FNDTOT         2
     O                                           94 'DIS-ALLOWED:'
     O                       XXFTD         K    113
     O          E            FNDTOT         2
     O                                           94 'PAID:'
     O                       XXFTP         K    113
     O          E            FINTOT         2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                       XXSBNM              60
     O          E            FINTOT      2  2
     O                                           74 'TOTALS FOR ALL GROUPS:'
     O          E            FINTOT         2
     O                                           94 'CHARGES:'
     O                       XXTTC         K    113
     O          E            FINTOT         2
     O                                           94 'ALLOWED:'
     O                       XXTTA         K    113
     O          E            FINTOT         2
     O                                           94 'DIS-ALLOWED:'
     O                       XXTTD         K    113
     O          E            FINTOT         2
     O                                           94 'PAID:'
     O                       XXTTP         K    113
     O          E            NOREC       2  1
     O                                           56 '0 RECORDS PROCESSED'
