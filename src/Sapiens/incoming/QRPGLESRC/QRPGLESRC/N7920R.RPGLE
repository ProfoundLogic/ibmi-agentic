     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H* PROGRAM: N7920R - PROVIDER UTILIZATION REPORT FOR NON-PAR    **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  VARIABLES:                                                  **
     H*  PDAMTC  PROVIDER DATE RANGE AMOUNT CHARGE SUBTOTAL          **
     H*  PDNDIS  PROVIDER DATE RANGE NETWORK DISC  SUBTOTAL          **
     H*  PYAMTC  PROVIDER YEAR-TO-DATE AMOUNT CHARGE SUBTOTAL        **
     H*  PYNDIS  PROVIDER YEAR-TO-DATE NETWORK DISC  SUBTOTAL        **
     H*  RDAMTC  REPORT DATE RANGE AMOUNT CHARGE TOTAL               **
     H*  RDNDIS  REPORT DATE RANGE NETWORK DISC  TOTAL               **
     H*  RYAMTC  REPORT YEAR-TO-DATE AMOUNT CHARGE TOTAL             **
     H*  RYNDIS  REPORT YEAR-TO-DATE NETWORK DISC  TOTAL             **
     H*****************************************************************
     H*  INDICATORS:                                                 **
     H*  08      BATCH MODE                                          **
     H*  09      DEMAND MODE                                         **
     H*  10      DETAIL REPORT                                       **
     H*  11      SUMMARY REPORT                                      **
     H*  60      GENERAL CHAIN                                       **
     H*  65      GENERAL CHAIN                                       **
     H*  90      PRIMARY CHAIN/DOWEQ (PEO01)                         **
     H*  95      GENERAL CHAIN (CHAIN TO PCM01)                      **
     H*  96      GENERAL CHAIN (CHAIN TO PCD01)                      **
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FTAEOL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D PRTDS1          DS
     D  FIL11                  1    200
     D  FIL12                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D PRTDS2          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XF#                    1     30
     D  XFSUB                  1      5  0
     D  XFHYP1                 6      6
     D  XFFND                  7      9  0
     D  XFHYP2                10     10
     D  XFYR                  11     12  0
     D  XFHYP3                13     13
     D  XFCASE                14     20  0
     D  XFHYP4                21     21
     D  XFEOB#                22     30  0
     D                 DS
     D  XXSS#                  1      9
     D  XXPRE                  1      3
     D  XXMID                  4      5
     D  XXSUF                  6      9

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
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
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE HBA FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE NETWORK TRANSACTION FILE
     C*
     C     TRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRTYP            1
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
     C* MOVE ENTRY PARAMETERS INTO REPORT SELECTION FIELDS
     C     QQFEIN        IFEQ      *ZEROS
     C                   MOVEL     'ALL PAR'     RSFEIN            9
     C                   ELSE
     C                   MOVE      QQFEIN        RSFEIN
     C                   END
     C     QQSUB#        IFEQ      *ZEROS
     C                   MOVEL     'ALL'         RSSUB#            5
     C                   ELSE
     C                   MOVE      QQSUB#        RSSUB#
     C                   END
     C     QQFND         IFEQ      *ZEROS
     C                   MOVE      'ALL'         RSFND             3
     C                   ELSE
     C                   MOVE      QQFND         RSFND
     C                   END
     C                   MOVE      QQMOD         RSMOD             3 0
     C                   MOVE      QQFRDT        RSFRDT            6 0
     C                   MOVE      QQTODT        RSTODT            6 0
     C                   MOVE      QQRTYP        RSRTYP            1
     C*
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    UPDT          XXFROM
     C     *LIKE         DEFINE    UPDT          XXTO
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C                   MOVE      QQTODT        XXTODT            8 0
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQFEIN        XXFEIN
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     0             PDAMTC           10 2
     C                   Z-ADD     0             PDNDIS           10 2
     C                   Z-ADD     0             PYAMTC           10 2
     C                   Z-ADD     0             PYNDIS           10 2
     C                   Z-ADD     0             RDAMTC           10 2
     C                   Z-ADD     0             RDNDIS           10 2
     C                   Z-ADD     0             RYAMTC           10 2
     C                   Z-ADD     0             RYNDIS           10 2
     C*
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C                   MOVE      '-'           XFHYP4
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     QQRTYP        IFEQ      'B'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN11
     C                   ELSE
     C     QQRTYP        IFEQ      'D'
     C                   MOVE      '1'           *IN10
     C                   ELSE
     C     QQRTYP        IFEQ      'S'
     C                   MOVE      '1'           *IN11
     C                   END
     C                   END
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
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
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        YYTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        XXTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C***************************************************************
     C*   READ EOB FILE
     C*
     C                   READ      PEO01                                  90
     C     *IN90         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   11              EXCEPT    HEAD1
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* GET THE PROVIDER'S NAME
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        QQLAST           56
     C                   MOVEL     PDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXPNAM           40
     C                   ELSE                                                   ELSEIF *IN96
     C                   MOVE      *BLANKS       XXPNAM
     C                   END                                                    END 0 *IN60
     C*
     C   10              EXCEPT    HEAD
     C   11              EXCEPT    HEAD1
     C   10              EXCEPT    PRHEAD                                       PROVIDER HEADING
     C*
     C     *IN90         DOWEQ     '0'
     C*
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C                   MOVE      EOEOB#        XXEOB#
     C                   MOVE      EOFEIN        XXFEIN
     C                   MOVE      EOMOD         XXMOD
     C*
     C                   Z-ADD     EOSUB#        XFSUB
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C     EOTRDT        IFGE      XXFROM
     C     EOTRDT        ANDLE     XXTO
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C*  ADD TO DATE RANGE TOTALS
     C*
     C                   ADD       EOAMTC        PDAMTC
     C                   ADD       EONDIS        PDNDIS
     C*
     C*   GET SOCIAL SECURITY NUMBER, CLAIMANT NAME, AND ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              95
     C     *IN95         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              96
     C     *IN96         IFEQ      '0'
     C                   MOVEL     CDLSN         XXLNAM           20
     C                   MOVEL     CDFSN         XXFNAM           20
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME           40
     C                   ELSE                                                   ELSEIF *IN96
     C                   MOVE      *BLANKS       XXNAME
     C                   END                                                    END IF *IN96
     C*
     C                   ELSE                                                   ELSEIF *IN95
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C                   END                                                    END IF *IN95
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRSV            6 0
     C*
     C   10              EXCEPT    DET
     C*
     C                   END                                                    IN DATE RANGE
     C*
     C*  ADD TO YEAR TO DATE TOTALS
     C*
     C                   ADD       EOAMTC        PYAMTC
     C                   ADD       EONDIS        PYNDIS
     C*
     C*  READ NEXT RECORD
     C                   READ      PEO01                                  90
     C                   END                                                     END DOWEQ *IN90
     C*
     C* END OF PROGRAM
     C                   MOVE      '1'           *INLR
     C                   EXSR      SRPRO
     C                   EXCEPT    FINTOT
     C                   EXCEPT    FINYTD
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*   SUBROUTINE SRPRO                                            *
     C*   PROCESS A CHANGE IN THE PROVIDER/MODIFIER                   *
     C*****************************************************************
     C     SRPRO         BEGSR
     C                   EXCEPT    PROTOT
     C                   EXCEPT    PROYTD
     C*
     C* ADD INTO REPORT TOTALS
     C                   ADD       PDAMTC        RDAMTC
     C                   ADD       PDNDIS        RDNDIS
     C                   ADD       PYAMTC        RYAMTC
     C                   ADD       PYNDIS        RYNDIS
     C*
     C* ZERO OUT PROVIDER TOTALS
     C                   Z-ADD     0             PDAMTC
     C                   Z-ADD     0             PDNDIS
     C                   Z-ADD     0             PYAMTC
     C                   Z-ADD     0             PYNDIS
     C*
     C   10              EXCEPT    HEAD
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     O*****************************
     O* QSYSPRT  = DETAIL REPORT  *
     O*****************************
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
     O**********        09   Q1USER              67
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
     O                  09   Q1USER              67
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
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'PROVIDER ID:'
     O                       RSFEIN              39
     O          E            ROUTE          2
     O                                           26 'MODIFIER   :'
     O                       RSMOD         Z     33
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER :'
     O                       RSSUB#              35
     O          E            ROUTE          2
     O                                           26 'GROUP      :'
     O                       RSFND               33
     O          E            ROUTE          2
     O                                           26 'FROM DATE  :'
     O                       RSFRDT              38 '  /  /  '
     O          E            ROUTE          2
     O                                           26 'TO DATE    :'
     O                       RSTODT              38 '  /  /  '
     O          E            ROUTE          2
     O                                           26 'REPORT TYPE:'
     O                       RSRTYP              31
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               14 '(C) COPYRIGHT '
     O****                               32 'PROGRESSIVE DATA '
     O****                               52 'SOLUTIONS INC. 1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD           1 10
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              60
     O                       DSDATE        Y     80
     O**
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL              70
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
     O**
     O          E            HEAD           1
     O                                           47 'PROVIDER UTILIZATION'
     O                                           54 'REPORT'
     O*
     O          E            HEAD           2
     O                                           39 'CLAIMS PROCESSED FROM:'
     O                       XXFRDT        Y     48
     O                                           52 'TO:'
     O                       XXTODT        Y     61
     O          E   NLR      PRHEAD         1
     O                                           13 '** PROVIDER #'
     O                       EOFEIN              23
     O                                           24 '-'
     O                       EOMOD               27
     O                       XXPNAM              69
     O          E            PRHEAD      1  1
     O                 NLR                        5 'DATE'
     O                                           80 'CONTRACT'
     O          E            PRHEAD         1
     O                 NLR                        4 'OF'
     O                                           65 'TOTAL'
     O                                           80 'DISCOUNT'
     O          E            PRHEAD         2
     O                 NLR                        7 'SERVICE'
     O                 NLR                       22 'PATIENT NAME'
     O                                           65 'CHARGES'
     O                                           80 'APPLIED'
     O          E            DET            1
     O                       XXFRSV               8 '  /  /  '
     O                       XXNAME              50
     O                       EOAMTC        1     65
     O                       EONDIS        1     80
     O          E    10      PROTOT      1  1
     O                                           11 'Subtotal'
     O                                           27 '(Date Range) **'
     O                       PDAMTC        1     65
     O                       PDNDIS        1     80
     O          E    10      PROTOT         2
     O                                           11 'Subtotal'
     O                                           29 '(Year to Date) **'
     O                       PYAMTC        1     65
     O                       PYNDIS        1     80
     O          E    10      FINTOT         1
     O                                           10 '*** REPORT'
     O          E    10      FINTOT         1
     O                                            9 'Total'
     O                                           26 '(Date Range) ***'
     O                       RDAMTC        1     65
     O                       RDNDIS        1     80
     O          E    10      FINYTD         2
     O                                            9 'Total'
     O                                           28 '(Year to Date) ***'
     O                       RYAMTC        1     65
     O                       RYNDIS        1     80
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O*****************************
     O* QSYSPRT1 = SUMMARY REPORT *
     O*****************************
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   Q1USER              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   Q1USER              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       YYTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
     O**********E            ROUTE1         2 33
     O**********                                 18 'REPORT SELECTIONS:'
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE1         2
     O                                           26 'PROVIDER ID:'
     O                       RSFEIN              39
     O          E            ROUTE1         2
     O                                           26 'MODIFIER   :'
     O                       RSMOD         Z     33
     O          E            ROUTE1         2
     O                                           26 'SUBSCRIBER :'
     O                       RSSUB#              35
     O          E            ROUTE1         2
     O                                           26 'GROUP      :'
     O                       RSFND               33
     O          E            ROUTE1         2
     O                                           26 'FROM DATE  :'
     O                       RSFRDT              38 '  /  /  '
     O          E            ROUTE1         2
     O                                           26 'TO DATE    :'
     O                       RSTODT              38 '  /  /  '
     O          E            ROUTE1         2
     O                                           26 'REPORT TYPE:'
     O                       RSRTYP              31
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                14 '(C) COPYRIGHT '
     O***                                32 'PROGRESSIVE DATA '
     O***                                52 'SOLUTIONS INC. 1997'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ***** END CHG ****** 02/02/06 ***********************************
     O*****************************************************************
     O          E            HEAD1          1 10
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              60
     O                       DSDATE        Y     80
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       YYTITL              70
     O                                           76 'PAGE:'
     O                       PAGE1         Z     80
     O          E            HEAD1          1
     O                                           47 'PROVIDER UTILIZATION'
     O                                           54 'REPORT'
     O          E            HEAD1          2
     O                                           39 'CLAIMS PROCESSED FROM:'
     O                       XXFRDT        Y     48
     O                                           52 'TO:'
     O                       XXTODT        Y     61
     O          E            HEAD1       1  1
     O                                           80 'CONTRACT'
     O          E            HEAD1          1
     O                                           65 'TOTAL'
     O                                           80 'DISCOUNT'
     O          E            HEAD1          3
     O                                           65 'CHARGES'
     O                                           80 'APPLIED'
     O          E    11      PROTOT         1
     O                                           13 '** PROVIDER #'
     O                       EOFEIN              23
     O                                           24 '-'
     O                       EOMOD               27
     O                       XXPNAM              70
     O          E    11      PROTOT         1
     O                                           11 'Subtotal'
     O                                           27 '(Date Range) **'
     O                       PDAMTC        1     65
     O                       PDNDIS        1     80
     O          E    11      PROYTD         2
     O                                           11 'Subtotal'
     O                                           29 '(Year to Date) **'
     O                       PYAMTC        1     65
     O                       PYNDIS        1     80
     O          E    11      FINTOT         1
     O                                           10 '*** REPORT'
     O          E    11      FINTOT         1
     O                                            9 'Total'
     O                                           26 '(Date Range) ***'
     O                       RDAMTC        1     65
     O                       RDNDIS        1     80
     O          E    11      FINYTD         2
     O                                            9 'Total'
     O                                           28 '(Year to Date) ***'
     O                       RYAMTC        1     65
     O                       RYNDIS        1     80
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
