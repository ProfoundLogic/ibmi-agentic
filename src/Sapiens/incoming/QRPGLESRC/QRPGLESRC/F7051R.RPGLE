     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F7051R - TABLE FILE REPORT 2
     H*****************************************************************
     H* CREATED 09/11/00 BY C:62 FOR PROJECT:MC205
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H* 08/08/02 C:62 PROJECT FS244                                   *
     H*               ADD GEO ZIP EFFECTIVE DATE.                     *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSTABL     IF   E           K DISK
1    FFTGIL     IF   E           K DISK
2    FFMSFL     IF   E           K DISK
3    FFTDLL     IF   E           K DISK
4    FFTOTL     IF   E           K DISK
5    FFTBSL     IF   E           K DISK
     F*** BEGIN CHG *** 08/08/02 **************************************
6    F**********FMGZL1  IF  E           K        DISK
6    FFMGZL3    IF   E           K DISK
     F*** END   CHG *** 08/08/02 **************************************
7    FFMRXL     IF   E           K DISK
8    FFTABL     IF   E           K DISK
9    FFMZRL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  FL                     1      9
     D                                     DIM(9)
     D  SCGICD                 1      1
     D  SCSSCD                 2      2
     D  SCLDCD                 3      3
     D  SCOSCD                 4      4
     D  SCBSCD                 5      5
     D  SCZACD                 6      6
     D  SCNDCD                 7      7
     D  SCZRCD                 8      8
     D  SCTSCD                 9      9
     D                 DS
     D  TSKEY                  1     10
     D  ST                     1      2
     D  KEY                    3      9
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKFIL#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    ORFIL#        KKFIL#
     C     *LIKE         DEFINE    GICO#         KKCO#
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    STNAME        XXSTNM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FLG             9
     C                   PARM                    @@ST              2
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVEA     @@FLG         FL
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZEROS        X                 2 0
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     @@MODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   WAS STATE PASSED IN
     C*
     C     @@ST          IFNE      *BLANKS
     C                   SETON                                        16        DEMAND MODE
     C*
     C                   MOVEL(P)  @@ST          KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C  N99              MOVEL(P)  STNAME        XXSTNM
     C   99              MOVEL     *BLANKS       XXSTNM
     C*
     C                   ELSE
     C                   SETOFF                                       16        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE.
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
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           KKFIL#
     C     ORKY          CHAIN     POR01                              60
     C*
     C*   CHECK TO SEE IF NETWORK/PPO INSTALLED SFBPF/FBFL06 = 'Y'
     C*   IF NOT INSTALLED DO NOT DISPLAY A/R TRANACTION CODE LISTING
     C*   OR ACTIVITY CODE LISTING OPTION ON SCREEN.
     C*
     C*
     C* SETON REPORT CONDITIONS FOR ROUTING SHEET
     C*
     C     FL(1)         IFNE      *BLANKS
     C                   SETON                                        21
     C                   ENDIF
     C*
     C     FL(2)         IFNE      *BLANKS
     C                   SETON                                        22
     C                   ENDIF
     C*
     C     FL(3)         IFNE      *BLANKS
     C                   SETON                                        23
     C                   ENDIF
     C*
     C     FL(4)         IFNE      *BLANKS
     C                   SETON                                        24
     C                   ENDIF
     C*
     C     FL(5)         IFNE      *BLANKS
     C                   SETON                                        25
     C                   ENDIF
     C*
     C     FL(6)         IFNE      *BLANKS
     C                   SETON                                        26
     C                   ENDIF
     C*
     C     FL(7)         IFNE      *BLANKS
     C                   SETON                                        27
     C                   ENDIF
     C*
     C     FL(8)         IFNE      *BLANKS
     C                   SETON                                        28
     C                   ENDIF
     C*
     C     FL(9)         IFNE      *BLANKS
     C                   SETON                                        29
     C                   ENDIF
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              60
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C*   PRINT THE REPORT FOR GENERIC TABLE ID'S  - Position 1
     C     FL(1)         IFNE      *BLANK
     C                   EXSR      PRTGI
     C                   END
     C*
     C*   PRINT THE REPORT FOR SUBSCRIBER STAFF CODES  - Position 2
     C     FL(2)         IFNE      *BLANK
     C                   EXSR      PRTSS
     C                   END
     C*
     C*   PRINT THE REPORT FOR LEVEL OF DUTY CODE - Position 3
     C     FL(3)         IFNE      *BLANK
     C                   EXSR      PRTDL
     C                   END
     C*
     C*   PRINT THE REPORT FOR OUTCOME/SAVINGS CODES - Position 4
     C     FL(4)         IFNE      *BLANK
     C                   EXSR      PRTOT
     C                   END
     C*
     C*   PRINT THE REPORT FOR BILL STATUS TYPE CODE - Position 5
     C     FL(5)         IFNE      *BLANK
     C                   EXSR      PRTBS
     C                   END
     C*
     C*   PRINT THE REPORT FOR ZIP AREAS - Position 6
     C     FL(6)         IFNE      *BLANK
     C                   EXSR      PRTGZ
     C                   END
     C*
     C*   Print the report for NDC DRUG CODES - Position 7
     C     FL(7)         IFNE      *BLANK
     C                   EXSR      PRTRX
     C                   END
     C*
     C*   Print the report for ZIP/CODE/REGIONS - Position 8
     C     FL(8)         IFNE      *BLANK
     C                   EXSR      PRTZR
     C                   END
     C*
     C*   Print the report for TYPE OF SERVICE CODES - Position 9
     C     FL(9)         IFNE      *BLANK
     C                   EXSR      PRTTS
     C                   END
     C*
     C*   END OF JOB.
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkFil# = '0';
       Chain Orky Por01;
       If %found and Orstat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*   PRINT THE BILL STATUS TYPE CODES                            *
     C*****************************************************************
     C     PRTBS         BEGSR
     C     KKCO#         CHAIN     PBS01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C                   END
     C                   EXCEPT    DETBS
     C     KKCO#         READE     PBS01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE GENERIC TABLE ID'S                                *
     C*****************************************************************
     C     PRTGI         BEGSR
     C     KKCO#         CHAIN     PGI01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END
     C                   EXCEPT    DETGI
     C     KKCO#         READE     PGI01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   Print the LEVEL OF DUTY CODES                               *
     C*****************************************************************
     C     PRTDL         BEGSR
     C     KKCO#         CHAIN     PDL01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   END
     C                   ADD       1             XXRCD
     C                   EXCEPT    DETDL
     C     KKCO#         READE     PDL01                                  58
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE NDS DRUG CODES                                    *
     C*****************************************************************
     C     PRTRX         BEGSR
     C     KKCO#         CHAIN     PRX01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD7
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CLOSURE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD7
     C                   END
     C                   EXCEPT    DETRX
     C     KKCO#         READE     PRX01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE OUTCOME/SAVINGS CODES                            *
     C*****************************************************************
     C     PRTOT         BEGSR
     C     KKCO#         CHAIN     POT01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CLAIMS STATUS CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C                   END
     C                   EXCEPT    DETOT
     C     KKCO#         READE     POT01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE SUBSCRIBER STAFF CODES                            *
     C*****************************************************************
     C     PRTSS         BEGSR
     C     KKCO#         CHAIN     PSF01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE COVERAGE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   END
     C                   EXCEPT    DETSS
     C     KKCO#         READE     PSF01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE TYPE OF SERVICE CODES                             *
     C*****************************************************************
     C     PRTTS         BEGSR
     C     KKCO#         CHAIN     PTS01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD9
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*                    Z-ADD*ZERO     PAGE
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C     ST            IFEQ      @@ST
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD9
     C                   END
     C                   EXCEPT    DETTS
     C                   ENDIF
     C     KKCO#         READE     PTS01                                  58
     C     ST            IFEQ      @@ST
     C                   ADD       1             XXRCD
     C                   ENDIF
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE GEOGRAPHIC ZIP AREA                               *
     C*****************************************************************
     C     PRTGZ         BEGSR
     C     KKCO#         CHAIN     PGZ01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*                    Z-ADD*ZERO     PAGE
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C                   END
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   Z-ADD     GZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C                   EXCEPT    DETGZ
     C     KKCO#         READE     PGZ01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE ZIP CODE/REGIONS                                  *
     C*****************************************************************
     C     PRTZR         BEGSR
     C     KKCO#         CHAIN     PZR01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD8
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*                    Z-ADD*ZERO     PAGE
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD8
     C                   END
     C*
     C                   Z-ADD     ZREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C*
     C                   EXCEPT    DETZR
     C     KKCO#         READE     PZR01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
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
     O**********        09   @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E    16      ROUTE          2
     O                                           23 'State:'
     O                       @@ST                27
     O                       XXSTNM              69
     O          E    21      ROUTE          1
     O                                           41 'Generic Table IDs       '
     O          E    22      ROUTE          1
     O                                           41 'Subscriber Staff codes  '
     O          E    23      ROUTE          1
     O                                           41 'Level of Duty codes     '
     O          E    24      ROUTE          1
     O                                           41 'Outcome/Savings codes   '
     O          E    25      ROUTE          1
     O                                           41 'Bill Status Type codes  '
     O          E    26      ROUTE          1
     O                                           41 'Zip Areas               '
     O          E    27      ROUTE          1
     O                                           41 'NDC Drug codes          '
     O          E    28      ROUTE          1
     O                                           41 'Zip Code/Regions        '
     O          E    29      ROUTE          1
     O                                           41 'Type of Service codes   '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                58 'INC. 2000       '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    122
     O          E            HEAD           3
     O                       Q1PGM               10
     O                                           64 'TABLE FILE LIST 2'
     O                                          116 'PAGE'
     O                       PAGE          Z    122
     O*****************************************************************
     O          E            HEAD1          0
     O                                           64 'GENERIC TABLE ID'
     O          E            HEAD1          1
     O                                           64 'GENERIC TABLE ID'
     O          E            HEAD1          2
     O                                           11 'CODE'
     O                                           24 'DESCRIPTION'
     O                                           57 'STS'
     O          E            DETGI          1
     O                       GITBID              11
     O                       GIDESC              53
     O                       GISTAT              56
     O*****************************************************************
     O          E            HEAD2          0
     O                                           66 'SUBSCRIBER STAFF CODE'
     O          E            HEAD2          1
     O                                           66 'SUBSCRIBER STAFF CODE'
     O          E            HEAD2          2
     O                                           13 'SUB#'
     O                                           19 'CODE'
     O                                           31 'DESCRIPTION'
     O                                           63 'STS'
     O          E            DETSS          1
     O                       SFSUB#        4     13
     O                       SFSTAF              18
     O                       SFNAME              60
     O                       SFSTAT              62
     O*****************************************************************
     O          E            HEAD3          0
     O                                           65 'LEVEL OF DUTY CODE'
     O          E            HEAD3          2
     O                                           65 'LEVEL OF DUTY CODE'
     O          E            HEAD3          2
     O                                           13 'CODE'
     O                                           32 'DESCRIPTION'
     O                                           65 'STS'
     O          E            DETDL          1
     O                       DLKEY               19
     O                       DLDESC              61
     O                       DLSTAT              64
     O*****************************************************************
     O          E            HEAD4          0
     O                                           66 'OUTCOME/SAVINGS CODE'
     O          E            HEAD4          1
     O                                           66 'OUTCOME/SAVINGS CODE'
     O          E            HEAD4          2
     O                                           16 'CODE'
     O                                           35 'DESCRIPTION'
     O                                           98 'STS'
     O          E            DETOT          1
     O                       OTCODE              22
     O                       OTDESC              94
     O                       OTSTAT              97
     O*****************************************************************
     O          E            HEAD5          0
     O                                           66 'BILL STATUS TYPE CODE'
     O          E            HEAD5          2
     O                                           66 'BILL STATUS TYPE CODE'
     O          E            HEAD5          2
     O                                           16 'TYPE'
     O                                           22 'CODE'
     O                                           35 'DESCRIPTION'
     O                                           68 'STS'
     O          E            DETBS          1
     O                       BSTYPE              15
     O                       BSCODE              21
     O                       BSDESC              64
     O                       BSSTAT              67
     O*****************************************************************
     O          E            HEAD6          0
     O                                           62 'GEO ZIP AREAS'
     O          E            HEAD6          2
     O                                           62 'GEO ZIP AREAS'
     O          E            HEAD6          2
     O                                           16 'CODE'
     O*** BEGIN CHG *** 08/08/02 **************************************
     O**********                         28 'DESCRIPTION'
     O**********                         60 'STS'
     O                                           28 'EFF. DATE'
     O                                           41 'DESCRIPTION'
     O                                           74 'STS'
     O*** END   CHG *** 08/08/02 **************************************
     O          E            DETGZ          1
     O                       GZZIPA              15
     O*** BEGIN CHG *** 08/08/02 **************************************
     O**********               GZDESC    57
     O**********               GZSTAT    59
     O                       XXEFDT            +002 '0  /  /    '
     O                       GZDESC            +002
     O                       GZSTAT            +002
     O*** END   CHG *** 08/08/02 **************************************
     O*****************************************************************
     O          E            HEAD7          0
     O                                           62 'NDC DRUG CODE '
     O          E            HEAD7          2
     O                                           62 'NDC DRUG CODE '
     O          E            HEAD7          2
     O                                           16 'CODE'
     O                                           36 'DESCRIPTION'
     O                                          104 'STS'
     O          E            DETRX          1
     O                       RXNDC               23
     O                       RXNAME             100
     O                       RXSTAT             103
     O*****************************************************************
     O          E            HEAD8          0
     O                                           63 'ZIP CODE/REGIONS'
     O          E            HEAD8          2
     O                                           63 'ZIP CODE/REGIONS'
     O          E            HEAD8          2
     O                                           14 'STATE:'
     O                       @@ST              +001
     O                       XXSTNM            +001
     O          E            HEAD8          1
     O                                           14 'FROM'
     O                                           23 'TO'
     O          E            HEAD8          2
     O                                           16 'ZIP CODE'
     O                                           26 'ZIP CODE'
     O                                           38 ' EFF. DATE'
     O                                           46 'REGION'
     O                                           50 'STS'
     O          E            DETZR          1
     O                       ZRFZCD        4     15
     O                       ZRTZCD        4     25
     O                       XXEFDT              38 '0  /  /    '
     O                       ZRRGN#        4     46
     O                       ZRSTAT              49
     O*****************************************************************
     O          E            HEAD9          0
     O                                           65 'TYPE OF SERVICE CODES'
     O          E            HEAD9          2
     O                                           65 'TYPE OF SERVICE CODES'
     O          E            HEAD9          2
     O                                           14 'STATE:'
     O                       @@ST              +001
     O                       XXSTNM            +001
     O          E            HEAD9          2
     O                                           13 'CODE'
     O                                           30 'DESCRIPTION'
     O                                           63 'STS'
     O          E            DETTS          1
     O                       KEY                 17
     O                       TSDESC              59
     O                       TSSTAT              62
     O*****************************************************************
     O          E            TOTL        2
     O                       XXRCD         Z     54
     O                                           69 'T O T A L    '
     O                                           82 'R E C O R D S'
     O          E            TOTL
     O                       XXRCD         Z     54
     O                                           69 'T O T A L    '
     O                                           82 'R E C O R D S'
