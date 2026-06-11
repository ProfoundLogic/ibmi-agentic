     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7070R - BCL-7 MONTHLY REPORT
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     F*
     F* * * Table Files.  NCCI Table file
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*
     F* * * Batch Routing File
     FSMORL     IF   E           K DISK
     F*
     F* * * Company File
     FSMCOL     IF   E           K DISK
     F*
     F* * * Claim File
     FWMCML5    IF   E           K DISK
     F*
     F* * * Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F* * * Claims Check Payment File
     FWDPAL     IF   E           K DISK
     F*
     F* * * Employer Detail File
     FWDELL     IF   E           K DISK
     F*
     F* * * Fund File
     FWMFNL     IF   E           K DISK
     F*
     F* * * Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*
     F* * * Output File
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F*

     D*
     D NC              S              2    DIM(999)
     D SZ              S              1    DIM(13)
     D ST              S              2    DIM(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXDATE                 1      6  0
     D  XXYM                   1      4  0
     D                 DS
     D  CMENTD                 1      7  0
     D  CMYM                   2      5  0
     D                 DS
     D  PATRDT                 1      7  0
     D  PAYM                   2      5  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    ELFYR
     C                   KFLD                    EMEMP#
     C*
     C*   KEY LIST FOR THE NCCI CODE TABLE FILE
     C*
     C     NCKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  CLAIMS CHECK PAYMENT FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C*
     C/EJECT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            6
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDATE            6 0
     C                   MOVEL     QQDATE        XXMO              2 0
     C                   MOVE      QQDATE        XXYR              2 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*
     C*  GET THE COMPANY RECORD.
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* * * GET TIME DATE INFORMATION
     C                   TIME                    DSDTTM
     C*
     C/EJECT
     C     EMKEY         CHAIN     PEM01                              40
     C*
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVEL     EMNAME        XXNAME           34
     C*
     C  N11              EXSR      SR1ST
     C*
     C     EMFND         IFNE      XHFND
     C                   EXSR      SREBRK
     C                   EXSR      SRFBRK
     C                   ELSE
     C     EMEMP#        IFNE      XHEMP#
     C     EMDIV         ORNE      XHDIV
     C                   EXSR      SREBRK
     C                   END
     C                   END
     C*
     C* * *  DETAIL ROUTINE
     C*
     C     ELKEY         CHAIN     PEL01                              41
     C     *IN41         DOWEQ     '0'
     C     CMKEY         CHAIN     PCM01                              42
     C     *IN42         DOWEQ     '0'
     C*
     C* * * CHECK TO SEE IF THIS CLAIM IS A MEDICAL ONE
     C                   Z-ADD     1             X
     C     CMNCCI        LOOKUP    NC(X)                                  60
     C   60              DO
     C* * * ADD ONE TO COUNT IF CLAIM ENTERED DATE IS THIS MONTH.
     C     XXYM          IFEQ      CMYM
     C                   ADD       1             XXEMCT            5 0
     C                   END
     C*
     C*
     C* * * GOTO PAYMENT FILE AND READ ALL PAYMENT RECORDS FOR THIS CASE.
     C* USE ONLY IF THIS MONTH.
     C     PAKEY         CHAIN     PPA01                              43
     C     *IN43         DOWEQ     '0'
     C*
     C* * * IF PATRDT IS THIS MONTH/YEAR THEN PROCEED
     C* * * IF PAADST IS BLANK, AND TYPE IS P,M, OR A, THEN ADD
     C* * * THE AMOUNT TO THIS MONTHS OR PRIOR BUCKET(CHECK DATE?)
     C*
     C     PAYM          IFEQ      XXYM
     C     PAADST        IFEQ      *BLANK
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C*
     C     XXYM          IFEQ      CMYM
     C                   ADD       PAAMT         XXEMAC           11 2
     C                   ELSE
     C                   ADD       PAAMT         XXEMAP           11 2
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     PAKEY         READE     PPA01                                  43
     C                   END                                                    END PMT LOOP 43
     C*
     C                   END                                                    END DO MED CLM
     C     CMKEY         READE     PCM01                                  42
     C                   END                                                    END CLM LOOP 42
     C*
     C     ELKEY         READE     PEL01                                  41
     C                   END
     C*
     C* * * PRINT INFO ONLY IF AT LEAST ONE OF THE 3 FIELDS IS > *BLANK.
     C     XXEMCT        IFNE      *ZERO
     C     XXEMAC        ORNE      *ZERO
     C     XXEMAP        ORNE      *ZERO
     C* * * PRINT DETAIL LINES
     C     XXEMAC        ADD       XXEMAP        XXEMAT
     C     *IN09         IFEQ      '0'
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXSR      EMPLR
     C                   EXCEPT    DETL
     C                   EXCEPT    DETL1
     C     XXAD2         IFEQ      *BLANKS
     C                   EXCEPT    DETL2
     C                   ELSE
     C                   EXCEPT    DETL3
     C                   END
     C                   EXCEPT    DETL4
     C                   END
     C*
     C                   END
     C*
     C     EMKEY         READE     PEM01                                  40
     C*
     C                   END
     C*
     C   11              EXSR      SRFPRT
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain orKey POR01;
          If %found and ORstat <> 'I';
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C/EJECT
      *==================================================================*
      *   Get employer name & address      ** Subroutine **   E M P L R  *
      *==================================================================*
     C*
     CSR   EMPLR         BEGSR
     C* * * SETUP EMPLOYER INFO
     C                   MOVE      *BLANKS       XXAD1            40
     C                   MOVE      *BLANKS       XXAD2            40
     C                   MOVE      *BLANKS       XXECSZ           40
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMAD1        XXAD1
     C                   MOVEL     EMMAD2        XXAD2
     C                   MOVE      EMMST         XXSTAT            2
     C                   MOVEL     EMMCTY        XXCITY           25
     C                   MOVEL     EMMZIP        XXZIP1            5
     C                   MOVE      EMMZIP        XXZIP2            4 0
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVEL     XXCSTZ        XXECSZ
     C*
     C                   ELSE
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C                   MOVEL     EMPCTY        XXCITY           25
     C                   MOVEL     EMPST         XXSTAT            2
     C                   MOVEL     EMPZIP        XXZIP1            5
     C                   MOVE      EMPZIP        XXZIP2            4 0
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVEL     XXCSTZ        XXECSZ
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C/EJECT
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C                   MOVE      EMFND         XHFND             3 0
     C                   MOVE      EMEMP#        XHEMP#            5 0
     C                   MOVE      EMDIV         XHDIV             3 0
     C                   MOVEL     XXMO          ##MY              4 0
     C                   MOVE      XXYR          ##MY              4 0
     C*
     C* * * GET FUND NAME FROM FUND MASTER FILE
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   MOVE      '0'           *IN09
     C                   ELSE
     C                   MOVEL     *BLANKS       XFNAME
     C                   MOVE      '1'           *IN09
     C                   END
     C*
     C* * * GET ALL MEDICAL TYPE NCCI CODES FROM TABLE
     C*
     C                   Z-ADD     1             X                 5 0
     C     NCKY1         CHAIN     PNC01                              60
     C     *IN60         DOWEQ     '0'
     C     X             ANDLE     999
     C     NCTYPE        IFEQ      'M'
     C                   MOVEL     NCKEY         NC(X)
     C                   ADD       1             X
     C                   END
     C     NCKY1         READE     PNC01                                  60
     C                   END
     C*
     C* * * OPEN PRINT FILES
     C                   OPEN      QSYSPRT
     C                   OPEN      QSYSPRT1
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDNE     'I'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C  N09              EXCEPT    HEAD
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  LOAD CITY, STATE, AND ZIP CODE                                K *
      *==================================================================*
     C*
     CSR   SR2ND         BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSTZ           40
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
     C* * * GET NEW FUND INFORMATION
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME
     C                   MOVE      '0'           *IN09
     C                   ELSE
     C                   MOVEL     *BLANKS       XFNAME
     C                   MOVE      '1'           *IN09
     C                   END
     C*
     C                   Z-ADD     0             XXFMAC
     C                   Z-ADD     0             XXFMAP
     C                   Z-ADD     0             XXFMAT
     C                   Z-ADD     0             XXFMCT
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      EMFND         XHFND
     C*
     C  N09              EXCEPT    HEAD
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Print Fund Totals                 ** Subroutine **  S R F P R T *
      *==================================================================*
     C*
     CSR   SRFPRT        BEGSR
     C*
     C     XXFMAC        ADD       XXFMAP        XXFMAT           11 2
     C  N09              EXCEPT    FTOTL
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Level Break on Employer           ** Subroutine **  S R E B R K *
      *==================================================================*
     C*
     CSR   SREBRK        BEGSR
     C*
     C* * * ROLL TO FUND TOTALS
     C                   ADD       XXEMAC        XXFMAC           11 2
     C                   ADD       XXEMAP        XXFMAP           11 2
     C                   ADD       XXEMCT        XXFMCT            5 0
     C*
     C                   MOVE      EMEMP#        XHEMP#
     C                   MOVE      EMDIV         XHDIV
     C                   Z-ADD     0             XXEMCT
     C                   Z-ADD     0             XXEMAC
     C                   Z-ADD     0             XXEMAP
     C                   Z-ADD     0             XXEMAT           11 2
     C                   ENDSR
     C*
     O*
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
      ****** BEG ADD ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
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
     O                                           05 'FUND:'
     O                       EMFND         Z     09
     O                       XFNAME              70
     O          E            HEAD           2
     O                                            5 'EMPLR'
     O                                            9 'DIV'
     O                                           23 'EMPLOYER NAME'
     O                                           63 'NEW INJURIES'
     O                                           86 'COSTS-NEW MED CASES'
     O                                          112 'COSTS-PRIOR MED CASES'
     O                                          130 'TOTAL COSTS'
     O          E            DETL           2
     O                       EMEMP#        Z      9
     O                       EMDIV         Z     15
     O                       XXNAME              50
     O                       XXEMCT        Z     58
     O                       XXEMAC        J     83
     O                       XXEMAP        J    107
     O                       XXEMAT        J    132
     O          E            FTOTL          2
     O                                           51 'FUND TOTAL:'
     O                       XXFMCT        Z     58
     O                       XXFMAC        J     83
     O                       XXFMAP        J    107
     O                       XXFMAT        J    132
     OQSYSPRT1  E            DETL1            10
     O                       EMFEIN              55
     O                       ##MY          Y     73
     O          E            DETL1            13
     O                       EMNAME              42
     O          E            DETL1            14
     O                       XXAD1               42
     O                       XXEMCT        Z     67
     O                       XXEMAC        J     84
     O          E            DETL2            15
     O                       XXECSZ              42
     O          E            DETL2            16
     O                       XXEMAP        J     84
     O          E            DETL3            15
     O                       XXAD2               42
     O          E            DETL3            16
     O                       XXECSZ              42
     O                       XXEMAP        J     84
     O          E            DETL4            18
     O                       XXEMAT        J     84
