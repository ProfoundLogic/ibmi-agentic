     H* (c) 2015 StoneRiver, Inc. All rights Reserved.                     *
     H* StoneRiver is a trademark of StoneRiver, Inc.                      *
      *
     H/TITLE W7501R - TABLE FILE REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/16/11 C:061 PROJECT 114WZW
     H*                Use expanded Minimum Premium field.
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/16/01 C:90 PROJECT: WC1041                                *
     H*                Modified the report ouput to fit the expanded  *
     H*                increased limit percent from 3.2. to 5.3       *
     H*                                                               *
     H*  10/21/99 C:87 PROJECT: WC577                                 *
     H*                Use WTCRP for the cancellation reason codes    *
     H*                and WTLMP for the limit codes.                 *
     H*                                                               *
     H*  04/12/91 KWK  PROJECT AHE                                    *
     H*                CHANGED TABLE FILE LISTING OF BILLING SCHEDULE *
     H*                CODES TO BE ABLE TO HANDLE PERCENTAGE CHANGE   *
     H*                FROM A32 TO A98 (UP TO SIX DECIMAL PLACES).    *
     H*                                                               *
     H*  07/29/91 KWK  PROJECT AHH                                    *
     H*                ADD SYSTEM A/R BATCH CODE TO PRINTING LIST.    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*** BEGIN ADD  ***  10/21/99  ***********************************
     FWTCRL     IF   E           K DISK
     FWTLML     IF   E           K DISK
     F*** END   ADD  ***  10/21/99  ***********************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D X1              S              2    DIM(50)
     D X2              S              1    DIM(50)
      ***** BEG ADD ******* 08/16/11 **********************************

     d usrdat          ds            20
     d  usrID                        10
     d  runDate                      10

      ***** END ADD ******* 08/16/11 **********************************
     D                 DS
     D  BR                     1    108  6
     D                                     DIM(12)
     D  BSBS01                 1      9  6
     D  BSBS02                10     18  6
     D  BSBS03                19     27  6
     D  BSBS04                28     36  6
     D  BSBS05                37     45  6
     D  BSBS06                46     54  6
     D  BSBS07                55     63  6
     D  BSBS08                64     72  6
     D  BSBS09                73     81  6
     D  BSBS10                82     90  6
     D  BSBS11                91     99  6
     D  BSBS12               100    108  6
     D                 DS
     D  BS                     1    108  8
     D                                     DIM(12)
     D  BSAB01                 1      9  8
     D  BSAB02                10     18  8
     D  BSAB03                19     27  8
     D  BSAB04                28     36  8
     D  BSAB05                37     45  8
     D  BSAB06                46     54  8
     D  BSAB07                55     63  8
     D  BSAB08                64     72  8
     D  BSAB09                73     81  8
     D  BSAB10                82     90  8
     D  BSAB11                91     99  8
     D  BSAB12               100    108  8
     D                 DS
     D  FL                     1     22
     D                                     DIM(22)
     D  SCSPCD                 1      1
     D  SCBLSC                 2      2
     D  SCNCCI                 3      3
     D  SCARTP                 4      4
     D  SCNATR                 5      5
     D  SCAGTP                 6      6
     D  SCPART                 7      7
     D  SCRMRK                 8      8
     D  SCCAUS                 9      9
     D  SCCHEK                10     10
     D  SCRESV                11     11
     D  SCCR                  12     12
     D  SCPAYM                13     13
     D  SCLIMT                14     14
     D  SCBR2                 15     15
     D  SCBR7                 16     16
     D  SCBC                  17     17
     D  SCCSCD                18     18
     D  SCCN                  19     19
     D  SCRS                  20     20
     D  SCLR                  21     21
     D  SCSYCD                22     22
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      ***** BEG ADD ******* 08/16/11 **********************************
      *
      *************************************************************
      * *LIKE DEFN
      *************************************************************
     C     *LIKE         DEFINE    LMMINE        XXMINE
      *
      *
      *************************************************************
      * PLIST
      *************************************************************
      ***** END ADD ******* 08/16/11 **********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFLG            22
     C                   PARM                    QQQUE            10
      *
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEA     QQFLG         FL
      ***** BEG ADD ******* 08/16/11 **********************************

      * Use expanded Minimum Premium if available

     c     *dtaara       define    minprm        usrdat
     c                   in        usrdat

     c                   if        usrid <> *blank
     c                   move      *on           xxMinPrm          1
     c                   else
     c                   move      *off          xxMinPrm
     c                   endif

      ***** END ADD ******* 08/16/11 **********************************
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE.
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
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C*   PRINT THE REPORT FOR SPECIALITY CODE                         D.
     C*
     C                   TIME                    DSDTTM
     C     FL(1)         IFNE      *BLANK
     C                   EXSR      PRTSP
     C                   END
     C*
     C*   PRINT THE REPORT FOR NCCI.                                   D.
     C*
     C     FL(3)         IFNE      *BLANK
     C                   EXSR      PRTNC
     C                   END
     C*
     C*   PRINT THE REPORT FOR NATURE CODE.                            D.
     C*
     C     FL(5)         IFNE      *BLANK
     C                   EXSR      PRTNA
     C                   END
     C*
     C*   PRINT THE REPORT FOR PART CODE.                              D.
     C*
     C     FL(7)         IFNE      *BLANK
     C                   EXSR      PRTPA
     C                   END
     C*
     C*   PRINT THE REPORT FOR CAUSE CODE                              D.
     C*
     C     FL(9)         IFNE      *BLANK
     C                   EXSR      PRTCA
     C                   END
     C*
     C*   PRINT THE REPORT FOR RESERVE CODE                            D.
     C*
     C     FL(11)        IFNE      *BLANK
     C                   EXSR      PRTRT
     C                   END
     C*
     C*   PRINT THE REPORT FOR PAYMENT CODE                            D.
     C*
     C     FL(13)        IFNE      *BLANK
     C                   EXSR      LOADRT
     C                   EXSR      PRTPC
     C                   END
     C*
     C*   PRINT THE REPORT FOR BR-2 CODE                               D.
     C*
     C     FL(15)        IFNE      *BLANK
     C                   EXSR      PRTB2
     C                   END
     C*
     C*   PRINT THE REPORT FOR BR-7 CODE                               D.
     C*
     C     FL(16)        IFNE      *BLANK
     C                   EXSR      PRTB7
     C                   END
     C*
     C*
     C*   PRINT THE REPORT FOR BILLING SCHEDULE.                       D.
     C*
     C     FL(2)         IFNE      *BLANK
     C                   EXSR      PRTBS
     C                   END
     C*
     C*   PRINT THE REPORT FOR A/R TRANSACTION.                        D.
     C*
     C     FL(4)         IFNE      *BLANK
     C                   EXSR      PRTTT
     C                   END
     C*
     C*   PRINT THE REPORT FOR AGENT TYPE CODE.                        D.
     C*
     C     FL(6)         IFNE      *BLANK
     C                   EXSR      PRTAG
     C                   END
     C*
     C*   PRINT THE REPORT FOR REMARK CODE                             D.
     C*
     C     FL(8)         IFNE      *BLANK
     C                   EXSR      PRTRC
     C                   END
     C*
     C*   PRINT THE REPORT FOR CHECK CODE                              D.
     C*
     C     FL(10)        IFNE      *BLANK
     C                   EXSR      PRTCC
     C                   END
     C*
     C*   PRINT THE REPORT FOR CANCELLATION REASON CODE                D.
     C*
     C     FL(12)        IFNE      *BLANK
     C                   EXSR      PRTCR
     C                   END
     C*
     C*   PRINT THE REPORT FOR LIMIT CODE                              D.
     C*
     C     FL(14)        IFNE      *BLANK
     C                   EXSR      PRTLM
     C                   END
     C*
     C*   PRINT THE REPORT FOR BILLING COMMENT CODE                    D.
     C*
     C     FL(17)        IFNE      *BLANK
     C                   EXSR      PRTBC
     C                   END
     C*
     C*   PRINT THE REPORT FOR COMMISSION PAYMENT CODE                 D.
     C*
     C     FL(18)        IFNE      *BLANK
     C                   EXSR      PRTCS
     C                   END
     C*
     C*   PRINT THE REPORT FOR COUNTY CODE                             D.
     C*
     C     FL(19)        IFNE      *BLANK
     C                   EXSR      PRTCN
     C                   END
     C*
     C*   PRINT THE REPORT FOR REFUND STATUS CODE                      D.
     C*
     C     FL(20)        IFNE      *BLANK
     C                   EXSR      PRTRS
     C                   END
     C*
     C*   PRINT THE REPORT FOR PAYMENT LATE REASON                     D.
     C*
     C     FL(21)        IFNE      *BLANK
     C                   EXSR      PRTLR
     C                   END
     C*
     C*
     C*   PRINT THE REPORT FOR A/R BATCH SYSTEM CODE
     C*
     C     FL(22)        IFNE      *BLANK
     C                   EXSR      PRTSY
     C                   END
     C*
     C*   END OF JOB.
     C*
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain OrKey POR01;
       If %found and Orstat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*   PRINT THE AGENT TYPE CODE TABLE                             *
     C*****************************************************************
     C     PRTAG         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PAT01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE AGENT TYPE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END
     C                   EXCEPT    DETL1
     C     XXCO#         READE     PAT01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE SPECIALTY TYPE CODE TABEL                         *
     C*****************************************************************
     C     PRTSP         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PSP01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE SPECIALTY FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   END
     C                   EXCEPT    DETL2
     C     XXCO#         READE     PSP01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE NCCI TYPE CODE TABLE                              *
     C*****************************************************************
     C     PRTNC         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PNC01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE NCCI TYPE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   END
     C                   EXCEPT    DETL3
     C     XXCO#         READE     PNC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE NATURE CODE TABLE                                 *
     C*****************************************************************
     C     PRTNA         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PNA01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE NATURE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C                   END
     C                   EXCEPT    DETL4
     C     XXCO#         READE     PNA01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE PART CODE TABLE                                   *
     C*****************************************************************
     C     PRTPA         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PPA01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE PART CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C                   END
     C                   EXCEPT    DETL5
     C     XXCO#         READE     PPA01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE CAUSE CODE TABLE                                  *
     C*****************************************************************
     C     PRTCA         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PCA01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CAUSE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C                   END
     C                   EXCEPT    DETL6
     C     XXCO#         READE     PCA01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE REMARK CODE TABLE                              *
     C*****************************************************************
     C     PRTRC         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PRC01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD8
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE REMARK FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD8
     C                   END
     C                   EXCEPT    DETL8
     C     XXCO#         READE     PRC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE RESERVE CODE TABLE                                *
     C*****************************************************************
     C     PRTRT         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PRT01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD9
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE RESERVE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD9
     C                   END
     C                   EXCEPT    DETL9
     C     XXCO#         READE     PRT01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE PAYMENT CODE TABLE                                *
     C*****************************************************************
     C     PRTPC         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PPC01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADA
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE PAYMENT CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PCRECD        XXKEY             2
     C                   MOVE      *BLANK        RTTYPE
     C                   Z-ADD     1             X
     C     XXKEY         LOOKUP    X1(X)                                  54
     C   54              MOVEL     X2(X)         RTTYPE
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADA
     C                   END
     C                   EXCEPT    DETLA
     C     XXCO#         READE     PPC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   LOAD THE RESERVE CODE TYPES                                 *
     C*****************************************************************
     C     LOADRT        BEGSR
     C     XXCO#         CHAIN     PRT01                              58
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C                   Z-ADD     *ZERO         X                 3 0
     C*
     C*** READ THE RESERVE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C                   ADD       1             X
     C  N58              MOVEL     RTKEY         X1(X)
     C  N58              MOVEL     RTTYPE        X2(X)
     C     XXCO#         READE     PRT01                                  58
     C                   END
     C                   END
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE BR-2 CODE TABLE                                   *
     C*****************************************************************
     C     PRTB2         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PB201                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADB
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE BR-2 CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADB
     C                   END
     C                   EXCEPT    DETLB
     C     XXCO#         READE     PB201                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE BR-7 CODE TABLE                                   *
     C*****************************************************************
     C     PRTB7         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PB701                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADC
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE BR-7 CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADC
     C                   END
     C                   EXCEPT    DETLC
     C     XXCO#         READE     PB701                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE BILLING SCHL CODE TABLE                           *
     C*****************************************************************
     C     PRTBS         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PBS01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADD
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE BILLING SCHEDULE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C                   MOVEL     BSKEY         BSXX              3 0
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADD
     C                   END
     C                   MOVE      BS            BR
     C                   EXCEPT    DETLD
     C     XXCO#         READE     PBS01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE A/R TX CODE TABLE                                 *
     C*****************************************************************
     C     PRTTT         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PTT01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADE
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE A/R TX FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADE
     C                   END
     C                   EXCEPT    DETLE
     C     XXCO#         READE     PTT01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE CHECK CONTROL TABLE FILE                          *
     C*****************************************************************
     C     PRTCC         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PCC01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADF
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE A/R TX FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADF
     C                   END
     C                   EXCEPT    DETLF
     C     XXCO#         READE     PCC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE LIMIT CODE TABLE FILE                             *
     C*****************************************************************
     C     PRTLM         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PLM01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADG
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE LIMIT CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADG
     C                   END
      ***** beg add ******* 08/16/11 **********************************

     c                   if        xxMinPrm = *off
     c                   z-add     lmmin         xxmine
     c                   else
     c                   z-add     lmmine        xxmine
     c                   endif

      ***** end add ******* 08/16/11 **********************************
     C                   EXCEPT    DETLG
     C     XXCO#         READE     PLM01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE BILLING CODE COMMENT                              *
     C*****************************************************************
     C     PRTBC         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PBC01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADH
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE LIMIT CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADH
     C                   END
     C                   EXCEPT    DETLH
     C     XXCO#         READE     PBC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE CANCELLATION REASON CODE                          *
     C*****************************************************************
     C     PRTCR         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PCR01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADX
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CAUSE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADX
     C                   END
     C                   EXCEPT    DETLX
     C     XXCO#         READE     PCR01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE COMMISSION PAYMENT CODE TABLE                     *
     C*****************************************************************
     C     PRTCS         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PCS01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADY
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE COMMISSION PAYMENT FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADY
     C                   END
     C                   EXCEPT    DETLY
     C     XXCO#         READE     PCS01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE COUNTY CODE TABLE     Added 6-22-88 LJQ           *
     C*****************************************************************
     C     PRTCN         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PCN01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD19
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE COUNTY CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD19
     C                   END
     C                   EXCEPT    DETL19
     C     XXCO#         READE     PCN01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE REFUND STATUS CODE TABLE    Added 6-22-88  LJQ    *
     C*****************************************************************
     C     PRTRS         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PRS01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD20
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE COUNTY CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD20
     C                   END
     C                   EXCEPT    DETL20
     C     XXCO#         READE     PRS01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE PAYMENT LATE REASON TABLE   Added 6-22-90  DVM    *
     C*****************************************************************
     C     PRTLR         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PLR01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD21
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE PAYMENT LATE REASON FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD21
     C                   END
     C                   EXCEPT    DETL21
     C     XXCO#         READE     PLR01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE SYSTEM A/R BATCH CODE TABLE                       *
     C*****************************************************************
     C     PRTSY         BEGSR
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C                   Z-ADD     *ZERO         PAGE
     C     XXCO#         CHAIN     PSY01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD22
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE SYSTEM A/R BATCH CD FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD22
     C                   END
     C                   EXCEPT    DETL22
     C     XXCO#         READE     PSY01                                  58
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
     O*   MLS 1/19/88  ADD DISTRIBUTION INFORMATION AND INDICATORS
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
     O*   MLS 1/19/88  ADD DISTRIBUTION INFORMATION AND INDICATORS
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
     O*  MLS 1/19/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*****                              51 '.1996'
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
      **** BEGIN CHG **** 08/16/01 ************************************
     O************************ DSDATEY   84
     O                       DSDATE        Y     86
      **** END   CHG **** 08/16/01 ************************************
     O          E            HEAD           1
     O                                            6 'W7501R'
      **** BEGIN CHG **** 08/16/01 ************************************
     O************************           79 'PAGE'
     O************************ PAGE  Z   84
     O                                           81 'PAGE'
     O                       PAGE          Z     86
      **** END   CHG **** 08/16/01 ************************************
     O                       ORTITL              70
     O          E            HEAD1          2
     O                                           50 'AGENT TYPE CODE'
     O          E            HEAD1          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           74 'STATUS'
     O          E            DETL1          1
     O                       ATKEY               32
     O                       ATDESC              67
     O                       ATSTAT              70
     O          E            HEAD2          2
     O                                           50 'SPECIALITY CODE'
     O          E            HEAD2          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL2          1
     O                       SPKEY               32
     O                       SPDESC              57
     O                       SPSTAT              60
     O          E            HEAD3          2
     O                                           46 'NCCI CODE'
     O          E            HEAD3          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'TYPE'
     O                                           74 'STATUS'
     O          E            DETL3          1
     O                       NCKEY               32
     O                       NCDESC              57
     O                       NCTYPE              62
     O                       NCSTAT              70
     O          E            HEAD4          2
     O                                           50 'NATURE CODE'
     O          E            HEAD4          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL4          1
     O                       NAKEY               32
     O                       NADESC              57
     O                       NASTAT              60
     O          E            HEAD5          2
     O                                           48 'PART CODE'
     O          E            HEAD5          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL5          1
     O                       PAKEY               32
     O                       PADESC              57
     O                       PASTAT              60
     O          E            HEAD6          2
     O                                           50 'CAUSE CODE'
     O          E            HEAD6          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL6          1
     O                       CAKEY               32
     O                       CADESC              57
     O                       CASTAT              60
     O          E            HEAD8          2
     O                                           50 'REMARK CODE'
     O          E            HEAD8          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           74 'STATUS'
     O          E            DETL8          1
     O                       RCKEY               32
     O                       RCDESC              67
     O                       RCSTAT              70
     O          E            HEAD9          2
     O                                           51 'RESERVE CODE'
     O          E            HEAD9          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'TYPE'
     O                                           74 'STATUS'
     O          E            DETL9          1
     O                       RTKEY               32
     O                       RTDESC              57
     O                       RTTYPE              62
     O                       RTSTAT              70
     O          E            HEADA          2
     O                                           51 'PAYMENT CODE'
     O          E            HEADA          1
     O                                           65 'RESERVE'
     O          E            HEADA          2
     O                                           16 'CODE'
     O                                           28 'DESCRIPTION'
     O                                           66 'CODE TYPE'
     O                                           76 'PMT TYPE'
     O                                           84 'STATUS'
     O          E            DETLA          1
     O                       PCKEY               22
     O                       PCDESC              57
     O                       PCRECD              60
     O                       RTTYPE              63
     O                       PCTRTP              73
     O                       PCSTAT              80
     O          E            HEADB          2
     O                                           49 'BR-2 CODE'
     O          E            HEADB          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           76 'PLACEMENT     '
     O                                           80 'STATUS'
     O          E            DETLB          1
     O                       B2KEY               32
     O                       B2DESC              57
     O                       B2PLCD              67
     O                       B2STAT              76
     O          E            HEADC          2
     O                                           49 'BR-7 CODE'
     O          E            HEADC          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           72 'LOST TIME'
     O                                           80 'STATUS'
     O          E            DETLC          1
     O                       B7KEY               32
     O                       B7DESC              57
     O                       B7LTLF              68
     O                       B7STAT              76
     O          E            HEADD          2
     O                                           48 'BILLING SCHEDULE CODE'
     O          E            HEADD          1
     O                                           65 'ANNUAL BILLING PERCENTAG'
     O                                           75 'E BY MONTH'
     O          E            HEADD          1
     O                                           59 '   1          2'
     O                                           70 '          3'
     O          E            HEADD          1
     O                                           59 '   4          5'
     O                                           70 '          6'
     O          E            HEADD          1
     O                                           59 '   7          8'
     O                                           70 '          9'
     O          E            HEADD          2
     O                                            9 'CODE'
     O                                           21 'DESCRIPTION'
     O                                           59 '  10         11'
     O                                           70 '         12'
     O                                           81 'S/E  ST'
     O          E            DETLD          1
     O                       BSXX          Z      8
     O                       BSDESC              40
     O                       BSBS01        4     52
     O                       BSBS02        4     63
     O                       BSBS03        4     74
     O                       BSSEFL              78
     O                       BSSTAT              82
     O          E            DETLD          1
     O                       BSBS04        4     52
     O                       BSBS05        4     63
     O                       BSBS06        4     74
     O          E            DETLD          1
     O                       BSBS07        4     52
     O                       BSBS08        4     63
     O                       BSBS09        4     74
     O          E            DETLD          2
     O                       BSBS10        4     52
     O                       BSBS11        4     63
     O                       BSBS12        4     74
     O          E            HEADE          2
     O                                           46 'A/R TRANSACTION CODE'
     O          E            HEADE          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           66 'TYPE'
     O                                           74 'STATUS'
     O          E            DETLE          1
     O                       TTKEY               32
     O                       TTDESC              57
     O                       TTTYPE              64
     O                       TTSTAT              71
     O          E            HEADF          2
     O                                           48 'CHECK CONTROL CODE'
     O          E            HEADF          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           72 'STATUS'
     O          E            DETLF          1
     O                       CCKEY               32
     O                       CCDESC              57
     O                       CCSTAT              69
     O          E            HEADG          2
     O                                           50 'LIMIT CODE'
     O          E            HEADG          1
     O                                           68 '---- BODILY INJURY BY --'
     O                                           70 '--'
      **** BEGIN CHG **** 08/16/01 ************************************
     O********************************** 83 'MIN'
      ***** beg chg ******* 08/16/11 **********************************
     O*********                                  85 'MIN'
     O                                           87 'MIN'
      ***** end chg ******* 08/16/11 **********************************
      **** END   CHG **** 08/16/01 ************************************
     O          E            HEADG          2
     O                                            7 'CODE'
     O                                           21 'DESCRIPTION'
     O                                           68 'ACCIDENT DISEASE AGGREGA'
     O                                           70 'TE'
      **** BEGIN CHG **** 08/16/01 ************************************
     O********************************** 74 'PCT'
     O********************************** 83 'PRM'
     O                                           76 'PCT'
      ***** beg chg ******* 08/16/11 **********************************
     O*************                              85 'PRM'
     O                                           87 'PRM'
      ***** end chg ******* 08/16/11 **********************************
      **** END   CHG **** 08/16/01 ************************************
     O          E            DETLG          1
     O                       LMKEY               14
     O                       LMDESC              40
     O                       LMACC         B     55
     O                       LMDIS         B     62
     O                       LMAGG         B     69
      **** BEGIN CHG **** 08/16/01 ************************************
     O************************ LMPCT B   76
     O************************ LMMIN     84 '   .  -'
     O                       LMPCT         B     78
      ***** BEG CHG ******* 08/16/11 **********************************
     O*********                LMMIN     86 '   .  -'
     O                       XXMINE           +0001 '     .  -'
      ***** END CHG ******* 08/16/11 **********************************
      **** END   CHG **** 08/16/01 ************************************
     O          E            HEADH          2
     O                                           52 'BILLING COMMENT CODE'
     O          E            HEADH          2
     O                                           26 'CODE'
     O                                           38 'COMMENT    '
     O                                           64 'STATUS'
     O          E            DETLH          1
     O                       BCKEY               32
     O                       BCDESC              57
     O                       BCSTAT              60
     O          E            HEADX          2
     O                                           52 'CANCELLATION REASON'
     O          E            HEADX          2
     O                                            7 'CODE'
     O                                           21 'DESCRIPTION'
     O                                           83 'STATUS'
     O          E            DETLX          1
     O                       CRKEY               14
     O                       CRDESC              70
     O                       CRSTAT              81
     O          E            HEADY          2
     O                                           46 'COMMISSION PAYMENT CODE'
     O          E            HEADY          2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           66 'TYPE'
     O                                           74 'STATUS'
     O          E            DETLY          1
     O                       CSKEY               32
     O                       CSDESC              57
     O                       CSTYPE              64
     O                       CSSTAT              71
     O*
     O* Added county code and refund status codes; 6-22-88  LJQ
     O*
     O          E            HEAD19         2
     O                                           48 'COUNTY CODE'
     O          E            HEAD19         2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL19         1
     O                       CNKEY               32
     O                       CNDESC              57
     O                       CNSTAT              60
     O          E            HEAD20         2
     O                                           52 'REFUND STATUS CODE'
     O          E            HEAD20         2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL20         1
     O                       RSKEY               32
     O                       RSDESC              57
     O                       RSSTAT              60
     O****************************************************************
     O* Added PAYMENT LATE REASON CODES; 6-22-90  DVM
     O****************************************************************
     O          E            HEAD21         2
     O                                           52 'PAYMENT LATE REASON CODE'
     O          E            HEAD21         2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           64 'STATUS'
     O          E            DETL21         1
     O                       LRKEY               32
     O                       LRDESC              57
     O                       LRSTAT              60
     O****************************************************************
     O* Added SYSTEM A/R BATCH CODES
     O****************************************************************
     O          E            HEAD22         2
     O                                           51 'SYSTEM A/R BATCH CODE'
     O          E            HEAD22         2
     O                                           26 'CODE'
     O                                           38 'DESCRIPTION'
     O                                           62 'TYPE'
     O                                           69 'STATUS'
     O          E            DETL22         1
     O                       SYKEY               32
     O                       SYDESC              57
     O                       SYTYPE              60
     O                       SYSTAT              66
     O****************************************************************
     O*     TOTAL RECORD COUNT FOR ALL PRINTS
     O****************************************************************
     O          E            TOTL        2  2
     O                       XXRCD         Z     30
     O                                           57 'T O T A L  R E C O R D S'
