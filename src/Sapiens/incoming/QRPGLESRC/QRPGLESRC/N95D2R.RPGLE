     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N95D2R - TYPE D2 DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9500R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9500R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: 98/NW001             *
     H*  CREATED: 03/21/94                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  03/07/95    C:20    REQ/PROJ: NW007                          *
     H*                      1. PGM HAS BEEN UPDATED TO ACCEPT NEW    *
     H*                         PARMS:                                *
     H*                         SYS  1                                *
     H*                         FL1  1                                *
     H*                         FL2  1                                *
     H*                         FL3  1                                *
     H*                         FL4  1                                *
     H*                         FL5  1                                *
     H*                         DS1 10                                *
     H*                         DS2 10                                *
     H*                         DS3 10                                *
     H*                         DS4 10                                *
     H*                         DS5 10                                *
     H*                                                               *
     H*  10/30/96    C:45    REQ/PROJ: NW009                          *
     H*                      1. ADD EOMB CODE 398 FOR NETWORK         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN
     FFAEOLC    IF   E           K DISK
     F                                     RENAME(PEO01:PEO0A)
     FFDCALN    UF   E           K DISK
     FFTABL     IF   E           K DISK

     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  40     41
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D* PROVIDER EFFECTIVE DATE
     D                 DS
     D  XXEDAT                 1      7  0
     D  XYEAR                  1      3  0
     D  XMONTH                 4      5  0
     D  XDAY                   6      7  0
     D* PROVIDER EFFECTIVE ENDING DATE
     D                 DS
     D  XENDAT                 1      7  0
     D  XEYEAR                 1      3  0
     D  XEMONT                 4      5  0
     D  XEDAY                  6      7  0
     D*
     D                 DS
     D  XYYMM                  1      4  2
     D  YY                     1      2  0
     D  MM                     3      4  2

     C*
     C*   KEY LIST FOR THE SERVICE CODE FILE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C*  EOB RECORDS BY PROVIDER -- FOR SETLL
     C*
     C     EOKEYA        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXEDAT
     C*
     C*  EOB RECORDS BY PROVIDER -- FOR READE
     C*
     C     EOKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  CASE DETAIL RECORDS
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXFDAT
     C                   KFLD                    XXDSEQ
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQCODE            3
     C                   PARM                    QQCEDT            8
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
     C                   PARM                    QQRTCD            1
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
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    CAAST         C1AST
     C*
     C                   SETOFF                                       99
     C                   Z-ADD     *ZEROS        XXPTOT
     C                   Z-ADD     *ZEROS        XXPERC            9 2
     C                   Z-ADD     *ZEROS        REMAIN            2 0
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C*
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        X1EDAT            8 0
     C*
     C                   Z-ADD     X1EDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEDAT
     C*
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*
     C* GET STATE FIRST
     C     CAAST         IFEQ      *BLANKS
     C                   MOVE      'FL'          C1AST
     C                   ELSE
     C                   MOVE      CAAST         C1AST
     C                   END
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           STOPN             1
     C                   MOVE      ' '           STOVR
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     C1AST         @@ST                           EOMB ADJUD STATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C*** FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C*** THAT APPLYS TO THAT DATE OF SERVICE.
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  60
     C*
     C*** VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      QQCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN60 OFF
     C*
     C*** IF A VALID DISCOUNT RECORD IS NOT FOUND, ZERO THE % SO THE
     C*** NETWORK DISCOUNT WILL BE ZERO, THEN READ NEXT RECORD ...
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        BDPC01
     C                   Z-ADD     *ZEROS        BDPC02
     C                   Z-ADD     *ZEROS        BDPC03
     C                   Z-ADD     *ZEROS        BDPC04
     C                   Z-ADD     *ZEROS        BDPC05
     C                   Z-ADD     *ZEROS        BDPC06
     C                   Z-ADD     *ZEROS        BDPC07
     C                   GOTO      TERM
     C                   END                                                    *IN60 ON
     C                   Z-ADD     *ZEROS        XXNDIS            9 2
     C*
     C*** RETREIVE SERVICE CODE RECORD TO DETERMINE WHICH FORMULA TO
     C*   USE IN CALC NETWORK DISCOUNT - MRA OR U & C.
     C*
     C                   Z-ADD     CASVCD        XXRVCD            3 0
     C                   MOVE      CACPT4        XXHCPC            5
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          ADD 05/31/92
     C*
     C     SVKEY         SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     XXCO#
     C     SVCMSV        ANDEQ     XXCMSV                                       CHG 04/28/92
     C     SVSTAT        IFNE      'I'
     C*
     C     SVALOW        IFGT      *ZEROS                                        MRA CALC
     C     CATOTP        MULT(H)   BDPC01        XXNDIS
     C                   ELSE
     C*
     C*** EXIT TO CALCULATE % -- ONLY ONCE PER EOB
     C*
     C  N99              EXSR      SRUC                                          U & C CALC
     C     CATOTP        MULT(H)   XXPERC        XXNDIS
     C                   END                                                     SVALOW
     C*
     C                   ELSE
     C                   GOTO      NXTSV1
     C                   END
     C                   END
     C*
     C                   Z-ADD     XXNDIS        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CANDIS        IFGT      *ZEROS
     C                   MOVE      '398'         CAEOMB
     C                   ENDIF
     C*
     C                   UPDATE    PCA01
     C*
     C                   END                                                     CLM STS CD
     C*
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*                  SRUC  - U&C SUBROUTINE                       *
     C*****************************************************************
     C*
     C     SRUC          BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXPERC
     C*
     C*  READ ALL CASE DETAIL RECORDS BY PROVIDER AS OF EFFECTIVE DATE
     C*  TO ACCUMULATE THE TOTAL REVENUE FOR THIS PROVIDER. THE TOTAL
     C*  AMT IS USED TO DETERMINE WHAT RANGE TO USE WHEN APPLYING THE
     C*  THE NETWORK DISCOUNT.
     C*
     C*  FIRST CALCULATE THE ENDING DATE
     C*
     C*
     C                   Z-ADD     XXEDAT        XENDAT
     C     BDAM25        DIV       12            XYYMM
     C     MM            MULT(H)   12            XMM               2 0
     C     XMM           ADD       XMONTH        XEMONT
     C*
     C     XEMONT        IFGT      12
     C     XEMONT        SUB       12            XEMONT
     C                   ADD       1             YY
     C                   END
     C*
     C                   ADD       YY            XEYEAR
     C*
     C*
     C*  START READING RECORDS BY PROVIDER -- TOTAL AMT BY PROVIDER
     C*
     C     EOKEYA        SETLL     PEO0A
     C     EOKEY1        READE     PEO0A                                  59
     C*
     C     *IN59         DOWEQ     '0'
     C     EOFRDT        ANDLE     XENDAT
     C*
     C                   ADD       EOAMT         XXPTOT           10 2
     C*
     C     EOKEY1        READE     PEO0A                                  59
     C*
     C                   END                                                     END *IN59
     C*
     C*  NOW FIND THE DISCOUNT PER CENT -- DETERMINE THE AMT RANGE
     C*
     C* U&C --  TOTAL PAID BETWEEN BDAM01/02 --- USE BDPC02
     C*         TOTAL PAID BETWEEN BDAM03/04 --- USE BDPC03
     C*         TOTAL PAID OVER BDAM05 --- USE BDPC04
     C*
     C     XXPTOT        IFGE      *ZEROS
     C     XXPTOT        ANDLE     BDAM01
     C                   Z-ADD     BDPC02        XXPERC
     C                   END                                                     TOTP=0
     C*
     C     XXPTOT        IFGE      BDAM02
     C     XXPTOT        ANDLE     BDAM03
     C                   Z-ADD     BDPC03        XXPERC
     C                   END                                                     BDAM02
     C*
     C     XXPTOT        IFGE      BDAM04
     C     XXPTOT        ANDLE     BDAM05
     C                   Z-ADD     BDPC04        XXPERC
     C                   END                                                     BDAM04
     C*
     C     XXPTOT        IFGE      BDAM06
     C     XXPTOT        ANDLE     BDAM07
     C                   Z-ADD     BDPC05        XXPERC
     C                   END                                                     BDAM04
     C*
     C     XXPTOT        IFGE      BDAM08
     C     XXPTOT        ANDLE     BDAM09
     C                   Z-ADD     BDPC06        XXPERC
     C                   END                                                     BDAM04
     C*
     C     XXPTOT        IFGE      BDAM10
     C     XXPTOT        ANDLE     BDAM11
     C                   Z-ADD     BDPC07        XXPERC
     C                   END                                                     BDAM04
     C*
     C                   SETON                                        99
     C*
     C                   ENDSR
     C*
**  QCMDEXC TABLE
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
