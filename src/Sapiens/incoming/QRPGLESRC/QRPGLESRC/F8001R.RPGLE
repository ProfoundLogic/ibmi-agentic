     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     F8001R                                        **
     H* DESCRIPTION: PURGE ROUTINE - DELETE RECORDS                **
     H*              AN OPEN QUERY IN F8000C SELECTS OUT THOSE     **
     H*                 EOMBS WHOSE TRANSACTION DATE (EOTRDT) IS   **
     H*                 GREATER THAN SIX MONTHS OLD                **
     H*              THE EOMB FILE IS THEN READ.  FIRST A CHAIN    **
     H*                 TO THE FMARP VIA CO/SUB/FUND/FYR/CASE/INV# **
     H* PBR/HBA      IF IT MATCHES THE KEY, ALL RECORDS (REGARDLESS**
     H*                 OF A/R TYPE) ARE READ FOR THAT INVOICE     **
     H*                 AND THE ARAMT IS ACCUMULATED. IF AFTER ALL **
     H*                 RECORDS FOR THAT INVOICE ARE READ AND THE  **
     H*                 TOTAL ARAMT = ZERO (I.E. HAS BEEN PAID OR  **
     H*                 ADJUSTED OUT)  THEN .....                  **
     H*   NETWORK    USING NETWORK INVOICE, ALL RECORDS (REGARDLESS**
     H*                 OF A/R TYPE) ARE READ FOR THAT INVOICE     **
     H*                 AND THE ARAMT IS ACCUMULATED. IF AFTER ALL **
     H*                 RECORDS FOR THAT INVOICE ARE READ AND THE  **
     H*                 TOTAL ARAMT = ZERO (I.E. HAS BEEN PAID OR  **
     H*                 ADJUSTED OUT)  THEN .....                  **
     H*       CHAIN TO NATRP, IF HIT - DO NOT PURGE                **
     H*       ELSE......                                           **
     H*              EACH FILE IS CHAINED TO VIA                   **
     H*                 CO/SUB/FUND/FYR/CASE/EOB#                  **
     H*              IF IT MATCHES THE KEY, THE RECORD IS          **
     H*                 DELETED FROM THE ORIGINAL FILE             **
     H*              PER JOANNE/LYNN OF 007  :                     **
     H*                 DON'T PURGE CODES 64550 & 90900            **
     H*                 "LIFE OF THE CLAIM" PROC CODES             **
     H*              UPDATE CLAIM MASTER RECORD WITH PURGE FLAG "Y"**
     H*                 AND PURGE DATE (SYSTEM DATE)               **
     H*                                                            **
     H* CREATED BY:  LORRI DAPPER         DATE:  10/18/89          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H* 02/13/90 LJD - PER KATHY KEMP:  REVISED PROCESSING TO ONLY **
     H*                PURGE NON-NETWORK RECORDS BECAUSE NETWORK   **
     H*                RECORDS ARE NEEDED FOR PROVIDER REPORTS     **
     H*                                                            **
     H* 09/14/92 KRR - ADDED FDHCP FILE TO PURGE PROCESSING        **
     H*                                                            **
     H* 12/07/92 C:20  ADDED NAPBP FILE TO PURGE PROCESSING        **
     H*                                                            **
     H* 01/06/93 C:20  ADDED NDTMP FILE TO PURGE PROCESSING        **
     H*                                                            **
     H* 03/25/93 C:20  ADDED FAE1P FILE TO PURGE PROCESSING        **
     H*                                                            **
     H* 10/06/97 C:32  RECOMPILE FOR FILE CHANGE TO FAE1P          **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  50              GENERAL CHAIN/DOWEQ                       **
     H*  60              FAEOP CHAIN/DOWEQ                         **
     H*  61              NATRP CHAIN                               **
     H***************************************************************
     H* FILES:                                                     **
     H* PRIMARY FILE - OPEN QUERY FROM F8000C:                     **
     H* FAEOL4:  EOB TRANSACTION FILE      (CO/SUB/FND/FYR/CASE)   **
     H*   SELECTION:  EOTRDT LE 180 DAYS BEFORE SYSTEM DATE        **
     H*                                                            **
     H* FAE1L:  EOB AUXILLARY FILE       (CO/SUB/FND/FYR/CASE/EOB) **
     H* FADUL2: DUPLICATE CLAIMS FILE    (CO/SUB/FND/FYR/CASE/EOB) **
     H* FAHBL:  HBA TRANSACTION FILE     (CO/SUB/FND/FYR/CASE/EOB) **
     H* FDHCL:  HOSP DET CHRG FILE       (CO/SUB/FND/FYR/CASE/DSEQ)**
     H* FDCALH: DETAIL CHARGE FILE       (CO/SUB/FND/FYR/CASE/EOB) **
     H* NAPBL1: PRE/RE-CERT BILLING      (CO/SUB/FND/FYR/CASE/EOB) **
     H* NATRL:  NETWORK TRANSACTION FILE (CO/SUB/FND/FYR/CASE/EOB) **
     H* NDTML:  T/M DETAIL CHARGE FILE   (CO/SUB/FND/FYR/CASE/EOB) **
     H* NMARL6: A/R TRANSACTION FILE     (CO/SUB/FND/FYR/CASE/INV) **
     H* FMCML1: CLAIM MASTER FILE        (CO/SUB/FND/FYR/CASE)     **
     H***************************************************************
     FFAEOL4    UF   E           K DISK
     FFADUL2    UF   E           K DISK
     FFAHBL     UF   E           K DISK
     F*** BEGIN ADD *** 09/14/92 **************************************
     FFDHCL     UF   E           K DISK
     F*** END ADD *** 09/14/92 ****************************************
     F*** BEGIN ADD *** 12/07/92 **************************************
     FNAPBL1    UF   E           K DISK
     F*** END ADD *** 12/07/92 ****************************************
     F*** BEGIN ADD *** 01/06/93 **************************************
     FNDTML     UF   E           K DISK
     F*** END ADD *** 01/06/93 ****************************************
     F*** BEGIN ADD *** 01/06/93 **************************************
     FFAE1L     UF   E           K DISK
     F*** END ADD *** 01/06/93 ****************************************
     FFDCALH    UF   E           K DISK
     FNATRL     IF   E           K DISK
     FNMARL6    IF   E           K DISK
     FFMCML1    UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  A/R TRANSACTION FILE
     C*
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
     C     ARKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XAINV#
     C*
     C*   DUPLICATE CLAIMS FILE
     C*
     C     DUKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  HBA TRANSACTION FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD *** 09/14/92 **************************************
     C*
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    CADSEQ
     C*** END ADD *** 09/14/92 ****************************************
     C*** BEGIN ADD *** 09/14/92 **************************************
     C*
     C*  PRE/RE-CERT BILLING FILE
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 09/14/92 ****************************************
     C*** BEGIN ADD *** 01/06/93 **************************************
     C*
     C*  T/M DETAIL CHARGE FILE
     C     TMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 01/06/93 ****************************************
     C*** BEGIN ADD *** 03/25/93 **************************************
     C*
     C*  EOB AUXILLARY FILE
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 03/25/93 ****************************************
     C*
     C*  DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  NETWORK TRANSACTION FILE
     C*
     C     TRKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C*** INITIALIZE VARIABLES
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    ARAMT         XPAMT
     C     *LIKE         DEFINE    ARAMT         XHAMT
     C     *LIKE         DEFINE    ARAMT         XNAMT
     C     *LIKE         DEFINE    ARINV#        XAINV#
     C*
     C*** READ EOB FILE
     C*
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*** CHAIN TO A/R TRANSACTION FILE - DETERMINE IF INVOICE CONTAINING
     C***    THIS EOMB HAS BEEN PAID OR ADJUSTED OUT (I.E. = ZERO)
     C***    IF XXAMT = ZERO, THEN CONTINUE WITH PURGE
     C***    ELSE, READ NEXT EOB RECORD
     C*
     C                   Z-ADD     0             XPAMT
     C                   Z-ADD     0             XHAMT
     C                   Z-ADD     0             XNAMT
     C*
     C                   Z-ADD     EOINV#        XAINV#
     C     ARKEY         CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         XPAMT
     C                   ELSE                                                   ELSE EQ 'P'
     C                   ADD       ARAMT         XPAMT
     C                   END                                                    END EQ 'P'
     C     ARKEY         READE     PAR01                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C     HBKEY         CHAIN     PHB01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     HBINV#        XAINV#
     C     ARKEY         CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         XHAMT
     C                   ELSE                                                   ELSE EQ 'P'
     C                   ADD       ARAMT         XHAMT
     C                   END                                                    END EQ 'P'
     C     ARKEY         READE     PAR01                                  50
     C                   END                                                    ENDDOW *IN50
     C                   END                                                    ENDEQ 0 *IN51
     C*
     C     TRKEY         CHAIN     PTR01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     TRINV#        XAINV#
     C     ARKEY         CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         XNAMT
     C                   ELSE                                                   ELSE EQ 'P'
     C                   ADD       ARAMT         XNAMT
     C                   END                                                    END EQ 'P'
     C     ARKEY         READE     PAR01                                  50
     C                   END                                                    ENDDOW *IN50
     C                   END                                                    ENDEQ 0 *IN51
     C*
     C     XPAMT         IFEQ      *ZERO
     C     XHAMT         ANDEQ     *ZERO
     C     XNAMT         ANDEQ     *ZERO
     C*
     C*** IF YOU DON'T HAVE A CORRESPONDING NETWORK RECORD, THEN PURGE
     C*
     C     TRKEY         CHAIN     PTR01                              61
     C     *IN61         IFEQ      '1'
     C*
     C*** CHAIN TO DUPLICATE CLAIMS FILE
     C*
     C     DUKEY         CHAIN     PDU01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PDU01
     C     DUKEY         READE     PDU01                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C*** CHAIN TO HBA TRANSACTION FILE
     C*
     C     HBKEY         CHAIN     PHB01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PHB01
     C     HBKEY         READE     PHB01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** BEGIN ADD *** 09/14/92 **************************************
     C*
     C*** CHAIN TO HOSPITAL DETAIL CHARGE FILE
     C*
     C     HCKEY         CHAIN     PHC01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PHC01
     C     HCKEY         READE     PHC01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** END ADD *** 09/14/92 ****************************************
     C*** BEGIN ADD *** 12/07/92 **************************************
     C*
     C*** CHAIN TO PRE/RE-CERT BILLING FILE
     C*
     C     PBKEY         CHAIN     PPB01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PPB01
     C     PBKEY         READE     PPB01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** END ADD *** 09/14/92 ****************************************
     C*** BEGIN ADD *** 01/06/93 **************************************
     C*
     C*** CHAIN TO T/M DETAIL CHARGE FILE
     C*
     C     TMKEY         CHAIN     PTM01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PTM01
     C     TMKEY         READE     PTM01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** END ADD *** 01/06/93 ****************************************
     C*** BEGIN ADD *** 03/25/93 **************************************
     C*
     C*** CHAIN TO EOB AUXILLARY FILE
     C*
     C     E1KEY         CHAIN     PE101                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PE101
     C     E1KEY         READE     PE101                                  50
     C                   END                                                    ENDDOW *IN50
     C*** END ADD *** 03/25/93 ****************************************
     C*
     C*** CHAIN TO DETAIL CHARGE FILE
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C     CACPT4        IFNE      '64550'
     C     CACPT4        ORNE      '90900'
     C                   DELETE    PCA01
     C                   END                                                    ENDNE 64550/90900
     C     CAKEY         READE     PCA01                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C*** DELETE ORIGINAL/READ NEXT RECORD
     C*
     C                   DELETE    PEO01
     C*
     C*** UPDATE CLAIM MASTER RECORD WITH PURGE FLAG/DATE
     C*
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C*
     C* KEEP TRACK OF WHETHER THIS IS THE FIRST TIME FOR PURGE '1' OR
     C*   IF THE CASE HAS BEEN PURGED 2+ TIMES '2'
     C*
     C     CMPRFL        IFEQ      *BLANK
     C                   MOVE      '1'           CMPRFL
     C                   ELSE                                                   ELS EQ *BLANK
     C                   MOVE      '2'           CMPRFL                         PURGE FLAG
     C                   END                                                    END EQ *BLANK
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMPRDT
     C                   UPDATE    PCM01
     C                   END                                                    END 0 *IN50
     C*
     C                   END                                                    END 1  *IN61
     C*
     C                   END                                                    END XXAMT = 0
     C*
     C                   READ      PEO01                                  60
     C                   END                                                    ENDDOW *IN60
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
