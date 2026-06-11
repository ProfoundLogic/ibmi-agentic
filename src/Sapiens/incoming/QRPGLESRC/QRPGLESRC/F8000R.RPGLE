     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H***************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*                                                            **
     H* PROGRAM:     F8000R                                        **
     H* DESCRIPTION: PURGE ROUTINE - WRITE TO PURGE FILE           **
     H*              AN OPEN QUERY IN F8000C SELECTS OUT THOSE     **
     H*                 EOMBS WHOSE TRANSACTION DATE (EOTRDT) IS   **
     H*                 GREATER THAN SIX MONTHS OLD                **
     H*              THE EOMB FILE IS THEN READ.  FIRST A CHAIN    **
     H*                 TO THE FMARP VIA CO/SUB/FUND/FYR/CASE/INV# **
     H*   PBR/HBA    IF IT MATCHES THE KEY, ALL RECORDS (REGARDLESS**
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
     H*              IF IT MATCHES THE KEY, A RECORD IS THEN       **
     H*                 WRITTEN TO THE RESPECTIVE PURGE FILE       **
     H*              PER JOANNE/LYNN OF 007  :                     **
     H*                 DON'T PURGE CODES 64550 & 90900            **
     H*                 "LIFE OF THE CLAIM" PROC CODES             **
     H*              A REPORT IS GENERATED FOR THE USER TO         **
     H*                 REFERENCE FOR HISTORY PURPOSES.            **
     H*                                                            **
     H* CREATED BY:  C:05                 DATE:  10/18/89          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H*                                                              **
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*             189  Retrieve brand Name                          *
     H*                                                               *
     H* 02/12/90 C:05- PER 007/KK:      REVISED PROCESSING TO ONLY **
     H*                PURGE NON-NETWORK RECORDS BECAUSE NETWORK   **
     H*                RECORDS ARE NEEDED FOR PROVIDER REPORTS     **
     H*                                                            **
     H*  08/26/92 C:05  PROJ: 99AAC                                **
     H*                 Changed all instances of EOFLG5 to EONLFL  **
     H*                                                            **
     H*  10/27/92 C:20  PROJ: 99AAC                                **
     H*                 CHANGED CAPMOD TO COMP TO BLANKS INSTEAD OF**
     H*                 ZEROS                                      **
     H*                                                            **
     H* 12/07/92 C:20  ADDED NAPBP FILE TO PURGE PROCESSING        **
     H*                                                            **
     H* 01/06/93 C:20  ADDED NDTMP FILE TO PURGE PROCESSING        **
     H*                                                            **
     H* 03/19/93 C:20  PROJ: 49/MC009                              **
     H*                1. CHANGED CODE TO NOT SUBTRACT CANDIS FROM **
     H*                   FROM CATOTP TO GET BOTTOM LINE CATOTP    **
     H*                   ALREADY HAVE BOTTOM LINE                 **
     H*                                                            **
     H* 03/25/93 C:20  PROJ: 79/MC039                              **
     H*                1. ADDED FILES FAE1P,FDE1P TO PURGE ROUTINE **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  20              THERE IS A PROVIDER MODIFIER              **
     H*  21              THERE IS A PROCEDURE CODE MODIFIER        **
     H*  50              GENERAL CHAIN/DOWEQ                       **
     H*  51              GENERAL CHAIN/DOWEQ                       **
     H*  60              FAEOP CHAIN/DOWEQ                         **
     H*  61              NATRP CHAIN                               **
     H***************************************************************
     H* FILES:                                                     **
     H* PRIMARY FILE - OPEN QUERY FROM F8000C:                     **
     H* FAEOL4:  EOB TRANSACTION FILE      (CO/SUB/FND/FYR/CASE)   **
     H*   SELECTION:  EOTRDT LE 180 DAYS BEFORE SYSTEM DATE        **
     H* PAEOP:  PURGE FILE                                         **
     H*                                                            **
     H* FAE1L :  EOB AUXILLARY FILE      (CO/SUB/FND/FYR/CASE/EOB) **
     H*   SELECTION:  EOTRDT LE 180 DAYS BEFORE SYSTEM DATE        **
     H* PAE1P:  PURGE FILE                                         **
     H*                                                            **
     H* FADUL2:  DUPLICATE CLAIMS FILE   (CO/SUB/FND/FYR/CASE/DESQ)**
     H* PADUP:  PURGE FILE                                         **
     H*                                                            **
     H* FAHBL:  HBA TRANSACTION FILE     (CO/SUB/FND/FYR/CASE/DSEQ)**
     H* PAHBP:  PURGE FILE                                         **
     H*                                                            **
     H* FDCALH:  DETAIL CHARGE FILE      (CO/SUB/FND/FYR/CASE/EOB#)**
     H* PDCAP:  PURGE FILE                                         **
     H*                                                            **
     H* NAPBL1: PRE/RE-CERT BILLING FILE (CO/SUB/FND/FYR/CASE/EOB#)**
     H* PAPBP:  PURGE FILE                                         **
     H*                                                            **
     H* NATRL:  NETWORK TRANSACTION FILE (CO/SUB/FND/FYR/CASE/EOB) **
     H* PATRP:  PURGE FILE (NO LONGER USED 02/12/90)               **
     H*                                                            **
     H* NDTMP:  T/M DETAIL CHARGE FILE   (CO/SUB/FND/FYR/CASE/EOB#)**
     H* PDTMP:  PURGE FILE                                         **
     H*                                                            **
     H* FDHCP:  HOSP DET CHRG FILE       (CO/SUB/FND/FYR/CASE/DSEQ)**
     H* PDHCP:  PURGE FILE                                         **
     H*                                                            **
     H* FMARL6: A/R TRANSACTION FILE  (CO/SUB/FND/FYR/CASE/INV)    **
     H***************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     F*
     FFAEOL4    IF   E           K DISK
     FPAEOP     O    E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFAE1L     IF   E           K DISK
     FPAE1P     O    E           K DISK
     F                                     RENAME(PE101:PE102)
     FFADUL2    IF   E           K DISK
     FPADUP     O    E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFAHBL     IF   E           K DISK
     FPAHBP     O    E           K DISK
     F                                     RENAME(PHB01:PHB02)
     FFDCALH    IF   E           K DISK
     FPDCAP     O    E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FNATRL     IF   E           K DISK
     FNMARL6    IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FPDHCP     O    E           K DISK
     F                                     RENAME(PHC01:PHC02)
     FNAPBL1    IF   E           K DISK
     FPAPBP     O    E           K DISK
     F                                     RENAME(PPB01:PPB02)
     FNDTML     IF   E           K DISK
     FPDTMP     O    E           K DISK
     F                                     RENAME(PTM01:PTM02)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS1          DS
     D  FIL11                  1    200
     D  FIL12                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D*
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    CADSEQ
     C*
     C*  PRE/RE-CERT BILLING FILE
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  T/M DETAIL CHARGE FILE
     C     TMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  EOB AUXILLARY FILE
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  A/R TRANSACTION FILE
     C*
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
     C*  ENTRY LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQLEDT            8
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
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** INITIALIZE VARIABLES
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQLEDT        X1LEDT            8 0
     C*
     C                   Z-ADD     X1LEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXLEDT            6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C     XXMODE        IFEQ      'D'
     C                   MOVE      '1'           *IN09                          DEMAND MODE
     C                   MOVE      '0'           *IN08
     C                   ELSE
     C                   MOVE      '1'           *IN08                          BATCH MODE
     C                   MOVE      '0'           *IN09
     C                   END
     C*
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    SMNAME        XXSBNM
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    FNNAME        XXFNNM
     C     *LIKE         DEFINE    FNNAME        XXCSNM
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOSYS         XXSYS
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    EOFLIC        XXFLIC
     C     *LIKE         DEFINE    EO#BLL        XX#BLL
     C     *LIKE         DEFINE    EODIA1        XXDIA1
     C     *LIKE         DEFINE    EODIA2        XXDIA2
     C     *LIKE         DEFINE    EODIA3        XXDIA3
     C     *LIKE         DEFINE    EODIA4        XXDIA4
     C     *LIKE         DEFINE    EONLFL        XXNLFL
     C     *LIKE         DEFINE    EOEMPL        XXEMPL
     C     *LIKE         DEFINE    EOINV#        XXEINV
     C     *LIKE         DEFINE    EOBAMT        XXEAMT
     C     *LIKE         DEFINE    EOINV#        XXNINV
     C     *LIKE         DEFINE    EOBAMT        XXNAMT
     C*
     C     *LIKE         DEFINE    CACSCD        XXCSCD
     C     *LIKE         DEFINE    CAPRTY        XXPRTY
     C     *LIKE         DEFINE    CACPT4        XXCODE
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CAUNTS        XXUNTS
     C     *LIKE         DEFINE    CAPART        XXPART
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CADXR         XXDXR
     C     *LIKE         DEFINE    CAEOMB        XXEOMB
     C     *LIKE         DEFINE    CATOTC        XXTOTC
     C     *LIKE         DEFINE    CATOTP        XXTOTP
     C     *LIKE         DEFINE    CATOTD        XXTOTD
     C     *LIKE         DEFINE    CAOTHR        XXOTHR
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C*
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    ARAMT         XPAMT
     C     *LIKE         DEFINE    ARAMT         XHAMT
     C     *LIKE         DEFINE    ARAMT         XNAMT
     C     *LIKE         DEFINE    ARINV#        XAINV#
     C*
     C                   Z-ADD     0             XXYR              2 0
     C                   Z-ADD     0             DUCNT             7 0
     C                   Z-ADD     0             HBCNT             7 0
     C                   Z-ADD     0             CACNT             7 0
     C                   Z-ADD     0             TRCNT             7 0
     C                   Z-ADD     0             EOCNT             7 0
     C                   Z-ADD     0             HCCNT             7 0
     C                   Z-ADD     0             PBCNT             7 0          ADD 12/07/92
     C                   Z-ADD     0             TMCNT             7 0          ADD 01/06/93
     C                   Z-ADD     0             E1CNT             7 0          ADD 03/25/93
     C                   MOVE      '0'           EOBCHG            1            EOB# CHGD
     C                   MOVE      '0'           ONERCD            1            1 RCD PROCESSED
     C*
     C*** READ EOB FILE
     C*
     C                   READ      PEO01                                  60
     C                   Z-ADD     EOCO#         XXCO#
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM
     C                   ELSE                                                   ELSE 0 *IN50
     C                   MOVE      *BLANKS       XXCONM
     C                   END                                                    END 0 *IN50
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** GET OUTPUT ROUTING RECORD
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END 0 *IN50
     C*
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
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
     C*** EXECUTE EOB SUBROUTINE TO GET EOB INFORMATION FOR REPORT
     C*
     C                   EXSR      EOB
     C*
     C*** CHAIN TO DUPLICATE CLAIMS FILE
     C*
     C     DUKEY         CHAIN     PDU01                              50
     C     *IN50         DOWEQ     '0'
     C                   WRITE     PDU02                                        PURGE
     C                   ADD       1             DUCNT
     C     DUKEY         READE     PDU01                                  50
     C                   END                                                    ENDDOW *IN50
     C     HCKEY         CHAIN     PHC01                              50
     C     *IN50         DOWEQ     '0'
     C                   WRITE     PHC02
     C                   ADD       1             HCCNT
     C     HCKEY         READE     PHC01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** PROCESS PRE/RE-CERT BILLING FILE
     C     PBKEY         CHAIN     PPB01                              50
     C     *IN50         DOWEQ     '0'
     C                   WRITE     PPB02
     C                   ADD       1             PBCNT
     C     PBKEY         READE     PPB01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** PROCESS T/M DETAIL CHARGE FILE
     C     TMKEY         CHAIN     PTM01                              50
     C     *IN50         DOWEQ     '0'
     C                   WRITE     PTM02
     C                   ADD       1             TMCNT
     C     TMKEY         READE     PTM01                                  50
     C                   END                                                    ENDDOW *IN50
     C*** PROCESS EOB AUXILLARY FILE
     C     E1KEY         CHAIN     PE101                              50
     C     *IN50         DOWEQ     '0'
     C                   WRITE     PE102
     C                   ADD       1             E1CNT
     C     E1KEY         READE     PE101                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C*** CHAIN TO HBA TRANSACTION FILE
     C*
     C     HBKEY         CHAIN     PHB01                              50
     C     *IN50         DOWEQ     '0'
     C                   WRITE     PHB02                                        PURGE
     C                   ADD       1             HBCNT
     C     HBKEY         READE     PHB01                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C*** CHAIN TO DETAIL CHARGE FILE
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C     CACPT4        IFNE      '64550'
     C     CACPT4        ORNE      '90900'
     C*** EXECUTE DETCHG SUBROUTINE TO GET DETAIL INFO FOR REPORT
     C                   EXSR      DETCHG
     C                   WRITE     PCA02                                        PURGE
     C                   ADD       1             CACNT
     C                   END                                                    ENDNE 64550/90900
     C     CAKEY         READE     PCA01                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C*** WRITE EOB PURGE RECORD/READ NEXT RECORD
     C*
     C                   WRITE     PEO02                                        PURGE
     C                   ADD       1             EOCNT
     C*
     C                   END                                                    END 1 *IN61
     C*
     C                   END                                                    END XXAMT = 0
     C*
     C                   READ      PEO01                                  60
     C                   END                                                    ENDDOW *IN60
     C*
     C*** IF AT LEAST ONE RECORD IS PROCESSED, THEN ..........
     C***     PRINT FINAL TOTAL PAGE - NUMBER OF RECORDS DELETED FROM EACH FILE
     C     ONERCD        IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTAL
     C*** ELSE.............
     C***     PRINT ERROR MESSAGE
     C                   ELSE                                                   ELS 1 ONERCD
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   END                                                    END 1 ONERCD
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C********************************************************************
     C* EOB SUBROUTINE - GET EOB INFORMATION                             *
     C********************************************************************
     C     EOB           BEGSR
     C*
     C*** SUBSCRIBER CHANGE - GET NEW INFORMATION
     C*
     C     EOSUB#        IFNE      XXSUB#
     C                   Z-ADD     EOSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END                                                    END 0 *IN50
     C                   MOVE      '1'           *INOF
     C                   END                                                    ENDNE XXSUB#
     C*
     C*** FUND CHANGE - GET NEW INFORMATION
     C*
     C     EOFND         IFNE      XXFND
     C                   Z-ADD     EOFND         XXFND
     C     FNKEY         CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END                                                    END 0 *IN50
     C                   MOVE      '1'           *INOF
     C                   END                                                    ENDNE XXFND
     C*
     C*** CASE CHANGE - GET NEW INFORMATION
     C*
     C     EOCASE        IFNE      XXCASE
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOFYR         XXYR
     C                   Z-ADD     EOCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        XXSS# = MaskData('SSN':'XXSS#':XXSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   MOVE      *BLANKS       XXPRFL            1
     C     CMPRFL        IFEQ      '2'
     C                   MOVE      '*'           XXPRFL
     C                   END
     C*
     C                   Z-ADD     CMPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPRDT            6 0
     C*
     C     CDKEY         CHAIN     PCD01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCSNM
     C                   ELSE                                                   ELSEIF *IN51
     C                   MOVE      *BLANKS       XXCSNM
     C                   END                                                    END IF *IN51
     C                   ELSE                                                   ELSEIF *IN50
     C                   MOVE      *BLANKS       XXCSNM
     C                   Z-ADD     *ZERO         XXACDT
     C                   MOVE      *BLANKS       XXPRFL
     C                   Z-ADD     *ZERO         XXPRDT
     C                   END                                                    END IF *IN50
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    HEAD1
     C                   END                                                    ENDNE XXCASE
     C*
     C*
     C                   Z-ADD     0             XXEINV
     C                   Z-ADD     0             XXEAMT
     C                   Z-ADD     EOEOB#        XXEOB#
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C                   MOVE      EOSYS         XXSYS
     C                   Z-ADD     EOFEIN        XXFEIN
     C                   Z-ADD     EOMOD         XXMOD
     C     XXMOD         IFNE      *ZERO
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C                   MOVE      EOFLIC        XXFLIC
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     EO#BLL        XX#BLL
     C                   MOVE      EODIA1        XXDIA1
     C                   MOVE      EODIA2        XXDIA2
     C                   MOVE      EODIA3        XXDIA3
     C                   MOVE      EODIA4        XXDIA4
     C                   MOVE      EONLFL        XXNLFL
     C                   MOVE      EOEMPL        XXEMPL
     C*
     C                   Z-ADD     EODFC         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDFC             6 0
     C*
     C                   Z-ADD     EOINV#        XXEINV
     C                   Z-ADD     EOBAMT        XXEAMT
     C*
     C     HBKEY         CHAIN     PHB01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     HBINV#        XXEINV
     C                   Z-ADD     HB1AAM        XXEAMT
     C                   END                                                    END 0 *IN50
     C*
     C     TRKEY         CHAIN     PTR01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     TRINV#        XXNINV
     C                   Z-ADD     TRBAMT        XXNAMT
     C                   ELSE
     C                   Z-ADD     0             XXNINV
     C                   Z-ADD     0             XXNAMT
     C                   END                                                    END 0 *IN50
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    HEAD2
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    DET2
     C                   MOVE      '1'           EOBCHG                         EOB# CHGD
     C                   MOVE      '1'           ONERCD                         1 RCD PROCESSED
     C*
     C                   ENDSR
     C********************************************************************
     C* DETCHG SUBROUTINE - GET DETAIL CHARGE INFORMATION                *
     C********************************************************************
     C     DETCHG        BEGSR
     C*
     C*
     C                   MOVE      CACSCD        XXCSCD
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXSVDT            6 0
     C*
     C                   MOVE      CAPRTY        XXPRTY
     C                   MOVE      *BLANKS       XXCODE
     C     CASVCD        IFNE      *ZERO
     C                   MOVEL     CASVCD        XXCODE
     C                   ELSE
     C                   MOVEL     CACPT4        XXCODE
     C                   END
     C                   MOVE      CAPMOD        XXPMOD                         CHG 10/27/92
     C     XXPMOD        IFNE      *BLANKS                                      CHG 10/27/92
     C                   MOVE      '1'           *IN21
     C                   ELSE
     C                   MOVE      '0'           *IN21
     C                   END
     C                   MOVE      CAUNTS        XXUNTS
     C                   MOVE      CAPART        XXPART
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CADXR         XXDXR
     C                   MOVE      CAEOMB        XXEOMB
     C                   Z-ADD     CATOTC        XXTOTC
     C                   Z-ADD     CATOTP        XXTOTP
     C                   Z-ADD     CATOTD        XXTOTD
     C                   Z-ADD     CAOTHR        XXOTHR
     C                   Z-ADD     CANDIS        XXNDIS
     C*
     C     EOBCHG        IFEQ      '1'
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           EOBCHG
     C                   END                                                    END 1 EOBCHG
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOF
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    DET3
     C*
     C                   ENDSR
     C********************************************************************
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 1997      '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSDATE        Y     08
     O                       XXCONM             119
     O                       DSTIME             198 '  :  :  '
     O*
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E            HEAD           1
     O                                          104 'RECORDS WITH EOTRDT LE'
     O                       XXLEDT             113 '  /  /  '
     O*
     O          E            HEAD1       1  0
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD1          0
     O                       XXSUB#              17
     O*
     O          E            HEAD1          1
     O                       XXSUB#              17
     O*
     O          E            HEAD1          0
     O                                            6 'GROUP:'
     O                       XXFND               17
     O                                           19 '-'
     O                       XXFNNM              80
     O*
     O          E            HEAD1          0
     O                       XXFND               17
     O*
     O          E            HEAD1          1
     O                       XXFND               17
     O*
     O          E            HEAD1          0
     O                                            5 'CASE:'
     O                       XXYR                 8
     O                                            9 '-'
     O                       XXCASE        Z     16
     O                                           29 'SS#/ACDT:'
     O                       XXSS#               39
     O                       XXACDT              49 '  /  /  '
     O                                           59 'NAME:'
     O                       XXCSNM             121
     O                                          150 'LAST PURGE DATE:'
     O                       XXPRDT             159 '  /  /  '
     O                       XXPRFL             161
     O*
     O          E            HEAD1          0
     O                       XXYR                 8
     O                                            9 '-'
     O                       XXCASE        Z     16
     O                       XXSS#               39
     O                       XXACDT              49 '  /  /  '
     O*
     O          E            HEAD1          1
     O                       XXYR                 8
     O                                            9 '-'
     O                       XXCASE        Z     16
     O                       XXSS#               39
     O                       XXACDT              49 '  /  /  '
     O*
     O          E            HEAD2       1  1
     O                                           17 'TRANS'
     O                                           77 'SERVICE DATES'
     O                                           87 '# OF'
     O                                          127 'NO LOST'
     O                                          135 'PAT'
     O                                          148 'DATE 1ST'
     O                                          182 'NETWK'
     O                                          195 'NETWK'
     O          E            HEAD2          2
     O                                            5 'EOMB#'
     O                                           16 'DATE'
     O                                           25 'SYS'
     O                                           42 'PAYEE NUMBER'
     O                                           57 'PHYS LIC#'
     O                                           68 'FROM'
     O                                           76 'TO'
     O                                           88 'BILLS'
     O                                          112 'DIAGNOSIS CODES'
     O                                          125 'TIME'
     O                                          135 'EMP'
     O                                          148 'CONSULTD'
     O                                          158 'INV #'
     O                                          172 'INV AMOUNT'
     O                                          182 'INV #'
     O                                          198 'INV AMOUNT'
     O*
     O          E            DET2           1
     O                       XXEOB#        Z      9
     O                       XXTRDT              18 '  /  /  '
     O                       XXSYS               24
     O                       XXFEIN        Z     39
     O               20                          40 '-'
     O               20      XXMOD         Z     43
     O                       XXFLIC              57
     O                       XXFRDT              70 '  /  /  '
     O                                           71 '-'
     O                       XXTODT              79 '  /  /  '
     O                       XX#BLL        Z     87
     O                       XXDIA1              98
     O                       XXDIA2             104
     O                       XXDIA3             110
     O                       XXDIA4             116
     O                       XXNLFL             123
     O                       XXEMPL             134
     O                       XXDFC              148 '  /  /  '
     O                       XXEINV        Z    158
     O                       XXEAMT        4    172
     O                       XXNINV        Z    182
     O                       XXNAMT        4    198
     O*
     O          E            HEAD3       1  1
     O                                           25 'DETAIL CHARGES:'
     O*
     O          E            HEAD3          1
     O                                           13 'CLM'
     O                                           26 'SERVICE'
     O                                           45 'SRV/PROC'
     O                                           64 'PART'
     O                                           74 'DETAIL'
     O                                           81 'DIAG'
     O                                           88 'EOMB'
     O*
     O          E            HEAD3          2
     O                                           13 'STS'
     O                                           24 'DATE'
     O                                           33 'PRTY'
     O                                           45 'CODE  MD'
     O                                           56 'UNITS'
     O                                           64 'CODE'
     O                                           73 'SEQ #'
     O                                           81 'CODE'
     O                                           88 'CODE'
     O                                          103 'TOTAL CHRG'
     O                                          118 'MRA REDUCS'
     O                                          133 'OTH REDUCS'
     O                                          148 'NET REDUCS'
     O                                          163 'TOTAL PAID'
     O*
     O          E            DET3           1
     O                       XXCSCD              13
     O                       XXSVDT              26 '  /  /  '
     O                       XXPRTY              32
     O                       XXCODE              42
     O               21                          43 '-'
     O               21      XXPMOD              45
     O                       XXUNTS        4     56
     O                       XXPART              63
     O                       XXDSEQ        Z     73
     O                       XXDXR               80
     O                       XXEOMB              88
     O                       XXTOTC        4    103
     O                       XXTOTD        4    118
     O                       XXOTHR        4    133
     O                       XXNDIS        4    148
     O                       XXTOTP        4    163
     O*
     O          E            TOTAL          2
     O                                          105 'FINAL TOTALS'
     O          E            TOTAL          1
     O                                          103 'RECORDS'
     O          E            TOTAL          2
     O                                           84 'FILE'
     O                                          102 'PURGED'
     O          E            TOTAL          2
     O                                           85 'FAEOP'
     O                       EOCNT         Z    103
     O          E            TOTAL          2
     O                                           85 'FAE1P'
     O                       E1CNT         Z    103
     O          E            TOTAL          2
     O                                           85 'FADUP'
     O                       DUCNT         Z    103
     O          E            TOTAL          2
     O                                           85 'FAHBP'
     O                       HBCNT         Z    103
     O          E            TOTAL          2
     O                                           85 'FDHCP'
     O                       HCCNT         Z    103
     O          E            TOTAL          2
     O                                           85 'FDCAP'
     O                       CACNT         Z    103
     O          E            TOTAL          2
     O                                           85 'NAPBP'
     O                       PBCNT         Z    103
     O          E            TOTAL          2
     O                                           85 'NDTMP'
     O                       TMCNT         Z    103
     O*
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
