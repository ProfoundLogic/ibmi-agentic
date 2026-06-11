     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3011R - AGENT COMMISSION MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM CALCULATES THE AMOUNT OF COMMISSION EACH AGENT   *
     H* HAS EARNED DURING THE ENTERED DATE RANGE.  EARNED COMMISSION  *
     H* IS DETERMINED BY MULTIPLYING THE AGT COMM RATE BY THE AMOUNT  *
     H* OF PAYMENTS RECEIVED FROM WRITTEN ACCOUNTS.                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   GENERATE AGT COMMISSION MAINT W3010C                        *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: JACKIE JANOWIAK                                   *
     H* CREATED:    10/07/88                                          *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   60  GLOBAL FILE CHAIN                                       *
     H*   50  AGT HISTORY FILE CHAIN                                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  NOTE: IF THIS PROGRAM IS BEING LOADED BEFORE V8R1 CD RELEASE *
     H*        THEN ENTIRE 132WKE PROJECT SHOULD BE LOADED!           *
     H*                                                               *
     H* 01/07/21  C:33  Project: 129WWK                               *
     H*                 Translate client codes.                       *
     H*                                                               *
     H* 10/09/15 C:33  37761                                          *
     H*               -Account for 'D' plan type for 129.             *
     H*                                                               *
     H* 06/08/15 C:33  Project: 114WPE  Priss #21                     *
     H*               -Clear the written premium arrays.              *
     H*               -Flip the check on the exp const flag.          *
     H*               -Clear exp const value before reading.          *
     H*                                                               *
     H* 10/14/14 C:33  Project: 114WPE                                *
     H*                Calculate commissions based on written premium *
     H*                 for selected plans.                           *
     H*                                                               *
     H* 02/29/12 C:136 SUPPORT: WA046 79232                           *
     H*                PULL RATES BY AGT HIST EFF DATE, NOT THE       *
     H*                PROMPT FROM DATE                               *
     H*                                                               *
     H* 12/06/10 C:136 SUPPORT: WA139 75093                           *
     H*                EXPAND SIZE OF ARRAYS                          *
     H*                                                               *
     H*  03/01/10  C:246  PROJECT 139WNQ - PRISS 11                   *
     H*                   Fix the issue with commission process.      *
     H*                                                               *
     H* 01/22/10 C:246 Project: 139WNQ. Add Tertary Agent for comm    *
     H*                processing.                                    *
     H* 11/13/08 C:132 Project: 71WHI  WA071/64555                    *
     H*                Add decimal positions TA2.                     *
     H*                                                               *
     H*  06/02/08 C:191 PROJECT: 132WKE                               *
     H*                 RE-COMPILE ONLY: NEW FIELDS IN WMACP WERE     *
     H*                 GETTING JUNK VALUES                           *
     H*                                                               *
     H* 11/13/07 C:33  Project: WC695   WA093/60835                   *
     H*                Update new vs renew calc                       *
     H*                                                               *
     H* 02/02/07 C:33  SEQ#: WA091/57013                              *
     H*                Expand arrays.                                 *
     H*                                                               *
     H* 10/20/06 C:33  Project: 71WHI   WA071/55406                   *
     H*                Load array appropriately.                      *
     H*                                                               *
     H* 09/08/06 C:132 Project: 71WHI                                 *
     H*                Add base to base code for following codes.     *
     H*                - Retro Credit Adjustement A/R Code - RC       *
     H*                - Audit Transfer credit A/R Code - TA          *
     H*                - Add check for Client 071 for above credits   *
     H*                - Add WDF1L & WMACL12 files and Key lists      *
     H*                                                               *
     H* 03/02/06 C:33  Project: 139WCE                                *
     H*                Implement "Split" plan.                        *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*  07/18/03 C:132 PROJECT: WC577                                *
     H*                Add Agent FEIN and MOD to WMACP file           *
     H*                                                               *
     H* 11/26/01 C:09 SEQ#: WA102/29711                               *
     H*               WHEN WRITING THE AGENT COMMISSION EARNED RECORD,*
     H*               MAKE SURE THE PRODUCER # CORRESPONDS TO THE     *
     H*               AGENT # BEING WRITTEN (I.E. AG,X - PR,X).       *
     H*                                                               *
     H* 08/10/00 C:33 SEQ#: WA109/23913                               *
     H*               CHECK FOR INACTIVE RATE CODES.                  *
     H*                                                               *
     H* 12/15/99 C:90 PROJECT: WC577                                  *
     H*               Y2K FIX - LOAD INITIAL MAINTENANCE FIELDS       *
     H*                FOR DATE, TIME, PROGRAM, AND USER.             *
     H*                                                               *
     H*  11/04/99 C:33 SUPPORT: WA048/20525                           *
     H*                CHANGED SIZE OF 'HIVAL' TO MATCH 'YD'          *
     H*                                                               *
     H*  07/19/99  C:33  PROJECT: WC899                               *
     H*                  DO NOT PROCESS ACCOUNT CURRENT ACCOUNTS      *
     H*                                                               *
     H* 06/24/99 C:33 PROJECT: WC695   SEQ# WA091/18569               *
     H*               NEW RATE CALC FOR RENEW VS. NEW                 *
     H*                                                               *
     H*  11/17/98 C:33 SUPPORT: WA095/12630                           *
     H*                LOAD SECONDARY AGENT FLAG                      *
     H*                                                               *
     H* 11/04/98 C:33 PROJECT: WC632                                  *
     H*               LOAD CORRECT YEAR FOR SECONDARY AGENT COMMISSION*
     H*                                                               *
     H* 06/07/98 C:33 PROJECT: WC695                                  *
     H*               NEW RATE CALC FOR RENEW VS. NEW                 *
     H*                                                               *
     H* 04/06/98 C:65 PROJECT: CM038                                  *
     H*               FYR IS NO LONGER BEING PASSED IN BECAUSE OF     *
     H*               COMMERCIAL PROCESSING.                          *
     H*                                                               *
     H* 12/17/97 C:33 PROJECT: WC632                                  *
     H*               ALLOW UP TO TWO AGENTS, PRIMARY & SECONDARY, TO *
     H*               RECEIVE COMMISSIONS AT THE SAME TIME.           *
     H*                                                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     FSMCOL     UF   E           K DISK
     FWMEAL5    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMARL     IF   E           K DISK
     FWMACL1    UF A E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PVS01)
     F*
     FWDELL2    IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDEHL6    if   e           k disk    rename(peh01:peh06)
     F*
     FWMAAL1    IF   E           K DISK
     F*
     FWMACL12   UF   E           K DISK
     F                                     RENAME(PAC01:PAC12)
     FWDF1L     IF   E           K DISK

     D MAX             C                   CONST(9999)
     D YD              S             10  0 DIM(max) ASCEND                      YEAR & DATE
     D AG              S              5  0 DIM(max) ASCEND                      AGENT
     D COL             S             11  2 DIM(max) ASCEND                      COLLECTIONS
     D FW              S              1    DIM(max) ASCEND                      FORCE WRITE
     D RT              S              5  0 DIM(max) ASCEND                      AGT RATE#
     D RAT             S              5  2 DIM(max) ASCEND                      RATE %
     D PR              S              5  0 DIM(max) ASCEND                      PRODUCER
     D PL              S              1    DIM(max)                             PLAN TYPE
     D COL2            S             11  2 DIM(max) ASCEND                      RENEW COLLECTIONS
     D RAT2            S              5  2 DIM(max) ASCEND                      RENEW RATE %
     D AG2             S              5  0 DIM(max) ASCEND                      SECONDARY AGENT
     D RT2             S              5  0 DIM(max) ASCEND                      2ND AGT RATE#
     D R2N             S              5  2 DIM(max) ASCEND                      2ND AGT NEW %
     D R2R             S              5  2 DIM(max) ASCEND                      2ND AGT RENEW %
     D PR2             S              5  0 DIM(max) ASCEND                      2ND AGT PRODUCER
     D PL2             S              1    DIM(max)                             2ND AGT PLAN TYPE
     D COL3            S             11  2 DIM(max) ASCEND                      RENEW COLLECTIONS
     D RAT3            S              5  2 DIM(max) ASCEND                      RENEW RATE %
     D AG3             S              5  0 DIM(max) ASCEND                      Tertiary  AGENT
     D RT3             S              5  0 DIM(max) ASCEND                      3rd AGT RATE#
     D R3N             S              5  2 DIM(max) ASCEND                      3rd AGT NEW %
     D R3R             S              5  2 DIM(max) ASCEND                      3rd AGT RENEW %
     D PR3             S              5  0 DIM(max) ASCEND                      3rd AGT PRODUCER
     D PL3             S              1    DIM(max)                             3rd AGT PLAN TYPE
     D RC3             S             11  2 DIM(max) ASCEND                      RENEW RETRO CRDTS
     D TA3             S             11  2 DIM(max) ASCEND                      RENEWE AUD TRF CR
     D RC              S             11  2 DIM(max) ASCEND                      RETRO CREDITS
     D RC2             S             11  2 DIM(max) ASCEND                      RENEW RETRO CRDTS
     D TA              S             11  2 DIM(max) ASCEND                      AUDIT TRNSFR CRDT
     D TA2             S             11  2 DIM(max) ASCEND                      RENEWE AUD TRF CR
     D prem            s             11  2 dim(max)
     D prem2           s             11  2 dim(max)
     D prem3           s             11  2 dim(max)
     D exp             s              1    dim(max)
     D exp2            s              1    dim(max)
     D exp3            s              1    dim(max)
     D decCurrPremW    s                   like(EHwApr)
     D decCurrPremWo   s                   like(EHwApr)
     D decPremW        s                   like(EHwNmP)
     D decPremWo       s                   like(EHwApr)
     D decPriorPremW   s                   like(EHwApr)
     D decPriorPremWo  s                   like(EHwApr)
     D intHoldYear     s                   like(EHfyr)
     D isRenewal       s              1n
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EMPLYR EFF DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)                               EMPLYR CANC DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXDATE                 1      8  0
     D  XXMMDD                 1      4  0
     D  XXYR                   5      8  0
     D                 DS
     D  X1DATE                 1      8  0
     D  X1MMDD                 1      4  0
     D  X1YR                   5      8  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D                 DS
     D  XXTOD                  1      7  0
     D  XXTDYR                 1      3  0
     D  XXTDMO                 4      5  0
     D                 DS
     D  XXYRDT                 1     10  0
     D  EAFYR                  1      3  0
     D  EATRDT                 4     10  0
     D                 DS
     D dsYearDate              1     10  0
     D  dsYear                 1      3  0
     D  dsDateCYMD             4     10  0
     D                 DS
     D  XXEFDT                 1      7  0
     D  XXEDYR                 1      3  0
     D  XXEDMO                 4      5  0
     D*
     D*  RENEWAL DATE
     D*
     D                 DS
     D  XXRWDT                 1      7  0
     D  XXRWYR                 1      3  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     DXLateARplan      pr                  like(ARplan)
     D inPlan                              like(ARplan)

     C*
     C     *LIKE         DEFINE    AHEMP#        XXEMP#
     C     *LIKE         DEFINE    AHDIV         XXDIV
     C     *LIKE         DEFINE    AHEFDT        XXFRDT
     C     *LIKE         DEFINE    AHEFDT        XXTODT
     C     *LIKE         DEFINE    AHEFDT        XXCEFD
     C     *LIKE         DEFINE    AHRT#         XXRT#
     C     *LIKE         DEFINE    CSKEY         XXKEY
      *
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    YD            HIVAL
     C*
     C     *LIKE         DEFINE    CSTYPE        XXRCTP
     C     *LIKE         DEFINE    CSDESC        XXRCDS
     C     *LIKE         DEFINE    CSTYPE        XXTATP
     C     *LIKE         DEFINE    CSDESC        XXTADS
     C     *LIKE         DEFINE    F1RCCD        XXRCCD
     C     *LIKE         DEFINE    F1TACD        XXTACD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCODE            2
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
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C                   MOVE      QQTODT        X1TODT            8 0
     C                   MOVE      QQCODE        XXCODE            2
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C*  KEY List for the Agent Assignment file
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHAGT#
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXTODT
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE AGENT COMMISSIONS FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFRDT
     C*
     C     ACKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE AGENT HISTORY MASTER FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     AHKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE AGENT RATE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRT#
     C                   KFLD                    KKEFDT            7 0
     C*
     C     ARKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRT#
     C*
     C*  KEY LIST FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRDT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE COMMISSION CODE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  Key List for the Commission Code file
     C*
     C     ACKY12        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXTODT
     C*
     C*  Key List for the Group Detail file
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*_______________________________________________________________
     C*
     C                   Z-ADD     *HIVAL        HIVAL
     C*
      *==============================================================
      * RTVCLTID - Retrieve Client Number from Data Area
      *==============================================================
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *==============================================================
     C*
     C*  CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*  REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Y                 2 0
     C     *BLANK        LOOKUP    CONM(Y)                            60
     C                   MOVEA     CONM(Y)       XXCONM           40
     C*
     C*  CHAIN TO THE FUND FILE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C*  CHAIN TO THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C*  DELETE RECORDS WITH "C" TYPE WHICH FALL IN DATE RANGE
     C*
     C     ACKY          SETLL     PAC01
     C     ACKY2         READE     PAC01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     ACCOTP        IFEQ      'C'
     C     ACPODT        ANDLE     XXTODT
     C     ACPODT        ANDGE     XXFRDT
     C                   DELETE    PAC01
     C                   END
     C     ACKY2         READE     PAC01                                  60
     C                   ENDDO
     C*
     C                   CLEAR                   XXRCCD
     C                   CLEAR                   XXTACD
     C*
     C     CLTID         IFEQ      '071'
     C*
     C*  Get Retro Credit Code & Audit Credit Code from Group Auxiliary
     C*
     C     F1KY          SETLL     PF101
     C     F1KY          READE     PF101                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XXTODT        IFLT      F1ENPO
     C     XXTODT        ANDGT     F1STPO
     C                   MOVEL     F1RCCD        XXRCCD
     C                   MOVEL     F1TACD        XXTACD
     C                   ENDIF                                                  F1ENPO < XXTODT
     C     F1KY          READE     PF101                                  60
     C                   ENDDO
     C*
     C*  DELETE RETRO-COMMISSION ADJUSTMENTS WITH SAME ENDING DATE
     C*
     C     ACKY12        CHAIN     PAC12                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     ACCOCD        IFEQ      XXRCCD
     C                   DELETE    PAC12
     C                   ENDIF                                                  ACCOCD=XXRCCD
      *
     C*  DELETE Audit Transfer Credit Code  WITH SAME ENDING DATE
     C*
     C     ACCOCD        IFEQ      XXTACD
     C                   DELETE    PAC12
     C                   ENDIF                                                  ACCOCD=XXRCCD
     C*
     C     ACKY12        READE     PAC12                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CLTID = 071
     C*
     C                   CLEAR                   CSTYPE
     C                   CLEAR                   CSDESC
     C*
     C                   MOVEL(P)  XXRCCD        XXKEY
     C     TBKY          CHAIN     PCS01                              60
     C                   MOVEL(P)  CSTYPE        XXRCTP
     C                   MOVEL(P)  CSDESC        XXRCDS
     C                   CLEAR                   CSTYPE
     C                   CLEAR                   CSDESC
     C*
     C                   MOVEL     XXTACD        XXKEY
     C     TBKY          CHAIN     PCS01                              60
     C                   MOVEL(P)  CSTYPE        XXTATP
     C                   MOVEL(P)  CSDESC        XXTADS
     C*
     C*  CHAIN TO THE COMMISSION CODE FILE
     C*
     C                   MOVEL(P)  XXCODE        XXKEY
     C*
     C     TBKY          CHAIN     PCS01                              60
     C*
     C*  START READING THE EMPLOYER AGENT HISTORY FILE
     C*
     C     AHKY          CHAIN     PAH01                              50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  RETRIEVE ALL ACTIVE IN EMPLOYER'S HISTORY FOR AGENTS
     C*
     C                   Z-ADD     AHEMP#        XXEMP#
     C                   Z-ADD     AHDIV         XXDIV
      *
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXFEIN
     C                   Z-ADD     *ZEROS        XXMOD
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN55
     C                   MOVE      *ZERO         YD
     C                   MOVE      *ZERO         COL
     C                   MOVE      *ZERO         AG
     C                   MOVE      *ZEROS        PR
     C                   MOVE      *BLANKS       FW
     C                   MOVE      *ZEROS        RT
     C                   MOVE      *ZEROS        RAT
     C                   Z-ADD     *ZEROS        COL2
     C                   Z-ADD     *ZEROS        RAT2
     C                   Z-ADD     *ZEROS        COL3
     C                   Z-ADD     *ZEROS        RAT3
     C                   Clear                   prem
     C                   Clear                   prem2
     C                   Clear                   prem3
     C                   Clear                   exp
     C                   Clear                   exp2
     C                   Clear                   exp3
     C                   CLEAR                   AG2
     C                   CLEAR                   RT2
     C                   CLEAR                   R2N
     C                   CLEAR                   R2R
     C                   CLEAR                   PR2
     C                   CLEAR                   AG3
     C                   CLEAR                   RT3
     C                   CLEAR                   R3N
     C                   CLEAR                   R3R
     C                   CLEAR                   PR3
     C                   CLEAR                   RC3
     C                   CLEAR                   TA3
     C                   CLEAR                   RC
     C                   CLEAR                   RC2
     C                   CLEAR                   TA
     C                   CLEAR                   TA2
     C                   Z-ADD     1             X                 5 0
     C*
     C     *IN55         DOWEQ     '0'
     C     AHSTAT        IFNE      'I'
     C                   MOVEL     AHFYR         YD(X)
     C                   Z-ADD     AHAGT#        AG(X)
     C                   Z-ADD     AHPROD        PR(X)
     C                   MOVE      AHEFDT        YD(X)
     C                   Z-ADD     AHRT#         RT(X)
     C                   Z-ADD     AHSAGT        AG2(X)                         SECONDARY AGENT
     C                   Z-ADD     AHSRT#        RT2(X)                         2ND AGT RATE#
     C                   Z-ADD     AHSPRD        PR2(X)                         2ND AGT PRODUCER
     C                   Z-ADD     AH3AGT        AG3(X)                         Tertiary  AGENT
     C                   Z-ADD     AH3RT#        RT3(X)                         3rd AGT RATE#
     C                   Z-ADD     AH3PRD        PR3(X)                         3rd AGT PRODUCER
     C                   ADD       1             X
     C                   END
     C*
     C     AHKY3         READE     PAH01                                  55
     C                   END
     C*
     C*  LOAD REMAINDER OF ARRAY
     C*
     C     X             DOWLE     MAX
     C                   Z-ADD     HIVAL         YD(X)
     C                   ADD       1             X                              |
     C                   END                                                    |
     C*
     C*  RETRIEVE THE EMPLOYER RECORD TO DETERMINE LENGTH OF ENROLLMENT
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     XXTODT        XXTOD
     C*  GET EMPLOYER'S MOST CURRENT EFFECTIVE DATE
     C*
     C                   Z-ADD     *ZERO         XXCEFD
     C     1             DO        5             P                 1 0
     C     EF(P)         IFNE      *ZERO
     C                   Z-ADD     EF(P)         XXCEFD
     C                   END
     C                   END                                                    DO 1-5
     C                   Z-ADD     XXCEFD        XXRWDT
     C                   ADD       1             XXRWYR
     C*
     C*  RETRIEVE THE RATE.
     C*  IF EMPLOYER ENROLLED 1 YEAR OR MORE, USE RENEW RATE.
     C*  OTHERWISE, USE NEW RATE.  USE EARLIEST EFFECTIVE DATE.
     C*  (IGNORE LAPSES IN COVERAGE.)
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     MAX
     C     YD(X)         ANDNE     HIVAL
     C*
     C     RT(X)         IFNE      0
     C*
     C                   Z-ADD     RT(X)         XXRT#
     C                   MOVE      YD(X)         KKEFDT
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*            DON'T CHECK FOR RENEWAL HERE; JUST LOAD NEW% AND
     C*            RENEW%.  RENEWAL HAS ALREADY BEEN DETERMINED WHEN
     C*            CALCULATING COLLECTIONS.
     C*
     C                   Z-ADD     ARNWRT        RAT(X)                         LOAD NEW RATE%
     C                   Z-ADD     ARRNRT        RAT2(X)                        LOAD RENEW%
     C                   eval      pl(x) = XLateARplan(ARplan)
     C                   eval      exp(x) = ARexpConst
     C*
     C                   ENDIF                                                  *IN60
     C*
     C                   ENDIF                                                  RT,X NE 0
     C*
     C*   RETRIEVE SECONDARY AGENT'S RATE
     C*
     C     RT2(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     RT2(X)        XXRT#
     C                   MOVE      YD(X)         KKEFDT
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ARNWRT        R2N(X)                         2ND AGENT'S NEW%
     C                   Z-ADD     ARRNRT        R2R(X)                         2ND AGENT'S RENEW%
     C                   eval      pl2(x) = XLateARplan(ARplan)
     C                   eval      exp2(x) = ARexpConst
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  RT2,X<>*ZERO
     C*
     C     PL(X)         IFEQ      'S'
     C                   SUB       R2N(X)        RAT(X)
     C                   SUB       R2R(X)        RAT2(X)
     C                   MOVE      AG(X)         AG2(X)
     C                   MOVE      PR(X)         PR2(X)
     C                   CLEAR                   PR(X)
     C                   ENDIF
     C*   RETRIEVE Tertiary  AGENT'S RATE
     C     RT3(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     RT3(X)        XXRT#
     C                   MOVE      YD(X)         KKEFDT
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ARNWRT        R3N(X)                         3rd AGENT'S NEW%
     C                   Z-ADD     ARRNRT        R3R(X)                         3rd AGENT'S RENEW%
     C                   eval      pl3(x) = XLateARplan(ARplan)
     C                   eval      exp3(x) = ARexpConst
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  RT3,X<>*ZERO
     C     PL(X)         IFEQ      'S'
     C                   SUB       R3N(X)        RAT(X)
     C                   SUB       R3R(X)        RAT3(X)
     C                   MOVE      AG(X)         AG3(X)
     C                   MOVE      PR(X)         PR3(X)
     C                   CLEAR                   PR(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C*
     C                   END                                                    X LT 201
     C*
     C                   Exsr      CalcCash
     C                   Exsr      CalcW
     C*
     C*  CHECK TO SEE IF WE NEED TO FORCE THE WRITE OF A RECORD
     C*  IF NO COLLECTIONS
     C*
     C                   XFOOT     COL           XXCOL            11 2          RLS 2
     C                   XFOOT     COL2          XXC2             11 2
     C                   Xfoot     prem          xxPrem           11 2
     C                   Xfoot     prem2         xxPrem2          11 2
     C                   Xfoot     prem3         xxPrem3          11 2
     C                   ADD       XXC2          XXCOL
     C*                                                    |
      /Free
       If (xxCol + xxC2 + xxPrem + xxPrem2 + xxPrem3) = 0;
      /End-free
     C     ELKY          SETGT     PEL01
     C     EMKY          READPE    PEL01                                  60
     C     *IN60         IFEQ      '0'
     C     ELPTDT        ANDGE     XXFRDT
     C                   Z-ADD     1             X
     C                   Z-ADD     ELFYR         EAFYR
     C                   Z-ADD     XXTODT        EATRDT
     C     XXYRDT        LOOKUP    YD(X)                                6060    |
     C     *IN60         IFEQ      '1'                                          |
     C                   MOVE      'Y'           FW(X)                          |
     C                   END                                                    |
     C                   END                                                    |
     C                   END                                                    |
     C*
     C*  WRITE RECORD(S) TO FILE
     C*
     C                   Z-ADD     1             X
     C     YD(X)         DOWNE     HIVAL
     C     X             ANDLE     MAX
     C*
      /Free
       // Ensure values from prior written records are not carried forward
       Clear pac01;

       // Determine Renewal or New Business
       dsYearDate = yd(x);
       Clear ELpfDt;
       Chain elKy1 pel02;
       Select;
       When xxRwDt <> ELptDt and ELptDt = ELpfDt;
          isRenewal = *on;
       When xxRwDt >= ELptDt;
          isRenewal = *off;
       When xxRwDt <= ELpfDt;
          isRenewal = *on;
       When dsDateCYMD >= xxRwDt;
          isRenewal = *on;
       Other;
          isRenewal = *off;
       Endsl;
      /End-free
     C*
     C                   SELECT
     C*
     C     PL(X)         WHENEQ    'A'
     C                   EXSR      WRITEA
     C*
     C                   OTHER
     C*
     C     CLTID         IFEQ      '071'
     C*
     C                   Z-ADD     AHCO#         ACCO#
     C                   Z-ADD     AHFND         ACFND
     C                   MOVEL     YD(X)         ACFYR
     C                   Z-ADD     AHEMP#        ACEMP#
     C                   Z-ADD     AHDIV         ACDIV
     C                   Z-ADD     AG(X)         ACAGT#
     C                   Z-ADD     PR(X)         ACPROD
     C                   Z-ADD     XXTODT        ACPODT
     C                   Z-ADD     XXTODT        ACTRDT
     C                   Z-ADD     *ZERO         ACCHCK
     C                   MOVE      XXCODE        ACCOCD
     C                   MOVE      CSTYPE        ACCOTP
     C                   MOVE      CSDESC        ACDESC
     C                   MOVE      *BLANKS       ACAGT2
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVEL(P)  Q1USER        ACUSER
     C                   MOVEL(P)  Q1PGM         ACPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        ACTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     DSTIME        ACITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACIDT
     C                   MOVE      Q1USER        ACIUSR
     C*
     C                   Z-ADD     ACFYR         XXFYR
     C     ELKY1         CHAIN     PEL02                              60
     C*
     C                   ENDIF
     C*
      /Free
       If (pl(x) = *blank and col(x) <> *zero)
        or (pl(x) = 'W' and prem(x) <> *zero and not isRenewal)
        or fw(x) <> *blank;
      /End-free
     C                   Z-ADD     AHCO#         ACCO#
     C                   Z-ADD     AHFND         ACFND
     C                   MOVEL     YD(X)         ACFYR                          RLS 2
     C                   Z-ADD     AHEMP#        ACEMP#
     C                   Z-ADD     AHDIV         ACDIV
     C                   Z-ADD     AG(X)         ACAGT#                         RLS 2
     C                   eval      ACplan = pl(x)
     C                   Z-ADD     PR(X)         ACPROD
     C                   Z-ADD     XXTODT        ACPODT
     C                   Z-ADD     XXTODT        ACTRDT
      /Free
          Clear ACtoCl;
          Select;
          When pl(x) = *blank;
             ACtoCl = col(x);
          When pl(x) = 'W';
             ACtoCl = prem(x);
          Endsl;
      /End-free
     C                   Z-ADD     RAT(X)        ACRATE
     C*
     C*   IF FORCE WRITE, USE RENEW RATE IF ACCT'S BEEN ACTIVE OVER
     C*   A YEAR AS OF THE TO-DATE.
     C*
     C     FW(X)         IFNE      *BLANK
     C     XXTODT        ANDGE     XXRWDT
     C                   Z-ADD     RAT2(X)       ACRATE
     C                   END
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C                   Z-ADD     0             ACCHCK
     C                   MOVE      XXCODE        ACCOCD
     C                   MOVE      CSTYPE        ACCOTP
     C                   MOVE      CSDESC        ACDESC
     C                   MOVE      *BLANK        ACAGT2
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVE      Q1USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ACTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     DSTIME        ACITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACIDT
     C                   MOVE      Q1USER        ACIUSR
      *
     C                   Z-ADD     XXFEIN        ACFEIN
     C                   Z-ADD     XXMOD         ACMOD
     C*
     C     COKY          CHAIN     PCO01                              60        JJJ 10-7-88
     C                   ADD       1             COBUL2                         |
     C                   Z-ADD     COBUL2        ACBUL2                         |
     C                   UPDATE    PCO01                                        |
     C                   WRITE     PAC01
     C                   END
     C*
     C*  WRITE ANOTHER COMMISSION RECORD FOR RENEWAL RATE
     C*
      /Free
       If (pl(x) = *blank and col2(x) <> *zero)
        or (pl(x) = 'W' and prem(x) <> *zero and isRenewal);
      /End-free
     C                   Z-ADD     AHCO#         ACCO#
     C                   Z-ADD     AHFND         ACFND
     C                   MOVEL     YD(X)         ACFYR
     C                   Z-ADD     AHEMP#        ACEMP#
     C                   Z-ADD     AHDIV         ACDIV
     C                   Z-ADD     AG(X)         ACAGT#
     C                   eval      ACplan = pl(x)
     C                   Z-ADD     PR(X)         ACPROD
     C                   Z-ADD     XXTODT        ACPODT
     C                   Z-ADD     XXTODT        ACTRDT
      /Free
          Clear ACtoCl;
          Select;
          When pl(x) = *blank;
             ACtoCl = col2(x);
          When pl(x) = 'W';
             ACtoCl = prem(x);
          Endsl;
      /End-free
     C                   Z-ADD     RAT2(X)       ACRATE
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C                   Z-ADD     0             ACCHCK
     C                   MOVE      XXCODE        ACCOCD
     C                   MOVE      CSTYPE        ACCOTP
     C                   MOVE      CSDESC        ACDESC
     C                   MOVE      *BLANK        ACAGT2
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVE      Q1USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ACTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     DSTIME        ACITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACIDT
     C                   MOVE      Q1USER        ACIUSR
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C                   UPDATE    PCO01
     C                   WRITE     PAC01
     C                   END
     C*
     C     CLTID         IFEQ      '071'
     C*
     C*  RETRO-CREDIT COMMISSIONS (NEW)
     C*
     C     RC(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     RC(X)         ACTOCL
     C                   Z-ADD     RAT(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXRCCD        ACCOCD
     C                   MOVEL(P)  XXRCTP        ACCOTP
     C                   MOVEL(P)  XXRCDS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC,X<>*ZERO
     C*
     C*  RETRO-CREDIT COMMISSIONS (RE-NEW)
     C*
     C     RC2(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     RC2(X)        ACTOCL
     C                   Z-ADD     RAT2(X)       ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXRCCD        ACCOCD
     C                   MOVEL(P)  XXRCTP        ACCOTP
     C                   MOVEL(P)  XXRCDS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC2,X<>*ZERO
     C*
     C*  AUDIT TRANSFER CREDIT COMMISSIONS (NEW)
     C*
     C     TA(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     TA(X)         ACTOCL
     C                   Z-ADD     RAT(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXTACD        ACCOCD
     C                   MOVEL(P)  XXTATP        ACCOTP
     C                   MOVEL(P)  XXTADS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC,X<>*ZERO
     C*
     C*  AUDIT TRANSFER CREDIT COMMISSIONS (RE-NEW)
     C*
     C     TA2(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     TA2(X)        ACTOCL
     C                   Z-ADD     RAT2(X)       ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXTACD        ACCOCD
     C                   MOVEL(P)  XXTATP        ACCOTP
     C                   MOVEL(P)  XXTADS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC2,X<>*ZERO
     C*
     C                   ENDIF                                                  CLTID = 071
     C*
     C                   ENDSL
     C*
     C*   IF THERE'S A SECONDARY AGENT, WRITE SEPARATE RECORDS FOR THEM
     C*
     C     AG2(X)        IFNE      *ZERO
     C*
     C                   SELECT
     C*
     C     PL2(X)        WHENEQ    'A'
     C                   EXSR      WRITEA
     C*
     C                   OTHER
     C*
     C                   Z-ADD     AHCO#         ACCO#
     C                   Z-ADD     AHFND         ACFND
     C                   MOVEL     YD(X)         ACFYR
     C                   Z-ADD     AHEMP#        ACEMP#
     C                   Z-ADD     AHDIV         ACDIV
     C                   Z-ADD     AG2(X)        ACAGT#
     C                   eval      ACplan = pl2(x)
     C                   Z-ADD     PR2(X)        ACPROD
     C                   Z-ADD     XXTODT        ACPODT
     C                   Z-ADD     XXTODT        ACTRDT
     C                   Z-ADD     *ZERO         ACCHCK
     C                   MOVE      XXCODE        ACCOCD
     C                   MOVE      CSTYPE        ACCOTP
     C                   MOVE      CSDESC        ACDESC
     C                   MOVE      'Y'           ACAGT2
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVEL(P)  Q1USER        ACUSER
     C                   MOVEL(P)  Q1PGM         ACPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        ACTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     DSTIME        ACITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACIDT
     C                   MOVE      Q1USER        ACIUSR
     C*
     C*  WRITE A RECORD FOR THE NEW RATE, FOR SECONDARY AGENT
     C*
      /Free
       If (pl2(x) = *blank and col(x) <> *zero)
        or (pl2(x) = 'W' and prem2(x) <> *zero and not isRenewal)
        or fw(x) <> *blank;

          Clear ACtoCl;
          Select;
          When pl2(x) = *blank;
             ACtoCl = col(x);
          When pl2(x) = 'W';
             ACtoCl = prem2(x);
          Endsl;
      /End-free
     C                   Z-ADD     R2N(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  COL,X<>*ZERO
     C*
     C*  WRITE ANOTHER RECORD FOR RENEWAL RATE, FOR SECONDARY AGENT
     C*
      /Free
       If (pl2(x) = *blank and col2(x) <> *zero)
        or (pl2(x) = 'W' and prem2(x) <> *zero and isRenewal);

          Clear ACtoCl;
          Select;
          When pl2(x) = *blank;
             ACtoCl = col2(x);
          When pl2(x) = 'W';
             ACtoCl = prem2(x);
          Endsl;
      /End-free
     C                   Z-ADD     R2R(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  COL2,X<>*ZERO
     C*
     C     CLTID         IFEQ      '071'
     C*
     C*  RETRO-CREDIT COMMISSIONS (NEW)
     C*
     C     RC(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     RC(X)         ACTOCL
     C                   Z-ADD     R2N(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXRCCD        ACCOCD
     C                   MOVEL(P)  XXRCTP        ACCOTP
     C                   MOVEL(P)  XXRCDS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC,X<>*ZERO
     C*
     C*  RETRO-CREDIT COMMISSIONS (RE-NEW)
     C*
     C     RC2(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     RC2(X)        ACTOCL
     C                   Z-ADD     R2R(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXRCCD        ACCOCD
     C                   MOVEL(P)  XXRCTP        ACCOTP
     C                   MOVEL(P)  XXRCDS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC2,X<>*ZERO
     C*
     C*  AUDIT TRANSFER CREDIT COMMISSIONS (NEW)
     C*
     C     TA(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     TA(X)         ACTOCL
     C                   Z-ADD     R2N(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXTACD        ACCOCD
     C                   MOVEL(P)  XXTATP        ACCOTP
     C                   MOVEL(P)  XXTADS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC,X<>*ZERO
     C*
     C*  AUDIT TRANSFER CREDIT COMMISSIONS (RE-NEW)
     C*
     C     TA2(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     TA2(X)        ACTOCL
     C                   Z-ADD     R2R(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXTACD        ACCOCD
     C                   MOVEL(P)  XXTATP        ACCOTP
     C                   MOVEL(P)  XXTADS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC2,X<>*ZERO
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  AG2,X<>*ZERO
     C*
     C*   IF THERE'S A Tertiary AGENT, WRITE SEPARATE RECORDS FOR THEM
     C*
     C     AG3(X)        IFNE      *ZERO
     C*
     C                   SELECT
     C*
     C     PL3(X)        WHENEQ    'A'
     C                   EXSR      WRITEA
     C*
     C                   OTHER
     C*
     C                   Z-ADD     AHCO#         ACCO#
     C                   Z-ADD     AHFND         ACFND
     C                   MOVEL     YD(X)         ACFYR
     C                   Z-ADD     AHEMP#        ACEMP#
     C                   Z-ADD     AHDIV         ACDIV
     C                   Z-ADD     AG3(X)        ACAGT#
     C                   eval      ACplan = pl3(x)
     C                   Z-ADD     PR3(X)        ACPROD
     C                   Z-ADD     XXTODT        ACPODT
     C                   Z-ADD     XXTODT        ACTRDT
     C                   Z-ADD     *ZERO         ACCHCK
     C                   MOVE      XXCODE        ACCOCD
     C                   MOVE      CSTYPE        ACCOTP
     C                   MOVE      CSDESC        ACDESC
     C                   MOVE      'Y'           ACAGT3
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVEL(P)  Q1USER        ACUSER
     C                   MOVEL(P)  Q1PGM         ACPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        ACTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     DSTIME        ACITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACIDT
     C                   MOVE      Q1USER        ACIUSR
     C*
     C*  WRITE A RECORD FOR THE NEW RATE, FOR Tertiary  AGENT
     C*
      /Free
       If (pl3(x) = *blank and col(x) <> *zero)
        or (pl3(x) = 'W' and prem3(x) <> *zero and not isRenewal)
        or fw(x) <> *blank;

          Clear ACtoCl;
          Select;
          When pl3(x) = *blank;
             ACtoCl = col(x);
          When pl3(x) = 'W';
             ACtoCl = prem3(x);
          Endsl;
      /End-free
     C                   Z-ADD     R3N(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  COL,X<>*ZERO
     C*
     C*  WRITE ANOTHER RECORD FOR RENEWAL RATE, FOR Tertiary AGENT
     C*
      /Free
       If (pl3(x) = *blank and col2(x) <> *zero)
        or (pl3(x) = 'W' and prem3(x) <> *zero and isRenewal);

          Clear ACtoCl;
          Select;
          When pl3(x) = *blank;
             ACtoCl = col2(x);
          When pl3(x) = 'W';
             ACtoCl = prem3(x);
          Endsl;
      /End-free
     C*
     C                   Z-ADD     COL3(X)       ACTOCL
     C                   Z-ADD     R3R(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  COL2,X<>*ZERO
     C*
     C*  RETRO-CREDIT COMMISSIONS (RE-NEW)
     C*
     C     RC3(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     RC3(X)        ACTOCL
     C                   Z-ADD     R3R(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXRCCD        ACCOCD
     C                   MOVEL(P)  XXRCTP        ACCOTP
     C                   MOVEL(P)  XXRCDS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC3,X<>*ZERO
     C*
     C*  AUDIT TRANSFER CREDIT COMMISSIONS (NEW)
     C*
     C     TA(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     TA(X)         ACTOCL
     C                   Z-ADD     R3N(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXTACD        ACCOCD
     C                   MOVEL(P)  XXTATP        ACCOTP
     C                   MOVEL(P)  XXTADS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC,X<>*ZERO
     C*
     C*  AUDIT TRANSFER CREDIT COMMISSIONS (RE-NEW)
     C*
     C     TA3(X)        IFNE      *ZERO
     C*
     C                   Z-ADD     TA3(X)        ACTOCL
     C                   Z-ADD     R3R(X)        ACRATE
     C                   Z-ADD     *ZERO         XXRAT1
     C*
     C     ACRATE        DIV       100           XXRAT1            5 4
     C     ACTOCL        MULT(H)   XXRAT1        ACAMT
     C*
     C                   MOVEL(P)  XXTACD        ACCOCD
     C                   MOVEL(P)  XXTATP        ACCOTP
     C                   MOVEL(P)  XXTADS        ACDESC
     C*
     C     ELPFDT        IFGT      XXTODT
     C                   Z-ADD     ELPFDT        ACPODT
     C                   ELSE
     C                   Z-ADD     XXTODT        ACPODT
     C                   ENDIF                                                  ELPFDT>ACPODT
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   ADD       1             COBUL2
     C                   Z-ADD     COBUL2        ACBUL2
     C*
     C                   UPDATE    PCO01
     C*
     C                   WRITE     PAC01
     C*
     C                   ENDIF                                                  RC3,X<>*ZERO
     C*
     C                   ENDSL
     C                   ENDIF                                                  AG3,X<>*ZERO
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   END
     C     AHKY3         SETGT     PAH01
     C     AHKY          READE     PAH01                                  50
     C                   END
     C*
     C*
     C                   MOVE      '1'           *INLR
     C*_______________________________________________________________
     C*
       //===================================================================
       // CalcCash - Calculate the A/R for cash commissions
       //===================================================================
     C     CalcCash      Begsr
     C*
     C*  RETRIEVE A/R RECORDS TO ACCUMULATE COLLECTIONS
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  51
     C*
     C     *IN51         DOWEQ     '0'
     C*
     C                   Z-ADD     EAFYR         XXFYR
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C*
     C*   CHAIN TO GET A/R TRANSACTION CODE EXCLUSION FLAG
     C*
     C                   MOVE      EACO#         XXCO#
     C                   MOVEL(P)  EAARCD        XXKEY
     C*
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     EATRDT        IFLE      XXTODT
     C     TTECFL        ANDNE     'Y'
     C*
     C                   MOVE      '0'           *IN20                          NEW BUSINESS
     C*
     C                   SELECT
     C     XXRWDT        WHENEQ    ELPTDT
     C     ELPTDT        ANDEQ     ELPFDT
     C                   MOVE      *ON           *IN20                          RENWAL
     C     XXRWDT        WHENGE    ELPTDT
     C                   MOVE      *OFF          *IN20                          NEW BUSINESS
     C     XXRWDT        WHENLE    ELPFDT
     C                   MOVE      *ON           *IN20                          RENWAL
     C                   OTHER
     C     EATRDT        COMP      XXRWDT                             20  20    RENEWAL
     C                   ENDSL
     C*
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'R'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'T'
     C     EAARCD        OREQ      XXRCCD
     C     EAARCD        OREQ      XXTACD
     C*
     C*  BUILD ARRAYS FOR FUND YEAR & COLLECTIONS
     C*
     C                   Z-ADD     1             X
     C     XXYRDT        LOOKUP    YD(X)                                6060
     C     *IN60         IFEQ      '1'
     C*
     C                   SELECT
     C     EAARCD        WHENEQ    XXRCCD
     C  N20              ADD       EAAMT         RC(X)
     C   20              ADD       EAAMT         RC2(X)
     C     EAARCD        WHENEQ    XXTACD
     C  N20              ADD       EAAMT         TA(X)
     C   20              ADD       EAAMT         TA2(X)
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'T'
     C  N20              ADD       EAAMT         COL(X)
     C   20              ADD       EAAMT         COL2(X)
     C     EAARTY        WHENEQ    'V'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'R'
     C  N20              SUB       EAAMT         COL(X)
     C   20              SUB       EAAMT         COL2(X)
     C                   ENDSL
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     EAKY2         READE     PEA01                                  51
     C                   END
     C*
     C                   Endsr
      /Free
       //===================================================================
       // CalcW - Calculate commissions for written premium
       //===================================================================
       Begsr CalcW;

        intHoldYear = *hival;

        For x = max by 1 downto 1;

           // Do not process records with *HIVAL
           If yd(x) = *hival;
              Iter;
           Endif;

           // Breach out year and date
           dsYearDate = yd(x);

           // Get the latest item prior to the 'To Date' for each year
           If intHoldYear <> dsYear
            and dsDateCYMD < xxToDt;

              intHoldYear = dsYear;

              // Get current written premium from premium history
              Clear EHwApr;
              Clear EHprtD;
              SetGt (xxCo#:xxFnd:xxEmp#:xxDiv:dsYear:xxToDt) WDEHL6;
              Readpe (xxCo#:xxFnd:xxEmp#:xxDiv:dsYear) WDEHL6;
              decCurrPremWo = EHwAPr - EHprtD;
              decCurrPremW  = EHwAPr;

              // Get prior written premium from premium history
              Clear EHwApr;
              Clear EHprtD;
              SetGt (xxCo#:xxFnd:xxEmp#:xxDiv:dsYear:xxFrDt) WDEHL6;
              Readpe (xxCo#:xxFnd:xxEmp#:xxDiv:dsYear) WDEHL6;
              decPriorPremWo = EHwAPr - EHprtD;
              decPriorPremW  = EHwAPr;

              // Calculate period commission premium
              decPremW  = decCurrPremW  - decPriorPremW;
              decPremWo = decCurrPremWo - decPriorPremWo;

              // Calculate premium for primary agent
              Select;
              When exp(x) <> 'Y';
                 prem(x) = decPremW;
              Other;
                 prem(x) = decPremWo;
              Endsl;

              // Calculate premium for secondary agent
              Select;
              When exp2(x) <> 'Y';
                 prem2(x) = decPremW;
              Other;
                 prem2(x) = decPremWo;
              Endsl;

              // Calculate premium for tertiary agent
              Select;
              When exp3(x) <> 'Y';
                 prem3(x) = decPremW;
              Other;
                 prem3(x) = decPremWo;
              Endsl;

           Endif;

        Endfor;

       Endsr;
      /End-free
     C*
     C*===============================================================
     C* WRITEA - NOT CURRENTLY USED IN THIS PROCESS
     C*===============================================================
     C*
     C     WRITEA        BEGSR
     C                   ENDSR                                                  WRITEA

     P*====================================================================
     P* Translate ARplan, as needed
     P*====================================================================
     PXLateARplan      b
     DXLateARplan      pi                  like(ARplan)
     D inPlan                              like(ARplan)

      /Free
       Select;
       //***  Begin Add  ***  01/07/21  ***********************************
       When cltID = '129' and inPlan = 'B';
          Return 'W';
       //***  End   Add  ***  01/07/21  ***********************************
       When cltID = '129' and inPlan = 'D';
          Return ' ';
       Other;
          Return inPlan;
       Endsl;
      /End-Free

     PXLateARplan      e
     P***  Begin Add  ***  10/09/15  **************************************
