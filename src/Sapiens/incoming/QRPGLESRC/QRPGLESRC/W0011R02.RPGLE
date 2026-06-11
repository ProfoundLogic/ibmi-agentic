     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0011R02 - GROUP DETAIL INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE GROUP DETAIL FILE.       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0011R - WORK WITH POLICY YEARS                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/16/96  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/14/24 C:062 Project: 146WKQ                                *
     H*                Add Assessments for S/R flag.                  *
     H*                                                               *
     H* 01/07/22 C:062 Project: WC2837                                *
     H*                Add Net Rate For SR Policies                   *
     H*                                                               *
     H* 06/08/18 C:136 Project: 150WAQ                                *
     H*                Add collateral A/R codes                       *
     H*                                                               *
     H* 09/12/17 C:65  Project: 150WAR                                *
     H*                Add percentage of manual premium entry for     *
     H*                group for client 150.                          *
     H*                                                               *
     H* 12/26/14 C:283 Support: WA045 87627                           *
     H*                Threshold Amount for Credit Adjustment F1cadj  *
     H*                should accommodate -ve values.                 *
     H*                                                               *
     H* 04/25/14 C:295 Project: 88WAX                                 *
     H*                MHCA Retrofit Custom Code to Base              *
     H*                                                               *
     H* 01/29/14 C:190 Project: 147WAI(SY336 Priss#35)                *
     H*                Display "Dividend Billed" only for CSI & Opta- *
     H*                Comp.                                          *
     H*                                                               *
     H* 05/27/13 C:190 Project: 147WAI                                *
     H*              - Add "Dividend Billed", "Dividend Paid" &       *
     H*                "Dividend Payment Transfer" to SCR02.          *
     H*                                                               *
     H* 03/10/11 C:33  Project: WC1851                                *
     H*              - Add "Stop collateral effective date" to SCR04. *
     H*                                                               *
     H* 03/11/09 C:61  Project: WC1630                                *
     H*              - Add "Deductible balance flag" prompt SCR03.    *
     H*                                                               *
     H* 01/08/09 C:65  Project: WC1769                                *
     H*              - Move new transfer fields to file to save.      *
     H*                                                               *
     H* 12/11/08 C:33  Project: WC1769                                *
     H*              - Convert to RPGIV (comments removed).           *
     H*              - Implement procedure for editing.               *
     H*              - Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 02/08/08 C:61  PROJECT 46WFQ                                  *
     H*                46WFL back to base corrections.                *
     H*
     H* 11/09/07 C:65  Project: 129WQM                                *
     H*                ADD NEW FLAG FOR CLIENT 129 FOR APPLYING 2007  *
     H*                NY RATE DECREASE.                              *
     H*
     H* 10/11/07 C:183 Project: 146WBC                                *
     H*                Added edits for the new field introduced in    *
     H*                WDF1P.                                         *
     H*
     H* 09/08/07 C:33  Project: WC1196  Priss# 26                     *
     H*                Audit Co. Assignment                           *
     H*                                                               *
     H* 07/19/07 C:035 PROJECT: 114WMF PRISS#48                       *
     H*                REMOVE CHANGES FOR 114WEA ON 08/28/06          *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Remove all processing related to project 45WFB *
     H*                                                              *
     H* 04/25/07 C:136 SUPPORT WA099: 56590                           *
     H*                REMOVE DEPOSIT TRANSFER CODES FROM SCR04.      *
     H*                DEPOSIT SHOULDN'T BE TRANSFERRED SINCE IT      *
     H*                MAY ACCUMULATE ACROSS YEARS.                   *
     H*                                                               *
      * 02/26/07 C:132 PROJECT: 90WEY (Back to Base)                  *
      *               -CLIENT SPECIFIC - Bill Expense Constant on all *
      *                                  Divisions (90WCG)            *
      *               -CLIENT SPECIFIC - Group Checks, add verbiage   *
      *                                  "Void After 90 days" (90WEM) *
      *                                                               *
      * 12/05/06 C:180 PROJECT:114WMF/45WFB
      *                For client 045, reorganize validation of
      *                INV#, Fee Billed, AND A/R code to avoid
      *                sending incorrect error message.
      *
      * 12/01/06 C:180 PROJECT:114WMF/45WFB
      *                        Validate Inv #, Fee Billed A/R code,
      *                        and fee amount of Screen 4 only for
      *                        client 045 since these fields are
      *                        applicable to client 045 only.
      *
      * 10/02/06 C:33  Project: WC1196                                *
      *                Audit Co. Assignment                           *
      *
      * 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE PHASE 1)          *
      *               -CLIENT SPECIFIC - POPULATE RETRO DEDUCTIBLE    *
      *                                  CODES                        *
      *                                                               *
      *
      * 08/28/06 C:180 PROJECT: 114WMF BACK TO BASE
      *                         114WEA SPREAD DISC ON CX POLICIES.
      *                add  prompt "Accumulate premium for all cx...."
      *                for client 114.
      *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY YEAR MODE FLAG                  *
     H*                                                               *
     H*
     H* 04/27/06 C:168 PROJECT: 45WFB   CERTIFICATION PRINT           *
     H*          C:33  ADDED CODE TO SCREEN 4 TO ALLOW USER INPUT     *
     H*                (CLIENT45 CONTROLLED)                          *
     H*                                                               *
     H* 04/26/06 C:132 PROJECT: 46WFL   S/R Discount A/R Code         *
     H*                ADD to screen two, S/R discount code (SCDSCD)  *
     H*                if is Client 046.                              *
     H*                                                               *
     H* 10/27/05 C:136 PROJECT: WC1191 (TRANSFER AUDIT CREDITS TO PREM)
     H*                NEED TO ADD FOURTH SCREEN FIELDS TO XFER       *
     H*                AUDIT CREDITS IN ALL CATEGORIES                *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109AX, 109CR}                                 *
     H*                Add Midterm S/R Deposit Billed Xfer            *
     H*                    Midterm S/R Deposit Paid Xfer              *
     H*                    Midterm S/R Deposit to Pmt Code            *
     H*                    Midterm S/R Deposit Tax Billed Xfer        *
     H*                    Midterm S/R Deposit Tax Paid Xfer          *
     H*                    Midterm S/R Deposit Tax to Pmt Code        *
     H*                    Midterm S/R Premium Offset                 *
     H*                    Midterm S/R Deposit Tax Premium Offset     *
     H*                                                               *
     H* 04/07/05 C:65  PROJECT: WC1273                                *
     H*                ALLOW DEPOSIT TO BE APPLIED TO AN ANNUAL LEVEL.*
     H*                                                               *
     H* 01/17/05 C:136 Support: WA109 44705                           *
     H*               -Correct deposit tax to prem transfer code      *
     H*                to be a Tax Paid A/R code in the S/R ->        *
     H*                Estimating section.                            *
     H*                                                               *
     H* 11/29/04 C:65  PROJECT: WC1167                                *
     H*                RE-ADDING NET RATE FLAG AND EFF DATE FIELDS.   *
     H*                THESE WERE DELETED SOMEHOW FROM THE PROGRAM.   *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227                                *
     H*                Add 4th and 5th assessment transfer codes to   *
     H*                screen 02.                                     *
     H*                Add 4th and 5th assessment fee transfer codes  *
     H*                to screen 03.                                  *
     H*                                                               *
     H* 10/03/03 C:136 PROJECT: WC1151                                *
     H*                Add field for cash transfer on flat canx.      *
     H*                                                               *
     H* 08/27/03 C:61  SUPPORT WA126 37184  WC577                     *
     H*                Remove change made 08/26/03.                   *
     H*                                                               *
     H* 08/26/03 C:61  SUPPORT WA126 37184                            *
     H*                - Group Policy Start date must not be less     *
     H*                  than Group Effective date.                   *
     H*                                                               *
     H* 06/29/02 C:136 PROJECT: WC1080 (KY DEDUCTIBLE BILLING TAX)    *
     H*                - ADD TWO NEW FIELDS FOR DEDUCTIBLE TAX BILLED *
     H*                  AND DEDUCTIBLE TAX PAID A/R CODES.           *
     H*                                                               *
     H* 04/26/02 C:65  PROJECT: WC1016                                *
     H*                CHANGE HEADING FOR ANNUAL PREM/STOCK DISC FLAG *
     H*                                                               *
     H* 03/15/02 C:61  SUPPORT WA091 31544                            *
     H*                Add group policy year date range overlap edit. *
     H*                                                               *
     H* 12/18/01 C:65  PROJECT: WC1056                                *
     H*                 ADD TRANSFER CODES FOR FEES ON FLAT CX.       *
     H*                                                               *
     H* 12/13/01 C:61  PROJECT WC577                                  *
     H*                Remove "F10=Search" from SCR01.                *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: WC1024                                *
     H*                 ADD 3RD ASSESSMENT TRANSFER CODE.             *
     H*                                                               *
     H* 09/07/00 C:65  PROJECT: WC1006                                *
     H*                ADD STATE TAX TRANSFER TO NEW BUSINESS PAYMENT *
     H*                AS WELL AS TAX ON DEPOSIT TRANSFER.            *
     H*                                                               *
     H* 08/15/00 C:61  SUPPORT 23970                                  *
     H*              - Disallow F5=Refresh in ADD mode.               *
     H*                                                               *
     H*  02/29/00 C:61  PROJECT WC577                                 *
     H*                 SCR02 - Edit A/R codes based on category.     *
     H*                                                               *
     H*  02/10/00 C:33  PROJECT: WC919                                *
     H*                 REMOVE RATE SET CODE.                         *
     H*                                                               *
     H*  10/25/99 C:65  PROJECT: WC908                                *
     H*                 REMOVE STANDARD INCR LIMITS DESC FIELDS       *
     H*                                                               *
     H*  04/12/99 C:65  PROJECT: WC875                                *
     H*                 ADD THREE CODES FOR TAX ON DEPOSIT TRANSFER   *
     H*                 CHANGED INDICATORS FOR DEPOSIT TRANSFER CODES *
     H*                                                               *
     H*  09/29/98 C:78  PROJECT: WC577                                *
     H*                 PRORATE, ADJUST ON YEARLY AND ADJUST ON       *
     H*                 CANCELLATION AUDIT FIELDS SHOULD BE REQUIRED  *
     H*                 ONLY IF MEMBER FEE AMOUNT IS KEYED IN.        *
     H*                                                               *
     H*  07/22/98 C:108 PROJECT SUPPORT: WA093/12777                  *
     H*                 CHANGE CLAIMS-ONLY FLAG FROM "Y" TO "C".      *
     H*                                                               *
     H*  07/21/98 C:108 PROJECT: WC577                                *
     H*                 ACCEPT ZERO OR NO ENTRY FOR LIABILITY OR      *
     H*                 BODILY INJURY LIMITS.                         *
     H*                                                               *
     H*  07/06/98 C:33 PROJECT: 21WAI                                 *
     H*                DO NOT VALIDATE FIELDS THAT ARE NOT DISPLAYED. *
     H*                                                               *
     H*  04/13/98 C:90 PROJECT: WC577                                 *
     H*                DISPLAY ERROR MESSAGE ON F10 SEARCH ON FIELD   *
     H*                WITHOUT TABLE BACKUP FOR SCR01.                *
     H*                                                               *
     H*  03/23/98 C:85 PROJECT: WC773                                 *
     H*                DETERMINE IF CLIENT IS ADMIN-ONLY (IND. 94),   *
     H*                OR CLAIMS-ONLY (IND. 95).                      *
     H*                                                               *
     H*  03/09/98 C:65 PROJECT: 91WBO                                 *
     H*                ADDED DEPOSIT ADJUSMENT THRESHOLD INFORMATION. *
     H*                                                               *
     H*  12/23/97 C:65 PROJECT: WC776                                 *
     H*                ADDED 1ST AND SECOND ASSESSMENT TRANSFER A/R.  *
     H*                                                               *
     H*  12/09/97 C:65 PROJECT: V3                                    *
     H*                ADDED FLAG FOR ANNUALIZE PREMIUM BEFORE DISC   *
     H*                AND ADDED EXPENSE CONSTANT TRANSFER A/R BACK.  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW0011F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDF1P     UF A E             DISK
     F                                     RENAME(PF101:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FWMFNP     IF   E             DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     D*
      *** Beg Add *** 01/29/14 *************************************************
     D/copy wPrGroup
      *** End Add *** 01/29/14 *************************************************
     D#ofAR            c                   75
     D AR              s             10    dim(#ofAR) ctdata perrcd(1)
     D ARInfo          s              6    dim(#ofAR) alt(AR)
     D TCInNx          ds
     D type                           1
     D category                       1
     D ind                            2
     D nxt                            2
     D ARValue         ds
     D* These MUST be in the same order as the AR compile time array
     D  afterSCR01                    2
     D  SCSRBB                        2
     D  SCNBPT                        2
     D  SCNBTR                        2
     D  SCMFPT                        2
     D  SCSFTX                        2
     D  SCPRTR                        2
     D  SCNBDT                        2
     D  SCCDET                        2
     D  SC1STT                        2
     D  SC2NDT                        2
     D  SC3RDT                        2
     D  SC4THT                        2
     D  SC5THT                        2
     D  SC6THT                        2
     D  SC7THT                        2
     D  SCTXTX                        2
     D  SCTXPY                        2
     D  SCDBTR                        2
     D  SCTFTR                        2
     D  SCDPTR                        2
     D  SCTQTR                        2
     D  SCPPTR                        2
     D  SCTPTR                        2
     D  SCDTXB                        2
     D  SCDTXP                        2
     D  SCDSCD                        2
      ****** beg add ***** 05/27/13 ************************************
     D  SCDBIL                        2
     D  SCDPAD                        2
     D  SCPYTF                        2
      ****** end add ***** 05/27/13 ************************************
     D  afterSCR02                    2
     D  SCMFTR                        2
     D  SCFCTR                        2
     D  SCSATR                        2
     D  SCTXTR                        2
     D  SC1ATR                        2
     D  SC3ATR                        2
     D  SC2ATR                        2
     D  SC4ATR                        2
     D  SC5ATR                        2
     D  SC6ATR                        2
     D  SC7ATR                        2
     D  afterSCR03                    2
     D  SCAXDD                        2
     D  SCAXEC                        2
     D  SCAXGF                        2
     D  SCAXLF                        2
     D  SCAXMF                        2
     D  SCAXDP                        2
     D  SCAXSF                        2
     D  SCAXTX                        2
     D  SCAXTD                        2
     D  SCAX1T                        2
     D  SCAX2T                        2
     D  SCAX3T                        2
     D  SCAX4T                        2
     D  SCAX5T                        2
     D  SCAX6T                        2
     D  SCAX7T                        2
     D  SCAXPT                        2
      *** Beg Chg ***** 06/08/18 **************************
     D**SCRCCD                        2
     D**SCRDCD                        2
     D**SCTACD                        2
     D  SCCOLB                        2
     D  SCCOLP                        2
     D  SCCOLT                        2
      *** end add ***** 06/08/18 ***************************
     D  afterSCR04                    2
     D  SCSHTR                        2
     D  SCSFTR                        2
     D  SCSITR                        2
     D  SCSQTR                        2
     D  SCSPTR                        2
     D  SCSRTR                        2
     D  SCSPOF                        2
     D  SCSRPO                        2
     D ARValues                       2    overlay(ARValue:1) dim(#ofAR)
     D*
     D*COPY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Group Detail Information')
     D*
     D COMAPI          C                   CONST('WA001R')
     D DOTS            C                   CONST(' . . . . . . . . . -
     D                                     . . . . . . . . .')
     D DEV             C                   CONST('Deviation')
     D DEV1            C                   CONST('Credit %, from')
     D DEV2            C                   CONST('2nd Deviation')
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D*
     D ##call          s              1
     D ##kc            s                   like(*inkc)
     D ##kl            s                   like(*inkl)
     D ##key           s                   like(TTKEY)
     D ARSearch        s                   like(*in90)
     D y               s              2  0
     d*** Beg Add *** 01/29/14 *************************************************
     d isOptaComp      s              1n
     d*** End Add *** 01/29/14 *************************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     DSearchAR         pr                  like(*in90)
     D fieldName                     10                 const
     D*
     DValidateAR       pr                  like(*in90)
     D fieldValue                          like(F1SRBB) const
     D fieldName                     10                 const
     C*
     C/COPY SCOPYMT05C
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  GROUP DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    SCFYR
     C*
     C     F1KY1         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0          WMFNP
     C*
      *===============================================================
      *  *LIKE
      *===============================================================
      *
     C     *LIKE         DEFINE    F1STPO        XXSTPO
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    F1NINV        XXNINV
     C     *LIKE         DEFINE    F1WFAM        XXWFAM
     C     *LIKE         DEFINE    F1WVAR        XXWVAR
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  POLICY YEAR MUST BE GREATER THAN ZERO (IF IN ADD MODE)
     C*
     C   40
B02  CANN90SCFYR         IFLE      *ZERO
     C                   SETON                                        20  90
E02  C                   ENDIF                                                  SCFND<=*ZERO
     C*
     C*  CHECK TO BE SURE POLICY YEAR HAS NOT BEEN USED PREVIOUSLY
     C*
     C   40
B02  CANN90F1KY          CHAIN     PF101                              60
     C   40
     CANN90
     CANN60              SETON                                        21  90
     C*
     C*  CHECK THE START POLICY DATE
     C*
     C                   Z-ADD     SCSTPO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTPO
     C*
     C  N90SRERR         IFEQ      '1'
     C                   SETON                                        22  90
     C                   ENDIF                                                  SRERR=1
     C*
     C*  CHECK THE END POLICY DATE
     C*
     C                   Z-ADD     SCENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENPO
     C*
     C  N90SRERR         IFEQ      '1'
     C                   SETON                                        23  90
     C                   ENDIF                                                  SRERR=1
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   Z-ADD     SCSTPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1            7 0
     C*
     C                   Z-ADD     SCENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2            7 0
     C*
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        24  90
     C                   ENDIF                                                  XXDAT2<XXDAT1
      *
      * Policy Date range must not overlap
      *  date range for other policy year
      *
     C     F1KY1         CHAIN     PF101                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SCFYR         IFNE      F1FYR
     C     XXDAT1        ANDLT     F1ENPO
     C     XXDAT2        ANDGT     F1STPO
     C                   SETON                                        71  90
     C                   ENDIF
      *
     C     F1KY1         READE     PF101                                  60
     C                   ENDDO
      *
     C*
     C*  DO NOT PERFORM THESE EDITS IF CLAIMS ONLY IS INSTALLED
     C*
     C  N90WCFL08        IFNE      'C'
     C*
     C*  SELF-REPORTING DEPOSIT STYLE MUST BE 'S', 'M' OR 'N'
     C*
     C  N90SCDPS         IFNE      'S'
     C     SCDPS         ANDNE     'M'
     C     SCDPS         ANDNE     'N'
     C     SCDPS         ANDNE     'A'
     C     *IN95         ANDEQ     *OFF
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCDPS<>S/M/N
     C*
     C*  UPDATE ACCOUNTS RECEIVABLE MUST BE 'Y' OR 'N'
     C*
     C  N90SCUPAR        IFNE      'Y'
     C     SCUPAR        ANDNE     'N'
     C     *IN95         ANDEQ     *OFF
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCUPAR<>Y/N
     C*
     C*  FINAL AUDIT COMPLETE MUST BE 'Y' OR 'N'
     C*
     C  N90SCFAUD        IFNE      'Y'
     C     SCFAUD        ANDNE     'N'
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCFAUD<>Y/N
     C*
     C*  SPLIT P/R FOR EXP MODS ON AUDIT BILLS MUST BY 'Y' OR 'N'
     C*
     C  N90SCSPLA        IFNE      'Y'
     C     SCSPLA        ANDNE     'N'
     C     *IN95         ANDEQ     *OFF
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCSPLA<>Y/N
     C*
     C*  RELAX CLAIMS COVERAGE EDITS MUST BE 'Y' OR BLANK
     C*
     C  N90SCRLXC        IFNE      'Y'
     C     SCRLXC        ANDNE     *BLANK
     C     *IN94         ANDEQ     *OFF
     C                   SETON                                        38  90
     C                   ENDIF                                                  SCRLXC<>Y/*BLANK
     C*
     C*  CLAIMS ONLY MUST BE 'Y' OR 'N'
     C*
     C  N90SCCLMS        IFNE      'Y'
     C     SCCLMS        ANDNE     'N'
     C     *IN94         ANDEQ     *OFF
     C                   SETON                                        39  90
     C                   ENDIF                                                  SCCLMS<>Y/N
     C*
     C*  ANNUALIZE PREMIUM BEFORE DISC FLAG MUST BE Y OR N
     C*
     C  N90SCAPFD        IFNE      'Y'
     C     SCAPFD        ANDNE     'N'
     C                   SETON                                        45  90
     C                   ENDIF                                                  SCAPFD<>Y/N
     C*
     C*  GENERATE BILLS WITH NET RATE FLAG MUST BE Y OR BLANK
     C*
     C  N90SCNETR        IFNE      'Y'
     C     SCNETR        ANDNE     *BLANK
     C                   SETON                                        46  90
     C                   ENDIF                                                  SCNETR IFNE
     C*
     C*  NET RATE EFFECTIVE DATE FIELD MUST BE VALID AND WITHIN FUND
     C*  DATES.
     C*
     C  N90SCNETR        IFEQ      'Y'
     C     SCNETD        ORNE      *ZERO
     C*
     C                   Z-ADD     SCNETD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCNETD
     C*
     C  N90SRERR         IFEQ      '1'
     C                   SETON                                        47  90
     C                   ENDIF                                                  SRERR=1
     C*
     C                   Z-ADD     SCNETD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C  N90XXDATE        IFLT      XXDAT1
     C     XXDATE        ORGE      XXDAT2
     C                   SETON                                        48  90
     C                   ENDIF                                                  XXDATE IFLT
     C*
     C                   ENDIF                                                  SCNETR IFEQ
     C*
     C* BILL EXPENSE CONSTATN FOR ALL DIVISIONS (CLIENT 90)
     C*
     C   69SCEXDV        IFNE      'Y'
     C     SCEXDV        ANDNE     ' '
     C                   SETON                                        49  90
     C                   ENDIF                                                  SCDPYM
     C*
     C* DEPOSIT YEAR MODE FLAG = 'Y' OR *BLANK
     C*
     C     SCDPYM        IFNE      'Y'
     C     SCDPYM        ANDNE     ' '
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCDPYM
     C*
     C                   ENDIF                                                  WCFL08<>C
     C*
     C*  STANDARD INCREASED LIMIT DESCRIPTION MUST BE ENTERED
     C*
     C*
     C  N90SCAPFL        IFNE      *BLANKS
     C     SCAPFL        ANDNE     'Y'
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C  N90SCAAFL        IFNE      *BLANKS
     C     SCAAFL        ANDNE     'Y'
     C                   SETON                                        74  90
     C                   ENDIF
     C  N90SCBLFL        IFNE      *BLANKS
     C     SCBLFL        ANDNE     'Y'
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCBLFL NE ' '
     c*** begin add ***  09/12/17  *******************************************
     C                   Clear                   msManP
     C                   Move      *off          erManP
     C*
     C* % of manual premium cannot be less than zero.
     C*
     C  N90SCManP        Iflt      0
     C     cltid         Andeq     '150'
     C                   SETON                                            90
     C                   Move      *on           erManP
     C                   Eval      msManP = '% of manual premium cannot be' +
     C                                      ' less than zero.'
     C                   Endif
     c*** end add *****  09/12/17  *******************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F1DT          IFGT      SAVEDT
     C     F1DT          OREQ      SAVEDT
     C     F1TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C* We will now use a procedure to validate the codes as follows:
     C* -Procedure will accept the field name and the field value.
     C* -Procedure will return *ON when an error is found
     C* -Procedure will use a lookup on the compile time array using the
     C*  field name to get the validation values and indicator
     C* -If error, procedure will turn on the indicator from array and
     C*  the appropriate indicator for type or category as appropriate
     C*
     C  N90              eval      *in90 = ValidateAR(SCSRBB:'SCSRBB')
     C  N90              eval      *in90 = ValidateAR(SCDSCD:'SCDSCD')
     C  N90              eval      *in90 = ValidateAR(SCNBPT:'SCNBPT')
     C  N90              eval      *in90 = ValidateAR(SCNBTR:'SCNBTR')
     C  N90              eval      *in90 = ValidateAR(SCMFPT:'SCMFPT')
     C  N90              eval      *in90 = ValidateAR(SCSFTX:'SCSFTX')
     C  N90              eval      *in90 = ValidateAR(SCPRTR:'SCPRTR')
     C  N90              eval      *in90 = ValidateAR(SCNBDT:'SCNBDT')
     C  N90              eval      *in90 = ValidateAR(SCCDET:'SCCDET')
     C  N90              eval      *in90 = ValidateAR(SC1STT:'SC1STT')
     C  N90              eval      *in90 = ValidateAR(SC2NDT:'SC2NDT')
     C  N90              eval      *in90 = ValidateAR(SC3RDT:'SC3RDT')
     C  N90              eval      *in90 = ValidateAR(SC4THT:'SC4THT')
     C  N90              eval      *in90 = ValidateAR(SC5THT:'SC5THT')
     C  N90              eval      *in90 = ValidateAR(SC6THT:'SC6THT')
     C  N90              eval      *in90 = ValidateAR(SC7THT:'SC7THT')
     C  N90              eval      *in90 = ValidateAR(SCTXTX:'SCTXTX')
     C  N90              eval      *in90 = ValidateAR(SCTXPY:'SCTXPY')
     C  N90              eval      *in90 = ValidateAR(SCDBTR:'SCDBTR')
     C  N90              eval      *in90 = ValidateAR(SCTFTR:'SCTFTR')
     C  N90              eval      *in90 = ValidateAR(SCDPTR:'SCDPTR')
     C  N90              eval      *in90 = ValidateAR(SCTQTR:'SCTQTR')
     C  N90              eval      *in90 = ValidateAR(SCPPTR:'SCPPTR')
     C  N90              eval      *in90 = ValidateAR(SCTPTR:'SCTPTR')
     C  N90              eval      *in90 = ValidateAR(SCDTXB:'SCDTXB')
     C  N90              eval      *in90 = ValidateAR(SCDTXP:'SCDTXP')
      ****** beg add ***** 05/27/13 ************************************
     C  N90              eval      *in90 = ValidateAR(SCDBIL:'SCDBIL')
     C  N90              eval      *in90 = ValidateAR(SCDPAD:'SCDPAD')
     C  N90              eval      *in90 = ValidateAR(SCPYTF:'SCPYTF')
      ****** end add ***** 05/27/13 ************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F1DT          IFGT      SAVEDT
     C     F1DT          OREQ      SAVEDT
     C     F1TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C     ERRCK3        BEGSR
      ****** beg add ***** 03/11/09 ************************************
     c
     c  n90              if        (scidbd <> 'Y') and (scidbd <> ' ')
     c                   eval      *in88 = *on
     c                   eval      *in90 = *on
     c                   endif
      ****** end add ***** 03/11/09 ************************************
     C*
     C  N90              eval      *in90 = ValidateAR(SCMFTR:'SCMFTR')
     C  N90              eval      *in90 = ValidateAR(SCSATR:'SCSATR')
     C  N90              eval      *in90 = ValidateAR(SCTXTR:'SCTXTR')
     C  N90              eval      *in90 = ValidateAR(SCFCTR:'SCFCTR')
     C  N90              eval      *in90 = ValidateAR(SC1ATR:'SC1ATR')
     C  N90              eval      *in90 = ValidateAR(SC2ATR:'SC2ATR')
     C  N90              eval      *in90 = ValidateAR(SC4ATR:'SC4ATR')
     C  N90              eval      *in90 = ValidateAR(SC3ATR:'SC3ATR')
     C  N90              eval      *in90 = ValidateAR(SC5ATR:'SC5ATR')
     C  N90              eval      *in90 = ValidateAR(SC6ATR:'SC6ATR')
     C  N90              eval      *in90 = ValidateAR(SC7ATR:'SC7ATR')
     C*
     C  N90              select
     C*
     C                   when      SCMFEE = *zero
     C                   select
     C                   when      SCPMFL <> *blank
     C                   seton                                        85  90
     C                   when      SCYRAD <> *blank
     C                   seton                                        86  90
     C                   when      SCCXAD <> *blank
     C                   seton                                        87  90
     C                   endsl                                                  SCCXAD<>*BLANK
     C*
     C                   when      SCMFEE >= *zero
     C*  PRORATE MEMBERSHIP FEE FLAG MUST BE 'Y' OR 'N'
     C                   select
     C                   when      SCPMFL <> 'Y' and SCPMFL <> 'N'
     C                   seton                                        76  90
     C*  ADJUST ON YEARLY AUDIT FLAG MUST BE 'Y' OR 'N'
     C                   when      SCYRAD <> 'Y' and SCYRAD <> 'N'
     C                   seton                                        77  90
     C*  ADJUST ON CX AUDIT FLAG MUST BE 'Y' OR 'N'
     C                   when      SCCXAD <> 'Y' and SCCXAD <> 'N'
     C                   seton                                        78  90
     C                   endsl                                                  SCCXAD<>Y/N
     C*
     C                   when      SCMFEE <= *zero
     C                   seton                                        75  90
     C*
     C                   endsl                                                  SCMFEE>ZERO
     C*
     C* GENERATE ADJ ON RENEWAL BILLS FLAG MUST BE 'Y' OR 'N'.
     C*
     C  N90SCBRFL        IFNE      'Y'
     C     SCBRFL        ANDNE     'N'
     C                   SETON                                        81  90
     C                   ENDIF                                                  SCBRFL IFNE
     C*
     C* GENERATE ADJ ON REVISED BILLS FLAG MUST BE 'Y' OR 'N'.
     C*
     C  N90SCBVFL        IFNE      'Y'
     C     SCBVFL        ANDNE     'N'
     C                   SETON                                        82  90
     C                   ENDIF                                                  SCBVFL IFNE
     C*
     C* DEBIT THRESHOLD CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCDADJ        IFLT      0
     C                   SETON                                        83  90
     C                   ENDIF                                                  SCDADJ IFNE
     C*
     C* CREDIT THRESHOLD CANNOT BE LESS THAN ZERO.
     C*
      ***** beg del *********** 12/26/14 ****************************************
     C************  N90SCCADJ        IFLT      0
     C************                   SETON                                        84  90
     C************                ENDIF                                                  SCCADJ IFNE
      ***** end del *********** 12/26/14 ****************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F1DT          IFGT      SAVEDT
     C     F1DT          OREQ      SAVEDT
     C     F1TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04
     C*===============================================================
     C     ERRCK4        BEGSR
      *
     C  N90              eval      *in90 = ValidateAR(SCAXDD:'SCAXDD')
     C  N90              eval      *in90 = ValidateAR(SCAXEC:'SCAXEC')
     C  N90              eval      *in90 = ValidateAR(SCAXGF:'SCAXGF')
     C  N90              eval      *in90 = ValidateAR(SCAXLF:'SCAXLF')
     C  N90              eval      *in90 = ValidateAR(SCAXMF:'SCAXMF')
     C  N90              eval      *in90 = ValidateAR(SCAXDP:'SCAXDP')
     C  N90              eval      *in90 = ValidateAR(SCAXSF:'SCAXSF')
     C  N90              eval      *in90 = ValidateAR(SCAXTX:'SCAXTX')
     C  N90              eval      *in90 = ValidateAR(SCAXTD:'SCAXTD')
     C  N90              eval      *in90 = ValidateAR(SCAX1T:'SCAX1T')
     C  N90              eval      *in90 = ValidateAR(SCAX2T:'SCAX2T')
     C  N90              eval      *in90 = ValidateAR(SCAX3T:'SCAX3T')
     C  N90              eval      *in90 = ValidateAR(SCAX4T:'SCAX4T')
     C  N90              eval      *in90 = ValidateAR(SCAX5T:'SCAX5T')
     C  N90              eval      *in90 = ValidateAR(SCAX6T:'SCAX6T')
     C  N90              eval      *in90 = ValidateAR(SCAX7T:'SCAX7T')
     C  N90              eval      *in90 = ValidateAR(SCAXPT:'SCAXPT')
     C*** beg add ***** 06/08/18 **************************************
     C  N90              eval      *in90 = ValidateAR(SCCOLB:'SCCOLB')
     C  N90              eval      *in90 = ValidateAR(SCCOLP:'SCCOLP')
     C  N90              eval      *in90 = ValidateAR(SCCOLT:'SCCOLT')
     C*** end add ***** 06/08/18 **************************************

     C***  Begin Add  ***  03/10/11  **************************************
     C                   if        *in90
     C                   leavesr
     C                   endif
     C*
     C* Validate stop collateral date
     C                   if        scScDt <> *zero
     C*
     C                   Z-ADD     SCSCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSCDT
     C*
     C                   if        srErr = *on
     C                   seton                                        88  90
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C***  End   Add  ***  03/10/11  **************************************
      *
      *  NY RATE DECREASE FLAG MUST EQUAL 'Y' OR *BLANK
      *
     C  N90SCNY07        IFNE      'Y'
     C     SCNY07        ANDNE     ' '
     C                   SETON                                        6590
     C                   ENDIF
      *
     C     CLTID         IFEQ      '071'
      *
      *  RETRO CREDIT FLAG MUST EQUAL 'Y' OR *BLANK
      *
     C     SCORCR        IFNE      'Y'
     C     SCORCR        ANDNE     ' '
     C                   SETON                                        7990
     C                   ENDIF
      *
      *  IF RETRO CREDIT FLAG = 'Y', R.C. A/R CODE MUST BE ENTERED
      *
     C  N90SCORCR        IFEQ      'Y'
     C     SCRCCD        ANDEQ     *BLANK
     C                   SETON                                        9075
     C                   ENDIF
      *
      *  IF RETRO CREDIT FLAG = 'Y', R.D. A/R CODE MUST BE ENTERED
      *
     C  N90SCORCR        IFEQ      'Y'
     C     SCRDCD        ANDEQ     *BLANK
     C                   SETON                                        9077
     C                   ENDIF
      *
      *  RETRO CRIDT FLAG MUST = 'Y' IF A/R CODE ENTERED
      *
     C  N90SCORCR        IFEQ      *BLANK
     C     SCRCCD        ANDNE     *BLANK
     C     SCORCR        OREQ      *BLANK
     C     SCRDCD        ANDNE     *BLANK
     C                   SETON                                        9080
     C                   ENDIF
      *
      *  RETRO CREDIT A/R CODE MUST BE VALID
     C  N90              eval      *in90 = ValidateAR(SCRCCD:'SCRCCD')
      *
      *  RETRO DEBIT A/R CODE MUST BE VALID
     C  N90              eval      *in90 = ValidateAR(SCRDCD:'SCRDCD')
      *
      *  Audit Transfer Credit A/R Code must be Valid and the type
      *  must be a "P", and the Category type must be a "P".
      *  It also must have a status code of active.
     C  N90              eval      *in90 = ValidateAR(SCTACD:'SCTACD')
      *
     C                   endif                                                  CLTID = '071'
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90F1DT          IFGT      SAVEDT
     C     F1DT          OREQ      SAVEDT
     C     F1TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C                   ENDSR                                                  ERRCK4
     C*===============================================================
     C* ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR05
     C*===============================================================
     C     ERRCK5        BEGSR
     C*
     C  N90              eval      *in90 = ValidateAR(SCSHTR:'SCSHTR')
     C  N90              eval      *in90 = ValidateAR(SCSFTR:'SCSFTR')
     C  N90              eval      *in90 = ValidateAR(SCSITR:'SCSITR')
     C  N90              eval      *in90 = ValidateAR(SCSQTR:'SCSQTR')
     C  N90              eval      *in90 = ValidateAR(SCSPTR:'SCSPTR')
     C  N90              eval      *in90 = ValidateAR(SCSRTR:'SCSRTR')
     C  N90              eval      *in90 = ValidateAR(SCSPOF:'SCSPOF')
     C  N90              eval      *in90 = ValidateAR(SCSRPO:'SCSRPO')
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F1DT          IFGT      SAVEDT
     C     F1DT          OREQ      SAVEDT
     C     F1TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C                   ENDSR                                                  ERRCK5
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C* An attempt will be made to search A/R.  If an A/R search is not
     C*  done because the field searched is other than an A/R code,
     C*  ARSearch will be off and other searches can then be performed.
     C                   eval      ARSearch = SearchAR(hfld)
     C*
     C                   if        not ARSearch
     C* Perform other than A/R searches here
     C                   endif
     C*
     C                   ENDSR                                                  KJDO
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0010R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C*  IF IN ADD MODE, DEFAULT TO THE NEXT YEAR (IF PREVIOUS EXISTED)
     C*  AND DEFAULT ALL CODES TO PRIOR YEARS.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     *ZERO         F1FYR
     C                   MOVEL     *HIVAL        SCFYR
     C     F1KY          SETGT     PF101
     C     F1KY1         READPE    PF101                                  60
     C  N60              ADD       1             F1FYR
     C*
     C                   ENDIF                                                  *IN40=*ON(ADD MODE)
     C*
     C                   Z-ADD     F1FYR         SCFYR
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSTPO
     C  N60
     CAN 40              ADD       1             SCSTPO
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENPO
     C  N60
     CAN 40              ADD       1             SCENPO
     C*
     C                   MOVEL     F1DPS         SCDPS
     C                   MOVEL     F1UPAR        SCUPAR
     C                   MOVEL     F1FAUD        SCFAUD
     C                   MOVEL     F1SPLA        SCSPLA
     C  N67              MOVEL     F1RLXC        SCRLXC
     C   67              MOVEL     'Y'           SCRLXC
     C  N67              MOVEL     F1CLMS        SCCLMS
     C   67              MOVEL     'Y'           SCCLMS
     C                   MOVEL     F1APFD        SCAPFD
     C                   MOVEL     F1CDET        SCCDET
     C                   MOVEL     F11STT        SC1STT
     C                   MOVEL     F12NDT        SC2NDT
     C                   MOVEL     F13RDT        SC3RDT
     C                   MOVEL     F14THT        SC4THT
     C                   MOVEL     F15THT        SC5THT
     C                   MOVEL     F16THT        SC6THT
     C                   MOVEL     F17THT        SC7THT
     C                   MOVEL     F1TXTX        SCTXTX
     C                   MOVEL     F1TXPY        SCTXPY
     C                   MOVEL     F1SRBB        SCSRBB
     C                   MOVEL     F1NBPT        SCNBPT
     C                   MOVEL     F1NBTR        SCNBTR
     C                   MOVEL     F1PRTR        SCPRTR
     C                   MOVEL     F1NBDT        SCNBDT
     C                   MOVEL     F1DBTR        SCDBTR
     C                   MOVEL     F1DPTR        SCDPTR
     C                   MOVEL     F1PPTR        SCPPTR
     C                   MOVEL     F1TFTR        SCTFTR
     C                   MOVEL     F1TQTR        SCTQTR
     C                   MOVEL     F1TPTR        SCTPTR
     C                   MOVEL     F1MFTR        SCMFTR
     C                   MOVEL     F1SATR        SCSATR
     C                   MOVEL     F1TXTR        SCTXTR
     C                   MOVEL     F11ATR        SC1ATR
     C                   MOVEL     F12ATR        SC2ATR
     C                   MOVEL     F13ATR        SC3ATR
     C                   MOVEL     F14ATR        SC4ATR
     C                   MOVEL     F15ATR        SC5ATR
     C                   MOVEL     F16ATR        SC6ATR
     C                   MOVEL     F17ATR        SC7ATR
     C                   MOVEL     F1FCTR        SCFCTR
     C                   MOVEL     F1MFPT        SCMFPT
     C                   MOVEL     F1MFEE        SCMFEE
     C                   MOVEL     F1PMFL        SCPMFL
     C   40              MOVE      *BLANK        SCPMFL
     C                   MOVEL     F1YRAD        SCYRAD
     C   40              MOVE      *BLANK        SCYRAD
     C                   MOVEL     F1CXAD        SCCXAD
      ****** beg add ***** 03/11/09 ************************************
     C                   MOVE      F1IDBD        SCIDBD
      ****** end add ***** 03/11/09 ************************************
     C   40              MOVE      *BLANK        SCCXAD
     C                   MOVEL     F1SFTX        SCSFTX
     C                   MOVE      F1BRFL        SCBRFL
     C                   MOVE      F1BVFL        SCBVFL
     C                   Z-ADD     F1DADJ        SCDADJ
     C                   Z-ADD     F1CADJ        SCCADJ
     C                   MOVEL     F1DTXB        SCDTXB
     C                   MOVEL     F1DTXP        SCDTXP
     C                   MOVE      F1NETR        SCNETR
     C                   Z-ADD     F1NETD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCNETD
     C  N60
     CAN 40              Z-ADD     SCSTPO        SCNETD
     C*** Begin Add *** 01/07/22 **************************************
     c                   If        F1netSR = ' '
     c                   Eval      scNetSR = 'N'
     c                   Else
     c                   Eval      scNetSR = F1netSR
     c                   EndIf
     C*** End   Add *** 01/07/22 **************************************
     C*
     C                   MOVEL     F1EMRT        SCEMRT
     C                   MOVE      F1SHTR        SCSHTR
     C                   MOVE      F1SITR        SCSITR
     C                   MOVE      F1SPTR        SCSPTR
     C                   MOVE      F1SFTR        SCSFTR
     C                   MOVE      F1SQTR        SCSQTR
     C                   MOVE      F1SRTR        SCSRTR
     C                   MOVE      F1SPOF        SCSPOF
     C                   MOVE      F1SRPO        SCSRPO
     C*
     C                   MOVEL     F1AXDD        SCAXDD
     C                   MOVEL     F1AXEC        SCAXEC
     C                   MOVEL     F1AXGF        SCAXGF
     C                   MOVEL     F1AXLF        SCAXLF
     C                   MOVEL     F1AXMF        SCAXMF
     C                   MOVEL     F1AXDP        SCAXDP
     C                   MOVEL     F1AXSF        SCAXSF
     C                   MOVEL     F1AXTX        SCAXTX
     C                   MOVEL     F1AXTD        SCAXTD
     C                   MOVEL     F1AX1T        SCAX1T
     C                   MOVEL     F1AX2T        SCAX2T
     C                   MOVEL     F1AX3T        SCAX3T
     C                   MOVEL     F1AX4T        SCAX4T
     C                   MOVEL     F1AX5T        SCAX5T
     C                   MOVEL     F1AX6T        SCAX6T
     C                   MOVEL     F1AX7T        SCAX7T
     C                   MOVEL     F1AXPT        SCAXPT
      *
     C                   MOVEL     F1DSCD        SCDSCD
     C                   MOVE      F1ORCR        SCORCR
     C                   MOVE      F1RCCD        SCRCCD
     C                   MOVE      F1RDCD        SCRDCD
     C                   MOVE      F1TACD        SCTACD
     C                   MOVEL     F1DPYM        SCDPYM
     C                   MOVEL     F1APFL        SCAPFL
     C                   MOVEL     F1AAFL        SCAAFL
      *
     C   40              MOVE      *BLANKS       SCEXDV
     C                   MOVE      F1EXDV        SCEXDV
     C                   MOVE      F1BLFL        SCBLFL
     C                   MOVE      F1NY07        SCNY07
      *
     C***  Begin Add  ***  03/10/11  **************************************
     C                   Z-ADD     F1SCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCDT
     C***  End   Add  ***  03/10/11  **************************************
      ****** beg add ***** 05/27/13 ************************************
      /free
       SCDBIL = F1dvbl;
       SCDPAD = F1dvpd;
       SCPYTF = F1dvxr;
       //*** begin add ***  09/12/17  *******************************************
       SCManP = F1ManP;
       //*** end add *****  09/12/17  *******************************************
       //*** beg add ***** 06/08/18 ***********************
       scColB = F1ColB;
       scColP = F1ColP;
       scColT = F1ColT;
       //*** end add ***** 06/08/18 ***********************
       //*** Begin Add *** 05/14/24 ****************************************
       scSRAs = F1SRAs;
       //*** End   Add *** 05/14/24 ****************************************
      /End-free
      ****** end add ***** 05/27/13 ************************************
      *
     C                   ENDSR                                                  MVDBSC
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCSTPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1STPO
     C*
     C                   Z-ADD     SCENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1ENPO
     C*
     C                   MOVEL     SCDPS         F1DPS
     C                   MOVEL     SCUPAR        F1UPAR
     C                   MOVEL     SCFAUD        F1FAUD
     C                   MOVEL     SCSPLA        F1SPLA
     C                   MOVEL     SCRLXC        F1RLXC
     C                   MOVEL     SCCLMS        F1CLMS
     C                   MOVEL     SCSRBB        F1SRBB
     C                   MOVEL     SCAPFD        F1APFD
     C                   MOVEL     SCCDET        F1CDET
     C                   MOVEL     SC1STT        F11STT
     C                   MOVEL     SC2NDT        F12NDT
     C                   MOVEL     SC3RDT        F13RDT
     C                   MOVEL     SCTXTX        F1TXTX
     C                   MOVEL     SCTXPY        F1TXPY
     C                   MOVEL     SCNBPT        F1NBPT
     C                   MOVEL     SCNBTR        F1NBTR
     C                   MOVEL     SCPRTR        F1PRTR
     C                   MOVEL     SCNBDT        F1NBDT
     C                   MOVEL     SCDBTR        F1DBTR
     C                   MOVEL     SCDPTR        F1DPTR
     C                   MOVEL     SCPPTR        F1PPTR
     C                   MOVEL     SCTFTR        F1TFTR
     C                   MOVEL     SCTQTR        F1TQTR
     C                   MOVEL     SCTPTR        F1TPTR
     C                   MOVEL     SCMFTR        F1MFTR
     C                   MOVEL     SCSATR        F1SATR
     C                   MOVEL     SCTXTR        F1TXTR
     C                   MOVEL     SC1ATR        F11ATR
     C                   MOVEL     SC2ATR        F12ATR
     C                   MOVEL     SC3ATR        F13ATR
     C                   MOVEL     SCFCTR        F1FCTR
     C                   MOVEL     SCMFPT        F1MFPT
     C                   MOVEL     SCMFEE        F1MFEE
     C                   MOVEL     SCPMFL        F1PMFL
     C                   MOVEL     SCYRAD        F1YRAD
     C                   MOVEL     SCCXAD        F1CXAD
      ****** beg add ***** 03/11/09 ************************************
     C                   MOVE      SCIDBD        F1IDBD
      ****** end add ***** 03/11/09 ************************************
     C                   MOVEL     SCSFTX        F1SFTX
     C                   MOVEL     SCEMRT        F1EMRT
     C                   MOVE      SCBRFL        F1BRFL
     C                   MOVE      SCBVFL        F1BVFL
     C                   Z-ADD     SCDADJ        F1DADJ
     C                   Z-ADD     SCCADJ        F1CADJ
     C                   MOVEL     SCDTXB        F1DTXB
     C                   MOVEL     SCDTXP        F1DTXP
     C                   MOVEL     SC4THT        F14THT
     C                   MOVEL     SC5THT        F15THT
     c*** begin add ***  01/08/09  ************************************************
     C                   MOVEL     SC6THT        F16THT
     C                   MOVEL     SC7THT        F17THT
     C                   MOVEL     SC6ATR        F16ATR
     C                   MOVEL     SC7ATR        F17ATR
     c*** end add *****  01/08/09  ************************************************
     C                   MOVEL     SC4ATR        F14ATR
     C                   MOVEL     SC5ATR        F15ATR
     C                   MOVE      SCNETR        F1NETR
     C                   Z-ADD     SCNETD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1NETD
     C*** Begin Add *** 01/07/22 **************************************
     c                   If        scNetSR = ' '
     c                   Eval      F1netSR = 'N'
     c                   Else
     c                   If        scNetR  = 'Y'
     c                   Eval      F1netSr = 'N'
     c                   Else
     c                   Eval      F1netSR = scNetSR
     c                   EndIf
     c                   EndIf
     C*** End   Add *** 01/07/22 **************************************
     C                   MOVE      SCSHTR        F1SHTR
     C                   MOVE      SCSITR        F1SITR
     C                   MOVE      SCSPTR        F1SPTR
     C                   MOVE      SCSFTR        F1SFTR
     C                   MOVE      SCSQTR        F1SQTR
     C                   MOVE      SCSRTR        F1SRTR
     C                   MOVE      SCSPOF        F1SPOF
     C                   MOVE      SCSRPO        F1SRPO
     C                   MOVEL     SCAXDD        F1AXDD
     C                   MOVEL     SCAXEC        F1AXEC
     C                   MOVEL     SCAXGF        F1AXGF
     C                   MOVEL     SCAXLF        F1AXLF
     C                   MOVEL     SCAXMF        F1AXMF
     C                   MOVEL     SCAXDP        F1AXDP
     C                   MOVEL     SCAXSF        F1AXSF
     C                   MOVEL     SCAXTX        F1AXTX
     C                   MOVEL     SCAXTD        F1AXTD
     C                   MOVEL     SCAX1T        F1AX1T
     C                   MOVEL     SCAX2T        F1AX2T
     C                   MOVEL     SCAX3T        F1AX3T
     C                   MOVEL     SCAX4T        F1AX4T
     C                   MOVEL     SCAX5T        F1AX5T
     c*** begin add ***  01/08/09  ***************************************************
     C                   MOVEL     SCAX6T        F1AX6T
     C                   MOVEL     SCAX7T        F1AX7T
     c*** end add *****  01/08/09  ***************************************************
     C                   MOVEL     SCAXPT        F1AXPT
      *
     C                   MOVEL     SCDSCD        F1DSCD
     C                   MOVE      SCORCR        F1ORCR
     C                   MOVE      SCRCCD        F1RCCD
     C                   MOVE      SCRDCD        F1RDCD
     C                   MOVE      SCTACD        F1TACD
     C                   MOVEL     SCDPYM        F1DPYM
     C                   MOVEL     SCAPFL        F1APFL
     C                   MOVEL     SCAAFL        F1AAFL
      *
     C                   MOVE      SCEXDV        F1EXDV
     C                   MOVE      SCBLFL        F1BLFL
     C                   MOVE      SCNY07        F1NY07
      *
     C***  Begin Add  ***  03/10/11  **************************************
     C                   Z-ADD     SCSCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1SCDT
     C***  End   Add  ***  03/10/11  **************************************
      ****** beg add ***** 05/27/13 ************************************
      /free
       F1dvbl = SCDBIL;
       F1dvpd = SCDPAD;
       F1dvxr = SCPYTF;
       //*** begin add ***  09/12/17  *******************************************
       F1ManP = SCManP;
       //*** end add *****  09/12/17  *******************************************
       //*** beg add ***** 06/08/18 ***********************
       F1ColB = scColB;
       F1ColP = scColP;
       F1ColT = scColT;
       //*** end add ***** 06/08/18 ***********************
       //*** Begin Add *** 05/14/24 ****************************************
       F1SRAs = scSRAs;
       //*** End   Add *** 05/14/24 ****************************************
      /end-free
      ****** end add ***** 05/27/13 ************************************
      *
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
      * If Client 046 (LSAT) Set ON Indicator for S/R Discount Code
     C                   eval      *in16 = cltID = '046'
     C                   eval      *in50 = cltID = '071'
      * If Client 090 (RMLA) Set ON Indicator Bill Exp Cnst all Divisions
      ***** beg chg *********** 04/25/14 ****************************************
     C*****              eval      *in69 = cltID = '090'
      /free
       *in69 = *off;
       If cltId ='088' or cltId = '090';
          *in69 = *on;
       Endif;
       //*** begin add ***  09/12/17  *******************************************
       vis150 = *off;
       If cltId ='150';
          vis150 = *on;
          arInfo(60) = 'PP3751';
       Endif;
       //*** end add *****  09/12/17  *******************************************
       //*** Begin Add *** 05/14/24 ****************************************
       vis146 = *off;
       If cltId ='146';
          vis146 = *on;
       Endif;
       //*** End   Add *** 05/14/24 ****************************************
      /end-free
      ***** end chg *********** 04/25/14 ****************************************
     C                   eval      *in70 = cltID = '129'
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  GET GROUP INFORMATION
     C*
     C     @@RRN2        CHAIN     WMFNP                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      ****** beg add ***** 01/29/14 ************************************
      * If Client CSI or OptaComp display Dividend Plan.
      /free
       visbCSIOPT = *Off;
       isOptaComp = *Off;
       isOptaComp = GetGrpAssFlg(FNco#:FNfnd);
       If isOptaComp or cltID = '114';
          visbCSIOPT = *On;
       Endif;
      /end-free
      ****** end add ***** 01/29/14 ************************************
     C*
     C                   clear                   WCFL07
     C                   clear                   WCFL08
     C                   clear                   WCFL10
     C*
     C*  Access System Control file 'WC' record
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C*  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
     C     WCFL07        COMP      'Y'                                    66
     C     WCFL08        COMP      'A'                                    94
     C     WCFL08        COMP      'C'                                    95
     C     WCFL10        COMP      'Y'                                    68
     C***  Begin Add  ***  03/10/11  **************************************
     C                   eval      *in64 = WCFL44 = 'Y'
     C***  End   Add  ***  03/10/11  **************************************
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C                   Z-ADD     4             #OFSCR
     C                   OTHER
     C                   Z-ADD     4             #OFSCR            1 0          SCR01,02 &03
     C                   ENDSL
     C*
     C                   select
     C                   when      WCFL08 = 'C'
     C                   eval      #ofScr = 1
     C                   endsl                                                  WCFL08<>*BLANK
     C*
     C*  GET SECURITY LEVEL OF THE USER
     C*
     C                   MOVE      *OFF          *IN41
     C*
     C                   exsr      UpdateInd
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* Update the indicators in the compile time array for non-displays
     C*===============================================================
     C     UpdateInd     begsr
     C*
     C* S/R discount code not displayed
     C                   if        not *in16
     C                   eval      y = 1
     C     'SCSRBB    '  lookup    AR(y)                                  60
     C                   if        %found
     C                   eval      TCInNx = ARInfo(y)
     C                   eval      nxt = '22'
     C                   eval      ARInfo(y) = TCInNx
     C                   endif
     C                   endif
     C*
     C                   endsr
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F1USER
     C                   MOVEL     Q1PGM         F1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1DT
     C                   MOVEL     DSTIME        F1TIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     FNCO#         F1CO#
     C                   Z-ADD     SCFND         F1FND
     C                   Z-ADD     SCFYR         F1FYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F1IUSR
     C                   MOVEL     Q1USER        F1USER
     C                   MOVEL     Q1PGM         F1IPGM
     C                   MOVEL     Q1PGM         F1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1IDT
     C                   MOVEL     SRDTO         F1DT
     C                   MOVEL     DSTIME        F1ITM
     C                   MOVEL     DSTIME        F1TIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDF1P
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER





     P*====================================================================
     P* Search A/R Codes
     P*====================================================================
     PSearchAR         b
     DSearchAR         pi                  like(*in90)
     D fieldName                     10                 const

     D indicator       s              2  0
     D nextInd         s              2  0

     C                   eval      y = 1
     C     fieldName     lookup    AR(y)                                  60
     C                   select
     C                   when      not %found
     C                   return    *off
     C                   other
     C*
     C                   eval      TCInNx = ARInfo(y)
     C                   move      ind           indicator
     C                   move      nxt           nextInd
     C*
     C                   call      'W2161R'
     C                   parm      '2'           ##call
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      *blank        ##key
     C*
     C                   select
     C                   when      ##Key = *blank
     C                   eval      *in(indicator) = *on
     C                   when      ##Key <> *blank
     C                   eval      ARValues(y) = ##Key
     C                   if        nextInd <> *zero
     C                   eval      *in(nextInd) = *on
     C                   endif
     C                   endsl
     C                   return    *on
     C                   endsl
     PSearchAR         e





     P*====================================================================
     P* Validate A/R Codes
     P* We will now use a procedure to validate the codes as follows:
     P* -Procedure will accept the field name and the field value.
     P* -Procedure will return *ON when an error is found
     P* -Procedure will use a lookup on the compile time array using the
     P*  field name to get the validation values and indicator
     P* -If error, procedure will turn on the indicator from array and
     P*  the appropriate indicator for type or category as appropriate
     P*====================================================================
     PValidateAR       b
     DValidateAR       pi                  like(*in90)
     D fieldValue                          like(F1SRBB) const
     D fieldName                     10                 const

     D key             s                   like(TTKEY)
     D indicator       s              2  0

     C     tableKey      klist
     C                   kfld                    FNCO#
     C                   kfld                    key

     C                   eval      y = 1
     C     fieldName     lookup    AR(y)                                  60

     C                   select
     C                   when      fieldValue = *blanks
     C                   return    *off
     C*
     C                   when      %found
     C*
     C                   eval      TCInNx = ARInfo(y)
     C                   move      ind           indicator
     C                   eval      *in(indicator) = *on
     C                   eval      key = fieldValue
     C     tableKey      chain     ptt01
     C*
     C                   select
     C                   when      not %found
     C                   eval      *in71 = *on
     C                   return    *on
     C                   when      TTSTAT = 'I'
     C                   eval      *in72 = *on
     C                   return    *on
     C                   when      TTTYPE <> type
     C                   eval      *in73 = *on
     C                   return    *on
     C                   when      TTCTYP <> category
     C                   eval      *in74 = *on
     C                   return    *on
     C                   endsl
     C*
     C                   other
     C                   return    *on
     C*
     C                   endsl
     C*
     C                   eval      *in(indicator) = *off
     C*
     C                   return    *off

     PValidateAR       e





** Code   TCInNx  Screen
afterSCR01               to break up screens
SCSRBB    BP2147  SCR02
SCNBPT    PP2223  SCR02
SCNBTR    PP2324  SCR02
SCMFPT    QM2425  SCR02
SCSFTX    QS2526  SCR02
SCPRTR    PP2627  SCR02
SCNBDT    ID2728  SCR02
SCCDET    PE2829  SCR02
SC1STT    Q12930  SCR02
SC2NDT    Q23031  SCR02
SC3RDT    Q33132  SCR02
SC4THT    Q43233  SCR02
SC5THT    Q53346  SCR02
SC6THT    Q64645  SCR02
SC7THT    Q74534  SCR02
SCTXTX    QT3435  SCR02
SCTXPY    QD3536  SCR02
SCDBTR    HD3637  SCR02
SCTFTR    FD3738  SCR02
SCDPTR    ID3839  SCR02
SCTQTR    QD3941  SCR02
SCPPTR    PP4142  SCR02
SCTPTR    QT4243  SCR02
SCDTXB    FR4344  SCR02
SCDTXP    QR44    SCR02
SCDSCD    BP4722  SCR02
SCDBIL    BP59    SCR02
SCDPAD    PP62    SCR02
SCPYTF    PP63    SCR02
afterSCR02               to break up screens
SCMFTR    QM2124  SCR03
SCFCTR    PP2322  SCR03
SCSATR    QS2427  SCR03
SCTXTR    QT2723  SCR03
SC1ATR    Q12225  SCR03
SC2ATR    Q22528  SCR03
SC3ATR    Q32826  SCR03
SC4ATR    Q42629  SCR03
SC5ATR    Q52930  SCR03
SC6ATR    Q63031  SCR03
SC7ATR    Q73180  SCR03
afterSCR03               to break up screens
SCAXDD    QR2122  SCR04
SCAXEC    PE2223  SCR04
SCAXGF    QF2324  SCR04
SCAXLF    QL2425  SCR04
SCAXMF    QM2526  SCR04
SCAXDP    ID2627  SCR04
SCAXSF    QS2728  SCR04
SCAXTX    QT2829  SCR04
SCAXTD    QD2930  SCR04
SCAX1T    Q13031  SCR04
SCAX2T    Q23132  SCR04
SCAX3T    Q33233  SCR04
SCAX4T    Q43334  SCR04
SCAX5T    Q53435  SCR04
SCAX6T    Q63536  SCR04
SCAX7T    Q73637  SCR04
SCAXPT    PP3751  SCR04                                          Chg 06/22/18
SCCOLB    HD5152
SCCOLP    ID5253
SCCOLT    ID53
afterSCR04               to break up screens
SCSHTR    HD2122  SCR05
SCSFTR    FD2223  SCR05
SCSITR    ID2324  SCR05
SCSQTR    QD2425  SCR05
SCSPTR    PP2526  SCR05
SCSRTR    QT2627  SCR05
SCSPOF    BP2728  SCR05
SCSRPO    FT28    SCR05
