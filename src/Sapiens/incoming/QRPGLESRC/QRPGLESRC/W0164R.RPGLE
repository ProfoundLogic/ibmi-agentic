     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H Dftactgrp(*no) ActGrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W0164R - UPDATE CANCELLATIONS & PRINT NOTICE             *
     H*****************************************************************
     H*                                                               *
     H*  FILES USED:                                                  *
     H*                                                               *
     H*  WXCNL3   PCN01  CO#/FND/BAT#/ENAM/DATE - CX NOTICE WORKFILE  *
     H*  WXCNL4   PCN02  CO#/FND/BAT#/EMP#/DIV  - CX NOTICE WORKFILE  *
     H*  WMEML    PEM01  CO#/FND/EMP#/DIV       - EMPLOYER MASTER     *
     H*  WMFNL    PFN01  CO#/FND                - FUND MASTER         *
     H*  WDELL    PEL01  CO#/FND/EMP#/DIV/FYR   - EMPLOYER DETAIL     *
     H*  SMCOL    PCO01  CO#                    - COMPANY MASTER      *
     H*  WMAHL    PCO01  CO#/FND/EMP#/DIV/FYR/EFDT/AGT#               *
     H*                                         - AGENT HISTORY       *
     H*  SSYLF    SSY01  KEY                    - SYSTEM CONTROL      *
     H*           SWC01  KEY                    - WORKERS COMP CNTRL  *
     H*  WXCCL    PCC01  CO#/FND/BAT#           - CX BATCH CRITERIA   *
     H*  WDCXL    PCX01  CO#/FND/FYR/EMP#/DIV/REQ#/CDAT               *
     H*                                         - CANCELLATION DETAIL *
     H*  WDXHL    PXH01  CO#/FND/FYR/EMP#/DIV/REQ#/TRDT/DT/TIME       *
     H*                                         - CX HISTORY          *
     H*  SMORL    POR01  PNAM/FIL#              - OUTPUT ROUTING      *
     H*  WMUPL    PUP01  USR                    - USER PARAMETERS     *
     H*  WASRL2   PSR01  CO#/FND/FYR/EMP#/DIV/RPMY/CLAS/EFDT          *
     H*                                         - SELF-RPT PAYROLL    *
     H*  WMBXL1   PBX01  CO#/FND/BSCD           - BILL SCHEDULE AUX   *
     H*  WTABL    PCR01  CO#/KEY                - CX REASON TABLE     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 08 - BATCH MODE                                               *
     H* 09 - DEMAND MODE                                              *
     H* 21 - PRINT SECOND FUND ADDRESS LINE                           *
     H* 25 - PRINT SECOND EMPLOYER ADDRESS LINE                       *
     H* 41 - EMPLOYER DETAIL CHAIN                                    *
     H* 42 - NOT SELECTED FOR CANCELLATION                            *
     H* 44 - INCLUDE ON MAILING LIST                                  *
     H* 45 - INCLUDE ON SUSPENSE LIST                                 *
     H* 50 - CANCELLATION WORKFILE READ                               *
     H* 51 - EMPLOYER MASTER CHAIN                                    *
     H* 57 - SELF-REPORTING PAYROLL AMOUNT <> 0                       *
     H* 58 - AGENT HISTORY READ                                       *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H* 88 - PROCESS AS A REVISED CANCELLATION                        *
     H* 89 - CANCELLATION DATE ALREADY EXISTS FOR EMPLOYER            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/28/25 C:062 Project: 153WBJ                                *
     H*                Populate batch# and late fee.                  *
     H*                                                               *
     H* 06/13/22 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Only load C7RPMY or C7WEDT, not both.          *
     H*                                                               *
     H* 02/17/21 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Update both WXC7P/C7WEDT and C7RPMY when       *
     H*                Frequency is Monthly or Weekly.                *
     H*                                                               *
     H* 09/16/19 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Load week ending date into WXC7P/C7WEDT and    *
     H*                WDCXP/CXWEDT for Mass batch S/R cancellations. *
     H*                                                               *
     H* 08/29/19 C:300 Support: 80214 (FMIC)                          *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Load missing reporting period value into       *
     H*                WXC7P/C7RPMY for S/R cancellations.            *
     H*                                                               *
     H* 02/16/18 C:136 Support: 64432 (FCC)
     H*                Save Output from OutputWindow in SPILETTER
     H*                uses position 190 of LDA.  If set, it
     H*                causes Scriptura to fail when processing a mass
     H*                run. Adjusted ldMassFile in LDA to use
     H*                different positions.
     H*
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 01/13/17 C:136 Project: SY371                                 *
     H*                Don't abend if Scriptura fails on PDSDEV       *
     H*                                                               *
     H* 10/06/16 C:65  Project: SY371                                 *
     H*                Submit the scriptura job if Scriptura data has *
     H*                been generated during the W7166C calls.        *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/23/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 08/17/11 C:190 PROJECT WC2019                                 *
     H*                 Pull the cancellation notice days and         *
     H*                 additional mailing days from the new          *
     H*                 fields for new policies.                      *
      *
      * 06/21/11  C:255  Support: 77019 WA133                         *
      *                  Populate Cancellation count when file WDCXP  *
      *                  gets updated (CXCNC#)                        *
      *                                                               *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *
      * 07/24/08 C:61  PROJECT 46WGC
      *                Enable printing agent labels.
      *
      * 04/16/08 C:61  PROJECT WC1620
      *                Write flag to indicate "current Estimated",
      *                prior year Audited, or Both.
      *
      * 12/28/07 C:136 SUPPORT: WA129 61644
      *                MASS CX IS NOT DELETING FUTURE A/R. CORRECTED
      *                POPULATON OF PARMS IN CALL TO W7166C FOR
      *                RENEWAL YEARS
      *
      * 07/03/07 C:190 PROJECT 71WIH
      *          C:61  Populate Mass CX Build Work File(WXCNP) sort
      *                field(CNSORT) based upon user selections indi-
      *                cated in new entry parameters(QQSRT1,QQSRT2)
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA099/44610                              *
     H*                Release record lock                            *
      *
      * 06/01/06 C:61  SUPPORT WA133 52980
      *                Delete CX batch detail records AFTER printing.
      *
      * 09/02/05 C:61  PROJECT 129WLT  WC1359
      *              - Process Cancellations in agent number order.
      *              - Change to ouput report to QSYSPRT4 instead of
      *                QSYSPRT.
      *
      * 07/25/05 C:61  PROJECT 133WBV
      *                Initialize "Number of Cancellations" in
      *                CX detail WDCXP and CX History WDXHP.
      *
      * 08/03/04 C:61  PROJECT WC966
      *                - Cancel renewal years.
     H*                                                               *
     H* 07/26/04 C:53  PROJECT: WC1235                                *
     H*                Add POL#/Federal name parm for W7331C          *
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*                Change text on routing pages for               *
     H*                "Minumum balance" or "% of premium".           *
     H*                                                               *
     H* 04/07/03 C:61  PROJECT 129WAG                                 *
     H*          C:136 Load rating state to CX Detail WDCXP.          *
     H*                                                               *
     H* 03/21/03 C:136 SUPPORT WA121 36533                            *
     H*                Print Zip Code with leading Zeros.             *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify reports for vendor processing.          *
     H*                                                               *
     H* 11/04/02 C:61  SUPPORT WA095 34615                            *
     H*                Do not print Emp on Mailing or Suspense list   *
     H*                if not cancelled.                              *
     H*                                                               *
     H* 08/27/02 C:61  PROJECT WC577                                  *
     H*                Print correct CX reason description.           *
     H*                                                               *
     H* 08/16/01 C:61  SUPPORT 28648                                  *
     H*                Use correct policy year record for multi       *
     H*                division cancellations.                        *
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987                               *
     H*                  CHECK FOR ALTERNATE COMPANY NAME FNANAM, IF  *
     H*                  NE BLANKS LOAD IN PLACE OF COMPANY NAME.     *
     H*                  MODIFIED ROUTING PAGE TO PRINT ON ONE PAGE.  *
     H*                                                               *
     H* 02/01/00  C:35   PROJECT: SUPPORT WA095 #21531                *
     H*                  WHEN PROCESSING A REVISED CANCELLATION, THE  *
     H*                  ISSUE DATE WAS NOT BEING UPDATED AND THE     *
     H*                  REQUEST NUMBER WAS NOT BEING PASSED TO       *
     H*                  W7166C.  ALSO, REVISED FLAG WAS BEING SET TO *
     H*                  'Y' INSTEAD OF 'R'.                          *
     H*                                                               *
     H* 12/02/99  C:90   PROJECT: WC912                               *
     H*                  ADDED 'BALANCE TYPE' (CCEAFL) TO THE ROUTING *
     H*                  PAGES.                                       *
     H*                                                               *
     H* 10/21/99  C:87   PROJECT: WC577                               *
     H*                  If cx eff date > ELPTDT and no later policy  *
     H*                  year - don't process.                        *
     H*                                                               *
     H* 10/20/99  C:87   PROJECT: WC577                               *
     H*                  Assign initial maintenance fields when       *
     H*                  writing to WDCXP.                            *
     H*                                                               *
     H* 10/07/99  C:87   PROJECT: WC894                               *
     H*                  Chg 'Balance date' to 'Billings through dt'  *
     H*                  and add 'Payments through dt' to all of the  *
     H*                  routing pages.                               *
     H*                                                               *
     H* 09/24/99 C:61  PROJECT WC577                                  *
     H*                Add employer phone # to suspense list.         *
     H*                                                               *
     H* 07/29/99  C:87   PROJECT: WC757                               *
     H*                  If an employer is selected for cancellation  *
     H*                  then process all divs of that employer for   *
     H*                  cancellation using the same criteria. Do     *
     H*                  this even if other div has not been selected *
     H*                  for canx.  Also, do not call Address Label   *
     H*                  Print (W7331C) if QQTYPE is blank.           *
     H*                                                               *
     H* 05/05/99  C:87   PROJECT: SUPPORT WA091 #17726                *
     H*                  Pgm was writing a request number equal to 0, *
     H*                  chg'd code to correct.                       *
     H*                                                               *
     H* 02/24/99  C:87   PROJECT: SUPPORT TASB #NONE                  *
     H*                  Change to use the current date as opposed to *
     H*                  the policy eff date when reading the canx    *
     H*                  reason code table file(WTCRP).               *
     H*                                                               *
     H* 12/08/98  C:35   PROJECT: SUPPORT WA081 #15436                *
     H*                  CHANGE KEY NAME TO STANDARD.                 *
     H*                                                               *
     H* 11/24/98  C:87   PROJECT: WC757                               *
     H*                  If there is a canx date in emp master greater*
     H*                  than the canx eff date, and there is a non   *
     H*                  reinstated canx detail record for the emp#   *
     H*                  w/ a matching canx eff date: then update     *
     H*                  emp master w/ new canx date and process the  *
     H*                  cancellation as a revised canx (i.e. update  *
     H*                  canx detail instead of writing new rec,      *
     H*                  flagging it and canx history as revised canx.*
     H*                                                               *
     H* 11/13/98  C:87   PROJECT: SUPP WA081 15052                    *
     H*                  Clear out cancellation detail file before    *
     H*                  loading fields.                              *
     H*                                                               *
     H* 11/10/98  C:87   PROJECT: WC852                               *
     H*                  Modify calc of canx eff date to look at all  *
     H*                  divs being cancelled. For each div, get the  *
     H*                  record from the Canx Rsn code file (WTCRP)   *
     H*                  based on the jurisdicton state, pol effective*
     H*                  date and canx reason code. Use the values in *
     H*                  WTCRP and employer detail to determine the # *
     H*                  of days to add to the issue date to determine*
     H*                  the Cx eff date. Take the highest result of  *
     H*                  all divs to get the cx eff dt for all divs.  *
     H*                  Remove all references to WTCXP. Change to use*
     H*                  WTCRL instead of WTABL to get the canx rsn   *
     H*                  code.  Add parm for the Certified Notif-     *
     H*                  ication flag to call to W7166C.              *
     H*                                                               *
     H* 09/11/98  C:35   PROJECT: SUPPORT WA090 13624                 *
     H*                  CORRECT PRINTING OF CX EFFECCTIVE DATE ON    *
     H*                  CANCELLATION SUSPENSE LIST.  ALSO, DO NOT    *
     H*                  PRINT CX EFF DT ON REPORTS LISTING EMPLOYERS *
     H*                  WHICH WERE NOT CANCELLED.                    *
     H*                                                               *
     H* 09/10/98  C:35   PROJECT: SUPPORT WA095 12537                 *
     H*                  NEED TO RESET CANCELLATION EFFECTIVE DATE    *
     H*                  FOR EACH RECORD PROCESSED IF IT WAS ENTERED  *
     H*                  ON THE PROMPT SCREEN.                        *
     H*                                                               *
     H* 06/30/98  C:35  PROJECT: SUPPORT WA091                        *
     H*                 IF CANCELLATION EFFECTIVE DATE IS GREATER     *
     H*                 THE POLICY TO DATE OF THE ACCOUNT BEING       *
     H*                 PROCESSED, CHECK FOR THE NEXT POLICY YEAR.    *
     H*                 IF THERE IS ONE, USE THAT YEAR, OTHERWISE USE *
     H*                 TO POLICY TO DATE AS THE CANCELLATION         *
     H*                 EFFECTIVE DATE.                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      *
     F***  BEGIN ADD  ***  07/03/07  **********************************
     FWXCNL7    UF   E           K DISK
     F                                     RENAME(PCN01:PCN07)
     F***  END   ADD  ***  07/03/07  **********************************
     FWXCNL3    UF   E           K DISK
     F***  BEGIN ADD  ***  11/10/98  **********************************
     F***  BEGIN CHG  ***  07/29/99  **********************************
     F***WXCNL4  IF  E           K        DISK
     FWXCNL4    UF   E           K DISK
     F***  END   CHG  ***  07/29/99  **********************************
     F                                     RENAME(PCN01:PCN02)
     F***  END   ADD  ***  11/10/98  **********************************
     FWMEML     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWXCCL     UF   E           K DISK
     FWDCXL     IF A E           K DISK
     F***  BEGIN ADD  ***  11/24/98  **********************************
     F***  Begin Chg  ***  01/03/07  **************************************
     F**********WDCXL5  UF  E           K        DISK
     F**********  PCX01                             KRENAMEPCX02
     FWDCXL5    IF   E           K DISK
     F                                     RENAME(PCX01:PCX05)
     F                                     INFDS(CXFDS5)
     FWDCXP     UF   E             DISK
     F                                     RENAME(PCX01:PCX0P)
     F***  End   Chg  ***  01/03/07  **************************************
     F***  END   ADD  ***  11/24/98  **********************************
     FWDXHL     O    E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWASRL2    IF   E           K DISK
     F*** BEG ADD *******  02/18/03  **********************************
     FWASRL10   IF   E           K DISK
     F                                     RENAME(PSR01:PSR02)
     F*** END ADD *******  02/18/03  **********************************
     FWMBXL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTXCL   IF  E           K        DISK
     FWTCRL     IF   E           K DISK
     F***  END   CHG  ***  11/10/98  **********************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PRS01)
      ***** BEG ADD ******* 09/02/05 **********************************
      *
      *  Mass CX issue work file
     FWXC6P     O    E             DISK
     F***  BEG DLT  ***  07/03/07  **********************************
     F***WXC6L   UF  E           K        DISK
     F***         PC601                             KRENAMEPC602
     F***  END DLT  ***  07/03/07  **********************************
      *
     F***  BEG ADD  ***  07/03/07  **********************************
     FWXC6L1    UF   E           K DISK
     F                                     RENAME(PC601:PC603)
     F***  END ADD  ***  07/03/07  **********************************
      *
      ***** END ADD ******* 09/02/05 **********************************
      *
      *  ERROR LIST - NO FYR FOUND, EMPLR NOT CANCELLED
      *
      ***** BEG CHG ******* 09/02/05 **********************************
     F***** QSYSPRT O   F     127     OD     PRINTER
     FQSYSPRT4  O    F  127        PRINTER OFLIND(*INOD)
      ***** END CHG ******* 09/02/05 **********************************
      *
      *  LIST EMPLOYERS NOT CANCELLED BECAUSE OF CX STATUS
      *
     FQSYSPRT1  O    F  127        PRINTER OFLIND(*INOA)
      *
      *  LIST EMPLOYERS ON CANCELLATION MAILING LIST
      *
     FQSYSPRT2  O    F  127        PRINTER OFLIND(*INOB)
      *
      *  LIST EMPLOYERS ON CANCELLATION SUSPENSE LIST
      *
     FQSYSPRT3  O    F  127        PRINTER OFLIND(*INOC)
      *

     D CLS             S              5  0 DIM(999)
     D AMT             S             11  2 DIM(999)
      ***** END ADD ******* 07/24/08 **********************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
      *
     D                 DS
     D  XXSTDT                 1      5  0
     D  XXSTYR                 1      3  0
     D  XXSTMN                 4      5  0
      * Comparison date for stopping search of payroll
      *
     D***  Begin Add  ***  01/03/07  **************************************
     D CXFDS5          DS
     D  CXRRN5               397    400B 0
     D***  End   Add  ***  01/03/07  **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** begin add ***  10/06/16  ****************************************
     D svlda           ds          1024    dtaara(*lda)
      *** beg chg ***** 02/16/18 *****************************
     D*** ldMassFile                    60    Overlay(svlda:131)
     D ldMassFile                    60    Overlay(svlda:129)
      *** end chg ***** 02/16/18 *****************************
     D*** end add *****  10/06/16  *****************************************
     D*** beg add ***** 01/13/17 *************************
     D S4401C          pr                  ExtPgm('S4401C')
     D   xmlName                     61    const
     D ObjExists       pr             1n
     D  object                       10    const
     D  objType                       7    const
     D*** end add ***** 01/13/17 *************************

     I*
     I***  BEGIN ADD  ***  11/10/98  **********************************
     IPCN02
     I              CNCO#                       C2CO#
     I              CNFND                       C2FND
     I              CNEMP#                      C2EMP#
     I              CNDIV                       C2DIV
     I              CNDATE                      C2DATE
     I              CNAMTD                      C2AMTD
     I              CNSELC                      C2SELC
     I              CNSTAT                      C2STAT
     I              CNRECD                      C2RECD
     I              CNCOM1                      C2COM1
     I              CNCOM2                      C2COM2
     I              CNBAT#                      C2BAT#
     I              CNVIP                       C2VIP
     I              CNENAM                      C2ENAM
     I              CNSRYM                      C2SRYM
     I              CNFYR                       C2FYR
     I              CNADJ                       C2ADJ
     I              CNUSER                      C2USER
     I              CNPGM                       C2PGM
     I              CNTIME                      C2TIME
     I              CNDT                        C2DT
     I              CNDOI#                      C2DOI#
     I              CNCEMN                      C2CEMN
     I              CNCNC#                      C2CNC#
     I              CNRFEE                      C2RFEE
     I              CNRFDS                      C2RFDS
     I              CNCAP#                      C2CAP#
     I***  END   ADD  ***  11/10/98  **********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     I              CNSORT                      C2SORT
      *
      ***** BEG ADD ******* 04/07/03 **********************************
      *
      *===============================================================
      *  KLIST
      *===============================================================
      ***** BEG DLT ******* 07/03/07 **********************************
      ******BEG ADD ******* 09/02/05 **********************************
      *******
      ***Mass CX Issue Work File
      *******

     C*******    C6KY      KLIST
     C*******              KFLD           XXCO#
     C*******              KFLD           XXFND
      *******
      ***** END ADD ******* 09/02/05 **********************************
      ***** END DLT ******* 07/03/07 **********************************
      *
      ***** BEG ADD ******* 07/03/07 **********************************
      *  Mass CX Issue Work File
      *
     C     C6KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    KKBAT#
      *
      ***** END ADD ******* 07/03/07 **********************************
      *
      *  EMPLOYER POLICY YEAR
      *
     C     ELKY3         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
      ***** END ADD ******* 04/07/03 **********************************
      *
      * KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      ***** BEG ADD *****  07/03/07  **********************************
      *
     C     EMKY1         KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
      ***** END ADD *****  07/03/07  **********************************
      *
      * KEY LIST FOR THE CANCELLATION WORK FILE
      *
     C     CNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
      **** BEGIN CHG **** 12/08/98 *********************************************
     C********** C2KY      KLIST
     C     CNKY1         KLIST
      **** END   CHG **** 12/08/98 *********************************************
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXEMP#
     C***  END   ADD  ***  11/10/98  **********************************
      *
      * KEY LIST FOR TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR BILLING SCHEDULE CODE AUXILIARY FILE
      *
     C     BXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBSCD
      *
      * KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      * KEY LIST FOR THE OUTPUT ROUTING FILE
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
      *
      * KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      * KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LIST FOR THE CANCELLATION SELECTION CRITERIA WORKFILE
      *
     C     CCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
      *
      * KEY LIST FOR THE SELF-REPORTING TRANSACTION FILE
      *
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C                   KFLD                    XXSTDT
      *
      *** BEG ADD ***** 02/18/03 ******************************
     C     SRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C                   KFLD                    X1STDT
      *** END ADD ***** 02/18/03 ******************************
      *
      * KEY LISTS FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C                   KFLD                    XXFYR
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C                   KFLD                    XXFYR
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    XXFYR
     C***  END   ADD  ***  11/10/98  **********************************
      ***** BEG ADD ***** 08/03/04 ************************************
      *
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     ELKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      ***** END ADD ***** 08/03/04 ************************************
      *
      * KEY LIST FOR THE AGENT HISTORY MASTER FILE
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * KEY LIST FOR THE CANCELLATION DETAIL FILE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C     CXKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCXDT
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***  END   ADD  ***  11/24/98  **********************************
      ***** BEG ADD ***** 08/03/04 ************************************
      *
     C     CXKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
      ***** END ADD ***** 08/03/04 ************************************
      *
      * KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSKEY
      *
     C***  BEGIN CHG  ***  11/10/98  **********************************
      *  KEY LIST FOR THE CANCELLATION/REINSTATEMENT STATE FILE
      **********
     C********** XCKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXFND
     C**********           KFLD           XXST
     C**********           KFLD           XXEFDT
      **********
     C********** XCKY2     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXFND
     C**********           KFLD           XXST
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C***  END   CHG  ***  11/10/98  **********************************
      *
      *  KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
      *
      **************************************************************
      *
      *
      *===============================================================
      *  *LIKE DEFN
      *===============================================================
      ***** BEG ADD ***** 08/03/04 ************************************
     C     *LIKE         DEFINE    CXREQ#        X1REQ#
      ***** END ADD ***** 08/03/04 ************************************
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C***  BEGIN CHG  ***  11/24/98  **********************************
     C*********  *LIKE     DEFN DSDATE    XXSYDT
     C     *LIKE         DEFINE    XHDT          XXSYDT
     C***  END   CHG  ***  11/24/98  **********************************
     C     *LIKE         DEFINE    CNBAT#        XXBAT#
     C***  BEG   ADD  ***  07/03/07  **********************************
     C     *LIKE         DEFINE    C6BAT#        KKBAT#
     C***  END   ADD  ***  07/03/07  **********************************
     C     *LIKE         DEFINE    EMFDNM        SVFDNM
     C     *LIKE         DEFINE    EMMAD1        SVADR1
     C     *LIKE         DEFINE    EMMAD2        SVADR2
     C     *LIKE         DEFINE    EMMCTY        SVCITY
     C     *LIKE         DEFINE    EMMST         SVST
     C     *LIKE         DEFINE    EMMZIP        SVZIP
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    CNCO#         XXCO#
     C     *LIKE         DEFINE    CNFND         XXFND
     C     *LIKE         DEFINE    CNEMP#        XXEMP#
     C     *LIKE         DEFINE    CNDIV         XXDIV
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    BXBSCD        XXBSCD
     C     *LIKE         DEFINE    SYKEY         XXSKEY
     C     *LIKE         DEFINE    CRKEY         XXKEY
     C     *LIKE         DEFINE    CXCXDT        XXCEFD
     C     *LIKE         DEFINE    RSJRST        XXST
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** *LIKE     DEFN XCEFDT    XXEFDT
     C     *LIKE         DEFINE    CREFDT        XXEFDT
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    CNDIV         KKDIV
     C     *LIKE         DEFINE    CNRECD        KKRECD
     C***  END   CHG  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C     *LIKE         DEFINE    CXCXDT        XXCXDT
     C     *LIKE         DEFINE    CXREQ#        ZZREQ#
     C***  END   ADD  ***  11/24/98  **********************************
      *
     C                   Z-ADD     0             XXCNT1            7 0          RECORD COUNT FOR RPT
     C                   Z-ADD     0             XXCNT2            7 0
     C                   Z-ADD     0             XXCNT3            7 0
     C                   Z-ADD     0             XXCNT4            7 0
     C                   Z-ADD     0             XXPAG1            7 0          PAGE COUNT FOR RPTS
     C                   Z-ADD     0             XXPAG2            7 0
     C                   Z-ADD     0             XXPAG3            7 0
     C                   Z-ADD     0             XXPAG4            7 0
     C                   Z-ADD     0             X@DATE            8 0
     C                   Z-ADD     0             X@CDAT            8 0
     C                   Z-ADD     0             XXDATE            7 0
     C                   Z-ADD     0             XXCDAT            7 0
     C                   Z-ADD     0             XXSRMY            6 0
     C                   Z-ADD     0             XXBLDT            8 0
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C                   Z-ADD     0             XXPTDT            8 0
     C***  END   ADD  ***  10/07/99  **********************************
     C                   Z-ADD     0             XXMTHS            3 0
     C                   Z-ADD     0             X2                3 0
     C                   Z-ADD     0             CNT               5 0
     C                   Z-ADD     0             Y1                5 0
     C                   Z-ADD     0             XXCPYM            5 0
     C                   Z-ADD     0             XAMT             11 2
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   Z-ADD     0             XXDAYS            5 0
     C                   Z-ADD     0             ZZDAYS            5 0
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C                   MOVE      *BLANKS       XXFNNM           60
     C                   MOVE      *BLANKS       XXFNMC           60
     C                   MOVE      *BLANKS       XXFAD1           40
     C                   MOVE      *BLANKS       XXFAD2           40
     C                   MOVE      *BLANKS       XXFCSZ           40
     C                   MOVE      *BLANKS       XXZIP1            5
     C                   MOVE      *BLANKS       XXZIP2            4
     C                   MOVE      *BLANKS       XREASN           50
     C                   MOVE      *BLANKS       XXCITY           30
      *
      **************************************************************
      *
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            8            CX ISSUE
     C                   PARM                    QQCDAT            8            CX EFFECTIVE
     C                   PARM                    QQBAT#            9            BATCH#
     C                   PARM                    QQCCRT            1            CERT HOLDER FLAG
     C                   PARM                    QQTYPE            1            LABEL TYPE
     C                   PARM                    QQFORM            1            LABEL FORM
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQMODE            1
     C***  BEG ADD  ***  07/03/07  **********************************
     C                   PARM                    QQSRT1            2
     C                   PARM                    QQSRT2            2
     C***  END ADD  ***  07/03/07  **********************************
      *
      * PARAMETER LISTS FOR DATE CONVERSION PROGRAM SRCVTDAT
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      ***** BEG DEL ******* 09/02/05 **********************************
      *********
      ***CALL*LIST FOR CANCELLATION NOTICE PRINT PROGRAM (W7166C)
      *********
     C*********  P7166     PLIST
     C*********            PARM           XQCO#   3
     C*********            PARM           XQFND   3
     C*********            PARM           XQFYR   3
     C*********            PARM           XQEMP#  9
     C*********            PARM           XQDIV   5
     C*********            PARM           XQREQ#  5        REQUEST#
     C*********            PARM           XQCCRT  1        CERT HOLDER FLAG
     C*********            PARM 'N'       XQREPR  1        REPRINT FLAG
     C*********            PARM QQOUTQ    XQOUTQ 10
     C*********            PARM           QQBAT#
     C*********            PARM           QQTYPE
     C*****BEGIN ADD  ***  11/10/98  **********************************
     C*********            PARM *BLANK    QQCERT  1
     C*****END*  ADD  ***  11/10/98  **********************************
      ***** END DEL ******* 09/02/05 **********************************
      ***** BEG ADD ******* 07/24/08 **********************************
      *
     C                   MOVE      *BLANK        XXLABL            1
      *
     C                   SELECT
      *
     C     QQTYPE        WHENEQ    'A'
     C     QQTYPE        OREQ      'E'
     C     QQTYPE        OREQ      'B'
     C                   MOVE      QQTYPE        XXLABL
     C                   MOVE      'R'           QQTYPE
      *
     C     QQTYPE        WHENEQ    'F'
     C                   MOVE      'L'           QQTYPE
     C                   MOVE      'A'           XXLABL
      *
     C     QQTYPE        WHENEQ    'G'
     C                   MOVE      'L'           QQTYPE
     C                   MOVE      'E'           XXLABL
      *
     C     QQTYPE        WHENEQ    'H'
     C                   MOVE      'L'           QQTYPE
     C                   MOVE      'B'           XXLABL
      *
     C                   ENDSL
      *
      ***** END ADD ******* 07/24/08 **********************************
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDATE        X@DATE
     C                   MOVE      QQCDAT        X@CDAT
     C                   Z-ADD     X@DATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     X@CDAT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         XXCDAT
      *
     C     XXCDAT        IFNE      0
     C                   MOVE      XXCDAT        XXCEFD
     C                   ENDIF                                                  XXCDAT IFNE 0
      *
     C                   MOVE      QQBAT#        XXBAT#
     C*** BEG ADD ***** 07/03/07 **************************************
     C                   MOVE      QQBAT#        KKBAT#
     C*** END ADD ***** 07/03/07 **************************************
      *
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF                                                  QQMODE IFEQ
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C                   MOVEL     'WC'          XXSKEY
      *
     C     SYKY          CHAIN     SWC01                              60
      *
     C                   MOVE      *BLANK        SYOFIF
     C                   MOVEL     'SY'          XXSKEY
      *
     C     SYKY          CHAIN     SSY01                              60
      *
     C*----------------------------------------------------------------
     C*   GET COMPANY, FUND, AND ROUTING INFO. PRINT ROUTING PAGES
     C*----------------------------------------------------------------
     C     COKY          CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     CCKY          CHAIN(N)  PCC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C***  BEG ADD ******  02/18/03  **********************************
     C***  BEG CHG ******  02/17/21  **********************************
     C***  CCFREQ        IFEQ      *BLANKS
     C***                SETOFF                                       46
     C***  END ADD ******  02/18/03  **********************************
     C***                MOVE      CCRPMY        SRDTI
     C***                CALL      'SRCVTMY'     DTMDYY
     C***                MOVEL     SRDTO         XXSRMY
      *
     C***  BEG ADD ******  02/18/03  **********************************
     C***                ELSE
     C***                SETON                                        46
     C***                Z-ADD     CCWEDT        SRDTI
     C***                CALL      'SRCVTDAT'    DTMDYY
     C***                MOVEL     SRDTO         XXWEDT            8 0
     C***                ENDIF
     C     CCFREQ        IFEQ      *BLANKS
     C                   SETOFF                                       46
     C                   ELSE
     C                   SETON                                        46
     C                   ENDIF
      *
     C                   MOVE      CCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         XXSRMY
      *
     C                   Z-ADD     CCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXWEDT            8 0
     C***  END CHG ******  02/17/21  **********************************
     C***  END ADD ******  02/18/03  **********************************
      *
      *
     C                   Z-ADD     CCBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBLDT
     C*
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C                   Z-ADD     CCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT
     C***  END   ADD  ***  10/07/99  **********************************
      ***** BEG CHG ******* 05/02/03 **********************************
     C     CCADPP        COMP      'P'                                    71
      ***** END CHG ******* 05/02/03 **********************************
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      * GET FUND MASTER & DETAIL INFO
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C                   MOVEL     FNNAME        XXFNNM
     C                   MOVEL(P)  FNNAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXFNMC
     C                   MOVEL     FNMAD1        XXFAD1
     C                   MOVEL     FNMAD2        XXFAD2
     C     XXFAD2        COMP      *BLANK                             2121
     C     FNMCTY        CAT(P)    ',':0         XXFCSZ
     C                   CAT       FNMST:1       XXFCSZ
     C                   MOVEL     FNMZIP        XXZIP1
     C                   MOVE      FNMZIP        XXZIP2
     C                   CAT       XXZIP1:1      XXFCSZ
      *
     C     XXZIP2        IFNE      '0000'
      *
     C     XXFCSZ        CAT       '-':0         XXFCSZ
     C     XXFCSZ        CAT       XXZIP2:0      XXFCSZ
      *
     C                   ENDIF                                                  XXZIP2 IFNE 0
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      *  PRINT ROUTING PAGE FOR REPORT 1
      *
      ****** BEG CHG  ***  07/03/07  **********************************
     C**********           MOVE '0'       XXPRT
     C                   MOVE      '4'           XXPRT
      ****** BEG CHG  ***  07/03/07  **********************************
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C************N60              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      *ON           *INOD
      *
      *  PRINT ROUTING PAGE FOR REPORT 2
      *
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C************N60              EXCEPT    ROUTE2
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      *ON           *INOA
      *
      *  PRINT ROUTING PAGE FOR REPORT 3
      *
     C                   MOVE      '2'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C***********N60              EXCEPT    ROUTE3
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      *ON           *INOB
      *
      *  PRINT ROUTING PAGE FOR REPORT 4
      *
     C                   MOVE      '3'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE4
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      *ON           *INOC
     C*
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C                   EXSR      PRECX
     C***  BEGIN ADD  ***  07/03/07  **********************************
     C                   EXSR      SRSORT
     C***  END   ADD  ***  07/03/07  **********************************
     C***  END   ADD  ***  07/29/99  **********************************
      *
     C***  BEGIN DLT  ***  07/03/07  **********************************
      *********  READ THE CANCELLATION RECORDS
      *********
     C*********  CNKY      SETLLPCN01
      *********
     C*********  *IN50     DOUEQ*ON
      *********
     C*********  CNKY      READEPCN01                    50
      *********
     C*********  *IN50     IFEQ *OFF
     C*****END DLT  ***  07/03/07  **********************************
     C***  BEG ADD ******  07/03/07  **********************************
      *
      *  READ THE CANCELLATION RECORDS
      *
     C     CNKY          SETLL     PCN07
      *
     C     *IN50         DOUEQ     *ON
     C     CNKY          READE     PCN07                                  50
      *
     C     *IN50         IFEQ      *OFF
     C**** END ADD  ***  07/03/07  **********************************
      ***** BEG ADD ******* 11/04/02 **********************************
     C                   MOVE      'N'           X1CX              1
      ***** END ADD ******* 11/04/02 **********************************
      *
     C                   MOVE      CNFYR         XXFYR
      *
     C     CNSELC        IFEQ      'Y'
      *
      *  UPDATE THE EMPLOYER RECORDS
      *
     C                   MOVE      CNEMP#        XXEMP#
     C                   Z-ADD     CNDIV         XXDIV
      *
     C     EMKY          CHAIN     PEM01                              51        CO/FND/EMP/DIV
      *
      * GET THE EMPLOYER DETAIL RECORD.  IF NO EMPLOYER DETAIL RECORDS
      * IS FOUND, LIST ON ERROR REPORT.
      *
     C     ELKY1         CHAIN     PEL01                              41
      *
     C     *IN41         IFEQ      *ON
      *
     C     *INOD         IFEQ      *ON
     C                   ADD       1             XXPAG1
     C                   EXCEPT    HEAD1                                        CHECK FOR OVERFLOW
     C                   MOVE      *OFF          *INOD
     C                   ENDIF                                                  *INOD IFEQ *ON
      *
     C                   EXCEPT    DET1
      *
     C                   ADD       1             XXCNT1                         COUNT RECORDS
      *
     C                   GOTO      NEXT                                         DON'T CANCEL
      *
     C                   ENDIF                                                  *IN41 IFEQ *ON
      *
      * IF NO CANCELLATION EFFECTIVE DATE WAS ENTERED, CALCULATE IT
      * BASED ON THE STATE CX DEFINITION FOR THE EMPLOYER'S JURISDICTION
      * STATE.  DEFAULT TO THE ISSUE DATE IF NOT STATE INFORMATION IS
      * FOUND.
      *
     C     XXCDAT        IFEQ      0
      *
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C**********           MOVE XXDATE    XXCEFD
     C**********           MOVE ELRTCD    XXRTCD
     C**********
     C********** RSKY      CHAINPRS01                60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********
     C**********           MOVE RSJRST    XXST
     C**********
      * ADD NOTICE DAYS AND ADDITIONAL MAILING DAYS TO CURRENT DATE TO
      * GET DEFAULT CANCELLATION DATE.  IF COMPUTED DATE IS PRIOR TO
      * THE POLICY EFFECTIVE DATE, USE THE POLICY EFFECTIVE DATE.  IF THE
      * COMPUTED DATE IS GREATER THAN THE POLICY EXPIRATION DATE, USE THE
      * POLICY EXPIRATION DATE.
     C**********
     C********** XCKY      SETGTPXC01
     C********** XCKY2     REDPEPXC01                    60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********
     C********** XCADDM    ADD  XCNOTC    QQDAYS
     C*
     C                   EXSR      SRDAYS
      *
      ***** BEG ADD ******* 08/16/01 **********************************
     C     ELKY1         CHAIN     PEL01                              41
      ***** END ADD ******* 08/16/01 **********************************
     C*
     C                   MOVE      XXDATE        XXCEFD
     C                   Z-ADD     ZZDAYS        QQDAYS
     C***  END   CHG  ***  11/10/98  **********************************
      *
     C                   CALL      'SRFUTR8'
     C                   PARM      X@DATE        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      '1'
      *
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCEFD
      *
     C     XXCEFD        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXCEFD
     C                   ENDIF                                                  XXCEFD IFLT ELPTDT
      *
      **** BEGIN DEL **** 06/30/98 ************************************
     C********** XXCEFD    IFGT ELPTDT
     C******************** MOVE ELPTDT    XXCEFD
     C******************** ENDIF                           XXCEFD IFGT ELPFDT
      **** END   DEL **** 06/30/98 ************************************
      *
     C                   ENDIF                                                  QQERR IFNE
      *
     C***  BEGIN DEL  ***  11/10/98  **********************************
     C******************** ENDIF                           *IN60 IFEQ *OFF
      ********************
     C******************** ENDIF                           *IN60 IFEQ *OFF
     C***  END   DEL  ***  11/10/98  **********************************
      *
      **** BEGIN ADD **** 09/10/98 *********************************************
     C                   ELSE
      *
     C                   MOVE      XXCDAT        XXCEFD
      **** END   ADD **** 09/10/98 *********************************************
     C                   ENDIF                                                  XXCDAT IFEQ 0
      *
      **** BEGIN ADD **** 06/30/98 ************************************
     C     XXCEFD        IFGT      ELPTDT
      *
     C                   ADD       1             XXFYR
      *
     C     ELKY1         CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     XXFYR         CNFYR
      *
     C                   ELSE
      *
     C                   Z-ADD     CNFYR         XXFYR
     C                   MOVE      ELPTDT        XXCEFD
     C***  BEGIN ADD  ***  10/21/99  **********************************
     C                   GOTO      NEXT
     C***  END   ADD  ***  10/21/99  **********************************
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  XXCEFD IFGT ELPFDT
      **** END   ADD **** 06/30/98 ************************************
      *
      *  LOAD CANCEL EFF DATE TO EMPLOYER MASTER FILE
      *
     C     *IN51         IFEQ      *OFF
     C*
     C***  BEGIN CHG  ***  11/24/98  **********************************
     C**********           Z-ADD1         X       30
      **********
     C********** *ZERO     LOKUPCX,X                     60
      **********
     C********** *IN60     IFEQ *ON
      **********
     C********** EF,X      IFNE 0
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
     C*
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date exists, set canx indicator
     C     XI            IFEQ      *ZERO
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C     CX(XI)        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
     C*
     C***  BEGIN ADD  ***  05/05/99  **********************************
     C                   MOVE      *OFF          *IN88
     C***  END   ADD  ***  05/05/99  **********************************
     C*
     C     *IN89         IFEQ      *ON
     C     CX(XI)        ANDGT     XXCEFD
     C*
     C                   Z-ADD     CX(XI)        XXCXDT
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** CXKY2     SETGTPCX02
     C********** CXKY2     REDPEPCX02                    60
     C                   CLEAR                   CXRRN5
     C     CXKY2         SETGT     PCX05
     C     CXKY2         READPE(N) PCX05                                  60
     C     *LIKE         DEFINE    CXRRN5        CXRRN
     C                   Z-ADD     CXRRN5        CXRRN
     C***  End   Chg  ***  01/03/07  **************************************
     C     *IN60         IFEQ      *OFF
     C     CXREIN        ANDNE     'Y'
     C                   MOVE      *ON           *IN88
     C                   Z-ADD     CXREQ#        ZZREQ#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN89=*ON
     C*
     C*  Process if no cancel date or if cancel date in employer
     C*  master is greater than canx eff date
     C     *IN89         IFEQ      *OFF
     C     *IN89         OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C*
     C***  END   CHG  ***  11/24/98  **********************************
      *
     C                   MOVE      XXCEFD        CX(XI)
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         EMDT
     C                   Z-ADD     DSTIME        EMTIME
      *
     C                   UPDATE    PEM01
      *
     C                   MOVE      XXCEFD        CNDATE
      ***  BEGIN CHG  ***  07/03/07  **********************************
     C*******              UPDATPCN01
     C                   UPDATE    PCN07
      ***  END CHG  *****  07/03/07  **********************************
      *
      *    FIRST GET THE MOST CURRENT "ACTIVE" AGENT HISTORY RECORD.
      *
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  58
      *
     C     *IN58         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
      *
     C     AHKY          READPE    PAH01                                  58
      *
     C                   ENDDO                                                  *IN58 DOWEQ *OF
      *
     C     *IN58         IFEQ      *OFF
     C                   MOVE      AHAGT#        XXAGT#
     C                   ELSE
     C                   Z-ADD     0             XXAGT#
     C                   ENDIF                                                  *IN58 IFEQ *OFF
     C*
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C     *IN88         IFEQ      *OFF
     C***  END   ADD  ***  11/24/98  **********************************
      *
      *  GET NEXT CANCEL REQUEST# AND WRITE A RECORD TO THE
      *  CANCEL DETAIL FILE (WDCXP) .
      *
     C                   Z-ADD     *ZERO         XXREQ#
      *
     C     CXKY          SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     CXREQ#        ADD       1             XXREQ#                         NEXT REQUEST #
     C                   ELSE
     C                   Z-ADD     1             XXREQ#
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C***  BEGIN ADD  ***  11/13/98  **********************************
     C*  Clear record format before loading fields
     C                   CLEAR                   PCX01
     C***  END   ADD  ***  11/13/98  **********************************
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         XXSYDT
     C                   Z-ADD     XXCO#         CXCO#
     C                   Z-ADD     XXFND         CXFND
     C                   Z-ADD     XXFYR         CXFYR
     C                   Z-ADD     XXEMP#        CXEMP#
     C                   Z-ADD     XXDIV         CXDIV
     C                   Z-ADD     XXREQ#        CXREQ#
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C                   Z-ADD     XXREQ#        ZZREQ#
     C***  END   ADD  ***  11/24/98  **********************************
     C                   Z-ADD     XXCEFD        CXCXDT                         CANCEL EFF DATE
     C                   Z-ADD     XXDATE        CXCDAT                         CANCEL NOTICE DATE
     C                   Z-ADD     XXAGT#        CXAGT#
     C                   MOVEL     CNRECD        CXRSN
     C                   MOVEL     CNCOM1        CXCOM1
     C                   MOVEL     CNCOM2        CXCOM2
     C                   MOVE      CNAMTD        CXAMTD
     C                   MOVE      CCRPMY        CXRPMY
     C                   MOVE      CNRFEE        CXRFEE
     C                   MOVE      QQCCRT        CXCCRT
     C                   MOVE      *BLANK        CXCDRT
     C                   MOVE      'N'           CXREIN
     C                   Z-ADD     0             CXREDT
     C                   Z-ADD     0             CXRDAT
     C                   MOVE      'N'           CXRCRT
     C                   MOVE      *BLANK        CXRDRT
     C*** Begin Add *** 07/28/25 *******************************************
     C                   Z-ADD     XXBAT#        CXBAT#
     C                   Z-ADD     CNLFEE        CXLFEE
     C*** End   Add *** 07/28/25 *******************************************
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   Z-ADD     DSTIME        CXTIME
     C                   Z-ADD     XXSYDT        CXDT
     C*
     C***  BEGIN ADD  ***  10/20/99  **********************************
     C                   MOVE      Q1USER        CXIUSR
     C                   MOVE      Q1PGM         CXIPGM
     C                   Z-ADD     DSTIME        CXITM
     C                   Z-ADD     XXSYDT        CXIDT
     C***  END   ADD  ***  10/20/99  **********************************
      *
      ***** BEG ADD ******* 04/07/03 **********************************
     C                   EXSR      SRRST
      ***** END ADD ******* 04/07/03 **********************************
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     CNCNC#        CXCNC#
      ***** END ADD ******* 07/25/05 **********************************
      ***** BEG ADD ******* 04/16/08 **********************************
     C                   MOVE      CCEAFL        CXEAFL
      ***** END ADD ******* 04/16/08 **********************************
      ***** Begin Add ******* 09/16/19 **********************************
     C                   MOVE      CCWEDT        CXWEDT
      ***** End   Add ******* 09/16/19 **********************************
     C                   WRITE     PCX01
     C*
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C                   ELSE                                                   *IN88=*OFF
     C*
     C***  Begin Add  ***  01/03/07  **************************************
     C     CXRRN         CHAIN     WDCXP                              60
     C***  End   Add  ***  01/03/07  **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT
     C*
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   Z-ADD     DSTIME        CXTIME
     C                   Z-ADD     XXSYDT        CXDT
     C*
     C                   Z-ADD     ZZREQ#        XHREQ#
      **** BEGIN ADD **** 02/01/00 *********************************************
     C                   Z-ADD     ZZREQ#        XXREQ#
     C                   Z-ADD     XXDATE        CXCDAT                         CANCEL NOTICE DATE
      **** END   ADD **** 02/01/00 *********************************************
     C                   Z-ADD     XXCEFD        CXCXDT                         CANCEL EFF DATE
     C                   MOVEL     CNRECD        CXRSN
     C                   MOVEL     CNCOM1        CXCOM1
     C                   MOVEL     CNCOM2        CXCOM2
     C                   MOVE      CNAMTD        CXAMTD
     C                   MOVE      CCRPMY        CXRPMY
     C                   MOVE      CNRFEE        CXRFEE
     C*
     C*   Update the canx record w/the NCCI reinstate type
     C                   SELECT
     C     CRNCCI        WHENEQ    '00'
     C                   MOVE      '3'           CXREVL
     C     ELPFDT        WHENEQ    CXCXDT
     C                   MOVE      '1'           CXREVL
     C                   OTHER
     C                   MOVE      '2'           CXREVL
     C                   ENDSL
     C*
      ***** BEG ADD ******* 04/07/03 **********************************
     C                   EXSR      SRRST
      ***** END ADD ******* 04/07/03 **********************************
      ***** Beg Add ******* 06/21/11 **********************************
     C                   Z-ADD     CNCNC#        CXCNC#
      ***** End Add ******* 06/21/11 **********************************
     C***  Begin Chg  ***  01/03/07  **************************************
     C**********           UPDATPCX02
      ***** BEG ADD ******* 04/16/08 **********************************
     C                   MOVE      CCEAFL        CXEAFL
      ***** END ADD ******* 04/16/08 **********************************
     C                   UPDATE    PCX0P
     C***  End   Chg  ***  01/03/07  **************************************
     C*
     C                   ENDIF                                                  *IN88=*OFF
     C***  END   ADD  ***  11/24/98  **********************************
      *
      *  WRITE A RECORD TO THE CANCELLATION HISTORY FILE
      *
     C                   Z-ADD     XXCO#         XHCO#
     C                   Z-ADD     XXFND         XHFND
     C                   Z-ADD     XXFYR         XHFYR
     C                   Z-ADD     XXEMP#        XHEMP#
     C                   Z-ADD     XXDIV         XHDIV
     C***  BEGIN DEL  ***  11/24/98  **********************************
     C*******************  Z-ADDXXREQ#    XHREQ#
     C***  END   DEL  ***  11/24/98  **********************************
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C                   Z-ADD     ZZREQ#        XHREQ#
     C***  END   ADD  ***  11/24/98  **********************************
     C                   Z-ADD     XXAGT#        XHAGT#
     C***  BEGIN CHG  ***  11/24/98  **********************************
     C*******************  MOVE *BLANK    XHRVFL
     C  N88              MOVE      *BLANK        XHRVFL
      **** BEGIN CHG **** 02/01/00 *********************************************
     C******** 88          MOVE 'Y'       XHRVFL
     C   88              MOVE      'R'           XHRVFL
      **** END   CHG **** 02/01/00 *********************************************
     C***  BEGIN CHG  ***  11/24/98  **********************************
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C                   MOVEL     CNRECD        XHRSN
     C                   Z-ADD     XXSYDT        XHTRDT                         SYSTEM DATE
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   MOVE      *BLANK        XHSTAT
     C                   MOVEL     Q1USER        XHUSER
     C                   MOVEL     Q1PGM         XHPGM
     C                   Z-ADD     XXSYDT        XHDT
     C                   Z-ADD     DSTIME        XHTIME
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     CNCNC#        XHCNC#
      ***** END ADD ******* 07/25/05 **********************************
     C                   WRITE     PXH01
      ***** BEG DEL ******* 09/02/05 **********************************
      *********
      ***CALL*PGM TO PRINT NCCI NOTICE, CERT HOLDER LETTER, & WRITE DARTS
      *********
     C*********            MOVE QQCO#     XQCO#
     C*********            MOVE QQFND     XQFND
     C*********            MOVE XXFYR     XQFYR
     C*********            MOVE XXEMP#    XQEMP#
     C*********            MOVE XXDIV     XQDIV
     C*********            MOVE XXREQ#    XQREQ#           REQUEST#
      *********
      ***PRINT*CERTIFICATE HOLDERS LETTERS IF ALLOWED & REQUESTED.
      *********
     C*********  SYOFIF    IFEQ 'Y'
     C*********  QQCCRT    ANDEQ'Y'
     C*********            MOVE QQCCRT    XQCCRT           CERT HOLDER FLAG
      *********
     C*********            ELSE
      *********
     C*********            MOVE *BLANK    XQCCRT
     C*********            ENDIF                           SYOFIF IFEQ 'Y'
      *********
     C*********            CALL 'W7166C'  P7166
      ***** END DEL ******* 09/02/05 **********************************
      ***** BEG ADD ******* 09/02/05 **********************************
      *
      *  Add record to Mass CX Issue Work file
      *
     C                   CLEAR                   PC601
      *
     C                   MOVE      QQCO#         C6CO#
     C                   MOVE      QQFND         C6FND
     C                   MOVE      XXFYR         C6FYR
     C                   MOVE      XXEMP#        C6EMP#
     C                   MOVE      XXDIV         C6DIV
     C                   MOVE      XXREQ#        C6REQ#                         REQUEST#
      *
      *  PRINT CERTIFICATE HOLDERS LETTERS IF ALLOWED & REQUESTED.
      *
     C     SYOFIF        IFEQ      'Y'
     C     QQCCRT        ANDEQ     'Y'
     C                   MOVE      QQCCRT        C6CCRT                         CERT HOLDER FLAG
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        C6CCRT
     C                   ENDIF                                                  SYOFIF IFEQ 'Y'
      *
     C                   MOVE      'N'           C6REPR
     C                   MOVE      QQOUTQ        C6OUTQ
     C                   MOVE      QQBAT#        C6BAT#
     C                   MOVE      QQTYPE        C6TYPE
     C                   MOVE      *BLANK        C6CERT
     C                   MOVE      CXAGT#        C6AGT#
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT
      *
     C                   MOVE      Q1USER        C6USER
     C                   MOVE      Q1PGM         C6PGM
     C                   Z-ADD     DSTIME        C6TIME
     C                   Z-ADD     XXSYDT        C6DT
      ***** BEG ADD ******* 07/03/07 **********************************
     C                   MOVEL(P)  CNSORT        C6SORT
      ***** END ADD ******* 07/03/07 **********************************
      *
     C                   WRITE     PC601
      *
      ***** END ADD ******* 09/02/05 **********************************
      ***** BEG ADD ******* 11/04/02 **********************************
     C                   MOVE      'Y'           X1CX
      ***** END ADD ******* 11/04/02 **********************************
      ***** BEG ADD ***** 08/03/04 ************************************
      *
      *  Process renewal years
      *
     C  N88              EXSR      SRRNEW
      ***** END ADD ***** 08/03/04 ************************************
      *
     C***  BEGIN CHG  ***  11/24/98  **********************************
     C******************** ENDIF                           EF,X IFNE 0
      ********************
     C******************** ENDIF                           *IN60 IFEQ *ON
     C*
     C                   ENDIF                                                  *IN89=*OFF
     C***  END   CHG  ***  11/24/98  **********************************
      *
     C                   ENDIF                                                  *IN51 IFEQ *OFF
      *
     C                   ENDIF                                                  CNSELC IFEQ 'Y'
      *
      *   MARK WHICH REPORT THE EMPLOYER SHOULD BE IN
      *
     C                   MOVE      *BLANK        SVFDNM                         SAVE EMPLOYER NAME
     C                   MOVE      *BLANK        SVADR1                         SAVE ADDRESS TO PRIN
     C                   MOVE      *BLANK        SVADR2                         SAVE ADDRESS TO PRIN
     C                   MOVE      *BLANK        SVCITY
     C                   MOVE      *BLANK        SVST
     C                   MOVE      *ZERO         SVZIP
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN43
     C                   MOVE      *OFF          *IN44
     C                   MOVE      *OFF          *IN45
     C                   Z-ADD     CNEMP#        XXEMP#
     C                   Z-ADD     CNDIV         XXDIV
      *
     C     EMKY          CHAIN     PEM01                              51        CO/FND/EMP/DIV
      *
     C     *IN51         IFEQ      *OFF
      *
     C                   MOVEL     EMFDNM        SVFDNM
      *
      *  SAVE ADDRESS FOR REPORTS
      *
     C     EMMPFL        IFEQ      'M'
      *
     C                   MOVEL     EMMAD1        SVADR1
     C                   MOVEL     EMMAD2        SVADR2
     C                   MOVEL     EMMCTY        SVCITY
     C                   MOVEL     EMMST         SVST
     C                   MOVE      EMMZIP        SVZIP
      *
     C                   ELSE
      *
     C                   MOVEL     EMPAD1        SVADR1
     C                   MOVEL     EMPAD2        SVADR2
     C                   MOVEL     EMPCTY        SVCITY
     C                   MOVEL     EMPST         SVST
     C                   MOVE      EMPZIP        SVZIP
      *
     C                   ENDIF                                                  EMMPFL IFEQ 'M'
      ***** BEG ADD ******* 09/24/99 **********************************
      *
      *  Save phone number for suspense list
      *
     C                   MOVE      *BLANKS       SVPH#            30
      *
     C     EMAREA        IFNE      *ZERO
     C     EMPHN#        ANDNE     *ZERO
     C                   MOVEL     EMAREA        SVPHA             3            AREA CODE
     C                   MOVEL     EMPHN#        SVPH3             3            1ST 3 PH #
     C                   MOVE      EMPHN#        SVPH4             4            LAST 4 PH #
     C     'Phone #:'    CAT(P)    '(':1         SVPH#
     C                   CAT(P)    SVPHA:0       SVPH#
     C                   CAT(P)    ')':0         SVPH#
     C                   CAT(P)    SVPH3:1       SVPH#
     C                   CAT(P)    '-':0         SVPH#
     C                   CAT(P)    SVPH4:0       SVPH#
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       SVPH#            30
      ***** END ADD ******* 09/24/99 **********************************
     C                   ENDIF                                                  *IN51 IFEQ *OFF
      *
     C***  BEGIN DEL  ***  11/10/98  **********************************
      * GET CANCELLATION REASON CODE DESCRIPTION
     C**********
     C**********           MOVELCNRECD    XXKEY     P
     C**********
     C********** TBKY      CHAINPCR01                60
     C**********
     C********** *IN60     IFEQ *ON
     C**********           MOVE *BLANK    XREASN
     C**********           ELSE
     C**********           MOVELCRDESC    XREASN    P      SAVE REASON FOR RPT4
     C**********           ENDIF                           *IN60 IFEQ *ON
     C***  END   DEL  ***  11/10/98  **********************************
      *
      *  REPORT 2: EMPLOYER NOT CANCELLED BECAUSE OF CX STATUS
      *
     C     CNSELC        IFEQ      'N'
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  CNSELC IFEQ 'N'
      *
      *  REPORT 3 & 4: CANCELLATION MAILING LIST & SUSPENSE LIST
      *
     C     CNSELC        IFEQ      'Y'
      ***** BEG ADD ******* 11/04/02 **********************************
     C     X1CX          ANDEQ     'Y'
      ***** END ADD ******* 11/04/02 **********************************
      *
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN45
      *
     C                   ENDIF                                                  CNSELC IFEQ 'Y'
      *
      *  PRINT THE EMPLOYER ON ONE OF THE CANCELLATION REPORTS
      *
     C                   EXSR      SRPRT
      *
     C     NEXT          TAG
      *
      *  REMOVE THE CANCELLATION RECORD FROM THE BATCH IF IT WASN'T
      *  CANCELLED.  REMAINING RECORDS WILL BE PROCESSED FOR MAILING
      *  LABELS AT END OF RUN, AND THEN DELETED.
      *
     C     *IN41         IFEQ      *ON                                          ERROR
     C     *IN42         OREQ      *ON                                          NOT SEL FOR CX
      *
     F***  BEGIN CHG  ***  07/03/07  **********************************
     C******               DELETPCN01
     C                   DELETE    PCN07
     F***  END   CHG  ***  07/03/07  **********************************
      *
     C                   ENDIF                                                  *IN41 IFEQ *ON
      *
     C                   ENDIF                                                  *IN50 IFEQ *OFF
      *
     C                   ENDDO                                                  *IN50 DOUEQ *ON
      *
      *   PRINT REPORT TOTALS ON ALL REPORTS. PRINT MSG IF NO DETAIL.
      *
      *  REPORT 1: NO EMPLR DETAIL RECORD FOR CANCEL FYR.
      *
     C     *INOD         IFEQ      *ON
     C                   ADD       1             XXPAG1
     C                   EXCEPT    HEAD1                                        CHECK FOR OVERFLOW
     C                   ENDIF                                                  *INOD IFEQ *ON
      *
     C     XXCNT1        IFEQ      0
     C                   EXCEPT    MSG1
     C                   ELSE
     C                   EXCEPT    TOT1
     C                   ENDIF                                                  XXCNT1 IFEQ 0
      *
      *  REPORT 2
      *
     C     *INOA         IFEQ      *ON
     C                   ADD       1             XXPAG2
     C                   EXCEPT    HEAD2                                        CHECK FOR OVERFLOW
     C                   ENDIF                                                  *INOA IFEQ *ON
      *
     C     XXCNT2        IFEQ      0
     C                   EXCEPT    MSG2
     C                   ELSE
     C                   EXCEPT    TOT2
     C                   ENDIF                                                  XXCNT2 IFEQ 0
      *
      *  REPORT 3
      *
     C     *INOB         IFEQ      *ON
     C                   ADD       1             XXPAG3
     C                   EXCEPT    HEAD3                                        CHECK FOR OVERFLOW
     C                   ENDIF                                                  *INOB IFEQ *ON
     C                   EXCEPT    TOT3
      *
      *  REPORT 4
      *
     C     *INOC         IFEQ      *ON
     C                   ADD       1             XXPAG4
     C                   EXCEPT    HEAD4                                        CHECK FOR OVERFLOW
     C                   ENDIF                                                  *INOC IFEQ *ON
      *
     C     XXCNT4        IFEQ      0
     C                   EXCEPT    MSG4
     C                   ELSE
     C                   EXCEPT    TOT4
     C                   ENDIF                                                  XXCNT4 IFEQ 0
      *
      *  PROCESS REMAINING RECORDS FOR MAILING LABELS
      *
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C     QQTYPE        IFNE      *BLANK
     C***  END   ADD  ***  07/29/99  **********************************
     C*
      ***** BEG ADD ******* 07/24/08 **********************************
     C     XXLABL        IFEQ      'E'
     C     XXLABL        OREQ      'B'
      ***** END ADD ******* 07/24/08 **********************************
     C                   CALL      'W7331C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM      '00000000'    QQFRDT            8
     C                   PARM      '00000000'    QQTODT            8
     C                   PARM      '1'           QQNUM             1
     C                   PARM                    QQOUTQ
     C                   PARM      'E'           QQSOR1            1
     C                   PARM      *BLANK        QQSOR2            1
     C                   PARM      *BLANK        QQSLCT            1
     C                   PARM      *BLANK        QQATYP            1
     C                   PARM      *ZEROS        QQFEMP            9
     C                   PARM      *ZEROS        QQTEMP            9
     C                   PARM      *ZEROS        QQFAGT            5
     C                   PARM      *ZEROS        QQTAGT            5
     C                   PARM      *BLANKS       QQFCNT            4
     C                   PARM      *BLANKS       QQTCNT            4
     C                   PARM      *ZEROS        QQFCHP            3
     C                   PARM      *ZEROS        QQTCHP            3
     C                   PARM      *ZEROS        QQFRGN            3
     C                   PARM      *ZEROS        QQTRGN            3
     C                   PARM      *ZEROS        QQFZIP            5
     C                   PARM      *ZEROS        QQTZIP            5
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQFORM            1
     C                   PARM                    QQBAT#                         BATCH#
     C                   PARM      *ZEROS        QQFYR             3
     C                   PARM      *ZEROS        QQEMP#            9
     C                   PARM      *ZEROS        QQDIV             5
     C*** BEG ADD *** 07/26/04  **********************************
     C                   PARM      *BLANK        QQPOL#            1
     C*** END ADD *** 07/26/04  **********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     C                   ENDIF
      *
     C     XXLABL        IFEQ      'A'
     C     XXLABL        OREQ      'B'
      *
     C                   CALL      'W4041C'
     C                   PARM      QQCO#         #5CO#             3
     C                   PARM      QQFND         #5FND             3
     C                   PARM      '00000'       #5AGT#            5
     C                   PARM      *BLANK        #5ALL             1
     C                   PARM      'Y'           #5CMFL            1
     C                   PARM      'Y'           #5MLFL            1
     C                   PARM      'N'           #5PRFL            1
     C                   PARM      '1'           #5NUM             1
     C                   PARM      QQTYPE        #5TYPE            1
     C                   PARM      'N'           #5PDFL            1
     C                   PARM      'N'           #5PHON            1
     C                   PARM      QQOUTQ        #5OUTQ           10
     C                   PARM      QQBAT#        #5BAT#            9
      *
     C                   ENDIF
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C                   ENDIF                                                  QQTYPE<>*BLANK
     C***  END   ADD  ***  07/29/99  **********************************
      *
      ***** BEG ADD ******* 06/01/06 **********************************
      *
     C                   UNLOCK    WXCNL3
     C                   UNLOCK    WXCNL4
     C                   UNLOCK    WMEML
     C                   UNLOCK    WXCCL
     C***  Begin Chg  ***  01/03/07  **************************************
     C**********           UNLCKWDCXL5
     C                   UNLOCK    WDCXP
     C***  End   Chg  ***  01/03/07  **************************************
      *
      ***** BEG DLT ******* 07/03/07 **********************************
      ********* Issue Cancellations in Agent Number order
      *********
     C*********  C6KY      CHAINPC602                60
      *********
     C*********  *IN60     DOWEQ*OFF
     C*********            MOVE C6CO#     #3CO#
     C*********            MOVE C6FND     #3FND
     C*********            MOVE C6FYR     #3FYR
     C*********            MOVE C6EMP#    #3EMP#
     C*********            MOVE C6DIV     #3DIV
     C*********            MOVE C6REQ#    #3REQ#           REQUEST#
     C*********            MOVE C6CCRT    #3CCRT           CERT HOLDER FLAG
     C*********            MOVE C6REPR    #3REPR           REPRINT FLAG
     C*********            MOVE C6OUTQ    #3OUTQ
     C*********            MOVE C6BAT#    #3BAT#
     C*********            MOVE C6TYPE    #3TYPE
     C*********            MOVE C6CERT    #3CERT
      *********
     C*********            CALL 'W7166C'
     C*********            PARM           #3CO#   3
     C*********            PARM           #3FND   3
     C*********            PARM           #3FYR   3
     C*********            PARM           #3EMP#  9
     C*********            PARM           #3DIV   5
     C*********            PARM           #3REQ#  5        REQUEST#
     C*********            PARM           #3CCRT  1        CERT HOLDER FLAG
     C*********            PARM           #3REPR  1        REPRINT FLAG
     C*********            PARM           #3OUTQ 10
     C*********            PARM           #3BAT#  9
     C*********            PARM           #3TYPE  1
     C*********            PARM           #3CERT  1
      *********
     C*********            DELETPC602
      *********
     C*********  C6KY      READEPC602                    60
     C*********            ENDDO
      ***** END DLT ******* 07/03/07 **********************************
      *
      ***** BEG CHG ******* 07/03/07 **********************************
      *  Issue Cancellations in Agent Number order
      *
     C     C6KY1         CHAIN     PC603                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      C6CO#         #3CO#
     C                   MOVE      C6FND         #3FND
     C                   MOVE      C6FYR         #3FYR
     C                   MOVE      C6EMP#        #3EMP#
     C                   MOVE      C6DIV         #3DIV
     C                   MOVE      C6REQ#        #3REQ#                         REQUEST#
     C                   MOVE      C6CCRT        #3CCRT                         CERT HOLDER FLAG
     C                   MOVE      C6REPR        #3REPR                         REPRINT FLAG
     C                   MOVE      C6OUTQ        #3OUTQ
     C                   MOVE      C6BAT#        #3BAT#
     C                   MOVE      C6TYPE        #3TYPE
     C                   MOVE      C6CERT        #3CERT
      ***** Begin Add ******* 08/29/19 **********************************
     C                   MOVE      XXSRMY        #SRPMY
      ***** End   Add ******* 08/29/19 **********************************
      ***** Begin Add ******* 09/16/19 **********************************
     C                   MOVE      XXWEDT        #SWEDT
      ***** End   Add ******* 09/16/19 **********************************
      *
     C                   CALL      'W7166C'
     C                   PARM                    #3CO#             3
     C                   PARM                    #3FND             3
     C                   PARM                    #3FYR             3
     C                   PARM                    #3EMP#            9
     C                   PARM                    #3DIV             5
     C                   PARM                    #3REQ#            5            REQUEST#
     C                   PARM                    #3CCRT            1            CERT HOLDER FLAG
     C                   PARM                    #3REPR            1            REPRINT FLAG
     C                   PARM                    #3OUTQ           10
     C                   PARM                    #3BAT#            9
     C                   PARM                    #3TYPE            1
     C                   PARM                    #3CERT            1
      ***** Begin Add ******* 08/29/19 **********************************
     C                   PARM                    #SRPMY            6
      ***** End   Add ******* 08/29/19 **********************************
      ***** Begin Add ******* 09/16/19 **********************************
     C                   PARM                    #SWEDT            8
      ***** End   Add ******* 09/16/19 **********************************
      *
     C                   DELETE    PC603
      *
     C     C6KY1         READE     PC603                                  60
     C                   ENDDO
      ***** END CHG ******* 07/03/07 **********************************
      *
      ***** END ADD ******* 06/01/06 **********************************
      *
      *  DELETE THE BATCH DETAIL RECORDS.
      *
     C     CNKY          SETLL     PCN01
     C     CNKY          READE     PCN01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   DELETE    PCN01
      *
     C     CNKY          READE     PCN01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OF
      *
      ***** BEG DEL ******* 06/01/06 **********************************
      ******BEG ADD ******* 09/02/05 **********************************
      *********
     C*********            UNLCKWXCNL3
     C*********            UNLCKWXCNL4
     C*********            UNLCKWMEML
     C*********            UNLCKWXCCL
     C*********            UNLCKWDCXL5
     C*********            UNLCKWXC6L
      *********
      ***Issue*Cancellations in Agent Number order
      *********
     C*********  C6KY      CHAINPC602                60
      *********
     C*********  *IN60     DOWEQ*OFF
     C*********            MOVE C6CO#     #3CO#
     C*********            MOVE C6FND     #3FND
     C*********            MOVE C6FYR     #3FYR
     C*********            MOVE C6EMP#    #3EMP#
     C*********            MOVE C6DIV     #3DIV
     C*********            MOVE C6REQ#    #3REQ#           REQUEST#
     C*********            MOVE C6CCRT    #3CCRT           CERT HOLDER FLAG
     C*********            MOVE C6REPR    #3REPR           REPRINT FLAG
     C*********            MOVE C6OUTQ    #3OUTQ
     C*********            MOVE C6BAT#    #3BAT#
     C*********            MOVE C6TYPE    #3TYPE
     C*********            MOVE C6CERT    #3CERT
      *********
     C*********            CALL 'W7166C'
     C*********            PARM           #3CO#   3
     C*********            PARM           #3FND   3
     C*********            PARM           #3FYR   3
     C*********            PARM           #3EMP#  9
     C*********            PARM           #3DIV   5
     C*********            PARM           #3REQ#  5        REQUEST#
     C*********            PARM           #3CCRT  1        CERT HOLDER FLAG
     C*********            PARM           #3REPR  1        REPRINT FLAG
     C*********            PARM           #3OUTQ 10
     C*********            PARM           #3BAT#  9
     C*********            PARM           #3TYPE  1
     C*********            PARM           #3CERT  1
      *********
     C*********            DELETPC602
      *********
     C*********  C6KY      READEPC602                    60
     C*********            ENDDO
      *********
      ******END ADD ******* 09/02/05 **********************************
      ***** END DEL ******* 06/01/06 **********************************
      *
      *  DELETE THE BATCH REQUEST. (IN THE BATCH SELECTION CRITERIA FILE)
      *
     C     CCKY          CHAIN     PCC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PCC01
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*** begin add ***  10/06/16  *****************************************
     C     *lock         in        *dtaara
     C                   out       *dtaara
     C*
     C* Call process to generate Scriptura output/Darts if requested.
     C*
      *** beg chg ***** 01/13/17 ***************************
     C***********        If        ldMassFile <> ' '
     C***********        Eval      QQXml = ldMassFile + '$'
     C***********        Call      'S4401C'
     C***********        Parm                    QQXml            61
     C***********        Endif
      /Free
       Select;
       When ldMassFile = *blank;
       When objExists('PDSDEV':'*LIB');
          Monitor;
          S4401C(ldMassFile + '$');
          On-error;
          Endmon;
       Other;
          S4401C(ldMassFile + '$');
       Endsl;
      /End-Free
      *** end chg ***** 01/13/17 ***************************
     C*** end add *****  10/06/16  *****************************************
      *
      *  END OF PROCESSING
      *
     C     TERM          TAG
      *
     C                   MOVE      *ON           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '4';
       Chain orKy POR01;
       If %found;
          Except Route1;
       Endif;
       xxPrt = '1';
       Chain orKy POR01;
       If %found;
          Except Route2;
       Endif;
       xxPrt = '2';
       Chain orKy POR01;
       If %found;
          Except Route3;
       Endif;
       xxPrt = '3';
       Chain orKy POR01;
       If %found;
          Except Route4;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
      *
      ***** BEG ADD ***** 08/03/04 ************************************
      *
      *================================================================
      * SRRNEW  - Cancel renewal years
      *================================================================
     C     SRRNEW        BEGSR
      *
      *  For each renewal policy year for employer division
      *
     C     ELKY4         SETGT     PEL01
     C     ELKY5         READE     PEL01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *    If not cancelled, process CX of renewal year
      *
     C     CXKY3         SETGT     PCX01
     C     CXKY3         READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *ON                                          NOT CX
     C     CXREIN        OREQ      'Y'                                          OR REINSTATED
     C   60              Z-ADD     1             X1REQ#
     C  N60CXREQ#        ADD       1             X1REQ#
      *
      *    Get Agent info
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    #1AGT#            5 0
     C***********        PARM                    #1ANAM           60
     C                   PARM      *zeros        #1AGT#            5 0
     C                   PARM      *blanks       #1ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
      *
      *  Write CX detail record for renewal year
      *
     C                   CLEAR                   PCX01
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT
      *
     C                   Z-ADD     ELCO#         CXCO#
     C                   Z-ADD     ELFND         CXFND
     C                   Z-ADD     ELFYR         CXFYR
     C                   Z-ADD     ELEMP#        CXEMP#
     C                   Z-ADD     ELDIV         CXDIV
     C                   Z-ADD     X1REQ#        CXREQ#
     C                   Z-ADD     ELPFDT        CXCXDT                         CANCEL EFF DATE
     C                   Z-ADD     XXSYDT        CXCDAT
     C                   Z-ADD     #1AGT#        CXAGT#
     C                   MOVEL     CNRECD        CXRSN
     C                   MOVEL     CNCOM1        CXCOM1
     C                   MOVEL     CNCOM2        CXCOM2
     C                   MOVE      CNAMTD        CXAMTD
      *
     C                   MOVE      'N'           CXCCRT
     C                   MOVE      'N'           CXREIN
     C                   MOVE      'N'           CXRCRT
     C                   MOVE      'Y'           CXAFLT
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        CXRST
     C*** Begin Add *** 07/28/25 *******************************************
     C                   Z-ADD     XXBAT#        CXBAT#
     C                   Z-ADD     CRLFEE        CXLFEE
     C*** End   Add *** 07/28/25 *******************************************
      *
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   Z-ADD     DSTIME        CXTIME
     C                   Z-ADD     XXSYDT        CXDT
     C*
     C                   MOVE      Q1USER        CXIUSR
     C                   MOVE      Q1PGM         CXIPGM
     C                   Z-ADD     DSTIME        CXITM
     C                   Z-ADD     XXSYDT        CXIDT
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     CNCNC#        CXCNC#
      ***** END ADD ******* 07/25/05 **********************************
      *
      ***** BEG ADD ******* 04/16/08 **********************************
     C                   MOVE      CCEAFL        CXEAFL
      ***** END ADD ******* 04/16/08 **********************************
      ***** Begin Add ******* 09/16/19 **********************************
     C                   MOVE      CCWEDT        CXWEDT
      ***** End   Add ******* 09/16/19 **********************************
     C                   WRITE     PCX01
      *
      *  Write a record to CX History File
      *
     C                   Z-ADD     ELCO#         XHCO#
     C                   Z-ADD     ELFND         XHFND
     C                   Z-ADD     ELFYR         XHFYR
     C                   Z-ADD     ELEMP#        XHEMP#
     C                   Z-ADD     ELDIV         XHDIV
     C                   Z-ADD     X1REQ#        XHREQ#
     C                   Z-ADD     #1AGT#        XHAGT#
     C                   MOVE      *BLANK        XHRVFL
     C                   Z-ADD     0             XHREDT
     C                   Z-ADD     0             XHRDAT
     C                   MOVEL     CNRECD        XHRSN
     C                   Z-ADD     XXSYDT        XHTRDT                         SYSTEM DATE
     C                   Z-ADD     ELPFDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   MOVE      *BLANK        XHSTAT
     C                   MOVEL     Q1USER        XHUSER
     C                   MOVEL     Q1PGM         XHPGM
     C                   Z-ADD     XXSYDT        XHDT
     C                   Z-ADD     DSTIME        XHTIME
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     CNCNC#        XHCNC#
      ***** END ADD ******* 07/25/05 **********************************
     C                   WRITE     PXH01
      *
      *  Process CX of Renewal Year
      *
      ***** BEG DEL ******* 09/02/05 **********************************
     C*********            MOVE ELCO#     #2CO#
     C*********            MOVE ELFND     #2FND
     C*********            MOVE ELFYR     #2FYR
     C*********            MOVE ELEMP#    #2EMP#
     C*********            MOVE ELDIV     #2DIV
     C*********            MOVE X1REQ#    #2REQ#
     C*********            MOVE QQBAT#    #2BAT#
     C*********            MOVE QQTYPE    #2TYPE
      *********
     C*********            CALL 'W7166C'
     C*********            PARM           #2CO#   3
     C*********            PARM           #2FND   3
     C*********            PARM           #2FYR   3
     C*********            PARM           #2EMP#  9
     C*********            PARM           #2DIV   5
     C*********            PARM           #2REQ#  5        REQUEST#
     C*********            PARM 'N'       #2CCRT  1        CERT HOLDER FLAG
     C*********            PARM 'N'       #2REPR  1        REPRINT FLAG
     C*********            PARM QQOUTQ    #2OUTQ 10
     C*********            PARM           #2BAT#  9
     C*********            PARM           #2TYPE  1
     C*********            PARM *BLANK    #2CERT  1
      ***** END DEL ******* 09/02/05 **********************************
      ***** BEG ADD ******* 09/02/05 **********************************
      *
      *  Add record to Mass CX Issue Work File
      *
     C                   CLEAR                   PC601
      *** BEG CHG ***** 12/28/07 **************************************
     C*******              MOVE ELCO#     #3CO#
     C*******              MOVE ELFND     #3FND
     C*******              MOVE ELFYR     #3FYR
     C*******              MOVE ELEMP#    #3EMP#
     C*******              MOVE ELDIV     #3DIV
     C*******              MOVE X1REQ#    #3REQ#
     C*******              MOVE 'N'       #3CCRT
     C*******              MOVE 'N'       #3REPR
     C*******              MOVE QQOUTQ    #3OUTQ
     C*******              MOVE QQBAT#    #3BAT#
     C*******              MOVE QQTYPE    #3TYPE
     C*******              MOVE *BLANK    #3CERT
     C                   MOVE      ELCO#         C6CO#
     C                   MOVE      ELFND         C6FND
     C                   MOVE      ELFYR         C6FYR
     C                   MOVE      ELEMP#        C6EMP#
     C                   MOVE      ELDIV         C6DIV
     C                   MOVE      X1REQ#        C6REQ#
     C                   MOVE      'N'           C6CCRT
     C                   MOVE      'N'           C6REPR
     C                   MOVE      QQOUTQ        C6OUTQ
     C                   MOVE      QQBAT#        C6BAT#
     C                   MOVE      QQTYPE        C6TYPE
     C                   MOVE      *BLANK        C6CERT
      *** END CHG ***** 12/28/07 **************************************
     C                   MOVE      CXAGT#        C6AGT#
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT
      *
     C                   MOVE      Q1USER        C6USER
     C                   MOVE      Q1PGM         C6PGM
     C                   Z-ADD     DSTIME        C6TIME
     C                   Z-ADD     XXSYDT        C6DT
      ***** BEG ADD ******* 07/03/07 **********************************
     C                   MOVEL(P)  CNSORT        C6SORT
      ***** END ADD ******* 07/03/07 **********************************
      *
     C                   WRITE     PC601
      *
      ***** END ADD ******* 09/02/05 **********************************
      *
     C                   ENDIF                                                  CXKY
      *
     C     ELKY5         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60
      *
     C                   ENDSR                                                  SRRNEW
      *
      *
      ***** END ADD ***** 08/03/04 ************************************
      ***** BEG ADD ******* 04/07/03 **********************************
      *
      *================================================================
      * SRRST   - Get rating state
      *================================================================
     C     SRRST         BEGSR
      *
     C     ELKY3         CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        XXRTCD
     C  N60RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        CXRST
     C   60              MOVE      *BLANKS       CXRST
      *
     C                   ENDSR                                                  SRRST
      *
      *
      ***** END ADD ******* 04/07/03 **********************************
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*================================================================
     C*  SRDAYS - Checks all divs of employer to determine the number
     C*           of days to add to issue date to get cancel date.
     C*================================================================
     C     SRDAYS        BEGSR
     C*
     C                   Z-ADD     *ZERO         ZZDAYS
     C*
     C*  Cycle through all divs for employer
      **** BEGIN CHG **** 12/08/98 *********************************************
     C********** C2KY      CHAINPCN02                60
     C     CNKY1         CHAIN(N)  PCN02                              60
      **** END   CHG **** 12/08/98 *********************************************
     C     *IN60         DOWEQ     *OFF
     C*
     C*    Get employer detail for fyr/emp#/div
     C                   Z-ADD     C2DIV         KKDIV
     C     ELKY2         CHAIN     PEL01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C*      Get jurisdiction state
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*      Chain to canx reason code file
     C                   MOVEL(P)  C2RECD        XXKEY
     C                   MOVE      RSJRST        XXST
     C***  BEGIN ADD  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
     C                   Z-ADD     XXEFDT        KKEFDT
     C***  END   ADD  ***  02/24/99  **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 08/27/02 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 08/27/02 **********************************
     C*
     C*      Get canx reason description if div is selected for canx
     C     C2SELC        IFEQ      'Y'
     C  N60              MOVEL(P)  CRDESC        XREASN
     C   60              MOVE      *BLANKS       XREASN
     C                   ENDIF
      ***** BEG ADD ******* 08/17/11 **********************************
     C                   MOVE      ' '           POLNEW            1
     C     CRNDNW        IFGT      0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
      *
     c* Number of days to consider policy new
     C                   Z-ADD     CRNDNW        QQDAYS
     C*
     c** Calculate new work date
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X@WRDT            7 0
     C                   ENDIF                                                  QQERR<>'1'
      *
     C                   Z-ADD     ELCO#         ##CO#
     C                   Z-ADD     ELFND         #FND
     C                   Z-ADD     ELEMP#        ##EMP#
     C                   Z-ADD     ELDIV         ##DIV
     C                   Z-ADD     *ZEROS        #@DATE
     C                   CALL      'WA084R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    #FND              3 0
     C                   PARM                    ##EMP#            9 0
     C                   PARM                    ##DIV             5 0
     C                   PARM                    #@DATE            7 0
     C*
     C                   MOVE      ' '           POLNEW            1
     C     #@DATE        IFGE      X@WRDT
     C                   MOVE      'Y'           POLNEW
     C                   ENDIF                                                  #@DATE > X@WRDT
     C                   ENDIF                                                  CRNDNW > 0
      ***** END ADD ******* 08/17/11 **********************************
     C*
     C*      Calculate number of days to add to issue date
     C                   Z-ADD     0             XXDAYS
     C*
     C     ELCXDY        IFNE      0
     C                   ADD       ELCXDY        XXDAYS
     C                   ELSE
      ***** BEG CHG ******* 08/17/11 **********************************
     C************         ADD  CRNOTC    XXDAYS
     C     POLNEW        IFNE      'Y'
     C                   ADD       CRNOTC        XXDAYS
     C                   ELSE
     C                   ADD       CRNTNP        XXDAYS
     C                   ENDIF                                                  POLNEW = 'Y'
      ***** END CHG ******* 08/17/11 **********************************
     C                   ENDIF                                                  ELCXDY<>0
     C*
     C     ELADDM        IFNE      0
     C                   ADD       ELADDM        XXDAYS
     C                   ELSE
      ***** BEG CHG ******* 08/17/11 **********************************
     C************         ADD  CRADDM    XXDAYS
     C     POLNEW        IFNE      'Y'
     C                   ADD       CRADDM        XXDAYS
     C                   ELSE
     C                   ADD       CRMDNP        XXDAYS
     C                   ENDIF                                                  POLNEW <>'Y'
      ***** END CHG ******* 08/17/11 **********************************
     C                   ENDIF                                                  ELADDM<>0
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C     XXDAYS        IFGT      ZZDAYS
     C                   Z-ADD     XXDAYS        ZZDAYS
     C                   ENDIF                                                  XXDAYS>ZZDAYS
     C*
      **** BEGIN CHG **** 12/08/98 *********************************************
     C********** C2KY      READEPCN02                    60
     C     CNKY1         READE(N)  PCN02                                  60
      **** END   CHG **** 12/08/98 *********************************************
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  SRDAYS
     C***  END   ADD  ***  11/10/98  **********************************
      *
      *
      *
      *=========================================================================
      *   SRPRT - SUBROUTINE TO PRINT ACCOUNT ON CANCEL REPORT        *
      *=========================================================================
     C     SRPRT         BEGSR
      *
      *  GET BILLING SCHEDULE CODE & TYPE
      *
     C                   MOVE      *BLANK        BXTYPE
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     ELBSCD        XXBSCD
      *
     C     BXKY          CHAIN     PBX01                              60
      *
     C                   MOVEL(P)  ELBSCD        XXKEY
      *
     C     TBKY          CHAIN     PBS01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        BSSEFL
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C***  BEG ADD ******  06/13/22  **********************************
     C                   IF        BSfreq = 'M'
     C                   Eval      XXWEDT = 0
     C                   Else
     C                   Eval      XXSRMY = 0
     C                   Endif
     C***  END ADD ******  06/13/22  **********************************
      *
      *  LIST EMPLOYERS NOT CANCELLED BECAUSE OF CX STATUS
      *
     C     *IN42         IFEQ      *ON
      *
     C*** BEG ADD *******  02/18/03  **********************************
     C     CCFREQ        IFEQ      'B'
     C     BSFREQ        ANDEQ     'W'
     C     CCFREQ        OREQ      'W'
     C     BSFREQ        ANDEQ     'W'
     C                   SETON                                        47
      *
     C     BSSEFL        IFEQ      'S'
     C                   EXSR      SRWK
     C                   ELSE
     C                   Z-ADD     0             XX#WKS            3 0
     C                   ENDIF
      *
     C                   ELSE
     C                   SETOFF                                       47
     C*** END ADD *******  02/18/03  **********************************
      *
     C     BSSEFL        IFEQ      'S'
     C                   EXSR      SRMN                                         FIND # MISSING MONTH
     C                   ELSE
     C                   Z-ADD     0             XXMTHS
     C                   ENDIF                                                  BSSEFL IFEQ 'S'
      *
     C*** BEG ADD *******  02/18/03  **********************************
     C                   ENDIF                                                  CCFREQ = 'B'
     C*** END ADD *******  02/18/03  **********************************
      *
     C     *INOA         IFEQ      *ON
     C                   ADD       1             XXPAG2
     C                   EXCEPT    HEAD2                                        CHECK FOR OVERFLOW
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  *INOA IFEQ *ON
      *
     C                   EXCEPT    DET2
      *
     C                   ADD       1             XXCNT2                         COUNT RECORDS
      *
     C                   ENDIF                                                  *IN42 IFEQ *ON
      *
      *  LIST EMPLOYERS ON CANCELLATION MAILING LIST
      *
     C     *IN44         IFEQ      *ON
      *
     C     XXCNT3        IFGE      20
     C                   MOVE      *ON           *INOB
     C                   ENDIF                                                  XXCNT3 IFGE 20
      *
     C     *INOB         IFEQ      *ON                                          CHECK FOR OVERFLOW
     C                   ADD       1             XXPAG3
     C                   EXCEPT    HEAD3
     C                   MOVE      *OFF          *INOB
     C                   MOVE      *ZERO         XXCNT3
     C                   ENDIF                                                  *INOB IFEQ *ON
      *
     C                   MOVE      *BLANK        XXCITY
     C     SVCITY        CAT(P)    ',':0         XXCITY                         CONCAT CITY & STATE
     C                   CAT       SVST:1        XXCITY
     C                   ADD       1             XXCNT3
      *
      * IF SECOND ADDRESS IS BLANK ADJUST THE LINES UPWARD
      *
     C     SVADR2        IFEQ      *BLANK                                                      LOW
     C                   MOVE      *ON           *IN25
     C                   ELSE
     C                   MOVE      *OFF          *IN25
     C                   ENDIF                                                  SVADR2 IFEQ
      *
     C                   EXCEPT    DET3
      *
     C                   ENDIF                                                  *IN44 IFEQ *ON
      *
      *  LIST EMPLOYERS ON CANCELLATION SUSPENSE LIST
      *
     C     *IN45         IFEQ      *ON
      *
     C     *INOC         IFEQ      *ON
     C                   ADD       1             XXPAG4
     C                   EXCEPT    HEAD4                                        CHECK FOR OVERFLOW
     C                   MOVE      *OFF          *INOC
     C                   ENDIF                                                  *INOC IFEQ *ON
      *
      **** BEGIN ADD **** 09/11/98 *********************************************
     C                   Z-ADD     CNDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         XXCEDT            8 0
      **** END   ADD **** 09/11/98 *********************************************
     C                   EXCEPT    DET4
      *
     C                   ADD       1             XXCNT4                         COUNT RECORDS
      *
     C                   ENDIF                                                  *IN45 IFEQ *ON
      *
     C                   ENDSR                                                  SRPRT
      *
      *=========================================================================
      *             MISSING MONTHS' REPORTS SUBROUTINE                *
      *=========================================================================
     C     SRMN          BEGSR
      *
      * Check for special cases that are invalid and valid:
      * 1. Case when a payroll class is incorrectly keyed but then amt.
      *    is reversed.  If there are no other payroll classes for that
      *    employer then there is a missing month.
      * 2. Case when there is payroll class but the reported amount = 0
      *    This is a valid entry and should not have a missing month.
      * 3. Case when there are no reported payrolls.  This case is a
      *    missing month.
      * 4. Case where there are reported payrolls that have not been
      *    reversed.  There is no missing month.
      *
     C                   Z-ADD     *ZERO         XXMTHS                         # OF MISSING MONTHS
     C                   MOVEL     ELPFDT        XXSTDT
      *
     C     XXDATE        IFGT      ELPTDT
     C                   MOVEL     ELPTDT        XXCPYM
     C                   ELSE
     C                   MOVEL     XXDATE        XXCPYM                         Compare Date
     C                   ENDIF                                                  XXDATE IFGT
      *
     C                   MOVE      *OFF          *IN54
     C                   Z-ADD     1             X2
      *
      * The range of dates begins with employer's policy from date to
      * THE CANCEL NOTICE DATE.
      *
      * Loop through all months from start date to comparison date.
      *
     C     XXSTDT        IFLE      XXCPYM
     C     XXSTDT        ANDNE     *ZERO
      *
     C     XXSTDT        DOWLE     XXCPYM
      *
     C                   MOVE      *ZERO         CLS
     C                   MOVE      *ZERO         AMT
     C                   MOVE      *OFF          *IN57
     C                   Z-ADD     *ZERO         CNT
      *
      * BEGIN READING SELF-REPORTING PAYROLL TRANSACTION FILE
      *
     C     SRKY          CHAIN     PSR01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     1             Y1
      *
      * CHECK FOR ANY AMOUNTS THAT ARE NOT = 0
      *
     C     SRAMTP        IFNE      0
     C                   MOVE      *ON           *IN57
     C                   ENDIF                                                  SRAMTP IFNE 0
      *
     C     Y1            DOWLE     999
      *
     C     CLS(Y1)       IFNE      0
     C     CLS(Y1)       ANDNE     SRCLAS
      *
     C                   ADD       1             Y1
      *
     C                   ELSE
      *
     C     SRCLAS        IFEQ      CLS(Y1)
      *
     C                   ADD       SRAMTP        AMT(Y1)
     C                   Z-ADD     1000          Y1
      *
     C                   ELSE
      *
     C     CLS(Y1)       IFEQ      0
      *
     C                   Z-ADD     SRCLAS        CLS(Y1)
     C                   Z-ADD     SRAMTP        AMT(Y1)
     C                   ADD       1             CNT
     C                   Z-ADD     1000          Y1
      *
     C                   ENDIF                                                  CLS,Y1 IFEQ 0
      *
     C                   ENDIF                                                  SRCLAS IFEQ CLS
      *
     C                   ENDIF                                                  CLS,Y1 IFNE 0
      *
     C                   ENDDO                                                  Y1 DOWLE 999
      *
     C     SRKY          READE     PSR01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OF
      *
      * Check for Case 1.
      *
      * Loop through and total amount array.  If amounts = 0 and there
      * were non-zero records added (*in57 = 1) then we have case 1.
      *
     C     *IN57         IFEQ      *ON
      *
     C                   Z-ADD     1             Y1
     C                   Z-ADD     *ZERO         XAMT             11 2
      *
     C     Y1            DOWLE     999
      *
     C                   ADD       AMT(Y1)       XAMT
     C                   ADD       1             Y1
      *
     C                   ENDDO                                                  Y1 DOWLE 999
      *
     C     XAMT          IFEQ      0
      *
     C                   ADD       1             XXMTHS                         # OF MISSING MONTHS
     C                   ADD       1             XXSTMN                         get nxt mnth
      *
     C                   GOTO      NXT1
      *
     C                   ENDIF                                                  XAMT IFEQ 0
      *
     C                   ENDIF                                                  *IN57 IFEQ *ON
      *
      * Check for case 3.
      *
     C     CNT           IFEQ      0
      *
     C                   ADD       1             XXMTHS                         # OF MISSING MONTHS
     C                   ADD       1             XXSTMN                         get nxt mnth
      *
     C                   GOTO      NXT1
      *
     C                   ENDIF                                                  CNT IFEQ 0
      *
      * Cases 2 & 4 continue for the next month
      *
     C                   ADD       1             XXSTMN                         get nxt mnth
      *
     C     NXT1          TAG
      *
     C     XXSTMN        IFGT      12
     C                   Z-ADD     1             XXSTMN
     C                   ADD       1             XXSTYR
     C                   ENDIF                                                  XXSTMN IFGT 12
      *
     C                   ENDDO                                                  XXSTDT DOWLE
      *
     C                   ENDIF                                                  XXSTDT IFLE
      *
      * DO NOT INCLUDE THE MONTH OF THE CANCELLATION NOTICE
      *
     C     XXMTHS        IFGT      0
     C     XXDATE        ANDLE     ELPTDT
     C                   SUB       1             XXMTHS
     C                   ENDIF                                                  XXMTHS
      *
     C                   ENDSR                                                  SRMN
     C***  BEG ADD ******  02/18/03  **********************************
      *=========================================================================
      *             MISSING WEEKS' REPORTS SUBROUTINE                *
      *=========================================================================
     C     SRWK          BEGSR
      *
      * Check for special cases that are invalid and valid:
      * 1. Case when a payroll class is incorrectly keyed but then amt.
      *    is reversed.  If there are no other payroll classes for that
      *    employer then there is a missing month.
      * 2. Case when there is payroll class but the reported amount = 0
      *    This is a valid entry and should not have a missing month.
      * 3. Case when there are no reported payrolls.  This case is a
      *    missing month.
      * 4. Case where there are reported payrolls that have not been
      *    reversed.  There is no missing month.
      *
     C                   Z-ADD     *ZERO         XX#WKS                         # OF MISSING MONTHS
     C                   MOVEL     ELPFDT        X1STDT            7 0
      *
     C     XXDATE        IFGT      ELPTDT
     C                   MOVEL     ELPTDT        X1CPYM            7 0
     C                   ELSE
     C                   MOVEL     XXDATE        X1CPYM                         Compare Date
     C                   ENDIF                                                  XXDATE IFGT
      *
     C                   MOVE      *OFF          *IN54
     C                   Z-ADD     1             X2
      *
      * The range of dates begins with employer's policy from date to
      * THE CANCEL NOTICE DATE.
      *
      * Loop through all weeks from start date to comparison date.
      *
     C     X1STDT        IFLE      X1CPYM
     C     X1STDT        ANDNE     *ZERO
      *
     C     X1STDT        DOWLE     X1CPYM
     C                   MOVE      *ZERO         CLS
     C                   MOVE      *ZERO         AMT
     C                   MOVE      *OFF          *IN57
     C                   Z-ADD     *ZERO         CNT
      *
      * BEGIN READING SELF-REPORTING PAYROLL TRANSACTION FILE
      *
     C     SRKY1         CHAIN     PSR02                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     1             Y1
      *
      * CHECK FOR ANY AMOUNTS THAT ARE NOT = 0
      *
     C     SRAMTP        IFNE      0
     C                   MOVE      *ON           *IN57
     C                   ENDIF                                                  SRAMTP IFNE 0
      *
     C     Y1            DOWLE     999
      *
     C     CLS(Y1)       IFNE      0
     C     CLS(Y1)       ANDNE     SRCLAS
     C                   ADD       1             Y1
      *
     C                   ELSE
      *
     C     SRCLAS        IFEQ      CLS(Y1)
     C                   ADD       SRAMTP        AMT(Y1)
     C                   Z-ADD     1000          Y1
      *
     C                   ELSE
      *
     C     CLS(Y1)       IFEQ      0
     C                   Z-ADD     SRCLAS        CLS(Y1)
     C                   Z-ADD     SRAMTP        AMT(Y1)
     C                   ADD       1             CNT
     C                   Z-ADD     1000          Y1
     C                   ENDIF                                                  CLS,Y1 IFEQ 0
      *
     C                   ENDIF                                                  SRCLAS IFEQ CLS
      *
     C                   ENDIF                                                  CLS,Y1 IFNE 0
      *
     C                   ENDDO                                                  Y1 DOWLE 999
      *
     C     SRKY1         READE     PSR02                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OF
      *
      * Check for Case 1.
      *
      * Loop through and total amount array.  If amounts = 0 and there
      * were non-zero records added (*in57 = 1) then we have case 1.
      *
     C     *IN57         IFEQ      *ON
     C                   Z-ADD     1             Y1
     C                   Z-ADD     *ZERO         XAMT             11 2
      *
     C     Y1            DOWLE     999
     C                   ADD       AMT(Y1)       XAMT
     C                   ADD       1             Y1
     C                   ENDDO                                                  Y1 DOWLE 999
      *
     C     XAMT          IFEQ      0
     C                   ADD       1             XX#WKS                         # OF MISSING WEEKS
     C                   EXSR      GNXTWK                                       get nxt week
     C                   GOTO      NXT2
     C                   ENDIF                                                  XAMT IFEQ 0
      *
     C                   ENDIF                                                  *IN57 IFEQ *ON
      *
      * Check for case 3.
      *
     C     CNT           IFEQ      0
     C                   ADD       1             XX#WKS                         # OF MISSING WEEKS
     C                   EXSR      GNXTWK                                       get nxt week
     C                   GOTO      NXT2
     C                   ENDIF                                                  CNT IFEQ 0
      *
      * Cases 2 & 4 continue for the next month
      *
     C                   EXSR      GNXTWK                                       get nxt week
     C     NXT2          TAG
     C                   ENDDO                                                  XXSTDT DOWLE
      *
     C                   ENDIF                                                  XXSTDT IFLE
      *
      * DO NOT INCLUDE THE MONTH OF THE CANCELLATION NOTICE
      *
     C     XX#WKS        IFGT      0
     C     XXDATE        ANDLE     ELPTDT
     C                   SUB       1             XX#WKS
     C                   ENDIF                                                  XX#WKS
      *
     C                   ENDSR                                                  SRWK
      *================================================================
      *  GNXTWK - Get next week ending date
      *================================================================
     C     GNXTWK        BEGSR
      *
     C                   Z-ADD     X1STDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DT1
      *
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DT1             8 0
     C                   PARM      7             ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C     ##ERR         IFEQ      *OFF
     C                   Z-ADD     ##DT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1STDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        X1STDT
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDSR                                                  GNXTWK
     C***  END ADD ******  02/18/03  **********************************
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C*================================================================
     C*  PRECX - Before processing the cancellations, this subroutine
     C*          loops through the canx workfile (WXCNP) : if an employer
     C*          is selected for canx then loop through and find all divs
     C*          and select them for cx if they have not been.
     C*================================================================
     C     PRECX         BEGSR
     C*
     C     CNKY          CHAIN     PCN02                              60
     C     *IN60         DOWEQ     *OFF
     C     C2SELC        IFEQ      'Y'
     C*
     C                   Z-ADD     C2EMP#        XXEMP#
     C     CNKY1         CHAIN     PCN02                              70
     C     *IN70         DOWEQ     *OFF
     C     C2SELC        IFNE      'Y'
     C                   MOVE      'Y'           C2SELC
     C                   UPDATE    PCN02
     C                   ENDIF                                                  C2SELC<>'Y'
     C     CNKY1         READE     PCN02                                  70
     C                   ENDDO                                                  *IN70=*OFF
     C*
     C     CNKY1         SETGT     PCN02
     C                   ENDIF                                                  C2SELC='Y'
     C     CNKY          READE     PCN02                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  PRECX
     C***  END   ADD  ***  07/29/99  **********************************
      *
      ***** BEG ADD ******* 07/03/07 **********************************
      *
      *=================================================================
      * SRSORT - Update Mass CX Batch Work File with Processing Sort Key
      *=================================================================
     C     SRSORT        BEGSR
      *
     C     CNKY          CHAIN     PCN01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     EMKY1         CHAIN(N)  PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       CNSORT
      *
     C     QQSRT1        IFEQ      'AA'
     C     QQSRT1        OREQ      'AN'
     C                   CALL      'WA002R'
     C                   PARM      CNCO#         #1CO#             3 0
     C                   PARM      CNFND         #1FND             3 0
     C                   PARM      CNEMP#        #1EMP#            9 0
     C                   PARM      CNDIV         #1DIV             5 0
     C                   PARM      CNFYR         #1FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    #1AGT#            5 0
     C***********        PARM                    #1ANAM           60
     C                   PARM      *zeros        #1AGT#            5 0
     C                   PARM      *blanks       #1ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
     C                   ENDIF
     C*
     C     QQSRT1        IFEQ      'AA'
     C                   MOVEL(P)  #1ANAM        CNSORT
     C                   ENDIF
      *
     C     QQSRT1        IFEQ      'AN'
     C                   MOVEL(P)  #1AGT#        CNSORT
     C                   ENDIF
      *
     C     QQSRT1        IFEQ      'EA'
     C     EMNAME        CAT(P)    EMNAM2:0      CNSORT
     C                   ENDIF
      *
     C     QQSRT1        IFEQ      'EN'
     C                   MOVE      EMEMP#        X1EMP#            9
     C                   MOVE      EMDIV         X1DIV             5
     C     X1EMP#        CAT(P)    X1DIV:0       CNSORT
     C                   ENDIF
      *
     C     QQSRT2        IFEQ      'EA'
     C                   CAT(P)    EMNAME:0      CNSORT
     C                   CAT(P)    EMNAM2:0      CNSORT
     C                   ENDIF
      *
     C     QQSRT2        IFEQ      'EN'
     C                   MOVE      EMEMP#        X1EMP#            9
     C                   CAT(P)    X1EMP#:0      CNSORT
     C                   MOVE      EMDIV         X1DIV             5
     C                   CAT(P)    X1DIV:0       CNSORT
     C                   ENDIF
      *
     C                   UPDATE    PCN01
     C                   ENDIF                                                  EMKY1
      *
     C     CNKY          READE     PCN01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  SRSORT
      *
      ***** END ADD ******* 07/03/07 **********************************
      *
      ***** BEG CHG ******* 09/02/05 **********************************
     O********QSYSPRT E  106           ROUTE1
     OQSYSPRT4  E            ROUTE1         1 06
      ***** END CHG ******* 09/02/05 **********************************
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********        09   QQOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE1         2 33
     O***** BEGIN CHG **** 04/10/00 ***********************************
     O**********                         18 'REPORT SELECTIONS:'
     O******* E  2             ROUTE1
     O**********                         26 'COMPANY        :'
     O**********               XXCO# 4   33
     O******* E  2             ROUTE1
     O**********                         26 'GROUP          :'
     O**********               XXFND 4   33
     O******* E  2             ROUTE1
     O**********                         26 'TYPE OF ACCOUNT:'
     O**********               CCETYP    31
      **** BEGIN ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE1
     O**********                         26 'BALANCE TYPE   :'
     O**********               CCEAFL    31
      **** END   ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE1
     O***  BEGIN CHG  ***  10/07/99  **********************************
     O********************************** 26 'BALANCE DATE   :'
     O*******                            26 'BILLNGS THRU DT:'
     O***  END   CHG  ***  10/07/99  **********************************
     O*******                  XXBLDT    38 '  /  /    '
     O***  BEGIN ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE1
     O**********                         26 'PAYMNTS THRU DT:'
     O**********               XXPTDT    38 '  /  /    '
     O***  END   ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE1
     O**********                         26 'MINIMUM BALANCE:'
     O**********               CCMIN 4   39
     O******* E  2             ROUTE1
     O**********                         26 'REPORT MONTH/YR:'
     O**********               XXSRMY    35 '  /    '
     O******* E  2             ROUTE1
     O**********                         26 'CANCEL REASON  :'
     O**********               CCRSN     32
     O******* E  2             ROUTE1
     O**********                         26 'COMMENT 1      :'
     O**********               CCCOM1    90
     O******* E  2             ROUTE1
     O**********                         26 'COMMENT 2      :'
     O**********               CCCOM2    90
     O******* E  2             ROUTE1
     O**********                         26 'CX NOTICE DATE :'
     O**********               X@DATE    38 '  /  /    '
     O******* E  2             ROUTE1
     O**********                         26 'CX EFFECTIVE DT:'
     O**********               X@CDAT    38 '  /  /    '
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE1         1
     O                                           26 'COMPANY        :'
     O                       XXCO#         4     33
     O          E            ROUTE1         1
     O                                           26 'GROUP          :'
     O                       XXFND         4     33
     O          E            ROUTE1         1
     O                                           26 'TYPE OF ACCOUNT:'
     O                       CCETYP              31
     O          E            ROUTE1         1
     O                                           26 'BALANCE TYPE   :'
     O                       CCEAFL              31
     O          E            ROUTE1         1
     O                                           26 'BILLNGS THRU DT:'
     O                       XXBLDT              38 '  /  /    '
     O          E            ROUTE1         1
     O                                           26 'PAYMNTS THRU DT:'
     O                       XXPTDT              38 '  /  /    '
     O          E            ROUTE1         1
      ***** BEG CHG ******* 05/02/03 **********************************
     O              N71                          26 'Minimum Balance:'
     O               71                          26 '% of Premium   :'
      ***** END CHG ******* 05/02/03 **********************************
     O                       CCMIN         4     39
     O          E            ROUTE1         1
      *** BEG CHG ***** 02/18/03 ****************************************
     O*******************                26 'REPORT MONTH/YR:'
     O*******************      XXSRMY    35 '  /    '
     O              N46                          26 'REPORT MONTH/YR:'
     O              N46      XXSRMY              35 '  /    '
     O               46                          26 'REPORT DATE    :'
     O               46      XXWEDT              37 '  /  /    '
      *** END CHG ***** 02/18/03 ****************************************
     O          E            ROUTE1         1
     O                                           26 'CANCEL REASON  :'
     O                       CCRSN               32
     O          E            ROUTE1         1
     O                                           26 'COMMENT 1      :'
     O                       CCCOM1              90
     O          E            ROUTE1         1
     O                                           26 'COMMENT 2      :'
     O                       CCCOM2              90
     O          E            ROUTE1         1
     O                                           26 'CX NOTICE DATE :'
     O                       X@DATE              38 '  /  /    '
     O          E            ROUTE1         1
     O                                           26 'CX EFFECTIVE DT:'
     O                       X@CDAT              38 '  /  /    '
     O******* E  160           ROUTE1
     O***** BEG   CHG **** 07/03/07 ***********************************
     O          E            ROUTE1         1
     O                                           26 'PRIMARY SORT   :'
     O                       QQSRT1              31
     O**
     O          E            ROUTE1         1
     O                                           26 'SECONDARY SORT :'
     O                       QQSRT2              31
      ***** BEG ADD ******* 07/24/08 **********************************
     O          E            ROUTE1         1
     O                                           26 'LABELS         :'
     O                       XXLABL              31
      ***** END ADD ******* 07/24/08 **********************************
     O***** END   CHG **** 07/03/07 ***********************************
     O***** END   CHG **** 04/10/00 ***********************************
     O***** BEG CHG ****** 07/03/07 ***********************************
     O*********                          23 '(C) COPYRIGHT PROGRESSI'
     O*********                          46 'VE DATA SOLUTIONS. INC,'
      ****** BEG DEL ***** 01/28/10 **********************************
     O*****   E   60           ROUTE1
     O****                               23 '(c) Copyright Fiserv,'
     O****                             +  1 'Inc.,'
      ****** END DEL ***** 01/28/10 **********************************
     O***** END CHG ****** 07/03/07 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *
      ***** BEG CHG ******* 09/02/05 **********************************
     O********QSYSPRT E  106           HEAD1
     OQSYSPRT4  E            HEAD1          1 06
      ***** END CHG ******* 09/02/05 **********************************
     O                       DSTIME              12 '  :  :  '
     O                       CONAME              83
     O                       DSDATE        Y    123
     O          E            HEAD1          2
     O                       Q1PGM               14
     O                                           51 'EMPLOYERS NOT CANCELLED'
     O                                           74 'BECAUSE OF NO EMPLOYER'
     O                                           92 'DETAIL RECORD    '
     O                                          117 'PAGE:'
     O                       XXPAG1        Z    123
     O          E            HEAD1          2
     O                                           13 'RUN DATE:'
     O                       DSDATE              22 '  /  /  '
     O                                           39 'NOTICE DATE:'
     O                       X@DATE              50 '  /  /    '
      **** BEGIN DEL **** 09/11/98 *********************************************
     O********************************** 73 'CX EFFECTIVE DATE:'
     O************************ X@CDAT    84 '  /  /    '
      **** END   DEL **** 09/11/98 *********************************************
     O          E            HEAD1          1
     O                                           09 'GROUP:'
     O                       XXFND         Z     13
     O                       XXFNNM              75
     O          E            HEAD1          1
     O                       XXFAD1              55
     O          E    21      HEAD1          1
     O                       XXFAD2              55
     O          E            HEAD1          2
     O                       XXFCSZ              55
     O          E            HEAD1          1
     O                                           13 'EMPLOYER'
     O                                           19 'DIV'
     O                                           35 'EMPLOYER NAME'
     O          E            HEAD1          1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                          100 '________________________'
     O                                          114 '________________________'
     O                                          124 '__________'
      ***** BEG CHG ******* 09/02/05 **********************************
     O*****QSYSPRT E 1              DET1
     OQSYSPRT4  E            DET1        1
      ***** END CHG ******* 09/02/05 **********************************
     O                       CNEMP#        4     13
     O                       CNDIV         4     19
     O                       CNENAM              61
      ***** BEG CHG ******* 09/02/05 **********************************
     O********QSYSPRT E 21             TOT1
     OQSYSPRT4  E            TOT1        2  1
      ***** END CHG ******* 09/02/05 **********************************
     O                                           24 'NUMBER OF EMPLOYERS'
     O                                           33 'PRINTED:'
     O                       XXCNT1        4     55
      ***** BEG CHG ******* 09/02/05 **********************************
     O*******QSYSPRT E 21             MSG1
     OQSYSPRT4  E            MSG1        2  1
      ***** END CHG ******* 09/02/05 **********************************
     O                                           27 '*** NO RECORD MEET THE'
     O                                           49 'SELECTED CRITERIA ***'
      *
      *****************************************************************
      *
     OQSYSPRT1  E            ROUTE2         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE2         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE2         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE2         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Chg ***** 08/05/15 ************************************
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE2         1
     O               08      ORDIS2              60
     O          E            ROUTE2         1
     O               08      ORDIS3              60
     O          E            ROUTE2         2
     O               08      ORDIS4              60
      ***** End Chg ***** 08/05/15 ************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE2         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE2         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE2         2 33
     O***** BEGIN CHG **** 04/10/00 ***********************************
     O**********                         18 'REPORT SELECTIONS:'
     O******* E  2             ROUTE2
     O**********                         26 'COMPANY        :'
     O**********               XXCO# 4   33
     O******* E  2             ROUTE2
     O**********                         26 'GROUP          :'
     O**********               XXFND 4   33
     O******* E  2             ROUTE2
     O**********                         26 'TYPE OF ACCOUNT:'
     O**********               CCETYP    31
      **** BEGIN ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE2
     O**********                         26 'BALANCE TYPE   :'
     O**********               CCEAFL    31
      **** END   ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE2
     O***  BEGIN CHG  ***  10/07/99  **********************************
     O********************************** 26 'BALANCE DATE   :'
     O**********                         26 'BILLNGS THRU DT:'
     O***  END   CHG  ***  10/07/99  **********************************
     O**********               XXBLDT    38 '  /  /    '
     O***  BEGIN ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE2
     O**********                         26 'PAYMNTS THRU DT:'
     O**********               XXPTDT    38 '  /  /    '
     O***  END   ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE2
     O**********                         26 'MINIMUM BALANCE:'
     O**********               CCMIN 4   39
     O******* E  2             ROUTE2
     O**********                         26 'REPORT MONTH/YR:'
     O**********               XXSRMY    35 '  /    '
     O******* E  2             ROUTE2
     O**********                         26 'CANCEL REASON  :'
     O**********               CCRSN     32
     O******* E  2             ROUTE2
     O**********                         26 'COMMENT 1      :'
     O**********               CCCOM1    90
     O******* E  2             ROUTE2
     O**********                         26 'COMMENT 2      :'
     O**********               CCCOM2    90
     O******* E  2             ROUTE2
     O**********                         26 'CX NOTICE DATE :'
     O**********               X@DATE    38 '  /  /    '
     O******* E  2             ROUTE2
     O**********                         26 'CX EFFECTIVE DT:'
     O**********               X@CDAT    38 '  /  /    '
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE2         1
     O                                           26 'COMPANY        :'
     O                       XXCO#         4     33
     O          E            ROUTE2         1
     O                                           26 'GROUP          :'
     O                       XXFND         4     33
     O          E            ROUTE2         1
     O                                           26 'TYPE OF ACCOUNT:'
     O                       CCETYP              31
     O          E            ROUTE2         1
     O                                           26 'BALANCE TYPE   :'
     O                       CCEAFL              31
     O          E            ROUTE2         1
     O                                           26 'BILLNGS THRU DT:'
     O                       XXBLDT              38 '  /  /    '
     O          E            ROUTE2         1
     O                                           26 'PAYMNTS THRU DT:'
     O                       XXPTDT              38 '  /  /    '
     O          E            ROUTE2         1
      ***** BEG CHG ******* 05/02/03 **********************************
     O              N71                          26 'Minimum Balance:'
     O               71                          26 '% of Premium   :'
      ***** END CHG ******* 05/02/03 **********************************
     O                       CCMIN         4     39
     O          E            ROUTE2         1
      *** BEG CHG ***** 02/18/03 ****************************************
     O*******************                26 'REPORT MONTH/YR:'
     O*******************      XXSRMY    35 '  /    '
     O              N46                          26 'REPORT MONTH/YR:'
     O              N46      XXSRMY              35 '  /    '
     O               46                          26 'REPORT DATE    :'
     O               46      XXWEDT              37 '  /  /    '
      *** END CHG ***** 02/18/03 ****************************************
     O          E            ROUTE2         1
     O                                           26 'CANCEL REASON  :'
     O                       CCRSN               32
     O          E            ROUTE2         1
     O                                           26 'COMMENT 1      :'
     O                       CCCOM1              90
     O          E            ROUTE2         1
     O                                           26 'COMMENT 2      :'
     O                       CCCOM2              90
     O          E            ROUTE2         1
     O                                           26 'CX NOTICE DATE :'
     O                       X@DATE              38 '  /  /    '
     O          E            ROUTE2         1
     O                                           26 'CX EFFECTIVE DT:'
     O                       X@CDAT              38 '  /  /    '
     O***** BEG   CHG **** 07/03/07 ***********************************
     O          E            ROUTE2         1
     O                                           26 'PRIMARY SORT   :'
     O                       QQSRT1              31
     O**
     O          E            ROUTE2         1
     O                                           26 'SECONDARY SORT :'
     O                       QQSRT2              31
     O***** END   CHG **** 07/03/07 ***********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     O          E            ROUTE2         1
     O                                           26 'LABELS         :'
     O                       XXLABL              31
      ***** END ADD ******* 07/24/08 **********************************
     O***** END   CHG **** 04/10/00 ***********************************
     O***** BEG CHG ****** 07/03/07 ***********************************
     O*********                          23 '(C) COPYRIGHT PROGRESSI'
     O*********                          46 'VE DATA SOLUTIONS. INC,'
      ****** BEG DEL ***** 01/28/10 **********************************
     O***     E  160           ROUTE2
     O***                                23 '(c) Copyright Fiserv,'
     O***                              +  1 'Inc.,'
      ****** END DEL ***** 01/28/10 **********************************
     O***** END CHG ****** 07/03/07 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD2          1 06
     O                       DSTIME              12 '  :  :  '
     O                       CONAME              83
     O                       DSDATE        Y    123
     O          E            HEAD2          2
     O                       Q1PGM               14
     O                                           64 'EMPLOYERS NOT CANCELLED'
     O                                           85 'BECAUSE OF CX STATUS'
     O                                          117 'PAGE:'
     O                       XXPAG2        Z    123
     O          E            HEAD2          1
     O                                           13 'RUN DATE:'
     O                       DSDATE              22 '  /  /  '
     O                                           39 'NOTICE DATE:'
     O                       X@DATE              50 '  /  /    '
      **** BEGIN DEL **** 09/11/98 *********************************************
     O********************************** 73 'CX EFFECTIVE DATE:'
     O************************ X@CDAT    84 '  /  /    '
      **** END   DEL **** 09/11/98 *********************************************
     O          E            HEAD2          2
      *** BEG CHG ***** 02/18/03 ***********************************************
     O************************           10 'GROUP:'
     O                                            8 'GRP:'
      *** END CHG ***** 02/18/03 ***********************************************
     O                       XXFND         Z     13
     O                       XXFNNM              75
     O          E            HEAD2          1
     O                                           13 'EMPLOYER'
     O                                           19 'DIV'
     O                                           34 'EMPLOYER NAME'
     O                                           72 'BILLING'
     O                                           79 'YEAR'
     O                                           95 'AMOUNT DUE'
      *** BEG CHG ***** 02/18/03 ***********************************************
     O********************              103 '# OF'
     O              N46                         103 '# OF'
     O               46                         107 '# OF'
      *** END CHG ***** 02/18/03 ***********************************************
      *** BEG CHG ***** 02/18/03 ***********************************************
     O********************              110 'CX'
     O                                          115 'CX'
      *** END CHG ***** 02/18/03 ***********************************************
     O          E            HEAD2          0
     O                                           73 'SCHD CD/T'
      *** BEG CHG ***** 02/18/03 ***********************************************
     O********************              104 'MONTHS'
     O********************              111 'FLAG'
     O              N46                         104 'MONTHS'
     O               46                         108 'REPORTS'
     O                                          116 'FLAG'
      *** END CHG ***** 02/18/03 ***********************************************
     O          E            HEAD2          1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                          100 '________________________'
     O                                          114 '________________________'
     O                                          124 '__________'
     OQSYSPRT1  E            DET2        1
     O                       CNEMP#        4     13
     O                       CNDIV         4     19
     O                       CNENAM              61
     O                       ELBSCD              68
     O                       BXTYPE              73
     O                       XXFYR         4     79
     O                       CNAMTD        4     95
      *** BEG CHG ***** 02/18/03 ***********************************************
     O********************     XXMTHS4  102
     O********************     CNSELC   110
     O              N47      XXMTHS        4    102
     O              N47                         109 'Months'
     O               47      XX#WKS        4    102
     O               47                         108 'Weeks'
     O                       CNSELC             115
      *** END CHG ***** 02/18/03 ***********************************************
     OQSYSPRT1  E            TOT2        2  1
     O                                           24 'NUMBER OF EMPLOYERS'
     O                                           35 'PROCESSED:'
     O                       XXCNT2        4     55
     OQSYSPRT1  E            MSG2        2  1
     O                                           27 '*** NO RECORD MEET THE'
     O                                           49 'SELECTED CRITERIA ***'
      *
      *************************************************************
      *
     OQSYSPRT2  E            ROUTE3         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE3         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE3         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE3         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE3         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE3         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE3         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE3         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE3         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE3         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE3         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE3         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE3         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE3         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE3         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE3         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Chg ***** 08/05/15 ************************************
     O          E            ROUTE3         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE3         1
     O               08      ORDIS2              60
     O          E            ROUTE3         1
     O               08      ORDIS3              60
     O          E            ROUTE3         2
     O               08      ORDIS4              60
      ***** End Chg ***** 08/05/15 ************************************
     O          E            ROUTE3         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE3         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE3         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE3         2 33
     O***** BEGIN CHG **** 04/10/00 ***********************************
     O**********                         18 'REPORT SELECTIONS:'
     O******* E  2             ROUTE3
     O**********                         26 'COMPANY        :'
     O**********               XXCO# 4   33
     O******* E  2             ROUTE3
     O**********                         26 'GROUP          :'
     O**********               XXFND 4   33
     O******* E  2             ROUTE3
     O**********                         26 'TYPE OF ACCOUNT:'
     O**********               CCETYP    31
      **** BEGIN ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE3
     O**********                         26 'BALANCE TYPE   :'
     O**********               CCEAFL    31
      **** END   ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE3
     O***  BEGIN CHG  ***  10/07/99  **********************************
     O********************************** 26 'BALANCE DATE   :'
     O**********                         26 'BILLNGS THRU DT:'
     O***  END   CHG  ***  10/07/99  **********************************
     O**********               XXBLDT    38 '  /  /    '
     O***  BEGIN ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE3
     O**********                         26 'PAYMNTS THRU DT:'
     O**********               XXPTDT    38 '  /  /    '
     O***  END   ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE3
     O**********                         26 'MINIMUM BALANCE:'
     O**********               CCMIN 4   39
     O******* E  2             ROUTE3
     O**********                         26 'REPORT MONTH/YR:'
     O**********               XXSRMY    35 '  /    '
     O******* E  2             ROUTE3
     O**********                         26 'CANCEL REASON  :'
     O**********               CCRSN     32
     O******* E  2             ROUTE3
     O**********                         26 'COMMENT 1      :'
     O**********               CCCOM1    90
     O******* E  2             ROUTE3
     O**********                         26 'COMMENT 2      :'
     O**********               CCCOM2    90
     O******* E  2             ROUTE3
     O**********                         26 'CX NOTICE DATE :'
     O**********               X@DATE    38 '  /  /    '
     O******* E  2             ROUTE3
     O**********                         26 'CX EFFECTIVE DT:'
     O**********               X@CDAT    38 '  /  /    '
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE3         1
     O                                           26 'COMPANY        :'
     O                       XXCO#         4     33
     O          E            ROUTE3         1
     O                                           26 'GROUP          :'
     O                       XXFND         4     33
     O          E            ROUTE3         1
     O                                           26 'TYPE OF ACCOUNT:'
     O                       CCETYP              31
     O          E            ROUTE3         1
     O                                           26 'BALANCE TYPE   :'
     O                       CCEAFL              31
     O          E            ROUTE3         1
     O                                           26 'BILLNGS THRU DT:'
     O                       XXBLDT              38 '  /  /    '
     O          E            ROUTE3         1
     O                                           26 'PAYMNTS THRU DT:'
     O                       XXPTDT              38 '  /  /    '
     O          E            ROUTE3         1
      ***** BEG CHG ******* 05/02/03 **********************************
     O              N71                          26 'Minimum Balance:'
     O               71                          26 '% of Premium   :'
      ***** END CHG ******* 05/02/03 **********************************
     O                       CCMIN         4     39
     O          E            ROUTE3         1
      *** BEG CHG ***** 02/18/03 ****************************************
     O*******************                26 'REPORT MONTH/YR:'
     O*******************      XXSRMY    35 '  /    '
     O              N46                          26 'REPORT MONTH/YR:'
     O              N46      XXSRMY              35 '  /    '
     O               46                          26 'REPORT DATE    :'
     O               46      XXWEDT              37 '  /  /    '
      *** END CHG ***** 02/18/03 ****************************************
     O          E            ROUTE3         1
     O                                           26 'CANCEL REASON  :'
     O                       CCRSN               32
     O          E            ROUTE3         1
     O                                           26 'COMMENT 1      :'
     O                       CCCOM1              90
     O          E            ROUTE3         1
     O                                           26 'COMMENT 2      :'
     O                       CCCOM2              90
     O          E            ROUTE3         1
     O                                           26 'CX NOTICE DATE :'
     O                       X@DATE              38 '  /  /    '
     O          E            ROUTE3         1
     O                                           26 'CX EFFECTIVE DT:'
     O                       X@CDAT              38 '  /  /    '
     O***** BEG   CHG **** 07/03/07 ***********************************
     O          E            ROUTE3         1
     O                                           26 'PRIMARY SORT   :'
     O                       QQSRT1              31
     O**
     O          E            ROUTE3         1
     O                                           26 'SECONDARY SORT :'
     O                       QQSRT2              31
     O***** END   CHG **** 07/03/07 ***********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     O          E            ROUTE3         1
     O                                           26 'LABELS         :'
     O                       XXLABL              31
      ***** END ADD ******* 07/24/08 **********************************
     O***** END   CHG **** 04/10/00 ***********************************
     O***** BEG CHG ****** 07/03/07 ***********************************
     O*********                          23 '(C) COPYRIGHT PROGRESSI'
     O*********                          46 'VE DATA SOLUTIONS. INC,'
      ****** BEG DEL ***** 01/28/10 **********************************
     O*****   E  160           ROUTE3
     O***                                23 '(c) Copyright Fiserv,'
     O***                              +  1 'Inc.,'
      ****** END DEL ***** 01/28/10 **********************************
     O***** END CHG ****** 07/03/07 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE3         1 59
     O                       #BRND1
     O          E            ROUTE3         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     OQSYSPRT2  E            HEAD3          1 06
     O                       DSTIME              12 '  :  :  '
     O                                           64 'CANCELLATION'
     O                                           77 'MAILING LIST'
     O                       DSDATE        Y    123
     O          E            HEAD3          1
     O                       Q1PGM               14
     O                       XXFNNM             104
     O                                          117 'PAGE:'
     O                       XXPAG3        Z    123
     O          E            HEAD3          1
     O                       XXFAD1              84
     O          E            HEAD3          1
     O                       XXFCSZ              84
     O          E            HEAD3          0
     O                                           16 '____________'
     O                                           25 '_________'
     O          E            HEAD3          1
     O                                           16 'NOTICE DATE:'
     O                       X@DATE              27 '  /  /    '
     O          E            HEAD3          1
     O                                           12 'POLICY #'
     O                                           27 'NAME'
     O                                           74 'ADDRESS'
     O          E            HEAD3          0
     O                                           31 'DBA NAME'
     O                                           79 'CITY & STATE'
     O                                          118 'ZIP CODE'
     O          E            HEAD3          1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                          100 '________________________'
     O                                          114 '________________________'
     O                                          124 '__________'
     OQSYSPRT2  E            DET3        1
     O                       CNEMP#        4     13
     O                       CNDIV         4     19
     O                                           21 '|'
     O                       CNENAM              63
     O                       SVADR1             107
     O          E            DET3        1
     O                                           21 '|'
     O                       SVFDNM              63
     O              N25      SVADR2             107
     O               25      XXCITY              97
      ***** BEG CHG ******* 03/21/03 **********************************
     O*********        25      SVZIP    120 '     -    '
     O               25      SVZIP              120 '0     -    '
      ***** END CHG ******* 03/21/03 **********************************
     O          E   N25      DET3        1
     O                       XXCITY              97
      ***** BEG CHG ******* 03/21/03 **********************************
     O*********                SVZIP    120 '     -    '
     O                       SVZIP              120 '0     -    '
      ***** END CHG ******* 03/21/03 **********************************
     OQSYSPRT2  E            TOT3             57
     O                                           24 'State Office: Please'
     O                                           49 'return one copy to above'
     O                                           71 'address after putting'
     O                                           77 'check'
     O          E            TOT3        1
     O                                           41 'mark in box to indicate'
     O                                           65 'receipt of cancellation'
     O                                           73 'notice.'
     O          E            TOT3        1
     O                                           83 '________________________'
     O                                          101 '__________________'
     O          E            TOT3        1
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT3             61
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT3        1
     O                                           41 'Please sign            '
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT3        1
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT3        0
     O                                           83 '________________________'
     O                                          101 '__________________'
      *
      ****************************************************************
      *
     OQSYSPRT3  E            ROUTE4         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE4         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE4         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE4         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE4         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE4         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE4         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE4         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE4         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE4         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE4         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE4         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE4         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE4         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE4         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE4         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Chg ***** 08/05/15 ************************************
     O          E            ROUTE4         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE4         1
     O               08      ORDIS2              60
     O          E            ROUTE4         1
     O               08      ORDIS3              60
     O          E            ROUTE4         2
     O               08      ORDIS4              60
      ***** End Chg ***** 08/05/15 ************************************
     O          E            ROUTE4         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE4         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE4         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE4         2 33
     O***** BEGIN CHG **** 04/10/00 ***********************************
     O**********                         18 'REPORT SELECTIONS:'
     O******* E  2             ROUTE4
     O**********                         26 'COMPANY        :'
     O**********               XXCO# 4   33
     O******* E  2             ROUTE4
     O**********                         26 'GROUP          :'
     O**********               XXFND 4   33
     O******* E  2             ROUTE4
     O**********                         26 'TYPE OF ACCOUNT:'
     O**********               CCETYP    31
      **** BEGIN ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE4
     O**********                         26 'BALANCE TYPE   :'
     O**********               CCEAFL    31
      **** END   ADD **** 12/02/99 ************************************
     O******* E  2             ROUTE4
     O***  BEGIN CHG  ***  10/07/99  **********************************
     O********************************** 26 'BALANCE DATE   :'
     O**********                         26 'BILLNGS THRU DT:'
     O***  END   CHG  ***  10/07/99  **********************************
     O**********               XXBLDT    38 '  /  /    '
     O***  BEGIN ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE4
     O**********                         26 'PAYMNTS THRU DT:'
     O**********               XXPTDT    38 '  /  /    '
     O***  END   ADD  ***  10/07/99  **********************************
     O******* E  2             ROUTE4
     O**********                         26 'MINIMUM BALANCE:'
     O**********               CCMIN 4   39
     O******* E  2             ROUTE4
     O**********                         26 'REPORT MONTH/YR:'
     O**********               XXSRMY    35 '  /    '
     O******* E  2             ROUTE4
     O**********                         26 'CANCEL REASON  :'
     O**********               CCRSN     32
     O******* E  2             ROUTE4
     O**********                         26 'COMMENT 1      :'
     O**********               CCCOM1    90
     O******* E  2             ROUTE4
     O**********                         26 'COMMENT 2      :'
     O**********               CCCOM2    90
     O******* E  2             ROUTE4
     O**********                         26 'CX NOTICE DATE :'
     O**********               X@DATE    38 '  /  /    '
     O******* E  2             ROUTE4
     O**********                         26 'CX EFFECTIVE DT:'
     O**********               X@CDAT    38 '  /  /    '
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE4         1
     O                                           26 'COMPANY        :'
     O                       XXCO#         4     33
     O          E            ROUTE4         1
     O                                           26 'GROUP          :'
     O                       XXFND         4     33
     O          E            ROUTE4         1
     O                                           26 'TYPE OF ACCOUNT:'
     O                       CCETYP              31
     O          E            ROUTE4         1
     O                                           26 'BALANCE TYPE   :'
     O                       CCEAFL              31
     O          E            ROUTE4         1
     O                                           26 'BILLNGS THRU DT:'
     O                       XXBLDT              38 '  /  /    '
     O          E            ROUTE4         1
     O                                           26 'PAYMNTS THRU DT:'
     O                       XXPTDT              38 '  /  /    '
     O          E            ROUTE4         1
      ***** BEG CHG ******* 05/02/03 **********************************
     O              N71                          26 'Minimum Balance:'
     O               71                          26 '% of Premium   :'
      ***** END CHG ******* 05/02/03 **********************************
     O                       CCMIN         4     39
     O          E            ROUTE4         1
      *** BEG CHG ***** 02/18/03 ****************************************
     O*******************                26 'REPORT MONTH/YR:'
     O*******************      XXSRMY    35 '  /    '
     O              N46                          26 'REPORT MONTH/YR:'
     O              N46      XXSRMY              35 '  /    '
     O               46                          26 'REPORT DATE    :'
     O               46      XXWEDT              37 '  /  /    '
      *** END CHG ***** 02/18/03 ****************************************
     O          E            ROUTE4         1
     O                                           26 'CANCEL REASON  :'
     O                       CCRSN               32
     O          E            ROUTE4         1
     O                                           26 'COMMENT 1      :'
     O                       CCCOM1              90
     O          E            ROUTE4         1
     O                                           26 'COMMENT 2      :'
     O                       CCCOM2              90
     O          E            ROUTE4         1
     O                                           26 'CX NOTICE DATE :'
     O                       X@DATE              38 '  /  /    '
     O          E            ROUTE4         1
     O                                           26 'CX EFFECTIVE DT:'
     O                       X@CDAT              38 '  /  /    '
     O***** BEG   CHG **** 07/03/07 ***********************************
     O          E            ROUTE4         1
     O                                           26 'PRIMARY SORT   :'
     O                       QQSRT1              31
     O**
     O          E            ROUTE4         1
     O                                           26 'SECONDARY SORT :'
     O                       QQSRT2              31
     O***** END   CHG **** 07/03/07 ***********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     O          E            ROUTE4         1
     O                                           26 'LABELS         :'
     O                       XXLABL              31
      ***** END ADD ******* 07/24/08 **********************************
     O***** END   CHG **** 04/10/00 ***********************************
     O***** BEG CHG ****** 07/03/07 ***********************************
     O*********                          23 '(C) COPYRIGHT PROGRESSI'
     O*********                          46 'VE DATA SOLUTIONS. INC,'
      ****** BEG DEL ***** 01/28/10 **********************************
     O*****   E  160           ROUTE4
     O***                                23 '(c) Copyright Fiserv,'
     O***                              +  1 'Inc.,'
      ****** END DEL ***** 01/28/10 **********************************
     O***** END CHG ****** 07/03/07 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE4         1 59
     O                       #BRND1
     O          E            ROUTE4         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     OQSYSPRT3  E            HEAD4          1 06
     O                       DSTIME              12 '  :  :  '
     O                                           64 'CANCELLATION'
     O                                           78 'SUSPENSE LIST'
     O                       DSDATE        Y    123
     O          E            HEAD4          1
     O                       Q1PGM               14
     O                       XXFNMC              94
     O                                          117 'PAGE:'
     O                       XXPAG4        Z    123
     O          E            HEAD4       1
     O                                           16 'NOTICE DATE:'
     O                       X@DATE              27 '  /  /    '
     O          E            HEAD4       0  1
     O                                           16 '____________'
     O                                           27 '___________'
     O          E            HEAD4       1
     O                                           12 'POLICY #'
     O                                           31 'DBA NAME'
     O                                           73 'CANCEL'
     O                                           83 'REASON'
     O          E            HEAD4       1
     O                                           27 'NAME'
     O                                           74 'EFF DATE'
     O          E            HEAD4       0  1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                          100 '________________________'
     O                                          114 '________________________'
     O                                          124 '__________'
     OQSYSPRT3  E            DET4        1
     O                       CNEMP#        4     13
     O                       CNDIV         4     19
     O                       CNENAM              63
      **** BEGIN CHG **** 09/11/98 *********************************************
     O************************ X@CDAT    75 '  /  /    '
     O                       XXCEDT              75 '  /  /    '
      **** END   CHG **** 09/11/98 *********************************************
     O                       XREASN             127
     O          E            DET4        1
     O                       SVFDNM              63
      ***** BEG ADD ******* 09/24/99 **********************************
     O                       SVPH#             +014
      ***** END ADD ******* 09/24/99 **********************************
     O          E            DET4        1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                          100 '________________________'
     O                                          114 '________________________'
     O                                          124 '__________'
     OQSYSPRT3  E            TOT4        2  1
     O                                           24 'NUMBER OF EMPLOYERS'
     O                                           35 'PROCESSED:'
     O                       XXCNT4        4     46
     OQSYSPRT3  E            MSG4        2  1
     O                                           27 '*** NO RECORD MEET THE'
     O                                           49 'SELECTED CRITERIA ***'
      *** beg add ***** 01/13/17 ******************
     P*============================================
     P* Determine if the object exists
     P*============================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
      *** end add ***** 01/13/17 ******************
