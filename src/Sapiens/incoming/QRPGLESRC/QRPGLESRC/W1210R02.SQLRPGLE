     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
      /title W1210R02 - A/R Batch Detail Info
     H option(*nodebugio: *srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H debug(*yes) bnddir('WTBNDDIR')
      *****************************************************************
      *  Created:                                                     *
      *  01/16/98  C:69  Project: WC753                               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 03/18/25 C:300 Support: 118661 (LUBA)                         *
      *                A/R Batch Entry not accepting entries          *
      *                Receiver value too small to hold result        *
      *                                                               *                            *
      * 08/27/24 C:062 Ticket: 115609(133)                            *
      *                Check to see if a payment is being reversed    *
      *                and create window to confrm if they want to    *                            *
      *                resend the invoice to IVC.                     *                            *
      *                                                               *                            *
      * 06/24/24 C:079 Project: 146WKR                                *
      *                Collateral Processing - Recomile               *
      *                                                               *
      * 10/20/23 C:65  SUPPORT:114398 (InHouse)                       *
      *                Remove the edit on preventing billing trans-   *
      *                actions from being posted to a prior month.    *
      *                This will prevent posting possibly needed      *
      *                adjustments for IVC.  The estimated, revised,  *
      *                and audit bills already allow for back posting.*
      *                                                               *
      * 02/03/23 C:300 Project: WC2675                                *
      *                Some Correction to Invoice Cloud changes       *
      *                                                               *
      * 12/02/22 C:062 Project: WC2663                                *
      *                Assign invoice#                                *
      *                                                               *
      * 04/11/22 C:340 Project: WC2663                                *
      *                Invoice Cloud - Add Invoice Type ID            *
      *                                                               *
      * 01/11/22 C:136 SUPPORT:99895 (InHouse)                        *
      *                Prevent policies not on EFT from being         *
      *                added to EFT Batch.                            *
      *                                                               *
      * 05/04/21 C:136 Project: WC1986                                *
      *                Remove 114WYI per GW (Not commented)           *
      *                Cleaned up old comment lines                   *                            *
      *                                                               *
      * 03/25/21 C:79  Project: WC2663                                *
      *                Invoice Cloud changes                          *
      *                                                               *
      * 01/10/18 C:33  Support 63341                                  *
      *                Clear agt# before WA002R call                  *
      *                                                               *
      * 12/14/15 C:136 PhaseWare 38850                                *
      *               -Rework edits so that they don't invoke until   *
      *                the Employer Info displays so that screen      *
      *                works smoother.                                *
      *               -Non cash dissolve edits should work only       *
      *                in non cash dissolve mode (svCash <> 'Y').     *
      *                                                               *
      * 08/06/15 C:298 Support: Phaseware 35393                       *
      *                Added edit on screen that Posting amount       *
      *                cannot be zero.                                *
      *                                                               *
      * 05/21/14 C:136 Support: RED4181 (CareWest)                    *
      *                Fix logic for dissolving renewal cash so       *
      *                that the current year will be dispersed to     *
      *                if a future year doesn't exist.                *
      *                                                               *
      * 02/27/14 C:295 Support: WA099 84933                           *
      *                Pop-up window with message that employer       *
      *                is not eligible for Auto cash has 3 buttons    *
      *                Correcting the pop-ups with correct buttons    *
      *                (Changes is in display file).                  *
      *                                                               *
      * 02/27/14 C:295 Support: WA099 84932                           *
      *                The Renewal Cash Only Flag should Be Check box *
      *                instead of text box.Correcting as Check box.   *
      *                                                               *
      * 03/14/13 C:279 Projecr: SY266 Redmine: 2442                   *
      *                Corrected Auto Cash A/R Batch Edit             *
      *                                                               *
      * 11/10/11 C:136 Support: WA132 77464                           *
      *                Fix date edits to check for *mdy type date     *
      *                                                               *
      * 11/07/11 C:062 Support: WA025 78298                           *
      *                Pass invoice # into SRINV# if entered.         *
      *                                                               *
      * 06/14/11 C:191 Project: SY255 Priss #56                       *
      *                Fix edit condition introduced in WC1932        *
      *                                                               *
      * 04/21/11 C:190 Project: 114WYI                                *
      *                ACH for Premium Payments via Agent.            *
      *                                                               *
      * 02/28/11 C:220 Project: WC1932                                *
      *                Added code for Exclude from Auto Cash Dissolve *
      *                                                               *
      * 01/14/11 C:136 Support: WA114 75441                           *
      *                Cash dissolve mode invoked for a transaction   *
      *                that was not in a cash dissolve batch.         *
      *                Rename fields for PEA02 to prevent over-       *
      *                writing field values from PEA01                *
      *                                                               *
      * 01/13/11 C:257 Project: WC1948                                *
      *                If the A/R Batch has been marked 'Y' for       *
      *                approved and/or 'Y' for transfer display a     *
      *                pop-up window                                  *
      *                                                               *
      * 10/06/10 C:255 Support: 74184                                 *
      *                Billing Adjustment can be back dated for A/R   *
      *                code 'S/R' and can be back posted for the same *
      *                month only when A/R code other than 'S/R'      *
      *
      * 09/30/10 C:220 Support: 73814                                 *
      *                Correct the XXNINV uses,clear after write/updt *
      *                                                               *
      * 09/13/10 C:136 Project: 71WKU PRISS 45                        *
      *                Year does not default correctly for            *
      *                new/renew flag. Also, only cash dissolve       *
      *                batches should default the year.               *
      *                                                               *
      * 08/09/10 C:191 Project: 71WKU                                 *
      *                Add transaction date field for Retro type      *
      *                                                               *
      * 06/08/10 C:227 Support: 72996                                 *
      *                Changed edit to check for prior Inv#           *
      *                                                               *
      * 01/04/10 C:190 Project: 25WRE                                 *
      *                Called 'SRINV#' instead 'RetrieveInv#'.        *
      *                                                               *
      * 11/20/09 C:33  Project: 25WQI                                 *
      *                Add statement# (invoice#) entry.               *
      *                                                               *
      * 10/27/09 C:136 Project: WC1854                                *
      *                Change edit that prevents a cash dissolve      *
      *                on a group not set up for cash dissolve.       *
      *                                                               *
      * 05/03/09 C:136 Project: WC1663                                *
      *                Change verbiage on cash dissolve renewal flag. *
      *                Use transaction date to find year to dissolve  *
      *                to for Lockbox transactions                    *
      *                                                               *
      * 09/23/08 C:136 Support: WA146 64767                           *
      *                Edit for valid Agent History record from       *
      *                transaction date if provided, per GW           *
      *                                                               *
      * 06/24/08 C:189 Project: SY156                                 *
      *                Reverse image remaining on Activity Status     *
      *                field after line  item  submission has been    *
      *                corrected
      *                                                               *
      * 02/16/07 C:136 SUPPORT: WA091 56836                           *
      *                FIX EMP SEARCH SO THAT CURSOR TABS TO          *
      *                CHECK # IN CASH DISSOLVE MODE                  *
      *                                                               *
      * 05/25/06 C:136 Support: WA109 51792                           *
      *                Add edit to prevent the same check from        *
      *                being dissolved twice to the same policy.      *
      *               (Added this code from BASEPNSRC but noticed     *
      *                that the code was incorrect and the error      *
      *                message missing for this support item, so      *
      *                the code was changed and the error message     *
      *                added so this edit will work.)                 *
      *                                                               *
      * 05/15/06 C:136 Support: WA109 51569                           *
      *                Allow cash dissolve on transaction if it       *
      *                has already been created, but not dissolved    *
      *                in a cash dissolve batch.                      *
      *                                                               *
      * 04/07/06 C:136 Support: WA109 52148                           *
      *                Invoke edit to prevent zero check amount       *
      *                on cash dissolve batch to prevent bad records  *
      *                from being transferred into A/R                *
      *                                                               *
      * 02/21/06 C:136 Support: WA109 51569                           *
      *                Allow user to re-enter batch detail and add    *
      *                cash dissolve transactions.                    *
      *                                                               *
      * 01/20/06 C:136 Support: WA129 50989                           *
      *                - Edit so that only transactions eligible for  *
      *                  cash dissolve can be added to cash dissolve  *
      *                  batch.                                       *
      *                - Remove window asking if user wants to cash   *
      *                  dissolve.                                    *
      *                                                               *
      * 01/06/06 C:136 Project: WC1179                                *
      *                Default the cash dissolve flag to 'Y' in the   *
      *                pop up window per KK.                          *
      *                                                               *
      * 12/05/05 C:167 Project: 129WMM                                *
      *                Fold client modifications into base.           *
      *                                                               *
     H* 10/07/05 C:136 Project: WC1179                                *
     H*                Delete previous code put in for WC1179.  Add   *
     H*                current code to auto dissolve cash payment     *
     H*                                                               *
     H* 07/29/05 C:33  Project: 133WBU                                *
     H*                Update retrieval of CLTID.                     *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205                                *
     H*              - Access activity status code from employer      *
     H*                detail WDELP rather than master WMEMP.         *
      *                                                               *
      * 06/23/05   c: 156  Project: 133WBU                            *
      *                    A/R Batch entry defaults                   *
      *                                                               *
      * 03/03/05   C: 062  Project: WC1229                            *
      *                   - Add group restrictions.                   *
      *                                                               *
      * 02/18/05   C: 132  Project: WC1179                            *
      *                   - If called in add mode and if flagged as   *
      *                     auto cash dissolve, call API WA055R to    *
      *                     create the records in WZEAP.              *
      *                                                               *
      * 01/20/05   C: 132  Project: WC1179                            *
      *                   - If called in display mode, past it on to  *
      *                     W1210R03 to force display in that program *
      *                                                               *
      * 09/07/04   C: 132  Project: WC1179                            *
      *                   - If EACASH is a "Y", then do not display   *
      *                     Policy year, Posting amount and the A/R   *
      *                     Description on Screen 01 if the A/R code  *
      *                     Entered into W0010r61 matched the A/R     *
      *                     code for the batch. If both conditions    *
      *                     are true, it is and AUTO CASH DISSOLVE    *
      *                     batch.                                    *
      *                                                               *
      * 05/17/04   C: 132  Project: WC1190                            *
      *                   - If EAIDT is zero, update with the EAIDT   *
      *                     which is the earliest EADT in the batch#, *
      *                     if there are more records in the batch.   *
      *                   - On ADD, check to see if have other recrds *
      *                     for same batch. If so, use the earlist    *
      *                     EAIDT (or EADT if EAIDTs are zeros for    *
      *                     ADD records EAIDT.                        *
      *                                                               *
      * 04/13/04 C:132 PROJECT:  WC1190  WA109 42171                  *
      *                Modify Activity Status field to display in     *
      *                Reverse Image if exists.                       *
      *              - If ADD mode or EAIDT = zeros, update with date *
      *                                                               *
     * 02/19/04 C:136 Project: WC1033 (Return Premium)               *
     *                Add processing for EAARTY = 'T' (true void)    *
     *                transaction.                                   *
     *                                                               *
      * 01/27/04 C:136 Support: WA090 40857                           *
      *                Remove the edit that requires a check amount   *
      *                per GW.                                        *
      *                                                               *
      * 12/12/03 C:136 Support: 40350                                 *
      *                Modify edit that requires a check amount to    *
      *                be invoked only if the transaction is a        *
      *                payment type.                                  *
      *                                                               *
      * 11/04/03 C:136 PROJECT: WC1161                                *
      *                Add code for new A/R category field in WMEAP.  *
      *                                                               *
      * 09/24/03 C:136 Support: 39075 (WC1116)                        *
      *                Add processing for initial maintenance fields. *
      *                The initial date field will be used as the     *
      *                ACH batch creation date.                       *
      *                                                               *
      * 07/11/03 C:136 SUPPORT: 35411                                 *
      *                Add edit for check amount <> 0.                *
      *                                                               *
      * 06/16/03 C:136 Support: WC1117                                *
      *                ACH Flag in 'ADD' mode if batch detail is      *
      *                added.                                         *
      *                                                               *
      * 05/08/03 C:136 Support: WA081 37150                           *
      *                Positive balance message being set on in       *
      *                error if last record used to calculate xxbal   *
      *                is a refund type.                              *
      *                                                               *
      * 04/16/03 C:142 WC1117  LOCK BOX PROCESSING CHANGES            *
      *                                                               *
      * 10/02/02 C:61  SUPPORT WA081 33774                            *
      *                Entry of *HIVAL for 'Check number' or          *
      *                'Check amount' causes processing errors in     *
      *                W1210F/R.  Add edit to prevent entry of *HIVAL *
      *                for these two fields.                          *
      *                                                               *
      * 06/10/02 C:136 SUPPORT: WA093 32918                           *
      *                Positve balance message (*IN88) is being set   *
      *                on in error. A/R Batch Detail records that     *
      *                have been transferred need to be excluded from *
      *                calculation of XXBAL.                          *
      *                                                               *
      * 05/10/01 C:134 PROJECT:  SUPPORT WA109/27410                  *
      *                ALOW BACK DATING FOR NS                        *
      *                                                               *
      * 10/19/00 C:33  PROJECT: WC899                                 *
      *                IF POLICY IS ACCOUNT CURRENT AND A PAYMENT IS  *
      *                ENTERED, DISPLAY MESSAGE.                      *
      *                                                               *
      * 09/05/00 C:61  PROJECT WC577                                  *
      *              - Don't require entry of Group for Employer      *
      *                search.                                        *
      *              - Return Group number with selection from        *
      *                Employer search.                               *
      *              - Subset employer search to Group, if Group      *
      *                has been entered.                              *
      *                                                               *
      * 08/02/00 C:61  SUPPORT WC999 23811                            *
      *                Correct display of employer information.       *
      *                                                               *
      * 07/28/00 C:61  SUPPORT 23724                                  *
      *                Always show CX date (if employer CX).          *
      *                                                               *
      * 06/29/00 C:33 PROJECT: WC577                                  *
      *               WHEN ENTERING A REFUND, INFORM THE USER WHEN THE*
      *               NET BALANCE BECOMES NEGATIVE.                   *
      *                                                               *
      *  06/09/00 C:33 PROJECT: WC696                                 *
      *                ADD A/R HISTORICAL SEARCH.                     *
      *                                                               *
      *  01/24/00 C:33 PROJECT: WC577                                 *
      *                KEEP PROGRAM FROM HAVING A DATA DECIMAL ERROR  *
      *                WHEN SYSTEM CODE IS NOT A SYSTEM TYPE.         *
      *                                                               *
      *  11/12/99 C:33 PROJECT: WC577                                 *
      *                GET THE EMPLOYER INFO WHEN EMPLOYER CHANGES    *
      *                                                               *
      *  11/11/99 C:33 PROJECT: WC910  WA081/20127                    *
      *                COMPARE TOTAL CHECK AMOUNT POSTED TO THE EMPLR *
      *                W/O DIVISION OR YEARS.                         *
      *                                                               *
      *  09/29/99 C:118 PROJECT: WC939                                *
      *                 Added 'Premium Finance Co.' in RI in display  *
      *                 if a premium finance co. has been assigned to *
      *                 the policy.  Added F7=Finance info which calls*
      *                 W0045R02 WW Premium Finance Co. maintenance   *
      *                 to display finance company details. Added     *
      *                 WMPFP.                                        *
      *
      *  08/27/99 C:33 PROJECT: WC910                                 *
      *                CHANGE TO RECOGNIZE WHEN AN ENTIRE CHECK HAS   *
      *                NOT BEEN ACCOUNTED FOR AND RE-DISPLAY CERTAIN  *
      *                VALUES.                                        *
      *                                                               *
      *  08/10/99 C:61 PROJECT WC577                                  *
      *                Reverse image CX date.                         *
      *                                                               *
      *  06/22/99 C:61 PROJECT 48WQJ                                  *
      *                Add previous fund number to header info.       *
      *                                                               *
      *  09/18/98 C:61  PROJECT: 48WLC                                *
      *                 Display Prev Employer from Employer Cross     *
      *                 Reference (WZEXP).                            *
      *                                                               *
      *  08/11/98 C:69  Support: 13116                                *
      *                 Added additional check so that it wouldn't    *
      *                 require a check number.                       *
      *                                                               *
      * 07/22/98  C:35  PROJECT: SUPPORT WA091 12801                  *
      *                 CHANGE CODE TO RETRIEVE CANCELLATION DATE     *
      *                 BASED ON POLICY EFFECTIVE DATE (SINCE THERE   *
      *                 CAN BE MULTIPLE SETS OF EFF/CX DATES.         *
      *                                                               *
      *  03/19/98 C:33 PROJECT: WC753                                 *
      *               -RUN DATES THRU DATCHK IF USING F10 (SEQ# 10757)*
      *               -VALIDATE PRE-EXISTING CONDITIONS B4 SEARCHING  *
      *                                                               *
      *  PLEASE ADD TO TOP                                            *
      *****************************************************************
     FW1210F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      *
     FWMAHL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(EMFDS)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELL     IF   E           K DISK
     FWDELL2    IF   E           K DISK    RENAME(PEL01:PEL0L2)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDS)
     FWDF1L     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWTAVL     IF   E           K DISK
     FWTABL     IF   E           K DISK    IGNORE(PAT01)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcn01)
     f                                     ignore(pcs01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f                                     ignore(pna01)
     f                                     ignore(pnc01)
     f                                     ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(ptr01)
     f                                     ignore(pvs01)
      *  Physical file definition (for maintaining record)
     FWDEAP     UF A E             DISK
     F                                     RENAME(PEA01:DBRCDF)
     F                                     INFDS(DBRCDFDS)
     FWDEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     FWMEAL8    IF   E           K DISK
     F                                     RENAME(PEA01:PEA08)
     F                                     PREFIX(L8)
     FWDEAL2    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F                                     PREFIX(L2)
      *
      *  Logical for Premium Finance Co.
      *
     FWMPFL     IF   E           K DISK    INFDS(DBFDSF)
     f*
     f* A/P Batch File
     fWDEAL8    if   e           k disk    Rename(PEA01:PEAL8) Prefix(EA8:2)
     f*
     f* A/R Batch File
     fwdeal9    if   e           k disk    rename(PEA01:PEA09) prefix(l9)
     f                                     infds(ea9dsf)
     Fwmupl     if   e           k disk
     fwdcdl     if   e           k disk    rename(pcd01 : pcd0l)
     fwtc3l     if   e           k disk    rename(pc301 : pc30l)
      *
     D EMFDS           DS
     D  EMRRN                397    400B 0
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D DBRCDFDS        DS
     D  DBRRN                397    400B 0
     d ea9dsf          ds
     d  ea9rrn               397    400B 0
     D                 DS                  INZ
     D  ECA                    1     35  0
     D                                     DIM(5)                               CX DATES/FILE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS                  INZ
     D  EEF                    1     35  0
     D                                     DIM(5)                               EFF DATES
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
      *
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D CLIENT          DS           256    INZ
      *
     d SVDTE           s                   LIKE(EAIDT)
     d SVIDTE          s                   LIKE(EAIDT)
     D DTflg           S              1n
     D DtChgd          S              1n
     D IDTflg          S              1n
     D Lesflg          S              1n
     D $$appl          s              1
     D $$auth          s              1
     D $$co#           s              3  0
     D $$emp#          s              9  0
     D $$fnd           s              3  0
     D $$rtcd          s              1
     D $$usr           s             10
     D failed          s                   like(*in99)
     D multig          s                   like(*in99)
     d svcash          s                   like(EACASH)
     d kkhier          s                   like(CDHIER)
     d cashDissolveFl  s               n
     d kkstat          s              1    inz(' ')
     d relRcd#         s              9  0 inz(0)
     d xxbal           s                   like(EABATA)
     d xxneg           s                   like(EABATA)
     d getValue        pr                  like(EAAMT)
     d  ArCode                             like(EAARCD) const
     d  ArAmt                              like(EAAMT)  const
     D useInvoice      s              1n
     D cashDisolve     s              1n
     D invDate         s               d
     D saveInvDate     s               d
     D*** begin add ***  10/20/23  *****************************************
     d dtcym           s              5  0 inz(0)
     D*** end add *****  10/20/23  *****************************************
     D xxDtChk         s                   like(scPoDt)
     D xxNInv          s                   like(scNInv)
     D saveScNInvAt    s                   like(scNInvAt)
     D dspNormal       c                   x'20'
     D dspRvsImg       c                   x'21'
     D dspHighInt      c                   x'22'
     D dspHighRvsImg   c                   x'23'
     D dspUnder        c                   x'24'
     D dspUnderRvsImg  c                   x'25'
     D dspUnderHigh    c                   x'26'
     D dspNonDsp       c                   x'27'
     D addMode         s              1n
     D chgMode         s              1n
     D dspMode         s              1n
     D InvView         s              1n
     d currYear        s                   like(ELfyr)
     d rnewYear        s                   like(ELfyr)
     d*** Beg Add *** 03/25/21 ****************************************
     d isInvoiceCloud  s              1n   Inz
     d isICgroup       s              1n   Inz
     d strArKey        s             10
     d inJson          s          32000    Varying
     d intnInv         s                   Like(EAinv#) Inz
     d*** End Add *** 03/25/21 ****************************************
     d*** Begin Add *** 08/27/24 *******************************************
     d chrJson         s          32000    Varying
     d chrMessage      s            256
     d chrPanelTitle   s             50
     d*** End   Add *** 08/27/24 *******************************************
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('A/R Batch Detail')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D Datusa          S               D   Datfmt(*Usa)
      *
      *** beg add ***** 01/11/22 **************************
     d/copy wPrBank
     d*** Begin Add *** 08/27/24 *******************************************
     d/copy sPrGenPop
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     d*** End   Add *** 08/27/24 *******************************************
       Dcl-S chrChkType Char(10);
       Dcl-S chrEftType Char(1) Inz('E');
       Dcl-S chrEftKey  Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEobType Char(3);
       Dcl-S chrCo# Char(3);
       Dcl-S chrFnd Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
       Dcl-S dbRRN1 Packed(9:0);
      *** end add ***** 01/11/22 **************************
     d*** Beg Add *** 03/25/21 ***************************************
     d/Copy wPrPmtSvc
     d/Copy sPrModule
     d*** End Add *** 03/25/21 ***************************************
     d/copy wPrAR
     d/copy srInv#Pr
      *
     d*** Begin Chg *** 08/27/24 *******************************************
     C*** /COPY SCOPYMT42C
     C/COPY SCOPYMT44C
     c*** End   Chg *** 08/27/24 *******************************************
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@BHDR            9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
      *================================================================
      *  KLIST definitions
      *================================================================
     C     AHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR

     C     AHKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    KKTRDT
      *
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR

     C     ELKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    sysDt7
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
      *
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
      *
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
      *
     C     EAKY4         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCBAT#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      *
      *  PREMIUM FINANCE CO. KEY
      *
     C     PFKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFCOD
      *
     C     PFKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFCOD
     C                   KFLD                    XXPFDT
      *
     C     EAKY8         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     c     eaky9         klist
     c                   kfld                    ##co#
     c                   kfld                    kkstat
     c                   kfld                    scbat#
     c                   kfld                    scchk#
     c                   kfld                    scckam
      *
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCBAT#
      *
     C     cdky          klist
     C                   kfld                    ##CO#
     C                   kfld                    SCFND
     C                   kfld                    kkhier
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EABAT#        KKBAT#
     C     *LIKE         DEFINE    EATRDT        KKTRDT
     C     *LIKE         DEFINE    EAACHF        SVACHF
     C     *LIKE         DEFINE    SYKEY         KKKEY
     C     *LIKE         DEFINE    EAFND         SVFND
     C     *LIKE         DEFINE    EASYCD        SVSYCD
     C     *LIKE         DEFINE    EAFYR         SVFYR
     C     *LIKE         DEFINE    EAEMP#        SVEMP#
     C     *LIKE         DEFINE    EADIV         SVDIV
     C     *LIKE         DEFINE    EACHK#        SVCHK#
     C     *LIKE         DEFINE    EACKAM        SVCKAM
     C     *LIKE         DEFINE    EAAMT         SVAMT
     C     *LIKE         DEFINE    *IN60         SAMECK
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    EAPODT        SYSDT7
     C     *LIKE         DEFINE    SCAMT         SVAMT2

      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *================================================================
      * EMPINF - Employer information to be displayed
      *================================================================
     C     EMPINF        BEGSR
      *
     C                   MOVEL(P)  SCSYCD        KKKEY
     C     TBKY          CHAIN     PSY01                              60
      *
     C     SYSTAT        IFNE      'I'
     C     SYTYPE        IFEQ      'Y'
      *
     C                   MOVEL(P)  EMNAME        SCEMNM
     C                   MOVEL     *ZEROS        SCEMCD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVEL(P)  ELASCD        EMASTS
     C*
     C                   MOVE      *BLANKS       AVDESC
     C                   MOVEL(P)  EMASTS        KKKEY
     C     TBKY          CHAIN     WTAVL                              99
     C     EMASTS        CAT(P)    AVDESC:1      SCEMAS
      *
      *  IF Special Activity Field is not blank, Reverse Image it
     C                   If        SCEMAS > *BLANKS
     C                   Eval      *IN62 = *ON
     C                   Else
     C                   Eval      *IN62 = *OFF
     C                   EndIF
      *
      *  Determine cancellation date
      *
     C     *LIKE         DEFINE    EMCAD1        CANCDT
     C                   MOVE      *ZEROS        CANCDT
     C                   Z-ADD     0             ELPFDT
     C                   Z-ADD     0             ELPTDT
     C     ELKY          CHAIN     WDELL                              60
     C                   Z-ADD     5             IX                1 0
      *
     C     EEF(IX)       DOWEQ     *ZERO
     C     IX            ANDGT     1
     C                   SUB       1             IX
     C                   ENDDO                                                  IX NE 0
      *
     C                   Z-ADD     ECA(IX)       CANCDT
      *
     C                   eval      scEmCd = DtMDYY(CancDt)
      *
     C                   MOVEL     *BLANKS       SCVIP
     C     EMVIP         IFEQ      'Y'
     C                   MOVEL     'VIP'         SCVIP
     C                   ENDIF
      *
     C     SCEMCD        IFEQ      *ZERO
     C                   SETON                                        64
     C                   ELSE
     C                   SETOFF                                       64
     C                   ENDIF
      *
     C                   eval      sysDt7 = DateTo7(SystemDate)
      *
      *  CALCULATE BALANCE
      *
     c                   eval      xxbal = 0
      *
      * Get balance from live A/R
      *
     c     eaky8         setll     pea08
     c     eaky8         reade     pea08
     c                   dow       not(%eof)
     c                   if        L8EAPODT <= sysDt7
     c                   eval      xxbal = xxbal +
     c                             getValue(L8EAARCD : L8EAAMT)
     c                   endif
     c     eaky8         reade     pea08
     c                   enddo
      *
      * Get balance from A/R Batch (not transferred)
      *
     c     eaky8         setll     pea02
     c     eaky8         reade     pea02
     c                   dow       not(%eof)
     c                   if        L2EAPODT <= sysDt7 and EASTAT <> 'T'
     c                   eval      xxbal = xxbal + getValue(L2EAARCD : L2EAAMT)
     c                   endif
     c     eaky8         reade     pea02
     c                   enddo
      *
      * IF A PREMIUM FINANCE CO. HAS BEEN ASSIGNED TO THE POLICY, LOAD
      *
     C                   MOVE      *OFF          *IN65
      *
     C                   MOVEL     *BLANKS       SCFCOD
     C                   MOVEL     *BLANKS       SCPFNM
     C                   MOVEL     ELPFIN        SCFCOD
     C                   MOVEL     ELPFDT        XXPFDT
      *
     C     SCFCOD        IFNE      *BLANKS
     C                   MOVE      *ON           *IN65
      *
     C     PFKY2         SETGT     PPF01
      *
     C     PFKY          READPE    PPF01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PFNAME        SCPFNM
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       SCFCOD
     C                   MOVE      *BLANKS       SCPFNM
      *
     C                   ENDIF                                                  *SCFCOD<>BLK
      *
     C                   ENDIF                                                  SYTYPE=Y
      *
     C                   ENDIF                                                  SYSTAT NE I
      *
     C                   CALL      'WA002R'
     C                   PARM      ##CO#         @@CO#             3 0
     C                   PARM      SCFND         @@FND             3 0
     C                   PARM      SCEMP#        @@EMP#            9 0
     C                   PARM      SCDIV         @@DIV             5 0
     C                   PARM      SCFYR         @@FYR             3 0
     C***  Begin Chg  ***  01/10/18  **************************************
     C**********         PARM                    @@AGT#            5 0
     C**********         PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***  End   Chg  ***  01/10/18  **************************************
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM      '*'           @@PLAN            1
     C                   PARM      *ZERO         @@DATE            8 0
      *
     C                   MOVE      *ON           CKPLAN            1
      *
     C                   ENDSR                                                  EMPINF
      *
      *================================================================
      * ERRCK1 - Error checking for screen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C                   MOVEA     '00000000'    *IN(30)
      /Free
       erAmt = *off;
      /End-Free
     C                   callp     RmvMsg
     c                   if        svcash = 'Y' and
     c                             @@mode = 'A'
     c                             or svcash = 'Y'
     c                             and @@mode = 'C'
     C                   eval      sysDt7 = DateTo7(SystemDate)
     c                   if        svachf = 'L' and scnwrn = ' '
     c                   eval      sysDt7 = svtrdt
     c                   endif

      /free
       currYear = 0;
       rnewYear = 0;
          // Get current year
       Setgt elky2 pel0l2;
       Readpe emky pel0l2;
       If not(%eof);
          currYear = ELfyr;
       Endif;
          // Get renewal year
       Setll elky2 pel0l2;
       Reade emky pel0l2;
       If not(%eof);
          rnewYear = ELfyr;
       Endif;
          //Use the renewal year as the current year if current year doesn't exist.
       If currYear = 0;
          currYear = rnewYear;
       Endif;
          //Use the current year as the renewal year if renewal year doesn't exist.
       If rnewYear = 0;
          rnewYEAR = currYear;
       Endif;
          //Set the year.
       If scNwRn = 'Y';
          scFyr = rnewYear;
       Else;
          scFyr = currYear;
       Endif;
      /end-free

     c                   endif
      *
B01  C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCSYCD        KKKEY
     C     TBKY          CHAIN     PSY01                              90
     C  N90SYSTAT        COMP      'I'                                    90
B02  C     *IN90         IFEQ      *ON
     C                   SETON                                        73
E02  C                   ENDIF
E01  C                   ENDIF
      *
      *  The following edits are done if SYTYPE='Y' or
      *
B01  C     SYTYPE        IFEQ      'Y'
      *
B02  C     *IN90         IFEQ      *OFF
     C     FNKY          CHAIN     WMFNL                              90
B03  C     *IN90         IFEQ      *ON
     C                   SETON                                        76
E03  C                   ENDIF
E02  C                   ENDIF
      *
B02  C     *IN90         IFEQ      *OFF
     C     EMKY          CHAIN     WMEML                              90
B03  C     *IN90         IFEQ      *ON
     C                   SETON                                        79
E03  C                   ENDIF
E02  C                   ENDIF

     C*
     C* Check to see if you have authority to this group or employer
     C*
     C                   if        not *in90
     C                   if        multig = *on
    C*
     C                   call      'SA008R'
     C                   parm      ##co#         $$co#
     C                   parm      q1user        $$usr
     C                   parm      'W'           $$appl
     C     failed        parm      *off          $$rtcd
     C                   parm      ' '           $$auth
     C                   parm      scfnd         $$fnd
     C                   parm      scemp#        $$emp#
    C*
     C                   if        failed = *on or
     C                             $$auth = 'I'
     C                   seton                                        8690
     C                   endif
    C*
     C                   else
     C                   if        upxfnd = 'Y'
     C                             and upfnd <> scfnd
     C                             or upxemp = 'Y'
     C                             and upfnd <> scfnd
     C                             or upxemp = 'Y'
     C                             and upemp# <> scemp#
     C                             or upxemp = 'Y'
     C                             and updiv <> scdiv
     C                   seton                                        8690
     C                   endif
     C                   endif
     C                   endif
      *
B02  C     *IN90         IFEQ      *OFF
     C     ELKY          CHAIN     WDELL                              90
B03  C     *IN90         IFEQ      *ON
     C                   SETON                                        80
E03  C                   ENDIF
E02  C                   ENDIF

      *** beg add ***** 01/11/22 **************************
       erEftSetup = *Off;
       msEftSetup = *Blanks;
       If svAchF = 'A' And @@Mode = 'A' And Not(*In90);
          chrEfDt = %Editc(SVtrdt:'X');
          chrEftKey = %Editc(SCemp#:'X') + %Editc(SCdiv:'X');
          chrCo# = %Editc(##co#:'X');
          chrFnd = %Editc(SCfnd:'X');
          chrFyr = %Editc(SCfyr:'X');
          If GetEftStatus(chrCo#:chrEftType:chrFnd:chrEftKey:
           chrEfDt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr) = *Off;
             *In90 = *On;
             erEftSetup = *On;
             msEftSetup = 'EFT setup is not valid for this ' +
              'employer/division.';
          Endif;
       Endif;
      *** end add ***** 01/11/22 **************************
      *
B02  C     *IN90         IFEQ      *OFF
     C     F1KY          CHAIN     WDF1L                              90
B03  C     *IN90         IFEQ      *ON
     C                   SETON                                        77
E03  C                   ENDIF
E02  C                   ENDIF
      *
      *
      * Do not allow same check number and check amount to be dissolved twice to the same policy.
      *
     c                   eval      *in68 = *off
     c  n90              if        svCash = 'Y'

     c     eaky9         setll     pea09
     c     eaky9         reade     pea09
     c                   dow       not(%eof)
     c                   move      ea9rrn        relRcd#
     c                   if        relRcd# <> @@rrn  and
     c                             l9eafnd  = scfnd  and
     c                             l9eaemp# = scemp# and
     c                             l9eadiv  = scdiv  and
     c                             l9eachk# = scchk# and
     c                             l9eackam = scckam
     c                   seton                                        90  68
     c                   leave
     c                   endif
     c     eaky9         reade     pea09
     c                   enddo

     c                   endif
     c  n90              if        svCash = 'Y'
     c     fnky          chain     pf201
     c                   eval      kkhier = 10000
     c     cdky          setll     pcd0l
     c     fnky          readpe    pcd0l
     c                   if        %eof
     c                   eval      cashDissolveFl  = *off
     c                   else
     c                   eval      cashDissolveFl  = *on
     c                   endif
     c     fnky          chain     pc30l
     c                   if           not(%found)
     c                             or C3CNCD = *blank
     c                             or C3CPCD = *blank
     c                             or C3SPCD = *blank
     c                             or C3APCD = *blank
     c                   eval      cashDissolveFl  = *off
     c                   endif
     c                   if        (F2CCYF = *blank and
     c                              F2RCYF = *blank and
     c                              F2ACYF = *blank and
     c                              F2LCYF = *blank)
     c                             or cashDissolveFl  = *off
     c                   seton                                        93  90
     c                   endif
     c                   endif
E01  C                   ENDIF                                                  SYTYPE=Y
      *
      * Position cursor to check # if screen values change. If payment type is
      * a refund, check if amount will cause a positive balance in A/R and
      * display message.
      *
      /free
       // Redisplay for Employer Information when emp info changes and
       //  position cursor at check number field
       If not(*in90) and
        (scEmp# <> svEmp# or
         scDiv  <> svDiv  or
         scFyr  <> svFyr);
          Exsr EmpInf;
          *in34 = *on;
          *in90 = *on;
          svEmp# = scEmp#;
          svDiv  = scDiv;
          svFyr  = scFyr;
       Endif;
       // Inform user that the refund is too large and that it will
       //  result in a net positive balance (excluded from cash dissolve)
       Select;
       When *in90;
       When svCash = 'Y';
       When scAmt <> svAmt2;
          svAmt2 = scAmt;
          kkKey = scArCd;
          Chain tbKy ptt01;
          Select;
          When not(%found);
          When TTstat = 'I';
          When TTtype = 'R';
             Exsr EmpInf;        // recalculate balance
             If @@mode = 'C';
                xxBal += (scAmt - svAmt3);
             Endif;
             If (xxBal + scAmt) > 0;
                *in88 = *on;
                *in90 = *on;
             Endif;
          Endsl;
       Endsl;
      /end-free
      *
B01  C     *IN90         IFEQ      *OFF
B01  C     SCCHK#        ANDLT     *ZEROS
     C                   SETON                                        7190
E01  C                   ENDIF
      *
      *  *HIVAL must not be entered for "Check number"
      *
     C  N90SCCHK#        IFEQ      *ALL'9'
     C                   SETON                                        84  90
     C                   ENDIF
      *
      *  *HIVAL must not be entered for "Check amount"
      *
     C  N90SCCKAM        IFEQ      *ALL'9'
     C                   SETON                                        85  90
     C                   ENDIF
      *
      /free
       // Do not allow post amount of zero (if not cash dissolve or LUBA)
       Select;
       When *in90;
       When svCash = 'Y';
       When client# = '133' and @@Mode = 'A' and scAmt = 0;
          scAmt = scCkAm;
       When scAmt = 0;
          erAmt = *on;
          *in90 = *on;
       Endsl;
       // Do not allow a check amount of zero for cash dissolve
       Select;
       When *in90;
       When svCash <> 'Y';
       When scCkAm = 0;
          *in51 = *on;
          *in90 = *on;
       Other;
          *in51 = *off;
       Endsl;
      /end-free
      *
     c                   if        *in66 = *off

B01  C     *IN90         IFEQ      *OFF
     C                   eval      scPoDt = DtCheck(scPoDt : '*mdy')
     C                   if        scPoDt = *zero
     C                   SETON                                        7290
E02  C                   ENDIF
E01  C                   ENDIF
      *
B01  C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCARCD        KKKEY
     C     TBKY          CHAIN     PTT01                              90
     C  N90TTSTAT        COMP      'I'                                    90
B02  C     *IN90         IFEQ      *ON
     C                   SETON                                        74
X02  C                   ELSE
B03  C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'Q'
     C     SCCHK#        ANDEQ     *ZEROS
     C                   SETON                                        7890
E03  C                   ENDIF
B03  C     SCDESC        IFEQ      *BLANKS
     C                   MOVEL     TTDESC        SCDESC
E03  C                   ENDIF
      *
     C                   if        client# = '129'
      *  Check user security level/type for posting A/R code
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      ##TYPE        #2UTYP
     C                   MOVE      ##LVL         #2ULVL
     C                   MOVE      TTMINS        #2CLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    #2UTYP            1            User Type
     C                   PARM                    #2ULVL            3            User Level
     C                   PARM      TTTY01        #2CTYP            1            Client Type
     C                   PARM                    #2CLVL            3            Client Level
     C                   PARM      '1'           #2EXEC            1            Execution Type
     C     *IN90         PARM      *OFF          #2RES             1            Result Flag
     C                   PARM      TTTY02        #2CTP2            1
     C                   PARM      TTTY03        #2CTP3            1
     C                   PARM      TTTY04        #2CTP4            1
     C                   PARM      TTTY05        #2CTP5            1
     C                   PARM      TTTY06        #2CTP6            1
      *
     C   90              SETON                                        32
     C                   ENDIF
      *
     C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDIF
      *
B01  C     *IN90         IFEQ      *OFF
     C     SCDESC        ANDEQ     *BLANKS
     C                   SETON                                        7590
E01  C                   ENDIF

     c                   endif
      *
      *  IF THE POLICY IS ACCOUNT CURRENT, DISPLAY MESSAGE
      *
     C  N90@@PLAN        IFEQ      'A'
     C     CKPLAN        ANDEQ     *ON
      *
     C     TTTYPE        IFEQ      'P'
     C     TTTYPE        OREQ      'I'
     C     TTTYPE        OREQ      'Q'
     C                   SETON                                        87  90
     C                   MOVE      *OFF          CKPLAN            1
     C                   ENDIF                                                  TTTYPE=P/I/Q
      *
     C                   ENDIF                                                  @@PLAN=A
     C*
     C  n90              if        useInvoice
     C*
     C                   eval      scNInvAt = saveScNInvAt
     C*
     C                   select
     C                   when      scPoDt <> *zero
     C                   eval      invDate = CvtToDate(scPoDt)
     C                   other
     C                   eval      invDate = SystemDate
     C                   endsl
     C*
     C  n90              if        (xxNInv = *zero or
     C                              (scninv <> 0 and TTTYPE = 'P'
     C                              and scArCd <> 'CH'
     C                              and scArCd <> 'AP') or
     C                               invDate <> saveInvDate)
     c                   eval      xxPoDt = DtCYMD(scPoDt)
     C                   if        scninv <> 0 and TTTYPE= 'P'
     C                              and scArCd <> 'CH' and scArCd <> 'AP'
     C                   eval      xxninv=scninv
     C                   endif

     c                   call      'SRINV#'
     c                   parm                    ##co#
     c                   parm                    scFnd
     c                   parm                    scEmp#
     c                   parm                    scDiv
     c                   parm                    scFyr
     c                   parm      xxpodt        #IDT              7 0
     c                   parm                    xxNInv
     c                   parm      *off          xxcrtn            1


     C                   eval      saveInvDate = invDate
     C                   endif
     C*
     C*  Statement # required, if billing type
     C  n90              if        BillType(TTTYPE)
     C                              and scNInv = *zeros
     C                   eval      scNInv = xxNInv
     C                   seton                                        37  90
     C                   if        scNInv = *zeros
     C                   seton                                        40
     C                   endif
     C                   endif
     C*
     C*  Invoice number cannot be less than zero.
     C  n90              if        scNInv < *zero
     C                   seton                                        71  90
     C                   endif
     C*
     C*  Invoice number must match prior invoice # for billing period
     C  n90              if        (xxNInv <> scNInv)
     C                   eval      scNInvOld = %editc(xxNInV:'Z')
     C*  For billing type entries, no need to match prior invoice #
     C  n90              if        not BillType(TTTYPE)
     C  n90              select
     C                   when      (BillType(TTTYPE)
     C                               or TTTYPE = 'P')
     C                   seton                                        38  90
     C                   other
     C                   eval      xxNInv = scNInv
     C                   seton                                        39  90
     C                   endsl
     C                   endif
     C                   eval      scNInvAt = dspUnderRvsImg
     C                   endif
     C*
     C*  Invoice entry is premium payment
     C*   and transaction code is not 'CH' or 'AP'
     C*   and entry is being transfered to live A/R file,
     C*  a premium billing transaction (type 'B' or 'A') must
     C*  exist in the A/R file with a matching invoice#.
     C*
     C  n90              if        TTTYPE = 'P'
     C                              and scArCd <> 'CH'
     C                              and scArCd <> 'AP'
     C                              and SYTYPE = 'Y'
     C* * * *              MOVE *ON       *IN38
     C* * * *              MOVE 'B'       XXARTY
     C* * * *    EAKY      CHAINPEA01                99
B03  C* * * *    *IN99     DOWEQ*OFF
     C* * * *    *IN38     ANDEQ*ON
B04  C* * * *    SCNINV    IFNE EANINV
     C* * * *    EAKY      READEPEA01                    99
X04  C* * * *              ELSE
     C* * * *              MOVE *OFF      *IN38
E04  C* * * *              ENDIF
E03  C* * * *              ENDDO
B03  C* * * *    *IN38     IFEQ *ON
     C* * * *              MOVE 'A'       XXARTY
     C* * * *    EAKY      CHAINPEA01                99
B04  C* * * *    *IN99     DOWEQ*OFF
     C* * * *    *IN38     ANDEQ*ON
B05  C* * * *    SCNINV    IFNE EANINV
     C* * * *    EAKY      READEPEA01                    99
X05  C* * * *              ELSE
     C* * * *              MOVE *OFF      *IN38
E05  C* * * *              ENDIF
E04  C* * * *              ENDDO
E03  C* * * *              ENDIF
B03  C* * * *    *IN38     IFEQ *ON
     C* * * *              SETON                       9039
E03  C* * * *              ENDIF
E02  C* * * *              ENDIF
E01  C* * * *              ENDIF
E01  C                   endif
     C*
E01  C                   endif
      *
B01  C     *IN90         IFEQ      *OFF
     C     SCLATE        ANDNE     ' '
     C     SCLATE        ANDNE     'Y'
     C                   SETON                                        8390
E01  C                   ENDIF
      *
     c  n90              if        scnwrn <> 'Y' and
     c                             scnwrn <> *blank
     c                   seton                                        92  90
     c                   endif
      *
     c  n90SYTYPE        ifeq      'Y'
     c     *in66         andeq     *off                                         not cash dissolve
     c     *like         define    EAPODT        xxdt
     c     *like         define    EAPODT        xxpodt
     C                   eval      xxDt   = DateTo7(SystemDate)
     C                   eval      xxPoDt = DtCYMD(scPoDt)
     C*** begin del ***  10/20/23  ********************************************
     c*****              move      *off          billTrans         1
     c*****TTTYPE        ifne      'P'
     c*****TTTYPE        andne     'I'
     c*****TTTYPE        andne     'Q'
     c*****TTTYPE        andne     'R'
     c*****TTTYPE        andne     'N'
     c*****TTTYPE        andne     'T'
     c*****              move      *on           billTrans
     c*****              endif
     c*****billTrans     ifeq      *on
     c*****SCARCD        andne     F1SRBB
     c*****xxpodt        iflt      xxdt
     c*****              movel     xxpodt        pocym             5 0
     c*****              movel     xxdt          dtcym             5 0
     c*****pocym         ifne      dtcym
     c*****              seton                                        8190
     c*****              endif
     c*****              endif
     c*****              endif
     C*** end del *****  10/20/23  ********************************************
      *
      *  Employer must have assigned Agent effective for the fund
      *  year effective as of the Posting Date
      *
     C     *IN90         IFEQ      *OFF

     c                   if        svtrdt <> 0
     c                   eval      kktrdt = svtrdt
     c                   else
     c                   eval      kktrdt = xxdt
     c                   endif

     c     ahky1         setgt     pah01
     c     ahky          readpe    pah01
     c                   dow       not(%eof) and
     c                             AHSTAT = 'I'
     c     ahky          readpe    pah01
     c                   enddo
     c                   if        %eof

     c                   if        svtrdt <> 0
     c                   seton                                        6990
     c                   else
     c                   seton                                        8290
     c                   endif

     c                   endif
     C                   ENDIF
      *
     C                   ENDIF                                                  SYTYPE='Y'
     C* Edit transaction date for Retro Batches
     C  n90              if        *in59 = *On
     C* should be a valid date
     C                   eval      scRbDt = DtCheck(scRbDt : '*mdy')
     C                   if        scRbDt = *zero
     C                   seton                                        5890
     C                   endif                                                  scRbDt = *zero
     C* should not be entered for prior month
     C     *LIKE         DEFINE    EAPODT        XXRBDT
     C                   eval      xxDt   = DateTo7(SystemDate)
     C                   eval      xxRbDt = DtCYMD(scRbDt)
     C                   movel     xxRbDt        rbcym             5 0
     C                   movel     xxdt          dtcym
     C                   if        rbcym < dtcym
     C                   seton                                        5790
     C                   endif                                                  rbcym < dtcym
     C* only retro payments can be added
     C  n90              if        scarcd <> f1rccd and *in66 =*off
     C                   seton                                        5690
     C                   endif                                                  scarcd <> f1rccd
     C                   endif                                                  *in59 = *on
     C                   if        *in90 = *off
     C     EAKY4         CHAIN     PEA04                              60
     c                   if        eatffl = 'Y' or eatfap = 'Y'
     c                   exfmt     wdw01
     C                   seton                                          90
     c                   endif
     c                   endif
     C  n90              if        emeacd = 'Y' and svcash = 'Y'
     C                   exfmt     wdw02
     C                   seton                                        90
     C                   endif                                                  ERRCK1
     C*** Begin Add *** 08/27/24 **************************************
       // Check to see if a payment reversal happenned to a paid
       // Invoice.  If there was a reversal then launch popup to ask
       // if they want to resend invoice to IVC
       If CheckPaymentReversal(##Co#:scFnd:scEmp#:scDiv:scPodt:scNinv);
          chrMessage = 'You are reversing a paid IVC invoice, do you ' +
                       'want to resend that invoice to IVC?';
          chrPanelTitle   = 'Resend Invoice to IVC';
          If PopTwoBtn(chrMessage:chrPanelTitle) = *On;
             chrJson = '{' +
                 '"type":"billing",' +
                 '"invoice":"'  + %EditC(scNinv:'X') + '",' +
                 '"company":"'  + %EditC(##Co#:'X')       + '",'  +
                 '"group":"'    + %EditC(scFnd:'X')       + '",'  +
                 '"employer":"' + %EditC(scEmp#:'X')      + '",'  +
                 '"division":"' + %EditC(scDiv:'X')       + '",'  +
                 '"active":"true"' +
                 '}';
             ThirdPartyInvoicing(chrJson);
             FinalizeThirdPartyInvoicing(##Co#:scFnd);
          EndIf;
       EndIf;
     C*** End   Add *** 08/27/24 **************************************
      *
     C                   ENDSR                                                  ERRCK1
      *================================================================
      * ERRCK2 - Error checking for screen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *================================================================
      * KIDO   - Processing for F9
      *================================================================
      *
     C     KIDO          BEGSR
      *
     C     *IN90         IFEQ      *OFF
     C     FNKY          CHAIN     WMFNL                              90
     C     *IN90         IFEQ      *ON
     C                   SETON                                        76
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     EMKY          CHAIN     WMEML                              90
     C     *IN90         IFEQ      *ON
     C                   SETON                                        79
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   CALL      'W0612R5'                            99
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      EMRRN         ##RRN             9 0
      *
     C     ##RRN         CHAIN     WDELP                              60
      *
     C     *INKL         IFNE      *ON
     C     *IN60         ANDEQ     *OFF
     C                   Z-ADD     ELFYR         SCFYR
     C                   SETON                                        34  90
     C                   ELSE
     C                   SETON                                        31  90
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  KIDO
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C                   MOVEA     '00000000'    *IN(30)
      *
     C     @@MODE        IFEQ      'A'
      *
     C     @@MODE        IFEQ      'A'
     C     $$CO#         SETLL     PSY01
     C     $$CO#         READE     PSY01                                  60
     C     *IN60         DOUEQ     '1'
     C     SYTYPE        IFEQ      'Y'
     C     SYSTAT        ANDEQ     'A'
     C                   EVAL      SCSYCD    =   SYKEY
     C                   LEAVE
     C                   ENDIF
     C     ##CO#         READE     PSY01                                  60
     C                   ENDDO
     C                   ELSE

     C                   MOVE      SVSYCD        SCSYCD
     C                   ENDIF
     C                   MOVE      SVFND         SCFND
     C                   MOVE      *BLANKS       SCFCOD
     C                   MOVE      *BLANKS       SCPFNM
     C                   MOVE      *OFF          *IN65
      *
     C     ADD2FL        IFEQ      *BLANKS
     C                   MOVE      'Y'           ADD2FL
      *
     C                   ELSE
      *
     C     SAMECK        IFEQ      *ON
     C                   EXSR      RELOAD
     C                   ELSE
     C                   SETON                                        30
     C                   ENDIF                                                  SAMECK=*ON
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVE      EASYCD        SCSYCD
     C                   MOVE      EAFND         SCFND
     C                   ENDIF
      *
     C                   MOVEL     *BLANKS       SCEMNM
     C                   MOVEL     *BLANKS       SCEMAS
     C                   MOVEL     *ZEROS        SCEMCD
     C                   MOVE      EAFYR         SCFYR
     C                   MOVE      EAEMP#        SCEMP#
     C                   MOVE      EADIV         SCDIV
     C                   Z-ADD     SCEMP#        SVEMP#
     C                   Z-ADD     SCDIV         SVDIV
     C                   Z-ADD     SCFYR         SVFYR
     C                   Z-ADD     EAAMT         SVAMT3            9 2
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX01                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
     C                   MOVE      EAARCD        SCARCD
     C                   MOVE      EADESC        SCDESC
     C                   MOVE      EACHK#        SCCHK#
     C                   MOVE      EACKAM        SCCKAM
     C                   MOVE      EAAMT         SCAMT
     C                   MOVE      SCAMT         SVAMT2
     C                   MOVE      EALATE        SCLATE
     C     SCLATE        IFEQ      'N'
     C                   MOVE      *BLANKS       SCLATE
     C                   ENDIF
     C                   eval      scNInv = EAINV#
       //*** Beg Add *** 04/11/22 *************************************
           SCInvTypID = EAInvTypId;
       //*** End Add *** 04/11/22 *************************************
     C*
     C     CLIENT#       IFEQ      '133'
     C     @@MODE        ANDEQ     'A'
     C                   eval      scPoDt = DateTo8(SystemDate)
     C                   ELSE
     C                   eval      scPoDt = DtMDYY(EAPODT)
     C                   ENDIF
     C                   eval      scRbDt = DtMDYY(EATRDT)
     C                   MOVEL     *BLANKS       SCVIP
     C     EAEMP#        IFNE      *ZEROS
     C     EMKY          CHAIN     WMEML                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      EMPINF
     C                   ENDIF
     C                   ENDIF
      *
     C     SCEMCD        IFEQ      *ZERO
     C                   SETON                                        64
     C                   ELSE
     C                   SETOFF                                       64
     C                   ENDIF
      *
     c                   if        EACASH = 'Y' or svcash = 'Y'
     c                   move      *on           *in66
     c                   eval      svcash = 'Y'
     c                   else
     c                   move      *off          *in66
     c                   endif
       //*** Beg Add *** 03/25/21 ************************************
       // If invoice number = 0 then look in WDEAP and WMEAP
       //*** Begin Add *** 12/02/22 ***********************************
       isICgroup = IcGroupOpt(##Co#:EAfnd);
       //*** End   Add *** 12/02/22 ***********************************
       If scNinv = 0 and isInvoiceCloud and isICgroup;
          Exsr SRfindInv#;
          scNinv = intnInv;
       Endif;
       //*** End Add *** 03/25/21 ************************************
      *
     C                   ENDSR                                                  MVDBSC
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      SCFND         EAFND
     C                   MOVE      SCFYR         EAFYR
     C                   MOVE      SCEMP#        EAEMP#
     C                   MOVE      SCDIV         EADIV
     C                   MOVE      SCARCD        EAARCD
     C                   EVAL      KKKEY = EAARCD
     C     TBKY          CHAIN     PTT01                              60
     C                   EVAL      EACTYP = TTCTYP
     c                   eval      EACASH = svcash
     C                   MOVE      SCDESC        EADESC
     C                   MOVE      SCCHK#        EACHK#
     C                   MOVE      SCCKAM        EACKAM
     C                   MOVE      SCAMT         EAAMT
     C                   MOVE      SCSYCD        EASYCD
     C                   MOVE      SCLATE        EALATE
     C                   eval      EAPODT = DtCYMD(scPoDt)
     C                   eval      EAINV# = scNInv
     C                   eval      EATRDT = DtCYMD(scRbDt)
       //*** Beg Add *** 03/25/21 ************************************
       // If invoice number = 0 then look in WDEAP and WMEAP
       //*** Begin Add *** 12/02/22 ***********************************
       isICgroup = IcGroupOpt(##Co#:EAfnd);
       //*** End   Add *** 12/02/22 ***********************************
       If scNinv = 0 and isInvoiceCloud and isICgroup;
          Exsr SRfindInv#;
          EAinv# = intnInv;
       Endif;
       //*** End Add *** 03/25/21 ************************************
     C*
     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   MOVEA     '00000000'    *IN(30)
      *
     C     *INKI         CASEQ     *ON           KIDO
     C                   ENDCS
      *
     C     *INKJ         IFEQ      *ON
      *
     C                   eval      xxDtChk = DtCheck(scPoDt : '*mdy')
     C                   if        xxDtChk <> *zero
     C                   eval      scPoDt = xxDtChk
     C                   endif
     C                   eval      xxDtChk = DtCheck(scRbDt : '*mdy')
     C                   if        xxDtChk <> *zero
     C                   eval      scRbDt = xxDtChk
     C                   endif
      *
     C                   SELECT
      *
      *  Fund
      *
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVE      ##CO#         #1CO#
      *
     C                   CALL      'W0010R'
     C     *INKC         PARM      *OFF          #1KC              1
     C     *INKL         PARM      *OFF          #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM      1             #1VIEW            1 0
      *
     C     #1FND         IFEQ      *BLANKS
     C                   SETON                                        3690
      *
     C                   ELSE
      *
     C                   SETON                                        3090
     C                   MOVE      #1FND         SCFND
     C                   ENDIF                                                  #1FND
      *
      *  Employer
      *
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C     FNKY          CHAIN     WMFNL                              99
     C  N99              Z-ADD     FNRRN         #1RRN2
     C   99              Z-ADD     *ZERO         #1RRN2
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM      *OFF          #1KC              1
     C     *INKL         PARM      *OFF          #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *ZERO         #1RRN             9 0
     C                   PARM                    #1RRN2            9 0
     C                   PARM      *BLANKS       #1QTE             1
     C                   PARM      1             #1VIEW            1 0
     C                   PARM                    #1RRN3            9 0
      *
     C     #1RRN         IFEQ      *ZERO
     C                   SETON                                        9030
      *
     C                   ELSE
      *
     C                   IF        *IN66
     C                   SETON                                        3490
     C                   ELSE
     C                   SETON                                        3190
     C                   ENDIF
      *
     C     #1RRN         CHAIN     WMEMP                              99
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      EMFND         SCFND
     C                   ENDIF                                                  #1RRN
      *
     C     HFLD          WHENEQ    'SCFYR'
     C     EMKY          CHAIN     WMEML                              99
      *
B03  C     *IN99         IFEQ      *ON
     C                   SETON                                        79
E03  C                   ELSE
      *
     C                   CALL      'W0051R'
     C     *INKC         PARM      *OFF          #1KC              1
     C     *INKL         PARM      *OFF          #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *ZERO         #1RRN             9 0
     C                   PARM      EMRRN         #1RRN2            9 0
     C                   PARM      *BLANKS       #1QTE             1
      *
     C     #1RRN         IFEQ      *ZERO
     C                   SETON                                        9031
      *
     C                   ELSE
      *
     C     #1RRN         CHAIN     WDELP                              99
     C                   MOVE      ELFYR         SCFYR
     C                   SETON                                        3490
     C                   EXSR      EMPINF
     C                   ENDIF
      *
E03  C                   ENDIF                                                  *IN99=*ON
      *
     C                   SETON                                        90
      *
     C     HFLD          WHENEQ    'SCAMT'
     C     HFLD          OREQ      'SCPODT'
     C                   MOVE      ##CO#         Q2CO#
     C                   MOVE      SCFND         Q2FND
     C                   MOVE      SCFYR         Q2FYR
     C                   MOVE      SCEMP#        Q2EMP#
     C                   MOVE      SCDIV         Q2DIV
     C                   CALL      'W0612R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           Q2CALL            1
     C                   PARM                    Q2CO#             3
     C                   PARM                    Q2FND             3
     C                   PARM                    Q2EMP#            9
     C                   PARM                    Q2DIV             5
     C                   PARM                    Q2FYR             3
     C                   PARM      *BLANKS       Q2DATE            8
     C                   PARM      *BLANKS       Q2AMT             9
     C                   PARM      *BLANKS       Q2CHK#            7
     C                   PARM      *BLANKS       Q2RTC             1
     C                   parm      *zeros        q2rrn             9 0
     C                   parm      *zeros        q2NInv           10 0
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     Q2AMT         OREQ      *BLANKS
     C     Q2DATE        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN34
     C                   ELSE
     C                   MOVE      Q2AMT         SCAMT
     C                   MOVE      Q2DATE        SCPODT
     C                   eval      scNInv = q2NInv
     C                   SETON                                        35
     C                   ENDIF
     C                   SETON                                        90
      *
     C     HFLD          WHENEQ    'SCARCD'
     C     HFLD          OREQ      'SCDESC'
     C                   CALL      'W2161R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *BLANKS       @@KEY            10
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     @@KEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN35
     C                   ELSE
     C                   MOVEL     @@KEY         SCARCD
     C                   MOVEL(P)  SCARCD        KKKEY
     C     TBKY          CHAIN     PTT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     TTDESC        SCDESC
     C                   ENDIF
     C                   SETON                                        37
     C                   ENDIF
     C                   SETON                                        90
      *
     C     HFLD          WHENEQ    'SCSYCD'
     C                   CALL      'W1202R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *BLANKS       @@KEY            10
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     @@KEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   ELSE
     C                   MOVEL     @@KEY         SCSYCD
     C                   SETON                                        36
     C                   ENDIF
     C                   SETON                                        90
      *
     C                   OTHER
     C                   MOVE      *ON           *IN91
     C                   MOVE      *ON           *IN90
     C                   ENDSL
      *
     C                   ENDIF                                                  INKJ
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX01                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      *  F7=FINANCE INFO
      *
     C     *INKG         IFEQ      *ON
      *
     C                   CALL      'W0045R02'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      DBRRNF        @@RRN             9 0
     C                   PARM      FNRRN         @@RRN2            9 0
      *
     C                   SETON                                        90
     C                   ENDIF                                                  *INKG=ON
      *
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   SETOFF                                       63
      *                                                               *
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   MOVE      *BLANKS       ADD2FL            1
     C                   MOVEA     '00000000'    *IN(30)
     C                   MOVE      *BLANKS       SVSYCD
     C                   MOVE      *ZEROS        SVFND
      *
      * Refresh renewal cash flag in case this program instance is
      * not the original instance to exist in the QILE activation
      * group.
      *
     c                   move      *blanks       scnwrn
      *
     C     @@BHDR        CHAIN(N)  WDEAP                              99
B01  C     *IN99         IFEQ      *OFF
     C     *LIKE         DEFINE    EABATA        SVBATA
     C     *LIKE         DEFINE    EATFAP        SVTFAP
     C     *LIKE         DEFINE    EATFFL        SVTFFL
     C     *LIKE         DEFINE    EASTAT        SVSTAT
     C     *LIKE         DEFINE    EATRDT        SVTRDT
     c                   eval      svcash = EACASH
     c* display transaction date field for retro batches
     c                   if        eaachf = 'R'
     c                   eval      *in59 = *on
     c                   else
     c                   eval      *in59 = *off
     c                   endif
      *
      * EACASH will equal 'D' if the batch header record has been dissolved.
      *
     c                   if        @@mode = 'A' and svcash = 'D'
     c                   eval      svcash = 'Y'
     c                   endif
     c                   if        @@mode <> 'A'
     C     @@rrn         chain(n)  wdeap
     c                   eval      svcash = EACASH
     c                   endif
     c                   if        svCash = 'Y'
     c                   eval      *in66 = *on
     c                   else
     c                   eval      *in66 = *off
     c                   endif
     C                   eval      cashDisolve = *in66
      *
     c                   eval      sctrdt = 0
     C                   eval      *in55 = *off
     C                   If        Eatrdt > *Zero
     C                   eval      scTrDt = DtMDYY(EATRDT)
     C                   Eval      *In55 = *On
     C                   Endif
     C                   MOVE      EATRDT        svtrdt
     C                   MOVE      EAACHF        SVACHF
     C                   MOVE      EABATA        SVBATA
     C                   MOVE      EATFAP        SVTFAP
     C                   MOVE      EATFFL        SVTFFL
     C                   MOVE      EASTAT        SVSTAT
     C                   MOVE      EABAT#        SCBAT#
     C                   MOVE      EABATA        SCBATA
     C                   MOVE      EABAT#        KKBAT#
     C                   MOVE      EAIDT         SVIDT             7 0
     C                   MOVEL     EAIPGM        SVIPGM           10
     C                   MOVEL     EAIUSR        SVIUSR           10
       //*** Beg Add *** 04/11/22 *************************************
           isInvType = IsModEnabled('S/R Online') or
                       IsModEnabled('Third Party Payment') ;
       //*** End Add *** 04/11/22 *************************************
E01  C                   ENDIF
     C     q1user        chain     pup01                              99
    C*
     C                   call      'SA008R'
     C                   parm      ##co#         $$co#
     C                   parm      q1user        $$usr
     C                   parm      'W'           $$appl
     C     multig        parm      *off          $$rtcd
     C*
     C                   eval      dspMode = @@mode = 'D'
     C*
     C                   Eval      InvView = *off
     C                   select
     C                   when      client# = '025'
     C                   eval      useInvoice = *on
     C                   endsl
     C*
     C                   select
     C                   when      cashDisolve
     C                   eval      useInvoice = *off
     C                   endsl
     C*
     C                   clear                   scNInvHd
     C                   eval      scNInvAt   = x'27'
     C                   eval      scNInvHdAt = x'27'
     C*
     C                   select
     C                   when      useInvoice
     C                   eval      scNInvHd   = 'Statement#(invoice#):'
     C                   eval      InvView = useInvoice
     C                   eval      scNInvHdAt = dspNormal
     C                   select
     C                   when      dspMode
     C                   eval      scNInvAt   = dspHighInt
     C                   other
     C                   eval      scNInvAt   = dspUnder
     C                   endsl
     C                   endsl
     C*
     C                   eval      saveScNInvAt = scNInvAt
       //*** Beg Add *** 03/25/21 *************************************
       isInvoiceCloud = isModEnabled('Invoice Cloud');
       //*** Begin Del *** 12/02/22 ***********************************
       //***isICgroup = IcGroupOpt(EAco#:EAfnd);
       //*** End   Del *** 12/02/22 ***********************************
       //*** End Add *** 03/25/21 *************************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
      *
      *================================================================
      * RELOAD - Re-load the infomation from the last check
      *================================================================
      *
     C     RELOAD        BEGSR
      *
     C                   Z-ADD     SVFYR         EAFYR
     C                   Z-ADD     SVEMP#        EAEMP#
     C                   Z-ADD     SVDIV         EADIV
     C                   Z-ADD     SVCHK#        EACHK#
     C                   Z-ADD     SVCKAM        EACKAM
     C                   Z-ADD     SVAMT         EAAMT
      *
     C                   MOVE      *ON           *IN89
     C                   MOVE      *OFF          SAMECK
      *
     C                   ENDSR                                                  RELOAD
       //*** Beg Add *** 03/25/21 *************************************
       //==============================================================
       // SRfindInv# - Look for invoice number in WMEAP and WDEAP
       //==============================================================
       Begsr SRfindInv#;
          intNinv = 0;
          strArKey = EAarCd;
          //*** Begin Add *** 12/02/22 ********************************
          EAco# = ##Co#;
          //*** End   Add *** 12/02/22 ********************************
          Chain (EAco#:strArKey) ptt01;
          //*** Begin Chg *** 12/02/22 ********************************
          //***If %Found(WTABL) and BillType(TTtype);
          If %Found(WTABL);
          //*** End   Chg *** 12/02/22 ********************************

             // Check WMEAP
             Setll (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea08;
             Reade (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt)
              pea08;
             Dow not %Eof(WMEAL8);
                Chain (L8EAco#:L8EAarCd) ptt01;
                //*** Begin Chg *** 12/02/22 **************************
                //*** If BillType(TTtype) and L8EAnInv > 0;
                If L8EAnInv > 0;
                //*** End   Chg *** 12/02/22 **************************
                   intNinv = L8EAnInv;
       //*** Beg Add *** 04/11/22 *************************************
                   SCInvTypID = L8EAInvTypId;
       //*** End Add *** 04/11/22 *************************************
                   Leave;
                Endif;
                Reade (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea08;
             Enddo;

             // Check WDEAP
       //*** Beg Chg *** 02/03/23 *************************************
       //*** If scNinv = 0;
             If intNinv = 0;
       //*** End Chg *** 02/03/23 *************************************
                Setll (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea02;
                Reade (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt)
                 pea02;
                Dow not %Eof(WDEAL2);
                   Chain (L2EAco#:L2EAarCd) ptt01;
                   //*** Begin Chg *** 12/02/22 ***********************
                   //*** If BillType(TTtype) and L2EAinv# > 0;
                   If L2EAinv# > 0;
                   //*** End   Chg *** 12/02/22 ***********************
                      intNinv = L2EAinv#;
       //*** Beg Add *** 04/11/22 *************************************
                      SCInvTypID = L2EAInvTypId;
       //*** End Add *** 04/11/22 *************************************
                      Leave;
                   Endif;
                   Reade (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea02;
                Enddo;
             Endif;
          Endif;
       Endsr;
       //*** End Add *** 03/25/21 *************************************
      *
      *================================================================
      * TTLCHK - Determine if the total check has been accounted for
      *================================================================
      *
     C     TTLCHK        BEGSR
      *
     C                   MOVE      *OFF          SAMECK
      *
     C     EAKY4         CHAIN     PEA04                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EACHK#        IFEQ      SVCHK#
     C                   SUB       EAAMT         SVAMT
     C                   ENDIF                                                  EACHK#=SVCHK#
      *
     C     EAKY4         READE     PEA04                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C     SVAMT         IFGT      *ZERO
     C                   MOVE      *ON           SAMECK
     C                   ENDIF                                                  SVAMT>*ZERO
      *
     C                   ENDSR                                                  TTLCHK
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      Q1USER        EAUSER
     C                   eval      EADT   = DateTo7(SystemDate)
     C                   eval      EATIME = TimeTo6(SystemTime)
      *
      * If Inital Date zeros, load the initial fields
     C                   IF        EAIDT = *ZEROS
     C                   eval      EAIDT = EADT
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
     C                   ENDIF
     C                   UPDATE    DBRCDF
     c                   if        EACASH = 'Y'
     c                   exsr      srDissolve
     c                   endif
E01  C                   ENDIF
     C                   Eval      *IN62 = *OFF
     C                   move      *zeros        xxninv
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      ##CO#         EACO#
     C                   MOVE      KKBAT#        EABAT#
     C  N59              MOVE      svtrdt        EATRDT
     C                   MOVE      SVACHF        EAACHF
     C                   MOVE      SVBATA        EABATA
     C                   MOVE      SVTFAP        EATFAP
     C                   MOVE      SVTFFL        EATFFL
     C                   MOVE      SVSTAT        EASTAT
     C                   EXSR      ZUPFLD
     C                   WRITE     DBRCDF
     c                   if        EACASH = 'Y'
     c                   exsr      srDissolve
     c                   endif
     C                   FEOD      WDEAP
      *
     C                   MOVE      EASYCD        SVSYCD
     C                   MOVE      EAFND         SVFND
     C                   Z-ADD     EAEMP#        SVEMP#
     C                   Z-ADD     EADIV         SVDIV
     C                   Z-ADD     EAFYR         SVFYR
     C                   Z-ADD     EACHK#        SVCHK#
     C                   Z-ADD     EACKAM        SVCKAM
     C                   Z-ADD     EACKAM        SVAMT
     c                   if        svCash <> 'Y'
     C                   exsr      ttlChk
     c                   endif
     C                   Eval      *IN62 = *OFF
     C                   move      *zeros        xxninv
      *
     C                   ENDSR
      *================================================================
      * ZUPFLD - Update program, user, date/time fields
      *================================================================
     C     ZUPFLD        BEGSR
      *
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      Q1USER        EAUSER
     C                   eval      EADT   = DateTo7(SystemDate)
     C                   eval      EATIME = TimeTo6(SystemTime)
     C                   IF        @@MODE = 'A' OR EAIDT = *ZEROS
     C                   EXSR      GETDTS
     C                   IF        SVIDTE > *ZEROS
     C                   EVAL      EAIDT = SVIDTE
     C                   ELSE
     C                   eval      EAIDT = EADT
     C                   ENDIF
      *
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
     C                   ENDIF
      *
     C                   ENDSR
      *
      * =========================================================== *
      *    GETDTs - check for different EAIDTs or Zero Dates
      * =========================================================== *
     c*
     c     getdts        begsr
      *
      * reset the save fields, then read WDEAL8 logical to get lowest dates.
      *
     c                   eval      svidte = *zeros
     c                   eval      svdte  = *zeros
     c                   eval      idtflg = *off
     c                   eval      dtflg  = *off
     c                   eval      lesflg = *off
      *
     c                   If        eadt <> *zeros AND svdte = *Zeros
     c                   eval      svdte  = eadt
      *
     c                   If        eaidt  = *zeros
     c                   eval      svidte  = eadt
     c                   eval      dtflg  = *ON
     c                   endif
      *
     c                   endif
      *
     c                   If        eaidt <> *zeros AND svidte = *zeros
     c                   eval      svidte = eaidt
     c                   endif
      *
     c                   If        svidte = *zeros AND svdte = *zeros
      *
     C                   eval      svIDte = DateTo7(SystemDate)
     C                   eval      svDte  = svIDte
     c                   eval      dtflg  = *ON
      *
     c                   endif
      *
     c     eaky          setll     peal8
     c     eaky          reade     peal8                                  61
      *
      * If no hit on file, then zero out save fields, there are no other
      * A/R batch records for this batch
     c                   if        *IN61
     c                   eval      svidte = *zeros
     c                   eval      svdte  = *zeros
     c                   endif
      *
     c                   dow       not *IN61
      *
     c                   select
     c                   When      ea8idt <> *zeros
     c                   if        ea8idt < svidte
     c                   eval      svidte = ea8idt
     c                   eval      idtflg = *on
     c                   eval      lesflg = *on
     c                   endif
      *
     c                   When      ea8idt = *zeros AND ea8dt <> *zeros
     c                   if        ea8dt  < svidte
     c                   eval      svdte  = ea8dt
     c                   eval      dtflg  = *on
     c                   eval      lesflg = *on
     c                   endif
      *
     c                   endsl
      *
     c     eaky          reade     peal8                                  61
     c                   enddo
      *
      * After pass loop, get the lower date if either Flag is on
      *    Else zero out the svdt field
     c                   if        idtflg OR dtflg
     c                   if        svdte < svidte
     c                   eval      svidte = svdte
     c                   endif
      *
     c                   else
      *
     c                   eval      svidte = *zeros
     c                   endif
      *
     c                   endsr
      *
      * =========================================================== *
c    C     *INZSR        BEGSR
     C*
     C                   call      'RTVCLTID'
     C                   parm                    client#           3

     C                   ENDSR
      * =========================================================== *
      * srDissolve - Dissolves eligible payments across A/R balances
      * =========================================================== *
     c     srDissolve    begsr

     c     fnky          chain     pf201

     c                   if        (F2CCYF <> *blank or
     c                              F2RCYF <> *blank or
     c                              F2ACYF <> *blank or
     c                              F2LCYF <> *blank) and
     c                             EACASH = 'Y'

     c                   if        scnwrn = 'Y'
     c                   eval      ##call = 'D'
     c                   else
     c                   eval      ##call = 'C'
     c                   endif

     c                   eval      earrn = dbrrn
     c                   call      'WA055C'
     c                   parm                    ##call            1
     c                   parm                    earrn             9 0

     c                   call      'W1210R03'
     c     *inkc         parm      *blank        ##kc
     c     *inkl         parm      *blank        ##kl
     c                   parm      ' '           ##call
     c                   parm      ' '           ##mode            1
     c                   parm                    earrn

     c                   endif

     c                   endsr
      *=================================================================
      * getValue - returns positive or negative amount based on A/R Type
      *=================================================================
     p getValue        b
     d getValue        pi                  like(EAAMT)
     d  arCode                             like(EAARCD) const
     d  arAmt                              like(EAAMT)  const

     d  arType         s                   like(TTTYPE)
     d  arCtgy         s                   like(TTCTYP)

     c                   eval      kkkey = arCode
     c     tbky          chain     ptt01
     c                   if        %found
     c                   eval      arType = TTTYPE
     c                   eval      arCtgy = TTCTYP
     c                   endif

     c                   select
     c                   when      arCtgy = 'P'

     c                   select
     c                   when      arType = 'P' or arType = 'T'
     c                   return    arAmt * -1
     c                   when      arType = 'A' or arType = 'B' or
     c                             arType = 'D' or arType = 'N' or
     c                             arType = 'R' or arType = 'V'
     c                   return    arAmt
     c                   other
     c                   return    *zero
     c                   endsl

     c                   other
     c                   return    *zero
     c                   endsl

     p getValue        e
     p*** Begin Add *** 08/27/24 *******************************************
      *=====================================================================
      * CheckPaymentReversal - Check to see if an invoice payment was
      *                        reversed.
      *=====================================================================
     p CheckPaymentReversal...
     p                 b
     d CheckPaymentReversal...
     d                 pi              n
     d  inCompany#                         like(EAco#)   const
     d  inGroup#                           like(EAfnd)   const
     d  inEmployer#                        like(EAemp#)  const
     d  inDivision                         like(EAdiv)   const
      *** Beg Chg *** 03/18/25 *************************************
     d*****  inPostDate                         like(EAPoDt)  const
     d  inPostDate                    8  0 const
      *** End Chg *** 03/18/25 *************************************
     d  inInvoice#                         like(EAinv#)  const

     d  reversedPayment...
     d                 s               n   Inz(*Off)
     d  arType         s              1    Inz

        // Initialize Return Variable
        reversedPayment = *Off;

        // Is this an IVC Group
        isICgroup = IcGroupOpt(##Co#:scFnd);

        // Invoice Cloud must enabled and the group must be an IVC group
        If isInvoiceCloud And isICgroup;

           // Get the A/R Type
           arType = GetARType(scArCD);

           // Check for a reversal
           If ((arType = 'P' or arType = 'I' or arType = 'Q' or
                arType = 'T') And scAmt < 0) Or
              ((arType = 'N' or arType = 'R') And scAmt > 0);

              // Now check to see if there was a paid invoice
              If MatchPaidInvoice(inCompany#:inGroup#:inEmployer#:
                    inDivision:inInvoice#);
                 reversedPayment = *On;
              EndIf;
           EndIf;

        EndIf;

        Return reversedPayment;

     p CheckPaymentReversal...
     p                 e

      *=====================================================================
      * MatchPaidInvoice - Look for a paid invoice record that matches
      *                    the reversal.
      *=====================================================================
     p MatchPaidInvoice...
     p                 b
     d MatchPaidInvoice...
     d                 pi              n
     d  inCompany#                         like(EAco#)   const
     d  inGroup#                           like(EAfnd)   const
     d  inEmployer#                        like(EAemp#)  const
     d  inDivision                         like(EAdiv)   const
     d  inInvoice#                         like(EAinv#)  const

     d  paymentMatched...
     d                 s               n   Inz(*Off)
     d  decCount       s              5  0 Inz(0)

        // Initialize Return Variable
        paymentMatched = *Off;

        // Look to see if a hold record exists.
        Exec Sql Select count(*)
                 Into :decCount
                 From wdeap_hld
                 Where EAco#=:inCompany# And EAfnd=:inGroup# And
                       EAemp#=:inEmployer# And EAdiv=:inDivision And
                       EAinv#=:inInvoice#;

        // If hold record doesn't exist then it may have been paid
        If decCount = 0;
           // Let's look for the payment for that invoice
           // In the A/R batch file
           Exec Sql Select count(*)
                    Into :decCount
                    From wdeap join wtttp on EAco#=TTco# And
                                             EAarCd=TTkey
                    Where EAco#=:inCompany# And EAfnd=:inGroup# And
                          EAemp#=:inEmployer# And EAdiv=:inDivision And
                          EAinv#=:inInvoice# And
                          TTtype in ('P', 'I', 'Q', 'T') And
                          EAamt>0;

           // Payment Found for the Invoice if the count <> 0
           If decCount <> 0;
              paymentMatched = *On;
           EndIf;
        EndIf;

        Return paymentMatched;

     p MatchPaidInvoice...
     p                 e
     p*** End   Add *** 08/27/24 *******************************************
