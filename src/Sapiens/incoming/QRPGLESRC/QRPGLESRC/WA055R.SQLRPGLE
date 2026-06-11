     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
      *============================================================================================*
      /title WA055R - API to Create records for Auto Cash Dissolve                                 *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H bnddir('WTBNDDIR')
      *============================================================================================*
      *
      *  Parameters:  @@pgm = 'A' called from WA059R to dissolve audit credit
      *                  -writes to WMEAP
      *               @@pgm = 'B' called from WIPOLPMT to dissolve billing payments
      *               from Invoice Cloud.
      *                  -writes to WDEAP or WMEAP conditioned on EXICPAYAR
      *               @@pgm = 'C' called from W1210R02 to dissolve cash payment
      *                  -writes to WZEAP -> WDEAP
      *               @@pgm = 'D' called from W1210R02 to dissolve new business payment
      *                  -writes to WZEAP -> WDEAP
      *               @@pgm = 'E' called from WA059R to dissolve credit resulting from est bill
      *                  -writes to WMEAP
      *               @@pgm = 'I' called from WIPOLPMT to write (not dissolve) payments
      *               from retro and deductible invoices from Invoice Cloud.
      *                  -writes to WDEAP or WMEAP conditioned on EXICPAYAR
      *               @@pgm = 'L' called from W1302R to dissolve lockbox payment (client specific)
      *                   called from WTARUPLD to dissolve lockbox payment
      *                  -writes to WDEAP
      *               @@pgm = 'M' called from WIPOLPMT to dissolve Make Payment from Invoice Cloud
      *                  -writes to WDEAP or WMEAP conditioned on EXICPAYAR
      *               @@pgm = 'N' called from W8010R to dissolve new business payment
      *                  -writes to WMEAP
      *               @@pgm = 'S' called from SRBAT to invoke S/R Online mode
      *                  -writes to WDEAP
      *               @@pgm = 'U' called from WTRPTUPLD to invoke A/R Batch Upload mode
      *                  -writes to WDEAP
      *               @@pgm = 'T' called from SRBAT to dissolve Pay As You Payments
      *                  -writes to WDEAP or WMEAP conditioned on EXICP2GAR
      *
      *  The program will disperse new business, cash payments, or audit payments depending on the
      *  Cash Dissolve setup at the Group Level.  WDCDP and WTC3P are the setup files.
      *
      *  CALLED FROM: W1210R02, W8010R, W1302R, W7720R, SRBAT, WIPOLPMT
      *
      *  CREATED 10/07/05   C:136   PROJECT: WC1179, WC1191
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 08/26/25 C:62  Ticket: 119572(153)                                                         *
      *                Make sure the invoice type id and reference#.                               *
      *                                                                                            *
      * 04/16/25 C:62  Ticket: 118896(129)                                                         *
      *                Make sure the transfer matches the original post date                       *
      *                                                                                            *
      * 02/19/25 C:079 Project: 146WKR
      *                Getting a compile error on DEAsyCd now so I used
      *                a temporary string field in the SQL.
      *                                                                                            *
      * 06/24/24 C:079 Project: 146WKR
      *                Collateral Processing - Recomile
      *                                                                                            *
      * 09/29/23 C:62  Project: WC2663/Phw#113704 (140)                                            *
      *                -Send EFT thru cash dissolve.  Look at AR batch file                        *
      *                 instead the hold file.
      *                -Control how to process mode T.                                             *
      *
      * 08/24/23 C:65  Project: WC2663/Phw#113679 (Internal)                                       *
      *                When non-premium payments are written to A/R batch or live A/R check if the *
      *                category of the payment has a corresponding billing record for the invoice  *
      *                #. If none is found, such as when prior balances are being paid off, then   *
      *                find the invoice # of the corresponding billing record that is being paid   *
      *                off based on the posting date being used.                                   *
      *                                                                                            *
      * 08/23/23 C:65  Project: WC2663/Phw#113679 (Internal)                                       *
      *                Correct the 12/01/22 changes to execute after posting date is assigned and  *
      *                to limit the SQL results to one record in GetInvoice procedure.             *
      *                                                                                            *
      * 12/01/22 C:062 Project: WC2663                                                             *
      *                Assign Invoice #.                                                           *
      *                                                                                            *
      * 11/29/22 C:062 Project: WC2663                                                             *
      *                Assign posting date.                                                        *
      *                                                                                            *
      * 06/29/22 C:340 Support: WC2854                                                             *
      *                Invoice Cloud:  Retain Payment Ref#                                         *
      *                                                                                            *
      * 07/06/22 C:136 Support: 102604 (WC2663 Priss)                                              *
      *                Include cash in A/R to calculate balance when                               *
      *                processing IVC payments.                                                    *
      *                                                                                            *
      * 06/08/22 C:340 Support: WC2663                                                             *
      *                Write InvoiceTypeID to A/R Batch.                                           *
      *                                                                                            *
      * 04/05/22 C:136 Support: 101556                                                             *
      *                Populate EAONLI in WDEAP for S/R Online.                                    *
      *                                                                                            *
      * 10/15/21 C:291 Support: 98792                                                              *
      *                Save Cash Dissolve and check number in GetDisbInfo                          *
      *                Fixed SQL statement in UpdateBatchTotal to not update check amount & check# *
      *                                                                                            *
      * 03/16/21 C:062 Project: WC2663                                                             *
      *                Add mode 'B' for dissolving billing payments from Invoice Cloud.            *
      *                Add mode 'I' for writing retro and deductible invoice payment               *
      *                from Invoice Cloud.  Skip include deposit in S/R online if opt'd in to IC.  *
      *                                                                                            *
      * 11/14/19 C:136 Project: SY407                                                              *
      *                Allow for different ACH flag values for S/R Online and Portal               *
      *                                                                                            *
      * 07/24/17 C:136 Support: 48355                                                              *
      *                Re-add line of code to save system code.  Batch won't total without it.     *
      *                                                                                            *
      * 06/09/17 C:136 Project: 129WUR                                                             *
      *                Add Upload Mode                                                             *
      *                                                                                            *
      * 09/25/15 C:136 Support: 35480                                                              *
      *                Adjust balance calculation logic for S/R Online. Consider all batches not   *
      *                yet transferred.                                                            *
      *                                                                                            *
      * 08/07/13 C:136 Redmine 3542                                                                *
      *                Do not let S/R Online dissolve to deposit category                          *
      *                unless flagged to do so.                                                    *
      *                                                                                            *
      * 07/12/13 C:136 Support: WA146 82649                                                        *
      *                Populate EARDIV correctly when in S/R online mode.                          *
      *                                                                                            *
      * 07/02/13 C:136 Project: 147WAI                                                             *
      *                Exclude Dividend Plan A/R codes from Cash Dissolve processing               *
      *                                                                                            *
      * 06/29/12 C:191 Project: WC2048 Priss                                                       *
      *                Fix bug - WDEAL7 fields are renamed with prefix D7 in F specs whereas       *
      *                prefix D is being used everywhere in program                               *
      *                                                                                            *
     H* 11/07/11 C:190 Project: WC2022.
     H*                Recompiled.
     H*
      * 06/16/11 C:191 Project: WC2048                                                             *
      *                Auto Cash/Dissolve/Pmts AR Batch, added new mode = 'S'                      *
      * 08/17/11       Add processing for Inculde prior year balance for S/R online
      *                                                                                            *
      * 04/27/11 C:136 Support: WA114 76716                                                        *
      *                Ensure excess payments are posted with system date.                         *
      *                                                                                            *
      * 02/28/11 C:220 Project: WC1932                                                             *
      *               -If Exclude from Auto Cash Dissolve flag is 'Y'                              *
      *                skip the employer from processing.                                          *
      *               -Seton *INLR before Return as per reveiwer's comment.                        *
      *                                                                                            *
      * 09/24/10 C:136 Support: WA114 74076                                                        *
      *               -Ensure the new business offset is written with system date instead of the   *
      *                first installment post date.                                                *
      *               -Ensure that new business payments are dissolved by Cash Billing type        *
      *               -Correct Lockbox defaults                                                    *
      *                                                                                            *
      * 08/09/10 C:191 Project: 71WKU                                                              *
      *                Modified to treat Retro payments by date so that premium installment amt    *
      *                are reduced by the corresponding Retro Credit                              *
      *                                                                                            *
      * 10/27/09 C:136 Project: WC1854                                                             *
      *                Add processing for new control grid set up flags in Auto Cash Dissolve      *
      *                set up.                                                                     *
      *                                                                                            *
      * 05/03/09 C:136 Project: WC1663                                                             *
      *               -Add functionality so that any credits generated in a policy year by an      *
      *                estimated or original bill is dissolved against any debit balances that     *
      *                might exist in the previous, current, or renewal year IAW with the cash     *
      *                dissolve hierarchy.                                                         *
      *               -Added code for WC1769 (6th and 7th assessments) that was missed.            *
      *               -Removed old subprocedures that aren't used anymore to clean up (Dissolve    *
      *                and PayInstall)                                                             *
      *                                                                                            *
      * 04/02/09 C:136 Support: WA099 66136                                                        *
      *                Ensure new business transfer posts with system date per GW.                 *
      *                                                                                            *
      * 07/18/08 C:136 Support: WA045 64172                                                        *
      *                Wash transfer payments against negative debit balances so that the API      *
      *                can dissolve against a small debit balance posted after a large negative    *
      *                adjustment that has a corresponding payment transfer.                       *
      *                                                                                            *
      * 05/30/08 C:136 Support: WA045 63513                                                        *
      *                Fix the 'LoadxxPastPay' procedures so that premium payments are totaled     *
      *                correctly if 'N' or 'R' type transactions are involved.                     *
      *                                                                                            *
      * 05/29/08 C:136 Support: WA045 63346                                                        *
      *                Fix LOOKUP operation in getARCodes so that the ARCode array will get built  *
      *                correctly when called from audit                                            *
      *                                                                                            *
      * 04/03/08 C:136 Support: WA045 62882                                                        *
      *                Use the Cash Payment codes instead of the New Business Transfer codes       *
      *                when called from A/R Batches in 'New Business' mode (@@pgm = 'D')           *
      *                                                                                            *
      * 01/14/08 C:136 Support: WA045 62339                                                        *
      *               -Prevent negative cash from becoming a invoice debit balance.                *
      *               -Ensure that new business payments get the new business A/R codes from       *
      *                the cash dissolve hierarchy.                                                *
      *               -Post new business transfer                                                  *
      *                                                                                            *
      * 01/11/08 C:136 Support: WA045 60330                                                        *
      *                Changed structure so that prior year and current balances would be paid     *
      *                off at the same time by date.                                               *
      *                                                                                            *
      * 01/10/08 C:136 Project: WC1536                                                             *
      *                Add logic to restrict dissolve to prior year if policy has a contested      *
      *                audit and the cash dissolve set up is flagged to restrict the dissolve      *
      *                for this condition.                                                         *
      *                                                                                            *
      * 10/15/07 C:136 Support: WA045 60330                                                        *
      *                Change logic so that all categories are paid off by date.                   *
      *                                                                                            *
      * 09/27/07 C:136 Project: 132WFZ                                                             *
      *          C:61  Change to allow lockbox Auto Cash Dissolve for                              *
      *                clt132.                                                                     *
      *                                                                                            *
      * 07/18/07 C:136 Project: WC1491 (Ind Cash Distribution)                                     *
      *                Add processing for deposit refund and void/stop pay transactions.           *
      *                                                                                            *
      * 06/28/07 C:136 Support: WA129 58589                                                        *
      *               -Account for negative balanaces by pulling their dates out of the            *
      *                installment date arrays and using a subprocedure to pull their balance      *
      *                into the prior installment to dissolve the payment against.                 *
      *               -Combine transactions in A/R if possible.                                    *
      *               -Don't pay more than the balance for installments.                           *
      *               -Change lockbox processing so that the lockBoxFlg controls lockbox           *
      *                without having to reference the clientId in preparation for Compwest's      *
      *                lockbox project                                                             *
      *                                                                                            *
      * 04/17/07 C:136 Support: WA091 58025                                                        *
      *                The PayInstall subprocedure goes out of bounds if there is excess balance   *
      *                to pay and no installment dates to pull in installment balances with.       *
      *                Installed code that prevents the abend by jumping out of                    *
      *                PayInstall if no installment dates are left and a balance exists. Any       *
      *                excess payment would be posted to premium via PostXPay.                     *
      *                                                                                            *
      * 04/12/07 C:136 Support: WA091 57962                                                        *
      *                Correct description, category, and type if posting excess payment in 'C'ash *
      *                mode.                                                                       *
      *                                                                                            *
      * 04/10/07 C:136 Support: WA091 57930                                                        *
      *               -Beef up PAYINSTALL logic so that it doesn't encounter an out of bounds     *
      *                error if a credit exists in one category and a debit in another.            *
      *               -Date counter for paying future installments in PAYINSTALL was incorrectly   *
      *                positioned. It was changed so that it will always increment to the next     *
      *                installment date.                                                           *
      *                                                                                            *
      * 03/28/07 C:136 Support: WA129 51589                                                        *
      *                Transactions for New Business transfer are being re-shuffled as a result    *
      *                of fixing EATIME. Corrected by getting latest EATIME for the post date      *
      *                being written to and incrementing from that.                                *
      *                                                                                            *
      * 02/16/07 C:136 Support: WA091 56836                                                        *
      *                Post excess payment to the correct A/R code from WTC3P.  Use the audit      *
      *                payment code for audited policies; the s/r code for s/r policies; and the   *
      *                estimating code for estimated policies.                                     *
      *                                                                                            *
      * 06/29/06 C:136 Support: WA129 53210                                                        *
      *                If the cash posting = 'C', and when dispersing new business payments, use   *
      *                the system date to write the posting date.                                  *
      *                                                                                            *
      * 01/20/06 C:136 Support: WA129 50989                                                        *
      *                - Add code to recognize installment fee transfer and flat cx transfer codes.*
      *                - Add code to emulate the new business payment dispersal in W8010R if       *
      *                  no Cash Dissolve is set up.                                               *
      *                                                                                            *
      * 01/18/06 C:136 Support:                                                                    *
      *                Call to Dissolve ends in error. Clear up problem with dates in PayInstall.  *
      *                                                                                            *
      * 01/12/06 C:136 Support: Project WC1191                                                     *
      *                GW requested spec change to use the audit transfer codes to pay balances    *
      *                when called from the audit process (WA059R).                                *
      *                                                                                            *
      * 01/11/06 C:136 Project:129WMM                                                              *
      *                Client specific: Dissolve Lockbox batches to A/R Batch                      *
      *                                                                                            *
      * 01/09/06 C:136 Support: WA129 50527                                                        *
      *                Correct writing of posting date when paying future installments.            *
      *                                                                                            *
      * 01/07/06 C:136  Project: 129WMM                                                            *
      *                 Client specific: If A/R Batch record is a Lockbox type (EAACHF = 'L'),     *
      *                 then dissolve to the year specified in the record itself. Do not dissolve  *
      *                 to prior year.                                                             *
      *                                                                                            *
      * 01/06/06 C:136  Support: WA136 50726                                                       *
      *                 Account for possiblity that account may not have premium billings to get   *
      *                 first installment date.                                                    *
      *                                                                                            *
      * 10/27/05  C:136 Project: WC1191 (Transfer Audit Credits to Premium)                        *
      *                 If called from Audit Billing process, dissolve excess premium to whatever  *
      *                 debits might exist on the account in accordance with the cash dissolve     *
      *                 hierarchy.                                                                 *
      *=============================================================================================
      * Cash Disbursement File  co/fnd/hier
     fwdcdl     if   e           k disk
      * A/R Premium Category Info
     fwtc3l     if   e           k disk
      * Group First Auxiliary File
     fwdf1l     if   e           k disk
      * Group Second Auxiliary File
     fwdf2l     if   e           k disk
      * Employer Detail File
     fwdelp     if   e             disk    rename(pel01 : pel0p)
     fwdell     if   e           k disk    rename(pel01 : pel0l)
      * A/R Batch Detail File
     fwdeap     uf a e             disk    rename(pea01 : dea0p) prefix(d)
     fwdeal     uf   e           k disk    rename(pea01 : dea0l) prefix(d0)
     fwdeal30   if   e           k disk    rename(pea01 : dea30) prefix(d30)
      * A/R Billing File
     fwmeal6    uf a e           k disk    rename(pea01 : mea0l6) prefix(m6)
      * Table File
     fwtabl     if   e           k disk
      * Rate Set Code File
     fwmrsl     if   e           k disk    rename(prs01 : prs0l)
      * Installment Fee File
     fwmifl     if   e           k disk    rename(pif01 : pif0l)
     fwzeal     uf a e           k disk    rename(pea01 : zea0l) prefix(z)
     fwmeml     if   e           k disk
     fwmexl     if   e           k disk
    d*=============================================================================================
    d* Stand Alone Fields - TOP
    d*=============================================================================================
      *** Begin Add *** 12/01/22 **********************************
     d   decCompany    s                   like(DEAco#)
     d   decGroup      s                   like(DEAfnd)
     d   decEmp#       s                   like(DEAemp#)
     d   decDiv        s                   like(DEAdiv)
     d   decPoDt       s                   like(DEApoDt)
     d   decInvoice#   s                   like(DEAinv#)
      *** End   Add *** 12/01/22 **********************************
     d   kkco#         s                   like(DEACO#)
     d   kkfnd         s                   like(DEAFND)
     d   kkemp#        s                   like(DEAEMP#)
     d   kkdiv         s                   like(DEADIV)
     d   svRDiv        s                   like(DEARDIV)
     d   kkfyr         s                   like(DEAFYR)
     d   k1fyr         s                   like(DEAFYR)
     d   kkPodt        s                   like(DEAPODT)
     d   kkbscd        s                   like(BSKEY)
     d   payment       s                   like(DEAAMT)
     d   priorYrFlg    s               n   inz(*off)
     d   currYr        s               n   inz(*off)
     d   sysTime       s                   like(DEATIME)
     d   svTime        s                   like(DEATIME)
     d   sysDate       s                   like(DEADT)
     d   newBusFlg     s                   like(DEASTAT) inz('N')
     d   auditFlg      s                   like(DEASTAT) inz('N')
     d   kkbat#        s                   like(DEABAT#)
     d   kkstat        s                   like(DEASTAT) inz(' ')
     d   lockBoxFlg    s                   like(DEASTAT) inz('N')
     d   svBatchPoDt   s                   like(DEAPODT)
     d   svBatchTrDt   s                   like(DEATRDT)
     d   svBatchCkam   s                   like(DEACKAM)
     d   svBatchSycd   s                   like(DEASYCD)
     d*** Begin Add *** 04/16/25 **********************************
     d   svBatchSycd2  s                   like(DEASYCD)
     d   svInvoice#    s                   like(DEAINV#)
     d*** End   Add *** 04/16/25 **********************************
     d*** Beg Add *** 02/19/25 *********************************************
     d   strSyCd       s                   like(DEASYCD)
     d*** End Add *** 02/19/25 *********************************************
     d   hierType      s              1    inz('C')
     d   dslvToNwYr    s                   like(F2RNEW)
     d   srOnliFlg     s              1    inz
     d   svBillBat#    s                   like(DEABAT#)
     d   svAchF        s                   like(DEAACHF)
     d   svInv#        s                   like(DEAINV#)
      *** Beg Add ***** 06/08/22 **********************************
     d   svInvTypID    s                   like(DEAINVTYPID)
      *** End Add ***** 06/08/22 **********************************
      *** Begin Add *** 11/29/22 **********************************
     d   svPgm         s                   like(@@pgm)
     d   svPoDtIVC     s                   like(DEAPODT)
      *** End   Add *** 11/29/22 **********************************
      *** Beg Add ***** 06/29/22 **********************************
     d   svRef#        s                   like(DEARef#)
      *** End Add ***** 06/29/22 **********************************
      *** Beg Add ***** 10/15/21 **********************************
     d   svChk#        s                   like(DEACHK#)
      *** End Add ***** 10/15/21 **********************************
      *** Begin Add *** 09/29/23 **********************************
     d   usesWDEAP     s               n   Inz(*Off)
      *** End   Add *** 09/29/23 **********************************
     d isBatchUpload   s               n
     d indPostCurrDate...
     d                 s               n
     d indDissToPriorYear...
     d                 s               n
    d*=============================================================================================
    d* Data Structures and Arrays
    d*=============================================================================================
     d psds          esds                  extname(szq1p)

     d  arCodes        s                   like (DEAARCD) dim(99)
     d  catCodes       s                   like (DEASTAT) dim(99)
     d  instDt         s                   like (DEAPODT) dim(99)
     d  pYrArCodes     s                   like (DEAARCD) dim(99)
     d  pYrCatCodes    s                   like (DEASTAT) dim(99)
     d  ctyTyp         s              2    dim(99)
     d  bDts           s                   like (DEAPODT) dim(99)
     d  ctyPays        s             +6    like (M6EAAMT) dim(99)
     d  pYrBDts        s                   like (DEAPODT) dim(99) inz(0)        pr Yr Billing Dates
     d  cYrBDts        s                   like (DEAPODT) dim(99) inz(0)        curr Yr Billin Dates
     d  allBDts        s                   like (DEAPODT) dim(199) inz(9999999) all Billing Dates
     d                                     ascend
     d   #OfAxCty      c                   16
     d  axCat          s              1    dim(#OfAxCty) ctdata perrcd(#OfAxCty)
     d*** Begin Add *** 03/16/21 *******************************************
     d iCtyTyp         s              2    dim(99)
     d arCode          s                   Like(DEAARCD)
     d arCatType       s                   Like(DEACTYP)
     d arPostDate      s                   Like(DEAPODT)
     d chrInvArCategory...
     d                 s              1a   Inz
     d chrInvArCd      s              2a   Inz
     d chrInvArType    s              1a   Inz
     d isIcPayment     s               n   Inz
     d isSinglePayment...
     d                 s               n   Inz
     d isWritingLiveAR...
     d                 s               n   Inz
     d isWritingARBatch...
     d                 s               n   Inz
     d isWritingWZEAP  s               n   Inz
     d payment2        s             +2    Like(DEAAMT)
     d strSqlStmt      s            256a
     d y               s              3  0
     d*** End   Add *** 03/16/21 *******************************************
     d  twoDimMODS     ds                  occurs(30) inz                       1 occur per cty
     d    ctyAmByDt            1   1089  2 dim(99)                              cty Installment
     d    ctyPastPay        1090   1100  2                                      cty Past Pays
     d    ctyBalance        1101   1111  2                                      cty Balance
     d    ctyNegaPay        1112   1122  2                                      cty Negative Pays

     d pYrMODS         ds                  likeds(twoDimMODS) occurs(30) inz    prior Year

     d cYrMODS         ds                  likeds(twoDimMODS) occurs(30) inz    current Year
     d nYrMODS         ds                  likeds(twoDimMODS) occurs(30) inz    renewal Year
     d nYrBDts         s                   like (DEAPODT) dim(99) inz(0)        renew Yr Bill Dates
     d cashPostMthd    s              2
     d newBusPaymCd    s              2
     d newBusTrfrCd    s              2
     d cashPostTyp     s              2

     d                 ds
     d  axCodes                1     32
     d                                     dim(#OfAxCty)
     d   F1AXDD                1      2
     d   F1AXEC                3      4
     d   F1AXGF                5      6
     d   F1AXLF                7      8
     d   F1AXMF                9     10
     d   F1AXDP               11     12
     d   F1AXSF               13     14
     d   F1AXTX               15     16
     d   F1AX1T               17     18
     d   F1AX2T               19     20
     d   F1AX3T               21     22
     d   F1AX4T               23     24
     d   F1AX5T               25     26
     d   F1AXPT               27     28
     d   F1AX6T               29     30
     d   F1AX7T               31     32
     D*** Begin Add *** 04/16/25 *******************************************
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxInv#                        10    overlay(svlda:1)
     D*** End   Add *** 04/16/25 *******************************************
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
     d WA055R          pr
     d  @@pgm                         1
     d  @@rrn                         9  0                                      WDEAP    or WDELP
     d WA055R          pi
     d  @@pgm                         1
     d  @@rrn                         9  0

     d GetARCodes      pr
     d   type                         1    const
     d   prYrFlg                      1    const
     d   cdArray                           like(DEAARCD) dim(99)
     d   catArray                          like(DEASTAT) dim(99)

     d Dissolve2       pr
     d   prYrFlg                      1    const

     d getType         pr             1
     d   arType                       1    const
     d   arCat                        1    const

     d postAR          pr
     d   prYrFlg                      1    const
     d   payment                           like(DEAAMT)
     d   balance                     +2    like(DEAAMT)
     d   arCodes                           const like(DEAARCD)
     d   cat                               const like(DEASTAT)
     d   type                              const like(DEASTAT)
     d   svPodt                            const like(DEAPODT)

     d PostXPay        pr
     d   payType                      1    const
     d   payment                           const like(DEAAMT)

     d fixTime         pr
     d fixBatchTime    pr
     d getHiTime       pr                  like(M6EATIME)
     d getBillingBat#  pr                  like(DEABAT#)
     d getNegAmount    pr            +2    like(M6EAAMT)
     d   podtDt                            const like(M6EAPODT)
     d   instDt                            like(DEAPODT) dim (99) ascend
     d   arCat                        1    const
     d loadBillDates   pr
     d   billingDates                      like(DEAPODT) dim (99)
     d loadPYCtyAmts   pr
     d loadCYCtyAmts   pr
     d adjPYNegAmts    pr
     d adjCYNegAmts    pr
     d loadPYPastPay   pr
     d loadCYPastPay   pr
     d applyPYPastPay  pr
     d applyCYPastPay  pr
     d loadAllDts      pr
     d   prYrFlg                      1    const
     d applyPayment    pr
     d   prYrFlg                      1    const
     d loadNYCtyAmts   pr
     d adjNYNegAmts    pr
     d loadNYPastPay   pr
     d applyNYPastPay  pr

     d/copy wprar
     d/copy sprdate
     d/copy sprSql
     d/copy wprPmtSvc
     d*** Begin Add *** 09/29/23 ******************************************
     d/copy sprJobInfo
     d*** End   Add *** 09/29/23 ******************************************
    c*=====================================================
      *  MAINLINE PROCESSING
    c*=====================================================
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3
     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

       //*** Begin Add *** 09/29/23 ****************************************
       // For PGM=T, what program called Cash Dissolve,
       // is it SRBAT or WISRPAY
       usesWDEAP = PgmInStack('SRBAT');
       //*** End   Add *** 09/29/23 ****************************************
       //*** Begin Add *** 04/16/25 ****************************************
       // Retieve invoice #
       In svLDA;
          Monitor;
             svInvoice# = %Dec(xxInv#:10:0);
          On-Error;
             svInvoice# = 0;
          EndMon;
       Out svLDA;
       //*** End   Add *** 04/16/25 ****************************************

       If GetDisbInfo(@@rrn:kkco#:kkfnd:
        kkemp#:kkdiv:kkfyr:payment) = *Off;
          *Inlr = *On;
          Return;
       Endif;

       If isSinglePayment;
          payment2 = payment;
          PostAR(indDissToPriorYear:payment:payment2:chrInvArCd:
           chrInvArCategory:chrInvArType:sysDate);
       Else;
          Select;
          When @@pgm   = 'C' and
             newBusFlg = 'N';
             Select;
             When indDissToPriorYear = *On;
                priorYrFlg = *on;
                Dissolve2 (priorYrFlg);
             When indDissToPriorYear = *Off;
                priorYrFlg = *off;
                Dissolve2 (priorYrFlg);
             Endsl;
          When @@pgm    = 'C' and
             newBusFlg = 'Y';
             priorYrFlg = *off;
             Dissolve2 (priorYrFlg);
          When @@pgm   = 'N';
             priorYrFlg = *off;
             Dissolve2 (priorYrFlg);
          Endsl;
          FixTime();
          If isWritingARBatch;
             FixBatchTime();
          Endif;
       Endif;
       *Inlr = *On;
     p*===========================================================================
     p* GetDisbInfo - Gets info from WDF2P, key info, and payment amount
     p*===========================================================================
     p GetDisbInfo     b                   export
     d GetDisbInfo     pi              n
     d   rrn                          9  0 const
     d   kkco#                             like(DEACO#)
     d   kkfnd                             like(DEAFND)
     d   kkemp#                            like(DEAEMP#)
     d   kkdiv                             like(DEADIV)
     d   kkfyr                             like(DEAFYR)
     d   payment                           like(DEAAMT)

     c     f1ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr

     c     f2ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     eakyp         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     rsky          klist
     c                   kfld                    kkco#
     c                   kfld                    ELRTCD

     c     ifky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    RSJRST

     c     ifky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    RSJRST
     c                   kfld                    ELPFDT

       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       Chain rrn dea0p;
       If %Found And @@pgm = 'D';
          newBusFlg = 'Y';
          @@pgm = 'C';
       Endif;
       //*** Begin Add *** 11/29/22 ****************************************
       svInv# = 0;
       //*** End   Add *** 11/29/22 ****************************************
       Select;
       When @@pgm = 'C'
        Or @@pgm = 'L'
        Or @@pgm = 'S'
        Or @@pgm = 'U';
          kkco#   = DEACO#;
          kkfnd   = DEAFND;
          kkemp#  = DEAEMP#;
          kkdiv   = DEADIV;
          svRDiv  = DEARDIV;
          kkfyr   = DEAFYR;
          payment = DEACKAM;
          kkbat#  = DEABAT#;
          svAchF  = DEAACHF;
          svInv#  = DEAINV#;
          //*** Beg Add ***** 06/08/22 ***************************
          svInvTypId = DEAINVTYPID;
          //*** End Add ***** 06/08/22 ***************************
          //*** Beg Add ***** 06/08/22 ***************************
          svRef#  = DEAref#;
          //*** End Add ***** 06/08/22 ***************************
          //*** Beg Add ***** 10/15/21 ***************************
          svChk#  = DEACHK#;
          //*** End Add ***** 10/15/21 ***************************
          svBatchPoDt = DEAPODT;
          svBatchTrDt = DEATRDT;
          svBatchCkam = DEACKAM;
          svBatchSycd = DEASYCD;
          If @@pgm = 'L'
           Or @@pgm = 'S'
           Or @@pgm = 'U';
             Delete dea0p;
          Endif;
          If @@pgm = 'S';
             svBillBat# = getBillingBat#;
          Endif;
          If @@pgm = 'U';
             @@pgm = 'C';
             isBatchUpload = *On;
          Endif;
       When @@pgm = 'N' Or @@pgm = 'A'
        Or @@pgm = 'E';
          Chain rrn pel0p;
          kkco#  = ELCO#;
          kkfnd  = ELFND;
          kkemp# = ELEMP#;
          kkdiv  = ELDIV;
          kkfyr  = ELFYR;
          //*** Begin Chg *** 08/26/25 *************************************
          //*** Begin Add *** 04/16/25 *************************************
          //*** If svBatchPoDt = 0;
          //***    Exec Sql
          //***      Select EApoDT
          //***      into   :svBatchPoDt
          //***      From   wmeap
          //***  Where  EAco#=:kkCo# And EAfnd=:kkFnd and EAemp#=:kkEmp# And
          //***            EAdiv=:kkDiv And EAninv=:svInvoice#
          //***      Limit 1;
          //*** EndIf;
          //*** End   Add *** 04/16/25 *************************************
          If svBatchPoDt = 0;
             Exec Sql
                Select EApoDt, EAinvTypID, EAref#
                Into :svBatchPoDt, :svInvTypID, :svRef#
                From WMEAP
                Where EAco#=:kkCo# And EAfnd=:kkFnd And EAemp#=:kkEmp# And
                   EAdiv=:kkDiv And EAfyr=:kkFyr And EAninv=:svInvoice# And
                   EAref#<>' ';
          Else;
             Exec Sql
                Select EAinvTypID, EAref#
                Into :svInvTypID, :svRef#
                From WMEAP
                Where EAco#=:kkCo# And EAfnd=:kkFnd And EAemp#=:kkEmp# And
                   EAdiv=:kkDiv And EAfyr=:kkFyr And EAinv#=:svInvoice# And
                   EAref#<>' ';
          EndIf;
          //*** End   Chg *** 08/26/25 *************************************
       When  @@pgm = 'T'
        Or @@pgm = 'M'
        Or @@pgm = 'I'
        Or @@pgm = 'B';
          //*** Begin Add *** 11/29/22 *********************
          svPgm = @@pgm;
          //*** End   Add *** 11/29/22 *********************
          //*** Begin Chg *** 09/29/23 *************************************
          //*** Exec Sql
          //***  Select EAco#,
          //***   EAfnd,
          //***   EAemp#,
          //***   EAdiv,
          //***   EAfyr,
          //***   EAinv#,
          //***   EAbat#,
          //***   EAamt,
          //***   EAcTyp,
          //***   EArdiv,
          //*** Beg Add ***** 06/08/22 ***************************
          //***   EAachf,
          //***   EAInvTypId,
          //*** End Add ***** 06/08/22 ***************************
          //*** Beg Add ***** 06/29/22 ***************************
          //*** Begin Del *** 11/29/22 ***************************
          //***  EAref#
          //*** End   Del *** 11/29/22 ***************************
          //*** End Add ***** 06/29/22 ***************************
          //*** Begin Add *** 11/29/22 ***************************
          //***   EAref#,
          //***   EApoDt
          //*** End   Add *** 11/29/22 ***************************
          //***  Into :kkCo#,
          //***   :kkfnd,
          //***   :kkEmp#,
          //***   :kkDiv,
          //***   :kkfyr,
          //***   :svInv#,
          //***   :kkBat#,
          //***   :payment,
          //***   :chrInvArCategory,
          //***   :svRDiv,
          //*** Beg Add ***** 06/08/22 ***************************
          //***   :svAchF,
          //***   :svInvTypId,
          //*** End Add ***** 06/08/22 ***************************
          //*** Beg Add ***** 06/29/22 ***************************
          //*** Begin Add *** 11/29/22 ***************************
          //***  :svRef#
          //*** Begin Add *** 11/29/22 ***************************
          //*** End Add ***** 06/29/22 ***************************
          //*** Begin Add *** 11/29/22 ***************************
          //***   :svRef#,
          //***   :svPoDtIVC
          //*** End   Add *** 11/29/22 ***************************
          //***  From wdeap_hld
          //***  Where rrn(WDEAP_HLD)=:@@rrn;
          // This was called from SRBAT with @@PGM = T
          If @@pgm = 'T' And usesWDEAP;
             Exec Sql
              Select EAco#,
               EAfnd,
               EAemp#,
               EAdiv,
               EAfyr,
               EAinv#,
               EAbat#,
               EAamt,
               EAcTyp,
               EArdiv,
               EAachf,
               EAInvTypId,
               EAref#,
               EApoDt
              Into :kkCo#,
               :kkfnd,
               :kkEmp#,
               :kkDiv,
               :kkfyr,
               :svInv#,
               :kkBat#,
               :payment,
               :chrInvArCategory,
               :svRDiv,
               :svAchF,
               :svInvTypId,
               :svRef#,
               :svPoDtIVC
              From wdeap
              Where rrn(WDEAP)=:@@rrn;
          Else;
             Exec Sql
              Select EAco#,
               EAfnd,
               EAemp#,
               EAdiv,
               EAfyr,
               EAinv#,
               EAbat#,
               EAamt,
               EAcTyp,
               EArdiv,
               EAachf,
               EAInvTypId,
               EAref#,
               EApoDt
              Into :kkCo#,
               :kkfnd,
               :kkEmp#,
               :kkDiv,
               :kkfyr,
               :svInv#,
               :kkBat#,
               :payment,
               :chrInvArCategory,
               :svRDiv,
               :svAchF,
               :svInvTypId,
               :svRef#,
               :svPoDtIVC
              From wdeap_hld
              Where rrn(WDEAP_HLD)=:@@rrn;
          EndIf;
          //*** End   Chg *** 09/29/23 **********************************
          If isValidFetch(sqlstt);
             //*** Begin Chg *** 09/29/23 **********************************
             //*** Exec Sql
             //*** Delete From WDEAP_HLD
             //***  Where rrn(WDEAP_HLD)=:@@rrn;
             If @@pgm = 'T' And usesWDEAP;
                Exec Sql
                 Delete From WDEAP
                 Where rrn(WDEAP)=:@@rrn;
             Else;
                Exec Sql
                 Delete From WDEAP_HLD
                 Where rrn(WDEAP_HLD)=:@@rrn;
             EndIf;
             //*** End   Chg *** 09/29/23 **********************************
          Else;
              Return *Off;
          Endif;
       Endsl;
       Chain f1ky pf101;
       Chain f2ky pex01;
       Chain f2ky pc301;
       Chain f2ky pf201;
       If F2CPMT = *blank;
          Select;
          When @@pgm  = 'A';
             Select;
             When F2APYF = 'Y';
                F2CPMT = '1';
             When F2ACYF = 'Y';
                F2CPMT = '2';
             Endsl;
          When @@pgm  = 'C';
             Select;
             When F2CPYF = 'Y';
              F2CPMT = '1';
             When F2CCYF = 'Y';
              F2CPMT = '2';
             Endsl;
          When @@pgm  = 'E';
             Select;
             When F2RPYF = 'Y';
                F2CPMT = '1';
             When F2RCYF = 'Y';
                F2CPMT = '2';
             Endsl;
          When @@pgm  = 'L';
             Select;
             When F2LPYF = 'Y';
                F2CPMT = '1';
             When F2LCYF = 'Y';
                F2CPMT = '2';
             Endsl;
             @@pgm = 'C';
             lockBoxFlg = 'Y';
          When @@pgm  = 'N';
             F2CPMT = '2';
          When @@pgm  = 'S';
             Select;
             When f2cpyf = 'Y';
                f2cpmt = '1';
             When f2ccyf = 'Y';
                f2cpmt = '2';
             Endsl;
             If expybs <> 'Y';
                f2cpmt = '2';
             Endif;
             @@pgm = 'C';
             srOnliFlg = 'Y';
          Endsl;
       Else;
          If @@pgm = 'L';
             @@pgm = 'C';
             lockBoxFlg = 'Y';
          Endif;
          If @@pgm = 'S';
             @@pgm = 'C';
             srOnliFlg = 'Y';
             If expybs <> 'Y';
                f2cpmt = '2';
             Endif;
          Endif;
       Endif;
       If F2RNEW = *blank;
          Select;
          When lockBoxFlg = 'Y';
             If F2LRYF = 'Y';
                F2RNEW = 'Y';
             Endif;
          When @@pgm  = 'A';
             If F2ARYF = 'Y';
                F2RNEW = 'Y';
             Endif;
          When @@pgm  = 'C'
           Or srOnliFlg = 'Y';
             If F2CRYF = 'Y';
                F2RNEW = 'Y';
             Endif;
          When @@pgm  = 'E';
             If F2RRYF = 'Y';
                F2RNEW = 'Y';
             Endif;
          Endsl;
       Endif;
       If F2CDTY = *blank;
          Select;
          When lockBoxFlg = 'Y';
             F2CDTY = F2LPDT;
          When @@pgm  = 'A';
             F2CDTY = F2APDT;
          When @@pgm  = 'C'
           Or srOnliFlg = 'Y';
             F2CDTY = F2CPDT;
          When @@pgm  = 'E';
             F2CDTY = F2RPDT;
          When @@pgm  = 'N';
             Select;
             When F2CPDT <> *blank;
                F2CDTY = F2CPDT;
             Other;
                F2CDTY = 'B';
             Endsl;
          Endsl;
       Else;
          If lockBoxFlg = 'Y';
             F2CDTY = 'B';
          Endif;
          If @@pgm  = 'A';
             F2CDTY = 'C';
          Endif;
       Endif;
       Select;
       When F2cpmt = '1';
          indDissToPriorYear = *On;
       When F2cpmt = '2';
          indDissToPriorYear = *Off;
       EndSl;
       // Post to the billing date or current date
       Select;
       When F2cdty = 'B';
          indPostCurrDate = *Off;
       When F2cdty = 'C';
          indPostCurrDate = *On;
       Endsl;
       newBusPaymCd = F2NPCD;
       newBusTrfrCd = F2NXCD;
       If indDissToPriorYear = *On;
          Setll eaky pel0l;
          Readpe eakyp pel0l;
          Select;
          When %Eof;
             indDissToPriorYear = *Off;
          When ELFAF  = 'A'
           And ELCTST = 'Y'
           And F2CAUD = 'Y';
             indDissToPriorYear = *Off;
          Endsl;
       Endif;
       Select;
       When @@pgm = 'T';
          //*** Begin Add *** 09/29/23 *************************************
          //*** isIcPayment = *On;
          If usesWDEAP;
             isIcPayment = *Off;
          Else;
             isIcPayment = *On;
          EndIf;
          //*** End   Add *** 09/29/23 *************************************
          indDissToPriorYear = *Off;
          F2rnew = *Blank;
          @@pgm = 'C';
          srOnliFlg = 'Y';
          If EXicP2GAR = 'Y';
             isWritingLiveAR  = *On;
          Else;
             isWritingARBatch = *On;
          Endif;
       When @@pgm = 'M';
          @@pgm = 'C';
          isIcPayment = *On;
          indDissToPriorYear = *Off;
          F2rnew = *Blank;
          If EXicAmpAR = 'Y';
             isWritingLiveAR  = *On;
          Else;
             isWritingARBatch = *On;
          Endif;
       When @@pgm = 'B';
          @@pgm = 'C';
          isIcPayment = *On;
          indDissToPriorYear = *Off;
          F2rnew = *Blank;
          If EXicPayAR = 'Y';
             isWritingLiveAR  = *On;
          Else;
             isWritingARBatch = *On;
          Endif;
       When @@pgm = 'I';
          @@pgm = 'C';
          isIcPayment = *On;
          isSinglePayment = *On;
          indDissToPriorYear = *Off;
          F2rnew = *Blank;
          Select;
          When chrInvArCategory = 'C';
             chrInvArCd   = F2rpcd;
             chrInvArType = 'P';
          When chrInvArCategory = 'R';
             chrInvArCd   = F2dpcd;
             chrInvArType = 'Q';
          Endsl;
          If EXicPayAR = 'Y';
             isWritingLiveAR  = *On;
          Else;
             isWritingARBatch = *On;
          Endif;
       Other;
          If lockBoxFlg <> 'Y';
             Chain emky pem01;
             If %Found and EMEACD = 'Y';
                Return *Off;
             Endif;
          Endif;
          Select;
          When @@pgm = 'A' Or @@pgm = 'E';
             isWritingLiveAR = *On;
             payment = 0;
             Setll eaky mea0l6;
             Reade eaky mea0l6;
             Dow not(%eof);
                If GetArCat(M6EAARCD) = 'P';
                   payment = payment - ArValue(M6EAAMT:M6EAARTY);
                Endif;
                Reade eaky mea0l6;
             Enddo;
             Select;
             When @@pgm = 'A';
                @@pgm = 'C';
                auditFlg = 'Y';
             When @@pgm = 'E';
                @@pgm = 'C';
                hierType = 'N';
                auditFlg = 'E';
             Endsl;
          When @@pgm = 'N';
             isWritingLiveAR = *On;
             hierType = 'N';
             payment = 0;
             Setll eaky mea0l6;
             Reade eaky mea0l6;
             Dow Not(%Eof);
                If M6EAARCD = F2NPCD;
                   payment = payment + M6EAAMT;
                   svInv# = m6EAninv;
                Endif;
                Reade eaky mea0l6;
             Enddo;
          When lockBoxFlg = 'Y' Or srOnliFlg = 'Y' Or isBatchUpload;
             isWritingARBatch = *On;
          When @@pgm = 'C';
             isWritingWZEAP = *On;
          Endsl;
          If F2RNEW = 'Y'
          And @@pgm <> 'N';
             Setgt eaky pel0l;
             Reade eakyp pel0l;
             If Not(%Eof);
                dslvToNwYr = 'Y';
                Readpe eakyp pel0l;
             Endif;
          Endif;
       Endsl;
       Chain eaky pel0l;
       Chain rsky prs0l;
       Setgt ifky1 pif0l;
       Readpe ifky pif0l;
       Return *On;
     p GetDisbInfo     e
     p*===========================================================================
     p* GetARCodes - Gets Cash  or NB codes from WDCDP and WTC3P and puts into array
     p*===========================================================================
     p GetARCodes      b                   export
     d GetARCodes      pi
     d   type                         1    const
     d   prYrFlg                      1    const
     d   codeArr                           like (DEAARCD) dim(99)
     d   catArr                            like (DEASTAT) dim(99)

     d   kkhier        s              5  0 inz(0)
     d   ctr           s              3  0 inz(1)
     d   kkbsky        s             10    inz('          ')
     d   x             s              3  0 inz(1)

     c     cdky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     cdky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkhier

     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     bsky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkbscd

     c                   if        prYrFlg = *on
     c                   eval      k1fyr = kkfyr - 1
     c                   else
     c                   eval      k1fyr = kkfyr
     c                   endif
      *
      *  If cash dissolve hierarchy doesn't exist, load up the default hierarchy to dissolve with.
      *  There's an edit in W1210R02 to prevent a transaction from being dissolved when the
      *  hierarchy isn't set up; however, this program might be called from billing when no
      *  hierarchy exists. The default hierarchy is the one that used to be in W8010R.
      *
     c     cdky          chain     pcd01
     c                   if        not(%found)
     c                   call      'FIXWC1179A'
     c                   parm                    kkco#
     c                   parm                    kkfnd
     c                   endif

     c                   clear                   codeArr
     c                   clear                   catArr
     c     cdky1         setll     pcd01
     c     cdky          reade     pcd01
     c                   dow       not(%eof)

     c                   if        CDHIER < 10000

     c                   if        CDCTYP <> 'P'
     c                   select

      * Iterate to next WDCDP record if S/R Online not flagged to include deposit
     c                   when      srOnliFlg = 'Y' and
     c                             CDctyp = 'D' and
     c                             EXindp <> 'Y'

     C*** begin add ***  03/16/21  ***********************************************
      * Iterate to next WDCDP record if S/R Online and flagged to include deposit
      * but policy is opt'd in to IC.
     c                   when      srOnliFlg = 'Y' and
     c                             CDctyp = 'D' and
     c                             EXindp = 'Y' and
     C                             ICpolicyOpt(kkCo#:kkFnd:kkEmp#) = 'Y'
     C
     C*** end add *****  03/16/21  ***********************************************
     c                   when      type = 'C'
     c                   if        auditFlg = 'Y'
     c                   select
     c                   when          CDCTYP = 'D'
     c                             and GetArType(CDCPCD) = 'Q'
     c                   if        F1AXTD <> *blank
     c                   eval      codeArr(ctr) = F1AXTD
     c                   eval      catArr(ctr)  = CDCTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   if           codeArr(ctr) = IFIFPD
     c                             or codeArr(ctr) = IFIFTR
     c                             or codeArr(ctr) = IFIFTC
     c                   movel     'I'           ctyTyp(ctr)
     c                   endif
     c                   add       1             ctr
     c                   endif
     c                   other
     c                   eval      x = 1
     c     CDCTYP        lookup    axCat(x)                               60
     c                   if            %found
     c                             and axCodes(x) <> *blank
     c                   eval      codeArr(ctr) = axCodes(x)
     c                   eval      catArr(ctr)  = CDCTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   if           codeArr(ctr) = IFIFPD
     c                             or codeArr(ctr) = IFIFTR
     c                             or codeArr(ctr) = IFIFTC
     c                   movel     'I'           ctyTyp(ctr)
     c                   endif
     c                   add       1             ctr
     c                   endif
     c                   endsl
     c                   else
     c                   eval      catArr(ctr)  = CDCTYP
     c                   eval      codeArr(ctr) = CDCPCD
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   if           codeArr(ctr) = IFIFPD
     c                             or codeArr(ctr) = IFIFTR
     c                             or codeArr(ctr) = IFIFTC
     c                   movel     'I'           ctyTyp(ctr)
     c                   endif
     c                   add       1             ctr
     c                   endif

     c                   when      type = 'N'
     c                   eval      codeArr(ctr) = CDNBCD
     c                   eval      catArr(ctr)  = CDCTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   if           codeArr(ctr) = IFIFPD
     c                             or codeArr(ctr) = IFIFTR
     c                             or codeArr(ctr) = IFIFTC
     c                   movel     'I'           ctyTyp(ctr)
     c                   endif
     c                   add       1             ctr
     c                   endsl

     c                   endif                                                  CDCTYP <> 'P'

     c                   if        CDCTYP = 'P'

     c                   select
     c                   when      type = 'C'
     c     elky          chain     pel0l

     c                   select
     c                   when      auditFlg = 'Y'
     c                   eval      x = 1
     c     CDCTYP        lookup    axCat(x)                               60
     c                   if            %found
     c                             and axCodes(x) <> *blank
     c                   eval      codeArr(ctr) = axCodes(x)
     c                   eval      catArr(ctr)  = CDCTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   if           codeArr(ctr) = IFIFPD
     c                             or codeArr(ctr) = IFIFTR
     c                             or codeArr(ctr) = IFIFTC
     c                   movel     'I'           ctyTyp(ctr)
     c                   endif
     c                   add       1             ctr
     c                   endif
     c                   when      ELFAF = 'A'
     c                   eval      codeArr(ctr) = C3APCD
     c                   eval      catArr(ctr)  = C3CTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   add       1             ctr
     c                   when      ELFAF <> 'A'
     c                   movel     ELBSCD        kkbscd
     c     bsky          chain     pbs01
     c                   if        BSSEFL = 'S'
     c                   eval      codeArr(ctr) = C3SPCD
     c                   eval      catArr(ctr)  = C3CTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   add       1             ctr
     c                   else
     c                   eval      codeArr(ctr) = C3CPCD
     c                   eval      catArr(ctr)  = C3CTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   add       1             ctr
     c                   endif
     c                   endsl
     c
     c                   when      type = 'N'
     c     cdky          chain     pc301
     c                   eval      codeArr(ctr) = C3CNCD
     c                   eval      catArr(ctr)  = C3CTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   add       1             ctr
     c                   endsl

     c                   endif                                                  CDCTYP  = 'P'

     c                   endif                                                  CDHIER < 10000

     c     cdky          reade     pcd01
     c                   enddo
     c                   return

     p GetARCodes      e
     p*===========================================================================
     p* Dissolve2 - procedure that replaces Dissolve and payInstall
     p*===========================================================================
     p Dissolve2       b
     d Dissolve2       pi
     d   prYrFlg                      1    const

     d  x              s              3  0 inz(1)

     c                   reset     *all          pYrMODS
     c                   reset     *all          cYrMODS

     c                   if        prYrFlg = *on
     c                   callp     getARCodes(hierType  :
     c                                        priorYrFlg:
     c                                        pYrArCodes:
     c                                        pYrCatCodes)
     c                   eval      k1fyr = kkfyr - 1
     c                   callp     loadBillDates(pYrBDts)
     c                   callp     loadPYCtyAmts
     c                   callp     loadPYPastPay
     c                   callp     adjPYNegAmts
     c                   callp     applyPYPastPay
     c                   endif

     c                   callp     getARCodes(hierType:
     c                                        currYr :
     c                                        arCodes:
     c                                        catCodes)
     c                   eval      k1fyr = kkfyr
     c                   callp     loadBillDates(cYrBDts)
     c                   callp     loadCYCtyAmts
     c                   callp     loadCYPastPay
     c                   callp     adjCYNegAmts
     c                   callp     applyCYPastPay

     c                   if        dslvToNwYr = 'Y'
     c                   eval      k1fyr = kkfyr + 1
     c                   callp     loadBillDates(nYrBDts)
     c                   callp     loadNYCtyAmts
     c                   callp     loadNYPastPay
     c                   callp     adjNYNegAmts
     c                   callp     applyNYPastPay
     c                   endif

     c                   callp     applyPayment(prYrFlg)

     p Dissolve2       e
     p*===========================================================================
     p* loadBillDates - loads post dates transactions into a bDts array
     p*===========================================================================
     p loadBillDates   b
     d loadBillDates   pi
     d   bDts                              like(DEAPODT) dim (99)

     d  y              s              3  0 inz(1)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     d4ky          klist
     c                   kfld                    kkco#
     c                   kfld                    svBillbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)
     c                   if        (M6EAARTY = 'A' or
     c                              M6EAARTY = 'B' or
     c                              M6EAARTY = 'D' or
     c                              M6EAARTY = 'H' or
     c                              M6EAARTY = 'F')
     c                   eval      y = 1
     c     M6EAPODT      lookup    bDts(y)                                60
     c                   if        not *in60
     c                   eval      y = 1
     c     *zero         lookup    bDts(y)                                60
     c                   eval      bDts(y)   = M6EAPODT
     c                   endif
     c                   endif
     c     eaky          reade     mea0l6
     c                   enddo


      /free
       If srOnliFlg = 'Y';
          Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
          Dou %Eof;
          Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
             If %Eof;
                Leave;
             Endif;
             If billtype(getARtype(d30eaarcd)) = *on;
                y = %Lookup(d30EAPoDt:bDts);
                If y = 0;
                   y = %Lookup(0:bDts);
                   bDts(y) = d30EAPoDt;
                Endif;
             Endif;
          Enddo;
       Endif;
      /end-free

     p loadBillDates   e
     p*===========================================================================
     p* loadAlllDates - loads post dates transactions into a bDts array
     p*===========================================================================
     p loadAllDts      b
     d loadAllDts      pi
     d   prYrFlg                      1    const

     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)

     c                   if        prYrFlg = *on
     c     1             do        99            x
     c                   if        pYrBDts(x) <> 0
     c                   eval      y = 1
     c     pYrBDts(x)    lookup    allBDts(y)                             60
     c                   if        not %found
     c                   eval      y = 1
     c     9999999       lookup    allBDts(y)                             60
     c                   eval      allBDts(y) = pYrBDts(x)
     c                   endif
     c                   endif
     c                   enddo
     c                   endif

     c     1             do        99            x
     c                   if        cYrBDts(x) <> 0
     c                   eval      y = 1
     c     cYrBDts(x)    lookup    allBDts(y)                             60
     c                   if        not %found
     c                   eval      y = 1
     c     9999999       lookup    allBDts(y)                             60
     c                   eval      allBDts(y) = cYrBDts(x)
     c                   endif
     c                   endif
     c                   enddo

     c     1             do        99            x
     c                   if        nYrBDts(x) <> 0
     c                   eval      y = 1
     c     nYrBDts(x)    lookup    allBDts(y)                             60
     c                   if        not %found
     c                   eval      y = 1
     c     9999999       lookup    allBDts(y)                             60
     c                   eval      allBDts(y) = nYrBDts(x)
     c                   endif
     c                   endif
     c                   enddo

     c                   sorta     allBDts

     p loadAllDts      e
     p*===========================================================================
     p* loadPYCtyAmts - Loop through pYrBDts array loading billing transaction amounts
     p*                 for each ctyAmByDt element in pYrMods
     p*===========================================================================
     p loadPYCtyAmts   b
     d loadPYCtyAmts   pi

     d  catType        s              2                                         cty + ARType
     d  arType         s              1
     d  arCat          s              1
     d  kkpodt         s                   like(DEAPODT)
     d  xxAmt          s                   like(DEAAMT)
     d  xxArCD         s                   like(DEAARCD)
     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)
     d  z              s              3  0 inz(1)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    kkpodt
     c*
     c     d4ky          klist
     c                   kfld                    kkco#
     c                   kfld                    svBillbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    kkpodt

     c     1             do        99            x
     c                   if        pYrBDts(x) <> 0
     c                   eval      kkpodt = pYrBDts(x)
     c     eaky          setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)
     c                   eval      xxAmt = M6EAAMT
     c                   eval      xxArcd = M6EAARCD
     c                   exsr      prcRec
      *
     c     eaky          reade     mea0l6
     c                   enddo

      /free
       If srOnliFlg = 'Y';
          kkPoDt = pYrBDts(x);
          Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr:kkPoDt) dea30;
          Dou %Eof;
          Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr:kkPoDt) dea30;
             If %Eof;
                Leave;
             Endif;
             xxAmt  = d30EAamt;
             xxArCd = d30EAArCd;
             Exsr prcRec;
          Enddo;
       Endif;
      /end-free
     c                   else
     c                   leave
     c                   endif
     c                   enddo
      *===============================================================
     c     prcRec        begsr
      *===============================================================

     c                   eval      arCat  = getARCat(xxARCD)
     c                   eval      arType = getARType(xxARCD)
      *
     c                   select
      * Exclude transaction if processing IC and A/R code is excluded
     c                   When      isExcFromIC(xxARCD) = *On
      * Exclude dividend billing
     c                   when      xxArCd = F1dvbl
     c                   when      xxARCD = IFIFBL
     c                   eval      y = 1
     c     'IQ'          lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     pYrMods
     c                   eval      pYrMods.ctyAmByDt(x) =
     c                             pYrMods.ctyAmByDt(x) + xxAMT
     c                   endif
     c                   when      billType(arType) = *on
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      y = 1
     c     catType       lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     pYrMods
     c                   eval      pYrMods.ctyAmByDt(x) =
     c                             pYrMods.ctyAmByDt(x) + xxAMT
     c                   endif
     c
     c* Reduce the installment premium amt within arrays by the Retro Credit amt
     c                   when      xxARCD = F1RCCD
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      y = 1
     c     catType       lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     pYrMods
     c                   eval      pYrMods.ctyAmByDt(x) =
     c                             pYrMods.ctyAmByDt(x) - xxAMT
     c                   endif
     c
     c                   endsl
     c                   endsr

     p loadPYCtyAmts   e
     p*===========================================================================
     p* loadCYCtyAmts - Loop through cYrBDts array loading billing transaction amounts
     p*                 for each ctyAmByDt element in cYrMods
     p*===========================================================================
     p loadCYCtyAmts   b
     d loadCYCtyAmts   pi

     d  catType        s              2                                         cty + ARType
     d  arType         s              1
     d  arCat          s              1
     d  kkpodt         s                   like(DEAPODT)
     d  xxAMT          s                   like(DEAAMT)
     d  xxArCD         s                   like(DEAARCD)
     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)
     d  z              s              3  0 inz(1)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    kkpodt
     c     d4ky          klist
     c                   kfld                    kkco#
     c                   kfld                    svBillbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    kkpodt


       For x = 1 to 99;
         // Gather A/R by post date
          If cYrBDts(x) <> 0;
             kkpodt = cYrBDts(x);
             Setll eaky mea0l6;
             Dou %Eof;
                Reade eaKy mea0l6;
                If %Eof;
                   Leave;
                EndIf;
                xxAmt = M6EAAMT;
                xxArcd = M6EAARCD;
                Exsr prcRec;
            Enddo;

            If srOnliFlg = 'Y';
             // Gather A/R Batch by post date for S/R Online
               kkPoDt = cYrBDts(x);
               Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr:kkPoDt) dea30;
               Dou %Eof;
               Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr:kkPoDt) dea30;
                  If %Eof;
                     Leave;
                  Endif;
                  xxAmt  = d30EAamt;
                  xxArCd = d30EAArCd;
                  Exsr prcRec;
               Enddo;
            Endif;
          Else;
             Leave;
          Endif;
       Endfor;

     c*================================================================*
     c     prcRec        begsr
     c*================================================================*

     c                   eval      arCat  = getARCat(xxARCD)
     c                   eval      arType = getARType(xxARCD)

     c                   select
      * Exclude transaction if processing IC and A/R code is excluded
     c                   When      isExcFromIC(xxARCD) = *On
      * Exclude dividend billing
     c                   when      xxArCd = F1dvbl
     c                   when      xxARCD = IFIFBL
     c                   eval      y = 1
     c     'IQ'          lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     cYrMods
     c                   eval      cYrMods.ctyAmByDt(x) =
     c                             cYrMods.ctyAmByDt(x) + xxAMT
     c                   endif
     c                   when      billType(arType) = *on
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      y = 1
     c     catType       lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     cYrMods
     c                   eval      cYrMods.ctyAmByDt(x) =
     c                             cYrMods.ctyAmByDt(x) + xxAMT
     c                   endif
     c*
     c* Reduce the installment premium amt within arrays by the Retro Credit amt
     c                   when      xxARCD = F1RCCD
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      y = 1
     c     catType       lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     cYrMods
     c                   eval      cYrMods.ctyAmByDt(x) =
     c                             cYrMods.ctyAmByDt(x) - xxAMT
     c                   endif
     c*
     c                   endsl
     c                   endsr
     p loadCYCtyAmts   e
     p*===========================================================================
     p* loadNYCtyAmts - Loop through nYrBDts array loading billing transaction amounts
     p*                 for each ctyAmByDt element in nYrMods
     p*===========================================================================
     p loadNYCtyAmts   b
     d loadNYCtyAmts   pi

     d  catType        s              2                                         cty + ARType
     d  arType         s              1
     d  arCat          s              1
     d  kkpodt         s                   like(DEAPODT)
     d  xxAMT          s                   like(DEAAMT)
     d  xxArCD         s                   like(DEAARCD)
     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)
     d  z              s              3  0 inz(1)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    kkpodt
     c     d4ky          klist
     c                   kfld                    kkco#
     c                   kfld                    svBillbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    kkpodt

     c     1             do        99            x
     c                   if        nYrBDts(x) <> 0
     c                   eval      kkpodt = nYrBDts(x)
     c     eaky          setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)
     c                   eval      xxAmt = M6EAAMT
     c                   eval      xxArcd = M6EAARCD
     c                   exsr      prcRec
      *
     c     eaky          reade     mea0l6
     c                   enddo

      /free
       If srOnliFlg = 'Y';
          kkPoDt = nYrBDts(x);
          Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr:kkPoDt) dea30;
          Dou %Eof;
          Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr:kkPoDt) dea30;
             If %Eof;
                Leave;
             Endif;
             xxAmt  = d30EAamt;
             xxArCd = d30EAArCd;
             Exsr prcRec;
          Enddo;
       Endif;
      /end-free
     c                   else
     c                   leave
     c                   endif
     c                   enddo

     c*================================================================*
     c     prcRec        begsr
     c*================================================================*

     c                   eval      arCat  = getARCat(xxARCD)
     c                   eval      arType = getARType(xxARCD)
      *
     c                   select
      * Exclude transaction if processing IC and A/R code is excluded
     c                   When      isExcFromIC(xxARCD) = *On
      * Exclude dividend billing
     c                   when      xxArCd = F1dvbl
     c                   when      xxARCD = IFIFBL
     c                   eval      y = 1
     c     'IQ'          lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     nYrMods
     c                   eval      nYrMods.ctyAmByDt(x) =
     c                             nYrMods.ctyAmByDt(x) + xxAMT
     c                   endif
     c                   when      billType(arType) = *on
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      y = 1
     c     catType       lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     nYrMods
     c                   eval      nYrMods.ctyAmByDt(x) =
     c                             nYrMods.ctyAmByDt(x) + xxAMT
     c                   endif
     c*
     c* Reduce the installment premium amt within arrays by the Retro Credit amt
     c                   when      xxARCD = F1RCCD
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      y = 1
     c     catType       lookup    ctyTyp(y)                              60
     c                   if        %found
     c     y             occur     nYrMods
     c                   eval      nYrMods.ctyAmByDt(x) =
     c                             nYrMods.ctyAmByDt(x) - xxAMT
     c                   endif
     c*
     c                   endsl
     c                   endsr
     p loadNYCtyAmts   e
     p*===========================================================================
     p* getType - gets hierarchy type for lookup
     p*===========================================================================
     p getType         b
     d getType         pi             1
     d   arType                       1    const
     d   arCat                        1    const

     c                   select
     c                   when        arCat = 'E'  or
     c                               arCat = 'P'  and
     c                             (arType = 'N'  or
     c                              arType = 'R'  or
     c                              arType = 'V'  or
     c                              arType = 'T'  or
     c                              arType = 'R'  or
     c                              arType = 'P')
     c                   return    'P'
     c                   when        arCat = 'D'  and
     c                             (arType = 'I'  or
     c                              arType = 'R'  or
     c                              arType = 'T') or
     c                               arType = 'H'
     c                   return    'I'
     c                   when      arType = 'A' or
     c                             arType = 'B' or
     c                             arType = 'D'
     c                   return    'P'
     c                   when      arType = 'F' or
     c                             arType = 'Q'
     c                   return    'Q'
     c                   other
     c                   return    *blank
     c                   endsl

     p getType         e
     p*===========================================================================
     p* adjPYNegAmts - wash negative amounts out of each ctyAmByDt array in pYrMODS
     p*===========================================================================
     p adjPYNegAmts    b
     d adjPYNegAmts    pi

     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)
     d  negAmount      s             15  2 inz(1)

     c                   eval      negAmount = 0
     c     1             do        30            x
     c     x             occur     pYrMODS
     c                   eval      negAmount = 0
     c     1             do        99            y
     c                   if        pYrMods.ctyAmByDt(y) < 0
     c                   eval      negAmount =
     c                             negAmount + pYrMods.ctyAmByDt(y)
     c                   eval      pYrMods.ctyAmByDt(y) = 0
     c                   endif
     c                   enddo
     c                   if        negAmount < 0 and
     c                             pYrMods.ctyNegaPay < 0
     c                   if        negAmount <= pYrMods.ctyNegaPay
     c                   eval      negAmount =
     c                             negAmount - pYrMods.ctyNegaPay
     c                   eval      pYrMods.ctyNegaPay = 0
     c                   else
     c                   eval      pYrMods.ctyNegaPay =
     c                             pYrMods.ctyNegaPay - negAmount
     c                   eval      negAmount = 0
     c                   endif
     c                   endif
     c                   eval      pYrMods.ctyPastPay =
     c                             pYrMods.ctyPastPay + pYrMods.ctyNegaPay
     c     1             do        99            y
     c                   if        negAmount < 0
     c                   select
     c                   when      pYrMODS.ctyAmByDt(y)  > 0
     c                             and pYrMODS.ctyAmByDt(y) >= (negAmount * -1)
     c                   eval      pYrMODS.ctyAmByDt(y) =
     c                             pYrMODS.ctyAmByDt(y) + negAmount
     c                   eval      negAmount     = 0
     c                   when      pYrMODS.ctyAmByDt(y) > 0  and
     c                             pYrMODS.ctyAmByDt(y) < (negAmount * -1)
     c                   eval      negAmount = negAmount + pYrMODS.ctyAmByDt(y)
     c                   eval      pYrMODS.ctyAmByDt(y) = 0
     c                   endsl
     c                   endif
     c                   enddo
     c                   enddo

     p adjPYNegAmts    e
     p*===========================================================================
     p* adjCYNegAmts - wash negative amounts out of each ctyAmByDt array in cYrMODS
     p*===========================================================================
     p adjCYNegAmts    b
     d adjCYNegAmts    pi

     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)
     d  negAmount      s             15  2 inz(1)

     c                   eval      negAmount = 0
     c     1             do        30            x
     c     x             occur     cYrMODS
     c                   eval      negAmount = 0
     c     1             do        99            y
     c                   if        cYrMods.ctyAmByDt(y) < 0
     c                   eval      negAmount =
     c                             negAmount + cYrMods.ctyAmByDt(y)
     c                   eval      cYrMods.ctyAmByDt(y) = 0
     c                   endif
     c                   enddo
     c                   if        negAmount < 0 and
     c                             cYrMods.ctyNegaPay < 0
     c                   if        negAmount <= cYrMods.ctyNegaPay
     c                   eval      negAmount =
     c                             negAmount - cYrMods.ctyNegaPay
     c                   eval      cYrMods.ctyNegaPay = 0
     c                   else
     c                   eval      cYrMods.ctyNegaPay =
     c                             cYrMods.ctyNegaPay - negAmount
     c                   eval      negAmount = 0
     c                   endif
     c                   endif
     c                   eval      cYrMods.ctyPastPay =
     c                             cYrMods.ctyPastPay + cYrMods.ctyNegaPay
     c     1             do        99            y
     c                   if        negAmount < 0
     c                   select
     c                   when      cYrMODS.ctyAmByDt(y)  > 0
     c                             and cYrMODS.ctyAmByDt(y) >= (negAmount * -1)
     c                   eval      cYrMODS.ctyAmByDt(y) =
     c                             cYrMODS.ctyAmByDt(y) + negAmount
     c                   eval      negAmount     = 0
     c                   when      cYrMODS.ctyAmByDt(y) > 0  and
     c                             cYrMODS.ctyAmByDt(y) < (negAmount * -1)
     c                   eval      negAmount = negAmount + cYrMODS.ctyAmByDt(y)
     c                   eval      cYrMODS.ctyAmByDt(y) = 0
     c                   endsl
     c                   endif
     c                   enddo
     c                   enddo

     p adjCYNegAmts    e
     p*===========================================================================
     p* adjNYNegAmts - wash negative amounts out of each ctyAmByDt array in nYrMODS
     p*===========================================================================
     p adjNYNegAmts    b
     d adjNYNegAmts    pi

     d  x              s              3  0 inz(1)
     d  y              s              3  0 inz(1)
     d  negAmount      s             15  2 inz(1)

     c                   eval      negAmount = 0
     c     1             do        30            x
     c     x             occur     nYrMODS
     c                   eval      negAmount = 0
     c     1             do        99            y
     c                   if        nYrMods.ctyAmByDt(y) < 0
     c                   eval      negAmount =
     c                             negAmount + nYrMods.ctyAmByDt(y)
     c                   eval      nYrMods.ctyAmByDt(y) = 0
     c                   endif
     c                   enddo

     c                   if        negAmount < 0 and
     c                             nYrMods.ctyNegaPay < 0
     c                   if        negAmount <= nYrMods.ctyNegaPay
     c                   eval      negAmount =
     c                             negAmount - nYrMods.ctyNegaPay
     c                   eval      nYrMods.ctyNegaPay = 0
     c                   else
     c                   eval      nYrMods.ctyNegaPay =
     c                             nYrMods.ctyNegaPay - negAmount
     c                   eval      negAmount = 0
     c                   endif
     c                   endif
     c                   eval      nYrMods.ctyPastPay =
     c                             nYrMods.ctyPastPay + nYrMods.ctyNegaPay

     c     1             do        99            y
     c                   if        negAmount < 0
     c                   select
     c                   when      nYrMODS.ctyAmByDt(y)  > 0
     c                             and nYrMODS.ctyAmByDt(y) >= (negAmount * -1)
     c                   eval      nYrMODS.ctyAmByDt(y) =
     c                             nYrMODS.ctyAmByDt(y) + negAmount
     c                   eval      negAmount     = 0
     c                   when      nYrMODS.ctyAmByDt(y) > 0  and
     c                             nYrMODS.ctyAmByDt(y) < (negAmount * -1)
     c                   eval      negAmount = negAmount + nYrMODS.ctyAmByDt(y)
     c                   eval      nYrMODS.ctyAmByDt(y) = 0
     c                   endsl
     c                   endif
     c                   enddo

     c                   enddo

     p adjNYNegAmts    e
     p*===========================================================================
     p* loadPYPastPay - apply prior payments to amtByDt array for each hierarchy in pYrMODS
     p*===========================================================================
     p loadPYPastPay   b
     d loadPYPastPay   pi

     d  arType         s              1
     d  arCat          s              1
     d  catType        s              2                                         cty + ARType
     d  x              s              3  0 inz(1)

     c     d7eaky        klist
     c                   kfld                    kkco#
     c                   kfld                    kkstat
     c                   kfld                    kkbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)
     c                   eval      arCat  = getARCat(M6EAARCD)
     c                   eval      arType = getARType(M6EAARCD)
     c                   select
     c* Prevent Retro credits from being accumulated
     c                   when         M6EAARCD = F1RCCD
      * Exclude Dividend Cash
     c                   when         M6EAARCD = F1dvpd or
     c                                M6EAARCD = F1dvxr
     c                   when         M6EAARCD = IFIFPD
     c                             or M6EAARCD = IFIFTR
     c                             or M6EAARCD = IFIFTC
     c                   eval      x = 1
     c     'IQ'          lookup    ctyTyp(x)                              60
     c                   if        %found
     c     x             occur     pYrMODS
     c                   eval      pYrMODS.ctyPastPay =
     c                             pYrMODS.ctyPastPay + M6EAAMT
     c                   endif
     c                   when         arType = 'I'
     c                             or arType = 'P'
     c                             or arType = 'Q'
     c                             or arType = 'R'
     c                             or arType = 'N'
     c                             or arType = 'T'
     c                             or arType = 'V'
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      x = 1
     c     catType       lookup    ctyTyp(x)                              60
     c                   if        %found
     c     x             occur     pYrMODS
     c                   if        M6EAAMT > 0
     c                   eval      pYrMODS.ctyPastPay =
     c                             pYrMODS.ctyPastPay - ARValue(M6EAAMT:arType)
     c                   else
     c                   eval      pYrMODS.ctyNegaPay =
     c                             pYrMODS.ctyNegaPay - ARValue(M6EAAMT:arType)
     c                   endif
     c                   endif
     c                   endsl
     c     eaky          reade     mea0l6
     c                   enddo

      /free
       //*** beg chg ***** 07/06/22 ***************
       //If lockBoxFlg = 'Y' or srOnliFlg = 'Y';
       If lockBoxFlg = 'Y'
        Or srOnliFlg = 'Y'
        Or isIcPayment = *On;
       //*** end chg ***** 07/06/22 ***************
          Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
          Dou %Eof;
          Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
             If %Eof;
                Leave;
             Endif;
             arCat  = getARCat(d30EAArCd);
             arType = getARType(d30EAArCd);
             If (lockBoxFlg = 'Y' and d30EAbat# <> kkBat#) or
              billType(arType) = *on;
                Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
                Iter;
             Endif;
             Select;
             When d30EAArCd = IFIfPd or d30EAArCd = IFIfTr or
              d30EAArCd = IFIfTc;
                x = %Lookup('IQ':ctyTyp);
             Other;
                catType = arCat + getType(arType:arCat);
                x = %Lookup(catType:ctyTyp);
             Endsl;
             If x <> 0;
                %Occur(pYrMods) = x;
                If d30EAamt > 0;
                   pYrMODS.ctyPastPay -= ARValue(d30EAamt:arType);
                Else;
                   pYrMODS.ctyNegaPay -= ARValue(d30EAamt:arType);
                Endif;
             Endif;
          Enddo;
       Endif;
      /end-free

     p loadPYPastPay   e
     p*===========================================================================
     p* loadCYPastPay - apply prior payments to amtByDt array for each hierarchy in cYrMODS
     p*===========================================================================
     p loadCYPastPay   b
     d loadCYPastPay   pi

     d  arType         s              1
     d  arCat          s              1
     d  catType        s              2                                         cty + ARType
     d  x              s              3  0 inz(1)

     c     d7eaky        klist
     c                   kfld                    kkco#
     c                   kfld                    kkstat
     c                   kfld                    kkbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)
     c                   eval      arCat  = getARCat(M6EAARCD)
     c                   eval      arType = getARType(M6EAARCD)
     c                   select
     c* Prevent Retro credits from being accumulated
     c                   when         M6EAARCD = F1RCCD
      * Exclude Dividend Cash
     c                   when         M6EAARCD = F1dvpd or
     c                                M6EAARCD = F1dvxr
     c                   when         M6EAARCD = newBusPaymCd or
     c                                M6EAARCD = newBusTrfrCd
     c                   when         M6EAARCD = IFIFPD
     c                             or M6EAARCD = IFIFTR
     c                             or M6EAARCD = IFIFTC
     c                   eval      x = 1
     c     'IQ'          lookup    ctyTyp(x)                              60
     c                   if        %found
     c     x             occur     cYrMODS
     c                   eval      cYrMODS.ctyPastPay =
     c                             cYrMODS.ctyPastPay + M6EAAMT
     c                   endif
     c                   when         arType = 'I'
     c                             or arType = 'P'
     c                             or arType = 'Q'
     c                             or arType = 'R'
     c                             or arType = 'N'
     c                             or arType = 'T'
     c                             or arType = 'V'
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      x = 1
     c     catType       lookup    ctyTyp(x)                              60
     c                   if        %found
     c     x             occur     cYrMODS
     c                   if        M6EAAMT > 0
     c                   eval      cYrMODS.ctyPastPay =
     c                             cYrMODS.ctyPastPay - ARValue(M6EAAMT:arType)
     c                   else
     c                   eval      cYrMODS.ctyNegaPay =
     c                             cYrMODS.ctyNegaPay - ARValue(M6EAAMT:arType)
     c                   endif
     c                   endif
     c                   endsl
     c     eaky          reade     mea0l6
     c                   enddo

      /free
       //*** beg chg ***** 07/06/22 ***************
       //If lockBoxFlg = 'Y' or srOnliFlg = 'Y';
       If lockBoxFlg = 'Y'
        Or srOnliFlg = 'Y'
        Or isIcPayment = *On;
       //*** end chg ***** 07/06/22 ***************
          Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
          Dou %Eof;
          Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
             If %Eof;
                Leave;
             Endif;
             arCat  = getARCat(d30EAArCd);
             arType = getARType(d30EAArCd);
             If (lockBoxFlg = 'Y' and d30EAbat# <> kkBat#) or
              billType(arType) = *on;
                Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
                Iter;
             Endif;
             Select;
             When d30EAArCd = IFIfPd or d30EAArCd = IFIfTr or
              d30EAArCd = IFIfTc;
                x = %Lookup('IQ':ctyTyp);
             Other;
                catType = arCat + getType(arType:arCat);
                x = %Lookup(catType:ctyTyp);
             Endsl;
             If x <> 0;
                %Occur(cYrMods) = x;
                If d30EAamt > 0;
                   cYrMODS.ctyPastPay -= ARValue(d30EAamt:arType);
                Else;
                   cYrMODS.ctyNegaPay -= ARValue(d30EAamt:arType);
                Endif;
             Endif;
          Enddo;
       Endif;
      /end-free

     p loadCYPastPay   e
     p*===========================================================================
     p* loadNYPastPay - apply prior payments to amtByDt array for each hierarchy in nYrMODS
     p*===========================================================================
     p loadNYPastPay   b
     d loadNYPastPay   pi

     d  arType         s              1
     d  arCat          s              1
     d  catType        s              2                                         cty + ARType
     d  x              s              3  0 inz(1)

     c     d7eaky        klist
     c                   kfld                    kkco#
     c                   kfld                    kkstat
     c                   kfld                    kkbat#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky          setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)
     c                   eval      arCat  = getARCat(M6EAARCD)
     c                   eval      arType = getARType(M6EAARCD)
     c                   select
     c* Prevent Retro credits from being accumulated
     c                   when         M6EAARCD = F1RCCD
      * Exclude Dividend Cash
     c                   when         M6EAARCD = F1dvpd or
     c                                M6EAARCD = F1dvxr
     c                   when         M6EAARCD = newBusPaymCd or
     c                                M6EAARCD = newBusTrfrCd
     c                   when         M6EAARCD = IFIFPD
     c                             or M6EAARCD = IFIFTR
     c                             or M6EAARCD = IFIFTC
     c                   eval      x = 1
     c     'IQ'          lookup    ctyTyp(x)                              60
     c                   if        %found
     c     x             occur     nYrMODS
     c                   eval      nYrMODS.ctyPastPay =
     c                             nYrMODS.ctyPastPay + M6EAAMT
     c                   endif
     c                   when         arType = 'I'
     c                             or arType = 'P'
     c                             or arType = 'Q'
     c                             or arType = 'R'
     c                             or arType = 'N'
     c                             or arType = 'T'
     c                             or arType = 'V'
     c                   eval      catType = arCat + getType(arType : arCat)
     c                   eval      x = 1
     c     catType       lookup    ctyTyp(x)                              60
     c                   if        %found
     c     x             occur     nYrMODS
     c                   if        M6EAAMT > 0
     c                   eval      nYrMODS.ctyPastPay =
     c                             nYrMODS.ctyPastPay - ARValue(M6EAAMT:arType)
     c                   else
     c                   eval      nYrMODS.ctyNegaPay =
     c                             nYrMODS.ctyNegaPay - ARValue(M6EAAMT:arType)
     c                   endif
     c                   endif
     c                   endsl
     c     eaky          reade     mea0l6
     c                   enddo

      /free
       //*** beg chg ***** 07/06/22 ***************
       //If lockBoxFlg = 'Y' or srOnliFlg = 'Y';
       If lockBoxFlg = 'Y'
        Or srOnliFlg = 'Y'
        Or isIcPayment = *On;
       //*** end chg ***** 07/06/22 ***************
          Setll (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
          Dou %Eof;
          Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:kkFyr) dea30;
             If %Eof;
                Leave;
             Endif;
             arCat  = getARCat(d30EAArCd);
             arType = getARType(d30EAArCd);
             If (lockBoxFlg = 'Y' and d30EAbat# <> kkBat#) or
              billType(arType) = *on;
                Reade (kkCo#:kkStat:kkFnd:kkEmp#:kkDiv:k1Fyr) dea30;
                Iter;
             Endif;
             Select;
             When d30EAArCd = IFIfPd or d30EAArCd = IFIfTr or
              d30EAArCd = IFIfTc;
                x = %Lookup('IQ':ctyTyp);
             Other;
                catType = arCat + getType(arType:arCat);
                x = %Lookup(catType:ctyTyp);
             Endsl;
             If x <> 0;
                %Occur(nYrMods) = x;
                If d30EAamt > 0;
                   nYrMODS.ctyPastPay -= ARValue(d30EAamt:arType);
                Else;
                   nYrMODS.ctyNegaPay -= ARValue(d30EAamt:arType);
                Endif;
             Endif;
          Enddo;
       Endif;
      /end-free

     p loadNYPastPay   e
     p*===========================================================================
     p* applyPYPastPay - apply prior payments to ctyAmByDt array for each hierarchy in pYrMODS
     p*===========================================================================
     p applyPYPastPay  b
     d applyPYPastPay  pi

     d   x             s              3  0 inz(1)
     d   y             s              3  0 inz(1)

     c     1             do        30            x
     c     x             occur     pYrMODS
     c     1             do        99            y
     c                   select
     c                   when      pYrMODS.ctyPastPay >= pYrMODS.ctyAmByDt(y)
     c                   eval      pYrMODS.ctyPastPay =
     c                             pYrMODS.ctyPastPay - pYrMODS.ctyAmByDt(y)
     c                   eval      pYrMODS.ctyAmByDt(y)  = 0
     c                   when      pYrMODS.ctyPastPay < pYrMODS.ctyAmByDt(y)
     c                             and pYrMODS.ctyPastPay > 0
     c                   eval      pYrMODS.ctyAmByDt(y) =
     c                             pYrMODS.ctyAmByDt(y) - pYrMODS.ctyPastPay
     c                   eval      pYrMODS.ctyPastPay    = 0
     c                   endsl
     c                   enddo
     c                   eval      pYrMODS.ctyBalance =
     c                             %xfoot(pYrMODS.ctyAmByDt)
     c                   enddo

     p applyPYPastPay  e
     p*===========================================================================
     p* applyCYPastPay - apply prior payments to ctyAmByDt array for each hierarchy in cYrMODS
     p*===========================================================================
     p applyCYPastPay  b
     d applyCYPastPay  pi

     d   x             s              3  0 inz(1)
     d   y             s              3  0 inz(1)

     c     1             do        30            x
     c     x             occur     cYrMODS
     c     1             do        99            y
     c                   select
     c                   when      cYrMODS.ctyPastPay >= cYrMODS.ctyAmByDt(y)
     c                   eval      cYrMODS.ctyPastPay =
     c                             cYrMODS.ctyPastPay - cYrMODS.ctyAmByDt(y)
     c                   eval      cYrMODS.ctyAmByDt(y)  = 0
     c                   when      cYrMODS.ctyPastPay < cYrMODS.ctyAmByDt(y)
     c                             and cYrMODS.ctyPastPay > 0
     c                   eval      cYrMODS.ctyAmByDt(y) =
     c                             cYrMODS.ctyAmByDt(y) - cYrMODS.ctyPastPay
     c                   eval      cYrMODS.ctyPastPay = 0
     c                   endsl
     c                   enddo
     c                   eval      cYrMODS.ctyBalance =
     c                             %xfoot(cYrMODS.ctyAmByDt)
     c                   enddo

     p applyCYPastPay  e
     p*===========================================================================
     p* applyNYPastPay - apply prior payments to ctyAmByDt array for each hierarchy in nYrMODS
     p*===========================================================================
     p applyNYPastPay  b
     d applyNYPastPay  pi

     d   x             s              3  0 inz(1)
     d   y             s              3  0 inz(1)

     c     1             do        30            x
     c     x             occur     nYrMODS
     c     1             do        99            y
     c                   select
     c                   when      nYrMODS.ctyPastPay >= nYrMODS.ctyAmByDt(y)
     c                   eval      nYrMODS.ctyPastPay =
     c                             nYrMODS.ctyPastPay - nYrMODS.ctyAmByDt(y)
     c                   eval      nYrMODS.ctyAmByDt(y)  = 0
     c                   when      nYrMODS.ctyPastPay < nYrMODS.ctyAmByDt(y)
     c                             and nYrMODS.ctyPastPay > 0
     c                   eval      nYrMODS.ctyAmByDt(y) =
     c                             nYrMODS.ctyAmByDt(y) - nYrMODS.ctyPastPay
     c                   eval      nYrMODS.ctyPastPay = 0
     c                   endsl
     c                   enddo
     c                   eval      nYrMODS.ctyBalance =
     c                             %xfoot(nYrMODS.ctyAmByDt)
     c                   enddo

     p applyNYPastPay  e
     p*===========================================================================
     p* applyPayment - apply the payment to category balances by date
     p*===========================================================================
     p applyPayment    b
     d applyPayment    pi
     d   prYrFlg                      1    const

     d   savePayment   s                   like(DEAAMT)
     d   saveAmount    s             +2    like(M6EAAMT)
     d   balAmt        s             +2    like(M6EAAMT)
     d   x             s              3  0 inz(1)
     d   y             s              3  0 inz(1)
     d   z             s              3  0 inz(1)
     d   newYrFl       s              1    inz('N')

     c                   eval                    savePayment = payment
     c                   callp     loadAllDts(prYrFlg)
      *
      *  Post new business transfer payment
      *
     c                   if        @@pgm = 'N'
     c                   callp     PostXPay('T':-savePayment)
     c                   endif

     c     1             do        199           x

     c                   if        allBDTS(x) = *hival
     c                   leave
     c                   endif

     c                   if        prYrFlg = *on

     c                   eval      z = 1
     c     allBDts(x)    lookup    pYrBDts(z)                             60
     c                   if        %found

     c     1             do        30            y
     c     y             occur     pYrMODS
     c                   select
     c                   when           pYrMODS.ctyAmByDt(z) <= payment
     c                             and  pYrMODS.ctyAmByDt(z) > 0
     c                             and  pYrMODS.ctyBalance   > 0
     c                             and  payment > 0
     c                   eval      balAmt = pYrMODS.ctyAmByDt(z)
     c                   callp     postAR(prYrFlg:
     c                                    payment:
     c                                    balAmt:
     c                                    pYrArCodes(y):
     c                                    getARCat(arCodes(y)):
     c                                    getARType(arCodes(y)):
     c                                    allBDts(x))
     c                   eval      pYrMODS.ctyAmByDt(z) = 0
     c                   eval      pYrMODS.ctyBalance =
     c                             %xfoot(pYrMODS.ctyAmByDt)
     c                   when           pYrMODS.ctyAmByDt(z) >  payment
     c                             and  pYrMODS.ctyAmByDt(z) > 0
     c                             and  pYrMODS.ctyBalance   > 0
     c                             and payment      > 0
     c                   eval      saveAmount =
     c                             pYrMODS.ctyAmByDt(z) - payment
     c                   eval      balAmt = pYrMODS.ctyAmByDt(z)
     c                   callp     postAR(prYrFlg:
     c                                    payment:
     c                                    balAmt:
     c                                    pYrArCodes(y):
     c                                    getARCat(arCodes(y)):
     c                                    getARType(arCodes(y)):
     c                                    pYrBDts(z))
     c                   eval      pYrMODS.ctyAmByDt(z) = saveAmount
     c                   eval      pYrMODS.ctyBalance =
     c                             %xfoot(pYrMODS.ctyAmByDt)
     c                   endsl
     c                   enddo

     c                   endif                                                  %found

     c                   endif                                                  prYrFlg

     c                   eval      z = 1
     c     allBDts(x)    lookup    cYrBDts(z)                             60
     c                   if        %found

     c     1             do        30            y
     c     y             occur     cYrMODS
     c                   select
     c                   when           cYrMODS.ctyAmByDt(z) <= payment
     c                             and  cYrMODS.ctyAmByDt(z) > 0
     c                             and  cYrMODS.ctyBalance   > 0
     c                             and  payment > 0
     c                   eval      balAmt = cYrMODS.ctyAmByDt(z)
     c                   callp     postAR(currYr :
     c                                    payment:
     c                                    balAmt:
     c                                    arCodes(y):
     c                                    getARCat(arCodes(y)):
     c                                    getARType(arCodes(y)):
     c                                    cYrBDts(z))
     c                   eval      cYrMODS.ctyAmByDt(z) = 0
     c                   eval      cYrMODS.ctyBalance =
     c                             %xfoot(cYrMODS.ctyAmByDt)
     c                   when           cYrMODS.ctyAmByDt(z) >  payment
     c                             and  cYrMODS.ctyAmByDt(z) > 0
     c                             and  cYrMODS.ctyBalance   > 0
     c                             and payment      > 0
     c                   eval      saveAmount =
     c                             cYrMODS.ctyAmByDt(z) - payment
     c                   eval      balAmt = cYrMODS.ctyAmByDt(z)
     c                   callp     postAR(currYr :
     c                                    payment:
     c                                    balAmt:
     c                                    arCodes(y):
     c                                    getARCat(arCodes(y)):
     c                                    getARType(arCodes(y)):
     c                                    allBDts(x))
     c                   eval      cYrMODS.ctyAmByDt(z) = saveAmount
     c                   eval      cYrMODS.ctyBalance =
     c                             %xfoot(cYrMODS.ctyAmByDt)
     c                   endsl
     c                   enddo

     c                   endif                                                  %found

     c                   eval      z = 1
     c     allBDts(x)    lookup    nYrBDts(z)                             60
     c                   if        %found

     c     1             do        30            y
     c     y             occur     nYrMODS
     c                   select
     c                   when           nYrMODS.ctyAmByDt(z) <= payment
     c                             and  nYrMODS.ctyAmByDt(z) > 0
     c                             and  nYrMODS.ctyBalance   > 0
     c                             and  payment > 0
     c                   eval      balAmt = nYrMODS.ctyAmByDt(z)
     c                   callp     postAR(newYrFl:
     c                                    payment:
     c                                    balAmt:
     c                                    arCodes(y):
     c                                    getARCat(arCodes(y)):
     c                                    getARType(arCodes(y)):
     c                                    nYrBDts(z))
     c                   eval      nYrMODS.ctyAmByDt(z) = 0
     c                   eval      nYrMODS.ctyBalance =
     c                             %xfoot(nYrMODS.ctyAmByDt)
     c                   when           nYrMODS.ctyAmByDt(z) >  payment
     c                             and  nYrMODS.ctyAmByDt(z) > 0
     c                             and  nYrMODS.ctyBalance   > 0
     c                             and payment      > 0
     c                   eval      saveAmount =
     c                             nYrMODS.ctyAmByDt(z) - payment
     c                   eval      balAmt = nYrMODS.ctyAmByDt(z)
     c                   callp     postAR(newYrFl:
     c                                    payment:
     c                                    balAmt:
     c                                    arCodes(y):
     c                                    getARCat(arCodes(y)):
     c                                    getARType(arCodes(y)):
     c                                    allBDts(x))
     c                   eval      nYrMODS.ctyAmByDt(z) = saveAmount
     c                   eval      nYrMODS.ctyBalance =
     c                             %xfoot(nYrMODS.ctyAmByDt)
     c                   endsl
     c                   enddo

     c                   endif                                                  %found

     c                   enddo                                                  199
      *
      * Post excess payment as a premium payment in current year (designated by @@rrn) only
      *
     c                   if            payment  > 0
     c                             and auditFlg =  'N'
     c                   callp     PostXPay('X':payment)
     c                   eval      payment = 0
     c                   endif
      *
      * Post transfer payment in current year only
      *
     c                   if            payment <> savePayment
     c                             and auditFlg <> 'N'
     c                   eval      payment = payment - savePayment
     c                   callp     PostXPay('T':payment)
     c                   endif

     p applyPayment    e
     p*===========================================================================
     p* postAR - posts payment to A/R balances
     p*===========================================================================
     p postAR          b                   export
     d postAR          pi
     d   prYrFlg                      1    const
     d   payment                           like (DEAAMT)
     d   balAmt                      +2    like(DEAAMT)
     d   arCode                            const like(DEAARCD)
     d   arCat                             const like(DEASTAT)
     d   arType                            const like(DEASTAT)
     d   svPodt                            const like(DEAPODT)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    ZEAPODT
     c                   kfld                    ZEAARCD

     c                   select
     c                   when      prYrFlg = *on
     c                   eval      k1fyr = kkfyr - 1
     c                   when      prYrFlg = *off
     c                   eval      k1fyr = kkfyr
     c                   when      prYrFlg = 'N'
     c                   eval      k1fyr = kkfyr + 1
     c                   endsl

     c                   if        balAmt > payment
     c                   eval      balAmt = payment
     c                   eval      payment = 0
     c                   else
     c                   eval      payment = payment - balAmt
     c                   endif





       Select;
       When isWritingARBatch = *On;
          WriteWDEAP(k1Fyr:balAmt:arCode:arType:arCat:svPodt);
       When isWritingLiveAR;
          WriteWMEAP(k1Fyr:balAmt:arCode:arType:arCat:svPodt);
       When isWritingWZEAP;
          WriteWZEAP(k1Fyr:balAmt:arCode:arType:arCat:svPodt);
       EndSl;

     p postAR          e
     p*===========================================================================
     p* PostXPay - Posts transfer payment
     p*===========================================================================
     p PostXPay        b                   export
     d PostXPay        pi
     d   payType                      1    const
     d   payment                           const like (DEAAMT)

     d arCode          s              2
     d arType          s              1
     d arCat           s              1

     c     bsky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkbscd

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    ZEAPODT
     c                   kfld                    ZEAARCD

       arType = 'P';
       arCat = 'P';
       Select;
       When isWritingARBatch = *On;
          arCode = C3cpcd;
          WriteWDEAP(kkFyr:payment:arCode:arType:arCat:sysDate);
       When isWritingLiveAR = *On;
          Select;
          When auditFlg = 'Y';
             arCode = F1AXPT;
          When auditFlg = 'E';
             arCode = C3CNCD;
          When payType  = 'T';
             arCode = newBusTrfrCd;
          When payType  = 'X';
             arCode = C3CNCD;
          Endsl;
          //*** Begin Chg *** 04/16/25 ********************************
          //*** WriteWMEAP(kkFyr:payment:arCode:arType:arCat:sysDate);
          If indPostCurrDate = *On Or svBatchPoDt = 0;
             WriteWMEAP(kkFyr:payment:arCode:arType:arCat:sysDate);
          Else;
             WriteWMEAP(kkFyr:payment:arCode:arType:arCat:svBatchPoDt);
          EndIf;
          //*** End   Chg *** 04/16/25 ********************************
       When isWritingWZEAP = *On;
          Select;
          When ELfaf = 'A';
             arCode = C3apcd;
          Other;
             kkBsCd = %Triml(%Editc(ELbscd:'X'));
             Chain (kkCo#:kkBsCd) pbs01;
             If BSsefl = 'S';
                arCode = C3spcd;
             Else;
                arCode = C3cpcd;
             Endif;
          EndSl;
          WriteWZEAP(kkFyr:payment:arCode:arType:arCat:sysDate);
       EndSl;

     p PostXPay        e
     p*===========================================================================
     p* fixTime - ensure that podt/dt/time values are unique for A/R Inquiry
     p*===========================================================================
     p fixTime         b                   export
     d fixTime         pi

     d count           s                   like(M6EATIME)
     d svpodt          s                   like(M6EAPODT)
     d svdt            s                   like(M6EAPODT)
     d svtime          s                   like(M6EATIME)
     d kkpodt          s                   like(M6EAPODT)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     eaky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    kkpodt

     c                   eval      count = 0
     c                   eval      kkpodt = *loval
     c     eaky1         setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)

     c                   if        count <> 0

     c                   if            M6EAPODT = svpodt
     c                             and M6EADT   = svdt
     c                             and M6EATIME = svtime
     c                   eval      M6EATIME = M6EATIME + count
     c                   eval      count = count + 1
     c                   else
     c                   eval      svpodt = M6EAPODT
     c                   eval      svdt   = M6EADT
     c                   eval      svtime = M6EATIME
     c                   eval      count = 1
     c                   endif

     c                   else
     c                   eval      svpodt = M6EAPODT
     c                   eval      svdt   = M6EADT
     c                   eval      svtime = M6EATIME
     c                   eval      count = 1
     c                   endif

     c                   update    mea0l6
     c     eaky          reade     mea0l6
     c                   enddo

     p fixTime         e
     p*===========================================================================
     p* fixBatchTime - Ensure that co/stat/bat#/chk#/ckam/dt/time values are unique
     p*===========================================================================
     p fixBatchTime    b                   export
     d fixBatchTime    pi

     d count           s                   like(D0EATIME)
     d svdt            s                   like(D0EAPODT)
     d svtime          s                   like(D0EATIME)
     d svbat#          s                   like(D0EABAT#)
     d svchk#          s                   like(D0EACHK#)
     d svckam          s                   like(D0EACKAM)

     c     d0eaky        klist
     c                   kfld                    kkco#
     c                   kfld                    kkstat
     c                   kfld                    kkbat#

     c                   eval      count = 0
     c     d0eaky        setll     dea0l
     c     d0eaky        reade     dea0l
     c                   dow       not(%eof)

     c                   if        count <> 0

     c                   if            D0EACHK# = svchk#
     c                             and D0EACKAM = svckam
     c                             and D0EADT   = svdt
     c                             and D0EATIME = svtime
     c                   eval      D0EATIME = D0EATIME + count
     c                   eval      count = count + 1
     c                   else
     c                   eval      svchk#  = D0EACHK#
     c                   eval      svckam  = D0EACKAM
     c                   eval      svdt    = D0EADT
     c                   eval      svtime  = D0EATIME
     c                   eval      count = 1
     c                   endif

     c                   else
     c                   eval      svchk#  = D0EACHK#
     c                   eval      svckam  = D0EACKAM
     c                   eval      svdt    = D0EADT
     c                   eval      svtime  = D0EATIME
     c                   eval      count = 1
     c                   endif

     c                   update    dea0l
     c     d0eaky        reade     dea0l
     c                   enddo

     p fixBatchTime    e
     p*===========================================================================
     p* getHiTime - get highest EATIME value written to WMEAP for podt/dt.
     p*===========================================================================
     p getHiTime       b
     d getHiTime       pi                  like(M6EATIME)

     d hiTime          s                   like(M6EATIME)

     c/exec sql
     c+ select max(EATIME)
     c+   into :hiTime
     c+   from WMEAP
     c+   where EACO#  = :M6EACO#  and
     c+         EAFND  = :M6EAFND  and
     c+         EAEMP# = :M6EAEMP# and
     c+         EADIV  = :M6EADIV  and
     c+         EAFYR  = :M6EAFYR  and
     c+         EAPODT = :M6EAPODT and
     c+         EADT   = :M6EADT
     c/end-exec

     c                   if        sqlstt = '00000'
     c                   return    hiTime
     c                   else
     c                   return    sysTime
     c                   endif

     p getHiTime       e
     p*===========================================================================
     p* getBillingBat# - Get billing batch# for srOnli cash dissolve
     p*===========================================================================
     p getBillingBat#  b
     d getBillingBat#  pi                  like(DEABAT#)

     d xxBat#          s                   like(DEABAT#) inz

     c                   eval      kkpodt = svbatchPoDt

     c/exec sql
     c+ select max(EABAT#) into :xxBat#
     c+   from WDEAP
     c+   where EACO#  = :KKCO#  and
     c+         EAFND  = :KKFND  and
     c+         EAEMP# = :KKEMP# and
     c+         EADIV  = :KKDIV  and
     c+         EAFYR  = :KKFYR  and
     c+         EAPODT = :KKPODT and
     c+         EASTAT = ' '     and
     c+         EAACHF = ' '     and
     c+         EAONLI = 'Y'
     c/end-exec

     c                   if        sqlstt = '00000'
     c                   return    xxBat#
     c                   else
     c                   return    0
     c                   endif

     p getBillingBat#  e
       //*** Begin Add *** 12/01/22 ***********************************
       //------------------------------
       Dcl-Proc getInvoice#;
       //------------------------------
        Dcl-Pi *N Packed(10:0);
        //*** begin chg ***  08/23/23  *************************************
        //***** inCompany Packed(3:0);
        //***** inGroup   Packed(3:0);
        //***** inEmp#    Packed(9:0);
        //***** inDiv     Packed(5:0);
        //***** inPoDt    Packed(7:0);
         inCompany Packed(3:0) const;
         inGroup   Packed(3:0) const;
         inEmp#    Packed(9:0) const;
         inDiv     Packed(5:0) const;
         inPoDt    Packed(7:0) const;
        //*** begin chg ***  08/23/23  *************************************
        End-Pi;

        decCompany = inCompany;
        decGroup   = inGroup;
        decEmp#    = inEmp#;
        decDiv     = inDiv;
        decPoDt    = inPoDt;

        Exec Sql
           Select EAninv Into :decInvoice#
           From WMEAP
           Where EAco# = :decCompany And EAfnd = :decGroup And
                 EAemp# = :decEmp# And EAdiv = :decDiv And
        //*** begin chg ***  08/23/23  *************************************
        //****** EApoDt = :decPoDt;
                 EApoDt = :decPoDt
                 Limit 1;
        //*** end chg *****  08/23/23  *************************************

        If decInvoice# <> 0;
           Return decInvoice#;
        Else;
           Return 0;
        Endif;

       End-Proc;
       //*** End   Add *** 12/01/22 ***********************************
      //*** begin add ***  08/24/23  *************************************
       //------------------------------
       Dcl-Proc ChkInvoice#;
       //------------------------------
        Dcl-Pi *N Ind;
         inCompany Packed(3:0) const;
         inGroup   Packed(3:0) const;
         inEmp#    Packed(9:0) const;
         inDiv     Packed(5:0) const;
         inInv#    Packed(10:0) const;
         inCat     Char(1) const;
         inType    Char(1) const;
        End-Pi;
        Dcl-S intCount Packed(10:0);
        Dcl-S chrArType char(1);

        decCompany = inCompany;
        decGroup   = inGroup;
        decEmp#    = inEmp#;
        decDiv     = inDiv;
        decInvoice# = inInv#;
        //* Find billing type for the category passed in.
        Select;
        When inCat = 'E' or inCat = 'C';
           chrArType = 'B';
        When inCat = 'D' and inType = 'I';
           chrArType = 'H';
        Other;
           chrArType = 'F';
        Endsl;

        //* Find number of a/r records for posting date, category and a/r type.
        Exec Sql
           Select count(*) Into :intCount
           From WMEAP
           Where EAco# = :decCompany And EAfnd = :decGroup And
                 EAemp# = :decEmp# And EAdiv = :decDiv And
                 EANinv = :decInvoice# And EACtyp = :inCat And
                 EAArty = :chrArType;
        //* Return false if no records found.
        If intCount = 0;
           Return *off;
        Else;
           Return *on;
        Endif;

       End-Proc;
      //*** end add *****  08/24/23  *************************************
     p*===========================================================================
     p* getNegAmount - get negative amounts from A/R
     p*===========================================================================
     p getNegAmount    b
     d getNegAmount    pi            +2    like(M6EAAMT)
     d   postDt                            const like(DEAPODT)
     d   iDts                              like(DEAPODT) dim (99) ascend
     d   arCat                        1    const

     d   xxCat         s              1
     d   negAmount     s             +2    like(M6EAAMT)  inz(0)
     d   x             s              3  0 inz(1)
     d   nextPostDt    s                   like(DEAPODT)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr

     c     eaky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    k1fyr
     c                   kfld                    postDt

     c     postDt        lookup    iDts(x)                            60
     c                   if        %found
     c                   eval      nextPostDt = iDts(x)
     c                   else
     c                   eval      nextPostDt = *hival
     c                   endif

     c     eaky1         setgt     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)

     c                   eval      xxCat  = GetARCat(M6EAARCD)
     c                   select

     c                   when      arCat = 'P' and xxCat = 'P'
     c                   if        (M6EAARTY = 'A'  or
     c                              M6EAARTY = 'B'  or
     c                              M6EAARTY = 'D') and
     c                             M6EAAMT < 0      and
     c                             M6EAPODT < nextPostDt
     c                   sub       M6EAAMT       negAmount
     c                   endif

     c                   if        M6EAPODT >= nextPostDt
     c                   leave
     c                   endif

     c                   when      arCat = 'T' and xxCat = 'T'
     c                   if        M6EAARTY = 'F' and
     c                             M6EAAMT < 0    and
     c                             M6EAPODT < nextPostDt
     c                   sub       M6EAAMT       negAmount
     c                   endif

     c                   if        M6EAPODT >= nextPostDt
     c                   leave
     c                   endif

     c                   endsl

     c     eaky          reade     mea0l6
     c                   enddo

     c                   return    negAmount

     p getNegAmount    e
       //------------------------------
       Dcl-Proc isExcFromIC;
       //------------------------------
        Dcl-Pi *N Ind;
         inArCode Char(2);
        End-Pi;
        Dcl-S chrArCode char(10);
        If isICPayment = *Off;
           Return *Off;
        Endif;
        chrArCode = inArCode;
        Chain (kkCo#:chrArCode) ptt01;
        Return TTexic = 'Y';
       End-Proc;
       //------------------------------
       Dcl-Proc Get3rdPartyBatch;
       //------------------------------
        Dcl-Pi *N Packed(9:0);
        End-Pi;
        Dcl-S intBat# Packed(9:0);
        Exec Sql
         Select EAbat#
         Into :intBat#
         From WDEAP
         Where EAachf = 'T'
          And EAtfap <> 'Y'
          And EAidt = :sysDate;
         If intBat# = 0;
            Exec Sql
             Select Max(EAbat#) + 1
             Into :intBat#
             From WDEAP;
         Endif;
         Return intBat#;
       End-Proc;
       //------------------------------
       Dcl-Proc UpdateBatchTotals;
       //------------------------------
        // Update Batch amount
        //*** Beg Chg ***** 10/15/21 **************************
        //Exec Sql
        //Update WDEAP
        //Set EAbata =
        // (Select Sum(EAamt)
        // From WDEAP
        // Where EAbat# = :kkBat#),
        //    EAckAm = EAamt, EAchk# = EAinv#
        //Where EAbat# = :kkBat#;
        Exec Sql
        Update WDEAP
        Set EAbata =
         (Select Sum(EAamt)
         From WDEAP
         Where EAbat# = :kkBat#)
        Where EAbat# = :kkBat#;
        //*** End Chg ***** 10/15/21 **************************

       End-Proc;
       //------------------------------
       Dcl-Proc WriteWDEAP;
       //------------------------------
        Dcl-Pi *N;
         inFyr Packed(3:0);
         inAmt Packed(11:2) Const;
         inArCode Char(2) Const;
         inArType Char(1) Const;
         inArCat Char(1) Const;
         inPodt Packed(7:0) Const;
        End-Pi;
        Clear dea0p;
        DEACO#  = kkco#;
        DEABAT# = kkbat#;
        DEAFND  = kkfnd;
        DEAEMP# = kkemp#;
        DEADIV  = kkdiv;
        DEAFYR  = inFyr;
        DEAARCD = inArCode;
        DEACTYP = inArCat;
        DEADESC = GetArDesc(DEAARCD);
        DEAAMT  = inAmt;
        DEAInv# = svInv#;
        //*** Begin Add *** 12/01/22 **********************************
        //*** begin del ***  08/23/23  ********************************
        //*****If DEAInv# = 0;
        //*****   DEAInv# = GetInvoice#(DEAco#:DEAfnd:DEAemp#:DEAdiv:DEApoDt);
        //*****EndIf;
        //*** end del *****  08/23/23  ********************************
        //*** End   Add *** 12/01/22 **********************************
        //*** Beg Add ***** 06/08/22 **********************************
        DEAInvTypID = svInvTypID;
        //*** End Add ***** 06/08/22 **********************************
        //*** Beg Add ***** 06/29/22 **********************************
        DEARef# = svRef# ;
        //*** End Add ***** 06/29/22 **********************************
        //*** Beg Add ***** 10/15/21 **********************************
        DEAChk# = svChk#;
        DEACash = 'D';
        //*** End Add ***** 10/15/21 **********************************
        Select;
        //*** Begin Add *** 11/29/22 **********************************
        When svPoDtIVC <> 0 And svPgm = 'B';
           DEApoDt  = svPoDtIVC;
        //*** End   Add *** 11/29/22 **********************************
        When clientID = '129';
           DEAPODT  = svBatchPoDt;
           DEATRDT  = svBatchTrDt;
        Other;
           If indPostCurrDate = *On;
              DEAPODT  = sysDate;
           Else;
              DEAPODT  = inPodt;
           EndIf;
           DEATRDT = sysDate;
        EndSl;
        //*** begin add ***  08/23/23  ********************************
        If DEAInv# = 0;
           DEAInv# = GetInvoice#(DEAco#:DEAfnd:DEAemp#:DEAdiv:DEApoDt);
        EndIf;
        //*** end add *****  08/23/23  ********************************
        //*** begin add ***  08/24/23  ********************************
        //* Find invoice # associated with original billing posting date if
        //* no billing records written to current invoice # for non-premium rcds.
        If DEACTYP <> 'P';
           If not ChkInvoice#(DEAco#:DEAfnd:DEAemp#:DEAdiv:DEAInv#:DEActyp:
            inARType);
              DEAInv# = GetInvoice#(DEAco#:DEAfnd:DEAemp#:DEAdiv:inPodt);
           Endif;
        Endif;
        //*** end add *****  08/24/23  ********************************
        DEACKAM = svBatchCkam;
        DEASYCD = svBatchSycd;
        If DEAsyCd = ' ';
          //*** Begin Chg *** 04/16/25 *************************************
          //*** Exec Sql
          //***   Select SYkey
            //*** Beg Add *** 02/19/25 *************************************
            //***Into :DEAsyCd
          //***  :strSyCd
            //*** End Add *** 02/19/25 *************************************
          //***   From WTSYP
          //***   Where SYstat <> 'I'
          //***    And SYtype = 'Y'
          //***   Order by SYkey
          //***   Limit 1;
          //***   DEAsyCd = strSyCd;
          Exec Sql
            Select SYkey
            Into :svBatchSycd2
            From WTSYP
            Where SYstat <> 'I'
             And SYtype = 'Y'
            Order by SYkey
            Limit 1;
            DEASYCD = svBatchSycd2;
          //*** End   Chg *** 04/16/25 *************************************
        EndIf;
        DEARDIV = svRDiv;
        DEATFAP = 'N';
        DEATFFL = 'N';
        DEAIPGM = Q1PGM;
        DEAIUSR = Q1USER;
        DEAIDT  = sysDate;
        DEAPGM  = Q1PGM;
        DEAUSER = Q1USER;
        DEADT   = sysDate;
        sysTime  = sysTime + 1;
        DEATIME = sysTime;
        Select;
        When isIcPayment;
           DEACASH = *Blank;
           DEAachf = 'T';
        When srOnliFlg = 'Y';
           DEAachf = svAchF;
          //*** beg add ***** 04/05/22 *************
           DEAonli = 'Y';
          //*** end add ***** 04/05/22 *************
        When lockBoxFlg = 'Y';
           Eval DEAachf = 'L';
        EndSl;
        Write dea0p;
        // Update the batch amount
        UpdateBatchTotals();
       End-Proc;
       //------------------------------
       Dcl-Proc WriteWMEAP;
       //------------------------------
        Dcl-Pi *N;
         inFyr Packed(3:0);
         inAmt Packed(11:2) Const;
         inArCode Char(2) Const;
         inArType Char(1) Const;
         inArCat Char(1) Const;
         inPodt Packed(7:0) Const;
        End-Pi;
        Clear mea0l6;
        M6EACO#  = kkco#;
        M6EAFND  = kkfnd;
        M6EAEMP# = kkemp#;
        M6EADIV  = kkdiv;
        M6EARDIV = kkdiv;
        M6EAFYR  = inFyr;
        M6EATRDT = sysDate;
        M6EANINV = svInv#;
        //*** Begin Add *** 12/01/22 **********************************
        //*** begin del ***  08/23/23  ********************************
        //*****If M6EAninv = 0;
        //*****   M6EAninv = GetInvoice#(M6EAco#:M6EAfnd:M6EAemp#:M6EAdiv:M6EApoDt);
        //*****EndIf;
        //*** end del *****  08/23/23  ********************************
        //*** End   Add *** 12/01/22 **********************************
        If indPostCurrDate = *On;
           M6EAPODT = sysDate;
        Else;
           M6EAPODT = inPodt;
        EndIf;
        M6EAAMT  = inAmt;
        M6EAARCD = inArCode;
        M6EACTYP = inArCat;
        M6EAARTY = inArType;
        M6EADESC = GetArDesc(M6EAARCD);
        M6EAPGM  = Q1PGM;
        M6EAUSER = Q1USER;
        M6EADT   = sysDate;
        M6EATIME = getHiTime + 1;
        //*** Beg Add ***** 06/29/22 **********************************
        M6EARef# = svRef# ;
        //*** End Add ***** 06/29/22 **********************************
        //*** Begin Add *** 11/29/22 **********************************
        If svPoDtIVC <> 0 And svPgm = 'B';
           M6EApoDt   = svPoDtIVC;
        EndIf;
        //*** End   Add *** 11/29/22 **********************************
        //*** begin add ***  08/23/23  ********************************
        If M6EAninv = 0;
           M6EAninv = GetInvoice#(M6EAco#:M6EAfnd:M6EAemp#:M6EAdiv:M6EApoDt);
        EndIf;
        //*** end add *****  08/23/23  ********************************
        //*** begin add ***  08/24/23  ********************************
        //* Find invoice # associated with original billing posting date if
        //* no billing records written to current invoice # for non-premium rcds.
        If M6EACTYP <> 'P';
           If not ChkInvoice#(M6EAco#:M6EAfnd:M6EAemp#:M6EAdiv:M6EANinv:
            M6EActyp:inARType);
              M6EAninv = GetInvoice#(M6EAco#:M6EAfnd:M6EAemp#:M6EAdiv:inPodt);
           Endif;
        Endif;
        //*** end add *****  08/24/23  ********************************
        Write mea0l6;
       End-Proc;
       //------------------------------
       Dcl-Proc WriteWZEAP;
       //------------------------------
        Dcl-Pi *N;
         inFyr Packed(3:0);
         inAmt Packed(11:2) Const;
         inArCode Char(2) Const;
         inArType Char(1) Const;
         inArCat Char(1) Const;
         inPodt Packed(7:0) Const;
        End-Pi;
        Clear zea0l;
        ZEACO#  = kkco#;
        ZEAFND  = kkfnd;
        ZEAEMP# = kkemp#;
        ZEADIV  = kkdiv;
        ZEAFYR  = inFyr;
        ZEAARCD = inArCode;
        ZEACTYP = inArCat;
        ZEAARTY = inArType;
        ZEADESC = GetArDesc(ZEAARCD);
        If indPostCurrDate = *On;
           ZEAPODT = sysDate;
        Else;
           ZEAPODT = inPodt;
        EndIf;
        Chain (ZEAco#:ZEAfnd:ZEAemp#:ZEAdiv:
         ZEAfyr:ZEApodt:ZEAarcd) zea0l;
        If %Found;
           ZEAAMT = ZEAAMT + inAmt;
           Update zea0l;
        Else;
           ZEAAMT  = inAmt;
           Write zea0l;
        EndIf;
       End-Proc;
** AXCAT   Audit Transfer Categories used for lookup
REGLMDST12345P67                                                                05/03/09
