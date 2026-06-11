     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLARINQ - Policy A/R Inquiry
     H DFTNAME(WTPOLARINQ)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 03/07/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 09/04/25 C:062 Project: 153WBE(119572)                        *
      *                Fix inv type that displays check.              *
      *                                                               *
      * 05/13/25 C:333 Support: 118669 (FCC)                          *
      *                Return Funds Screen Request.                   *
      *                DSPF Changes:                                  *
      *                - Added Cancel button to rcdPanel.             *
      *                - Added Employer# field to all record format   *
      *                  having SFL with visibility ind visRFARInq.   *
      *                - Added Find/Filter options for grids.         *
      *                                                               *
      * 09/23/24 C:342 Support: 117518 (UBIC)                         *
      *                Do not process MeetFlt when @@next= PostAmtDt  *
      *                bcoz this is not loading future Installments in*
      *                A/R transaction detail grid.                   *
      *                                                               *
      * 04/18/24 C:342 Project: WC2906 Support : 115312               *
      *                Show Fnd/Emp#/Div when called from WTCXBTCH    *
      *                or WTARBTCH on Transaction detail screen and   *
      *                Summary All Years screen.                      *
      *                Changes added to read subfile sflArSumm and    *
      *                sflTrnDtl in do while loop.                    *
      *                DSPF Changes:                                  *
      *                1)Added scEmpNo field to display emp info      *
      *                  on rcdAllYear and rcdTrDtl screen.           *
      *                                                               *
      * 05/24/23 C:342 Support: 129/108862                            *
      *                Display file changes only: added "Batch Type"  *
      *                field on Transaction Detail screen.            *
      *                                                               *
      * 10/17/23 C:344 Support: 150/114344 (SUNZ)                     *
      *                Increased the size of the amount and total     *
      *                fields within the "Summary - All Years" tab    *
      *                so that large amounts can be stored.           *
      *                                                               *
      * 09/06/23 C:300 Project: WC2906                                *
      *                Show "Summary - All Years" tab when called from*
      *                WTCXBTCH (new Cancellation Batches Screen)     *
      *                when passed @@next = 'AmountDue'               *
      *                DSPF Changes:                                  *
      *                1)Changes visibility indicator for Simple      *
      *                  container from visbSelCon to visbSelAll on   *
      *                  All Year Summary Tab                         *
      *                                                               *
      * 07/03/23 C:342 Project: WC2906                                *
      *                1.Show Trsanction Details screen when call from*
      *                WTCXBTCH (new Cancellation Batches Screen)     *
      *                when passed @@next = 'ARInq'                   *
      *                DSPF Changes:                                  *
      *                1)Changes visibility indicator for select      *
      *                  button from vsbPnl to visbSelCon on All year *
      *                  Summary Tab and Transaction Detail Tab.      *
      *                2)Changes visibility indicator for Simple      *
      *                  container from vsbPnl to visbSelCon on All   *
      *                  Year Summary Tab and Transaction Detail Tab. *
      *                3)Changes visibility indicator for Cancel      *
      *                  button from vsbPnl to visbCancel on All      *
      *                  Year Summary Tab and Transaction Detail Tab. *
      *                                                               *
      * 02/21/23 C:300 Project: WC2675                                *
      *                Changes for Policy Year and Posting Amount search
      *                from new A/R Batches screen                    *
      *                DSPF Changes:                                  *
      *                1)Added Panel, Select Year, Cancel button,     *
      *                  ConARSumm Container and Show as Window       *
      *                  on rcdAllYear - AR Summary Grid              *
      *                2)Added Panel, Select Posting Amount, Cancel   *
      *                  button, ConTrnDtl container and Show as Window
      *                  on rcdTrDtl - A/R Transaction Detail Grid    *
      *                                                               *
      * 06/29/22 C:340 Project: WC2854                                *
      *                Invoice Cloud:  Retain Payment Ref#            *
      *                                                               *
      * 04/11/22 C:340 Project: WC2663                                *
      *                Invoice Cloud - Add Invoice Type ID            *
      *                                                               *
      * 01/04/22 C:079 Support: 150/100236                            *
      *                Display file changes only: Increase size of    *
      *                Total Premium Due on Summary by Year Screen    *
      *                                                               *
      * 07/30/21 C:079 Project: WC2663                                *
      *                Add invoice # to Summary Tab                   *
      *                                                               *
      * 06/01/21 C:062 Project: WC2663                                *
      *                Add invoice # to grid, detail box and filter   *
      *                                                               *
      * 04/29/21 C:300 Support: 95712 & 94996 (SUNZ)                  *
      *                Display file changes only: Increase size of    *
      *                field scTcAmount, scCcBilled, scCcPaid, scCcBalnc
      *                to 11,2 on Summary by A/R Code (tab 4)         *
      *                                                               *
      * 04/28/21 C:180 Support: 95712 (SUNZ)                          *
      *                Display file changes only: Increase size of    *
      *                field SCdepdue to 10,2                         *
      *                                                               *
      * 12/30/20 C:062 Support: 93718(150)                            *
      *                Expand amount fields.                          *
      *                                                               *
      * 08/18/20 C:300 Support: 90794 (SUNZ)                          *
      *                A/R FIlter limits SR division selection to 500 *
      *                -- Changes only in Display File                *
      *                Self-Reporting Division field on A/R Iquiry Filter
      *                Max Choices property set to 100000             *
      *                                                               *
      * 04/10/19 C:300 Support: 78443 (SUNZ)                          *
      *                A/R transaction summary tab - if you hit enter or
      *                refresh or Click on a row, the figures all double.
      *                                                               *
      * 12/10/18 C:180 Support: 75290                                 *
      *                Fix loading of summary by Code                 *
      *                                                               *
      * 07/02/18 C:33  Support: 71955                                 *
      *               -Only load grid(s) for displayed tabs.          *
      *               -Check for more than 9999 records in subfile.   *
      *                                                               *
      * 05/25/18 C:291 Support: 63478                                 *
      *                Fix A/R Inquiry filter to retain S/R division  *
      *                value on the screen after filter is applied.   *
      *                Display file change only to Choice value field.*
      *                                                               *
      * 03/05/18 C:291 Support: 63478                                 *
      *                Add Self-Rpt Division column on Transaction    *
      *                detail grid. Add Self-Rpt Division filter.     *
      *                                                               *
      * 02/06/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *  10/28/15 C:300 Support: 37942                                *
      *                 Added Account Current link in A/R Summary-All *
      *                 Years Tab in the detail box                   *
      *                 Also added hidden field for Account Current   *
      *                 in A/R Summary-All Years grid                 *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *

      *  08/18/15 C:279 Support: 36421                                *
      *                 Modified to use WMEAL31 logical file to sort  *
      *                 records in transaction detail grid based on   *
      *                 Posting Date/Transaction Date/Division        *
      *                                                               *
      *  08/11/15 C:279 Support: 36343                                *
      *                 Changes only in display file. Enabled program *
      *                 trigger event for Transaction Summary Grid's  *
      *                 OnRowClick event.                             *
      *                                                               *
      *  06/03/15 C:136 Project: MC396                                *
      *                 Add column for EFT flag                       *
      *                                                               *
      *    04/16/15 C:65  Project: SY251                              *
      *                   Add API call to launch new Department       *
    *                   letters.                                    *
      *                                                               *
      *    03/31/15 C:279 Support: 33035 (Phaseware)                  *
      *                   Modified to change date filed type from     *
      *                   numeric to date to make filter work.        *
      *                                                               *
      *    10/31/14 C:65  Support: WA133 86604                        *
      *                   Remove checks on plan type to allow a/r to  *
      *                   appear for account current policies.        *
      *                                                               *
      *    10/28/14 C:65  Redmine: 4685 (Base)                        *
      *                   Recompiled since division # has been added  *
      *                   to WMEAL15 for sorting purposes.            *
      *                                                               *
      *    09/09/14 C:283 Support: WA133 86604                        *
      *                   On Clear Filter click, the filter should get*
      *                   removed for the Exclude Deposits, etc.      *
      *                                                               *
      *    07/28/14 C:295 Project: 114WMD                             *
      *                   Compilation only as the changes are in      *
      *                   Action Panel                                *
      *                                                               *
      *    06/24/14 C:279 Redmine: 4319                               *
      *                   Modified to display check# on summary grid  *
      *                   and detail box.                             *
      *                                                               *
      *    06/12/14 C:300 Redmine: 4320                               *
      *                   Display Refund Premium Void transaction on  *
      *                   Transaction summary screen                  *
      *                                                               *
      *    06/11/14 C:300 Redmine: 4245                               *
      *                   Correct RF description on A/R Inquiry Screen*
      *                   It should be Refund instaed of Recovery     *
      *                                                               *
      *    05/19/14 C:180 Support: WC147 85626                        *
      *                   Increase array sizes to prevent execution   *
      *                   error when more than 1000 transactions      *
      *                                                               *
      *    04/25/14 C:283 Support: WA114 85448                        *
      *                   Use correct logical so that the Transaction *
      *                   Detail grid sort the data by Posting Date   *
      *                   and not by Division for Multi Division emp. *
      *                                                               *
      *    11/21/13 C:282 Redmine: 3310                               *
      *                   If "A/R Inq subset default to exclude       *
      *                   deposits on pol yr bal" flag is set to 'Y'  *
    *                   than Deposit should not be showing in       *
    *                   AR Inquiry screens.                         *
    *                                                               *
      *    10/07/13 C:190 Project: 25WSS                              *
      *                   - For TASB: Replace Check# column with      *
      *                     Statement # in the grids and detail box.  *
      *                   - Change the program to determine if        *
      *                     Premium or Contribution should be printed *
      *                     /displayed based on Group setup.          *
      *                                                               *
      *    09/11/13 C:291 Redmine: 1195                               *
      *                   Combine 2 tabs Summary by AR Code (tab 4)   *
      *                   and Summary by Category Code (tab 5) into   *
      *                   one tab Summary by codes (tab 4).           *
      *                                                               *
      *    09/03/13 C:189 Redmine: 3015                               *
      *                   Use include instead of ignore for action    *
      *                   Panel record formats                        *
    *                                                               *
    *    05/31/13 C:65  Support: Redmine #2482                      *
    *                   Change logical for Transaction Detail grid  *
    *                   to enable proper order for transactions.    *
    *                                                               *
    *    04/24/13 C:279 Support: Redmine #2477                      *
    *                   Enable year drop down for year selection.   *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     FWTPOLARINQCF   E             workstn extfile('WTPOLARINQ')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflArSumm:rrnArSum)
     f                                     Sfile(sflTrSumm:rrnTrSum)
     f                                     Sfile(sflTrnDtl:rrnTrDtl)
     f                                     Sfile(sflTrCode:rrnCodeTr)
     f                                     Sfile(sflcatCode:rrncodeCt)
     f                                     Sfile(sflCatDtl:rrnCatDtl)
     f                                     Sfile(sflRtnFund:rrnRtnFund)
     f                                     Sfile(sflTrnSmDt:rrnDtlTrn)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f*********** beg chg ****** 09/03/13 *************************
     f                                     Include(rcdPolPnl)
     f                                     Include(rcdQtePnl)
     f**********                           ignore(rcdClmPnl)
     f*********** End chg ****** 09/03/13 *************************


     FWMEMP     IF   E             DISK
     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWTABL     IF   E           K DISK    ignore(PRT01)
     fwma3l     If   e           k disk    rename(pa301:pa300)
     FWTAVL     IF   E           K DISK    rename(PAV01:PAV01)
     FWMEAL     IF   E           K DISK    rename(PEA01:PEA01)
     FWMEAL6    IF   E           K DISK    rename(PEA01:PEA06)
     f                                     infds(dbfds2)
     FWMEAL10   IF   E           K DISK    rename(PEA01:PEA10)
     f                                     infds(dbfds3)
     FWMEAL30   IF   E           K DISK    rename(PEA01:PEA30)
     f                                     infds(dbfds30)
     FWDELL1    IF   E           K DISK    rename(PEL01:PEL01)
     FWDELL5    IF   E           K DISK    rename(PEL01:PEL05)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     f                                     infds(dbfds)
     FWTCTL     IF   E           K DISK    rename(PCT01:PCT00)
     FWAR4L6    IF   E           K DISK    rename(PR401:PR406)
     f                                     infds(dbfds9)
     FWTGTL     IF   E           K DISK    rename(PGT01:PGT00)
      *** Beg Add *** 11/21/13 *****************************************************************
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
      *** End Add *** 11/21/13 *****************************************************************
      *** Beg Add *** 04/25/14 *****************************************************************
     FWMEAL15   IF   E           K DISK    rename(PEA01:PEA15)
     f                                     infds(dbfds15)
      *** End Add *** 04/25/14 *****************************************************************
      *** beg add *** 08/18/15 *****************************************************************
     fwmeal31   if   e           k disk    rename(pea01:pea31)
     f                                     infds(dbfds31)
      *** end add *** 08/18/15 *****************************************************************
      ******** Beg Add ******* 10/28/15 ************************************************
     FWMAHL     IF   E           K DISK    rename(PAH01:PAH00)
     FWMARL     IF   E           K DISK    rename(PAR01:PAR00)
      ******** End Add ******* 10/28/15 ************************************************

      /copy SCOPYPOLPR

     d PSDS          ESDS                  extname(SZQ1P)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds9          ds
     d  dbrrn9               397    400b 0
     d dbfds30         ds
     d  dbrrn30              397    400b 0
      *** Beg Add *** 04/25/14 *****************************************************************
     d dbfds15         ds
     d  dbrrn15              397    400b 0
      *** End Add *** 04/25/14 *****************************************************************
      *** beg add *** 08/18/15 *****************************************************************
     d dbfds31         ds
     d  dbrrn31              397    400b 0
      *** end add *** 08/18/15 *****************************************************************

     d arTrds                        38
     d arCdds                        33

     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d ##plan          s              1
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##asOfDate      s              8  0
     d****** beg add ********** 04/24/13 *********************
     d ##year          s                   like(ELfyr)
     d****** end add ********** 04/24/13 *********************

     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d clientID        s              3    inz('000')
     d process         s               n   inz(*off)
     d flgCtgy         s                   like(TTCTYP)
     d mode            s              1
     d xxKey           s                   like(AVkey)
     d xxRrn           s              9  0
     d xxSt            s              2
     d xxPostFrmDt     s              7  0
     d xxPostToDt      s              7  0
     d xxTrnsFrmDt     s              7  0
     d xxTrnsToDt      s              7  0
     d xxBalanceDt     s              7  0
     d @@mode          s              1
     d @@Next          s             10
     d ##zip           s              9  0
     d AdL1            s             40    inz
     d AdL2            s             40    inz
     d AdL3            s             40    inz

     d arrCode         s              2    dim(199)
     d arrType         s              1    dim(199)
     d arrTypeDesc     s             25    dim(199)
     d arrCatgry       s              1    dim(199)
     d arrCatgryC      s              1    dim(199)
     d arrCatgryDesc   s             25    dim(199)
     d arrCatgryDescC  s             25    dim(199)
     d*** Begin Chg *** 12/30/20 *******************************************
     d*arrAmount       s              9  2 dim(199)
     d*arrPaidAmount   s              9  2 dim(199)
     d*arrBilledAmt    s              9  2 dim(199)
     d arrAmount       s             11  2 dim(199)
     d arrPaidAmount   s             11  2 dim(199)
     d arrBilledAmt    s             11  2 dim(199)
     d*** End   Chg *** 12/30/20 *******************************************
     d arrCdDtl        s             33    dim(3000)
     d arrCdDiv        s              5  0 dim(3000)
     d arrCdPoDt       s               d   dim(3000)
     d arrCdTyp        s              1    dim(3000)
     d arrCdCat        s              1    dim(3000)
     d arrCdDtTy       s             25    dim(3000)
     d arrCdBlAmt      s             11  2 dim(3000)
     d arrCdPdAmt      s             11  2 dim(3000)
     d arrCdTypAmt     s             13  2 dim(3000)
     d arrCdDtlRRN     s              9  0 dim(3000)
     d arrTrnSum       s             38    dim(3000)
     d arrDtlDiV       s              5  0 dim(3000)
     d arrDtlPoDt      s              8  0 dim(3000)
     d arrDtlTrTy      s             25    dim(3000)
     d arrDtlBlAmt     s             11  2 dim(3000)
     d arrDtlPdAmt     s             11  2 dim(3000)
     d******** Beg Chg ******* 10/07/13 ********************************
     d******** arrDtlChk#      s              7    dim(999)
     d arrDtlChk#      s             10    dim(3000)
     d******** End Chg ******* 10/07/13 ********************************
     d*** Beg Add *** 07/30/21 *****************************************
     d arrDtlInv#      s             10  0 dim(3000)
     d*** End Add *** 07/30/21 *****************************************
     d*** Beg Add *** 06/29/22 *****************************************
     d arrDtlRef#      S             40    Dim(3000)
     d*** End Add *** 06/29/22 *****************************************
     d arrTrSumRRN     s              9  0 dim(3000)
     d arrTrSumTyp     s                   like(EAarty) dim(3000)
     d arrCodeCount    s              3  0
     d arrCatCount     s              3  0
     d trSmCount       s              5  0
     d trDtcount       s              3  0
     d xxIndex         s              3  0
     d xxIndexC        s              3  0
     d xxIndexTD       s              5  0
     d xxIndexCD       s              5  0
     d intPriorAmt     s             15  2
     d intPriorAmt1    s             15  2
     d y               s              5  0
     d x               s              3  0
     d x1              s              1  0
     d x2              s              1  0
     d typeAmount      s             11  2
     d typeAmount1     s             11  2
     d typeAmount2     s             11  2
     d typeAmount3     s             11  2
     d typeAmount4     s             11  2
     d firstLoad       s               n   inz(*off)
     d typeDesc        s             25
     d typeDesc1       s             25
     d kkType          s                   like(EAarty)
     d kkType1         s                   like(EAarty)
     d kkPoDt          s                   like(EApodt)
     d xxPoDt          s                   like(EApodt)
     d pAmount         s                   like(EAamt)
     d pAmount1        s                   like(EAamt)
     d pAmount2        s                   like(EAamt)
     d bAmount         s                   like(EAamt)
     d bAmount1        s                   like(EAamt)
     d bAmount2        s                   like(EAamt)
     d saveFyr         s                   like(EAfyr)
     d flgPaidBil      s              1
     d flgPaidBil1     s              1
     d KkRfChk#        s                   Like(R4chk#)
     d KkRfCkdt        s                   Like(R4ckdt)
     d KkRfPtNm        s                   Like(R4ptnm)
     d ssFnd           s                   Like(ELfnd)
     d ssEmp#          s                   Like(ELemp#)
     d ssDiv           s                   Like(ELdiv)
     d meetSS1         s              1
     d @r4Cust         s                   like(R4cust)
     d @ssCus1         s                   like(R4cust)
     d @ssCus2         s                   like(R4cust)
     d ssFDat7         s                   Like(R4ckdt)
     d ssTDat7         s                   Like(R4ckdt)
     d kkArSmFyr       s                   Like(EAfyr)
     d kkArSmdiv       s                   Like(EAdiv)
     d saveRtfRrn      s              9  0
     d saveDtTRrn      s              9  0
     d saveArSmRrn     s              9  0
     d u1pdsc          s             40
     d u2pdsc          s             40
     d******** Beg Add ******* 10/07/13 ********************************
     d isContribution  s               n   inz(*off)
     d******** End Add ******* 10/07/13 ********************************
      *** BEG ADD ***  11/21/13  *******************************************************
     d isEdflFlg       s              1
      *** END ADD ***  11/21/13  *******************************************************
      ******** Beg Add ******* 10/28/15 ************************************************
     d isACurFlg       s              1
     d ##fyr           s              3
      ******** End Add ******* 10/28/15 ************************************************
     d***  Begin Add  ***  07/02/18  *************************************
     d str9999Msg      s            256
     d***  End   Add  ***  07/02/18  *************************************

     d arrMadDs        ds
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d arrMad                              overlay(arrMadDs)
     d                                     like(##emn1) dim(10)

     d dsFilter        ds                  likerec(rcdPopWind)
     d dsRtnFilter     ds                  likerec(rcdRtnFltr)

     d* Proto-types
     d AR_value        pr             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

     d AR_value_Cat    pr             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

     DCurrentBal       PR            11  2
     D fromDate                       7  0 const
     D toDate                         7  0 const

     DCurBalDiv        PR            11  2
     D toDate                         7  0 const

     d Category        pr             1
     d co#                                 like(EAco#)  const
     d code                                like(EAarcd) const

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* Load Selection fields
     d LoadSelection   pr

     d* Load AR Inquiry Grid (TAB 1)
     d LoadArSumGrid   pr

     d* Load Transaction Summary Grid (TAB 2)
     d LoadTrSumGrid   pr

     d* Load Transaction Detail Grid. (TAB 3)
     d LoadTrDtlGrid   pr

     d* Load A/R Summary by Transaction Code Grid. (TAB 4)
     d LoadTrCdeGrid   pr

     d* Load A/R Summary by Category Code Grid. (TAB 5)
       //********** begin del **** 09/11/13 ****************
     d***** LoadCatCdeGrid  pr
       //********** end del ****** 09/11/13 ****************

     d* Load A/R Category Detail Grid.  (TAB 6)
     d LoadCatDtlGrid  pr

     d* Load Return Fund.  (TAB 7)
     d LoadReturnFund  pr

     d* Initialize the Filter fields
     d LoadIntFilter   pr

     d* Load Filter Clear Button.
     d LoadClrFltr     pr

     d* Load Detail grid for row selected in main Grid (TAB 2)
     d LoadDtlGrd      pr

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr
     d processMe                       n

     d MeetFilter      pr              n

     d RtnFndFilter    pr

     d* Get data to load transaction summary
     d BldTrnSum       pr

     d* Get data to load transaction detail
     d BldTrnDetail    pr

     d* Procees the selected row
     d ProcRoWClk      pr

     d SrAddr          pr                  extpgm('SRADDR')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##odr                          1
     d ##nodr                         1
     d ##aodr                         1
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d ##adrl                         3  0
     d ##naml                         3  0
     d ##fedl                         3  0
     d ##peo1                        57
     d ##peo2                        57
     d ##pgfl                         1
     d ##conl                         3  0

     dWA002R           pr                  extpgm('WA002R')
     d ##co                                like(EMCO#)  const
     d ##fnd#                              like(EMFND)  const
     d ##emp                               like(EMEMP#) const
     d ##div#                              like(EMDIV)  const
     d ##fyr                               like(ELFYR)  const
     d ##agt#                         5  0              const
     d ##agt_name                    60                 const
     d ##prod                         5  0              const
     d ##prod_nm                     60                 const
     d ##area_cd                      3  0              const
     d ##phone#                       7  0              const
     d ##plan                              like(##plan)
     d ##asOfDate                          like(##asOfDate) const

     d/copy sPrDate
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrAR
     d/copy wPrUsrPrms
     d/copy sPrString
     d/copy sPrAtrium
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************
       //*** Begin add *  09/17/15
     d/copy wPrPolCpy
       //*** End add *  09/17/15
       //***  Begin Add  ***  07/02/18  *******************************
     d/copy sprGenPop
       //***  End   Add  ***  07/02/18  *******************************
       //***  Begin Add  ***  04/11/22  *******************************
     d/copy sprModule
       //***  End   Add  ***  04/11/22  *******************************
      ******** Beg Add ******* 10/28/15 ************************************************
     C* Agency History File Key List
     C*
     C     keyWMAHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
     C*
     C* Agent Rate File Key List
     C*
     C     keyWMARL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    AHrt#
     C                   Kfld                    AHEfdt
     C*
     C     key1WMARL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    AHrt#
     C*
      ******** End Add ******* 10/28/15 ************************************************

     C* Employer Detail File Key List

     c     keyWDELL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr

     c     key1WDELL     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWDELL1     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scYear
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWDELL#     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scFyr
     c                   Kfld                    scEmp#
     c                   Kfld                    scTrDiv

     c     keyWDELL5     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    kkArSmDiv
     c                   Kfld                    kkArSmFyr

     c     key1WDELL5    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    kkArSmDiv

     c     keyWMEAL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr

     c     key1WMEAL     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWMEAL6     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   kfld                    kkFyr1            3 0
     c*** begin add ***  05/31/13  *******************************************************

     c     key1WMEAL6    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c*** end add *****  05/31/13  *******************************************************

     c     keyWMEAL10    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   kfld                    kkFyr1
     c                   Kfld                    scEmp#
     c*
     c     keyTable      klist
     c                   kfld                    ELco#
     c                   kfld                    xxKey

     c     key1Table     klist
     c                   kfld                    ELco#

     c     keyWTCTL      klist
     c                   kfld                    scCo#
     c                   kfld                    scCdCatgry
    
     c     keyWTCTL#     klist
     c                   kfld                    scCo#
     c                   kfld                    EActyp
    
     c     keyWMEAL30    klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scFyr
     c                   kfld                    scEmp#
     c                   kfld                    kkPoDt
     c                   kfld                    scTrDiv
    
     c     key1WMEAL30   klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scFyr
     c                   kfld                    scEmp#
    
     c     KeyWAR4L6     klist
     c                   kfld                    scCo#
     c                   Kfld                    KkRfPtNm
     c                   Kfld                    KkRfCkDt
     c                   Kfld                    KkRfChk#
    
     C     keyWTGTL      klist
     C                   kfld                    scCo#
     C                   kfld                    kkPgmD            4
     C                   kfld                    kkKey1           10
      *** BEG ADD ***  11/21/13  *******************************************************
     c     keyWMEXL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
      *** END ADD ***  11/21/13  *******************************************************
    
     c* Entry of Program.
     c*
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode                         mode
     C                   Parm                    @@next
     c                   Parm                    @@TabInd          5            Tab index
     c                   Parm                    @@Qte             1            Quote
      *** Beg Add *** 02/21/23 *****************************************************************
     c                   Parm                    @@PostDt          8 0          Post Dt
     c                   Parm                    @@PostAmt        11 2          Post Amt
      *** End Add *** 02/21/23 *****************************************************************

      /free

       If %parms >= 1;
         xxRrn = %int(@@rrn);
       Else;
         xxRrn = 3386;
       Endif;

       // Tab Index Mode.
       TabIndex = 0;
       If %parms > 3;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
           //********** beg chg ****** 09/11/13 ****************
           //If tabIndex < 0 or tabIndex > 6;
             If tabIndex < 0 or tabIndex > 5;
           //********** beg chg ****** 09/11/13 ****************
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       // Quote Environment
       If %parms > 4;
          quoteMode = @@Qte;
          Select;
             When @@Qte = 'Q';
                qteMode = *on;
          Endsl;
       Endif;
       scEnv = quoteMode;
       // Program Mode.
       If %parms >= 2;
         mode = @@mode;
       Endif;

       Select;
       When mode = 'D';
         dspMode = *on;
       When mode = 'A';
         addMode = *on;
       When mode = 'C';
         chgMode= *on;
       Endsl;
       scActMode = mode;

       GetClientID(ClientID);
       scActCltId=ClientID;

       ProgramStart(process);

       //***  Beg Chg  **** 07/03/23 *******************************
       //***** Beg Add ******* 02/21/23 **************************
       //***** If @@next <>'PostAmtDt' and @@next <>'PostYear';
       //***  Beg Chg  **** 09/06/23 *******************************
       //***** If @@next <>'PostAmtDt' and @@next <>'PostYear' and @@next <>'ARInq';
       //*** Beg Chg ***** 05/13/25 ******************************
       //If @@next <>'PostAmtDt' and @@next <>'PostYear' and @@next <>'ARInq'
       If @@next <>'PostAmtDt' and @@next <>'PostYear'
       //*** End Chg ***** 05/13/25 ******************************
        and @@next <> 'AmountDue';
       //***** End Add ******* 02/21/23 **************************
       //***  End Chg  **** 07/03/23 *******************************
       //***  End Chg  **** 09/06/23 *******************************
       If process;
         Main();
       Endif;
       //***** Beg Add ******* 02/21/23 **************************
       Endif;

       //***** Beg Add ******* 04/18/24 **************************
       //*** Beg Del ***** 05/13/25 ******************************
       //**scEmpNo = %editc(ELfnd:'X') + ' - ' + %editc(ELemp#:'X') +
       //**          ' - ' + %editc(ELdiv:'X');
       //*** End Del ***** 05/13/25 ******************************
       //***** End Add ******* 04/18/24 **************************
      // Load Transaction detail search screen
       If @@next = 'PostAmtDt';
          // Panel Heading
          scPnlHdg = 'Select Posting Date and Posting Amount';

          LoadTrDtlGrid();
          BldTrnDetail();
          visbPnl = *on;                                // Show Panel for Search Screen
          isInvType = *off;                            // Hide Inv type ID
          //***  Beg Add  **** 07/03/23 *******************************
          visbCancel = *on;                            // Show Cancel Button
          visbSelCon = *on;                            // Show Select Button and Hide Detail box
          PnlARHt = 310;                               // Panel Height
          //***  End Add  **** 07/03/23 *******************************
          Exfmt rcdTrDtl;

          //***** Beg Chg ******* 04/18/24 **************************
          //***ReadC sflTrnDtl;
          //***If btnPostSel = *on;
          //***   btnPostSel = *off;
          //***   @@PostDt = %dec(scTDPostDT: *usa);
          //***   @@PostAmt = scTDamount;
          //***Endif;
          //***If btnPostCan = *on;
          //***   btnPostCan = *off;
          //***Endif;
          ReadC sflTrnDtl;
          Dow not %eof;
             If btnPostSel = *on;
                btnPostSel = *off;
                @@PostDt = %dec(scTDPostDT: *usa);
                @@PostAmt = scTDamount;
             Endif;
             If btnPostCan = *on;
                btnPostCan = *off;
             Endif;
             ReadC sflTrnDtl;
          Enddo;
          //***** End Chg ******* 04/18/24 **************************
       Endif;
      // End of transaction Screen

      // Load Year summary search screen
       If @@next = 'PostYear';
          scPnlHdg = 'Select Policy Year';

         // Load Year Info
          LoadArSumGrid();
          visbPnl = *on;                                // Show Panel for Search Screen
          DspACLnk  = *off;                            // Hide Link
          //***  Beg Add  **** 07/03/23 *******************************
          visbCancel = *on;                            // Show Cancel Button
          visbSelCon = *on;                            // Show Select Button and Hide Detail box
          PnlARHt = 310;                               // Panel Height
          //***  End Add  **** 07/03/23 *******************************
          //***  Beg Add  **** 09/06/23 *******************************
          visbSelAll = *on;                            // Hide Detail box
          //***  End Add  **** 09/06/23 *******************************
          Exfmt rcdAllYear;

          //***** Beg Chg ******* 04/18/24 **************************
          //***ReadC sflArSumm;
          //***If btnYearSel = *on;
          //***   btnYearSel = *off;
          //***   @@TabInd = %char(scYear);
          //***Endif;
          //***If btnYearCan = *on;
          //***   btnYearCan = *off;
          //***Endif;
          ReadC sflArSumm;
          Dow not %eof;
             If btnYearSel = *on;
                btnYearSel = *off;
                @@TabInd = %char(scYear);
             Endif;
             If btnYearCan = *on;
                btnYearCan = *off;
             Endif;
             ReadC sflArSumm;
          Enddo;
          //***** End Chg ******* 04/18/24 **************************
       Endif;
      // End of transaction Screen
       //***** End Add ******* 02/21/23 **************************

       //***  Beg Add  **** 09/06/23 *******************************
      // Load Year summary search screen for Amount Due
       If @@next = 'AmountDue';
          scPnlHdg = 'Amount Due';

         // Load Year Info
          LoadArSumGrid();
          visbPnl = *on;                                // Show Panel for Search Screen
          DspACLnk  = *off;                            // Hide Link
          visbCancel = *on;                            // Show Cancel Button
          visbSelCon = *off;                           // Hide Select Button
          visbSelAll = *on;                            // Hide Detail box
          PnlARHt = 310;                               // Panel Height
          Exfmt rcdAllYear;

          If btnYearCan = *on;
             btnYearCan = *off;
          Endif;
       Endif;
      // End of Amount Due
       //***** End Add ******* 09/06/23 **************************

       //***  Beg Add  **** 07/03/23 *******************************
      // Load A/R Inquiry Screen
       //*** Beg Del ***** 05/13/25 ********************************
       //***If @@next = 'ARInq';
             // Panel Heading
       //***   scPnlHdg = 'A/R Transaction Details';
       //***   PnlARHt = 650;                            // Panel Height
       //***   LoadTrDtlGrid();
       //***   BldTrnDetail();
       //***   visbPnl = *on;                             // Show Panel for Search Screen
       //***   visbSelCon = *off;                         // Hide Select Button and Show Detail box
       //***   visbCancel = *on;                          // Show Cancel Button
       //***   Exfmt rcdTrDtl;

       //***   If btnPostCan = *on;
       //***      btnPostCan = *off;
       //***   Endif;
       //***Endif;
       //*** End Del ***** 05/13/25 ********************************
      // End of A/R Inquiry Screen
       //***  Beg Add  **** 07/03/23 *******************************

       *InLr = *On;
       Return;

      /end-free

       //==================================================================
        //AR_value - Determine if A/R amount is positive or negative
       //==================================================================
     P AR_value        b
    
     d AR_value        PI             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

     d amount          s              9  2

      /free

       Select;
       When ar_Type = 'P' or ar_Type = 'Q' or
         ar_Type = 'I' or ar_Type = 'T';
         amount = ar_Amount * -1;
         If %parms >=3;
           outPaidBil = 'P';
         Endif;
       //********* beg add ******** 06/24/14 *****************************************
       When ar_Type = 'R';
         amount = ar_Amount;
         If %parms >=3;
           outPaidBil = 'P';
         Endif;
       //********* end add ******** 06/24/14 *****************************************
       Other;
         amount = ar_Amount;
         If %parms >=3;
           outPaidBil = 'B';
         Endif;
       Endsl;

       Return amount;

      /end-free
     P AR_value        e

       //=======================================================================
        //AR_value_Cat - Determine if A/R amount is +ve or -ve for Category code
       //=======================================================================
     P AR_value_Cat    b
    
     d AR_value_Cat    PI             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

     d amount          s              9  2

      /free

       Select;
       When ar_Type = 'P' or ar_Type = 'Q' or
         ar_Type = 'I' or ar_Type = 'T';
         amount = ar_Amount * -1;
         If %parms >=3;
           outPaidBil = 'P';
         Endif;
       When ar_Type = 'R' or ar_Type = 'N' or
         ar_Type = 'V';
         amount = ar_Amount;
         If %parms >=3;
           outPaidBil = 'P';
         Endif;
       Other;
         amount = ar_Amount;
         If %parms >=3;
           outPaidBil = 'B';
         Endif;
       Endsl;

       Return amount;

      /end-free
     P AR_value_Cat    e

    P*====================================================================
    P* currentBal - Calculate the current balance
    P*====================================================================
    P*
     PcurrentBal       b
    P*
     DcurrentBal       PI            11  2
     D fromDate                       7  0 const
     D toDate                         7  0 const

     D balance         s             11  2

     C     eaky          klist
     C                   kfld                    scco#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scfyr
    C*
     C     eaky1         klist
     C                   kfld                    scco#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scfyr
     C                   kfld                    fromDate

     C                   clear                   balance
     C*
     C     eaky1         setll     pea06
     C     eaky          reade     pea06                                  60
     C*
     C                   dow       not *in60 and EAPODT <= toDate
     C*
     C                   eval      balance = balance + AR_value(EAAMT:EAARTY)
     C*
     C     eaky          reade     pea06                                  60
     C*
     C                   enddo

     C                   return    balance

     PcurrentBal       e


    P*====================================================================
    P* CurBalDiv - Calculate the current balance for selected division
    P*====================================================================
     PCurBalDiv        b
     DCurBalDiv        PI            11  2
     D toDate                         7  0 const

     D curBalance      s             11  2

     C     eaky          klist
     C                   kfld                    scco#
     C                   kfld                    scfnd
     C                   kfld                    scfyr
     C                   kfld                    scemp#

     C     eaky1         klist
     C                   kfld                    scco#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scfyr
      /free
       clear curBalance;

       If scVarDiv <> ' ';
          Setll eaky1 PEA06;
          Reade eaky1 PEA06;
       Else;
          Setll eaky PEA10;
          Reade eaky PEA10;
       Endif;
       Dow not %eof;
          //***** Beg Add ******* 04/24/13 **************************
          ##plan = '*';
          ##asOfDate = DateTo8(SystemDate);
          WA002R(scco#:scfnd:scemp#:EAdiv:scfyr:0:' ':0:' ':0:0:
           ##plan:##asOfDate);
          //***** End Add ******* 04/24/13 **************************
          //***** Beg Chg ******* 04/24/13 **************************
          // If EAPODT <= toDate;
          //*** begin chg ***  10/31/14  ****************************
          //*****If EAPODT <= toDate and ##plan <> 'A';
          If EAPODT <= toDate;
          //*** end chg *****  10/31/14  ****************************
          //***** Beg Chg ******* 04/24/13 **************************
             curBalance = curBalance + AR_value(EAAMT:EAARTY);
          Endif;
          If scVarDiv <> ' ';
             Reade eaky1 PEA06;
          Else;
             Reade eaky PEA10;
          Endif;
       Enddo;

       return curBalance;
      /end-free
     PCurBalDiv        e
       //==================================================================
        //Category - Get the category for the A/R code
       //==================================================================
     P Category        b
    
     d Category        PI             1
     d co#                                 like(EAco#)  const
     d code                                like(EAarcd) const

     d flgCtgy         s                   like(TTctyp)

      /free

       clear TTctyp;
       xxKey = code;
       Chain keyTable ptt01;
       flgCtgy = TTctyp;

       Return flgCtgy;

      /end-free
     P Category        e

       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI

     DfyrLine          s             31
     DdivLine          s             49
     Dfirst            s               n   inz(*on)
      /free

       xxSt = RatingSt(ELco#:ELrtcd);

       Clear fyrChoices;
       fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
        + ' AND ELdiv = ' + %char(kkDiv);

       Clear divChoices;
       divChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);


       //***** Beg Add ******* 03/05/18 **************************
       Clear SdivChoice;
       SdivChoice = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);
       //***** End Add ******* 03/05/18 **************************

      /end-free
     P LoadSelection   E
       //===================================================================
       // Load AR Summary Grid.
       //===================================================================
     P LoadARSumGrid   B
     D LoadARSumGrid   PI
      /free

       // Clear the subfile.
       clrArInq = *On;
       dspArSumSb = *Off;
       Write rcdAllYear;
       clrArInq = *Off;
       dspArSumSb = *On;
       rrnArSum = 0;

       Setll key1WDELL5 PEL05;
       Reade key1WDELL5 PEL05;

       Clear scPrDuTot;
       Clear scExpCnTot;
       Clear scDpDueTot;
       Clear scFeDueTot;
       Clear scTaxDuTot;
       Clear scAsmntTot;
       Clear scTotTot;
       Clear scTotDepPd;
       Clear scTotDepBi;

       DoW not %eof;

         Clear scYear;
         Clear scPremDue;
         Clear scExpConst;
         Clear scDepDue;
         Clear scFeeDue;
         Clear scTaxDue;
         Clear scAssmnts;
         Clear scTotal;


         // Populate the subfile
         kkFyr1 = ELfyr;
         If scVarDiv <> ' ';
           Setll keyWMEAL6 PEA06;
           Reade keyWMEAL6 PEA06;
         Else;
           Setll keyWMEAL10 PEA10;
           Reade keyWMEAL10 PEA10;
         Endif;

         //***** Beg Add ******* 02/21/23 **************************
         If @@next ='PostYear';
            //***** Beg Chg ******* 07/03/23 **************************
            //***** visbPnl = *on;
            visbSelCon = *on;
            //***** End Chg ******* 07/03/23 **************************
         Endif;
         //***** End Add ******* 02/21/23 **************************
         scYear =  kkFyr1;
         DoW not %eof;

           //***** Beg Chg ******* 04/24/13 **************************
           // ##plan = '*';
           // ##asOfDate = DateTo8(SystemDate);
           // WA002R(scco#:scfnd:scemp#:scdiv:scfyr:0:' ':0:' ':0:0:
           //  ##plan:##asOfDate);

           // If MeetFilter = *on and ##plan <> 'A';

           ##year = scYear;
           If MeetFilter = *on;
           //***** End Chg ******* 04/24/13 **************************
             Select;
             When scVarDiv= ' ';
               scArSumRRN =  dbrrn3;
             Other;
               scArSumRRN =  dbrrn2;
             Endsl;

             flgCtgy= category(EAco#:EAarcd);

             If flgCtgy= 'D';
                Select;
                When EAarty = 'P' or EAarty = 'I' or EAarty = 'Q';
                   scTotDepPd = scTotDepPd + EAamt;
                Other;
                   scTotDepBi = scTotDepBi + EAamt;
                Endsl;
             Endif;

             If scVardiv = ' ';
                scGr1Div# = '*All';
                scDivAlign = 'Left';
             Else;
                scGr1Div# = %editc(scDiv:'X');
                scDivAlign = 'Right';
             Endif;

             Select;

             When flgCtgy = 'P';
               scPremDue = scPremDue + AR_value(EAamt:EAarty);

             When flgCtgy = 'E';
               scExpConst = scExpConst + AR_value(EAamt:EAarty);

             When flgCtgy = 'D';
               scDepDue  = scDepDue + AR_value(EAamt:EAarty);

             When flgCtgy = 'T';
               scTaxDue  = scTaxDue + AR_value(EAamt:EAarty);

             When flgCtgy = '1' or flgCtgy = '2' or
               flgCtgy = '3' or flgCtgy = '4' or
               flgCtgy = '5' or flgCtgy = '6' or
               flgCtgy = '7';
               scAssmnts = scAssmnts + AR_value(EAamt:EAarty);

             Other;
               scFeeDue  = scFeeDue + AR_value(EAamt:EAarty);

             Endsl;

           Endif;

           If  scVarDiv <> ' ';
             Reade keyWMEAL6 PEA06;
           Else;
             Reade keyWMEAL10 PEA10;
           Endif;

         Enddo;

         scTotal = scPremDue + scExpConst + scDepDue + scFeeDue +
                    scTaxDue  + scAssmnts;

         scPrDuTot = scPrDuTot +  scPremDue;
         scExpCnTot = scExpCnTot + scExpConst;
         scDpDueTot = scDpDueTot + scDepDue;
         scFeDueTot = scFeDueTot + scFeeDue;
         scTaxDuTot = scTaxDuTot + scTaxDue;
         scAsmntTot = scAsmntTot + scAssmnts;
         scTotTot   = scTotTot   + scTotal;

         //***** Beg Add ******* 10/28/15 ********************************
         // Create URL for Account Cuurent (hidden field in grid)
         // Pass year from A/R Summary All Years Grid row selection

         ##fnd = %char(scfnd);
         ##emp# = %char(scemp#);
         ##div = %char(scdiv);
         ##fyr = %char(scyear);
         scACUrl = CreateURL('W0614RW':'Account Current A/R':'1':'1':'1':
          ##fnd:##emp#:##div:##fyr);
         //***** End Add ******* 10/28/15 ********************************

         rrnArSum =  rrnArSum + 1;
         If  rrnArSum = 1;
            rcdAllSmRw = 'A';
         Else;
            clear rcdAllSmRw;
         Endif;
         //*** Beg Add ***** 05/13/25 **********************************
         If @@Next = 'ARInq';
            visRFARInq = *On;
         Endif;
         //*** End Add ***** 05/13/25 **********************************
         Write sflArSumm;
         Reade key1WDELL5 PEL05;

       Enddo;
       //***** Beg Add ******* 10/07/13 ********************************
       If isContribution;
          flexhead4 = 'Year,Contribution Due,Expense Constant,Deposit Due,+
           Fee Due,Tax Due,Assessments,Total';
          flxPrmHd4 = 'Contribution';
       Else;
          flexhead4 = 'Year,Premium Due,Expense Constant,Deposit Due,+
           Fee Due,Tax Due,Assessments,Total';
          flxPrmHd4 = 'Premium';
       Endif;
       //***** End Add ******* 10/07/13 ********************************

       rrnARSum = 1;

       Chain xxRrn PEL0P;

       //***** Beg Add ******* 10/28/15 ********************************
       // Check if Account Current Link is applicable or not
       // This link will only show where the policy year selected has
       // an agency bill commission rate set.
       // The designation for the agency billing is found in
       // Group/Agent Rates where the plan type is Agency Bill.

       isACurFlg = 'N';
       DsbACLnk = *on;
       DspACLnk = *off;

       Setll keyWMAHL PAH00;
       Reade keyWMAHL PAH00;
       Dow not %eof;
          If AHSTAT = 'A';
             Setgt keyWMARL PAR00;
             Readpe key1WMARL PAR00;
             If %found() and ARplan = 'A';
                isACurFlg = 'Y';
                Leave;
             Endif;
          Endif;
          Reade keyWMAHL PAH00;
       Enddo;

       If isACurFlg = 'Y';
          DsbACLnk = *off;
          DspACLnk = *on;
       Endif;
       //***** End Add ******* 10/28/15 ********************************

      /end-free
     P LoadARSumGrid   E
       //===================================================================
       // Initilize filter values.
       //===================================================================
     P LoadIntFilter   B
     D LoadIntFilter   PI
      /free

       Clear dsFilter;
       Clear rcdPopWind;
       Clear dsRtnFilter;
       Clear rcdRtnFltr;

      /end-free
     P LoadIntFilter   E

       //===================================================================
       // Display/Hide "Clear Filter" button.
       //===================================================================
     P LoadClrFltr     B
     D LoadClrFltr     PI

      /free

       If btnClrFltr = *on;
         firstLoad = *on;
         rrnArTsRow = 0;
         LoadIntFilter();
       // *** BEG ADD ***  11/21/13  *******************************************************
       // If "A/R Inq subset default to exclude deposits on pol yr bal" flag is set to 'Y'
       // than Deposit should not be showing in AR Inquiry screens. In A/R Categories Exclude
       // list Deposit will not get clear.
       // *** BEG DEL ***  09/09/14  *******************************************************
       // User should be given the authority to remove the default filter if desired.
          //************* Chain keyWmexl pex00;
          //************* If %Found(Wmexl) and Exedfl = 'Y';
          //*************    scArCatEx = 'D';
          //*************    dsFilter.scArCatEx = scArCatEx;
          //************* EndIf;
       // *** END DEL ***  09/09/14  *******************************************************
       // *** END ADD ***  11/21/13  *******************************************************
       Endif;

       xxPostFrmDt = DtCYMD(dsFilter.scPoDt_fr);
       xxPostToDt  = DtCYMD(dsFilter.scPoDt_to);
       xxTrnsFrmDt = DtCYMD(dsFilter.scTrDt_fr);
       xxTrnsToDt  = DtCYMD(dsFilter.scTrDt_to);
       xxBalanceDt = DtCYMD(dsFilter.scBal_dt);

       //*** beg chg ***** 06/03/15 ***********************
       //*** If (xxPostFrmDt = *zero and
       //***   xxPostToDt  = *zero   and
       //***   xxTrnsFrmDt = *zero   and
       //***   xxTrnsToDt  = *zero   and
       //***   xxBalanceDt = *zero)  and
       //***   (dsFilter.scArCatIn   = *blanks  and
       //***   dsFilter.scArCatEx   = *blanks   and
       //***   dsFilter.scArCodes   = *blanks);
       //***   dspclrfltr = *off;
       //*** Else;
       //***   dspclrfltr = *on;
       //*** Endif;
       If (xxPostFrmDt = *zero and
        xxPostToDt  = *zero   and
        xxTrnsFrmDt = *zero   and
        xxTrnsToDt  = *zero   and
        xxBalanceDt = *zero)  and
        (dsFilter.scArCatIn  = *blanks  and
         dsFilter.scArCatEx  = *blanks  and
         dsFilter.scArCodes  = *blanks  and
         //***** Beg Add ******* 03/05/18 **************************
         dsFilter.scSRDiv#  = *zero    and
         //***** End Add ******* 03/05/18 **************************
         //*** Begin Add *** 06/01/21 ******************************
         dsFilter.scInv#    = *zero    and
         //*** End   Add *** 06/01/21 ******************************
         dsFilter.scChkEFT = *blank);
         dspclrfltr = *off;
       Else;
         dspclrfltr = *on;
       Endif;
       //** end chg ***** 06/03/15 ***********************

      /end-free
     P LoadClrFltr     E

       //===================================================================
     P LoadTrSumGrid   B
     D LoadTrSumGrid   PI

      /free

       // Clear the subfile.
       rrnTrSum=0;
       clrArTrns = *On;
       // clrTrnDtBx = *On;
       dspTrSumSb = *Off;
       dspTrSmDSb = *Off;
       Write rcdTrSum;
       clrArTrns = *Off;
       // clrTrnDtBx = *Off;
       dspTrSumSb = *On;
       dspTrSmDSb = *On;

       Clear scTrDiv;
       Clear scTrPodt;
       Clear scTrTraTyp;
       Clear scTrBilAmt;
       Clear scTrChk#;
       Clear scTrPdAmt;
       Clear scTrBal;
       Clear intPriorAmt1;
       //*** Beg Add *** 07/30/21 ******************************************
       Clear scTrInv#;
       //*** End Add *** 07/30/21 ******************************************


       // Populate the subfile
       y = 1;
       DoW   y <= 999;
         If ArrTrnSum(y) = ' ';
           Leave;
         Endif;

         scTrDiv    = arrDtlDiv(y);
         scTrPodt   = arrDtlPoDt(y);
         scTrTraTyp = arrDtlTrTy(y);
         scTrBilAmt = arrDtlBlAmt(y);
         scTrChk#   = arrDtlChk#(y);
         scTrPdAmt  = arrDtlPdAmt(y);
         scTrSumRRN = arrTrSumRRN(y);
         scTrSmType = arrTrSumTyp(y);
         //*** Beg Add *** 07/30/21 ********************************************
         scTrInv#   = arrDtlInv#(y);
         //*** End Add *** 07/30/21 ********************************************
         //*** Beg Add *** 06/29/22 ********************************************
         scTrChk# = RightJstfy(scTrChk#:10);
         If arrDtlRef#(y) <> *Blank;
            scTrChk# = arrDtlRef#(y);
         EndIf;
         //*** End Add *** 06/29/22 ********************************************

         scTrBal    = intPriorAmt1 +
                        arrDtlBlAmt(y) - (arrDtlPdAmt(y) * -1);
         intPriorAmt1 = scTrBal;

         // process to load detail for selected row
         If firstLoad=*On or (rrnTrSum+1 = rrnArTsRow And rrnArTsRow<>0);
            kkPoDt = dtCYMD(scTrPodt);
            firstLoad = *Off;
            clear kkPoDt;
            kkPoDt = dtCYMD(scTrPodt);
            LoadDtlGrd();
         Endif;

         y = y + 1;
         rrnTrSum =  rrnTrSum + 1;
         If (rrnTrSum = rrnArTsRow) or (rrnTrSum=1 and rrnArTsRow=0);
            rcdTrSumRw = *On;
         Else;
            rcdTrSumRw = *Off;
         Endif;
         Write sflTrSumm;
       Enddo;

       //***** Beg Add ******* 10/07/13 ********************************
       If ClientID <> '025';
       //*** Beg Chg *** 07/30/21 **********************************************
       //***   flexhead1 = 'Division,Post Date,Transaction Type,Billing Amount,+
       //***    Check#,Paid Amount,Balance';
       //***   flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
       //***   on,Category,Check#,Amount,User,Date,Program';
       //*** Beg Chg *** 06/29/22 **********************************************
       //*** flexhead1 = 'Division,Post Date,Transaction Type,Billing Amount,+
       //***  Check#,Paid Amount,Balance,Invoice#';
       //*** flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
       //*** on,Category,Check#,Amount,User,Date,Program,Invoice#';
          flexhead1 = 'Division,Post Date,Transaction Type,Billing Amount,+
           Reference#,Paid Amount,Balance,Invoice#';
          flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
          on,Category, Reference#,Amount,User,Date,Program,Invoice#';
       //*** End Chg *** 06/29/22 **********************************************
       //*** End Chg *** 07/30/21 **********************************************
          FlxChkSt# = 'Check#';
       Else;
          flexhead1 = 'Division,Post Date,Transaction Type,Billing Amount,+
           Statement#,Paid Amount,Balance';
          flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
           on,Category,Statement#,Amount,User,Date,Program';
          FlxChkSt# = 'Statement#';
       Endif;
       //***** End Add ******* 10/07/13 ********************************

       If rrnTrSum > 0;
          If rrnArTsRow=0;
             rrnTrSum =  1;
          Else;
             rrnTrSum = rrnArTsRow;
          EndIf;
       Else;
          Clear DtlPolSts;
          Clear DtlBillSts;
          Clear DtlActSts;
          Clear DtlPmtPln;
          Clear DtlAdr1;
          Clear DtlAdr2;
          Clear DtlAdr3;
          Clear DtlAdr4;
          Clear DtlAdr5;
          Clear DtlAdr6;
          Clear DtlAdr7;
          Clear scDTTotal;
          Clear DtlDiv;
          Clear DtlPoDt;
          Clear DtlChk#;
          Clear DtlTraTyp;
          Clear DtlBal;
          Clear DtlBilAmt;
          Clear DtlPdAmt;
          //*** Beg Add *** 07/30/21 *******************************************
          Clear DtlInv#;
          //*** End Add *** 07/30/21 *******************************************

          clrTrnDtBx = *On;
          dspTrSumSb = *Off;
          dspTrSmDSb = *Off;
          Write rcdTrSum;
          clrTrnDtBx = *Off;
          dspTrSumSb = *On;
          dspTrSmDSb = *On;
          rrnDtlTrn = 0;
       EndIf;

      /end-free
     P LoadTrSumGrid   E

       //===================================================================
     P LoadTrDtlGrid   B
     D LoadTrDtlGrid   PI
       //===================================================================

      /free

       // Clear the subfile.
       clrTrDtl = *On;
       dspTrDtlSb = *Off;
       Write rcdTrDtl;
       clrTrDtl = *Off;
       dspTrDtlSb = *On;
       rrnTrDtl = 0;

       Clear scTdDiv;
       //***** Beg Add ******* 03/05/18 **************************
       Clear scTdDivSR;
       //***** End Add ******* 03/05/18 **************************
       Clear scTdPostDt;
       Clear scTdTrnsDt;
       Clear scTdCode;
       Clear scTdType;
       Clear scTdDesc;
       Clear scTdCatgry;
       Clear scTdChk#;
       Clear scTdAmount;
       Clear scTdBalanc;
       Clear scTdLate;
       //*** Begin Add *** 06/01/21 ******************************
       Clear scTdInv#;
       //*** End   Add *** 06/01/21 ******************************

      /end-free
     P LoadTrDtlGrid   E

       //===================================================================
       // Load AR Category Detail.
       //===================================================================
     P LoadCatDtlGrid  B
     D LoadCatDtlGrid  PI

      /free
       // Clear the subfile.
       ClrCatDtl  = *On;
       dspCtDtlSb = *Off;
       Write rcdCatDtl;
       clrCatDtl = *Off;
       dspCtDtlSb = *On;
       rrnCatDtl = 0;

       // Load A/R Transaction Detail Grid
       LoadTrDtlGrid();

       Clear scCDDiv;
       Clear scCdPostDt;
       Clear scCdCatgry;
       Clear scCdType;
       Clear scCdDesc;
       Clear scCdAmount;
       Clear scCdBalanc;
       Clear arrCodecount;
       Clear arrCode;
       Clear arrType;
       Clear arrTypeDesc;
       Clear arrCatgry;
       Clear arrCatgryDesc;
       Clear arrAmount;
       Clear xxIndex;
       Clear xxIndexC;
       Clear arrCatcount;
       Clear arrCatgryC;
       Clear arrCatgryDescC;
       Clear arrPaidAmount;
       Clear arrBilledAmt;
       Clear xxIndexTD;
       Clear xxIndexCD;
       Clear trSmcount;
       Clear arrTrnSum;
       Clear arrCdDtl;
       Clear arrCdTyp;
       Clear arrCdCat;
       Clear arrCdDiv;
       Clear arrCdPoDt;
       Clear arrCdDtTy;
       Clear arrCdBlAmt;
       Clear arrCdPdAmt;
       Clear arrTrSumTyp;
       Clear arrDtlDiv;
       Clear arrDtlPoDt;
       Clear arrDtlTrTy;
       Clear arrDtlBlAmt;
       Clear arrDtlPdAmt;
       Clear arrDtlChk#;
       Clear arrTrSumRRN;
       Clear arrCdDtlRRN;
       Clear typeAmount;
       Clear typeAmount4;
       Clear arrCdTypAmt;
       Clear intPriorAmt;
       //*** Beg Add *** 07/30/21 **********************************************
       Clear arrDtlInv#;
       //*** End Add *** 07/30/21 **********************************************

       // Populate the subfile
       Setll key1WMEAL30 pea30;
       Reade key1WMEAL30 pea30;
       Dow not %eof(WMEAL30);
          //***** Beg Add ******* 04/24/13 **************************
          ##year = scfyr;
          //***** End Add ******* 04/24/13 **************************
          If MeetFilter = *on and (scVarDiv = ' '
             or  (scVarDiv <> ' ' and %dec(scVarDiv:5:0) = EAdiv));

             typeAmount = AR_value(EAAMT:EAARTY:flgPaidBil);
             typeAmount4 = AR_value_Cat(EAAMT:EAARTY:flgPaidBil1);
             If flgPaidBil  = 'B';
                bAmount =  typeAmount;
                Clear  pamount;
             Else;
                pAmount = typeAmount;
                Clear bamount;
             Endif;

             If flgPaidBil1 = 'B';
                bAmount2 =  typeAmount4;
                Clear  pamount2;
             Else;
                pAmount2 = typeAmount4;
                Clear bamount2;
             Endif;

             If EActyp = ' ';
                xxKey = EAarcd;
                Chain keyTable ptt01;
                If %found;
                   EActyp = TTctyp;
                Endif;
             Endif;

             arCdDs = %editc(EAco#:'X')     +
                      %editc(EAfnd:'X')     +
                      %editc(EAfyr:'X')     +
                      %editc(EAemp#:'X')    +
                      %editc(EAdiv:'X')     +
                      %editc(EApodt:'X')    +
                      %trim(EActyp);


             xxIndexCD = %Lookup(arCdDs : arrCdDtl);
             If xxIndexCD = 0;
                trSmcount = trSmcount + 1;
                arrCdDtl(trSmcount) =  %editc(EAco#:'X')   +
                                       %editc(EAfnd:'X')   +
                                       %editc(EAfyr:'X')   +
                                       %editc(EAemp#:'X')  +
                                       %editc(EAdiv:'X')   +
                                       %editc(EApodt:'X')  +
                                       %trim(EActyp);

                arrCdTyp(TrSmcount)  = EActyp;
                arrCdCat(TrSmcount)  = EAarty;
                arrCdDiv(TrSmcount)  = EAdiv;
                arrCdPoDt(TrSmcount) = CvtToDate(EApodt);
                clear typeDesc;
                Select;
                When EAarty = 'P' or EAarty = 'I'
                 or EAarty = 'Q';
                   typeDesc = 'Payment';
                When EAarty = 'B' or EAarty = 'F'
                 or EAarty = 'H' or EAarty = 'A'
                 or EAarty = 'D';
                   typeDesc = 'Billed';
                When EAarty = 'R';
       // *** Beg Chg ***  06/11/14  *******************************************************
       //********* typeDesc = 'Recovery';
                   typeDesc = 'Refund';
       // *** End Chg ***  06/11/14  *******************************************************
                When EAarty = 'N';
                   typeDesc = 'NSF Charge';
                When EAarty = 'V';
                   typeDesc = 'Void';
       // *** Beg Add ***  06/12/14  *******************************************************
                When EAarty = 'T';
                   typeDesc = 'Return Premium Void';
       // *** End Add ***  06/12/14  *******************************************************
                Endsl;

                arrCdDtTy(TrSmcount)  = typeDesc;
                arrCdBlAmt(TrSmcount) = bAmount;
                arrCdPdAmt(TrSmcount) = pAmount;
                arrCdTypAmt(TrSmcount) = typeAmount;

                arrCdDtlRRN(TrSmcount) = dbrrn30;

             Else;
                arrCdCat(TrSmcount)  = EAarty;
                arrCdPdAmt(xxIndexCD)  = pAmount + arrCdPdAmt(xxIndexCD);
                arrCdBlAmt(xxIndexCD)  = bAmount + arrCdBlAmt(xxIndexCD);
                arrCdTypAmt(TrSmcount) = typeAmount + arrCdTypAmt(TrSmcount);
                clear typeDesc;
                Select;
                When EAarty = 'P' or EAarty = 'I'
                 or EAarty = 'Q';
                   typeDesc = 'Payment';
                When EAarty = 'B' or EAarty = 'F'
                 or EAarty = 'H' or EAarty = 'A'
                 or EAarty = 'D';
                   typeDesc = 'Billed';
                When EAarty = 'R';
       // *** Beg Chg ***  06/11/14  *******************************************************
       //********* typeDesc = 'Recovery';
                   typeDesc = 'Refund';
       // *** End Chg ***  06/11/14  *******************************************************
                When EAarty = 'N';
                   typeDesc = 'NSF Charge';
                When EAarty = 'V';
                   typeDesc = 'Void';
       // *** Beg Add ***  06/12/14  *******************************************************
                When EAarty = 'T';
                   typeDesc = 'Return Premium Void';
       // *** End Add ***  06/12/14  *******************************************************
                Endsl;

                arrCdDtTy(TrSmcount)  = typeDesc;
             Endif;
             Exsr BldSumTrnCd;
             Exsr BldSumCatCd;
          Endif;

          Reade key1WMEAL30 pea30;
       Enddo;

       // Populate the subfile
       y = 1;
       DoW   y <= 999;
         If arrCdDtl(y) = ' ';
           Leave;
         Endif;

         scCDDiv    = arrCdDiv(y);
         scCdPostDt = arrCdPoDt(y);
         scCdCatgry = arrCdTyp(y);
         scCdType   = arrCdCat(y);
         scCdAmount = arrCdTypAmt(y);
         scCdBalanc = intPriorAmt + arrCdTypAmt(y);
         intPriorAmt = scCdBalanc;
         Chain keyWTCTL pct00;
         scCdDesc = %trim(CTdesc) + '-' + arrCdDtTy(y);

         y = y + 1;
         rrnCatDtl =  rrnCatDtl + 1;
         Write sflCatDtl;
       Enddo;
       rrnCatDtl = 1;
       Clear trSmcount;

      /end-free
       //==========================================================================
        // BldSumCatCd - Build Up Array for A/R Summary by Transaction Code (grid)
       //==========================================================================
      /free
       Begsr BldSumTrnCd;
         xxIndex = %Lookup(EAarcd : arrCode);
         If xxIndex = 0;
           arrCodecount = arrCodecount + 1;
           arrCode(arrCodecount) = EAarcd;
           arrType(arrCodecount) = EAarty;
           arrTypeDesc(arrCodecount) = typeDesc;
           If EActyp = ' ';
             xxKey = EAarcd;
             Chain keyTable ptt01;
             If %found;
               EActyp = TTctyp;
             Endif;
           Endif;
           arrCatgry(arrCodecount) = EActyp;
           Chain keyWTCTL# pct00;
           arrCatgryDesc(arrCodecount) = CTdesc;
           arrAmount(arrCodecount) = typeAmount;
         Else;
           arrAmount(xxIndex) = typeAmount + arrAmount(xxIndex);
         Endif;
       Endsr;
      /end-free
       //=======================================================================
        // BldSumCatCd - Build Up Array for A/R Summary by Category Code (grid)
       //=======================================================================
      /free
       Begsr BldSumCatCd;
         If EActyp = ' ' and EAarcd <> ' ';
           xxKey = EAarcd;
           Chain keyTable ptt01;
           If %found;
             EActyp = TTctyp;
           Endif;
         Endif;
         xxIndexC = %Lookup(EActyp : arrCatgryC);
         If xxIndexC = 0;
           arrCatcount = arrCatcount + 1;
           arrCatgryC(arrCatcount) = EActyp;
           Chain keyWTCTL# pct00;
           arrCatgryDescC(arrCatcount) = CTdesc;
           arrPaidAmount(arrCatcount) = pAmount2;
           arrBilledAmt(arrCatcount)  = bAmount2;
         Else;
           arrPaidAmount(xxIndexC) = pAmount2 + arrPaidAmount(xxIndexC);
           arrBilledAmt(xxIndexC)  = bAmount2 + arrBilledAmt(xxIndexC);
         Endif;
       Endsr;
      /end-free

     P LoadCatDtlGrid  E

       //===================================================================
       // Load AR Summary by Transaction Code Grid.
       //===================================================================
     P LoadTrCdeGrid   B
     D LoadTrCdeGrid   PI
      /free

       // Clear the subfile.
       clrTrCode = *On;
       dspRcCdSb = *Off;
       //******** begin add ****** 09/11/13 ****************
       clrCatCode = *On;
       dspCtCdeSb = *Off;
       //******** end add ******** 09/11/13 ****************
       Write rcdCdeSum;
       clrTrCode = *Off;
       dspRcCdSb = *On;
       //******** begin add ****** 09/11/13 ****************
       clrCatCode = *Off;
       dspCtCdeSb = *On;
       rrncodeCt = 0;

       Clear scCcCat;
       Clear scCcBilled;
       Clear scCcPaid;
       Clear scCcBalnc;
       Clear scBillTot;
       Clear scPaidTot;
       Clear scBalTot;

       y = 1;
       DoW y <= 199;
         If ArrCatgryC(y) = ' ';
           Leave;
         Endif;
         scCcCat  = ArrCatgryDescC(y);
         scCcBilled = ArrBilledAmt(y);
         scCcPaid = ArrPaidAmount(y);
         scCcBalnc = (ArrBilledAmt(y) -  (ArrPaidAmount(y) * -1));

         scBillTot = scCcBilled + scBillTot;
         scPaidTot = scCcPaid   + scpaidTot;
         scBalTot =  scCcBalnc  + scBalTot;
         y = y + 1;
         rrncodeCt =  rrncodeCt + 1;
         Write sflcatCode;
       EndDo;
       rrncodeCt = 1;

       //******** end add ******** 09/11/13 ****************
       rrnCodeTr = 0;

       Clear scTcCode;
       Clear scTcType;
       Clear scTcDesc;
       Clear scTcCatDs;
       Clear scTcAmount;
       Clear scAmtTotal;
       y = 1;
       DoW y <= 199;
         If Arrcode(y) = ' ';
           Leave;
         Endif;

         scTcCode =  arrCode(y);
         scTcType =  arrType(y);
         scTcdesc =  arrTypeDesc(y);
         scTcCatDs=  arrCatgry(y) + '/' + arrCatgryDesc(y);
         scTcAmount= arrAmount(y);

         y = y + 1;
         rrnCodeTr =  rrnCodeTr + 1;
         scAmtTotal = %xfoot(arrAmount);
         Write sflTrCode;
       Enddo;
       rrnCodeTr = 1;

      /end-free
     P LoadTrCdeGrid   E

       //********** beg del ****** 09/11/13 ****************
       //===================================================================
       // Load A/R Summary by Category Code Grid.
       //===================================================================
     P***** LoadCatCdeGrid  B
     D***** LoadCatCdeGrid  PI
      /free

       // Clear the subfile.
       //*****clrCatCode = *On;
       //*****dspCtCdeSb = *Off;
       //*****Write rcdCatCde;
       //*****clrCatCode = *Off;
       //*****dspCtCdeSb = *On;
       //*****rrncodeCt = 0;

       //*****Clear scCcCat;
       //*****Clear scCcBilled;
       //*****Clear scCcPaid;
       //*****Clear scCcBalnc;
       //*****Clear scBillTot;
       //*****Clear scPaidTot;
       //*****Clear scBalTot;

       //*****y = 1;
       //*****DoW y <= 199;
       //*****  If ArrCatgryC(y) = ' ';
       //*****    Leave;
       //*****  Endif;
       //*****  scCcCat  = ArrCatgryDescC(y);
       //*****  scCcBilled = ArrBilledAmt(y);
       //*****  scCcPaid = ArrPaidAmount(y);
       //*****  scCcBalnc = (ArrBilledAmt(y) -  (ArrPaidAmount(y) * -1));
       //*****
       //*****  scBillTot = scCcBilled + scBillTot;
       //*****  scPaidTot = scCcPaid   + scpaidTot;
       //*****  scBalTot =  scCcBalnc  + scBalTot;
       //*****  y = y + 1;
       //*****  rrncodeCt =  rrncodeCt + 1;
       //*****  Write sflcatCode;
       //*****EndDo;
       //*****rrncodeCt = 1;

      /end-free
     P***** LoadCatCdeGrid  E
       //********** end del ****** 09/11/13 ****************

       //===================================================================
       // Load Return Fund
       //===================================================================
     P LoadReturnFund  B
     D LoadReturnFund  PI
      /free

       // Clear the subfile.
       clrRtnFund = *On;
       dspRtnFdSb = *Off;
       Write rcdRtnFnd;
       clrRtnFund = *Off;
       dspRtnFdSb = *On;
       rrnRtnFund = 0;

       Clear scRFDiv;
       Clear scRFName;
       Clear scRFCkDt;
       Clear scRFChk#;
       Clear scRFBnKAct;
       Clear scRFCkType;
       Clear scRFAmount;
       Clear scRFVoSt;

       KkRfPtNm = *Loval;
       KkRfCkDt = *hival;
       KkRfChk# = *hival;

       Setll KeyWAR4L6 PR406;
       Reade scCO# PR406;

       DoW not %eof;
         meetSS1  = *on;
         RtnFndFilter();
         If meetss1 = *on;
           scRFDiv    = R4div;
           scRFName   = R4ptnm;
           scRFCkDt   = CvtToDate(r4ckdt);
           scRFChk#   = R4chk#;
           scRFBnKAct = R4acct;
           scRFAmount = R4amt;

           Select;
           When R4BTYP = 'A';
             scRFCkType = 'Account Current';
           When R4BTYP = 'D';
             scRFCkType = 'Deposit';
           When R4BTYP = 'P';
             scRFCkType = 'Premium';
           When R4BTYP = 'I';
             scRFCkType = 'Independent Cash Distribution';
           When R4BTYP = ' ';
             scRFCkType = ' ';
           Endsl;

           Select;
           When R4vdsp = 'V';
             scRFVoSt = 'Void';
           When R4vdsp = 'S';
             scRFVoSt = 'Stop';
           When R4vdsp = ' ';
             scRFVoSt = ' ';
           Endsl;

           scRtFndrrn = dbrrn9;

           rrnRtnFund =  rrnRtnFund + 1;
           Write sflRtnFund;
         Endif;
         Reade scCo# PR406;
       Enddo;

       rrnRtnFund = 1;

      /end-free
     P LoadReturnFund  E

       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
     D saveDiv         s              5
     D LoadSel         s               n   inz(*off)
      /free

       // Load action bar
       //*** Beg Chg ***** 05/13/25 ****************************
       //***LoadActionBar();
       If @@next <> 'ARInq';
          LoadActionBar();
       Else;
          scVardiv = %char(scDiv);
       Endif;
       //*** End Chg ***** 05/13/25 ****************************

       // Process until exit button is pressed.

       DoU ExitNow = *On;

       //***** Beg Add ***** 04/11/22 **************************
          isInvType = IsModEnabled('S/R Online') or
                      IsModEnabled('Third Party Payment');
       //***** End Add ***** 04/11/22 **************************

          // Check if new division or policy year has been selected.
          If scVardiv <> ' ';
             kkdiv = %int(scVarDiv);
          Endif;

          If kkDiv <> scDiv or (scVarDiv = ' ' and kkDiv > 0);
             LoadSel = *on;
          Endif;

          If kkFyr <> scFyr or kkDiv <> scDiv;
             scFyr = kkFyr;
             scDiv = kkDiv;
             kkArSmdiv = kkdiv;
             firstLoad = *on;
             rrnArTsRow = 0;
             Chain(n) keyWDELL PEL00;
             If not %found;
                Setgt key1WDELL PEL00;
                Readpe(n) key1WDELL PEL00;
                If not %eof;
                   scFyr = ELfyr;
                   kkFyr = ELfyr;
                   kkDiv = ELdiv;
                Endif;
             Endif;
             xxRRN = dbrrn;
          Endif;

          If scvardiv <> ' ';
             If q1prms >= 1;
                @@rrn = %editc(dbrrn: 'X');
             Endif;
          Else;
             @@rrn = %editc(xxRrn:'X');
          Endif;

          If kkFyr <> scFyr or scVardiv <> saveDiv;
             saveDiv = scVarDiv;
             If scVarDiv = ' ';
                kkDiv = 0;
                kkArSmdiv = kkdiv;
             Endif;
          Endif;

         xxBalanceDt = DtCYMD(dsFilter.scBal_dt);
         xxPostToDt  = DtCYMD(dsFilter.scPoDt_to);
         If xxBalanceDt = *zero;
            If xxPostToDt  <> *zero;
               xxBalanceDt = xxPostToDt;
            Else;
               xxBalanceDt = DateTo7(SystemDate);
            Endif;
         Endif;
         sccurr = CurBalDiv(xxBalanceDt);
         scbal_to = '  Balances through: ' +
          %trim(alphaUSA(cvtToDate(xxBalanceDt)));
         //*** Beg Add ***** 05/13/25 *********************************
         scEmpNo = %editc(ELfnd:'X') + ' - ' + %editc(ELemp#:'X');
         //*** End Add ***** 05/13/25 *********************************

          If LoadSel = *on;
             LoadSel = *off;
             LoadSelection();
          Endif;

          // Load Filter Clear Button.
          LoadClrFltr();

        //***  Begin Chg  ***  07/02/18  ******************************
        //// Load A/R Summary Grid.
        //LoadARSumGrid();
        //
        //// Load A/R Category Detail Grid
        //LoadCatDtlGrid();
        //
        //// Load A/R Summary by Transaction Code Grid.
        //LoadTrCdeGrid();
        //
        //// Load A/R Summary by Category Code Grid.(TAB 5)
       //********** beg del ****** 09/11/13 ****************
       //***LoadCatCdeGrid();
       //********** beg del ****** 09/11/13 ****************
        //
        //// Load A/R Transaction Detail Grid
        //BldTrnDetail();
        //
        //// Load A/R Transaction Summary Grid
        //BldTrnSum();
        //LoadTRSumGrid();
        //
        //// Load Return Fund.
        //LoadReturnFund();
          //========
          Select;
          When tabIndex = 0;
             // Load A/R Summary Grid.
             LoadARSumGrid();
          When tabIndex = 1;
             // Load A/R Transaction Summary Grid
             BldTrnSum();
             LoadTRSumGrid();

          When tabIndex = 2;
             // Load A/R Transaction Detail Grid
             LoadTrDtlGrid();
             BldTrnDetail();

          When tabIndex = 3;
            //*** Begin Add *** 12/10/18 ********************
             // Load A/R Category Detail Grid
             LoadCatDtlGrid();
            //*** End   Add *** 12/10/18 ********************
             // Load A/R Summary by Transaction Code Grid.
             LoadTrCdeGrid();
          When tabIndex = 4;
             // Load A/R Category Detail Grid
             LoadCatDtlGrid();
          When tabIndex = 5;
             // Load Return Fund.
             LoadReturnFund();
          Endsl;
        //***  End   Chg  ***  07/02/18  ******************************

          // Process A/R Inquiry screen.
          Exsr ExfmtScreens;

          Select;
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'PS');
       //*** end add *****  04/16/15  ****************************************
          //*** Begin add *  09/17/15
       //*****   When btnShowWin = *on;
       //*****      btnShowWin = *off;
       //*****      Select;
       //*****         When TxtShowWin = 'Copy Employer';
       //*****            CopyPolicy(scActRRN:@@Qte);
       //*****         When TxtShowWin = 'Department Letter';
       //*****            DocWindow(scActRRN:'PS');
       //*****      Endsl;
       //*** End add * 09/17/15
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************

          // Do nothing - Catch tab click event
          When tabClick=*On;
             tabClick = *Off;
             firstLoad = *on;
             rrnArTsRow = 0;

          // AR Filter Processing.
          When btnFilter = *on;
             firstLoad = *on;
             rrnArTsRow = 0;
             DoU btnAppFltr = *On or btnCancel = *On;
                LoadSelection();
                ExFmt rcdPopWind;
                If btnAppFltr = *On;
                   dsFilter.scBal_Dt  = scBal_Dt;
                   dsFilter.scPodt_Fr = scPodt_Fr;
                   dsFilter.scPodt_To = scPodt_To;
                   //******* beg chg ******** 03/31/15 ***************************
                   // dsFilter.scTrdt_Fr = scTrdt_To;

                   dsFilter.scTrdt_Fr = scTrdt_Fr;
                   dsFilter.scTrdt_To = scTrdt_To;
                   //******* end chg ******** 03/31/15 ***************************
                   dsFilter.scArCatIn = scArCatIn;
                   dsFilter.scArCatEx = scArCatEx;
                   dsFilter.scArCodes = scArCodes;
                 //***** Beg Add ******* 03/05/18 **************************
                   dsFilter.scSRDiv#  = scSRDiv#;
                 //***** End Add ******* 03/05/18 **************************
                 //*** beg add ***** 06/03/15 **********************************
                   dsFilter.scChkEft  = scChkEft;
                 //*** beg add ***** 06/03/15 **********************************
                 //*** Begin Add *** 06/01/21 ******************************
                   dsFilter.scInv#    = scInv#;
                 //*** End   Add *** 06/01/21 ******************************
                Endif;
                If btnCancel  = *On;
                   scBal_Dt  = dsFilter.scBal_Dt;
                   scPodt_Fr = dsFilter.scPodt_Fr;
                   scPodt_To = dsFilter.scPodt_To;
                   scTrdt_Fr = dsFilter.scTrdt_Fr;
                   //******* beg add ******** 03/31/15 ***************************
                   dsFilter.scTrdt_To = scTrdt_To;
                   //******* end add ******** 03/31/15 ***************************
                   scArCatIn = dsFilter.scArCatIn;
                   scArCatEx = dsFilter.scArCatEx;
                   scArCodes = dsFilter.scArCodes;
                   //***** Beg Add ******* 03/05/18 **************************
                   scSRDiv#  = dsFilter.scSRDiv#;
                   //***** End Add ******* 03/05/18 **************************
                 //*** beg add ***** 06/03/15 **********************************
                   scChkEft  = dsFilter.scChkEft;
                 //*** beg add ***** 06/03/15 **********************************
                 //*** Begin Add *** 06/01/21 ******************************
                   scInv#    = dsFilter.scInv#;
                 //*** End   Add *** 06/01/21 ******************************
                Endif;
             EndDo;

          // Return Funds Filter Processing.
          When BtnRtnFltr = *on;
             DoU BtnRtnAppf = *On or BtnRtnCan = *On;
                ExFmt rcdRtnFltr;
                If BtnRtnAppf = *On;
                   dsRtnFilter.ssAcct  = ssAcct;
                   dsRtnFilter.ssPtFl  = ssPtFl;
                   dsRtnFilter.ssBTyp  = ssBTyp;
                   dsRtnFilter.ssPtNm  = ssPtNm;
                   dsRtnFilter.ssFAmt  = ssFAmt;
                   dsRtnFilter.ssTAmt  = ssTAmt;
                   dsRtnFilter.ssFDat  = ssFDat;
                   dsRtnFilter.ssTDat  = ssTDat;
                   dsRtnFilter.ssCus1  = ssCus1;
                   dsRtnFilter.ssCus2  = ssCus2;
                   dsRtnFilter.ssAgt1  = ssAgt1;
                   dsRtnFilter.ssAgt2  = ssAgt2;
                Endif;
                If BtnRtnCan  = *On;
                   ssAcct  = dsRtnFilter.ssAcct;
                   ssPtFl  = dsRtnFilter.ssPtFl;
                   ssBTyp  = dsRtnFilter.ssBTyp;
                   ssPtNm  = dsRtnFilter.ssPtNm;
                   ssFAmt  = dsRtnFilter.ssFAmt;
                   ssTAmt  = dsRtnFilter.ssTAmt;
                   ssFDat  = dsRtnFilter.ssFDat;
                   ssTDat  = dsRtnFilter.ssTDat;
                   ssCus1  = dsRtnFilter.ssCus1;
                   ssCus2  = dsRtnFilter.ssCus2;
                   ssAgt1  = dsRtnFilter.ssAgt1;
                   ssAgt2  = dsRtnFilter.ssAgt2;
                Endif;
                // Remain on Return Funds tab after any subset is done.
              //********** beg chg ****** 09/11/13 ****************
              //tabIndex = 6;
                tabIndex = 5;
              //********** end chg ****** 09/11/13 ****************
             EndDo;

          When btnRefresh = *On;
           btnRefresh =*Off;
           firstLoad = *on;
           rrnArTsRow = 0;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next=scSelOpt;
             Leave;

          //*** Beg Add ***** 05/13/25 *************************************
          // A/R Inquiry Close
          When btnRFARcan = *On;
             Leave;
          //*** End Add ***** 05/13/25 *************************************
          Other;
             ProcRoWClk();

          Endsl;

          // Popup Windows
          Popup();

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       kkDsbYr = *off;
       TabHeight = 1495;
       APnlHeight = TabHeight + 50;

       Chain xxRrn PEL0P;
       //*** Beg Chg ***** 05/13/25 ***************************
       //***LoadActionBar();
       If @@next <> 'ARInq';
          LoadActionBar();
       Endif;
       //*** End Chg ***** 05/13/25 ***************************
       Select;
          When TabIndex = 0;
             //**** Beg Del ***** 04/24/13 ********************
             // kkDsbYr = *on;
             //**** End Del ***** 04/24/13 ********************
             //*** Beg Chg ***** 05/13/25 *********************
             // Exsr WriteAction;
             // Write rcdPanel;
             // Exfmt rcdAllYear;
             // Exsr ReadAction;
             If @@next <> 'ARInq';
                Exsr WriteAction;
             Endif;
             Write rcdPanel;
             Exfmt rcdAllYear;
             If @@next <> 'ARInq';
                Exsr ReadAction;
             Endif;
             //*** End Chg ***** 05/13/25 *********************
             Read rcdPanel;
          When TabIndex = 1;
             //*** Beg Chg ***** 05/13/25 *********************
             // Exsr WriteAction;
             // Write rcdPanel;
             // Exfmt rcdTrSum;
             // Exsr ReadAction;
             If @@next <> 'ARInq';
                Exsr WriteAction;
             Endif;
             Write rcdPanel;
             Exfmt rcdTrSum;
             If @@next <> 'ARInq';
                Exsr ReadAction;
             Endif;
             //*** End Chg ***** 05/13/25 *********************
             Read rcdPanel;
          When TabIndex = 2;
             //*** Beg Chg ***** 05/13/25 *********************
             // Exsr WriteAction;
             // Write rcdPanel;
             // Exfmt rcdTrDtl;
             // Exsr ReadAction;
             If @@next <> 'ARInq';
                Exsr WriteAction;
             Endif;
             Write rcdPanel;
             Exfmt rcdTrDtl;
             If @@next <> 'ARInq';
                Exsr ReadAction;
             Endif;
             //*** End Chg ***** 05/13/25 *********************
             Read rcdPanel;
          When TabIndex = 3;
             //*** Beg Chg ***** 05/13/25 *********************
             // Exsr WriteAction;
             // Write rcdPanel;
             // Exfmt rcdCdeSum;
             // Exsr ReadAction;
             If @@next <> 'ARInq';
                Exsr WriteAction;
             Endif;
             Write rcdPanel;
             Exfmt rcdCdeSum;
             If @@next <> 'ARInq';
                Exsr ReadAction;
             Endif;
             //*** End Chg ***** 05/13/25 *********************
             Read rcdPanel;
          When TabIndex = 4;
          //********** beg del ****** 09/11/13 ****************
          // Exsr WriteAction;
          // Write rcdPanel;
          // Exfmt rcdCatCde;
          // Exsr ReadAction;
          // Read rcdPanel;
        //When TabIndex = 5;
          //********** end del ****** 09/11/13 ****************
             //*** Beg Chg ***** 05/13/25 *********************
             // Exsr WriteAction;
             // Write rcdPanel;
             // Exfmt rcdCatDtl;
             // Exsr ReadAction;
             If @@next <> 'ARInq';
                Exsr WriteAction;
             Endif;
             Write rcdPanel;
             Exfmt rcdCatDtl;
             If @@next <> 'ARInq';
                Exsr ReadAction;
             Endif;
             //*** End Chg ***** 05/13/25 *********************
             Read rcdPanel;
        //********** beg chg ****** 09/11/13 ****************
        //When TabIndex = 6;
          When TabIndex = 5;
        //********** end chg ****** 09/11/13 ****************
             //*** Beg Chg ***** 05/13/25 *********************
             // Exsr WriteAction;
             // Write rcdPanel;
             // Exfmt rcdRtnFnd;
             // Exsr ReadAction;
             If @@next <> 'ARInq';
                Exsr WriteAction;
             Endif;
             Write rcdPanel;
             Exfmt rcdRtnFnd;
             If @@next <> 'ARInq';
                Exsr ReadAction;
             Endif;
             //*** End Chg ***** 05/13/25 *********************
             Read rcdPanel;
       EndSl;

       Endsr;

       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;

       If qteMode;
          Write rcdQtePnl;
       Else;
          Write rcdPolPnl;
       Endif;

       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       If qteMode;
          Read rcdQtePnl;
       Else;
          Read rcdPolPnl;
       Endif;

       Endsr;
      /end-free
     P Main            E

       //==================================================================
        // MeetFilter - Determine if the records meet the Filter criteria
       //==================================================================
     P MeetFilter      b
    
     d MeetFilter      PI             1n

     d meetSS          s               n
     d compareDt       s              7  0

      /free

       meetSS = *On;
       compareDt = EApodt;
       // *** BEG ADD ***  11/21/13  *******************************************************
       // If "A/R Inq subset default to exclude deposits on pol yr bal" flag is set to 'Y'
       // than Deposit should not be showing in AR Inquiry screens.
       If isEdflFlg <> 'N';
          Chain keyWmexl pex00;
          If %Found(Wmexl) and Exedfl = 'Y';
             scArCatEx = 'D';
             dsFilter.scArCatEx = scArCatEx;
             isEdflFlg = 'N';
          EndIf;
       EndIf;
       // *** END ADD ***  11/21/13  *******************************************************

       // Posting date range.
       xxPostFrmDt = DtCYMD(dsFilter.scPoDt_fr);
       xxPostToDt  = DtCYMD(dsFilter.scPoDt_to);

       If xxPostToDt <> *zero and
         ((xxPostFrmDt > compareDt) or
         (xxPostToDt  < compareDt));
         meetSS = *Off;
       Endif;

       // Transaction date.
       xxTrnsFrmDt =  DtCYMD(dsFilter.scTrDt_fr);
       xxTrnsToDt  =  DtCYMD(dsFilter.scTrDt_to);
       If xxTrnsToDt <> *zero and
         ((xxTrnsFrmDt > EAtrdt) or
         (xxTrnsToDt <  EAtrdt));
         meetSS = *Off;
       Endif;

       // A/R categories.
       Clear   TTctyp;
       xxKey = EAarcd;
       Chain keyTable ptt01;

       If dsFilter.scArCatIn <> *blanks and
         %scan(TTctyp:dsFilter.scArCatIn) = 0;
         meetSS = *Off;
       Else;
         If dsFilter.scArCatEx <> *blanks and
           %scan(TTctyp:dsFilter.scArCatEx) <> 0;
           meetSS = *Off;
         Endif;
       Endif;

       //A/R codes.
       If dsFilter.scArCodes <> *blanks  and
         %scan(EAarcd:dsFilter.scArCodes) = 0;
         meetSS = *Off;
       Endif;

       //***** Beg Add ******* 03/05/18 **************************
       //Self-Rpt Division
       If dsFilter.scSRDiv# <> ' ' and %dec(dsFilter.scSRDiv#:5:0) <> EArdiv;
          meetSS = *Off;
       Endif;
       //***** End Add ******* 03/05/18 **************************
       //*** Begin Add *** 06/01/21 ******************************
       //Invoice #
       If dsFilter.scInv# <> 0 and dsFilter.scInv# <> EAnInv;
          meetSS = *Off;
       Endif;
       //*** End   Add *** 06/01/21 ******************************
       //*** Beg Add *** 06/29/22 ********************************************
       //Check#/EFT#
       If ssChk# <> 0 and ssChk# <> EAchk#;
          meetSS = *off;
       EndIf;

       //Reference#
       If ssRef# <> *Blank and %scan(ssRef#:EAref#:1) = 0;
          meetSS = *off;
       EndIf;
       //*** End Add *** 06/29/22 ********************************************

       //*** beg add ***** 06/03/15 ******************************
       Select;
       When dsFilter.scChkEft = *blanks;
       When dsFilter.scChkEft = 'E';
          If EAfl01 = *blank;
             meetSS = *off;
          Endif;
       When dsFilter.scChkEft = 'C';
          If EAchk# = 0 or EAfl01 = 'E';
             meetSS = *off;
          Endif;
       //*** Beg Add *** 06/29/22 ********************************************
       When dsFilter.scChkEft = '3' and EAref# = *Blank;
          meetSS = *off;
       //*** End Add *** 06/29/22 ********************************************
       Endsl;
       //*** end add ***** 06/03/15 ******************************

       //***** Beg Add ******* 04/24/13 **************************
       ##plan = '*';
       ##asOfDate = DateTo8(SystemDate);
       WA002R(scco#:scfnd:scemp#:EAdiv:##year:0:' ':0:' ':0:0:
        ##plan:##asOfDate);
       //*** begin del ***  10/31/14  ****************************
       //****If ##plan = 'A';
       //****  meetSS = *Off;
       //****Endif;
       //*** end del *****  10/31/14  ****************************
       //***** End Add ******* 04/24/13 **************************

       // Balanece Date.
       xxBalanceDt =  DtCYMD(dsFilter.scBal_Dt);
       If xxBalanceDt = *zero;
         If xxPostToDt  <> *zero;
           xxBalanceDt = xxPostToDt;
         Else;
           xxBalanceDt = DateTo7(SystemDate);
         Endif;
       Endif;

       scbal_to = '  Balances through: ' +
        %trim(alphaUSA(cvtToDate(xxBalanceDt)));

       If xxBalanceDt < compareDt And (TabIndex = 0 or TabIndex = 3 or
        TabIndex = 4);
          meetSS = *Off;
       Endif;

       Return meetSS;

       /end-free
     P MeetFilter      e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;
       scActRrn=xxRrn;
       dbRrn=xxRrn;

       // Process only if valid employer detail record found.
       If not %found;
         processme = *Off;
         Return;
       Endif;

       scCo#     = ELco#;
       scFnd     = ELfnd;
       scEmp#    = ELemp#;
       scDiv     = ELdiv;
       scFyr     = ELfyr;
       saveFyr   = ELfyr;
       kkArSmFyr = ELfyr;
       kkArSmdiv = ELdiv;

       ssFnd  = ELfnd;
       ssEmp# = ELemp#;
       ssDiv  = ELdiv;
       ssPtFl = 'E';

       kkDiv = ELdiv;
       kkFyr = ELfyr;

       saveRtfRRN  = 1;
       saveArsmRRN = 1;
       saveDtTRRN  = 1;
       firstLoad = *on;
       //***** Beg Add ******* 10/07/13 ********************************
       isContribution = GetPrmContflg(scCO#:scFND);
       //***** End Add ******* 10/07/13 ********************************

       LoadSelection();

       //Initialize the Filter fields
       LoadIntFilter();

       //***  Begin Add  ***  07/02/18  *******************************
       str9999msg = 'Your request returned too many records. ' +
        'Click Continue to see the first 9999 records. ' +
        'Click Filter on the display screen to narrow your results.';
       //***  End   Add  ***  07/02/18  *******************************

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E

       //==================================================================
        // RtnFndFilter -  Determine if the records meet the Filter criteria
       //==================================================================
     P RtnFndFilter    b
     d RtnFndFilter    PI

      /free

       If R4chk# = 0;
         meetss1 = *off;
       Endif;

       If ssAcct <> *Blanks And ssAcct <> R4acct;
         meetss1 = *Off;
       Endif;

       If ssFnd <> *Zeros And ssFnd <> R4fnd;
         meetss1 = *Off;
       Endif;

       If ssPtFl <> *Blanks And ssptfl <> R4ptfl;
         meetss1 = *Off;
       Endif;

       If ssEmp# <> *Zeros And ssEmp# <> R4emp# ;
         meetss1 = *Off;
       Endif;

       If ssDiv <> *Zeros And ssDiv <> R4div;
         meetss1 = *Off;
       Endif;

       If ssbtyp <> *blanks And ssbtyp <> R4btyp;
         meetss1 = *off;
       Endif;

       // Check Payee against search string
       If ssptnm <> *Blanks;
         U1pdsc = Upper(R4ptnm);
         U2pdsc = Upper(ssptnm);
         Exsr String;
       Endif;

       If (ssvdsp = 'V' And R4vdsp <> 'V');
          meetss1 = *Off;
       Endif;

       If (ssvdsp = 'S' And R4vdsp <> 'S');
         meetss1 = *Off;
       Endif;

       If ((ssvdsp = 'B' And R4vdsp <> 'V') and
           (ssvdsp = 'B' And R4vdsp <> 'S'));
          meetss1 = *Off;
       Endif;

       If ssfamt <> *Zeros And R4amt < SSfamt;
          meetss1 = *Off;
       Endif;

       If sstamt <> *Zeros And R4amt > SStamt;
         meetss1 = *Off;
       Endif;

       If ssfdat <> *zeros;
         ssfdat7 = DtCYMD(ssfdat);
         If r4ckdt < ssfdat7;
           meetss1 = *off;
         Endif;
       Endif;

       If sstDat <> *zeros;
         sstDat7 = DtCYMD(sstdat);
         If R4ckdt > sstdat7;
           meetss1 = *off;
         Endif;
       Endif;

       If ssCus1 <> *Blanks And ssCus2 <> *Blanks;
         @r4cust = Upper(R4cust);

         If @R4cust < @ssCus1 Or @R4cust > @ssCus2;
            meetss1 = *Off;
         Endif;

       Endif;

       If ssAgt1 <> *Zero And ssAgt2 <> *Zero and
           (R4agt# < ssAgt1 Or R4agt# > ssAgt2);
       meetss1 = *Off;
       Endif;

      /end-free
       //=============================================================
        //STRING - SUBROUTINE TO SEARCH STRINGS
       //=============================================================
     c     String        Begsr

     c                   Call      'QCLSCAN'
     c                   parm                    U1PDSC
     c                   parm      40            STRLEN            3 0
     c                   parm      1             START             3 0
     c                   parm                    U2PDSC
     c                   parm      40            PATLEN            3 0
     c                   parm      '0'           TRANS             1
     c                   parm      '1'           TRIM              1
     c                   parm      ' '           WILD              1
     c                   parm                    SCNRES            3 0

     c     scnres        ifeq      0
     c                   move      *off          meetss1
     c                   endif

     c                   Endsr
     P RtnFndFilter    E
       //===================================================================
       // BldTrnSum  - A/R Transaction Summary (grid)
       //===================================================================
     P BldTrnSum       B
     D BldTrnSum       PI

      /free
      //***** Beg Add ******* 04/10/19 **************************
       Clear arrDtlBlAmt;
       Clear arrDtlPdAmt;
      //***** End Add ******* 04/10/19 **************************
      //***** Beg Add ******* 07/30/21 **************************
       Clear arrTrnSum;
       Clear arrDtlDiv;
       Clear arrDtlPoDt;
       Clear arrDtlTrTy;
       Clear arrDtlBlAmt;
       Clear arrDtlPdAmt;
       Clear arrTrSumTyp;
       Clear arrTrSumRRN;
       Clear arrDtlChk#;
       Clear arrDtlPdAmt;
       Clear arrDtlBlAmt;
       Clear arrDtlInv#;
       Clear trSmCount;
      //***** End Add ******* 07/30/21 **************************

       Setll key1WMEAL30 pea30;
       Reade key1WMEAL30 pea30;
       Dow not %eof(WMEAL30);
          //***** Beg Add ******* 04/24/13 **************************
          ##year = scfyr;
          //***** End Add ******* 04/24/13 **************************
          If MeetFilter = *on and (scVarDiv = ' '
             or  (scVarDiv <> ' ' and %dec(scVarDiv:5:0) = EAdiv));
             Select;
             When EAarty = 'A' or EAarty= 'B' or
                EAarty = 'D' or EAarty= 'F' or
                EAarty = 'H';
                kktype = 'B';
             When  EAarty = 'P' or EAarty= 'Q' or
                EAarty = 'I' or EAarty= 'R' or
                EAarty = 'V';
                kktype = 'P';
             Endsl;

             typeAmount1 = AR_value(EAAMT:EAARTY:
                            flgPaidBil);
             If flgPaidBil  = 'B';
                bAmount1 =  typeAmount1;
                Clear  pamount1;
             Else;
                pAmount1 = typeAmount1;
                Clear bamount1;
             Endif;

             arTrDs = %editc(EAco#:'X')     +
                      %editc(EAfnd:'X')     +
                      %editc(EAfyr:'X')     +
                      %editc(EAemp#:'X')    +
                      %editc(EAdiv:'X')     +
                      %editc(EApodt:'X')    +
                      %trim(kkType)         +
                      %editc(EAchk#:'X');

             xxIndexTD = %Lookup(artrds : ArrTrnSum);
             If xxIndexTD = 0;
                trSmcount = trSmcount + 1;
                kkType1 = kkType;
                arrTrnSum(trSmcount) =  %editc(EAco#:'X')   +
                                        %editc(EAfnd:'X')   +
                                        %editc(EAfyr:'X')   +
                                        %editc(EAemp#:'X')  +
                                        %editc(EAdiv:'X')   +
                                        %editc(EApodt:'X')  +
                                        %trim(kktype1)      +
                                        %editc(EAchk#:'X');
                arrDtlDiv(TrSmcount)   = EAdiv;
                arrDtlPoDt(TrSmcount)  = dtMDYY(EApodt);
                clear typeDesc1;
                Select;
                When EAarty = 'P' or EAarty = 'I'
                 or EAarty = 'Q';
                   typeDesc1 = 'Payment';
                When EAarty = 'B' or EAarty = 'F'
                 or EAarty = 'H' or EAarty = 'A'
                 or EAarty = 'D';
                   typeDesc1 = 'Billed';
                When EAarty = 'R';
       // *** Beg Chg ***  06/11/14  *******************************************************
       //********* typeDesc1 = 'Recovery';
                   typeDesc1 = 'Refund';
       // *** End Chg ***  06/11/14  *******************************************************
                When EAarty = 'N';
                   typeDesc1 = 'NSF Charge';
                When EAarty = 'V';
                   typeDesc1 = 'Void';
       // *** Beg Add ***  06/12/14  *******************************************************
                When EAarty = 'T';
                   typeDesc1 = 'Return Premium Void';
       // *** End Add ***  06/12/14  *******************************************************
                Endsl;
                arrDtlTrTy(TrSmcount)  = typeDesc1;
                arrDtlBlAmt(TrSmcount) = bAmount1;
                arrDtlPdAmt(TrSmcount) = pAmount1;
                arrTrSumTyp(TrSmcount) = kkType;

                arrTrSumRRN(TrSmcount) = dbrrn30;
       // *** Beg Add ***  06/29/22  *******************************************************
                arrDtlRef#(TrSmcount) = EAref#;
       // *** End Add ***  06/29/22  *******************************************************
                   Select;
                //***** Beg Add ******* 10/07/13 **************************
                When ClientID ='025';
                   If EAninv = 0;
                      arrDtlChk#(TrSmcount) =  '0';
                   Else;
                      arrDtlChk#(TrSmcount) =  %char(Eaninv);
                   Endif;
                //***** End Add ******* 10/07/13 **************************
                   When  EAarty = 'B' or EAarty = 'F'
                      or EAarty = 'H' or EAarty = 'A'
                      or EAarty = 'D';
                      arrDtlChk#(TrSmcount) =  *blanks;
                   When  EAarty = 'P' or EAarty = 'I'
                   //******* beg add ***** 06/24/14 ****************************
                      or EAarty = 'R' or EAarty = 'T'
                   //******* end add ***** 06/24/14 ****************************
                      or EAarty = 'Q';
                      If EAchk# = 0;
                         arrDtlChk#(TrSmcount) =  '0';
                      Else;
                         arrDtlChk#(TrSmcount) =  %char(Eachk#);
                      Endif;
                   Endsl;
                //*** Beg Add *** 07/30/21 *************************************
                arrDtlInv#(TrSmCount) =  EAnInv;
                //*** End Add *** 07/30/21 *************************************
             Else;
                arrDtlPdAmt(xxIndexTD)  = pAmount1 + arrDtlPdAmt(xxIndexTD);
                arrDtlBlAmt(xxIndexTD)  = bAmount1 + arrDtlBlAmt(xxIndexTD);
                //*** Beg Add *** 07/30/21 *************************************
                arrDtlInv#(xxIndexTD) =  EAnInv;
                //*** End Add *** 07/30/21 *************************************
                Select;
                   //***** Beg Add ******* 10/07/13 **************************
                When ClientID ='025';
                   If EAninv = 0;
                      arrDtlChk#(TrSmcount) =  '0';
                   Else;
                      arrDtlChk#(TrSmcount) =  %char(Eaninv);
                   Endif;
                   //***** End Add ******* 10/07/13 **************************
                When  EAarty = 'B' or EAarty = 'F'
                    or EAarty = 'H' or EAarty = 'A'
                    or EAarty = 'D';
                    arrDtlChk#(xxIndexTD)   = *blanks;
                When  EAarty = 'P' or EAarty = 'I'
                   //******* beg add ***** 06/24/14 ****************************
                      or EAarty = 'R' or EAarty = 'T'
                   //******* end add ***** 06/24/14 ****************************
                    or EAarty = 'Q';
                      If EAchk# = 0;
                         arrDtlChk#(xxIndexTD) =  '0';
                      Else;
                         arrDtlChk#(xxIndexTD) =  %char(Eachk#);
                      Endif;
                Endsl;
             Endif;
          Endif;
          Reade key1WMEAL30 pea30;
       Enddo;

      /end-free
     P BldTrnSum       E
       //===================================================================
       // BldTrnDetail  - A/R Transaction Detail (grid)
       //===================================================================
     P BldTrnDetail    B
     D BldTrnDetail    PI

      /free

       //*** begin chg ***  05/31/13  ***************************************/
       //******  Setll key1WMEAL30 pea30;
       //******  Reade key1WMEAL30 pea30;
       //******  Dow not %eof(WMEAL30);
         //*** begin chg ***  04/25/14  ***************************************/
         //********** Setll key1WMEAL6 pea06;
         //********** Reade key1WMEAL6 pea06;
         //********** Dow not %eof(WMEAL6);

         //***** beg chg ***** 08/18/15 ***************************************/
         // Setll key1WMEAL30 pea15;
         // Reade key1WMEAL30 pea15;
         // Dow not %eof(WMEAL15);

         Setll key1WMEAL30 pea31;
         Reade key1WMEAL30 pea31;
         Dow not %eof(WMEAL31);
         //***** end chg ***** 08/18/15 ***************************************/
         //*** end chg *****  04/25/14  ***************************************/
            //***** Beg Add ******* 02/21/23 **************************
            If @@next = 'PostAmtDt';
               //***** Beg Chg ******* 07/03/23 **************************
               //***** visbPnl = *on;
               visbSelCon = *on;
               //***** End Chg ******* 07/03/23 **************************
            Endif;
            //***** End Add ******* 02/21/23 **************************

           If EAFyr = scFyr;
       //*** end chg *****  05/31/13  ***************************************/
            //***** Beg Add ******* 02/21/23 **************************
            If EAdiv <> scDiv and @@next = 'PostAmtDt';
               // Process nothing
            Else;
            //***** End Add ******* 02/21/23 **************************
            //***** Beg Add ******* 04/24/13 **************************
            ##year = scfyr;
            //***** End Add ******* 04/24/13 **************************
            //***** Beg Chg ******* 09/23/24 **************************
            //*** If MeetFilter = *on and (scVarDiv = ' '
            //*** or  (scVarDiv <> ' ' and %dec(scVarDiv:5:0) = EAdiv));
            If (MeetFilter = *on and (scVarDiv = ' '
             or  (scVarDiv <> ' ' and %dec(scVarDiv:5:0) = EAdiv)))
             or @@next = 'PostAmtDt';
            //***** End Chg ******* 09/23/24 **************************
               scTdDiv    = Eadiv;
               //***** Beg Add ******* 03/05/18 **************************
               scTdDivSR  = Eardiv;
               //***** End Add ******* 03/05/18 **************************
               scTdPostDt = CvtToDate(EApodt);
               scTdTrnsDt = CvtToDate(EAtrdt);
               scTdcode   = EAarcd;
               scTdtype   = EAarty;

               Select;
                //***** Beg Add ******* 10/07/13 **************************
               When  ClientId = '025';
                  scTdchk# = %Char(EAninv);
                //***** End Add ******* 10/07/13 **************************
               When  EAarty = 'B' or EAarty = 'F'
                  or EAarty = 'H' or EAarty = 'A'
                  or EAarty = 'D';
                  scTdchk# = *blanks;
               Other;
                  If EAchk# = 0;
                     scTdchk# = '0';
                  Else;
                     scTdchk# = %char(Eachk#);
                  Endif;
               Endsl;
       //***  Beg Add ******* 06/29/22 **************************
                     scTdchk# = RightJstfy(scTdchk#:10);
       //***  End Add ******* 06/29/22 **************************

       //*** beg add ***** 06/03/15 ****************************
               Select;
               //***  Beg Add ******* 06/29/22 **************************
               When EAref# <> *blank;
                  scTDChk# = EAref#;
                  scTDEft = '3rd';
               //***  End Add ******* 06/29/22 **************************
               //*** Begin Chg *** 09/04/25 *****************************
               //*** When scTdChk# = '0' or scTdChk# = *blanks;
               When %Trim(scTdChk#) = '0' or scTdChk# = *blanks;
               //*** End   Chg *** 09/04/25 *****************************
                  sctdeft = *blanks;
               When EAfl01 = 'E';
                  sctdeft = 'EFT';
               Other;
                  sctdeft = 'Check';
               Endsl;
       //*** end add ***** 06/03/15 ****************************

               Select;
               When EAlate = 'Y';
                  scTdLate= 'Yes';
               Other;
                  scTdLate= ' ';
               Endsl;

              //*** Begin Add *** 06/01/21 ******************************
              scTdInv# = EAnInv;
              //*** End   Add *** 06/01/21 ******************************

               typeAmount2 = AR_value(EAAMT:EAARTY:
                            flgPaidBil);
               scTdAmount   = typeAmount2;

               Select;
               When scTdType = 'P' or scTdType = 'I'
                  or scTdType = 'Q';
                  typeDesc = 'Payment';
               When scTdType = 'B' or scTdType = 'F'
                  or scTdType = 'H' or scTdType = 'A'or scTdType = 'D';
                  typeDesc = 'Billed';
               When scTdType = 'R';
       // *** Beg Chg ***  06/11/14  *******************************************************
       //******** typeDesc = 'Recovery';
                  typeDesc = 'Refund';
       // *** End Chg ***  06/11/14  *******************************************************
               When scTdType = 'N';
                  typeDesc = 'NSF Charge';
               When scTdType = 'V';
                  typeDesc = 'Void';
       // *** Beg Add ***  06/12/14  *******************************************************
               When EAarty = 'T';
                  typeDesc = 'Return Premium Void';
       // *** End Add ***  06/12/14  *******************************************************
               Endsl;
               // scTdDesc = typeDesc;
               scTdDesc = EADesc;

               Clear CTdesc;
               If EActyp = ' ' and EAarcd <> ' ';
                  xxKey = EAarcd;
                  Chain keyTable ptt01;
                  If %found;
                     EActyp = TTctyp;
                  Endif;
               Endif;
               Chain keyWTCTL# pct00;
               scTdCatgry = EActyp + '/' + %trim(CTdesc);

               scTdBalanc  = scTdBalanc +  scTdAmount;

       //*** begin chg ***  05/31/13  ***************************************/
       //***** scTrDtlRRN = dbrrn30;
       //*** begin chg ***  04/25/14  ***************************************/
               //************** scTrDtlRRN = dbrrn2;
               //***** beg del ***** 08/18/15 ******** **********************/
               // scTrDtlRRN = dbrrn15;
               //***** end del ***** 08/18/15 ******** **********************/
       //*** end   chg ***  04/25/14  ***************************************/
       //*** begin chg ***  05/31/13  ***************************************/

               //***** beg add ***** 08/18/15 ******** **********************/
               scTrDtlRRN = dbrrn31;
               //***** end add ***** 08/18/15 ******** **********************/

               rrnTrDtl =  rrnTrDtl + 1;
               If rrnTrDtl = 1;
                  DtlRcdRow = 'A';
               Else;
                  clear DtlRcdRow;
               Endif;

               //***  Begin Add  ***  07/02/18  **************************
               Select;
               When rrnTrDtl > 9999;
                  PopOneBtn(str9999msg:'Message-Transaction Detail':'Continue');
                  Leave;
               Endsl;
               //***  End   Add  ***  07/02/18  **************************

               Write sflTrnDtl;
            Endif;

            //***** Beg Add ******* 02/21/23 **************************
            Endif;
            //***** End Add ******* 02/21/23 **************************
           //***** beg add ***** 08/18/15 ***************************************/
           Endif;

           Reade key1WMEAL30 pea31;
           //***** end add ***** 08/18/15 ***************************************/

       //*** begin chg ***  05/31/13  ***************************************/
       //******   Reade key1WMEAL30 pea30;
           //***** beg del ***** 08/18/15 ******** **********************/
           // Endif;
           //*** begin chg ***  04/25/14  ***************************************/
           //*********** Reade key1WMEAL6 pea06;
           // Reade key1WMEAL30 pea15;
           //***** end del ***** 08/18/15 ******** **********************/
           //*** end chg *****  04/25/14  ***************************************/
       //*** end chg *****  05/31/13  ***************************************/
       //***** Beg Add ******* 10/07/13 ********************************
       //*** beg dlt ***** 06/03/15 ***********************************
       //*If*ClientID <> '025';
       //*flexhead2*= 'Division,Post Date,Trans Date,Code,Type,Description,+
       //***********   Category,Check,Amount,Balance,Late';
       //*Flx2ChkSt# = 'Check#';
       //*flexhead2*= 'Division,Post Date,Trans Date,Code,Type,Description,+
       //***********   Category,Check#/EFT#,Type,Amount,Balance,Late';
       //*Flx2ChkSt# = 'Check#';
       //*If*EAfl01*= 'E';
       //****Flx2ChkSt# = 'EFT#';
       //*Endif;****
       //*Else;*****
       //*flexhead2*= 'Division,Post Date,Trans Date,Code,Type,Description,+
       //***********   Category,Statement#,Amount,Balance,Late';
       //*Flx2ChkSt# = 'Statement#';
       //*Endif;****
       //*** end dlt ***** 06/03/15 ***********************************
       //***** End Add ******* 10/07/13 ********************************
       //*** beg add ***** 06/03/15 ***********************************
       If ClientID <> '025';
          //***** Beg Chg ******* 03/05/18 **************************
          //flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
          //            Category,Check#/EFT#,Type,Amount,Balance,Late';
          //*** Begin Chg *** 06/01/21 *************************************
          //flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
          //             Category,Check#/EFT#,Type,Self-Rpt Division,Amount,+
          //             Balance,Late';
          //*** Beg Chg ******* 06/29/22 **************************
          //*** flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
          //***              Category,Check#/EFT#,Type,Self-Rpt Division,Amount,+
          //***              Balance,Late,Invoice#    ';
          flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
                       Category,Reference#,Type,Self-Rpt Division,Amount,+
                       Balance,Late,Invoice#    ';
          //*** End Chg ******* 06/29/22 **************************
          //*** End   Chg *** 06/01/21 *************************************
          //***** End Chg ******* 03/05/18 **************************
          Flx2ChkSt# = 'Check#';
       Else;
          //***** Beg Chg ******* 03/05/18 **************************
          //flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
          //           Category,Statement#,Type,Amount,Balance,Late';
          //*** Begin Chg *** 06/01/21 *************************************
          //flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
          //           Category,Statement#,Type,S/R Division,Amount,Balance,Late';
          flexhead2 = 'Division,Post Date,Trans Date,Code,Type,Description,+
                     Category,Statement#,Type,S/R Division,Amount,Balance,Late,+
                     Invoice#';
          //*** End   Chg *** 06/01/21 *************************************
          //***** End Chg ******* 03/05/18 **************************
          Flx2ChkSt# = 'Statement#';
       Endif;
       //*** end add ***** 06/03/15 ***********************************
         Enddo;
         rrnTrDtl =  1;

      /end-free
     P BldTrnDetail    E
       //===================================================================
       // LoadDtlGrd - Load detail grid for selected row
       //===================================================================
     P LoadDtlGrd      B
     D LoadDtlGrd      PI

      /free

       // Clear the detail box
       // Clear DtlDiv#;
       // Clear DtlPtDt;
       Clear DtlPolSts;
       Clear DtlBillSts;
       Clear DtlActSts;
       Clear DtlPmtPln;
       Clear DtlAdr1;
       Clear scDTTotal;

       // Clear the subfile.
       clrTrnDtBx = *On;
       dspTrSumSb = *Off;
       dspTrSmDSb = *Off;
       Write rcdTrSum;
       clrTrnDtBx = *Off;
       dspTrSumSb = *On;
       dspTrSmDSb = *On;
       rrnDtlTrn = 0;

       DtlDiv=scTrDiv;
       DtlPoDt=scTrPodt;
       DtlChk#=scTrChk#;
       DtlTraTyp=scTrTraTyp;
       DtlBal=scTrBal;
       DtlBilAmt=scTrBilAmt;
       DtlPdAmt=scTrPdAmt;
       //*** Beg Add *** 07/30/21 **********************************************
       DtlInv# = scTrInv#;
       //*** End Add *** 07/30/21 **********************************************

       // rrnTrSum=1;
       //If rcdTrSumRw=*On;
       //   rrnTrSum=rrnArTsRow;
       //EndIf;

       // Populate the subfile
       Setll keyWMEAL30 pea30;
       Reade keyWMEAL30 pea30;
       Dow not %eof(WMEAL30);
           If (scTrSmType = 'B' and (EAarty = 'A' or EAarty = 'B' or EAarty ='D'
            or EAarty = 'F' or EAarty = 'H')) or
            (scTrSmType = 'P' and (EAarty = 'P' or EAarty = 'Q' or EAarty = 'I'
       // *** Beg Chg ***  06/12/14  *******************************************************
            or EAarty = 'T'
       // *** End Chg ***  06/12/14  *******************************************************
            or EAarty = 'R' or EAarty = 'V'));
              scDtlDiv  = Eadiv;
              scDtPtDt  = CvtToDate(EApodt);
              scDtTrDt  = CvtToDate(EAtrdt);
              scDtCode  = EAarcd;
              scDtType  = EAarty;

              Select;
                //***** Beg Add ******* 10/07/13 **************************
               When  ClientId = '025';
                  If EAninv = 0;
                     scDtCheck = '0';
                  Else;
                     scDtCheck= %Char(EAninv);
                  Endif;
                //***** End Add ******* 10/07/13 **************************
              When  EAarty = 'B' or EAarty = 'F'
               or EAarty = 'H' or EAarty = 'A'
               or EAarty = 'D';
                 scDtCheck = *blanks;
              When  EAarty = 'P' or EAarty = 'I'
               //******* beg add ***** 06/24/14 ****************************
               or EAarty = 'R' or EAarty = 'T'
               //******* end add ***** 06/24/14 ****************************
               or EAarty = 'Q';
                 If EAchk# = 0;
                    scDtCheck = '0';
                 Else;
                    scDtCheck = %char(Eachk#);
                 Endif;
              Endsl;

              //******* Beg add ***** 06/29/22 ****************************
               scDtCheck = RightJstfy(scDtCheck:10);
               If EAref# <> *Blank;
                  scDtCheck = EAref#;
               EndIf;
              //******* End add ***** 06/29/22 ****************************

              typeAmount3 = AR_value(EAAMT:EAARTY:
                             flgPaidBil);
              scDtAmount  = typeAmount3;
              scDTTotal   = scDTTotal + scDtAmount;

              Select;
              When scDtType = 'P' or scDtType = 'I'
               or scDtType = 'Q';
                 typeDesc = 'Payment';
              When scDtType = 'B' or scDtType = 'F'
               or scDtType = 'H' or scDtType = 'A'or scDtType = 'D';
                 typeDesc = 'Billed';
              When scDtType = 'R';
       // *** Beg Chg ***  06/11/14  *******************************************************
       //******* typeDesc = 'Recovery';
                 typeDesc = 'Refund';
       // *** End Chg ***  06/11/14  *******************************************************
              When scDtType = 'N';
                 typeDesc = 'NSF Charge';
              When scDtType = 'V';
                 typeDesc = 'Void';
       // *** Beg Add ***  06/12/14  *******************************************************
              When EAarty = 'T';
                 typeDesc = 'Return Premium Void';
       // *** End Add ***  06/12/14  *******************************************************
              Endsl;
              scDtDesc = typeDesc;

              Clear CTdesc;
              If EActyp = ' ' and EAarcd <> ' ';
                 xxKey = EAarcd;
                 Chain keyTable ptt01;
                 If %found;
                    EActyp = TTctyp;
                 Endif;
              Endif;
              Chain keyWTCTL# pct00;
              scDtCatDes = EActyp + '/' + %trim(CTdesc);

              clear kkkey1;
              kkPgmD = 'PGMD';
              kkkey1 = EApgm;
              Chain keywtgtl pgt00;
              If %found;
                 scDtPgm = GTdesc;
              Else;
                 scDtPgm = EApgm;
              Endif;

              scDtUser = GetUsrName(EAuser);
              scDtDate = CvtToDate(EAdt);
              //*** Beg Add *** 07/30/21 ***************************************
              scDtInv#= EAnInv;
              //*** End Add *** 07/30/21 ***************************************

              rrnDtlTrn = rrnDtlTrn + 1;
              Write sflTrnSmDt;
           Endif;
           Reade keyWMEAL30 pea30;
       Enddo;

       //***** Beg Add ******* 10/07/13 ********************************
       If ClientID <> '025';
       //***** Beg chg ******* 06/29/22 ********************************
       //*** flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
       //***  on,Category,Check#,Amount,User,Date,Program';
          flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
           on,Category, Reference#,Amount,User,Date,Program';
       //***** End chg ******* 06/29/22 ********************************
       Else;
          flexhead3 = 'Division,Post Date,Trans Date,Code,Type,Descripti+
           on,Category,Statement#,Amount,User,Date,Program';
       Endif;
       //***** End Add ******* 10/07/13 ********************************
       rrnDtlTrn =  1;

       Chain keyWDELL# pel01;

       clear xxKey;
       xxKey = %editc(ELbscd : 'X');
       Chain keyTable PBS01;
       DtlPmtPln = %trimr(xxKey) + ' ' + BSdesc;

       Select;
       When ELFAF = 'A';
         DtlBillSts = 'Audited';
       When  ELFAF = 'E';
         DtlBillSts = 'Estimated';
       When  ELFAF = 'R';
         DtlBillSts = 'Revised';
       Endsl;

       DtlPolSts = GetQuoteStsDesc(scco#:scfnd:scEmp#:scTrDiv);

       clear AVdesc;
       clear xxKey;
       xxKey = ELascd;
       Chain keyTable PAV01;
       DtlActSts  = %trimr(xxkey) + ' ' + AVdesc;

       //* Access third address file
       Clear DtlAdr1;
       Clear DtlAdr2;
       Clear DtlAdr3;
       Clear DtlAdr4;
       Chain(n) (scCO#:scFnd:scEmp#:scDiv) pa300;
       If not %found(wma3l);
         clear pa300;
       Else;
         If A3zip1 = *Blanks;
            A3zip1 = *Zeros;
         Endif;
         If A3zip2 = *Blanks;
            A3zip2 = *Zeros;
         Endif;
         ##zip = %Int(A3zip1)*10000 + %Int(A3zip2);
         FormatAddress(A3adr1:A3adr2:A3cty:A3st:##zip:AdL1:AdL2:AdL3);
         DtlAdr1    = A3cont;
         DtlAdr2    = AdL1;
         DtlAdr3    = AdL2;
         DtlAdr4    = AdL3;
         If A3ooc <> ' ';
            DtlAdr3 = A3ooc;
         Endif;
       Endif;

      /end-free

     P LoadDtlGrd      E
       //===================================================================
       // ProcRoWClk - Process the selected row in Transaction summary grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load detail for selected row
          Chain rrnArTsRow sflTrSumm;
          clear kkPoDt;
          kkPoDt = dtCYMD(scTrPodt);

      /end-free

     P ProcRoWClk      E
       //===================================================================

      * utility procedure definitions
      /COPY SCOPYPOLPI
