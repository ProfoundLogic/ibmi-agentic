     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WPIPOLPOP2 - Policy Popups Service Program (Profound)
     H DFTNAME(WPIPOLPOP2)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H/DEFINE   PROFOUNDUI
    *===============================================================*
    *  Created   : 05/18/12                                         *
    *  Programmer: C:65                                             *
    *  Project   : WC2116 - Policy Transform                        *
    *===============================================================*
      *  Description:  Service program which contains the screen files
      *                and processing for Policy related popups.
      *
      *  The popups included are:
      *    Payroll            (for adding/editing payroll classes)
      *    Excluded States    (for adding/deleting excluded states)
      *    Rating Information (for editing rating info on policy detail)
      *    Large Deductible   (for editing large deduc info on policy detail)
      *    Retro Rating       (for editing retro info on policy detail)
      *    Additional Info    (for editing additional info on policy detail)
      *    Employee Payroll   (for adding/editing Employee Payroll)
      *
      *********************************************************************
      * REVISIONS:
      *
      * 10/08/25 C:333 Support: 119776 (SCS)
      *                Self-reporting billed with no payroll E3
      *                - Moved population of net waiver amount (Q1NWAV) to be within
      *                  net waiver flag condition.
      *
      * 10/07/25 C:333 Support: 119833 (UBIC)
      *                Not billing $0 SR to A/R
      *                - Including $0 SR to A/R.
      *
      * 10/03/25 C:300 Support: 119589 (Prescient)
      *                Unable to click & clear Installment Fees
      *                on Calculated Self Reporting Premium screen.
      *                - fix overlapping issue of Cancel/Save Button
      *                - on Installment Fees fields
      *                Additional Changes:
      *                1) Installment Fees heading should always appear like the
      *                 premium heading, if no change show Fee heading with 0 amount.
      *                2) Installment Fees Amount should not be presented if the fee
      *                 has already been billed for the selected reporting period.
      *                3) in DSPF, in Calculate SR Premium window (rcdCalPrm)
      *                 Change heading on payment side to read payment not billed
      *                 A/R Tax Billed Amount/Code To A/R Tax Payment Amount/Code
      *                 Ins Fee Billed Amount/Code To Ins Fee Payment Amount/Code
      *
      * 09/03/25 C:333 Support: 119610 (SCS)
      *                Installment Fees Charging on MSR Account
      *                - Skipping Installment fee for S/R online Plan.
      *
      * 06/24/24 C:333 Project: 146WKR
      *                Collateral Processing.
      *                Dspf : rcdCalPrm changes
      *                -Adding Collateral Billed/Payment Amount/Code
      *
      * 06/17/24 C:344 Project: WC2952
      *                - Increase Rate field to zoned 8.3.
      *                - Increase Work Class Description field.
      *
      * 06/12/24 C:062 Project: 146WKQ
      *                Add assessment processing.
      *
      * 04/11/23 C:342 Support: 103044
      *                changed added in display file to disable Posting
      *                date field on Calculated Self Reporting Premium
      *                screen.
      *
      * 02/14/23 C:342 Support: 107242
      *                changed column heading from Futures to Future for
      *                subfile on cross reference screen.
      *
      * 09/21/22 C:65  Support: 103692 (FMIC)
      *                Show net waiver charge on net rated calculations.
      *
      * 04/08/22 C:300 Support: 101682 (CoveRisk)
      *                Error while Cross Referencing Policies
      *                When x-ref file getting same policy we started from
      *                Then PrvEmpXRef procedure getting into infinte loop
      *
      * 03/07/22 C:65  Project: Phw#101316 (UBIC)
      *                Do not show installment fee on S/R entry
      *                window if policy is flagged exempt.
      *
      * 03/01/22 C:65  Project: WC2819 (Phw#101215)
      *                Calculate net rates for S/R using SRPREM's net rate
      *                factor instead of using the annual net rate from
      *                the payroll file - unless CareWest.
      *
      * 01/19/22 C:33  Project: WC2837
      *                Extend net rate conditional
      *
      * 10/05/21 C:65  Project: WC2819
      *                Display payroll net rates for net rate groups on
      *                S/R payroll window.
      *
      * 06/22/21 C:295 Project: WC2302                                *
      *                Auto Tab Closure on Deletion                   *
      *                                                               *
      * 05/19/21 C:65  Support: 95945 (InHouse)
      *                Add processing to create wablp record when all
      *                payrolls are audited for a division.
      *
      * 07/13/20 C:136 Project: SY407
      *                Correct man prem calc when adding payroll
      *
      * 06/09/20  C:298 Project: SY420
      *                 Mask SSN
      *
      * 06/11/20 C:65  Support: 89525 (150WEH Sunz Project)
      *                Do not lock employer detail unless its going to be
      *                updated.
      *
      * 01/21/20 C:180 Project WC2702
      *                Add div column to Estimated Payroll Edit Pop Up.
      *                and allow call from Policy review.
      *
      * 12/16/19 C:180 Project 150WEH
      *                Add SR payroll as Audited Payroll
      *
      * 10/18/19 C:180 Project WC2507
      *                Add Estimated Payroll Edit Pop Up.
      *
      * 06/17/19 C:180 Support: 71987
      *                Change claim class edit to send error message
      *                based on class-ext-spec, not on class alone.
      *
      * 11/29/18 C:136 Project: 150WDI
      *                Calculate S/R Premium to the penny
     *
     * 06/08/18 C:136 Project: 150WAQ
     *                Add process to write collateral batch for Sunz.
     *
     * 04/13/18 C:136 Support: 69830
     *                Correct premium calculation
     *
     * 04/10/18 C:33  Project: 150WCF
     *               -Update all divisions for PEOs
     *
     * 02/28/18 C:136 Project: 150WAN
     *                Populate manual premium for self reported payrolls
     *
     * 01/30/18 C:33  Support: 51498
     *                Load type and date prior to calling SelfRptPrm
     *
     * 01/18/18 C:291 Support: 50657
     *                Changed record format rcdVndPyrl, field scPVendor#
     *                contain match property to "true" to search by
     *                vendor number or name. Screen change only.
     *
     * 07/27/17 C:062 Project: 150WAL
     *                Replace PRRT with EPARAT.
     *
     * 07/26/17 C:267 Support: WC2259
     *                Added Employee Payroll ADD/EDIT popup screen         .
     *
     * 05/18/17 C:136 Support: 47339
     *                Weekly self reporting abends when SRRPMY is populated.
     *
     * 03/15/16 C:136 Phaseware: 40196
     *                Populate EPSAFL with an E instead of a blank if
     *                the audit flag is unchecked.
     *
      * 12/11/14 C:283 Support: WA099 86748
      *                Populate the State Code only when State is set for
      *                State Tax.
      *
      * 11/03/14 C:295 Support :WA099 86746
      *                Changed the validation A/R Tax Billed Amount/Code
      *                to allow zero value in it if also Pay to A/R flag
      *                is checked.
     *
      * 10/30/14 C:295 Redmine :4730
      *                Changed the logic so that if the Pay to A/R flag is
      *                not checked then it should not ask for A/R Payment
      *                Amount/Code.Corrected the Error message also.
     *
      * 09/25/14 C:290 Support: WA099 86746
      *                Changed the logic so that if the description field is
      *                blank, then use the default values from WTTTP when
      *                writing the payment records.
     *
      * 09/15/14 C:283 Support: WA099 86748
      *                Write the State Tax record only when the Amount is non-zero
      *                and the AR Code is non-blank.
      *                Initialize the Terrorism Payroll.
     *
      * 05/09/14 C:302 Support: CI099 82147
      *                Doesn't account for payroll ext on A rated payrolls correctly.
      *                It combines payrolls and pulls rates by class/spec.
     *
      * 05/07/14 C:279 Support: WA114 85524
      *                Modified to not allow selection of Inactive vendor.
     *
     * 03/26/14 C:290 Support: WA099 85180
     *                Corrected the screen fields lentgth in
     *                WPIPOLPOP2(Contains DSPF changes only).
     *
     * 03/12/14 C:283 Support: WA114 85073
     *                Correct the Handling on Final Audit Payrolls screen
     *                in case when Cancel button is pressed.
     *
     * 02/27/14 C:283 Support: WA099 84925
     *                Verify if the Cross Ref Employer is in Quote or Live
      *                and pass the mode for getting the correct RRN number.
     *
     * 02/06/14 C:65  Project: WC1473 (Added under SY336 Priss 57)
     *                Change the logical used to write/update the S/R payroll
     *                records from the s/r payroll entry grid as the original
     *                logicals were missing the Effective Date field as part
     *                of the key structure.
     *
     * 10/10/13 C:136 Support: WA114 83781
     *                Fix SelfRptEdit to not reference parms unless
     *                they are passed in.
     *
      * 10/07/13 C:190 Project: 25WSS
      *                - From the Policy Year Detail/ Rating Tab replace
      *                  "Deviation #2" with  Stabilization Factor.
      *                - Change the program to determine if
      *                   Premium or Contribution should be printed
      *                   /displayed based on Group setup.
      *
     * 09/25/13 C:136 Support: WA114 83597
     *                Fix auth edit on zero audit amt
     *
      * 04/26/13 C:279 Redmine: 2801
      *                Modified to pass the correct WDELP rrn.
      *
      * 04/19/13 C:283 Support: WA102 81833
      *                Comment out the Logic for ARATE handling.
      *                ARATEed rating is handled in WPIPOLPOP program.
      *
      * 04/08/13 C:136 Project: 46WGO
      *                Retrofit LSAT stock discount calc into
      *                CompSuite
      ****************************************************************
     H nomain
    F*
     FWPIPOLPOP2CF   E             WORKSTN extfile('WPIPOLPOP2')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     usropn
     f                                     Sfile(sflFinAud:rrnFinAudt)
     f                                     Sfile(sflSrAdEdt:rrnSrRpt)
     f                                     Sfile(sflPrem:rrnSrPrem)
     f                                     Sfile(sflPrvEmp:rrnPrvEmp)
     f                                     Sfile(sflFutEmp:rrnFutEmp)
     f                                     Sfile(sflEstPyr:rrnEstPyr)
     FWDELP     UF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     F                                     infds(dbfds)
     FWDELL1    IF   E           K DISK    rename(PEL01:PEL01)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     FWMEAL10   IF   E           k disk    rename(PEA01:PEA10)
     FWMEAL     O    E           K DISK    rename(PEA01:PEA00)
     FWDDDL1    IF   E           K DISK    rename(PDD01:PDD01)
     FWTRTL1    IF   E           K DISK    rename(PRT01:PRT01)
     FWTGTL     IF   E           K DISK    rename(pgt01:PGT00)
     FWTGIL     IF   E           K DISK    rename(pgi01:PGI00)
     FWDFHL     IF   E           K DISK    rename(PFH01:PFH00)
     FWMGCL     IF   E           K DISK    rename(PGC01:PGC00)
     FWMRPL     UF   E           K DISK    rename(PRP01:PRP00)
     FWMRPL3    IF   E           K DISK    rename(PRP01:PRP03)
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWMEPL     IF   E           K DISK    rename(PEP01:PEP00)
     f                                     infds(dbfds2)
     FWMEPLE    IF   E           K DISK    rename(PEP01:PEP0E)
     FWMEPL1E   UF A E           K DISK    rename(PEP01:PEP01E)
     FWMEPP     UF A E             DISK    rename(PEP01:PEP0P)
     Fwmepl9    if   e           k disk    Rename(pep01:pep09) Prefix(E9:2)
     F*** Begin Add *** 01/21/20 ************************************
     Fwmepl1    if   e           k disk    Rename(pep01:PepL1)
     f                                     infds(dbfds7)
     F*** End   Add *** 01/21/20 ************************************
     FWMEEP     UF A E             DISK    rename(PEE01:PEE0P)
     FWMEEL4    IF   E           K DISK    rename(PEE01:PEEL4)
     F                                     prefix(l1_)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPX01)
     FWDXSL     IF   E           K DISK    rename(PXS01:PXS00)
     f                                     infds(dbfds3)
     FWDXSP     UF A E             DISK    rename(PXS01:PXS0P)
     FSTSTL3    IF   E           K DISK    rename(PST01:PST03)
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
     FWMMGL     IF   E           K DISK    rename(PMG01:PMG00)
     FWMDSL3    IF   E           K DISK    rename(PDS01:PDS03)
     FWTLML1    IF   E           K DISK    rename(PLM01:PLM01)
     FWTDDL2    IF   E           K DISK    rename(PDD01:PDD02)
     FWMCMLZ    IF   E           K DISK    rename(PCM01:PCM01)
     FWMCXL     IF   E           K DISK    rename(PCX01:PCX01)
     FWASRP     IF A E             DISK    rename(PSR01:PSR0P)
     FWASRL6E   IF   E           K DISK    rename(PSR01:PSR01)
     f                                     infds(dbfds5)
     FWASRL8    IF   E           K DISK    rename(PSR01:PSR08)
     FWASRL9    IF   E           K DISK    rename(PSR01:PSR09)
     f                                     infds(dbfds4)
     FWMEPL7E   IF   E           K DISK    rename(PEP01:PEP07E)
     f                                     infds(dbfds6)
     FWASRL12   UF   E           K DISK    rename(PSR01:PSR12)
     FWASRL13   UF   E           K DISK    rename(PSR01:PSR13)
     FWMEPL7    IF   E           K DISK    rename(PEP01:PEL07)
     FWDVHP     UF A E             DISK    rename(PVH01:PVH0P)
     FWMVPL     IF   E           k DISK    rename(PVP01:PVP01)
     FWDVHL     uf a e           k disk    rename(PVH01:PVH00)
     FWDVHL1    uf   e           k disk    rename(PVH01:PVH01)
     F                                     prefix(l1_)
     FWDHEL     IF   E           K DISK    rename(PHE01:PHE01)
     FWMIFL     IF   E           K DISK    rename(PIF01:PIF01)
     FWMLDL1    IF   E           K DISK    rename(PLD01:PLD01)
     FWDMPL     IF   E           K DISK    rename(PMP01:PMP01)
     FWDHCL3    IF   E           K DISK    rename(PHC01:PHC03)
     FWMEAL4    IF   E           K DISK    rename(PEA01:PEA04)
     FWMAHL     IF   E           K DISK    rename(PAH01:PAH00)
     F*** begin add ***  05/19/21  **********************************
     FWABLL     UF A E           K DISK
     F*** end add *****  05/19/21  **********************************
     FWTABL     IF   E           K DISK
     F                                     ignore(PRT01)
     FSSYLF     IF   E           K disk
     F                                     ignore(SSY01)
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)

     d PSDS          ESDS                  extname(SZQ1P)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     d dbfds5          ds
     d  dbrrn5               397    400b 0
     d dbfds6          ds
     d  dbrrn6               397    400b 0
     d******** Beg Add ******* 01/21/20 ********************************
     d dbfds7          ds
     d  dbrrn7               397    400b 0
     d******** End Add ******* 01/21/20 ********************************

     d clientID        s              3    inz('000')
     d isContribution  s               n   inz(*off)

     d md              s              2  0 Dim(12) CTDATA PERRCD(12)
     d con             S             40    DIM(14) CTDATA PERRCD(1)

     d                 DS
     d  Cma                    1     90  2
     d                                     DIM(10)                              PMT AMOUNT
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2
     d*** Begin Add *** 06/12/24 *******************************************
     d dsAssessment    ds                  qualified inz
     d  chrPrintAss                   1a
     d  chrAssessment#...
     d                                1    dim(7)
     d  chrAssessmentHdg...
     d                               20    dim(7)
     d  decAssessmentPct...
     d                                6  4 dim(7)
     d  decAssessmentAmt...
     d                               11  0 dim(7)

     d dsPriorBilled   ds                  qualified inz
     d  decPriorBilledAmt...
     d                               11  0 dim(7)

     d isSRAssessments...
     d                 s               n   Inz(*Off)

     d idx             s              1  0 Inz(0)
     d decAssIdx       s              1  0 Inz(0)
     d arCode          s              2    Inz
     d chrAssHdg       s             32
     d chrAssPct       s              8

     d* Assessment Adjustment Amounts
     d                 ds
     d  aAAmt                  1     77  2
     d                                     DIM(7)
     d  scTA13                 1     11  2
     d  scTA14                12     22  2
     d  scTA15                23     33  2
     d  scTA16                34     44  2
     d  scTA17                45     55  2
     d  scTA18                56     66  2
     d  scTA19                67     77  2

     d* Assessment Current Amounts
     d                 ds
     d  aCAmt                  1     77  2
     d                                     DIM(7)
     d  scTC13                 1     11  2
     d  scTC14                12     22  2
     d  scTC15                23     33  2
     d  scTC16                34     44  2
     d  scTC17                45     55  2
     d  scTC18                56     66  2
     d  scTC19                67     77  2

     d* Assessment Previous Amounts
     d                 ds
     d  aPAmt                  1     77  2
     d                                     DIM(7)
     d  scTP13                 1     11  2
     d  scTP14                12     22  2
     d  scTP15                23     33  2
     d  scTP16                34     44  2
     d  scTP17                45     55  2
     d  scTP18                56     66  2
     d  scTP19                67     77  2

     d* Assessment Heading
     d                 ds
     d  aHdg                   1    280
     d                                     DIM(7)
     d  scHd13                 1     40
     d  scHd14                41     80
     d  scHd15                81    120
     d  scHd16               121    160
     d  scHd17               161    200
     d  scHd18               201    240
     d  scHd19               241    280

     d* Assessment Adjustment Amounts
     d                 ds
     d  aVis                   1      7
     d                                     DIM(7)
     d  visAssmnt1             1      1n
     d  visAssmnt2             2      2n
     d  visAssmnt3             3      3n
     d  visAssmnt4             4      4n
     d  visAssmnt5             5      5n
     d  visAssmnt6             6      6n
     d  visAssmnt7             7      7n

     d* Billing Assessment Amounts
     d                 ds
     d  bAmt                   1     63  2
     d                                     DIM(7)
     d  scBAssAmt1             1      9  2
     d  scBAssAmt2            10     18  2
     d  scBAssAmt3            19     27  2
     d  scBAssAmt4            28     36  2
     d  scBAssAmt5            37     45  2
     d  scBAssAmt6            46     54  2
     d  scBAssAmt7            55     63  2

     d* Billing Assessment Codes
     d                 ds
     d  bCod                   1     14
     d                                     DIM(7)
     d  scBAssCd1              1      2
     d  scBAssCd2              3      4
     d  scBAssCd3              5      6
     d  scBAssCd4              7      8
     d  scBAssCd5              9     10
     d  scBAssCd6             11     12
     d  scBAssCd7             13     14

     d* Billing Assessment Error Messages
     d                 ds
     d  bMsg                   1     70
     d                                     DIM(7)
     d  msBAssCd1              1     10
     d  msBAssCd2             11     20
     d  msBAssCd3             21     30
     d  msBAssCd4             31     40
     d  msBAssCd5             41     50
     d  msBAssCd6             51     60
     d  msBAssCd7             61     70

     d* Billing Assessment Error Indicators
     d                 ds
     d  bErr                   1      7
     d                                     DIM(7)
     d  erBAssCd1              1      1n
     d  erBAssCd2              2      2n
     d  erBAssCd3              3      3n
     d  erBAssCd4              4      4n
     d  erBAssCd5              5      5n
     d  erBAssCd6              6      6n
     d  erBAssCd7              7      7n

     d* Paid Assessment Amounts
     d                 ds
     d  pAmt                   1     63  2
     d                                     DIM(7)
     d  scPAssAmt1             1      9  2
     d  scPAssAmt2            10     18  2
     d  scPAssAmt3            19     27  2
     d  scPAssAmt4            28     36  2
     d  scPAssAmt5            37     45  2
     d  scPAssAmt6            46     54  2
     d  scPAssAmt7            55     63  2

     d* Paid Assessment Codes
     d                 ds
     d  pCod                   1     14
     d                                     DIM(7)
     d  scPAssCd1              1      2
     d  scPAssCd2              3      4
     d  scPAssCd3              5      6
     d  scPAssCd4              7      8
     d  scPAssCd5              9     10
     d  scPAssCd6             11     12
     d  scPAssCd7             13     14

     d* Paid Assessment Error Messages
     d                 ds
     d  pMsg                   1     70
     d                                     DIM(7)
     d  msPAssCd1              1     10
     d  msPAssCd2             11     20
     d  msPAssCd3             21     30
     d  msPAssCd4             31     40
     d  msPAssCd5             41     50
     d  msPAssCd6             51     60
     d  msPAssCd7             61     70

     d* Paid Assessment Error Indicators
     d                 ds
     d  pErr                   1      7
     d                                     DIM(7)
     d  erPAssCd1              1      1n
     d  erPAssCd2              2      2n
     d  erPAssCd3              3      3n
     d  erPAssCd4              4      4n
     d  erPAssCd5              5      5n
     d  erPAssCd6              6      6n
     d  erPAssCd7              7      7n
     d*** End   Add *** 06/12/24 *******************************************

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Load Final Audit Grid
     d LoadFinAudtGrd  pr
     d inRtcd                         3

     d* Load S/R Payroll Grid.
     d LoadSelfRptGrd  pr
     d inShdTyp                       1
     d inSrDt                         8  0
     d inRtCd                         3
     d inMode                         1
     d outRRn#                        9  0

     d* Load Future Cross Ref Employers
     d LoadFutEmp      pr
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const

     d* Effective Date
     d SrEffd          pr             7  0
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)

     d* Get Manual Premium
     d getManPrem      pr            11  0
     d*
     d getHarborExp    pr                  like(HELHNF)
     d*
     d getLossCost     pr             5  4
     d  class                              like(SRCLAS)
     d  effDt                              like(SRTODT)
     d GetSRManPrem    pr

     d ComAPI          pr                  extpgm('WA001R')
     d   ##Co#                        3  0
     d   ##Fnd                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type                       1

     d* StandAlone variables.
     d xxEfDt          s                   like(ELpfdt)
     d xxPxSt          s              2
     d xxSt            s              2
     d xxType          s              1
     d x2EfDt          s                   like(ELpfdt)
     d x3EfDt          s                   like(ELpfdt)
     d x4EfDt          s                   like(ELpfdt) inz
     d inRTCD          s              3
     d i               s              2  0
     D manPremium      s             11  0 inz(0)
     D terrPayroll     s             15  4 inz(0)
     D ratingState     s              2
     d**** Beg Add ***** 06/17/24 ******************************************
     d***xxDesc          s             15
     d xxDesc          s             30    inz
     d**** End Add ***** 06/17/24 ******************************************

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D/copy sPrDate
     D/copy wPrRateSet
     d/copy WPRPOLPOP2
     d/copy WPRPOLPOP
     d/copy WPREMPLR
     d/copy sPrAtrium
     d/copy wPrGroup
     d*** Begin Add *** 06/12/24 *******************************************
     d/copy wPrAR
     d/copy wPrUsrPrms
     d*** End   Add *** 06/12/24 *******************************************


       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0
     d   inFND                        3  0
     d   inFYR                        3  0
     d   inEMP#                       9  0
     d   inDIV                        5  0
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
     d*
     d* SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5
     d*
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
     d*

      /free

       ##Type = *blank;
       If %parms >= 7;
          ##Type = inType;
       Endif;

       ##Clas = *blanks;
       If %parms >= 8;
          ##Clas = %editc(inClas: 'X');
       Endif;

       ##CO# = %editc(inCO#: 'X');
       ##FND = %editc(inFND: 'X');
       ##FYR = %editc(inFYR: 'X');
       ##EMP# = %editc(inEMP#: 'X');
       ##DIV = %editc(inDIV: 'X');
       ##Date = %editc(inDate: 'X');

       SReffdAPI(##CO#:##FND:##FYR:##EMP#:##DIV:##Date:##Type:##Clas);

       outDate = %int(##Date);

       Return outDate;
      /end-free
     P SrEffd          E
       //***************************************************************************//
       //===================================================================
       // Final Audit Payroll Edit Popup
       //===================================================================
     P FinAudtEdit     B                   export
     D FinAudtEdit     PI
     D epRRN                          9  0 const
     D inRTCD                         3    const
     D elRRN                          9  0 const options(*nopass)

     d ComAPI          pr                  extpgm('WA001R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type1                      1


     d xxKey           s                   like(BSKEY)
     d xxDate          s                   like(ELpfdt)
     d xxEfDt          s                   like(ELpfdt)
     d finError        s               n   inz(*off)
     d holdError       s               n   inz(*off)
     d xxSt            s              2
     d xxPxSt          s              2
     d xxType          s              1
     d ##LVL           s              3  0
     d ##Type1         s              1
     d adminFlg        s              1
     d compName        s             40
     d fundName        s             40
     d secType         s              1
     d secLevel        s              3  0
     d intCo#          s              3  0 inz
     d intFnd          s              3  0 inz
     d ##RtCd          s              3

     c* Payroll Class File Key List
     c*
     c     keyWMPRL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas
     c                   Kfld                    xxEfDt
     c     key1WMPRL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas

     c* Payroll Description File Key List
     c*
     c     keyWMPXL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
     c                   Kfld                    EPefdt

     c     key1WMPXL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt

     c* Claim Master
     c     keyWMCMLZ     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    ELfyr
     c                   Kfld                    scFinECLAS

      * Claim/DARTS Control Definition File
     c     keyWMCMLX     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
      /free

       Open WPIPOLPOP2;
       GetClientID(clientID);

       Read SWC01;
       If WCfl08 = 'A';
       Eval adminFlg = 'Y';
       Else;
       Eval adminFlg = ' ';
       Endif;

       Chain(n) elRRN PEL0P;
       intCo# = ELco#;
       intFnd = Elfnd;

       // Load Final Audit Grid, in Edit mode.
       Dou BtnFEdtCnl = *on  or BtnFEdtSav = *on or BtnAddFAdt = *on;
           Dou (finError = *off and holderror = *off) or (BtnFEdtCnl = *on);
               If finError = *off;
                  ##RtCd = inRtcd;
                  LoadFinAudtGrd(##RtCd);
               Endif;
               //***** Beg Add ****** 06/22/21 *************************
               scPF1rrn = elrrn;
               //***** End Add ****** 06/22/21 *************************
               Exfmt rcdAudtPyr;
               If BtnFEdtCnl = *on;
                  Iter;
               Endif;
               holderror = *off;
               Readc sflFinAud;
               Dow not %eof;
                   Exsr FinAudtCheck;
                   If finError = *off;
                      Eval nxtFinAudt = *off;
                      Exsr FinAudtSave;
                   Else;
                      Eval nxtFinAudt = *on;
                   Endif;
                   Update sflFinAud;
                   Readc sflFinAud;
               Enddo;
           Enddo;
           Exsr ClearFinErr;

       // Load Final Audit Grid, this in Add/Edit mode.
           If BtnAddFAdt = *on;
              PayrollEdit(epRRN:inRTCD:elRRN:'B');
              ##RtCd = inRtcd;
              LoadFinAudtGrd(##rtcd);
              BtnAddFAdt = *off;
           Endif;

           If finEditCnl = *on or finEditSav = *on;
              BtnAddFAdt = *off;
           Endif;

       Enddo;
           Clear BtnFEdtCnl;
       Close WPIPOLPOP2;
       //*** begin add ***  05/19/21  **********************************
       //* If audit payroll info is saved, determine if WABLP record needed.
       If BtnFEdtSav = *on;

         //* Populate the subfile
          Setll ahky PEP00;
          Reade ahky PEP00;

          Dow not %eof;
             If EPSafl <> 'A';
                Leave;
             Endif;
             Reade ahky PEP00;
          Enddo;

         //* Update/Create WABLP record if all payrolls are audited.
          If %eof;
             Exsr BILLNG;
          Endif;

       Endif;
       //*** end add *****  05/19/21  **********************************

      /end-free

       //***************************************************************************//
       //===================================================================
       // Final Audit Payroll Error Checking
       //===================================================================
     c     FinAudtCheck  Begsr
     c*
     c     Datchk        Plist
     c                   Parm                    Srdti             8 0
     c                   Parm      *Blanks       Srffmt            8
     c                   Parm      *Blanks       Srtfmt            8
     c                   Parm                    Srdto            10
     c                   Parm                    Srerr             1
     c*
     c     Dtcymdp       Plist
     c                   Parm                    Srdti
     c                   Parm      *Blanks       Srffmt
     c                   Parm      '*CYMD'       Srtfmt
     c                   Parm                    Srdto
     c                   Parm                    Srerr
     c*
     c     Dtmdyyp       Plist
     c                   Parm                    Srdti
     c                   Parm      '*CYMD'       Srffmt
     c                   Parm      *Blanks       Srtfmt
     c                   Parm                    Srdto
     c                   Parm                    Srerr
      *
     c                   Exsr      ClearFinErr
     c     scAudRRN      Chain     PEP0P                              60
      *
      *  Audited amount cannot be less than zero
      *
     C     scFinEAAPR    IFLT      *ZERO
     c                   eval      finError = *on
     c                   eval      holderror = *on
     c                   eval      erFinEAAPR = *on
     c                   eval      msFinEAAPR = 'WCR0547'
     c                   ENDIF                                                  SCEAPR<*ZERO
      *
      *
      *  Audited # of Emplotees cannot be less than Zero.
      *
     C     scFinEACNT    IFLT      *ZERO
     C                   eval      finError = *on
     C                   eval      holderror = *on
     C                   eval      erFinEACNT = *on
     C                   eval      msFinEACNT = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO

     ** Display override window if amt=0 and claims with payments exist and
     ** record hasn't been audited yet. Bypass this edit if client is 'admin
     ** only'
     c                   If        adminFlg <> 'Y' or clientID = '093'
     c                   If        EPsafl <> 'A' or
     c                             scFinEAAPR = 0 and EPaapr <> 0
     c                   If        scFinEAAPR  = 0 and scFinESTAT = 'A'
     c                   Eval      CMA = *zeros
     c     keyWMCMLZ     Chain     PCM01                              60
     c                   Dow       *in60 = *off
     c                   XFOOT     CMA           xxSum            13 2
     c                   If        xxSum > 0 and
     c                             scFinEext = CMext and scFinEspec = CMspec
     c
     C                   eval      finError = *on
     C                   eval      holderror = *on
     C                   eval      erFinEAAP2 = *on
     C                   eval      msFinEAAP2 = 'WC30287'
     c                   Move      *Blanks       XBAUTH            1
     c                   Leave
     c                   Endif                                                  xxSum > 0  or
     c     keyWMCMLZ     ReadE     PCM01                                  60
     c                   Enddo

     c* Display Different window message for client = 93
     c****               If        clientID = '093'
     c****               move      *on           *In51
     c****               Else
     c****               move      *off          *In51
     c****               Endif

     c                   If        *in60 = *off
     c     keyWMCMLX     Chain     PCX01
     c                   If        %found
      /free
       ComAPI(intCo#:intFnd:compName:fundName:secLevel:secType);
      /end-free
     c                   Move      secLevel      ##ULVL
     c                   Move      CXausl        ##CLVL
     c                   Call      'SRCCSC'
     c                   Parm      secType       ##UTYP            1
     c                   Parm                    ##ULVL            3
     c                   Parm      CXAUT1        ##CTYP            1
     c                   Parm                    ##CLVL            3
     c                   Parm      '1'           ##EXEC            1
     c                   Parm                    ##RES             1
     c                   Parm      CXaut2        ##CTP2            1
     c                   Parm      CXaut3        ##CTP3            1
     c                   Parm      CXaut4        ##CTP4            1
     c                   Parm      CXaut5        ##CTP5            1
     c                   Parm      ' '           ##CTP6            1
     c                   Else
     c                   Move      '1'           ##RES
     c                   Endif

     **  ##RES = '0' if "Passed" Security Check
     **  '1' if "Did Not Pass" Security Check
     c                   If        ##RES = '0'
     c                   Move      'Y'           XBAUTH                         Authorized to
     c                   Endif                                                   overide edit

     c                   Eval      usrauth = *off
     c                   Eval      usrNoauth = *off
     c                   If        XBAUTH = 'Y'
     c                   eval      sceovr = 'N'
     c                   Eval      usrauth = *on
      /Free
       scClmClass = %Editc(scFinEclas:'X') + ' ' + scfinEspec + ' ' +
                    %Trim(scFinEext);
      /End-Free
     c                   Exfmt     rcdWMSG2
     c                   If        SCEOVR = 'Y'
     c                   eval      finError = *off
     c                   eval      holderror = *off
     c                   eval      erFinEAAP2 = *off
     c                   Endif
     c                   Else                                                   NOT AUTHORIZED
     c                   Eval      usrNoauth = *on
     c                   Move      'N'           SCEOVR
     c                   EXFMT     rcdWMSG2
     c                   Endif                                                  XBAUTH = 'Y'
     c                   Endif
     c                   Endif                                                  SCAMT = 0
     c                   Endif                                                  EPSAFL <> 'A'
     c                   Endif                                                  adminFlg <> 'Y'
      *
      *  ADDITIONAL LOAD CANNOT BE LESS THAN ZERO
      *
     C     scFinEADLD    IFLT      *ZERO
     C                   eval      finError = *on
     C                   eval      holderror = *on
     C                   eval      erFinEADLD = *on
     C                   eval      msFinEADLD = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  IF AUDIT INFORMATION IS ENTERED, THE FLAG MUST BE 'A'
     C                   If        scFinESTAT <> 'A'
     C                   If        scFinEAAPR <> 0 or scFinEACNT <> 0
     C                   eval      finError = *on
     C                   eval      holderror = *on
     C                   eval      erFinESta3 = *on
     C                   eval      msFinESta3 = 'WC30007'
     C                   Endif
     C                   Endif

      *  VALIDATE CANCEL DATE, IF ENTERED
      *
     C                   Z-ADD     scFinECADT    SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scFinECADT
      *
     C     SRERR         IFEQ      *ON
     C     scFinECADT    ANDNE     *ZEROS
     C                   eval      finError = *on
     C                   eval      holderror = *on
     C                   eval      erFinECADT = *on
     C                   eval      msFinECADT = 'WCR0500'
     C                   ENDIF                                                  SRERR=*ON
      *
      ** This should move to WPIPOLPOP
     c***                If        FinAddSav = *on and ELstat = 'Y'
     c***                Exfmt     rcdWMSG
     c****               Endif

     C                   ENDSR                                                  PayrollCheck

       //===================================================================
       // Save Final Audit Payroll Data.
       //===================================================================
      /free
       Begsr FinAudtSave;
          Chain scAudRRN PEP0P;
          If %found;
             EPcadt = dtCYMD(scFinECADT);
             EPsafl = scFinEStat;
             EPaapr = scFinEaApr;
             EPacnt = scFinEaCnt;
             EPadld = scFinEaDld;
             EPpgm  = Q1pgm;
             EPuser = Q1user;
             EPtime = Timeto6(SystemTime());
             EPdt   = Dateto7(SystemDate());

             //Clear SR Payroll audit flag upon saving changes.
             //*** begin chg *** 06/11/20 ************************************
             //*****Chain elRRN PEL0P;
             Chain(n) elRRN PEL0P;
             //*** end chg ***** 06/11/20 ************************************
             If %found(WDELP) and ELaudsr = 'Y';
                //*** begin add *** 06/11/20 *********************************
                Chain elRRN PEL0P;
                //*** end add ***** 06/11/20 *********************************
                ELaudsr = *blank;
                Update PEL0P;
             Endif;

             Update PEP0P;

          Endif;
       Endsr;
      /end-free

       //===================================================================
       // Clear all Errors
       //===================================================================
      /free
       Begsr ClearFinErr;
          finError = *off;
          erFinECaDt = *off;
          msFinECaDt = *blanks;
          erFinEStat = *off;
          msFinEStat = *blanks;
          erFinEsta2 = *off;
          msFinEsta2 = *blanks;
          erFinESta3 = *off;
          msFinESta3 = *blanks;
          erFinEaApr = *off;
          msFinEaApr = *blanks;
          erFinEaAp2 = *off;
          msFinEaAp2 = *blanks;
          erFinEaCnt = *off;
          msFinEaCnt = *blanks;
          erFinEaDlD = *off;
          msFinEaDld = *blanks;
          usrauth    = *blanks;
          usrNoauth  = *blanks;
       Endsr;
      /end-free
     C***  BEGIN ADD  ***  05/19/21  *********************************
     C*===============================================================
     C* BILLNG - SUBROUTINE FOR WRITING/UPDATING BILL TRANSACTION FILE
     C*===============================================================
     C*
     C     BILLNG        BEGSR
     C*
     C*  BILLING WORK FILE
     C*
     C     BLKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*  WRITE TO BILLING WORK FILE
     C*
     C     BLKY          CHAIN     PBL01                              60
     C*
     C*  CLEAR FLAGS IF WRITING NEW RECORD
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        BLFLG1
     C                   MOVE      *BLANK        BLFLG2
     C                   MOVE      *BLANK        BLFLG3
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   Z-ADD     ELCO#         BLCO#
     C                   Z-ADD     ELFND         BLFND
     C                   Z-ADD     ELFYR         BLFYR
     C                   Z-ADD     ELEMP#        BLEMP#
     C                   Z-ADD     ELDIV         BLDIV
     C*
     C*  WRITE/UPDATE BILLING FLAGS IN WABLP WHEN PAYROLL IS AUDIT TYPE
     C*
     C                   MOVE      *BLANKS       BLFLG1
     C                   MOVE      *BLANKS       BLFLG3
     C                   MOVE      'Y'           BLFLG2
     C*
     C                   Z-ADD     ELPFDT        BLPFDT
     C*
     C     AHKY          SETGT     PAH00
     C     AHKY          READPE    PAH00                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH00                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C     *IN61         IFEQ      *OFF
     C     AHSTAT        ANDNE     'I'
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ELSE
     C                   Z-ADD     *ZERO         BLAGT#
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   MOVEL(P)  Q1USER        BLUSER
     C                   MOVEL(P)  Q1PGM         BLPGM
     C                   Eval      BLTime = Timeto6(SystemTime())
     C                   Eval      BLDt = Dateto7(SystemDate())
     C*
     C                   MOVE      ELNWRW        BLRFLG
     C*
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    PBL01
     C                   ELSE
     C                   WRITE     PBL01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  BILLNG
     C***  END   ADD  ***  05/19/21  *********************************

     P FinAudtEdit     E



       //===================================================================
       // Load Final Audit Grid
       //===================================================================
     P LoadFinAudtGrd  B
     D LoadFinAudtGrd  PI
     D inRtcd                         3

     c*
     c     keyWMPRL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas
     c                   Kfld                    xxEfDt
     c     key1WMPRL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas
     c*
     c     keyWMPXL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
     c                   Kfld                    EPefdt

     c     key1WMPXL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
     c* Employer Payroll File Key List
     c*
     c     keyWMEPL      Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    ELfyr
      /free

         clrFinAudt = *On;
         Write rcdAudtPyr;
         clrFinAudt= *Off;
         rrnFinAudt = 0;

         Clear scTotPayAm;
         Clear scTotEmp#;
         Clear scTotAddLd;

         // Populate the subfile
         Setll keyWMEPL PEP00;
         Reade keyWMEPL PEP00;

         Dow not %eof;

           If EPstat = 'A';
              scFinEEFDT =  dtMDYY(EPefdt);
              scFinECADT =  dtMDYY(EPcadt);
              scFinEClas =  EPclas;
              scFinESpec =  EPspec;
              scFinEExt  =  EPext;
              scFinEAAPR =  EPaapr;
              scFinEACNT =  EPacnt;
              scFinEADLD =  EPadld;
              scAudRRN =  dbrrn2;
              If EPsafl = 'A';
                 scFinESTAT=  'A';
              Else;
                 scFinESTAT=  'E';
              Endif;

              Clear scFinEDesc;
              xxType = 'P';
              xxEfDt = SrEffd(ELco#:ELfnd:ELfyr:ELemp#:ELdiv:EPefdt:xxType:
               EPclas);
              Setgt keyWMPRL1 PPR01;
              Readpe key1WMPRL1 PPR01;
              If not %eof(WMPRL1);
                 scFinEDesc = PRdesc;
                 If EPext <> *blanks;
                    Clear xxPxSt;
                    Setgt keyWMPXL1 PPX01;
                    Readpe key1WMPXL1 PPX01;
                    If Not %Eof(WMPXL1);
                       scFinEDesc = PXdesc;
                    Else;
                       xxPxSt = RatingSt(ELco#:ELrtcd);
                       Setgt keyWMPXL1 PPX01;
                       Readpe key1WMPXL1 PPX01;
                       If Not %Eof(WMPXL1);
                          scFinEDesc = PXdesc;
                       Endif;
                    Endif;
                 Endif;
              Endif;

              scTotPayAm= scTotPayAm + EPaapr;
              scTotEmp# = scTotEmp#  + EPacnt;
              scTotAddLd= scTotAddLd + EPadld;

              rrnFinAudt =  rrnFinAudt + 1;
              Write sflFinAud;

           Endif;
           Reade keyWMEPL PEP00;
         Enddo;

       //* Position payroll grid at top at all times.
         rrnFinAudt = 1;


      /end-free
     P LoadFinAudtGrd  E
       //===================================================================
       // Estimated Payroll Edit Pop Up
       //===================================================================
     P EstPyrEdit      B                   export
     D EstPyrEdit      PI
     D epRRN                          9  0 const
     D inRTCD                         3    const
     D elRRN                          9  0 const options(*nopass)
     D inPayMode                     10    const options(*nopass)

     d xxEfDt          s                   like(ELpfdt)
     d xxPxSt          s              2
     d ##RtCd          s              3
     d EstError        s               n   inz(*off)
     d xxType          s              1
     d xxPayMode       s             10a   inz

     c* Payroll Class File Key List
     c*
     c     keyWMPRL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas
     c                   Kfld                    xxEfDt

     c     key1WMPRL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas

     c* Payroll Description File Key List
     c*
     c     keyWMPXL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
     c                   Kfld                    EPefdt

     c     key1WMPXL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt

      /free

       Open WPIPOLPOP2;
       If %parms >= 4;
          xxPayMode = inPayMode;
       Else;
          xxPayMode = *blanks;
       Endif;

       EstError = *Off;
       GetClientID(clientID);

       Chain(n) elRRN PEL0P;

       // Load Estimated Payroll Grid, in Edit mode.
       Dou BtnCnlEpyr = *on  or BtnSavEpyr = *on or BtnAddEpyr = *on;
           Dou EstError = *off or BtnCnlEpyr = *on ;
               If EstError = *off;
                  ##RtCd = inRtcd;
                  LoadEstPyrGrd(##RtCd:xxPayMode);
               Endif;
              //***** Beg Add ****** 06/22/21 *************************
               scPF1rrn = elrrn;
              //***** End Add ****** 06/22/21 *************************
               Exfmt rcdEstPyr;

               If BtnCnlEpyr = *on;
                  Iter;
               Endif;

               Readc sflEstPyr;
               Dow not %eof;
                   Exsr EstPyrCheck ;
                   If EstError = *off;
                      Eval nxtEstPyr = *off;
                      Exsr EstPyrSave;
                   Else;
                      Eval nxtEstPyr = *on;
                   Endif;
                   Update sflEstPyr;
                   Readc sflEstPyr;
               Enddo;
           Enddo;
           Exsr ClearEstErr;

       // Load Estimated payroll grid, this in Add/Edit mode.
           If BtnAddEpyr = *on;
              If xxPayMode = 'A';
                 PayrollEdit(eprrn:inRTCD:elRRN:'A');
              Else;
                 PayrollEdit(epRRN:inRTCD:elRRN);
              Endif;
              ##RtCd = inRtcd;
              LoadEstPyrGrd(##rtcd:xxPayMode);
              BtnAddEpyr = *off;
           Endif;

       Enddo;
           Clear BtnCnlEpyr;
       Close WPIPOLPOP2;

      /end-free

       //===================================================================
       // Estimated Payroll Error Checking
       //===================================================================
     c     EstPyrCheck   Begsr
     c*
      *// PENDING CHANGE OR CHECK ERROR MESSAGES
      *
     c     Datchk        Plist
     c                   Parm                    Srdti             8 0
     c                   Parm      *Blanks       Srffmt            8
     c                   Parm      *Blanks       Srtfmt            8
     c                   Parm                    Srdto            10
     c                   Parm                    Srerr             1
      *
     c                   Exsr      ClearEstErr
     c     scEstRRN      Chain     PEP0P                              60
      *
      *  Estimated amount cannot be less than zero
      *
     C     scEAPR        IFLT      *ZERO
     c                   eval      EstError = *on
     c                   eval      erEAPR = *on
     c                   eval      msEAPR = 'WCR0547'
     c                   ENDIF                                                  SCEAPR<*ZERO
      *
      *
      *  Estimated # of Employees cannot be less than Zero.
      *
     C     scECNT        IFLT      *ZERO
     C                   eval      EstError = *on
     C                   eval      erECNT = *on
     C                   eval      msECNT = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO

      *
      *  ADDITIONAL LOAD CANNOT BE LESS THAN ZERO
      *
     C     scEstADLD     IFLT      *ZERO
     C                   eval      EstError = *on
     C                   eval      erADLD = *on
     C                   eval      msADLD = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO

      *  VALIDATE CANCEL DATE, IF ENTERED
      *
     C                   Z-ADD     scCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scCADT
      *
     C     SRERR         IFEQ      *ON
     C     scCADT        ANDNE     *ZEROS
     C                   eval      EstError = *on
     C                   eval      erCADT = *on
     C                   eval      msCADT = 'WCR0500'
     C                   ENDIF                                                  SRERR=*ON

     C                   ENDSR                                                  PayrollCheck

       //===================================================================
       // Save Estimated Payroll Data.
       //===================================================================
      /free
       Begsr EstPyrSave;
          Chain scEstRRN PEP0P;
          If %found;
             EPdiv  = scEdiv;
             EPcadt = dtCymd(scCADT);
             EPeapr = scEapr;
             EPecnt = scEcnt;
             EPadld = scEstAdld;
             EPpgm  = Q1pgm;
             EPuser = Q1user;
             EPtime = Timeto6(SystemTime());
             EPdt   = Dateto7(SystemDate());
             Update PEP0P;
          Endif;
       Endsr;
      /end-free

       //===================================================================
       // Clear All Estimated Payroll Errors
       //===================================================================
      /free
       Begsr ClearEstErr;
          erEAPR = *off;
          msEApr = *blanks;
          erEAPR2 = *off;
          msEAPR2 = *blanks;
          erECnt = *off;
          msECnt = *blanks;
          erADld = *off;
          msADld = *blanks;
          erCaDt = *off;
          msCaDt = *blanks;
          EstError = *off;
          usrauth    = *blanks;
          usrNoauth  = *blanks;
       Endsr;
      /end-free

     P EstPyrEdit      E
       //===================================================================
       // Load Estimated Payroll Grid
       //===================================================================
     P LoadEstPyrGrd   B
     D LoadEstPyrGrd   PI
     D inRtcd                         3
     D inPayMode                     10    Const

     c* Employer Payroll File Key List
     c*
     c     keyWMEPL      Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    ELfyr
     c*
     c     keyWMPRL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas
     c                   Kfld                    xxEfDt

     c     key1WMPRL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    inRTCD
     c                   Kfld                    EPclas
     c*
     c     keyWMPXL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
     c                   Kfld                    EPefdt

     c     key1WMPXL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
      /free

         clrEstPyr = *On;
         Write rcdEstPyr;
         clrEstPyr= *Off;
         rrnEstPyr = 0;

         Clear scTEPayAm;
         Clear scTEEmp#;
         Clear scTEAddLd;

         // Populate the subfile
         If inPayMode = 'A';
            Setll (ELco#:ELfnd:ELfyr:ELemp#) PepL1;
            Reade (ELco#:ELfnd:ELfyr:ELemp#) PepL1;
         Else;
            Setll keyWMEPL PEP00;
            Reade keyWMEPL PEP00;
         Endif;

         Dow not %eof;

           If EPstat = 'A';
              scEdiv =  EPdiv;
              scEfdt =  dtMDYY(EPefdt);
              scCadt =  dtMDYY(EPcadt);
              scClas =  EPclas;
              scSpec =  EPspec;
              scExt  =  EPext;
              scEapr =  EPeapr;
              scEcnt =  EPecnt;
              scEstAdld =  EPadld;
              If inPayMode = 'A';
                 scEstRRN =  dbrrn7;
              Else;
                 scEstRRN =  dbrrn2;
              Endif;

              Clear scACDS;
              xxType = 'P';
              xxEfDt = SrEffd(ELco#:ELfnd:ELfyr:ELemp#:ELdiv:EPefdt:xxType:
               EPclas);
              Setgt keyWMPRL1 PPR01;
              Readpe key1WMPRL1 PPR01;
              If not %eof(WMPRL1);
                 scACDS = PRdesc;
                 If EPext <> *blanks;
                    Clear xxPxSt;
                    Setgt keyWMPXL1 PPX01;
                    Readpe key1WMPXL1 PPX01;
                    If Not %Eof(WMPXL1);
                       scACDS = PXdesc;
                    Else;
                       xxPxSt = RatingSt(ELco#:ELrtcd);
                       Setgt keyWMPXL1 PPX01;
                       Readpe key1WMPXL1 PPX01;
                       If Not %Eof(WMPXL1);
                          scACDS = PXdesc;
                       Endif;
                    Endif;
                 Endif;
              Endif;

              scTEPayAm= scTEPayAm + EPeapr;
              scTEEmp# = scTEEmp#  + EPecnt;
              scTEAddLd= scTEAddLd + EPadld;

              rrnEstPyr =  rrnEstPyr + 1;
              Write sflEstPyr;

           Endif;
           If inPayMode = 'A';
              Reade (ELco#:ELfnd:ELfyr:ELemp#) PepL1;
           Else;
              Reade keyWMEPL PEP00;
           Endif;
         Enddo;

       //* Position payroll grid at top at all times.
         rrnEstPyr = 1;


      /end-free
     P LoadEstPyrGrd   E


       //***************************************************************************//
       //===================================================================
       // SR Payroll Add/Edit Popup
       //===================================================================
     P SelfRptEdit     B                   export
     D SelfRptEdit     PI
     D epRRN                          9  0 const
     D inRTCD                         3    const
     D elRRN                          9  0 const
     D inSrDate                       8  0 const
     D inschdTyp                      1    const options(*nopass)
     D inSrLateRpt                    1    const options(*nopass)

     d SRLOGC          c                   CONST('SRLOGCLAS')
     d scCo#           s                   like(ELCo#)
     d scFnd           s                   like(ELFnd)
     d scEmp#          s                   like(ELEmp#)
     d count           s              2  0
     d xxSt            s              2
     d xxPxSt          s              2
     d xxType          s              1
     d ca2002          s                   like(*In03)
     d flgexpmod       s                   like(*In03)
     d found           s               n   inz(*off)
     d xxKey           s                   like(BSKEY)
     d xxDate          s                   like(ELpfdt)
     d xxEfDt          s                   like(ELpfdt)
     d x2EfDt          s                   like(ELpfdt)
     d x3EfDt          s                   like(ELpfdt)
     d xxArat          s                   like(EParat)
     d xxEapr          s                   like(EPeapr)
     d xxRtCd          s                   like(PRrtcd)
     d x1EfDt          s                   like(ELpfdt)
     d xxCo#           s                   like(ELco#)
     d xxDiv           s                   like(Eldiv)
     d xsDiv           s                   like(Eldiv)
     d xxEmp#          s                   like(Elemp#)
     d xxFnd           s                   like(Elfnd)
     d xxFyr           s                   like(Elfyr)
     d x1Key           s                   like(BSkey)
     d xxMod           s                   like(ELexm1)
     d xxSexc          s                   like(ELenmp)
     d svHcn           s                   like(ELhcn)
     d xxBadj          s                   like(EAamt)
     d xxFac2          s                   like(xxfact)
     d xxTerr          s                   like(RRterr)
     d xxHcn           s                   like(ELhcn)
     dxxTa11           s              8  6
     d x3Arty          s                   like(EAarty)
     d x3Co#           s                   like(EAco#)
     d x3Fnd           s                   like(EAfnd)
     d x3Emp#          s                   like(EAemp#)
     d x3Div           s                   like(EAdiv)
     d x3Fyr           s                   like(EAfyr)
     d x3Podt          s                   like(EApodt)
     d vndrFl          s              1n
     d ldModule        s              1n
     d srEditMode      s               n   inz(*off)
     d srAddMode       s               n   inz(*off)
     d srRError        s               n   inz(*off)
     d srRAddMode      s               n   inz(*off)
     d finSrError      s               n   inz(*off)
     d finClError      s               n   inz(*off)
     d holdSrError     s               n   inz(*off)
     d xsKey           s              2
     d ##Mode          s              1
     d lrgdfl          s              1
     d ##schTyp        s              1
     d changedRec      s              1
     d ##srdate        s              8  0
     d xxAmt           s             11  2
     d xxRpmy          s              5  0
     d**** Beg Chg ***** 06/17/24 ******************************************
     d***xxDesc          s             15
     d xxDesc          s             30    inz
     d**** End Chg ***** 06/17/24 ******************************************
     d xxAmpy          s             11  2
     d**** Beg Chg ***** 06/17/24 ******************************************
     d***xxAmrt          s              7  2
     d xxAmRt          s              8S 3 inz
     d**** End Chg ***** 06/17/24 ******************************************
     d xnAmrt          s              7  4
     d xxAmpr          s              9  2
     d xxWk90          s              9  0
     d ##RRN           s              9  0
     d xxDays          s              5  0
     d xxPayr          s             15  4
     d xxPror          s             12  9
     d xxPro           s             11  9
     d xxDivr          s              2  0
     d xxRem           s              2  0
     d x1St            s              2
     d x               s              3  0
     d y               s              3  0
     d z               s              3  0
     d x1yr            s              3  0
     d x1ayr           s              3  0
     d xxIndex         s              3  0
     d priorcount      s              5  0
     d aftercount      s              5  0
     d inRRN           s              9  0
     d inELRRN         s              9  0
     d ##RtCd          s              3
     d sed             s              7  0 Dim(200)
     d scs             s              6    Dim(200)
     d**** Beg Chg ***** 06/17/24 ******************************************
     d***sds             s             15    Dim(200)
     d sds             s             30    Dim(200)
     d**** End Chg ***** 06/17/24 ******************************************
     d spy             s             11  2 Dim(200)
     d**** Beg Chg ***** 06/17/24 ******************************************
     d***srt             s              7  2 Dim(200)
     d srt             s              8S 3 Dim(200)
     d**** End Chg ***** 06/17/24 ******************************************
     d sam             s              9  2 Dim(200)
     d srtn            s              7  2 Dim(200)
     d tmp             S              1    DIM(40)
     d num             S              1    DIM(5)
     d num6            S              1    DIM(6)
     D*** begin add ***  10/05/21  ***************************************
     DisNetRate        s              1n   inz('0')
     D*** end add *****  10/05/21  ***************************************

     d                 DS
     d  xxrpdt                 1      7  0
     d  xxyymm                 1      5  0
     d  xxday                  6      7  0
     d  xxrpyr                 1      3  0
     d  xxrpmn                 4      5  0
     d  xxrpym                 1      5  0

     d                 DS
     d  xxyymm_1               1      5  0
     d  x1                     4      5  0

     d                 DS
     d  xxrpfm                 1      7  0
     d  xxyymm_2               1      5  0
     d  xxday_2                6      7  0

     d                 DS
     d  EPefdt_1               1      7  0
     d  x2yymm                 1      5  0

     d                 DS
     d  xxclsp                 1      6
     d  EPclas_1               1      5  0
     d  EPspec_1               6      6

     d                 DS
     d  xxAnDt                 1      7  0
     d  xxAnYr                 1      3  0

     d                 DS
     d  EM                     1     15  4
     d                                     DIM(3)
     d  ELexm1_1               1      5  4
     d  ELexm2_1               6     10  4
     d  ELexm3_1              11     15  4

     d                 DS
     d  EX                     1    021  0
     d                                     DIM(3) DESCEND
     d  ELemd1_1             001    007  0
     d  ELemd2_1             008    014  0
     d  ELemd3_1             015    021  0

     d  HDG                    1    400
     d                                     DIM(10)
     d  xxHD01                 1     40
     d  xxHD02                41     80
     d  xxHD03                81    120
     d  xxHD04               121    160
     d  xxHD05               161    200
     d  xxHD06               201    240
     d  xxHD07               241    280
     d  xxHD08               281    320
     d  xxHD09               321    360
     d  xxHD10               361    400

     d                 DS
     d  TAM                    1    110  2
     d                                     DIM(10)
     d  xxTA01                 1     11  2
     d  xxTA02                12     22  2
     d  xxTA03                23     33  2
     d  xxTA04                34     44  2
     d  xxTA05                45     55  2
     d  xxTA06                56     66  4
     d  xxTA07                67     77  2
     d  xxTA08                78     88  2
     d  xxTA09                89     99  2
     d  xxTA10               100    110  2

     d                 DS
     d  Q1INFO                 1     39    inz
     d  XXAMT3                 1     15  4
     d  XXFACT                16     26  9
     d  XXPLV1                27     27
     d  XXWFAC                28     38  9
     d  XXWVFL                39     39  0

     d                 DS
     d  ELpfDt_1               1      7  0
     d  xxFPYM                 1      5  0
     d  xxFPYR                 1      3  0
     d  xxFPMO                 4      5  0
     d  xxFPDY                 6      7  0

     d                 DS
     d  ELptDt_1               1      7  0
     d  xxTPYM                 1      5  0
     d  xxTPYR                 1      3  0
     d  xxTPMO                 4      5  0
     d  xxTPDY                 6      7  0

     d                 DS
     d  xxRECF                 1     10
     d  xxSCR#                 2      5

     d                 DS
     d  DSDTTM                 1     12  0
     d  DSTIME                 1      6  0
     d  DSDATE                 7     12  0

     d                 DS
     d  Q1FACT                 1    143    inz
     d  Q1LMPC                 1      5  3
     d  Q1TOT1                 6     16  0
     d  Q1DSCB                17     27  9
     d  Q1TOT2                28     38  0
     d  Q1EXPM                39     43  4
     d  Q1TOT3                44     54  0
     d  Q1DSCA                55     65  9
     d  Q1TOT4                66     76  0
     d  Q1TERR                77     87  0
     d  Q1WAIV                88     98  0
     d  Q1REIN                99    109  0
     d  Q1NETF               110    121  9
     d  Q1NETP               122    132  0
     d  Q1NWAV               133    143  0

     D                 DS
     D  Q2FACT                 1     48
     D  Q2TOT1                 1     12  2
     D  Q2TOT2                13     24  2
     D  Q2TOT3                25     36  2
     D  Q2TOT4                37     48  2

     c     keyWMEPL1E    Klist
     c                   Kfld                    SRco#
     c                   Kfld                    SRfnd
     c                   Kfld                    SRfyr
     c                   Kfld                    SRemp#
     c                   Kfld                    SRdiv
     c                   Kfld                    SRclas
     c                   Kfld                    SRspec
     c                   Kfld                    SRext
     c                   Kfld                    SRefdt

     c     EPKY7A        KLIST
     c                   KFLD                    EPco#
     c                   KFLD                    EPfnd
     c                   KFLD                    EPfyr
     c                   KFLD                    EPemp#
     c                   KFLD                    EPdiv
     c                   KFLD                    EPclas
     c                   KFLD                    EPspec
      *
     c     EPKY7B        KLIST
     c                   KFLD                    EPco#
     c                   KFLD                    EPfnd
     c                   KFLD                    EPfyr
     c                   KFLD                    EPemp#
     c                   KFLD                    EPdiv
     c                   KFLD                    EPclas
     c                   KFLD                    EPspec
     c                   KFLD                    xxrpdt

     c     keyWMEPLE     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    ELfyr

     c     HEKY          KLIST
     c                   KFLD                    xxCo#
     c                   KFLD                    xxFnd
     c                   KFLD                    xxSt
     c                   KFLD                    x2Efdt
     c*
     c     HEKY2         KLIST
     c                   KFLD                    xxCo#
     c                   KFLD                    xxFnd
     c                   KFLD                    xxSt
     c*
     c     PRKEY         KLIST
     c                   KFLD                    xxCo#
     c                   KFLD                    xxRtCd
     c                   KFLD                    xxClas
     c                   KFLD                    x1EfDt
      *
     c     PXKY          KLIST
     c                   KFLD                    ELco#
     c                   KFLD                    xxClas
     c                   KFLD                    EPext
     c                   KFLD                    x1St              2
     c                   KFLD                    x1Efdt            7 0

     c     PXKY1         KLIST
     c                   KFLD                    ELco#
     c                   KFLD                    xxClas
     c                   KFLD                    EPext
     c                   KFLD                    x1St

     c*  KEY LIST FOR THE RATE SET CODE FILE
     c*
     c     RSKY          KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXRTCD

     c*  KEY LIST FOR THE SELF REPORTING FILE
     c*
     c     SRKEY         KLIST
     c                   KFLD                    xxCo#
     c                   KFLD                    xxFnd
     c                   KFLD                    xxFyr
     c                   KFLD                    xxEmp#
     c                   KFLD                    xxDiv
     c                   KFLD                    xxRpmy
     c                   KFLD                    xxClas
     c*
     c     SRKY1         KLIST
     c                   KFLD                    xxCo#
     c                   KFLD                    xxFnd
     c                   KFLD                    xxFyr
     c                   KFLD                    xxEmp#
     c                   KFLD                    xxDiv
     c                   KFLD                    xxRpmy
     c*
     c     SRKY2         KLIST
     c                   KFLD                    xxCo#
     c                   KFLD                    xxFnd
     c                   KFLD                    xxFyr
     c                   KFLD                    xxEmp#
     c                   KFLD                    xxDiv
     c                   KFLD                    xxRpdt
     c                   KFLD                    xxClas

     c     SRKY3         KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXFYR
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXDIV
     c                   KFLD                    XXRPDT

     c*  KEY LIST FOR THE STAT FILE
     c     RRKY          KLIST
     c                   KFLD                    xxCO#
     c                   KFLD                    xxFND
     c                   KFLD                    xxST
     c                   KFLD                    x2EFDT
     c*
     c     RRKY2         KLIST
     c                   KFLD                    xxCO#
     c                   KFLD                    xxFND
     c                   KFLD                    xxST
     c*
     c     EMKY1         KLIST
     c                   KFLD                    xxCO#
     c                   KFLD                    xxFND
     c                   KFLD                    xxEMP#

     c     KEYWMLDL1     KLIST
     c                   KFLD                    xxCO#
     c                   KFLD                    xxFND
     c                   KFLD                    xxEMP#
     c                   KFLD                    xxDIV
     c                   KFLD                    xxFYR

     c     MPKY          KLIST
     c                   KFLD                    xxCO#
     c                   KFLD                    xxFND
     c                   KFLD                    xxFYR
     c                   KFLD                    xxEMP#
     c                   KFLD                    xxDIV

      *  Employer Accounts
      *
     c     EAKY          KLIST
     c                   KFLD                    X3CO#
     c                   KFLD                    X3FND
     c                   KFLD                    X3EMP#
     c                   KFLD                    X3DIV
     c                   KFLD                    X3FYR
     c                   KFLD                    X3ARTY
     c                   KFLD                    X3PODT
      *
     c*
     c* HCN CREDIT KEY
     c*
     c     HCKY          KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXDIV
     c                   KFLD                    XXFYR

     c*  INCR LIMIT TABLE FILE
     c*
     c     LMKY          KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXST
     c                   KFLD                    XXKEY
     c                   KFLD                    XXILEF            7 0
     c*
     c     LMKY2         KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXST
     c                   KFLD                    XXKEY
     c*



     c* Table File Key List
     c*
     c     keyWTABL      Klist
     c                   Kfld                    xxCO#
     c                   Kfld                    x1Key

     c     TBKY1         KLIST
     c                   KFLD                    XXCO#

     c*   KEY LIST FOR THE FUND DETAIL
     c*
     c     F1KY          KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXFYR

     c*  KEY LIST FOR THE AGENT HISTORY FILE
     c*
     c     AHKY          KLIST
     c                   KFLD                    XXCO#
     c                   KFLD                    XXFND
     c                   KFLD                    XXEMP#
     c                   KFLD                    XXDIV
     c                   KFLD                    XXFYR

     c* Employer Detail File Key List
     c*
     c     keyWDELL      Klist
     c                   Kfld                    SRCo#
     c                   Kfld                    SRFnd
     c                   Kfld                    SREmp#
     c                   Kfld                    SRDiv
     c                   Kfld                    SRFyr

     c     keyWASRL12    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELfyr
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    scEASrCLAS
     c                   Kfld                    scEASpec
     c                   Kfld                    kksrefdt          7 0
     c                   Kfld                    scEASrRExt
     c                   Kfld                    kkrpmy            5 0

     c     keyWASRL13    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELfyr
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    kktodt            7 0
     c                   Kfld                    scEASrCLAS
     c                   Kfld                    scEASpec
     c                   Kfld                    kksrefdt          7 0
     c                   Kfld                    scEASrRExt

      /free

       Open WPIPOLPOP2;
       GetClientID(clientID);

       //  Calculate Bill link is clicked from Self-Reporting
       If epRRN <> 0 and elRRN <> 0;
          ##srDate = inSrdate;
          ##schtyp = inschdTyp;
          Chain(n) elRRN PEL0P;
          Exsr SelfRptPrm;
          Dou BtnPrmCnl = *on  or BtnPrmCalc = *on or BtnCalCnl = *off;
              Exfmt rcdSrPrems;
              If BtnPrmCalc = *on;
                 Exsr SrCalculation;
                 Dou BtnCalCnl = *On  or BtnCalSv = *On;
                    Dou (finClError = *Off or BtnCalCnl = *On) and
                         (BtnAdjCnl <> *On);
                        Exfmt rcdCalPrm;
                        If BtnCalCnl = *off;
                           Exsr ClearSrRErr;
                           Exsr SelfCalCheck;
                        Endif;
                    Enddo;
                 Enddo;
                 If BtnCalSv  = *On and BtnAdjCnl <> *On;
                    Exsr CalcSave;
                 Endif;
              Endif;
              BtnPrmCalc = *off;
          Enddo;
          Exsr ClearSrRErr;
       Else;

          srEditMode = *off;
          srAddMode= *off;

          If  epRRN <> 0;
             Chain(n) epRRN PSR0P;
             Chain keyWDELL PEL00;
             srEditMode = *on;
          Else;
             Chain(n) elRRN PEL0P;
             srAddMode = *on;
          Endif;

          xxamt = 0;

          xxSt = RatingSt(ELco#:ELrtcd);

          BtnCalSv  = '0';
          BtnImpCan = '0';
       // Load Self Reporting for Edit mode.
          Dou BtSrEdtCnl = *On  or BtnSRptAdd = *On or BtnSrEdtSv = *On
       //  or BtnPrmCnl = *Off or BtnImpCan = *On;
           or BtnCalSv  = *On  or BtnImpCan = *On;


            Dou (finSrError = *off and holdSrerror = *off)
             or (BtSrEdtCnl = *on) or (BtnSRptAdd = *On);
                ##srDate   = inSrdate;
                ##schtyp   = inschdTyp;
                If holdSrerror = *off;
                   ##RtCd = inRtCd;
                   Select;
                   When srAddMode = *on;
                      ##Mode = 'A';
                   When srEditMode = *on;
                      ##Mode = 'C';
                   Endsl;
                   If changedRec = ' ';
                      LoadSelfRptGrd(##schtyp:##srDate:##RtCd:##Mode:##RRn);
                   Endif;
                Endif;
                Exfmt rcdSlfRpt;
                holdSrerror = *off;
                Readc sflSrAdEdt;
                Dow not %eof;
                    changedRec = 'Y';
                    Readc sflSrAdEdt;
                Enddo;
                If ((priorcount <> aftercount) and
                    (BtSrEdtCnl = *On or BtnSRptAdd = *On)) Or
                   ((changedRec = 'Y') and
                   (BtSrEdtCnl = *On or BtnSRptAdd = *On));
                   Dou BtnImpCan = *On or BtnImpOk = *On;
                       Exfmt rcdWMSG4;
                   Enddo;
                   If BtnImpOk = *On;
                      BtSrEdtCnl = *Off;
                      BtnSRptAdd = *Off;
                   Endif;
                Endif;

                x = 0;
                If ##RRn<>0;
                   DoU x = ##RRn;
                      x = x + 1;
                      Chain x sflSrAdEdt;
                      Exsr SelfRptCheck;
                      If finSrError = *On;
                         Update sflSrAdEdt;
                      Endif;
                   EndDo;
                EndIf;

                If holdSrError = *off and BtnSrEdtSv = *on;
                   Exsr SelfRptSave;
                   Exsr SelfRptPrm;

                   Dou BtnPrmCnl = *on  or BtnPrmCalc = *on or BtnCalCnl = *off;
                       Exfmt rcdSrPrems;
                       If BtnPrmCalc = *on;
                          Exsr SrCalculation;
                          Dou BtnCalCnl = *On  or BtnCalSv = *On;
                              Dou (finClError = *Off or BtnCalCnl = *On) and
                                  (BtnAdjCnl <> *On);
                                  Exfmt rcdCalPrm;
                                  Exsr ClearSrRErr;
                                  If BtnCalCnl = *off;
                                     Exsr SelfCalCheck;
                                  Endif;
                              Enddo;
                          Enddo;
                          If BtnCalSv  = *On and BtnAdjCnl <> *On;
                             Exsr CalcSave;
                          Endif;

                       Endif;
                       BtnPrmCalc = *off;

                   Enddo;
                Endif;
            Enddo;
            Exsr ClearSrRErr;
            BtnSrEdtSv = *off;

       // Load Final Audit Grid, this in Add/Edit mode.
            If BtnSRptAdd = *on;
               inRRN = 0;
               inELRRN = GetEmpYrRR(ELfyr:ELemp#:ELdiv:ELfnd:ELco#);
               PayrollEdit(inRRN:inRTCD:inELRRN);
               priorcount = ##RRn;
               LoadSelfRptGrd(##schtyp:##srDate:##RtCd:##Mode:##RRn);
               BtnSRptAdd = *off;
               aftercount = ##RRn;
            Endif;


          Enddo;
       Endif;

       Close WPIPOLPOP2;
      /end-free


       //***************************************************************************//
       //===================================================================
       // Calculation screen SAVE
       //===================================================================
     C     CalcSave      Begsr

     C                   TIME                    DSDTTM
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORDS,
     C*
     C*
     C     SCBLAR        IFEQ      'Y'
     C*
     C*** Begin Add *** 06/12/24 *******************************************
      *** Beg Chg ***** 10/07/25 *************************************
     C***                If        scBlAm <> 0 And scBlCd <> ' '
     C                   If        scBlCd <> ' '
      *** End Chg ***** 10/07/25 *************************************
     C*** End   Add *** 06/12/24 *******************************************
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   ELSE
     C                   Z-ADD     XXDIV         EADIV
     C                   ENDIF
     C                   Z-ADD     XXDIV         EARDIV
     C                   MOVE      SCBLCD        EAARCD
     C                   MOVE      'B'           EAARTY
     C                   MOVEL     SCBLCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     SCBLDS        EADESC
     C                   Z-ADD     SCBLAM        EAAMT
      *
     C     clientId      IFEQ      '046'
     C     EAAMT         ADD       XXAMT3        EAAMT
     C                   ENDIF                                                  CLTID = '046'
      *
     C                   MOVE      SCBLAT        EALATE
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      *BLANKS       EAUSER
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     'S/R EDIT'    EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     0             EABUL1
     C*
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C*** Begin Add *** 06/12/24 *******************************************
     C                   EndIf
     C*** End   Add *** 06/12/24 *******************************************
      /Free
       If clientID = '150';
          CollateralSR(EACo#:EAFnd:EAEmp#:EAfyr:'E':0:EApodt);
       Endif;
      /End-Free
      *
      *  WRITE INSTALLMENT FEE BILLING RECORD
      *
     C     SCISFE        IFNE      *ZERO
     C     ELIFFL        ANDEQ     *BLANKS
     C                   MOVE      SCIFBL        EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVEL     SCIFBL        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     SCISFE        EAAMT
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C                   ENDIF
       //*** Begin Add *** 06/12/24 ****************************************
       // Write Billing Assessments to A/R
       For idx=1 to 7;
          If bCod(idx) <> ' ' and bAmt(idx) <> 0;
             Clear pea00;
             EAco#  = xxCo#;
             EAfnd  = xxFnd;
             EAfyr  = xxFyr;
             EAemp# = xxEmp#;

             If ELarSm = 'Y';
                EAdiv = xsDiv;
             Else;
                EAdiv = xxDiv;
             EndIf;

             EArDiv  = xxDiv;
             EAarCd = bCod(idx);
             EAarTy = GetARType(EAarCd);
             EAcTyp = GetARCat(EAarCd);
             EAdesc = GetARDesc(EAarCd);
             EAamt  = bAmt(idx);
             EAlate = ' ';
             EAtrDt = DateTo7(systemDate());
             EApodt = dtCYMD(scPoDt);
             EAchk# = scChk#;

             // Get the invoice #
             ExSr GetInv;

             EAdt   = DateTo7(systemDate());
             EAtime = TimeTo6(systemTime());
             EAuser = q1User;
             EApgm  = q1Pgm;
             Write pea00;
          EndIf;
       EndFor;
       //*** End   Add *** 06/12/24 ****************************************
      *
     C*
     C* WRITE DISCOUNT RECORD.
     C*
Q    C     clientID      IFEQ      '046'
     C     XXAMT3        IFNE      *ZERO
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     F1DSCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  TTDESC        EADESC
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      *BLANKS       EACTYP
     C                   Z-SUB     XXAMT3        EAAMT
     C                   MOVE      F1DSCD        EAARCD
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXAMT3 IFNE
     C                   ENDIF                                                  CLTID = '046'
      *
      /Free
       If scTXBA <> *Zeros and scTXBC <> *Blanks;
      /End-Free
     C                   MOVE      *BLANK        EALATE
     C                   MOVE      SCTXBC        EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVE      'T'           EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     XXBCDC        EADESC
     C                   Z-ADD     SCTXBA        EAAMT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C                   ENDIF                                                  SCTXBC IFNE
      //*** Beg Add ***** 06/24/24 **********************************
       If scCoBlAm <> 0 and scCoBlCd <> ' ';
          x1Key = scCoBlCd;
          Chain keyWTABL ptt01;
          EAarcd = scCoBlCd;
          EAdesc = TTdesc;
          EAarty = TTtype;
          EActyp = TTctyp;
          EAamt = scCoBlAm;
          EAstat = 'A';
          EAtime = TimeTo6(systemTime());
          EAuser = q1User;
          EApgm  = q1Pgm;
          Exsr GetInv;
          Write pea00;
       Endif;
      //*** End Add ***** 06/24/24 **********************************
     C                   END
     C*
     C     SCPYAR        IFEQ      'Y'
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   ELSE
     C                   Z-ADD     XXDIV         EADIV
     C                   ENDIF
     C                   Z-ADD     XXDIV         EARDIV
     C                   MOVE      SCPYCD        EAARCD
     C                   MOVE      'P'           EAARTY
     C                   MOVEL     SCPYCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVE      SCPLAT        EALATE
     C                   MOVEL     SCPYDS        EADESC
     C                   Z-ADD     SCPYAM        EAAMT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      *BLANKS       EAUSER
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     'S/R EDIT'    EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     SCCHK#        EACHK#
     C*
     C                   EXSR      GETINV
     C                   WRITE     PEA00
      *
      *  WRITE INSTALLMENT FEE PAYMENT RECORD
      *
     C     SCISFE        IFNE      *ZERO
     C     ELIFFL        ANDEQ     *BLANKS
     C                   MOVE      SCIFPD        EAARCD
     C                   MOVE      'Q'           EAARTY
     C                   MOVEL     SCIFPD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     SCISFP        EAAMT
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C                   ENDIF
     C     SCTXPC        IFNE      *BLANKS
     C                   MOVE      *BLANK        EALATE
     C                   MOVE      SCTXPC        EAARCD
     C                   MOVE      'Q'           EAARTY
     C                   MOVE      'T'           EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     XXPCDC        EADESC
     C                   Z-ADD     SCTXPA        EAAMT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C                   ENDIF                                                  SCTXPC IFNE
      //*** Beg Add ***** 06/24/24 **********************************
       If scCoPyAm <> 0 and scCoPyCd <> ' ';
          x1Key = scCoPyCd;
          Chain keyWTABL ptt01;
          EAarcd = scCoPyCd;
          EAarty = TTtype;
          EAdesc = TTdesc;
          EActyp = TTctyp;
          EAamt = scCoPyAm;
          EAstat = 'A';
          EAtime = TimeTo6(systemTime());
          EAuser = q1User;
          EApgm  = q1Pgm;
          Exsr GetInv;
          Write pea00;
       Endif;
      //*** End Add ***** 06/24/24 **********************************
       //*** Begin Add *** 06/12/24 ****************************************
       // Write Payment Assessments to A/R
       For idx=1 to 7;
          If pCod(idx) <> ' ' and pAmt(idx) <> 0;
             Clear pea00;
             EAco#  = xxCo#;
             EAfnd  = xxFnd;
             EAfyr  = xxFyr;
             EAemp# = xxEmp#;
             EAdiv  = xxDiv;
             EAarCd = pCod(idx);
             EAarTy = GetARType(EAarCd);
             EAcTyp = GetARCat(EAarCd);
             EAdesc = GetARDesc(EAarCd);
             EAamt  = pAmt(idx);
             EAlate = ' ';
             EAtrDt = DateTo7(systemDate());
             EApoDt = dtCYMD(scPoDt);
             EAchk# = scChk#;

             // Get the invoice #
             ExSr GetInv;

             EAdt   = DateTo7(systemDate());
             EAtime = TimeTo6(systemTime());
             EAuser = q1User;
             EApgm  = q1Pgm;
             Write pea00;
          EndIf;
       EndFor;
       //*** End   Add *** 06/12/24 ****************************************
     C                   END
      *
      * "Bill to A/R" Late Fee
      *
     C     SCBLAT        IFEQ      'Y'
     C     RRLATE        ANDEQ     'Y'
     C                   CLEAR                   PEA00
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C                   Z-ADD     XXDIV         EADIV
     C                   MOVE      RRLTCD        EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVE      SCPLAT        EALATE
      *
     C                   MOVEL(P)  RRLTCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C  N60              MOVEL(P)  TTDESC        EADESC
     C                   MOVE      *BLANKS       EACTYP
      *
     C                   Z-ADD     RRLTAM        EAAMT
      *
     C                   ADD       1             DSTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EATRDT
      *
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EAPODT
      *
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     'S/R EDIT'    EAPGM
     C                   Z-ADD     DSTIME        EATIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         EADT
      *
     C                   Z-ADD     SCCHK#        EACHK#
      *
     C                   EXSR      GETINV
     C                   WRITE     PEA00
     C                   ENDIF                                                  SCPLAT

     C*
     C*
     C*  END THE PROGRAM
     C                   Endsr
       //***************************************************************************//


     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EANINV        #IINV#
     C*
     C     CLIENTID      IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      EAPODT        #IDT
     C     EANINV        PARM                    #IINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR


       //===================================================================
       // Self Reporting Error Checking
       //===================================================================
     C     SelfRptCheck  Begsr
     c*
     c     Datchk        Plist
     c                   Parm                    Srdti             8 0
     c                   Parm      *Blanks       Srffmt            8
     c                   Parm      *Blanks       Srtfmt            8
     c                   Parm                    Srdto            10
     c                   Parm                    Srerr             1
     c*
     c     Dtcymdp       Plist
     c                   Parm                    Srdti
     c                   Parm      *Blanks       Srffmt
     c                   Parm      '*CYMD'       Srtfmt
     c                   Parm                    Srdto
     c                   Parm                    Srerr
     c*
     c     Dtmdyyp       Plist
     c                   Parm                    Srdti
     c                   Parm      '*CYMD'       Srffmt
     C                   Parm      *Blanks       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
      *
     C                   Exsr      ClearSrRErr

      *   AMOUNT CANNOT BE LESS THAN ZERO
     c                   If        scEASRAmt < 0
     c                   eval      finSrError = *on
     c                   eval      holdSrerror = *on
     c                   eval      erSrAmount = *on
     c                   eval      msSrAmount = 'WCR0547'
     c                   Endif
     c                   Endsr

       //===================================================================
       // Self Reporting Calculation screen Error Checking
       //===================================================================
     C     SelfCalCheck  Begsr

     c* Calculation screen edits.

     c*  Billing flag must be 'Y' or 'N'.
     c*  If Posting a Bill, Edit Billing entry Fields
     c*
     c**** SCBLAR        IFNE      'Y'
     c**** SCBLAR        ANDNE     'N'
     c****               eval      finClError = *on
     c****               eval      erBillAr = *on
     c****               eval      msBillAr = 'WCR0501'
     c****               ENDIF
     c*
     c     SCBLAR        IFEQ      'Y'
     c     BtnCalSv      ANDEQ     *On
     c*
     C*  BILLING CODE MUST BE A 'B' TYPE AND ACTIVE
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     SCBLCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C     *IN60         IFEQ      '1'
     C     TTTYPE        ORNE      'B'
     C     TTSTAT        OREQ      'I'
     c                   eval      finClError = *on
     c                   eval      erArBillCd = *on
     c                   eval      msArBillCd = 'WCR0610'
     c                   LeaveSr
     c                   ENDIF
     c*
     c*  IF THE BILLING CODE DESCRIPTION IS BLANK LOAD THE DESCRIPTION
     c*  FROM THE TABLE FILE
     c*
     c     SCBLDS        IFEQ      *BLANKS
     c                   MOVEL     TTDESC        SCBLDS
     c                   ENDIF
     c     *LIKE         DEFINE    SCBLDS        XXBCDC
     c*
     c*  BILLING CODE MUST BE A 'F' TYPE, 'T' CATEGORY, AND ACTIVE
     c*
     c     SCTXBC        IFNE      *BLANKS
     c     SCTXBA        ORNE      *ZERO
     c*
     c                   MOVE      *BLANKS       X1KEY
     c                   MOVEL     SCTXBC        X1KEY
     c     keyWTABL      CHAIN     PTT01                              60
     c     *IN60         IFEQ      '1'
     c     TTTYPE        ORNE      'F'
     c     TTCTYP        ORNE      'T'
     c     TTSTAT        OREQ      'I'
     c                   eval      finClError = *on
     c                   eval      erTaxCode= *on
     c                   eval      msTaxCode  = 'WC30156'
     c                   LeaveSr
     C                   ENDIF
     C*
     C                   MOVEL(P)  TTDESC        XXBCDC
     C                   ENDIF                                                  SCTXBC IFNE
     C*
     C*  VALIDATE THE POSTING DATE
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     c                   If        SRERR = '1'
     c                   eval      finClError = *on
     c                   eval      erPostDt1  = *on
     c                   eval      msPostDt1  = 'WCR0500'
     c                   LeaveSr
     c                   Endif
     C*
     c**** SCBLAT        IFNE      'Y'
     c**** SCBLAT        ANDNE     *BLANK
     c****               eval      finClError = *on
     c****               eval      erLate1   = *on
     c****               eval      msLate1   = 'WCR0571'
     c****               LeaveSr
     c****               ENDIF                                                  SCBLAT IFNE
      *
      *  Display info window if billing transaction for this month
      *  has already been posted to the A/R.
      *
     c     X3AMT         IFNE      *ZERO
     c                   EXSR      INFWDW
     c                   ENDIF
      *
     C*
     C                   END
     C*
     C     SCPYAR        IFEQ      'Y'
     c     BtnCalSv      ANDEQ     *On
     c*  AMOUNT CANNOT BE LESS THAN OR EQUAL TO ZERO
     c     SCpyam        Iflt      0
     c                   eval      finClError = *on
     c                   eval      erPayAmt = *on
     c                   eval      msPayAmt = 'WCR0547'
     c                   LeaveSr
     c                   Endif
     c*
     c*  PAYMENT CODE MUST BE A 'P' TYPE AND ACTIVE
     c*
     c     scPyam        Ifgt      0
     c                   MOVE      *BLANKS       X1KEY
     c                   MOVEL     SCPYCD        X1KEY
     c     keyWTABL      CHAIN     PTT01                              60
     c     *IN60         IFEQ      '1'
     c     TTTYPE        ORNE      'P'
     c     TTSTAT        OREQ      'I'
     c                   eval      finClError = *on
     c                   eval      erPmtCode = *on
     c                   eval      msPmtCode  = 'WCR0629'
     c                   LeaveSr
     c                   ENDIF
     c                   Endif
     c*
     c*  IF THE PAYMENT CODE DESCRIPTION IS BLANK LOAD THE DESCRIPTION
     c*  FROM THE TABLE FILE
     c*
     c     SCPYDS        IFEQ      *BLANKS
     c                   MOVEL     TTDESC        SCPYDS
     c                   ENDIF
     c     *LIKE         DEFINE    SCPYDS        XXPCDC
     c*
     c*  TAX PAYMENT AMOUNT CANNOT BE LESS THAN ZERO
     c*
     c     SCTXPA        IFLT      0
     c                   eval      finClError = *on
     c                   eval      erTxAmount = *on
     c                   eval      msTxAmount = 'WCR0502'
     c                   LeaveSr
     c                   Endif
     c*
     c*  PAYMENT CODE MUST BE A 'Q' TYPE, 'T' CATEGORY, AND ACTIVE
     c*
     C     SCTXPC        IFNE      *BLANKS
     C     SCTXPA        ORNE      *ZERO
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     SCTXPC        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C     *IN60         IFEQ      '1'
     C     TTTYPE        ORNE      'Q'
     C     TTCTYP        ORNE      'T'
     C     TTSTAT        OREQ      'I'
     c                   eval      finClError = *on
     c                   eval      erPaidCde = *on
     c                   eval      msPaidCde = 'WC30157'
     c                   LeaveSr
     C                   Endif                                                  *IN60 EQ 1
     C*
     C                   MOVEL(P)  TTDESC        XXPCDC
     C                   ENDIF                                                  SCTXPC IFNE
     C*
     C*  VALIDATE THE POSTING DATE
     C*
     c                   Z-ADD     SCPODT        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   MOVEL     SRDTO         SCPODT
     c                   If        SRERR = '1'
     c                   eval      finClError = *on
     c                   eval      erPostDt1 = *on
     c                   eval      msPostDt1 = 'WCR0500'
     c                   LeaveSr
     c                   Endif
     c*
     c*  EMPLOYER MUST HAVE ASSIGNED AGENT FOR THE FUND YEAR EFFECTIVE
     c*  AS OF THE SYSTEM DATE
     c*
     C                   TIME                    DSDTTM
     c                   Z-ADD     DSDATE        SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMDP
     c                   MOVEL     SRDTO         XXPODT            6 0
     c     AHKY          SETGT     PAH00
     c                   READP     PAH00                                  55
     c     *IN55         DOWEQ     '0'
     c     AHSTAT        ANDEQ     'I'
     c                   READP     PAH00                                  55
     c                   END
     c     *IN55         IFEQ      '1'
     c     AHCO#         ORNE      XXCO#
     c     AHFND         ORNE      XXFND
     c     AHEMP#        ORNE      XXEMP#
     c     AHDIV         ORNE      XXDIV
     c     AHFYR         ORNE      XXFYR
     c                   eval      finClError = *on
     c                   eval      erAgentInf = *on
     c                   eval      msAgentInf = 'WCR0615'
     c                   eval      msPostDt1 = 'WCR0500'
     c                   ENDIF
     c*
     c                   ENDIF
       //*** Begin Add *** 06/12/24 ****************************************
       // Edit Assessment A/R codes
       For idx=1 to 7;
           // Load Assessment Index
           // Billing codes
           If (bCod(idx) <> ' ' or bAmt(idx) <> 0) And scBlAR = 'Y';
              arCode = bCod(idx);
              If ValidCode(arCode) = *Off;
                 finClError = *On;
                 bMsg(idx)  = 'WCR0619';
                 bErr(idx)  = *On;
              Else;
                 If GetARType(arCode) <> 'F';
                    finClError = *On;
                    bMsg(idx)  = 'WCR0619';
                    bErr(idx)  = *On;
                 Else;
                    If GetARCat(arCode) <> %EditC(idx:'X');
                       finClError = *On;
                       bMsg(idx)  = 'WCR0619';
                       bErr(idx)  = *On;
                    Else;
                       bMsg(idx)  = ' ';
                       bErr(idx)  = *Off;
                    EndIf;
                 EndIf;
              EndIf;
           Else;
              bMsg(idx)  = ' ';
              bErr(idx)  = *Off;
           EndIf;

           // Payment codes
           If (pCod(idx) <> ' ' or pamt(idx) <> 0) And scPyAR = 'Y';
              arCode = pCod(idx);
              If ValidCode(arCode) = *Off;
                 finClError = *On;
                 pMsg(idx)  = 'WCR0619';
                 pErr(idx)  = *On;
              Else;
                 If GetARType(arCode) <> 'Q';
                    finClError = *On;
                    pMsg(idx)  = 'WCR0619';
                    pErr(idx)  = *On;
                 Else;
                    If GetARCat(arCode) <> %EditC(idx:'X');
                       finClError = *On;
                       pMsg(idx)  = 'WCR0619';
                       pErr(idx)  = *On;
                    Else;
                       pMsg(idx)  = ' ';
                       pErr(idx)  = *Off;
                    EndIf;
                 EndIf;
              EndIf;
           Else;
              pMsg(idx)  = ' ';
              pErr(idx)  = *Off;
           EndIf;
       EndFor;
       //*** End   Add *** 06/12/24 ****************************************

     c                   Endsr                                                  SelfRptCheck


      *===============================================================
      *  INFWDW - DISPLAY WINDOW IF PREVIOUS BILLING EXISTS FOR PODT
      *===============================================================
     C     INFWDW        BEGSR
      *
     C                   Dou       BtnAdjCnl = *on or BtnAdjWrt = *on
     C                   Exfmt     RcdWmsg3
     C                   Enddo
      *
     C                   Eval      BtnAdjWrt = *Off
      *
     C                   Endsr                                                  INFWDW
       //===================================================================
       // Save Self Reorting Payroll Data.
       //===================================================================
     c     SelfRptSave   Begsr

      /free

       x = 0;
       DoU x = ##RRn;
           x = x + 1;
           Chain x sflSrAdEdt;

          kksrefdt = dtCYMD(scEASREfDt);

          Select;
          When ##schtyp = 'W';
             kktodt =  DTCymd(inSrdate);
             Chain keyWASRL13 PSR13;
          When ##schtyp <> 'W';
             kkrpmy = DT5Cymd(inSrDate);
             Chain keyWASRL12 PSR12;
          Endsl;

           If %found;
              SRamtp = scEASRAmt;
              GetSrManPrem();
              SRpgm  = Q1pgm;
              SRuser = Q1user;
              SRtime = TimeTo6(SystemTime());
              SRdt   = DateTo7(SystemDate());
              If ##schtyp <> 'W';
                 Update PSR12;
              Else;
                 Update PSR13;
              Endif;
           Else;
              Clear PSR0P;
              SRco#  = ELco#;
              SRfnd  = ELfnd;
              SRemp# = ELemp#;
              SRdiv  = ELdiv;
              SRfyr  = ELfyr;
              Select;
              When ##schtyp = 'W';
                 SRtodt = DTCymd(inSrDate);
              When ##schtyp <> 'W';
                 SRrpmy = dt5CYMD(inSrDate);
              Endsl;
              SRipgm = Q1pgm;
              SRpgm  = Q1pgm;
              SRiusr = Q1user;
              SRuser = Q1user;
              SRitm  = TimeTo6(SystemTime());
              SRtime = TimeTo6(SystemTime());
              SRidt  = DateTo7(SystemDate());
              SRdt   = DateTo7(SystemDate());
              SRefdt = dtCYMD(scEASREfDt);
              SRamtp = scEASRAmt;
              SRclas = scEASrCLAS;
              SRspec = scEASpec;
              SRext =  scEASrRExt;
              GetSrManPrem();
              If %parms >= 6;
                 SRstat = inSrLateRpt;
              Endif;
              Write PSR0P;
           Endif;
B02        Chain keyWMEPL1E PEP01E;
           If %found;
              xxamt = scEASRAmt - svEASRAmt;
              EPsapr = EPsapr + xxamt;
              Exsr UPDTEP;
              Update PEP01E;
           Endif;

       Enddo;

      /end-free
     C                   Endsr                                                  SelfRptSave

       //===================================================================
       // Calculate S/R Premium
       //===================================================================
     c     SrCalculation Begsr

     c*   Initialize Work Fields
     c                   Z-ADD     0             XXAMT3           15 4
     c                   Z-ADD     0             XXFACT           11 9
     c                   MOVE      ' '           XXPLV1            1
     c                   MOVE      ' '           XXPLV2            1
     c                   Z-ADD     0             XXWFAC           11 9
     c                   Z-ADD     0             XXWVFL            1 0
     c                   Z-ADD     0             xxPRCT            5 4
     c                   Z-ADD     0             xxPRC2            5 4
     c                   Z-ADD     0             xxPCT             5 4
     c                   Z-ADD     0             xxWK92           11 2
     c                   CLEAR                   manPremium
     c                   Clear                   terrPayroll
     c                   CLEAR                   AddLoad          11 0
     c                   MOVE      '0'           *IN93

     c     RSKY          CHAIN     PRS00                              60
     c                   MOVE      RSJRST        XXST
     c     RRKY          SETGT     PRR00
     c     RRKY2         READPE    PRR00                                  60

     c*  Initialize Screen Arrays
     c                   CLEAR                   HDG
     c                   CLEAR                   TAM

     c     ##schtyp      IFNE      'W'
     c     SRKY1         CHAIN     PSR01                              60
     c                   ELSE
     c     SRKY3         CHAIN     PSR08                              60
     c                   ENDIF

     c     *IN60         DOWEQ     '0'
     c     SRSTAT        IFEQ      'L'
     c                   MOVE      '1'           *IN93
     c                   END

     c*   total man premium
     c                   Eval      xxType = 'P'
     c                   Clear                   x4EfDt
     c                   Eval      x4EfDt = SrEffd(xxCO#:xxFND:xxFYR:xxEMP#:
     c                              xxDIV:SRefdt:xxType:SRclas)

     c                   Eval      manPremium = manPremium + getManPrem
     c                   Eval(h)   AddLoad = AddLoad + SRadld

     c     ##schtyp      IFNE      'W'
     c     SRKY1         READE     PSR01                                  60
     c                   ELSE
     c     SRKY3         READE     PSR08                                  60
     c                   ENDIF

     c                   END

     c*  Load Screen Arrays
     c                   Z-ADD     1             A                 3 0          MANUAL PREM

      *******************************************************************************
     c                   If        elRrn > 0
     c                   Move      elRRN         @@RRN
     c                   ElseIf    dbrrn > 0
     c                   Move      dbRrn         @@RRN
     c                   Endif
     c                   Clear                   @@INFO
     c                   Clear                   Q1INFO
     c                   If        ##schtyp = 'W'
     c                   Clear                   @@TODT            8 0
     c                   Eval      @@TODT =  ##srdate
     c                   Else
     c                   Clear                   @@RPMY            6 0
     c                   Eval      @@RPMY = dt6MDYY(xxyymm)
     c                   Endif
      * If LSAT flags as a late report, send info to SRPREM so that stock discount
      * will not be calculated for the self report.
     c                   if        *in93
     c                   eval      @@fact = 'L'
     c                   else
     c                   eval      @@fact = *blanks
     c                   endif
       //*** Begin Add *** 06/12/24 **********************************
       // Inital fields
       Clear dsAssessment;
       Clear dsPriorBilled;
       @@asmnt = dsAssessment;
       @@PrvAsmnt = dsPriorBilled;
       //*** End   Add *** 06/12/24 **********************************
     c
     c                   Call      'SRPREM'
     c                   Parm                    @@RRN             9 0
     c                   Parm                    @@RPMY
     c                   Parm                    @@TODT
     c                   Parm      manPremium    @@MAPR           11 0
     c                   Parm      AddLoad       @@ADLD           11 0
     c                   Parm      terrPayroll   @@PAYR           11 0
     c                   Parm      0             @@PREM           11 0
     c                   Parm      0             @@TAX            11 0
     c                   Parm      0             @@PPRM           11 0
     c                   Parm      0             @@PTAX           11 0
     c                   Parm                    @@FACT          143
     c                   Parm      0             @@PREV           11 2
     c                   Parm      ' '           @@SRON            1
     c                   Parm                    @@INFO           39
     C                   PARM                    @@HDG           270
     C                   PARM                    @@PRO           100
     C                   PARM                    @@PREM2          11 2
     C                   PARM                    @@TAX2           11 2
     C                   PARM                    @@PPRM2          11 2
     C                   PARM                    @@PTAX2          11 2
     C                   PARM                    @@FACT2          48
     C*** Begin Add *** 06/12/24 *******************************************
     C                   PARM                    @@ASMNT         267
     C                   PARM                    @@PRVASMNT       77
     C*** End   Add *** 06/12/24 *******************************************
     C*** Beg Add ***** 06/24/24 *******************************************
     C                   PARM                    @@COLB           11 0
     C                   PARM                    @@PRCOLB         11 0
     C*** End Add ***** 06/24/24 *******************************************

      /Free
       Q2Fact = @@Fact2;
      /End-Free
     c
     c                   Clear                   Q1FACT
     c                   Move      @@FACT        Q1FACT
      /free
       isContribution = GetPrmContflg(ELco#:ELfnd);
       If isContribution;
          flxCalcTit = 'Calculated Self Reported Contribution';
       Else;
          flxCalcTit = 'Calculated Self Reported Premium';
       Endif;
       //*** Begin Add *** 06/12/24 ****************************************
       // Load Assessment Data Structures and window variables
       dsAssessment    = @@Asmnt;
       dsPriorBilled   = @@PrvAsmnt;
       posBill         = 350;
       posButtons      = 480;
       hgtWindow       = 430;
       //*** Beg Add ***** 10/03/25 ***************************************
       If feeVisb = *On or ColVisb = *On;      // any one visible
          posButtons   = 500;
          hgtWindow    = 450;
       Endif;
       If feeVisb = *On and ColVisb = *On;     // both visible
          posButtons   = 520;
          hgtWindow    = 470;
       Endif;
       //*** End Add ***** 10/03/25 ***************************************
       //*** Beg Add ***** 06/24/24 ***************************************
       If ELcolrp <> 0;
          If feeVisb = *Off;
             hgtWindow = 410;
             posButtons = 520;
          Else;
             hgtWindow = 490;
             posButtons = 540;
          Endif;
       Endif;
       //*** End Add ***** 06/24/24 ***************************************
       visSRAs         = *Off;
       isSRassessments = *Off;
       If dsAssessment.chrPrintAss = 'Y';
          isSRassessments = *On;
          posSRAs         = 350;
          posBill         = 490;
          posSRAsB        = 610;
          posButtons      = 765;
          hgtWindow       = 710;
          //*** Beg Add ***** 06/24/24 ***************************************
          If feeVisb = *On or ColVisb = *On;
             posSRAsB        = 630;
             posButtons      = 785;
             hgtWindow       = 730;
          Endif;
          If feeVisb = *On and ColVisb = *On;
             posSRAsB        = 650;
             posButtons      = 805;
             hgtWindow       = 750;
          Endif;
          //*** End Add ***** 06/24/24 ***************************************
          visSRAs         = *On;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
      /end-free
     c
     c***  Begin Chg  ***  01/19/22  *****************
     c**********         If        not (F1netr = 'Y' and ELpfdt >= F1netd)
     c                   If        not IsUseNetRate(ELco#:ELfnd:ELemp#:ELdiv
     c                              :ELFyr)
     c***  End   Chg  ***  01/19/22  *****************
     c
     c* manual premium
     c                   If        isContribution
     c                   Eval      hdg(A) = con(12)
     c                   Else
     c                   Eval      hdg(A) = con(1)
     c                   Endif
     c                   Z-Add     manPremium    tam(A)
     c                   Eval      A = A + 1
     c* Increased Limit
     c                   Clear                   TMP
     c                   MoveA     Con(A)        TMP
     c                   Move      X'22'         TMP(30)
     c                   Move      '%'           TMP(37)
     c                   Move      '.'           TMP(33)
     c                   Move      Q1lmpc        XXasp1            5
     c                   MoveA     XXasp1        NUM
     c     NUM(1)        IFNE      '0'
     c                   MOVE      NUM(1)        TMP(31)
     c                   END
     c     NUM(2)        IFNE      '0'
     c     NUM(1)        ORGT      '0'
     c                   MOVE      NUM(2)        TMP(32)
     c                   END
     c                   MOVE      NUM(3)        TMP(34)
     c                   MOVE      NUM(4)        TMP(35)
     c                   MOVE      NUM(5)        TMP(36)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c* Subtotal
     c                   ADD       1             A
     c                   MOVEL     CON(A)        HDG(A)
     c                   Z-ADD     Q1TOT1        TAM(A)
      /Free
       If clientID = '150';
          tam(a) = Q2tot1;
       Endif;
      /End-Free
     c* Discount Factor Before Modifier
     c                   ADD       1             A
     c                   MOVEL     CON(A)        HDG(A)
      /free
       If ClientId = '025' ;
          xxTa11 = %dec(Q1dscb:8:6);
       Else;
      /End-free
     c                   Z-ADD     Q1DSCB        TAM(A)
      /free
       Endif;
      /End-free
     c* Subtotal
     c                   ADD       1             A
     c                   MOVEL     CON(A)        HDG(A)
     c                   Z-ADD     Q1TOT2        TAM(A)
      /Free
       If clientID = '150';
          tam(a) = Q2tot2;
       Endif;
      /End-Free
     c* Experience Modifier
     c                   ADD       1             A
     c                   IF        ELexm1 <> *zeros
     c                   MOVEL     CON(A)        HDG(A)
     c                   Z-ADD     Q1EXPM        TAM(A)
     c                   ENDIF
     c* Subtotal
     c                   ADD       1             A
     c                   MOVEL     CON(A)        HDG(A)
     c                   Z-ADD     Q1TOT3        TAM(A)
      /Free
       If clientID = '150';
          tam(a) = Q2tot3;
       Endif;
      /End-Free
     c* Discount Factor After Modifier
     c                   ADD       1             A
     c                   MOVEL     CON(A)        HDG(A)
      /free
       If ClientId = '025' ;
          scTa12 = %dec(Q1dsca:8:6);
       Else;
      /End-free
     c                   Z-ADD     Q1DSCA        SCTA08
      /free
       Endif;
      /End-free
     c                   Z-ADD     Q1WAIV        SCWVAM
     c                   Z-ADD     Q1REIN        SCARAM
     c*                  Z-ADD     Q1TOT4        xxRnd            11 0
     c*                  Eval      xxRnd = xxRnd + Q1WAIV + Q1REIN
     c
     c                   Else
     c                   Z-Add     8             A
     c                   If        not isContribution
     c                   Movel     CON(11)       HDG(A)
     c                   Else
     c                   Movel     CON(14)       HDG(A)                         CLT025
     c                   Endif
     c                   Clear                   SCNETP
     c                   Eval      SCNETP = Q1NETP
      *** Beg Add ***** 10/08/25 *********************************
     c                   Z-ADD     Q1NWAV        SCWVAM
      *** End Add ***** 10/08/25 *********************************
     c                   Endif
     c                   Eval      A = A + 1
     c                   Z-Add     Q1TERR        SCTERR
     C*** begin add ***  09/21/22  ***********************************************
      *** Beg Del ***** 10/08/25 *********************************
     c***                Z-ADD     Q1NWAV        SCWVAM
      *** End Del ***** 10/08/25 *********************************
     C*** end add *****  09/21/22  ***********************************************
     c
     c                   Movel(P)  'Foreign'     XXTERH
     c                   Cat       'Terror':1    XXTERH
     c                   Cat       'ism:':0      XXTERH
     c*
     c     RRTERH        IFNE      *BLANKS
     c                   MOVEL(P)  RRTERH        XXTERH
     c                   CAT       ':':0         XXTERH
     c                   ENDIF
     c*  PREMIUM DUE

     c                   If        not isContribution
     c                   Movel     CON(A)        HDG(A)
     c                   Else
     c                   Movel     CON(13)       HDG(A)                         CLT025
     c                   Endif
     c                   Z-Add     @@PREM        TAM(A)
      /Free
       If clientID = '150';
          tam(a) = @@prem2;
       Endif;
      /End-Free
     c                   Eval      A = A + 1
     c                   MOVE      *OFF          prvsvisb
     c                   Z-ADD     @@PPRM        SCPRVB
      /Free
       If clientID = '150';
          scPrvB = @@pprm2;
       Endif;
      /End-Free
      *** Beg Add ***** 10/03/25 ***************************************
      * Check for Installment fee written previously with same posting date
     C                   EXSR      GETPBL
      *** End Add ***** 10/03/25 ***************************************
     c                   MOVE      *OFF          reinsvisb
     c                   MOVE      *OFF          fornvisb
     c                   MOVE      *OFF          wavrvisb
     c     ELEWAV        IFNE      *ZERO
     c     ELWVPC        ANDEQ     *ZERO
     c     ELEWAV        ORNE      *ZERO
     c     ELWVTP        ANDEQ     'S'
     c                   MOVE      *ON           wavrvisb
     c                   ENDIF
     c     ELEARC        IFNE      *ZERO
     c     ELARCP        ANDEQ     *ZERO
     c                   MOVE      *ON           reinsvisb
     c                   ENDIF
     c     SCTERR        IFNE      0
     c                   MOVE      *ON           fornvisb
     c                   ENDIF
     C*** Begin Chg *** 06/12/24 *******************************************
     c*****SCPRVB        IFNE      0
     c**********         MOVE      *ON           prvsvisb
     c**********         ENDIF
     c                   MOVE      *ON           prvsvisb
     C*** End   Chg *** 06/12/24 *******************************************
     c     @@PREM        SUB       SCPRVB        XXBADJ                         BILLING ADJ
      /Free
       If clientID = '150';
          xxBAdj = @@prem2 - scPrvB;
       Endif;
      /End-Free
     c     XXBADJ        IFGT      *ZERO
     c                   Z-ADD     XXBADJ        SCPYAM
     c                   ELSE
     c                   CLEAR                   SCPYAM
     c                   ENDIF
     c     @@TAX         SUB       @@PTAX        SCTXBA
     c                   Z-Add     @@TAX         TAM(A)
      /Free
       If clientID = '150';
          scTxBa = @@Tax2 - @@PTax2;
          tam(a) = @@Tax2;
       Endif;
      /End-Free
     c     SCTXBA        IFGT      *ZERO
     c                   Z-ADD     SCTXBA        SCTXPA
     c                   ELSE
     c                   CLEAR                   SCTXPA
     c                   CLEAR                   SCTXBA
     c                   ENDIF
      //*** Beg Add ***** 06/24/24 **********************************
       If ELcolrp <> 0;
          scCoBlAm = @@Colb - @@PrColb;
          Chain(n) (ELco#:ELfnd) pex00;
          scCoBlcd = Excolb;
          scCoPycd = Excolp;

          If scCoBlAm > 0;
             scCoPyAm = scCoBlAm;
          Else;
             //***scCoBlAm = 0;
             scCoPyAm = 0;
          Endif;
       Endif;
      //*** End Add ***** 06/24/24 **********************************
     c     RRTXHD        IFNE      *BLANKS
     c                   MOVEA     RRTXHD        TMP
     c                   ELSE
     c                   MOVEA     CON(10)       TMP
     c                   ENDIF                                                  RRTXHD IFNE
     c                   MOVE      X'22'         TMP(21)
     c                   MOVE      '%'           TMP(29)
     c                   MOVE      '.'           TMP(24)
     c                   MOVE      RRTXPC        X6ASP1            6
     c                   MOVEA     X6ASP1        NUM6

     c     NUM6(1)       IFNE      '0'
     c                   MOVE      NUM6(1)       TMP(22)
     c                   ENDIF                                                  NUM6,1 IFNE

     c     NUM6(2)       IFNE      '0'
     c     NUM6(1)       ORGT      '0'
     c                   MOVE      NUM6(2)       TMP(23)
     c                   ENDIF                                                  NUM6,2 IFNE

     c                   MOVE      NUM6(3)       TMP(25)
     c                   MOVE      NUM6(4)       TMP(26)
     c                   MOVE      NUM6(5)       TMP(27)
     c                   MOVE      NUM6(6)       TMP(28)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP

     c                   MOVE      @@INFO        Q1INFO
     c                   MOVE      XXPLV1        XXPLV2            1
      *******************************************************************************

     c                   Z-ADD     XXBADJ        SCBLAM

     c                   MOVE      *BLANKS       SCTXPC
     c                   MOVE      'Y'           SCBLAR
     c                   MOVE      'N'           SCPYAR
     c                   MOVE      *BLANKS       SCPYCD
     c                   MOVE      *BLANKS       SCBLDS
     c                   MOVE      *BLANKS       SCPYDS
     c
     c* CHECK IF DATE IS FEB. 29 AND LEAP YEAR
     c
     c                   Z-ADD     XXRPDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTMDYYP
     c                   MOVEL     SRDTO         X1RPDT            8 0

     c                   Z-ADD     X1RPDT        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   MOVEL     SRDTO         X1RPDT

     c     SRERR         IFEQ      *ON
     c                   SUB       1             XXDAY
     c                   ENDIF                                                  SRERR=ON

     c                   Z-ADD     XXRPDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTMDYYP
     c                   MOVEL     SRDTO         SCPODT
     c                   Z-ADD     0             SCCHK#

     c                   eval      count = 1
     c                   Dow       count <= 10
     c                   Select
     c                   When      count = 1
     c                   eval      schd01 = hdg(count)
     c                   eval      scta01 = tam(count)
     c                   When      count = 2
     c                   eval      schd02 = hdg(count)
     c                   eval      scta02 = tam(count)
     c                   When      count = 3
     c                   eval      schd03 = hdg(count)
     c                   eval      scta03 = tam(count)
     c                   When      count = 4
     c                   eval      schd04 = hdg(count)
      /free
       If ClientId = '025' ;
          scTa11 = xxTa11;
       Else;
      /End-free
     c                   eval      scta04 = tam(count)
      /free
       Endif;
      /End-free
     c                   When      count = 5
     c                   eval      schd05 = hdg(count)
     c                   eval      scta05 = tam(count)
     c                   When      count = 6
     c                   eval      schd06 = hdg(count)
     c                   eval      scta06 = tam(count)
     c                   When      count = 7
     c                   eval      schd07 = hdg(count)
     c                   eval      scta07 = tam(count)
     c                   When      count = 8
     c                   eval      schd08 = hdg(count)
     c                   When      count = 9
     c                   eval      schd09 = hdg(count)
     c                   eval      scta09 = tam(count)
     c                   When      count = 10
     c                   eval      schd10 = hdg(count)
     c                   eval      scta10 = tam(count)
     c                   Endsl
     c                   eval      count = count + 1
     c                   Enddo

       //*** Begin Add *** 06/12/24 ****************************************
       // Load Assessments if indicated to do so
       Clear aHdg;
       Clear aAAmt;
       Clear aCAmt;
       Clear aPAmt;
       Clear bAmt;
       Clear bCod;
       Clear pAmt;
       Clear pCod;
       If isSRAssessments;
          For idx=1 to 7;
             If dsAssessment.chrAssessmentHdg(idx) <> ' ';
                chrAssHdg = dsAssessment.chrAssessmentHdg(idx);
                chrAssPct = %EditC(dsAssessment.decAssessmentPct(idx):'4') +
                            '%';
                aHdg(idx) = chrAssHdg + chrAssPct;

                aCAmt(idx) = dsAssessment.decAssessmentAmt(idx);
                aPAmt(idx) = dsPriorBilled.decPriorBilledAmt(idx);
                aAAmt(idx) = dsAssessment.decAssessmentAmt(idx) -
                             dsPriorBilled.decPriorBilledAmt(idx);

                If dsAssessment.chrAssessment#(idx) <> ' ' And
                   dsAssessment.chrAssessment#(idx) <> '0';
                   decAssIdx       = %Dec(dsAssessment.chrAssessment#(idx):1:0);
                   bAmt(decAssIdx) = dsAssessment.decAssessmentAmt(idx) -
                                     dsPriorBilled.decPriorBilledAmt(idx);
                   pAmt(decAssIdx) = dsAssessment.decAssessmentAmt(idx) -
                                     dsPriorBilled.decPriorBilledAmt(idx);
                   bCod(decAssIdx) =
                      GetDefaultArCode('F':%EditC(decAssIdx:'X'));
                   pCod(decAssIdx) =
                      GetDefaultArCode('Q':%EditC(decAssIdx:'X'));
                EndIf;
             Else;
                Leave;
             EndIf;
          EndFor;

          // Fill in unfilled assessment codes
          For idx=1 to 7;
             If bCod(idx) = ' ';
                bCod(idx) = GetDefaultArCode('F':%EditC(idx:'X'));
             EndIf;
             If pCod(idx) = ' ';
                pCod(idx) = GetDefaultArCode('Q':%EditC(idx:'X'));
             EndIf;
          EndFor;

          // Set visibility of assessment fields
          For idx=1 to 7;
             aVis(idx) = *On;
             If aHdg(idx) = ' ';
                aVis(idx) = *Off;
             EndIf;
          EndFor;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
     c
     C                   Endsr
      ***
       //===================================================================
       //===================================================================
     C*  SUBROUTINE TO PRORATE PREMIUM FACTORS
     C*
     C     SRPRO         BEGSR
     C*
     C                   MOVE      *ON           *IN74
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   ADD       1             QQDAYS
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         XQTO
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     QQERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PRORATE AGAINST THE NUMBER OF DAYS IN THE MONTH IF RPT FREQ NOT WEEKLY
     C*
     C     ##schtyp      IFNE      'W'
     C                   Z-ADD     XXRPMN        X                 3 0
     C                   Z-ADD     MD(X)         XXDAYS            5 0
     C     XXRPMN        IFEQ      2
     C     XXRPYR        DIV       4             XXDIVR            2 0
     C                   MVR                     XXREM             2 0
     C     XXREM         IFNE      *ZERO
     C                   Z-ADD     28            XXDAYS
     C                   ENDIF
     C                   ENDIF
     C*
     c                   eval      ELpfdt_1 = ELpfdt
     C     XXRPMN        IFEQ      XXFPMO
     C     XXRPYR        ANDEQ     XXFPYR
     C                   SUB       XXFPDY        XXDAYS
     C                   ADD       1             XXDAYS
     C*
     C     XXDAYS        IFEQ      *ZERO
     C                   ADD       1             XXDAYS
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXRPMN = XXFPMO
     C*                                                AND XXRPYR = XXFPYR
     c                   eval      ELptdt_1 = ELptdt
     C     XXRPMN        IFEQ      XXTPMO
     C     XXRPYR        ANDEQ     XXTPYR
     C                   Z-ADD     XXTPDY        XXDAYS
     C                   ENDIF                                                  XXRPMN = XXTPMO
     C*                                                AND XXRPYR = XXTPYR
     C                   ENDIF
     C     QQDAYS        DIV       XXDAYS        XXPRO            11 9
     C*
     C                   ENDSR
       //===================================================================
       //===================================================================
     C     CALDBR        BEGSR
     C*  SCHEDULE DB/CR
     C*
     C     ELDBCR        IFNE      *ZERO
     C*
     C     ELSDEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELSDEF        XXILEF
     C                   ENDIF
     C*
     C     ELSDEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELSDEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDNE     ELSDEF
     C     ELSDEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   SUB       1             ELDBCR
     C                   MULT(H)   XXPRO         ELDBCR
     C                   ADD       1             ELDBCR
     C*
     C                   ENDIF
     C*
     C                   MULT(H)   ELDBCR        XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELDBCR        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     clientID      IFEQ      '136'
     C     ELCMUL        ANDNE     *ZERO
     C                   MULT(H)   ELCMUL        XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELCMUL        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     XXFACT        SVFACT           11 9
     C*
     C                   ENDSR

      *============================================================
      * APPLY LARGE DEDUCTIBLE DISCOUNT
      *============================================================
     C     APLGDD        BEGSR
      *
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C     LDPLV1        ANDEQ     XXPLV1
     C*
     C     LDEFD1        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD1        XXILEF
     C                   ENDIF
     C*
     C     LDEXP1        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDNE     LDEFD1
     C     LDEXP1        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT1
     C*
     C                   ENDIF
     C*
     C     LDPCT1        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C     LDPLV2        ANDEQ     XXPLV2
     C*
     C     LDEFD2        IFEQ      *ZERO
     C                   MOVE      LDEXP1        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD2        XXILEF
     C                   ENDIF
     C*
     C     LDEXP2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDNE     LDEFD2
     C     LDEXP2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT2
     C*
     C                   ENDIF
     C*
     C     LDPCT2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   ENDSR

      *================================================================
      * GETPBL - Get 'B' type records written previously with same posting date
      *================================================================
     C     GETPBL        BEGSR
      *
     C                   Z-ADD     XXCO#         X3CO#
     C                   Z-ADD     XXFND         X3FND
     C                   Z-ADD     XXEMP#        X3EMP#
     C                   Z-ADD     XXDIV         X3DIV
     C                   Z-ADD     XXFYR         X3FYR
     C                   MOVE      'B'           X3ARTY
      *
     C                   Z-ADD     XXRPDT        X3PODT
      *
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         X3DIV
     C                   ENDIF
      *
     C                   Z-ADD     *ZERO         X3AMT            15 5
     C     EAKY          CHAIN     PEA04                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'C'
     C     TTCTYP        ANDNE     'E'
     C     ELARSM        IFEQ      'Y'
     C     EARDIV        ANDEQ     XXDIV
     C     ELARSM        ORNE      'Y'
     C*
     C                   MOVEL(P)  EAARCD        X1KEY
     C     keyWTABL      CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     EAARCD        ANDEQ     SCBLCD
     C                   ADD       EAAMT         X3AMT
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  ELARSM IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     EAKY          READE     PEA04                                  60
     C                   ENDDO                                                  *IN60
      *
     C*** Begin Chg *** 06/12/24 *******************************************
     C*****X3AMT         IFNE      0
     C**********         MOVE      *ON           prvsvisb
     C**********         ENDIF
     C                   MOVE      *ON           prvsvisb
     C*** End   Chg *** 06/12/24 *******************************************
      *
      *
      *  CHECK TO SEE IF AN INSTALLMENT FEE HAS ALREADY BEEN BILLED. IF SO,
      *  WRITE AN ADJUSTMENT. IF NO ADJUSTMNET IS TO BE WRITTEN, CONDITION
      *  THE DISPLAY SO THAT IT DOESN'T SHOW THE INSTALLMENT FEES BY TURNING
      *  *IN64 *OFF
      *
     C     SCISFE        IFNE      *ZERO
     C     ELIFFL        ANDEQ     *BLANKS
     C                   Z-ADD     *ZERO         XXISFE
     C                   MOVE      'F'           X3ARTY
     C     EAKY          CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C     ELARSM        IFEQ      'Y'
     C     EARDIV        ANDEQ     XXDIV
     C     ELARSM        ORNE      'Y'
     C     EAARCD        IFEQ      SCIFBL
     C                   ADD       EAAMT         XXISFE            7 2
     C                   ENDIF
     C                   ENDIF
     C     EAKY          READE     PEA04                                  60
     C                   ENDDO                                                  *IN60
     C     XXISFE        IFGE      SCISFE
     C                   Z-ADD     *ZERO         SCISFE
     C                   ELSE
     C                   SUB       XXISFE        SCISFE
     C                   ENDIF
      *** Beg Chg ***** 10/03/25 ***************************************
     C*****SCISFE        IFEQ      *ZERO
     C*****              MOVE      *OFF          feeVisb
     C*****              ELSE
     C*****              MOVE      *ON           feeVisb
     C*****              Z-ADD     SCISFE        SCISFP
     C*****              ENDIF
      *
     C                   MOVE      *ON           feeVisb
     C                   Z-ADD     SCISFE        SCISFP
      *** End Chg ***** 10/03/25 ***************************************
     C                   ENDIF
      *
     C                   ENDSR                                                  CKPBAL
       //===================================================================
       // Clear all S/R Errors
       //===================================================================
     C     ClearSrRErr   Begsr
      /free

       finSrError = *off;
       finClError = *off;

       erSrAmount = *off;
       msSrAmount = *blanks;
       erArBillCd = *off;
       msArBillCd = *blanks;
       erTaxCode= *off;
       msTaxCode  = *blanks;
       erPostDt1  = *off;
       msPostDt1  = *blanks;
       erPayAmt = *off;
       msPayAmt = *blanks;
       erPmtCode = *off;
       msPmtCode  = *blanks;
       erTxAmount = *off;
       msTxAmount = *blanks;
       erPaidCde = *off;
       msPaidCde = *blanks;
       erAgentInf = *off;
       msAgentInf = *blanks;

       //*** Begin Add *** 06/12/24 ****************************************
       Clear bMsg;
       Clear pMsg;
       For idx=1 to 7;
          bErr(idx) = *Off;
          pErr(idx) = *Off;
       EndFor;
       //*** End   Add *** 06/12/24 ****************************************

      /end-free
     C                   Endsr                                                  ClearErrors

       //===================================================================
       // SUBROUTINE FOR Calculating S/R Prem Payrolls.
       //===================================================================
     C     SelfRptPrm    Begsr
      /free

       sed = 0;
       scs = ' ';
       sds = ' ';
       spy = 0;
       srt = 0;
       srtn = 0;
       sam = 0;
       y = 0;
       scadld = 0;

       If ##schtyp <> 'W';
          xxyymm = dt5CYMD(insrDate);
          xxyymm_1 = xxyymm;
          xxday = md(x1);
          If xxrpmn = 2;
             xxdivr = xxrpyr/4;
             xxrem = %REM( xxrpyr : 4);
             If xxrem <> 0;
                xxday = 28;
             Endif;
          Endif;
          xxyymm_2 = xxyymm;
          xxday_2  = 01;
       Else;
          xxrpdt = DTCymd(##srDate);
       Endif;

       //** SET REPORT DATE EQUAL TO POLICY EXPIRATION DATE IF POLICY
       //** EXPIRES MID-MONTH.
       If xxrpdt > Elptdt;
          xxrpdt = ELptdt;
       Endif;

       If ELandt <> 0 and ELandt <> ELpfdt;
          xxandt = ELandt;
          xxanyr = xxanyr + 1;
       Else;
          xxandt = 0;
       Endif;

       xxCo# = ELco#;
       xxFnd = ELfnd;
       xxFyr = ELfyr;
       xxEmp#= ELemp#;
       xxDiv = ELdiv;
       xxrtcd = Elrtcd;

       Chain RSKY PRS00;
       xxst = RSjrst;
       x2efdt = ELpfdt;
       Setgt  RRKY  PIF01;
       Readpe RRKY2 PIF01;
       If Not %Eof;
          //*** Beg Add ***** 09/03/25 ********************************
          Chain (ELco#:%editc(ELbscd:'X')) pbs01;
          //*** End Add ***** 09/03/25 ********************************
       //*** begin chg ***  03/07/22  *********************************
       //*****   If IFISFE <> 0;
          //*** Beg Chg ***** 09/03/25 ********************************
          //***If IFISFE <> 0 and ELIFFL = ' ';
          If IFISFE <> 0 and ELIFFL = ' ' and BSonli <> 'Y';
          //*** End Chg ***** 09/03/25 ********************************
       //*** end chg *****  03/07/22  *********************************
             scisfe = IFISFE;
             scisfp = IFISFE;
             scifbl = IFIFBL;
             scifpd = IFIFPD;
             feevisb = *on;
          Else;
             scisfe = 0;
             scisfp = 0;
             scifbl = ' ';
             scifpd = ' ';
             feevisb = *off;
          Endif;
       Endif;

       //*** Beg Add ***** 06/24/24 ***********************************
       If ELcolrp <> 0;
          ColVisb = *On;
          posButtons = 500;
          If FeeVisb = *Off;
             ColTop = 130;
          Else;
             ColTop = 150;
          Endif;
       Else;
          ColVisb = *Off;
       Endif;
       //*** End Add ***** 06/24/24 ***********************************

       If ELarsm = 'Y';
          Chain EMKY1 PEM00;
          If %Found(WMEML);
             DoW Not %Eof(WMEML);
                 If EMstrp = 'Y';
                    xsdiv = EMdiv;
                    Leave;
                 Endif;
                 Reade EMKY1 PEM00;
             Enddo;
          EndIf;
       Endif;

       xsKey = 'WC';
       Chain xsKey SWC01;
       vndrFl = WCFL23 = 'Y';

       lrgdfl = ' ';
       Clear ldModule;
       If WCFL43 = 'Y';
          ldModule = *on;
          x1key = %editc(elbscd : 'X');
          Chain keyWTABL PBS01;
          If BSLPGM = 'L';
             Chain(n) keyWMLDL1 PLD01;
             If %found;
                LRGDFL = 'Y';
             Endif;
          Endif;
       Endif;

       //* Default A/R Billing code
       Chain F1KY PF100;
       If %found;
          scblcd = F1srbb;
       Endif;
       //*** begin add *** 10/05/21  ***************************************
       //***  Begin Chg  ***  01/19/22  *****************
       //isNetRate = *off;
       //If %found and F1NetR = 'Y' and ELPfdt >= F1NetD;
       //   isNetRate = *on;
       //Endif;
       isNetRate = IsUseNetRate(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
       //***  End   Chg  ***  01/19/22  *****************
       //*** end add ***** 10/05/21  ***************************************

       // Default A/R tax billing code
       sctxbc = ' ';
       found = *off;
       Chain TBKY1 PTT01;
       If %Found(WTABL);
          DoU %eof(WTABL);
              If TTctyp = 'T' and TTstat = 'A' and
                 TTtype = 'F';
                 xxST = RSjrst;
                 Setgt RRky wdrrl;
                 Readpe(N) RRky2 wdrrl;
                 If Not %Eof (wdrrl);
                    If RRtxpc <> *Zeros;
                       sctxbc = TTkey;
                    Endif;
                 Endif;
                 Leave;
              Endif;
              Reade TBKY1 PTT01;
          Enddo;
       EndIf;


       Exsr DaysPrem;

       xxrpmy = xxyymm;

         // Populate the subfile
       Setll keyWMEPLE PEP0E;
       Reade keyWMEPLE PEP0E;
       Dow not %eof;
           If EPefdt <= xxrpdt;
              EPefdt_1 = EPefdt;
              EPclas_1 = EPclas;
              EPspec_1 = EPspec;

       //* GET LATEST RATE AS OF REPORT DATE FOR 'A' RATED POLICIES THAT ARE
       //* ANNIVERSARY RATED
              If Elstat = 'Y'and ELandt <> 0;
                 Setgt  EPKY7B PEL07;
                 ReadpE EPKY7A PEL07;
              Endif;

              IF EPstat <> 'I' and
                 x2yymm <= xxyymm and
                 EPclas <> RRdccd and
                 ##schtyp <> 'W';
                 x = 1;
                 xxIndex = %Lookup(xxclsp : scs);
       //** Do Not DUPLICATE A PAYROLL CLASS CODE WHEN DISPLAYING
       //** SELF-REPORTING CALCULATED MANUAL PREMIUM

                 If xxIndex = 0;
                    y = y + 1;
                    sed(y) = EPefdt;
                    scs(y) = xxclsp;
                    Exsr SRPREM;
                    sds(y) =  xxdesc;
                    spy(y) =  xxampy;
                    srt(y) =  xxamrt;
                    eval(h)  srtn(y) = XNAMRT;
                    sam(y) = xxampr;
                    If PRpcf = 'O';
                       eval(h) xxwk90 = EPempr/12;
                       sam(y) =  XXWK90;
                       spy(y) =  0;
                       srt(y) =  0;
                       srtn(y)=  0;
                       xxampr = XXWK90;
                    Endif;
                       scmapr = scmapr +  xxampr;
                 Else;

       //** THE ONLY TIME A PAYROLL CLASS CODE SHOULD BE DUPLICATED
       //** FOR AN EMPLOYER IS IF ANNIVERSARY RATING WAS IN EFFECT AND
       //** A PAYROLL CLASS WAS SPLIT.  IN THIS CASE WE WANT TO SHOW
       //** THE PROPER EFF DT, DESCRIPTION AND RATE FOR WHATEVER YEAR
       //** AND MONTH THE SELF-REPORTING MANUAL PREMIUM IS BEING
       //** CALCULATED.

                   sed(x) =  EPEFDT;
                 Endif;
              Endif;

              If EPstat <> 'I' and EPefdt <= xxrpdt and
                 EPclas <> RRdccd and ##schtyp = 'W';
                 x = 1;
                 xxIndex = %Lookup(xxclsp : scs);

       //** DO NOT DUPLICATE A PAYROLL CLASS CODE WHEN DISPLAYING
       //** SELF-REPORTING CALCULATED MANUAL PREMIUM

                 If xxIndex = 0;
                    y = y + 1;
                    sed(y) =  EPefdt;
                    scs(y) =  XXCLSP;
                    EXSR SRPREM;
                    sds(y) =  xxDesc;
                    spy(y) =  xxAmpy;
                    srt(y) =  xxAmRt;
                    eval(h) srtn(y)= xnAmRt;
                    sam(y) = xxAMPR;
                    If PRpcf = 'O';
                       xxpro = xxdays/365;
                       xxwk90 = EPempr * xxpro;
                       sam(y) = xxWK90;
                       spy(y) = 0;
                       srt(y) = 0;
                       srtn(y) = 0;
                       xxampr = xxWK90;
                    Endif;
                    scmapr = scmapr +  xxampr;

                 Else;

       //** THE ONLY TIME A PAYROLL CLASS CODE SHOULD BE DUPLICATED
       //** FOR AN EMPLOYER IS IF ANNIVERSARY RATING WAS IN EFFECT AND
       //** A PAYROLL CLASS WAS SPLIT.  IN THIS CASE WE WANT TO SHOW
       //** THE PROPER EFF DT, DESCRIPTION AND RATE FOR WHATEVER YEAR
       //** AND MONTH THE SELF-REPORTING MANUAL PREMIUM IS BEING
       //** CALCULATED.

                    sed(x) =  EPefdt;
                Endif;

              Endif;
           Endif;
              Reade keyWMEPLE PEP0E;
       Enddo;
       Exsr LoadRptPrm;

      /end-free
     C                   Endsr                                                  SelfRptPrm


       //===================================================================
       //===================================================================
     C     LoadRptPrm    Begsr
      /free
       // Clear the subfile.
       clrSrPrem  = *On;
       Write rcdSrPrems;
       clrSrPrem = *Off;
       rrnSrPrem = 0;

       // Clear payroll totals
       Clear scPrmTotPr;
       Clear scPrmTotMa;

       y = 1;
       DoW y <= 200;
           If sed(y) = 0;
              Leave;
           Endif;
           scPrmEffDt = dtMDYY(sed(y));
           scPrmClass = scs(y);
           scPrmDesc  = sds(y);
           scPrmPayAm = spy(y);
           scPrmRate  = srt(y);
        //*** begin add ***  10/05/21  **********************************
           If isNetRate;
              scPrmRate  = srtn(y);
           Endif;
        //*** end add *****  10/05/21  **********************************
           scPrmMan   = sam(y);

           scPrmTotPr= scPrmTotPr + scPrmPayAm;
           scPrmTotMa= scPrmTotMa + scPrmMan;
           y = y + 1;
           rrnSrPrem = rrnSrPrem + 1;
           Write sflPrem;
       Enddo;
       isContribution = GetPrmContflg(ELco#:ELfnd);
       If isContribution;
          flxManPrem = 'Effective Date,Class Spec Ext   ,Description ,+
           Self Reported Payroll,Rate,Manual Contribution';
          flxManHdg = 'Calculated Manual Contribution';
       Else;
          flxManPrem = 'Effective Date,Class Spec Ext   ,Description ,+
           Self Reported Payroll,Rate,Manual Premium';
          flxManHdg = 'Calculated Manual Premium';
       Endif;

       //* Position payroll grid at top at all times.
       rrnSrPrem = 1;

      /end-free
     C                   Endsr                                                  LoadRptPrm

       //===================================================================
       //===================================================================
     c     DaysPrem      Begsr
     c                   eval      ca2002 = *off
     C                   MOVEL     ELPFDT        X1YR              3 0
     C                   MOVEL     ELANDT        X1AYR             3 0
     C     xxST          IFEQ      'CA'
     C     x1YR          ANDEQ     102
     C     xxST          OREQ      'CA'
     C     x1AYR         ANDEQ     102
     c                   eval      ca2002 = *on
     c                   Endif

     C     ##schtyp      IFNE      'W'
     C     xxRPFM        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXRPFM
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     ##srDate      ##DT8
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      6             ##PAST            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         XXRPFM
      *
     C     XXRPFM        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXRPFM
     C                   ENDIF
     C*
     C* CALCULATE DAYS IN THE TIME PERIOD.
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXRPFM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   ADD       1             QQDAYS
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         XQTO
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     QQERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        XXDAYS            5 0
      *
     C                   ENDIF                                                  ##schtyp
     C                   Z-ADD     *ZEROS        SCMAPR
     C                   Z-ADD     *ZEROS        XXPAYR           15 4
     C                   Z-ADD     1             XXPROR           12 9
     C                   MOVE      *OFF          flgexpmod
     C***  Begin Chg  ***  01/19/22  *************************
     C*****F1NETR        IFEQ      'Y'
     C*****ELPFDT        ANDGE     F1NETD
     C                   If        IsUseNetRate(ELco#:ELfnd:ELemp#:ELdiv:ELfyr)
     C***  End   Chg  ***  01/19/22  *************************
     C*
     C*
     C*  SUBTRACT EXP MOD CHG FROM NORMAL PREMIUM, GET CURRENT EXP MOD AND
     C*  MULTIPLY IT BACK INTO ELENMP SO THAT NET RATE REFLECT CURRENT EXP
     C*  MOD
     C*
     C                   MOVE      *ON           flgexpmod
     C     ELMAPR        IFNE      *ZERO
     C     ELEMPM        ANDNE     *ZERO
     C*
     C     MPKY          CHAIN     PMP01                              60
     C*
     C*  GET EXP MOD CHG IF NO HIT OR IF WDMPP/MPEXPA = 0
     C*
     C     *IN60         IFEQ      *ON
     C     MPEXPA        OREQ      0
     C*
     C     keyWMEPLE     CHAIN     PEP0E                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
     C                   ADD       EPADLD        XXADLD           11 2
     C                   ENDIF
     C     keyWMEPLE     READE     PEP0E                                  60
     C                   ENDDO
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS00                              60
     C                   MOVE      ELPFDT        X2EFDT
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR00
     C     RRKY2         READPE    PRR00                                  60
     C                   Z-ADD     0             X1PREM           11 2
     C                   Z-ADD     0             X2PREM           11 2
     C                   ADD       ELEXP1        X1PREM
     C                   ADD       ELEXP2        X1PREM
     C                   ADD       ELEXP3        X1PREM
     C                   ADD       ELESBJ        X2PREM
     C                   SUB       ELEDCS        X2PREM
     C                   SUB       ELEDCM        X2PREM
     C     RRDV1L        IFEQ      'J'
     C                   SUB       ELEDC2        X2PREM
     C                   ENDIF
     C     RRDV2L        IFEQ      'J'
     C                   SUB       ELEDV2        X2PREM
     C                   ENDIF
     C     ELWVFL        IFEQ      'J'
     C                   ADD       ELEWAV        X2PREM
     C                   ENDIF
     C     RRASFL        IFEQ      'J'
     C                   ADD       XXADLD        X2PREM
     C                   ENDIF
     C     X1PREM        IFNE      *ZERO
     C     X1PREM        SUB       X2PREM        MPEXPA
     C                   ENDIF
     C                   ENDIF
      *
     C*
     C                   Z-ADD     0             XXPRTT
     C     ##schtyp      IFNE      'W'
     C     XXRPDT        SUB       XXRPFM        XXDAYS
     C                   ELSE
     C                   Z-ADD     XXRPFM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ##FDT8            8 0
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ##TDT8            8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    ##FDT8
     C                   PARM                    ##TDT8
     C     XXDAYS        PARM                    ##DAYS
     C                   PARM                    ##ERR
     C                   ENDIF
     C*
     C                   ADD       1             XXDAYS
     C                   Z-ADD     XXRPFM        XXWKDT            7 0
     C     XXWKDT        DOWLE     XXRPDT
     C*
     c                   eval      ELexm1_1 = ELexm1
     c                   eval      ELexm2_1 = ELexm2
     c                   eval      ELexm3_1 = ELexm3
     c                   eval      ELemd1_1 = Elemd1
     c                   eval      ELemd2_1 = Elemd2
     c                   eval      ELemd3_1 = Elemd3
     C     EX(3)         IFNE      *ZERO
     C     XXWKDT        ANDGE     EX(3)
     C                   Z-ADD     EM(3)         XXMOD
     C                   ELSE
     C     EX(2)         IFNE      *ZERO
     C     xxWKDT        ANDGE     EX(2)
     C                   Z-ADD     EM(2)         XXMOD
     C                   ELSE
     C     EX(1)         IFEQ      0
     C     xxWKDT        ORLT      EX(1)
     C                   Z-ADD     1             XXMOD
     C                   ELSE
     C                   Z-ADD     EM(1)         XXMOD
     C                   ENDIF
     C                   ENDIF                                                  EX,2 IFNE
     C                   ENDIF                                                  EX,3 IFNE
     C*
     C     xxMOD         DIV(H)    XXDAYS        XXPROD           11 9
     C                   ADD(H)    XXPROD        XXPRTT           11 9
     C     ##schtyp      IFNE      'W'
     C                   ADD       1             XXWKDT
     C                   ELSE
     C                   Z-ADD     XXWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ##DT8             8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         XXWKDT
     C                   ENDIF
      *
     C                   ENDDO
     C*
     C*
     C                   Z-ADD(H)  XXPRTT        XXWK54            5 4
     C     XXWK54        IFEQ      *ZERO
     C                   Z-ADD     1             XXWK54
     C                   ENDIF
      *
     C                   MOVE      *OFF          wavrvisb
     C     ELWVPC        IFEQ      0
     C     ELEWAV        ANDNE     0
     C     ELEWAV        ORNE      0
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *ON           wavrvisb
      *
     C     ##schtyp      IFNE      'W'
     C     1             DIV(H)    12            XXDPRO            9 8
     C                   ELSE
     C     XXDAYS        DIV(H)    365           XXDPRO
     C                   ENDIF
      *
     C     ELWVFL        IFEQ      'M'
     C     ELWVFL        OREQ      'J'
     C     ELEMPM        SUB       ELEWAV        XXSEXC
     C                   SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR           12 9
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELEWAV        MULT(H)   XXDPRO        XXEWAV           11 0
     C     ELWVFL        IFEQ      'M'
     C     XXEWAV        MULT(H)   XXPROR        XXEWAV
     C                   ELSE
     C     XXEWAV        MULT(H)   XXWK54        XXEWAV
     C                   ENDIF
     C                   ELSE
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C                   ENDIF
      *
     C     ELWVFL        IFEQ      'E'
     C     ELWVFL        OREQ      'S'
     C     ELENMP        SUB       ELEWAV        ELENMP
     C     ELENMP        DIV(H)    ELEMPM        X1PROR           12 9
     C                   MULT(H)   X1PROR        XXPROR
     C     XXEWAV        MULT(H)   XXDPRO        XXEWAV
     C     ELEWAV        MULT(H)   X1PROR        XXEWAV
     C                   ELSE
     C     ELWVFL        IFEQ      'D'
     C     ELEWAV        MULT(H)   XXDPRO        XXEWAV
     C     ELENMP        SUB       ELEWAV        ELENMP
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C                   ELSE
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C     XXEWAV        MULT      X1PROR        XXEWAV
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C                   ENDIF
     C*
     C                   ENDIF                                                            ELMAPR
     C*
     C                   ENDIF                                                  F1NETR IFEQ
     C                   Endsr                                                  DaysPrem
       //===================================================================
       //===================================================================
     C     SRPREM        BEGSR
     C**** Beg Chg ***** 06/17/24 ******************************************
     C****************** Z-ADD     *ZEROS        XXAMRT            7 2
       Clear xxAmRt;
     C**** End Chg ***** 06/17/24 ******************************************
     C                   Z-ADD     *ZEROS        XNAMRT            7 4
     C                   Z-ADD     *ZEROS        XXAMPY           11 2
     C                   Z-ADD     *ZEROS        XXAMPR            9 2
     C                   Z-ADD     EPCLAS        XXCLAS            5 0
     C*
     C* USE 4/1/3 FL RATES IF APPLICABLE.
     C*
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         Q1CO#             3
     C                   MOVE(P)   ELFND         Q1FND             3
     C                   MOVE(P)   ELFYR         Q1FYR             3
     C                   MOVE(P)   ELEMP#        Q1EMP#            9
     C                   MOVE(P)   ELDIV         Q1DIV             5
     C                   MOVE(P)   XXRPDT        Q1RPDT            7
     C                   MOVE(P)   'P'           Q1TYPE            1
     C                   MOVE(P)   XXCLAS        Q1CLAS            5
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C                   PARM                    Q1RPDT
     C                   PARM                    Q1TYPE
     C                   PARM                    Q1CLAS
      *
     C                   MOVE      Q1RPDT        X1EFDT
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCO#         COMP      EPCO#                              6060
     C  N60PRCLAS        COMP      EPCLAS                             6060
     C  N60PRRTCD        COMP      XXRTCD                             6060
     C                   END
     C   60              MOVE      *BLANKS       PRDESC
     C                   MOVE      *BLANKS       XXDESC
      **** Beg Chg **** 06/17/24 **************************************
     C***                MOVEL     PRDESC        XXDESC           15
       xxDesc = PRdesc;
      **** End Chg **** 06/17/24 **************************************
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       X1ST
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELdiv         #2DIV             5
     C                   MOVE(P)   EPefdt        #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   XXCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X1EFDT
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        XXDESC
     C   60RSKY          CHAIN     PRS00                              61
     C  N61
     CAN 60              MOVE      RSJRST        X1ST
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELdiv         #2DIV             5
     C                   MOVE(P)   EPefdt        #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   XXCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X1EFDT
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        XXDESC
     C                   ENDIF                                                  EPEXT IFNE
     C     PXCODE        IFNE      *ZERO
     C     EPEXT         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN62
     C                   ENDIF                                                  PXCODE IFNE
     C*
     C*  CHECK TO SEE IF EMPLOYER IS USING "A" RATING
     C*  AND IF THE CLASS CODE IS 'A' RATABLE
     C*
     C*  USE "A" RATE AS ENTERED THRU EMPLOYER PAYROLL MAINTENANCE
     C*
     C                   Z-ADD     EPARAT        XXARAT
     C*
     C     flgexpmod     IFEQ      *ON
     C     EPENTR        IFNE      *ZERO
     C*** begin add ***  03/01/22  **********************************************
     C     clientID      Andeq     '136'
     C*** end add *****  03/01/22  **********************************************
     C                   Z-ADD     EPENTR        XNARAT            7 4
     C                   ELSE
     C     XXARAT        MULT(H)   XXPROR        XNARAT            7 4
     C                   ENDIF
     C                   Z-ADD     XNARAT        XNAMRT
     C                   ENDIF
     C                   Z-ADD     XXARAT        XXAMRT
     C*
     C     ##schtyp      IFNE      'W'
     C     SRKEY         CHAIN     PSR01                              60
     C                   ELSE
     C     SRKY2         CHAIN     PSR08                              60
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXWK11           13 4
     C                   Z-ADD     *ZEROS        XXTLSP           11 2
     C     *IN60         DOWEQ     '0'
     C     SRSPEC        IFEQ      EPSPEC
     C     PXCODE        IFNE      *ZERO
     C     EPEXT         ANDNE     *BLANKS
     C                   ADD       SRADLD        SCADLD
     C                   ENDIF                                                  PXCODE IFNE
     C                   ADD       SRAMTP        XXTLSP
     C     PRPCF         IFEQ      'P'
     C                   ADD       SRAMTP        XXPAYR
     C     SRAMTP        DIV       100           X2WK11           13 4
     C                   ELSE
     C     clientID      IFEQ      '102'
     C     PRLOGF        IFEQ      'Y'
     C                   CALL      SRLOGC
     C                   PARM                    PRCLAS
     C                   PARM      *OFF          QQFOND            1
     C                   PARM      *ZEROS        QQMPYR            7 5
     C                   PARM      *ZEROS        QQMRAT            7 5
     C                   PARM      *ZEROS        QQRPCL            5 0
     C     QQFOND        IFEQ      '1'
     C     SRAMTP        MULT(H)   QQMPYR        XXEAPR
     C     XXPAYR        ADD       XXEAPR        XXPAYR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  CLTID='102'
     C                   Z-ADD     SRAMTP        X2WK11
     C                   END
     C                   ADD       X2WK11        XXWK11
     C                   END
      *
     C     ##schtyp      IFNE      'W'
     C     SRKEY         READE     PSR01                                  60
     C                   ELSE
     C     SRKY2         READE     PSR08                                  60
     C                   ENDIF
      *
     C                   END                                                    DOWEQ '0' *IN60
      *
     C     XXWK11        MULT(H)   XXARAT        X1AMPR           11 4
     C     flgexpmod     IFEQ      *ON
     C     XXWK11        MULT(H)   XNARAT        X1AMPR
     C                   ENDIF
     C                   Z-ADD     XXTLSP        XXAMPY
     C                   Z-ADD(H)  X1AMPR        XXSAVE           11 0
     C                   Z-ADD     XXSAVE        XXAMPR
     C                   ENDSR                                                  SRPREM

       //===================================================================
       // SUBROUTINE FOR UPDATING RECORDS IN WMEPP
       //===================================================================
     C     UPDTEP        Begsr
      /free

       EPpgm  = Q1pgm;
       EPuser = Q1user;
       EPtime = Timeto6(SystemTime());
       EPdt   = Dateto7(SystemDate());

      /end-free
     C                   Endsr                                                  UPDTEP


     P SelfRptEdit     E
       //===================================================================
     P LoadSelfRptGrd  B
     D LoadSelfRptGrd  PI
     d inShdTyp                       1
     d inSrDt                         8  0
     d inRtCd                         3
     d inMode                         1
     d outRRn#                        9  0

     d ##nSrRpt        s              9  0
     c*
     c     keyWASRL9     Klist
     c                   Kfld                    EPco#
     c                   Kfld                    EPfnd
     c                   Kfld                    EPfyr
     c                   Kfld                    EPemp#
     c                   Kfld                    EPdiv
     c                   Kfld                    kkTODT            7 0
     c                   Kfld                    EPclas


     c     keyWASRL3     Klist
     c                   Kfld                    EPco#
     c                   Kfld                    EPfnd
     c                   Kfld                    EPfyr
     c                   Kfld                    EPemp#
     c                   Kfld                    EPdiv
     c                   Kfld                    kkRPMY            5 0
     c                   Kfld                    EPclas


     c* Employer Payroll Self-Reporting Transaction File
     c*
     c     keyWMEPL7E    Klist
     c                   Kfld                    Elco#
     c                   Kfld                    Elfnd
     c                   Kfld                    Elfyr
     c                   Kfld                    Elemp#
     c                   Kfld                    Eldiv

     c*
     c* Payroll Class File Key List
     c     keyWMPRL1_1   Klist
     c                   Kfld                    Elco#
     c                   Kfld                    inRtCd
     c                   Kfld                    EPclas
     c                   Kfld                    xxEfDt

     c     key1WMPRL1_1  Klist
     c                   Kfld                    Elco#
     c                   Kfld                    inRtCd
     c                   Kfld                    EPclas

     c* Payroll Description File Key List
     c     keyWMPXL1_1   Klist
     c                   Kfld                    Elco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt
     c                   Kfld                    EPefdt

     c     key1WMPXL1_1  Klist
     c                   Kfld                    ELco#
     c                   Kfld                    EPclas
     c                   Kfld                    EPext
     c                   Kfld                    xxPxSt

      /free
       // Clear the subfile.
       clrSrRpt   = *On;
       Write rcdSlfRpt;
       clrSrRpt  = *Off;
       rrnSrRpt  = 0;

       // Clear payroll totals
       Clear scSrAmtTot;

       Setll keyWMEPL7E PEP07E;
       Reade keyWMEPL7E PEP07E;

       Dow not %eof;
          Select;
          When inMode = 'C' and  inShdtyp = 'W';
             kktodt =  DTCymd(inSrdt);
             Chain keyWASRL9 PSR09;

          When inMode = 'C'  and  inShdtyp <> 'W';
             kkrpmy = dt5CYMD(insrdt);
             Chain (EPCo#:EPfnd:EPfyr:EPemp#:EPdiv:kkrpmy:EPclas:EPspec:EPext)
              Psr01;

          Endsl;

          If inMode = 'C';
             If %found;
                scEASRAmt  =  SRamtp;
                svEASRAmt  =  SRamtp;
                scEASREfDt =  dtMDYY(SRefdt);
                scEASrCLAS =  SRclas;
                scEASpec   =  SRspec;
                scEASrRExt =  SRext;
             Else;
                scEASRAmt  =  0;
                svEASRAmt  =  0;
                scEASREfDt =  dtMDYY(EPefdt);
                scEASrCLAS =  EPclas;
                scEASpec   =  EPspec;
                scEASrRExt =  EPext;
             Endif;
             If inShdtyp = 'W';
                scsrRRRN   =  dbrrn4;
             Else;
                scsrRRRN   =  dbrrn5;
             Endif;

          Else;
             scEASRAmt  =  0;
             scEASREfDt =  dtMDYY(EPefdt);
             scEASrCLAS =  EPclas;
             scEASpec   =  EPspec;
             scEASrRExt =  EPext;
             scsrRRRN   =  dbrrn6;
          Endif;

             Clear scEADesc;
             xxType = 'P';
             xxEfDt = SrEffd(EPco#:EPfnd:EPfyr:EPemp#:EPdiv:SRefdt:xxType:
              EPclas);
             Setgt keyWMPRL1_1 PPR01;
             Readpe key1WMPRL1_1 PPR01;
             If not %eof;
                scEADesc = PRdesc;
                If EPext <> *blanks;
                   Clear xxPxSt;
                   Setgt keyWMPXL1_1 PPX01;
                   Readpe key1WMPXL1_1 PPX01;
                   If Not %Eof(WMPXL1);
                      scEADesc= PXdesc;
                   Else;
                      xxPxSt = RatingSt(ELco#:ELrtcd);
                      Setgt keyWMPXL1_1 PPX01;
                      Readpe key1WMPXL1_1 PPX01;
                      If Not %Eof(WMPXL1);
                         scEADesc = PXdesc;
                      Endif;
                   Endif;
                Endif;
             Endif;

             scSrAmtTot= scSrAmtTot + SRamtp;

             rrnSrRpt = rrnSrRpt + 1;
             Write sflSrAdEdt;

             Reade keyWMEPL7E PEP07E;
       Enddo;

       //* Position payroll grid at top at all times.
       outRRn# = rrnSrRpt;
       rrnSrRpt = 1;

      /end-free
     P LoadSelfRptGrd  E

     *=============================================================================================
      * Get manual premium for the record
     *=============================================================================================
     p getManPrem      b
     d getManPrem      pi            11  0

     d manPrem         s             11  0
     d payRate         s                   like(PRRT)
     d lossCostMult    s              5  4
     d harborExpRate   s                   like(HELHNF)

     c     prky          klist
     c                   kfld                    Elco#
     c                   kfld                    ELRTCD
     c                   kfld                    SRCLAS

     c     prky1         klist
     c                   kfld                    ELco#
     c                   kfld                    ELRTCD
     c                   kfld                    SRCLAS
     c                   kfld                    xxefdt

     c                   if        x4EfDt > 0
     c                   eval      xxefdt = x4efdt
     c                   endif
     c     prky1         setgt     PPR01
     c     prky          readpe    PPR01
     c                   if        %eof
     c                   return    0
     c                   endif

      /free
       // Get the Payroll rate instead of calculating it
       Chain (SRco#:SRfnd:SRfyr:SRemp#:SRdiv:SRclas:SRefdt:SRspec) pep09;
       If %Found;
          payRate = E9aRat;
       Else;
          payRate = 0;
       EndIf;
      /end-free

     c                   If        PRpcf = 'P'
     c                   eval(h)   terrPayroll = terrPayroll + SRAMTP
     c                   eval(h)   manPrem = SRamtp * payRate/100
     c                   else
     c                   eval(h)   manPrem = SRamtp * payRate
     c                   Endif

     c                   Return    manPrem

     p getManPrem      e

     *=============================================================================================
      * Gets loss cost multiplier for payroll class being reported  (returns 1 if no LCM)
     *=============================================================================================
     p getLossCost     b
     d getLossCost     pi             5  4
     d  class                               like(SRCLAS)
     d  effDt                               like(SRTODT)

     d qqco#           s              3
     d qqfnd           s              3
     d qqrtcd          s              3
     d qqclas          s              5
     d qqefdt          s              7
     d qqlcm           s              5
     d lossCostMult    s              5  4       inz(1)

     c                   move(p)   ELco#         qqco#
     c                   move(p)   ELfnd         qqfnd
     c                   move(p)   ELrtcd        qqrtcd
     c                   move(p)   class         qqclas
     c                   move(p)   effDt         qqefdt

     c                   call      'SRLCM'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqrtcd
     c                   parm                    qqclas
     c                   parm                    qqefdt
     c                   parm      *blanks       qqlcm

     c                   if        (qqlcm <> *blanks)
     c                   move      qqlcm         lossCostMult
     c                   endif

     c                   return    lossCostMult

     p getLossCost     e
     *=============================================================================================
      * Gets harbor exposure rate for payroll class being reported (returns 1 if no harbor exp rate)
     *=============================================================================================
     p getHarborExp    b
     d getHarborExp    pi                  like(HELHNF)

     d harborExpRate   s                   like(HELHNF) inz(1)

     c     heky1         klist
     c                   kfld                    SRco#
     c                   kfld                    SRfnd
     c                   kfld                    ratingState
     c                   kfld                    ELPFDT

     c     heky          klist
     c                   kfld                    SRco#
     c                   kfld                    SRfnd
     c                   kfld                    ratingState

     c                   eval      ratingState = RatingSt(ELCO#:ELRTCD)
     c                   if        SRSPEC = 'F' and PRLHCD = 'N'
     c     heky1         setgt     phe01
     c     heky          readpe    phe01                                  60
     c                   if        not(%eof(wdhel))
     c                   eval      harborExpRate = HELHNF
     c                   endif
     c                   endif

     c                   return    harborExpRate

     p getHarborExp    e
      *=====================================================
      * Populates manual premium for SR payroll record
      *=====================================================
     p GetSRManPrem    b
     d GetSRManPrem    pi

     c     keyWMEPL1E    Klist
     c                   Kfld                    SRco#
     c                   Kfld                    SRfnd
     c                   Kfld                    SRfyr
     c                   Kfld                    SRemp#
     c                   Kfld                    SRdiv
     c                   Kfld                    SRclas
     c                   Kfld                    SRspec
     c                   Kfld                    SRext
     c                   Kfld                    SRefdt

      /Free
       Chain keyWMEPL1E Pep01e;
       If Not(%Found);
          SRManPrm = 0;
       Endif;
       Setgt (SRco#:ELrtcd:SRclas:SRefdt) ppr01;
       Readpe (SRco#:ELrtcd:SRclas) ppr01;
       Select;
       When PRpcf = 'P';
          Eval(h) SRManPrm = SRamtp * EParat/100;
       When PRpcf = 'C' and clientId <> '150';
          Eval(h) SRManPrm = SRamtp * EParat;
       When PRpcf = 'C' and clientId  = '150';
          SRamtp = 0;
          SRManPrm = 0;
       Endsl;
      /End-Free

     p GetSRManPrem    e


       //***************************************************************************//
       //===================================================================
       // Payroll Vendors Edit Popup
       //===================================================================
     P VendorPEdit     B                   export
     D VendorPEdit     PI
     D vhRRN                          9  0 const
     D elRRN                          9  0 const options(*nopass)

     D kkefdt          s              7  0
     D xxexdt          s              7  0
     D vndAddMode      s               n   inz(*off)
     D vndEdtMode      s               n   inz(*off)
     D vndError        s               n   inz(*off)

     DdsWDVHL          ds                  qualified inz
     d in                                  likerec(pvh00:*input)
     d out                                 likerec(pvh00:*output) overlay(in)
     c*
     c* Payroll Vendors File Key List
     c*
     c     keyWMVPL      Klist
     c                   Kfld                    ELco#
     c                   Kfld                    scPVendor#
     c*
     C     keyWDELL      Klist
     C                   Kfld                    VHco#
     C                   Kfld                    VHgrp
     C                   Kfld                    VHemp#
     C                   Kfld                    VHdiv
     C                   Kfld                    VHfyr
      /free

       Open Wpipolpop2;

       GetClientID(clientID);
       vndEditMod = *off;
       vndAddMode = *off;

       If  vhRRN <> 0;
          Chain(n) vhRRN PVH0P;
          Chain keyWDELL PEL00;
          vndEditMod = *on;
       Else;
          Chain(n) elRRN PEL0P;
          vndAddMode = *on;
       Endif;

       If vndAddMode;
          titleVend = 'Payroll Vendor Add';
       Else;
          titleVend = 'Payroll Vendor Edit';
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou vndAddMode = *off;

       //* Populate the popup window with selected record.

          If vndEditMod = *on;

             scPVendor#  = VHvdr#;
             scPVndEfDt = dtMDYY(VHefdt);
             scPVndExDt = dtMDYY(VHexdt);
             Chain keyWMVPL PVP01;
             If %found;
                scPVendNm =  VPname;
             Endif;
          Else;
             Clear scPVendor#;
             Clear scPVndEfDt;
             Clear scPVndExDt;
             Clear scPVendNm;
          Endif;

       //* Clear errors and buttons.
          Exsr ClearVndErrors;
          Clear vndEditSav;
          Clear vndEditCnl;
          Clear vndEditAdd;

          Clear scVndrSlCh;
          scVndrSlCh = 'VPco# = ' + %EditC(ELco#:'X') + ' and (VPexdt = 0 Or '
           + 'VPexdt >= ' + %Editc(Dateto7(SystemDate()):'X') + ')';

       //* Display popup window for Add or Edit mode.

             Dou vndEditSav = *on or vndEditCnl = *on or vndEditAdd = *on;

                Dou vndError = *off or vndEditCnl = *on;

                   //***** Beg Add ****** 06/22/21 *************************
                     scPF1rrn = elrrn;
                   //***** End Add ****** 06/22/21 *************************
                   Exfmt rcdVndPyrl;

                   Exsr VendorPCheck;

                Enddo;

             Enddo;

             If vndEditSav = *on or vndEditAdd = *on;
                Exsr VendorPSave;
             Endif;

          If vndEditCnl = *on or vndEditSav = *on;
             vndAddMode = *off;
          Endif;

       Enddo;

       Close Wpipolpop2;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Vendor Payroll Error Checking
       //===================================================================
     C     VendorPCheck  Begsr
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C     Dtcymdp       Plist
     C                   Parm                    Srdti
     C                   Parm      *BLANKS       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
     C*
     C     Dtmdyyp       Plist
     C                   Parm                    Srdti
     C                   Parm      '*CYMD'       Srffmt
     C                   Parm      *BLANKS       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr

     c     vhky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr

     c     vhky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C                   kfld                    kkefdt

      *
     C                   Exsr      ClearVndErrors

     c                   If        vndAddMode = *on
      *  Validate vendor number
     c     KEYWMVPL      chain     pvp01
     c                   if        not %found
     C                   eval      vndError = *on
     C                   eval      erVndVend# = *on
     C                   eval      msVndVend# = 'WC30277'
     c                   eval      scPVendNm = *blanks
     c                   else
     c                   eval      scPVendNm = vpname
     c                   endif
      *
      *  Validate effective date
      *
     c                   eval      scPVndEfDt = dtCheck(scPVndEfDt)
     c                   if        scPVndEfDt = *zero
     C                   eval      vndError = *on
     C                   eval      erVndEEFDT = *on
     C                   eval      msVndEEFDT = 'WCR0500'
     c                   endif
      *
      *  Ensure new effective date doesn't overlap other dates.

     c                   eval      kkefdt = dtCYMD(scPVndEfDt)
      *
      *  Ensure effective date falls within policy range
      *

     c                   if        kkefdt <  elpfdt or
     c                             kkefdt >= elptdt
     C                   eval      vndError = *on
     C                   eval      erVndEFDT1 = *on
     C                   eval      msVndEFDT1 = 'WC30295'
     c                   endif

     c                   endif
      *
      *
      *  Validate expiration date
      *
     c                   if        scPVndExDt <> 0
     c                   eval      scPVndExDt = dtCheck(scPVndExDt)
     c                   if        scPVndExDt = *zero
     C                   eval      vndError = *on
     C                   eval      erVndEXPDT = *on
     C                   eval      msVndEXPDT = 'WCR0500'
     c                   endif
     c                   endif

      *
      *  Ensure expiration date is greater or equal to effective date
      *
     c                   if        scPVndExDt <> 0
     c                   eval      vhexdt = dtCymd(scPVndExDt)
     c                   eval      vhefdt = dtCymd(scPVndEfDt)
     c                   if        vhexdt < vhefdt
     c                   eval      vndError = *on
     c                   eval      erVndEXDT1 = *on
     c                   eval      msVndEXDT1 = 'WCR2367'
     c                   endif
     c                   endif
      *
      *  Ensure expiration date falls within policy range
      *
     c                   if        scPVndExDt  <> 0
     c                             and vhefdt <> vhexdt

     c                   if        vhexdt <  elpfdt or
     c                             vhexdt >= elptdt
     C                   eval      vndError = *on
     C                   eval      erVndEXDT2 = *on
     C                   eval      msVndEXDT2 = 'WC30295'
     c                   endif

     c                   endif

     c                   eval      xxexdt = dtCYMD(scPVndExDt)
     c                   eval      kkefdt = dtCYMD(scPVndEfDt)
     c     vhky1         setgt     pvh01
     c     vhky          reade(N)  pvh01

     c                   dow       not(%eof)
     c                   if        l1_vhefdt <> l1_vhexdt
     c                   leave
     c                   endif
     c     vhky          reade(N)  pvh01
     c                   enddo

     c                   if        not %eof and kkefdt < l1_vhexdt
     c                   eval      vndError = *on
     c                   eval      erVndEEFDT = *on
     c                   eval      msVndEEFDT = 'WC30296'
     c                   endif

     c                   if        not %eof and
     c                             (xxexdt > l1_vhefdt or xxexdt = 0)
     c                   eval      vndError = *on
     c                   eval      erVndEXDT3 = *on
     c                   eval      msVndEXDT3 = 'WC30381'
     c                   endif


     c     vhky1         setgt     pvh01
     c     vhky          readpe(N) pvh01
     c                   dow       not(%eof)
     c                   select
     c                   when      vndAddMode = *on
     c                   if        l1_vhefdt <> l1_vhexdt
     c                   leave
     c                   endif
     c                   when      vndEdtMode = *on
     c                   if        l1_vhefdt <> l1_vhexdt
     c***********                  pfrrn <> vhrrn
     c                   leave
     c                   endif
     c                   endsl
     c     vhky          readpe(N) pvh01
     c                   enddo

     c                   if        not %eof and
     c                             kkefdt < l1_vhexdt and
     c                             vndAddMode = *on
     c                   eval      vndError = *on
     c                   eval      erVndEEFDT = *on
     c                   eval      msVndEEFDT = 'WC30296'
     c                   endif

     c                   if        not %eof         and
     c                             vndEdtMode = *on and
     c                             xxexdt <> vhexdt and
     c                             (l1_vhexdt > kkefdt or l1_vhexdt = 0)  and
     c                             l1_vhefdt <> l1_vhexdt
     C                   eval      vndError = *on
     C                   eval      erVndEXDT3 = *on
     C                   eval      msVndEXDT3 = 'WC30381'
     c                   endif

     C**                 if        #ofscr = 1 and *in90 = *off
     C***                exsr      srupda
     C**                 endif
      *
      *
     C                   Endsr                                                  VendorPCheck

      /Free
       //===================================================================
       // Update add divisions, for PEO groups
       //===================================================================
       Begsr VendorPDiv;
        If GetPEOFlag(VHgrp:VHco#) = *blank;
           Leavesr;
        Endif;
        Setll (VHco#:VHgrp:VHfyr) wdell1;
        Dou %eof(wdell1);
           Reade (VHco#:VHgrp:VHfyr) wdell1;
           Select;
           When %eof;
              Iter;
           When ELemp# = 1 and ELdiv = 0;
              Iter;
           Endsl;
           Reset dsWDVHL;
           Chain (VHco#:VHgrp:ELemp#:ELdiv:VHfyr:VHvdr#:VHefDt)
            wdvhl dsWDVHL.in;
           dsWDVHL.out.VHco#  = VHco#;
           dsWDVHL.out.VHgrp  = VHgrp;
           dsWDVHL.out.VHemp# = ELemp#;
           dsWDVHL.out.Vhdiv  = ELdiv;
           dsWDVHL.out.VHfyr  = VHfyr;
           dsWDVHL.out.VHvdr# = VHvdr#;
           dsWDVHL.out.VHefDt = VHefDt;
           dsWDVHL.out.VHexDt = VHexDt;
           dsWDVHL.out.VHiUsr = VHiUsr;
           dsWDVHL.out.VHiPgm = VHiPgm;
           dsWDVHL.out.VhiTim = VhiTim;
           dsWDVHL.out.VHiDt  = VHiDt;
           dsWDVHL.out.VHuser = VHuser;
           dsWDVHL.out.VHpgm  = VHpgm;
           dsWDVHL.out.VHuser = VHuser;
           dsWDVHL.out.VHtime = VHtime;
           dsWDVHL.out.VHdt   = VHdt;
           If %found(wdvhl);
              Update pvh00 dsWDVHL.out;
           Else;
              Write pvh00 dsWDVHL.out;
           Endif;
        Enddo;
        Chain (VHco#:VHgrp:VHfyr:VHemp#:VHdiv) wdell1;
       Endsr;
      /End-free

       //===================================================================
       // Save Payroll Data.
       //===================================================================
     C     VendorPSave   Begsr
      /free

       If vndEditMod = *on;
          Chain vhRRN PVH0P;
       Else;
          Clear PVH0P;
          VHco#  = ELco#;
          VHgrp  = ELfnd;
          VHemp# = ELemp#;
          Vhdiv  = ELdiv;
          VHfyr  = ELfyr;
          VHvdr# = scPVendor#;
          VHEfdt = dtCYMD(scPVndEfDt);
          VHiusr = Q1user;
          VHipgm = Q1pgm;
          Vhitim = Timeto6(SystemTime());
          VHidt  = Dateto7(SystemDate());
          Endif;
          VHExdt = dtCYMD(scPVndExDt);


          VHuser  = Q1user;
          VHpgm  = Q1pgm;
          VHuser = Q1user;
          VHtime = Timeto6(SystemTime());
          VHdt   = Dateto7(SystemDate());

       If vndEditMod = *on;
          Update PVH0P;
       Else;
          Write PVH0P;
       Endif;

       Exsr VendorPDiv;

      /end-free
     C                   Endsr                                                  VendorPSave

       //===================================================================
       // Clear Vendor Payroll all Errors
       //===================================================================
     C     ClearVndErrorsBegsr
      /free

       vndError = *off;

       erVndVend# = *off;
       msVndVend# = *blanks;
       erVndEEFDT = *off;
       msVndEEFDT = *blanks;
       erVndEFDT1 = *off;
       msVndEFDT1 = *blanks;
       erVndEXPDT = *off;
       msVndEXPDT = *blanks;
       erVndEXDT1 = *off;
       msVndEXDT1 = *blanks;
       erVndEXDT2 = *off;
       msVndEXDT2 = *blanks;
       erVndEXDT3 = *off;
       msVndEXDT3 = *blanks;


      /end-free
     C                   Endsr                                                  ClearVndErrors

     P VendorPEdit     E



       //===================================================================
       // Load Previous Cross Reference Employer
       //===================================================================
     P PrvEmpXRef      B                   export
     D PrvEmpXRef      PI
     D inCO#                          3  0 const
     D inFND                          3  0 const
     D inEMP#                         9  0 const
     D inDIV                          5  0 const

     D ##CO#           s              3  0
     D ##FND           s              3  0
     D ##EMP#          s              9  0
     D ##DIV           s              5  0
     D outFnd          s              3  0
     D outEmp#         s              9  0
     D outDiv          s              5  0
     D intRRN1         s              9  0
     D strRRN          s              9
     D empName         s             50
     D QteMode         s              1A
     D mode            s              1A
     D strLibrary      s             10    inz(' ')

      /free
       Open WPIPOLPOP2;
       ##CO#   = inCO#;
       ##FND   = inFND;
       ##EMP#  = inEMP#;
       ##DIV   = inDIV;
       // Clear the subfile.
       clrPrvEmp = *On;
       dspPrvSfl  = *off;
       Write rcdCrosRef;
       dspPrvSfl  = *on;
       clrPrvEmp = *Off;
       rrnPrvEmp  = 0;
       // Populate the subfile
       Dou outEmp# = 0;
         outFND  = 0;
         outEMP# = 0;
         outDIV  = 0;
         GetPrvEmp#(##CO#:##FND:##EMP#:##DIV:outFnd:outEmp#:outDiv);
           //***** Beg Add ****** 04/08/22 *************************
           If outFnd = inFnd and outEmp# = inEmp# and outDiv = inDiv;
              Leave;
           EndIf;
           //***** End Add ****** 04/08/22 *************************
           If outEmp# <> 0;
              scPrvEmp# = %EditC(outFnd:'X') + '-' + %EditC(outEmp#:'X') +
               '-' + %EditC(outDiv:'X');
              mode = ' ';
              QteMode = GetQteMode(##co#:outFnd:outEmp#:outDiv);
              If QteMode = 'Q';
                 Mode = 'Q';
              EndIf;
              RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
              If (mode = 'Q' and strLibrary <> RtvQuoteLib('*QTE'))
                 Or (mode = ' ' and strLibrary <> RtvQuoteLib('*LIV'));
                 intRRN1=  GetEmpYrRR(0:outEmp#:outDiv:outFnd:##Co#:' ':mode);
                 strRRN =  %Editc(intRRN1:'X');
                 empName = GetEmpName(outEmp#:outDiv:intRRN1:outFnd:##Co#:mode);
                 scPrvEmUrl = CreateQteURL('STDRVPGM':empName:strRRN:'D':
                                           'WTPOLSUM':*Blanks:mode);
              Else;
                 intRRN1=  GetEmpYrRR(0:outEmp#:outDiv:outFnd:##Co#:' ');
                 strRRN =  %Editc(intRRN1:'X');
                 empName = GetEmpName(outEmp#:outDiv:intRRN1:outFnd:##Co#);
                 scPrvEmUrl = CreateURL('STDRVPGM':empName:strRRN:'D':
                                        'WTPOLSUM');
              EndIf;
              rrnPrvEmp = rrnPrvEmp + 1;
              Write sflPrvEmp;
           Endif;
           ##FND = outFnd;
           ##EMP#= outEmp#;
           ##DIV = outDiv;
       Enddo;

       rrnPrvEmp = 1;
       LoadFutEmp(inCO#:inFND:inEMP#:inDIV);

       //* Populate the popup window with selected record.
       DoU BtnCroCan = *On;
            ExFmt rcdCrosRef;
       EndDo;

       Close WPIPOLPOP2;
      /end-free

     P PrvEmpXRef      E


       //===================================================================
       // Load Future Cross Reference Employer
       //===================================================================
     P LoadFutEmp      B                   export
     D LoadFutEmp      PI
     D inCO#                          3  0 const
     D inFND                          3  0 const
     D inEMP#                         9  0 const
     D inDIV                          5  0 const

     D ##CO#           s              3  0
     D ##FND           s              3  0
     D ##EMP#          s              9  0
     D ##DIV           s              5  0
     D outFnd          s              3  0
     D outEmp#         s              9  0
     D outDiv          s              5  0
     D intRRN1         s              9  0
     D strRRN          s              9
     D empName         s             50
     D QteMode         s              1A
     D mode            s              1A
     D strLibrary      s             10    inz(' ')
      /free
       ##CO#   = inCo#;
       ##FND   = inFnd;
       ##EMP#  = inEmp#;
       ##DIV   = inDiv;
       // Clear the subfile.
       clrFutEmp = *On;
       dspFutSfl  = *off;
       Write rcdCrosRef;
       dspFutSfl  = *on;
       clrFutEmp = *Off;
       rrnFutEmp  = 0;
       Dou outEmp# = 0;
           outFND  = 0;
           outEMP# = 0;
           outDIV  = 0;
           GetFutEmp#(##CO#:##FND:##EMP#:##DIV:outFnd:outEmp#:outDiv);
           //***** Beg Add ****** 04/08/22 *************************
           If outFnd = inFnd and outEmp# = inEmp# and outDiv = inDiv;
              Leave;
           EndIf;
           //***** End Add ****** 04/08/22 *************************
           Clear sflFutEmp;
           If outEmp# <> 0;
              scFutEmp# = %EditC(outFnd:'X') + '-' + %EditC(outEmp#:'X') +
               '-' + %EditC(outDiv:'X');
              rrnFutEmp = rrnFutEmp + 1;
              mode = ' ';
              QteMode = GetQteMode(##co#:outFnd:outEmp#:outDiv);
              If QteMode = 'Q';
                 Mode = 'Q';
              EndIf;

              RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
              If (mode = 'Q' and strLibrary <> RtvQuoteLib('*QTE'))
                 Or (mode = ' ' and strLibrary <> RtvQuoteLib('*LIV'));
                 intRRN1= GetEmpYrRR(0:outEmp#:outDiv:outFnd:##Co#:' ':mode);
                 strRRN =  %Editc(intRRN1:'X');
                 empName = GetEmpName(outEmp#:outDiv:intRRN1:outFnd:##Co#:mode);
                 scFutEmUrl = CreateQteURL('STDRVPGM':empName:strRRN:'D':
                                        'WTPOLSUM':*Blanks:mode);
              Else;
                 intRRN1=  GetEmpYrRR(0:outEmp#:outDiv:outFnd:##Co#:' ');
                 strRRN =  %Editc(intRRN1:'X');
                 empName = GetEmpName(outEmp#:outDiv:intRRN1:outFnd:##Co#);
                 scFutEmUrl = CreateURL('STDRVPGM':empName:strRRN:'D':
                                        'WTPOLSUM');
              EndIf;
              Write sflFutEmp;
           Endif;
           ##FND = outFnd;
           ##EMP#= outEmp#;
           ##DIV = outDiv;
       Enddo;
       rrnFutEmp = 1;

      /end-free
     P LoadFutEmp      E

       //===================================================================
       // Employee Payroll Edit Popup
       //===================================================================
     P EmpPayEdit      B                   export
     D EmpPayEdit      PI
     D eeRRN                          9  0 const
     D elRRN                          9  0 const options(*nopass)
     D inMode                         1    const options(*nopass)

     D kkefdt          s              7  0
     D EmEdDt          s              7  0
     D EmStDt          s              7  0
     D EmpAddMode      s               n   inz(*off)
     D EmpEdtMode      s               n   inz(*off)
     D EmpDltMode      s               n   inz(*off)
     D vsAddNext       s               n   inz(*off)
     D EmpError        s               n   inz(*off)
     D iOpen           s               n   inz(*off)
     D p2Open          s               n   inz(*off)
     D xxss#           s              9A   inz
     D xPClas          s              5A   inz
     D XXEMPPCLS2      s                   like(scEMPPCLS2)
     D dashPos1        s              5  0 inz
     D dashPos2        s              5  0 inz

     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    EECo#
     C                   Kfld                    EEFnd
     C                   Kfld                    EEEmp#
     C                   Kfld                    EEDiv
     C                   Kfld                    EEFyr
     C*
     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EEClas
     C                   Kfld                    xxEfDt
     C     key1WMPRL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EEClas
      /free

       If not %Open(Wpipolpop2);
         Open Wpipolpop2;
         p2Open = *On;
       Endif;

       If not %Open(wmepp);
         Open wmepp;
         iOpen = *On;
       Endif;

       GetClientID(clientID);
       EmpEditMod = *off;
       EmpAddMode = *off;
       EmpDltMode = *off;
       vsAddNext  = *off;

       If eeRRN <> 0 and inMode = 'E';
          Chain(n) eeRRN PEE0P;
          Chain keyWDELL PEL00;
          scELRRN = dbrrn;
          EmpEditMod = *on;
          scNextText = 'Save / Add New';
       ElseIf inMode = 'A';
          Chain(n) elRRN PEL0P;
          scELRRN = elRRN;
          EmpAddMode = *on;
          scNextText = 'Save / Add Next';
       ElseIf inMode = 'D';
          EmpDltMode = *on;
       Endif;

       // update hidden fields
       scCo#  = ELCo#;
       scFnd# = ELFnd;
       scEmp# = ELEmp#;
       scDiv# = ELDiv;
       scFyr#= ELFyr;

       If EmpAddMode;
          EmpTitle = 'Employee Payroll Add';
          vsAddNext = *on;         //set visibility add next button
       ElseIf EmpEdtMode;
          EmpTitle = 'Employee Payroll Edit';
          vsAddNext = *off;        //set visibility add next button
       ElseIf EmpDltMode;
          scDelTitle = 'Employee Payroll record deletion';
       Endif;

       Exsr SetCriteria;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou EmpAddMode = *off;
          // Populate the popup window with selected record.
          If EmpEditMod = *ON;
             scEmpPLNam = EElsn;
             scEmpPFNam = EEfsn;
             scEmpPMInt = EEmid;
             scEmpPTitl = EEtitl;
             //***  Begin Chg  ***  06/09/20  **********************
             //scEmpPSS#  = EEss#;
             scEmpPSS# = MaskData('SSN':'EEss#':EEss#);
             //***  End   Chg  ***  06/09/20  **********************
             scEmpPPos  = EEposn;

             xxEmpPCLS2 = %char(EEclas) +
                             '/' + EEspec + '/' + EEEXT;
             scEmpPCLS2 = xxEmpPCLS2;
             scEmpPSRPR = EEsapr;
             scEmpPStDt = dtMDYY(EEsdt);
             scEmpPEdDt = dtMDYY(EEedt);
             scEmpPCkDt = dtMDYY(EEckdt);

          Else;
             Clear scEmpPLNam;
             Clear scEmpPFNam;
             Clear scEmpPMInt;
             Clear scEmpPTitl;
             Clear scEmpPSS# ;
             Clear scEmpPPos ;
             Clear scEmpPCLS2;
             Clear scEmpPDesc;
             Clear scEmpPSRPR;
             Clear scEmpPStDt;
             Clear scEmpPEdDt;
             Clear scEmpPCkDt;
          Endif;

          //* Clear errors and buttons.
          Exsr ClrEmpErrs;
          EmpEditSav = *off;
          EmpEditCnl = *off;
          EmpEditAdd = *off;

       //* Display popup window for Deletion mode.
       If EmpDltMode;
          //write rcdEmpPEdt;
          //write rcdDelete;
          Dou deleteBtn = *on or cancelBtn = *on;
             Exfmt rcdDelete;
          Enddo;
          If deleteBtn = *on;
             Chain eeRRN PEE0P;
             If %found;
                Delete PEE0P;
             Endif;
          Endif;
       Else;
          //* Display popup window for Add or Edit mode.
          Dou EmpEditSav = *on or empEditCnl = *on or EmpEditAdd = *on;
             Dou EmpError = *off or EmpEditCnl = *on;
              //***** Beg Add ****** 06/22/21 *************************
               scPF1rrn = elrrn;
              //***** End Add ****** 06/22/21 *************************
                Exfmt rcdEmpPEdt;
                Exsr EmpPyrlEdt;
             Enddo;
             If EmpEditSav = *on or EmpEditAdd = *on and EmpError = *off;
                Exsr EmpPyrlSave;
             Endif;
          Enddo;

          If EmpEditCnl = *on or EmpEditSav = *on;
             EmpAddMode = *off;
          Endif;
       EndIf;

       Enddo;

       If %Open(wmepp) And iOpen;
         Close wmepp;
         iOpen = *Off;
       Endif;

       If %Open(Wpipolpop2) And p2Open;
          Close Wpipolpop2;
       Endif;
      /end-free

       //===================================================================
       // Employee Payroll Error Checking
       //===================================================================
     C     EmpPyrlEdt    Begsr
     C*
     c     EmpKy         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
      * WMEEL1 - SS# must be unique for Add mode
     c     EmpKy1        klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C                   kfld                    eeclas
     C                   kfld                    eespec
     C                   kfld                    xxss#

     C                   Exsr      ClrEmpErrs

     C                   If        EmpAddMode = *on
      *  Validate Employee Last Name - field cannot be blank
     C                   if        scEmpPLNam = *blanks
     C                   eval      EmpError = *on
     C                   eval      erEmpPLNam = *on
     C                   eval      msEmpPLNam = 'WCR0974'
     C                   endif
      *
      *  Validate Employee First Name - field cannot be blank
     c                   if        scEmpPFNam = *blanks
     C                   eval      EmpError = *on
     C                   eval      erEmpPFNam = *on
     C                   eval      msEmpPFNam = 'WCR0974'
     c                   endif
      *
      *  Validate Employee SS# - field cannot be blank
     c                   if        scEmpPSS#  = *blanks
     C                   eval      EmpError = *on
     C                   eval      erEmpPSS#  = *on
     C                   eval      msEmpPSS#  = 'WCR0974'
     c                   endif
      *
      *  Validate Employee Class - field cannot be zeros
      *  (come back for this one)
     c                   if        scEmpPCls2 = *zeros
     C                   eval      EmpError = *on
     C                   eval      erEmpPCl2 = *on
     C                   eval      msEmpPcl2 = 'WCR0556'
     c                   endif
      *
      *  Validate Employee Start date - start date cannot be zero
      *
     C                   eval      scEmpPStDt = dtCheck(scEmpPStDt)
     C                   if        scEmpPStDt = *zero
     C                   eval      EmpError = *on
     C                   eval      erEmpPStDt = *on
     C                   eval      msEmpPStDt = 'WCR0500'
     C                   Else
      *
      *  Ensure Employee Start date doesn't overlap other dates.
     C                   eval      kkefdt = dtCYMD(scEmpPStDt)
      *
      *  Ensure effective date falls within policy range
      *  'Date not in Policy Date range'
     C                   if        kkefdt <  elpfdt or
     C                             kkefdt >= elptdt
     C                   eval      EmpError = *on
     C                   eval      erEmpPStDt = *on
     C                   eval      msEmpPStDt = 'WC30295'
     C                   endif
     C                   endif

      *
      *  Validate Employee End Date - Invalid date entered
      *
     C                   eval      scEmpPEdDt = dtCheck(scEmpPEdDt)
     C                   if        scEmpPEdDt = *zero
     C                   eval      EmpError = *on
     C                   eval      erEmpPEdDt = *on
     C                   eval      msEmpPEdDt = 'WCR0500'
     C                   else
      *
      *  Ensure Employee End date is greater or equal to effective date
      *
     C                   if        scEmpPEdDt <> 0
     C                   eval      EmEdDt = dtCymd(scEmpPEdDt)
     C                   eval      EmStDt = dtCymd(scEmpPStDt)
     C                   if        EmEdDt < EmStDt
     C                   eval      EmpError = *on
     C                   eval      erEmpPEdD1 = *on
     C                   eval      msEmpPEdD1 = 'WCR2367'
     C                   endif
     C                   endif
      *
      *  Ensure Employee End date falls within policy range
      *
     C                   if        scEmpPEdDt  <> 0
     C                             and EmStDt <> EmEdDt
     C                   if        EmEdDt <  elpfdt or
     C                             EmEdDt >= elptdt
     C                   eval      EmpError = *on
     C                   eval      erEmpPEdD2 = *on
     C                   eval      msEmpPEdD2 = 'WC30295'
     C                   endif
     C                   endif

     C                   endif                                                  if date = 0

     C                   endif                                                  End Add mode

      ** Edit or ADD modes.....
     C                   If        EmpAddMode = *on OR
     C                             EmpEdtMode = *on
      *
      *  Validate Employee Check Date - Invalid date entered
      *
     C                   if        scEmpPCkDt <> 0
     C                   eval      scEmpPCkDt = dtCheck(scEmpPCkDt)
     C                   if        scEmpPCkDt = *zero
     C                   eval      EmpError = *on
     C                   eval      erEmpPCkDt = *on
     C                   eval      msEmpPCkDt = 'WCR0500'
     C                   endif
     C                   endif

     C                   endif

     C                   Endsr                                                  VendorPCheck
      /Free
       //===================================================================
       // Save Employee Payroll Data.
       //===================================================================
       BegSr EmpPyrlSave;

          If EmpEditMod = *on;
             Chain eeRRN PEE0P;
          Else;
             Clear PEE0P;
             EEco#  = ELco#;
             EEfnd  = ELfnd;
             EEemp# = ELemp#;
             EEdiv  = ELdiv;
             EEfyr  = ELfyr;
             EEiusr = Q1user;
             EEipgm = Q1pgm;
             EEitm  = Timeto6(SystemTime());
             EEidt  = Dateto7(SystemDate());
          Endif;
          EElsn  = scEmpPLNam;
          EEfsn  = scEmpPFNam;
          EEmid  = scEmpPMInt;
          EEtitl = scEmpPTitl;
          //***  Begin Chg  ***  06/09/20  **********************
          //EEss#  = scEmpPSS#;
          EEss#  = UnMaskData('SSN':'EEss#':scEmpPSS#);
          //***  End   Chg  ***  06/09/20  **********************
          EEposn = scEmpPPos;
          EEsapr = scEmpPSRPR;
          EEsdt  = dtCYMD(scEmpPStDt);
          EEedt  = dtCYMD(scEmpPEdDt);
          EEckdt = dtCYMD(scEmpPCkDt);

          dashPos1 = %scan('/':scEmpPCLs2);
          If dashPos1 > 0;
             EEclas   = %dec(%trim(%subst(scEmpPCLs2:1:dashPos1-1)):5:0);
             EEspec   = %trim(%subst(scEmpPCLs2:dashPos1+1:1));
             dashPos1 = dashPos1 + 1;
             dashPos2 = %scan('/':scEmpPCLs2:dashPos1);
             If dashPos2 > 0;
                EEext = %trim(%subst(scEmpPCLs2:dashPos2+1:5));
             Else;
                EEext = *blanks;
             EndIf;
          Else;
             EEclas  = %dec(%trim(%subst(scEmpPCLs2:1:5)):5:0);
             EEspec  = *blanks;
             EEext   = *blanks;
          EndIf;

          EEuser  = Q1user;
          EEpgm  = Q1pgm;
          EEtime = Timeto6(SystemTime());
          EEdt   = Dateto7(SystemDate());

          If EmpEditMod = *on;
             Update PEE0P;
          Else;
             Write PEE0P;
          Endif;

       EndSr;

       //===================================================================
       // Clear Employee Payroll Errors
       //===================================================================
       BegSr ClrEmpErrs;

          EmpError = *off;
          erEmpPCkDt = *off;
          msEmpPCkDt = *blanks;
          erEmpPCl2  = *off;
          msEmpPCl2  = *blanks;
          erEmpPEdDT = *off;
          msEmpPEdDT = *blanks;
          erEmpPFNam = *off;
          msEmpPFNam = *blanks;
          erEmpPLNam = *off;
          msEmpPLNam = *blanks;
          erEmpPMInt = *off;
          msEmpPMInt = *blanks;
          erEmpPPos  = *off;
          msEmpPPos  = *blanks;
          erEmpPSrPr = *off;
          msEmpPSrPr = *blanks;
          erEmpPSS#  = *off;
          msEmpPSS#  = *blanks;
          erEmpPStDt = *off;
          msEmpPStDt = *blanks;
          erEmpPTitl = *off;
          msEmpPTitl = *blanks;

       EndSr;

       //===================================================================
       // Set dropdown/text box selection criteria
       //===================================================================
       BegSr SetCriteria;

       scExpDate = ELPTDT;
       scRTCD = ELRTCD;
       scJRST = GetRatingState(scCo#:scFnd#:scEmp#:scDiv#:scFyr#);
       xxSt = RatingSt(ELCO#:ELRTCD);
       Clear scEmpPDesc;
       Clear clsChoices;
       clsChoices = 'EPCO# = ' + %char(ELCO#) +
                ' AND EPFND = ' + %char(ELFND) +
                ' AND EPFYR = ' + %char(ELFYR) +
                ' AND EPEMP# = ' + %char(ELEMP#) +
                ' AND EPDIV = ' + %char(ELDIV) +
                ' AND EPSTAT = ''A''' ;
       EndSr;

      /end-free

     P EmpPayEdit      E
      ***===================================================================
**
312931303130313130313031
**
Manual Premium
Increased Limits (1st/2nd)
Subtotal
Discount Factor Before Modifier
Subtotal
Experience Modifier
Subtotal
Discount Factor After Modifier
Total Premium Due
State Tax
Net Premium
Manual Contribution
Total Contribution Due
Net Contribution
