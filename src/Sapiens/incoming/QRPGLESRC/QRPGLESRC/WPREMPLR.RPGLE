      *********************************************************************
      * REVISIONS:
      *
      * 09/14/23 C:62  Ticket: 113477(129)
      *                Add GetEmpEnv to tell where employer exists.
     *
     *  07/07/23  C:300 Project: WC2810
     *                  Add GetEmpBalToMin
     *
     *  02/22/22  C:62  Project: 150WFN
     *                  Add GetNameAddress
     *
     *  01/19/22  C:33  Project: WC2837
     *                  Add IsUseNetRate
      *
      * 08/24/21 C:291 Project: WC2812
      *                Add DefaultRatingFactors
      *
      * 07/01/21 C:33  Support: 96462
      *                Add DeleteEmployer
      *
      * 12/14/20 C:33  Project: SY406
      *                Add CxJSONData
      *
      * 06/22/20 C:33  Project: SY406
      *               -Add SetFQLockSts
      *               -Add UpdFQLockSts
      *
      * 03/16/20 C:136 Project: WC2700
      *                Add check to override non-renewal cancellations
      *                with OvrRenewalCx.
      *
      * 05/29/19 C:65  Project: SY407
      *                Add procedure for generating json of policy data.
      *
      *  04/22/19  C:33  Project: WC2610
      *                  Add GetScheduleRatingFactor
      *
      * 01/14/19 C:33  Project: SY407
      *                Add 'QuoteStsChgHst'
      *
      * 12/18/18 C:33  Project: SY406
      *                Allow for 'Portal' lockout
      *
      * 06/26/18 C:136 Project: 150WAQ
      *                Add PrvRptCollat subprocedure
      *
      * 06/21/18 C:33  Support: 71743
      *                Update Last Division to use 5.0
      *
      * 06/11/18 C:65  Project: 150WAQ
      *                Add procedure for calculating collateral and
      *                writing the amounts to A/R Batch.
      *
      * 05/30/18 C:65  Project: WC2607
      *                Add procedure for deductible information.
      *
      * 05/15/18 C:291 Project: 150WCR
      *                Add inCall parameter to EdtPEOPolType procedure
      *
      * 08/01/17 C:062 Project: 150WAP
      *                Create new procedure to verify if entered employer is valid
      *
      * 01/09/17 C:136 PhaseWare: 44983
      *                Correct ChkRenewal - remove unneeded parms
      *
      * 04/06/16 C:136 PHASEWARE: 40646
      *                Add GetStRptPDays
      *
      * 02/10/16 C:242 PW37116
      *                DBA/Fed Names and Links for Quote not working
      *                correctly.
      *
      * 09/17/15 C:302 Project: WC2254
      *                Employer Copy - User Control
      *
     *
     * 06/23/15 C:136 Project: WC2182
     *                Add various edit routines for Single Employer
     *                Renewal
     *
      * 05/22/15 C:279 Project: 114WKA
      *                Add procedure PolPageProcessed to determine if a
      *                policy page is genearted.
      *
      * 04/02/15 C:279 Project: WC2356
      *                Add procedure MultiState to determine if a policy
      *                is a multi-state policy.
      *
      * 03/16/15 C:062 Support: 33798 (089)
      *                Add procedure to get next division.
     *
      * 12/29/14 C:283 Support: RedMine 4809
      *                Add procedure to check the FQ lock on an Employer.
     *
      * 02/27/14 C:283 Support: WA099 84925
      *                Added procedure GetQteMode; to fetch if the employer
      *                is in Quote or Live environment.
      *                Added procedure RtvEnviLib which will return the
      *                library of the current environment.
      *
     * 08/01/13 C:136 Project: 147WAI
     *                Add RetTotPrem and SelPrem subprocedures prototypes
     *                to return total premium from all divisions
     *
      * 05/31/13 C:33  Redmine #2644
      *                - CoverageLapse - Determine if there is a lapse in
      *                                   coverage
     *
      * 11/16/12 C:65  Project: WC2116
      *                - GetEmpRating - Get Large Deductible, Retrospective
     *                  Rating, or Scheduled Rating status for div or policy.
     *
      * 10/16/12 C:190 Project: WC2116
      *                - GetRnwlQte - Get Renewal Quote Flag
      *                - GetRnwlQteDiv - Get Renew to Quote Division Flag
     *
      * 10/07/12 C:062 Project: WC2115
      *                Add GetExclMed
     *
      * 09/28/12 C:190 Project: WC2115
      *                - GetIncLimit - Add Additional parameters for Description.
     *
      * 07/10/12 C:62  Project: WC2115
      *                Add RtvPolYr, RtvPolFrDt and RtvPolToDt
     *
      * 05/16/12 C:33  Project: WC2116
      *                Add RtvQuoteLib
     *
      * 05/15/12 C:65  Project: WC2116
      *                Add parm to EmpYrRR procedure to allow for year selection.
     *
      * 05/14/12 C:189 Project: WC2115
      *                Add procedures:
      *                - GetIncLimit
     *
      * 04/12/12 C:33  Project: WC2116
      *                Add procedures:
      *                - GetRatingState
      *                - GetQuoteSts
      *                - GetQuoteStsDesc
     *
      * 03/06/12  C:189 Project: WC2115
     *                 Added a new procedure to fetch Employer Phone# and
     *                 area code
     *
      * 03/07/12  C:190 Project: WC2116
     *                 Add new procedures to fetch full name of employer.
     *
      *********************************************************************
     ddsWAQTPpr      e ds                  extname(WAQTP)
     D*=====================================================================
     D* Employer processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* CoverageLapse - Determine if there is a lapse in coverage
    P*
    P*  CoverageLapse - returns *on if lapse, otherwise *off
    P*   inCo#      - accepts the current company          (required)
    P*   inGrp      - accepts the current group            (required)
    P*   inEmp#     - accepts the current employer         (required)
    P*   inDiv      - accepts the current division         (required)
    P*   inFyr      - accepts the current year             (required)
    P*   inQte      - accepts the environment              (required)
    P*   inEffDate  - accepts the current effective date   (optional)
    P*   inXRefGrp  - accepts the x-ref employer           (optional)
    P*   inXRefEmp# - accepts the x-ref division           (optional)
    P*   inXRefDiv  - accepts the x-ref year               (optional)
    P*====================================================================
    P*
     dCoverageLapse    pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
     d inQte                          1    const
     d inEffDate                       d   const options(*nopass)
     d inXRefGrp                      3  0 const options(*nopass)
     d inXRefEmp#                     9  0 const options(*nopass)
     d inXRefDiv                      5  0 const options(*nopass)

     P***  Begin Add  ***  07/01/21  ***********************************
     P*====================================================================
     P* DeleteEmployer - Delete employer (one or all divisions)
     P*
     P* DeleteEmployer - returns *on if successful
     P*   json      - accepts a json string of parms       (required)
     P*    accepts--
     P*     rrn:     - rrn of WMEMP             (required)
     P*     quote:   - 'Q' if from quote
     P*     div:     - '*ALL' for all divisions
     P*    returns-- this will be blank if successful deletion
     P*     ind:     - indicators (this was from W0050R)
     P*     msgId:   - id from WMESGG
     P*     msg:     - message text
     P*====================================================================
     DDeleteEmployer   pr             1n
     D ioJson                          a   len(1000) varying
     P***  End   Add  ***  07/01/21  ***********************************
    D*
    P*====================================================================
    P* DivCount - Count the # of divisions for an employer
    P*
    P*  DivCount - returns # of divsions for an employer
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*====================================================================
    P*
     DDivCount         PR             5  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
    D*
    P*====================================================================
    P* EmpAuth - Determine if user is authorized to employer
    P*
    P*  EmpAuth - returns *on if the user is authorized, otherwise *off
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   userEntry - accepts the user to search over (defaults to Q1USER)
    P*   accessLvl - returns access level (ie. 'I'nquiry or 'A'll)
    P*====================================================================
    P*
     DEmpAuth          PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D userEntry                     10    const options(*nopass)
     D accessLvl                      1          options(*nopass)
    D*
    P*====================================================================
    P* EmpCommRt - Load field with commission rate (renewal vs. renewal)
    P*
    P*  EmpCommRt - returns commission rate
    P*   AHrrn     - passed in for validation                  (required)
    P*   ARCode    - passed in for validation                  (required)
    P*   transDt   - passed in for validation                  (required)
    P*   agent     - passed in for primary or secondary agent  (optional)
    P*====================================================================
    P*
     DEmpCommRt        PR             5  2
     D AHrrn                          9  0 const
     D ARCode                         2    const
     D transDt                        7  0 const
     D agent                          1    const options(*nopass)
    D*
    P*====================================================================
    P* EmpDivName - Load field with compressed Emp#/Div/Name
    P*
    P*  EmpDivName - returns compressed Emp#/Div/Name
    P*   emp#      - passed in for validation  (required)
    P*   div       - passed in for validation  (required)
    P*   emrrn     - returns the RRN of WMEMP
    P*   group     - passed in for validation
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     DEmpDivName       PR            80
     D emp#                           9  0                  const
     D div                            5  0                  const
     D emrrn                          9  0 options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D*
    D*====================================================================
    D* EmpSrch - Search employers
    D*
    D*  EmpSrch - returns RRN of WMEMP
    D*   inkc   - returns *on if F3 was pressed from search  (required)
    D*   inkl   - returns *on if F12 was pressed from search (required)
    D*   error  - returns a '1' if group passed in is invalid
    D*   group  - accepts the group # to search over
    D*   co#    - accepts the company # to search over
    D*====================================================================
     DEmpSrch          PR             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D*
    D*====================================================================
    D* EmpYrSrch - Search employer policy years
    D*
    D*  EmpYrSrch - returns RRN of WDELP
    D*   inkc     - returns *on if F3 was pressed from search  (required)
    D*   inkl     - returns *on if F12 was pressed from search (required)
    D*   error    - returns a '1' if group passed in is invalid
    D*              returns a '2' if employer passed in is invalid
    D*   emp#     - accepts the employer # to search over
    D*   div      - accepts the divsion to search over
    D*   group    - accepts the group # to search over
    D*   co#    - accepts the company # to search over
    D*====================================================================
     DEmpYrSrch        PR             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D emp#                           9  0 options(*nopass) const
     D div                            5  0 options(*nopass) const
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     P*
    P*====================================================================
    P* GetBillSch - Get employer billing schedule
    P*
    P*  GetBillSch - returns billing schedule (code/desc)
    P*   ELrrn     - accepts the RRN of WDELP             (required)
    P*   BSrrn     - returns the RRN of WTBSP
    P*   code      - returns the billing schedule code
    P*====================================================================
    P*
     DGetBillSch       PR            50
     D ELrrn                          9  0 const
     D BSrrn                          9  0       options(*nopass)
     D code                          10          options(*nopass)
     P*
    P*====================================================================
    P* GetCxDate - Get cancellation date
    P*
    P*  GetCxDate  - returns the cancellation date
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   cx_reason - returns the cx code/description
    P*   env       - accepts the environment Quote/Live
    P*====================================================================
    P*
     DGetCxDate        PR             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D cx_reason                     65    options(*nopass)
     D env                            1          options(*nopass)
     P*
    P*====================================================================
    P* GetCxRsn - Get cancellation reason
    P*
    P*  GetCxRsn - returns the cancellation reason code/desc
    P*   co#     - accepts the company # to search over (required)
    P*   group   - accepts the group to search over     (required)
    P*   emp#    - accepts the employer to search over  (required)
    P*   div     - accepts the employer to search over  (required)
    P*   cxDate  - accepts the cx date to search over   (required)
    P*   cxCode  - returns the cx code
      *   outNcciCode - returns NCCI Cx Code (Optional)
      *   outOvrCode  - returns override renewal code (Optional)
      *   inTransDate - to hit table with (Optional)
    P*====================================================================
    P*
     DGetCxRsn         PR            65
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D cxDate                         7  0 const
     D cxCode                         2    options(*nopass)
      *** beg add ***** 03/16/20 **************************
     D outNcciCode                    2    options(*nopass)
     D outOvrCode                     1    options(*nopass)
     D inTransDate                    7s 0 options(*nopass)
      *** end add ***** 03/16/20 **************************
     P*
    P*====================================================================
    P* GetEmpName - Get employer name
    P*
    P*  GetEmpName - returns employer name
    P*   emp#      - passed in for validation  (required)
    P*   div       - passed in for validation  (required)
    P*   emrrn     - returns the RRN of WMEMP
    P*   group     - passed in for validation
    P*   co#       - passed in for validation
    P*====================================================================
    P*
     DGetEmpName       PR            40
     D emp#                           9  0                  const
     D div                            5  0                  const
     D emrrn                          9  0 options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D mode                           1A   options(*nopass) const
     P*
    P*====================================================================
    P* GetEmpPol# - Get employer policy #
    P*
    P*  GetEmpPol# - returns employer policy #
    P*   elrrn     - recives the RRN for WDELP (required)
    P*   type      - if passed as blank or not passed the policy # will
     P*                be returned as WMRPP/RPCAP# if found, otherwise
     P*                GRP/EMP#/YR
     P*               if passed as 'C' the policy # will be returned as
     P*                the carrier policy # (*blank if not found)
     P*               if passed as 'E' the policy # will be returned as
     P*                GRP/EMP#/YR
    P*====================================================================
    P*
     DGetEmpPol#       PR            25
     D elrrn                          9  0 const
     D type                           1    const options(*nopass)
     P*
    P*====================================================================
    P* GetEmpRRN - Get employer name
    P*
    P*  GetEmpRRN - returns employer RRN
    P*   emp#     - passed in for validation  (required)
    P*   div      - passed in for validation  (required)
    P*   group    - passed in for validation
    P*   co#      - passed in for validation
    P*====================================================================
    P*
     DGetEmpRRN        PR             9  0
     D emp#                           9  0                  const
     D div                            5  0                  const
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     P*
    P*====================================================================
    P* GetEmpYrRR - Get employer policy year RRN
    P*
    P*  GetEmpYrRR - return employer policy year RRN
    P*   yr        - passed in for validation  (required)
    P*   emp#      - passed in for validation  (required)
    P*   div       - passed in for validation  (required)
    P*   group     - passed in for validation
    P*   co#      - passed in for validation
    P*   yearFlag - passed in for validation
    P*   env       - accepts the environment Quote/live
    P*====================================================================
    P*
     DGetEmpYrRR       PR             9  0
     D yr                             3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D yearFlag                       1    options(*nopass) const
     D env                            1    options(*nopass) const
    P*
    P*====================================================================
    P* GetPremFin - Get employer premium finance co
    P*
    P*  GetPremFin - returns premimium finance co (code/name)
    P*   ELrrn     - accepts the RRN of WDELP             (required)
    P*   date      - premium finance co as of date
    P*   PFrrn     - returns the RRN of WMPFP
    P*   code      - returns the premium finance co code
    P*====================================================================
    P*
     DGetPremFin       PR            50
     D ELrrn                          9  0 const
     D date                            d   const options(*nopass)
     D PFrrn                          9  0       options(*nopass)
     D code                           5          options(*nopass)
    P*
    P*====================================================================
    P* GetPrvEmp# - Get previous employer #
    P*
    P*  GetPrvEmp# - returns previous employer # (Group/Emp#/Div)
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   p_group   - returns the previous group #
    P*   p_emp#    - returns the previous employer #
    P*   p_div     - returns the previous division
    P*====================================================================
    P*
     DGetPrvEmp#       PR            20
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D p_group                        3  0 options(*nopass)
     D p_emp#                         9  0 options(*nopass)
     D p_div                          5  0 options(*nopass)

     P*====================================================================
     P* Get the quote status for employer
     P*
     P*  GetRatingState - returns rating state for a policy
     P*   co#   - accepts the company # of policy (required)
     P*   group - accepts the group of policy     (required)
     P*   emp#  - accepts the employer of policy  (required)
     P*   div   - accepts the divis on of policy  (required)
     P*   yr    - accepts the yrvision of policy  (optional, default *HIVAL)
     P*====================================================================
     DGetQuoteSts      pr             1
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inYr                           3  0 const options(*nopass)
     D inStat                         1    const options(*nopass)

     P*====================================================================
     P* Get the quote status description for employer
     P*
     P*  GetRatingState - returns rating state for a policy
     P*   co#   - accepts the company # of policy (required)
     P*   group - accepts the group of policy     (required)
     P*   emp#  - accepts the employer of policy  (required)
     P*   div   - accepts the divis on of policy  (required)
     P*   yr    - accepts the yrvision of policy  (optional, default *HIVAL)
     P*====================================================================
     DGetQuoteStsDesc  pr            30
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inYr                           3  0 const options(*nopass)
     D inStat                         1    const options(*nopass)

     P*====================================================================
     P* Get the rating state for employer
     P*
     P*  GetRatingState - returns rating state for a policy
     P*   co#   - accepts the company # of policy (required)
     P*   group - accepts the group of policy     (required)
     P*   emp#  - accepts the employer of policy  (required)
     P*   div   - accepts the divis on of policy  (required)
     P*   yr    - accepts the yrvision of policy  (optional, default *HIVAL)
     P*====================================================================
     DGetRatingState   pr             2
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inYr                           3  0 const options(*nopass)
     D env                            1          options(*nopass)

    P*====================================================================
    P* GetRateSet - Get rate set code
    P*
    P*  GetRateSet - returns rate set code
    P*   ELrrn     - accepts the RRN of WDELP             (required)
    P*====================================================================
    P*
     DGetRateSet       PR             3
     D ELrrn                          9  0 const

     P*====================================================================
     P* GetScheduleRatingFactor - Calculate/return the schedule rating factor
     P*  based on the answers to the questions
     P*
     P*  GetScheduleRatingFactor - returns schedule rating factor
     P*   co#       - accepts the company # to search over      (required)
     P*   app#      - accepts the application #                 (required)
     P*   year      - accepts the employer year to search over  (required)
     P*   group     - accepts the group to search over          (required)
     P*   emp#      - accepts the employer to search over       (required)
     P*   division  - accepts the division to search over       (required)
     P*   asOfdt    - returns the date of the latest question answered
     P*====================================================================
     DGetScheduleRatingFactor...
     D                 pr             5  4
     D inCo#                          3  0 const
     D inApp#                         9  0 const
     D inYear                         3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D outAsOfDt                       d   options(*nopass)
    P*
    P*====================================================================
    P* GetStRptDv - Get employer state reporting division
    P*
    P*  GetStRptDv - returns employer state reporting division
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   emrrn     - returns the RRN of WMEMP
    P*====================================================================
    P*
     DGetStRptDv       PR             5  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D emrrn                          9  0 options(*nopass)
     D strpflg                        1    options(*nopass)
    P*
    P*====================================================================
    P* HCNPgmExists - Check for HCN Program on the policy
    P*
    P*  HCNPgmExists - return *on if exists
    P*   co#     - passed in for validation  (required)
    P*   group   - passed in for validation  (required)
    P*   emp#    - passed in for validation  (required)
    P*   div     - passed in for validation  (required)
    P*   fyr     - passed in for validation  (required)
    P*====================================================================
     DHCNPgmExists     pr             1n
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
     D date                           7  0 const options(*nopass)
     D rrn                            9  0       options(*nopass)
    P*
    P*====================================================================
    P* RtvQuoteLib - Retrieve the library for employer
    P*
    P*  RtvQuoteLib - returns library where WMEMP should exist
    P*   env     - passed in for envrionment requesting (required)
    P*====================================================================
     DRtvQuoteLib      pr            10
     D inEnv                          4    const
    D*
    D*====================================================================
    D* ValidEmp - Valid employer/division
    D*
    D*  ValidEmp - return *on if, employer (division) valid
    D*   emp#    - passed in for validation  (required)
    D*   div     - passed in for validation
    D*   emrrn   - returns the RRN of WMEMP
    D*   group   - passed in for validation
    D*   co#     - passed in for validation
    D*====================================================================
     DValidEmp         PR             1
     D emp#                           9  0                  const
     D div                            5  0 options(*nopass) const
     D emrrn                          9  0 options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
    D*
    D*====================================================================
    D* ValidEmpYr - Valid employer policy year
    D*
    D*  ValidEmpYr - return *on if, year is valid for employer
    D*   yr        - passed in for validation  (required)
    D*   emp#      - passed in for validation  (required)
    D*   div       - passed in for validation
    D*   elrrn     - returns the RRN of WDELP
    D*   group     - passed in for validation
    D*   co#       - passed in for validation
    D*====================================================================
     DValidEmpYr       PR             1
     D yr                             3  0                  const
     D emp#                           9  0                  const
     D div                            5  0 options(*nopass) const
     D elrrn                          9  0 options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D
    P*====================================================================
    P* GetEmpFullName - Get employer name
    P*
    P*  GetEmpFullName - returns employer name
    P*   emp#      - passed in for validation  (required)
    P*   div       - passed in for validation  (required)
    P*   emrrn     - returns the RRN of WMEMP
    P*   group     - passed in for validation
    P*   co#       - passed in for validation
    P*   nametype  - passed in for which name: DBA or federal (default to DBA)
    P*====================================================================
    P*
     DGetEmpFullName   PR           100
     D emp#                           9  0                  const
     D div                            5  0                  const
     D emrrn                          9  0 options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D nametype                       3    options(*nopass) const
     D mode                           1A   options(*nopass) const
     P*
    P*====================================================================
    P* GetCxDate1- Get cancellation date
    P*
    P*  GetCxDate1 - returns the cancellation date
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   fyr       - accepts the employer to search over  (required)
    P*====================================================================
    P*
     DGetCxDate1       PR             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
     D cfdt                           7  0
     D redt                           7  0
     P*
    P*====================================================================
    P*====================================================================
    P* GetBillSts  Get Bill Status.
    P*
    P*  GetBillsts - returns the bill status
    P*   ELrrn     - passed in for validation                  (required)
    P*====================================================================
    P*
     DGetBillSts       PR            15
     D ELrrn                          9  0 const
     P*
    P*====================================================================
    P*====================================================================
    P* GetEmpPhn#- Get Employer Phone#
    P*
    P*  GetEMpPhn# - returns the Emplyer Phone#
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   Area      - Returns Area Code
    P*====================================================================
    P*
     DGetEMpPhn#       PR             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Area                           3  0
    P*====================================================================
    P* GetEmIncDt  Get Employer Inception date.
    P*
    P*  GetEmIncDt - returns the Employer Inception date
    P*   co#     - passed in for validation    (required)
    P*   group   - passed in for validation    (required)
    P*   emp#      - passed in for validation  (required)
    P*   div       - passed in for validation  (required)
    P*====================================================================
    P*
     DGetEmIncDt       PR             7  0
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     d outFedId                       9  0 options(*nopass)
    P*====================================================================
    P* GetIncLimit - Get Employer Increased/Liability Limits
    P*
    P*  GetIncLimit -
    P*   co#     - passed in for validation  (required)
    P*   group   - passed in for validation  (required)
    P*   emp#    - passed in for validation  (required)
    P*   div     - passed in for validation  (required)
    P*   fyr     - passed in for validation  (required)
    P*   OutincLt1   Returns Increased Limit 1
    P*   OutincLt2   Returns Increased Limit 2
    P*   OutincDt1   Returns Increased Limit 1 Eff Date
    P*   OutincDt2   Returns Increased Limit 2 Eff Date
     P*   OutincDes   Returns Standard Liability limit description
     P*   Outeffdt    Returns policy year eff date
     P*   OutincLt1D  Returns Inc Limit 1 description
     P*   OutincLt2D  Returns Inc Limit 2 description
    P*====================================================================
    P*
     DGetIncLimit      PR
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inST                           2
     D OutincLt1                      2
     D OutincLt2                      2
     D OutincDt1                      7  0
     D OutincDt2                      7  0
     D OutincDes                     30
     D Outeffdt                       7  0
     D OutincLt1D                    30    options(*nopass)
     D OutincLt2D                    30    options(*nopass)
    P*====================================================================
    P* RtvPolYr - Retrieve Policy Year
    P*
    P*  RtvPolYr   - returns the Policy Year
    P*   co#       - passed in to find policy year (Required)
    P*   group     - passed in to find policy year (Required)
    P*   emp#      - passed in to find policy year (Required)
    P*   div       - passed in to find policy year (Required)
    P*   date      - passed in to find policy year (Required)
    P*====================================================================
     DRtvPolYr         PR             3  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D date                           7  0 const
    P*====================================================================
    P* RtvPolFrDt - Retrieve Policy From Date
    P*
    P*  RtvPolFrDt - returns the Policy From Date
    P*   co#       - passed in to find policy from date (Required)
    P*   group     - passed in to find policy from date (Required)
    P*   emp#      - passed in to find policy from date (Required)
    P*   div       - passed in to find policy from date (Required)
    P*   fyr       - passed in to find policy from date (Required)
    P*====================================================================
     DRtvPolFrDt       PR             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
    P*====================================================================
    P* RtvPolToDt - Retrieve Policy To Date
    P*
    P*  RtvPolToDt - returns the Policy To Date
    P*   co#       - passed in to find policy to date (Required)
    P*   group     - passed in to find policy to date (Required)
    P*   emp#      - passed in to find policy to date (Required)
    P*   div       - passed in to find policy to date (Required)
    P*   fyr       - passed in to find policy to date (Required)
    P*====================================================================
     DRtvPolToDt       PR             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
    P*====================================================================
    P* RtvMedExcl - Medical Exclusion Flag
    P*
    P*  RtvMedExcl - returns the Medical Exclusion Flag
    P*   co#       - passed in to find policy to date (Required)
    P*   group     - passed in to find policy to date (Required)
    P*   emp#      - passed in to find policy to date (Required)
    P*   div       - passed in to find policy to date (Required)
    P*   fyr       - passed in to find policy to date (Required)
    P*====================================================================
     DRtvMedExcl       PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
    P*====================================================================
    P* GetPrvEmp# - Get previous employer #
    P*
    P*  GetLatXEmp#- returns future employer # (Group/Emp#/Div)
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   f_co#     - returns the future company
    P*   f_group   - returns the future group #
    P*   f_emp#    - returns the future employer #
    P*   f_div     - returns the future division
    P*   f_fyr     - returns the future year
    P*====================================================================
    P*
     DGetLatXEmp#      PR            20
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D f_co#                          3  0 options(*nopass)
     D f_group                        3  0 options(*nopass)
     D f_emp#                         9  0 options(*nopass)
     D f_div                          5  0 options(*nopass)
     D f_fyr                          3  0 options(*nopass)

    P*====================================================================
    P* GetFutEmp# - Get future employer #
    P*
    P*  GetfutEmp# - returns future employer # (Group/Emp#/Div)
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   f_group   - returns the future group #
    P*   f_emp#    - returns the future employer #
    P*   f_div     - returns the future division
     DGetFutEmp#       PR            20
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D f_group                        3  0 options(*nopass)
     D f_emp#                         9  0 options(*nopass)
     D f_div                          5  0 options(*nopass)
    P*====================================================================
    P* GetExclMed - Get Exclude Medical Flag
    P*
    P*  GetExclMed - returns Exclude Medical Flag
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*   year      - accepts the policy year              (required)
     DGetExclMed       PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D year                           3  0 const
    P*====================================================================
    P* GetRnwlQte - Get Renewal Quote Flag
    P*
    P*  GetRnwlQte - returns Exclude Medical Flag
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*====================================================================
     DGetRnwlQte       PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const

    P*====================================================================
    P* GetRnwlQteDiv - Get Renew to Quote Division Flag
    P*
    P*  GetRnwlQteDiv - returns Renew to Quote Division Flag
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the employer to search over  (required)
    P*====================================================================
     DGetRnwlQteDiv    PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const

    P*====================================================================
    P* GetEmpRating - Get Large Deduc, Retro Rating, or Schedule Rating info
    P*
    P*  GetEmpRating - returns Large Deductible or Retro Rating flag
    P*   co#       - accepts the company # to search over  (required)
    P*   group     - accepts the group to search over      (required)
    P*   emp#      - accepts the employer to search over   (required)
    P*   div       - accepts the employer to search over   (required)
    P*   fyr       - accepts the emplr year to search over (required)
    P*   all       - accepts flag if entire policy is used (optional)
    P*   sched     - returns the scheduled rating flag     (optional)
    P*   large     - returns the large deductible for policy (optional)
    P*   retro     - returns the retro rating for policy     (optional)
    P*====================================================================
     DGetEmpRating     PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
     D all                            1    const options(*nopass)
     D sched                          1    options(*nopass)
     D large                          1    options(*nopass)
     D retro                          1    options(*nopass)
     *====================================================================
      * GetTotPrem  - returns total premium of all divisions
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  fyr        - accepts the emplr year to search over
     *  emp#       - accepts the employer to search over
      *  audFlg     - 'C' pulls premium for current status
      *               'E' pulls estimated premium
      *               'A' pulls audited premium
      *  lvlPrmFlg  - 'M' for Manual
      *               'J' for Subject
      *               'E' for Exp Modified
      *               'S' for Standard
      *               'N' for Normal
      *               'A' for Annual
      *               'Z' for Annual - Exp Const
     *====================================================================
     dGetTotPrem       pr            13  2
     d co#                            3  0 const
     d fnd                            3  0 const
     d fyr                            3  0 const
     d emp#                           9  0 const
     d audFlg                         1    const
     d lvlPremFLg                     1    const
     *====================================================================
      * SelPremAmt - returns estimated or audited premium amount
      *              depending on what the request is for and what
      *              the status of the policy is in
      *  polStat   - indicates audit status of policy
      *  audFlg    - indicates whether Audited, Current, or Estimated
      *              Premium is requested.
      *  estPrem   - Estimated premium from WDELP
      *  audPrem   - Audited premium from WDELP
     *====================================================================
     dSelPremAmt       pr            13  2
     d polStat                        1    const
     d audFlg                         1    const
     d estPrem                       11  2 const
     d audPrem                       11  2 const
      //***  Begin Add  ***  06/22/20  *********************************
     P*====================================================================
     P* Set the FQ lock status field (EMFQLK).  This value will be determined
     P*  by the quote status.
     P*
     P* SetFQLockSts - returns ' ' if the policy should be available to underwriter
     P*                returns 'P' if the policy should be locked to all but Portal
     P*                returns 'Y' if the policy should be locked to all but FQ
     P*                returns the current value if unable to determine locking
     P*   co#        - accepts the company #                 (required)
     P*   status     - accepts the quote status value        (required) {QTtype}
     P*   entryPoint - accepts the entry point of the policy (required) {EMiUsr}
     P*   current    - accepts the current value of EMFQLK   (optional)
     P*====================================================================
     dSetFQLockSts     pr             1
     d inCo#                          3  0 const
     d inQteSts                       1    const
     d inEntryPoint                  10    const
     d inCurrent                      1    const options(*nopass)
      //***  End   Add  ***  06/22/20  *********************************
      *====================================================================
      * GetQteMode - Get Quote Mode
      *
      * GetQteMode - returns 'Q' if the Employer is in Quote.
      *              returns 'L' if the Employer is in Live.
      *              returns '2' if the Employer is in both Live and Quote.
      *   co#       - accepts the company # to search over  (required)
      *   group     - accepts the group to search over      (required)
      *   emp#      - accepts the employer to search over   (required)
      *   div       - accepts the employer to search over   (required)
      *   QteMode   - returns the Quote Mode for policy     (optional)
      *====================================================================
     dGetQteMode       pr             1
     d co#                            3  0 const
     d fund                           3  0 const
     d emp#                           9  0 const
     d div                            5  0 const

     P*====================================================================
     P* RtvEnviLib - Retrieve the library for the Environment
     P*
     P*  RtvEnviLib - returns environment in which the job is running
     P*   env     - *CURRENT/*OTHER is passed to fetch the library
     P*====================================================================
     DRtvEnviLib       pr
     D inEnv                         10    const
     D outLib                        10    const
     D outMLib                       10    const

     P*====================================================================
     P* GetFQLockSts - Get FastQuote Employer lock status
     P*
     P* GetFQLockSts - returns '0' if the Employer is not locked by FQ.
     P*                returns '1' if the Employer is locked by FQ.
     P*
     P*   co#       - accepts the company # to search over  (required)
     P*   group     - accepts the group to search over      (required)
     P*   emp#      - accepts the employer to search over   (required)
     P*   div       - accepts the employer to search over   (required)
     P*   QteMode   - returns the Quote Mode for policy     (optional)
     P*   message   - accepts whether the message should be
     P*                displayed                            (optional)
     P*   app       - returns the locking app               (optional)
     P*====================================================================
     DGetFQLockSts     PR             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D qteMode                        1    const
     D inMsg                          1n   const options(*nopass)
     D outApp                        25          options(*nopass)
     P*====================================================================
     P* GetLastDivision - Get Last Division Used
     P*
     P* GetLastDivision - Returns Lat Division Used.
     P*
     P*   co#       - accepts the company # to search over  (required)
     P*   group     - accepts the group to search over      (required)
     P*   emp#      - accepts the employer to search over   (required)
     P*====================================================================
     DGetLastDivision  PR             5  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const

      //***  Begin Add  ***  01/19/22  *************************
      *====================================================================
      * IsUseNetRate - Determine if the policy should be using net rate
      *
      * IsUseNetRate - returns *on if using net rate, otherwise *off
      *
      *   co#       - accepts the company # to search over  (required)
      *   group     - accepts the group to search over      (required)
      *   emp#      - accepts the employer to search over   (required)
      *   div       - accepts the division to search over   (required)
      *   fyr       - accepts the year to search over       (required)
      *====================================================================
     dIsUseNetRate     pr             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
      //***  End   Add  ***  01/19/22  *************************

      *====================================================================
      * MultiState - Determine if a policy is a multi-state policy
      *
      * MultiState - returns '0' if not a multi-state policy
      *                returns '1' if a multi-state policy
      *
      *   co#       - accepts the company # to search over  (required)
      *   group     - accepts the group to search over      (required)
      *   emp#      - accepts the employer to search over   (required)
      *   fyr       - accepts the year to search over       (optional)
      *====================================================================
     dMultiState       pr             1n
     d co#                            3  0 const
     d fund                           3  0 const
     d emp#                           9  0 const
     d fyr                            3  0 const options(*nopass)
     *====================================================================
      * ChkRnwStatus - Checks Renewal Flag from State Reporting division
      *              - Returns *on if policy is flagged to renew
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  fyr        - accepts the emplr year to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     dChkRnwStatus     pr              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d fyr                            3  0 const
     d errorMsg                     200    options(*nopass)
     *====================================================================
      * ChkCxStatus - Retrieves cancellation status of division
      *             - Returns *on if division has cancellation date
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     dChkCxStatus      pr              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d errorMsg                     200    options(*nopass)
     *====================================================================
      * ChkFutDtl   - Checks if policy year has already been renewed to
      *                Live or Quote
      *             - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
     *  fyr        - accepts the emplr year to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     dChkFutDtl        pr              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d fyr                            3  0 const
     d errorMsg                     200    options(*nopass)
     *====================================================================
      * ChkQteEmp    - Checks if employer exists in quote
      *              - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     dChkQteEmp        pr              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d errorMsg                     200    options(*nopass)
      *====================================================================
      * ChkPolDates - Checks if future group policy year exists to renew to
      *               and if new policy from date lands in the future group
      *               policy year date range
      *               Validates that the expiration date is valid to
      *               to renew. It can't exist in the current fund yea
      *               fund year.
      *             - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
     *  fyr        - accepts the emplr year to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     dChkPolDates      pr              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d fyr                            3  0 const
     d errorMsg                     200    options(*nopass)
     *====================================================================
     *====================================================================
      * PolPageProcessed - checks if policy page generated
      *              - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  fyr        - accepts the year to search over
      *  inState    - (optional)
     *====================================================================
     dPolPagProcessed  pr              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d fyr                            3  0 const
     d inState                        2    options(*nopass)
       //===================================================================
       // Create 'with' conditions to build policy information including data from:
       //  WDELP, WMEMP, WMAHP, WMAAP
       //             - Returns a partial SQL string to be included in 'with'
       // as of Date  - agent history as of date (current date if not passed)
       //==================================================================
     dSqlWithPolicyInfo...
     d                 pr          5000    varying
     d inAsOfDate                      d   options(*nopass)
      *====================================================================
      * SubHistStatus - checks if policy page generated
      *               - Returns *on if so
     *  co#          - accepts the company # to search over
     *  group        - accepts the group to search over
     *  emp#         - accepts the employer to search over
     *  fyr          - accepts the year to search over
      *  inState      - (optional)
     *====================================================================
     dSubHistStatus    pr             1
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d fyr                            3  0 const
     d inState                        2    options(*nopass)
      *====================================================================
      //***  Begin Add  ***  06/22/20  *********************************
     P*====================================================================
     P* Update the FQ lock status field (EMFQLK).  This value will be determined
     P*  by the quote status.
     P*
     P* UpdFQLockSts
     P*   co#        - accepts the company #                 (required)
     P*   fnd        - accepts the group                     (required)
     P*   emp#       - accepts the employer #                (required)
     P*   div        - accepts the division                  (required)
     P*   status     - accepts the quote status value        (required) {QTtype}
     P*====================================================================
     dUpdFQLockSts     pr
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inQteSts                       1    const
      //***  End   Add  ***  06/22/20  *********************************
     P*====================================================================
     P* ChkRenewal - Determine if it is a renewal
     P*
     P*  ChkRenewal - returns *on if Renewal, otherwise *off
     P*   inCo#      - accepts the current company          (required)
     P*   inGrp      - accepts the current group            (required)
     P*   inEmp#     - accepts the current employer         (required)
     P*   inFyr      - accepts the current year             (required)
     P*   inQte      - accepts the environment              (required)
     P*====================================================================
     P*
     dChkRenewal       pr             1n
     d inCo#                          3  0 const
     d inGrp                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inQte                          1    const
      *====================================================================
      * GetStRptPDays - returns the number of policy days (Subtracts
      *  for leap day)
      *   inCo#  - accepts the current company   (required)
      *   inGrp  - accepts the current group     (required)
      *   inEmp# - accepts the current employer  (required)
      *   inFyr  - accepts the current year      (required)
      *====================================================================
     dGetStRptPDays    pr             3  0
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inDiv                          5  0 const
      *====================================================================
      * EdtPeoPolType - Returns Error Message
      *   inCo#     - accepts the current company     (required)
      *   inGrp     - accepts the current group       (required)
      *   inEmp#    - accepts the current employer    (required)
      *   inDiv     - accepts the division            (required)
      *   inJrSt    - accepts the jurisdiction state  (required)
      *   inEfDt    - accepts the effective date      (required)
      *====================================================================
     dEdtPeoPolType    pr           100
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inJrSt                         2    const
     d inEfDt                         7  0 const
     d inCall                         1    const options(*nopass)
      *====================================================================
      * QuoteStsChgHst - Updates quote status change history
      *====================================================================
     dQuoteStsChgHst...
     d                 pr             1n
     d inWAQTPnew                          const likeds(dsWAQTPpr)
     d inWAQTPold                          const likeds(dsWAQTPpr)
      *====================================================================
      * RtvDeductible - returns deductible plan (blank, small, large)
      *                 as well as all deductible information.
      *   inCo#  - accepts the current company   (required)
      *   inGrp  - accepts the current group     (required)
      *   inEmp# - accepts the current employer  (required)
      *   inDiv  - accepts the current division  (required)
      *   inFyr  - accepts the current year      (required)
     *   premPct - returns the premium disc pct (optional)
     *   dedAmt  - returns the deduct amt/occur (optional)
     *   dedAgg  - returns the deduct aggregate (optional)
     *   lossSub - returns the loss subject cde (optional)
     *   basisDed- returns the basis deduct cde (optional)
     *   dedNet  - returns the deduct net/gross (optional)
     *   dedVerb - returns the deduct verbiage  (optional)
     *   dedClas - returns the deduct class cde (optional)
     *   dedPct  - returns the deduct percentag (optional) (not used on our system)
      *====================================================================
     dRtvDeductible    pr             1
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
     D premPct                       12  9 options(*nopass)
     D dedAmt                         9  0 options(*nopass)
     D dedAgg                         9  0 options(*nopass)
     D lossSub                        2  0 options(*nopass)
     D basisDed                       2  0 options(*nopass)
     D dedNet                         1    options(*nopass)
     D dedVerb                        1    options(*nopass)
     D dedClas                        5  0 options(*nopass)
     D dedPct                         2  0 options(*nopass)
      *====================================================================
      * CollateralSR   - returns amount of collateral based on amount passed
      *                  into program.
      *   inCo#  - accepts the current company   (required)
      *   inGrp  - accepts the current group     (required)
      *   inEmp# - accepts the current employer  (required)
      *   inFyr  - accepts the current pol year  (required)
     *   inMode - accepts the mode/type of call (required)
     *   inAmt  - accepts the S/R billing amt   (required)
     *   inDate - accepts the date for setup.   (optional)
     *   inBBatch - Billing Batch # in/out.     (optional)
     *   inPBatch - Payment Batch # in/out.     (optional)
     *   inBAR  - Billing a/r code in/out.      (optional)
     *   inPAR  - Payment a/r code in/out.      (optional)
      *====================================================================
     dCollateralSR     pr            11  2
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inMode                         1    const
     d inAmt                         11  2 const
     d inDate                         7  0 const options(*nopass)
     d inBBatch                       9  0 options(*nopass)
     d inPBatch                       9  0 options(*nopass)
     d inBAR                          2    options(*nopass)
     d inPAR                          2    options(*nopass)
      //***  Begin Add  ***  12/14/20  *******************
      *====================================================================
      * CxJSONData returns JSON string of cx data
      *  inCo#  - accepts the current company   (required)
      *  inFnd  - accepts the current group     (required)
      *  inEmp# - accepts the current employer  (required)
      *  inDiv  - accepts the current division  (required)
      *====================================================================
     dCxJSONData       pr              a   Len(1000) Varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
      //***  End   Add  ***  12/14/20  *******************
      ***  Begin Add  ***  08/24/21  ***********************************
      *====================================================================
      * DefaultRatingFactors - Default Rating Factors
      *
      *    accepts--
      *     rrn:       - rrn of WDELP   (required)
      *     Entry Type - Entry type, Curent Rate code and
      *                  Current Billing plan  (required)
      *====================================================================
     dDefaultRatingFactors...
     d                 pr
     d inRrn                          9  0 const
     d inEntryType                   30    const
      ***  End   Add  ***  08/24/21  ***********************************
     d*** Begin Add *** 02/22/22 ******************************************
     P*====================================================================
     P* GetNameAddress - Retrieve Employer Name/Address Information.
     P*
     P* inCompany#             - Company # (Required)
     P* inGroup#               - Group # (Required)
     P* inEmployer#            - Employer# (Required)
     P* inDivision             - Division (Required)
     P* inYear                 - Policy Year (Required)
     P* outEmployerName1       - Employer Name 1
     P* outEmployerName2       - Employer Name 2
     P* outEmployerName3       - Employer Name 3
     P* outEmployerName4       - Employer Name 4
     P* outEmployerName5       - Employer Name 5
     P* outEmployerName6       - Employer Name 6
     P* outEmployerName7       - Employer Name 7
     P* outAddressLocation     - Address Location Position
     P* outNameLocation        - Name Location Position
     P* outFederalNameLocation - Feneral Name Location Position
     P* outPEOName1            - PEO Name Line 1
     P* outPEOName2            - PEO Name Line 2
     P* outContactLocation     - Constact Location Position
     P* outAMName1             - Fully Formatted Name 1
     P* outAMName2             - Fully Formatted Name 2
     P* outAMName3             - Fully Formatted Name 3
     P* outAMName4             - Fully Formatted Name 4
     P* outAMName5             - Fully Formatted Name 5
     P* outAMName6             - Fully Formatted Name 6
     P* outAMName7             - Fully Formatted Name 7
     P* outAMName8             - Fully Formatted Name 8
     P* outAMName9             - Fully Formatted Name 9
     P* outPEOLocation         - PEO Location Position
     P* outAddressListCount    - Address List Count
     P* outNameListCount       - Name List Count
     P* outFedNameListCount    - Federal Name List Count
     P* outPEONameListCount    - PEO Name List Count
     P* outContactListCount    - Contact List Count
     P* outScripturaName1      - Name Field 1 for Scriptura
     P* outScripturaName2      - Name Field 2 for Scriptura
     P* outScripturaName3      - Name Field 3 for Scriptura
     P* outScripturaName4      - Name Field 4 for Scriptura
     P* outInsuredFein         - Insured FEIN
     P* inOrder                - Fill From Bottom if value is W
     P*                          (Optional)
     P* inNameOrder            - Name Order, employer name prints
     P*                          first or federal name prints first
     P*                          (value F) (Optional)
     P* inAddressOrder         - Determines the address print order
     P*                          (Optional)
     P* inPEOOutputControl     - PEO Output Control (Optional)
     P* inPrintDBALabel        - Return DBA instead of federal name
     P*                          (Optional)
     P* inPEOExtension         - Flexible field for PEO acronym to be
     P*                          used populated in place of L/C/F.
     P*                          (Optional)
     P*
     P*====================================================================
     DGetNameAddress   pr
01   D inCompany#                     3  0 const
02   D inGroup#                       3  0 const
03   D inEmployer#                    9  0 const
04   D inDivision                     5  0 const
04   D inYear                         3  0 const
05   D outEmployerName1...
     D                               50
06   D outEmployerName2...
     D                               50
07   D outEmployerName3...
     D                               50
08   D outEmployerName4...
     D                               50
09   D outEmployerName5...
     D                               50
10   D outEmployerName6...
     D                               50
11   D outEmployerName7...
     D                               50
12   D outAddressLocation...
     D                                3  0
13   D outNameLocation...
     D                                3  0
14   D outFederalNameLocation...
     D                                3  0
15   D outPEOName1                   57
16   D outPEOName2                   57
17   D outContactLocation...
     D                                3  0
18   D outAMName1                    60
19   D outAMName2                    60
20   D outAMName3                    60
21   D outAMName4                    60
22   D outAMName5                    60
23   D outAMName6                    60
24   D outAMName7                    60
25   D outAMName8                    60
26   D outAMName9                    60
27   D outPEOLocation                 3  0
28   D outAddressListCount...
     D                                3  0
29   D outNameListCount...
     D                                3  0
30   D outFedNameListCount...
     D                                3  0
31   D outPEONameListCount...
     D                                3  0
32   D outContactListCount...
     D                                3  0
33   D outScripturaName1...
     D                              120
33   D outScripturaName2...
33   D                              120
33   D outScripturaName3...
33   D                              120
33   D outScripturaName4...
33   D                              120
37   D outInsuredFein                 9  0
38   D inOrder                        1    options(*nopass)
39   D inNameOrder                    1    options(*nopass)
40   D inAddressOrder                 1    options(*nopass)
41   D inPEOGroupFlag...
     D                                1    options(*nopass)
42   D inPrintDBALabel...
     D                                1    options(*nopass)
43   D inPEOExtension...
     D                               10    options(*nopass)
     d*** End   Add *** 02/22/22 ******************************************
      *====================================================================
      * PrvRptManPrm returns the amount of manual premium previously reported
      *  inCo#  - accepts the current company   (required)
      *  inGrp  - accepts the current group     (required)
      *  inEmp# - accepts the current employer  (required)
      *  inEmp# - accepts the current division  (required)
      *  inFyr  - accepts the current pol year  (required)
      *  inDate - accepts the report date       (required)
      *====================================================================
     dPrvRptManPrm     pr            11  0
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
     d inDate                         7  0 const
      *** beg add *****  03/16/20  *******************************************
      *====================================================================
      * OvrRenewalCx allows certain processes if cancellation can be
      *  overridden
      *====================================================================
     dOvrRenewalCx     pr              n
     d inMode                         3
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inCxDate                       7  0
     d inTrDate                       7  0 Options(*NoPass)
     d inTrTime                       6  0 Options(*NoPass)
      *** end add *****  03/16/20  *******************************************
      *====================================================================
      * PrvRptManPrm returns the amount of manual premium previously reported
      *  inCo#  - accepts the current company   (required)
      *  inFnd  - accepts the current group     (required)
      *  inEmp# - accepts the current employer  (required)
      *  inDiv  - accepts the current division  (required)
      *  inFyr  - accepts the current pol year  (required)
      *====================================================================
     dPolicyJSONData   pr              a   Len(5000) Varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
      *** Beg Add *****  07/07/23  *******************************************
      *====================================================================
      * GetEmpBalToMin - Calculate and Return the Balance to Minimum Premium
      *  inCo#         - accepts the company # to search over
      *  inFnd         - accepts the group to search over
      *  inEmp#        - accepts the employer to search over
      *  inFyr         - accepts the emplr year to search over
      *  audFlg        - 'C' pulls premium for current status
      *                  'E' pulls estimated premium
      *                  'A' pulls audited premium
      *====================================================================
     dGetEmpBalToMin   pr             9  2
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d audFlg                         1    const
      *** End Add *****  07/07/23  *******************************************
      *** Begin Add *** 09/14/23 *******************************************


     P*====================================================================
     P* GetEmpEnv - Get the Environment of the Policy
     P*
     P*  GetEmpEnv  - returns Environment
     P*   co#       - passed in for validation  (required)
     P*   group     - passed in for validation  (required)
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*====================================================================
     DGetEmpEnv        PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
      *** End   Add *** 09/14/23 *******************************************
