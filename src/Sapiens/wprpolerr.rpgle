      *********************************************************************
      * REVISIONS:
      *
      *********************************************************************
      *
     d dsRateEdit      ds
     d  addMode                        n
     d  chgMode                        n
     d  s2BSCD                        3s 0
     d  xxSVST                        2
     d  saveSCBSCD                    3s 0
     d  saveBSLPGM                    1
     d  svRTCD                        3
     d  xxSTRP                        1
     d  xxSRAD                        8s 0
     d  xxOTAR                        1
     d  xxOnce                        1
     d  xxOFlg                        1
     d  xxUpdA                        1
     d  xxUpdB                        1
     d  xxUpdC                        1
     d  xxUpdD                        1
     d  flex                          1
     d  XXRQD1                        1
     d  XXRQD2                        1
     d  XXRQD3                        1
     d  XXRQD4                        1
     d  XXRQD5                        1
     d  XXRQD6                        1
     d  x5PFDT                        7s 0
     d  x5PTDT                        7s 0
     d  xxMailSt                      2
     d  xxPhysSt                      2
      *
     D*=====================================================================
     D* Policy Errors procedure prototypes
     D*=====================================================================
     D*
     P*====================================================================
     P* ValidDate - Verify if date passed in is valid and return.
     P*
     P*   date      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DValidDAte        PR             1
     D date                           8  0
     D messageID                    100
     D*
     P*====================================================================
     P* PolEfDtLess - Policy from date cannot be less than initial Eff Date
     P*
     P*   polDate   - passed in for validation                  (required)
     P*   empDate   - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolEfDtLess      PR             1
     D polDate                        8  0 const
     D empDate                        7  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* PolEfDtPayroll - Policy from date cannot be less than initial Eff Date
     P*
     P*   polDate   - passed in for validation                  (required)
     P*   elRRN     - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolEfDtPayroll   PR             1
     D polDate                        8  0 const
     D elRRN                          9  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* AscendDates - Policy from date cannot be less than initial Eff Date
     P*
     P*   fromDate  - passed in for validation                  (required)
     P*   toDate    - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DAscendDates      PR             1
     D fromDate                       8  0 const
     D toDate                         8  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* PolFundDates - Policy from date cannot be less than initial Eff Date
     P*
     P*   fromDate  - passed in for validation                  (required)
     P*   toDate    - passed in for validation                  (required)
     P*   elRRN     - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolFundDates     PR             1
     D fromDate                       8  0 const
     D toDate                         8  0 const
     D co#                            3  0 const
     D fnd                            3  0 const
     D fyr                            3  0 const
     D strp                           1    const
     D messageID                    100
     D*
     P*====================================================================
     P* ValidAgent  - Agent # is valid for the group
     P*
     P*   co#       - passed in for validation                  (required)
     P*   fnd       - passed in for validation                  (required)
     P*   agent     - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DValidAgent       PR             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agent                          5  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* ValidProducer - Producer is valid for the agent
     P*
     P*   co#       - passed in for validation                  (required)
     P*   fnd       - passed in for validation                  (required)
     P*   agent     - passed in for validation                  (required)
     P*   producer  - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DValidProducer    PR             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agent                          5  0 const
     D producer                       5  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* ValidAgtRate - Agent Rate is valid for the group
     P*
     P*   co#       - passed in for validation                  (required)
     P*   fnd       - passed in for validation                  (required)
     P*   rate      - passed in for validation                  (required)
     P*   effDate   - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DValidAgtRate     PR             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D rate                           3  0 const
     D effDate                        8  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* PolDtLessPolEf - Date must be less than or equal to Policy eff date
     P*
     P*   date      - passed in for validation                  (required)
     P*   polDate   - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolDtLessPolEf   PR             1
     D date                           8  0 const
     D polDate                        8  0 const
     D messageID                    100
     D*
     P*====================================================================
     P* ValidFlagY - Flag must have value of Y or blank.
     P*
     P*   flag      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DValidFlagY       PR             1
     D flag                           1    const
     D messageID                    100
     P*
     P*====================================================================
     P* ValidFlagYN - Flag must have value of Y or N.
     P*
     P*   flag      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DValidFlagYN      PR             1
     D flag                           1    const
     D messageID                    100
     P*
     P*====================================================================
     P* PolProdChg - Producer cannot change if commission exists
     P*
     P*   prod      - passed in for validation                  (required)
     P*   svProd    - passed in for validation                  (required)
     P*   emRRN     - passed in for validation                  (required)
     P*   fyr       - passed in for validation                  (required)
     P*   agt#      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolProdChg       PR             1
     D Prod                           5  0 const
     D svProd                         5  0 const
     D emRRN                          9  0 const
     D fyr                            3  0 const
     D agt#                           5  0 const
     D messageID                    100
     P*
     P*====================================================================
     P* PolAgtDate - Policy agent has to have agent assignment effective/cancel
     P*              date valid for the agent history effective date.
     P*
     P*   co#       - passed in for validation                  (required)
     P*   fnd       - passed in for validation                  (required)
     P*   agt#      - passed in for validation                  (required)
     P*   efdt      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolAgtDate       PR             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agt#                           5  0 const
     D efdt                           8  0 const
     D messageID                    100
     P*
     P*====================================================================
     P* PolProdDate - Policy producer has to have a cancel date
     P*               valid for the agent history effective date.
     P*
     P*   co#       - passed in for validation                  (required)
     P*   fnd       - passed in for validation                  (required)
     P*   agt#      - passed in for validation                  (required)
     P*   prod      - passed in for validation                  (required)
     P*   efdt      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolProdDate      PR             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agt#                           5  0 const
     D prod                           5  0 const
     D efdt                           8  0 const
     D messageID                    100
     P*
     P*====================================================================
     P* PolAgtCount - Policy agent cannot be inactivated unless there is
     P*               a replacement agent record.
     P*
     P*   elRRN     - passed in for validation                  (required)
     P*   stat      - passed in for validation                  (required)
     P*   efdt      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolAgtCount      PR             1
     D elRRN                          9  0 const
     D stat                           1    const
     D efdt                           8  0 const
     D messageID                    100
     P*
     p*=====================================================================
     p* ErrPolRating - Validate policy rating fields
     p*
     p*  Parm1   - Call                                         (required)
     p*  Parm2   - Mode                                         (required)
     p*  Parm3   - Quote                                        (required)
     p*  Parm4   - Pointer to format DS                         (required)
     p*  Parm5   - Company                                      (required)
     p*  Parm6   - Fund                                         (required)
     p*  Parm7   - Employer                                     (required)
     p*  Parm8   - Division                                     (required)
     p*  Parm9   - Year                                         (required)
     p*  Parm10  - Employer Detail RRN                          (required)
     p*  Parm11  - Program Data                                 (required)
     p*  Parm12  - Pointer to secondary format DS               (optional)
     p*
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     DErrPolRating     pr              n
     D inCall                         1    const
     D inMode                         1    const
     D inQuote                        1    const
     D inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inELrrn                        9  0 const
     D inData                       500
     D inPtrFormat2                    *   options(*nopass)
     P*
     p*=====================================================================
     p* ErrAddInfo - Validate additional info fields
     p*
     p*  Parm1   - Call                                         (required)
     p*  Parm2   - Mode                                         (required)
     p*  Parm3   - Quote                                        (required)
     p*  Parm4   - Pointer to format DS                         (required)
     p*  Parm5   - Company                                      (required)
     p*  Parm6   - Fund                                         (required)
     p*  Parm7   - Employer                                     (required)
     p*  Parm8   - Division                                     (required)
     p*  Parm9   - Year                                         (required)
     p*  Parm10  - Employer Detail RRN                          (required)
     p*  Parm11  - Program Data                                 (required)
     p*
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     DErrAddInfo       pr              n
     D inCall                         1    const
     D inMode                         1    const
     D inQuote                        1    const
     D inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inELrrn                        9  0 const
     D inData                       500
     P*
     p*=====================================================================
     p* ErrLargeDed - Validate large deductible info fields
     p*
     p*  Parm1   - Call                                         (required)
     p*  Parm2   - Mode                                         (required)
     p*  Parm3   - Quote                                        (required)
     p*  Parm4   - Pointer to format DS                         (required)
     p*  Parm5   - Company                                      (required)
     p*  Parm6   - Fund                                         (required)
     p*  Parm7   - Employer                                     (required)
     p*  Parm8   - Division                                     (required)
     p*  Parm9   - Year                                         (required)
     p*  Parm10  - Employer Detail RRN                          (required)
     p*  Parm11  - Program Data                                 (required)
     p*
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     DErrLargeDed      pr              n
     D inCall                         1    const
     D inMode                         1    const
     D inQuote                        1    const
     D inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inELrrn                        9  0 const
     D inData                       500
     P*
     p*=====================================================================
     p* ErrRetroRate - Validate retrospective rating info fields
     p*
     p*  Parm1   - Call                                         (required)
     p*  Parm2   - Mode                                         (required)
     p*  Parm3   - Quote                                        (required)
     p*  Parm4   - Pointer to format DS                         (required)
     p*  Parm5   - Company                                      (required)
     p*  Parm6   - Fund                                         (required)
     p*  Parm7   - Employer                                     (required)
     p*  Parm8   - Division                                     (required)
     p*  Parm9   - Year                                         (required)
     p*  Parm10  - Employer Detail RRN                          (required)
     p*  Parm11  - Program Data                                 (required)
     p*
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     DErrRetroRate     pr              n
     D inCall                         1    const
     D inMode                         1    const
     D inQuote                        1    const
     D inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inELrrn                        9  0 const
     D inData                       500
     P*
     p*=====================================================================
     p* ErrSchedRate - Validate scheduled rating info fields
     p*
     p*  Parm1   - Call                                         (required)
     p*  Parm2   - Mode                                         (required)
     p*  Parm3   - Quote                                        (required)
     p*  Parm4   - Pointer to format DS                         (required)
     p*  Parm5   - Company                                      (required)
     p*  Parm6   - Fund                                         (required)
     p*  Parm7   - Employer                                     (required)
     p*  Parm8   - Division                                     (required)
     p*  Parm9   - Year                                         (required)
     p*  Parm10  - Employer Detail RRN                          (required)
     p*  Parm11  - Program Data                                 (required)
     p*
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     DErrSchedRate     pr              n
     D inCall                         1    const
     D inMode                         1    const
     D inQuote                        1    const
     D inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inELrrn                        9  0 const
     D inData                       500
     P*
     P*====================================================================
     P* PolMaxLength - Policy year cannot be more than 365(366) days or max extended days.
     P*
     P*   fromDate  - passed in for validation                  (required)
     P*   toDate    - passed in for validation                  (required)
     P*   co#       - passed in for validation                  (required)
     P*   fund      - passed in for validation                  (required)
     P*   messageID - passed in for validation                  (required)
     P*
     P*====================================================================
     P*
     DPolMaxLength     PR             1
     D fromDate                       8  0 const
     D toDate                         8  0 const
     D co#                            3  0 const
     D fnd                            3  0 const
     D messageID                    100
