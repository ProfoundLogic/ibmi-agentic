     D*=====================================================================
     D* Claim code-validation procedure prototypes
     D*=====================================================================

    P*====================================================================
    P* ValidOccup   - Validate Occupation code WTOCL
    P*
    P* ValidOccup   - return *on if occupation is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   Occucde    - passed in for validation  (required)
    P*====================================================================
    P*
     DValidOccup       PR             1
     D co#                            3  0 const
     D Occucde                       10    const
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)
    P*
    P*====================================================================
    P* ValidOffice  - Validate Office  WMEXL
    P*
    P* ValidOffice  - return *on if department is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   Office#    - passed in for validation  (required)
    P*====================================================================
    P*
     DValidOffice      PR             1
     D co#                            3  0 const
     D fund                           3  0 const
     D Office#                        5    const

    P*====================================================================
    P* ValidDept    - Validate Department  WMDPL
    P*
    P* ValidDept    - return *on if department is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   emp#       - passed in for validation  (required)
    P*   div        - passed in for validation  (required)
    P*   Depcode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WMDPL
    P*   depname    - returns the name from WMDPL
    P*====================================================================
    P*
     DValidDept        PR             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Depcode                       10
     D cderrn                         9  0             options(*nopass)
     D deptnam                       40                options(*nopass)


    P*====================================================================
    P* ValidLoc     - Validate Location code   WMALL
    P*
    P* ValidLoc     - return *on if location is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   emp#       - passed in for validation  (required)
    P*   div        - passed in for validation  (required)
    P*   loccode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WMALP
    P*   locname    - returns the name from WMALP
    P*====================================================================
    P*
     DValidLoc         PR             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Loccode                       10
     D cderrn                         9  0             options(*nopass)
     D locname                       40                options(*nopass)

    P*====================================================================
    P* ValidClas    - Validate Class code                  WMEPP
    P*
    P* ValidClas    - return *on if class code is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   fyr        - passed in for validation  (required)
    P*   emp#       - passed in for validation  (required)
    P*   div        - passed in for validation  (required)
    P*   eppcode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WMALP
    P*====================================================================
    P*
     DValidClas        PR             1
     D co#                            3  0 const
     D fund                           3  0 const
     D fyr                            3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Clscode                        5  0 const
     D Spec                           1    const
     D efdate                         7P 0 const
     D cderrn                         9  0 options(*nopass)


    P*====================================================================
    P* ValidNatr - Validate Nature code
    P*
    P* ValidNatr - return *on if Nature code is valid, else *off
    P*   co#     - passed in for validation (required)
    P*   Natcde  - passed in for validation (required)
    P*   cderrn  - returns the RRN of WTNAP
    P*   cdedsc  - returns the description from WTNAP
    P*====================================================================
     DValidNatr        PR             1
     D co#                            3  0 const
     D Natcde                         2
     D cderrn                         9  0 options(*nopass)
     D cdedsc                        30    options(*nopass)

    P*====================================================================
    P* ValidPart - Validate Part code
    P*
    P* ValidPart - return *on if Part code is valid, else *off
    P*   co#     - passed in for validation (required)
    P*   Partcde - passed in for validation (required)
    P*   cderrn  - returns the RRN of WTPAP
    P*   cdedsc  - returns the description from WTPAP
    P*====================================================================
     DValidPart        PR             1
     D co#                            3  0 const
     D Partcde                        2
     D cderrn                         9  0 options(*nopass)
     D cdedsc                        30    options(*nopass)

    P*====================================================================
    P* ValidCause - Validate Cause code
    P*
    P* ValidCause - return *on if Cause code is valid, else *off
    P*   co#      - passed in for validation (required)
    P*   Causcde  - passed in for validation (required)
    P*   cderrn   - returns the RRN of WTCAP
    P*   cdedsc   - returns the description from WTCAP
    P*====================================================================
     DValidCause       PR             1
     D co#                            3  0 const
     D Causecde                       2
     D cderrn                         9  0 options(*nopass)
     D cdedsc                        30    options(*nopass)

    P*====================================================================
    P* ValidResType - Validate reserve type code
    P*
    P* ValidResType - return *on if cause is valid
    P*   co#        - passed in for validation  (required)
    P*   Restycde   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTRTP
    P*   cdedsc     - returns the description from WTRTP
    P*====================================================================
    P*
     DValidResType     PR             1
     D co#                            3  0 const
     D Restycde                       2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidPmtCode - Validate payment code
    P*
    P* ValidPmtCode - return *on if payment code is valid
    P*   co#        - passed in for validation  (required)
    P*   Pmtcode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTPCP
    P*   cdedsc     - returns the description from WTPCP
    P*====================================================================
    P*
     DValidPmtCode     PR             1
     D co#                            3  0 const
     D Pmtcode                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidBR2Code - Validate BR2 code (payment value 1)
    P*
    P* ValidBR2Code - return *on if BR2 code is valid
    P*   co#        - passed in for validation  (required)
    P*   BR2code    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTB2P
    P*   cdedsc     - returns the description from WTB2P
    P*====================================================================
    P*
     DValidBR2Code     PR             1
     D co#                            3  0 const
     D BR2code                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidBR7Code - Validate BR7 code (payment value 2)
    P*
    P* ValidBR7Code - return *on if BR7 code is valid
    P*   co#        - passed in for validation  (required)
    P*   BR7code    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTB7P
    P*   cdedsc     - returns the description from WTB7P
    P*====================================================================
    P*
     DValidBR7Code     PR             1
     D co#                            3  0 const
     D BR7code                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidCounty  - Validate County Code
    P*
    P* ValidCounty  - return *on if County is valid
    P*   co#        - passed in for validation  (required)
    P*   Cntycde    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTCNP
    P*   cdedsc     - returns the description from WTCNP
    P*====================================================================
    P*
     DValidCounty      PR             1
     D co#                            3  0 const
     D Cntycde                       10
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidTRCode  - Validate Transaction Code
    P*
    P* ValidTRCode  - return *on if TR code is valid
    P*   co#        - passed in for validation  (required)
    P*   Cntycde    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTPCP
    P*   cdedsc     - returns the description from WTPCP
    P*====================================================================
    P*
     DValidTRCode      PR             1
     D co#                            3  0 const
     D TRCode                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)


    P*====================================================================
    P* ValidSPCode  - Validate Specialty Code
    P*
    P* ValidSPCode  - return *on if specialty code is valid
    P*   co#        - passed in for validation  (required)
    P*   Speccde    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTSPP
    P*   cdedsc     - returns the description from WTSPP
    P*====================================================================
    P*
     DValidSPCode      PR             1
     D co#                            3  0 const
     D Speccde                        3
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)


    P*====================================================================
    P* ValidState  - Validate state code      Table file STABL
    P*
    P* ValidState - return *on if state code is valid
    P*   Statecd  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of STSTP
    P*   cdename  - returns the state name
    P*====================================================================
    P*
     DValidState       PR             1
     D Statecd                        2
     D cderrn                         9  0             options(*nopass)
     D cdename                       40                options(*nopass)

    P*====================================================================
    P* ValidCntry  - Validate country code    Using TBID = 'CTRY'
    P*
    P* ValidCntry - return *on if country code is valid
    P*   co#      - passed in for validation  (required)
    P*   Cntrycde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidCntry       PR             1
     D co#                            3  0 const
     D Cntrycde                       3
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidNCCI   - Validate NCCI code    Using TBID = 'xxNC'  (xx=ST)
    P*                                      then TBID = '  NC'  (general)
    P*
    P* ValidNCCI  - return *on if NCCI code is valid
    P*   co#      - passed in for validation  (required)
    P*   NCCIcde  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidNCCI        PR             1
     D co#                            3  0 const
     D NCCIcde                        2
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidLossC  - Validate Loss Coverage Using TBID = 'xxLC'  (xx=ST)
    P*                                       then TBID = '  LC'  (general)
    P*
    P* ValidLossC - return *on if Loss coverage is valid
    P*   co#      - passed in for validation  (required)
    P*   Losscde  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidLossC       PR             1
     D co#                            3  0 const
     D Losscde                        2
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidCovT   - Validate Coverage type Using TBID = 'xxCT'  (xx=ST)
    P*                                       then TBID = '  CT'  (general)
    P*
    P* ValidCovT  - return *on if Coverage type is valid
    P*   co#      - passed in for validation  (required)
    P*   Covtyp   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidCovT        PR             1
     D co#                            3  0 const
     D covtyp                         2
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidMethC - Validate method of contact  Using TBID = 'MCON'
    P*
    P* ValidMethC - return *on if method of contact is valid
    P*   co#      - passed in for validation  (required)
    P*   Methccde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidMethC       PR             1
     D co#                            3  0 const
     D Methccde                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidLostT - Validate lost time flag   Using TBID = 'LTFL'
    P*
    P* ValidLostT - return *on if lost time flag is valid
    P*   co#      - passed in for validation  (required)
    P*   Losttcde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidLostT       PR             1
     D co#                            3  0 const
     D Losttcde                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidLossR - Validate loss recovery    Using TBID = 'LSRV'
    P*
    P* ValidLossR - return *on if loss recovery is valid
    P*   co#      - passed in for validation  (required)
    P*   Lossrcde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidLossR       PR             1
     D co#                            3  0 const
     D Lossrcde                       2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidJurSt - Validate jurisdiction ST  Using TBID = 'JRST'
    P*
    P* ValidJurSt - return *on if jurisdiction ST is valid
    P*   co#      - passed in for validation  (required)
    P*   jurst    - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidJurSt       PR             1
     D co#                            3  0 const
     D jurst                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)
    P*====================================================================
    P* ValidShift - Validate shift       Using TBID = 'SHFT'
    P*
    P* ValidShift - return *on if shift is valid
    P*   co#      - passed in for validation  (required)
    P*   Shift    - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidShift       PR             1
     D co#                            3  0 const
     D Shift                          1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidEmpSt - Validate employment status  Using TBID = 'EMST'
    P*
    P* ValidEmpSt - return *on if employment status is valid
    P*   co#      - passed in for validation  (required)
    P*   Empsts   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidEmpSt       PR             1
     D co#                            3  0 const
     D Empsts                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidActSt - Validate activity status  Using TBID = 'ACST'
    P*
    P* ValidActSt - return *on if activity status is valid
    P*   co#      - passed in for validation  (required)
    P*   Actsts   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
    P*
     DValidActSt       PR             1
     D co#                            3  0 const
     D Actsts                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidMCOtyp - Validate MCO Type  Using TBID = 'MCO '
    P*
    P* ValidMCOtyp - return *on if MCO type is valid
    P*   co#       - passed in for validation  (required)
    P*   Mctype    - passed in for validation  (required)
    P*   cderrn    - returns the RRN of WTGTL
    P*   cdedsc    - returns the description from WTGTL
    P*====================================================================
    P*
     DValidMCOtyp      PR             1
     D co#                            3  0 const
     D Mctype                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidInitTr - Validate Initial treatment  Using TBID = 'INIT'
    P*
    P* ValidInitTr - return *on if Init Treatment code is valid
    P*   co#       - passed in for validation  (required)
    P*   Initrt    - passed in for validation  (required)
    P*   cderrn    - returns the RRN of WTGTL
    P*   cdedsc    - returns the description from WTGTL
    P*====================================================================
    P*
     DValidInitTr      PR             1
     D co#                            3  0 const
     D Initrt                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidAgrComp - Validate Agree to compensate  Using TBID = 'AGCP'
    P*
    P* ValidAgrComp - return *on if Agree  to comp code is valid
    P*   co#        - passed in for validation  (required)
    P*   Agreecmp   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidAgrComp     PR             1
     D co#                            3  0 const
     D Agreecmp                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidMethPIW - Validate Meth/determn pre-Injury wage  TBID = 'WMTH'
    P*
    P* ValidMethPIW - return *on if Meth/Det PIW is valid
    P*   co#        - passed in for validation  (required)
    P*   Mdetpiw    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidMethPIW     PR             1
     D co#                            3  0 const
     D Mdetpiw                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidFWageP  - Validate FROI Wage period              TBID = 'FRWP'
    P*
    P* ValidFWageP  - return *on if FROI Wage Period is valid
    P*   co#        - passed in for validation  (required)
    P*   Fwageper   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidFWageP      PR             1
     D co#                            3  0 const
     D Fwageper                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidRTWQual - Validate RTW qualifier              TBID = 'RTWQ'
    P*
    P* ValidRTWQual - return *on if Return to work qualifier is valid
    P*   co#        - passed in for validation  (required)
    P*   Retwqual   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidRTWQual     PR             1
     D co#                            3  0 const
     D Retwqual                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidFraudI  - Validate Fraudulent indicator       TBID = 'FRAU'
    P*                             (WMLLP)
    P* ValidFraudI  - return *on if Fraud indicator is valid
    P*   co#        - passed in for validation  (required)
    P*   Fraudin    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidFraudI      PR             1
     D co#                            3  0 const
     D Fraudin                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P*ValidMethSettle Validate Meth/settle  Using TBID = 'xxMS'  (xx=ST)
    P*                  (WMLLP)              then TBID = '  MS'  (general)
    P*
    P* ValidMethSettle - return *on if Method of settlement is valid
    P*   co#           - passed in for validation  (required)
    P*   Methset       - passed in for validation  (required)
    P*   cderrn        - returns the RRN of WTGTL
    P*   cdedsc        - returns the description from WTGTL
    P*====================================================================
    P*
     DValidMethSettle  PR             1
     D co#                            3  0 const
     D Methset                        3
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)
    P*====================================================================
    P* ValidGender  - Validate Gender code                TBID = 'GNDR'
    P*                             (WMLLP)
    P* ValidGender  - return *on if Gender code is valid
    P*   co#        - passed in for validation  (required)
    P*   GendCode   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidGender      PR             1
     D co#                            3  0 const
     D GendCode                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidMariSt  - Validate Marital status          TBID = 'MSTS'
    P*                             (WMCDP)
    P* ValidGMariSt - return *on if marital status is valid
    P*   co#        - passed in for validation  (required)
    P*   Marists    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidMariSt      PR             1
     D co#                            3  0 const
     D Marists                        1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)
    
    P*====================================================================
    P* ValidSICCde  - Validate SIC code                TBID = 'SIC '
    P*
    P* ValidSICCde  - return *on if SIC code is valid
    P*   co#        - passed in for validation  (required)
    P*   Siccode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
    P*
     DValidSICCde      PR             1
     D co#                            3  0 const
     D Siccde                         6
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

    P*====================================================================
    P* ValidPayeeTyp - Validate Payee type                TBID = 'PTYP'
    P*
    P* ValidPayeeTyp - return *on if Payee type is valid
    P*   co#         - passed in for validation  (required)
    P*   Pyeetyp     - passed in for validation  (required)
    P*   cderrn      - returns the RRN of WTGTL
    P*   cdedsc      - returns the description from WTGTL
    P*====================================================================
    P*
     DValidPayeeTyp    PR             1
     D co#                            3  0 const
     D Pyeetype                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)


