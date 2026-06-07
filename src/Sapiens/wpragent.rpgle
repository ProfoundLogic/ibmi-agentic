     D*=====================================================================
     D* Agent processing procedure prototypes
     D*=====================================================================
     D*
     P*====================================================================
     P* AgtSrchAA - Search agents
     P*
     P*  AgtSrchAA - Returns RRN of WMAAP
     P*   inkc     - returns *on if F3 was pressed from search  (required)
     P*   inkl     - returns *on if F12 was pressed from search (required)
     P*   error    - returns a '1' if group passed in is invalid
     P*   group    - accepts the group # to search over
     P*   co#      - accepts the company # to search over
     P*   view     - sets the view
     P*   quote    - indicates if called from quote or not
     P*   mode     - used to indicate if called from Agent History
     P*   name     - subsets to the name that matches
     P*====================================================================
     P*
     DAgtSrchAA        PR             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     D*** Begin Add *** 02/11/15 ************************************************
     D view                           1  0 options(*nopass) const
     D quote                          1    options(*nopass) const
     D mode                           1    options(*nopass) const
     D name                          32    options(*nopass) const
     D*** End   Add *** 02/11/15 ************************************************
     P*
     P*====================================================================
     P* AgtSrchAG - Search agents
     P*
     P*  AgtSrchAG - Returns RRN of WMAGP
     P*   inkc     - returns *on if F3 was pressed from search  (required)
     P*   inkl     - returns *on if F12 was pressed from search (required)
     P*   error    - returns a '1' if group passed in is invalid
     P*   co#      - accepts the company # to search over
     P*   view     - sets the view
     P*   quote    - indicates if called from quote or not
     P*   mode     - used to indicate if called from Agent History
     P*   name     - subsets to the name that matches
     P*====================================================================
     P*
     DAgtSrchAG        PR             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D co#                            3  0 options(*nopass) const
     D*** Begin Add *** 02/11/15 ************************************************
     D view                           1  0 options(*nopass) const
     D quote                          1    options(*nopass) const
     D mode                           1    options(*nopass) const
     D name                          32    options(*nopass) const
     D*** End   Add *** 02/11/15 ************************************************
     P*
     P*====================================================================
     P* AgtHisSrch - Search employer agent history records
     P*
     P*  AgtHisSrch - returns RRN of WMAHP
     P*   inkc     - returns *on if F3 was pressed from search   (required)
     P*   inkl     - returns *on if F12 was pressed from search  (required)
     P*   error    - returns a '1' if group passed in is invalid (required)
     P*              returns a '2' if employer passed in is invalid
     P*              returns a '3' if employer year passed in is invalid
     P*   quote    - accepts the environment to search over      (required)
     P*   emp#     - accepts the employer # to search over       (required)
     P*   div      - accepts the divsion to search over          (required)
     P*   yr       - accepts the employer year to search over
     P*   group    - accepts the group # to search over
     P*   co#    - accepts the company # to search over
     P*====================================================================
     P*
     DAgtHisSrch       PR             9  0
     D inkc                           1
     D inkl                           1
     D error                          1
     D quote                          1                     const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D yr                             3  0 options(*nopass) const
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     P*
     P***  Begin Add  ***  11/05/04  **************************************
     P*====================================================================
     P* GetAgent# - Get agent #
     P*
     P*  GetAgent#  - returns agent assignment #
     P*   co#       - accepts the company #  (required)
     P*   fein      - accepts the agent fein (required)
     P*   mod       - accepts the agent mod  (required)
     P*   group#    - accepts the group #
     P*====================================================================
     P*
     DGetAgent#        PR             9  0
     D co#                            3  0 const
     D fein                           9  0 const
     D mod                            3  0 const
     D group#                         3  0 const options(*nopass)
     P***  End   Add  ***  11/05/04  **************************************
     P*
     P*====================================================================
     P* GetAgtFein - Get agent fed id # (mod)
     P*
     P*  GetAgtFein - returns agent fed id #
     P*   co#       - accepts the company # (required)
     P*   group#    - accepts the group #   (required)
     P*   agent#    - accepts the agent #   (required)
     P*   mod       - returns the modifier of the agent's fein
     P*   aa_rrn    - returns the RRN of WMAAP
     P*====================================================================
     P*
     DGetAgtFein       PR             9  0
     D co#                            3  0 const
     D group#                         3  0 const
     D agent#                         5  0 const
     D mod                            3  0       options(*nopass)
     D aa_rrn                         9  0       options(*nopass)
     P*
     P*====================================================================
     P* GetAgtName - Get agent name
     P*
     P*  GetAgtName - returns agent name
     P*   agent#    - passed in for validation  (required)
     P*   ag_rrn    - returns the RRN of WMAGP
     P*   aa_rrn    - returns the RRN of WMAAP
     P*   group#    - accepts the group # to search over
     P*   co#       - accepts the company # to search over
     P*====================================================================
     P*
     DGetAgtName       PR            40
     D agent#                         5  0 const
     D ag_rrn                         9  0              options(*nopass)
     D aa_rrn                         9  0              options(*nopass)
     D group#                         3  0 const        options(*nopass)
     D co#                            3  0 const        options(*nopass)
     P*
     P*====================================================================
     P* GetAgtRRN - Get agent RRN
     P*
     P*  GetAgtRRN - returns the RRN of WMAAP
     P*   group    - passed in for validation  (required)
     P*   agent#   - passed in for validation  (required)
     P*   co#      - passed in for validation
     P*====================================================================
     P*
     DGetAgtRRN        PR             9  0
     D group                          3  0                  const
     D agent#                         5  0                  const
     D co#                            3  0 options(*nopass) const
     P*
     P*====================================================================
     P* GetEmpAgt - Get employers agent
     P*
     P*  GetEmpAgt  - returns the employers agent #
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the division to search over  (required)
     P*   yr        - accepts the year to search over
     P*   efdt      - accepts the eff date to search over
     P*   agtName   - returns the agent name
     P*   AH_rrn    - returns the RRN of WMAHP
     P*   AA_rrn    - returns the RRN of WMAAP
     P*   AG_rrn    - returns the RRN of WMAGP
     P*   env       - accepts the environment Quote/live
     P*====================================================================
     P*
     DGetEmpAgt        PR             5  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D yr                             3  0 const options(*nopass)
     D efdt                           7  0 const options(*nopass)
     D agtName                       40          options(*nopass)
     D AH_rrn                         9  0       options(*nopass)
     D AA_rrn                         9  0       options(*nopass)
     D AG_rrn                         9  0       options(*nopass)
     D env                            1          options(*nopass)
     P*
     P*====================================================================
     P* GetEmpAgtSF - Get employers agent (stored function)
     P*
     P*  GetEmpAgtSF- returns the employers agent #
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the division to search over  (required)
     P*   env       - accepts the environment Quote/live   (required)
     P*====================================================================
     DGetEmpAgtSF      PR             5p 0
     D co#                            3p 0 const
     D group                          3p 0 const
     D emp#                           9p 0 const
     D div                            5p 0 const
     D env                            1    const varying
     P*
     P*====================================================================
     P* GetEmpPlan - Get employers commission plan
     P*
     P*  GetEmpPlan - returns the employers commission plan
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the division to search over  (required)
     P*   yr        - accepts the year to search over
     P*   efdt      - accepts the eff date to search over
     P*====================================================================
     P*
     DGetEmpPlan       PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D yr                             3  0 const
     D efdt                           7  0 const
     P*
     P*====================================================================
     P* GetMstrRRN - Get agent master RRN
     P*
     P*  GetMstrRRN - return agent master RRN (WMAGP)
     P*   fein      - passed in for validation  (required)
     P*   mod       - passed in for validation  (required)
     P*   co#       - passed in for validation
     P*====================================================================
     P*
     DGetMstrRRN       PR             9  0
     D fein                           9  0 const
     D mod                            3  0 const
     D co#                            3  0 const options(*nopass)
      ***  Begin Add  ***  10/10/03  **************************************
     P*
     P*====================================================================
     P* ValidAgt# - Validate Agent #
     P*
     P*  ValidAgt# - returns *on if the agent is valid
     P*   agent#   - passed in for validation  (required)
     P*   group    - passed in for validation
     P*   co#      - passed in for validation
     P*====================================================================
     P*
     DValidAgt#        PR                  like(*in60)
     D agent#                         5  0                  const
     D group                          3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
     P*
     P*====================================================================
     P* ValidFein - Validate Agent Fein/Mod
     P*
     P*  ValidFein - returns *on if the agent is valid
     P*   fein     - passed in for validation  (required)
     P*   mod      - passed in for validation
     P*   co#      - passed in for validation
     P*====================================================================
     P*
     DValidFein        PR                  like(*in60)
     D fein                           9  0                  const
     D mod                            3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const
      ***  End   Add  ***  10/10/03  **************************************
     P*
      ***  Beg   Add  ***  03/06/12  **************************************
     P*====================================================================
     P* GetAgtInfo - Get the agent information AgtPhn#, AgtEmail, AgtName
     P*** Begin Add ***  07/30/13 - Proj: 147WBV  Pgmr: 287
     P* Added agent address AgtAd1, AgtCty, AgtSt, AgtZip
     P*** End Add ***    07/30/13
     P*
     P*   co#      - passed co#                (required)
     P*   fnd      - passed fnd                (required)
     P*   emp#     - passed emp#               (required)
     P*   div      - passed div#               (required)
     P*  AgtName   - returns Agent Name
     P*  AgtEmail  - returns Agent Email
     P*  AgtPhn#   - returns Agent Phone no#
     P*** Begin Add ***  07/30/13
     P*  AgtAd1    - returns Agent Address 1
     P*  AgtCty    - returns Agent City
     P*  AgtSt     - returns Agent State
     P*  AgtZip    - returns Agent Zip Code
     P*** End Add ***    07/30/13
     P*====================================================================
     p*
     DGetAgtInfo       PR
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D AgtName                       40
     D AgtEmail                      60
     D AgtPhn#                       15
      ***  End   Add  ***  03/06/12  **************************************
     D*** Begin Add ***  07/30/13
     D AgtAddr                       40    options(*nopass)
     D AgtCity                       25    options(*nopass)
     D AgtState                       2    options(*nopass)
     D AgtZipCd                       9  0 options(*nopass)
     D*** End Add ***    07/30/13
      ***  Beg   Add  ***  03/07/12  **************************************
     P*====================================================================
     P* GetAgtInfo1 - Get the agent information based on RRN
     P*
     P*   RRN      - passed rrn                (required)
     P*  AgtCo#    - returns Company Number
     P*  AgtFnd    - returns Agent Group Number
     P*  AgtAgt#   - returns Agent# assigned by Fumd.
     P*  AgtAgrc   - returns Agent# of Record.
     P*  AgtPrrc   - returns Producer of Record.
     P*  AgtProd   - returns agent Producer.
     P*
     P*====================================================================
     p*
     DGetAgtInfo1      PR
     D AHrrn1                         9  0                  const
     D xxAgCo#                        3  0
     D xxAgFnd                        3  0
     D xxAgAgt#                       5  0
     D xxAgAgrc                       5  0
     D xxAgPrrc                       5  0
     D xxAgProd                       5  0
      ***  End   Add  ***  03/07/12  **************************************

      ***  Beg   Add  ***  10/12/12  **************************************
     P*====================================================================
     P* GetHAgtRRN  - Get the agent RRN based on information
     P*   agent#   - accepts the agent# # to search over         (required)
     P*   co#      - accepts the company # to search over        (required)
     P*   group    - accepts the group # to search over          (required)
     P*   emp#     - accepts the employer # to search over       (required)
     P*   div      - accepts the divsion to search over          (required)
     P*   yr       - accepts the employer year to search over    (required)
     P*
     P*====================================================================
     p*
     DGetHAgtRRN       PR             9  0
     D agent#                         5  0 const
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D yr                             3  0 const
      ***  End   Add  ***  10/12/12  **************************************
      //***  Begin Add  ***  10/14/14  *************************************************
     dGetPlanTypeDesc  pr            30
     d inPlan                         1
      //***  End   Add  ***  10/14/14  *************************************************
      * beg add ***** 04/04/16 *****************************
      *=====================================================
      * GetActAgtAsg - returns *on if active agt assignment
      *                found for FEIN/MOD
      *  co#      - passed in for validation  (required)
      *  fein     - passed in for validation  (required)
      *  mod      - passed in for validation  (required)
      *=====================================================
     dGetActAgtAsg     pr              n
     d inCo#                          3  0 const
     d inFein                         9  0 const
     d inMod                          3  0 const
      * end add ***** 04/04/16 *********************************************************
