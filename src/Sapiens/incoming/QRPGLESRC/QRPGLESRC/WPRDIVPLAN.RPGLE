    *===============================================================*
      * Calculates Dividend
      *  - returns the calculated amount of dividend premium
    *===============================================================*
     dCalcDividend     pr             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
    *===============================================================*
      * Retrieves Dividend Premium
      *  - returns the amount of dividend premium stored in WDELP
    *===============================================================*
     dGetDividendPrm   pr             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
    *===============================================================*
      * Prints Dividend Information
      *  - returns the amount of dividend premium  stored in WDELP
      *  - writes DARTS Remark if policy doesn't meet criteria required
      *    for dividend processing
    *===============================================================*
     dPrintDividend    pr             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inEnv                          1
    *===============================================================*
      * Updates Dividend Information
      *  - updates dividend premium in Employer Detail
      *  - writes A/R transactions for dividend premium
      *  - returns the calculated amount of dividend premium
    *===============================================================*
     dUpdateDividend   pr             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
    *===============================================================*
      * Validates Client is Eligible for Dividend
      *  - returns *on if client is eligible for dividend processing
    *===============================================================*
     dValidDivClient   pr              n
    *===============================================================*
      * Validates Dividend Plan
      *  - returns *on if division is eligible for dividend processing
    *===============================================================*
     dValidDivPlan     pr              n
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inPlanID                      10
    *===============================================================*
