     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title WPIDIVPLAN - Interface with Dividend API (WA119R)
     h option(*nodebugio:*srcstmt)
     h nomain
    * Created   : 06/12/13                                          *
    * Programmer: C:136                                             *
    * Project   : 147WAI                                            *
    *===============================================================*
    * Revisions:                                                    *
      *
      *  04/30/19 C:062 Project: 152WAB
      *                 Apply Optacomp Mods.
      *                                                               *
    *===============================================================*
     d/copy wprDivPlan
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3
     d DivPlanAPI      pr                  extpgm('WA119R')
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inEmp#                        9  0
     d  inDiv                         5  0
     d  inFyr                         3  0
     d  inPlanID                     10
     d  inMode                        1    const
     d  inEnv                         1    const
     d  outResult                      n
     d  outDivBill                    9  2
    *===============================================================*
      * Calculates Dividend
      *  - returns the calculated amount of dividend premium
    *===============================================================*
     pCalcDividend     b                   export
     dCalcDividend     pi             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inPlanID        s             10
     d outDivBilled    s              9  2
     d outResult       s               n
      /free
       DivPlanAPI (inCo# : inFnd : inEmp# : inDiv : inFyr :
        inPlanID : '2' : ' ' : outResult : outDivBilled);
       Return outDivBilled;
      /end-free
     pCalcDividend     e
    *===============================================================*
      * Retrieves Dividend Premium
      *  - returns the amount of dividend premium stored in WDELP
    *===============================================================*
     pGetDividendPrm   b                   export
     dGetDividendPrm   pi             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inPlanID        s             10
     d outDivBilled    s              9  2
     d outResult       s               n
      /free
       DivPlanAPI (inCo# : inFnd : inEmp# : inDiv : inFyr :
        inPlanID : '1' : ' ' : outResult : outDivBilled);
       Return outDivBilled;
      /end-free
     pGetDividendPrm   e
    *===============================================================*
      * Prints Dividend Information
      *  - returns the amount of dividend premium  stored in WDELP
      *  - writes DARTS Remark if policy doesn't meet criteria required
      *    for dividend processing
    *===============================================================*
     pPrintDividend    b                   export
     dPrintDividend    pi             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inQte                          1
     d inPlanID        s             10
     d outDivBilled    s              9  2
     d outResult       s               n
      /free
       DivPlanAPI (inCo# : inFnd : inEmp# : inDiv : inFyr :
        inPlanID : '4' : inQte : outResult : outDivBilled);
       Return outDivBilled;
      /end-free
     pPrintDividend    e
    *===============================================================*
      * Updates Dividend Information
      *  - updates dividend premium in Employer Detail
      *  - writes A/R transactions for dividend premium
      *  - returns the calculated amount of dividend premium
    *===============================================================*
     pUpdateDividend   b                   export
     dUpdateDividend   pi             9  2
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inPlanID        s             10
     d outDivBilled    s              9  2
     d outResult       s               n
      /free
       DivPlanAPI (inCo# : inFnd : inEmp# : inDiv : inFyr :
        inPlanID : '3' : ' ' : outResult : outDivBilled);
       Return outDivBilled;
      /end-free
     pUpdateDividend   e
    *===============================================================*
      * Validates Client is Eligible for Dividend
      *  - returns *on if client is eligible for dividend processing
    *===============================================================*
     pValidDivClient   b                   export
     dValidDivClient   pi              n
     d clientID        s              3
      /free
       GetClientID(clientID);
       Select;
       //*** Begin Chg *** 04/30/19 ***********************************
       //*** When clientID  = '114';
       When clientID  = '114' Or clientID = '152';
       //*** End   Chg *** 04/30/19 ***********************************
          Return *on;
       Other;
          Return *off;
       Endsl;
      /end-free
     pValidDivClient   e
    *===============================================================*
      * Validates Dividend Plan
      *  - returns *on if division is eligible for dividend processing
    *===============================================================*
     pValidDivPlan     b                   export
     dValidDivPlan     pi              n
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inPlanID                      10
     d outDivBilled    s              9  2
     d outResult       s               n
      /free
       DivPlanAPI (inCo# : inFnd : inEmp# : inDiv : inFyr :
        inPlanID : '1' : ' ' : outResult : outDivBilled);
       Return outResult;
      /end-free
     pValidDivPlan     e
    *===============================================================*
