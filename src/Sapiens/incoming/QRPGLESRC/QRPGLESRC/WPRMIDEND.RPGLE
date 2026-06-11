      **********************************************************************
      * REVISIONS:                                                         *
     *                                                                    *
      *  09/20/18 C:267 Project: WC2584                                    *
      *                 Add procedure isPenRec to check existance of       *
      *                 record in WMENP. Used along side State Specific    *
      *                 midterm endorsement record.                        *
     *                                                                    *
      **********************************************************************
     p*=====================================================================
     p* GetProcPtr  - Get relevant procedure pointer
     p*  Parm1   - Name of the procedure
     p*  Returns - pointer to the procedure
     P*=====================================================================
     DGetProcPtr       pr              *   procptr
     D inProcName                    10
      *
      *** Beg add ***** 09/20/18 ****************
     p*=====================================================================
     p* isPenRec - Chk WMENP record existance
     p*  Parms   - CO#, FND, EMP#, DIV, FYR, JOBN, END#
     p*  Returns - found indicator
     P*=====================================================================
     DisPenRec         pr              n
     D inCO#                          3  0
     D inFND                          3  0
     D inEMP#                         9  0
     D inDIV                          5  0
     D inFYR                          3  0
     D inJOBN                         6s 0
     D inEND#                         5  0
      *
      *** End add ***** 09/20/18 ****************
     p*=====================================================================
     p* GetPenRec  - Get WMENP record
     p*  Parm1   - Endorsement number
     p*  Returns - not found indicator
     P*=====================================================================
     DGetPenRec        pr              n
     D inEND#                         5  0
      *
     p*=====================================================================
     p* GetPolSumData - Get policy summary data for various endorsements
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Returns - *On for success
     P*=====================================================================
     DGetPolSumData    pr
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inEEDdt                        8  0
