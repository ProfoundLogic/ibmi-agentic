      *********************************************************************
      * REVISIONS:
      *                                                               *
      * 12/12/17 C:33  Project: 150WAQ                                *
      *               -Add collateral                                 *
      *
      *  04/11/15 C:295 Support: 33960
      *                 Pass FYR mode when updating the details
      *                 on Location Master through UpdateBillDiv proc.
      *
      *  03/03/15 C:302 PROJECT: 114WRG
      *                 Policy Concentration Details
      *
      *  10/13/14 C:291 RED 3161
      *                 Change the date definition on the grid from
      *                 Number to date data type.
      *
      *  08/05/13 C:283 Support: WA133 82980
      *                 Pass QTE mode when updating the details
      *                 on Location Master through UpdateBillDiv proc.
      *
      *  01/22/13  C:277  PROJECT: 133WNZ
      *                   Add new parameter inNBquote
      *
      *********************************************************************
     d dsLocYr         ds
     d  scLrrn                        9s 0
     d  scLyear                       3s 0
     d  scL#emp                       7s 0
     d  scLPayroll                   15s 2
     d  scLTypBldg                   40
       //*** Beg Add *** 03/03/15
     d  scLBldgYr                     4s 0
     d  scL#Storie                    3s 0
     d  scLPeak#Em                    7s 0
       //*** End Add *** 03/03/15

     d dsOwnOff        ds
     d  scOOrrn                       9s 0
     d  scOLname                     30
     d  scOFname                     15
     d  scOpercent                    5s 2
     d  scOTitle                     30
     d  scOcov                       10
       //*** Beg Chg *** 10/13/14 *****************************
       //d  scOEffdt                      8s 0
       //d  scOExpDt                      8s 0
     d  scOEffdt                       D
     d  scOExpDt                       D
       //*** End Chg *** 10/13/14 *****************************
     d  scOClass                      5s 0
     d  scODiv                        5s 0
     d  scOEnt#                      10
     d  scOSelect                      n
     d  scOClDsc                     40

     d dsDept          ds
     d  scsDPrrn                      9s 0
     d  scsDeptNm                    40
     d  scsDept                      10
     d  scsStatus                    10
     d  scsDDiv                       5s 0

     d ptrBillDiv      s               *   inz
     d ptrLocEnt       s               *   inz
     d ptrMdsLocYr     s               *   inz
     d ptrMdsOwnOff    s               *   inz
     d ptrMdsDept      s               *   inz
     d scCo#           s              3s 0 inz
     d scFnd           s              3s 0 inz
     d scEmp#          s              9s 0 inz
     d scDiv           s              5s 0 inz
     d scFyr           s              3s 0 inz

      /If defined (WPILOCMST)
     d dsBillDiv     e ds                  extname(wpilocmst:rcdBillDiv)
     d                                     based(ptrBillDiv)

     d mdsLocYr        ds                  occurs(999) inz
     d  fldsLY                             like(dsLocYr)
     d mdsOwnOff       ds                  occurs(999) inz
     d  fldsOO                             like(dsOwnOff)
     d mdsDept         ds                  occurs(999) inz
     d  fldsDP                             like(dsDept)
      /else
     d dsBillDiv     e ds                  extname(wpilocmst:rcdBillDiv) inz

     d mdsLocYr        ds                  occurs(999)
     d                                     based(ptrMdsLocYr)
     d  fldsLY                             like(dsLocYr)

     d mdsOwnOff       ds                  occurs(999)
     d                                     based(ptrMdsOwnOff)
     d  fldsOO                             like(dsOwnOff)
     d mdsDept         ds                  occurs(999)
     d                                     based(ptrMdsDept)
     d  fldsDP                             like(dsDept)
      /Endif


      *
     d dsInitData      ds
     d ClientId                       3    overlay(dsInitData)
     d NumOfDivs                      5  0 overlay(dsInitData:4)
     d stateRepDiv                    5  0 overlay(dsInitData:9)
     d AddDiv                          n   overlay(dsInitData:14)
     d flgPEOStatus                   1    overlay(dsInitData:15)
     d flgSRRB                        1    overlay(dsInitData:16)
     d flgSRRQ                        1    overlay(dsInitData:17)
     d flgNRRB                        1    overlay(dsInitData:18)
     d flgNRRQ                        1    overlay(dsInitData:19)
     d flgMANC                        1    overlay(dsInitData:20)
     d flgFIFG                        1    overlay(dsInitData:21)
     d ClmAdmOnly                     1    overlay(dsInitData:22)
     d flgQte                         1    overlay(dsInitData:23)
      *
     d dsB             ds                  likeds(dsInitData)
     d                                     based(ptrInitData)
     d ptrInitData     s               *   inz
      *
     D*=====================================================================
     D* Location master procedures
     D*=====================================================================
     p*=====================================================================
     p* LoadBillSel  - Load Selection criterion
     P*=====================================================================
     DLoadBillSel      pr
     D inPState                       2
     D inMState                       2
     p*=====================================================================
     p* ClearBillDiv - ClearBillDiv Screen
     p*  Parm1   - Pointer to format DS
     P*=====================================================================
     DClearBillDiv     pr              n
     D inPtrBillDiv                    *
     p*=====================================================================
     p* LoadBillDiv - Load Billing division screen
     p*  Parm1   - RRN (9A) Employer master (WMEMP)
     p*  Parm2   - Pointer for format DS
     p*  Parm3   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - Endorsement
     p*  Parm4   - Mode(1A) 'C'-Change, 'D'-Display,
     p*  Parm5   - Fund Year for Employer detail for Quote
     p*  Returns - success indicator
     P*=====================================================================
     DLoadBillDiv      pr              n
     D inRRN                          9  0
     D inPtrBillDiv                    *
     D inCaller                       3a   const
     D inMode                         1a   const
     d inFyr                          3  0 const options(*nopass)
     p*=====================================================================
     p* ValidBillDiv - Validate Billing Division data
     p*  Parm1   - Mode
     p*  Parm2   - Pointer to format DS
     p*  Parm3   - Employer RRN
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     DValidBillDiv     pr              n
     D inMode                         1    const
     D inPtrBillDiv                    *
     D inEMrrn                        9  0
     p*=====================================================================
     p* WriteBillDiv - Write Billing division data to file
     p*  Parm1   - Pointer to format DS
     p*  Returns - RRN of Employer Master record
     P*=====================================================================
     DWriteBillDiv     pr             9  0
     D inPtrBillDiv                    *
     p*=====================================================================
     p* UpdateBillDiv - Update Billing division changes to file
     p*  Parm1   - Pointer to format DS
     p*  Parm2   - RRN of Employer Master record
     p*  Parm3   - Mode
     p*  Returns - Error indicator
     P*=====================================================================
     DUpdateBillDiv    pr              n
     D inPtrBillDiv                    *
     D inRRN                          9  0
     D inMode                         1
      *** beg add ******** 08/05/13 *****************************
     D inQTEM                         1    options(*nopass)
      *** end add ******** 08/05/13 *****************************
      *** beg add ******** 04/11/15 *****************************
     D inFyr                          3  0 const options(*nopass)
      *** end add ******** 04/11/15 *****************************
     p*=====================================================================
     p* DspBillDiv - DspBillDiv Screen
     p*  Parm1   - Employer RRN
     p*  Parm2   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - Endorsement
     p*  Parm3   - Mode(1A) 'C'-Change, 'D'-Display,
     p*  Returns - success indicator
     P*=====================================================================
     dDspBillDiv       pr              n
     d inEMrrn                        9  0
     d inCall                         3    const
     d inMode                         1
      *
     p*=====================================================================
     p* InitBillDiv - Initialize values
     p*  Parm1   - RRN (9) Employer master (WMEMP)
     p*  Parm2   - Mode(1A) 'C'-Change, 'D'-Display,
     p*  Parm3   - Pointer for format DS
     p*  Parm4   - Pointer for format DS
     p*  Parm5   - Fund Year for Employer detail for Quote
     p*  Returns - success indicator
     P*=====================================================================
     DInitBillDiv      pr              n
     D inRRN                          9  0
     D inMode                         1
     D inPtrBillDiv                    *
     D inPtrInitData                   *
     D inFyr                          3  0 const options(*nopass)
      ***  Begin Add  ***  12/12/17  **************************************
     dPrcCollatWdw     pr
     d  inEmpRRN                      9  0 const
     d  inCall                        3    const
     d  inMode                        1    const
     d  inRrn                         9  0 const options(*nopass)
      ***  End   Add  ***  12/12/17  **************************************
       //***************************************************************************//
       //===================================================================
       // Add/Edit Entity/Location popup  window                            xes.
     p*  Parm1   - RRN (9) Employer master (WMEMP)
     p*  Parm2   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - Endorsement
     p*  Parm3   - Mode(1A) 'C'-Change, 'A'-Add
     p*  Parm4   - RRN for location file
       //===================================================================
     d PrcLocEntWdw    pr
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLocRrn                      9  0 options(*nopass)
       //*** Beg Add *** 01/22/13 **************************************************//
     d  inNBquote                     1    options(*nopass)
       //****End Add *** 01/22/13 **************************************************//
       //***************************************************************************//
       //===================================================================
       // Add/Edit Owner/Officer popup  window                              xes.
     p*  Parm1   - RRN (9) Employer master (WMEMP)
     p*  Parm2   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - Endorsement
     p*  Parm3   - Mode(1A) 'C'-Change, 'A'-Add
     p*  Parm4   - RRN for Owner/officer file
     p*  Parm5   - RRN for Location file
       //===================================================================
     d PrcOwnOffWdw    pr
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inOwnRrn                      9  0 options(*nopass) const
     d  inLocRrn                      9  0 options(*nopass) const
     p*=====================================================================
     p* DeleteBillDiv - ClearBillDiv Screen
     p*  Parm1   - RRN (9A) Employer master (WMEMP)
     p*  Parm2   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - Endorsement
     P*=====================================================================
     DDeleteBillDiv    pr              n
     D inrrn                          9  0
     D inCall                         3    Const
       //===================================================================
     p*=====================================================================
     p*=====================================================================
     d PrcLocYrWdw     pr
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLDRrn                       9  0 options(*nopass) const
     d  inLocRrn                      9  0 options(*nopass) const

     p*=====================================================================
     d BldLocYrGrd     pr              *
     d  inLocRrn                      9  0
     d  outCount                      5  0

     p*=====================================================================
     d PrcLocYrGrd     pr            10
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLocRrn                      9  0
     d*
     p*=====================================================================
     d BldOwnOffGrd    pr              *
     d  inRrn                         9  0
     d  outCount                      5  0
     d  inLocRrn                      9  0 options(*nopass) const

     p*=====================================================================
     d PrcOwnOffGrd    pr            10
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLocRrn                      9  0 options(*nopass) const
     d*
     p*=====================================================================
     d BldDeptGrd      pr              *
     d  inRrn                         9  0
     d  outCount                      5  0

     p*=====================================================================
     d PrcDeptWdw      pr
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inDPRrn                       9  0 options(*nopass) const
     d*
     p*=====================================================================
     d PrcAddNamWdw    pr
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inInsRrn                      9  0 options(*nopass) const
     d*
