e    h Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WPIENDORSE')
     H nomain
     *========================================================================
     *
     * WPIENDORSE - Print Policy Endorsement (API)
     *
     * Created   : 03/13/12
     * Programmer: C:282
     * Project   : 114WEG
     *
     *========================================================================
     * REVISIONS:
      *
      * 01/07/25 C:079 Support: 150/118204
      *                Change editing on Waiver Endorsement
      *
      * 05/10/24 C:079 Ticket: 115133-133
      *                Update WVEMST
      *
      * 12/11/23 C:340 Project: PHW114578
      *                Add Validation for Change on Default Waiver
      *                Description.
      *
      * 12/09/22 C:333 Support: 105489
      *                Waiver endorsements are not rolling with renewals
      *
      * 07/11/22 C:33  Support: 94693
      *               -Add RenewWaivers
      *               -Add RtvCurrentForm
      *
      * 02/03/22 C:136 Support: 100884
      *                Correct logic in IsIncludedState
      *
      * 02/02/21 C:136 Project: WC2757
      *                Add GetWaiverInfo
      *                Add UpdateBlkWvrDft
      *                Add IsIncludedState
      *                Add GetWaiverType
      *
      * 10/28/20 C:079 Project: WC2671
      *                Do not zero out the LDA in PrtPolEnd
      *                Take out hard coding in PrtPolEnd
      *
      * 05/08/19 C:300 Support: 78170 (LUBA)
      *                Audit Requests - Job Failure while printing    *
      *                WC000311A endorsement form                     *
      *
     *========================================================================
     * File Definitions
     *========================================================================
     fsmupl     if   e           k disk
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwdell1    uf   e           k disk    rename(pel01:pel01)
     fwmmul6    if   e           k disk
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwtenl     if   e           k disk
     fwtesl     if   e           k disk
      *
     *---------------------------------------------------------------------
     * Global Variable
     *---------------------------------------------------------------------
     d inForm          s             15
     d inCo#           s              3
     d inFnd#          s              3
     d inFyr           s              3
     d inEmp#          s              9
     d inOutq          s             10
     d outPrt          s              1
      //*** Beg Add 05/10/24 *****************************************
       dcl-s intStRptDiv Zoned(5:0);
      //*** End Add 05/10/24 *****************************************
     *---------------------------------------------------------------------
     * Data Structures
     *---------------------------------------------------------------------
     d svLda           ds          1024    dtaara(*LDA)
     d  ldEnd#                 1      5  0
     d  ldEnda                 1      5
     d  ldMyrr                 6     14  0
     d  ldEedt                15     22  0
     d  ldFyr                 23     25  0
     d  ldXml                191    250
     d*** Beg Add *** 10/28/20 ******************************************
     d*** End Add *** 10/28/20 ******************************************
      *
     d psds          esds                  extname(szq1p)
      *

       //***  Begin Add  ***  07/11/22  *************
      /copy sPrDate
       //***  End   Add  ***  07/11/22  *************
      /Copy SprSql
      /Copy wPrEmplr
      /Free
       //===================================================
       // Look through all divisions for waiver flag value
       //  for a waiver form
       //===================================================
       Dcl-Proc GetDivForWaiverType Export;
       Dcl-Pi *N Ind;
        inCo# Like(MUco#);
        inFnd Like(MUfnd);
        inFyr  Like(MUfyr);
        inEmp Like(MUemp#);
        inForm Like(MUform);
        inRateState Like(MUenst);
        inWvrType Char(1);
        outDiv Like(Mudiv);
       End-Pi;
       Dcl-S chrEndState Like(ENst);
       Dcl-S chrRateCode Like(ELrtcd);
       Dcl-S chrRateState Like(RSjrst);
       Dcl-S decPolEffDate Like(ELpfdt);
       Dcl-S chrWvrType Like(ELwvtp);
       // Return *Off if form isn't flagged for waiver
       Chain (inCo#:inFnd:inForm) pen01;
       If ENtype <> 'Waiver';
          Return *Off;
       Endif;
       Exec Sql
        Select Min(ELdiv)
        Into :outDiv
        From WDELP
        Join WMRSP
         On RSco# = ELco#
         And RSrtcd = ELrtcd
        Where ELco# = :inCo#
         And ELfnd = :inFnd
         And ELfyr = :inFyr
         And ELemp# = :inEmp
         And ELpfdt <> ELptdt
         And ELwvtp = :inWvrType
         And RSjrst = :inRateState;
       If Not IsValidFetch(sqlStt);
          Return *Off;
       Endif;
       Return *On;
       End-Proc;
       //*** Beg Add *** 10/28/20 *****************************************
       //===================================================
       // Get the Endorsment Form Name
       //========================================================================================
       Dcl-Proc GetPolEnd Export;

       Dcl-Pi *N;
        inCo# Like(MUco#);
        inFnd Like(MUfnd);
        prState Like(MUenSt);
        inEfDt Like(MUeedt);
        prForm like(MUform);
        outPrintPgm Like(MUpgm) Options(*Nopass);
        inFyr  Like(MUfyr) Options(*Nopass);
        inEmp Like(MUemp#) Options(*Nopass);
        inDiv  Like(MUdiv) Options(*Nopass);
       End-Pi;

       Dcl-S chrAddlInfoPgm Like(MUpgm);
       Dcl-S chrENblkWvDs Char(500);
       Dcl-S chrEnForm Like(MUform);
       Dcl-S chrEnState Like(MUenSt);
       Dcl-S chrForm Like(MUform);
       Dcl-S chrPrintPgm Like(MUpgm);
       Dcl-S chrNcciForm Like(MUform);
       Dcl-S chrNcciPrtPgm Like(MUpgm);
       Dcl-S chrNcciAddInfoPgm Like(MUpgm);
       Dcl-S chrNcciWvrDesc Char(500);
       Dcl-S chrRateState Char(2);
       Dcl-S hasDivForForm Ind;
       Dcl-S hasNcciForm Ind;
       Dcl-S hasStateForm Ind;
       Dcl-S hasWvrType Ind;
       Dcl-S hasWvrForm Ind;
       If %Parms > 4;
          chrForm = prForm;
       Endif;
       Select;
       When chrForm <> *Blanks;
        // Find the form for Midterm Endorsements.
          Exec Sql
           Declare E2 Cursor For
            Select ENst, ENform, ENpgm2
            From WTENP
            Where ENco# = :inCo#
              And ENfnd = :inFnd
              And ENstat <> 'I'
              And Substr(ENform,1,8) = Substr(:prForm,1,8)
              And ENeddt <= :inEfDt
              And (ENrpdt = 0 Or ENrpdt > :inEfDt)
            Order by ENeddt Desc;
          Exec Sql
           Open E2;
          Dou sqlStt = '02000';
             chrEnState = *Blank;
             chrEnForm = *Blank;
             chrPrintPgm = *Blank;
             chrAddlInfoPgm = *Blank;
             chrENblkWvDs = *Blank;
             Exec Sql
              Fetch Next
              From E2
              Into :chrEnState,
                   :chrEnForm,
                   :chrPrintPgm;
             Select;
             When Not IsValidFetch(sqlStt);
                Iter;
             When chrEnState = prState;
                hasStateForm = *On;
                Leave;
             When chrEnState = *Blank
              And %Subst(chrEnForm:3:2) = '00'
              And IsIncludedState(inCo#:chrEnForm:prState:inEfDt) = *On;
                chrNcciForm = chrEnForm;
                chrNcciPrtPgm = chrPrintPgm;
                chrNcciAddInfoPgm = chrAddlInfoPgm;
                chrNcciWvrDesc = chrENblkWvDs;
                hasNcciForm = *On;
             Endsl;
          Enddo;
          Exec Sql
           Close E2;
          Select;
          When hasStateForm = *On;
             hasWvrForm = *On;
             prForm = chrEnForm;
             If %Parms > 5;
                outPrintPgm = chrPrintPgm;
             Endif;
          When hasNcciForm = *On;
             hasWvrForm = *On;
             prForm = chrNcciForm;
             prState = *Blanks;
             If %Parms > 5;
                outPrintPgm = chrNcciPrtPgm;
             Endif;
           // Get policy override verbiage or
           //  error message
          Endsl;
       Endsl;
       End-Proc;
       //*** End Add *** 10/28/20 *****************************************
       //===================================================
       // Returns *On if endorsement form exists for input
       //  conditions.
       // If *Off, error message is passed back in outWvrProjDesc.
       // If prForm is passed in as *blank (Midterm Endorsement)
       //    5 - 8 parms - returns *On if form is found for
       //                  co#/fnd/state/effdate
       //                - returns waiver info in associated
       //                  parms
       //                - for NCCI form the state is passed back
       //                  as a *blank to signal midterm endorsement
       //                  to populate MUENST as a *blank
       //                - default verbiage returned from groups
       //    11 parms    - returns *On if form is found for
       //                  co#/fnd/state/effdate
       //                - default verbiage comes from policy first
       //                  then from group setup
       // If prForm is not *Blank (W/W Endorsements, Manual Rollver, Renewal)
       //    5 - 8 parms - returns *On (the return is irrelevant)
       //                - just the group default is returned in proj desc
       //    11 parms    - returns *On if the policy qualifies for the
       //                  endorsement
       //                - default verbiage comes from policy first
       //                  then from group setup
       //===================================================
       Dcl-Proc GetWaiverInfo Export;
       Dcl-Pi *N Ind;
        inCo# Like(MUco#);
        inFnd Like(MUfnd);
        prState Like(MUenSt);
        inEfDt Like(MUeedt);
        prForm like(MUform) Options(*Nopass);
        outPrintPgm Like(MUpgm) Options(*Nopass);
        outAddlInfoPgm Like(MUpgm) Options(*Nopass);
        outWvrProjDesc Char(500) Options(*Nopass);
        inFyr  Like(MUfyr) Options(*Nopass);
        inEmp Like(MUemp#) Options(*Nopass);
        inDiv  Like(MUdiv) Options(*Nopass);
       End-Pi;
       Dcl-S chrAddlInfoPgm Like(MUpgm);
       Dcl-S chrENblkWvDs Char(500);
       Dcl-S chrEnForm Like(MUform);
       Dcl-S chrEnState Like(MUenSt);
       Dcl-S chrForm Like(MUform);
       Dcl-S chrPrintPgm Like(MUpgm);
       Dcl-S chrNcciForm Like(MUform);
       Dcl-S chrNcciPrtPgm Like(MUpgm);
       Dcl-S chrNcciAddInfoPgm Like(MUpgm);
       Dcl-S chrNcciWvrDesc Char(500);
       Dcl-S chrRateState Char(2);
       Dcl-S hasDivForForm Ind;
       Dcl-S hasNcciForm Ind;
       Dcl-S hasStateForm Ind;
       Dcl-S hasWvrType Ind;
       Dcl-S hasWvrForm Ind;
       If %Parms > 4;
          chrForm = prForm;
       Endif;
       Select;
       When chrForm = *Blanks;
        // Find the form for Midterm Endorsements.
          Exec Sql
           Declare E1 Cursor For
            Select ENst, ENform, ENpgm2, ENpgm3, ENblkWvDs
            From WTENP
            Where ENco# = :inCo#
              And ENfnd = :inFnd
              And ENstat <> 'I'
              And ENtype = 'Waiver'
              And ENeddt <= :inEfDt
              And (ENrpdt = 0 Or ENrpdt > :inEfDt)
            Order by ENeddt Desc;
          Exec Sql
           Open E1;
          Dou sqlStt = '02000';
             chrEnState = *Blank;
             chrEnForm = *Blank;
             chrPrintPgm = *Blank;
             chrAddlInfoPgm = *Blank;
             chrENblkWvDs = *Blank;
             Exec Sql
              Fetch Next
              From E1
              Into :chrEnState,
                   :chrEnForm,
                   :chrPrintPgm,
                   :chrAddlInfoPgm,
                   :chrENblkWvDs;
             Select;
             When Not IsValidFetch(sqlStt);
                Iter;
             When chrEnState = prState;
                hasStateForm = *On;
                Leave;
             When chrNcciForm <> *Blank;
             When chrEnState = *Blank
              And %Subst(chrEnForm:3:2) = '00'
              And IsIncludedState(inCo#:chrEnForm:prState:inEfDt) = *On;
                chrNcciForm = chrEnForm;
                chrNcciPrtPgm = chrPrintPgm;
                chrNcciAddInfoPgm = chrAddlInfoPgm;
                chrNcciWvrDesc = chrENblkWvDs;
                hasNcciForm = *On;
             Endsl;
          Enddo;
          Exec Sql
           Close E1;
          Select;
          When hasStateForm = *On;
             hasWvrForm = *On;
             prForm = chrEnForm;
             If %Parms > 5;
                outPrintPgm = chrPrintPgm;
             Endif;
             If %Parms > 6;
                outAddlInfoPgm = chrAddlInfoPgm;
             Endif;
             If %Parms > 7;
                outWvrProjDesc = chrENblkWvDs;
             Endif;
             If %Parms > 8;
                Exsr GetPolicyOvrDesc;
             Endif;
          When hasNcciForm = *On;
             hasWvrForm = *On;
             prForm = chrNcciForm;
             prState = *Blanks;
             If %Parms > 5;
                outPrintPgm = chrNcciPrtPgm;
             Endif;
             If %Parms > 6;
                outAddlInfoPgm = chrNcciAddInfoPgm;
             Endif;
             If %Parms > 7;
                outWvrProjDesc = chrNcciWvrDesc;
             Endif;
           // Get policy override verbiage or
           //  error message
             If %Parms > 8;
                Exsr GetPolicyOvrDesc;
             Endif;
          Endsl;
       Other;
        // Get default verbiage from endorsement form
        //  and policy detail if form is passed in.
        // If policy detail is passed in, edit to
        //  ensure that policy is eligible for the
        //  endorsement.
          Exec Sql
           Select ENst, ENblkWvDs
           Into :chrEnState, :chrENblkWvDs
           From WTENP
           Where ENco# = :inCo#
            And ENfnd = :inFnd
            And ENform = :chrForm
            And Enstat <> 'I';
          If IsValidFetch(sqlStt);
             hasWvrForm = *On;
             outWvrProjDesc = chrENblkWvDs;
           // Get policy override verbiage or
           //  error message
             If %Parms > 8;
                Exsr GetPolicyOvrDesc;
             Endif;
          Endif;
       Endsl;
       Select;
       When %Parms < 9;
          If hasWvrForm = *Off;
          outWvrProjDesc = 'No waiver form exists ' +
           'in group setup for this policy.';
          EndIf;
          Return hasWvrForm;
       When hasWvrForm = *Off;
          outWvrProjDesc = 'No waiver form exists ' +
           'in group setup for this policy.';
          Return *Off;
       When hasDivForForm = *Off;
          outWvrProjDesc = 'This form is valid for a state ' +
           'that does not exist on the policy.';
          Return *Off;
       When hasWvrType = *Off;
          outWvrProjDesc = 'Set Waiver Surcharge Flag as Blanket or ' +
           'Specific in Policy Year Detail.';
        // Don't invoke this edit in WTPOLDTL...
          %Subst(outWvrProjDesc:495:6) = 'IGNORE';
          Return *Off;
       Other;
          Return *On;
       Endsl;
      //---------------------------------------------------
       Begsr GetPolicyOvrDesc;
      //---------------------------------------------------
        Select;
        When chrForm = *Blanks;
          // Return policy detail vbg if it exists for midterm endorsement
           Chain (inCo#:inFnd:inEmp:inDiv:inFyr) pel00;
           hasDivForForm = *On;
           If ELblnkwd <> *Blanks;
              outWvrProjDesc = ELblnkWd;
           Endif;
           If ELwvtp <> *Blank;
              %Subst(outWvrProjDesc:500:1) = ELwvtp;
              hasWvrType = *On;
           Endif;
        Other;
           If %Parms > 10;
              Setll (inCo#:inFnd:inFyr:inEmp:inDiv) pel01;
           Else;
              Setll (inCo#:inFnd:inFyr:inEmp) pel01;
           Endif;
           Dou %Eof(WDELL1);
              If %Parms > 10;
                 Reade(n) (inCo#:inFnd:inFyr:inEmp:inDiv) pel01;
              Else;
                 Reade(n) (inCo#:inFnd:inFyr:inEmp) pel01;
              Endif;
              If %Eof;
                 Leave;
              Endif;
              chrRateState =
               GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
              Select;
              When chrEnState = *Blank
               And %Subst(chrForm:3:2) = '00'
               And IsIncludedState(inCo#:chrForm:
                chrRateState:inEfDt) = *On;
                 hasDivForForm = *On;
                 If ELblnkwd <> *Blanks;
                    outWvrProjDesc = ELblnkWd;
                 Endif;
                 If ELwvtp <> *Blank;
                    %Subst(outWvrProjDesc:500:1) = ELwvtp;
                    hasWvrType = *On;
                    //*** Beg Add *** 01/07/25 *****************************
                    Leave;
                    //*** End Add *** 01/07/25 *****************************
                 Endif;
                 //*** Beg Del *** 01/07/25 ********************************
                 //***Leave;
                 //*** End Del *** 01/07/25 ********************************
              When chrEnState = chrRateState;
                 hasDivForForm = *On;
                 If ELblnkwd <> *Blanks;
                    outWvrProjDesc = ELblnkWd;
                 Endif;
                 If ELwvtp <> *Blank;
                    %Subst(outWvrProjDesc:500:1) = ELwvtp;
                    hasWvrType = *On;
                    //*** Beg Add *** 01/07/25 *****************************
                    Leave;
                    //*** End Add *** 01/07/25 *****************************
                 Endif;
                 //*** Beg Del *** 01/07/25 ********************************
                 //***Leave;
                 //*** End Del *** 01/07/25 ********************************
              Endsl;
           Enddo;
        Endsl;
       Endsr;
       End-Proc;
       //===================================================
       Dcl-Proc IsIncludedState Export;
       //===================================================
       Dcl-Pi *N Ind;
        inCo# Like(ESco#);
        inForm like(ESform) Const;
        inState Like(ESst) Const;
        inEfDt Like(ELpfdt) Const;
       End-Pi;
       Dcl-S isIncluded Ind Inz(*On);
       Chain (inCo#:inForm:inState) pes01;
       Select;
       When Not(%Found);
       When ESaDte > inEfDt;
          isIncluded = *Off;
       When ESexdt <> 0 And ESexdt <= inEfDt;
          isIncluded = *Off;
       When ESapex = 'A' And ESst <> inState;
          isIncluded = *Off;
       When ESapex = 'E' And ESst = inState;
          isIncluded = *Off;
       EndSl;
       Return isIncluded;
       End-Proc;
      /End-Free
     *------------------------------------------------------------------------
     * PrtPolEnd - This API will the print the endorsement WC000311A with
     *             audit request.
     *------------------------------------------------------------------------
     P PrtPolEnd       B                   export

     *---------------------------------------------------------------------
     * Procedure Interface
     *---------------------------------------------------------------------
     d PrtPolEnd       PI             1
     d inForm                        15
     d inCo#                          3
     d inFnd#                         3
     d inFyr                          3
     d inEmp#                         9
     d inDiv                          5
     d*** Beg Add *** 10/28/20 *********************************************
     d inPfDt                         7
     d*** End Add *** 10/28/20 *********************************************
     d inOutq                        10
     d outPrt                         1

     *---------------------------------------------------------------------
     * Local Variable
     *---------------------------------------------------------------------
     d srDti           s              8  0
     d srFfmt          s              8
     d srTfmt          s              8
     d srDto           s             10
     d srErr           s              1

     d strCo#          s              3
     d strFnd#         s              3
     d strEmp#         s              9
     d strDiv          s              5
     d strFyr          s              3
     d strJob          s              6
     d strPrtp         s             10
     d strOutq         s             10
     d strForm         s             15
     d strFldr         s             32

     d intCo#          s              3  0
     d intFnd#         s              3  0
     d intEmp#         s              9  0
     d intFyr          s              3  0

     d intEnd#         s              5  0
     d*** Beg Add *** 10/28/20 ********************************************
     d intDiv          s                   Like(ELdiv)
     d intPfDt         s              7  0
     d strRateState    s             32
     d strPrtPgm       s                   Like(MUpgm)
     d*** End Add *** 10/28/20 ********************************************
      *
     *---------------------------------------------------------------------
     * Prototype Declaration
     *---------------------------------------------------------------------
     d GetSrcVDat      pr                  extPgm('SRCVTDAT')
     d srDti                          8  0
     d srFfmt                         8
     d srTfmt                         8
     d srDto                         10
     d srErr                          1
      *
     d GetPrtOpt       pr                  extPgm('W7054C')
     d strCo#                         3
     d strFnd#                        3
     d strEmp#                        9
     d strDiv                         5
     d strFyr                         3
     d strJob                         6
     d strPrtp                       10
     d strOutq                       10
     d strForm                       15
     d strFldr                       32
     *================================================================
     *  KLIST definitions
     *================================================================
     ** Key List for WMMUL6
     c     keyWMMUL6     klist
     c                   kfld                    intCo#
     c                   kfld                    intFnd#
     c                   kfld                    intFyr
     c                   kfld                    intEmp#
     c                   kfld                    intEnd#
     c                   kfld                    strForm
      *
     ** Key List for WTENL
     c     keyWTENL      klist
     c                   kfld                    intCo#
     c                   kfld                    intFnd#
     c                   kfld                    strForm
      *
     ** Key List for SMUPL
     c     keySMUPL      klist
     c                   kfld                    q1user
      *
     *** Procedure Definition ***
      /free

       strCo#   = inCo#;
       strFnd#  = inFnd#;
       strFyr   = inFyr;
       strEmp#  = inEmp#;
       intCo#   = %dec(inCo#:3:0);
       intFnd#  = %dec(inFnd#:3:0);
       intFyr   = %dec(inFyr:3:0);
       intEmp#  = %dec(inEmp#:9:0);
       strDiv   = inDiv;
       intPfDt  = %Dec(inPfDt:7:0);
       strOutq  = inOutq;
       //*** Beg Add *** 10/28/20 ******************************************************************
       intDiv   = %Dec(inDiv:5:0);
       strRateState = GetRatingState(intCo#:intFnd#:intEmp#:intDiv:intFyr);
       //*** End Add *** 10/28/20 ******************************************************************
       //*** Beg Chg *** 10/28/20 ******************************************************************
       //***strForm  = 'WC000311A';
       GetPolEnd(intCo#:intFnd#:strRateState:intPfDt:inForm:strPrtPgm);
       //*** End Chg *** 10/28/20 ******************************************************************
       intEnd# = 0;
       //*** Beg Add *** 10/28/20 ******************************************************************
       strForm  = inForm;
       //*** End Add *** 10/28/20 ******************************************************************

       Chain keyWMMUL6 pmu01;

       If %Found(wmmul6);

       //*** Beg Del *** 10/28/20 ******************************************************************
       //***   If inForm = 'WC000311';
       //*** End Del *** 10/28/20 ******************************************************************


       // Retrieve the print program used to print the endorsement form.

             Chain keyWTENL pen01;

             If %found(wtenl);

                strPrtp = ENpgm2;
                strFldr = ENfldr;

             Endif;                                                    // %found(pen01)

       // Retrieve the output queue if blank

             Chain keySMUPL pup01;

             If %found(smupl);

                If strOutq = *blank;

                   strOutq = UPoutq;

                Endif;                                                 // xxoutq = *blank

             Endif;                                                    // %found(pup01)

       // Subroutine to write Endorsement Number, Effective date, and policy year
       // to local data area.

             in  *Dtaara;

       //*** Beg Del *** 10/28/20 ******************************************
       //*** svLda  = *zero;
       //*** End Del *** 10/28/20 ******************************************

             ldEnd# = intEnd#;

       // Retrieve the Effective date in MDY format.
             srDti  = MUeedt;
             srFfmt = '*CYMD';
             srTfmt = *blanks;

             GetSrcVDat(srDti:srFfmt:srTfmt:srDto:srErr);

             ldEedt = %dec(srDto:8:0);
             ldFyr =  intFyr;

             out *Dtaara;

       // Call W7054C to print single endorsement.

             GetPrtOpt(strCo#:strFnd#:strEmp#:strDiv:strFyr:
                         strJob:strPrtp:strOutq:strForm:strFldr);

       // Form get printed than,

             outPrt = '1';

       //*** Beg Del *** 10/28/20 ******************************************************************
       //***   Endif;                                                           //@@form = 'WC000311
       //*** End Del *** 10/28/20 ******************************************************************

       Else;

          outPrt = '0';

       Endif;                                                              // %found(pmu01)

       return outPrt;

       /end-free

     P PrtPolEnd       E

       //***  Begin Add  ***  07/11/22  ************************
       //===================================================
     pRenewWaivers...
     p                 b                   export
     dRenewWaivers...
     d                 pi             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inYearNew                      3  0 const
     d inYearOld                      3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5    const options(*nopass)

     ddecCount         s              9  0 inz
     ddecWav#          s              5  0 inz(1)
     disEofInfoYear    s              1n   inz
     disEofWMMUP       s              1n   inz
     disEofWMWVP       s              1n   inz
     disFoundYearNew   s              1n   inz
     ddecDivStRpt      s              5  0 inz
     dstrFormNew       s             15    inz
     dstrFormOld       s             15    inz
     dstrRatingSt      s              2    inz
     dstrSql           s           1000    varying inz

     ddsInfoYear     e ds                  extname(WDELP) inz
     ddsInfoYearOld    ds                  likeds(dsInfoYear) inz
     ddsInfoYearNew    ds                  likeds(dsInfoYear) inz
     ddsWTENP        e ds                  extname(WTENP) inz qualified
     ddsWMMUP        e ds                  extname(WMMUP) inz qualified
     ddsWMWVP        e ds                  extname(WMWVP) inz qualified
     ddsWXFMP1       e ds                  extname(WXFMP1) inz qualified
      /Free
       decDivStRpt = GetStrptDv(inCo#:inFnd:inEmp#);
       // WDELP should be in PDSFIL* for this to work correctly
       Exsr BuildSqlInfoYearOld;
       Dou isEofInfoYear;
          Exec sql fetch next from cursorInfoYear into :dsInfoYearOld;
          Select;
          When not IsValidFetch(sqlstt);
             isEofInfoYear = *on;
             Iter;
          Endsl;
          // WDELLQ will need to be overridden to PDSQTE* if renewal in Quote
          Exsr GetInfoYearNew;
          Select;
          When not isFoundYearNew;
             Iter;
          Endsl;
          Exsr BuildSqlWMMUP;
          Dou isEofWMMUP;
             // WMMUP should be in PDSFIL* for this to work correctly
             Exec sql fetch next from cursorWMMUP into :dsWMMUP;
             Select;
             When not IsValidFetch(sqlstt);
                isEofWMMUP = *on;
                Iter;
             When MUexFl = 'E'; // Excluded from prior year
                Iter;
             Endsl;
             strFormOld = dsWMMUP.MUform;
             // Determine if form has been replaced
             strFormNew = RtvCurrentForm(inCo#:dsWMMUP.MUform:strRatingSt
              :dsInfoYearNew.ELpfDt);
             // WMMMULQ will need to be overridden to PDSQTE* if renewal in Quote
             // WMWVL1Q will need to be overridden to PDSQTE* if renewal in Quote
             // WXFML11 will need to be overridden to PDSQTE* if renewal in Quote
             // Check for waivers already in new year
             Reset decCount;
             Exec sql
              select count(*) into :decCount
               from WMWVL1Q
               where WVco#=:dsInfoYearNew.ELco#
                and WVfnd=:dsInfoYearNew.ELfnd
                and WVfyr=:dsInfoYearNew.ELfyr
                and WVemp#=:dsInfoYearNew.ELemp#
                and WVdiv=:dsInfoYearNew.ELdiv
                and WVeNm=:strFormNew;
             Select;
             When decCount > 0;
                Iter;
             Endsl;
             // Write WMMULQ for new year, if not existing
             Exsr WriteWMMUP;
             // Copy WMWVP records from old year to new year (WMWVL1Q)
             Exsr WriteWMWVP;
          Enddo;
       Enddo;
       Return *on;
       //====================================================
       Begsr BuildSqlInfoYearOld;
        strSql = 'select * from WDELP ' +
         'where ELco# = ' + %char(inCo#) +
         ' and ELfnd = ' + %char(inFnd) +
         ' and ELfyr = ' + %char(inYearOld) +
         ' and ELemp# = ' + %char(inEmp#);
        If %parms >=  6 and inDiv <> *blanks;
           strSql += ' and ELdiv = ' + inDiv;
        Endif;
        Exec sql close cursorInfoYear;
        Exec sql prepare sqlInfoYear from :strSql;
        Exec sql declare cursorInfoYear cursor for sqlInfoYear;
        Exec sql open cursorInfoYear;
        Reset isEofInfoYear;
       Endsr;
       //====================================================
       Begsr BuildSqlWMMUP;
        // Get waiver endorsements that have not been excluded
        // (must not be excluded from state reporting division ither)
        strSql = 'select * from WMMUP d ' +
         'join WTENP on ENco#=d.MUco# and ENfnd=d.MUfnd and ENform=d.MUFORM ' +
         'where d.MUco# = ' + %char(dsInfoYearOld.ELco#) +
         ' and d.MUfnd = ' + %char(dsInfoYearOld.ELfnd) +
         ' and d.MUfyr = ' + %char(dsInfoYearOld.ELfyr) +
         ' and d.MUemp# = ' + %char(dsInfoYearOld.ELemp#) +
         ' and (d.MUdiv = ' + %char(dsInfoYearOld.ELdiv) +
         '  or d.MUdiv = ' + %char(decDivStRpt) + ')' +
         ' and d.MUexfl <> ''E''' +
         ' and ENtype = ''Waiver''' +
         ' and d.MUform not in' +
         ' (select st.MUform' +
         //***** Beg Chg *** 12/09/22 *******************************
         //*****'   from V2r1qadta.WMMUP st' +
         '   from WMMUP st' +
         //***** End Chg *** 12/09/22 *******************************
         '    where st.MUco#=d.MUco#' +
         '     and st.MUfnd=d.MUfnd' +
         '     and st.MUemp#=d.MUemp#' +
         '     and st.MUdiv=' + %char(decDivStRpt) +
         '     and st.MUexfl=''E''' +
         '     and st.MUfyr=d.MUfyr' +
         '     and st.MUform=d.MUform)';
        Exec sql close cursorWMMUP;
        Exec sql prepare sqlWMMUP from :strSql;
        Exec sql declare cursorWMMUP cursor for sqlWMMUP;
        Exec sql open cursorWMMUP;
        Reset isEofWMMUP;
       Endsr;
       //====================================================
       Begsr BuildSqlWMWVP;
        strSql = 'select * from WMWVP ' +
         'where WVco# = ' + %char(dsInfoYearOld.ELco#) +
         ' and WVfnd = ' + %char(dsInfoYearOld.ELfnd) +
         ' and WVfyr = ' + %char(dsInfoYearOld.ELfyr) +
         ' and WVemp# = ' + %char(dsInfoYearOld.ELEmp#) +
         ' and WVdiv = ' + %char(dsInfoYearOld.ELdiv) +
         ' and WVEnM = ''' + strFormOld + '''';
        Exec sql close cursorWMWVP;
        Exec sql prepare sqlWMWVP from :strSql;
        Exec sql declare cursorWMWVP cursor for sqlWMWVP;
        Exec sql open cursorWMWVP;
        Reset isEofWMWVP;
       Endsr;
       //====================================================
       Begsr GetInfoYearNew;
        Exec sql
         select * into :dsInfoYearNew
          from WDELLQ
          where ELco# = :dsInfoYearOld.ELco#
           and ELfnd = :dsInfoYearOld.ELfnd
           and ELemp# = :dsInfoYearOld.ELemp#
           and ELdiv = :dsInfoYearOld.ELdiv
           and ELfyr = :inYearNew;
        isFoundYearNew = IsValidFetch(sqlstt);
        strRatingSt = GetRatingState(dsInfoYearNew.ELco#:dsInfoYearNew.ELFND
         :dsInfoYearNew.ELemp#:dsInfoYearNew.ELdiv:dsInfoYearNew.ELfyr);
       Endsr;
       //====================================================
       // Write WMMULQ for new year, if not existing
       Begsr WriteWMMUP;
        Exec sql
         select * into :dsWMMUP
          from WMMULQ
          where MUco# = :dsInfoYearNew.ELco#
           and MUfnd = :dsInfoYearNew.ELfnd
           and MUemp# = :dsInfoYearNew.ELemp#
           and MUdiv = :decDivStRpt
           and MUfyr = :dsInfoYearNew.ELfyr
           and MUform = :strFormNew;
        If not IsValidFetch(sqlstt);
           Reset dsWMMUP;
           Exec sql
            select * into :dsWMMUP
             from WMMUP
             where MUco# = :dsInfoYearOld.ELco#
              and MUfnd = :dsInfoYearOld.ELfnd
              and MUemp# = :dsInfoYearOld.ELemp#
              and MUdiv = :dsInfoYearOld.ELdiv
              and MUfyr = :inYearOld
              and MUform = :strFormOld
             order by MUend#
             limit 1;
           If not IsValidFetch(sqlstt);
              Reset dsWTENP;
              Exec sql
               select * into :dsWTENP
                from WTENP
                where ENco# = dsWMMUP.MUco#
                 and ENfnd = dsWMMUP.MUfnd
                 and ENform = dsWMMUP.MUform;
              dsWMMUP.MUco#  = dsInfoYearNew.ELco#;
              dsWMMUP.MUfnd  = dsInfoYearNew.ELfnd;
              dsWMMUP.MUemp# = dsInfoYearNew.ELemp#;
              dsWMMUP.MUform = strFormNew;
              dsWMMUP.MUseq# = 0;
              dsWMMUP.MUenSt = dsWTENP.ENst;
              dsWMMUP.MUdesc = dsWTENP.ENdesc;
              dsWMMUP.MUdsc2 = dsWTENP.ENdsc2;
              dsWMMUP.MUmnAu = dsWTENP.ENmnAu;
              dsWMMUP.MUprnt = dsWTENP.ENprnt;
              dsWMMUP.MUexfl = 'I';
              dsWMMUP.MUmnau = 'M';
              dsWMMUP.MUstat = 'A';
           Endif;
           dsWMMUP.MUdiv  = decDivStRpt;
           dsWMMUP.MUfyr  = dsInfoYearNew.ELfyr;
           dsWMMUP.MUeEDt = dsInfoYearNew.ELpfDt;
           dsWMMUP.MUend# = 0;
           dsWMMUP.MUiusr = q1user;
           dsWMMUP.MUuser = q1user;
           dsWMMUP.MUiPgm = q1pgm;
           dsWMMUP.MUpgm  = q1pgm;
           dsWMMUP.MUiDt  = DateTo7(SystemDate);
           dsWMMUP.MUdt   = dsWMMUP.MUiDt;
           dsWMMUP.MUtime = TimeTo6(SystemTime);
           Exec sql
            insert into WMMULQ
            values(:dsWMMUP);
           // Copy WXFMP1 records from old year to new year (WXFML11)
           Exsr WriteWXFMP1;
        Endif;
       Endsr;
       //====================================================
       // Copy WMWVP records from old year to new year (WMWVL1Q)
       Begsr WriteWMWVP;
        Exsr BuildSqlWMWVP;
        Reset decWav#;
        Dou isEofWMWVP;
           Reset dsWMWVP;
           Exec sql fetch next from cursorWMWVP into :dsWMWVP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMWVP = *on;
              Iter;
           Endsl;
           dsWMWVP.WVfyr  = dsInfoYearNew.ELfyr;
           dsWMWVP.WVefDt = dsInfoYearNew.ELpfDt;
           //*** Beg Chg *** 05/10/24 ********************************
           //***dsWMWVP.WVemSt = strRatingSt;
           intStRptDiv = GetStRptDv(dsWMWVP.WVco#:dsWMWVP.WVfnd:
            dsWMWVP.WVemp#);
           dsWMWVP.WVemst = GetRatingState(dsWMWVP.WVco#:dsWMWVP.WVfnd:
             dsWMWVP.WVemp#:intStRptDiv:dsWMWVP.WVfyr);
           //*** End Add *** 05/10/24 ********************************
           dsWMWVP.WVeNm  = strFormNew;
           dsWMWVP.WVend# = 0;
           dsWMWVP.WVprt# = 0;
           dsWMWVP.WVwav# = decWav#;
           dsWMWVP.WViUsr = q1user;
           dsWMWVP.WVuser = q1user;
           dsWMWVP.WViPgm = q1pgm;
           dsWMWVP.WVpgm  = q1pgm;
           dsWMWVP.WViTm  = TimeTo6(SystemTime);
           dsWMWVP.WViDt  = DateTo7(SystemDate);
           dsWMWVP.WVdt   = dsWMWVP.WViDt;
           dsWMWVP.WVtime = dsWMWVP.WViTm;
           Exec sql
            insert into WMWVL1Q
            values(:dsWMWVP);
           decWav#  += 1;
        Enddo;
       Endsr;
       //====================================================
       // Copy WXFMP1 records from old year to new year (WXFML11)
       Begsr WriteWXFMP1;
        Exec sql
         select * into :dsWXFMP1
          from WXFMP1
          where F3co# = :dsInfoYearOld.ELco#
           and F3fnd = :dsInfoYearOld.ELfnd
           and F3emp# = :dsInfoYearOld.ELemp#
           and F3div = :dsInfoYearOld.ELdiv
           and F3fyr = :dsInfoYearOld.ELfyr
           and F3form = :strFormOld;
        If IsValidFetch(sqlstt);
           dsWXFMP1.F3form = strFormNew;
           dsWXFMP1.F3div  = decDivStRpt;
           dsWXFMP1.F3fyr  = dsInfoYearNew.ELfyr;
           dsWXFMP1.F3eEDt = dsInfoYearNew.ELpfDt;
           dsWXFMP1.F3end# = 0;
           dsWXFMP1.F3iUsr = q1user;
           dsWXFMP1.F3iPgm = q1pgm;
           dsWXFMP1.F3iDt  = DateTo7(SystemDate);
           dsWXFMP1.F3user = q1user;
           dsWXFMP1.F3pgm  = q1pgm;
           dsWXFMP1.F3dt   = dsWXFMP1.F3iDt;
           dsWXFMP1.F3time = TimeTo6(SystemTime);
           Exec sql
            insert into WXFML11
            values(:dsWXFMP1);
        Endif;
       Endsr;
      /End-free
     pRenewWaivers...
     p                 e

       //===================================================
     pRtvCurrentForm...
     p                 b                   export
     dRtvCurrentForm...
     d                 pi            15
     d inCo#                          3  0 const
     d inForm                        15    const
     d inSt                           2    const
     d inEffDt                        7  0 const

     ddsEndorse        ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d form                          15
     d editionDt                      7  0
     d state                          2
     d formReplace                   15
     d stateApproved                  2
     d adoptionDt                     7  0
     d expDt                          7  0
     d excluded                       1
     ddsEndorseNull    s              5i 0 dim(10)
     ddsReplacement    ds                  qualified inz
     d form                          15
     d date                           7  0
      /Free
       Exec sql
        select ENrpFm, ENrpDt into :dsReplacement
         from WTENP
         where ENco#=:inCo# and ENform=:inForm and ENrpDt<:inEffDt;
       Select;
       When not IsValidFetch(sqlstt);
          Return inForm;
       Endsl;
       Exec sql
        select ENco#, ENFND, ENFORM, ENEDDT, ENST, ENRPFM, ESST, ESADTE, ESEXDT,
         case
         when ESAPEX is null then ' '
         when ESAPEX = 'E' then 'Y'
         else 'N' end as excluded
         into :dsEndorse :dsEndorseNull
          from WTENP
          left join WTESP on ESco#=ENco# and ESform=ENform
          where ENco#=:inCo# and ENform=:dsReplacement.form
           and ENedDt < :inEffDt and (ENst = ' ' or ENst = :inSt);
       Select;
       // Replacement form information not found
       When not IsValidFetch(sqlstt);
          Return inForm;
       // Replacement form not yet adopted
       When dsEndorse.adoptionDt > inEffDt;
          Return inForm;
       // Replacement form expired
       When dsEndorse.expDt <> 0 And dsEndorse.expDt < inEffDt;
          Return inForm;
       When dsEndorse.excluded = 'Y';
          Return inForm;
       Endsl;
       // Check if the replacement form has been replaced
       Return RtvCurrentForm(inCo#:dsEndorse.formReplace:inSt:inEffDt);
      /End-free
     pRtvCurrentForm...
     p                 e
       //***  End   Add  ***  07/11/22  ************************

       //===================================================
       Dcl-Proc UpdateBlkWvrDft Export;
       //===================================================
       Dcl-Pi *N Ind;
        inCo# Like(ELco#);
        inFnd like(ELfnd);
        inEmp Like(ELemp#);
        inDiv Like(Eldiv);
        inFyr Like(ELfyr);
        inForm Like(ENform);
        inDesc Like(ENblkWvDs);
       End-Pi;

       Dcl-S chrRateState Char(2);
       Dcl-S chrAddPgm Like(MUpgm);
       Dcl-S chrBlkPrjDs Char(500);
       Dcl-S chrWvrForm Like(MUform);
       Dcl-S chrPrtPgm Like(MUpgm);

       Chain (inCo#:inFnd:inForm) pen01;
       If Not(%Found);
          Return *Off;
       Endif;
       Setll (inCo#:inFnd:inFyr:inEmp) pel01;
       Dou %Eof(WDELL1);
          Reade (inCo#:inFnd:inFyr:inEmp) pel01;
          If %Eof;
             Leave;
          Endif;
          If ELwvtp <> 'B';
            Iter;
          Endif;
          chrRateState =
           GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
          chrWvrForm = *Blanks;
          //*** Beg Chg *** 12/11/23 *** PHW114578 ***********************
          //  If GetWaiverInfo(ELco#:ELfnd:
          //   chrRateState:ELpfdt:chrWvrForm) = *On;

          If GetWaiverInfo(ELco#:ELfnd:chrRateState:ELpfdt:chrWvrForm:
           chrPrtPgm:chrAddPgm:chrBlkPrjDs) = *On;

          //*** End Chg *** 12/11/23 *** PHW114578 ***********************
             Select;
             When inForm <> chrWvrForm;
                Iter;
          //*** Beg Chg *** 12/11/23 *** PHW114578 ***********************
          //   When inDesc <> *Blanks;
             When inDesc <> *Blanks and inDesc <> chrBlkPrjDs;
          //*** End Chg *** 12/11/23 *** PHW114578 ***********************
                ELovrWd = 'Y';
                ELblnkWd = inDesc;
             Other;
                ELovrWd = ' ';
                ELblnkWd = *Blanks;
             Endsl;
             Update pel01;
          Endif;
       Enddo;
       Return *On;
       End-Proc;

