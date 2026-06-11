     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SROFFICER - Officer/Owner Endorsement Program
      //===============================================================*
      // SROFFICER - Officer/Owner Endorsement Program                 *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //   To add an endorsement:                                      *
      //     1) Add endorsement to ctDataName                          *
      //     2) Indicate options for endorsement in ctDataEdits        *
      //        as described by dsEdits.                               *
      //        a) Add another selection to GetProcPtr if needed.      *
      //        b) Flag Y or N for a title or location check           *
      //        c) Indicate name of data structure used for            *
      //           electronic reporting                                *
      //        d) Flag for TextOnly if needed.                        *
      //     3) Add endorsement to FIXWTENPC                           *
      //                                                               *
      // Created   : 04/22/20                                          *
      // Programmer: C:291                                             *
      // Project   : WC2608                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 09/29/25 C:300 Project: 129WXX (CoveRisk)                     *
      //                Adding division info to Officer Endorsements   *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 07/25/24 C:333 Project: WC2978                                *
      //                WCPOLS WC040314B, WC040366, WC040602B          *
      //                - ds changed (E04302 to E04303 for forms       *
      //                  WC040303,WC040303A,WC040303B, WC040303C,     *
      //                  WC040366.                                    *
      //                                                               *
      // 04/09/24 C:300 Support: 102091 (AmWins)                       *
      //                Print TextOnly Endorsement forms in MTE document
      //                (for example - WC040304 form)                  *
      //                                                               *
      // 03/21/24 C:079 Project: WC2959                                *
      //                WC120307 - Update to use SROFFICER             *
      //                                                               *
      // 02/22/23 C:079 Project: 148WDZ                                *
      //                Add Endorsement WC990310                       *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/20/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 08/03/21 C:291 Support: 95862 (LUBA)                          *
      //                Add state as a unique value for WC000310       *
      //                so that the same name in different states      *
      //                prints to the WC000310 schedule.               *
      //                                                               *
      // 01/29/21 C:180 Support: 93990 (LUBA)                          *
      //                Monitor error when running SendToScriptura     *
      //                Changes were made in Copy Book SCOPYEN01C      *
      //                                                               *
      // 01/26/21 C:291 Support: 94020 (LUBA)                          *
      //                Do not print duplicate Name on Endorsement     *
      //                forms.                                         *
      //                                                               *
      // 01/20/21 C:136 Support: 93990 (LUBA)                          *
      //                Look for file in XMLPREP instead of XMLTEMP    *
      //                when printing from W/W Endorsements. Change    *
      //                in SCOPYEN01C and SCOPYEN01D.                  *
      //                                                               *
      //  01/13/21  C:65  Project: Phw#93965 (LUBA)                    *
      //                  Correct the officer file loop in OTHINF, it  *
      //                  was dropping out because of %EOF is not file *
      //                  specific.                                    *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*
     Fwmool     if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
      ** Additional Entity/Location File
     Fwmall     if   e           k disk
      ** Employer payroll file
     Fwmepl     if   e           k disk    rename(pep01:pep00)
      ** Payroll master file
     Fwmprl1    if   e           k disk
     Fststl3    if   e           k disk
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)

      /COPY SCOPYEN01F
      /COPY SCOPYEN01D

      /COPY WPRPEO
      //***** Beg Del *** 10/20/22 **********************************
      //****Copy WprEmplr
      //***** End Del *** 10/20/22 **********************************
      /COPY WWCPOLDAT4

     D ix              s              2  0
     D x               s              2  0
     D index           s              3  0
     D flgFirst        s              1
     D strName         s             46
      *** Beg Add ***** 08/03/21 *******************
     D strNameSt       s             50
      *** End Add ***** 08/03/21 *******************
     D strNmTl         s             60
     D intUnid         s                   like(peunid)
     D intEnd#         s                   like(peend#)
     D flgInStack      s              1
     D intMaintDt      s              7  0
     D ArrOffTOB       s              1      Dim(50)
     D strTbid         s             10a
     D strKey          s             10a
     D strds           s              1a
     D strSt           s              2a
     D intCompDate     s                   like(ELpfdt)


     D addpgm          c                   const('SRXXXXXXXX')
     d qqMod2          s              1
     d qqRRN           s              9  0

     D SROFFICER       PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15
     D                                1

     D SROFFICER       PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15
     D  qqCrtFrm                      1

     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  qqPgmName                    10    const
     d  qqFound                       1

     d ConvertDate     pr                  extPgm('SRCVTDAT')
     d srDti                          8  0
     d srFfmt                         8    const
     d srTfmt                         8    const
     d srDto                         10
     d srErr                          1

      /Free
       Dcl-S ctDataName  Char(10) Dim(40) CtData PerRcd(1);
       Dcl-S ctDataEdits Char(20) Dim(40) Alt(ctDataName);
       Dcl-S ptrSpecHandl Pointer(*PROC);
       //*** Beg Add ***** 01/26/21 *************************
       Dcl-S arrOOName   Char(50) Dim(100);
       //*** End Add ***** 01/26/21 *************************
       //*** Begin Add ***** 09/29/25 *************************
     D ii              s              4  0 inz
     D iy              s              2  0 inz
     D iz              s              4  0 inz
     D index2          s              3  0 inz
     D xxDivi          s              5  0 inz

       // 2 Dimentional Division Array
        Dcl-Ds OOarr   Qualified   Dim(100);
         OODivi   Packed(5) Dim(5000) inz;
        End-Ds;

       // Creating Multiple Array to Load WorkFile fields in LoadWorkFile
       // Title Array
        Dcl-S arrOOTitl  Char(30) Dim(100) inz;

       // Social Security # Array
        Dcl-S arrOOOss#   Char(9) Dim(100) inz;

       // Officiers Percentage Array
        Dcl-S arrOOOfpc   Packed(5:2) Dim(100) inz;

       // Type of Business code Array
        Dcl-S arrTpob     Char(1) Dim(100) inz;

       // PEO Name Array
        Dcl-S arrPEONam   Char(100) Dim(100) inz;

       // Fed Name Array
        Dcl-S arrFedNam   Char(100) Dim(100) inz;

       // Employer State Array
        Dcl-S arrJrst     Char(2) Dim(100) inz;

       // Client Name Array
        Dcl-S arrCltNam   Char(100) Dim(100) inz;

       // Category Array
        Dcl-S arrCateg    Char(1) Dim(100) inz;
       //*** End Add ***** 09/29/25 *************************

       Dcl-Ds dsEdits Qualified;
        specialHandlingProc Char(11);
        chkTitle Char(1);
        chkLocations Char(1);
        textOnly Char(1);
        dsWCPOLS Char(6);
       End-Ds;

       Dcl-Pr ProcSpecHandl Ind ExtProc(ptrSpecHandl);
       End-Pr;
      /End-Free

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr Othinf;

        If RunningSRtext = *On;
           LeaveSr;
        Endif;

        Exsr OpenWorkFile;

        ix = *Zeros;
        Clear E04302;
        Clear E00308;
        Clear E00310;
        Clear E37310;
        //*** Beg Add ***** 07/25/24 ********************************
        Clear E04303;
        //*** End Add ***** 07/25/24 ********************************
        flgFirst = *Blanks;
        x1st1 = 0;
        x1st2 = 0;
        x1st3 = 0;
        xxpo1 = 0;
        xxpo2 = 0;
        xxPo3 = 0;

        //Get PEO information for PEO group
        If FNpeof <> *blank and ldenda <> *blanks and ldend# > 0;
           dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
        Endif;

        Clear ArrOffTOB;
        For i = 1 to %len(ENotobv);
           ArrOffTOB(i) = %subst(ENotobv:i:1);
        Endfor;

        If qqmode = '4'; // Only need to run one time
           srDti = ldeedt;
           ConvertDate(srDti:*Blanks:'*CYMD':srDto:srErr);
           intCompDate = %Int(srDto);
        Else;
           intCompDate = ELpfdt;
        Endif;

        i = *Zeros;
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Dow Not %Eof(wdell1);
           Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           If %Eof;
              Leave;
           Endif;

           //***** Beg Add *** 10/20/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
              Iter;
           Endif;
           //***** End Add *** 10/20/22 ******************************

           //Skip flat cx'd policies
           If ELpfdt = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
              Iter;
           Endif;

           //Check endorsement state
           Chain (xxCo#:ELrtcd) prs01;
           Select;
           When Not(%found);
              Iter;
           When ENst = *Blanks;
           When ENst <> RSjrst;
              Iter;
           Endsl;

           //Excluded states
           //*** Beg Chg ***** 01/27/23 *****************************
           //***Chain (ELco#:qqForm:RSjrst) pes01;
           //***If %Found(wtesl) And
           //*** (ESapex = 'A' and ESst <> RSjrst
           //*** Or ESapex = 'E' and ESst = RSjrst);
           //***   Iter;
           //***Endif;
           Exsr ChkApprExclSt;
           If isApprExclSt = *Off;
              Iter;
           Endif;
           //*** End Chg ***** 01/27/23 *****************************

           //Get Client's information
           dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);

           //Check business type
           If InclExclTOB() = *Off;
              Iter;
           Endif;

           //Get Category
           strTbId = 'BUSI';
           Chain (EMco#:strTbid:EMtpob) pgt00;

           //Check if record qualifies for print and reports to WCPOLS
           Setll (ELco#:ELfnd:ELemp#:ELdiv) wmool;
           //*** begin chg ***  01/13/21  **********************************
           //*****Dou %Eof;
           Dou %Eof(wmool);
           //*** end chg *****  01/13/21  **********************************
              Reade (ELco#:ELfnd:ELemp#:ELdiv) wmool;
              If %Eof;
                 Leave;
              EndIf;

              If OOstat = 'I'
               Or OOExFl <> ENOEF
               Or OOefdt > intCompDate
               Or OOexdt <> *Zero And OOexdt <= ELpfdt;
                 Iter;
              EndIf;

              If dsEdits.specialHandlingProc <> *Blanks;
                 ptrSpecHandl = GetProcPtr(dsEdits.specialHandlingProc);
                 If ProcSpecHandl() = *Off;
                    Iter;
                 Endif;
              EndIf;

              If dsEdits.chkTitle = 'Y'
               And TitleExist() = *Off;
                 Iter;
              EndIf;

              If dsEdits.chkLocations = 'Y'
               And LocationExist() = *Off;
                 Iter;
              EndIf;

              strName = %Trim(OOofsn) + ' ' + %Trim(OOolsn);
              //*** Beg Add ***** 01/26/21 ***************************
              //*** Begin Del ***** 09/29/25 ***************************
              //*** sorta arrOOName;
              //*** End   Del ***** 09/29/25 ***************************
              //*** Beg Chg ***** 08/03/21 ***************************
              //index = %Lookup(strName:arrOOname);
              //If index = 0;                //NO Duplicate Name found
              //   ix = ix + 1;
              //   arrOOName(ix) = StrName;
              //Else;
              //   Iter;
              //Endif;
              Select;
              When qqForm = 'WC000310';
                 strNameSt = %trim(RSjrst) + ' ' + %Trim(OOofsn) + ' ' +
                  %Trim(OOolsn);
                 index = %Lookup(strNameSt:arrOOname);
                 If index = 0;                //NO Duplicate Name/State Found
                    ix = ix + 1;
                    arrOOName(ix) = StrNameSt;

                    //*** Begin Add ***** 09/29/25 ***************************
                    If cltID = '129';
                       iz = 1;
                       OOarr(ix).OODivi(iz) = OOdiv;   // Load Division Array
                       // Store Div 0 as 99999
                       If OOdiv = 0;
                          OOarr(ix).OODivi(iz) = 99999;   // Load Division Array
                       Endif;

                       arrOOTitl(ix) = OOtitl;         // Load Title Array
                       arrOOOss#(ix) = OOoss#;         // Load Social Security # Array
                       arrOOOfpc(ix) = OOofpc;         // Load Officiers Percentage Array
                       arrTpob(ix) = EMtpob;           // Load Type of Business code Array
                       arrPEONam(ix) = dsPeoInfo.Name;    //Load PEO Name Array
                       arrFedNam(ix) = dsPeoInfo.FedName; //Load Federal Name Array
                       arrJrst(ix) = RSjrst;              // Load Employer State Array
                       arrCltNam(ix) = dsCltInfo.name;    // Load Client Name Array
                       arrCateg(ix) = GTfg01;           // Load Category Array
                    Endif;
                    //*** End   Add ***** 09/29/25 ***************************
                 Else;
                    //*** Begin Add ***** 09/29/25 ***************************
                    If cltID = '129';
                       // Search Div 0 as 99999
                       If OOdiv = 0;
                          index2 = %Lookup(99999:OOarr(index).OOdivi);
                       Else;
                          index2 = %Lookup(OOdiv:OOarr(index).OOdivi);
                       Endif;

                       If index2 = 0;                //NO Duplicate Div found
                          // Find next avilable slot to load Division number
                          for ii = 1 to 5000;
                             If OOarr(index).OODivi(ii) = 0;
                                OOarr(index).OODivi(ii) = OOdiv;
                                // Store Div 0 as 99999
                                If OOdiv = 0;
                                   OOarr(ix).OODivi(ii) = 99999;   // Load Division Array
                                Endif;
                                Leave;
                             Endif;
                          endfor;
                       Endif;
                    Endif;
                    //*** End Add ***** 09/29/25 ***************************

                    Iter;
                 Endif;
              Other;
                 index = %Lookup(strName:arrOOname);
                 If index = 0;                //NO Duplicate Name found
                    ix = ix + 1;
                    arrOOName(ix) = StrName;

                    //*** Begin Add ***** 09/29/25 ***************************
                    If cltID = '129';
                       iz = 1;
                       OOarr(ix).OODivi(iz) = OOdiv;   // Load Division Array
                       // Store Div 0 as 99999
                       If OOdiv = 0;
                          OOarr(ix).OODivi(iz) = 99999;   // Load Division Array
                       Endif;

                       arrOOTitl(ix) = OOtitl;         // Load Title Array
                       arrOOOss#(ix) = OOoss#;         // Load Social Security # Array
                       arrOOOfpc(ix) = OOofpc;         // Load Officiers Percentage Array
                       arrTpob(ix) = EMtpob;           // Load Type of Business code Array
                       arrPEONam(ix) = dsPeoInfo.Name;    //Load PEO Name Array
                       arrFedNam(ix) = dsPeoInfo.FedName; //Load Federal Name Array
                       arrJrst(ix) = RSjrst;              // Load Employer State Array
                       arrCltNam(ix) = dsCltInfo.name; // Load Client Name Array
                       arrCateg(ix) = GTfg01;           // Load Category Array
                    Endif;
                    //*** End Add ***** 09/29/25 ***************************
                 Else;
                    //*** Begin Add ***** 09/29/25 ***************************
                    If cltID = '129';
                       // Search Div 0 as 99999
                       If OOdiv = 0;
                          index2 = %Lookup(99999:OOarr(index).OOdivi);
                       Else;
                          index2 = %Lookup(OOdiv:OOarr(index).OOdivi);
                       Endif;

                       If index2 = 0;                //NO Duplicate Div found
                          // Find next avilable slot to load Division number
                          for ii = 1 to 5000;
                             If OOarr(index).OODivi(ii) = 0;
                                OOarr(index).OODivi(ii) = OOdiv;
                                // Store Div 0 as 99999
                                If OOdiv = 0;
                                   OOarr(ix).OODivi(ii) = 99999;   // Load Division Array
                                Endif;
                                Leave;
                             Endif;
                          endfor;
                       Endif;
                    Endif;
                   //*** End Add ***** 09/29/25 ***************************

                    Iter;
                 Endif;
              Endsl;
              //*** End Chg ***** 08/03/21 ***************************
              //*** End Add ***** 01/26/21 ***************************

              //*** Begin Chg ***** 09/29/25 ***************************
              //*** Exsr LoadWorkFile;
              //*** Exsr WriteMultiRcd;
              If cltID <> '129';
                 Exsr LoadWorkFile;
                 Exsr WriteMultiRcd;
              Endif;
              //*** End Chg ***** 09/29/25 ***************************

              If dsEdits.dsWCPOLS <> *Blanks;
                 Exsr LoadWCPOLS;
              Endif;

           EndDo;

        Enddo;

        If i <> 0 and dsEdits.dsWCPOLS <> *Blanks;
           Exsr WriteWXPEP;
        Endif;

        //*** Begin Add ***** 09/29/25 ***************************
        If cltID = '129';
           // Loop over Name array
           iy = 1;
           Dow arrOOName(iy) <> ' ';
              strName = arrOOName(iy);        // Name Array
              OOtitl = arrOOTitl(iy);         // Title Array
              OOoss# = arrOOOss#(iy);         // Social Security # Array
              OOofpc = arrOOOfpc(iy);         // Officiers Percentage Array
              EMtpob = arrTpob(iy);           // Load Type of Business code Array
              dsPeoInfo.Name = arrPEONam(iy);             //Load PEO Name Array
              dsPeoInfo.FedName = arrFedNam(iy);          //Load Federal Name Array
              RSjrst = arrJrst(iy);                       // Load Employer State Array
              dsCltInfo.name = arrCltNam(iy);  // Load Client Name Array
              GTfg01 = arrCateg(iy);           // Load Category Array

              Exsr LoadWorkFile;
              Exsr WriteMultiRcd;

              // Loop over Division sub-array
              iz = 1;
              Dow OOarr(iy).OODivi(iz) <> 0;
                 xxDivi = OOarr(iy).OODivi(iz);
                 // Convert Div 99999 into 0
                 If xxDivi = 99999;
                    xxDivi = 0;
                 Endif;

                 Exsr LoadWorkFileDiv;
                 Exsr WriteMultiRcd;

                 // Read next Array element of Division sub array
                 iz += 1;
                 If iz = 5001;
                    Leave;
                 Endif;
              Enddo;

              // Read next Array element of OOname array
              iy += 1;
              If iy = 101;
                 Leave;
              Endif;
           Enddo;
        Endif;
        //*** End Add ***** 09/29/25 ***************************

        Exsr CloseWorkFile;

       Endsr;
       //=========================================================================
       //  LoadWorkFile - Load WorkFile fields
       //=========================================================================
       Begsr LoadWorkFile;
         FMnm01 = strName;                            //Name
         FMc301 = OOtitl;                             //Title
         FMc101 = OOoss#;                             //SSN
         FMa521 = OOofpc;                             //Office Percentage
         FMtobc = EMtpob;                             //Type of business
         FM2001 = dsPeoInfo.Name;                     //PEO Name
         FM2002 = dsPeoInfo.FedName;                  //Federal name
         FMEMST = RSjrst;                             //Employer State
         FM2003 = dsCltInfo.name;                     //Client Name
         FMFL01 = GTfg01;                             //Category
       Endsr;

       //*** Begin Add ***** 09/29/25 ***************************
       //=========================================================================
       //  LoadWorkFileDiv - Load WorkFile Division fields
       //=========================================================================
       Begsr LoadWorkFileDiv;
         FMnm01 = '     (Div ' + %editc(xxDivi:'X') + ')';  //Division
         FMc301 = OOtitl;                             //Title
         FMc101 = OOoss#;                             //SSN
         FMa521 = OOofpc;                             //Office Percentage
         FMtobc = EMtpob;                             //Type of business
         FM2001 = dsPeoInfo.Name;                     //PEO Name
         FM2002 = dsPeoInfo.FedName;                  //Federal name
         FMEMST = RSjrst;                             //Employer State
         FM2003 = dsCltInfo.name;                     //Client Name
         FMFL01 = GTfg01;                             //Category
       Endsr;
       //*** End   Add ***** 09/29/25 ***************************

       //=========================================================================
       //  LoadWCPOLS - Load WCPOLS data structure data
       //=========================================================================
       Begsr LoadWCPOLS;

        If OOtitl <> *Blanks;
           strNmTl = %Trim(OOtitl) + ' ' + %Trim(strName);
        Else;
           strNmTl = %Trim(strName);
        Endif;
        i = i +1;
        Select;
        When dsEdits.dsWCPOLS = 'E04302';
           If i = 1;
              x2Nm1 = %Trim(strNmTl);
           Endif;
           If i = 2;
              x2Nm2 = %Trim(strNmTl);
           Endif;
           If i = 3;
              x2Nm3 = %Trim(strNmTl);
           Endif;

        //*** Beg Add ***** 07/25/24 ********************************
        When dsEdits.dsWCPOLS = 'E04303';
           If i = 1;
              x3Nm1 = %Trim(strNmTl);
           Endif;
           If i = 2;
              x3Nm2 = %Trim(strNmTl);
           Endif;
           If i = 3;
              x3Nm3 = %Trim(strNmTl);
           Endif;
        //*** End Add ***** 07/25/24 ********************************

        When dsEdits.dsWCPOLS = 'E00308';
           If i = 1;
              x1Nm1 = %Trim(strNmTl);
              Select;
              When EMtpob = '2';
                 x1ds1 = 'P';
              When EMtpob = '3';
                 x1ds1 = 'O';
              Other;
                 x1ds1 = 'X';
              ENdsl;
           Endif;
           If i = 2;
              x1Nm2 = %Trim(strNmTl);
              Select;
              When EMtpob = '2';
                 x1ds2 = 'P';
              When EMtpob = '3';
                 x1ds2 = 'O';
              Other;
                 x1ds2 = 'X';
              ENdsl;
           Endif;
           If i = 3;
              x1Nm3 = %Trim(strNmTl);
              Select;
              When EMtpob = '2';
                 x1ds3 = 'P';
              When EMtpob = '3';
                 x1ds3 = 'O';
              Other;
                 x1ds3 = 'X';
              ENdsl;
           Endif;
        When dsEdits.dsWCPOLS = 'E00310';
           Select;
           When EMtpob = '1';
              strds = 'S';
           When Emtpob = '2';
              strds = 'P';
           When EMtpob = '3';
              strds = 'O';
           Other;
              strds = 'X';
           Endsl;
           Clear strSt;
           strKey = RSjrst;
           Chain strKey pst01;
           If %found(ststl3);
              strSt = stnst;
           Endif;
           If i = 1;
              xxNm1 = %Trim(strNmTl);
              xxds1 = strds;
              x1St1 = %int(strSt);
           Endif;
           If i = 2;
              xxNm2 = %Trim(strNmTl);
              xxds2 = strds;
              x1St2 = %int(strSt);
           Endif;
           If i = 3;
              xxNm3 = %Trim(strNmTl);
              xxds3 = strds;
              x1St3 = %int(strSt);
           Endif;
        When dsEdits.dsWCPOLS = 'E37310';
           If i = 1;
              x4Nm1 = %Trim(strName);
              xxOf1 = OOTitl;
              xxPo1 = OOofpc;
           Endif;
           If i = 2;
              x4Nm2 = %Trim(strName);
              xxOf2 = OOTitl;
              xxPo2 = OOofpc;
           Endif;
           If i = 3;
              x4Nm3 = %Trim(strName);
              xxOf3 = OOTitl;
              xxPo3 = OOofpc;
           Endif;
        Endsl;

        If i = 3;
           Exsr WriteWXpep;
        Endif;

       Endsr;

       //=========================================================================
       //  WriteWXPEP - Write WXPEP record
       //=========================================================================
       Begsr WriteWXPEP;

         ChkStack('W1054R':flgInStack);
         If flgInStack = '0';
            intUnid = *Zeros;
            intMaintDt = 9999999;
            Setgt (xxCo#:xxFnd:xxEmp#:xxFyr:intMaintDt) wxpel1;
            Readpe (xxCo#:xxFnd:xxEmp#:xxFyr) wxpel1;

            If Not %EOF(wxpel1);
               intUnid = PEunid;
            Endif;
            intEnd# = xxForm;
            Chain (xxCo#:xxFnd:xxEmp#:xxFyr:intUnid:intEnd#) wxpel;

            If %found(wxpel);
               Clear PEdata;
               Select;
               When dsEdits.dsWCPOLS = 'E04302';
                  PEdata = E04302;
               //*** Beg Add ***** 07/25/24 *************************
               When dsEdits.dsWCPOLS = 'E04303';
                  PEdata = E04303;
               //*** End Add ***** 07/25/24 *************************
               When dsEdits.dsWCPOLS = 'E00308';
                  PEdata = E00308;
               When dsEdits.dsWCPOLS = 'E00310';
                  PEdata = E00310;
               When dsEdits.dsWCPOLS = 'E37310';
                  PEdata = E37310;
               Endsl;
               PEuser = q1User;
               PEpgm  = q1Pgm;
               PEtime = dsTime;
               PEdt = %Int(srDto);
               If flgFirst = *blank;
                  Update ppe01;
                  flgFirst = 'y';
               Else;
                  PEiusr =  q1User;
                  PEipgm =  q1Pgm;
                  PEitim =  q1Time;
                  PEidt = %Int(srDto);
                  Write ppe01;
               EndIf;
               i = 0;
               Clear E04302;
               Clear E00308;
               Clear E00310;
               Clear E37310;
               //*** Beg Add ***** 07/25/24 *************************
               Clear E04303;
               //*** End Add ***** 07/25/24 *************************
               x1st1 = 0;
               x1st2 = 0;
               x1st3 = 0;
               xxpo1 = 0;
               xxpo2 = 0;
               xxPo3 = 0;
            Endif;              //%found(wxpel)
         Endif;                 //flgInStack = '0'

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
        RunningSRtext = *Off;
        WriteInCopybook = *Off;
        index = %LookUp(qqForm:ctDataName);
        If index = 0;
           Return;
        Else;
           dsEdits = ctDataEdits(index);
        Endif;
        If dsEdits.textOnly = 'Y';
           RunningSRtext = *On;
           //*** Beg Add ***** 04/09/24 ***************************
           WriteInCopybook = *On;
           //*** End Add ***** 04/09/24 ***************************
        Endif;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr Pgmend;
        If RunningSRtext = *On;
           LeaveSr;
        Endif;
        Chain inXML XMLTEML1;
        If %found(XMLTEML1);
           XTtype = 'Officer Endorsement';
           Update XT100;
        Endif;
       Endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;

        crtFrm = *Off;
        Clear ArrOffTOB;
        For i = 1 to %len(ENotobv);
           ArrOffTOB(i) = %subst(ENotobv:i:1);
        Endfor;
        If InclExclTOB() = *Off;
           LeaveSr;
        EndIf;

        If qqmode = '4';
           srDti = ldeedt;
           ConvertDate(srDti:*Blanks:'*CYMD':srDto:srErr);
           intCompDate = %Int(srDto);
        Else;
           intCompDate = Elpfdt;
        Endif;

        Setll (ELco#:ELfnd:ELemp#:ELdiv) wmool;
        //*** begin chg ***  01/13/21  **********************************
        //*****Dou %Eof;
        Dou %Eof(wmool);
        //*** end chg *****  01/13/21  **********************************
           Reade (ELco#:ELfnd:ELemp#:ELdiv) wmool;
           If %Eof;
              Leave;
           EndIf;

           If OOstat = 'I'
            Or OOExFl <> ENOEF
            Or OOefdt > intCompDate
            Or OOexdt <> *Zero And OOexdt <= ELpfdt;
              Iter;
           EndIf;

           If dsEdits.specialHandlingProc <> *Blanks;
              ptrSpecHandl = GetProcPtr(dsEdits.specialHandlingProc);
              If ProcSpecHandl() = *Off;
                 Iter;
              Endif;
           EndIf;

           If dsEdits.chkTitle = 'Y'
            And TitleExist() = *Off;
              Iter;
           EndIf;

           If dsEdits.chkLocations = 'Y'
            And LocationExist() =*Off;
              Iter;
           EndIf;

           crtFrm = *On;

           If CrtFrm = *on;
              Leave;
           Endif;

        Enddo;

       Endsr;

       //=========================================================================
       // InclExclTOB - Include/Exclude Type of Business based on table setup
       //=========================================================================
       Dcl-Proc InclExclTOB;
       Dcl-Pi *N Ind;
       End-Pi;

        Chain (ELco#:ELfnd:ELemp#:ELdiv) wmeml;
        Select;
        When (ENotobf = 'I') and ENotobv = *Blank;
           Return *on;
        When (ENotobf = 'E') and ENotobv = *Blank;
           Return *off;
        When ENotobf = 'I' and ENotobv <> *Blank;
           If %Lookup(EMtpob:ArrOffTOB) <> 0;
              Return *on;
           Else;
              Return *off;
           Endif;
        When ENotobf = 'E' and ENotobv <> *Blank;
           If %Lookup(EMtpob:ArrOffTOB) <> 0;
              Return *off;
           Else;
              Return *on;
           Endif;
        Other;
           Return *off;
        Endsl;

       End-Proc;
       //=========================================================================
       // LocationExist - Additional Entity/Location Exist
       //=========================================================================
       Dcl-Proc LocationExist;
       Dcl-Pi *N Ind;
       End-Pi;
       Dcl-S CreateForm IND inz(*off);

        If OOelid <> *blanks and OOelid <> *zeros;
           Chain (OOco#:OOfnd:OOemp#:OOdiv:OOelid) pal01;
           If ALdldt > intCompDate or ALdldt = *zeros;
              CreateForm = *on;
           Endif;
        Else;
           CreateForm = *on;
        Endif;

        If CreateForm = *on;
           Return *on;
        Else;
           Return *off;
        Endif;
       End-Proc;

       //=========================================================================
       // TitleExist - Owner/Officer Title is Required to attach the form
       //=========================================================================
       Dcl-Proc TitleExist;
       Dcl-Pi *N Ind;
       End-Pi;
       Dcl-S CreateForm IND inz(*off);
        If OOtitl <> *blanks;
           CreateForm = *on;
        Else;
           CreateForm = *off;
        Endif;

        If CreateForm = *on;
           Return *on;
        Else;
           Return *off;
        Endif;
       End-Proc;

       //=========================================================================
       // PRWC000308 - Special handling procedure for form WC000308
       //=========================================================================
       Dcl-Proc PRWC000308;
       Dcl-Pi *N Ind;
       End-Pi;
       Dcl-S CreateForm IND inz(*off);

        //Attach if Florida and Contractor's credit = 0 and Contractor Payroll Exist
        If RSjrst = 'FL' and ELasp1 = *zero;
           Setll (ELCo#:ELfnd:ELemp#:ELdiv:ELfyr) pep00;
           Reade (ELCo#:ELfnd:ELemp#:ELdiv:ELfyr) pep00;
           Dow not %Eof(wmepl);
              If EPstat <> 'I';
                 Setgt (ELCo#:ELrtcd:EPclas:ELpfdt) ppr01;
                 Readpe (ELCo#:ELrtcd:EPclas) ppr01;
                 If not %Eof(wmprl1) and PRccfl = 'Y';
                    CreateForm = *on;
                    Leave;
                 Endif;
              Endif;
              Reade (ELCo#:ELfnd:ELemp#:ELdiv:ELfyr) pep00;
           Enddo;
        Endif;

        //Attach if NOT Florida or Type of Business <> "1"
        If RSjrst <> 'FL' or EMtpob <> '1';
           If LocationExist();
              CreateForm = *on;
           Else;
              CreateForm = *off;
           Endif;
        Endif;

        If CreateForm = *on;
           Return *on;
        Else;
           Return *off;
        Endif;
       End-Proc;

       //=========================================================================
       // PRWC000310 - Special handling procedure for form WC000310
       //=========================================================================
       Dcl-Proc PRWC000310;
       Dcl-Pi *N Ind;
       End-Pi;
        Select;
        When RSjrst = 'NJ' and (EMtpob = '1' or EMtpob = '2');
            Return *off;
        When RSjrst = 'MA' and EMtpob <> '1' and EMtpob <> '2'
         and EMtpob <> '6' and EMtpob <> 'A'
         and EMtpob <> 'D' and EMtpob <> 'F';
           Return *off;
        Other;
           Return *on;
        EndSl;
       End-Proc;

       //=========================================================================
       // GetProcPtr - Retrieve procedure pointer
       //=========================================================================
       Dcl-Proc GetProcPtr;
       Dcl-Pi *N Pointer(*PROC);
        inProc char(11);
       End-Pi;
        Dcl-S ptrProc Pointer(*PROC) Inz;
        Select;
        When inProc = 'PRWC000308';
            ptrProc = %paddr(PRWC000308);
        When inProc = 'PRWC000310';
            ptrProc = %paddr(PRWC000310);
        EndSl;
        Return ptrProc;
       End-Proc;
** ctDataName/ctDataEdits
WC000308  PRWC000308 NNNE00308
WC000310  PRWC000310 NYNE00310
WC040302A            NNNE04302
WC040302B            NNNE04302
WC040302C            NNNE04302
WC040302D            NNNE04302
WC040303             NNNE04303                                                             *07/25/24
WC040303A            NNNE04303                                                             *07/25/24
WC040303B            NNNE04303                                                             *07/25/24
WC040303C            NNNE04303                                                             *07/25/24
WC040304             NNY
WC040318             NNNE04302
WC040318A            NNNE04302
WC040318B            NNNE04302
WC040318C            NNNE04302
WC040366             NNNE04303                                                             *07/25/24
WC070301             NYN
WC070302             NNN
WC120307             NNN
WC290307             NYN
WC310304             YNN
WC310306A            YNN
WC310313A            NYN
WC310313B            NYN
WC310313C            NYN
WC310316             NYN
WC310316A            NYN
WC310316B            NYN
WC310316C            NYN
WC370310B            YNNE37310
WC370310C            NNNE37310
WC420308             NYN
WC420310             NYN
WC420313             NNN
WC420314             NNN
WC990310             NNN




