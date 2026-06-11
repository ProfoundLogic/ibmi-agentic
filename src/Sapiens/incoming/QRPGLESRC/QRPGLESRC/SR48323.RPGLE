     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR48323 - WI Designated Named Insured Cancellation
      *===============================================================*
      * SR48323 - Designated Named Insured Cancellation Endorsement   *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXWXFMP) prior to a form merge.                   *
      *                                                               *
      * Created   : 03/07/23                                          *
      * Programmer: C:291                                             *
      * Project   : WC2896                                            *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 09/21/23 C:344 Project: WC2923                                *
      *                Add processing to populate the named insured   *
      *                CX information into the WXPEP record for each  *
      *                named insured record listed/printed.           *
      *                Populate the EJ record type data into the      *
      *                PEDATA field in WXPEP record.                  *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
       Dcl-Pi *N;
        qqCo#  char(3);
        qqFnd  char(3);
        qqEmp# char(9);
        qqDiv  char(5);
        qqFyr  char(3);
        qqMode char(1);
        qqRrn  packed(9:0);
        qqMod2 char(1);
       End-Pi;

       Dcl-F Wmanl  disk usage(*input) keyed Rename(pan01:pan00);
       Dcl-F Wxnal  disk usage(*input) keyed;
       Dcl-F Wmshl1 disk usage(*input) keyed;
       //*** Begin Add *** 09/21/23 *** WC2923 ******************************
       Dcl-F Wxpel  disk usage(*update:*output) keyed;
       Dcl-F Wxpel1 disk usage(*input) keyed Rename(ppe01:ppe02);
       //*** End   Add *** 09/21/23 *** WC2923 ******************************
      /Copy sCopyEn01f
      /Copy sCopyEn01d
       //*** Begin Add *** 09/21/23 *** WC2923 ****************************
     D/Copy wPrPeo
     D/COPY SPRSTRING
     D/Copy WWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
       //*** End   Add *** 09/21/23 *** WC2923 ****************************

      // Additional entry program
       Dcl-c AddPgm const('SR48323I  ');
      // Form ID
       Dcl-s qqForm char(15) inz('WC480323  ');

       Dcl-s xxType char(1) inz;
       Dcl-s strPrName char(400) inz;
       Dcl-s strPrAddr1 char(40) inz;
       Dcl-s strPrAddr2 char(40) inz;
       Dcl-s strPrCtyStZip char(45) inz;
       //*** Begin Add *** 09/21/23 *** WC2923 ****************************
       Dcl-s xxDt zoned(7:0) inz;
       Dcl-s intLen zoned(2:0) inz;
       //*** End   Add *** 09/21/23 *** WC2923 ****************************

      // Copybook C specs
      /Copy sCopyEn01C

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr othinf;

        WriteInCopybook = *Off;
        //Find Primary Named Insured
        xxType = 'P';
        Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxType) WMSHL1;
        If %Found(WMSHL1);
           Setll (xxCo#:xxFnd:xxEmp#:xxFyr:SHunid) WXNAL;
           Reade (xxCo#:xxFnd:xxEmp#:xxFyr:SHunid) WXNAL;
           Dow not %Eof(WXNAL);
              If NAencd = 1;
                 strPrName = NAname;
                 Select;
                 When NAnam4 <> *Blank;
                    strPrName = %Trim(strPrName) + %Trim(Nanam4);
                 When NAnam2 <> *Blank;
                    strPrName = %Trim(strPrName) + %Trim(Nanam2);
                 Endsl;
                 strPrAddr1 = NAmad1;
                 strPrAddr2 = NAmad2;
                 strPrCtyStZip = %Trim(NAmCty) + ', ' + NAmSt + ' ' +
                  %Subst(%Editc(NAmZip:'X'):1:5);
                 If %Subst(%Editc(NAmZip:'X'):6:4) <> '0000';
                    strPrCtyStZip = strPrCtyStZip + '-' +
                     %Subst(%Editc(NAmZip:'X'):6:4);
                 Endif;
                 Leave;
              Endif;
              Reade (xxCo#:xxFnd:xxEmp#:xxFyr:SHunid) WXNAL;
           Enddo;
        Endif;

        //*** Begin Add *** 09/21/23 *** WC2923 **********************
        i = 1;
        //*** End   Add *** 09/21/23 *** WC2923 **********************
        Setll (xxCo#:xxFnd:xxEmp#) pan00;
        Dow Not %Eof(wmanl);
           Reade (xxCo#:xxFnd:xxEmp#) pan00;
           If %Eof;
              Leave;
           Endif;
           If ANcxFyr <> xxFyr;
              Iter;
           Endif;
           Chain (xxCo#:xxFnd:xxEmp#:ANDiv:xxFyr) pel01;
           Chain (Elco#:ELrtcd) prs01;
           Select;
           When Not %Found(WMRSL);
              Iter;
           When RSjrst <> ENST;
              Iter;
           Endsl;
           If ANdldt <> 0 and ANcxRsn <> *Blanks;
              Chain (AnCo#:ANFnd:ANEmp#:ANdiv) WMEML;
              FMcd01 =  ANcxRsn;                                      //Cancellation Reason code
              FMDt01 =  %dec(%char(%date(ANdldt:*cymd):*usa0):8:0);   //Cancellation date
              FM2001 = %Trim(ANname) + %Trim(ANnam2);
              Select;
              When EMpad1 <> *Blanks;
                 FMc401 = EMpad1;
                 FMc402 = EMpad2;
                 FMc403 = %Trim(EMpCty) + ', ' + EMpSt + ' ' +
                  %Subst(%Editc(EMpZip:'X'):1:5);
                 If %Subst(%Editc(EMpZip:'X'):6:4) <> '0000';
                    FMc403 = FMc403 + '-' + %Subst(%Editc(EMpZip:'X'):6:4);
                 Endif;
              Other;
                 FMc401 = EMmad1;
                 FMc402 = EMmad2;
                 FMc403 = %Trim(EMmCty) + ', ' + EMmSt + ' ' +
                  %Subst(%Editc(EMmZip:'X'):1:5);
                 If %Subst(%Editc(EMmZip:'X'):6:4) <> '0000';
                    FMc403 = FMc403 + '-' + %Subst(%Editc(EMmZip:'X'):6:4);
                 Endif;
              Endsl;
              FM2002 = strPrName;
              FMc404 = strPrAddr1;
              FMc405 = strPrAddr2;
              FMc406 = strPrCtyStZip;
           Endif;
           // Write the output record in WXFMP.
           Exsr WriteWorkFile;
           //*** Begin Add *** 09/21/23 *** WC2923 **********************
           //* Get latest UNID
           xxDT = *hival;
           Setgt (xxco#:xxfnd:xxemp#:xxfyr:xxdt) ppe02;
           Readpe(n) (xxco#:xxfnd:xxemp#:xxfyr) ppe02;
           If not %eof;
              xxUnId = PEunid;
              x1End# = %trim(qqform);
              //Copy data into record
              Chain (xxco#:xxfnd:xxemp#:xxfyr:xxunid:x1end#) ppe01;
              If %found;
                 Clear e48323;

                 // Name of Insured
                 xjCName = %trim(ExtrAlphaNum(FM2001));

                 // Address of Insured
                 intLen = %len(%trim(FMc401) + ' ' + %trim(FMc403) + ' ');
                 If FMc402 <> ' ';
                    If intLen < %Size(xjCAdr);
                       xjCAdr = %Trim(FMc401) + ' ' +
                       %Trim(%Subst (FMc402:1:%Size(xjCAdr) - intLen)) +
                       ' ' + %Trim(FMc403);
                    Else;
                       xjCAdr = %Trim(FMc401) + ' ' + %Trim(FMc403);
                    Endif;
                 Else;
                    xjCAdr = %Trim(FMc401) + ' ' + %Trim(FMc403);
                 Endif;

                 // Cancellation Code
                 xjCCnlc = %dec(FMcd01:2:0);

                 // Cancellation Effective Date
                 xjCCnld = FMdt01;

                 PEData = %trim(e48323);

                 PEName = %trim(ExtrAlphaNum(PEName));
                 PEuser = q1User;
                 PEpgm = q1Pgm;
                 PEtime = %Dec(%Char(%Time():*iso0):6:0);
                 PEdt = %Dec(%Char(%Date():*cymd0):7:0);

                 If i = 1;
                    Update ppe01;
                    i = i + 1;
                 Else;
                    PEiusr = q1User;
                    PEipgm = q1Pgm;
                    PEitim = %Dec(%Char(%Time():*iso0):6:0);
                    PEidt = %Dec(%Char(%Date():*cymd0):7:0);
                    Write ppe01; //WXPEP
                 Endif;
              Endif;
           Endif;
           //*** End   Add *** 09/21/23 *** WC2923 *****************
        EndDo;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
        RunningSRtext = *Off;
        WriteInCopybook = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr Pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
        crtFrm = *On;
       Endsr;
