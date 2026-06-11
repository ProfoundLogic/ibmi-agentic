     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H BNDDIR('WTBNDDIR')
      /TITLE SRALTEMP - Alternate Employer ONLY Endorsement Print/Attach
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXP6P) prior to a form merge.                     *
      //                                                               *
      // Created   : 10/15/18                                          *
      // Programmer: C:291                                             *
      // Project   : WC2549                                            *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 11/27/25 C:333 Support: 120156 (SCS)                          *
      //                WCPOLS File Issues.                            *
      //                - New line (LF/CR) from the screen field is    *
      //                  making WCPOLS Extract data to break the line *
      //                  abruptly. Putting fix to replace LF/CR with  *
      //                  space before evaluating xxcop (contract and  *
      //                  Project).                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 11/03/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 10/25/22 C:079 Project: WC2856                                *
      //                Alt Employer Endorse-Single/Multi option       *
      //                                                               *
      // 01/29/21 C:180 Support: 93990 (LUBA)                          *
      //                Monitor error when running SendToScriptura     *
      //                Changes were made in Copy Book SCOPYEN01C      *
      //                                                               *
      // 10/28/20 C:079 Project: WC2671                                *
      //                Audit Request - Change to a TMPLRQST call.     *
      //                This was done in the copybook.                 *
      //                                                               *
      // 04/02/20 C:136 Support: 88384                                 *
      //                Increase the size of the arrNmFeinAddr array.  *
      //                                                               *
      // 01/23/20 C:136 Support: 77734 (Not commented)                 *
      //                Use WriteMultiRcd to write multiple addresses  *
      //                on schedule.                                   *
      //                                                               *
      // 06/25/19 C:180 Support: 78400                                 *
      //                Write wcpols record and populate 'state' for   *
      //                Auto endorsement mode to be consistent with    *
      //                manual endorsement mode.                       *
      //                                                               *
      // 02/19/19 C:291 Support: WC2549                                *
      //                Additional changes to Alternate Employer       *
      //                Endorsement.                                   *
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
      //* Copybook F specs
     F/COPY SCOPYEN01F

     Fwdael4    If   e           k disk    rename(pae01:pae04)
     Fwxpel     uf a e           k disk
     Fwxpel1    If   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    If   e           k disk
     Fwdrrl     If   e           k disk

      /copy wprpeo
      //***** Beg Del *** 11/03/22 **********************************
      ***/copy wpremplr
      //***** End Del *** 11/03/22 **********************************
      /copy wwcpoldat4
     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)

     D  index          s              5  0
     D  arrNmFeinAddr  s            409    Dim(999) Inz

     D StrNmFeinAddr   DS
     D  StrName                1    100
     D  StrFein              101    109
     D  StrAddr1             110    209
     D  StrAddr2             210    309
     D  StrAddr3             310    409

      * Standalone fields
     D k3emp#          s                   like(EMemp#)
     D k3div           s                   like(EMdiv)
     D xxefdt          s                   like(fmeedt)
     D xxunid          s                   like(PEunid)
     D x1end#          s                   like(PEend#)
     D svfein          s                   like(EMfein)
     D sv1emp#         s                   like(EMemp#)
     D sv1div          s                   like(EMdiv)
     D qqRrn           s              9  0
     D qqMod2          s              1
     D xxuniq          s              1
     D xxactive        s              1
     D x1              s              3  0
     D x2efdt          s                   like(emefd1)
     D x2cxdt          s                   like(emcad1)
     D x2rndt          s                   like(emcad1)
     D #1pgm           s             10a   INZ('W1054R')
     D #1flag          s              1a
     D xxdt            s              7  0
     D x1Type          s              1
     D xxdate          s              7  0 inz
      *** Beg Add ***** 11/27/25 *************************
     D cleanValue      s                   like(AEproj)
     D pos             s              3  0
      *** End Add ***** 11/27/25 *************************

      //* Copybook D specs
     D/COPY SCOPYEN01D

      //* Additional entry program
     D addpgm          c                   const('SRALTEMPI ')

     D SRALTEMP        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15

     D SRALTEMP        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15

     d* Check Pgm
     d SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     d                               10
     d                                1

      ** Alternate Employer file
     C     aeKey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxfyr
     C                   kfld                    xxform

     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0

     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr

     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#

      //* Copybook C specs
     C/COPY SCOPYEN01C

      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr othinf;

        //*** Beg Add *** 10/25/22 **********************************************
        If ENsngl = 'S';
        //*** End Add *** 10/25/22 **********************************************
           Exsr OpenWorkFile;
        //*** Beg Add *** 10/25/22 **********************************************
        Endif;
        //*** End Add *** 10/25/22 **********************************************
        //---------------------------------------------------------------
        //Manual Attach: Write the output record for Manual attached form
        //---------------------------------------------------------------
        i = 1;
        If ENMNAU = 'M';
           Setll aekey pae04;
           Reade aekey pae04;
           Dow not %eof(wdael4);
              If AEefdt = *zeros;
                 xxEfDt = %dec(%char(%date(ELpfdt:*cymd):*usa0):8:0);
              Else;
                 xxEfDt = %dec(%char(%date(AEefdt:*cymd):*usa0):8:0);
              Endif;
              FMnm01 = AENam1;
              FMc401 = AEAdr1;
              FMc402 = AEAdr2;
              Clear FMc403;
              FMc403 = %trim(AECty) + ', ' + %trim(AESt) + ' ' +
               %subst(%editc(AEZip:'X'):1:5);
              If %subst(%editc(AEZip:'X'):6:4) <> '0000';
                 FMc403 = %trim(FMc403) + '-' + %subst(%editc(AEZip:'X'):6:4);
              Endif;
              State = AEEmst;
              FMBLK01 = AEProj;
              //*** Beg Add *** 10/25/22 **************************************
              Select;
              When ENsngl = 'S';
              //*** End Add *** 10/25/22 **************************************
                 Exsr WriteMultiRcd;
              //*** Beg Add *** 10/25/22 **************************************
              When ENsngl = 'M';
                 Exsr OpenWorkFile;
                 Exsr WriteMultiRcd;
                 Exsr CloseWorkFile;
              Endsl;
              //*** End Add *** 10/25/22 **************************************
             //Report the Information to WCPOLS: Write/Update WXPEP
              SrChkPgm(#1pgm:#1flag);
              If #1flag = '0';
                 //Update the endorsement reporting file with the proper data.
                 //Find the unique ID number.
                 x1Type = 'P';
                 xxDt = *hival;
                 xxUnid = *zeros;
                 Setgt pekey ppe02;
                 Readpe pekey1 ppe02;
                 If not %Eof(wxpel1);
                    xxUnid = PEUnid;
                 Endif;
                 x1End# = xxForm;
                 Chain pekey2 ppe01;
                 If %found(wxpel);
                    Clear E00301;
                    xxaen = FMnm01;
                    xxaea = %subst(AEadr1:1:20);
                    xxaea = %trim(xxaea) + ' ' + %subst(AECty:1:15);
                    xxaea = %trim(xxaea) + ' ' +
                     %trim(AESt) + ' ' + %subst(%editc(AEZip:'X'):1:5);
                    If %subst(%editc(AEZip:'X'):6:4) <> '0000';
                       xxaea = %trim(xxaea) + '-' +
                        %subst(%editc(AEZip:'X'):6:4);
                    Endif;
                    xxsse = AEEmSt;
                    //*** Beg Chg ***** 11/27/25 ***************************
                    //***xxcop = AEProj;
                    cleanValue = AEProj;

                    // Replace LF with a space, Hex value x'25'
                    Dow %scan(x'25': cleanValue) > 0;
                       pos = %scan(x'25': cleanValue);
                       cleanValue = %replace(' ': cleanValue: pos: 1);
                    Enddo;

                    // Replace CR with a space, Hex value x'0D'
                    Dow %scan(x'0D': cleanValue) > 0;
                       pos = %scan(x'0D': cleanValue);
                       cleanValue = %replace(' ': cleanValue: pos: 1);
                    Enddo;

                    xxcop = %trim(cleanValue);
                    //*** End Chg ***** 11/27/25 ***************************
                    Clear PEdata;
                    PEData = E00301;
                    PEuser = q1user;
                    PEpgm  = q1pgm;
                    PEtime = %Dec(%Char(%Time():*iso0):6:0);
                    PEdt   = %Dec(%Char(%Date():*cymd0):7:0);
                    If i = 1;
                       Update ppe01;
                       i = i + 1;
                    Else;
                       PEiusr = q1user;
                       PEipgm = q1pgm;
                       PEitim = %Dec(%Char(%Time():*iso0):6:0);
                       PEidt  = %Dec(%Char(%Date():*cymd0):7:0);
                       Write ppe01;
                    Endif;
                 Endif;
                 Clear FMnm01;
                 Clear FMc401;
                 Clear FMc402;
                 Clear FMc403;
                 Clear FMBlk01;
                 Clear FMfl01;
                 Clear FMfl02;
              Endif;
              Reade aekey pae04;
           Enddo;
        Endif;                //MUMNAU = 'M'
        //-------------------------------------------------------------
        //Auto Attach: Write the output record For Auto Attached form
        //-------------------------------------------------------------
        Clear arrNmFeinAddr;
        If ENMNAU = 'A';
           Setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           Dow Not %Eof(WDELL1);
              Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
              If %Eof;
                 Leave;
              Endif;
           //***** Beg Add *** 11/03/22 ******************************
              xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
              If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
                 Iter;
              Endif;
           //***** End Add *** 11/03/22 ******************************
              If ELpfdt = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                 Iter;
              Endif;
              Chain (Elco#:ELrtcd) prs01;
              Select;
              When Not(%Found);
                 Iter;
              When ENst = *Blanks;
              When Enst <> RSjrst;
                 Iter;
              Endsl;
              //*** Beg Chg ***** 01/27/23 *****************************
              //***Chain (ELco#:qqForm:RSjrst) pes01;
              //***If Not(%Found) or
              //*** (ESapex = 'A' and ESst = RSjrst) or
              //*** (ESapex = 'E' and ESst <> RSjrst);
              Exsr ChkApprExclSt;
              If isApprExclSt = *On;
              //*** End Chg ***** 01/27/23 *****************************
                 dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                 StrName  = dsCltInfo.name;
                 StrFein  = dsCltInfo.fein;
                 StrAddr1 = dsCltInfo.address1;
                 StrAddr2 = dsCltInfo.address2;
                 StrAddr3 = dsCltInfo.address3;
                 index = %lookup(StrNmFeinAddr:arrNmFeinAddr);
                 If index = 0; //Not Found Client Name/Fein/Address
                    index = %lookup(' ':arrNmFeinAddr);
                    arrNmFeinAddr(index) = strNmFeinAddr;
                    //Write work file record here
                    FMnm01 = dsCltInfo.name;
                    FMc401 = dsCltInfo.address1;
                    FMc402 = dsCltInfo.address2;
                    FMc403 = dsCltInfo.address3;
                    state = dsCltInfo.State;
                    Clear FMBlk01;
                    FMBlk01 = 'Any and All';
                    //*** Beg Add *** 10/25/22 **************************************
                    Select;
                    When ENsngl = 'S';
                    //*** End Add *** 10/25/22 **************************************
                       Exsr WriteMultiRcd;
                   //*** Beg Add *** 10/25/22 **************************************
                   When ENsngl = 'M';
                      Exsr OpenWorkFile;
                      Exsr WriteMultiRcd;
                      Exsr CloseWorkFile;
                   Endsl;
                   //*** End Add *** 10/25/22 **************************************
                   // Report the Information to WCPOLS: Write/Update WXPEP
                   // if not printing from W/W Endorsements
                    SrChkPgm(#1pgm:#1flag);
                    If #1flag = '0';
                    //Update the endorsement reporting file with the proper data.
                    //Find the unique ID number.
                       x1Type = 'P';
                       xxDt = *hival;
                       xxUnid = *zeros;
                       Setgt pekey ppe02;
                       Readpe pekey1 ppe02;
                       If not %Eof(wxpel1);
                          xxUnid = PEUnid;
                       Endif;
                       x1End# = xxForm;
                       Chain pekey2 ppe01;
                       If %found(wxpel);
                          Clear E00301;
                          xxaen = FMnm01;
                          If FMc403 = *blanks;
                             xxaea = %trim(%subst(FMc401:1:20)) + ' ' +
                                     %trim(FMc402);
                          Else;
                             xxaea = %trim(%subst(FMc401:1:20)) + ' ' +
                                     %trim(%subst(FMc402:1:10)) + ' ' +
                                      %trim(FMc403);
                          Endif;

                          xxsse = dsCltInfo.State;
                          xxcop = FMblk01;

                          Clear PEdata;
                          PEData = E00301;
                          PEuser = q1user;
                          PEpgm  = q1pgm;
                          PEtime = %Dec(%Char(%Time():*iso0):6:0);
                          PEdt   = %Dec(%Char(%Date():*cymd0):7:0);
                          If i = 1;
                             Update ppe01;
                             i = i + 1;
                          Else;
                            PEiusr = q1user;
                            PEipgm = q1pgm;
                            PEitim = %Dec(%Char(%Time():*iso0):6:0);
                            PEidt  = %Dec(%Char(%Date():*cymd0):7:0);
                            Write ppe01;
                          Endif;
                       Endif;

                    Endif;

                    Clear FMnm01;
                    Clear FMc401;
                    Clear FMc402;
                    Clear FMc403;
                    Clear FMBlk01;
                    Clear FMfl01;
                    Clear FMfl02;
                 Endif;
              Endif;
           Enddo;
        Endif;             //ENMNAU = 'A'

        //*** Beg Add *** 10/25/22 **********************************************
        If ENsngl = 'S';
        //*** End Add *** 10/25/22 **********************************************
           Exsr CloseWorkFile;
        //*** Beg Add *** 10/25/22 **********************************************
        Endif;
        //*** End Add *** 10/25/22 **********************************************

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
        RunningSRtext = *Off;
        WriteInCopybook = *Off;
        //*** Beg Add *** 10/25/22 ********************************************
        scripturaType = 'Alternate Employer Endorsement';
        //*** End Add *** 10/25/22 ********************************************
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr Pgmend;
        //*** Beg Del *** 10/25/22 *********************************************
        //***If printingPolicy;
        //***   Chain inXML XMLTEML1;
        //***   If %Found(XMLTEML1);
        //***      XTtype = 'Alternate Employer Endorsement';
        //***      Update XT100;
        //***   Endif;
        //***Else;
        //***   Chain inXML XMLPREL1;
        //***   If %Found(XMLPREL1);
        //***      XPtype = 'Alternate Employer Endorsement';
        //***      Update XP100;
        //***   Endif;
        //***Endif;
        //*** End Del *** 10/25/22 *********************************************
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
        crtFrm = *On;
       Endsr;
      /End-Free
