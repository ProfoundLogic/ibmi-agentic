     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H BNDDIR('WTBNDDIR')
      /TITLE SRALTEMP - PEO Client ONLY Endorsement Print/Attach
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXP6P) prior to a form merge.                     *
      //                                                               *
      // Created   : 02/27/20                                          *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 07/25/24 C:333 Project: WC2978                                *
      //                WCPOLS WC040314B, WC040366, WC040602B.         *
      //                Code WC040314B to produce one form per client  *
      //                rather than multiple records to show up on     *
      //                one form.  Add electronic reporting for        *
      //                WC040314B.                                     *
      //                                                               *
      // 06/04/24 C:300 Support: 115141 (AmWINS)                       *
      //                Changes in SR480314 to include Cancel Divisions*
      //                Also fix another error 'Length or start position
      //                is out of range'                               *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/28/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/06/22 C:079 Project: WC2870                                *
      //                Special condition for form WC290311.           *
      //                                                               *
      // 08/18/21 C:136 Support: 96952                                 *
      //                Add PEO address                                *
      //                                                               *
      // 07/28/21 C:136 Project: WC2808                                *
      //                Add special processing for WC480318.           *
      //                                                               *
      // 01/29/21 C:180 Support: 93990 (LUBA)                          *
      //                Monitor error when running SendToScriptura     *
      //                Changes were made in Copy Book SCOPYEN01C      *
      //                                                               *
      // 01/18/21 C:136 WC2758                                         *
      //                Implement logic for WC320306 (borrowed from    *
      //                SR32306)                                       *
      //                                                               *
      // 11/18/20 C:180 Project WC2757                                 *
      //                Recompiled only.                               *
      //                                                               *
      // 10/26/20 Project: WC2752                                      *
      //          Add endorsement WC480314                             *
      //                                                               *
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
      //*** Begin Add *** 10/26/20 ************************************
     Fwmshl1    if   e           k disk
     Fwxnal     if   e           k disk    rename(pna01:pna00)
     Fwxpel     uf a e           k disk
     Fwxpel1    If   e           k disk
     F                                     rename(ppe01:ppe02)
     Fwmeml19   if   e           k disk    rename(PEM01:PEM19)
      //*** End   Add *** 10/26/20 ************************************
      //*** Beg Add ***** 01/18/21 ************************************
     fwdrrl     if   e           k disk    rename(prr01:prr00)
      //*** End Add ***** 01/18/21 ************************************

      /copy wprpeo
      //***** Beg Del *** 10/28/22 **********************************
     D*/Copy WprEmplr
      //***** End Del *** 10/28/22 **********************************

      //*** Begin Add *** 10/26/20 ************************************
      /copy sprstring
      /copy wwcpoldat4
      //*** End   Add *** 10/26/20 ************************************

      //* Copybook D specs
     D/COPY SCOPYEN01D

      //* Additional entry program
     D addpgm          c                   const('XXXXXXXXXX')

     D SRPEOCLT        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15

     D SRPEOCLT        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15

     D qqRrn           s              9  0
     D qqMod2          s              1
     D x1anpr          s             11  0
      //*** Begin Add *** 10/26/20 ************************************
     D is48            s               n   Inz(*Off)
     D #1pgm           s             10a   INZ('W1054R')
     D #1flag          s              1a
     D xxdt            s              7  0
     D x1Type          s              1
     D xxunid          s                   like(PEunid)
     D x1end#          s                   like(PEend#)
     D strName         s                   like(EMname)
     D intFein         s                   like(EMfein)
      *** Beg Add ***** 06/04/24 *****************************
     D intEnty         s                   like(NAencd)
     D strCty          s                   like(NAmCty)
     D strSt           s                   like(NAmSt)
     D strZip          s                   like(NAmZip)
      *** End Add ***** 06/04/24 *****************************

     D                 DS
     D  xxName                 1    100
     D  NAName                 1     40
     D  NANam3                41    100

     D                 DS
     D  X1ZIP                  1      9  0
     D  X1ZIP1                 1      5  0
     D  X1ZIP2                 6      9  0

     D SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1
      //*** End   Add *** 10/26/20 ************************************
      //*** beg add ***** 07/28/21 *************************
       Dcl-S xxStRptDiv Packed(5:0);
      //*** end add ***** 07/28/21 *************************

      *** Beg Add ***** 07/25/24 ***************************************
       Dcl-S xxline2 Ind Inz(*Off);
      *** End Add ***** 07/25/24 ***************************************
      //* Copybook C specs
     C/COPY SCOPYEN01C

      *** Beg Add ***** 07/25/24 ***************************************
     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv

      ** WXPEL - Policy page endorsement transaction file
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
    
      *** End Add ***** 07/25/24 ***************************************
      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr OthInf;
        //*** Beg Chg ***** 07/25/24 ******************************************
        //***Exsr OpenWorkFile;
        If %Subst(qqForm:1:8) <> 'WC040314';
           Exsr OpenWorkFile;
        Endif;
        //*** End Chg ***** 07/25/24 ******************************************

        If QQform = 'WC480314';
           Exsr SR480314;
           Exsr CloseWorkFile;
           LeaveSr;
        Endif;

        // Get PEO's name for WC320306
        dsPEOInfo = GetPEOInfo(xxCo#:xxFnd);
        //*** Beg Add *** 07/06/22 ********************************************
        If %Subst(QQform:1:8) = 'WC290311';
           Exsr writeMultiRcd;
           Exsr CloseWorkFile;
           LeaveSr;
        Endif;
        //*** End Add *** 07/06/22 ********************************************
        FM2003 = dsPEOInfo.name;
        FMc404 = dsPEOInfo.address1;
        FMc405 = dsPEOInfo.address2;
        FMc406 = dsPEOInfo.address3;

        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Dou %Eof(WDELL1);
           Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           If %Eof;
              Leave;
           Endif;
           //***** Beg Add *** 10/28/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
              Iter;
           Endif;
           //***** End Add *** 10/28/22 ******************************
           If FNpeof <> *Blank And ELemp# = 1 And ELdiv = 0;
              Iter;
           Endif;
           If ELpfdt = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
              Iter;
           Endif;
           Chain (Elco#:ELrtcd) prs01;
           // Skip if division's rating state <> endorsement state (if specified)
           Select;
           When Not(%Found);
              Iter;
           When ENst = *Blanks;
           When Enst <> RSjrst;
              Iter;
           Endsl;
           // Skip if division's rating state is not approved for
           //  the endorsment or if excluded from the endorsement.
           //*** Beg Chg ***** 01/27/23 *****************************
           //***Chain (ELco#:qqForm:RSjrst) pes01;
           //***Select;
           //***When Not(%Found);
           //***When ESapex = 'A' and ESst = RSjrst;
           //***When ESapex = 'E' and ESst <> RSjrst;
           //***Other;
           //***   Iter;
           //***Endsl;
           Exsr ChkApprExclSt;
           If isApprExclSt = *Off;
              Iter;
           Endif;
           //*** End Chg ***** 01/27/23 *****************************
           dsCltInfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
           FM2001 = dsCltInfo.fedName;
           FM2002 = dsCltInfo.name;
           FMead1 = dsCltInfo.address1;
           FMead2 = dsCltInfo.address2;
           FMead3 = dsCltInfo.address3;
           FMFein = %dec(dsCltInfo.fein:9:0);
           x1AnPr = ELanpr;
           FMc201 = %trim(%editc(x1anpr:'K':'$'));
          // Entries specific to WC320306
          //   -State where work performed
          //   -Check 'Direct Workers' box if leasing ID = '2'
          //     on a master policy (PEOF = 'S')
           FMc101 = Rsjrst;
           FMfl01 = *Blank;
           Select;
           When EMlsid <> '2';
           When FNpeof = 'M';
           When FNpeof = 'S';
              FMfl01 = 'Y';
           Other;
              Setgt (ELco#:ELfnd:RSjrst:ELpfdt) prr00;
              Readpe (ELco#:ELfnd:RSjrst) prr00;
              If RRpeof = 'S';
                 FMfl01 = 'Y';
              Endif;
           Endsl;
           If %Subst(qqForm:1:8) = 'WC480318';
              xxStRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#);
              Chain (xxCo#:xxFnd:xxEmp#:xxStRptDiv) pem01;
              Select;
              When EMmad1 <> *Blanks;
                 FMc401 = EMmad1;
                 FMc402 = EMmad2;
                 FMc403 = %Trim(EMmCty) + ', ' + EMmSt + ' ' +
                  %Subst(%Editc(EMmZip:'X'):1:5);
                 If %Subst(%Editc(EMmZip:'X'):6:4) <> '0000';
                    FMc403 = FMc403 + '-' + %Subst(%Editc(EMmZip:'X'):6:4);
                 Endif;
              Other;
                 FMc401 = EMpad1;
                 FMc402 = EMpad2;
                 FMc403 = %Trim(EMpCty) + ', ' + EMpSt + ' ' +
                  %Subst(%Editc(EMpZip:'X'):1:5);
                 If %Subst(%Editc(EMpZip:'X'):6:4) <> '0000';
                    FMc403 = FMc403 + '-' + %Subst(%Editc(EMpZip:'X'):6:4);
                 Endif;
              Endsl;
              If FMc402 = *Blanks;
                 FMc402 = FMc403;
                 FMc403 = *Blanks;
              EndIf;
           Endif;
           //*** Beg Chg ***** 07/25/24 *******************************
           //***Exsr WriteMultiRcd;
           If %Subst(qqForm:1:8) <> 'WC040314';
              Exsr WriteMultiRcd;
           Else;
              Exsr OpenWorkFile;
              Exsr WriteMultiRcd;
              Exsr CloseWorkFile;
           Endif;
           //*** End Chg ***** 07/25/24 *******************************
           //*** Beg Add ***** 07/25/24 *******************************
           If %Subst(qqForm:1:8) = 'WC040314';
              Clear E04315;
      /free
    
              // Get client address info
              xxdiv = ELdiv;
              Chain emky1 pem01;
              If %found(WMEML);
                 If EMpad1 <> *blanks;
                    xkadr1    = %trim(EMpad1);
                    xkadrcty  = %trim(EMpcty);
                    xkadrstat = EMpst;
                    xkadrzip  = %editc(EMpzip:'X');
                 Else;
                    xkadr1    = EMmad1;
                    xkadrcty  = %trim(EMmcty);
                    xkadrstat = EMmst;
                    xkadrzip  = %editc(EMmzip:'X');
                 Endif;
                 If EMname <> *blanks;
                    If Emnam2 <> *blanks;
                       strName = %trim(Emname) + %trim(Emnam2);
                    Else;
                       strName = %trim(Emname);
                    EndIf;
                 EndIf;
                 If EMFdnm <> *blanks;
                    If EMFdn2 <> *blanks;
                       strName = %trim(EMFdnm) + %trim(EMFdn2);
                    Else;
                       strName = %trim(EMFdnm);
                    EndIf;
                 EndIf;
      /end-free
     c                   movel(p)  strName       ##line
     c                   eval      ##length = 79

     c                   call      'SRLINE'
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256
     /free
                 xkNmCl = %trim(##line);
                 xkseq# = '01';

                 ExSr WrtWC040314B;

                 xxline2 = *On;
                 If ##remain <> *blanks;
                    clear E04315;
                    xkNmCl = %trim(##remain);
                    xkseq# = '02';
                    ExSr WrtWC040314B;
                 EndIf;

              Endif;
           Endif;
      /end-free
           //*** End Add ***** 07/25/24 *******************************
           If %Subst(qqForm:1:8) = 'WC480318';
              Leave;
           Endif;
        Enddo;
        //*** Beg Chg ***** 07/25/24 ******************************************
        //***Exsr CloseWorkFile;
        If %Subst(qqForm:1:8) <> 'WC040314';
           Exsr CloseWorkFile;
        Endif;
        //*** End Chg ***** 07/25/24 ******************************************
       Endsr;

     C*** Beg Add ***** 07/25/24 **************************************
     *=========================================================================
     *  WrtWC040314B - Update Endorsement Reporting File for WCPOLS (WC480314)
     *=========================================================================
     C     WrtWC040314B  BegSr

      *  If printing a policy page.  (Not called from W1054R)

     c                   Call      'SRCHKPGM'
     c                   Parm      'W1054R   '   #1pgm            10
     c                   Parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

      * Update the endorsement reporting file with the proper data.

     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     C     pekey         SetGT     ppe02
     C     pekey1        ReadPe    ppe02                                  60

     C                   If        *in60 = *off
     C                   Z-add     peunid        xxunid
     C                   Endif

     C                   Movel     xxform        x1end#
     c     pekey2        Chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off

     c                   Move      *blanks       pedata
     c                   Movel     E04315        pedata
     c                   Movel     q1user        peuser
     c                   Movel     q1pgm         pepgm
     c                   Z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
                         If xkseq# = '01' and xxline2 = *Off;
     C                   Update    ppe01
     c                   Else

     c                   Movel     q1user        peiusr
     c                   Movel     q1pgm         peipgm
     c                   Z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   Write     ppe01

     c                   Endif
     c                   Endif                                                  *in60 ifeq off
     C                   Endif

     C                   Endsr
     C*** End Add ***** 07/25/24 **************************************
       //=========================================================================
       //  SR480314 - Process endorsement WC480314
       //=========================================================================
       Begsr SR480314;
       i = 1;
       x1end# = %trim(qqform);
       is48 = *Off;
       //*** Beg Chg ***** 06/04/24 *****************************
       //*** Chain (xxCo#:xxFnd:xxEmp#:xxFyr:'P') psh01;

       //*** If %found(wmshl1);
       //***    Setll (xxCo#:xxFnd:xxEmp#:xxFyr:SHUnid) pna00;
       //***
       //***    If %equal(wxnal);
       //***       Reade (xxCo#:xxFnd:xxEmp#:xxFyr:SHUnid) pna00;
       //***       Dow Not %Eof;
       //***
       //***          If NAName <> ' ' and NANamp = 'C';
       //***             FM2001 = xxName;
       //***             FM2002 = xxName;
       //***             FMead1 = NAMAD1;
       //***             FMead2 = NAMAD2;
       //***             x1Zip = NAMzip;
       //***             If x1Zip2 <> 0;
       //***                FMead3 = %Trim(NAMCTY) + ', ' +
       //***                         %Trim(NAMST) + '  ' +
       //***                         %Trim(%editc(x1Zip1:'X')) + '-' +
       //***                         %Trim(%editc(x1Zip2:'X'));
       //***             Else;
       //***                FMead3 = %Trim(NAMCTY) + ', ' +
       //***                         %Trim(NAMST) + '  ' +
       //***                         %Trim(%editc(x1Zip1:'X'));
       //***             Endif;
       //***             If FMead2 = ' ';
       //***                FMead2 = FMead3;
       //***                FMead3 = ' ';
       //***             Endif;
       //***             FMFein = NAFein;
       //***             FMc202 = %editc(NAENCD:'X');
       //***             is48 = *On;
       //***             Exsr WriteMultiRcd;
       //***             Exsr WriteWcPolsEr;
       //***          Endif;

       //***          Reade (xxCo#:xxFnd:xxEmp#:xxFyr:SHUnid) pna00;
       //***       Enddo;
       //***    Endif;
       //*** Endif;

       intEnty = 2;                            // Start Entity code with 002

       Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Dou %Eof(WDELL1);
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          If %Eof;
             Leave;
          Endif;

          If FNpeof <> *Blank And ELemp# = 1 And ELdiv = 0;
             Iter;                             // this is PEO, not a client
          Endif;                               // this form is for printing clients

          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
             Iter;
          Endif;

          Chain (Elco#:ELrtcd) prs01;
          // Skip if division's rating state <> endorsement state (if specified)
          Select;
          When Not(%Found);
             Iter;
          When ENst = *Blanks;
          When Enst <> RSjrst;                   // not WI state
             Iter;
          Endsl;

          x1AnPr = ELanpr;
          FMc201 = %trim(%editc(x1anpr:'K':'$'));

          Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
          If %Found(wmeml);
             If EMrpeo = 'Y';                    // Report as PEO
                Iter;
             Endif;

             FM2001 = EMname;
             If EMfdnm <> *Blanks;
                FM2001 = EMfdnm;
             Endif;
             FM2002 = *Blanks;
             strCty = *Blanks;
             strSt  = *Blanks;
             strZip = *zeros;

             If EMpad1 <> *Blanks;
                FMead1 = EMpad1;
                FMead2 = EMpad2;
                x1Zip = EMpzip;
                If x1Zip2 <> 0;
                   FMead3 = %Trim(EMpcty) + ', ' +
                            %Trim(EMpst) + '  ' +
                            %Trim(%editc(x1Zip1:'X')) + '-' +
                            %Trim(%editc(x1Zip2:'X'));
                Else;
                   FMead3 = %Trim(EMpcty) + ', ' +
                            %Trim(EMpst) + '  ' +
                            %Trim(%editc(x1Zip1:'X'));
                Endif;

                strCty = EMpcty;
                strSt  = EMpst;
                strZip = EMpzip;
             Else;
                FMead1 = EMmad1;
                FMead2 = EMmad2;
                x1Zip = EMmzip;
                If x1Zip2 <> 0;
                   FMead3 = %Trim(EMmcty) + ', ' +
                            %Trim(EMmst) + '  ' +
                            %Trim(%editc(x1Zip1:'X')) + '-' +
                            %Trim(%editc(x1Zip2:'X'));
                Else;
                   FMead3 = %Trim(EMmcty) + ', ' +
                            %Trim(EMmst) + '  ' +
                            %Trim(%editc(x1Zip1:'X'));
                Endif;

                strCty = EMmcty;
                strSt  = EMmst;
                strZip = EMmzip;
             Endif;

             If FMead2 = ' ';
                FMead2 = FMead3;
                FMead3 = ' ';
             Endif;
             FMFein = EMFein;
             FMc202 = %editc(intEnty:'X');
             intEnty = intEnty + 1;
             is48 = *On;
             Exsr WriteMultiRcd;
             Exsr WriteWcPolsEr;
          Endif;

       Enddo;
       //*** End Chg ***** 06/04/24 *****************************

       If is48 = *Off;
          dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
          If dsCltInfo.fedName > *blanks;
             FM2001 = dsCltInfo.fedName;
          Else;
             FM2001 = dsCltInfo.name;
          Endif;
          FM2002 = dsCltInfo.name;
          FMead1 = dsCltInfo.address1;
          FMead2 = dsCltInfo.address2;
          FMead3 = dsCltInfo.address3;
          If FMead2 = *Blanks;
             FMead2 = FMead3;
             FMead3 = *Blanks;
          Endif;
          FMFein = 0;
          If dsCltInfo.fein <> ' ';
             FMFein = %int(dsCltInfo.fein);
          Endif;
          //*** Beg Chg ***** 06/04/24 *****************************
          //*** FMc202 = '001';
          FMc202 = '002';                     // Start Entity code with 002
          //*** End Chg ***** 06/04/24 *****************************
          Exsr WriteMultiRcd;
          Exsr WriteWcPolsEr;
       Endif;
       Endsr;
       //=========================================================================
       //  Write WCPOLS Endorsement record
       //=========================================================================
       Begsr WriteWcPolsEr;
        //Report the Information to WCPOLS: Write/Update WXPEP
        SrChkPgm(#1pgm:#1flag);

        If #1flag = '0';
           //Update the endorsement reporting file with the proper data.
           //Find the unique ID number.
           x1Type = 'P';
           xxUnid = *zeros;
           xxDt = *hival;
           Setgt (xxCo#:xxFnd:xxEmp#:xxFyr:xxdt) ppe02;
           Readpe (xxCo#:xxFnd:xxEmp#:xxFyr) ppe02;

           If not %Eof(wxpel1);
              xxUnid = PEUnid;
              Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxUnid:x1end#)  ppe01;

              If %found(wxpel);
                 Clear e48315;
                 x8cname = FM2001;

                 Select;
                 When is48 = *On;

                    //*** Beg Chg ***** 06/04/24 *****************************
                    //*** If %subst(FMead2:1:5) <> %subst(NAmCty:1:5);
                    //***    x8cadr = %trim(ExtrAlphaNum(FMead1)) + ' ' +
                    //***    %trim(ExtrAlphaNum(FMead2));
                    //*** Else;
                    //***    x8cadr = %trim(ExtrAlphaNum(FMead1));
                    //*** Endif;

                    //*** x8ccity = %trim(ExtrAlphaNum(NAmCty));
                    //*** x8cst   = %trim(NAmSt);
                    //*** x1Zip = NAmZip;
                    //*** If x1Zip2 <> 0;
                    //***    x8cZip =   %Trim(%editc(x1Zip1:'X')) + '-' +
                    //***               %Trim(%editc(x1Zip2:'X'));
                    //*** Else;
                    //***    x8cZip = %Trim(%editc(x1Zip1:'X'));
                    //*** Endif;
                    //*** x8cfein = %editc(NAfein:'X');

                    If %subst(FMead2:1:5) <> %subst(strCty:1:5);
                       x8cadr = %trim(ExtrAlphaNum(FMead1)) + ' ' +
                       %trim(ExtrAlphaNum(FMead2));
                    Else;
                       x8cadr = %trim(ExtrAlphaNum(FMead1));
                    Endif;

                    x8ccity = %trim(ExtrAlphaNum(strCty));
                    x8cst   = %trim(strSt);
                    x1Zip = strZip;
                    If x1Zip2 <> 0;
                       x8cZip =   %Trim(%editc(x1Zip1:'X')) + '-' +
                                  %Trim(%editc(x1Zip2:'X'));
                    Else;
                       x8cZip = %Trim(%editc(x1Zip1:'X'));
                    Endif;
                    x8cfein = %editc(EMfein:'X');
                    //*** Beg Chg ***** 06/04/24 *****************************

                 Other;
                    If FMead2 <> *blanks and FMead3 <> *blanks;
                       x8cadr = %trim(ExtrAlphaNum(FMead1)) + ' ' +
                       %trim(ExtrAlphaNum(FMead2));
                    Else;
                       x8cadr = %trim(ExtrAlphaNum(FMead1));
                    Endif;

                    x8ccity = dsCltInfo.city;
                    x8cst   = dsCltInfo.state;
                    x1Zip   = %int(dsCltInfo.zip);
                    If x1Zip2 <> 0;
                       x8cZip =   %Trim(%editc(x1Zip1:'X')) + '-' +
                                  %Trim(%editc(x1Zip2:'X'));
                    Else;
                       x8cZip = %Trim(%editc(x1Zip1:'X'));
                    Endif;

                    x8cfein = dsCltInfo.fein;
                 Endsl;
                 strName = FM2001;
                 x8cprem = GetPremium(xxCo#:xxFnd:xxEmp#:FMfein:strName);

                 PEData = %trim(e48315);

                 PEName = %trim(ExtrAlphaNum(PEName));
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

           Clear FM2001;
           Clear FM2002;
           Clear FMead1;
           Clear FMead2;
           Clear FMead3;
           Clear FMfein;
           Clear FMc201;
           Clear FMc202;
        Endif;
       Endsr;
        //*** End   Add *** 10/26/20 *********************************************
       //================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr Pgmstr;
          RunningSRtext = *Off;
          WriteInCopybook = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr Pgmend;
        If printingPolicy;
           Chain inXML XMLTEML1;
           If %Found(XMLTEML1);
              XTtype = 'PEO Client Endorsement';
              Update XT100;
           Endif;
        Else;
           Chain inXML XMLPREL1;
           If %Found(XMLPREL1);
              XPtype = 'PEO Client Endorsement';
              Update XP100;
           Endif;
        Endif;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
        crtFrm = *On;
       Endsr;
      /End-Free

        //*** Begin Add *** 10/26/20 *********************************************
       //=========================================================================
       //  GetPremium
       //=========================================================================
       DCL-PROC GetPremium;
          DCL-PI  *N zoned(10);
             inCo# packed(3:0);
             inFnd packed(3:0);
             inEmp packed(9:0);
             inFein packed(9:0);
             inName char(40);
          END-PI;

          Dcl-C Upper Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
          Dcl-C Lower Const('abcdefghijklmnopqrstuvwxyz');
          Dcl-S intPrem zoned(10) inz;
          Dcl-S intPos packed(5:0) inz;

          intPrem = *Zeros;
          inName = %xlate(Lower:Upper:inName);

          Setll (inCo#:inFnd:inEmp:inFein) WMEML19;
          If %equal(WMEML19);
             Reade (inCo#:inFnd:inEmp:inFein) WMEML19;
             Dow not %eof(WMEML19);
                EMname = %xlate(Lower:Upper:EMname);

                If inName > *blanks and EMname > *blanks;
                   intPos = %scan(%trim(inName):EMname);
                Else;
                   intPos = *Zeros;
                Endif;

                //*** Beg Chg ***** 06/04/24 *****************************
                //*** If intPos = 0;
                If intPos = 0 and EMfdnm <> *blanks;
                //*** End Chg ***** 06/04/24 *****************************
                   EMfdnm = %xlate(Lower:Upper:EMfdnm);
                   intPos = %scan(%trim(inName):EMfdnm);
                Endif;

                If intPos > 0;
                   chain (inCo#:inFnd:inEmp:EMdiv) WDELL;
                   If %found(WDELL);
                      intPrem = ELanpr;
                      Leave;
                   Endif;
                Endif;
                Reade (inCo#:inFnd:inEmp:inFein) WMEML19;
             Enddo;
          Endif;
          Return intPrem;
       END-PROC;

