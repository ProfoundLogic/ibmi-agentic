     HCopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SRNOE - MA Notice to Employees
      //===============================================================
      // NOE - MA Notice to Employees
      //
      // Program Description:
      //
      //   Determines if this endorsement form should be attached to
      //   the policy and creates data in the Endorsement Form
      //   Workfile (WXFMP) prior to a form merge.
      //
      // Created   : 01/23/18
      // Programmer: C:136
      // Project   : 129WVA
      //===============================================================
      // REVISIONS:
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
      //
      // 02/24/21 C:65  Phw#94445 (CoveRisk)
      //                The cancelled division array processing was
      //                excluding all locations on division 0 because each
      //                slot in array is initially equal to zero.  Add one
      //                to each division # to prevent falsely finding div 0.
      //
      // 06/21/19 C:136 WC2677
      //               -Add code to exclude locations that are
      //                expired or under canceled divisions.
      //               -Add processing to attach and print the NH
      //                posting notice
      //
      //===============================================================
      // Notes:
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).
      //  QQMODE = 1 - Called to determine if form should be attached.
      //  QQMODE = 2 - Called to print endorsement.
      //  QQMODE = 3 - Called for additional input.
      //  QQMODE = 4 - Called for mid-term processing.
      //===============================================================
     FWMALL     IF   E           K DISK    RENAME(PAL01:PAL00)
     F/COPY SCOPYEN01F
     D ADDPGM          C                   CONST('xxxxxxxxxx')
     D/COPY SCOPYEN01D
     D*** Beg Del *** 09/20/24 *****************************************
     D***/Copy WprEmplr
     D*** End Del *** 09/20/24 *****************************************
      //================================================================
      //  PLIST definitions
      //================================================================
      //entry parameters
     D SRNOE           PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15
     D SRNOE           PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15

     D aryCxDiv        s              5  0 Dim(99999) Inz
     D aryFein         s              9  0 Dim(99999) Inz
     d cxDate7         s              7  0 inz
     D idx             S              5  0 inz
     D qqRrn           s              9  0                                      //Not used
     D qqMod2          s              1                                         //Not used
     C/COPY SCOPYEN01C
       //--------------------------------------------------------
       //  OthInf - Subroutine to load form specific information & write record
       //--------------------------------------------------------
      /Free
       Begsr OthInf;

        // Process divisions
        Setll (ELco#:ELfnd:ELemp#) pem01;
        Dou  %Eof(WMEML);
           Reade (ELco#:ELfnd:ELemp#) pem01;
           If %Eof;
              Leave;
           Endif;

           If EMmst <> ENst and EMpst <> ENst and ENst <> *Blanks;
              Iter;
           Endif;

          // Skip if division doesn't exist in policy year
          // Store to filter locations
           Chain (EMco#:EMfnd:EMemp#:EMdiv:xxFyr) pel01;
           If Not(%Found);
              idx = %LookUp(*Zeros:aryCxDiv);
              //*** begin chg ***  02/24/21  *************************
              //*****aryCxDiv(idx) = EMdiv;
              aryCxDiv(idx) = EMdiv +1;
              //*** end chg *****  02/24/21  *************************
              Iter;
           Endif;

          // Skip if division is canceled before policy effective date
          // Store to filter locations
           cxDate7 = GetCxDate(EMco#:EMfnd:EMemp#:EMdiv);
           If cxDate7 <> 0 And cxDate7 <= ELpfdt;
              idx = %LookUp(*Zeros:aryCxDiv);
              //*** begin chg ***  02/24/21  *************************
              //*****aryCxDiv(idx) = EMdiv;
              aryCxDiv(idx) = EMdiv +1;
              //*** end chg *****  02/24/21  *************************
              Iter;
           Endif;

           Select;
           When qqForm = 'NOC';
              idx = %LookUp(EMfein:aryFein);
              If idx <> 0;
                 Iter;
              Else;
                 idx = %LookUp(*Zeros:aryFein);
                 aryFein(idx) = EMfein;
                 Exsr WriteWorkFile;
              Endif;
           When QQform = 'NOE';
              FMinnm = EMname + EMnam2;
              If EMpst = Enst;
                 FMead1 = EMpad1;
                 FMead2 = EMpad2;
                 xxZip = %EditC(EMpzip:'X');
                 If xxZip2 <> *Zeros;
                    FMead3 = %Trim(EMpcty) + ', ' + EMpst + ' ' +
                     xxZip1 + '-' + xxZip2;
                 Else;
                    FMead3 = %Trim(EMpcty) + ', ' + EMpst + ' ' +
                     xxZip1;
                 Endif;
              Else;
                 If EMmst = Enst;
                    FMead1 = EMmad1;
                    FMead2 = EMmad2;
                    xxZip = %EditC(EMmzip:'X');
                    If xxZip2 <> *Zeros;
                       FMead3 = %Trim(EMmcty) + ', ' + EMmst + ' ' +
                       xxZip1 + '-' + xxZip2;
                    Else;
                       FMead3 = %Trim(EMmcty) + ', ' + EMmst + ' ' +
                       xxZip1;
                    Endif;
                  Endif;
              Endif;
              If FMead2 = *Blanks;
                 FMead2 = FMead3;
                 FMead3 = *Blanks;
              Endif;
              Exsr WriteWorkFile;
           Endsl;
        Enddo;

        //Process Locations
        Setll (ELco#:ELfnd:ELemp#) pal00;
        Dou  %Eof(WMALL);
           Reade (ELco#:ELfnd:ELemp#) pal00;
           If %Eof;
              Leave;
           Endif;

           If ALmst <> Enst and ALpst <> Enst and ENst <> *Blanks
             or ALncci <> 'Y';
              Iter;
           Endif;

          // Skip if division is canceled or year doesn't exist for division
           //*** begin chg ***  02/24/21  *************************
           //*****idx = %LookUp(ALdiv:aryCxDiv);
           idx = %LookUp(ALdiv+1:aryCxDiv);
           //*** end chg *****  02/24/21  *************************
           If idx <> 0;
              Iter;
           Endif;

          // Skip if location is 'flat canceled'
           If ALefdt = ALdldt;
              Iter;
           Endif;

          // Skip if location is expired before policy effective date
           If ALdldt <> 0;
              Chain (ALco#:ALfnd:ALemp#:ALdiv:xxFyr) pel01;
              If ALdldt <= ELpfdt;
                 Iter;
              Endif;
           Endif;

           Select;
           When qqForm = 'NOC';
              idx = %LookUp(ALfein:aryFein);
              If idx <> 0;
                 Iter;
              Else;
                 idx = %LookUp(*Zeros:aryFein);
                 aryFein(idx) = ALfein;
                 Exsr WriteWorkFile;
              Endif;
           When qqForm = 'NOE';
              FMinnm = ALname + ALnam2;
              If ALpst = Enst;
                 FMead1 = ALpad1;
                 FMead2 = ALpad2;
                 xxZip = %EditC(ALpzip:'X');
                 If xxZip2 <> *Zeros;
                    FMead3 = %Trim(ALpcty) + ', ' + ALpst + ' ' +
                     xxZip1 + '-' + xxZip2;
                 Else;
                    FMead3 = %Trim(ALpcty) + ', ' + ALpst + ' ' +
                     xxZip1;
                 Endif;
              Else;
                 If ALmst = Enst;
                    FMead1 = ALmad1;
                    FMead2 = ALmad2;
                    xxZip = %EditC(ALmzip:'X');
                    If xxZip2 <> *Zeros;
                       FMead3 = %Trim(ALmcty) + ', ' + ALmst + ' ' +
                       xxZip1 + '-' + xxZip2;
                    Else;
                       FMead3 = %Trim(ALmcty) + ', ' + ALmst + ' ' +
                       xxZip1;
                    Endif;
                 Endif;
              Endif;
              If FMead2 = *Blanks;
                 FMead2 = FMead3;
                 FMead3 = *Blanks;
              Endif;
              Exsr WriteWorkFile;
           Endsl;
        Enddo;

       Endsr;
       //--------------------------------------------------------
       //  PGMSTR - Beginning of program processing
       //--------------------------------------------------------
       Begsr PgmStr;
        WriteInCopybook = *Off;
        If qqForm = 'NOC';
          runningSrText = *On;
        Endif;
       Endsr;
       //--------------------------------------------------------
       //  PGMEND - End of program processing
       //--------------------------------------------------------
       Begsr PgmEnd;
       Endsr;
       //--------------------------------------------------------
       //  VALID - Subroutine to attach this form to the policy
       //--------------------------------------------------------
       Begsr Valid;
        CrtFrm = *On;
       Endsr;
      /End-Free
