     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Email Contacts to iDart Mail
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from:
     h*  Calls to:
     h*
     h*  Created on 06/27/2014 by C:300 for Project DR312 PID-iDARTS Email Workflow
     h**********************************************************************
     h* Revisions:
     h*
     h* 10/21/25 C:65  Support: 119661
     h*                Change the gathering of agent history contacts to pull the current
     h*                policy year and forward.  If no currently active policy year then
     h*                pull the latest policy year instead.  Remove the check on agent
     h*                history effective date to allow agent of record to pull in.
     h*                Add default title for each contact type when no data is going to
     h*                pull in for the email address.
     h*
     h* 10/16/25 C:300 Support: 119661
     h*                When attempting to email from iDARTS it not bringing Contacts
     h*                1) Pull Policy Contacts only that are specific to division passed
     h*                  OR NOT flagged as Division Specific
     h*                2) From Policy - Include Division & Year while reading Agency Contacts
     h*                   and Check Agent effective date should be on or before current date
     h*                3) From Agency Configuration screen, IDarts Email is sending
     h*                   just FEIN and MOD. so using Agtkey2 and BuildAgt2 SR
     h*                4) Pull Claim Contacts from more file like
     h*                   WMPDP, WMSBP, WDACP, WMCNP, WMCDP
     h*
     h* 05/04/16 c:62 Ticket 40872(099)
     h*               When loading employer contacts, load agent contacts as well.
     h*
     h*
     h**********************************************************************
      * file for Policy Contacts
     fwmecl1    if   e           k Disk    reName(pec01:pec0p)

      * files for Claims Contacts
     fwmlll     if   e           k Disk    ReName(pll01:pll0p)
     fwmtpl     if   e           k Disk    ReName(ptp01:ptp0p)
     fwmcml1    if   e           k Disk    ReName(pcm01:pcm0p)
      *** Begin Add ***** 10/16/25 *****************************
      * Additional files for Claims Contacts
     fwmpdl     if   e           k Disk    ReName(ppd01:ppd0p)
     fwmsbl     if   e           k Disk    ReName(psb01:psb0p)
     fwdacl     if   e           k Disk    ReName(pac01:pac0c)
     fwmcnl     if   e           k Disk    ReName(pcn01:pcn0p)
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd0p)
      *** End   Add ***** 10/16/25 *****************************
     F*** begin add ***  10/21/25  *********************************
     fwdell2    if   e           k Disk    ReName(pel01:pel02)
     F*** end add *****  10/21/25  *********************************

      * files for Agency and Producer Contacts
     fwmahl     if   e           k Disk    ReName(pah01:pah0p)
     fwmaal1    if   e           k Disk    ReName(paa01:paa0p)
     fwma5l     if   e           k Disk    ReName(pac01:pac0p)
     fwmapl     if   e           k Disk    ReName(pap01:pap0p)

      *** Begin Add ***** 10/16/25 *****************************
     fsmupl     if   e           k Disk    ReName(pup01:pup0p)

     d psds          esds                  extname(szq1p)
      *** End   Add ***** 10/16/25 *****************************

      * *Entry
     dGetContact       pr                  ExtPgm('DTGETEML')
     d                              512
     d                            32767
     d                                1

     dGetContact       pi
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

      /copy sPrAtrium
      *** Begin Add ***** 10/16/25 *****************************
     d/copy sPrDate
     d/copy sPrCompany
     d/copy sPrString
      *** End   Add ***** 10/16/25 *****************************

      * Policy Key Data Structure
     D                 DS
     D  polkey                 1     40
     D  intpco#                1      3  0
     D  intpfnd                4      6  0
     D  intpemp#               7     15  0
     D  intpdiv               16     20  0
     D  strpfill              21     40

      * Claims Key Data Structure
     D                 DS
     D  clmkey                 1     40
     D  intcco#                1      3  0
     D  intcfnd                4      6  0
     D  intcfyr                7      9  0
     D  intccase              10     16  0
     D  strcfill              17     40

      * Agency Key Data Structure
     D                 DS
     D  agtkey                 1     40
     D  intaco#                1      3  0
     D  intafnd                4      6  0
     D  intaemp#               7     15  0
     D  intadiv               16     20  0
     D  intafyr               21     23  0
     D  strafill              24     40

      *** Begin Add ***** 10/16/25 *****************************
      * Agency Key 2 Data Structure
     D                 DS
     D  agtkey2                1     40
     D  intafein               1      9  0
     D  intamod               10     12  0
     D  strafill2             13     40
      *** End   Add ***** 10/16/25 *****************************


     d inkey           s             40    inz(*blanks)
     d keytyp          s              2    inz(*blanks)
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d strname         s             45    inz(*blanks)
     d strtitle        s             40    inz(*blanks)
     d stremail        s             60    inz(*blanks)
      *** Begin Add ***** 10/16/25 *****************************
     d sysdate         s              7  0 inz
     d intCo#          s              3  0 inz
     d intMod          s              5  0 inz
     d ##Fein          s              9  0 inz
     d ##Mod           s              3  0 inz
      *** End   Add ***** 10/16/25 *****************************

      * Policy key
     c     keyWMECP      Klist
     c                   Kfld                    intpco#
     c                   Kfld                    intpfnd
     c                   Kfld                    intpemp#

      * Claims key
     c     keyWMLLP      Klist
     c                   Kfld                    intcco#
     c                   Kfld                    intcfnd
     c                   Kfld                    intcfyr
     c                   Kfld                    intccase

      * Agency key 01
     c     keyWMAHP      Klist
     c                   Kfld                    intaco#
     c                   Kfld                    intafnd
     c                   Kfld                    intaemp#
      *** Begin Add ***** 10/16/25 *****************************
     c                   Kfld                    intadiv
     c*****              Kfld                    intafyr
      *** End   Add ***** 10/16/25 *****************************
      *** begin add ***** 10/21/25 *****************************
     c     key2WMAHL     Klist
     c                   Kfld                    intaco#
     c                   Kfld                    intafnd
     c                   Kfld                    intaemp#
     c                   Kfld                    intadiv
     c                   Kfld                    intafyr
      *** end add ******* 10/21/25 *****************************

      * Agency key 02
     c     keyWMAAP      Klist
     c                   Kfld                    intaco#
     c                   Kfld                    intafnd
     c                   Kfld                    AHagt#

      * Agency key 03
     c     keyWMA5P      Klist
     c                   Kfld                    intaco#
     c                   Kfld                    AAfein
      *** Begin Add ***** 10/16/25 *****************************
     c                   Kfld                    intMod
      *** End   Add ***** 10/16/25 *****************************

      *** Begin Add ***** 10/16/25 *****************************
      * Agency key 04
     c     keyWMA5P2     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intafein
     c                   Kfld                    intMod
      *** End   Add ***** 10/16/25 *****************************

      * Producer key 01
     c     keyWMAPP1     Klist
     c                   Kfld                    intaco#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod
     c                   Kfld                    AHprod

      * Producer key 02
     c     keyWMAPP2     Klist
     c                   Kfld                    intaco#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod
     c                   Kfld                    AHsprd

      * Producer key 03
     c     keyWMAPP3     Klist
     c                   Kfld                    intaco#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod
     c                   Kfld                    AH3prd

      *** Begin Add ***** 10/16/25 *****************************
      * Producer key 04
     c     keyWMAPP4     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intafein
     c                   Kfld                    intamod

      * Payee Demographics File key
     c     keyWMPDP      Klist
     c                   Kfld                    ##Fein
     c                   Kfld                    ##Mod
      *** End   Add ***** 10/16/25 *****************************

      /free
       // Main process
       outJSON = ' ';

       // Extract inkey
       startPos = %Check(' ':inGenParm);
       If startPos > 0;
          endPos = %Scan(',':inGenParm:startPos);
          If endPos > 0;
             len = endPos-startPos;
             If len > 0;
                inkey = %Subst(inGenParm:startPos:len);
             Endif;
          Endif;
       Endif;

       // extract keytyp
        startPos = endPos+1;
        If startPos > 0;
          endPos = %Scan(' ':inGenParm:startPos);
          If endPos > 0;
            len = endPos-startPos;
            If len > 0;
              keytyp = %Subst(inGenParm:startPos:len);
            Endif;
          Endif;
        Endif;

       outJSON = %Trim(outJSON) + '"contacts":';
       StartJSONarray(outJSON);

       //*** Begin Add *** 10/16/25 *********************
       sysdate = Dateto7(SystemDate());
       // get Co#
       Chain q1user pup0p;
       If %found(smupl);
          intCo# = UPco#;
       Endif;
       If intCo# = *zero;
          intCo# = GetOnlyCo#;
       Endif;
       //*** End   Add *** 10/16/25 *********************

       // Check key type to build contacts information.
       Select;
          When keytyp = 'WE';
       // Build policy Contacts
             polkey = Inkey;
             Exsr BuildPol;
             //*** Begin Add *** 05/04/16 *********************
             intaCo#  = intpCo#;
             intaFnd  = intpFnd;
             intaEmp# = intpEmp#;
             //*** Begin Add *** 10/16/25 *********************
             intaDiv = intpDiv;
             // Get Year from Current System Date
       //*** begin chg ***  10/21/25  *********************************
       //****intaFyr = sysdate/10000;
       //* note - can never depend on Fyr being in a specific format.
             Setgt (intaCo#:intaFnd:intaEmp#:intaDiv:sysDate) pel02;
             Readpe (intaCo#:intaFnd:intaEmp#:intaDiv) pel02;
             If not %eof;
                intaFyr = ELFyr;
             Else;
                intaFyr = 0;
             Endif;
       //*** end chg *****  10/21/25  *********************************
             //*** End   Add *** 10/16/25 *********************
             Exsr BuildAgt;
             Exsr BuildPrd;
             //*** End   Add *** 05/04/16 *********************
          When keytyp = 'WC';
       // Build Claim Contacts
             clmkey = Inkey;
             Exsr BuildClm;
          When keytyp = 'WA';
       // Build Agency Contacts
             //*** Begin Chg *** 05/04/16 *********************
             //*** agtkey = Inkey;
             //*** Exsr BuildAgt;
       // Build Producer Contacts
             //*** Exsr BuildPrd;

             agtkey2 = Inkey;
             intMod = intamod;
             Exsr BuildAgt2;
       // Build Producer Contacts
             Exsr BuildPrd2;
             //*** End   Chg *** 05/04/16 *********************
       Endsl;

       EndJSONarray(outJSON);

       RtnCode = ' ';
       Return;

       //***********************************************************
        // BuildPol for Policy Contacts
       //***********************************************************
       BegSr BuildPol;

       //get the Contact info from WMECP file
       Setll keyWMECP pec0p;
       ReadE keyWMECP pec0p;
       Dow not %eof(WMECL1);
          //*** Begin Add ***** 10/16/25 *****************************
          If ECdivSpec <> 'Y' or                      // NOT flagged as Division Specific
           (ECdivSpec = 'Y' and ECdiv = intpdiv);     // Specific to Division
          //*** End   Add ***** 10/16/25 *****************************
          If ECemal <> *Blanks;
             strname = %trim(ECfsn) + ' ' + %trim(EClsn);
             strtitle = %trim(ECposn);
             stremail = %trim(ECemal);
          //*** begin add ***** 10/21/25 *****************************
             If strTitle = ' ';
                strtitle = 'Policy Contact';
             Endif;
          //*** end add ******* 10/21/25 *****************************

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;
          //*** Begin Add ***** 10/16/25 *****************************
          Endif;
          //*** End   Add ***** 10/16/25 *****************************
          ReadE keyWMECP pec0p;
       Enddo;
       EndSr;

       //***********************************************************
        // BuildClm for Claim Contacts
       //***********************************************************
       BegSr BuildClm;

       //get the Contact info from WMLLP file
       Setll keyWMLLP pll0p;
       ReadE keyWMLLP pll0p;
       Dow not %eof(WMLLL);

       //for Defense Attorney
          If LLemdd <> *Blanks;
             strname = %trim(LLdefn);
             strtitle = 'Defense Attorney';
             stremail = %trim(LLemdd);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);

             //*** Begin Add *** 10/16/25 *********************
             //Get Claims - Payee Demographics Contacts from WMPDP
             If LLdefa <> 0;          // Defense Attorney FEIN
                ##Fein = LLdefa;
                ##Mod  = LLdefm;
                exsr GetWMPDP;
             Endif;
             //*** End   Add *** 10/16/25 *********************
          Endif;

       //for Plaintiff Attorney
          If LLemdp <> *Blanks;
             strname = %trim(LLplan);
             strtitle = 'Plaintiff Attorney';
             stremail = %trim(LLemdp);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);

             //*** Begin Add *** 10/16/25 *********************
             //Get Claims - Payee Demographics Contacts from WMPDP
             If LLplaa <> 0;          // Plaintiff Attorney FEIN
                ##Fein = LLplaa;
                ##Mod  = LLplam;
                exsr GetWMPDP;
             Endif;
             //*** End   Add *** 10/16/25 *********************
          Endif;

       //for Third Party Attorney
          If LLemdt <> *Blanks;
             strname = %trim(LLtpan);
             strtitle = 'Third Party Attorney';
             stremail = %trim(LLemdt);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);

             //*** Begin Add *** 10/16/25 *********************
             //Get Claims - Payee Demographics Contacts from WMPDP
             If LLtpaa <> 0;          // Third Party Attorney FEIN
                ##Fein = LLtpaa;
                ##Mod  = LLtpam;
                exsr GetWMPDP;
             Endif;
             //*** End   Add *** 10/16/25 *********************
          Endif;

          ReadE keyWMLLP pll0p;
       Enddo;

       //get the Contact info from WMTPP file
       Setll keyWMLLP ptp0p;
       ReadE keyWMLLP ptp0p;
       Dow not %eof(WMTPL);

       //for Treating Physician
          If TPemad <> *Blanks;
             strname = %trim(TPphyn);
             strtitle = 'Treating Physician';
             stremail = %trim(TPemad);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);

             //*** Begin Add *** 10/16/25 *********************
             //Get Claims - Payee Demographics Contacts from WMPDP
             If TPphyf <> 0;          // Treating Physician FEIN
                ##Fein = TPphyf;
                ##Mod  = TPphym;
                exsr GetWMPDP;
             Endif;
             //*** End   Add *** 10/16/25 *********************
          Endif;
          ReadE keyWMLLP ptp0p;
       Enddo;


       //Get Policy number from WMCMP and policy contacts from WMECP
       Chain keyWMLLP pcm0p;
       If %found(WMCML1);
          intpco#  = intcco#;
          intpfnd  = intcfnd;
          intpemp# = CMemp#;
          //*** Begin Add *** 10/16/25 *********************
          intpDiv = CMdiv;
          //*** End   Add *** 10/16/25 *********************

          Setll keyWMECP pec0p;
          ReadE keyWMECP pec0p;
          Dow not %eof(WMECL1);
             //*** Begin Add ***** 10/16/25 *****************************
             If ECdivSpec <> 'Y' or                      // NOT flagged as Division Specific
              (ECdivSpec = 'Y' and ECdiv = intpdiv);     // Specific to Division
             //*** End   Add ***** 10/16/25 *****************************
             If ECemal <> *Blanks;
                strname = %trim(ECfsn) + ' ' + %trim(EClsn);
                strtitle = %trim(ECposn);
                stremail = %trim(ECemal);
             //*** begin add ***** 10/21/25 *****************************
                If strTitle = ' ';
                   strtitle = 'Policy Contact';
                Endif;
             //*** end add ******* 10/21/25 *****************************

                StartJSONelemnt(outJSON);
                outJSON = %Trim(outJSON) + '"contact":';
                StartJSONelemnt(outJSON);
                BuildJSON(outJSON :'name' :%trim(strname));
                BuildJSON(outJSON :'title' :%trim(strtitle));
                BuildJSON(outJSON :'email' :%trim(stremail));
                EndJSONelemnt(outJSON);
                EndJSONelemnt(outJSON);
             Endif;
             //*** Begin Add ***** 10/16/25 *****************************
             Endif;
             //*** End   Add ***** 10/16/25 *****************************
             ReadE keyWMECP pec0p;
          Enddo;
       Endif;


       //*** Begin Add ***** 10/16/25 *****************************
       //get the Contact info from WMSBP (Subrogation Info)
       Setll keyWMLLP psb0p;
       ReadE keyWMLLP psb0p;
       Dow not %eof(WMSBL);

       //for Responsible Party contact info
          If SBrmad <> *Blanks;
             strname = %trim(SBrnam);
             strtitle = 'Responsible Party';
             stremail = %trim(SBrmad);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;

       //for Insurance Company contact info
          If SBimad <> *Blanks;
             strname = %trim(SBinam);
             strtitle = 'Insurance Company';
             stremail = %trim(SBimad);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;

          ReadE keyWMLLP psb0p;
       Enddo;



       //get the Contact info from WDACP (Additional Payee)
       Setll keyWMLLP pac0c;
       ReadE keyWMLLP pac0c;
       Dow not %eof(WDACL);

       //for Additional Payee contact info
          If ACemdc <> *Blanks;
             strname = %trim(ACpnam);
             strtitle = 'Additional Payee';
             stremail = %trim(ACemdc);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;

          ReadE keyWMLLP pac0c;
       Enddo;



       //get Non-System Contact for Claim Contacts from WMCNP file
       Setll keyWMLLP pcn0p;
       ReadE keyWMLLP pcn0p;
       Dow not %eof(WMCNL);

       //for Non-System Contact for Claim Contacts
          If CNemal <> *Blanks;
             strname = FormatName(CNlsn:CNfsn:CNmid:'*FML');
          //*** begin add ***** 10/21/25 *****************************
          //*****   strtitle = CNtitl;
             strtitle = CNType;
             If strTitle = ' ';
                strtitle = 'Claim Contact';
             Endif;
          //*** end add ******* 10/21/25 *****************************
             stremail = %trim(CNemal);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;

          ReadE keyWMLLP pcn0p;
       Enddo;



       //get System Contact for Claimant Demographics file WMCDP
       Setll CMss# pcd0p;
       ReadE CMss# pcd0p;
       Dow not %eof(WMCDL);

       //for System Contact for Claim Contacts
          If CDemad <> *Blanks;
             strname = FormatName(CDlsn:CDfsn:CDmid:'*FML');
             strtitle = ' ';
             stremail = %trim(CDemad);
          //*** begin add ***** 10/21/25 *****************************
             If strTitle = ' ';
                strtitle = 'Claim Demographics';
             Endif;
          //*** end add ******* 10/21/25 *****************************

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;

          ReadE CMss# pcd0p;
       Enddo;
       //*** End   Add ***** 10/16/25 *****************************

       EndSr;

       //***********************************************************
        // BuildAgt for Agency Contacts
       //***********************************************************
       BegSr BuildAgt;

       //Get Agent # assigned by Fund from WMAHP
       //*** begin chg ***** 10/21/25 *****************************
       //*****Setll keyWMAHP pah0p;
       Setll key2WMAHL pah0p;
       //*** end chg ******* 10/21/25 *****************************
       ReadE keyWMAHP pah0p;
       Dow not %eof(WMAHL);
          //*** Begin Chg ***** 10/16/25 *****************************
          //*** If AHstat = 'A';
          // Agents effective Date should on or before the current system date
       //*** begin chg ***** 10/21/25 *****************************
       //******   If AHstat = 'A' and AHefdt <= sysdate;
          If AHstat = 'A';
       //*** end chg ******* 10/21/25 *****************************
          //*** End   Chg ***** 10/16/25 *****************************

       //Get Federal Tax ID from WMAAP
             Chain keyWMAAP paa0p;
             If %found(WMAAL1);

       //Get Agency Contact from WMA5P
                Setll keyWMA5P pac0p;
                ReadE keyWMA5P pac0p;
                Dow not %eof(WMA5L);

                   If A5emal <> *Blanks;
                      strname = %trim(A5fsn) + ' ' + %trim(A5lsn);
                      strtitle = %trim(A5posn);
                      stremail = %trim(A5emal);

                      StartJSONelemnt(outJSON);
                      outJSON = %Trim(outJSON) + '"contact":';
                      StartJSONelemnt(outJSON);
                      BuildJSON(outJSON :'name' :%trim(strname));
                      BuildJSON(outJSON :'title' :%trim(strtitle));
                      BuildJSON(outJSON :'email' :%trim(stremail));
                      EndJSONelemnt(outJSON);
                      EndJSONelemnt(outJSON);
                   Endif;
                   ReadE keyWMA5P pac0p;
                Enddo;
             Endif;
          Endif;
          ReadE keyWMAHP pah0p;
       Enddo;

       EndSr;

       //***********************************************************
        // BuildPrd for Producer Contacts
       //***********************************************************
       BegSr BuildPrd;

       //Get Agent # assigned by Fund from WMAHP
       //*** begin chg ***** 10/21/25 *****************************
       //*****Setll keyWMAHP pah0p;
       Setll key2WMAHL pah0p;
       //*** end chg ******* 10/21/25 *****************************
       ReadE keyWMAHP pah0p;
       Dow not %eof(WMAHL);
          //*** Begin Chg ***** 10/16/25 *****************************
          //*** If AHstat = 'A';
          // Agents effective Date should on or before the current system date
       //*** begin chg ***** 10/21/25 *****************************
       //******   If AHstat = 'A' and AHefdt <= sysdate;
          If AHstat = 'A';
       //*** end chg ******* 10/21/25 *****************************
          //*** End   Chg ***** 10/16/25 *****************************

       //Get Federal Tax ID and Modifier from WMAAP
             Chain keyWMAAP paa0p;
             If %found(WMAAL1);

       //Get Producer Contact of Primary Agency # from WMAPP
                If AHprod <> 0;
                   Chain keyWMAPP1 pap0p;
                   If %found(WMAPL);

                      If APemal <> *Blanks;
                         strname = %trim(APfsn) + ' ' + %trim(APlsn);
                         strtitle = 'Producer of Primary Agency #';
                         stremail = %trim(APemal);

                         StartJSONelemnt(outJSON);
                         outJSON = %Trim(outJSON) + '"contact":';
                         StartJSONelemnt(outJSON);
                         BuildJSON(outJSON :'name' :%trim(strname));
                         BuildJSON(outJSON :'title' :%trim(strtitle));
                         BuildJSON(outJSON :'email' :%trim(stremail));
                         EndJSONelemnt(outJSON);
                         EndJSONelemnt(outJSON);
                      Endif;
                   Endif;
                Endif;

       //Get Producer Contact of Secondary Agency # from WMAPP
                If AHsprd <> 0;
                   Chain keyWMAPP2 pap0p;
                   If %found(WMAPL);

                      If APemal <> *Blanks;
                         strname = %trim(APfsn) + ' ' + %trim(APlsn);
                         strtitle = 'Producer of Secondary Agency #';
                         stremail = %trim(APemal);

                         StartJSONelemnt(outJSON);
                         outJSON = %Trim(outJSON) + '"contact":';
                         StartJSONelemnt(outJSON);
                         BuildJSON(outJSON :'name' :%trim(strname));
                         BuildJSON(outJSON :'title' :%trim(strtitle));
                         BuildJSON(outJSON :'email' :%trim(stremail));
                         EndJSONelemnt(outJSON);
                         EndJSONelemnt(outJSON);
                      Endif;
                   Endif;
                Endif;

       //Get Producer Contact of Tertiary Agency # from WMAPP
                If AH3prd <> 0;
                   Chain keyWMAPP3 pap0p;
                   If %found(WMAPL);

                      If APemal <> *Blanks;
                         strname = %trim(APfsn) + ' ' + %trim(APlsn);
                         strtitle = 'Producer of Tertiary Agency #';
                         stremail = %trim(APemal);

                         StartJSONelemnt(outJSON);
                         outJSON = %Trim(outJSON) + '"contact":';
                         StartJSONelemnt(outJSON);
                         BuildJSON(outJSON :'name' :%trim(strname));
                         BuildJSON(outJSON :'title' :%trim(strtitle));
                         BuildJSON(outJSON :'email' :%trim(stremail));
                         EndJSONelemnt(outJSON);
                         EndJSONelemnt(outJSON);
                      Endif;
                   Endif;
                Endif;

             Endif;
          Endif;
          ReadE keyWMAHP pah0p;
       Enddo;

       EndSr;

       //*** Begin Add ***** 10/16/25 *****************************
       //***********************************************************
        // BuildAgt2 for Agency Contacts
       //***********************************************************
       BegSr BuildAgt2;

       //Get Agency Contact from WMA5P
       Setll keyWMA5P2 pac0p;
       ReadE keyWMA5P2 pac0p;
       Dow not %eof(WMA5L);

          If A5emal <> *Blanks;
             strname = %trim(A5fsn) + ' ' + %trim(A5lsn);
             strtitle = %trim(A5posn);
             stremail = %trim(A5emal);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;
          ReadE keyWMA5P2 pac0p;
       Enddo;

       EndSr;

       //***********************************************************
        // BuildPrd2 for Producer Contacts
       //***********************************************************
       BegSr BuildPrd2;

       //Get Producer Contacts from WMAPP
       Setll keyWMAPP4 pap0p;
       ReadE keyWMAPP4 pap0p;
       Dow not %eof(WMAPL);
          If APemal <> *Blanks;
             strname = %trim(APfsn) + ' ' + %trim(APlsn);
             strtitle = 'Producer of Agency #';
             stremail = %trim(APemal);

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;
          ReadE keyWMAPP4 pap0p;
       Enddo;

       EndSr;

       //***********************************************************
        // GetWMPDP for Claims - Payee Demographics Contacts
       //***********************************************************
       BegSr GetWMPDP;

       //Get Claims - Payee Demographics Contacts from WMPDP
       Setll keyWMPDP ppd0p;
       ReadE keyWMPDP ppd0p;
       Dow not %eof(WMPDL);
          If PDemad <> *Blanks;
             strname = %trim(PDname);
             strtitle = %trim(PDtitl);
             stremail = %trim(PDemad);
          //*** begin add ***** 10/21/25 *****************************
             strtitle = 'Payee Demographics';
          //*** end add ******* 10/21/25 *****************************

             StartJSONelemnt(outJSON);
             outJSON = %Trim(outJSON) + '"contact":';
             StartJSONelemnt(outJSON);
             BuildJSON(outJSON :'name' :%trim(strname));
             BuildJSON(outJSON :'title' :%trim(strtitle));
             BuildJSON(outJSON :'email' :%trim(stremail));
             EndJSONelemnt(outJSON);
             EndJSONelemnt(outJSON);
          Endif;
          ReadE keyWMPDP ppd0p;
       Enddo;

       EndSr;
       //*** End   Add ***** 10/16/25 *****************************
      /end-free
