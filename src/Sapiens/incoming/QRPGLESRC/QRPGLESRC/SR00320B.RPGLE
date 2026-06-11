     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00320B - PEO Extension Endorsement
      //===============================================================*
      // WC000320B
      //
      // PROGRAM DESCRIPTION:
      //   MULTIPLE MANUAL ADDITIONAL INFO stored in WDL2L2
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/20/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation.       *
      //                                                               *
      // 10/25/18 C:180 Support: 70270                                 *
      //                Include PEO relationship as part of attachment *
      //                criteria. Make auto attach.                    *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*

     Fwdl2l2    if   e           k disk

      /COPY SCOPYEN01F


     D                 ds
     D xxproj                        60
     D  fmc404                       40    overlay(xxproj:1)
     D  fmc204                       20    overlay(xxproj:41)

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC000320B ')

      *** Begin Change *** 10/25/18 ******************************
     D***addpgm          c                   const('SR00320I  ')
     D addpgm          c                   const('XXXXXXXX  ')
      *** End   Change *** 10/25/18 ******************************

     D SR00320B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00320B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
      *** Begin add *** 10/25/18 **************************************
     D/COPY WPRPEO
      ***** Beg Del *** 10/20/22 **************************************
     D***/Copy WprEmplr
      ***** End Del *** 10/20/22 **************************************
      *** End   add *** 10/25/18 **************************************

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       begsr othInf;

        //*** Begin Delete *** 10/25/18 ****************************************
          //* Write the output record
          //setll (xxCo#:xxFnd:xxFyr:xxEmp#) wdl2l2;
          //reade (xxCo#:xxFnd:xxFyr:xxEmp#) wdl2l2;

          //Dow Not %EOF(wdl2l2);
             //FMnm01 = L2nam1;
             //FMc401 = L2adr1;
             //FMc402 = L2adr2;
             //xxZip = %Char(L2Zip);
             //FMc403 = %Trim(L2cty) + ', ' + %Trim(L2st) + ' '
             //         + %Trim(xxZip1);
             //If xxzip2 <> *Blank And xxZip2 <> *zero;
             //   FMc403 = FMc403 + '-' + xxZip2;
             //Endif;

             //If fmc402 = *blank;
             //   fmc402 = fmc403;
             //   fmc403 = *blank;
             //Endif;

             //FMc101 = L2emst;
             //xxProj = L2proj;

             //WriteInCopybook = *Off;
             //Exsr WriteWorkFile;

             //reade (xxCo#:xxFnd:xxFyr:xxEmp#) wdl2l2;
          //Enddo;
        //*** End   Delete *** 10/25/18 ****************************************

        //*** Begin Add *** 10/25/18 *******************************************
        //Contractor's name and address

        dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
        If dsPeoInfo.Name  <> *blanks;
           FMnm01 = dsPeoInfo.Name;
        Else;
           FMnm01 = dsPeoInfo.FedName;
        Endif;

        FMc401 = dsPeoInfo.Address1;
        FMc402 = dsPeoInfo.Address2;
        FMc403 = dsPeoInfo.Address3;

        //Get Client's information
          setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;

          Dow Not %EOF(wdell1);
          //***** Beg Add *** 10/20/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/20/22 ******************************
                Chain (Elco#:ELrtcd) prs01;
                //*** Beg Add ***** 01/27/23 ************************
                Exsr ChkApprExclSt;
                If isApprExclSt = *On;
                //*** End Add ***** 01/27/23 ************************
                   If FNpeof = 'M' or FNpeof ='C' and ENpeof = '4';
                      Select;
                      When FNpeof = 'M' and ELemp# = 1;
                         //Omit
                      When FNpeof = 'C' and ELemp# = 1 and ELdiv = 0;
                         //Omit
                      Other;
                         If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                            dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:
                                        ELfyr);
                            FMc101 = dsCltInfo.state;
                            xxproj = *blanks;
                            WriteInCopybook = *Off;
                            Exsr WriteWorkFile;
                         Endif;
                      Endsl;
                   Endif;
                //*** Beg Add ***** 01/27/23 ************************
                Endif;
                //*** End Add ***** 01/27/23 ************************
           //***** Beg Add *** 10/20/22 ******************************
             Endif;
           //***** End Add *** 10/20/22 ******************************

             reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          Enddo;
        //*** End  Add *** 10/25/18 *******************************************

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
          UseDfltAddParms = *off;
       endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
          exsr GetFnd;
          If ENpeof = '4';          // MCP Client
             If FNpeof = 'M' or FNpeof ='C';
             crtFrm = *On;
             Endif;
          Endif;
       endsr;

      /End-free
