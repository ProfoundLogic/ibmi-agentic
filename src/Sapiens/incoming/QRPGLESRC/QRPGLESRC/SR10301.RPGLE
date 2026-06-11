     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR10301 - GA - Employee Leasing Endorsement
      //===============================================================*
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
      //                                                               *
      // 10/10/17 C:65  Project: 150WAP                                *
      //                Recode to use new copybook for PEO attachment, *
      //                and recode to use WCPOLS work file WXNAP to    *
      //                list the names/addresses.                      *
      //                                                               *
      // 07/21/17 C:180 Project: 150WAP                                *
      //                Handle groups with combined PEOs.              *
      //                                                               *
      // 03/15/17 C:136 Project: WC2481                                *
      //                Set up for auto attach and populate            *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
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

     Fwmshl1    if   e           k disk
     Fwxnal     if   e           k disk    rename(pna01:pna00)

      /COPY SCOPYEN01F
      /COPY WWCPOLDAT4

     D xxefdt          s                   like(fmeedt)
     D xxProcessed     s              1
     D                 DS
     D  xxName                 1    100
     D  NAName                 1     40
     D  NANam3                41    100
     D                 DS
     D  X1ZIP                  1      9  0
     D  X1ZIP1                 1      5  0
     D  X1ZIP2                 6      9  0

      /Copy SCOPYEN01D
      /Copy WPRPEO
     D*** Beg Del *** 09/20/24 *****************************************
     D*** /Copy WPREMPLR
     D*** End Del *** 09/20/24 *****************************************

     D qqForm          s             15A   Inz('WC100301  ')
     D addpgm          c                   const('SRXXXXXX  ')

     D SR10301         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR10301         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1


      /COPY SCOPYEN01C
       //======================================================
       // OTHINF - Subroutine to load form specific information
       //======================================================
      /Free
       Begsr othinf;

          Chain(n) (xxCo#:xxFnd:xxEmp#:xxFyr:'P') psh01;

          If not %found;
             dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
             FMnm01 = dsCltInfo.name;
             FM2001 = dsCltInfo.name;
             FMc401 = dsCltInfo.address1;
             FMc402 = dsCltInfo.address2;
             FMc403 = dsCltInfo.address3;
             FMFein = 0;
             If dsCltInfo.fein <> ' ';
                FMFein = %int(dsCltInfo.fein);
             Endif;
             FMc201 = '001';
             Exsr WriteWorkFile;
             Leavesr;
          Endif;

          Setll (xxCo#:xxFnd:xxEmp#:xxFyr:SHUnid) pna00;
          Reade(n) (xxCo#:xxFnd:xxEmp#:xxFyr:SHUnid) pna00;

          If %eof;
             dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
             FMnm01 = dsCltInfo.name;
             FM2001 = dsCltInfo.name;
             FMc401 = dsCltInfo.address1;
             FMc402 = dsCltInfo.address2;
             FMc403 = dsCltInfo.address3;
             FMFein = 0;
             If dsCltInfo.fein <> ' ';
                FMFein = %int(dsCltInfo.fein);
             Endif;
             FMc201 = '001';
             Exsr WriteWorkFile;
             Leavesr;
          Endif;

          xxProcessed = ' ';

          Dow Not %Eof;

             If NAName <> ' ' and NANamp <> 'P' and NAEncd <> 999;
                FMnm01 = xxName;
                FM2001 = xxName;
                FMc401 = NAMAD1;
                FMc402 = NAMAD2;
                x1Zip = NAMzip;
                If x1Zip2 <> 0;
                   FMc403 = %Trim(NAMCTY) + ', ' +
                            %Trim(NAMST) + '  ' +
                            %Trim(%editc(x1Zip1:'X')) + '-' +
                            %Trim(%editc(x1Zip2:'X'));
                Else;
                   FMc403 = %Trim(NAMCTY) + ', ' +
                            %Trim(NAMST) + '  ' +
                            %Trim(%editc(x1Zip1:'X'));
                Endif;
                If FMc402 = ' ';
                   FMc402 = FMc403;
                   FMc403 = ' ';
                Endif;
                FMFein = NAFein;
                FMc201 = %editc(NAENCD:'X');
                xxProcessed = 'Y';
                Exsr WriteWorkFile;
             Endif;

             Reade(n) (xxCo#:xxFnd:xxEmp#:xxFyr:SHUnid) pna00;
          Enddo;

          If xxProcessed = ' ';
             dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
             FMnm01 = dsCltInfo.name;
             FM2001 = dsCltInfo.name;
             FMc401 = dsCltInfo.address1;
             FMc402 = dsCltInfo.address2;
             FMc403 = dsCltInfo.address3;
             FMFein = 0;
             If dsCltInfo.fein <> ' ';
                FMFein = %int(dsCltInfo.fein);
             Endif;
             FMc201 = '001';
             Exsr WriteWorkFile;
             Leavesr;
          Endif;

       Endsr;
       //===================================================
       //  PGMSTR - Beginning of program processing
       //===================================================
       Begsr Pgmstr;
          writeInCopybook = *Off;
          useDfltAddparms = *Off;
       Endsr;
       //======================================================
       // PGMEND - End of program processing
       //======================================================
       Begsr pgmend;

       Endsr;
       //======================================================
       // VALID - Subroutine to attach this form to the policy
       //======================================================
       Begsr valid;
          crtFrm = *on;
       Endsr;
      /End-Free
