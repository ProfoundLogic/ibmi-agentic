     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR32306  - NC -Professional Employer Organization Extension Endorsement
      //===============================================================*
      // WC320306 - North Carolina Professional Employer Organization  *
      //            (PEO) Extension Endorsement                        *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
     // 10/06/17 C:136 Project: 150WBL                                *
     //                PEO attachment criteria moved to copy code     *
     //                                                               *
      // 09/11/17 C:291 Project: WC2525                                *
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

      /COPY SCOPYEN01F
      /COPY SCOPYEN01D
      /copy wprpeo

     D qqForm          s             15A   Inz('WC320306  ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR32306         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR32306         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       Begsr othinf;

           //For each employer division

           setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

           Dow not %eof;
             Chain (Elco#:ELrtcd) prs01;

             Select;
             When FNpeof = 'M' and ELemp# = 1;
             When FNpeof = 'S' and ELdiv = 0;
             Other;
               If RSjrst = ENst;
                   dsPEOInfo = GetPEOInfo(ELco#:ELfnd);
                 //FM2001 = dsPEOInfo.fedName;
                   FM2001 = dsPEOInfo.name;
                   FMead1 = dsPEOInfo.address1;
                   FMead2 = dsPEOInfo.address2;
                   FMead3 = dsPEOInfo.address3;

                   dsCltInfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                   FM2002 = dsCltInfo.name;

                   //State where work performed
                   FMc101 = Rsjrst;

                   //Check box: Direct workers of the named insured on the policy are provided cov
                   If EMLSID = '2';
                      Fmfl01 = 'Y';
                   Else;
                      Fmfl01 = ' ';
                   Endif;

                   Exsr WriteWorkFile;
               Endif;
             Endsl;

             reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           Enddo;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
        WriteInCopybook = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;
       crtFrm = *On;
       Endsr;
      /End-Free
