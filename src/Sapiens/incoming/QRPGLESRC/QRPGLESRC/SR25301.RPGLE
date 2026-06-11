     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR25301 - MT Employee Leasing/Professional Employer End.       er Policy Endorsement
      //===============================================================*
      // WC250301  - Montana Employee Leasing/Professional Employer    *
      //             Endorsement                                       *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/06/17 C:062 Project: 150WAP                                *
      //                Remove peo stuff from valid                    *
      //                                                               *
      // 07/25/17 C:180 Project: 150WAP                                *
      //                Handle groups with combined PEOs.              *
      //                                                               *
      // 03/02/17 C:267 Project: WC2481                                *
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
      /COPY wPRPEO

     D qqForm          s             15A   Inz('WC250301  ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR25301         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR25301         PI
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

           setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

           Dow not %eof;
             Chain (Elco#:ELrtcd) prs01;
               If RSjrst = ENst;
                  dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                  FM2001 = dsCltInfo.fedName;
                  FM2002 = dsCltInfo.name;
                  FMead1 = dsCltInfo.address1;
                  FMead2 = dsCltInfo.address2;
                  FMead3 = dsCltInfo.address3;
                  Exsr WriteWorkFile;
               Endif;
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
       //*** Begin Chg *** 10/06/17 *********************************************
       //Exsr GetFnd;
       //Select;
       //When FNpeof = *blank;
       //   crtFrm = *Off;
       //When FNpeof = 'S' and ELdiv <> 0;
       //   crtFrm = *On;
       //When FNpeof = 'M' and ELemp# <> 1;
       //   crtFrm = *On;
       //
       ////*** Begin Add *** 07/25/17 *******************************************
       //// Combined PEO Group - Master PEO client
       //When FNpeof = 'C' and ELemp# = 1 and ELdiv <> *Zero;
       //   Crtfrm = *on;
       //
       //// Combined PEO Group - MCP client
       //When FNpeof = 'C' and ELemp# <> 1;
       //   Crtfrm = *on;
       ////*** End   Add *** 07/25/17 *******************************************
       //
       //Other;
       //   crtFrm = *off;
       //Endsl;
         crtFrm = *On;
       //*** End   Chg *** 10/06/17 *********************************************
       Endsr;

      /End-Free
