     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR32305  - NC -Professional Employer Organization Exclusion Endorsement
      //===============================================================*
      // WC320305 - North Carolina Professional Employer Organization  *
      //            (PEO) Exclusion Endorsement                        *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 09/08/17 C:291 Project: WC2525                                *
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

     F/COPY SCOPYEN01F
      ** Additional Information File
     FWxfmp1    if   e           k disk    Usropn
     F                                     Rename(Pfm01:Pfm02)

      *=========================================================================
      *  /COPY required constants
      *=========================================================================
     D qqForm          s             15A   Inz('WC320305  ')
     D addpgm          c                   const('SR32305I  ')
     D x1Fnd           s              3  0 Inz

      /COPY SCOPYEN01D
      /copy wprpeo

     D SR32305         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR32305         PI
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

        // Open the files if Closed
        If not %Open(Wxfmp1);
           Open Wxfmp1;
        Endif;

        Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) Wxfmp1;
        If %Found(Wxfmp1);
           x1Fnd = f3seq#;

           //Read all the records in employer master for selected group
           setll (xxco#:x1fnd) pem01;
           reade (xxco#:x1fnd) pem01;
           Dow not %eof(wmeml);
            If ememp# = 1 and emdiv = 0;
            Else;
               setll (emco#:emfnd:xxfyr:ememp#:emdiv) pel02;
               reade (emco#:emfnd:xxfyr:ememp#:emdiv) pel02;
               If not %eof(wdell1);
                  Chain (Elco#:ELrtcd) prs01;
                  If RSjrst = ENst;
                    dsCltInfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                    FM2002 = dsCltInfo.name;
                    FMead1 = dsCltInfo.address1;
                    FMead2 = dsCltInfo.address2;
                    FMead3 = dsCltInfo.address3;
                    Exsr WriteWorkFile;
                  Endif;
               Endif;
            Endif;
           reade (xxco#:x1fnd) pem01;
           Enddo;
        Endif;

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
