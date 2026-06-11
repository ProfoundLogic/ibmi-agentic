     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR32307  - NC -Professional Employer Organization Client Exclusion Endorsement
      //===============================================================*
      // WC320307 - North Carolina Professional Employer Organization  *
      //            (PEO) Client Exclusion Endorsement                 *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/19/22 C:342 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 09/19/17 C:291 Project: WC2525                                *
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
     D qqForm          s             15A   Inz('WC320307  ')
     D addpgm          c                   const('SR32307I  ')
     D x1Fnd           s              3  0 Inz
     D xxZIPC          s              9a
     D xxZIP5          s              5a
     D xxZIP4          s              4a

      /COPY SCOPYEN01D
      /copy wprpeo

     D SR32307         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR32307         PI
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
           setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           Dow not %eof;
             //***** Beg Add *** 10/19/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
             //***** End Add *** 10/19/22 ******************************
             Chain (Elco#:ELrtcd) prs01;
             If RSjrst = ENst;
                FM2002 = F3EMNM;
                FMead1 = F3EAD1;
                FMead2 = F3EAD2;
                xxZIPC = %editc(F3FEIN:'X');
                xxZIP5 = %subst(xxZIPC:1:5);
                xxZIP4 = %subst(xxZIPC:6:4);
                FMead3 = %Trim(F3C301) + ', ' +
                         %Trim(F3EMST) + '  ' +
                         %Trim(xxZIP5) + '-' +
                         %Trim(xxZIP4);

                Exsr WriteWorkFile;
             Endif;
             //***** Beg Add *** 10/19/22 ******************************
             Endif;
             //***** End Add *** 10/19/22 ******************************
           reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
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
