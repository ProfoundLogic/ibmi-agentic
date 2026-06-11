     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR42315  - TX Advanced Notice of Cancellation to the PEO
      //===============================================================*
      // WC420315 - TX Advanced Notice of Cancellation to the PEO      *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // Created   : 09/21/17                                          *
      // Programmer: C:267                                             *
      // Project   : WC2527                                            *
      //             Copied from SR42316                               *
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
     D qqForm          s             15A   Inz('WC420315  ')
     D addpgm          c                   const('SR42315I  ')
     D x1Fnd           s              3  0 Inz
     D xxZIPC          s              9a
     D xxZIP5          s              5a
     D xxZIP4          s              4a

      /COPY SCOPYEN01D
      /copy wprpeo

     D SR42315         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR42315         PI
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
         //Chain (Elco#:ELrtcd) prs01;
         //If RSjrst = ENst;
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
              FMA701 = F3A701;

              Exsr WriteWorkFile;
         //Endif;
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
