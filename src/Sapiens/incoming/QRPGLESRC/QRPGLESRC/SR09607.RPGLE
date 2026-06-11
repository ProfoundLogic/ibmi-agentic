     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H*option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR09607  - FL Guaranty Association Surcharge Endorsement
      //===============================================================*
      // WC090607 - FL Guaranty Association Surcharge Endorsement      *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Created   : 10/20/17                                          *
      // Programmer: C:267                                             *
      // Project   : WC2533                                            *
      //             Cloned SR42312                                    *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/03/18  C:33  Support: 72468                                *
      //                 Use surchage value from FL division.          *
      //                                                               *
      // 01/03/18  C:65  Support: 51727                                *
      //                 Fix to use a larger field for work file.      *
      //                                                               *
      // 12/18/17  C:33  Support: 51727                                *
      //                 Populate the workfile without additional entry*
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
      //* Additional Information File
     FWxfmp1    if   e           k disk    Usropn
     F                                     Rename(Pfm01:Pfm02)

      //=========================================================================
      //  /COPY required constants
      //=========================================================================
     D qqForm          s             15A   Inz('WC090607  ')
     D addpgm          c                   const('SR09607I  ')
     D x1Fnd           s              3  0 Inz
     D xxZIPC          s              9a
     D xxZIP5          s              5a
     D xxZIP4          s              4a

      /COPY SCOPYEN01D
      /copy wprpeo

     D SR09607         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR09607         PI
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

        //***  Begin Add  ***  12/18/17  **********************************
        Select;
        When cltId = '150';
        //***  Begin Add  ***  10/03/18  **********************************
           Clear EL1aPr;
           Setll elky2 pel02;
           Dou %eof(wdell1);
              Reade elky2 pel02;
              Select;
              When %eof;
                 Iter;
              Endsl;
              Clear RSjrSt;
              Chain rsky prs01;
              If RSjrSt = ENst;
                 Leave;
              Endif;
           Enddo;
        //***  End   Add  ***  10/03/18  **********************************
        //***  begin chg ****  01/03/18  **********************************
        //*****   FMA541 = EL1APR;
           FMAM01 = EL1APR;
        //***  end chg ******  01/03/18  **********************************
           Exsr WriteWorkFile;
           Leavesr;
        Endsl;
        //***  End   Add  ***  12/18/17  **********************************

        // Open the files if Closed
        If not %Open(Wxfmp1);
           Open Wxfmp1;
        Endif;

        Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) Wxfmp1;
        If %Found(Wxfmp1);
           Chain (Elco#:ELrtcd) prs01;
           If RSjrst = ENst;
              FMA541 = F3A541;
              Exsr WriteWorkFile;
           Endif;
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
           If RSjrst = ENst;
              crtFrm = *On;
           EndIf;
       Endsr;

      /End-Free
