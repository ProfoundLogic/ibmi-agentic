     HCopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR43302  - ENDORSEMENT ATTACH/PRINT PROGRAM
      //===============================================================*
      // WC430302  - UT Employee Leasing Endorsement                   *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
      //                                                               *
     // 10/06/17 C:136 Project: 150WBL                                *
     //                PEO attachment criteria moved to copy code     *
     //                Use longer field in workfile for name          *
     //                                                               *
      // 07/25/17 C:180 Project: 150WAP                                *
      //                Handle groups with combined PEOs               *
      //                                                               *
      // 03/24/17 C:180 Project: SY371                                 *
      //                Fix fein number error.                         *
      //                                                               *
      // 10/12/16 C:267 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      // ------------------------------------------------------------- *
      //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      // ------------------------------------------------------------- *
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
      /Copy sCopyEn01F
     D*** Beg Del *** 09/20/24 *****************************************
     D*** /Copy WprEmplr
     D*** End Del *** 09/20/24 *****************************************
      /Copy sCopyEn01D
      /Copy WprPeo
     D qqform          s             15A   Inz('WC430302  ')
     D addpgm          c                   const('SRXXXXXXXX')
     D SR43302         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR43302         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /Copy sCopyEn01C
      //=========================================================================
      //  OTHINF - Subroutine to load form specific information
      //=========================================================================
      /Free
       Begsr OthInf;
       dsPeoInfo = GetPeoInfo(ELco#:ELfnd);
       FMc401    = dsPeoInfo.name;
       FM2001    = dsPeoInfo.name;
       FMc402    = dsPeoInfo.address1;
       FMc403    = dsPeoInfo.address2;
       FMc404    = dsPeoInfo.address3;
       If  dsPeoInfo.fein <> *blanks;
           FMfein  = %dec(dsPeoInfo.fein:9:0);
       Else;
          FMfein = *Zeros;
       Endif;
       Endsr;
       //=======================================================================
       //  PGMSTR - Beginning of program processing
       //=======================================================================
       Begsr PgmStr;
       Endsr;
       //========================================================================
       //  PGMEND - End of program processing
       //========================================================================
       Begsr PgmEnd;
       Endsr;
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
       crtFrm = *On;
       Endsr;
      /End-Free
