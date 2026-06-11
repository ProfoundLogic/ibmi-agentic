     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR43302A - UT Employee Leasing Endorsement
      //===============================================================*
      // WC430302A - UT Employee Leasing Endorsement                   *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 11/04/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*
      /Copy sCopyEn01F
      //***** Beg Del *** 11/04/22 **********************************
      ***/Copy WprEmplr
      //***** End Del *** 11/04/22 **********************************
      /Copy sCopyEn01D
      /Copy WprPeo
     D qqform          s             15A   Inz('WC430302A ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR43302A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR43302A        PI
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
