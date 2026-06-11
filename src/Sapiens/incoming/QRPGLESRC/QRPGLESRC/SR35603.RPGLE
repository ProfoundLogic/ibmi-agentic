     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR35603 - OK MERIT RATING ENDORSEMENT
       //===============================================================*
       // WC350603A - Oklahoma Fraud Warning  Endorsement               *
       //                                                               *
       //    PROGRAM DESCRIPTION:                                       *
       //                                                               *
       //       THIS PROGRAM IS CALLED TO:                              *
       //         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
       //            ATTACHED TO THE POLICY.                            *
       //         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
       //            (WXFMP) PRIOR TO A FORM MERGE.                     *
       //                                                               *
       //                                                               *
       //****************************************************************
       //                                                               *
       // REVISIONS:                                                    *
       //                                                               *
       // 10/19/22 C:342 Project: WC2834 (Recompile)                    *
       //                Endorsement Recognition of Cancellation        *
       //                                                               *
       // 01/03/17 C:180 Project: SY371                                 *
       //                Indicate this is a text-only endorsement.      *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
       //****************************************************************

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D
     D qqForm          s             15A   Inz('WC350603  ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR35603         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR35603         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /COPY SCOPYEN01C

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
      /Free
       Begsr othinf;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
         RunningSRtext = *On;
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
          CrtFrm = *On;
       Endsr;
      /End-free
