     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR05403 - CO PREMIUM CREDIT FOR CERTIFIED RISK MANAGEMENT
       //===============================================================*
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
       // 10/31/22 C:333 Project: WC2834 (Recompile)                    *
       //                Endorsement Recognition of Cancellation        *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
       //****************************************************************

      /COPY SCOPYEN01F

     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC050403  ')
     D addpgm          c                   const('SR05403I  ')

     D SR05403         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR05403         PI
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
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr othinf;
          open wxfmp1;
          chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) wxfmp1;
          FMa521 = F3a521;
          FMc401 = F3c401;
          close wxfmp1;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
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

      /End-free
