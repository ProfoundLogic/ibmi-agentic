     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR32602 - Pro-rata Cancellation Endorsement
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 11/03/17                                          *
      // Programmer: C:291                                             *
      // Project   : WC2525                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      *                                                                *
      *    10/19/22 C:342 Project: WC2834 (Recompile)                  *
      *                   Endorsement Recognition of Cancellation      *
      *                                                                *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D

     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
     d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      // employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
     d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

     D qqform          s             15A   Inz('WC320602  ')
     D addpgm          c                   const('SRXXXXXXXX')


      *---------------------------------------------------------------------
      * Prototype Declaration
      *---------------------------------------------------------------------
     D SR32602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1


     D SR32602         PI
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
       begsr othinf;

             FM2001 = 'Cancelled by Employer';
             FM2002 = 'Coverage placed elsewhere';
             FM2003 = 'Duplicate coverage';

       endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
       endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
          crtfrm = *on;
       endsr;
      /End-free
