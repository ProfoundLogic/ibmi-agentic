     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
     *
    H/TITLE SR049903 - CA - Notice Required by Law
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/04/04                                          *
     * Programmer: C: 61                                             *
     * Project   : 132WAC                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/15/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 02/24/17 C:267 Project SY371 scriptura - WC2481               *
      *                Add Scriptura interface.                       *
     *                Cleanup code                                   *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     ** Form ID
      *** Beg Chg *** 02/24/17 *************************************************
      *** xxform          c                   const('PN049903  ')
     D QQFORM          S             15A   inz('PN049903  ')
      *** End Chg *** 02/24/17 *************************************************

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      **** Beg Add **** 02/24/17 ***********************************************
      * Entry
     D SR049903        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR049903        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End Add **** 02/24/17 ***********************************************

      **** Beg Dlt **** 02/24/17 ***********************************************
     ** Entry parameters
      **   *entry        plist
      **                 parm                    qqco#
      **                 parm                    qqfnd
      **                 parm                    qqemp#
      **                 parm                    qqdiv
      **                 parm                    qqfyr
      **                 parm                    qqmode
     **
     ** Additional input program parameters
      **   addpl         pList
      **                 parm                    qqprds
      **                 parm      qqmod2        qqcall
      **                 parm                    qqco#
      **                 parm                    qqfnd
      **                 parm                    qqemp#
      **                 parm                    qqdiv
      **                 parm                    qqfyr

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     ** Additional Input file
      **   fmky3         klist
      **                 kfld                    xxco#
      **                 kfld                    xxfnd
      **                 kfld                    xxfyr
      **                 kfld                    xxemp#
      **                 kfld                    xxdiv
      **                 kfld                    xkform

      **** End Dlt **** 02/24/17 ***********************************************
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
      **** Beg Chg **** 02/24/17 ***********************************************
      /Free
      //=======================================================================
      //  OTHINF-Subroutine to load form specific information & write record
      //=======================================================================
       Begsr othinf;
         open  wxfmp1;
         chain (xxco#:xxfnd:xxfyr:xxemp#:xxdiv:xxform) pfm02;

         If %found;
          // Move the custom fields to print
           fmdt01 =  f3dt01;
           fmdt02 =  f3dt02;
           fmdt03 =  f3dt03;
           fmc401 =  f3c401;

          // Write the output record
           write     pfm01;

        EndIf;

         close     wxfmp1;

       EndSr;

      //=======================================================================
      //  PGMSTR - Beginning of program processing
      //=======================================================================
       BegSr pgmstr;
         RunningSRtext = *ON;
       EndSr;

      //=======================================================================
      //  PGMEND - End of program processing
      //=======================================================================
       BegSr pgmend;

       EndSr;

      //=======================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=======================================================================
       BegSr valid;
         crtfrm = *on;
       EndSr;

       //*** End Chg **** 02/24/17 *********************************************
