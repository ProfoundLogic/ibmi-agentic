     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
     *
    H/TITLE SR24406D- MO Employer Paid Medical Endorsement
      ** form Edition (Ed. 8-16)
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 03/01/17                                          *
     * Programmer: C: 267                                            *
     * Project   : WC2481                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 11/03/22 C:333 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
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
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D

      ** Excess Loss Split Point percentage
      *ExcLosSpltPt    c                   const('20%')
     D ExcLosPct       S              5  4 INZ (0.2000)

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     ** Form ID
     D QQFORM          S             15A   inz('WC240406D ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      * Entry
     D SR24406D        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR24406D        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
      /Free
      //=======================================================================
      //  OTHINF-Subroutine to load form specific information & write record
      //=======================================================================
       Begsr othinf;

           // calculate % of Excess loss split point premium
           Eval(h) FMPREM = Elenmp * ExcLosPct;
           FMA701 = ExcLosPct * 100;

       EndSr;
      //=======================================================================
      //  PGMSTR - Beginning of program processing
      //=======================================================================
       BegSr pgmstr;
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
         crtFrm = *On;
       EndSr;
