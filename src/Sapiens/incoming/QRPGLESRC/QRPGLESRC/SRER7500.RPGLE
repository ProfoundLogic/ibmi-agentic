     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SRER7500 - Notice of Election to Accept or Reject Deductible
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  ER07500                                                      *
     *    Notice of Election to Accept or Reject Deductible          *
     *    Must be Large Deductible and Deleware                      *
     *                                                               *
     *  Print/Attach for Multiple Manual with Additional Input       *
     *  Endorsement (data stored in file specifed for endorsement)   *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * Revisions:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
     *                                                               *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*
     ** Copybook F specs
     F/COPY SCOPYEN01F
     fwtabl     if   e           k disk    Include(pbs01) Rename(pbs01:pbs00)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D SRER7500        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SRER7500        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     D* Determine if Large or Small Deductible
     D RtvDedType      pr             1                                         Returns S/L

     ** Copybook D specs

     D/COPY SCOPYEN01D
     D/COPY WPRPEO
     D*** Beg Del *** 09/20/24 *****************************************
     D***/Copy WprEmplr
     D*** End Del *** 09/20/24 *****************************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4

     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqform          s             15a   inz('ER07500   ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')

     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C

      /Free
       //========================================================================
       //  OthInf - Subroutine to load form specific information
       //========================================================================
       BegSR OthInf;

       EndSR;
      //=======================================================================
      // PgmStr - Beginning of program processing
      //=======================================================================
       BegSR PgmStr;

          RunningSRText = *On;

       EndSR;
      //=======================================================================
      // PgmEnd - End of program processing
      //=======================================================================
       BegSR PgmEnd;

       EndSR;
      //=======================================================================
      // Valid - Subroutine to attach this form to the policy
      //=======================================================================
       Begsr Valid;

          Chain (Elco#:ELrtcd) prs01;
          If %Found(wmrsl);
             If (RSjrst = ENst or ENst = *blanks);
                If RtvDedType() <> 'L';
                   crtfrm = *on;
                Endif;
             Endif;
          Endif;

       Endsr;
      /End-Free
      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     p RtvDedType      b
     d RtvDedType      pi             1

     d billCode        s                   like(BSkey)
      /Free

       billCode = %EditC(ELbsCd:'X');
       Chain (ELco#:billCode) pbs00;
       If %Found(wtabl);
          Select;
          When BSlpgm = 'L';
             Return 'L';
          Other;
             Return 'S';
          Endsl;
       Else;
          Return ' ';
       Endif;

      /End-Free
     p RtvDedType      e
