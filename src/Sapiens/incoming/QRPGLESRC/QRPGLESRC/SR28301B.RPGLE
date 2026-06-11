     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     H/TITLE SR28301B - NH - WC And Employers Liability Coverage Endorsement
      *===============================================================*
      * WC280301B - New Hampshire WC and Employers Liability Coverage  *
      *                                                                *
      * Program Description:                                           *
      *                                                                *
      *   Determines if this endorsement form should be attached to    *
      *   the policy and creates data in the Endorsement Form          *
      *   Workfile (WXFMP) prior to a form  merge.                     *
      *                                                                *
      * Created   : 02/03/15                                           *
      * Programmer: C:295                                              *
      * Project   : WC2315                                             *
      *================================================================*
      * Revisions:                                                     *
      *                                                                *
     * 11/03/22 C:333 Project: WC2834 (Recompile)                     *
      *                Endorsement Recognition of Cancellation
     H*                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                *
      * -------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.            *
      * -------------------------------------------------------------- *
      *=============================================================== *
      * Notes:                                                         *
      *                                                                *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).           *
      *                                                                *
      *  QQMODE = 1 - Called to determine if form should be attached.  *
      *  QQMODE = 2 - Called to print endorsement.                     *
      *  QQMODE = 3 - Called for additional input.                     *
      *  QQMODE = 4 - Called for mid-term processing.                  *
      *================================================================*
      ** Copybook F specs
     F/Copy SCOPYEN01F
      *
      ** Group Detail File
     FWdf1l     if   e           k disk    Usropn
      *
      ** Additional Information File
     FWxfmp1    if   e           k disk    Usropn
     F                                     Rename(Pfm01:Pfm02)
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC280301B ')

      ** Additional entry program
     D AddPgm          C                   CONST('SR28301BI ')

     D/COPY SCOPYEN01D

     *=========================================================================
     *  Plist definitions
     *=========================================================================

    D* *Entry
     D SR28301B        pr                  ExtPgm('SR28301B')
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR28301B        pi
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /Free
       // Copybook C specs
        /COPY SCOPYEN01C
       //=========================================================================
       //  OthInf - Subroutine to load form specific information & write record
       //=========================================================================
         Begsr OthInf;

            WriteInCopybook = *Off;

            // Open the files if Closed

            If not %Open(Wxfmp1);
               Open Wxfmp1;
            Endif;
            If not %Open(Wdf1l);
               Open Wdf1l;
            Endif;

            Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) Wxfmp1;
            If %Found(Wxfmp1);
               Chain (xxCo#:xxFnd:xxFyr) Wdf1l;
               If %Found(Wdf1l);
                  // Move the custom fields to print

                  FMa701  = F3a701;
                  FMa521  = F3a521;
                  FMa921  = F3a921;
                  FMa702  = F3a702;
                  FMa522  = F3a522;
                  FMa922  = F3a922;

                  // Write the output record
                  Exsr WriteWorkFile;
               Endif;
            Endif;

         // Close the files
            If %Open(Wxfmp1);
               Close Wxfmp1;
            Endif;
            If %Open(Wdf1l);
               Close Wdf1l;
            Endif;
         Endsr;
       //=======================================================================
       //  PgmStr - Beginning of program processing
       //=======================================================================
        Begsr PgmStr;
        Endsr;
       //=======================================================================
       //  PgmEnd - End of program processing
       //=======================================================================
        Begsr PgmEnd;
        Endsr;
       //=======================================================================
       // Valid - Subroutine to attach this form to the policy
       //=======================================================================
         Begsr Valid;
           CrtFrm = *on;
         EndSr;
