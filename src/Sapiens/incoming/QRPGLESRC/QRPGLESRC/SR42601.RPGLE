     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR42601 - TX - Notice of Material Change
      *===============================================================*
      * WC4206001- Texas Notice of Material Change Endorsement        *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 12/17/99                                          *
      * Programmer: C:90                                              *
      * Project   : CM027                                             *
      *                                                               *
      * Rewrite   : 04/10/00                                          *
      * Programmer: C:90                                              *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
     *                                                               *
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
     * 02/15/19 C:300 Support: 77030 (LUBA) (Recompile)              *
      *                Printing end from Work with End Forms
      *                Changes in SCOPYEN01C copybook
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                               *
      * 02/12/15 C:282 Project WC2335                                 *
      *                Replaced the file WXFMP1 with WLEDL to print   *
      *                the mutiple entities.                          *
      *                                                               *
      * 05/23/13 C:190 Support 82315                                  *
      *                Corrected pgm error while calling additional   *
      *                input pgm.                                     *
      *                                                               *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
     FWledl     if   e           k disk    Prefix(fm2:2)
     F                                     Rename(ped01:ped00)
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC420601  ')

      ** Additional entry program
     D ADDPGM          C                   CONST('SR42601I  ')

     D/COPY SCOPYEN01D

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      *entry parameters
     D SR42601         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42601         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     C     keyWledl      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
     C                   kfld                    xxFrm1
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
     C     FMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     OTHINF        BEGSR
      *
     C                   MOVEL(P)  XXFORM        XXFRM1           15

      /Free
       WriteInCopybook = *Off;

       Setll keyWledl ped00;
       Reade keyWledl ped00;

          Dow Not %Eof(wledl);
          FMa701 =  FM2a701;
          FMc401 =  FM2c401;
          FMc402 =  FM2c402;
          FMc201 =  FM2c201;
          FMc101 =  FM2c101;
          FMc102 =  FM2c102;
          FMc103 =  FM2c103;

          // Write the output record in WXFMP.
          Exsr WriteWorkFile;

          Reade keyWledl ped00;
          EndDo;
      /End-Free
     C*
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      *
      *
     C                   ENDSR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
      *
      *
     C                   ENDSR
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
      *
     C                   ENDSR
