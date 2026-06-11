     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00101A - ENDORSEMENT ATTACH/PRINT PROGRAM
      *===============================================================*
      * WC000101A - Defense Base Act Coverage Endorsement             *
      *                                                               *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * CREATED   : 06/28/99                                          *
      * PROGRAMMER: C:113                                             *
      * PROJECT   : CM027                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      //                                                               *
      // 10/18/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * NOTES:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      *                                                               *
      *  qqmode = 1 - Called to determine if form should be attached. *
      *  qqmode = 2 - Called to print endorsement.                    *
      *  qqmode = 3 - Called for additional input.                    *
      *  qqmode = 4 - Called for midterm processing.                  *
      *===============================================================*
      ** Additional Input File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)

      ** Copybook F specs
     F/COPY SCOPYEN01F
      ** Copybook D specs
     D/COPY SCOPYEN01D
      ** Form ID
     D qqForm          s             15A   Inz('WC000101A ')

      ** Additional entry program
     D addpgm          c                   const('SR00101I  ')

     D SR00101A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00101A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      ** Additional Input file
     C     fmKey3        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
      ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        Begsr

     C                   Open      wxfmp1
     C     fmKey3        Chain     pfm02                              60
      ** Move the custom Fields to print
     C                   Movel     f3nm01        fmnm01
     C                   Movel     f3nm02        fmnm02
     C                   Movel     f3nm03        fmnm03
     C                   Movel     f3nm04        fmnm04
     C                   Movel     f3nm05        fmnm05
     C                   Movel     f3nm06        fmnm06
     C                   Movel     f3nm07        fmnm07
     C                   Movel     f3nm08        fmnm08
     C                   Movel     f3nm09        fmnm09
     C                   Movel     f3nm10        fmnm10

     C                   Close     wxfmp1

     C                   Endsr
      *=========================================================================
      *  PGMSTR - BeginniNg of program processing
      *=========================================================================
     C     pgmstr        Begsr
     C                   Endsr
      *=========================================================================
      *  PGMEND - End of Program processing
      *=========================================================================
     C     pgmend        Begsr
     C                   Endsr
      *=========================================================================
      *  VALID - SubroutiNe to attach this form to the policy
      *=========================================================================
     C     valid         Begsr
      ** This is a manual attach endorsement.
     C                   Move      *on           crtfrm
     C                   Endsr
