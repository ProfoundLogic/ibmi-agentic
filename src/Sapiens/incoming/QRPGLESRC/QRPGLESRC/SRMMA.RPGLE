     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     *
    H/TITLE SRxxxxxx - xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx Print/Attach
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  Print/Attach for Multiple Manual with Additional Input       *
     *  Endorsement (data stored in file specifed for endorsement)   *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * xx/xx/xx C:xx  PROJECT xxxxxx                                 *
     *                Created.                                       *
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

      * Endorsement data
     Fwledl     if   e           k disk

     ** Copybook F specs
     F/COPY SCOPYEN01F


     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##view          s              1  0
     d kkco#           s                   like(fmco#)
     d kkfnd           s                   like(fmfnd)
     d kkemp#          s                   like(fmemp#)
     d kkdiv           s                   like(fmdiv)
     d kkfyr           s                   like(fmfyr)


     ** Copybook D specs

     D/COPY SCOPYEN01D


     *=========================================================================
     *  Copybook required constants
     *=========================================================================

      * zzz - change to match endorsement name

     ** Form ID
     D xxform          c                   const('WCxxxxxx  ')

      * zzz - change to match "addtional information "work with" program

     ** Additional entry program
     D addpgm          c                   const('SRMMAI    ')


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    qqmode
      *                                     wdelp
     C                   parm                    qqrrn
     C                   parm                    qqmod2
    
     ** Additional input program parameters
    
     C     addpl         pList
     C                   parm      *off          ##kc
     C                   parm      *off          ##kl
     C                   parm      qqmod2        ##call
      *                                     additional input data file
     C                   parm      0             ##rrn
      *                                     wdelp
     C                   parm      qqrrn         ##rrn2
     C                   parm                    xkform


     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     C     addky         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    xkform


     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C


     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
    
     C                   z-add     1             i                 1 0
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqemp#        kkemp#
     C                   move      qqdiv         kkdiv
     C                   move      qqfyr         kkfyr
      *
     C     addky         setll     ped01
     C     addky         reade     ped01                                  60
    
     C                   dow       not %eof
     C                   write     pfm01
     C     addky         reade     ped01                                  60
     C                   enddo
    
     C                   endsr


     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr


     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr


     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
      ** This is a manual attach endorsement with additional input

     C                   move      *on           crtfrm
    
     C                   endsr


