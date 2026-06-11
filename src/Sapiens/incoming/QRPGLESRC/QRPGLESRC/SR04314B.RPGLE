     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
    H/TITLE SR04314B - Agreement Limiting and Restricting this Insurance
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  WC040314B                                                    *
     *    Labor Contractor As Named Insured                          *
     *    Restriction of coverage to Client Workers                  *
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
     * 03/02/17 C:180 PROJECT WC2481                                 *
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

      * Agreement Limiting and Restricting Endorsement Form Data
     FWLL1L     if   e           k disk

     ** Copybook F specs
     F/COPY SCOPYEN01F


     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR04314B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04314B        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1




     ** Copybook D specs

     D/COPY SCOPYEN01D

     d nam1            ds            60
     d  fmc401                             overlay(nam1:1)
     d  fmc201                             overlay(nam1:41)

     d nam2            ds            60
     d  fmc402                             overlay(nam2:1)
     d  fmc202                             overlay(nam2:41)

     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D QQFORM          c                   const('WC040314B ')

     ** Additional entry program
     D addpgm          c                   const('SR04314I  ')


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     C     l1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#


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
     C     l1ky          setll     pl101
     C     l1ky          reade     pl101                                  60
    
     C                   dow       not %eof
      *
      * move information to WXFMP fields to print on endorsement
      *
     C                   movel(p)  l1nam1        nam1
     C                   movel(p)  l1nam2        nam2
     C                   movel(p)  l1adr1        fmnm01
     C                   movel(p)  l1adr2        fmnm02
     C                   movel(p)  l1adr3        fmnm03
     C                   movel(p)  l1adr4        fmnm04
     /Free
       Exsr WriteWorkfile;
     /End-Free
      *
      *  clear data fields
      *
     C                   clear                   fmc601
     C                   clear                   fmc602
     C                   clear                   fmnm01
     C                   clear                   fmnm02
     C                   clear                   fmnm03
     C                   clear                   fmnm04
      *
     C     l1ky          reade     pl101                                  60
     C                   enddo
    
     C                   endsr


     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       UseDfltAddParms = *OFF;
       WriteInCopyBook = *OFF;
      /End-Free
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

