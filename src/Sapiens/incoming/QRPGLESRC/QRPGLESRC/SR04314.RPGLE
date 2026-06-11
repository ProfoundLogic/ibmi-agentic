     * (C) Copyright FISERV Insurance Solutions, Inc., 2008
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR04314 - Agreement Limiting and Restricting
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  WC040314                                                     *
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
     * 06/09/08 C:61  PROJECT 136WDS                                 *
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
     D qqform          c                   const('WC040314  ')

     ** Additional entry program
     D addpgm          c                   const('SR04314I  ')


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
      *                                     wdelp
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1


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
     C                   z-add     1             o                 1 0
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
    
     C                   write     pfm01
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

