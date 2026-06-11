     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     *
    H/TITLE SR02301  - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC020301  - EMPLOYEE LEASING ENDORSEMENT                      *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 12/14/06                                          *
     * PROGRAMMER: C:184                                             *
     * PROJECT   : WC1505                                            *
     *                                                               *
     *===============================================================*
     * Revisions:                                                    *
      *                                                               *
      * 12/29/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     * ------------------------------------------------------------- *
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

     Fwniel2    if   e           k disk
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      **** Begin Add **** 12/29/16 *********************************************
      * Entry
     D SR02301         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR02301         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/29/16 *********************************************
     D/COPY WWCPOLDAT4
     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)

      **** Begin Delete *** 12/29/16 *******************************************
     D*** ##call          s              1
     D*** ##kc            s              1
     D*** ##kl            s              1
     D*** ##rrn           s              9  0
     D*** ##rrn2          s              9  0
     D*** ##view          s              1  0
      **** End   Delete *** 12/29/16 *******************************************
     D xxefdt          s                   like(fmeedt)
     D xxuniq          s              1
     D xxactive        s              1
     D x1              s              3  0
     D x2efdt          s                   like(emefd1)
     D x2cxdt          s                   like(emcad1)
     D x2rndt          s                   like(emcad1)

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
      *** Begin Change *** 12/29/16 ********************************************
     D*** xxform          c                   const('WC020301  ')
     D QQFORM          S             15A   inz('WC020301  ')
      *** End   Change *** 12/29/16 ********************************************

     ** Additional entry program
     D addpgm          c                   const('SR02301M  ')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
      ** Begin Delete *******   12/29/16 ***************************************
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqrrn
     C***                parm                    qqmod2
    
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm      *off          ##kc
     C***                parm      *off          ##kl
     C***                parm      qqmod2        ##call
     C***                parm      0             ##rrn
     C***                parm      qqrrn         ##rrn2
     *** End   Delete *******   12/29/16 ***************************************
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Alternate Employer file
     C     aeKey         klist
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
     *
     ** Write the output record
     C     aeKey         setll     PIE01
     C     aeKey         reade     PIE01                                  60
    
     C                   Dow       *in60 = *off
     C                   If        iefein = *Zeros
    
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt
     C
     C                   movel     IECNAM        fmc401
     C                   movel     ieadr1        fmc402
     C****** BEG ADD *** 05/27/08 ******************************************************
     C                   movel     ieadr2        fmc403
     C****** BEG ADD *** 05/27/08 ******************************************************
     C
     C                   eval      srdti = ieefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fmeedt
    
     C                   move      iezip         xxzip
     C     iecty         cat       ',':0         fmc404
     C                   cat       iest:1        fmc404
     C                   cat       xxzip1:1      fmc404
    
     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc404
     C                   cat       xxzip2:0      fmc404
     C                   endif

     c                   if        fmc403 = *blank
     c                   eval      fmc403 = fmc404
     c                   eval      fmc404 = *blank
     c                   endif
    
      *** Begin Delete *** 12/29/16 ********************************************
     C***                write     pfm01
      *** End   Delete *** 12/29/16 ********************************************
     C                   endif
     C     aeKey         reade     PIE01                                  60
    C                   Enddo

     C                   endsr

     *=========================================================================
     *  PEOGRP - PEO GROUP - Auto-attach endorsment
     *=========================================================================
     c     peogrp        begsr

     c                   endsr
     *=========================================================================
     *  UNIQFEIN - Determine if FEIN for Emloyer/Division is identical
     *           - to that of another division of same employer
     *=========================================================================
     c     uniqfein      begsr
     c                   endsr


     *=========================================================================
     *  PEOWRT - PEO GROUP - Write Endorsement
     *=========================================================================
     c     peowrt        begsr
     c                   endsr

     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    c                   exsr      getfnd
      /Free
       UseDfltAddParms = *OFF;
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
     *
     C                   move      *on           crtfrm
     C                   endsr
