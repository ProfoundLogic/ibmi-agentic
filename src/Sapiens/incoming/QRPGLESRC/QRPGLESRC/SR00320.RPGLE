     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001
     *===============================================================*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00320 - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC000320 - Labor Contractor Endorsement                      *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 05/30/01                                          *
     * PROGRAMMER: C: 90                                             *
     * PROJECT   : CM027                                             *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/09/20 C: 079 Project: WC2769                               *
     *                 Since SR00320A refers to the same form per    *
     *                 Matt D. we are changing this obsolet Endorse. *
     *                 to refer to WC000320                          *
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

     Fwdl2l2    if   e           k disk

     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)

     D xxefdt          s                   like(fmeedt)

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D*** Beg Chg *** 10/09/20 ********************************************
     D*** qqform          c                   const('WC000320A ')
     D qqform          c                   const('WC000320  ')
     D*** End Chg *** 10/09/20 ********************************************

     ** Additional entry program
     D addpgm          c                   const('SR00320I  ')
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
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Alternate Employer file
     C     l2Key         klist
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
     ** Write the output record
     C     l2Key         setll     pl201
     C     l2Key         reade     pl201                                  60
    
     C                   dow       *in60 = *off
    
     C                   if        l2efdt = *zeros
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt

     C                   else
     c                   eval      srdti = l2efdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt
     C                   endif

     C                   if        xxefdt = fmeedt
     C
     C                   movel     l2nam1        fmnm01
     C                   movel     l2adr1        fmc401
    
     C                   if        l2adr2 <> *blank
     C                   movel     l2adr2        fmc402
     C                   endif
    
     C                   move      l2zip         xxzip
     C     l2cty         cat       ',':0         fmc403
     C                   cat       l2st:1        fmc403
     C                   cat       xxzip1:1      fmc403
    
     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc403
     C                   cat       xxzip2:0      fmc403
     C                   endif
    
     C*****************  movel     l2emst        xxst
     C                   movel     l2emst        fmc101
     C                   movel     l2proj        fmc404
    
     C                   write     pfm01
     C                   clear                   fmnm01
     C                   clear                   fmc401
     C                   clear                   fmc402
     C                   clear                   fmc403
     C                   clear                   fmc404
     C                   clear                   fmc101
     C                   clear                   fmfl01
     C                   clear                   fmfl02
     C                   clear                   xxzip

     C                   endif
    
     C     l2Key         reade     pl201                                  60
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
