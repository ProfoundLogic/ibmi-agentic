     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

      *** zzz
    H/TITLE SRxxxxxx - Print/Attach WCxxxxxxx
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   "Print/Attach" program for single manual attach endorsement *
     *   with addtional input program that stores information        *
     *   in generic endorsement data file wledp.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:
     *
      *** zzz
     * xx/xx/xx C:000 Project xxxxxx
     *                Created.
     *
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
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     ** Additional Information File
     Fwledl     if   e           k disk    usropn
     f                                     prefix(ed:2)
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
      *** zzz
     ** Form ID
     D xxform          c                   const('WCxxxxxxx ')
    
      *** zzz
     ** Additional entry program
     D addpgm          c                   const('SRxxxxxxI ')
    
    
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
    
    
     ** Additional input program parameters
     C     addpl         pList
     C                   parm                    qqprds
     C                   parm      qqmod2        qqcall
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    xkform
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     ** Additional Input
    
     C     edky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xkform
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     c                   open      wledl
     c     edky          chain     ped01

     c                   if        %found
      *** zzz
     c*************      eval      fma521 = eda521
      *** zzz
     C                   write     pfm01
     c                   endif

     c                   close     wledl
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
    
