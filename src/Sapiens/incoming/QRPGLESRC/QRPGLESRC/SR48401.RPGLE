     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H option(*nodebugio:*srcstmt)
    H/TITLE SR48401 - Wisconsin Contractors Premium Adjustment
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Wisconsin Contractors Premium Adjustement Program           *
     *   Endorsement.                                                *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy.                                                 *
     *                                                               *
     * Created   : 02/03/03                                          *
     * Programmer: C:61                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 01/03/17 C:136 Project: SY371                                 *
     *                Implement Phase 2 - changes not commented      *
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
     * Employer Payroll
     Fwmepl     if   e           k disk
    
     * Payroll Class Description
     Fwmprl1    if   e           k disk
    
     * Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *Entry
     D SR48401         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR48401         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     * Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          s             15a   inz('WC480401  ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *                                                                     ====
     * Employer Payroll                                                    ====
     *                                                                     ====
     C     epky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     *
     * Payroll Class Description
     *
     C     prky          klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
     C                   kfld                    elpfdt
     *
     C     prky1         klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
     *=========================================================================
     *  Main
     *=========================================================================
     * Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    C                   eval      runningSrText = *On
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
    
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C                   if        ELasp1 = 0
    
     C     epky          setll     pep01
     C     epky          reade     pep01                                  60
      *
     C     *in60         doweq     *off
      *
     C     epstat        ifne      'I'
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
      *
     C     *in60         ifeq      *off
     C     prccfl        andeq     'Y'
     *
     *  Found division with Wisconsin Rating state and Contractor Class
     *
     C                   move      *on           crtfrm
     C                   leave
     C                   endif
     *
     C                   endif
     *
     C     epky          reade     pep01                                  60
     C                   enddo
     *
     C                   endif
    
     C                   endsr
