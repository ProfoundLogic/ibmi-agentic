     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR27301  - NV Employee Leasing Exclusion Endorsement
     *                 Print/Attach program
     *===============================================================*
     * WC270301                                                      *
      *   Single Manual attached endorsement                          *
     *   with no additional information entry needed                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 02/21/17 C:180 Support: 41094                                 *
     *                Fix interface to Scriptura so Addtl info       *
      *                can be entered. - Changes not commented.       *
     *                                                               *
     * 09/30/16 C:136 Project: SY371                                 *
     *                Phase 2 Scriptura - changes not commented      *
     *                                                               *
     * 04/30/07  C:184 Project W1552                                 *
     *                 Created.                                      *
     *                                                               *
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
     ** Additional Information File
     Fwxfmp1    if   e           k disk
     F                                     rename(pfm01:pfm02)
     Fwllel     if   e           k disk
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          s             15a   inz('WC270301  ')
    
     ** Additional entry program
     D addpgm          c                   const('SR27301I  ')
    
      //Entry
     D SR27301         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR27301         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     C     lekey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     C     leKey         setll     ple01
     c     leKey         reade     ple01                                  60
     C                   dow       *in60 = *off
     C                   MOVEL     LENMLK        FMC201
     C                   MOVEL     LENAME        FMC301
     C                   MOVE      LENAME        FMC302
     C                   MOVEL     LEADR1        FMC401
     C                   MOVEL     LEADR2        FMC402
     C                   MOVEL     LEADR3        FMC403
     C                   MOVEL     LEFEIN        FMC101
     C                   exsr      WriteWorkFile
     c     leKey         reade     ple01                                  60
     c                   enddo
     C
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
      /free
       Begsr PgmStr;
       WriteInCopyBook = *Off;
       UseDfltAddParms = *Off;
       Endsr;
     /end-free

     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
    
