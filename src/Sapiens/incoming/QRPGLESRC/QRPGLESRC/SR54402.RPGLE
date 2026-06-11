     *===============================================================*
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H option(*nodebugio:*srcstmt)
    H/TITLE SR54402 - AK CONSTRUCTION CLASSIFICATION PREMIUM ADJUSTMENT END
     *===============================================================*
     * WC540402 - Alaska Construction Classification Premium         *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 05/04/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 12/29/16 C:136 Project: SY371                                 *
     *                Scriptura Phase 2 Implementation               *
     *                Changes not commented                          *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
     Fwmepl     if   e           k disk
     Fwmprl1    if   e           k disk
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      **Entry
     D SR54402         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR54402         PI
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
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqform          s             15a   inz('WC540402  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
     C     epky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
    C*
     C     prky          klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
     C                   kfld                    elpfdt
    C*
     C     prky1         klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
    C*
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   eval      runningSrText = *On
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

     C     elasp1        ifeq      *zero

     C     epky          setll     pep01
     C     epky          reade     pep01                                  60
     C     *in60         doweq     *off

     C     epstat        ifne      'I'
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
     C     *in60         ifeq      *off
     C     prccfl        andeq     'Y'
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
     C                   endif

     C     epky          reade     pep01                                  60
     C                   enddo

     C                   endif

     C                   endsr
