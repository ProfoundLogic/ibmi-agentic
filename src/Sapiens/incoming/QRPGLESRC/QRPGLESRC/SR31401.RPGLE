     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR31401 - NY - Construction Class Premium Adjustment Endorsement
     *===============================================================*
     * WC310401 - New York Construction Classification Premium Adjust*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/15/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 08/24/01  C:90 PROJECT: CM027 WA107/28711                     *
     *                Read thru all divisions within the OTHINF      *
     *                subroutine.                                    *
     *                                                               *
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

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC310401  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

      *entry parameters
     D SR31401         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31401         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
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
     *
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C     elasp1        ifne      *zero
      *
     C                   z-add     elasp1        fmam01
     C*
     C                   leave
     *
     C                   endif
     *
     C                   endif
     C     elky2         reade     pel02                                  60
     C                   enddo
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
      *
     C     elasp1        ifne      *zero
      *
     C     epky          setll     pep01
     C     epky          reade     pep01                                  60
      *
     C     *in60         doweq     *off
      *
     C     epstat        ifne      'I'
      *
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
      *
     C     *in60         ifeq      *off
     C     prccfl        andeq     'Y'
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endif
     *
     C     epky          reade     pep01                                  60
     C                   enddo
     *
     C                   endif
     *
     C                   endsr
