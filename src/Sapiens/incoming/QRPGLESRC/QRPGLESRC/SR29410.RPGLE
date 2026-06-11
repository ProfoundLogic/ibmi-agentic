     *===============================================================*
     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR29410 - NJ CONSTRUCTION CLASSIFICATION PREM. ADJ. END.
     *===============================================================*
     * WC290410 - New Jersey Construction Classification Endorsement *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/05/00                                          *
     * Programmer: C:90                                              *
     * Project   : WA101/23054                                       *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 01/04/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
     * 07/30/01 C:135 Project Support WA101/sequence 28383           *
      *                Add processing to check for contracting code   *
      *                flag being set.                                *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for mid-term processing.                 *
     *===============================================================*
      *** BEGIN ADD *** 07/30/01 **************************************
     Fwmepl     if   e           k disk
     Fwmprl1    if   e           k disk
      ***  END  ADD *** 07/30/01 **************************************
     ** Copybook F specs
     F/COPY SCOPYEN01F

     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D SR29410         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29410         PI
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
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC290410  ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')                  Dummy placeholder

     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *** BEGIN CHG *** 07/30/01 **************************************
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
      ***  END  CHG *** 07/30/01 **************************************
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs (PLISTs, KLISTs and mainline)
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
     /Free
       RunningSRtext = *ON;
     /End-Free
    
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
    
    
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C*** BEGIN ADD **** 07/30/01 ******************************************
     C     elasp1        ifeq      *zero
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
     C***  END  ADD **** 07/30/01 ******************************************
    
     C*** BEGIN DEL **** 07/30/01 ******************************************
     ** Found a division with a matching jurisdiction state
     C*************      move      *on           crtfrm
     C***  END  DEL **** 07/30/01 ******************************************
     C                   endif
    
     C                   endsr
