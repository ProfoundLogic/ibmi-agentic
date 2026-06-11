     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     *
    H/TITLE SR29304 - NJ EMPLOYEE LEASING CLIENT EXCLUSION ENDORSEMENT
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 04/03/03                                          *
     * Programmer: C:61                                              *
     * Project   : 114WFI                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *
     *  10/06/17 C:136 Project: 150WBL
     *                 PEO attachment criteria moved to copy code
     *
      * 03/07/17 C:180 Project SY371
      *                Interface with Scriptura.
      *
      * 08/15/05 C:61 Project CM027
      *               Change to auto attach if PEO flag set.
      *               WMFNP/FNPEOF <> blank.
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR29304         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29304         PI
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
     **
     ** Form ID
     D QQFORM          c                   const('WC290304  ')
     **
     ** Additional entry program
     D addpgm          c                   const('SR29304I  ')
     **
     *
     *
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
     *
     *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     ** Additional Input file
     *
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
     *
     *
     *
     *=========================================================================
     *  Main
     *=========================================================================
     *
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
      /Free
       If not %open(Wxfmp1);
          Open wxfmp1;
       Endif;
      /End-Free
     *
     C     fmky3         chain     pfm02                              60
     *
     ** Move the custom fields to print
     *
     C                   movel(p)  f3C401        fmc401
     C                   movel(p)  f3C402        fmc402
     C                   movel(p)  f3C403        fmc403
     *
     C                   movel(p)  f3C202        fmc404
     C                   cat(p)    ',':0         fmc404
     *
     C                   cat(p)    f3c101:1      fmc404
     *
     C                   move      f3a705        xxzp1a            5
     C                   move      f3a704        xxzp1b            4
     C                   cat(p)    xxzp1a:1      fmc404
     *
     C                   if        (xxzp1b <> *zeros)
     C                   cat(p)    '-':0         fmc404
     C                   cat(p)    xxzp1b:0      fmc404
     C                   endif
     *
     C                   if        (fmc403 = *blanks)
     C                   movel(p)  fmc404        fmc403
     C                   movel(p)  *blanks       fmc404
     C                   endif

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
      /Free
       If %open(Wxfmp1);
          Close wxfmp1;
       Endif;
      /End-Free
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   eval      crtfrm = *on
     C                   endsr
