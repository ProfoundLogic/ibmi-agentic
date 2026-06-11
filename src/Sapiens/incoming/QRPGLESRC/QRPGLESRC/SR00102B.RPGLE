     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR00102B - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC000102B - Federal Coal Mine Health & Safety Act Coverage    *
     *             Endorsement                                       *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 11/12/14                                          *
     * PROGRAMMER: C:282                                             *
     * PROJECT   : WC2318                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/18/22 C:333 Project WC2834 (Recompile)                     *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 01/05/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
     *                                                               *
     *===============================================================*
      ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
      *
     Fwxpel     uf a e           k disk
     Fststl3    if   e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR00102B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00102B        PI
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
     D/COPY WWCPOLDAT4
     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC000102B ')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     ** Additional entry program
     D addpgm          c                   const('SR00102BI ')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Additional Input file
     C     fmKey3        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm

     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0

     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr

     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#

     C     stky          KList
     C                   KFld                    xxkey            10
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record.
     C                   open      wxfmp1
     C     fmKey3        chain     pfm02                              60
    
     ** Move the custom fields to print.
     C                   movel     f3c101        fmc101
     C                   movel     f3c102        fmc102
     C                   movel     f3c103        fmc103
     C                   movel     f3c104        fmc104
     C                   movel     f3c105        fmc105
     C                   movel     f3c106        fmc106
     C                   movel     f3c107        fmc107
     C                   movel     f3c108        fmc108
     C                   movel     f3c109        fmc109
     C                   movel     f3c110        fmc110
     *
     C                   close     wxfmp1
      *
      *  If printing a policy page.  (Not called from W1054R)

     C                   call      'SRCHKPGM'
     C                   parm      'W1054R   '   #1pgm            10
     C                   parm      *BLANK        #1flag            1
     C                   if        #1flag = '0'

      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
      *
     C                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     C     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     C                   clear                   E00102
      *
     C                   movel     f3c101        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   movel     xxst          xxsch
     c                   endif
     C                   movel     f3c102        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c103        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   movel     f3c104        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   movel     f3c105        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c106        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   movel     f3c107        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   movel     f3c108        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   movel     f3c109        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   movel     f3c110        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     C                   move      stnst         xxst              2
     C                   cat       xxst:0        xxsch
     C                   endif
     C                   move      *blanks       pedata
     C                   movel     E00407        pedata
     C                   movel     q1user        peuser
     C                   movel     q1pgm         pepgm
     C                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   update    ppe01
     C                   endif
      *
     C                   endif
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
     * This is a manual attach endorsement with additional input.
     C                   move      *on           crtfrm
     C                   endsr
