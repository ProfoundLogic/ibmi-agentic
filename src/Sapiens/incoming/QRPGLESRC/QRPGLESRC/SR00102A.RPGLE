     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

     H/TITLE SR00102A - ENDORSEMENT ATTACH/PRINT PROGRAM
      *===============================================================*
      * WC000102A - Federal Coal Mine Health & Safety Act Coverage    *
      *            Endorsement                                        *
      *                                                               *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * CREATED   : 10/06/98                                          *
      * PROGRAMMER: C:114                                             *
      * PROJECT   : CM027                                             *
      *===============================================================*
      * REVISIONS:
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      *===============================================================*
      ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     Fwxpel     uf a e           k disk
     Fststl3    if   e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)

     F/COPY SCOPYEN01F
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4

     D qqForm          s             15A   Inz('WC000102A ')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     D addpgm          c                   const('SR00102IA ')

     D SR00102A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00102A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
      ** Additional Input file
     C     fmKey3        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform

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

     C/COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

      ** Write the output record.
     C                   open      wxfmp1
     C     fmKey3        chain     pfm02                              60

      ** Move the custom fields to print.
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
      *
     C                   close     wxfmp1
      *  If printing a policy page.  (Not called from W1054R)
     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid

     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     C                   If        *in60 = *off

     c                   clear                   E00102

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
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c104        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c105        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c106        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c107        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c108        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c109        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c110        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     c                   move      *blanks       pedata
     c                   movel     E00407        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     C                   update    ppe01
     C                   endif

     C                   endif

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     c     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
