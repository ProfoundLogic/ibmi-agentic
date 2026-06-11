     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00201A - MARITIME COVERAGE ENDORSEMENT WC000201A
      *===============================================================*
      * WC000201A - Maritime Coverage Endorsement                     *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
      ** Group Detail File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
      ** Form ID
     D qqForm          s             15A   Inz('WC000201A ')
     D addpgm          c                   const('SR00201I  ')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     D SR00201A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00201A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      ** Group Detail file
     C     f1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C     fmky3         klist
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
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
      *=========================================================================
      *  Main
      *=========================================================================
     C/COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
     C                   open      wxfmp1
     C     fmky3         chain     pfm02                              60
      ** Move the custom fields to print
     C                   movel     f3c401        fmc401
     C                   movel     f3c402        fmc402
     C                   eval      fmc101 = f3c101
     C                   eval      fmam02 = f3a701
     C                   eval      fmam03 = f3a702
      ** Write the output record
     C                   close     wxfmp1
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
     c                   clear                   E00201
     c                   movel     fmc401        xxdesc           80
     c                   move      fmc402        xxdesc           80
     c                   movel     xxdesc        xxdow
     c                   z-add     fmam02        xpacc
     c                   z-add     fmam03        xpdis
     c                   movel     fmc101        xxtwm
     c                   move      *blanks       pedata
     c                   movel     E00201        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
     C                   update    ppe01
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
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
