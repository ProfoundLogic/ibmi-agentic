     *===============================================================*
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')

    H/TITLE SR00201B - MARITIME COVERAGE ENDORSEMENT WC000201B
     *===============================================================*
     * WC000201B - Maritime Coverage Endorsement                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/12/14                                          *
     * Programmer: C:282                                             *
     * Project   : WC2318                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/18/22 C:333 Project : WC2834 (Recompile)                   *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 01/05/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
     ** Group Detail File
     Fwdf1l     if   e           k disk    usropn
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D SR00201B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00201B        PI
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
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC000201B ')
     ** Additional entry program
     D addpgm          c                   const('SR00201BI ')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Group Detail file
     C     f1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     *
     ** Additional Input file
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    XXFORM
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
      *
      * SUBMISSION HISTORY FILE
      *
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C                   open      wxfmp1
     C                   open      wdf1l
     *
     C     fmky3         chain     pfm02                              60
     C     f1ky          chain     pf101                              60
     *
     ** Move the custom fields to print
     C                   movel     f3c401        fmc401
     C                   movel     f3c402        fmc402
     C                   eval      fmam01 = f3am01
     C                   eval      fma701 = f3a701
     C                   eval      fma702 = f3a702
     *
     C                   close     wxfmp1
     C                   close     wdf1l
      *
      * Update the endorsement reporting file with the proper data.
      *
      *
      * Find the unique ID number.
      *
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
     C                   clear                   E00201
      *
     C                   movel     fmc401        xxdesc           80
     C                   move      fmc402        xxdesc           80
     C                   movel     xxdesc        xxdow
     C                   z-add     fmam02        xpacc
     C                   z-add     fmam03        xpdis
     C                   movel     fmc101        xxtwm
      *
     C                   move      *blanks       pedata
     C                   movel     E00201        pedata
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
     *
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
