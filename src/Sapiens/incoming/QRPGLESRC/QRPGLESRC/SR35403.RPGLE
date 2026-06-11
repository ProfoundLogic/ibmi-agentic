     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR35403 - OK MERIT RATING ENDORSEMENT
       //===============================================================*
       // WC350403A - Oklahoma Merit Rating Plan Endorsement            *
       //                                                               *
       //    PROGRAM DESCRIPTION:                                       *
       //                                                               *
       //       THIS PROGRAM IS CALLED TO:                              *
       //         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
       //            ATTACHED TO THE POLICY.                            *
       //         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
       //            (WXFMP) PRIOR TO A FORM MERGE.                     *
       //                                                               *
       //                                                               *
       //****************************************************************
       //                                                               *
       // REVISIONS:                                                    *
      //                                                               *
       // 10/19/22 C:342 Project: WC2834 (Recompile)                    *
       //                Endorsement Recognition of Cancellation        *
      //                                                               *
       // 01/03/17 C:180 Project SY371 scriptura.                       *
       //                Recompile only.                                *
      //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
       //****************************************************************

      /COPY SCOPYEN01F

     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC350403B ')
     D addpgm          c                   const('SR35403I  ')

     D SR35403         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR35403         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr

     C                   open      wxfmp1

     C     fmky3         chain     pfm02                              60
      ** Move the custom fields to print
     C                   movel     f3c101        fmc101
     C                   movel     f3a521        fma521
     C                   movel     f3a522        fma522
     C                   movel     f3a523        fma523

     C                   close     wxfmp1
     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
     C                   if        elprom = 'M'
     C                   move      *on           crtfrm
     C                   endif
     C                   endsr
