     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

     H/TITLE SR00111 - ENDORSEMENT WC000111
      *===============================================================*
      * WC000111 - Migrant and Seasonal Agricultural Worker           *
      *            Protection Act Coverage Endorsement                *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 11/10/99                                          *
      * Programmer: C:113                                             *
      * Project   : SUPPORT WA096/#20573                              *
      *===============================================================*
      * REVISIONS:                                                    *
      * ------------------------------------------------------------- *
      //                                                               *
      // 10/18/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
     F/COPY SCOPYEN01F
     FWXFMP1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     D/COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC000111  ')

     D addpgm          c                   const('SR00111I  ')

     D SR00111         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00111         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      ** Additional Input file
     C     fmKey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform

     C/COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr

     C                   open      WXFMP1

     C     fmKey         chain     pfm02                              60

     C                   movel     F3C301        FMC301
     C                   movel     F3C302        FMC302
     C                   movel     F3C303        FMC303
     C                   movel     F3C304        FMC304
     C                   movel     F3C305        FMC305
     C                   movel     F3C306        FMC306
     C                   movel     F3C307        FMC307
     C                   movel     F3C308        FMC308
     C                   movel     F3C309        FMC309
     C                   movel     F3C310        FMC310
     C                   movel     F3C311        FMC311
     C                   movel     F3C312        FMC312
     C                   movel     F3C313        FMC313
     C                   movel     F3C314        FMC314
     C                   movel     F3C315        FMC315
     C                   movel     F3C316        FMC316
     C                   movel     F3C401        FMC401
     C                   movel     F3C201        FMC201
     C                   movel     F3C402        FMC402
     C                   movel     F3C202        FMC202
     C                   close     WXFMP1
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
     C                   move      *on           crtfrm
     C                   endsr
