     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00321 - LABOR CONTRACTOR EXCLUSION ENDORSEMENT
      //===============================================================*
      // WC000321 - Labor Contractor Exclusion Endorsement            *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/20/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*

     Fwdlcl     if   e           k disk

      /Copy SCOPYEN01F

     D xxefdt          s                   like(fmeedt)

      /Copy SCOPYEN01D

     D qqForm          s             15A   Inz('WC000321  ')

     D addpgm          c                   const('SR00321I  ')

     D SR00321         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00321         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     lcKey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

     C     lcKey         setll     plc01
     C     lcKey         reade     plc01                                  60

     C                   dow       *in60 = *off

     C                   if        lcefdt = *zeros
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt

     C                   else
     c                   eval      srdti = lcefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt
     C                   endif

     C                   if        xxefdt = fmeedt
     C
     C                   movel     lcnam1        fmnm01
     C                   movel     lcadr1        fmc401

     C                   if        lcadr2 <> *blank
     C                   movel     lcadr2        fmc402

     C                   move      lczip         xxzip
     C     lccty         cat       ',':0         fmc403
     C                   cat       lcst:1        fmc403
     C                   cat       xxzip1:1      fmc403

     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc403
     C                   cat       xxzip2:0      fmc403
     C                   endif

     C                   else

     C                   move      lczip         xxzip
     C     lccty         cat       ',':0         fmc402
     C                   cat       lcst:1        fmc402
     C                   cat       xxzip1:1      fmc402

     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc402
     C                   cat       xxzip2:0      fmc402
     C                   endif

     C                   endif

      /Free
           WriteInCopybook = *Off;
           Exsr WriteWorkFile;
      /End-Free
     C                   clear                   fmnm01
     C                   clear                   fmc401
     C                   clear                   fmc402
     C                   clear                   fmc403
     C                   clear                   xxzip

     C                   endif

     C     lcKey         reade     plc01                                  60
     C                   enddo

     C                   endsr

      /Free

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
          UseDfltAddparms = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;
          crtFrm = *on;
       Endsr;

      /End-Free
