     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR14602  - ENDORSEMENT ATTACH/PRINT PROGRAM
      //===============================================================*
      // WC140602  - Benefit Level Selection  Endorsement              *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 11/02/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      // ------------------------------------------------------------- *
      //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      // ------------------------------------------------------------- *
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

      /Copy SCOPYEN01F

     Fwxfmp1    if   e           k disk
     F                                     rename(pfm01:pfm02)
     Fwpbsl     if   e           k disk

      /Copy SCOPYEN01D

     D qqForm          s             15A   Inz('WC140602  ')

     D addpgm          c                   const('SR14602I  ')

     D INM             S              2  0 INZ
     D IMC             S              2  0 INZ
      ** Names to include on form
     D                 ds
     D  nm                     1    640
     D                                     dim(16)
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     D  FMC406               201    240
     D  FMC407               241    280
     D  FMC408               281    320
     D  FMC409               321    360
     D  FMC410               361    400
     D  FMC411               401    440
     D  FMC412               441    480
     D  FMC413               481    520
     D  FMC414               521    560
     D  FMC415               561    600
     D  FMC416               601    640
      ** Title to include on form
     D                 ds
     D  mc                     1    176  2
     D                                     dim(16)
     D  FMAM01                 1     11  2
     D  FMAM02                12     22  2
     D  FMAM03                23     33  2
     D  FMAM04                34     44  2
     D  FMAM05                45     55  2
     D  FMAM06                56     66  2
     D  FMAM07                67     77  2
     D  FMAM08                78     88  2
     D  FMAM09                89     99  2
     D  FMAM10               100    110  2
     D  FMAM11               111    121  2
     D  FMAM12               122    132  2
     D  FMAM13               133    143  2
     D  FMAM14               144    154  2
     D  FMAM15               155    165  2
     D  FMAM16               166    176  2

     D SR14602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR14602         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     bskey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv

      /Copy SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr

      /Free
          WriteInCopybook = *Off;
      /End-Free

     C                   Z-ADD     0             X                 2 0
     C     bsKey         setll     pbs01
     C     bsKey         reade     pbs01                                  60

     C                   dow       *in60 = *off
     C                   eval      inm= inm+ 1
     C                   movel     BSPNAM        nm(inm)
     C                   movel     BSPRBD        mc(inm)
      ** Write the output record
     C                   If        inm= 16
      /Free
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm
     C                   clear                   mc
     C                   eval      inm= *zero

     C                   endif
     C     bsKey         reade     pbs01                                  60

     C                   Enddo
      ** Write the output record
     C                   If        inm < 16
      /Free
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm
     C                   clear                   MC
     C                   eval      inm= *zero

     C                   endif

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      /Free
          UseDfltAddparms = *Off;
      /End-Free
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
