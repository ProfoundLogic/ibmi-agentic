     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00509A - Retro Premium Changes Endorsement Print/Attach

      *===============================================================*
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      *  Print/Attach for Multiple Manual with Additional Input       *
      *  Endorsement (data stored in file specifed for endorsement)   *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      *===============================================================*
      * NOTES:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      *                                                               *
      *  qqmode = 1 - Called to determine if form should be attached. *
      *  qqmode = 2 - Called to print endorsement.                    *
      *  qqmode = 3 - Called for additional input.                    *
      *  qqmode = 4 - Called for midterm processing.                  *
      *===============================================================*

     FWLRPL     if   e           k disk
     F                                     rename(prp01:prp02)

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D

     D line01          ds
     D
     D FMC401                  1     40
     D FMC402                 41     80

     D line02          ds
     D FMC403                  1     40
     D FMC404                 41     80

     D line03          ds
     D FMC405                  1     40
     D FMC406                 41     80

     D line04          ds
     D FMC407                  1     40
     D FMC408                 41     80

     D line05          ds
     D
     D FMC409                  1     40
     D FMC410                 41     80

     D line06          ds
     D FMC411                  1     40
     D FMC412                 41     80

     D line07          ds
     D FMC413                  1     40
     D FMC414                 41     80

     D line08          ds
     D FMC415                  1     40
     D FMC416                 41     80

     D line09          ds
     D FMC417                  1     40
     D FMC418                 41     80

     D line10          ds
     D FMC419                  1     40
     D FMC420                 41     80

     D qqForm          S             15A   Inz('WC000509A ')

     D addpgm          c                   const('SR00509I  ')


     D SR00509A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00509A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     rplky         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    x1seq#            9 0

     C     rplky1        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

     C                   z-add     1             i
     C     rplky         setll     prp02
     C     rplky1        reade     prp02                                  60

     C                   dow       not %eof
      * move information to WXFMP fields to print on endorsement
     C                   move      *blanks       x1eld1           10
     C                   move      *blanks       x1eld2           10
     C                   move      *blanks       x1eld3           10
     C                   move      *blanks       x1fcd1           10
     C                   move      *blanks       x1fcd2           10
     C                   move      *blanks       x1fcd3           10
     C                   move      *blanks       x1tmd1           10
     C                   move      *blanks       x1tmd2           10
     C                   move      *blanks       x1tmd3           10
     C
     c                   if        (rpeld1 <> 0)
     C                   move      rpeld1        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxeld1            8 0
     c                   eval      x1eld1 = %editw(xxeld1:'  /  /    ')
     c                   endif
     C
     c                   if        (rpeld2 <> 0)
     C                   eval      srdti = rpeld2
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxeld2            8 0
     c                   eval      x1eld2 = %editw(xxeld2:'  /  /    ')
     c                   endif
     C
     c                   if        (rpeld3 <> 0)
     C                   eval      srdti = rpeld3
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxeld3            8 0
     c                   eval      x1eld3 = %editw(xxeld3:'  /  /    ')
     c                   endif
     C
     c                   if        (rpfcd1 <> 0)
     C                   eval      srdti = rpfcd1
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxfcd1            8 0
     c                   eval      x1fcd1 = %editw(xxfcd1:'  /  /    ')
     c                   endif
     C
     c                   if        (rpfcd2 <> 0)
     C                   eval      srdti = rpfcd2
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxfcd2            8 0
     c                   eval      x1fcd2 = %editw(xxfcd2:'  /  /    ')
     c                   endif
     C
     c                   if        (rpfcd3 <> 0)
     C                   eval      srdti = rpfcd3
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxfcd3            8 0
     c                   eval      x1fcd3 = %editw(xxfcd3:'  /  /    ')
     c                   endif

     c                   if        (rptmd1 <> 0)
     C                   eval      srdti = rptmd1
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxtmd1            8 0
     c                   eval      x1tmd1 = %editw(xxtmd1:'  /  /    ')
     c                   endif

     c                   if        (rptmd2 <> 0)
     C                   eval      srdti = rptmd2
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxtmd2            8 0
     c                   eval      x1tmd2 = %editw(xxtmd2:'  /  /    ')
     c                   endif

     c                   if        (rptmd3 <> 0)
     C                   eval      srdti = rptmd3
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxtmd3            8 0
     c                   eval      x1tmd3 = %editw(xxtmd3:'  /  /    ')
     c                   endif

     c                   eval      line01 = '  ' +
     c                             rpels1 +
     c                             '                                  ' +
     c                             %editc(rpelf1:'4') +
     c                             '                         ' +
     c                             x1eld1

     c                   eval      line02 = '  ' +
     c                             rpels2 +
     c                             '                                  ' +
     c                             %editc(rpelf2:'4') +
     c                             '                         ' +
     c                             x1eld2

     c                   eval      line03 = '  ' +
     c                             rpels3 +
     c                             '                                  ' +
     c                             %editc(rpelf3:'4') +
     c                             '                         ' +
     c                             x1eld3

     c                   eval      line04 = '  ' + rpna01 +
     c                             '   ' + rpna02 +
     c                             '   ' + rpna03 +
     c                             '   ' + rpna04 +
     c                             '   ' + rpna05 +
     c                             '   ' + rpna06 +
     c                             '   ' + rpna07 +
     c                             '   ' + rpna08 +
     c                             '   ' + rpna09 +
     c                             '   ' + rpna10

     c                   eval      line05 = '  ' +
     c                             rpfcs1 +
     c                             '                    ' +
     c                             %editc(rpfc11:'4') +
     c                             '     ' +
     c                             %editc(rpfc12:'4') +
     c                             '     ' +
     c                             %editc(rpfc13:'4') +
     c                             '                ' +
     C                             x1fcd1

     C                   eval      line06 = '  ' +
     C                             rpfcs2 +
     c                             '                    ' +
     c                             %editc(rpfc21:'4') +
     c                             '     ' +
     c                             %editc(rpfc22:'4') +
     c                             '     ' +
     c                             %editc(rpfc23:'4') +
     c                             '                ' +
     c                             x1fcd2

     C                   eval      line07 = '  ' +
     C                             rpfcs3 +
     c                             '                    ' +
     c                             %editc(rpfc31:'4') +
     c                             '     ' +
     c                             %editc(rpfc32:'4') +
     c                             '     ' +
     c                             %editc(rpfc33:'4') +
     c                             '                ' +
     c                             x1fcd3

     C                   eval      line08 = '   ' +
     C                             rptms1 +
     c                             '                 ' +
     c                             %editc(rptmo1:'4') +
     c                             '            ' +
     c                             %editc(rptmf1:'4') +
     c                             '          ' +
     c                             x1tmd1

     C                   eval      line09 = '   ' +
     C                             rptms2 +
     c                             '                 ' +
     c                             %editc(rptmo2:'4') +
     c                             '            ' +
     c                             %editc(rptmf2:'4') +
     c                             '          ' +
     c                             x1tmd2

     C                   eval      line10 = '   ' +
     C                             rptms3 +
     c                             '                 ' +
     c                             %editc(rptmo3:'4') +
     c                             '            ' +
     c                             %editc(rptmf3:'4') +
     c                             '          ' +
     c                             x1tmd3

      /Free
          //  Map to different fields for scriptura.
          FMcd01 = RPels1;
          FMcd02 = RPels2;
          FMcd03 = RPels3;
          FMam01 = RPelf1 * 10000;
          FMam02 = RPelf2 * 10000;
          FMam03 = RPelf3 * 10000;
          FMdt01 = XXeld1;
          FMdt02 = XXeld2;
          FMdt03 = XXeld3;

          FMcd41 = RPna01;
          FMcd42 = RPna02;
          FMcd43 = RPna03;
          FMcd44 = RPna04;
          FMcd45 = RPna05;
          FMcd46 = RPna06;
          FMcd47 = RPna07;
          FMcd48 = RPna08;
          FMcd49 = RPna09;
          FMcd50 = RPna10;

          FMcd04 = RPfcs1;
          FMcd05 = RPfcs2;
          FMcd06 = RPfcs3;
          FMam04 = RPfc11 * 10000;
          FMam05 = RPfc21 * 10000;
          FMam06 = RPfc31 * 10000;
          FMam07 = RPfc12 * 10000;
          FMam08 = RPfc22 * 10000;
          FMam09 = RPfc32 * 10000;
          FMam10 = RPfc13 * 10000;
          FMam11 = RPfc23 * 10000;
          FMam12 = RPfc33 * 10000;
          FMdt04 = XXfcd1;
          FMdt05 = XXfcd2;
          FMdt06 = XXfcd3;

          FMcd06 = RPtms1;
          FMcd07 = RPtms2;
          FMcd08 = RPtms3;
          FMam13 = RPtmo1 * 10000;
          FMam14 = RPtmo2 * 10000;
          FMam15 = RPtmo3 * 10000;
          FMam16 = RPtmf1 * 10000;
          FMam17 = RPtmf2 * 10000;
          FMam18 = RPtmf3 * 10000;
          FMdt07 = XXtmd1;
          FMdt08 = XXtmd2;
          FMdt09 = XXtmd3;

          //* Write the output record
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free

      *  clear WXFMP data fields used above.  Do NOT clear entire WXFMP
      *       record.
     C                   clear                   line01
     C                   clear                   line02
     C                   clear                   line03
     C                   clear                   line04
     C                   clear                   line05
     C                   clear                   line06
     C                   clear                   line07
     C                   clear                   line08
     C                   clear                   line09
     C                   clear                   line10

     C     rplky1        reade     prp02                                  60
     C                   enddo

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      /Free
         UseDfltAddParms = *off;
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
