     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR42304A - TX - Waiver to Recover Endorsement
      *------------------------------------------------------------- *
      * WC420304A - Texas Waiver of Our Right to Recover from Others  *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 03/26/13                                          *
      * Programmer: C: 287                                            *
      * Project   : WC2194                                            *
      *------------------------------------------------------------- *
      *Revisions:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 01/16/19 C:300 Support: 73970 (LUBA) (Recompile)              *
      *                Printing end from Work with End Forms          *
      *                Changes in SCOPYEN01C copybook                 *
     *                                                               *
     * 07/27/17 C:180 Support: 48458                                 *
     *                Check for replaced date.                        *
      *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                               *
      * 09/11/14 C:282 Support: 86622                                 *
      *                Added the condition to only print Waivers      *
      *                for TX state.                                  *
      *                                                               *
      *------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      *------------------------------------------------------------- *
      *Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *------------------------------------------------------------- *

      **Copybook F specs
     F/COPY SCOPYEN01F
    
      **Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
    
      **Waiver Information File
     Fwmwvl     if   e           k disk
      *------------------------------------------------------------- *
      * Definition Specs
      *------------------------------------------------------------- *

      **Copybook D specs
     D/COPY SCOPYEN01D

      *------------------------------------------------------------- *
      * /COPY required constants
      *------------------------------------------------------------- *

      **Form ID
     D qqForm          s             15a   inz('WC420304A ')
      **Additional entry program
     D addpgm          c                   const('SR42304AI ')

     D xxefdt          s                   like(wvefdt)

      *entry parameters
     D SR42304A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42304A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *------------------------------------------------------------- *
      * KLIST definitions
      *------------------------------------------------------------- *
    
      **Additional Input file
     C     fmky3         Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv
     C                   Kfld                    xxForm
    
      **Waiver Information file
     C     wvky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv

      *------------------------------------------------------------- *
      * Main
      *------------------------------------------------------------- *

      **Copybook C specs
     C/COPY SCOPYEN01C
    
      *---------------------------------------------------------------------- *
      * OTHINF -Subroutine to load form specific information & write record
      *---------------------------------------------------------------------- *

     C     othinf        Begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
    
     C                   Open      wxfmp1
     C     fmky3         Chain     pfm02                              60
    
     C                   If        f3fl03 = 'S'
     C                   Move      'X'           fmfl01
     C                   Move      *blank        fmfl02
     C                   Else
     C                   Move      *blank        fmfl01
     C                   Move      'X'           fmfl02
     C                   Endif

     C                   Eval      fmc407 = f3c401
     C                   Eval      fmc408 = f3c402
     C                   Eval      fma921 = f3a921
     C                   Eval      fma521 = f3a521
     C                   Eval      fma923 = f3a923
    
     C     wvky          Setll     pwv01
     C     wvky          Reade     pwv01                                  60
    
     C     *in60         Doweq     *off

     C                   If        wvstat <> 'I'
     C                             And wvst = 'TX'

     C     wvname        Cat       wvnam2:0      fm2001
     C                   Movel     wvadr1        fmc402

     C                   If        wvadr2 <> *blank
     C                   Movel     wvadr2        fmc403
     C                   Move      wvzip         xxzip
     C     wvcty         Cat       ',':0         fmc404
     C                   Cat       wvst:1        fmc404
     C                   Cat       xxzip1:1      fmc404

     C                   If        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   Cat       '-':0         fmc404
     C                   Cat       xxzip2:0      fmc404
     C                   Endif

     C                   Movel     wvproj        fmc405
     C                   Movel     wvprj2        fmc406

     C                   Else

     C                   Move      wvzip         xxzip
     C     wvcty         Cat       ',':0         fmc403
     C                   Cat       wvst:1        fmc403
     C                   Cat       xxzip1:1      fmc403

     C                   If        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   Cat       '-':0         fmc403
     C                   Cat       xxzip2:0      fmc403
     C                   Endif

     C                   Movel     wvproj        fmc404
     C                   Movel     wvprj2        fmc405
     C                   Movel     *blanks       fmc406

     C                   Endif                                                  wvadr2 <> blanks

      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free

     C                   Move      *blanks       fm2001
     C                   Move      *blanks       fmc402
     C                   Move      *blanks       fmc403
     C                   Move      *blanks       fmc404
     C                   Move      *blanks       fmc405
     C                   Move      *blanks       fmc406
    
     C                   Endif                                                  wvstat = 'A'
    
     C     wvky          Reade     pwv01                                  60
    
     C                   Enddo
    
     C                   If        f3fl03 <> 'S'
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   Endif

     C                   Close     wxfmp1
    
     C                   Endsr

      *---------------------------------------------------------------------- *
      * PGMSTR -Beginning of program processing
      *---------------------------------------------------------------------- *

     C     pgmstr        Begsr
    
     C                   Endsr

      *---------------------------------------------------------------------- *
      * PGMEND -End of program processing
      *---------------------------------------------------------------------- *

     C     pgmend        Begsr
    
     C                   Endsr

      *---------------------------------------------------------------------- *
      * VALID -Subroutine to attach this form to the policy
      *---------------------------------------------------------------------- *

     C     valid         Begsr
    
     C     wvky          Setll     pwv01
     C     wvky          Reade     pwv01                                  60
    
     C     *in60         Doweq     *off

     C                   If        wvstat <> 'I'
     C                   Move      *on           crtfrm
     C                   Leave
     C                   Endif

     C     wvky          Reade     pwv01                                  60
    
     C                   Enddo
     /Free
       //*** Begin Add *** 07/27/17 ******************************************
       If ENrpdt > *Zero;
          If ELpfdt >= ENrpdt;
             crtfrm = *off;
          Endif;
       Endif;
       //*** End   Add *** 07/27/17 ******************************************
      /End-Free
     C                   Endsr

