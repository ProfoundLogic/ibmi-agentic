     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR42304 - TX WAIVER OF OUR RIGHT TO RECOVER FROM OTHERS
     *===============================================================*
     * WC420304 - Texas Waiver of Our Right to Recover from Others   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 07/18/01                                          *
     * Programmer: C: 90                                             *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
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
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     *
     ** Waiver Information File
     Fwmwvl     if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqform          c                   const('WC420304  ')
     ** Additional entry program
     D addpgm          c                   const('SR42304I  ')
      *
     D xxefdt          s                   like(wvefdt)
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     ** Additional Input file
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
     *
     ** Waiver Information file
     C     wvky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C                   open      wxfmp1
     C     fmky3         chain     pfm02                              60
     *
     C                   if        f3fl03 = 'S'
     C                   move      'X'           fmfl01
     C                   move      *blank        fmfl02
     C                   else
     C                   move      *blank        fmfl01
     C                   move      'X'           fmfl02
     C                   endif
     C*
     C                   eval      fmc407 = f3c401
     C                   eval      fmc408 = f3c402
     C                   eval      fma921 = f3a921
     C                   eval      fma521 = f3a521
     C                   eval      fma923 = f3a923
     *
     C     wvky          setll     pwv01
     C     wvky          reade     pwv01                                  60
     *
     C     *in60         doweq     *off
      *
     C                   if        wvstat <> 'I'
      *
     C                   movel     wvname        fmc401
     C                   movel     wvadr1        fmc402
      *
     C                   if        wvadr2 <> *blank
     C                   movel     wvadr2        fmc403
     C                   move      wvzip         xxzip
     C     wvcty         cat       ',':0         fmc404
     C                   cat       wvst:1        fmc404
     C                   cat       xxzip1:1      fmc404
      *
     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc404
     C                   cat       xxzip2:0      fmc404
     C                   endif
     C*
     C                   movel     wvproj        fmc405
     C                   movel     wvprj2        fmc406
     C*
     C                   else
     C*
     C                   move      wvzip         xxzip
     C     wvcty         cat       ',':0         fmc403
     C                   cat       wvst:1        fmc403
     C                   cat       xxzip1:1      fmc403
      *
     C                   if        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         fmc403
     C                   cat       xxzip2:0      fmc403
     C                   endif
     C*
     C                   movel     wvproj        fmc404
     C                   movel     wvprj2        fmc405
     C                   movel     *blanks       fmc406
     C*
     C                   endif                                                  wvadr2 <> blanks
      *
     ** Write the output record
     C                   write     pfm01
      *
     C                   move      *blanks       fmc401
     C                   move      *blanks       fmc402
     C                   move      *blanks       fmc403
     C                   move      *blanks       fmc404
     C                   move      *blanks       fmc405
     C                   move      *blanks       fmc406
     *
     C                   endif                                                  wvstat = 'A'
     *
     C     wvky          reade     pwv01                                  60
     *
     C                   enddo
     *
     C                   if        f3fl03 <> 'S'
     C                   write     pfm01
     C                   endif
     C*
     C                   close     wxfmp1
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     wvky          setll     pwv01
     C     wvky          reade     pwv01                                  60
     *
     C     *in60         doweq     *off
      *
     C                   if        wvstat <> 'I'
     C                   move      *on           crtfrm
     C                   leave
     C                   endif
      *
     C     wvky          reade     pwv01                                  60
     *
     C                   enddo
     *
     C                   endsr
