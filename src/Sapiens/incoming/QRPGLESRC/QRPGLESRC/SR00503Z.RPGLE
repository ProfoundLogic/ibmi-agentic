     * (C) Copyright Progressive Data Solutions, Inc., 2008
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00503Z - Retrospective Premium Endorsement Attached Schd
     *===============================================================*
     * WA000503 - WC000503A Attached Schedule                        *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/25/08                                          *
     * Programmer: C:65                                              *
     * Project   : 114WRH                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
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
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     * Billing Schedule
     fwtabl     if   e           k disk    include(pbs01)
    
     * Admin/Darts Control File
     Fwmexl     if   e           k disk
    
      ** Additional Work File
     Fwxfmp3    o    e             disk    usropn
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  stand alone
     *=========================================================================
     d x1              s              2  0
     d svemp#          s              9  0
     d x               s              4  0
     d x1carr          s             20
     d xpcarr          s             20
     d xx30            s             30
     d xx26            s             26
     d xx14            s             14
     d xx10            s             10
     d xx6             s              6
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('WC000503Z ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
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
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

      * Admin/Darts Control File

     c     exky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

      * Employer Detail

     c     elky3         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr

      * Carrier Policy # file

     c     rpky3         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    xxfyr

      * Billing Schedule

     c     bsky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10
    
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  CLRDAT - Clear data fields for next page.
     *=========================================================================
     C     clrdat        begsr
     c*
     c                   clear                   fmnm01
     c                   clear                   fmnm02
     c                   clear                   fmnm03
     c                   clear                   fmnm04
     c                   clear                   fmnm05
     c                   clear                   fmnm06
     c                   clear                   fmnm07
     c                   clear                   fmnm08
     c                   clear                   fmnm09
     c                   clear                   fmnm10
     c                   clear                   fmnm11
     c                   clear                   fmnm12
     c                   clear                   fmnm13
     c                   clear                   fmnm14
     c*
     c                   clear                   fmc101
     c                   clear                   fmc102
     c                   clear                   fmc103
     c                   clear                   fmc104
     c                   clear                   fmc105
     c                   clear                   fmc106
     c                   clear                   fmc107
     c                   clear                   fmc108
     c                   clear                   fmc109
     c                   clear                   fmc110
     c                   clear                   fmc111
     c                   clear                   fmc112
     c                   clear                   fmc113
     c                   clear                   fmc114
     c                   clear                   fmc115
     c                   clear                   fmc116
     c*
     c                   clear                   fmc201
     c                   clear                   fmc202
     c                   clear                   fmc203
     c                   clear                   fmc204
     c                   clear                   fmc205
     c                   clear                   fmc206
     c                   clear                   fmc207
     c                   clear                   fmc208
     c                   clear                   fmc209
     c                   clear                   fmc210
     c                   clear                   fmc211
     c                   clear                   fmc212
     c                   clear                   fmc213
     c                   clear                   fmc214
     c*
     c                   clear                   fmc301
     c                   clear                   fmc302
     c                   clear                   fmc303
     c                   clear                   fmc304
     c                   clear                   fmc305
     c                   clear                   fmc306
     c                   clear                   fmc307
     c                   clear                   fmc308
     c                   clear                   fmc309
     c                   clear                   fmc310
     c                   clear                   fmc311
     c                   clear                   fmc312
     c                   clear                   fmc313
     c                   clear                   fmc314
     c                   clear                   fmc315
     c                   clear                   fmc316
     c*
     c                   clear                   fmc401
     c                   clear                   fmc402
     c                   clear                   fmc403
     c                   clear                   fmc404
     c                   clear                   fmc405
     c                   clear                   fmc406
     c                   clear                   fmc407
     c                   clear                   fmc408
     c                   clear                   fmc409
     c                   clear                   fmc410
     c                   clear                   fmc411
     c                   clear                   fmc412
     c                   clear                   fmc413
     c                   clear                   fmc414
     c                   clear                   fmc415
     c                   clear                   fmc416
     c                   clear                   fmc417
     c                   clear                   fmc418
     c                   clear                   fmc419
     c                   clear                   fmc420
     c                   clear                   fmc421
     c                   clear                   fmc422
     c                   clear                   fmc423
     c                   clear                   fmc424
     c*
     C                   endsr
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr
    
     c                   open      wmrpl
    
     ** open work file
     c                   open      wxfmp3
    
     *    initialize
     c                   clear                   pfm03
     c                   movel(p)  xxform        fmform
    
     C                   Eval      srdti = elpfdt
     C                   Call      'SRCVTDAT'    dtmdyy
     C                   MoveL     srdto         fmeedt
    
     c                   z-add     0             svemp#
     c                   z-add     1             x
     c*
     c                   move      *blank        xx1st             1
     c*
     c                   z-add     1             xxline            2 0
     c                   z-add     1             xxspot            2 0
     c*
     c* Loop through all employers in group to list their policy #'s.
     c*
    
     c     elky3         Chain     pel02                              60
    
     c     *in60         doweq     *off
     c     x             andlt     9999
     *
     c                   if        elemp# <> xxemp#
     c                             and elemp#  <> svemp#
     c*
     c     rpky3         Chain     prp01                              60
     c                   if        *in60 = *off and rpcap# <> *blanks
     c                   movel     rpcap#        x1carr
     c                   else
     c                   move      elemp#        xaemp#            9
     c                   move      xxfyr         xafyr             3
     c                   movel(p)  xxfnd         x1carr
     c     x1carr        cat       xaemp#:0      x1carr
     c     x1carr        cat       xafyr:0       x1carr
     c                   endif
     c*
     c                   exsr      setcar
     c*
     c                   add       1             x
     c                   z-add     elemp#        svemp#
     c*
     c                   endif
     *
     c     elky3         reade     pel02                                  60
     c                   EndDo
    
    
     ** Write the output record
     C                   write     pfm01
     c*
     c                   if        xx1st = *blank
     C                   write     pfm03
     c                   move      'Y'           xx1st
     c                   endif
     *
     ** Close the endorsement form workfile
     C                   close     wxfmp3
    
     c                   close     wmrpl
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr

     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  SETCAR - Set the carrier policy# into data field.
     *=========================================================================
     C     SETCAR        begsr

     c                   select
     c*
     c     xxline        wheneq    1
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm01
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm01
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm01
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc201
     c                   endsl
     c*
     c     xxline        wheneq    2
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm02
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm02
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm02
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc202
     c                   endsl
     c*
     c     xxline        wheneq    3
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm03
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm03
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm03
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc203
     c                   endsl
     c*
     c     xxline        wheneq    4
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm04
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm04
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm04
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc204
     c                   endsl
     c*
     c     xxline        wheneq    5
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm05
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm05
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm05
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc205
     c                   endsl
     c*
     c     xxline        wheneq    6
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm06
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm06
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm06
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc206
     c                   endsl
     c*
     c     xxline        wheneq    7
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm07
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm07
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm07
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc207
     c                   endsl
     c*
     c     xxline        wheneq    8
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm08
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm08
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm08
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc208
     c                   endsl
     c*
     c     xxline        wheneq    9
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm09
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm09
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm09
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc209
     c                   endsl
     c*
     c     xxline        wheneq    10
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm10
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm10
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm10
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc210
     c                   endsl
     c*
     c     xxline        wheneq    11
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm11
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm11
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm11
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc211
     c                   endsl
     c*
     c     xxline        wheneq    12
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm12
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm12
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm12
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc212
     c                   endsl
     c*
     c     xxline        wheneq    13
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm13
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm13
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm13
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc213
     c                   endsl
     c*
     c     xxline        wheneq    14
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmnm14
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx26
     c                   move      xx26          fmnm14
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx6
     c                   move      xx6           fmnm14
     c                   move(p)   x1carr        xx14
     c                   movel(p)  xx14          fmc214
     c                   endsl
     c*
     c     xxline        wheneq    15
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc301
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc301
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc401
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc401
     c                   endsl
     c*
     c     xxline        wheneq    16
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc302
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc302
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc402
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc402
     c                   endsl
     c*
     c     xxline        wheneq    17
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc303
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc303
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc403
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc403
     c                   endsl
     c*
     c     xxline        wheneq    18
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc304
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc304
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc404
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc404
     c                   endsl
     c*
     c     xxline        wheneq    19
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc305
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc305
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc405
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc405
     c                   endsl
     c*
     c     xxline        wheneq    20
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc306
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc306
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc406
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc406
     c                   endsl
     c*
     c     xxline        wheneq    21
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc307
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc307
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc407
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc407
     c                   endsl
     c*
     c     xxline        wheneq    22
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc308
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc308
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc408
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc408
     c                   endsl
     c*
     c     xxline        wheneq    23
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc309
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc309
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc409
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc409
     c                   endsl
     c*
     c     xxline        wheneq    24
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc310
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc310
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc410
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc410
     c                   endsl
     c*
     c     xxline        wheneq    25
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc311
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc311
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc411
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc411
     c                   endsl
     c*
     c     xxline        wheneq    26
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc312
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc312
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc412
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc412
     c                   endsl
     c*
     c     xxline        wheneq    27
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc313
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc313
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc413
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc413
     c                   endsl
     c*
     c     xxline        wheneq    28
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc314
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc314
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc414
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc414
     c                   endsl
     c*
     c     xxline        wheneq    29
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc315
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc315
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc415
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc415
     c                   endsl
     c*
     c     xxline        wheneq    30
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc316
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc316
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc416
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc416
     c                   endsl
     c*
     c     xxline        wheneq    31
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc101
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc417
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc417
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc417
     c                   move(p)   x1carr        fmc102
     c                   endsl
     c*
     c     xxline        wheneq    32
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc103
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc418
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc418
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc418
     c                   move(p)   x1carr        fmc104
     c                   endsl
     c*
     c     xxline        wheneq    33
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc105
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc419
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc419
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc419
     c                   move(p)   x1carr        fmc106
     c                   endsl
     c*
     c     xxline        wheneq    34
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc107
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc420
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc420
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc420
     c                   move(p)   x1carr        fmc108
     c                   endsl
     c*
     c     xxline        wheneq    35
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc109
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc421
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc421
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc421
     c                   move(p)   x1carr        fmc110
     c                   endsl
     c*
     c     xxline        wheneq    36
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc111
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc422
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc422
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc422
     c                   move(p)   x1carr        fmc112
     c                   endsl
     c*
     c     xxline        wheneq    37
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc113
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc423
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc423
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc423
     c                   move(p)   x1carr        fmc114
     c                   endsl
     c*
     c     xxline        wheneq    38
     c*
     c                   select
     c     xxspot        wheneq    1
     c                   movel(p)  x1carr        fmc115
     c                   move(p)   x1carr        xx10
     c                   movel(p)  xx10          fmc424
     c     xxspot        wheneq    2
     c                   movel(p)  x1carr        xx30
     c                   move      xx30          fmc424
     c     xxspot        wheneq    3
     c                   movel(p)  x1carr        xx10
     c                   move      xx10          fmc424
     c                   move(p)   x1carr        fmc116
     c                   endsl
     c*
     c                   endsl
     c*
     c* Increment line and spot counters.  Write record if page is full.
     c*
     c                   if        xxline = 38 and xxspot = 3

     C                   write     pfm01
     c*
     c                   if        xx1st = *blank
     C                   write     pfm03
     c                   move      'Y'           xx1st
     c                   endif
     c*
     c                   exsr      clrdat
     c*
     c                   z-add     1             xxline
     c                   z-add     1             xxspot

     c                   else

     c                   add       1             xxspot
     c                   if        xxspot > 3
     c                   add       1             xxline
     c                   z-add     1             xxspot
     c                   endif

     c                   endif

     c                   move      x1carr        xpcarr

     C                   endsr
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     exky          chain     pex01                              60
    
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))
     c                             and exretg = 'Y'
    
     C                   move      *on           crtfrm
    
     C                   endif
    
     C                   endsr
     *=========================================================================
    
