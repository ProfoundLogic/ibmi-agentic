     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR31617 - NEW YORK FOREIGN VOLUNTARY COMPENSATION AND EMP
     *===============================================================*
     * WC310617 - NY Foreign Voluntary Compensation and Employers  . *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/09/01                                          *
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
     D qqform          c                   const('WC310617  ')
     ** Additional entry program
     D addpgm          c                   const('SR31617I  ')
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
     *
     C     fmky3         chain     pfm02                              60
     *
     ** Move the custom fields to print
     C                   movel     f3c401        fmc401
     C                   movel     f3c402        fmc402
     C                   movel     f3c403        fmc403
     C                   movel     f3c404        fmc404
     C                   movel     f3c405        fmc405
     C                   movel     f3c406        fmc406
     C                   movel     f3c407        fmc407
     C                   movel     f3c408        fmc408
     C                   movel     f3c409        fmc409
     C                   movel     f3c410        fmc410

     C                   movel     f3c411        fmc411
     C                   movel     f3c412        fmc412

     C                   movel     f3c201        fmc201
     C                   movel     f3c202        fmc202
     C                   movel     f3c203        fmc203
     C                   movel     f3c204        fmc204
     C                   movel     f3c205        fmc205
     C                   movel     f3c206        fmc206
     C                   movel     f3c207        fmc207
     C                   movel     f3c208        fmc208
     C                   movel     f3c209        fmc209
     C                   movel     f3c210        fmc210

     C                   movel     f3c101        fmc101
     C                   movel     f3c102        fmc102
     C                   movel     f3c103        fmc103
     C                   movel     f3c104        fmc104
     C                   movel     f3c105        fmc105
     C                   movel     f3c106        fmc106
     C                   movel     f3c107        fmc107
     C                   movel     f3c108        fmc108
     C                   movel     f3c109        fmc109
     C                   movel     f3c110        fmc110

     C                   movel     f3am01        fmam01
     C                   movel     f3am02        fmam02
     C                   movel     f3am03        fmam03
     C                   movel     f3am04        fmam04
     C                   movel     f3am05        fmam05

     ** Write the output record
     C                   write     pfm01
     *
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
     C                   move      *on           crtfrm
     *
     C                   endsr
