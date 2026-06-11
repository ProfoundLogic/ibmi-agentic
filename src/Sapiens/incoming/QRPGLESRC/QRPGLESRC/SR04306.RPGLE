     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR04306  - CA - Waiver Right Recover Others Endorsement
      *===============================================================*
      * WC040306                                                      *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 06/09/04                                          *
      * Programmer: C:61                                              *
      * Project   : CM027                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 08/12/22 C:079 Support: 148/93027                             *
      *                Populate FMC440 thru FMC442 instead of FMC401  *
      *                FMC401 thru FMC403                             *
      *                                                               *
      * 01/16/19 C:300 Support: 73970 (LUBA) (Recompile)              *
      *                Printing end from Work with End Forms          *
      *                Changes in SCOPYEN01C copybook                 *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
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
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('WC040306  ')
    
     ** Additional entry program
     D addpgm          c                   const('SR04306I  ')
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
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     ** Additional Input
    
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
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
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     C                   open      wxfmp1
    
     C     fmky3         chain     pfm02                              60
    
     ** Move the custom fields to print

     c                   eval      fma921 = f3a921
       //*** Beg Chg *** 08/12/22 *********************************************
     c************       eval      fmc401 = f3c401
     c************       eval      fmc402 = f3c402
     c************       eval      fmc403 = f3c403
     c*
       fmc440 = f3c401;
       fmc441 = f3c402;
       fmc442 = f3c403;
       //*** End Chg *** 08/12/22 *********************************************
     c                   eval      fmc404 = f3c404
     c                   eval      fmc405 = f3c405
     c                   eval      fmc406 = f3c406
     c                   eval      fmc407 = f3c407
     c                   eval      fmc408 = f3c408
     c                   eval      fmc409 = f3c409
     c                   eval      fmc410 = f3c410
     c                   eval      fmc411 = f3c411
     c                   eval      fmc412 = f3c412
     c                   eval      fmc413 = f3c413
     c                   eval      fmc414 = f3c414
     c                   eval      fmc415 = f3c415
     c                   eval      fmc416 = f3c416

     c                   eval      fmc201 = f3c201
     c                   eval      fmc202 = f3c202
     c                   eval      fmc203 = f3c203
     c                   eval      fmc204 = f3c204
     c                   eval      fmc205 = f3c205
     c                   eval      fmc206 = f3c206
     c                   eval      fmc207 = f3c207
     c                   eval      fmc208 = f3c208
     c                   eval      fmc209 = f3c209
     c                   eval      fmc210 = f3c210
     c                   eval      fmc211 = f3c211
     c                   eval      fmc212 = f3c212
     c                   eval      fmc213 = f3c213
     c                   eval      fmc214 = f3c214
     c                   eval      fmc215 = f3c215
     c                   eval      fmc216 = f3c216

     ** Write the output record
     C                   write     pfm01
    
     C                   close     wxfmp1
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
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
    
    
