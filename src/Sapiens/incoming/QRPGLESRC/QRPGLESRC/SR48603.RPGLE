     * (C) Copyright Progressive Data Solutions, Inc., 2003
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR48603 - WI FOREIGN COVERAGE ENDORSEMENT
     *===============================================================*
     * WC480603  - Wisconsin Foreign Coverage Endorsement            *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 04/22/03                                          *
     * Programmer: C:61                                              *
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
     *
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     *
     *
     *
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
      *
      *
      *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
      *
     ** Form ID
     D qqform          c                   const('WC480603  ')
      *
     ** Additional entry program
     D addpgm          c                   const('SR48603I  ')
      *
      *
      *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
      *
     ** Entry parameters
      *
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
     *
     ** Additional Input file
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
    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C                   open      wxfmp1
     C     fmky3         chain     pfm02                              60
     *
     ** Move the custom fields to print
     *
     C                   MOVEL     F3C101        fmc101
     C                   MOVEL     F3C102        fmc102
     C                   MOVEL     F3C103        fmc103
     C                   MOVEL     F3C104        fmc104
     C                   MOVEL     F3C105        fmc105
     C                   MOVEL     F3C106        fmc106
     C                   MOVEL     F3C107        fmc107
     C                   MOVEL     F3C108        fmc108
     C                   MOVEL     F3C109        fmc109
     C                   MOVEL     F3C110        fmc110
     C*
     C                   MOVEL     F3C201        fmc201
     C                   MOVEL     F3C202        fmc202
     C                   MOVEL     F3C203        fmc203
     C                   MOVEL     F3C204        fmc204
     C                   MOVEL     F3C205        fmc205
     C                   MOVEL     F3C206        fmc206
     C                   MOVEL     F3C207        fmc207
     C                   MOVEL     F3C208        fmc208
     C                   MOVEL     F3C209        fmc209
     C                   MOVEL     F3C210        fmc210
     C*
     C                   MOVEL     F3C301        fmc301
     C                   MOVEL     F3C302        fmc302
     C                   MOVEL     F3C303        fmc303
     C                   MOVEL     F3C304        fmc304
     C                   MOVEL     F3C305        fmc305
     C                   MOVEL     F3C306        fmc306
     C                   MOVEL     F3C307        fmc307
     C                   MOVEL     F3C308        fmc308
     C                   MOVEL     F3C309        fmc309
     C                   MOVEL     F3C310        fmc310
     C*
     C                   MOVEL     F3A921        fma921
     C                   MOVEL     F3A922        fma922
     C                   MOVEL     F3A923        fma923
     C                   MOVEL     F3A924        fma924
     C                   MOVEL     F3A925        fma925
     C*
     C                   MOVEL     F3C111        fmc111
     C                   MOVEL     F3C112        fmc112
     C                   MOVEL     F3C113        fmc113
     C*
     C                   MOVEL     F3AM01        fmam01
     C                   MOVEL     F3AM02        fmam02
     C                   MOVEL     F3AM03        fmam03
     C*
     C                   MOVEL     F3A521        fma521
     C                   MOVEL     F3A522        fma522
     C                   MOVEL     F3A523        fma523
     C*
     C                   MOVEL     F3AM04        fmam04
     C                   MOVEL     F3AM05        fmam05
     C                   MOVEL     F3AM06        fmam06
     *
     ** Write the output record
     *
     C                   write     pfm01
     C                   close     wxfmp1
     *
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C                   move      *on           crtfrm
     *
     C                   endsr
     *
     *
