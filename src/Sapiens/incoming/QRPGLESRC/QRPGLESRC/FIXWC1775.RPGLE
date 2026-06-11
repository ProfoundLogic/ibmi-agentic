     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE FIXWC1775 - Populate value to the Deleted A/R back on
    H*********************************************************************
/*  CH* Created: 09/29/10  C:220 Project: WC1775                          *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
    H*********************************************************************
     fwmexp     uf   e           k disk
     D*********************************************************************
     D                 DS
     D  dsdttm                 1     12  0
     D  dstime                 1      6  0
     D  dsdate                 7     12  0
     D*
     D psds          esds                  extname(SZQ1P)
     C**********************************************************************
     C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C                   time                    dsdttm
     C**********************************************************************
     C* Main Line code
     C**********************************************************************
     C*
     C* Read WMEXP  in loop
     C                   read      pex01                                  60
     C*
     C                   dow       *in60 = *off
     C                   if        exarri = *blank
     C*
     C                   eval      exarri = 'A'
     C* Update Maint fields
     C                   movel     q1user        exuser
     C                   movel     q1pgm         expgm
     C                   movel     dstime        extime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         exdt
     C
     C                   update    pex01
     C                   endif
     C*
     C                   read      pex01                                  60
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C**********************************************************************
