     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE FIXWC1795 - Populate the OFQTLV field of SDOFP file for the
     H*                  existing records with value 'L'                  *
    H*********************************************************************
     H* Created: 01/12/11  C:235 Project: WC1795                          *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
    H*********************************************************************
     fsdofp     uf   e           k disk
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
     C* Read SDOFP  in loop
     C                   read      pof01                                  60
     C*
     C                   dow       *in60 = *off
     C                   if        ofqtlv = *blank
     C*
     C                   eval      ofqtlv = 'L'
     C* Update Maint fields
     C                   movel     q1user        ofuser
     C                   movel     q1pgm         ofpgm
     C                   movel     dstime        oftime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         ofdt
     C
     C                   update    pof01
     C                   endif
     C*
     C                   read      pof01                                  60
     C                   enddo
     C*
     C                   eval      *inlr = *on
