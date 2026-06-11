     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *****************************************************************
      * CALLED FROM:                                                  *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 09/09/11  C:255  Support  75700                       *
      *****************************************************************
      * Revision:                                                     *
     H* 10/12/11 C:255 SUPPORT: WA045 75700                           *
     H*                         Deleted updation of USER field of     *
     H*                         file SDCMP.                           *
      *                                                               *
      *****************************************************************
     fsdcmp     uf   e             disk
     fsmarl     if   e           k disk
     d                 ds
     ddsdttm                         12  0
     d dstime                         6  0 overlay(dsdttm)
     d dsdate                         6  0 overlay(dsdttm:7)
     D srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d xxdt            s              7  0
      *
     dpsds           esds                  extname(szq1p)
      *
      *Processing starts here
     c     arky          klist
     c                   kfld                    cmkey
     c                   kfld                    cmseq#
     c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c                   time                    dsdttm
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         XXDT
     c*
     c                   read      pcm01
     c                   dow       not %eof(sdcmp)
     c*
     c     arky          chain     par01                              70
     c                   if        *in70 = *off
     c                   eval      cmique = 'Y'
     c                   else
     c                   eval      cmique = 'N'
     c                   endif
      *Update maintenance fields
     c                   eval      cmtime = dstime
     c                   eval      cmdt   = xxdt
     c**  Beg Del *****  10/12/11 **********************************
     c*********          eval      cmuser = q1user
     c**  End Del *****  10/12/11 **********************************
     c                   eval      cmpgm  = q1pgm
     c                   update    pcm01
     c*
     c                   read      pcm01
     c                   enddo
     c*
     c                   eval      *inlr = *on
