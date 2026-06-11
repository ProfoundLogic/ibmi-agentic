     h*  (C) Copyright Fiserv Inc., 2008
    h** ======================================================================================== **
    h/TITLE - Import - Process Prompt                                                             *
    h** ======================================================================================== **
    h*  DESCRIPTION:                                                                              *
     h*    Load historical records with Assigned flag                                              *
     h*                                                                                            *
     h*  Called from:                                                                              *
     h*                                                                                            *
     h*  Calls to:                                                                                 *
     h*                                                                                            *
     h*  Created: 06/02/08  by C:191    Project: 132WKE                                            *
    h** ======================================================================================== **
     h* Revisions:                                                                                 *
     h*                                                                                            *
    h** ======================================================================================== **
     fwmacp     uf   e             disk
     f*
     d                 ds
     ddsdttm                         14  0
     ddstime                          6  0 overlay(dsdttm)
     ddsdate                          8  0 overlay(dsdttm:7)
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d psds          esds                  extname(szq1p)
    d*
      *
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      '     '       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     1             setll     pac01
     c                   read      pac01
     c* When ACCCHK <> *blank (this is to ensure the program can only be run once)
     c*        End program
     c                   if        accchk <> *blanks
     c                   eval      *inlr = *on
     c                   return
     c                   endif                                                  accchk<>*blanks

     c* Read all records in WMACP
     c                   dow       not %eof
     c* When ACCOTP = 'C' and ACCCHK = *blank; Update record
     c                   if        accotp = 'C' and accchk = *blanks
     c                   eval      accchk = 'X'
     c                   eval      acuser = q1user
     c                   eval      acpgm = q1pgm
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         acdt
     c                   movel     dstime        actime
     c*
     c                   update    pac01
     c                   endif                                                  accotp = 'C'
     c                   read      pac01
     c                   enddo

     c                   eval      *inlr = *on
