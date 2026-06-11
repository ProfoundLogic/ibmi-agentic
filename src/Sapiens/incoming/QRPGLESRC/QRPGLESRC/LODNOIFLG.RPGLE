     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE LODNOIFLG - Populate the ST1A6 field of STSTP file for the
     H*                  FROI mudule install/uninstall.                   *
    H*********************************************************************
     H* Created: 03/25/11  C:235 Project: WC1983                          *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
     H* NOTE: THERE IS A LOGIC ERROR IN THIS PROGRAM.                     *
     H*       ALL CLIENTS AFFECTED BY THIS PROGRAM HAVE ALREADY LOADED V11*
     H*       THIS PROGRAM NEEDS TO BE FIXED IF RE-USED.                  *
     H*                                                                   *
    H*********************************************************************
     fststp     uf   e           k disk
     D*********************************************************************
     D clta            s              3    dim(7) ctdata perrcd(1)
     D statea          s              2    dim(7) ctdata perrcd(1)
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
     C* Retrieve Client Id:
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   movel     CLTID         XXCLT             3
     C* Read STSTP  in loop
     C*
     C                   read      pst01                                  60
     C*
     C                   dow       *in60 = *off
     C*
     C                   z-add     1             i                 1 0
     C     xxclt         lookup    clta(I)                                99
     C                   dow       *in99 = *on  and I <= 7
     C                   if        stkey = statea(I) and
     C                             st1a6 <> 'Y'
     C                   eval      st1a6 = 'Y'
     C*
     C* Update Maint fields
     C                   movel     q1user        stuser
     C                   movel     q1pgm         stpgm
     C                   movel     dstime        sttime
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         stdt
     C                   update    pst01
     C*
     C                   else
     C                   eval      I = I+1
     C*
     C     xxclt         lookup    clta(I)                                99
     C                   endif
     C                   enddo
     C                   read      pst01                                  60
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
** clta *****************
071
114
114
132
133
135
136
** Statea****************
AL
PA
DE
CA
LA
FL
CA
