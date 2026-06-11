     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
     H*/TITLE WA117R - CMS Edit: Remind users to check the ORM and/or TPOC date
    H**************************************************************************************
    H*  Description: CMS Edit: Remind users to check the ORM and/or TPOC date             *
    H*                                                                                    *
    H*  Called from: W0080R, W0070R12                                                     *
    H*                                                                                    *
    H*  Calls to:                                                                         *
    H*                                                                                    *
    H*  Created: C:227  Project: 133WJX     Date:  11/09/10                               *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
     fwa117f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     D psds          esds                  extname(szq1p)
     D*
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*
     D @@prds          s             25
     D*
     D xxsydt          s              7  0
     D xxefdt          s              7  0
     D*
     D srdti           s              8  0
     D srdto           s             10
     D srffmt          s              8
     D srtfmt          s              8
     D srerr           s              1
    C*
     C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C* Entry key list
     C*
     C     *entry        plist
     C                   parm                    @@call            1
     C                   parm                    @@kl              1
     C                   parm                    @@yes             1
     C*
     C*** Screen display and error checking
     C*
     C                   dou       *in90 = *off
     C                   eval      *in45 = *off
     C                   select
     C                   when      @@call = 'R'
     C                   eval      *in45 = *on
     C                   when      @@call = 'C'
     C                   eval      *in45 = *off
     C                   endsl
     C*
     C                   exfmt     scr01
     C*
     C                   select
     C*
     C* If F5 is pressed
     C                   when      *inke = *on
     C                   eval      scresp = *blanks
     C*
     C* If F12 is pressed
     C                   when      *inkl = *on
     C                   leave
     C*
     C                   other
     C                   eval      *in90  = *off
     C                   exsr      errck1
     C*
     C                   endsl
     C*
     C                   enddo
     C*
     C                   move      scresp        @@yes
     C                   move      *inkl         @@kl
     C                   eval      *inlr=*on
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C* Edits for CMS Response
     C                   if        scresp <> *blanks and scresp <> 'Y'
     C                   seton                                        419042
     C                   leavesr
     C                   endif
     C*
     C                   endsr
