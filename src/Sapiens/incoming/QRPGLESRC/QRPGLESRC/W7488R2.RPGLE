     H* (C) Copyright StoneRiver Insurance Solutions, Inc., 2010
    H*
     H*/TITLE W7488R2 - Prompt for Medicare Query selection
    H**************************************************************************************
    H*  Description: Prompt for Medicare Query Selection screen                           *
    H*                                                                                    *
    H*  Called from: W7944R                                                               *
    H*                                                                                    *
    H*  Calls to: W7488C                                                                  *
    H*                                                                                    *
    H*  Created: C:251  Project: WC1944     Date:  10/05/10                               *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7488F2   cf   e             workstn
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
     C                   parm                    @@prds
     C                   parm                    @@call            1
     C*
     C                   eval      scprds = 'Medicare Query File Selection'
     C*
     C*** Screen display and error checking
     C*
     C                   dou       *in90 = *off
     C                   exfmt     scr01
     C                   eval      *in76 = *off
     C*
     C                   select
     C*
     C* If F5 is pressed
     C                   when      *inke = *on
     C                   eval      scstat = *blanks
     C                   eval      scltfl = *blanks
     C                   eval      scefdt = *zeros
     C*
     C* If F3 is pressed
     C                   when      *inkc = *on
     C                   leave
     C*
     C* If F7 is pressed
     C                   when      *inkg = *on
     C                   eval      *in90  = *off
     C                   exsr      errck1
     C*
     C                   if        *in90  = *off
     C*
     C                   eval      @@stat = scstat
     C*
     C* Internally convert 'L' on screen to 'I' status of file for lost time flag
     C*
     C                   if        scltfl = 'L'
     C                   eval      @@ltfl = 'I'
     C                   else
     C                   eval      @@ltfl = scltfl
     C                   endif
     C*
     C* Pass zeros in effective date if open claim
     C*
     C                   if        scstat = 'O'
     C                   move      *zeros        @@efdt
     C                   else
     C                   move      xxefdt        @@efdt
     C                   endif
     C*
     C                   call      'W7488C'
     C                   parm                    @@stat            1
     C                   parm                    @@efdt            7
     C                   parm                    @@ltfl            1
     C                   endif
     C*
     C                   other
     C                   eval      *in90  = *off
     C                   exsr      errck1
     C                   if        *in90 = *off
     C                   seton                                        7690
     C                   endif
     C*
     C                   endsl
     C*
     C                   enddo
     C*
     C                   eval      *inlr=*on
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C* Edits for Claim status flag
     C                   if        scstat <> *blanks and scstat <> 'C'
     C                             and scstat <> 'O'
     C                   seton                                        719080
     C                   leavesr
     C                   endif
     C*
     C* Date cannot be blank for closed claims
     C*
     C                   if        scstat = 'C' and scefdt = *zeros
     C                   seton                                        729081
     C                   leavesr
     C                   endif
     C
     C* Date should be blank for open claims
     C*
     C                   if        scstat = 'O' and scefdt <> *zeros
     C                   seton                                        779081
     C                   leavesr
     C                   endif
     C
     C*
     C* Effective Date must be valid
     C                   if        scefdt <> *zeros
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        739081
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Effective Date should not be > than system date
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxSYDT
     C*
     C                   Z-ADD     scefdt        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxefdt
     C*
     C                   if        scefdt <> *zeros AND xxEFDT > xxSYDT
     C                   seton                                        749081
     C                   leavesr
     C                   endif
     C*
     C* Edits for Lost time flag
     C                   if        scltfl <> *blanks and scltfl <> 'L'
     C                             and scltfl <> 'M'
     C                   seton                                        759082
     C                   leavesr
     C                   endif
     C*
     C                   endsr
