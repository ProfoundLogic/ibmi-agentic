     H* (C) Copyright StoneRiver Insurance Solutions, Inc., 2009
    H*
     H*/TITLE W7480R  - Prompt for Medicare 2nd Pay Extract
    H**************************************************************************************
    H*  Description: Prompt for Medicare 2nd pay extract process                          *
    H*                                                                                    *
    H*  Called from: W7944R                                                               *
    H*                                                                                    *
    H*  Calls to: W7480C                                                                  *
    H*                                                                                    *
    H*  Created: C:65   Project: WC1819     Date:  06/12/09                               *
    H**************************************************************************************
    H*  Revisions:                                                                        *
/* 04H*       04/12/11  C: 58  Project: WC1975                                             *
/*   H*                        Add sort sequence parameter: G/S                            *
     H*                                                                                    *
    H**************************************************************************************
     C* **Beg Add *** 04/12/11 **********************************************
    C*  Report Selection Display                                                          *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7480F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
    C*  System Control WC
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*
     C* **End Add *** 04/12/11 **********************************************
     Fwmupl     if   e           k disk
     D psds          esds                  extname(szq1p)
     D @@prds          s             25
     D @@srt           s              1
     **
     **  KEY LIST FOR USER PARAMETER FILE
     **
     C     UPKEY         KLIST
     C                   KFLD                    q1user
     c*
     c* Entry key list
     c*
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call            1
     C*
     C     UPKEY         CHAIN     pup01                              60
     C                   MOVE      UPOUTQ        @@OUTQ           10
     c                   move      upco#         @@co#             3
     C*
     C* **Beg Add *** 04/12/11 *********************************************
     C                   read      swc01                                  60
     C                   if        @@call = 'E' and wcfl48 = 'Y'
     C                   eval      scprds = 'Medicare Edit Sort Selection'
     C*
     C*** Screen display and error checking
     C                   dou       *in90 = *off
     C                   exfmt     scr01
     C                   eval      *in71 = *off
     C                   eval      *in90 = *off
     C*
     C* If F3 is pressed
     C                   if        *inkc = *on
     C                   leave
     C                   endif
     C*
     C*     Edit Report sort code
     C                   if        scsort <> 'G' and scsort <> 'S'
     C                   seton                                        7190
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   eval      @@srt = scsort
     C                   endif                                                  e- @@call = E
     C*
     C                   if        *inkc = *off
     C* **End Add *** 04/12/11 *****************************************
     C*
     c                   call      'W7480C'
     c                   parm                    @@co#
     c                   parm                    @@call
     c                   parm                    @@outq
     C* **Beg Add *** 04/12/11 *****************************************
     c                   parm                    @@srt
     C                   endif                                                  e- inkc = off
     C* **End Add *** 04/12/11 *****************************************
     C*
     C                   eval      *inlr=*on
