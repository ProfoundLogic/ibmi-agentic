    H*****************************************************************
    H** (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2003
    H*****************************************************************
    H*    PROGRAM: F9620RMA                                          *
    H*    DESCRIPTION: MA DATA SUBMISSION (PROMPT)                   *
    H*                                                               *
    H*    CALLED FROM:                                               *
    H*      MENU                                                     *
    H*    CALLS TO:                                                  *
    H*      F9621RMA - MA DATA SUBMISSION FOR BACK INJURIES          *
    H*****************************************************************
    H** CREATED:                                                     *
    H*  07/30/03 C:79 PROJ: MA013                                    *
    H*****************************************************************
    H** REVISIONS:
    H*
    H*****************************************************************
    H*  INDICATOR USAGE:                                             *
    H*  22 - ERROR IF TO DATE FIELD IS BLANK
    H*  23 - ERROR IF FROM DATE FIELD IS BLANK
    H*  26 - ERROR IF FROM DATE NOT VALID
    H*  27 - ERROR IF TO DATE NOT VALID
    H*  28 - ERROR IF DATES NOT ASCENDING
    H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff9620fma  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fsmcol     if   e           k disk
     Fsmupl     if   e           k disk
    F*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
    D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D qqcall          s              1
     D qqprds          s             40
     D srdfld          s             80
     D srdlen          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxco#           s              3  0
     D xxfdt           s              7  0
     D xxfrdt          s              8
     D xxtdt           s              7  0
     D xxtodt          s              8
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C* Key List for the Company Master File
    C*
     C     cokey         klist
     C                   kfld                    upco#
    C*
    C* Key List for the User Parameter Master File
    C*
     C     upkey         klist
     C                   kfld                    q1user
    C*
    C*  Entry Parameters
    C*
     C     *entry        plist
     C                   parm                    qqprds
     C                   parm                    qqcall
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   clear                   upco#
     C                   endif
    C*
     C                   time                    dsdttm
    C*
    C*  GET COMPANY NAME & CENTER IF EXISTS
    C*
     C     strt01        tag
     C     cokey         chain     pco01                              60
     C                   if        *in60 = *on
     C                   clear                   scconm
     C                   else
     C                   movel(p)  coname        srdfld
     C                   call      'SRCENTER'
     C                   parm                    srdfld
     C                   parm      40            srdlen
     C                   movel     srdfld        scconm
     C                   endif
    C*
    C* Clear all fields before screen is loaded
    C*
     C                   clear                   scfrdt
     C                   clear                   sctodt
    C*
    C* Get the Requested Record, Load Information
    C*
     C                   move      q1user        scuser
     C                   move      qqprds        scprds
    C*
    C* Load the Record Format Information for the First Time
    C*
     C                   clear                   q2recf
     C                   movel     'SCR01'       q2recf
    C*
    C* Display Screen Formats
    C*
     C     strt          tag
    C*
     C                   if        q2recf = 'SCR01'
     C                   exfmt     scr01
     C                   endif
    C*
    C* Process All Command Keys
    C*
    C************************************
    C* F3 = EXIT                        *
    C************************************
     C                   if        *inkc = *on
     C                   move      *on           *inlr
     C                   goto      term
     C                   endif
    C*
     C     q2recf        cabeq     'SCR01'       scr01t
    C*
    C****************************
    C* CHOOSE PROCESSING        *
    C****************************
     C     scr01t        tag
    C*
    C* Validate the Entered "FROM DATE"
    C*
     C                   if        scfrdt = *zeros
     C                   move      *on           *in23
     C                   goto      strt
     C                   endif
    C*
     C                   eval      srdti = scfrdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scfrdt
     C                   if        srerr = '1'
     C                   move      *on           *in26
     C                   goto      strt
     C                   endif
    C*
    C* Validate the Entered "TO DATE"
    C*
     C                   if        sctodt = *zeros
     C                   move      *on           *in22
     C                   goto      strt
     C                   endif
    C*
     C                   eval      srdti = sctodt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sctodt
     C                   if        srerr = '1'
     C                   move      *on           *in27
     C                   goto      strt
     C                   endif
    C*
    C* Validate the Entered Dates to Verify Ascending Order
    C*
     C                   eval      srdti = scfrdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxfdt
     C                   eval      srdti = sctodt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtdt
     C     xxfdt         cabgt     xxtdt         strt                 28
    C*
    C*  Call data submission program
    C*
     C                   eval      xxco# = upco#
     C                   movel     scfrdt        xxfrdt
     C                   movel     sctodt        xxtodt
     C                   call      'F9621RMA'
     C                   parm                    xxfrdt
     C                   parm                    xxtodt
    C*
    C* End the Program
    C*
     C     term          tag
     C                   move      *on           *inlr
