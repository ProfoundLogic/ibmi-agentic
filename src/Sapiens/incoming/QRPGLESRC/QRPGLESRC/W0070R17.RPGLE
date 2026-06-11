     H* (c) 2012 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
    H*
     H dftactgrp(*no) actgrp(*caller)
      /title Add, Update/Display Subrogation Information for a claim
      *================================================================
     h* PROGRAM DESCRIPTION:
     h*
     h*   Add, Update and/or display Claim Subrogation Information
     h*   for Claim Displayed in program W0070R09
     h*
     h*   Called from: W0070R09
     h*
     h*   Calls to:    None
     h*
     h* Created 03/23/02  C:132  Project 132WBO
     h*
      *================================================================
      *
      * REVISIONS:
      *
      * 05/25/12 C:282 Project: WC2109
      *                Added Claim Contacts email field
      *
      * 09/14/05  C: 132 WC132 4890 Make Adjustor field optional
      *
      * 08/17/05  C: 132 132WBO - Screen Changes for Zip Codes
      *                                                               *
      *================================================================
     FW0070F17  cf   e             workstn
    F*
    F*  Group Master file
     Fwmfnl     if   e           k disk
    F*
    F*  Physical file definition (for maintaining record)
     Fwmsbp     uf a e             disk
     F                                     rename(psb01:dbrcdf)
    F*
    F*  Physical file definition (for maintaining record)
     Fwmsbl     if   e           k disk
     F                                     infds(dbfdsf)
    F*
    F*  Group Master file
     Fststl3    if   e           k disk
    F*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Subrogation Information Display'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     d usaDate         s               d   datfmt(*usa)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d dbfdsf          ds
     d   dbrrnf              397    400B 0
    D*
    D* Responsible Party Zip Code
     D                 ds
     D sbrzip                         9  0
     D  xxrzp1                        5  0 overlay(sbrzip)
     D  xxrzp2                        4  0 overlay(sbrzip:6)
    D*
    D* Insurance Company Zip Code
     D                 ds
     D sbizip                         9  0
     D  xxizp1                        5  0 overlay(sbizip)
     D  xxizp2                        4  0 overlay(sbizip:6)
    D*
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@co#           s              3
     D @@fnd           s              3
     D @@fyr           s              3
     D @@case          s              7
     d @@rrn           s              9  0
     d qqrrn           s              9  0
     D xxco#           s              3  0 inz
     D xxfnd           s              3  0 inz
     D xxfyr           s              3  0 inz
     D xxcase          s              7  0 inz
     D zzco#           s              3  0 inz
     D zzfnd           s              3  0 inz
     D zzfyr           s              3  0 inz
     D zzcase          s              7  0 inz
     D Error           s              1n
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

     c     dtusis        plist
     c                   parm                    srdti
     c                   parm      '*USA'        srffmt
     c                   parm      '*ISO'        srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================

     c     sbky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxcase

     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C
     C     *Like         Define    STkey         XXkey
     C
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C
     C                   Eval      Error = *OFF
     C
     C*  Edits for Responsible Party Fields
     C
     C*  Name cannot be blank if 1st Addr line or City or State > BLANKS
     C                   IF        scrnam = *blanks AND scRad1 <> *blanks OR
     C                             scrnam = *blanks AND scrcty  <> *blanks  OR
     C                             scrnam = *blanks AND scrst  <> *blanks
     C                   Eval      *in71 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             scrnam <> *blanks AND scrad1 = *blanks
     C                   Eval      *in72 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             scrad1 <> *blanks AND scrcty  = *blanks
     C                   Eval      *in73 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             scrcty <> *blanks AND scrst  = *blanks
     C                   Eval      *in74 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND scrst <> *blanks
     C                   movel(P)  scrst         xxkey
     C     XXkey         Chain     pst01                              60
     C                   IF        *in60
     C                   Eval      *in75 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             scsldt <= *Zeros
     C                   Eval      *in76 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C* Statue Limitations Date must be entered and be a valid Date
     C                   IF        NOT Error AND
     C                             scsldt > *Zeros
     C                   eval      srdti = scsldt
     C                   call      'SRCVTDAT'    datchk
     C                   movel(p)  srdto         sbsldt
     C                   IF        SRERR = '1'
     C                   Eval      *in76 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C                   EndIf
     C
     C*  Edits for Insurance Company Fields
     C
     C*  Name cannot be blank if 1st Addr line or City or State > BLANKS
     C                   IF        scinam = *blanks AND sciad1 <> *blanks OR
     C                             scinam = *blanks AND scicty  <> *blanks  OR
     C                             scinam = *blanks AND scist  <> *blanks
     C                   Eval      *in77 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             scinam <> *blanks AND sciad1 = *blanks
     C                   Eval      *in78 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             sciad1 <> *blanks AND scicty  = *blanks
     C                   Eval      *in79 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND
     C                             scicty <> *blanks AND scist  = *blanks
     C                   Eval      *in80 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C
     C                   IF        NOT Error AND scist <> *blanks
     C                   movel(P)  scist         xxkey
     C     XXkey         Chain     pst01                              60
     C                   IF        *in60
     C                   Eval      *in81 = *on
     C                   Eval      Error = *ON
     C                   EndIf
     C                   EndIf
     C
     C* Adjustor Name is required
     C
     C                   IF        Error
     C                   Eval      *in90 = *ON
     C                   EndIf
     C
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C
     C     sbky          chain     psb01                              60
     C                   IF        not *in60
     C
     C                   eval      srdti = sbsldt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel(p)  srdto         scsldt
     C
     C                   movel(p)  sbrnam        scrnam
     C                   movel(p)  sbrad1        scrad1
     C                   movel(p)  sbrad2        scrad2
     C                   movel(p)  sbrcty        scrcty
     C                   movel(p)  sbrst         scrst
     C                   z-add     xxrzp1        scrzp1
     C                   z-add     xxrzp2        scrzp2
     C                   z-add     sbrphn        scrphn
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scRmad = SBrmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C
     C                   movel(p)  sbinam        scinam
     C                   movel(p)  sbiad1        sciad1
     C                   movel(p)  sbiad2        sciad2
     C                   movel(p)  sbicty        scicty
     C                   movel(p)  sbist         scist
     C                   z-add     xxizp1        scizp1
     C                   z-add     xxizp2        scizp2
     C                   z-add     sbiphn        sciphn
     C                   movel(P)  sbipol        scipol
     C                   movel(P)  sbiadj        sciadj
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scImad = SBimad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C
     C                   ELSE
     C
     C                   IF        @@MODE = 'A'
     C                   move      *blanks       scrnam
     C                   move      *blanks       scrad1
     C                   move      *blanks       scrad2
     C                   move      *blanks       scrcty
     C                   move      *blanks       scrst
     C                   z-add     *zeros        scrzp1
     C                   z-add     *zeros        scrzp2
     C                   z-add     *zeros        scrphn
     C                   z-add     *zeros        scsldt
     C                   move      *blanks       scinam
     C                   move      *blanks       sciad1
     C                   move      *blanks       sciad2
     C                   move      *blanks       scicty
     C                   move      *blanks       scist
     C                   z-add     *zeros        scizp1
     C                   z-add     *zeros        scizp2
     C                   z-add     *zeros        sciphn
     C                   move      *blanks       scipol
     C                   move      *blanks       sciadj
     C
     C                   ELSE
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   Endif
     C
     C                   endif                                                  If *in60
     C
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C
     C                   z-add     xxco#         sbCO#
     C                   z-add     scfnd         sbfnd
     C                   z-add     scfyr         sbfyr
     C                   z-add     sccase        sbcase
     C
     C                   movel(p)  scrnam        sbrnam
     C                   movel(p)  scrad1        sbrad1
     C                   movel(p)  scrad2        sbrad2
     C                   movel(p)  scrcty        sbrcty
     C                   movel(p)  scrst         sbrst
     C                   z-add     scrzp1        xxrzp1
     C                   z-add     scrzp2        xxrzp2
     C                   z-add     scrphn        sbrphn
     C                   eval      srdti = scsldt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         sbsldt
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       SBrmad = scRmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C
     C                   movel(p)  scinam        sbinam
     C                   movel(p)  sciad1        sbiad1
     C                   movel(p)  sciad2        sbiad2
     C                   movel(p)  scicty        sbicty
     C                   movel(p)  scist         sbist
     C                   z-add     scizp1        xxizp1
     C                   z-add     scizp2        xxizp2
     C                   z-add     sciphn        sbiphn
     C                   movel(p)  scipol        sbipol
     C                   movel(p)  sciadj        sbiadj
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       SBimad = scImad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     @@mode        comp      'A'                                    40
     C     @@mode        comp      'D'                                    09

     C                   move      @@co#         xxco#
     C                   move      @@fnd         xxfnd
     C                   move      @@fyr         xxfyr
     C                   move      @@case        xxcase
     C
     C     fnky          chain     pfn01                              60
     C                   IF        not *in60
     C                   movel     fnname        scfnam
     C                   z-add     fnfnd         scfnd
     C                   z-add     xxfyr         scfyr
     C                   z-add     xxcase        sccase
     C
     C     sbky          chain     psb01                              61
     C                   If        *in61
     C                   If        @@mode = 'D'
     C                   move      *on           *inKC
     C                   endif                                                  *in61 & @@mode = C
     C                   If        @@mode = 'C'
     C                   move      'A'           @@mode
     C                   endif                                                  *in61 & @@mode = C
     C                   else
     C                   endif                                                  *in61 & @@mode = C
     C
     C                   else
     C
     C                   z-add     ##fnd         scfnd
     C
     C
     C                   endif                                                  If not *IN60
     C
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C                   Z-ADD     DBRRNF        @@RRN
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      sbuser = q1user
     C                   eval      sbpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sbtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         sbdt
     c                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
B01  C
     C                   exsr      mvscdb
     C                   eval      sbuser = q1user
     C                   eval      sbiusr = q1user
     C                   eval      sbpgm = q1pgm
     C                   eval      sbipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sbtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         sbdt
     C                   movel(p)  srdto         sbidt
     C                   write     dbrcdf
     C                   feod      wmsbL
     C
     C                   eval      @@mode = 'C'
     C
     C                   endsr
