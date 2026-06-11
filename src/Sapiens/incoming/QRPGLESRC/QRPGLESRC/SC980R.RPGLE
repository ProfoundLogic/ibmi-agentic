     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *========================================================================
     H/TITLE SC980R - Print All Remarks Prompt
      *========================================================================
      * DESCRIPTION:
      *  This program will allow the user to choose a Group or Employer or c
      *  claim to process report.
      *
      * CREATED   : 07/17/02
      * PROGRAMMER: C:002
      * PROJECT   : 114IEC
      *========================================================================
      * REVISIONS:
      *
      * 07/11/05 C:62  Proj: DR193
      *                Added 'Output type' field to screen and processing.
      *
      * 11/27/02 C:87  Proj: 114IEC
      *                Added 'Print Server ID' field to screen and processing.
      *                Pass SCPSID to submitter program.
      *
      * 07/24/02 C:87  Proj: 114IEC
      *                Added error check to ensure users enter values for an
      *                Employer OR Year/Case combo, but not both (but they can be
      *                both left blank).  Added a confirmation screen which
      *                displays the users selections and allows them to continue
      *                or to cancel.
      *
      * ----------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ----------------------------------------------------------------------
      *========================================================================
      * FILE USAGE:
      *
      *  File    Description                    Rcdfmt  Key
      *
      *========================================================================
      * INDICATOR USAGE:
      *  Global Indicators
      *   60 - General chain.
      *   90 - General error.
      *
      *  Error Indicators
      *
      *
      *========================================================================
      * NOTES:
      *
      *
      *========================================================================

      *========================================================================
      * File Definitions
      *========================================================================
      * Must use INFDS(WSDS) on workstation for FKeys to work.
     FSC980F    CF   E             WORKSTN INFDS(WSDS)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     Fwmemp     if   e             disk
     Fwmeml     if   e           k disk    rename(pem01:pem02)
     F                                     infds(dbfds)
     Fwdelp     if   e             disk
     Fwdell     if   e           k disk    rename(pel01:pel02)
     Fwmfnl     if   e           k disk
     F                                     infds(dbfds1)
     Fwmcmp     if   e             disk
     Fwmcml1    if   e           k disk    rename(pcm01:pcm02)
     F                                     infds(dbfds2)
     F***  begin add  ***  11/27/02  ***************************************
     Fimpsl1    if   e           k disk
     F***  end   add  ***  11/27/02  ***************************************

      * Program-specific includes
      /COPY SC980RPR

      *========================================================================
      * Variable Definitions
      *========================================================================
     D MAX_SCREEN      C                   CONST(01)
     D COMAPI          C                   Const('WA001R')
     D EMPSEARCH       C                   const('W0050R')
     D GRPSEARCH       C                   const('W0010R')
     D CLMSEARCH       C                   const('W0070R')
     D***  begin add  ***  11/27/02  ***************************************
     D PIDSEARCH       C                   const('SC970R')
     D***  end   add  ***  11/27/02  ***************************************
     D PDSNAME         C                   CONST('StoneRiver, Inc.')

     D cancelled       s              1a
     D callMode        s              1a
     D company         S              3  0
     D pCompany        S              3a
     D year            S              3  0
     D pYear           S              3a
     D case            S              7  0
     D pCase           S              7a
     D coName          S             40a
     D ended           s              1a
     D group           S              3  0
     D pGroup          S              3a
     D pEmployer       s              9a
     D groupName       S             40a
     D*** begin add *** 07/11/05 ******************************************
     D poutt           s              1a
     D pfrdt           s              7a
     D ptodt           s              7a
     D frdt            s                   like(cmdt)
     D todt            s                   like(cmdt)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*** end   add *** 07/11/05 ******************************************
     D outq            s             10a
     D quote           s              1a
     D rrn             s              9  0
     D rrn2            s              9  0
     D Title           S             40a   Inz('Print All Remarks Process')
     D qtype           s              7a   inz('*OUTQ')
     D rtnError        s              1a
     D eDebug          s              1a
     D Fids01        e ds                  extname(szq2p)
     D*** begin add *** 07/11/05 ******************************************
     D eMode           s              1a
     D*** end   add *** 07/11/05 ******************************************
     D debug           s              1a
     D call            s              1a
     D sort            s              2a

     D dbfds           ds
     D  dbrrn                397    400b 0

     D dbfds1          ds
     D  dbrrn1               397    400b 0

     D dbfds2          ds
     D  dbrrn2               397    400b 0

      *========================================================================
      * KList Definitions
      *========================================================================
     C     emkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scemp#

     C     fnkey         klist
     C                   kfld                    company
     C                   kfld                    group


     C     cmkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scfyr
     C                   kfld                    sccase

     C***  begin add  ***  11/27/02  ***************************************
     C     pskey         klist
     C                   kfld                    scpsid
     C***  end   add  ***  11/27/02  ***************************************

      *========================================================================
      * PList Definitions
      *========================================================================
      * entry parameters
     C     *entry        plist
     C                   parm                    eDebug
     C*** begin add *** 07/11/05 ******************************************
     C                   parm                    eMode
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
     C*** end   add *** 07/11/05 ******************************************

     C                   if        %PARMS > 0
     C                   eval      debug = eDebug
     C                   else
     C                   clear                   debug
     C                   endif


      * company/fund api
     C     COMPL         PLIST
     C                   PARM                    company
     C                   PARM                    group
     C                   PARM                    coName
     C                   PARM                    groupName

      * employer select
     C     emppl         plist
     C     *inkc         PARM      '0'           ended
     C     *inkl         PARM      '0'           cancelled
     C                   PARM      '2'           callMode
     C                   PARM      0             rrn
     C                   PARM                    rrn2
     C                   PARM      *blank        quote

      * group select
     C     grppl         plist
     C     *inkc         PARM      '0'           ended
     C     *inkl         PARM      '0'           cancelled
     C                   PARM      '2'           callMode
     C                   PARM                    pCompany
     C                   PARM                    pGroup


      * Claim    select
     C     clmpl         plist
     C                   PARM      '2'           call
     C     *inkc         PARM      '0'           ended
     C     *inkl         PARM      '0'           cancelled
     C                   PARM      ' '           callMode
     C                   PARM      *zeros        rrn
     C                   PARM      'CN'          sort
     C                   PARM                    rrn2
     C                   PARM      *zeros        year

     C***  begin add  ***  11/27/02  ***************************************
      * Printer id select
     C     pidpl         plist
     C                   PARM      '2'           callMode
     C     *inkc         PARM                    ended
     C     *inkl         PARM                    cancelled
     C                   PARM                    pPsid             2
     C***  end   add  ***  11/27/02  ***************************************

     **** need claim select **** !!!

      *========================================================================
      * Main
      *========================================================================
      * Get company
     C                   exsr      GETCOF

      * Center the program title.
     C                   Eval      SCPRDS = Center(Title)

     C                   CALLP     PgmStart

      ** Main program loop.
     C                   DoU       PgmStatus = EXIT
     C                             OR PgmStatus = CANCEL

      ** Process the screens recursively.
     C                   Eval      PgmStatus = ProcScreen( 1 : @@Mode )

     C                   If        PgmStatus <> EXIT
     C                             AND PgmStatus <> CANCEL

      **   Confirm the screen selections, allow user to cancel
     C                   eval      isDone = AllDone(SCGRP:SCEMP#:SCFYR:SCCASE)
     C                   If        isDone = YES
      **     We're done, so process the information.
     C                   Eval      PgmStatus = Finish( @@Mode )
     C                   EndIf                                                  isDone

     C                   EndIf                                                  PgmStatus<>EXIT

     C                   EndDo

     C                   CALLP     PgmEnd

     C                   Eval      *inlr = *on
     C                   Return

      *========================================================================*
      * Subroutine Definitions                                                 *
      *========================================================================*



      *------------------------------------------------------------------------*
      *  GETCOF - Gets company and fund names if possible; otherwise
      *           get PDS name for company name
      *------------------------------------------------------------------------*
     C     getcof        begsr

     C                   CALL      ComAPI        compl                  99
B01  C                   if        *in99 = *on
     C                   eval      SCCONM = Center(PDSNAME)
     C                   else
     C                   MoveL     coName        SCCONM
E01  C                   endif

     C                   endsr

      *========================================================================*
      * Procedure Definitions                                                  *
      *========================================================================*



      *------------------------------------------------------------------------*
      * CheckFkys - Checks for the press of a function key and branches program
      *------------------------------------------------------------------------*
     P CheckFkys       B

      ** Procedure Interface
     D CheckFkys       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C                   Select

     C                   When      Fkey = F3
     C                   Return    EXIT

     C                   When      Fkey = F12
     C                   Return    CANCEL

     C                   Other
     C                   Return    OtherFkys( Screen# : Mode : Fkey )

     C                   EndSL

     P CheckFkys       E


      *------------------------------------------------------------------------*
      * DspScreen - Explicitly execute format for each screen.
      *------------------------------------------------------------------------*
     P DspScreen       B

      ** Procedure Interface
     D DspScreen       PI
     D  Screen#                       2  0 Const

     C                   Select

     C                   When      Screen# = 1
     C                   EXFMT     SCR01

     C                   EndSL

     P DspScreen       E



      *------------------------------------------------------------------------*
      * ErrorChk- This is the general error check function.
      *------------------------------------------------------------------------*
     P ErrorChk        B

      ** Procedure Interface
     D ErrorChk        PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C                   Select

     C                   When      Screen# = 1
     C                   Return    ErrorChk1(Screen#:Mode)

     C                   EndSL

     P ErrorChk        E



      *------------------------------------------------------------------------*
      * ErrorChk1 - Check screen 1 for errors.
      *------------------------------------------------------------------------*
     P ErrorChk1       B

      ** Procedure Interface
     D ErrorChk1       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C     emkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scemp#

     C     fnkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp

     C     cmkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scfyr
     C                   kfld                    sccase

     C                   eval      *in90 = *off

      * employer and year/case are mutually exclusive
     C  n90scemp#        ifne      *zeros
     C     scfyr         ifne      *zeros
     C     sccase        orne      *zeros
     C                   seton                                        90  76
     C                   endif
     C                   endif                                                  scemp#<>*zeros

      * year and case are mutually inclusive
     C  n90scfyr         ifne      *zeros
     C     sccase        ifeq      *zeros
     C                   seton                                        90  78
     C                   endif
     C                   else
     C     sccase        ifne      *zeros
     C                   seton                                        90  77
     C                   endif
     C                   endif                                                  scfyr<>*zeros

      * group must be valid
     C     fnkey         chain     pfn01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90  71
     C                   endif

      * employer must be valid
     C     scemp#        ifne      *zeros
     C     emkey         chain     pem02                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90  72
     C                   endif
     C                   endif

      * year and case must be valid
     C     scfyr         ifne      *zeros
     C     cmkey         chain     pcm02                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90  75
     C                   endif
     C                   endif

     C*** begin add *** 07/11/05 ******************************************
      * from date must be valid if entered
     C                   if        (not *in90 and scfrdt <> 0) or
     C                             (not *in90 and sctodt <> 0)
     C                   z-add     scfrdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   seton                                        90  88
     C                   else
     C                   movel     srdto         scfrdt
     C                   z-add     scfrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         frdt
     C                   endif
     C                   endif

      * to date must be valid if entered
     C                   if        (not *in90 and scfrdt <> 0) or
     C                             (not *in90 and sctodt <> 0)
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   seton                                        90  89
     C                   else
     C                   movel     srdto         sctodt
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         todt
     C                   endif
     C                   endif

      * to date must be valid if entered
     C                   if        not *in90 and todt < frdt
     C                   seton                                        90  70
     C                   endif

      * output type must be valid
     C                   if        *in16
     C                   if        not *in90 and scoutt <> 'P' and
     C                             scoutt <> 'F' and scoutt <> 'B'
     C                   seton                                        90  87
     C                   endif
     C*** end   add *** 07/11/05 ******************************************

      * queue must not be blank and must be valid
     C  n90              if        scoutq = *blanks
     C                   seton                                        90  73
     C                   else
     C                   callp     CheckOutq(scoutq:qtype:rtnError)
     C                   if        rtnError = *on
     C                   seton                                        90  74
     C                   endif
     C                   endif

     C***  begin add  ***  11/27/02  ***************************************
      * Print Server ID must be valid if entered
     C     scpsid        ifne      *blank
     C     pskey         chain     pps01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90  79
     C                   endif
     C                   endif
     C***  end   add  ***  11/27/02  ***************************************
     C*** begin add *** 07/11/05 ******************************************
     C                   endif
     C*** end   add *** 07/11/05 ******************************************

     C  n90              return    0
     C   90              return    ERROR

     P ErrorChk1       E



      *------------------------------------------------------------------------*
      * Finish - The data's been entered/viewed, so do final processing.
      *------------------------------------------------------------------------*
     P Finish          B

      ** Procedure Interface
     D Finish          PI             2  0
     D  Mode                          1a   Const

     C                   move      scgrp         pGroup
     C                   move      scemp#        pEmployer
     C*** begin add *** 07/11/05 ******************************************
     c                   move      '0000000'     pfrdt
     c                   move      '0000000'     ptodt
     C                   eval      poutt = scoutt
     c                   if        scfrdt <> 0
     c                   z-add     scfrdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         pfrdt
     c                   endif
     c                   if        sctodt <> 0
     c                   z-add     sctodt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         ptodt
     c                   endif
     C*** end   add *** 07/11/05 ******************************************
     C                   moveL     scoutq        outq
     C                   move      company       pCompany
     C                   move      scfyr         pyear
     c                   move      sccase        pcase
     C***  begin add  ***  11/27/02  ***************************************
     C                   move      scpsid        psid              2
     C***  end   add  ***  11/27/02  ***************************************
     C*** begin add *** 07/11/05 ******************************************
     C                   if        not *in16
     C                   eval      poutt = 'F'
     C                   eval      outq = 'QPRINT'
     C                   eval      psid = '  '
     C                   endif
     C*** end   add *** 07/11/05 ******************************************

     C***  begin chg  ***  11/27/02  ***************************************
     C****************** CALLP     submitter(pCompany:pGroup:pEmployer:
     C******************           pYear:pCase:outq:debug)
     C*** begin chg *** 07/11/05 ******************************************
     C**********         CALLP     submitter(pCompany:pGroup:pEmployer:
     C**********                   pYear:pCase:outq:debug:psid)
     C                   CALLP     submitter(pCompany:pGroup:pEmployer:
     C                             pYear:pCase:outq:debug:psid:poutt:
     C                             pfrdt:ptodt)
     C*** end   chg *** 07/11/05 ******************************************
     C***  end   chg  ***  11/27/02  ***************************************
     C                   Return    EXIT

     P Finish          E



      *------------------------------------------------------------------------*
      * GetDate - Gets the current date and time.  Used for maint fields.
      *------------------------------------------------------------------------*
     P GetDate         B

      ** Procedure Interface
     D GetDate         PI

     C                   time                    dsDateTime
     C                   Eval      svTime = dsTime
     C                   Eval      dateOut = CvtDate(dsDate:BLANK:DT_CYMD)
     C                   MoveL     dateOut       svDate

     C                   Return

     P GetDate         E



      *------------------------------------------------------------------------*
      * OtherFkys - Handles processing of Function keys other than F3=Exit and
      *             F12=Cancel.  The function key pressed will be passed in,
      *             along with the screen# and mode.
      *------------------------------------------------------------------------*
     P OtherFkys       B

      ** Procedure Interface
     D OtherFkys       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const
     D  KeyPressed                    1a   Const

     C                   Select

     C                   When      Fkey = F10
     C                   Return    F10pressed

     C                   Other
     C                   Return    0

     C                   EndSL

     P OtherFkys       E


      *------------------------------------------------------------------------*
      * F10pressed - Calls the search programs based on the field that had
      *              focus when F10 was pressed.
      *------------------------------------------------------------------------*
     P F10pressed      B

      ** Procedure Interface
     D F10pressed      PI             2  0

     C     fnkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp

     C                   Select

     C                   When      scfld = 'SCGRP'
     C                   move      company       pCompany
     C                   eval      pGroup = *blanks

     C                   call      GRPSEARCH     grppl
     C                   if        *inkc = *on
     C                   return    EXIT
     C                   endif
     C                   if        *inkl = *on
     C                   seton                                        81
     C                   return    ERROR
     C                   endif
     C                   if        pGroup <> *blanks
     C                   move      pGroup        scgrp
     C     fnkey         chain     pfn01                              60
     C                   if        *in60 = *off
     C                   eval      scgrp = fnfnd
     C                   eval      scemp# = *zeros
     C                   seton                                        82
     C                   endif
     C                   endif
     C                   Return    ERROR

     C                   When      scfld = 'SCEMP#'
     C                   if        scgrp <> *zeros
     C     fnkey         chain     pfn01                              60
     C                   if        *in60 = *off
     C                   eval      rrn2 = dbrrn1
     C                   else
     C                   seton                                        90  71
     c*** begin add *** 07/11/05 *******************************************
     C                   return    ERROR
     c*** end   add *** 07/11/05 *******************************************
     C                   eval      rrn2 = *zeros
     C                   endif
     C                   else
     C                   eval      rrn2 = *zeros
     C                   endif

     C                   if        *in90 <> *on
     C                   call      EMPSEARCH     emppl
     C*** begin chg *** 07/11/05 *******************************************
     C**********         if        *inkc = *on
     C**********         return    EXIT
     C**********         endif
     C**********         if        *inkl = *on
     C**********         seton                                        82
     C**********         return    ERROR
     C**********         endif
     C**********         if        rrn <> *zeros
     C*****rrn**         chain     pem01                              60
     C**********         if        *in60 = *off
     C**********         eval      scgrp = emfnd
     C**********         eval      scemp# = ememp#
     C**********         seton                                        83
     C**********         endif
     C**********         endif
     C**********         endif
     C**********         eval      *in90 = *off
     C**********         Return    ERROR
     C                   if        rrn = *zeros
     C                   seton                                        8290
     C                   return    ERROR
     C                   else
     C     rrn           chain     pem01
     C                   eval      scgrp = emfnd
     C                   eval      scemp# = ememp#
     C                   seton                                        8390
     C                   return    ERROR
     C                   endif
     C                   endif
     C*** end   chg *** 07/11/05 *******************************************

     ***** claims search added *****************************
     C                   When      (scfld = 'SCCASE' or scfld = 'SCFYR')
     C                   if        scgrp <> *zeros
     C     fnkey         chain     pfn01                              60
     C                   if        *in60 = *off
     C                   eval      rrn2 = dbrrn1
     C                   else
     C                   seton                                        90  71
     c*** begin add *** 07/11/05 *******************************************
     C                   return    ERROR
     c*** end   add *** 07/11/05 *******************************************
     C                   eval      rrn2 = *zeros
     C                   endif
     C                   else
     C                   eval      rrn2 = *zeros
     C                   endif

     C                   if        *in90 <> *on
     C                   call      CLMSEARCH     clmpl
     C*** begin chg *** 07/11/05 *******************************************
     C**********         if        *inkc = *on
     C**********         return    EXIT
     C**********         endif
     C**********         if        *inkl = *on
     C**********         seton                                        85
     C**********         return    ERROR
     C**********         endif
     C**********         if        rrn <> *zeros
     C*****rrn**         chain     pcm01                              60
     C**********         if        *in60 = *off
     C**********         eval      scgrp = cmfnd
     C**********         eval      scfyr  = cmfyr
     C**********         eval      sccase = cmcase
     C**********         seton                                        83
     C**********         endif
     C**********         endif
     C**********         endif
     C**********         eval      *in90 = *off
     C**********         Return    ERROR
     C                   if        rrn = 0
     C                   seton                                        8590
     C                   return    ERROR
     c                   else
     C     rrn           chain     pcm01                              60
     C                   eval      scgrp = cmfnd
     C                   eval      scfyr  = cmfyr
     C                   eval      sccase = cmcase
     C                   seton                                        8390
     C                   return    ERROR
     c                   endif
     c                   endif
     C*** end   chg *** 07/11/05 *******************************************
     ********** end claim search added *************
     C***  begin add  ***  11/27/02  ***************************************
     C*** begin chg *** 07/11/05 *******************************************
     C**********         when      scfld = 'SCPSID'
     C                   when      scfld = 'SCPSID' and *in16
     C*** end   chg *** 07/11/05 *******************************************

     C                   call      PIDSEARCH     pidpl
     C                   if        *inkc = *on
     C                   return    EXIT
     C                   endif
     C                   if        *inkl = *on
     C                   seton                                        86
     C                   return    ERROR
     C                   endif
     C                   if        pPsid <> *blanks
     C                   move      pPsid         scpsid
     C                   endif
     C                   Return    ERROR
     C***  end   add  ***  11/27/02  ***************************************
     C                   Other
     C                   Return    ERROR

     C                   EndSL

     P F10pressed      E

      *------------------------------------------------------------------------*
      * PgmEnd - Any processing at the very end of the program.
      *------------------------------------------------------------------------*
     P PgmEnd          B

      ** Procedure Interface
     D PgmEnd          PI


     P PgmEnd          E



      *------------------------------------------------------------------------*
      * PgmStart - Processing before entering the screen display cycles.
      *------------------------------------------------------------------------*
     P PgmStart        B

      ** Procedure Interface
     D PgmStart        PI

     Dsuffix           s              3    inz(*blanks)
     Dfolder_prefix    s             80    inz(*blanks)

     C                   eval      @@mode='P'
     C                   eval      scgrp = *zeros
     C                   eval      scemp# = *zeros
     C*** begin add *** 07/11/05 ******************************************
     C                   eval      scoutt = *blanks
     C*** end   add *** 07/11/05 ******************************************
     C                   eval      scoutq = *blanks
     C*** begin add *** 07/11/05 ******************************************
     C                   seton                                        16
     C                   if        q1prms > 1 and eMode = 'D'
     C                   setoff                                       16
     C                   endif
     C*** end   add *** 07/11/05 ******************************************

     P PgmStart        E



      *------------------------------------------------------------------------*
      * ProcScreen - This handles all the processing of any number of screens
      *              through recursion.
      *------------------------------------------------------------------------*
     P ProcScreen      B

      ** Procedure Interface
     D ProcScreen      PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C                   DoU       PgmStatus <> ERROR

      ** Display the screens.
     C                   CALLP     DspScreen(Screen#)
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)

      ** Check for the press of function keys.
     C                   Eval      PgmStatus = CheckFkys( Screen# : Mode )

     C                   If        PgmStatus <> EXIT
     C                             AND PgmStatus <> CANCEL
     C                             AND PgmStatus <> ERROR
     C                   Eval      PgmStatus = ErrorChk( Screen# : Mode )

     C                   EndIf

     C                   EndDo

     C                   Return    PgmStatus

     P ProcScreen      E



      *------------------------------------------------------------------------*
      * AllDone - This handles the processing of the screen that allows the
      *           user to confirm/cancel their choices.
      *------------------------------------------------------------------------*
     P AllDone         B

      ** Procedure Interface
     D AllDone         PI             1a
     D  Grp                           3  0 Const
     D  Emp                           9  0 Const
     D  Yr                            3  0 Const
     D  Case                          7  0 Const

     D  ScreenStatus   S              2  0

     C                   eval       SXGRP1 = Grp
     C                   eval       SXGRP2 = Grp
     C                   eval       SXEMP# = Emp
     C                   eval       SXFYR = Yr
     C                   eval       SXCASE = Case

     C                   DoU       ScreenStatus <> ERROR
     C                   eval      ScreenStatus = 0

     C                   If        Emp <> 0
     C                   seton                                          72
     C                   setoff                                       71  73
     C                   else
     C                   If        Yr <> 0
     C                   seton                                            73
     C                   setoff                                       7172
     C                   else
     C                   seton                                        71
     C                   setoff                                         7273
     C                   EndIf
     C                   EndIf

     C                   EXFMT     SCCONF

     C                   If        Fkey = F3 or Fkey = F10
     C                   eval      ScreenStatus = ERROR
     C                   EndIf

     C                   EndDo

     C                   If        Fkey = F12
     C                   Return    NO
     C                   else
     C                   Return    YES
     C                   EndIf

     C                   Return    NO

     P AllDone         E

      * utility procedure definitions
      /COPY SC980RPI
