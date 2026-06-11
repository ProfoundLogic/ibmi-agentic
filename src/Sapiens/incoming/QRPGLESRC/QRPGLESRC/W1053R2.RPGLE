     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
      *========================================================================*
     H/TITLE W1053R2 - Endorsement Versioning Program                          *
      *========================================================================*
      * DESCRIPTION:                                                           *
      *  This program will replace an existing endorsement in one or all       *
      *  groups with a new version.                                            *
      *                                                                        *
      * CREATED   : 03/21/00                                                   *
      * PROGRAMMER: C:113                                                      *
      * PROJECT   : CM027                                                      *
      *========================================================================*
      * Revised:
      *
      * 11/10/16 C:136 Project: SY371
      *                Use extended description field
      *
      * 09/23/05  C:61  Support WA093 48974
      *                 Initialize counters prior to LOOKUP to prevent
      *                 "Out of Range" error.                                  *
      *                                                                        *
      * ---------------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
      * ---------------------------------------------------------------------- *
      *========================================================================*
      * FILE USAGE:                                                            *
      *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
      *  WTENL   Endorsement Table File         PEN01   CO#/FND/FORM           *
      *  WTENL3  Endorsement Table File         PEN03   CO#/FORM               *
      *  WTENP   Endorsement Table File         PF_PEN01                       *
      *  WMFNL   Group Master File              PFN01   CO#/FND                *
      *                                                                        *
      *========================================================================*
      * INDICATOR USAGE:                                                       *
      *  Global Indicators                                                     *
      *   60 - General chain.                                                  *
      *   90 - General error.                                                  *
      *                                                                        *
      *  Error Indicators                                                      *
      *                                                                        *
      *                                                                        *
      *========================================================================*
      * NOTES:                                                                 *
      *                                                                        *
      *                                                                        *
      *========================================================================*

      *========================================================================*
      * File Definitions                                                       *
      *========================================================================*
      * Must use INFDS(WSDS) on workstation for FKeys to work.
     FW1053F2   CF   E             WORKSTN INFDS(WSDS)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')

     FWTENL     UF A E           K DISK    RENAME(PEN01:DBRCDF1)
     FWTENL3    UF A E           K DISK    RENAME(PEN01:DBRCDF3)
     FWTENP     UF A E             DISK    RENAME(PEN01:DBRCDF)
     FWMFNL     IF   E           K DISK

      * Program-specific includes
      /COPY W1053R2INC

      *========================================================================*
      * Variable Definitions                                                   *
      *========================================================================*
     D MAX_SCREEN      C                   CONST(01)
     D COMAPI          C                   Const('WA001R')

     D Title           S             40a   Inz('Update Endorsement Edition')
     D xGroup          S              3  0
     D company         S              3  0
     D coName          S             40a
     D group           S              3  0
     D groupName       S             40a

     D a2z             s              1    dim(26) ctdata perrcd(26)

     D tempEnd1        ds            15
     D tempEnd                        1    dim(15)
      *========================================================================*
      * KList Definitions                                                      *
      *========================================================================*
     C     FNkey         KList
     C                   KFld                    company
     C                   KFld                    xGroup

     C     FNkey1        KList
     C                   KFld                    company

      *========================================================================*
      * PList Definitions                                                      *
      *========================================================================*
     C     *entry        PList
     C                   Parm                    @@Mode
     C                   Parm                    @@RRN
     C                   Parm                    @@Return

      * company/fund api
     C     COMPL         PLIST
     C                   PARM                    company
     C                   PARM                    group
     C                   PARM                    coName
     C                   PARM                    groupName
      *========================================================================*
      * Main                                                                   *
      *========================================================================*
      * Get company
     C                   exsr      GETCOF

      * Center the program title.
     C                   Eval      SCPRDS = Center(Title)

     C                   CALLP     PgmStart

      ** Main program loop.
     C                   DoU       PgmStatus = EXIT
     C                             OR PgmStatus = CANCEL

     C                   CALLP     Refresh(1:@@Mode:'1')

      ** Process the screens recursively.
     C                   Eval      PgmStatus = ProcScreen( 1 : @@Mode )

     C                   If        PgmStatus <> EXIT
     C                             AND PgmStatus <> CANCEL

      ** We're done, so process the information.
     C                   Eval      PgmStatus = Finish( @@Mode )
     C                   EndIf

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
      * Deleter - Pre-processing before delete occurs
      *------------------------------------------------------------------------*
     P Deleter         B

      ** Procedure Interface
     D Deleter         PI


     P Deleter         E



      *------------------------------------------------------------------------*
      * Displayer - Processing after displaying information
      *------------------------------------------------------------------------*
     P Displayer       B

      ** Procedure Interface
     D Displayer       PI


     P Displayer       E



      *------------------------------------------------------------------------*
      * DspScreen - Explicitly execure format for each screen.
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

     C                   eval      *in90 = *off

     C                   If        SCNEWGROUP <> *zeros

     C                   Eval      xGroup = SCNEWGROUP
     C     FNkey         Chain     PFN01                              60
     C                   If        *in60 = *on
     C                   SetOn                                        71  90
     C                   EndIf

     C                   EndIf

     C  n90              If        SCNEWFORM = *blanks
     C                   SetOn                                        72  90
     C                   EndIf

     C                   If        *in90 = *on
     C                   Return    ERROR
     C                   Else
     C                   Return    0
     C                   EndIf

     P ErrorChk1       E



      *------------------------------------------------------------------------*
      * Finish - The data's been entered/viewed, so do final processing.
      *------------------------------------------------------------------------*
     P Finish          B

      ** Procedure Interface
     D Finish          PI             2  0
     D  Mode                          1a   Const

     C                   Select

     C                   When      Mode = ADD_MODE
     C                   CALLP     Writer
     C                   Return    ERROR

     C                   When      Mode = CHG_MODE
     C                   CALLP     Updater
     C                   Return    CANCEL

     C                   When      Mode = DSP_MODE
     C                   CALLP     Displayer

     C                   When      Mode = CPY_MODE
     C                   CALLP     Writer

     C                   Other
     C                   Return    OtherModes(Mode)

     C                   EndSl

     C                   Return    0

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
      * MoveDB2SC - Move the database values into the screen for display/edit.
      *------------------------------------------------------------------------*
     P MoveDB2SC       B

      ** Procedure Interface
     D MoveDB2SC       PI

     D x               S              2  0
     D z               S              2  0
     D c               S              1a
     D c1              S              1a

     C                   Eval      SCNEWFORM = SCEND
      * increment the suffix of the endorsement
     C                   MoveL     SCEND         tempEnd1
      ******* beg add ****** 09/23/05 *********************************
     C                   Eval      x = 1
      ******* end add ****** 09/23/05 *********************************
     C     ' '           Lookup    tempEnd(x)                             99
     C                   if        *in99
     C                   Eval      x = x-1
     C                   Else
     C                   Eval      x = 14
     C                   EndIf

      ******* beg add ****** 09/23/05 *********************************
     C                   Eval      z = 1
      ******* end add ****** 09/23/05 *********************************
     C     tempEnd(x)    Lookup    a2z(z)                                 99
     C                   if        NOT *in99
     C                   Eval      tempEnd(x+1) = 'A'
     C                   Else
     C                   Eval      tempEnd(x) = a2z(z+1)
     C                   EndIf

     C                   MoveL     tempEnd1      SCNEWFORM

     P MoveDB2SC       E



      *------------------------------------------------------------------------*
      * MoveSC2DB - Move the screen values to the database for update/write.
      *------------------------------------------------------------------------*
     P MoveSC2DB       B

      ** Procedure Interface
     D MoveSC2DB       PI

     C                   Eval      ENFORM = SCNEWFORM
     C                   Eval      ENPGM = q1pgm
     C                   Eval      ENUSER = q1user
     C                   Eval      ENDT = svdate
     C                   Eval      ENTIME = svtime

     P MoveSC2DB       E



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

     C                   Other
     C                   Return    0

     C                   EndSL

     P OtherFkys       E



      *------------------------------------------------------------------------*
      * OtherModes - Handles processing of modes other than the standard ones.
      *------------------------------------------------------------------------*
     P OtherModes      B

      ** Procedure Interface
     D OtherModes      PI             2  0
     D  Mode                          1a   Const

     C                   Select

     C                   Other
     C                   Return    0

     C                   EndSL

     P OtherModes      E



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

     C     @@RRN         chain     WTENP                              60
     C                   eval      SCEND = ENFORM
     C                   eval      SCGROUP = ENFND
      *** beg chg ***** 11/10/16 ***************************************
     C***********        eval      SCENDDESC = ENDESC
      /Free
       If ENdsc2 <> *blanks;
          scEndDesc = ENdsc2;
       Else;
          scEndDesc = ENdesc;
       Endif;
      /End-free
      *** end chg ***** 11/10/16 ***************************************

     C                   eval      xGroup = ENFND
     C     FNkey         chain     PFN01                              60
     C                   eval      SCGRPNAME = FNNAME

     P PgmStart        E



      *------------------------------------------------------------------------*
      * ProcEndors - This handles all the processing new endorsement versions.
      *------------------------------------------------------------------------*
     P ProcEndors      B

      ** Procedure Interface
     D ProcEndors      PI
     D  Group                         3  0 Const

     C     ENkey         KList
     C                   KFld                    company
     C                   KFld                    Group
     C                   KFld                    SCEND

     C                   If        Group = *zeros
      * process all groups
     C     FNkey1        Chain     PFN01                              60
     C                   DoW       *in60 = *off
     C                   CALLP     ProcEndors(FNFND)
     C     FNkey1        ReadE     PFN01                                  60
     C                   EndDo

     C                   Else
      * process group passed in
     C     ENkey         Chain     DBRCDF1                            60
     C                   If        *in60 = *off
     C                   CALLP     MoveSC2DB
     C                   UPDATE    DBRCDF1
     C                   EndIf

     C                   endif

     P ProcEndors      E



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
     C                   CALLP     Refresh( Screen# : Mode : '0' )

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

     C                   If        Mode <> DSP_MODE
     C                   Eval      PgmStatus = ErrorChk( Screen# : Mode )
     C                   EndIf

     C                   If        PgmStatus <> ERROR

     C                   If        Screen# < MAX_SCREEN

      ** Process next screen, if any.
     C                   Eval      PgmStatus = ProcScreen( Screen#+1 : Mode)
     C                   EndIf

     C                   EndIf

     C                   EndIf

      ** This is to back the program up one screen.
     C                   If        PgmStatus = CANCEL
     C                             AND Screen# <> 1
     C                   Return    ERROR
     C                   EndIf

     C                   EndDo

     C                   Return    PgmStatus

     P ProcScreen      E



      *------------------------------------------------------------------------*
      * PreAdd - Preprocessing before adding a record.
      *------------------------------------------------------------------------*
     P PreAdd          B

      ** Procedure Interface
     D PreAdd          PI
     D  Mode                          1a   Const

     C                   Select

     C                   When      Mode = ADD_MODE
     C                   Clear                   DBRCDF

     C                   When      Mode = CPY_MODE
     C     @@RRN         Chain(n)  DBRCDF                             60

     C                   EndSL

     C                   MoveL(p)  MODEA         SCMODE
     C                   CALLP     MoveDB2SC

     C                   Return

     P PreAdd          E



      *------------------------------------------------------------------------*
      * PreChange - Preprocessing before a change.
      *------------------------------------------------------------------------*
     P PreChange       B

      ** Procedure Interface
     D PreChange       PI

     C     @@RRN         Chain(n)  DBRCDF                             60
     C                   MoveL(p)  MODEC         SCMODE
     C                   CALLP     MoveDB2SC

     C                   Return

     P PreChange       E



      *------------------------------------------------------------------------*
      * PreDelete - Preprocessing before a delete.
      *------------------------------------------------------------------------*
     P PreDelete       B

      ** Procedure Interface
     D PreDelete       PI

     C     @@RRN         Chain(n)  DBRCDF                             60
     C                   CALLP     Deleter
     C     @@RRN         Delete    DBRCDF                             99
     C                   CALLP     PgmEnd

     C                   Return

     P PreDelete       E



      *------------------------------------------------------------------------*
      * PreDisplay - Preprocessing before displaying.
      *------------------------------------------------------------------------*
     P PreDisplay      B

      ** Procedure Interface
     D PreDisplay      PI

     C     @@RRN         Chain(n)  DBRCDF                             60
     C                   MoveL(p)  MODED         SCMODE
     C                   CALLP     MoveDB2SC

     C                   Return

     P PreDisplay      E



      *------------------------------------------------------------------------*
      * Refresh - Refresh the screen information.
      *------------------------------------------------------------------------*
     P Refresh         B

      ** Procedure Interface
     D Refresh         PI
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const
     D  Flag                          1a   Const

     C                   Select

     C                   When         Mode = ADD_MODE
     C                   If           Flag = *on
     C                   CALLP        PreAdd(Mode)
     C                   EndIf

     C                   When         Mode = CHG_MODE
     C                   CALLP        PreChange

     C                   When         Mode = DSP_MODE
     C                   CALLP        PreDisplay

     C                   When         Mode = DLT_MODE
     C                   If           Flag = *on
     C                   CALLP        PreDelete
     C                   EndIf

     C                   When         Mode = CPY_MODE
     C                   If           Flag = *on
     C                   CALLP        PreAdd(Mode)
     C                   EndIf

     C                   Other
     C*                  CALLP        PreOther

     C                   EndSL

     C                   CALLP        GetDate

     C                   Return

     P Refresh         E



      *------------------------------------------------------------------------*
      * Updater - Updates the database record after processing.
      *------------------------------------------------------------------------*
     P Updater         B

      ** Procedure Interface
     D Updater         PI

     C                   CALLP     ProcEndors(SCNEWGROUP)

     P Updater         E



      *------------------------------------------------------------------------*
      * Writer - Writes the database record after processing.
      *------------------------------------------------------------------------*
     P Writer          B

      ** Procedure Interface
     D Writer          PI

     C                   Clear                   DBRCDF
     C                   CALLP     MoveSC2DB

     P Writer          E



      * utility procedure definitions
      /COPY W1053R2UTL

** CTDTA a2z
ABCDEFGHIJKLMNOPQRSTUVWXYZ
