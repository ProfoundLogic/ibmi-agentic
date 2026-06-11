     *===================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 1999              *
     *===================================================================*
    H/TITLE SR00111I - ADDITIONAL ENTRY PROGRAM FOR WC000111A
     *===================================================================*
     * Description: Collects additional information for form WC000111.   *
     *                                                                   *
     * Called from: SR00111                                              *
     * Calls to   : None                                                 *
     *                                                                   *
     * Created    : 11/10/99                                             *
     * Programmer : C:113                                                *
     * Project    : CM027                                                *
     *===================================================================*
     * REVSIONS:                                                         *
     *                                                                   *
     *                                                                   *
     * ----------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.               *
     * ----------------------------------------------------------------- *
     *===================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR00111F  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      ** Additional Information File
     FWXFMP1    uf a e           k DISK
      ** Group Master File
     FWMFNL     if   e           k DISK
      ** Employer Master File
     FWMEML     if   e           k DISK
     *
     *================================================================
     * /COPY 'D' specs
     *================================================================
     *
     D/COPY SCOPYPR01D
     *
     *================================================================
     * Stand Alone Fields - TOP
     *================================================================
     D optnm           c                   const('Additional information for WC+
     D                                     000111A')
     D comapi          c                   const('WA001R')
      *
     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3
      *
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
      *
     D xxForm1         s             15
     D xxcall          s                   like(@@call)
     D xxForm          s             10    inz('WC000111  ')
     D xxco#           s                   like(EMCO#)
     D xxfnd           s                   like(EMFND)
     D xxemp#          s                   like(EMEMP#)
     D xxdiv           s                   like(EMDIV)
     D xxfyr           s              3  0
     D xxedn           s             80
      *
     D                 DS
     D scDesc                       600a
     D  SCC601                       60a   Overlay(scDesc)
     D  SCC602                       60a   Overlay(scDesc:61)
     D  SCC603                       60a   Overlay(scDesc:121)
     D  SCC604                       60a   Overlay(scDesc:181)
     D  SCC605                       60a   Overlay(scDesc:241)
     D  SCC606                       60a   Overlay(scDesc:301)
     D  SCC607                       60a   Overlay(scDesc:361)
     D  SCC608                       60a   Overlay(scDesc:421)
     D  SCC609                       60a   Overlay(scDesc:481)
     D  SCC610                       60a   Overlay(scDesc:541)
      *
     D dbDesc                       600a
     D  F3C301                       30a   Overlay(dbDesc)
     D  F3C302                       30a   Overlay(dbDesc:31)
     D  F3C303                       30a   Overlay(dbDesc:61)
     D  F3C304                       30a   Overlay(dbDesc:91)
     D  F3C305                       30a   Overlay(dbDesc:121)
     D  F3C306                       30a   Overlay(dbDesc:151)
     D  F3C307                       30a   Overlay(dbDesc:181)
     D  F3C308                       30a   Overlay(dbDesc:211)
     D  F3C309                       30a   Overlay(dbDesc:241)
     D  F3C310                       30a   Overlay(dbDesc:271)
     D  F3C311                       30a   Overlay(dbDesc:301)
     D  F3C312                       30a   Overlay(dbDesc:331)
     D  F3C313                       30a   Overlay(dbDesc:361)
     D  F3C314                       30a   Overlay(dbDesc:391)
     D  F3C315                       30a   Overlay(dbDesc:421)
     D  F3C316                       30a   Overlay(dbDesc:451)
     D  F3C401                       40a   Overlay(dbDesc:481)
     D  F3C201                       20a   Overlay(dbDesc:521)
     D  F3C402                       40a   Overlay(dbDesc:541)
     D  F3C202                       20a   Overlay(dbDesc:581)
    C*
     C/COPY SCOPYDTPL
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
      ** Entry parameters
     C     *entry        PList
     C                   Parm                    @@prds
     C                   Parm                    @@call
     C                   Parm                    @@co#
     C                   Parm                    @@fnd
     C                   Parm                    @@emp#
     C                   Parm                    @@div
     C                   Parm                    @@fyr
     *
     ** Company file/fund (group) retrieve program parameter list
     C     compl         PList
     C                   Parm                    ##co#
     C                   Parm                    ##fnd
     C                   Parm                    ##conm
     C                   Parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *
      ** Employer Master file
     C     emKey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
      *
      ** Additional Information file
     C     fmKey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxfyr
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
     C                   KFld                    xxForm1
     *
      ** Group Master file
     C     fnKey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     *================================================================
     * Main
     *================================================================
     *
     C/COPY SCOPYPR01C
     *
     *====================================================================
     * DSCAPI - Call API for getting descriptions (use 91 on error)
     *====================================================================
     C     DSCAPI        BegSR
     *
     C                   Call      comapi        compl                  91
     *
     C                   EndSR
     *====================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *====================================================================
     C     ERRCK1        BegSR
     *
     C                   ExSR      OTHFKS
     *
     C                   EndSR
     *====================================================================
     * KJDO   - Processing for F10
     *====================================================================
     C     KJDO          BegSR
     C                   EndSR
     *====================================================================
     * OTHFKS - Processing for other functions keys
     *====================================================================
     C     OTHFKS        BegSR
     *
     C     *inkj         CasEQ     *on           KJDO                     90
     C                   EndCS
     *
     C                   EndSR
     *====================================================================
     * PGMEND - End of program processing
     *====================================================================
     C     PGMEND        BegSR
      *
     C                   If        *inkc = *on OR
     C                             *inkl = *on
     C                   Eval      @@call = 'F'
     C                   EndIf
      *
     C                   EndSR
     *====================================================================
     * PGMSTR - Used for any start of program processing (one-time)
     *====================================================================
     C     PGMSTR        BegSR
     *
     C                   If        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   MoveL(p)  @@prds        ##data
     C                   Else
     C                   MoveL(p)  optnm         ##data
     C                   EndIf
     *
     C                   Call      'SRCENTER'
     C                   Parm                    ##data
     C                   Parm      40            ##leng
     *
     C                   MoveL     ##data        SCPRDS
     *
     C                   If        q1prms >= 2
     C                   MoveL     @@call        xxcall
     C                   Else
     C                   Clear                   xxcall
     C                   EndIf
     *
     C                   Eval      xxfyr = 0
     C                   Move      @@co#         xxco#
     C                   Move      @@fnd         xxfnd
     C                   Move      @@emp#        xxemp#
     C                   Move      @@div         xxdiv
     C                   Move      @@fyr         xxfyr
      *
     C                   Eval      SCFND = xxfnd
     C                   Eval      SCFYR = xxfyr
     C                   Eval      xxForm1 = xxForm
      *
     C     fnKey         Chain     pfn01                              60
     C                   Eval      SCFNNM = FNNAME
      *
     C     emKey         Chain     pem01                              60
      *
     C                   Call      'WA005R'
     C                   Parm                    EMEMP#
     C                   Parm                    EMDIV
     C                   Parm                    EMNAME
     C                   Parm                    xxedn
      *
     C                   Eval      SCEDN = xxedn
      *
     C                   EndSR
     *====================================================================
     * PREDSP - Pre-processing before displaying (every time)
     *====================================================================
     C     PREDSP        BegSR
      *
      ** Get endorsement additional information
     C     fmKey         Chain     pfm01                              60
      *
     C                   If        *in60 = *off
      *
      ** Move the custom fields to the Screen
     C                   Eval      scDesc = dbDesc
      *
     C                   Else
      *
     C                   Eval      scDesc = *blanks
      *
     C                   EndIf
      *
     C                   EndSR
     *====================================================================
     * PRSCTL - Process control after no errors found (Call Submitter)
     *====================================================================
     C     PRSCTL        BegSR
      *
     C     fmKey         Chain     pfm01                              60
      *
     C                   If        *in60 = *on
     C                   Clear                   pfm01
     C                   EndIf
      *
      ** Move the additional information fields to the file
     C                   Eval      F3CO# = xxco#
     C                   Eval      F3FND = xxfnd
     C                   Eval      F3FYR = xxfyr
     C                   Eval      F3EMP# = xxemp#
     C                   Eval      F3DIV = xxdiv
     C                   Eval      F3FORM = xxForm
     C                   Eval      dbDesc = scDesc
     C*
     C   60              Write     pfm01
     C  N60              Update    pfm01
      *
     C                   EndSR
