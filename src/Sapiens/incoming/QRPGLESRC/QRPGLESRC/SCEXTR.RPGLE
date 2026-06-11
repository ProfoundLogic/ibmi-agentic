     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*********************************************************************
    H/TITLE DARTS Extract - Prompt
    H*********************************************************************
    H*  DESCRIPTION: DARTS Extract - Prompt                              *
    H*                                                                   *
    H*  CALLED FROM: DARTSEXT(CMD)                                       *
    H*                                                                   *
    H*  CALLS TO:    SCEXTC                                              *
    H*                                                                   *
    H*  CREATED: 10/17/11      C:190     PROJECT: 139WQI                 *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
      *  12/23/19 C:180 Project: 133IRJ                                  *
      *                 Remove key Code screen field and any logic for   *
      *                 it.                                              *
     H*  11/26/15 C:306 Support: 38620                                    *
    H*                 To fix the DARTS Extract - Prompt                 *
    H*                                                                   *
     H*  06/30/15 C:062 Project: 48WYI
    H*                 Correct parm usage
    H*                                                                   *
     H*  07/27/12 C:280 Project: 114IHH
    H*                 Added parm for sis
    H*                                                                   *
    H*********************************************************************
     Fscextf    cf   e             workstn
      *** Beg Add *** 11/26/15 *********************************************
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *** end Add *** 11/26/15 *********************************************
    F*
     D/COPY SCOPYPR01D
     D/copy sPrDate
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('DARTS Extract')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##FrDt          s              8
     D ##ToDt          s              8
     D ##RunType       s              1
     D ##Lib           s             10
     D ##CrtLib        s              1
     D @@kc            s              1
     D @@kl            s              1
     D ##KeyCode       s             12
     D*
     D                 DS
     D  XXPRDS                 1     40
     D  SISCLM                 1      1
     D  COMPANY                2      3
     D  PRDS                   4     40
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     d inKeyCode       s             12a
    d*
     d/COPY sprsecure
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C*******            parm                    @@prds
     C*******            parm                    @@call
     C                   parm                    @SISCLM           1
     C                   parm                    @COMPANY          2
     C*** Begin Del *** 06/30/15 **************************************
     C********           parm                    @PROGM           10
     C*** End   Del *** 06/30/15 **************************************

    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm      *inkc         @@inkc            1
     C                   parm      *inkl         @@inkl            1
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
      *** Begin Delete *** 12/23/19 ***************************************
     c*  Validate Key Code:
     c*                  if        sccode = ' '
     c*                  seton                                        3590
     c*                  leavesr
     c*                  endif

     c*                  eval      inKeyCode = sccode
     c*                  if        Valid_KeyCode(inKeyCode) = *zero
     c*                  seton                                        3490
     c*                  leavesr
     c*                  endif
      *** End   Delete *** 12/23/19 ***************************************

     C*  From date must be valid
     c                   if        DtCheck(scFrDt) = *zeros
     c                              and scFrDt <> *zeros
     c                   seton                                        2890
     c                   leavesr
     c                   endif
     c                   eval      scFrDt = DtCheck(scFrDt)

     C*  To date must be valid
     c                   if        DtCheck(sctoDt) = *zeros
     c                              and scToDt <> *zeros
     C                   seton                                        2990
     c                   leavesr
     c                   endif
     c                   eval      scToDt = DtCheck(scToDt)

     C*  Dates must be ascending order
      *
     c                   if        CvtToDate(scToDt) <= CvtToDate(scFrDt)
     c                              and scToDt <> *zeros
     c                   seton                                        32  90
     c                   leavesr
     c                   endif                                                    dat2 <> *zeros

     C*  Validate run type
     c                   if        scRnty <> 'P'
     c                              and scRnty <> 'C'
     c                              and scRnty <> 'B'
     c                   seton                                        3090
     c                   leavesr
     c                   endif

     c* Validate destination library
     c                   if        scDlib = *blanks
     c                   seton                                        3190
     c                   leavesr
     c                   endif

     c                   call      'SRCHKOBJ'
     c                   parm      scdlib        ##Obj            10
     c                   parm      '*LIB'        ##Type            7
     c                   parm                    ##Err             1

     c                   dou       *in90 = *off
     c                   If        ##Err = *on
     c                   clear                   ScnLib
     c                   exfmt     wdw01
     c                   eval      *in90 = *off
     c                   eval      *in66 = *off
     c                   if        scNlib <> 'Y' and scNlib <> 'N'
     c                   seton                                        66  90
     c                   endif                                                  ##Err = *on

     c                   endif
     c                   enddo

     c                   if        scNlib = 'N'
     c                   seton                                        67  90
     c                   exfmt     wdw01
     c                   endif

     c                   endsr                                                  errck1
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     c                   eval      *inlr = *on
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   move      @@PRDS        XXPRDS
    C*
     C                   if        %parms >= 1
     C                             and @@prds <> *blanks
     C*******            movel(p)  @@prds        ##data
     C                   movel(p)  PRDS          ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     prsctl        begsr

     c                   if        scNlib <> 'N'
     c                   movel     scFrdt        ##Frdt
     c                   movel     scTodt        ##Todt
     c                   movel     scRnty        ##RunType
     c                   movel     scDlib        ##Lib
     c                   movel     ScnLib        ##CrtLib
     C*** Beg Del *** 12/23/19 *********************************************
     c*                  movel     Sccode        ##KeYCode
     C*** End Del *** 12/23/19 *********************************************
    C*
     C*** Beg Add *** 06/15/12 *********************************************
    C*
     C*** Begin Chg *** 06/30/15 **************************************
     c**********         if        SISCLM =  'N'
     c                   if        @SISCLM =  'N'
     C*** End   Chg *** 06/30/15 **************************************
    C*
     C*** End Add *** 06/15/12 *********************************************
    C*
     c                   call      'SCEXTC'
     c                   parm                    ##FrDt
     c                   parm                    ##ToDt
     c                   parm                    ##RunType
     c                   parm                    ##Lib
     c                   parm                    ##CrtLib
     C*** Beg Del *** 12/23/19 *********************************************
     c**                 parm                    ##KeYCode
     C*** End Del *** 12/23/19 *********************************************
    C*
     C*** Beg Add *** 06/15/12 *********************************************
    C*
    C                   ELSE
    C*
     c                   call      'SCEXTC4'
     c                   parm                    ##FrDt
     c                   parm                    ##ToDt
     c                   parm                    ##RunType
     c                   parm                    ##Lib
     c                   parm                    ##CrtLib
     C*** Beg Del *** 12/23/19 *********************************************
     c**                 parm                    ##KeYCode
     C*** End Del *** 12/23/19 *********************************************
     c                   parm                    @COMPANY
    C*
     c                   endif
    C*
     C*** End Add *** 06/15/12 *********************************************
    C*

     c                   endif                                                  scNlib <> 'N'

     C                   endsr                                                  prsctl
    c*
