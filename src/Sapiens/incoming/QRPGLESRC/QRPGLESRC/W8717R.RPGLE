     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
    H/title Mass Transfer - Quote to Live
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 10/10/03      C:33      PROJECT: WC1017                 *
     *********************************************************************
     * REVSIONS:
      *
      * 12/09/19  C:298 Project: WC2691
      *                 Pass parameter for State Reporting Division only.
      *
      * 09/13/10  C:61  Project: SY190 PRISS 157
      *                 Pass User OUTQ for routing of exception report.
      *
     *********************************************************************
     H Bnddir('WTBNDDIR')
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     FW8717F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

      ******* beg del ****** 09/13/10 *********************************
     Fwmupl     if   e           k disk    rename(pup01:pup00)
      ******* end del ****** 09/13/10 *********************************
     Fsmupp     if   e             disk    rename(pup01:pup0p)
     Fwmaap     if   e             disk    rename(paa01:paa0p)
    
     Fststl3    if   e           k disk    rename(pst01:pst03)
     Fwmupl1    if   e           k disk
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
    
     D*entry
     D W8717R          pr
     D  @@prds                       40
     D  @@call                        1
     D W8717R          pi
     D  @@prds                       40
     D  @@call                        1

     D ChgUserParms    pr                  extpgm('W0006R')

     D Co#FndApi       pr                  extpgm('WA001R')
     D  ##co#                         3  0
     D  ##fnd                         3  0
     D  ##conm                       40
     D  ##fnam                       40

     D SubmitPgm       pr                  extpgm('W8717C')
     D  ##co#                         3    const
     D  ##Fnd                         3    const
     D  ##NRB                         1    const
     D  ##FrDt                        8    const
     D  ##ToDt                        8    const
     D  ##State                       2    const
     D  ##UnWtr                       3    const
     D  ##Agt#                        5    const
     D  ##FrPrem                     11    const
     D  ##ToPrem                     11    const
      ******* beg add ****** 09/13/10 *********************************
     D  ##outq                       10    const
      ******* end add ****** 09/13/10 *********************************
      ***** Beg Add ***** 12/09/19 ************************************
     D  ##strptdiv                    1
      ***** End Add ***** 12/09/19 ************************************

     D ChkAgent#       pr                  like(*in90)
     D ChkDates        pr                  like(*in90)
     D ChkNRB          pr                  like(*in90)
     D ChkPremiums     pr                  like(*in90)
     D ChkState        pr                  like(*in90)
     D ChkUnderWtr     pr                  like(*in90)

     D SearchAgt#      pr                  like(scAgt#)
     D  agent#                             like(scAgt#)

     D SearchStaff     pr                  like(scUnWtr)
     D  staffCode                          like(scUnWtr)

      ******* beg add ****** 09/13/10 *********************************
     D ##OUTQ          s                   like(UPOUTQ)
      ******* end add ****** 09/13/10 *********************************

     D/copy sPrDate
     D/copy sPrString
     D/copy wPrAgent

     D/copy sCopyPR01d

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optName         c                   const('Mass Quote Transfer')
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      ******* beg add ****** 09/13/10 *********************************
     C*
     C     keyWMUPL      klist
     C                   kfld                    q1user
      ******* end add ****** 09/13/10 *********************************
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   callp     Co#FndApi(##co#:scFnd:##conm:scFndNm)
     C                   eval      *in91 = *off
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C*
     C  n90              eval      *in90 = ChkNRB
     C  n90              eval      *in90 = ChkDates
     C  n90              eval      *in90 = ChkState
     C  n90              eval      *in90 = ChkUnderWtr
     C  n90              eval      *in90 = ChkAgent#
     C  n90              eval      *in90 = ChkPremiums
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C*
     C                   select
     C                   when      hfld = 'SCAGT#'
     C                   eval      scAgt# = SearchAgt#(scAgt#)
     C                   when      hfld = 'SCUNWTR'
     C                   eval      scUnWtr = SearchStaff(scUnWtr)
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KMDO   - Processing for F13
    C*====================================================================
    C*
     C     KMDO          begsr
    C*
     C                   callp     ChgUserParms
     C                   exsr      getCoF
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   if        %parms >= 1 and @@prds <> *blanks
     C                   eval      scPrds = CenterFld(@@prds:%size(scPrds))
     C                   else
     C                   eval      scPrds = CenterFld(optName:%size(scPrds))
     C                   endif
    C*
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    C*
      ***** Beg Add ***** 12/09/19 ****************************************
     C                   eval      scStRptDiv = 'Y'
      ***** End Add ***** 12/09/19 ****************************************
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
     C     PRSCTL        begsr
      ******* beg add ****** 09/13/10 *********************************

     C     keyWMUPL      chain     wmupl

     C                   if        %found(wmupl)
     C                   eval      ##outq = UPOUTQ
     C                   else
     C                   eval      ##outq = *blanks
     C                   endif
      ******* end add ****** 09/13/10 *********************************
     C*
      ******* beg chg ****** 09/13/10 *********************************
      *******
     C*******            callp     SubmitPgm(%editc(##co#:'Z'):
     C*******                                %editc(scFnd:'Z'):
     C*******                                scNRB:
     C*******                                %editc(scFrDt:'Z'):
     C*******                                %editc(scToDt:'Z'):
     C*******                                scState:
     C*******                                scUnWtr:
     C*******                                %editc(scAgt#:'Z'):
     C*******                                %editc(scFrPrem:'Z'):
     C*******                                %editc(scToPrem:'Z')
     C*******                               )
     C                   callp     SubmitPgm(%editc(##co#:'Z'):
     C                                       %editc(scFnd:'Z'):
     C                                       scNRB:
     C                                       %editc(scFrDt:'Z'):
     C                                       %editc(scToDt:'Z'):
     C                                       scState:
     C                                       scUnWtr:
     C                                       %editc(scAgt#:'Z'):
     C                                       %editc(scFrPrem:'Z'):
     C                                       %editc(scToPrem:'Z'):
     c                                       ##outq
     C***** Beg Add ***** 12/09/19 ************************************
     C                                       :scStRptDiv
     C***** End Add ***** 12/09/19 ************************************
     C                                      )
      ******* end chg ****** 09/13/10 *********************************
     C*
     C                   endsr
    C*____________________________________________________________________
    C*





    C*====================================================================
    C* ChkAgent# - Check the scAgt# field
    C*====================================================================
     PChkAgent#        b

     D ChkAgent#       pi                  like(*in90)

     C                   select
     C                   when      scAgt# <> *zero
     C                             and not(ValidAgt#(scAgt#:scFnd))
     C                   eval      *in77 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkAgent#        e





    C*====================================================================
    C* ChkDates - Check the scFrDt & scToDt fields
    C*====================================================================
     PChkDates         b

     D ChkDates        pi                  like(*in90)

     C                   select
     C                   when      scFrDt <> *zero
     C                             and DtCheck(scFrDt) = *zero
     C                   eval      *in72 = *on
     C                   return    *on
     C                   when      scToDt <> *zero
     C                             and DtCheck(scToDt) = *zero
     C                   eval      *in73 = *on
     C                   return    *on
     C                   when      CvtToDate(scToDt) < CvtToDate(scFrDt)
     C                   eval      *in74 = *on
     C                   return    *on
     C                   endsl

     C                   eval      scFrDt = DtCheck(scFrDt)
     C                   eval      scToDt = DtCheck(scToDt)

     C                   return    *off

     PChkDates         e





    C*====================================================================
    C* ChkNRB - Check the scNRB field
    C*====================================================================
     PChkNRB           b

     D ChkNRB          pi                  like(*in90)

     C     keyWMFNL      klist
     C                   kfld                    ##co#
     C                   kfld                    scFnd

     C     keyWMFNL      chain     wmfnl

     C                   select
     C                   when          scNRB <> 'N'
     C                             and scNRB <> 'R'
     C                             and scNRB <> 'B'
     C                   eval      *in71 = *on
     C                   return    *on
     C                   when      FNAUTE <> 'Y' and scNRB <> 'R'
     C                   eval      *in79 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkNRB           e





    C*====================================================================
    C* ChkPremiums - Check the scFrPrem & scToPrem fields
    C*====================================================================
     PChkPremiums      b

     D ChkPremiums     pi                  like(*in90)

     C                   select
     C                   when      scToPrem < scFrPrem
     C                   eval      *in78 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkPremiums      e





    C*====================================================================
    C* ChkState - Check the scState field
    C*====================================================================
     PChkState         b

     D ChkState        pi                  like(*in90)

     D stateKey        s                   like(STKEY)

     C     keySTSTL3     klist
     C                   kfld                    stateKey

     C                   eval      stateKey = scState
     C     keySTSTL3     chain     pst03

     C                   select
     C                   when      scState <> *blanks
     C                             and not %found
     C                   eval      *in75 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkState         e





    C*====================================================================
    C* ChkUnderWtr - Check the scUnWtr field
    C*====================================================================
     PChkUnderWtr      b

     D ChkUnderWtr     pi                  like(*in90)

     C     scUnWtr       chain     pup01

     C                   select
     C                   when      scUnWtr <> *blank
     C                             and not %found
     C                   eval      *in76 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkUnderWtr      e





    C*====================================================================
    C* SearchAgt# - Process the agent # search
    C*====================================================================
     PSearchAgt#       b

     D SearchAgt#      pi                  like(scAgt#)
     D  agent#                             like(scAgt#)

     D AArrn           s              9  0
     D error           s              1
     D inKL            s                   like(*inKL)

     C                   eval      AArrn = AgtSrchAA(*inkc:inkl:
     C                                               error:scFnd)

     C                   if        AArrn <> *zero
     C     AArrn         chain     WMAAP
     C                   eval      *in88 = *on
     C                   return    AAAGT#
     C                   endif

     C                   eval      *in87 = *on
     C                   return    agent#

     PSearchAgt#       e





    C*====================================================================
    C* SearchStaff - Process the staff code search
    C*====================================================================
     PSearchStaff      b

     D SearchStaff     pi                  like(scUnWtr)
     D  staffCode                          like(scUnWtr)

     D UserParmSrch    pr                  extpgm('S0005R')
     D  ##call                        1    const
     D  ##rrn                         9  0

     D UPrrn           s              9  0

     C                   callp     UserParmSrch('2':UPrrn)

     C                   if        UPrrn <> *zero
     C     UPrrn         chain     SMUPP
     C                   eval      *in87 = *on
     C                   return    UPSTAF
     C                   endif

     C                   eval      *in86 = *on
     C                   return    staffCode

     PSearchStaff      e
