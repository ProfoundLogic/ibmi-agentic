    Hcopyright('(C) Copyright Fiserv, Inc., 2005')
    H*********************************************************************
    H/title Written Vs Collected Premium Report (Prompt)
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 06/16/05      C:136     PROJECT: 114WJB                 *
    H*********************************************************************
    H*  Revisions                                                        *
    H*                                                                   *
    H*  08/16/10 C:136  Project: 114WVX PRISS                            *
    H*                  Chg name of report per client request            *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw7760f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     D/COPY SCOPYPR01D
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
      *** beg chg ***** 08/16/10 *******************************************
     D*optnm*****      c                   const('Written vs Collected Premium +
     D***********                                 Report')
     D optnm           c                   const('Month End A/R Report')
      *** end chg ***** 08/16/10 *******************************************
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D xxJan1          s              4
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     d*entry
     d W7760R          pr
     d  @@prds                       40
     d  @@call                        1
     d W7760R          pi
     d  @@prds                       40
     d  @@call                        1

     d submitter       pr                  extpgm('W7760C')
     d  @@co#                         3    const
     d  @@FrGrp#                      3    const
     d  @@ToGrp#                      3    const
     d  @@AsoD                        8    const
     d  @@Output                      1    const
     d  @@OutQ                       10    const

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrUsrPrms

    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C/COPY SCOPYPR01C
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C     DSCAPI        begsr
    C*
     C                   clear                   ##co#
     C                   eval      ##co#  = UserCo#(q1user)
     C                   eval      ##conm = CenterFld(GetCompNam(##co#):
     C                                                %len(scprds))
     C                   eval      *in91 = ##co# = *zero
     C
    C*
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     ERRCK1        begsr
     C*
     C  n90              if        scFrGrp# > scToGrp#
     C                   seton                                        71  90
     C                   endif
     C*
     C                   eval      scAsod = dtCheck(scAsod)
     C  n90              if        scAsod = *zero
     C                   seton                                        72  90
     C                   endif
     C*
     C  n90              if        cvtToDate(scAsod) <>
     C                             getLastDay(CvtToDate(scAsod))
     C                   seton                                        73  90
     C                   endif
     C*
     C  n90              if        scOutput <> 'P' and scOutput <> 'F'
     C                              and scOutput <> 'B'
     C                   seton                                        74  90
     C                   endif
     C*
     C                   endsr
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     OTHFKS        begsr
     C                   endsr
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     PGMEND        begsr
     C                   endsr
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     PGMSTR        begsr
    C*
     C                   movel(p)  optnm         ##data
     C                   if        %parms >= 1 and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   endif

     C                   eval      scprds = CenterFld(##data:%len(scprds))
    
     C                   clear                   xxcall
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   endif
    
     C                   eval      scFrGrp# = userGrp(Q1USER)
     C                   eval      scToGrp# = userGrp(Q1USER)
    
     C                   endsr
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
     C     PREDSP        begsr
     C                   endsr
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
     C     PRSCTL        begsr
     C*
     C                   if        scToGrp# = *zero
     C                   eval      scToGrp# = *hival
     C                   endif
     C*
     C                   callp     submitter(
     C                                       %editc(##co#:'Z'):
     C                                       %editc(scFrGrp#:'Z'):
     C                                       %editc(scToGrp#:'Z'):
     C                                       %editc(scASOD:'Z'):
     C                                       scOutput:
     c                                       GetUsrOutq(q1user)
     C                                      )
     C*
     C                   endsr
