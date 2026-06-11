    Hcopyright('(C) Copyright Fiserv, Inc., 2005')
    H*********************************************************************
    H/TITLE W7270R2 - Prompt for Loss Ratio by Job Class W/Workfile
    H*********************************************************************
    H* Description:  Workfile for the Loss Ratio by Job Class Report     *
    H*                using PPC Premium.                                 *
    H*                                                                   *
    H* Created: 11/14/05    Programmer: C:33    Project: 135WFH          *
    H*********************************************************************
    H* Reviions:                                                         *
    H*                                                                   *
    H*********************************************************************
     Fw7270f2   cf   e             workstn infds(fids01)
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
    F*
     D*entry
     D W7270R2         PR                  extpgm('W7270R2')
     D  @@prds                       40
     D  @@call                        1
     D W7270R2         PI
     D  @@prds                       40
     D  @@call                        1

     D submitter       PR                  extpgm('W7270C2')
     D  ##Co#                         3    const
     D  ##Fnd                         3    const
     D  ##Fyr                         3    const
     D  ##Prem                        1    const
     D  ##AsOf                        8    const
     D  ##TypO                        1    const
     D  ##Outq                       10    const

     D suffixPgm       PR                  extpgm('SA030R')
     D  suffix                        3
     D
     D/copy sCopyPR01d
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrUsrPrms
     D/copy wPrGroup
    D*
     DGrpYrSrch        pr             3  0
     D inkc                           1
     D inkl                           1
    D*
     DValidGrpYr       pr             1n
     D group                               like(F1FND) const
     D groupYr                             like(F1FYR) const
    D*
     D fnfds           ds
     D fnrrn                 397    400b 0
    D*
     D optSel          c                   const(', if selected.')
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Loss Ratio by Job Class -
     D                                     using PPC Prem')
     D suffix          s              3
     D sysDt           s               d
     D xxCo#           s              3  0
     D xxCall          s                   like(@@Call)
     D ##Fnd           s              3  0
     D ##kl            s               n
     D ##rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/copy sCopyPR01c
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   eval      xxCo#  = UserCo#(*blanks)
    C*
     C                   eval      xxCo#  = UserCo#(q1user:scCoNm)
     C                   eval      scCoNm = CenterFld(scCoNm)
     C                   eval      scFnd  = UserGrp(q1user)
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
    C*
    C*  Validate the group
    C*
     C  n90              if        scFnd <> 0
     C                              and ValidGrp(scFnd) = *off
     C                   seton                                        71  90
     C                   endif
    C*
    C*  Validate the policy year for the group
    C*
     C  n90              if        scFnd <> 0 and scFyr <> 0
     C                              and not ValidGrpYr(scFnd:scFyr)
     C                   seton                                        72  90
     C                   endif
    C*
    C*  Validate premium type
    C*
     C  n90              if        scPrem <> 'M'
     C                             and scPrem <> 'W'
     C                   seton                                        73  90
     C                   endif
    C*
    C*  Validate the entered run date
    C*
     C                   eval      scAsOf = DtCheck(scAsOf)
     C  n90              if        scAsOf = *zero
     C                   seton                                        74  90
     C                   endif
    C*
    C*  Validate output type
    C*
     C  n90              if        sctypo <> 'P'
     C                             and sctypo <> 'F'
     C                             and sctypo <> 'B'
     C                   seton                                        75  90
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
    C*
    C*  Group search
    C*
     C                   if        hfld = 'SCFND'
     C                              or (hfld = 'SCFYR' and scFnd = *zero)
     C*
     C                   eval      ##rrn = GrpSrch(*inkc:##kl:##Fnd)
    C*
     C                   if        ##rrn = *zeros
     C                   seton                                        71  89
     C                   else
     C                   eval      scFnd = ##Fnd
     C                   seton                                        72  89
     C                   endif
    C*
     C                   endif
    C*
    C*  Group year search
    C*
     C                   if        hfld = 'SCFYR'
     C*
     C                   setoff                                       7172
     C*
     C                   select
     C                   when      scFnd <> *zero
     C                   eval      scFyr = GrpYrSrch(*inkc:##kl)
     C                   seton                                        73  89
     C                   when      scFnd =  *zero and ##kl
     C                   seton                                        72  89
     C                   when      scFnd =  *zero
     C                   seton                                        71  90
     C                   setoff                                         89
     C                   endsl
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KMDO   - Processing for F13
    C*====================================================================
     C     kmdo          begsr
    C*
     C                   call      'W0006R'
     C*
     C                   exsr      DscApi
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
     C                   select
     C                   when      %parms >= 1 and @@prds <> *blanks
     C                   eval      scPrds = CenterFld(@@prds:%len(scPrds))
     C                   other
     C                   eval      scPrds = CenterFld(optNm:%len(scPrds))
     C                   endsl
    C*
     C                   select
     C                   when      %parms >= 2
     C                   eval      xxCall = @@call
     C                   other
     C                   clear                   xxCall
     C                   endsl
    C*
     C                   eval      sysDt  = SystemDate
     C                   eval      scAsOf = DateTo8(sysDt)
    C*
    C* Display location of output file
    C*
     C                   callp     suffixPgm(suffix)
    C*
     C                   eval      scRptL = 'PDSRPT' + %trim(suffix)
     C                              + optSel
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
     C     PRSCTL        begsr
    C*
     C                   callp     submitter(%editc(xxCo#:'X'):
     C                                       %editc(scFnd:'X'):
     C                                       %editc(scFyr:'X'):
     C                                       scPrem:
     C                                       %editc(scAsOf:'X'):
     C                                       scTypO:
     C                                       GetUsrOutq(q1user)
     C                                       )
    C*
     C                   endsr



    P*====================================================================
    P* GrpYrSrch - Search group years
    P*
    P*  GrpYrSrch - returns group year
    P*   inkc   - returns *on if F3 was pressed from search  (required)
    P*   inkl   - returns *on if F12 was pressed from search (required)
    P*====================================================================
    P*
     PGrpYrSrch        b
    P*
     DGrpYrSrch        PI             3  0
     D inkc                           1
     D inkl                           1

     DW0011R           PR                  extpgm('W0011R')
     D ##kc                           1
     D ##kl                           1
     D ##call                         1    const
     D ##rrn                          9  0 const
     D ##yr                           3  0 const

     D ##Yr            s              3  0

    C*
     C                   callp     W0011R(*inkc:##kl:'2':
     C                              GetGrpRRN(xxCo#:scFnd):##Yr)
    C*
     C                   if        ##Yr = *zero
     C                   eval      ##Yr = scFyr
     C                   seton                                        72  89
     C                   else
     C                   seton                                        73  89
     C                   endif
     C*
     C                   return    ##Yr
     C*
     PGrpYrSrch        e



    P*====================================================================
    P* ValidGrpYr - Valid group year
    P*
    P*  ValidGrpYr - return *on if group year is valid
    P*   group    - passed in for validation  (required)
    P*   group yr - passed in for validation  (required)
    P*====================================================================
    P*
     PValidGrpYr       b                   export
    P*
     DValidGrpYr       PI             1n
     D group                               like(F1FND) const
     D groupYr                             like(F1FYR) const

     C     keyWDF1P      klist
     C                   kfld                    xxCo#
     C                   kfld                    group
     C                   kfld                    groupYr
     C*
     C     keyWDF1P      chain     pf100
     C*
     C                   return    %found

     PValidGrpYr       e
