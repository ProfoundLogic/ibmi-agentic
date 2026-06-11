     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*====================================================================
     H/TITLE F7020R - Prompt for ICD-9-CM Diagnosis Code File Listing
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H*====================================================================
     Fs7020f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
     Fsmupl     if   e           k disk    rename(pup01:sup00)
    F*
     D/copy sCopyPR01d
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('ICD-9 Diag Code List')
     D syComApi        c                   const('WA001R')
     D mcComApi        c                   const('FA007R')
     D @@call          s              1
     D @@prds          s             40
     D @@Area          s              2
     D xxcall          s                   like(@@call)
     D ##Co#           s              3  0
     D ##FNam          s             40
     D ##Fnd           s              3  0
     D ##Sub#          s              5  0
     D ##Fyr           s              3  0
     D ##Call          s              1
     D ##KC            s              1
     D ##KL            s              1
     D ##ICD9          s              4
     D ##Lost          s              4
     D ##Area          s              2
     D ##Lvl           s              3  0
     D ##Type          s              1
     D ##OutQ          s             10
     D ##From          s                   like(scFrom)
     D ##To            s                   like(scTo)
     D ##Mode          s              1
     D xxArea          s              2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/copy sPrDate
     C/copy sPrString
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@Area
    C*
    C*  Company file/fund (group) retrieve program parameter list
     C     syComPl       plist
     C                   parm                    ##Co#
     C                   parm                    ##Fnd
     C                   parm                    ##CoNm
     C                   parm                    ##FNam
     C                   parm                    ##Lvl
     C                   parm                    ##Type
     C                   parm                    ##OutQ
     C*
     C     mcComPl       plist
     C                   parm                    ##Co#
     C                   parm                    ##Fnd
     C                   parm                    ##CoNm
     C                   parm                    ##FNam
     C                   parm                    ##Sub#
     C                   parm                    ##Fyr
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
     C                   select
     C                   when      xxArea = 'MC'
     C                   call      mcComApi      mcComPl                91
     C     q1user        chain     sup00
     C                   eval      ##OutQ = UPOUTQ
     C                   other
     C                   call      syComApi      syComPl                91
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C*
     C*  From value must be less than or equal to To value
     C                   if        scTo < scFrom
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
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
     C*__________
     C*
     C                   when      hfld = 'SCFROM' or hfld = 'SCTO'
     C*
     C                   call      'S2010R'
     C                   parm      '2'           ##Call            1
     C     *inkc         parm                    ##KC              1
     C     *inkl         parm                    ##KL              1
     C                   parm      *blanks       ##ICD9            4
     C                   parm                    ##Lost            4
     C                   parm      xxArea        ##Area            2
     C*
     C                   if        *inkc = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        *inkl = *on or ##ICD9 = *blanks
     C                   select
     C                   when      hfld = 'SCFROM'
     C                   seton                                        7181
     C                   when      hfld = 'SCTO'
     C                   seton                                        7281
     C                   endsl                                                  IFEQ SCFROM
     C                   leavesr
     C                   endif
     C*
     C                   select
     C                   when      hfld = 'SCFROM'
     C                   eval      scFrom = ##ICD9
     C                   seton                                        7281
     C                   when      hfld = 'SCFROM'
     C                   eval      scTo = ##ICD9
     C                   seton                                        7381
     C                   endsl                                                  HFLD SCFROM
     C*__________
     C*
     C                   endsl                                                  HFLD SCFROM
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KMDO   - Processing for F13
    C*====================================================================
    C*
     C     KMDO          begsr
     C*
     C                   select
     C                   when      xxArea = 'MC'
     C                   call      'F0006R'
     C                   other
     C                   call      'W0006R'
     C                   endsl
     C*
     C                   exsr      GetCoF
     C*
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
     C                   eval      scPrDs = optNm
     C                   if        %parms >= 1 and @@prds <> *blanks
     C                   eval      scPrDs = @@PrDs
     C                   endif
     C                   eval      scPrDs = CenterFld(scPrDs)
    C*
     C                   clear                   xxCall
     C                   if        %parms >= 2
     C                   eval      xxCall = @@Call
     C                   endif
     C*
     C                   eval      xxArea = 'SY'
     C                   if        %parms >= 3 and @@Area <> *blanks
     C                   eval      xxArea = @@Area
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
     C     PRSCTL        begsr
     C*
     C                   move      ##Co#         #@Co#             3
     C*
     C                   call      'S7020C'
     C                   parm                    #@Co#
     C                   parm      scFrom        ##From
     C                   parm      scTo          ##To
     C                   parm                    ##OutQ
     C                   parm      'D'           ##Mode
     C*
     C                   endsr
    C*
