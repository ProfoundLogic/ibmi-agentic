    Hcopyright('(C) Copyright Fiserv, Inc., 2005')
    H*********************************************************************
    H/title S/R Premium Report (Prompt)
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 06/08/05      C:33      PROJECT: 135UDN                 *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw8410f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     D/COPY SCOPYPR01D
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('S/R Premium')
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     d*entry
     d W8410R          pr
     d  @@prds                       40
     d  @@call                        1
     d W8410R          pi
     d  @@prds                       40
     d  @@call                        1

     d submitter       pr                  extpgm('W8410C')
     d  @@co#                         3    const
     d  @@FrRgn#                      3    const
     d  @@ToRgn#                      3    const
     d  @@FrFyr                       3    const
     d  @@ToFyr                       3    const
     d  @@AsOfDt                      8    const
     d  @@FrDt                        8    const
     d  @@ToDt                        8    const
     d  @@DtType                      1

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrUsrPrms

    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
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
     C                   clear                   ##co#
     C                   eval      ##co#  = UserCo#(q1user)
     C                   eval      ##conm = CenterFld(GetCompNam(##co#):
     C                                                %len(scprds))
     C                   eval      *in91 = ##co# = *zero
     C
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C*
     C  n90              if        scFrRgn# > scToRgn#
     C                   seton                                        31  90
     C                   seton                                        7172
     C                   endif
     C*
     C  n90              if        (scFrFyr <> *zero or scToFyr <> *zero)
     C                              and
     C                             (scFrDt <> *zero or scToDt <> *zero)
     C                   seton                                        32  90
     C                   seton                                        7374
     C                   seton                                        7677
     C                   endif
     C*
     C  n90              if        (scFrFyr = *zero and scToFyr = *zero)
     C                              and
     C                             (scFrDt = *zero and scToDt = *zero)
     C                   seton                                        33  90
     C                   seton                                        7374
     C                   seton                                        7677
     C                   endif
     C*
     C  n90              if        scFrFyr > scToFyr
     C                   seton                                        31  90
     C                   seton                                        7374
     C                   endif
     C*
     C  n90              if        scToFyr <> *zero and scAsOfDt <> *zero
     C                   eval      scAsOfDt = DtCheck(scAsOfDt)
     C  n90              if        scAsOfDt = *zero
     C                   seton                                        34  90
     C                   seton                                        75
     C                   endif
     C                   endif
     C*
     C  n90              if        scFrDt <> *zero or scToDt <> *zero
     C*
     C                   eval      scFrDt = DtCheck(scFrDt)
     C  n90              if        scFrDt = *zero
     C                   seton                                        34  90
     C                   seton                                        76
     C                   endif
     C*
     C                   endif
     C*
     C  n90              if        scToDt <> *zero
     C*
     C                   eval      scToDt = DtCheck(scToDt)
     C  n90              if        scToDt = *zero
     C                   seton                                        34  90
     C                   seton                                        77
     C                   endif
     C*
     C  n90              if        CvtToDate(scFrDt) > CvtToDate(scToDt)
     C                   seton                                        31  90
     C                   seton                                        7677
     C                   endif
     C*
     C  n90              if        scDtType <> 'R' and scDtType <> 'P'
     C                   seton                                        35  90
     C                   seton                                        78
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
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
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   movel(p)  optnm         ##data
     C                   if        %parms >= 1 and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   endif
     C*
     C                   eval      scprds = CenterFld(##data:%len(scprds))
    C*
     C                   clear                   xxcall
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
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
     C                   if        scToRgn# = *zero
     C                   eval      scToRgn# = *hival
     C                   endif
     C*
     C                   if        scAsOfDt = *zero
     C                   eval      scAsOfDt = *hival
     C                   endif
     C*
     C                   callp     submitter(
     C                                       %editc(##co#:'Z'):
     C                                       %editc(scFrRgn#:'Z'):
     C                                       %editc(scToRgn#:'Z'):
     C                                       %editc(scFrFyr:'Z'):
     C                                       %editc(scToFyr:'Z'):
     C                                       %editc(scAsOfDt:'Z'):
     C                                       %editc(scFrDt:'Z'):
     C                                       %editc(scToDt:'Z'):
     C                                       scDtType
     C                                      )
     C*
     C                   endsr
    C*
