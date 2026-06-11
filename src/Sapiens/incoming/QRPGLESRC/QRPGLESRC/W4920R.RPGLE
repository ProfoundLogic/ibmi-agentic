    Hcopyright('(C) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/title California Quarterly CIRB Report (Prompt)
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*********************************************************************
    H*  CALLED FROM:  Menu                                               *
    H*                                                                   *
    H*  CALLS TO:  W4920C                                                *
    H*                                                                   *
    H*  CREATED: 07/10/06      C:136     PROJECT: 132WGK/136WCH          *
    H*********************************************************************
    H*  Revisions                                                        *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw4920f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     fsmcol     if   e           k disk
    F*
     D/COPY SCOPYPR01D
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('California Quarterly CIRB +
     D                                            Report')
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D scconm          s                   like(##co#)
     D scprds          s             40
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     d*entry
     d W4920R          pr
     d  @@prds                       40
     d  @@call                        1
     d W4920R          pi
     d  @@prds                       40
     d  @@call                        1

     d submitter       pr                  extpgm('W4920C')
     d  @@co#                         3    const
     d  @@rpdt                        8    const

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrUsrPrms
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
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     ERRCK1        begsr

     C                   eval      scrpdt = dtCheck(scrpdt)
     C  n90              if        scrpdt = *zero
     C                   seton                                        71  90
     C                   endif

     C  n90              if        cvtToDate(scrpdt) <>
     C                             getLastDay(CvtToDate(scrpdt))
     C                   seton                                        72  90
     C                   endif

     C                   if        *in90 = *off
     C     ##co#         chain     pco01
     C                   if        COCART = *blanks
     C                   seton                                        73  90
     C                   endif
     C                   endif

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

     C                   callp     submitter(
     C                                       %editc(##co#:'Z'):
     C                                       %editc(scrpdt:'Z')
     C                                      )

     C                   endsr
