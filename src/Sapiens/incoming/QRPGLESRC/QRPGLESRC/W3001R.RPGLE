    H*  (C) Copyright Progressive Data Solutions, Inc., 2003
    H*********************************************************************
    H/TITLE - Internal Account Rep Commission Report Prompt
    H*********************************************************************
    H*  CALLED FROM:                                                     *
    H*    Menu                                                           *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    W3001C                                                         *
    H*                                                                   *
    H*  CREATED: 08/28/06      C:180     PROJECT: 114WMF                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw3001f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     D/COPY SCOPYPR01D
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Calculate Internal Acct Comm-
     D                                      Rates')
     D comapi          c                   const('WA001R')
     D @@prds          s             40
     D @@call          s              1
     D ##co#           s              3  0
     D ##fnd           s              3  0
     D ##fnam          s             40
     D ##level         s              3  0
     D ##type          s              1
     D #@co#           s              3
     D ##frdt          s              8
     D ##todt          s              8
     D ##medt          s              8
     D ##outq          s             10
     D xxtodt          s               d   datfmt(*usa)
     D xxfrdt          s               d   datfmt(*usa)
     D xxmedt          s               d   datfmt(*usa)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##level
     C                   parm                    ##type
     C                   parm                    ##outq
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
     C     DSCAPI        begsr
     C                   call      comapi        compl                  91
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     c     ERRCK1        begsr
    c*
     c                   if        scfrdt > 1000000
     c     *usa          test(d)                 scfrdt                 99
     c  n99*usa          move      scfrdt        xxfrdt
     C                   else
     C     *jobrun       test(d)                 scfrdt                 99
     c  n99*jobrun       move      scfrdt        xxfrdt
     c                   endif
      *
     c  n99*usa          move      xxfrdt        scfrdt
     c  n99              move      scfrdt        ##frdt
     c   99              seton                                        71  90
      *
     c                   if        not *in90
     c                   if        sctodt > 1000000
     c     *usa          test(d)                 sctodt                 99
     c  n99*usa          move      sctodt        xxtodt
     C                   else
     C     *jobrun       test(d)                 sctodt                 99
     c  n99*jobrun       move      sctodt        xxtodt
     c                   endif
      *
     c  n99*usa          move      xxtodt        sctodt
     c  n99              move      sctodt        ##todt
     c   99              seton                                        72  90
     c                   endif
      *
     c                   if        not *in90
     c                   if        scmedt > 1000000
     c     *usa          test(d)                 scmedt                 99
     c  n99*usa          move      scmedt        xxmedt
     C                   else
     C     *jobrun       test(d)                 scmedt                 99
     c  n99*jobrun       move      scmedt        xxmedt
     c                   endif
      *
     c  n99*usa          move      xxmedt        scmedt
     c  n99              move      scmedt        ##medt
     c   99              seton                                        73  90
     c                   endif
    c*
     c                   if        not *in90
     c                   if        xxtodt < xxfrdt
     c                   seton                                        74  90
     c                   endif
     c                   endif
    c*
     c                   endsr
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
     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
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
    C*
     C                   move      ##co#         #@co#
     C*
     C                   call      'W3001C'
     C                   parm                    #@co#
     C                   parm                    ##frdt
     C                   parm                    ##todt
     C                   parm                    ##medt
     C                   parm                    ##outq
    C
     C                   endsr
