     Hcopyright('(c) xxxx StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: XX/XX/XX      C:XX      PROJECT: XXXXX                  *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fshellf2   cf   e             workstn
    F*
     D/COPY SCOPYPR01D
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Title')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
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
     C                   if        %parms >= 1
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
     C     PRSCTL        begsr
     C                   endsr
    C*
