     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
    H/title Print Error log
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H* ============================================================= *
     H*FIX78018  - Fixes the CMC Queries                              *
     H*                                                               *
     H*  DESCRIPTION:  This program Fix the CMS queries data.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    None                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*                                                               *
     H*  CREATED: 09/22/11  PROGRAMMER:        SUPPORT: 78018         *
     H*                                                               *
     H* ============================================================= *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/06/25 C:333  Project WC2998                               *
     H*                  Changes to CMS Settlement Reporting.         *
     H*                  - Recompiled.                                *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*                  Recompile only
     H* ============================================================= *
     f*  CMS QUERY
     fwxqrp     uf   e             disk
     fwdd1l4    if   e           k disk
     f*
     f* ============================================================= *
     d@@ssn            s              9    inz(*blanks)
     d@@claim          s             13    inz(*blanks)
      *
     c* ============================================================= *
     c* main process
     c                   exsr      process
     c
     c                   eval      *inlr = *on
     c
     c* ============================================================= *
     c*    process => process for updating the claim in query w15589
     c* ============================================================= *
     c     process       begsr
     c
     c*    read the records from file to be updated.
     c                   read      pqr01
     c                   dow       not %eof(wxqrp)
     c                   if        %subst(mrdata:148:12) = ' '
     c                   eval      @@ssn = %subst(mrdata:29:9)
     c
     c*    get the claim number for SSN number.
     c     @@ssn         chain     wdd1l4
     c                   if        %found(wdd1l4)
     c                   eval      @@claim = %editc(D1FND:'X') +
     c                                     %editc(D1FYR:'X') +
     c                                     %editc(D1CASE:'X')
     c
     c                   eval      %subst(mrdata:147:13) = @@claim
     c                   update    pqr01
     c                   endif
     c                   endif                                                  claim# = *blanks
     c                   read      pqr01
     c
     c                   clear                   @@ssn
     c                   clear                   @@claim
     c
     c                   enddo
     c
     c                   endsr
     c*
