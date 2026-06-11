     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
    H/TITLE RTVBRND - API - Retrieve company brand information
    H*********************************************************************
     H* inType: '@' - Copyright Information                               *
     H*         'C' - Company Information                                 *
     H*         'I' - Name, Inc. Only Information                         *
     H*         'N' - Name Only Information                               *
     H*         'F' - Information from file                               *
     H*         'D' - Document Indexing information with company info     *
     H* outLine1: required for all types                                  *
     H* outLine2: optional                                                *
     H*           - loaded only for '@'                                   *
     H* inCo#: required for 'F'                                           *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
     H* 06/10/20  C:33  Project: SY419   *** No comments                  *
     H*                 Sapiens                                           *
    H*********************************************************************
     fsmcol     if   e           k disk

     DnameOnly         c                   'Sapiens'
     DnameIncOnly      c                   'Sapiens, Inc.'
     Dcopyright1       c                   'Sapiens, Inc. All rights-
     D                                      reserved.'
     Dcopyright2       c                   'Sapiens is a trademark of-
     D                                      Sapiens, Inc.'

     Dindexonly        c                   'Document indexing has rolled. -
     D                                      Please contact Sapiens support.'
     D*entry
     D/copy SPrRtvBrnd
     DRtvBrnd          pi
     D inType                         1    const
     D outLine1                      80
     D outLine2                      80    options(*nopass)
     D inCo#                          3    options(*nopass)

     DxxCo#            s                   like(COCO#)

     C                   select
     C                   when      inType = '@'
     C                   exsr      LoadCopyright
     C                   when      inType = 'C'
     C                   exsr      LoadCompanyNm
     C                   when      inType = 'I'
     C                   exsr      LoadNameInc
     C                   when      inType = 'N'
     C                   exsr      LoadNameOnly
     C                   when      inType = 'F'
     C                   exsr      LoadFromFile
     C                   when      inType = 'D'
     C                   exsr      Loadindname
     C                   endsl

     C                   eval      *inlr = *on

     C*====================================================================
     C* Initialization
     C*====================================================================
     C     *inzsr        begsr
     C*
     C                   clear                   outLine1
     C                   if        %parms >= 3
     C                   clear                   outLine2
     C                   endif
     C*
     C                   endsr

     C*====================================================================
     C* Load company name information
     C*====================================================================
     C     LoadCompanyNm begsr
     C*
     C                   eval      outLine1 = copyright1
     C*
     C                   endsr

     C*====================================================================
     C* Load copyright information
     C*====================================================================
     C     LoadCopyright begsr
     C*
     C                   eval      outLine1 = '(c) ' + %char(*year)
     C                                      + ' ' + copyright1
     C                   if        %parms >= 3
     C                   eval      outLine2 = copyright2
     C                   endif
     C*
     C                   endsr

     C*====================================================================
     C* Load name information from file
     C*====================================================================
     C     LoadFromFile  begsr
     C*
     C                   move      inCo#         xxCo#
     C                   clear                   COBRND
     C     xxCo#         chain     smcol
     C                   select
     C                   when      COBRND <> *blank
     C                   eval      outLine1 = COBRND
     C                   other
     C                   eval      outLine1 = nameOnly
     C                   endsl
     C*
     C                   endsr

     C*====================================================================
     C* Load name, inc. only information
     C*====================================================================
     C     LoadNameInc   begsr
     C*
     C                   eval      outLine1 = nameIncOnly
     C*
     C                   endsr

     C*====================================================================
     C* Load name only information
     C*====================================================================
     C     LoadNameOnly  begsr
     C*
     C                   eval      outLine1 = nameOnly
     C*
     C                   endsr
     C*====================================================================
     C* Load name only information
     C*====================================================================
     C     Loadindname   begsr
     C*
     C                   eval      outLine1 = indexonly
     C*
     C                   endsr
