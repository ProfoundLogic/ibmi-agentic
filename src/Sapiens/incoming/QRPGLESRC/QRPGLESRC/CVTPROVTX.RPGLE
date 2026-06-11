     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE CVTPROVTX - Update Provider Taxonomy Codes
    H*********************************************************************
    H*  DESCRIPTION:  This program will update taxonomy code in FMPDP    *
    H*                and WMPDP file and generate report for invalid     *
    H*                taxonomy code and providers without taxonomy code. *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    COMMAND LINE                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  CREATED: 03/05/10  PROGRAMMER: C:227  Project: 133MIC            *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*********************************************************************
     Fprovtx    if   e             disk
     Ffmpdl     uf   e           k disk
     Fwmpdl     uf   e           k disk    rename(ppd01:wpd01) prefix(wd:2)
     Ffttcl1    if   e           k disk
     Fqsysprt   o    f   80        printer oflind(*inof) usropn
     Fqsysprt1  o    f   80        printer oflind(*inoa) usropn
    D*
     D psds          esds                  extname(szq1p)
    D*
     D*
     D ##CO#           S              3  0 inz
     D ##FND           S              3  0 inz
     D ##CONM          S             40    inz
     D ##FNAM          S             40    inz
     d companynm       s             40
     D comapi          c                   const('WA001R')
     D qca             S             80    dim(2) ctdata perrcd(1)
     D length          S             15  5
     D qqcmd           S             80
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     D kkfein          s                   like(pdfein)
     D kkmod           s                   like(pdmod)
     D kkcode          s                   like(tccode)
     D dsprov          ds            13
     D dsfein                  1      9  0
     D dsmod                  11     13  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM
     C                   PARM                    ##FNAM
    C*  Provider Master File
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
    C*  Taxonomy Master File
    C*
     C     tcky          klist
     C                   kfld                    kkcode
     C*
     C* Main program
     C*
     C* Company name
     C                   move      '090'         ##CO#
     c                   call      comapi        compl
     c                   move      ##conm        CompanyNm
      *
     C                   time                    dsdttm
     C*
     C                   eval      length = 80
     C                   movel(p)  qca(1)        qqcmd
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(2)        qqcmd
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   open      qsysprt
     C                   open      qsysprt1
     C                   except    head_tc
     C                   except    head_pr
     C*
     C* Update the taxonomy code in FMPDP
     C*
     C                   read      ptx01                                  99
     C                   dow       *in99 = *off

     C                   movel     txprov        dsprov
     C                   if        txcode <> *blanks

     C                   movel(p)  txcode        kkcode
     C     tcky          chain     ptc01                              98
     C                   if        *in98 = *on
     C                   if        *inof = *on
     C                   move      *off          *inof
     C                   except    head_tc
     C                   endif
     C*
     C                   except    detail_tc
     C                   else
     C                   eval      kkfein = dsfein
     C                   eval      kkmod = dsmod
     C     pdky          chain     ppd01                              97
     C                   if        *in97 = *off
     C                   eval      pdtaxo = txcode
     C                   movel(p)  q1user        pduser
     C                   movel(p)  q1pgm         pdpgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         pddt
     C                   move      dstime        pdtime
     C                   update    ppd01
     C                   endif                                                  *in97 = *off
     C                   endif                                                  *in98 = *on

     C                   endif                                                  txcode <> ' '

     C                   read      ptx01                                  99
     C                   enddo
     C*
    C*  Update provider master file
    C*
     C     *loval        setll     ppd01
     C                   read      ppd01                                  99
     C                   dow       *in99 = *off
     C                   if        (pdtype = '1' or pdtype = '4')
     C                             and pdtaxo = *blanks
     C                             and pdstat = 'A'
     C                   eval      kkfein = pdfein
     C                   eval      kkmod = pdmod
     C     pdky          chain     wpd01                              60
     C                   if        *in60 = *off
     C                   eval      wdstat = 'I'
     C                   movel(p)  q1user        wduser
     C                   movel(p)  q1pgm         wdpgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wddt
     C                   movel     dstime        wdtime
    C*
     C                   update    wpd01
    C*
     C                   endif
    C*
     C                   eval      pdstat = 'I'
     C                   movel(p)  q1user        pduser
     C                   movel(p)  q1pgm         pdpgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         pddt
     C                   move      dstime        pdtime
     C                   movel(p)  pdname        qqlast           56
     C                   movel(p)  pdfsn         qqfrst           15
     C                   move      '1'           qqterm            1
     C                   call      'SRNAME'
     C                   parm                    qqlast
     C                   parm                    qqfrst
     C                   parm                    qqterm
     C                   movel(p)  qqlast        xxname           40
     C                   update    ppd01
     C                   if        *inoa = *on
     C                   move      *off          *inoa
     C                   except    head_pr
     C                   endif
     C*
     C                   except    detail_pr
    C*
     C                   endif
    C*
     C                   read      ppd01                                  99
    C*
     C                   enddo
    C*
     C                   close     qsysprt
     C                   close     qsysprt1
     C                   move      *on           *inlr
    C*
     OQSYSPRT   E            head_tc        1 03
     O                       CompanyNm           60
     O          E            head_tc        1
     O                                           46 'Report of Invalid Taxonomy'
     O                                           63 'Code Assignments'
     O          E            head_tc        3
     O                                           14 'Report Date:'
     O                       dsdate              25 '  /  /    '
     O          E            head_tc        1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            head_tc        1
     O                                           12 'FEIN'
     O                                           19 'Mod'
     O                                         +004 'Invalid Taxonomy Code'
     O          E            head_tc        1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            detail_tc      1
     O                       dsfein        z     12
     O                       dsmod         z     19
     O                       txcode            +004

     OQSYSPRT1  E            head_pr        1 03
     O                       CompanyNm           60
     O          E            head_pr        1
     O                                           34 'Report of Providers'
     O                                           57 'Without Taxonomy Codes'
     O                                           66 'Assigned'
     O          E            head_pr        3
     O                                           14 'Report Date:'
     O                       dsdate              25 '  /  /    '
     O          E            head_pr        1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            head_pr        1
     O                                           12 'FEIN'
     O                                           19 'Mod'
     O                                         +004 'Provider Name'
     O          E            head_pr        1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            detail_pr      1
     O                       pdfein        z     12
     O                       pdmod         z     19
     O                       xxname            +004
** (QCMDEXC)
OVRPRTF FILE(QSYSPRT) PAGESIZE(66 82) LPI(6) CPI(10) PAGRTT(0) FORMTYPE(*STD)
OVRPRTF FILE(QSYSPRT1) PAGESIZE(66 82) LPI(6) CPI(10) PAGRTT(0) FORMTYPE(*STD)
