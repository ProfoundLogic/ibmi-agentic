     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE CVTNCLOC  - Generate Additional Location records
    H*********************************************************************
    H*  DESCRIPTION:  This program will Generate Additional Location     *
    H*                records based on the WXULP                         *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    COMMAND LINE                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  CREATED: 04/20/10  PROGRAMMER: C:189  Project: 114WWA            *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*********************************************************************
     Fwxulp     if   e             disk
     Fwmcml15   if   e           k disk
     Fwmall     uf a e           k disk
     Fwdell     if   e           k disk
     Fqsysprt   o    f  132        printer oflind(*inof) usropn
    D*
     D psds          esds                  extname(szq1p)
    D*
     D ##conm          S             40    inz
     D ##fnam          S             40    inz
     D xxco#           S              3  0 inz
     D xxfnd           S              3  0 inz
     D xxpds#          S                   LIKE(CMPDS#)
     d companynm       s             40
     D comapi          c                   const('WA001R')
     D qca             S             80    dim(1) ctdata perrcd(1)
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

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
    C*
     C     COMPL         PLIST
     C                   PARM                    xxco#
     C                   PARM                    xxfnd
     C                   PARM                    ##conm
     C                   PARM                    ##fnam
     C*
    C*  key list for Claim file
    C*
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxpds#
     C     cmky1         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
    C*
     C     elky1         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     C* Main program
     C*
     C* Company name
     c                   call      comapi        compl
     c                   move      ##conm        CompanyNm
      *
     C                   move      qqco#         xxco#
     C                   move      qqfnd         xxfnd
     C                   time                    dsdttm
     C*
     C                   open      qsysprt
     C                   except    header
     C*
     C* Update Additional Entity/Location File WMALP.
     C*
     C                   read      pul01                                  99
     C                   dow       *in99 = *off

     C                   movel     ulicc#        xxpds#
     C     cmky          chain     pcm01                              98
     C                   if        *in98 = *off
     C                   z-add     0             xxelid           10 0
     C     cmky1         setgt     pal01
     C     cmky1         readpe    pal01                                  97
     C                   if        *in97 = *off
     C                   move      aleltp        @@eltp            1
     C                   move      alelid        xxelid
     C                   add       1             xxelid
     c                   else
     C                   add       1             xxelid
     c                   endif
     C                   clear                   PAL01
     C                   eval      alco# = cmco#
     C                   eval      alfnd = cmfnd
     C                   eval      alemp# = cmemp#
     C                   eval      aldiv = cmdiv
     C                   eval      aleltp = @@eltp
     C     elky1         chain     pel01
     c                   if        %found
     c                   eval      alefdt = elpfdt
     C                   movel     ulpol#        alname
     C                   move      xxelid        alelid
     C                   move      'A'           alstat
     C                   movel(p)  q1user        aluser
     C                   movel(p)  q1user        aliusr
     C                   movel(p)  q1pgm         alpgm
     C                   movel(p)  q1pgm         alipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         aldt
     C                   movel     srdto         alidt
     C                   move      dstime        altime
     C                   move      dstime        alitm
     C                   WRITE     PAL01
     C                   endif
     C                   else
     C*
     C                   move      ulunq#        qqunq#           10
     C                   move      ulicmp        qqicmp            5
     C                   move      ulpol#        qqpol#           20
     C                   move      ulicc#        qqicc#           20
     C                   move      ulrc#         qqrc#            20
     C                   move      ulinl1        qqinl1           30
     C     *INOF         IFEQ      *ON
     C                   except    header
     c                   endif
     C                   except    detail
     C                   endif
     C                   read      pul01                                  99
     C                   enddo
    C*
     C                   close     qsysprt
     C                   move      *on           *inlr
    C*
     OQSYSPRT   E            header         1 03
     O                       CompanyNm           83
     O          E            header         1
     O                       dstime              08 '  :  :  '
     O                                           52 'Report of Records not'
     O                                           79 'found in Claim master/Empl'
     O                                           95 'oyer Detail File'
     O                       dsdate             128 '  /  /    '
     O          E            header         3
     O                       Q1PGM               10
     O                                          124 'PAGE:'
     O                       PAGE               128
     O          E            header         1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            header         1
     O                                           10 'Unique ID#'
     O                                         +002 'Insolvent Company'
     O                                         +002 'Policy #'
     O                                         +014 'Insolvent Co#s Claim'
     O                                         +004 'Receiver Claim'
     O                                         +008 'Insured Name'
     O          E            header         1
     O                                         +053 'Number'
     O                                         +018 'Number'
     O                                         +016 'Line #1'
     O          E            header         1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            detail         1
     O                       qqunq#              10
     O                       qqicmp            +002
     O                       qqpol#            +014
     O                       qqicc#            +002
     O                       qqrc#             +003
     O                       qqinl1            +002
