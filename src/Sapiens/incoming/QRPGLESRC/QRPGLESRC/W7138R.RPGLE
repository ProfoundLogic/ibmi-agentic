     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE W7138R  - Large Deductible Update & Exception Report
    H**************************************************************************
    H*  Description:
     H*  This program
    H*  The report program will be called to update all divisions and/or polici
     H*  the large deductible information from the Group level. ).  An exception
    H*  will be created to generate for all policies/divisions that do not have
    H*  deductible pay plan.
    H*
    H*
    H*  Called from:
    H*
    H*  Calls to: None.
    H*
    H*  Created: C:190     Project: WC1954       Date:  01/28/11
    H*
    H**************************************************************************
    H*  Revisions:                                                            *
     H*                                                                        *
     H* 07/23/19 C:136 Support: 81246                                          *
     H*                Correct date format for maint fields in WMLDP           *
     H*                                                                        *
     H*                                                                        *
     H* 01/22/13  C:287 Seq#: SY266_OVR                                        *
     H*                 Added DFTACTGRP and ACTGRP compile options             *
     H*                                                                        *
    H**************************************************************************
    F*
     Fsmorl     if   e           k disk
     Fwmldl1    uf a e           k disk
     Fwmldl4    if   e           k disk    rename(pld01:pld04) prefix(l4:2)
     Fwmldl2    if   e           k disk    rename(pld01:pld02) prefix(l2:2)
     Fwdelp     if   e             disk
     Fwdell     if   e           k disk    rename(pel01:pel02) prefix(e2:2)
     Fwdrrp     if   e             disk
     Fsmupl     if   e           k disk
     Fwxl7p     o    e             disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmrsl     if   e           k disk
     Fqsysprt   o    f   80        printer oflind(*inof)
    D*================================================================
    D*  External Data Structure
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*================================================================
    D* Stand Alone Fields
    D*================================================================
     Dxxfile           S                   like(orfil#)
     D ##CO#           S              3  0 inz
     D @@CO#           S              3    inz
     D ##FND           S              3  0 inz
     D ##CONM          S             40    inz
     D ##FNAM          S             40    inz
     D xxconm          s             40
     D ##data          s             80
     D ##leng          s              3  0
     D scdata          S             20    inz
     D scfld           S             30
     D rcd_found       S               n   inz
     D xxdt            S              8  0
     D xxdate          S              8  0
     D xxandt          S              7  0
     D svandt          S              7  0
     D i               S              2  0
     D j               S              2  0
     D k               S              2  0
     D xxtitl          s                   like(ortitl)
     D x1titl          s                   like(ortitl)
     D##jrst           s                   like(rsjrst)
     Dkkbscd           s                   like(bskey)
     D xxdata          s             60
     D n               s              3  0
     D xxormd          s              8  0
     D x1ormd          s              8  0
     D xxdt1           s               d
     Dxxexdt           s              8  0
     D xxdt2           s               d
     D xxmnth          s             15P 0
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     d sysDate7        s              7  0
    D*================================================================
    D* Constants
    D*================================================================
     D comapi          c                   const('WA001R')
    D*================================================================
    D* Data Structures
    D*================================================================
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     D**/copy SPrRtvBrnd
     D*
    C*================================================================
    C* Klist Definitions
    C*================================================================
     C*
     C     elky          KLIST
     C                   KFLD                    rrco#
     C                   KFLD                    rrfnd
     C*
     C     elky1         KLIST
     C                   KFLD                    elco#
     C                   KFLD                    elfnd
     C                   KFLD                    elemp#
     C*
     C     bsky          KLIST
     C                   KFLD                    e2co#
     C                   KFLD                    kkbscd
     C*
     C     rsky          KLIST
     C                   KFLD                    e2co#
     C                   KFLD                    e2rtcd
     C*
     C     ldky          KLIST
     C                   KFLD                    e2co#
     C                   KFLD                    e2fnd
     C                   KFLD                    e2emp#
     C                   KFLD                    e2div
     C                   KFLD                    e2fyr
     C*
     C     ldky1         KLIST
     C                   KFLD                    e2co#
     C                   KFLD                    e2fnd
     C                   KFLD                    ##jrst
     C                   KFLD                    ##emp#            9 0
     C                   KFLD                    e2pfdt
     C*
     C     ldky2         KLIST
     C                   KFLD                    e2co#
     C                   KFLD                    e2fnd
     C                   KFLD                    ##jrst
     C                   KFLD                    ##emp#
     C*
     C     ldky3         KLIST
     C                   KFLD                    elco#
     C                   KFLD                    elfnd
     C                   KFLD                    elfyr
     C                   KFLD                    elemp#
     C                   KFLD                    eldiv
     C*
     C     orky          KLIST
     C                   KFLD                    q1pgm
     C*
    C*================================================================
    C* Plist Definitions
    C*================================================================
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     dtyymd        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtyymd1       plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtmdyy        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      *blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9
     C                   parm                    @@mode            1
     C                   parm                    @@update          1
     C*
    C*================================================================
     C*
     C                   eval      rcd_found = *off
     C                   move      @@rrn         ##rrn             9 0
     C                   move      @@mode        ##mode            1
     C                   move      @@update      ##upd             1
      /Free
       sysDate7 = %Dec( %Char( %Date() : *Cymd0) : 7 : 0);
      /End-Free
     C*
     C                   if        ##mode = 'G'
     C     ##rrn         chain     prr01                              98
     C                   elseif    ##mode = 'E'
     C     ##rrn         chain     pel01                              98
     C                   endif
     C*
     C                   if        *in98 = *off
     C*
     C                   if        ##mode = 'G'
     C     elky          setll     pel02
     C     elky          reade     pel02                                  97
     C                   elseif    ##mode = 'E'
     C     elky1         setll     pel02
     C     elky1         reade     pel02                                  97
     C                   endif
     C*
     C                   if        ##mode = 'G'
     c                   eval      *in91 = *on
     C                   elseif    ##mode = 'E'
     c                   eval      *in92 = *on
     C                   endif
     C*
     C                   exsr      srrte
     C                   except    header
     C*
     C                   dow       *in97 = *off
     C*
     C                   if        ##upd = 'Y'
     C*
     C* Check if its Large deductible
     C*
     C                   movel(p)  e2bscd        kkbscd

     C     bsky          chain(n)  pbs01
     C                   if        %found
     C                   if        bslpgm = 'L'


     C* Check if record exists in WMLDP.
     C*
     C     ldky          chain(n)  pld01
     C                   if        not %found
     C*
     C* Get jurisdiction state
     C*
     C     rsky          chain(n)  prs01                              60
     C  N60              movel     rsjrst        ##jrst
     C*
     C* Write record in WMLDP if setup state matches jurisdiction state for poli
     C*
     c                   if        *in91 = *on
     c                   eval      ##emp# = 0
     C     ldky1         setgt     pld04
     C     ldky2         readpe    pld04                                  95
     C                   if        *in95 = *off
     C                   exsr      wrtldp
     C                   endif
     C                   endif

     c                   if        *in92 = *on
     C     ldky3         chain(n)  pld02
     C                   exsr      wrtldpe
     C                   endif

     C                   endif
     C                   else
     C   of              except    header
     C   of              eval      *inof = *off
     C*
     C                   eval      rcd_found = *on

     C* Get jurisdiction state
     C*
     C     rsky          chain(n)  prs01                              60
     C  N60              movel     rsjrst        ##jrst

     C                   except    detail
     C                   exsr      wrtl7p
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   if        ##mode = 'G'
     C     elky          reade     pel02                                  97
     C                   elseif    ##mode = 'E'
     C     elky1         reade     pel02                                  97
     C                   endif
     C*
     C                   enddo
     C*
     C                   endif
     C*
     C                   If        rcd_found
     C                   except    foot1
     C                   Endif
     C*
     C                   If        not rcd_found
     C                   except    foot2
     C                   except    foot1
     C                   Endif
     C*
     C                   seton                                        lr
    C*================================================================
     C* WRTLDP- Write record in for Group
     C*================================================================
     C     wrtldp        begsr
     C*
     C                   clear                   pld01
     C*
     C                   eval      ldco# = e2co#
     C                   eval      ldfnd = e2fnd
     C                   eval      ldemp# = e2emp#
     C                   eval      lddiv = e2div
     C                   eval      ldfyr = e2fyr
     C                   eval      ldefd1 = e2pfdt
     C*****              eval      ldexp1 = *ZEROS
     C                   eval      lddes1 = l4des1
     C                   eval      ldpct1 = l4pct1
     C                   eval      ldtyp1 = l4typ1
     C                   eval      ldamp1 = l4amp1
     C                   eval      ldamg1 = l4amg1
     C                   eval      ldpln1 = l4pln1
     C                   eval      ldvrb1 = l4vrb1
     C                   eval      ldocv1 = l4ocv1
     C                   eval      ldtot1 = l4tot1
     C                   eval      ldcls1 = l4cls1
     C                   eval      ldplv1 = l4plv1
     C                   eval      ldala1 = l4ala1
     C                   eval      ldrnw1 = l4rnw1
     C                   eval      ldr101 = l4r101
     C                   eval      ldr102 = l4r102
     C                   eval      ldr103 = l4r103
     C                   eval      ldr104 = l4r104
     C                   eval      ldr105 = l4r105
     C                   eval      ldr106 = l4r106
     C                   eval      ldr107 = l4r107
     C                   eval      ldr108 = l4r108
     C                   eval      ldr109 = l4r109
     C                   eval      ldr110 = l4r110

     C*  Load Anniversary rating date to SCR02 effective date.
     C     e2andt        add       10000         xxandt
     C     e2andt        ifeq      *zero
     C     e2andt        oreq      e2pfdt
     C     xxandt        orle      e2pfdt
     C     xxandt        orge      e2ptdt
     C                   z-add     *zeros        svandt
     C                   else
     C                   z-add     xxandt        svandt
     C                   endif

     c                   if        l2des2 <> ' '
     C                   eval      ldefd2 = e2pfdt
     c                   else
     C                   eval      ldefd2 = *zeros
     c                   endif

     C                   if        svandt <> *zeros
     C                   eval      ldefd2 = svandt
     C                   eval      ldexp1 = svandt
     c                   endif

     C                   eval      ldexp2 = ldexp2
     C                   eval      lddes2 = l4des2
     C                   eval      ldpct2 = l4pct2
     C                   eval      ldtyp2 = l4typ2
     C                   eval      ldamp2 = l4amp2
     C                   eval      ldamg2 = l4amg2
     C                   eval      ldpln2 = l4pln2
     C                   eval      ldvrb2 = l4vrb2
     C                   eval      ldocv2 = l4ocv2
     C                   eval      ldtot2 = l4tot2
     C                   eval      ldcls2 = l4cls2
     C                   eval      ldplv2 = l4plv2
     C                   eval      ldala2 = l4ala2
     C                   eval      ldrnw2 = l4rnw2
     C                   eval      ldr201 = l4r201
     C                   eval      ldr202 = l4r202
     C                   eval      ldr203 = l4r203
     C                   eval      ldr204 = l4r204
     C                   eval      ldr205 = l4r205
     C                   eval      ldr206 = l4r206
     C                   eval      ldr207 = l4r207
     C                   eval      ldr208 = l4r208
     C                   eval      ldr209 = l4r209
     C                   eval      ldr210 = l4r210
     C                   eval      lduser = q1user
     C                   eval      ldpgm  = q1pgm
     C                   eval      ldtime = dstime
     C                   eval      lddt   = sysDate7
     C                   eval      ldiusr = q1user
     C                   eval      ldipgm = q1pgm
     C                   eval      lditim = dstime
     C                   eval      ldidt  = sysDate7
     C                   eval      ldlirt = l4lirt
     C                   eval      ldclal = l4clal
     C                   eval      ldst   = l4st
     C*
     C                   write     pld01
     C*
     C                   endsr

    C*================================================================
     C* WRTLDPE- Write record at Employer level.
     C*================================================================
     C     wrtldpe       begsr
     C*
     C                   eval      ldco# = e2co#
     C                   eval      ldfnd = e2fnd
     C                   eval      ldemp# = e2emp#
     C                   eval      lddiv = e2div
     C                   eval      ldfyr = e2fyr
     C                   eval      ldefd1 = e2pfdt
     C                   eval      ldexp1 = *ZEROS
     C                   eval      lddes1 = l2des1
     C                   eval      ldpct1 = l2pct1
     C                   eval      ldtyp1 = l2typ1
     C                   eval      ldamp1 = l2amp1
     C                   eval      ldamg1 = l2amg1
     C                   eval      ldpln1 = l2pln1
     C                   eval      ldvrb1 = l2vrb1
     C                   eval      ldocv1 = l2ocv1
     C                   eval      ldtot1 = l2tot1
     C                   eval      ldcls1 = l2cls1
     C                   eval      ldplv1 = l2plv1
     C                   eval      ldala1 = l2ala1
     C                   eval      ldrnw1 = l2rnw1
     C                   eval      ldr101 = l2r101
     C                   eval      ldr102 = l2r102
     C                   eval      ldr103 = l2r103
     C                   eval      ldr104 = l2r104
     C                   eval      ldr105 = l2r105
     C                   eval      ldr106 = l2r106
     C                   eval      ldr107 = l2r107
     C                   eval      ldr108 = l2r108
     C                   eval      ldr109 = l2r109
     C                   eval      ldr110 = l2r110
     c                   if        l2des2 <> ' '
     C                   eval      ldefd2 = e2pfdt
     c                   else
     C                   eval      ldefd2 = *zeros
     c                   endif
     C                   eval      lddes2 = l2des2
     C                   eval      ldpct2 = l2pct2
     C                   eval      ldtyp2 = l2typ2
     C                   eval      ldamp2 = l2amp2
     C                   eval      ldamg2 = l2amg2
     C                   eval      ldpln2 = l2pln2
     C                   eval      ldvrb2 = l2vrb2
     C                   eval      ldocv2 = l2ocv2
     C                   eval      ldtot2 = l2tot2
     C                   eval      ldcls2 = l2cls2
     C                   eval      ldplv2 = l2plv2
     C                   eval      ldala2 = l2ala2
     C                   eval      ldrnw2 = l2rnw2
     C                   eval      ldr201 = l2r201
     C                   eval      ldr202 = l2r202
     C                   eval      ldr203 = l2r203
     C                   eval      ldr204 = l2r204
     C                   eval      ldr205 = l2r205
     C                   eval      ldr206 = l2r206
     C                   eval      ldr207 = l2r207
     C                   eval      ldr208 = l2r208
     C                   eval      ldr209 = l2r209
     C                   eval      ldr210 = l2r210
     C                   eval      lduser = q1user
     C                   eval      ldpgm  = q1pgm
     C                   eval      ldtime = dstime
     C                   eval      lddt   = sysDate7
     C                   eval      ldiusr = q1user
     C                   eval      ldipgm = q1pgm
     C                   eval      lditim = dstime
     C                   eval      ldidt  = sysDate7
     C                   eval      ldlirt = l2lirt
     C                   eval      ldclal = l2clal
     C                   eval      ldst   = l2st
     C*
     C                   write     pld01
     C*
     C                   endsr
    C*================================================================
     C* WRTL7P- Write record in
     C*================================================================
     C     wrtl7p        begsr
     C*
     C                   clear                   pl701
     C*
     C                   eval      l7co# = e2co#
     C                   eval      l7fnd = e2fnd
     C                   eval      l7emp# = e2emp#
     C                   eval      l7div = e2div
     C                   eval      l7fyr = e2fyr
     C                   eval      l7bscd = e2bscd
     C*
     C                   write     pl701
     C*
     C                   endsr
    C*================================================================
     C* SRRTE - Print routing page
     C*================================================================
     C     srrte         begsr
     C*
     C                   call      comapi        compl                  99
     C*
     C                   if        *in99 = *off
     C                   movel(p)  ##conm        ##data
     C                   endif
     C                   call      'RTVBRND'
     C                   parm      '@'           #btype            1
     C                   parm                    #brnd1
     C                   parm                    #brnd2
     C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C*
     C                   movel     ##data        xxconm
     C*
     C     q1user        chain     pup01                              60
     C*
     C                   time                    dsdttm
     C*
     C     orky          chain     por01                              60
     C*
     C     *in60         ifeq      '0'
     C*
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
    C*
     C                   movel     ortitl        x1titl
    C*
     C                   if        ortitl <> *blanks
     C                   move(p)   ortitl        xxdata
     C     ' '           check     xxdata        n
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      xxtitl
     C                   endif
     C                   endif
    C*
     C                   movel(p)  xxtitl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        xxtitl
     C*
     C                   except    route
     C*
     C                   endif
     C*
     C                   endsr
     C*=========================================================================
     O*** Qsysprt
     OQsysprt   E            ROUTE          1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE          1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE          1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE          1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE          3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE          1
     O                                           15 'Route to:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
     O*
     O          E            ROUTE          2
     O                                           19 'Report Title:'
     O                       x1titl              80
     O          E            ROUTE          2
     O                                           19 'Program Name:'
     O                       ORPNAM              30
     O                                           36 'File:'
     O                       ORFIL#              38
     O                                           46 'Copies:'
     O                       ORCOPY        Z     50
     O                                           56 'Hold:'
     O                       ORHOLD              61
     O*
     O          E            ROUTE          2
     O                                           19 'Output queue:'
     O                       OROUTQ              30
     O                                           37 'Forms:'
     O                       ORFORM              48
     O                                           56 'Save:'
     O                       ORSAVE              61
     O**   SELECTION CRITERIA
     O          E            ROUTE       2  1
     O                                         +004 'REPORT SELECTIONS'
     O          E            ROUTE          1
     O                                         +006 'COMPANY        :'
     O                       e2CO#         Z   +002
     O          E            ROUTE          1
     O                                         +006 'GROUP          :'
     O                       e2FND         Z   +002
     O          E            ROUTE          1
     O               92                        +006 'POLICY YEAR    :'
     O               92      e2fYR         Z   +002
     O          E            ROUTE          1
     O               92                        +006 'EMPLOYER       :'
     O               92      e2EMP#        Z   +002
     O          E            ROUTE          1
     O               92                        +006 'DIVISION       :'
     O               92      E2DIV             +002
     O*
     O          E            ROUTE      23  1
     O*                                          05 '(c)'
     O                       #brnd1
     O          E            ROUTE          1
     O                       #brnd2
     O          E            route          1  1
     O                       dstime               9 '  :  :  '
     O                       xxconm              60
     O                       dsdate              80 '  /  /    '
     O          E            Header      1  2
     o                       q1pgm               11
     O                       xxtitl              60
     O                                           75 'Page'
     O                       page          Z     80
     O*
     O          E            Header         2
     O                                            8 'Group'
     O                                           23 'Employer'
     O                                           34 'State'
     O                                           47 'Division'
     O                                           63 'Policy Year'
     O                                           76 'Pay Plan'
     O*
     O          E            detail         1
     O                       e2fnd                8
     O                       e2emp#              23
     O                       ##jrst              33
     O                       e2div               47
     O                       e2fyr               59
     O                       e2bscd              76
     O*
     O          e            foot1          1
     O                                           30 ' *** End  Of  Report *** '
     O*
     O          e            foot2          1
     O                                           30 ' *** No Records Found *** '
     O*
