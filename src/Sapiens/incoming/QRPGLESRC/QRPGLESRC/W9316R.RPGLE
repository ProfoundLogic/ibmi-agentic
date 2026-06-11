    h*====================================================================
     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h*====================================================================
    h*--------------------------------------------------------------------
    h*  CALLED FROM: W9316C                                              *
    h*                                                                   *
    h*  CALLS TO:                                                        *
    h*                                                                   *
    h*  CREATED: 06/09/10      C: 235    PROJECT: 114WWR                 *
    h*--------------------------------------------------------------------
    h*  REVISIONS                                                        *
    h*                                                                   *
     H* 08/05/15 C:307 Project: SY354                                     *
     H*                -Adding 'Route To:'                                *
     H*                                                                   *
     h* 08/26/14 C:298 Project: SY354                                     *
     h*                -Implement Routing Page Changes.                   *
     h*                                                                   *
     h*  04/21/11 C:246 Support: 76662  CLT:114                           *
     h*                 Modified the recoveries field  CITTRC to have     *
    h*                 decimals if the amount is zeros.                   *
    h*                                                                   *
     h*  02/23/11 C:53  PROJECT: 114WYG                                   *
     h*                 Include all closed claims not just for current mth
    h*                 (remove 06/30/10 changes that controled this)
    h*                                                                   *
     h*  06/30/10 C:248 SUPPORT: 73423                                    *
     h*                 The claimant field should be populated with       *
     H*                 added zeros
     h*                 Date format changed to ISO standard               *
     h*                 Run Date text changed to Evaluation Date in WZCIP *
     h*                 Evaluation date value is the month end date       *
    h*                                                                   *
    h*--------------------------------------------------------------------
     h*
    f*====================================================================
    f*  F I L E S
    f*====================================================================
     f*
     fwmcml1    if   e           k disk
     fwmcdl     if   e           k disk
     fsmorl     if   e           k disk
     fwmchl     if   e           k disk
     fwdpal     if   e           k disk    rename(ppa01:ppa02)
     fwmeml     if   e           k disk
     fwzcip     uf a e             disk
     fwtabl     if   e           k disk
     f                                     ignore(pat01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f*************                        ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcs01)
     f                                     ignore(pcn01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f*************                        ignore(pna01)
     f                                     ignore(pnc01)
     f*************                        ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
     **System Company
     fsmcol     if   e           k disk
     ** User Parameter
     fwmupl     if   e           k disk
     ******
     *
     Fqsysprt   o    f  132        printer oflind(*inof)
     *
    d*====================================================================
    d*  DEFINITION SPECIFICATIONS
    d*====================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    d*====================================================================
    d*  DEFINITION SPECIFICATIONS
    d*====================================================================
     d*
     d @co#            s              3  0
     d @fnd            s              3  0
     d ##DATA          s             80
     d ##leng          s              3  0
     d xxdata          s             60
     d n               s              3  0
     Dincdutydays      s              3  0
     Dexcdutydays      s              3  0
     Dtotdutydays      s              3  0
     denddate          s               d
     dstrdate          s               d
     d kkefdt          s                   like(chefdt)
     d xxfile          s                   like(orfil#)
     d xxtitl          s                   like(ortitl)
     d x1titl          s                   like(ortitl)

     d                 ds
     d dsdttm                        14  0
     d dstime                         6  0 overlay(dsdttm)
     d dsdate                         8  0 overlay(dsdttm:7)

     d                 ds
     d lstStrDt                       7  0
     d lstYear                        3  0 overlay(lstStrDt)
     d lstMth                         2  0 overlay(lstStrDt:4)
     d lstDay                         2  0 overlay(lstStrDt:6)

     d                 ds
     d lstEndDt                       7  0
     d endyr                          3  0 overlay(lstEndDt)
     d endMth                         2  0 overlay(lstEndDt:4)
     d endday                         2  0 overlay(lstEndDt:6)

     d                 ds
     d dtymd                          8  0
     d @@yy                    1      4
     d @@mm                    5      6
     d @@dd                    7      8

     D tmpmr           S             11  2 inz(0)
     D tmpmp           S             11  2 inz(0)
     D tmpmi           S             11  2 inz(0)
     D tmpmrc          S             11  2 inz(0)
     D tmpmlr          S             11  2 inz(0)
     D tmpmlp          S             11  2 inz(0)
     D tmpir           S             11  2 inz(0)
     D tmpip           S             11  2 inz(0)
     D tmpii           S             11  2 inz(0)
     D tmpirc          S             11  2 inz(0)
     D tmpilr          S             11  2 inz(0)
     D tmpilp          S             11  2 inz(0)

     D tmpfmr          S             11  2 inz(0)
     D tmpfmp          S             11  2 inz(0)
     D tmpfmi          S             11  2 inz(0)
     D tmpfmrc         S             11  2 inz(0)
     D tmpfmlr         S             11  2 inz(0)
     D tmpfmlp         S             11  2 inz(0)
     D tmpfir          S             11  2 inz(0)
     D tmpfip          S             11  2 inz(0)
     D tmpfii          S             11  2 inz(0)
     D tmpfirc         S             11  2 inz(0)
     D tmpfilr         S             11  2 inz(0)
     D tmpfilp         S             11  2 inz(0)

     D totlre          S             11  2 inz(0)
     D totlpd          S             11  2 inz(0)
     D totlin          S             11  2 inz(0)
     D totlrec         S             11  2 inz(0)
     D totllr          S             11  2 inz(0)
     D totllp          S             11  2 inz(0)

     D tmpcnt          S              5  0 inz(0)
     D tmpicnt         S              5  0 inz(0)
     D tmpfcnt         S              5  0 inz(0)
     D tmpficnt        S              5  0 inz(0)
     D totlcnt         S              5  0 inz(0)
     D tottyp          S              3

     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8

      ***  Beg Add ***** 06/30/10  ************************************
     D amount_edtwrd   C                   '0         .  -'
     D cittrr#         S             11  2 inz(0)
     D cittar#         S             11  2 inz(0)
     D cittti#         S             11  2 inz(0)
     D cittrc#         S             11  2 inz(0)
     D cittlr#         S             11  2 inz(0)
     D cittlp#         S             11  2 inz(0)
     D ZeroDt          S               d   datfmt(*iso)
      ***  Beg Del ***** 02/23/11  ************************************
      *****
     D*ClmEftDt        S               d   datfmt(*iso)
      *****
     d*****            ds
     D*EvalDt                          d   datfmt(*iso)
     d*CN##                    1      2
     d*YY##                    3      4
     d*SPS1                    5      5    INZ('-')
     d*MM##                    6      7
     d*SPS2                    8      8    INZ('-')
     d*DD##                    9     10
      ***  End Del ***** 02/23/11  ************************************

      ***  End Add ***** 06/30/10  ************************************

     D                 ds                  inz
     D rr                      1     90  2
     D                                     dim(10)                              RR REMAIN RSV
     D cmrr01                  1      9  2
     D cmrr02                 10     18  2
     D cmrr03                 19     27  2
     D cmrr04                 28     36  2
     D cmrr05                 37     45  2
     D cmrr06                 46     54  2
     D cmrr07                 55     63  2
     D cmrr08                 64     72  2
     D cmrr09                 73     81  2
     D cmrr10                 82     90  2

     D                 ds                  inz
     D ar                      1     90  2
     D                                     dim(10)                              RT RSV TYPES
     D cmar01                  1      9  2
     D cmar02                 10     18  2
     D cmar03                 19     27  2
     D cmar04                 28     36  2
     D cmar05                 37     45  2
     D cmar06                 46     54  2
     D cmar07                 55     63  2
     D cmar08                 64     72  2
     D cmar09                 73     81  2
     D cmar10                 82     90  2

     D                 ds                  inz
     D rt                      1     20
     D                                     dim(10)                              RT RSV TYPES
     D cmrt01                  1      2
     D cmrt02                  3      4
     D cmrt03                  5      6
     D cmrt04                  7      8
     D cmrt05                  9     10
     D cmrt06                 11     12
     D cmrt07                 13     14
     D cmrt08                 15     16
     D cmrt09                 17     18
     D cmrt10                 19     20
     D*
    D*--------------------------------------------------------------------
    D*  Copybooks for Prototypes                                         *
    D*--------------------------------------------------------------------
     D*
     D/copy sprdate
     D*
    C*--------------------------------------------------------------------
    C*  Parameter Lists                                                  *
    C*--------------------------------------------------------------------
    C*  PLIST                                                            *
    C*--------------------------------------------------------------------
     C*
     C     *Entry        plist
     C                   parm                    @DATE             8
     C*
      ***** Beg Chg ***** 08/26/14 *************************************************
     C*****dtyymd        plist
     C     dtyymd0       plist
      ***** End Chg ***** 08/26/14 *************************************************
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtyymd1       plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
      ***  Beg Chg ***** 06/30/10  ************************************
     C     dtyymd#1      plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*ISO'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdyy#1      plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*ISO'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      ***  End Chg ***** 06/30/10  ************************************

    C*--------------------------------------------------------------------
    C*  Key Lists                                                        *
    C*--------------------------------------------------------------------
     C*
     C     cmlky         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C*
     C     wdky1         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C     emlky         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C*   Table file
     C*
     C     tbky          klist
     C                   kfld                    cmco#
     C                   kfld                    kkkey            10

     C     chky1         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    kkefdt
     C*
     C     chky2         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C     cmpky         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
      *================================================================
      *  *LIKE DEFN
      *================================================================
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********     *LIKE         DEFINE    cittrc        xxtotr
     C**********     *LIKE         DEFINE    cittrr        xxsumrr
     C**********     *LIKE         DEFINE    cittar        xxsumar
     C**********     *LIKE         DEFINE    cittlr        xxsumlr
     C**********     *LIKE         DEFINE    cittlp        xxsumlp
     C**********     *LIKE         DEFINE    cittti        xxtoti
      *
     C     *LIKE         DEFINE    cittrc#       xxtotr
     C     *LIKE         DEFINE    cittrr#       xxsumrr
     C     *LIKE         DEFINE    cittar#       xxsumar
     C     *LIKE         DEFINE    cittlr#       xxsumlr
     C     *LIKE         DEFINE    cittlp#       xxsumlp
     C     *LIKE         DEFINE    cittti#       xxtoti
      ***  END Chg ***** 06/30/10  ************************************
     C     *LIKE         DEFINE    tmpmr         xxpmr
     C     *LIKE         DEFINE    tmpmp         xxpmp
     C     *LIKE         DEFINE    tmpmi         xxpmi
     C     *LIKE         DEFINE    tmpmrc        xxpmrc
     C     *LIKE         DEFINE    tmpmlr        xxpmlr
     C     *LIKE         DEFINE    tmpmlp        xxpmlp
     C*
     C     *LIKE         DEFINE    tmpir         xxpir
     C     *LIKE         DEFINE    tmpip         xxpip
     C     *LIKE         DEFINE    tmpii         xxpii
     C     *LIKE         DEFINE    tmpirc        xxpirc
     C     *LIKE         DEFINE    tmpilr        xxpilr
     C     *LIKE         DEFINE    tmpilp        xxpilp
     C     *LIKE         DEFINE    tmpcnt        xxcnt
     C     *LIKE         DEFINE    tmpicnt       xxicnt
     C     *LIKE         DEFINE    pafrdt        stdate
     C     *LIKE         DEFINE    pafrdt        enddt
     C     *LIKE         DEFINE    pafrdt        cal_sdt
     C     *LIKE         DEFINE    pafrdt        cal_edt
    C*====================================================================
     C*  MAIN LINE LOGIC
    C*====================================================================
     C*
     C                   Read      pcm01
     C*
     C                   dow       not %eof
     C*
     c** Record only claims will be excluded.
     C*
     c                   if        (cmfnd= 630 and  cmltfl <> 'R')
     c                              or  (cmfnd= 640 and  cmltfl <> 'R')

      ***  Beg Del ***** 02/23/11  ************************************
      ***  Beg Chg ***** 06/30/10  ************************************
     C**Skip*claim if it is closed previous months
     c********
     C*****chky2         setgt     pch01
     C*****chky2         readpe    pch01                                  50
     C********           if        *in50 = *off
     C********
     C****Claim Effective date format changed to ISO
     C********           eval      srdti = CHEFDT
     C********           call      'SRCVTDAT'    dtyymd#1
     C********           if        srerr = *off
     C********           move      srdto         ClmEftDt
     C********
     C****Run*Date/Evaluation date format changed to ISO
     C********           move      @date         srdti
     C********           call      'SRCVTDAT'    dtmdyy#1
     C********           if        srerr = *off
     C********           move      srdto         EvalDt
     C********           move      '01'          DD##
     C********
     c**process only for claims closed current month
     C********           IF        (CHOCST = 'O') OR
     C********                     (CHOCST = 'C'  AND ClmEftDt >= EvalDt)
     c*****End Chg ***** 06/30/10  ************************************
     c*
     c                   exsr      SrBldRec
     c                   exsr      SrReserve
     c                   exsr      SrOther
     c*
     c                   write     pci01
     c*
      ***  Beg Del ***** 02/23/11  ************************************
     c*****Beg Chg ***** 06/30/10  ************************************
     C********           ENDIF
     C********           ENDIF
     C********           ENDIF
     C********           ENDIF
     c*****End Chg ***** 06/30/10  ************************************
      ***  End Del ***** 02/23/11  ************************************
     C                   endif
     c*
     C                   Read      pcm01
     c*
     c                   enddo
     c*
    C*====================================================================
     C*  PRINTER FILE INFO
    C*====================================================================
     c*******Write the summery Report
     c*
     C                   time                    dsdttm
     c*
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     *  User Name if no Distribution specified in routing
     *
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
     *
     C                   if        ortitl <> *blanks
     C                   move(p)   ortitl        xxdata
     C     ' '           check     xxdata        n
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      xxtitl
     C                   endif
     C                   endif
     *
     C                   movel(p)  xxtitl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        xxtitl
     *
     C                   movel(p)  ortitl        x1titl
     c                   endif
     c*
      ***** Beg Del ***** 08/26/14 *************************************************
     c************       except    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     c                   except    header
     c*
     c                   exsr      wrtdta1
     c                   except    detail
     c                   except    space#
     c                   except    total
     c                   except    space#
     c*
     c                   exsr      wrtdta2
     c                   except    detail
     c                   except    space#
     c                   except    total
     c                   except    space#
     c                   except    fot1
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
          Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c*
     c                   eval      *inlr = *on
    C*====================================================================
    C*====================================================================
     C     SrBldRec      begsr
     C*
     C                   clear                   pci01
     c
     c                   movel(p)  cmfnd         cifnd
     C                   move      cmemp#        citwn                          ACCDT DATE
     c
     c                   eval      cicase = %editc(cmfnd:'X') +
     c                             %editc(cmfyr:'X') + %editc(cmcase:'X')
     c
      ***  Beg Chg ***** 06/30/10  ************************************
     c***********        eval      ciclmt= '1'
     c                   eval      ciclmt= '00000001'
      ***  End Chg ***** 06/30/10  ************************************
     c                   eval      cityp1= 'WC'
     c                   if        cmltfl = 'M'
     c                   eval      cityp2= 'MED'
     C                   elseif    cmltfl = 'I'
     c                   eval      cityp2= 'IND'
     C                   endif
     C*
     C***Accident month,Accident day,Accident year
     C*
     C                   eval      srdti = cmacdt
      ***** Beg Chg ***** 08/26/14 *************************************************
     C**********         call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        srerr = *off
     C                   movel     srdto         dtymd                          ACCDT DATE
     C                   movel     @@dd          ciacdd                         ACCDT DATE
     C                   movel     @@mm          ciacmm
     C                   movel     @@yy          ciacyr
     C                   else
     C                   movel     *blanks       ciacdd
     C                   movel     *blanks       ciacmm
     C                   movel     *blanks       ciacyr
     C                   endif
     C*
     C***Reported month,Reported day,Reported year
     C*
     C                   clear                   dtymd
     C                   eval      srdti = cmrcvd
      ***** Beg Chg ***** 08/26/14 *************************************************
     C**********         call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        srerr = *off
     C                   movel     srdto         dtymd                          ACCDT DATE
     C                   movel     @@dd          cirpdd                         ACCDT DATE
     C                   movel     @@mm          cirpmm
     C                   movel     @@yy          cirpyy
     C                   else
     C                   movel     *blanks       cirpdd
     C                   movel     *blanks       cirpmm
     C                   movel     *blanks       cirpyy
     C                   endif
     C*
     C* Claim Closed date
     C*
     C                   time                    dsdttm
     C                   eval      kkefdt = dtcymd(dsdate)
     C*
     C     chky1         setgt     pch01
     C     chky2         readpe    pch01
     C*
     C                   if        CHOCST = 'C'
     C*
     C                   eval      SRDTI = CHEFDT
      ***** Beg Chg ***** 08/26/14 *************************************************
     C*********          call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        SRERR = *OFF
     C                   movel     srdto         dtymd                          ACCDT DATE
     C                   movel     @@dd          cicldd                         ACCDT DATE
     C                   movel     @@mm          ciclmm
     C                   movel     @@yy          ciclyy
     C                   else
     C                   movel     *blanks       cicldd
     C                   movel     *blanks       ciclmm
     C                   movel     *blanks       ciclyy
     C                   endif
     C                   endif
     C*
     C                   eval      cidesc = cmacds +
     C                             cmacd2 + cmacd3
     C*
     C                   eval      cisite = cmctg1

     C                   movel     *blanks       kkkey
     C                   movel(p)  cmcaus        kkkey
     C     tbky          chain     pca01                              99
     C                   if        (not *in99)
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel(p)  caedic        cicaus
     C                   Eval      cicaus = (' ' + caedic)
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif
     C*
     C                   eval      cilcau = cmlscv
     C*
     C                   movel     *blanks       kkkey
     C                   movel(p)  cmpart        kkkey
     C*
     C     tbky          chain     ppa01                              99
     C                   if        (not *in99)
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel(p)  paedip        cipart
     C                   Eval      cipart = (' ' + paedip)
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif

     C                   movel     *blanks       kkkey
     C                   movel(p)  cmnatr        kkkey
     C     tbky          chain     pna01                              99
     C                   if        (not *in99)
     C                   movel(p)  naedin        cinart
     C                   endif

     C                   eval      ciocst = cmstat
     C                   eval      cimelj = 'CCIC'
     C*
     C***MELTown
     C*
     C     emlky         chain     pem01                              99
     C                   if        (not *in99)
     C                   eval      cimelt = emname + emnam2
     C                   endif
    C*--------------------------------------------------------------------
     C*
     C***Lost duty days
     C*
     C                   move      @date         srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   if        srerr = *off
     C                   movel     srdto         lststrdt          7 0
     C                   movel     srdto         lstenddt          7 0
     C                   endif
     C*
     C*** Build the first date of month
     C*
     C                   eval      lstday = 01
     C*
     C*** Build last day of submitted month
     C*
     C     *cymd         move      lststrdt      enddate
     C                   adddur    1:*m          enddate
     C                   subdur    1:*d          enddate
     C                   Extrct    enddate:*d    endday
     C                   move      lststrdt      stdate
     C                   move      lstenddt      enddt
     C*
     C                   eval      totdutydays = 0
     C                   eval      incdutydays = 0
     C                   eval      excdutydays = 0
     C*
     C     wdky1         setll     ppa02                                  70
     C     wdky1         reade     ppa02                                  70
     C                   dow       *in70 = *off
     C*
     C********           if        pabr2 ='PT' or pabr2 ='TT'
     C                   if        paretp='C' and pabr2 <>'PP'
     C*
     C                   if        (pafrdt <= enddt) and
     C                             (patodt >= stdate)
     C*
     C                   if        pafrdt <= stdate
     C                   eval      cal_sdt = stdate
     C                   else
     C                   eval      cal_sdt = pafrdt
     C                   endif
     C*
     C                   if        patodt >= enddt
     C                   eval      cal_edt = enddt
     C                   else
     C                   eval      cal_edt = patodt
     C                   endif
     C*
     C                   call      'SRDDIF7'
     C                   parm                    cal_sdt
     C                   parm                    cal_edt
     C                   parm      *Zeros        @los              5 0
     C                   parm      *Blanks       srerr             1
     C*
     C                   if        patype = 'V' or patype = 'S'
     c                             or  patype = 'R'
     C                   eval      excdutydays = excdutydays+@los
     c                   else
     C                   eval      incdutydays = incdutydays+@los
     C                   endif
     C                   endif
     C                   endif
     C     wdky1         reade     ppa02                                  70
     c                   enddo
     C                   eval      totdutydays = incdutydays- excdutydays
     C                   eval      cild17 = totdutydays
     C
     C*
     C** Disability date
     C*
      ***  Beg Add ***** 06/30/10  ************************************
     C                   movel     *zeros        cidbdt
      ***  End Add ***** 06/30/10  ************************************
     C                   eval      srdti = cmdbdt
      ***** Beg Chg ***** 08/26/14 *************************************************
     C**********         call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        srerr = *off
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel(p)  srdto         cidbdt                         ACCDT DATE
     C                   If        srdto <> *blanks
     C                   Eval      cidbdt = %subst(srdto :1:4) +'-'+
     C                                      %subst(srdto :5:2) +'-'+
     C                                      %subst(srdto :7:2)
     C                   Endif
      ***  End Chg ***** 06/30/10  ************************************
     C                   else
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         movel     *blanks       cidbdt
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif
     C*
     C** Return to work date
     C*
      ***  Beg Add ***** 06/30/10  ************************************
     C                   movel     *zeros        cirtw
      ***  End Add ***** 06/30/10  ************************************
     C                   eval      srdti = cmrtw
      ***** Beg Chg ***** 08/26/14 *************************************************
     C**********         call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        srerr = *off
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel(p)  srdto         cirtw
     C                   If        srdto <> *blanks
     C                   Eval      cirtw  = %subst(srdto :1:4) +'-'+
     C                                      %subst(srdto :5:2) +'-'+
     C                                      %subst(srdto :7:2)
     C                   Endif
      ***  End Chg ***** 06/30/10  ************************************
     C                   else
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         movel     *blanks       cirtw
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif
     C*
     c** SAI Claim number
     C*
     c                   eval      cisclm = %editc(cmfnd:'X') +
     c                             %editc(cmfyr:'X') + %editc(cmcase:'X')
     C*
     C                   eval      ciacst = cmjrst
     C                   eval      ciocds = cmocds
     C                   eval      cijmid = cmrecl
      ***  Beg Add ***** 06/30/10  ************************************
     C                   If        cijmid = *blanks
     C                   movel     *zeros        cijmid
     C                   Endif
      ***  End Add ***** 06/30/10  ************************************
     C*
    C*--------------------------------------------------------------------
    C*   Populate Claimant data
    C*--------------------------------------------------------------------
     C     cmss#         chain     wmcdl

     C                   if        %FOUND(WMCDl)

     C                   eval      CISEX = CDSEX
     C                   movel     CDSS#         CISS#
     C                   movel     CDFSN         CIFSN
     C                   movel     CDLSN         CILSN
     C*
     C***Claimant date of hire
     C*
      ***  Beg Add ***** 06/30/10  ************************************
     C                   movel     *zeros        cihidt
      ***  End Add ***** 06/30/10  ************************************
     C                   eval      srdti = CMHIDT
      ***** Beg Chg ***** 08/26/14 *************************************************
     C**********         call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        srerr = *off
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel(p)  srdto         cihidt
     C                   If        srdto <> *blanks
     C                   Eval      cihidt = %subst(srdto :1:4) +'-'+
     C                                      %subst(srdto :5:2) +'-'+
     C                                      %subst(srdto :7:2)
     C                   Endif
      ***  End Chg ***** 06/30/10  ************************************
     c                   else
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         movel     *blanks       cihidt
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif
     C*
     C***Claimant date of birth
     C*
      ***  Beg Add ***** 06/30/10  ************************************
     C                   movel     *zeros        cidtob
      ***  End Add ***** 06/30/10  ************************************
     C                   eval      srdti = cddtob
      ***** Beg Chg ***** 08/26/14 *************************************************
     C*********          call      'SRCVTDAT'    dtyymd
     C                   call      'SRCVTDAT'    dtyymd0
      ***** End Chg ***** 08/26/14 *************************************************
     C                   if        srerr = *off
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel(p)  srdto         cidtob
     C                   If        srdto <> *blanks
     C                   Eval      cidtob = %subst(srdto :1:4) +'-'+
     C                                      %subst(srdto :5:2) +'-'+
     C                                      %subst(srdto :7:2)
     C                   Endif
      ***  End Chg ***** 06/30/10  ************************************
     c                   else
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         movel     *blanks       cidtob
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif

     C                   endif

     C*
     C****Run date - Evaluation Date
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd1
      ***  Beg Chg ***** 06/30/10  ************************************
     C***********        movel     srdto         cirdt
     C                   Movel     enddate       cirdt
      ***  End Chg ***** 06/30/10  ************************************
     C                   endsr
     C*
    C*====================================================================
    C*  SrReserve  Load Reserve Info
    C*====================================================================
     C     SrReserve     begsr

     C                   eval      xxsumrr = *zeros
     C                   eval      xxsumar = *zeros
     C                   eval      xxtoti = *zeros
     C                   eval      xxtotr = *zeros
     C                   eval      xxsumlr = *zeros
     C                   eval      xxsumlp = *zeros

     C                   eval      cittrr = *zeros
     C                   eval      cittar = *zeros
     C                   eval      cittti = *zeros
     C***Beg Chg 04/21/11****************************************
     C***                eval      cittrc = *zeros
     C                   EvalR     cittrc = %Editw(cittrc#:amount_edtwrd)
     C***End Chg 04/21/11****************************************
     C                   eval      cittlr = *zeros
     C                   eval      cittlp = *zeros

     C                   z-add     0             i                 2 0

     C                   for       i = 1 to 10

     C                   if        rt(i) <> 'LE'

     C                   eval      xxsumrr = xxsumrr + rr(i)
     C                   eval      xxsumar = xxsumar + ar(i)

     C                   elseif    rt(i) =  'LE'

     C                   eval      xxsumlr = xxsumlr + rr(i)
     C                   eval      xxsumlp = xxsumlp + ar(i)
     C                   endif

     C                   endfor

      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      cittrr = xxsumrr
     C**********         eval      cittar = xxsumar
     C**********         eval      cittlr = xxsumlr
     C**********         eval      cittlp = xxsumlp
     C**********         eval      cittti = cittrr + cittar
      ****
     C                   eval      cittrr#= xxsumrr
     C                   eval      cittar#= xxsumar
     C                   eval      cittlr#= xxsumlr
     C                   eval      cittlp#= xxsumlp
     C                   eval      cittti#= cittrr# + cittar#
      ****
     C                   EvalR     cittrr = %Editw(xxsumrr:amount_edtwrd)
     C                   EvalR     cittar = %Editw(xxsumar:amount_edtwrd)
     C                   EvalR     cittlr = %Editw(xxsumlr:amount_edtwrd)
     C                   EvalR     cittlp = %Editw(xxsumlp:amount_edtwrd)
     C                   EvalR     cittti = %Editw(cittti#:amount_edtwrd)
      ***  End Chg ***** 06/30/10  ************************************

     c*****
     c*****Total
     c*****
     C                   if        (cifnd= '630' and  cityp2 = 'MED')
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpmr  = cittrr + tmpmr
     C**********         eval      tmpmp  = cittar + tmpmp
     C**********         eval      tmpmlr = cittlr + tmpmlr
     C**********         eval      tmpmlp = cittlp + tmpmlp
     C**********         eval      tmpmi  = cittti + tmpmi
     C                   eval      tmpmr  = cittrr# + tmpmr
     C                   eval      tmpmp  = cittar# + tmpmp
     C                   eval      tmpmlr = cittlr# + tmpmlr
     C                   eval      tmpmlp = cittlp# + tmpmlp
     C                   eval      tmpmi  = cittti# + tmpmi
      ***  End Chg ***** 06/30/10  ************************************
     C                   eval      tmpcnt = tmpcnt +1
     C*
     C                   elseif    (cifnd= '630' and  cityp2 = 'IND')
     C*
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpmr  = cittrr + tmpmr
     C**********         eval      tmpmp  = cittar + tmpmp
     C**********         eval      tmpmlr = cittlr + tmpmlr
     C**********         eval      tmpmlp = cittlp + tmpmlp
     C**********         eval      tmpmi  = cittti + tmpmi
     C                   eval      tmpir  = cittrr# + tmpir
     C                   eval      tmpip  = cittar# + tmpip
     C                   eval      tmpilr = cittlr# + tmpilr
     C                   eval      tmpilp = cittlp# + tmpilp
     C                   eval      tmpii  = cittti# + tmpii
      ***  End Chg ***** 06/30/10  ************************************
     C                   eval      tmpicnt = tmpicnt +1
     C                   endif

     C                   if        (cifnd= '640' and  cityp2 = 'MED')
     C*
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpmr   = cittrr + tmpmr
     C**********         eval      tmpmp   = cittar + tmpmp
     C**********         eval      tmpmlr  = cittlr + tmpmlr
     C**********         eval      tmpmlp  = cittlp + tmpmlp
     C**********         eval      tmpmi   = cittti + tmpmi
     C                   eval      tmpfmr  = cittrr# + tmpfmr
     C                   eval      tmpfmp  = cittar# + tmpfmp
     C                   eval      tmpfmlr = cittlr# + tmpfmlr
     C                   eval      tmpfmlp = cittlp# + tmpfmlp
     C                   eval      tmpfmi  = cittti# + tmpfmi
      ***  End Chg ***** 06/30/10  ************************************
     C                   eval      tmpfcnt = tmpfcnt + 1
     C*
     C                   elseif    (cifnd= '640' and  cityp2 = 'IND')
     C*
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpmr   = cittrr + tmpmr
     C**********         eval      tmpmp   = cittar + tmpmp
     C**********         eval      tmpmlr  = cittlr + tmpmlr
     C**********         eval      tmpmlp  = cittlp + tmpmlp
     C**********         eval      tmpmi   = cittti + tmpmi
     C                   eval      tmpfir  = cittrr# + tmpfir
     C                   eval      tmpfip  = cittar# + tmpfip
     C                   eval      tmpfilr = cittlr# + tmpfilr
     C                   eval      tmpfilp = cittlp# + tmpfilp
     C                   eval      tmpfii  = cittti# + tmpfii
      ***  End Chg ***** 06/30/10  ************************************
     C                   eval      tmpficnt = tmpficnt +1
     C                   endif

     C     wdky1         setll     ppa02
     C     wdky1         reade     ppa02
     C*
     C                   if        PATYPE = 'R'
     C*                  eval      xxamt   = ppamt
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         move      xxtotr        cittrc
     C                   move      xxtotr        cittrc#
     C                   EvalR     cittrc = %Editw(xxtotr:amount_edtwrd)
      ***  End Chg ***** 06/30/10  ************************************

     C*
     C****Total Recoveries (Fnd=640 AND 630)
     C*
     C                   if        (cifnd= '630' and  cityp2 = 'MED')
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpmrc = cittrc  + tmpmrc
     C                   eval      tmpmrc = cittrc# + tmpmrc
      ***  End Chg ***** 06/30/10  ************************************
     C*
     C                   elseif    (cifnd= '630' and  cityp2 = 'IND')
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpirc = cittrc  + tmpirc
     C                   eval      tmpirc = cittrc# + tmpirc
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif
     C
     C                   if        (cifnd= '640' and  cityp2 = 'MED')
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpfmrc = cittrc  + tmpfmrc
     C                   eval      tmpfmrc = cittrc# + tmpfmrc
      ***  End Chg ***** 06/30/10  ************************************
     C*
     C                   elseif    (cifnd= '640' and  cityp2 = 'IND')
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      tmpfirc = cittrc  + tmpfirc
     C                   eval      tmpfirc = cittrc# + tmpfirc
      ***  End Chg ***** 06/30/10  ************************************
     C                   endif
     C                   endif
     c                   endsr

    c*====================================================================
    c*  wrdata1- write data in report for fnd 630 Claim type= (MED&IND)
    c*====================================================================
     C     wrtdta1       begsr
     C*
     C                   Move      tmpcnt        xxcnt
     C                   Move      tmpmr         xxpmr
     C                   Move      tmpmp         xxpmp
     C                   Move      tmpmi         xxpmi
     C                   Move      tmpmrc        xxpmrc
     C                   Move      tmpmlr        xxpmlr
     C                   Move      tmpmlp        xxpmlp
     C
     C                   Move      tmpicnt       xxicnt
     C                   Move      tmpir         xxpir
     C                   Move      tmpip         xxpip
     C                   Move      tmpii         xxpii
     C                   Move      tmpirc        xxpirc
     C                   Move      tmpilr        xxpilr
     C                   Move      tmpilp        xxpilp
     C                   Move      '630'         tmpfnd            3
     C
     c                   eval      totlre = tmpmr + tmpir
     c                   eval      totlpd = tmpmp + tmpip
     c                   eval      totlin = tmpmi + tmpii
     c                   eval      totlrec =tmpmrc  + tmpirc
     c                   eval      totllr = tmpmlr +  tmpilr
     c                   eval      totllp = tmpmlp +  tmpilp
     c                   eval      totlcnt = tmpcnt + tmpicnt
     c*
     c                   endsr
    c*====================================================================
    c*  wrdata2- write data in report for fnd=640 Claim type= (MED&IND)
    c*====================================================================
     C     wrtdta2       begsr
     c*
     C                   Move      tmpfcnt       xxcnt
     C                   Move      tmpfmr        xxpmr
     C                   Move      tmpfmp        xxpmp
     C                   Move      tmpfmi        xxpmi
     C                   Move      tmpfmrc       xxpmrc
     C                   Move      tmpfmlr       xxpmlr
     C                   Move      tmpfmlp       xxpmlp
     C
     C                   Move      tmpficnt      xxicnt
     C                   Move      tmpfir        xxpir
     C                   Move      tmpfip        xxpip
     C                   Move      tmpfii        xxpii
     C                   Move      tmpfirc       xxpirc
     C                   Move      tmpfilr       xxpilr
     C                   Move      tmpfilp       xxpilp
     c*
     C                   Move      '640'         tmpfnd
     c                   eval      totlre = tmpfmr + tmpfir
     c                   eval      totlpd = tmpfmp + tmpfip
     c                   eval      totlin = tmpfmi + tmpfii
     c                   eval      totlrec =tmpfmrc  + tmpfirc
     c                   eval      totllr = tmpfmlr +  tmpfilr
     c                   eval      totllp = tmpfmlp +  tmpfilp
     c                   eval      totlcnt = tmpfcnt + tmpficnt
     c*
     c                   endsr
    c*====================================================================
    c*  SrOther- LOAD OTHER  INFO
    c*====================================================================
     C     SrOther       begsr

     C** Cause code 2 of accident
     C                   eval      cicau2= *blanks

     C** Cause code 3 of accident
     C                   eval      cicau3 = *blanks

     C** Site code1
     C                   eval      cisit1 = *blanks
     C** Site code1 description
     C                   eval      cis1ds = *blanks

     C** Benefit code
     C                   eval      cibenc = *blanks

     C** Benefit description
     C                   eval      cibend = *blanks
      *********
     C                   eval      cild15 = *zeros
     C                   eval      cild25 = *zeros
     C                   eval      cild27 = *zeros
     C                   eval      cild35 = *zeros
     C                   eval      cild37 = *zeros
     C                   eval      cild45 = *zeros
     C                   eval      cild47 = *zeros
     C                   eval      cild55 = *zeros
     C                   eval      cild57 = *zeros
     C                   eval      cild65 = *zeros
     C                   eval      cild67 = *zeros
      ***  Beg Chg ***** 06/30/10  ************************************
     C**********         eval      ciowd2 = *blanks
     C**********         eval      cirwd2 = *blanks
     C**********         eval      ciowd3 = *blanks
     C**********         eval      cirwd3 = *blanks
     C**********         eval      ciowd4 = *blanks
     C**********         eval      cirwd4 = *blanks
     C**********         eval      ciowd5 = *blanks
     C**********         eval      cirwd5 = *blanks
     C**********         eval      ciowd6 = *blanks
     C**********         eval      cirwd6 = *blanks

     C                   eval      ciowd2 = *zeros
     C                   eval      cirwd2 = *zeros
     C                   eval      ciowd3 = *zeros
     C                   eval      cirwd3 = *zeros
     C                   eval      ciowd4 = *zeros
     C                   eval      cirwd4 = *zeros
     C                   eval      ciowd5 = *zeros
     C                   eval      cirwd5 = *zeros
     C                   eval      ciowd6 = *zeros
     C                   eval      cirwd6 = *zeros
      ***  End Chg ***** 06/30/10  ************************************
     C                   endsr
    C*================================================================
     O*** Qsysprt
     OQsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'Route to:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'Route to:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O*
     O          E            ROUTE          2
     O                                           19 'Report Title:'
     O                       x1titl              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUTE          2
     O**********                                 19 'Program Name:'
     O**********             ORPNAM              30
     O**********                                 36 'File:'
     O**********             ORFIL#              38
     O**********                                 46 'Copies:'
     O**********             ORCOPY        Z     50
     O**********
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'Output queue:'
     O**********             OROUTQ              30
     O**********                                 37 'Forms:'
     O**********             ORFORM              48
     O**********                                 56 'Save:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O*
     O          E            ROUTE          1 58
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
     O          E            route          1  1
     O                       dstime               9 '  :  :  '
     O                       dsdate             132 '  /  /    '
     O*
     O          E            header         1
     O                       q1pgm               11
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            Header         1
     O                                           60 'CCIC Monthly Reporting'
     O                                           73 ' File Summary'
     O*
     O          E            detail      1  2
     O                                            5 'Fund:'
     O                       tmpfnd            +002
     O
     O          E            detail         1
     O                                            5 'Claim'
     O                                         +003 'Transaction'
     O                                         +002 'Total Reserves'
     O                                         +003 'Total Paid'
     O                                         +007 'Total Incurred'
     O                                         +003 'Total Recoveries'
     O                                         +002 'Legal Reserves'
     O                                         +003 'Legal Paid'
     O          E            detail         1
     O                                            4 'Type'
     O                                         +004 'Count'
     O          E            detail         1
     O                                            3 'IND'
     O                       xxicnt            +005
     O                       xxpir             +008
     O                       xxpip             +006
     O                       xxpii             +006
     O                       xxpirc            +006
     O                       xxpilr            +007
     O                       xxpilp            +007
     o
     O          E            detail         1
     O                                            3 'MED'
     O                       xxcnt             +005
     O                       xxpmr             +008
     O                       xxpmp             +006
     O                       xxpmi             +006
     O                       xxpmrc            +006
     O                       xxpmlr            +007
     O                       xxpmlp            +007
     O*
     O          E            total          1
     O                                            6 'Totals'
     O                       totlcnt           +002
     O                       totlre            +008
     O                       totlpd            +006
     O                       totlin            +006
     O                       totlrec           +006
     O                       totllr            +007
     O                       totllp            +007
     O*
     O          E            Space#         1
     O*
     O          E            Skip#                1
     O*
     O          E            Fot1           1
     O                                           78 ' *** End  Of  Report *** '
     O*
