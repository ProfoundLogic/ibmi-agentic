    h*====================================================================
     hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
    h*====================================================================
    h/TITLE Advantage Export
     h OPTION(*NODEBUGIO: *SRCSTMT)
     h DEBUG(*YES)
    h*                                                                   *
    h*--------------------------------------------------------------------
    h*  CALLED FROM: W7086C1                                             *
    h*                                                                   *
    h*  CALLS TO:                                                        *
    h*                                                                   *
    h*  CREATED: 03/03/10      C: 190    PROJECT: 139WNP                 *
    h*--------------------------------------------------------------------
    h*  REVISIONS                                                        *
    h*                                                                   *
     h* 05/02/11 C:180 Seq# 76727
     h*                Fix error: Invalid Adv Pol #
     h*
     h* 09/01/10 C:190 Seq# 74081                                         *
    h*                Set all the amount fields to print with            *
    h*                2 decimal places.                                  *
    h*                                                                   *
    h*--------------------------------------------------------------------
     h*
    f*====================================================================
    f*  F I L E S
    f*====================================================================
     f*
     fwmcml     if   e           k disk
     fwmrpl     if   e           k disk
     fwmupl1    if   e           k disk
     fwmlll     if   e           k disk
     fwmchl     if   e           k disk
     fwdpal     if   e           k disk    rename(ppa01:ppa02)
     fwmwhl     if   e           k disk
     fwxcap     uf a e             disk    rename(pca01:pca02)
     fwmfnl     if   e           k disk
     fwmcdl     if   e           k disk
     fwmdpl     if   e           k disk
     fwxgsl     if   e           k disk
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
    d*====================================================================
    d*  DEFINITION SPECIFICATIONS
    d*====================================================================
     d*
     d psds          esds                  extname(szq1p)
     d*
     d xxbar1          s              2    dim(10) ctdata perrcd(10)
     d xxbar2          s              2    dim(10) ctdata perrcd(10)
     d xxbar3          s              2    dim(15) ctdata perrcd(15)
     d xxbar4          s              2    dim(10) ctdata perrcd(10)
     d xxbar5          s              2    dim(10) ctdata perrcd(10)
     d xxbar6          s              2    dim(35) ctdata perrcd(35)
     d xxbar7          s              2    dim(40) ctdata perrcd(40)
     d xxbar8          s              2    dim(10) ctdata perrcd(10)
     d xxbar9          s              2    dim(20) ctdata perrcd(20)
     d xxbar10         s              2    dim(25) ctdata perrcd(25)

     d                 ds
     d dsdttm                        14  0
     d dstime                         6  0 overlay(dsdttm)
     d dsdate                         8  0 overlay(dsdttm:7)

     d                 ds
     d xxphn                   1     10
     d upphna                  1      3  0
     d upphn#                  4     10  0

     d                 ds
     d xxactm                  1      4
     d xxhh                    1      2  0
     d xxmm                    3      4  0

     d                 ds
     d xxactm1                 1      5
     d xxhh1                   1      2
     d xxcl1                   3      3
     d xxmm1                   4      5

     c************* beg add ** 09/01/10 ************************************
     d                 ds
     d xxamt1                  1     10
     d x1amt                   1      7
     d x2amt                   8      8
     d x3amt                   9     10

     d                 ds
     d xxamt1w                 1      8
     d x1amtw                  1      5
     d x2amtw                  6      6
     d x3amtw                  7      8
     c************* end add ** 09/01/10 ************************************

     d                 ds                  inz
     d rr                      1     90  2
     d                                     dim(10)                              RR REMAIN RSV
     d cmrr01                  1      9  2
     d cmrr02                 10     18  2
     d cmrr03                 19     27  2
     d cmrr04                 28     36  2
     d cmrr05                 37     45  2
     d cmrr06                 46     54  2
     d cmrr07                 55     63  2
     d cmrr08                 64     72  2
     d cmrr09                 73     81  2
     d cmrr10                 82     90  2

     d                 ds                  inz
     d rt                      1     20
     d                                     dim(10)                              RT RSV TYPES
     d cmrt01                  1      2
     d cmrt02                  3      4
     d cmrt03                  5      6
     d cmrt04                  7      8
     d cmrt05                  9     10
     d cmrt06                 11     12
     d cmrt07                 13     14
     d cmrt08                 15     16
     d cmrt09                 17     18
     d cmrt10                 19     20

     d @co#            s              3  0
     d @fnd            s              3  0
     d x               s              3  0
     d kkefdt          s                   like(chefdt)
     d xxwkdt          s                   like(whdate)
     d xxcafn          s                   like(upusnm)
     d policy#         s             17
     d xxcomr          s             15  2
     d xxmedr          s             15  2
     d xxrehr          s             15  2
     d xxalar          s             15  2
     d xxtors          s             15  2
     d xxmsam          s              9  2
     d xxmsa1          s              9  2
     d xxmsa2          s              9  2
     d xxmsa3          s              9  2
     d xxmsa4          s              9  2
     d xxmsa5          s              9  2
     d xxmsa6          s              9  2
     d xxmsa7          s              9  2
     d xxmsa8          s              9  2
     d xxmsa9          s              9  2
     d xxmsa10         s              9  2
     d xxmsa11         s              9  2
     d xxmsa12         s              9  2
     d xxmsa13         s              9  2
     d xxmsa14         s              9  2
     d xxmsa15         s              9  2
     d xxmsa16         s              9  2
     d xxmsa17         s              9  2
     d xxmsa18         s              9  2
     d xxmsa19         s              9  2
     d xxmsa20         s              9  2
     d xxmsa21         s              9  2
     d xxmsa22         s              9  2
     d xxmsa23         s              9  2
     d xxmsa24         s              9  2
     d xxmsa25         s              9  2
     d xxmsa26         s              9  2
     d xxmsa27         s              9  2
     d xxmsa28         s              9  2
     d xxmsa29         s              9  2
     d xxmsa30         s              9  2
     dxxtptd           s              9  2
     dxxrecp           s              9  2
     d xxamt           s             15
     d x#amt           s             15
     d xxsgn           s              1
     d xxawamt         s              7  2
     d*
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d*
    d*--------------------------------------------------------------------
    d*  Copybooks for Prototypes                                         *
    d*--------------------------------------------------------------------
     d*
     d/copy sprdate
     C*
    C*--------------------------------------------------------------------
    C*  Parameter Lists                                                  *
    C*--------------------------------------------------------------------
     C*
    C*--------------------------------------------------------------------
    C*  Key Lists                                                        *
    C*--------------------------------------------------------------------
     C*
     C     gsky          klist
     C                   kfld                    gsco#
     C                   kfld                    gsfnd

     C     cmlky         klist
     C                   kfld                    @co#
     C                   kfld                    @fnd
     C*
     C     rpky          klist
     C                   kfld                    @co#
     C                   kfld                    @fnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr

     C     emlky         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C*
     C     ELLKY         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     C*
     C*
     C*   Table file
     C     tbky          klist
     C                   kfld                    cmco#
     C                   kfld                    kkkey            10

     C     upkey         klist
     C                   kfld                    kkey

     C     dpky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmdept

     C     llky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
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
     C     paky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     c*
     C     whky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    xxwkdt

     C     whky2         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
    C*--------------------------------------------------------------------
    C*  PLIST                                                            *
    C*--------------------------------------------------------------------
     C*
     C     dtyymd        plist
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
    C*====================================================================
     C*  MAIN LINE LOGIC
    C*====================================================================
     C*
     C                   eval      @co# = *zeros
     C                   eval      @fnd = *zeros
     C*
     C*  Clear output files
     C                   exsr      clrout                                       Clear Output
     C*
     C*  Read Fund Work File
     C                   Read      wxgsl
     C                   Dow       not %eof(wxgsl)
     C                   move      gsco#         @co#
     C*
     C*  Check for Included Group
     C                   if        gsiefl = 'I'

     C*  And Skip Test Group
     C     gsky          chain     pfn01                              60
     C                   if        fntest <> 'Y'

     C                   z-add     gsfnd         @fnd
     C*
     C                   exsr      SrPrcFnd

     C                   endif                                                  fntest <> 'Y'
     C                   endif                                                  gsfnd  = 'I'
     C*
     C                   Read      wxgsl

     C                   enddo                                                  not %eof(wxgsl)
     C*
     C                   eval      *inlr = *on

    C*====================================================================
    C*   Process Fund
    C*====================================================================
     C     SrPrcFnd      begsr

     C     cmlky         setll     wmcml
     C     cmlky         reade     wmcml
     C*
     C                   dow       not %eof(wmcml)
     C*
     c** " Record only claims will be excluded.
     c                   if        cmltfl <> 'R'

     c** TPA assigned claim number
     c                   eval      policy# = %editc(cmfnd:'X') +
     c                             %editc(cmfyr:'X') + %editc(cmcase:'X')
     c*
     c                   time                    dsdttm

     C                   exsr      SrBldRec                                     Claim/Claimant File
     C                   exsr      SrReserve                                    Reserve info
     C                   exsr      SrPaidToDt                                   Paid to date info
     C                   exsr      SrOther                                      Other info
     C*
     C                   write     pca02

     C                   endif                                                  cmltfl <> 'R'

     C     cmlky         reade     wmcml
     C*
     C                   enddo                                                  WMCML NOT EOF

     c                   endsr                                                  SrPrcFnd
     C*
    C*====================================================================
    C*====================================================================
     C     SrBldRec      begsr
     C*
     C                   eval      catpac = policy#                             TPA CLM NUM
     C*
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   if        srerr = *off
     C                   movel(p)  srdto         cadoi                          ACCDT DATE
     C                   else
     C                   movel     *blanks       cadoi
     C                   endif
     C*
     C*
     C                   eval      caclst = cmstat                              CLAIM STATUS
     C                   eval      canoi  = cmnatr                              NCCI NATURE INJ CDE
     C                   eval      caonoc = cmnatr                              EXT NATURE INJ CDE

     c                   movel     *blanks       kkkey
     c                   movel(p)  cmnatr        kkkey
     c     tbky          chain     pna01                              99
     c                   if        (not *in99)
     c                   movel(p)  nadesc        canoid                         NCCI NATURE INJ DESC
     c                   movel(p)  nadesc        caonod                         EXT NATURE INJ DESC
     c                   endif

     C                   eval      caaccd = cmcaus                              NCCI ACCDT CDE
     C                   eval      caoacc = cmcaus                              EXT ACCDT CDE

     c                   movel     *blanks       kkkey
     C                   movel(p)  cmcaus        kkkey
     c     tbky          chain     pca01                              99
     c                   if        (not *in99)
     c                   movel(p)  cadesc        caacds                         ACCDT DESC
     c                   movel(p)  cadesc        caoacd                         EXT ACCDT DESC
     c                   endif

     C                   eval      capart = cmpart                              NCCI BODY PART CDE
     C                   eval      caopbc = cmpart                              EXT BODY PART CDE

     c                   movel     *blanks       kkkey
     c                   movel(p)  cmpart        kkkey
     c     tbky          chain     ppa01                              99
     c                   if        (not *in99)
     c                   movel(p)  padesc        capard                         BODY PART DESC
     c                   movel(p)  padesc        caopbd                         EXT BODY PART DESC
     c                   endif

     C                   eval      caage  = *blanks
     C*
     C                   move      cmclas        caclsc                         CLM CLASS CDE
     C                   eval      caclsd = *blanks                             CLM CLASS DES

     C                   movel(p)  cmactm        xxactm
     C                   movel     xxhh          xxhh1
     c                   movel     ':'           xxcl1
     C                   movel     xxmm          xxmm1
     C                   movel(p)  xxactm1       catoi                          ACCDT TIME
     C*
     c** Claim Category
     c                   if        cmdny = 'Y'
     c                   eval      cacat = 'D'
     c                   else
     c                   eval      cacat = *blanks
     C                   endif
     C*
     c     llky          chain     pll01                              99
     C                   movel     *blanks       castld
     C                   movel     *blanks       castlm

     C                   if        *in99 = *off
     c                   if        llstdt > *zeros
     c                   eval      cacat = *blanks
     c                   eval      cacat = 'S'
     C                   endif
     C                   movel     llmtst        castlm                         CLM SETTELMENT CDE
     C                   eval      srdti = llstdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   if        srerr = *off
     C                   movel(p)  srdto         castld                         CLM SETTLED DATE
     c                   else
     C                   movel     *blanks       castld
     C                   endif                                                  *IN99 = *OFF
     C                   endif                                                  LLKY CHAIN


     c
     C* Claim Closed date
     C*
     C                   eval      kkefdt = dtcymd(dsdate)
     C*
     C                   if        CMSTAT = 'C'
     C     chky1         setgt     pch01
     C     chky2         readpe    pch01
     C                   eval      SRDTI = CHEFDT
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        SRERR = *OFF
     C                   movel(p)  srdto         CACLOS                         CLM CLOSE DATE
     C                   endif
     c                   else
     C                   movel     *blanks       CACLOS
     C                   endif                                                  CMSTAT = 'C'
     C*
     C                   EVAL      SRDTI = KKEFDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   if        SRERR = *OFF
     C                   movel(p)  SRDTO         CAVALD                         CLM VALUED DATE
     c                   else
     C                   movel     *blanks       CAVALD
     C                   endif

     C                   eval      SRDTI = CMENTD
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        SRERR = *OFF
     C                   movel(p)  SRDTO         CAIDAT                         DATE CLM CREATED
     C                   else
     C                   movel(p)  *blanks       CAIDAT
     C                   endif
     C*
     C                   eval      SRDTI = CMDNDT
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        SRERR = *OFF
     C                   movel(p)  SRDTO         CADEND                         CLM DENIAL DATE
     c                   else
     c                   move      *blanks       CADEND
     C                   endif
     C*
     C                   if        CMHIDT <> *ZERO
     C                   eval      SRDTI = CMHIDT
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        SRERR = *OFF
     C                   movel(p)  SRDTO         CAEMDT                         CLMNT EMPL DATE
     C                   endif
     c                   else
     C                   movel     *blanks       CAEMDT
     C                   endif
     C*
     C                   if        CMDOD <> *ZERO
     C                   eval      SRDTI = CMDOD
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        SRERR = *OFF
     C                   movel(p)  SRDTO         CADEAD                         CLMNT DEATH DATE
     C                   endif
     c                   else
     C                   movel     *blanks       CADEAD
     C                   endif
     C*
     C                   movel     *blanks       kkey
     C                   movel(p)  cmclex        kkey              3
     c                   eval      cacafn = *blanks
     c                   eval      cacaln = *blanks
     c                   eval      xxcafn = *blanks
     c                   eval      cacaph = *blanks
     C     kkey          chain     pup01                              99
     c                   if        (not *in99)
     C                   eval      xxcafn = upusnm
     c                   eval      cacaph = xxphn                               CLM_ADJ_PHONE

      ** Look for the space in the name.
     c                   eval      x = %scan(' ' : xxcafn)

      ** Parse the first and last names.
     C                   if        x <> 0
     C                   eval      cacafn = %trim(%subst(xxcafn : 1 : x-1))     CLM_ADJ_FNAME
     C                   eval      cacaln = %trim(%subst(xxcafn : x+1))         CLM_ADJ_LNAME
     C                   endif
     C                   endif

     C                   eval      caacst = cmast                               ACCDT STATE
     C                   eval      cainjc = cmncci                              NCCI INJ CODE
     C                   eval      caoccp = cmocds                              CLMNT OCCUPATION
     C                   eval      cadepc = cmdept                              CLM DEPT CODE

     C     dpky          chain     pdp01                              99
     c                   if        (not *in99)
     C                   eval      CADEPD = DPDESC                              CLM DEPT DESC
     C                   endif

     C** Claimant Return to Work date

     C                   eval      xxwkdt = *hival
     C                   movel(p)  *blanks       cartwd
     c     whky          setgt     pwh01
     c     whky2         readpe    pwh01                                  60
     c                   dow       *in60 = *off
     c                   if        whwkst = 'R'
     C                   if        whdate <> *zeros
     C                   EVAL      srdti = whdate
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        srerr = *off
     C                   movel(p)  srdto         cartwd                         CLMNT EMPL DATE
     C                   endif
     c                   leave
     C                   endif
     C                   endif
     c     whky2         readpe    pwh01                                  60
     c                   enddo

     c                   if        cartwd = *blanks
     C                   eval      srdti = cmrtw
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        srerr = *off
     C                   movel(p)  srdto         cartwd
     c                   else
     C                   movel(p)  *blanks       cartwd
     C                   endif
     C                   endif

     C                   eval      CAJRST = CMJRST                              JURISDICTION STATE
     C                   eval      CATPAT = CMLTFL                              TPA CLM TYPE
     C                   eval      CANCRC = CMLSRV                              NCCI REC TYP CDE
     C                   eval      CANCTY = CMCOVT                              NCCI TYP OF CLM
     C                   eval      CADEDA = *blanks                             DEDUCTIBLE APPLIED

     c                   move      *zeros        caaww
     c************* beg chg ** 09/01/10 ************************************
     c                   eval      xxawamt = cmaww
     c                   exsr      srvalue1
     C                   eval      caaww  = *zeros
     c                   movel     xxaww         caaww                          AVG WEEKLY WAGE
     c*********          move      cmaww         caaww                          AVG WEEKLY WAGE
     c************* end chg ** 09/01/10 ************************************

     C                   eval      caotrc = *blanks                             ALL OTH REC TYPE
     C                   eval      camuls = *blanks                             MULT EMP LOSS
     C*
    C*--------------------------------------------------------------------
    C*   Populate Claimant data
    C*--------------------------------------------------------------------
     C     cmss#         chain     wmcdl

     C                   if        %FOUND(WMCDL)

     C                   eval      CASEX = CDSEX                                ClMNT SEX

     C                   eval      CAMST = *blanks
     C                   SELECT                                                 MARITAL STATUS
     c                   when      cdmsts = 'K'                                 UNKNOWN
     C                   eval      CAMST = *blanks
     c                   when      cdmsts = 'U'                                 UNMARRIED
     C                   eval      CAMST = 'S'
     c                   when      cdmsts = 'M'                                 MARRIED
     C                   eval      CAMST = 'M'
     c                   when      cdmsts = 'S'                                 SINGLE
     C                   eval      CAMST = 'S'
     c                   when      cdmsts = ' '
     C                   eval      CAMST = *BLANKS
     c                   endsl
     c
     C                   eval      CASSN = CMSS#                                CLMNT SSN
     C                   eval      CAFNAM = CDFSN                               CLMNT FST NAME
     C                   eval      CAMNAM = CDMID                               CLMNT MID NAME
     C                   eval      CALNAM = CDLSN                               CLMNT LST NAME
     C                   eval      CAADR1 = CDADR1                              CLMNT ADDR 1
     C                   eval      CAADR2 = CDADR2                              CLMNT ADDR 2

     C                   eval      XXCSZ = *blanks
     C                   eval      CAADR3 = *blanks
     C                   if        (CDCTRY <> *BLANKS)
     C                             AND (CDCTRY <> 'USA')
     C                   movel(p)  CDADR3        XXCSZ            30
     C                   move      CDCTRY        XXCSZ
     C                   eval      CAADR3 = XXCSZ                               CLMNT ADDR 3
     C                   endif

     C                   eval      cacity = cdcty                               CLMNT CITY
     C                   eval      cast   = cdst                                CLMNT STATE
     C                   eval      cazip  = %editc(cdzip:'X')                   CLMNT ZIP
     C                   eval      caphon =%editc(cdarea:'X')+%editc(cdphn#:'X')CLMNT PHN
     C*
     C                   eval      srdti = cddtob
     C                   call      'SRCVTDAT'    DTYYMD
     C                   if        srerr = *off
     C                   movel(p)  srdto         cadob                          CLMNT DOB
     c                   else
     C                   movel     *blanks       cadob
     C                   endif

     C                   endif                                                  %FOUND(WMCDL)

     C                   endsr                                                  SrBldRec
     C*
    C*====================================================================
    C*  SrReserve  Load Reserve Info
    C*====================================================================
     C     SrReserve     begsr

     C                   eval      xxcomr = *zeros
     C                   eval      xxmedr = *zeros
     C                   eval      xxrehr = *zeros
     C                   eval      xxalar = *zeros
     C                   eval      xxtors = *zeros

     C                   eval      cacors = *zeros
     C                   eval      camers = *zeros
     C                   eval      carers = *zeros
     C                   eval      caalrs = *zeros
     C                   eval      cators = *zeros

     C                   z-add     0             i                 2 0

     C                   for       i = 1 to 10

     C                   select
     C                   when      rt(i) = '12'                                 COMP_RESERVE
     C                   eval      xxcomr = rr(i) + xxcomr
     c************* beg dlt ** 09/01/10 ************************************
     C*********          eval      cacors = *zeros
     C*********          move      xxcomr        cacors
     c************* end dlt ** 09/01/10 ************************************

     C                   when      rt(i) = '11'                                 MED_RESERVE
     C                   eval      xxmedr = rr(i) + xxmedr
     c************* beg dlt ** 09/01/10 ************************************
     C*********          eval      camers = *zeros
     C*********          move      xxmedr        camers
     c************* end dlt ** 09/01/10 ************************************

     C                   when      rt(i) = '14'                                 REHAB_RESERVE
     C                   eval      xxrehr = rr(i) + xxrehr
     c************* beg dlt ** 09/01/10 ************************************
     C*********          eval      carers = *zeros
     C*********          move      xxrehr        carers
     c************* end dlt ** 09/01/10 ************************************

     C                   when      rt(i) = '13' or                              ALAE_RESERVE
     C                             rt(i) = '16'
     C                   eval      xxalar = rr(i) + xxalar
     c************* beg dlt ** 09/01/10 ************************************
     C*********          eval      caalrs = *zeros
     C*********          move      xxalar        caalrs
     c************* end dlt ** 09/01/10 ************************************

     C                   endsl

     c                   endfor

     c************* beg add ** 09/01/10 ************************************
     c                   eval      xxmsam = xxcomr
     c                   exsr      srvalue
     C                   eval      cacors = *zeros
     c                   movel     xxamt         cacors                         COMP_RESERVE
      *
     c                   eval      xxmsam = xxmedr
     c                   exsr      srvalue
     C                   eval      camers = *zeros
     c                   movel     xxamt         camers                         MED_RESERVE
      *
     c                   eval      xxmsam = xxrehr
     c                   exsr      srvalue
     C                   eval      carers = *zeros
     c                   movel     xxamt         carers                         REHAB_RESERVE
      *
     c                   eval      xxmsam = xxalar
     c                   exsr      srvalue
     C                   eval      caalrs = *zeros
     c                   movel     xxamt         caalrs                         ALAE_RESERVE
     c************* end add ** 09/01/10 ************************************

     c                   eval      xxtors = xxcomr + xxmedr + xxrehr + xxalar
     c************* beg chg ** 09/01/10 ************************************
     c                   eval      xxmsam = xxtors
     c                   exsr      srvalue
     C                   eval      cators = *zeros
     c                   movel     xxamt         cators                         Total reserves
     c*********          move      xxtors        cators                         Total reserves
     c************* end chg ** 09/01/10 ************************************

     c                   endsr                                                  SrReserve

    c*====================================================================
    c*  SrOther- LOAD OTHER  INFO
    c*====================================================================
     C     SrOther       begsr

     C** Internal (Advantage) assigned claim number
     C                   eval      cacl# = *blanks                              Claim Nbr

     C** TPA system's internal claimant identification number
     C                   eval      caclid = *blanks                             Clmnt ID Nbr

     C** ADVANTAGE Policy Number

      ** Begin Add ***** 05/02/11 ******************************************
     c                   move      *blanks       capol#
      ** End   Add ***** 05/02/11 ******************************************

     C     rpky          chain     prp01                              99
     C                   if        *in99 = *off
     C                   movel(p)  rpcap#        capol#
     C                   endif

     C** Advantage Policy Number Renewal
     C                   eval      cap#rn = *blanks

     C** Three letter code indicating program administrator
     C                   eval      camisp = *blanks

     C** Policy System Location Code
     C                   eval      caplcd = *blanks

     C** Policy System BA for location code
     C                   eval      caplbc = *blanks

     C** Policy System Location Name
     C                   eval      caplnm = *blanks

     C** Name of third party administrator
     C                   eval      catpan = 'PRM'

     C** Number of days Claim has been open
     C                   eval      caopnd = *blanks

     C** Date of last change in data for this record
     C                   eval      cadtch = *blanks

     c** Number of Claimant claims with this policy holder
     C                   eval      catclm = *blanks

     c** Total payments PTD + total reserves - total recoveries
     C                   eval      catoti = *blanks

     C                   endsr                                                  SrOther
     C
    C*====================================================================
    C*  PTDSR  - PAID TO DATE INFO.
    C*====================================================================
     C     SrPaidToDt    begsr

     C     paky          chain     ppa02                              99

     c                   eval      xxmsa1 = *zeros
     c                   eval      xxmsa2 = *zeros
     c                   eval      xxmsa3 = *zeros
     c                   eval      xxmsa4 = *zeros
     c                   eval      xxmsa5 = *zeros
     c                   eval      xxmsa6 = *zeros
     c                   eval      xxmsa7 = *zeros
     c                   eval      xxmsa8 = *zeros
     c                   eval      xxmsa9 = *zeros
     c                   eval      xxmsa10= *zeros
     c                   eval      xxmsa11= *zeros
     c                   eval      xxmsa12= *zeros
     c                   eval      xxmsa13= *zeros
     c                   eval      xxmsa14= *zeros
     c                   eval      xxmsa15= *zeros
     c                   eval      xxmsa16= *zeros
     c                   eval      xxmsa17= *zeros
     c                   eval      xxmsa18= *zeros
     c                   eval      xxmsa19= *zeros
     c                   eval      xxmsa20= *zeros
     c                   eval      xxmsa21= *zeros
     c                   eval      xxmsa22= *zeros
     c                   eval      xxmsa23= *zeros
     c                   eval      xxmsa24= *zeros
     c                   eval      xxmsa25= *zeros
     c                   eval      xxmsa26= *zeros
     c                   eval      xxmsa27= *zeros
     c                   eval      xxmsa28= *zeros
     c                   eval      xxmsa29= *zeros
     c                   eval      xxmsa30= *zeros
     c                   eval      xxrecp = *zeros
     c                   eval      xxtptd = *zeros

     c                   dow       *in99 = *off

     c                   Select

     c                   When      parecd = '11'

     c** Medical payments paid to date
     c     pabr2         lookup    xxbar7                                 60
     c                   if        *in60 = *on
     c                   exsr      Srpmnt
     c                   eval      xxmsa7 = xxmsa7 + paamt
     c                   endif                                                  *in60 = *on

     C** Vocational rehabilitation paid to date
     c                   if        pabr2 = 'RH' or
     c                             pabr2 = 'VR'
     c                   exsr      Srpmnt
     c                   eval      xxmsa8 = xxmsa8 + paamt
     c                   endif                                                   if pabr2

     c** Medical case management expenses
     c                   if        pabr2 = 'IC' or
     c                             pabr2 = 'IO' or
     c                             pabr2 = 'IT'
     c                   exsr      Srpmnt
     c                   eval      xxmsa11 = xxmsa11 + paamt
     c                   endif                                                  if pabr2

     c** Attorney & other legal expenses including court cost
     c                   if        pabr2 = 'SA'
     c                   exsr      Srpmnt
     c                   eval      xxmsa12 = xxmsa12 + paamt
     c                   endif                                                  if pabr2

     c** Vocational rehabilitation expenses (ALAE)
     c                   if        pabr2 = 'VR'
     c                   exsr      Srpmnt
     c                   eval      xxmsa13 = xxmsa13 + paamt
     c                   endif                                                  if pabr2

     c** Other allocated loss adjusting expenses
     c                   if        pabr2 = 'MP'
     c                   exsr      Srpmnt
     c                   eval      xxmsa14 = xxmsa14 + paamt
     c                   endif                                                  if pabr2

     c** Total ALAE payments paid to date
     c                   if        pabr2 = 'SA' or
     c                             pabr2 = 'MP'
     c                   exsr      Srpmnt
     C                   eval      xxmsa15 = xxmsa15 + paamt
     C                   endif                                                  if pabr2

     C** Medical recoveries
     C                   if        pabr2 = 'DQ' or
     C                             pabr2 = 'SR' or
     C                             pabr2 = 'FU' or
     C                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     C                   eval      xxmsa18 = xxmsa18 + paamt
     C                   endif                                                  if pabr2

     c** Deductible recoveries from policyholders per policy terms
     C                   if        pabr2 = 'DQ'
     c                   exsr      Srpmnt
     C                   eval      xxmsa21 = xxmsa21 + paamt
     C                   endif                                                  if pabr2

     C** Second Injury Fund Recovery
     C                   if        pabr2 = 'FU'  or
     C                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     C                   eval      xxmsa25 = xxmsa25 + paamt
     C                   endif                                                  if pabr2


     C                   when      parecd = '12'

     c** Temporary total disability compensation paid to date
     c     pabr2         lookup    xxbar1                                 60
     c                   if        *in60 = *on
     c                   exsr      Srpmnt
     c                   eval      xxmsa1 = xxmsa1 + paamt
     c                   endif                                                  *in60 = *on

     c** Temporary partial disability compensation paid to date
     c     pabr2         lookup    xxbar2                                 60
     c                   if        *in60 = *on
     c                   exsr      Srpmnt
     c                   eval      xxmsa2 = xxmsa2 + paamt
     c                   endif                                                  *in60 = *on

     c**Permanent partial disability compensation paid to date
     c     pabr2         lookup    xxbar3                                 60
     c                   if        *in60 = *on
     c                   exsr      Srpmnt
     c                   eval      xxmsa3 = xxmsa3 + paamt
     c                   endif                                                  *in60 = *on

     c** Permanent total disability compensation paid to date
     C     pabr2         lookup    xxbar4                                 60
     C                   if        *in60 = *on
     c                   exsr      Srpmnt
     c                   eval      xxmsa4 = xxmsa4 + paamt
     c                   endif                                                  *in60 = *on

     c** Fatality compensation paid to date
     C     pabr2         lookup    xxbar5                                 60
     C                   if        *in60 = *on
     c                   exsr      Srpmnt
     C                   eval      xxmsa5 = xxmsa5 + paamt
     C                   endif                                                  *in60 = *on

     C** Total compensation payments paid to date
     C     pabr2         lookup    xxbar6                                 60
     C                   if        *in60 = *on
     c                   exsr      Srpmnt
     C                   eval      xxmsa6 = xxmsa6 + paamt
     C                   endif                                                  *in60 = *on

     C** Vocational rehabilitation paid to date
     C                   if        pabr2 = 'RH'
     c                   exsr      Srpmnt
     C                   eval      xxmsa8 = xxmsa8 + paamt
     C                   endif                                                  if pabr2

     c** Compensation recoveries
     c                   if        pabr2 = 'CQ' or
     c                             pabr2 = 'DQ' or
     c                             pabr2 = 'SR' or
     c                             pabr2 = 'FU' or
     c                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     c                   eval      xxmsa17 = xxmsa17 + paamt
     c                   endif                                                  if pabr2

     c** Deductible recoveries from policy holders terms
     C                   if        pabr2 = 'CQ' or
     C                             pabr2 = 'DQ'
     c                   exsr      Srpmnt
     C                   eval      xxmsa21 = xxmsa21 + paamt
     C                   endif                                                  if pabr2

     C** Voc Rehab Compensation paid to date
     c                   if        pabr2 = 'VT'
     c                   exsr      Srpmnt
     c                   eval      xxmsa22 = xxmsa22 + paamt
     c                   endif                                                  if pabr2

     c* On the Job paid to date
     c                   if        pabr2 = 'TR'
     c                   exsr      Srpmnt
     c                   eval      xxmsa23 = xxmsa23 + paamt
     C                   endif                                                  if pabr2

     C** Loss of Earning Capacity paid to date
     C                   if        pabr2 = 'WL'
     c                   exsr      Srpmnt
     C                   eval      xxmsa24 = xxmsa24 + paamt
     C                   endif                                                  if pabr2

     C** Second Injury Fund Recovery
     C                   if        pabr2 = 'SD'  or
     C                             pabr2 = 'FU'
     c                   exsr      Srpmnt
     C                   eval      xxmsa25 = xxmsa25 + paamt
     C                   endif                                                  if pabr2

     c** Burial Expenses paid to date
     C                   if        pabr2 = 'FR'
     c                   exsr      Srpmnt
     C                   eval      xxmsa26 = xxmsa26 + paamt
     C                   endif                                                  if pabr2


     C                   when      parecd = '13'

     c**Permanent partial disability compensation paid to date
     c                   if        pabr2  = 'SS'
     c                   exsr      Srpmnt
     C                   eval      xxmsa3 = xxmsa3 + paamt
     c                   endif                                                  if pabr2

     c** Fatality compensation paid to date
     c                   if        pabr2  = 'FR'
     c                   exsr      Srpmnt
     C                   eval      xxmsa5 = xxmsa5 + paamt
     C                   endif                                                  if pabr2

     C** Total compensation payments paid to date
     c                   if        pabr2  = 'FR' or
     c                             pabr2  = 'SS'
     c                   exsr      Srpmnt
     C                   eval      xxmsa6 = xxmsa6 + paamt
     C                   endif                                                  if pabr2

     c**Investigation expenses
     C                   if        pabr2 = 'BC'  or
     C                             pabr2 = 'IR'  or
     C                             pabr2 = 'SV'
     c                   exsr      Srpmnt
     C                   eval      xxmsa9 = xxmsa9 + paamt
     C                   endif                                                  if pabr2

     C** Bill review expenses
     C                   if        pabr2 = 'BR'
     c                   exsr      Srpmnt
     C                   eval      xxmsa10 = xxmsa10 + paamt
     C                   endif                                                  if pabr2

     C** Attorney & other legal expenses including court cost
     C                   if        pabr2 = 'CR' or
     C                             pabr2 = 'DA' or
     c                             pabr2 = 'EF'
     c                   exsr      Srpmnt
     C                   eval      xxmsa12 = xxmsa12 + paamt
     C                   endif                                                  if pabr2

     C** Other allocated loss adjusting expenses
     C     pabr2         lookup    xxbar9                                 60
     C                   if        *in60 = *on
     c                   exsr      Srpmnt
     C                   eval      xxmsa14 = xxmsa14 + paamt
     C                   endif                                                  if *in60

     C** Total ALAE payments paid to date
     C     pabr2         lookup    xxbar10                                60
     C                   if        *in60 = *on
     c                   exsr      Srpmnt
     C                   eval      xxmsa15 = xxmsa15 + paamt
     C                   endif                                                  if *in60

     C** Expense Recoveries
     C                   if        pabr2 = 'SR' or
     C                             pabr2 = 'FU' or
     C                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     C                   eval      xxmsa20 = xxmsa20 + paamt
     C                   endif                                                  if pabr2

     c** Deductible recoveries from policyholders per policy terms
     C                   if        pabr2 = 'EQ'
     c                   exsr      Srpmnt
     C                   eval      xxmsa21 = xxmsa21 + paamt
     C                   endif                                                  if pabr2

     C** Second Injury Fund Recovery
     C                   if        pabr2 = 'FU'  or
     C                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     C                   eval      xxmsa25 = xxmsa25 + paamt
     C                   endif                                                  if pabr2

     c** Burial Expenses paid to date
     C                   if        pabr2 = 'FR'
     c                   exsr      Srpmnt
     C                   eval      xxmsa26 = xxmsa26 + paamt
     C                   endif                                                  if pabr2


     C                   when      parecd = '14'

     C** Medical payments paid to date
     c                   if        pabr2  = 'IO' or
     c                             pabr2  = 'IT'
     c                   exsr      Srpmnt
     C                   eval      xxmsa7 = xxmsa7 + paamt
     C                   endif                                                  pabr2 = 'IO', 'IT'

     C** Vocational rehabilitation paid to date
     C     pabr2         lookup    xxbar8                                 60
     C                   if        *in60 = *on
     c                   exsr      Srpmnt
     C                   eval      xxmsa8 = xxmsa8 + paamt
     C                   endif                                                  *in60 = *on

     c** Medical case management expenses
     C                   if        pabr2 = 'IO' or
     C                             pabr2 = 'IT'
     c                   exsr      Srpmnt
     C                   eval      xxmsa11 = xxmsa11 + paamt
     C                   endif                                                  if pabr2

     C** Vocational rehabilitation expenses (ALAE)
     C                   if        pabr2 = 'VR' or
     C                             pabr2 = 'VT' or
     C                             pabr2 = 'VE' or
     C                             pabr2 = 'VM'
     c                   exsr      Srpmnt
     C                   eval      xxmsa13 = xxmsa13 + paamt
     C                   endif                                                  if pabr2

     C** Vocational Rehabilitation Recoveries
     C                   if        pabr2 = 'DQ' or
     C                             pabr2 = 'SR' or
     C                             pabr2 = 'FU'
     c                   exsr      Srpmnt
     C                   eval      xxmsa19 = xxmsa19 + paamt
     C                   endif                                                  if pabr2

     c** Deductible recoveries from policyholders per policy terms
     C                   if        pabr2 = 'DQ'
     c                   exsr      Srpmnt
     C                   eval      xxmsa21 = xxmsa21 + paamt
     C                   endif                                                  if pabr2

     C** Second Injury Fund Recovery
     C                   if        pabr2 = 'FU'
     c                   exsr      Srpmnt
     C                   eval      xxmsa25 = xxmsa25 + paamt
     C                   endif                                                  if pabr2


     C                   when      parecd = '16'

     C** Attorney & other legal expenses including court cost
     C                   if        pabr2 = 'DA' or
     C                             pabr2 = 'MS' or
     C                             pabr2 = 'SA'
     c                   exsr      Srpmnt
     C                   eval      xxmsa12 = xxmsa12 + paamt
     C                   endif                                                  if pabr2

     C** Total ALAE payments paid to date
     C                   if        pabr2 = 'DA' or
     C                             pabr2 = 'MS' or
     C                             pabr2 = 'SA'
     c                   exsr      Srpmnt
     C                   eval      xxmsa15 = xxmsa15 + paamt
     C                   endif                                                  if pabr2

     C** Expense Recoveries
     C                   if        pabr2 = 'SR' or
     C                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     C                   eval      xxmsa20 = xxmsa20 + paamt
     C                   endif                                                  if pabr2

     c** Deductible recoveries from policyholders per policy terms
     C                   if        pabr2 = 'LQ'
     c                   exsr      Srpmnt
     C                   eval      xxmsa21 = xxmsa21 + paamt
     C                   endif                                                  if pabr2

     C** Second Injury Fund Recovery
     C                   if        pabr2 = 'FU'  or
     C                             pabr2 = 'SD'
     c                   exsr      Srpmnt
     C                   eval      xxmsa25 = xxmsa25 + paamt
     C                   endif                                                  if pabr2

     c                   endsl

     C** Subrogated recoveries
     C                   if        pabr2 = 'SR'
     c                   exsr      Srpmnt
     C                   eval      xxmsa16 = xxmsa16 + paamt
     C                   endif                                                  pabr2 = 'SR'

     c     paky          reade     ppa02                                  99
     c                   enddo

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa1
     c                   exsr      srvalue
     c                   movel     xxamt         cattpd                        TEMP_TOTAL_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa2
     c                   exsr      srvalue
     c                   movel     xxamt         catppd                        TEMP_PARTIAL_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa3
     c                   exsr      srvalue
     c                   movel     xxamt         CAPPPD                        PERM_PARTIAL_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa4
     c                   exsr      srvalue
     c                   movel     xxamt         CAPTPD                        PERM_TOTAL_PTDT

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa5
     c                   exsr      srvalue
     c                   movel     xxamt         CAFAPD                        FAT_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa6
     c                   exsr      srvalue
     c                   movel     xxamt         CATCOPD                       TOTAL_COMP_PTDT

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa7
     c                   exsr      srvalue
     c                   movel     xxamt         CAMEPD                        MED_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa8
     c                   exsr      srvalue
     c                   movel     xxamt         CARHPD                        REHAB_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa9
     c                   exsr      srvalue
     c                   movel     xxamt         CAINVX                        INVESTIGATION_EXP

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa10
     c                   exsr      srvalue
     c                   movel     xxamt         CABRVX                        BILL_REVIEW_EXP

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa11
     c                   exsr      srvalue
     c                   movel     xxamt         CACAMX                        MED_CASE_MGT_EXP

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa12
     c                   exsr      srvalue
     c                   movel     xxamt         CALGLX                        LEGAL_EXP

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa13
     c                   exsr      srvalue
     c                   movel     xxamt         CAVREX                        VOC_REHAB_EXP

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa14
     c                   exsr      srvalue
     c                   movel     xxamt         CAOALX                        OTHER_ALAE_EXP

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa15
     c                   exsr      srvalue
     c                   movel     xxamt         CATALP                        TOTAL_ALAE_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa16
     c                   exsr      srvalue
     c                   movel     xxamt         CASUBR                        SUBROGATION

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa17
     c                   exsr      srvalue
     c                   movel     xxamt         CACORC                        COMP_RECOVERED

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa18
     c                   exsr      srvalue
     c                   movel     xxamt         CAMERC                        MED_RECOVERED


     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa19
     c                   exsr      srvalue
     c                   movel     xxamt         CARHRC                        REHAB_RECOVERED

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa20
     c                   exsr      srvalue
     c                   movel     xxamt         CAALRC                        ALAE_RECOVERED

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa21
     c                   exsr      srvalue
     c                   movel     xxamt         CADDRC                        DEDUCTIBLE_RECOVERED

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa22
     c                   exsr      srvalue
     c                   movel     xxamt         CAVRPD                        Voc Rehab

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa23
     c                   exsr      srvalue
     c                   movel     xxamt         CAOJPD                        COMP_OJT_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa24
     c                   exsr      srvalue
     c                   movel     xxamt         CALEPD                        LEC_PTD

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa25
     c                   exsr      srvalue
     c                   movel     xxamt         CASIRC                        SIF_REC

     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxmsa26
     c                   exsr      srvalue
     c                   movel     xxamt         CABURX                        BURIAL_EXP

     c** Total payments paid to date
     c                   eval      xxtptd = (xxmsa6 + xxmsa7)
     c                              +  (xxmsa8 + xxmsa15)
     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxtptd
     c                   exsr      srvalue
     c                   movel     xxamt         CATOPD                        TOTAL_PTDP

     c** Total recovered
     c                   eval      xxrecp = (xxmsa17+ xxmsa18)
     c                              + (xxmsa19 + xxmsa20)
     c                   eval      xxamt = *blanks
     c                   eval      xxmsam = xxrecp
     c                   exsr      srvalue
     c                   movel     xxamt         CATORC                        TOTAL_RECOVERED

     c                   endsr                                                  SrPaidToDt
    c*====================================================================
    c*====================================================================
    c*  Srpmnt - Calculate payment
    c*====================================================================
     c     Srpmnt        begsr
     c*
     c                   if        patype = 'S'
     c                             or patype  = 'V'
     c                             or patype  = 'R'
     c                   if        paamt >= 0
     c                   eval      paamt = paamt * -1
     c                   endif
     c                   endif                                                  PATYPE = 'S','V','R'
     c*
     c                   endsr
    c*====================================================================
    c*====================================================================
    c*  Srvalue- Determine sign and store calue in Char field
    c*====================================================================
     c     Srvalue       begsr
     c                   if        xxmsam >= 0
     c                   eval      xxsgn = '+'
     c                   else
     c                   eval      xxsgn = '-'
     c                   endif                                                  XXMSAM >= 0

     c                   move      *zeros        xxamt
     c                   eval      xxmsam = %abs(xxmsam)
     c************* beg chg ** 09/01/10 ************************************
     c                   move      xxmsam        x3amt
     c                   eval      x2amt  = '.'
     c                   movel     xxmsam        x1amt
     c                   move      xxamt1        xxamt
     c
     c************       move      xxmsam        xxamt
     c************* end chg ** 09/01/10 ************************************
     c                   movel     xxsgn         xxamt
     c                   endsr

     c************* beg add ** 09/01/10 ************************************
     c     Srvalue1      begsr
     c                   if        xxawamt >= 0
     c                   eval      xxsgn = '+'
     c                   else
     c                   eval      xxsgn = '-'
     c                   endif                                                  XXAWW >= 0

     c                   move      *zeros        xxaww
     c                   eval      xxawamt = %abs(xxawamt)
     c                   move      xxawamt       x3amtw
     c                   eval      x2amtw  = '.'
     c                   movel     xxawamt       x1amtw

     c                   move      xxamt1w       xxaww            15
     c                   movel     xxsgn         xxaww
     c                   endsr
     c************* end add ** 09/01/10 ************************************

    c*====================================================================
    c*  clrout - Clear the RF for WXUxx files.
    c*====================================================================
     c     clrout        begsr
     c*
     c                   clear                   pca02
     c*
     c                   endsr
    c*====================================================================
     c*
     c*
** ctdta xxbar1
AWCAINIPMSPETRTTVT
** ctdta xxbar2
ASTPTS
** ctdta xxbar3
AVCECTDFIBPIPPPQPUSOWBWLWO
** ctdta xxbar4
PLPSPT
** ctdta xxbar5
DBDSFIFR
** ctdta xxbar6
TRWLFRAWCAINIPMSPETTVTASTPTSAVCECTDFIBPIPPPQPUSOWBWOPLPSPTDBDSFL
** ctdta xxbar7
ICIOITACANCMDCDDDGDMDXHIHOIMMCMDMIMQMRMSMTNENSOPORPAPHPLPYRARCRERXSCWBWO
** ctdta xxbar8
MSVEVMVRVT
** ctdta xxbar9
AUCLEXFNIAISIVLCLIMPMSPRRPSESFSM
** ctdta xxbar10
BCIRSVBRCRDAEFAUCLEXFNIAISIVLCLIMPMSPRRPSESFSM
