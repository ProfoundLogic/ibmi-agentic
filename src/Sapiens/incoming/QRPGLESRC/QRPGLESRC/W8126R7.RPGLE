     H copyright('(C) Copyright Progressive Data Solutions, Inc., 2000-04')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *****************************************************************
     *    PROGRAM: W8126R7                                           *
     *                                                               *
     *    DESCRIPTION: CLARENDON DATA EXTRACT PROCESS                *
     *                                                               *
     *    CREATED BY:  C:53   10/12/01   PROJECT# WC970              *
     *                                                               *
     *    CALLED FROM:                                               *
     *      W8126C - MAIN CL                                         *
     *    CALLS TO:                                                  *
     *      W81??? - POLICY WRITE                                    *
     *****************************************************************
      * Revised:                                                      *
      *
      * 08/13/18 C:53  Project: WC1917
      *                Part code field change
      *                                                               *
      * 11/24/04 C:136 Suoport: WA129 44786                           *
      *                Fixed algorithm to correctly find claim when   *
      *                claim payment has been transferred.            *
      *                                                               *
      * 08/31/04 C:136 Support: WA122 43204                           *
      *                Change the way deductible recoveries are       *
      *                calculated per meeting with Laura Joosten with *
      *                Clarenden per LK, CG, BA.                      *
      *                                                               *
      * 08/02/04 C:136 Support: WA121 43427                           *
      *                Ensure that MNDEDS gets loaded with '.00'     *
      *                                                               *
      * 04/06/04 C:136 Support: WA095 41418                           *
      *                Prevent decimal data error if payments have    *
      *                been transferred to a new claim.               *
      *                                                               *
      * 04/05/04 C:136 Support: WA095 41488                           *
      *                Zero out the Deductibles generated field.      *
      *                It is for third party claims only which our    *
      *                aplication doesn't cover. Correct the          *
      *                deductibles recovered algorithm. Requires      *
      *                changing the claim logical in the main loop.   *
      *                                                               *
      * 02/24/04  C:33  Support: WA095/41418                          *
      *                 Added comments for support purposes.          *
      *                                                               *
      * 07/22/03  C:61  support wa099 32376                           *
      *                 Change to use Wxpgl.                          *
     *                                                               *
      * 08/20/02  C:142  PROJECT: WC970                               *
      *                  Changed to select any claims with            *
      *                  outstanding reserves.                        *
     *****************************************************************
     H DATFMT(*USA)
     *                                                              *
XX   Fclmml     if   e           k disk
     F                                     rename(cmm01:cmm02)
      *** beg chg ***** 11/24/04 ***********************************************
     F************* clmmp     o  a e             disk
     F************* clmnp     o  a e             disk
     Fclmmp     uf a e             disk
     Fclmnp     uf a e             disk
XX   Fclmnl     if   e           k disk
     F                                     rename(cmn01:cmn02)
      *** end chg ***** 11/24/04 ***********************************************
      ******* beg chg ****** 07/22/03 *********************************
XX   F**********wxpgp     if   e             disk
XX   Fwxpgl     if   e             disk
      ******* end chg ****** 07/22/03 *********************************
XX   Fwmcml1    if   e           k disk
XX   Fwmcdl     if   e           k disk
XX   Fwdell     if   e           k disk
XX   Fwmeml     if   e           k disk
XX   Fwmrsl     if   e           k disk
XX   Fwmrpl4    if   e           k disk
     Fwtgtl     if   e           k disk
     F                                     rename(pgt01:pgt02)
XX   Fwdpal5    if   e           k disk
X?   Fwmchl     if   e           k disk
     *
     * Employer Detail File
     FWdell2    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr2)
     F                                     Prefix(L2:2)
     *
     FWmgcl     IF   E           K Disk
      *** beg chg ***** 04/05/04 ***********************************************
X?   F************ wdddl     if   e           k disk
X?   Fwdddl1    if   e           k disk
     Fwxx1l     IF   E           K Disk
XX   Fwmcml     if   e           k disk    rename(pcm01:pcm02)
      *** end chg ***** 04/05/04 ***********************************************
      *** beg add ***** 04/06/04 ***********************************************
      *** beg del ***** 11/24/04 ***********************************************
XX   F*********** wacml     if   e           k disk    rename(pcm01:pcm03) prefix(C2:2)
      *** end del ***** 11/24/04 ***********************************************
XX   Fwactl1    if   e           k disk
      *** end add ***** 04/06/04 ***********************************************
      *** beg add ***** 08/31/04 ***********************************************
XX   Fwmfnl     if   e           k disk
      *** end add ***** 08/31/04 ***********************************************
     Fwtabl     if   e           k disk
     F                                     ignore(prs01)
     F                                     ignore(psy01)
     F                                     ignore(ptr01)
     F                                     ignore(ppc01)
     F                                     ignore(prc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     F                                     ignore(pid01)
     F                                     ignore(pat01)
     F                                     ignore(psp01)
     F                                     ignore(pnc01)
     F************************             ignore(pna01)
     F                                     rename(ppa01:ppa02)
     F                                     ignore(pb701)
     F                                     ignore(pbs01)
     F                                     ignore(ptt01)
     *
     D                 ds                  inz
     D  tp                     1     10
     D                                     dim(10)
     D  xxtp01                 1      1
     D  xxtp02                 2      2
     D  xxtp03                 3      3
     D  xxtp04                 4      4
     D  xxtp05                 5      5
     D  xxtp06                 6      6
     D  xxtp07                 7      7
     D  xxtp08                 8      8
     D  xxtp09                 9      9
     D  xxtp10                10     10
     D                 ds                  inz
     D  rt                     1     20
     D                                     dim(10)
     D  cmrt01                 1      2
     D  cmrt02                 3      4
     D  cmrt03                 5      6
     D  cmrt04                 7      8
     D  cmrt05                 9     10
     D  cmrt06                11     12
     D  cmrt07                13     14
     D  cmrt08                15     16
     D  cmrt09                17     18
     D  cmrt10                19     20
     D                 ds                  inz
     D  ar                     1     90  2
     D                                     dim(10)
     D  cmar01                 1      9  2
     D  cmar02                10     18  2
     D  cmar03                19     27  2
     D  cmar04                28     36  2
     D  cmar05                37     45  2
     D  cmar06                46     54  2
     D  cmar07                55     63  2
     D  cmar08                64     72  2
     D  cmar09                73     81  2
     D  cmar10                82     90  2
     D                 ds                  inz
     D  rr                     1     90  2
     D                                     dim(10)
     D  cmrr01                 1      9  2
     D  cmrr02                10     18  2
     D  cmrr03                19     27  2
     D  cmrr04                28     36  2
     D  cmrr05                37     45  2
     D  cmrr06                46     54  2
     D  cmrr07                55     63  2
     D  cmrr08                64     72  2
     D  cmrr09                73     81  2
     D  cmrr10                82     90  2
    D*
     D                 ds
     D gtcf01                        10
     D  x1cf                          2    overlay(gtcf01)
     D  x2cf                          2    overlay(gtcf01:3)
     D   x3cf                         1    overlay(x2cf)
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D                 ds
     D xxpol#                        15
     D  cmfnd                         3  0 overlay(xxpol#)
     D  cmemp#                        9  0 overlay(xxpol#:4)
     D  cmfyr                         3  0 overlay(xxpol#:13)
     D                 ds
      *** beg chg ***** 11/24/04 ***********************************************
     D************* xxclm                         15
     D xxclm                         30
      *** end chg ***** 11/24/04 ***********************************************
     D  xxfnd                         3  0 overlay(xxclm:1)
     D  xxdsh1                        1    overlay(xxclm:4)
     D  xxfyr                         3  0 overlay(xxclm:5)
     D  xxdsh2                        1    overlay(xxclm:8)
     D  xxcase                        7  0 overlay(xxclm:9)
     D                 ds
     D tm2                            6  0
     D  zzhh                          2  0 overlay(tm2)
     D  zzmm                          2  0 overlay(tm2:3)
     D  zzss                          2  0 overlay(tm2:5)
     D                 ds
     D rtr                           20
     D  rhrt01                        2    overlay(rtr)
     D  rhrt02                        2    overlay(rtr:3)
     D  rhrt03                        2    overlay(rtr:5)
     D  rhrt04                        2    overlay(rtr:7)
     D  rhrt05                        2    overlay(rtr:9)
     D  rhrt06                        2    overlay(rtr:11)
     D  rhrt07                        2    overlay(rtr:13)
     D  rhrt08                        2    overlay(rtr:15)
     D  rhrt09                        2    overlay(rtr:17)
     D  rhrt10                        2    overlay(rtr:19)
     D                 ds
     D xxb                           22
     D  xxb1                         20    overlay(xxb)
     D  xxb2                          2    overlay(xxb:21)
     D                 ds
     D xxa                           22
     D  xxa1                         19    overlay(xxa)
     D  xxad                          1    overlay(xxa:20)
     D  xxa2                          2    overlay(xxa:21)
     *
     D                 ds
     D x7frdt                         7  0
     D  x7fday                        2    overlay(x7frdt:6)
     *
     D  xa                            1    dim(19)
     *
     D psds          esds                  extname(szq1p)
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D xxnbr           s             20  2
     D ##eoj           s              1
     D ##frst          s             15
     D ##last          s             56
     D @@co#           s              3
     D @@todt          s              8
     D i               s              2  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D PrmCo#          S              3  0
     D PrmCo           S              3
     D xxamt           s                   like(paamt)
     D xxcf02          s                   like(gtcf02)
     D xxco#           s                   like(cmco#)
     D xxdedr          s                   like(xxdeds)
     D @@clm           s                   like(mnclm#)
     D grpkey          s             20
     D Efdkey          S             10
     D*svpol#          s                   like(rpcap#)
     D xxdeds          s             20  2
     D xxelig          s              1
     D xxexpd          s                   like(xxdeds)
     D xxexrv          s                   like(xxdeds)
     D xxinpd          s                   like(xxdeds)
     D xxinrv          s                   like(xxdeds)
     D xxkey           s                   like(gtkey)
     D xxmdpd          s                   like(xxdeds)
     D xxmdrv          s                   like(xxdeds)
     D xxsant          s                   like(xxdeds)
     D xxsban          s                   like(xxdeds)
     D xxslvg          s                   like(xxdeds)
     D xxsubr          s                   like(xxdeds)
     D xxtbid          s                   like(gttbid)
     D xxuapd          s                   like(xxdeds)
     D xxuarv          s                   like(xxdeds)
     D x               s              3  0
     D xx02            s             10
     D xx03            s             10
     D x1key           s                   like(gtkey)
     D x1tbid          s                   like(gttbid)
     D x7todt          s                   like(cmidt)
     D x8todt          s              8  0
     D Polkey          s             30
     D Editzip         c                    '     -    '
     D H11A            S             11
     D*
     D Datusa          s               D   DATFMT(*USA)
     D Today           s               D   DATFMT(*USA)
     *** beg add ***** 04/05/04 ***********************************************
     D  svfnd          s                   like(cmfnd)
     D  svemp#         s                   like(cmemp#)
     D  svdiv          s                   like(cmdiv)
     D  svfyr          s                   like(cmfyr)
     D  totDedRcvrd    s             15  2
     D  dedLeft        s             15  2
     D  dedBilled      s             15  2
     *** end add ***** 04/05/04 ***********************************************
     *** beg add ***** 11/24/04 ***********************************************
     D  kkfyr          s                   like(pafyr)
     D  kkcase         s                   like(pacase)
     *** end add ***** 11/24/04 ***********************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
      *
     C     Wmgcl_Key     Klist
     C                   Kfld                    xxco#
     C                   Kfld                    Pggrp
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Kyrtcd
      *
     C     Wmeml_Key3    Klist
     C                   Kfld                    xxco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Cmemp#
     C                   Kfld                    Cmdiv
      *
     C     Wmeml_Key     Klist
     C                   Kfld                    xxco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Elemp#
     C                   Kfld                    Eldiv
      *
     C     Poky          Klist
     C                   Kfld                    Grpkey
     C                   Kfld                    Polkey
     C                   Kfld                    Efdkey
     *
     *  CLAIM MASTER FILE
     *
     C     cmky          klist
     C                   kfld                    pgco#
     C                   kfld                    pggrp
     *
     C     cmky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     *
     C     clky          klist
      *** beg add ***** 11/24/04 ************************************************
     C*****************  kfld                    @@clm
     C                   kfld                    xxclm
      *** end add ***** 11/24/04 ************************************************
     *
     *  CLAIMANT DEMOGRAPHICS FILE
     *
     C     cdky          klist
     C                   kfld                    cmss#
     *
     *  EMPLOYER MASTER FILE
     *
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     *
     *  EMPLOYER DETAIL FILE
     *
     C     elky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     *
     *  KEY LIST FOR THE RATE SET CODE FILE
     *
     C     rsky          klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     *
     *  RESERVE HISTORY FILE
     *
     C     rhky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     *
     C     rhky1         klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    x7todt
     *
     *  PAYMENT FILE
     *
     C     paky          klist
     C                   kfld                    xxco#
    C                   kfld                    cmfnd
      *** beg chg ***** 11/24/04 ************************************
    C****************   kfld                    cmfyr
    C****************   kfld                    cmcase
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
      *** end chg ***** 11/24/04 ************************************
     *
      *** beg add ***** 08/31/04 ************************************
     C     paky1         klist
     C                   kfld                    xxco#
    C                   kfld                    cmfnd
      *** beg chg ***** 11/24/04 ************************************
    C****************   kfld                    cmfyr
    C****************   kfld                    cmcase
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
      *** end chg ***** 11/24/04 ************************************
     C                   kfld                    x7frdt
      *** end add ***** 08/31/04 ************************************
     *
     *  GROUP MASTER FILE
     *
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     *
     *  PROVIDER DEMOG FILE
     *
     C     pdky          klist
     C                   kfld                    pafein
     C                   kfld                    pamod
     *
     *  GENERIC TABLE FILE
     *
     C     gtky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxtbid
     *
     C     gtky1         klist
     C                   kfld                    xxco#
     C                   kfld                    x1tbid
     C                   kfld                    x1key
     *
     *  TABLE FILE
     *
     C     tbky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey
      *
     C     Wdell2_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    CmFnd
     C                   Kfld                    Cmemp#
     *
     *  CARRIER REPORTING MASTER FILE
     *
     C     rpky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmemp#
      *** beg add ***** 04/05/04 ***********************************************
     *
     *  Deductible Detail File
     *
     C     ddky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
      *** end add ***** 04/05/04 ***********************************************
     *
     C     *Like         Define    Elpfdt        FirstEfd
     C     *Like         Define    RsJrst        Risk_St
     C     *Like         Define    ElFyr         Fyear
     C     *Like         Define    Elrtcd        Kyrtcd
     *
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     /EJECT
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@todt
     C                   parm                    @@clm
      *
     C                   Move      @@co#         Prmco#
      *
     * @@clm# format 000-000-0000000 (fnd-fyr-case)
     *
     * Set-up and Initialize
     *
     C                   move      @@co#         xxco#
     C                   move      @@todt        x8todt
     *
     C                   eval      srdti = x8todt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x7todt
     C                   movel     srdto         x7frdt
     C                   move      '01'          x7fday
     *
     C                   move      '.'           xxad
     C                   move      '-'           xxdsh1
     C                   move      '-'           xxdsh2
     *
     C                   time                    dsdttm
     C                   Move      Udate         Today
     *
      *** <= 2 PARMS: FULL RUN
     *
     C                   if        q1prms <= 2
     *
     *  USE GROUP CONTROL FILE
     *
     C                   read      xpg01                                  54
     *
     C                   dow       *in54 = *off
     *
     C                   clear                   cmm01
     C                   clear                   cmn01
     * INCLUDE FUND ?
     C     pgflag        cabne     'I'           nxtfnd
     *
     C     Wmgcl_Key     Chain     Wmgcl
      *
     C                   If        not %Found
     C                   Eval      Gcpgid  = *blank
     C                   Endif
      *
     *
     *  READ THE CLAIM RECORDS
     *
      *** beg chg ***** 04/05/04 ***********************************************
     C*****cmky********  chain     pcm01                              50
     C     cmky          chain     pcm02                              50
      *** end chg ***** 04/05/04 ***********************************************
     *
     C                   dow       *in50 = *off
     *
      *** beg add ***** 08/31/04 **********************************************
     C                   if        cmfnd <> svfnd
     C     fnky          chain     pfn01
     c                   eval      svfnd = cmfnd
     C                   endif
      *** end add ***** 08/31/04 **********************************************
      *
     C                   eval      xxfnd = cmfnd
     C                   eval      xxfyr = cmfyr
     C                   eval      xxcase = cmcase
      *** beg add ***** 11/24/04 ***********************************************
     C                   eval      kkfyr  = cmfyr
     C                   eval      kkcase = cmcase
      *** end add ***** 11/24/04 ***********************************************
     *
     C                   exsr      eligsr
     *
     C                   if        xxelig = 'Y'
     C                   exsr      info
     C                   exsr      rsrv
     C                   exsr      pymt
      *** beg add ***** 04/05/04 ************************************************
      *** beg del ***** 08/30/04 ************************************************
     C*****************  exsr      dedRecvr
      *** end del ***** 08/30/04 ************************************************
      *** end add ***** 04/05/04 ************************************************
     C                   exsr      move
     *
     * CHECK FOR POLICY RCD
     *
     C                   Exsr      Wrtpol
     *
     C                   Eval      Mmfyr  = Cmfyr
     C                   Eval      Mmfnd  = Cmfnd
     C                   Eval      Mmemp# = Cmemp#
     C                   Eval      Mmcase = Cmcase
     C                   Eval      Mnfyr  = Cmfyr
     C                   Eval      Mnfnd  = Cmfnd
     C                   Eval      Mnemp# = Cmemp#
     C                   Eval      Mncase = Cmcase
     C                   write     cmm01
     C                   write     cmn01
     C                   endif
     *
      *** beg chg ***** 04/05/04 ***********************************************
     C*****cmky********* reade     pcm01                                  50
     C     cmky          reade     pcm02                                  50
      *** end chg ***** 04/05/04 ***********************************************
     C                   enddo
     *
     C     nxtfnd        tag
     C                   read      xpg01                                  54
     C                   enddo
     *
     C                   move      *on           *inlr
     *
     C                   else
     *
      *** > 2 PARMS: SINGLE CLAIM EXECUTION
     *
     C                   movel(p)  @@clm         xxclm
      *** beg add ***** 11/24/04 ***********************************************
     C                   eval      kkfyr  = xxfyr
     C                   eval      kkcase = xxcase
      *** end add ***** 11/24/04 ***********************************************
     *
     C     clky          chain     cmm02
     *
     C                   If        not %Found
     *
     C                   clear                   cmm01
     C                   clear                   cmn01
     *
     *
      *   If no hit on claims master file, check in claims transfer and rechain from year to
     *   claim master to find where the payment was originally paid from.
     *
      *** beg chg ***** 04/06/04 **********************************************
     C*****cmky1*******  chain     pcm01                              50
     C     cmky1         chain     pcm01
     C                   if        not %found
     C     cmky1         chain     pct01
     C                   if        %found
     C                   eval      xxfyr  = ctxfyr
     C                   eval      xxcase = ctxcas
      *** beg chg ***** 11/24/04 **********************************************
     C*****cmky1*******  chain     pcm03
     C*****************  if        %found
     C*****************  eval      cmfnd  = c2fnd
     C*****************  eval      cmfyr  = c2fyr
     C*****************  eval      cmemp# = c2emp#
     C*****************  eval      cmdiv  = c2div
     C*****************  eval      cmcase = c2case
     C*****************  eval      cmss#  = c2ss#
     C*****************  eval      cmstat = c2stat
     C*****************  eval      cmpart = c2part
     C*****************  eval      cmcaus = c2caus
     C*****************  eval      cmnatr = c2natr
     C*****************  eval      cmacdt = c2acdt
     C*****************  eval      cmidt  = c2idt
     C*****************  eval      cmodis = c2odis
     C*****************  eval      cmjrst = c2jrst
     C*****************  eval      cmlscv = c2lscv
     C*****************  eval      cmrt01 = c2rt01
     C*****************  eval      cmrt02 = c2rt02
     C*****************  eval      cmrt03 = c2rt03
     C*****************  eval      cmrt04 = c2rt04
     C*****************  eval      cmrt05 = c2rt05
     C*****************  eval      cmrt06 = c2rt06
     C*****************  eval      cmrt07 = c2rt07
     C*****************  eval      cmrt08 = c2rt08
     C*****************  eval      cmrt09 = c2rt09
     C*****************  eval      cmrt10 = c2rt10
     C*****************  eval      cmrr01 = c2rr01
     C*****************  eval      cmrr02 = c2rr02
     C*****************  eval      cmrr03 = c2rr03
     C*****************  eval      cmrr04 = c2rr04
     C*****************  eval      cmrr05 = c2rr05
     C*****************  eval      cmrr06 = c2rr06
     C*****************  eval      cmrr07 = c2rr07
     C*****************  eval      cmrr08 = c2rr08
     C*****************  eval      cmrr09 = c2rr09
     C*****************  eval      cmrr10 = c2rr10
     C*****************  endif
     C*****************  endif
     C*****************  endif
     C     cmky1         chain     pcm01
     C                   if        %found
      *** end chg ***** 11/24/04 **********************************************
      *** end chg ***** 04/06/04 **********************************************
     C                   eval      Pggrp = cmfnd
     *
     C     Wmgcl_Key     Chain     Wmgcl
      *
     C                   If        not %Found
     C                   Eval      Gcpgid  = *blank
     C                   Endif
     *
      *** beg add ***** 08/31/04 **********************************************
     C     fnky          chain     pfn01
      *** end add ***** 08/31/04 **********************************************
     C                   exsr      info
     C                   exsr      rsrv
     C                   exsr      pymt
      *** beg add ***** 04/05/04 ************************************************
      *** beg del ***** 08/31/04 ************************************************
     C*****************  exsr      dedRecvr
      *** end del ***** 08/31/04 ************************************************
      *** end add ***** 04/05/04 ************************************************
     C                   exsr      move
    
     C                   Eval      Mmfyr  = Cmfyr
     C                   Eval      Mmfnd  = Cmfnd
     C                   Eval      Mmemp# = Cmemp#
     C                   Eval      Mmcase = Cmcase
     C                   Eval      Mnfyr  = Cmfyr
     C                   Eval      Mnfnd  = Cmfnd
     C                   Eval      Mnemp# = Cmemp#
     C                   Eval      Mncase = Cmcase

      *** beg chg ***** 11/24/04 ***********************************************
     C*****************  write     cmm01
     C*****************  write     cmn01
     C     clky          chain     cmm02
     C                   if        not %found
     C                   write     cmm01
     C                   else
     C                   update    cmm01
     C                   endif
      *
     C     clky          chain     cmn02
     C                   if        not %found
     C                   write     cmn01
     C                   else
     C                   update    cmn01
     C                   endif
      *** end chg ***** 11/24/04 ***********************************************
     *
     C                   endif
      *** beg chg ***** 11/24/04 ***********************************************
     C**************     return
     C                   endif
     C                   endif
     C                   endif
     C                   return
      *** end chg ***** 11/24/04 ***********************************************
     C                   endif
     *===============================================================
     * INFO - PICK UP INFO FROM FILES OTHER THAN THE CLAIM FILE
     *===============================================================
     *
     C     info          begsr
     *
     C                   Clear                   Risk_St
     *
     *  GET CLAIMANT DEMOGRAPHICS
     *
     C     cdky          chain     pcd01                              61
     *
     C                   movel     cdfsn         ##frst
     C                   movel     cdlsn         ##last
     *
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm      '1'           ##eoj
     *
     C                   movel     ##last        mncltn
     *
     *  GET CARRIER FILE RECORD IF EXISTS
     *
     C     rpky          chain     prp01                              99
     *
     C                   if        *in99 = *off and
     C                             rpcap# <> *blank
     C                   movel(p)  rpcap#        mmpol
     C                   else
     C                   movel(p)  xxpol#        mmpol
     C                   endif
     *
     C     elky          chain     pel01                              99
     *
     C                   clear                   mmefdt
     *
     C                   if        *in99 = *off
     C     rsky          chain     prs01                              99
     C                   endif
     *
     C                   if        *in99 = *off
     C                   move      rsjrst        mmrst
     C                   Move      Rsjrst        Risk_St
     C                   endif
      *
     C                   Eval      Fyear = CmFyr
      *
     C                   Exsr      Eff_Date_St
      *
     C                   If        Firstefd <> *Zero
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Mmefdt
     C                   Else
     C                   Eval      Mmefdt = *Blank
     C                   Endif
     *
     *  GET CLAIM HISTORY RECORD IF CLOSED
     *
     C                   clear                   mmcldt
     C                   clear                   mmrodt
     *
     C     rhky          setgt     pch01
     C     rhky          readpe    pch01                                  99
     *
     C                   if        *in99 = *off
     * CLOSED
     C                   if        cmstat = 'C'
     C                              and chefdt <> *zeros
     C     *CYMD         Movel     chefdt        Datusa
     C                   Movel     Datusa        mmcldt
     * OPEN/RE-OPEN
     C                   else
     C     rhky          readpe    pch01                                  60
     C                   if        *in60 = *off
     C                              and chefdt <> *zeros
     C     *CYMD         Movel     chefdt        Datusa
     C                   Movel     Datusa        mmrodt
     C                   endif
     C                   endif
     C                   endif
     *
      *** beg del ***** 04/05/04 ***********************************************
    C*****************  clear                   xxdeds
    C*****rhky********  setll     pdd01
    C*****rhky********  reade     pdd01                                  99
    C*****************  dow       *in99 = *off
    C*****************  if        ddidt >= x7frdt
    C*****************            and ddidt <= x7todt
    C*****************  eval      xxdeds = xxdeds + ddamtb
    C*****************  endif
    C*****rhky********  reade     pdd01                                  99
    C*****************  enddo
      *** end del ***** 04/05/04 ***********************************************
      *
     C                   movel(p)  Cmpart        xxkey
     C     tbky          chain     ppa02
     C                   If        %Found
      *** Beg Chg ***** 08/31/04 *************************************
     C****************** move      paedip        mmtypb
     C                   movel(p)  paedip        mmtypb
      *** End Chg ***** 08/31/04 *************************************
     C                   else
     C                   clear                   mmtypb
     C                   Endif
      *
     C                   movel(p)  Cmnatr        xxkey
     C     tbky          chain     Pna01
     C                   if        %Found
     C                   move      naedin        mmnoi
     C                   else
     C                   clear                   mmnoi
     C                   endif
      *
     C                   movel(p)  Cmcaus        xxkey
     C     tbky          chain     Pca01
     C                   if        %Found
     C                   move      caedic        mmcoi
     C                   else
     C                   clear                   mmnoi
     C                   endif
      *
     C                   endsr
     *
      /eject
     **********************************************************************
     *  Get Earliest Effective Date for State /Year
     **********************************************************************
      *
     C     Eff_Date_St   Begsr
      *
     C                   Clear                   FirstEfd
      *
     C     Wdell2_Key    Setll     Wdell2
      *
     C                   Dou       %eof(Wdell2)
      *
     C     Wdell2_Key    Reade     Wdell2
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        L2Fyr <> Fyear
     C                   Iter
     C                   Endif
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
      *
     C                   If        RsJrst = Risk_St
      *
     C                   If        L2pfdt < FirstEfd or
     C                             FirstEfd = *Zero
     C                   Eval      FirstEfd = L2Pfdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
     /EJECT
     *===============================================================
     * RSRV  - PROCESS RESERVE INFO
     *===============================================================
     *
     C     rsrv          begsr
     *
     C                   clear                   xxinrv
     C                   clear                   xxmdrv
     C                   clear                   xxexrv
     C                   clear                   xxuarv
     *
     ** LOAD RESERVES
     *
     C                   do        10            i
     *
     C                   movel(p)  rt(i)         xxkey
     C     tbky          chain     prt01                              60
     C                   if        *in60 = *off
     *
     C                   if        rttype = 'C'
     C                   eval      xxinrv = xxinrv + rr(i)
     C                   endif
     *
     C                   if        rttype = 'M'
     C                   eval      xxmdrv = xxmdrv + rr(i)
     C                   endif
     *
     C                   if        rttype = 'L' or
     C                             rttype = 'E'
     C                   eval      xxexrv = xxexrv + rr(i)
     C                   endif
     *
     C                   endif
     *
     C                   enddo
     *
     C                   endsr
     /EJECT
     *===============================================================
     * PYMT  - PROCESS PAYMENT INFO
     *===============================================================
     *
     C     pymt          begsr
     *
    C                   Clear                   xxinpd
    C                   Clear                   xxmdpd
    C                   Clear                   xxexpd
    C                   Clear                   xxuapd
     C                   clear                   xxslvg
    C                   Clear                   xxsubr
     C                   clear                   xxsant
     C                   clear                   xxsban
    C                   Clear                   xxdedr
     *
      *** beg chg ***** 08/31/04 *************************************
     C*****paky********  chain     ppa01                              65
     C     paky1         setll     ppa01
     C     paky          reade     ppa01                                  65
      *** end chg ***** 08/31/04 *************************************
     C                   dow       *in65 = *off
     *
      *** beg chg ***** 08/31/04 *************************************
     C*****************  if        patrdt >= x7frdt
     C*****************            and patrdt <= x7todt
     C                   if        patrdt <= x7todt
      *** end chg ***** 08/31/04 *************************************
     *
     C                   If        Patype = 'S' or
     C                             Patype = 'R' or
     C                             Patype = 'V'
     C                   Eval      xxamt = paamt * -1
     C                   Else
     C                   Eval      xxamt = paamt
     C                   Endif
     *
     C                   If        paretp = 'C'
     C                   Eval      xxinpd = xxinpd + xxamt
     C                   Endif
     C                   if        paretp = 'M'
     C                   eval      xxmdpd = xxmdpd + xxamt
     C                   endif
      *******************************************************************   ******
     C                   If        Paretp = 'L' or Paretp = 'E'
     C                   Eval      xxexpd = xxexpd + xxamt
    C*************      If        Patype = 'P' or
    C*************                Patype = 'M' or
    C*************                Patype = 'A' or
    C*************                Patype = 'Z'
    C*************      Eval      xxexpd = xxexpd + xxamt
    C*************      Endif
     *************
    C*************      If        Patype = 'S' or
    C*************                Patype = 'R' or
    C*************                Patype = 'V'
    C*************      Eval      xxexpd = xxexpd - xxamt
    C*************      Endif
     C                   Endif
      *******************************************************************   ******
     C                   if        patype = 'R'
     *
     C                   movel(p)  pabr2         xxkey
     C     tbky          chain     pb201                              99
     C  n99              if        b2plcd = 'O'
     C                             or b2plcd = 'P'
     C                   eval      xxsubr = xxsubr + xxamt
     C                   endif
      *** beg del ***** 04/05/04 **********************************************
    C**n99***********   if        b2plcd = 'Q'
    C****************   eval      xxdedr = xxdedr + xxamt
    C****************   endif
      *** end del ***** 04/05/04 **********************************************

     C                   endif
     *
      *** beg add ***** 08/31/04 **********************************************
     c                   if        (PABR2 = FNCOP1 or
     c                              PABR2 = FNMEP1 or
     c                              PABR2 = FNEXP1 or
     c                              PABR2 = FNLEP1)
     c                   eval      xxdedr = xxdedr + xxamt
     c                   endif
      *** end add ***** 08/31/04 **********************************************
     *
     C                   endif
     *
     C     paky          reade     ppa01                                  65
     C                   enddo
     *
     *
     C                   endsr
     /EJECT
     *===============================================================
     * MOVE  - ASSIGN VALUES FROM MAJOR FILES RETRIEVED IN MAIN LOOP
     *===============================================================
     *
     C     move          begsr
     *
     * CLMMP
     C                   move      gcpgid        mmpgid
     C                   movel(p)  xxclm         mmclm#
     *
     C                   clear                   mmaccd
     C                   clear                   mmrpdt
     C                   clear                   mmsysd
     C                   clear                   mmacdt
     *
     C                   if        cmacdt <> *zeros
     C     *CYMD         Movel     cmacdt        Datusa
     C                   Movel     Datusa        mmaccd
     C                   endif
     C                   if        cmdtrp <> *zeros
     C     *CYMD         Movel     cmdtrp        Datusa
     C                   Movel     Datusa        mmrpdt
     C                   endif
     C                   if        cmidt <> *zeros
     C     *CYMD         Movel     cmidt         Datusa
     C                   Movel     Datusa        mmsysd
     C                   endif
     C                   if        x7todt <> *zeros
     C     *CYMD         Movel     x7todt        Datusa
     C                   Movel     Datusa        mmacdt
     C                   endif
     *
     C                   clear                   mmcom
     C                   clear                   mmcat#
     *
     C                   if        cmodis = 'Y'
     C                   move      'OD'          mmtypl
     *
     C                   else
     * TRY STATE 1ST
     C     cmjrst        cat(p)    'LC':0        x1tbid
     C                   movel     cmlscv        x1key
     *
     C     gtky1         chain     pgt02                              99
     C                   if        *in99 = *on
     C                   movel(p)  '  LC'        x1tbid
     *
     C     gtky1         chain     pgt02                              99
     C                   endif
     *
     C                   movel     '02'          xx02
     C                   movel     '03'          xx03
     *
     C                   select
     C                   when      gtcf01 = xx02
     C                   move      'OD'          mmtypl
     *
     C                   when      gtcf01 = xx03
     C                   move      'CT'          mmtypl
     *
     C                   other
     C                   move      'WC'          mmtypl
     C                   endsl
     C                   endif
     *
     * CLMNP
     C                   move      gcpgid        mnpgid
     C                   movel(p)  xxclm         mnclm#
     C                   move      1             mnclt#
     C                   movel(p)  '16'          mnasln
     C                   movel(p)  'WC'          mnlob
     *
     C                   clear                   mneomd
     C                   if        x7todt <> *zeros
     C     *CYMD         Movel     x7todt        Datusa
     C                   Movel     Datusa        mneomd
     C                   endif
     *
     C                   Eval      mninpd = %editc(xxinpd:'P')
     C                   Eval      Mninpd# = Xxinpd
     *
     C                   Eval      mninrv = %editc(xxinrv:'P')
     C                   Eval      Mninrv# = Xxinrv
     *
     C                   Eval      mnmdpd = %editc(xxmdpd:'P')
     C                   Eval      Mnmdpd# = Xxmdpd
     *
     C                   Eval      mnmdrv = %editc(xxmdrv:'P')
     C                   Eval      Mnmdrv# = Xxmdrv
     *
      *** beg chg ***** 04/05/04 **********************************************
     C****************** Eval      mndeds = %editc(xxdeds:'P')
     C****************** Eval      Mndeds# = Xxdeds
      *** beg chg ***** 08/02/04 **********************************************
     C*****************  Eval      mndeds = *blanks
     C                   Eval      Mndeds# = *zeros
     C                   Eval      MNDEDS = %editc(MNDEDS#:'P')
      *** end chg ***** 08/02/04 **********************************************
      *** end chg ***** 04/05/04 **********************************************
     *
     C                   Eval      mnexpd = %editc(xxexpd:'P')
     C                   Eval      Mnexpd# = Xxexpd
     *
     C                   Eval      mnexrv = %editc(xxexrv:'P')
     C                   Eval      Mnexrv# = Xxexrv
     *
     C                   Eval      mnuapd = %editc(xxuapd:'P')
     C                   Eval      Mnuapd# = Xxuapd
     *
     C                   Eval      mnuarv = %editc(xxuarv:'P')
     C                   Eval      Mnuarv# = Xxuarv
     *
     C                   Eval      mnslvg = %editc(xxslvg:'P')
     C                   Eval      Mnslvg# = Xxslvg
     *
     C                   Eval      mnsubr = %editc(xxsubr:'P')
     C                   Eval      Mnsubr# = Xxsubr
     *
     C                   Eval      mnsant = %editc(xxsant:'P')
     C                   Eval      Mnsant# = Xxsant
     *
     C                   Eval      mnsban = %editc(xxsban:'P')
     C                   Eval      Mnsban# = Xxsban
     *
     C                   Eval      mndedr = %editc(xxdedr:'P')
     C                   Eval      Mndedr# = Xxdedr
     *
     C                   endsr
     *
     *===============================================================
     * ELIGSR - CHECK IF CLAIM IS ELIGIBLE FOR PROCESSING
     *===============================================================
     *
     C     eligsr        begsr
     *
     *  THE FOLLOWING CLAIMS SHOULD BE LOADED TO THE EXTRACT FILE:
     *    1. CLAIMS MAINTAINED WITHIN THE DATE RANGE
     *    2. ANY CLAIMS WITH OUTSTANDING RESERVES
     *
     C                   move      'N'           xxelig
     *
     C                   if        cmdt >= x7frdt
     C                             and cmdt <= x7todt
     C                   move      'Y'           xxelig
     C                   endif
     *
      ****** Begin Add 08/20/02 ********************************************
     C                   Xfoot     RR            RRtot            11 0
     C                   If        RRtot <> *Zero
     C                   Move      'Y'           Xxelig
     C                   Endif
      ****** End   Add 08/20/02 ********************************************
     *
     C                   endsr
     *
     /EJECT
     **********************************************************************
     *  Write Policy Master For Clarendon Pre Processing
     **********************************************************************
     C     Wrtpol        Begsr
      *
     C                   call      'W8126R2'
     C                   parm      @@co#         ##co#             3
     C                   parm      @@todt        ##todt            8
     C                   parm      xxpol#        ##pol            15
     C                   parm      CmDiv         ##Div             5 0
      *
     C                   Endsr
      *** beg add ***** 04/05/04 *******************************************
      *** beg del ***** 08/31/04 *******************************************
      *=====================================================================
      *  dedRecvr - get Deductible Recovered Amount
      *=====================================================================
     C*****dedRecvr****  begsr
      *** beg del ***** 08/31/04 *******************************************
      *****************
      ***Get*Total*Deductible Recovered on fnd/emp/div/fyr break
      *****************
     C*****************  if        svfnd  <> cmfnd  or
     C*****************            svemp# <> cmemp# or
     C*****************            svdiv  <> cmdiv  or
     C*****************            svfyr  <> cmfyr
      *****************
     C*****************  clear                   totDedRcvrd
     C*****elky********  setll     px101
     C*****elky********  reade     px101
     C*****************  dow       not %eof
     C*****************  if        x1ctyp = 'R' and x1arty = 'Q'
     C*****************  add       x1amt         totDedRcvrd
     C*****************  endif
     C*****elky********  reade     px101
     C*****************  enddo
      *****************
     C*****************  eval      dedLeft  = totDedRcvrd
     C*****************  eval      svfnd    = cmfnd
     C*****************  eval      svemp#   = cmemp#
     C*****************  eval      svdiv    = cmdiv
     C*****************  eval      svfyr    = cmfyr
     C*****************  endif
      *****************
      *****************
      ***Disperse*the*total Deductible Recovered across each case with same fnd/emp/div/yr. Note:
      ***It*is*impossible to peg the deductible recovered amount to the case # that generated the
      ***deductible*because A/R doesn't cross reference case #'s.
      *****************
     C*****************  clear                   dedBilled
     C*****ddky********  setll     pdd01
     C*****ddky********  reade     pdd01
     C*****************  dow       not %eof
     C*****************  add       ddamtb        dedBilled
     C*****ddky********  reade     pdd01
     C*****************  enddo
      *****************
     C*****************  if        dedLeft >= dedBilled
     C*****************  eval      xxdedr   = dedBilled
     C*****************  eval      dedLeft  = dedLeft - dedBilled
     C*****************  else
      *****************
     C*****************  if        dedLeft >= 0
     C*****************  eval      xxdedr   = dedLeft
     C*****************  else
     C*****************  eval      xxdedr   = 0
     C*****************  endif
      *****************
     C*****************  eval      dedLeft  = dedLeft - dedBilled
     C*****************  endif
     C*****************  endsr
      *** end del ***** 08/31/04 *******************************************
      *** end add ***** 04/05/04 *******************************************
