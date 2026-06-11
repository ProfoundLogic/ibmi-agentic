     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H* ***************************************************************
    H/TITLE W0070RX1 - CLAIM SUMMARY SCREEN
    H* ***************************************************************
    H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY SUMMARY INFORMATION  *
    H*                ON A CLAIM                                     *
    H*                                                               *
    H*  CALLED FROM: MENU                                            *
    H*                                                               *
    H*  CALLS TO: WA010R - GET SPECIFIC RETENTION & FLAG             *
    H*                                                               *
    H*****************************************************************
    H*  CREATED:                                                     *
    H*                                                               *
     H*  07/13/10 C:235 Project: 114WWK                               *
     H*                 Add Recovery Option to Subro Drop Down        *
    H*                                                               *
    H*  05/05/06 C:53 PROJECT: 129WMW                                *
    H*                ADDITIONAL REQUESTS FROM CLIENT                *
    H*                                                               *
    H*  01/30/06 C:53 PROJECT: 129WMW                                *
    H*                COPIED FROM W0070R                             *
    H*****************************************************************
    H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/31/11 C:277 SEQ:77829                                      *
     H*                Recompile Program to reflect the screen changes*
     H*                                                               *
     H* 04/07/11 C:191 PROJECT: 114WYU                                *
     H*                Delete notes recs when blanked out on screen   *
     H*                Fix roll up/down to account for missing seq#   *
     H*                                                               *
     H* 04/06/11 C: 58 PROJECT: 114WYU                                *
     H*                Add + to screen if it has comment.             *
     H*                                                               *
     H* 07/16/10 C:246 SEQ:73582 - Show JR State in Claims Summary    *
     H*                and Subset in WW Claims.                       *
     H*                                                               *
     H* 02/16/10 C:246 Project: 133WIL                                *
     H*                Add Jurisdiction State on Claims Summary Screen*
    H*                                                               *
     H*  01/28/10 C:249  Project: SY215                               *
     H*                  Retrieve brand Name
     H*
    H*  05/07/07 C:191 PROJECT: WC1438                               *
    H*                 - Moved from client to Base                   *
    H*                 - Remove companion claim coding               *
    H*                 - Rearrange function keys                     *
    H*                                                               *
    H*  00/00/00 C:00  Project: XXXXXX                               *
    H*                                                               *
    H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0070fx1  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
     Fwmcmp     if   e             disk
     F                                     rename(pcm01:dbrcdf)
     F                                     infds(dbfds)
     Fwmfnl     if   e           k disk
     Fwmeml     if   e           k disk
     *  CLAIMANT DEMOGRAPHICS
     Fwmcdl     if   e           k disk
     *  PAYMENT DETAIL FILE
     Fwdpal     if   e           k disk
     *
     Fwtabl     if   e           k disk
     F                                     ignore(prs01)
     F                                     ignore(psy01)
     F                                     ignore(ptr01)
     F                                     rename(ppa01:ppa02)
     F                                     ignore(pcc01)
     *
    F*           PDD01                             KRENAMEPDD02
     Fwmpdl     if   e           k disk
     Fwamcl     if   e           k disk
      *** BEG DEL ****** 05/07/07 **********************************************
     F***waccl     if   e           k disk
      *** END DEL ****** 05/07/07 **********************************************
     Fwmchl     if   e           k disk
     Fwmlll     if   e           k disk
     Fwmupl1    if   e           k disk
     Fwmsil     uf a e           k disk
     *
     *
     D nm1             s              1    dim(80)
     D nm2             s              1    dim(80)
     D of              s              1    dim(24) ctdata perrcd(1)
     D ofd             s             35    dim(24) alt(of)
     D ol              s             80    dim(14)
     D fl              s             80    dim(14)
     D fc              s              1    dim(80)
     D vo              s              1    dim(99)
     D vf              s              1    dim(24)
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
     D dbfds           ds
     D dbrrn                 397    400b 0
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
     D  scrr                   1     36  2
     D                                     dim(4)
     D  scrr01                 1      9  2
     D  scrr02                10     18  2
     D  scrr03                19     27  2
     D  scrr04                28     36  2
     D                 ds                  inz
     D  scar                   1     36  2
     D                                     dim(4)
     D  scar01                 1      9  2
     D  scar02                10     18  2
     D  scar03                19     27  2
     D  scar04                28     36  2
     D                 ds                  inz
     D  scti                   1     36  2
     D                                     dim(4)
     D  scti01                 1      9  2
     D  scti02                10     18  2
     D  scti03                19     27  2
     D  scti04                28     36  2
     D                 ds                  inz
     D  scrc                   1     36  2
     D                                     dim(4)
     D  scrc01                 1      9  2
     D  scrc02                10     18  2
     D  scrc03                19     27  2
     D  scrc04                28     36  2
     D                 ds                  inz
     D  scsp                   1     96
     D                                     dim(6)
     D  scsp01                 1     16
     D  scsp02                17     32
     D  scsp03                33     48
     D  scsp04                49     64
     D  scsp05                65     80
     D  scsp06                81     96
     D                 ds                  inz
     D cmss#                          9
     D  ss1                           3    overlay(cmss#)
     D  ss2                           2    overlay(cmss#:4)
     D  ss3                           4    overlay(cmss#:6)
     D                 ds                  inz
     D xdkey                         40
     D  xdco#                         3    overlay(xdkey)
     D  xdfnd                         3    overlay(xdkey:4)
     D  xdemp#                        9    overlay(xdkey:7)
     D  xddiv                         5    overlay(xdkey:16)
     D                 ds
     D qckey                         40
     D  qcco#                         3  0 overlay(qckey)
     D  qcfnd                         3  0 overlay(qckey:4)
     D  qcfyr                         3  0 overlay(qckey:7)
     D  qccase                        7  0 overlay(qckey:10)
     D                 ds
     D xxcomm                       150
     D  xxcom1                       75    overlay(xxcomm)
     D  xxcom2                       75    overlay(xxcomm:76)
     *
     D ##ofd           ds          3500
     *
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     *
     D optnm           c                   const('Claim Summary Screen')
     D moded           c                   const('-Display- ')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D**** pdsnam          c               const('Fiserv Insurance So-
     D****                                 lutions, Inc.')
     D pdsNam          S             40    Inz
     D*** END CHG ************* 01/28/10 ************************************
     D comapi          c                   const('WA001R')
     D xccomp          c                   const('Compensation')
     D xcmed           c                   const('Medical     ')
     D xcexp           c                   const('Expense     ')
     D xcleg           c                   const('Legal       ')
     D xcrect          c                   const('Recovery')
     D xcmulc          c                   const('Multiple claim')
     D xccont          c                   const('Controverted')
     D xcrein          c                   const('Reinsurance')
     *
     D xcmedo          c                   const('Med Only')
     D xclt            c                   const('Lost Time')
     D xcreco          c                   const('Record Only')
     D xf24            c                   const('g   F24=More keys')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##acdt          s              8
     D ##amt           s              9
     D ##call          s              1
     D ##case          s              7  0
     D ##co#           s              3  0
     D ##conm          s             40
     D ##data          s             80
     D ##efdt          s              8  0
     D ##eob#          s              9
     D ##fein          s              9
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##frdt          s              6
     D ##fyr           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##leng          s              3  0
     D ##mod           s              3
     D ##mode          s              1
     D ##of            s            100
     D ##pgm           s             10
     D ##pmtf          s              1
     D ##pnam          s             40
     D ##prds          s             40
     D ##req#          s              5  0
     D ##rrn           s              9  0
     D ##rtcd          s              1
     D ##sort          s              2
     D ##todt          s              6
     D ##trcd          s              2
     D ##wwmo          s              1
     D #acase          s              7
     D #aco#           s              3
     D #afnd           s              3
     D #afyr           s              3
     D #apyst          s              2
     D #aschk          s              7
     D #aseob          s              9
     D #asfdt          s              8
     D #asfei          s              9
     D #asgrp          s              1
     D #asirq          s             10
     D #aspc           s             10
     D #aspv1          s             10
     D #asrc           s             10
     D #astdt          s              8
     D #astt           s              5
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D*** BEG ADD ****** 05/07/07 ******************************************************************
     D @@pgm           s             10
     D xwwccall        s              1N
     D ##xfyr          s              3  0
     D ##rrn2          s              9  0
     D*** END ADD ****** 05/07/07 ******************************************************************
     D fl#1            s              3  0
     D fl#2            s              3  0
     D i               s              2  0
     D j               s              2  0
     D kkfein          s                   like(pafein)
     D kkmod           s                   like(pamod)
     D n               s              3  0
     D ol#1            s              3  0
     D ol#2            s              3  0
     D qqmode          s              1
     D s               s              3  0
     D scamt           s              9  2
     D scbr2           s              2
     D scbr2d          s             20
     D scfrdt          s              8  0
     D sctodt          s              8  0
     D sctrdt          s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D w               s              3  0
     D x               s              3  0
     D x@case          s             14
     D xacase          s              7
     D xafnd           s              3
     D xafyr           s              3
     D xhopen          s              1
     D xxblnk          s              3  0
     D xxdays          s              5  0
     D xxdt6           s              6
     D xxexre          s              1
     D xxfndr          s             13  2
     D xxcpay          s             13  2
     D xxkey           s             10
     D xxleav          s              1
     D xxlen           s              3  0
     D xxnum           s             80
     D xxou54          s              1
     D xxpgm           s             10
     D xxzero          s              3  0
     D y               s              3  0
     D z               s              3  0
     D kkseq#          s                   like(siseq#)
     D svseq#          s                   like(siseq#)
     D*** BEGIN DEL ****** 04/07/11 ************************************
     D******** s1seq#          s                   like(siseq#)
     D*** END   DEL ****** 04/07/11 ************************************
     D first           s                   like(*in01)
     D dumnum          s              2
     D dumofd          s             30
     D xxmis           s              3
     D xxnmbr          s              2
     D xxopfn          s              1
     D found           s              1
     D svin50          s                   like(*in50)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     *
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C*** BEG ADD ****** 05/07/07 **********************************************
     C                   parm                    @@pgm
     C*** END ADD ****** 05/07/07 **********************************************
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      dbrrn         ##rrn
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
    C*  CLAIMANT DEMOGRAPHICS FILE
    C*
     C     cdky          klist
     C                   kfld                    cmss#
    C*
    C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE (WDDDL)
    C*
     C     mcky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    x@case
    C*
    C*  EMPLOYER MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
    C*
    C*   KEY LIST FOR THE FUND
    C*
     C     fnky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
    C*
    C*   Key List for CLAIM INFO
    C*
     C     cmky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
    C*
     C     siky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    kkseq#
    C*
    C*  KEY LIST FOR THE TABLE FILE
    C*
     C     rtky          klist
     C                   kfld                    cmco#
    C*
    C*  TABLE FILE
    C*
     C     tbky          klist
     C                   kfld                    cmco#
     C                   kfld                    xxkey
    C*
    C*
    C*   KEY LIST FOR THE PROVIDER MASTER FILE
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
    C*================================================================
    C*  Normal Programming Area
    C*================================================================
     *
     C*** BEG CHG ****** 07/16/10 *****************************************************************
     C*** BEG ADD ****** 02/16/10 *****************************************************************
     C****               CALL      'RTVCLTID'
     C****               PARM                    CLTID             3
     C****               EVAL      *IN59 = *ON
     C****               IF        CLTID = '133'
     C****               EVAL      *IN59 = *OFF
     C****               ENDIF
     C***** END ADD ****** 02/16/10 **********************************
     C*** END CHG ****** 07/16/10 *****************************************************************
     C*** BEG ADD ****** 05/07/07 *****************************************************************
     C                   eval      xwwccall = *off
     C* Disable function key 1 if called from ww claims
     C                   if        q1prms >= 5
     C                   if        @@pgm = 'W0070R'
     C                   eval      xxopfn = '2'
     C                   eval      xxmis = 'NNN'
     C                   eval      xxnmbr = '01'
     C                   exsr      updofa
     C*** seton named indicator to denote called from ww claims
     C                   eval      xwwccall = *on
     C                   endif                                                  @@pgm = 'W0070R'
     C                   endif                                                  q1prms >= 5
     C*** END ADD ****** 05/07/07 *****************************************************************
      *
     C                   move      *on           *in11
     *
     C                   dow       *in11 = *on
     *
     C     @@rrn         chain     dbrcdf                             60
     C*
     C                   if        first = *off
    C                   exsr      srspec
    C*
     C                   exsr      getcof
     C                   exsr      bldlns
     C                   exsr      getpgt
     C                   exsr      pgdndo
     C                   move      *on           first
     C                   endif
     *
     C                   exsr      srspec
     C                   exsr      srstat
     C                   exsr      sratny
     C                   exsr      srstaf
     *
     C                   movel(p)  cmacds        scacds
     C                   movel(p)  cmacd2        scacd2
     *
     *  Load nature, part and cause descriptions.
     *
     C                   movel(p)  cmnatr        xxkey
     C     tbky          chain     pna01                              60
     C  n60              movel     nadesc        scnads
     *
     C                   movel(p)  cmpart        xxkey
     C     tbky          chain     ppa02                              60
     C  n60              movel     padesc        scpads
     *
     C                   movel(p)  cmcaus        xxkey
     C     tbky          chain     pca01                              60
     C  n60              movel     cadesc        sccads
     *
     *  Load claim type
     *
     C                   select
     C                   when      cmltfl = 'M'
     C                   movel(p)  xcmedo        scctyp
     C                   when      cmltfl = 'I'
     C                   movel(p)  xclt          scctyp
     C                   when      cmltfl = 'R'
     C                   movel(p)  xcreco        scctyp
     C                   endsl
     C*
     C*** BEG CHG ****** 07/16/10 *****************************************************************
     C*** BEG ADD ****** 02/16/10 *****************************************************************
     C***                IF        CLTID = '133'
     C                   Eval      SCJRST = CMJRST
     C***                ENDIF
     C***** END ADD ****** 02/16/10 **********************************
     C***** END CHG ****** 07/16/10 **********************************
     *
     *  GET FUND NAME & IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
     *
     C     fnky          chain     pfn01                              60
     *
     C     emky          chain     pem01                              60
     C                   if        *in60 = *off
     C                   movel(p)  emname        scedn
     C                   endif
     *
     *  CLAIMANT SS#
     *
     C                   if        cmss# <> *blank
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmod            10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'cmss#'       qqname           25
     C     cmss#         parm      cmss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C     ss1           cat(p)    '-':0         scss#
     C     scss#         cat       ss2:0         scss#
     C     scss#         cat       '-':0         scss#
     C     scss#         cat       ss3:0         scss#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmod            10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'cmss#'       qqname           25
     C     cmss#         parm      cmss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C                   endif
     *
     *
     *  CLAIMANT NAME
     *
     C                   clear                   sccnam
     C     cdky          chain     pcd01                              60
     C                   if        *in60 = *off
     C     cdlsn         cat(p)    ',':0         sccnam
     C     sccnam        cat       cdfsn:1       sccnam
     C                   if        cdmid <> *blank
     C     sccnam        cat       cdmid:1       sccnam
     C     sccnam        cat       '.':0         sccnam
     C                   endif
     C                   endif
     *  ACCIDENT DATE
     *
     C                   if        cmacdt <> *zero
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel(p)  srdto         scacdt
     C                   else
     C                   clear                   scacdt
     C                   endif
     *
     *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     *
     C                   if        cmcvt# <> *blank
     C                   endif
     *  FYR-CASE
     C                   move(p)   cmfnd         xafnd
     C                   move(p)   cmfyr         xafyr
     C                   move(p)   cmcase        xacase
     C     xafnd         cat(p)    '-':0         scyrcs
     C     scyrcs        cat       xafyr:0       scyrcs
     C     scyrcs        cat       '-':0         scyrcs
     C     scyrcs        cat       xacase:0      scyrcs
     *
     *  AGENGY CLAIM# (OR, SATE CLAIM#)
     *
     C                   movel(p)  cmacl#        scscl#
     *
     * ---------------------------------------------------------------
     *  RESERVE SUMMARY
     *
     C                   eval      scrr = *zeros
     C                   eval      scar = *zeros
     C                   eval      scti = *zeros
     C                   eval      scrc = *zeros
     *
    C*  Load the reserve descriptions.
    C*
     C                   movel(p)  xccomp        sctp01
     C                   movel(p)  xcmed         sctp02
     C                   movel(p)  xcexp         sctp03
     C                   movel(p)  xcleg         sctp04
    C*
     C                   do        10            n
    C*
     C                   movel     rt(n)         xxkey
     C     tbky          chain     prt01                              60
     C                   if        *in60 = *off
    C*
     C                   if        rttype = 'C'
     C                   eval      scrr(1) = scrr(1) + rr(n)
     C                   eval      scar(1) = scar(1) + ar(n)
     C                   eval      scti(1) = scti(1) + ar(n)
     C                   eval      scti(1) = scti(1) + rr(n)
     C                   endif
     C                   if        rttype = 'M'
     C                   eval      scrr(2) = scrr(2) + rr(n)
     C                   eval      scar(2) = scar(2) + ar(n)
     C                   eval      scti(2) = scti(2) + ar(n)
     C                   eval      scti(2) = scti(2) + rr(n)
     C                   endif
     C                   if        rttype = 'E'
     C                   eval      scrr(3) = scrr(3) + rr(n)
     C                   eval      scar(3) = scar(3) + ar(n)
     C                   eval      scti(3) = scti(3) + ar(n)
     C                   eval      scti(3) = scti(3) + rr(n)
     C                   endif
     C                   if        rttype = 'L'
     C                   eval      scrr(4) = scrr(4) + rr(n)
     C                   eval      scar(4) = scar(4) + ar(n)
     C                   eval      scti(4) = scti(4) + ar(n)
     C                   eval      scti(4) = scti(4) + rr(n)
     C                   endif
    C*
     C                   endif
    C*
     C                   enddo
     *
     * TOTAL LINE
     C                   xfoot     rr            scrrto
     C                   xfoot     ar            scarto
     C                   eval      scrcto = *zeros
     C                   eval      sctito = scrrto + scarto
     *
     * ---------------------------------------------------------------
     *  GET THE LAST COMP PAYMENT INFO
     *
     C                   eval      scamt = *zero
     C                   clear                   sctrdt
     C                   eval      xxdays = *zero
     C                   clear                   scbr2
     C                   clear                   scbr2d
     C                   clear                   scfrdt
     C                   clear                   sctodt
     C                   clear                   xxleav
     *
     C     cmky          setgt     ppa01
     C                   dou       *in60 = *on
     C     cmky          readpe    ppa01                                  60
     C                   if        *in60 = *off
     C                   if        paretp = 'C'
     C                             and paadst = *blank
    C*
     C                   movel(p)  patrcd        xxkey
    C*
     C     tbky          chain     ppc01                              60
    C*
    C*  DO NOT DISPLAY ADJUSTMENTS, RECOVERIES, STOP PYMTS OR VOIDS
    C*  AS LAST COMP PAYMENT
    C*
     C                   if        pctrtp = 'R'
     C                             or pctrtp = 'S'
     C                             or pctrtp = 'V'
     C                   iter
     C                   endif
    C*
     C                   move      *on           *in57
     C                   eval      scamt = paamt
     *
     C                   eval      srdti = patrdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sctrdt
     * CALC DAYS LOST
     C                   if        pawks <> *zero
     C                   if        cmdays <> *zero
     C                   eval      xxdays = pawks * cmdays
     C                   else
     C                   eval      xxdays = pawks * 5
     C                   endif
     C                   endif
     C                   eval      xxdays = xxdays + padays
     *
     *  GET ONLY ACTIVE RECORD THAT IS NOT INTEREST OR PENALTY
     *
     C                   movel(p)  pabr2         scbr2
     C                   movel(p)  pabr2         xxkey
     C     tbky          chain     pb201                              60
     C                   if        *in60 = *off
     C                             and b2stat <> 'I'
     *
     C                   if        b2plcd = 'A'
     C                             or b2plcd = 'S'
     C                   move(p)   'N'           xxleav
     *
     C                   else
     C                   movel(p)  b2desc        scbr2d
     C                   move(p)   'Y'           xxleav
     C                   endif
     C                   endif
     *
     C                   eval      srdti = pafrdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scfrdt
     *
     C                   eval      srdti = patodt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sctodt
     *
     C                   if        xxleav = 'Y'
     C                   leave
     C                   endif
     *
     C                   endif
     C                   endif
     C                   enddo
     *
     C     cmky          chain     ppa01                              60
     C                   dou       *in60 = *on
    C*
    C*** PROVIDER RECOVERIES
    C*
     C                   if        patype = 'R'
     C                   move      pafein        kkfein
     C                   move      pamod         kkmod
     C     pdky          chain     ppd01                              61
     C                   if        *in61 = *off
     C                   if        pdtype = '2'
     C                             or pdtype = '3'
    C* DO ADD
    C*
     C                   select
     C                   when      paretp = 'C'
     C                   eval      scar(1) = scar(1) + paamt
     C                   eval      scrc(1) = scrc(1) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   goto      nextpa
     C                   when      paretp = 'M'
     C                   eval      scar(2) = scar(2) + paamt
     C                   eval      scrc(2) = scrc(2) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   goto      nextpa
     C                   when      paretp = 'E'
     C                   eval      scar(3) = scar(3) + paamt
     C                   eval      scrc(3) = scrc(3) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   goto      nextpa
     C                   when      paretp = 'L'
     C                   eval      scar(4) = scar(4) + paamt
     C                   eval      scrc(4) = scrc(4) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   goto      nextpa
     C                   endsl
     C                   endif
     C                   endif
    C*
    C*** DEDUCTIBLE RECOVERIES
    C*
     C                   if        *in61 = *on
     C                             or pdtype <> '2'
     C                             and pdtype <> '3'
    C*
     C                   select
     C                   when      pabr2 = fncop1
     C                   eval      scar(1) = scar(1) + paamt
     C                   eval      scrc(1) = scrc(1) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   when      pabr2 = fnmep1
     C                   eval      scar(2) = scar(2) + paamt
     C                   eval      scrc(2) = scrc(2) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   when      pabr2 = fnexp1
     C                   eval      scar(3) = scar(3) + paamt
     C                   eval      scrc(3) = scrc(3) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   when      pabr2 = fnlep1
     C                   eval      scar(4) = scar(4) + paamt
     C                   eval      scrc(4) = scrc(4) + paamt
     C                   eval      scrcto = scrcto + paamt
     C                   eval      scarto = scarto + paamt
     C                   endsl
    C*
     C                   endif
     C                   endif
    C*
     C     nextpa        tag
    C*
     C     cmky          reade     ppa01                                  60
     C                   enddo
     *
     * ---------------------------------------------------------------
     *  DISPLAY SCREEN
     *
    C*
     C     screen        tag
    C*
     C                   exfmt     scr01
    C*
     C                   exsr      srfkey
     C                   enddo
    C*
     C                   move      *on           *inlr
     C                   return
    C*
    C*================================================================
    C*  SRFKEY - Function keys
    C*================================================================
     C     srfkey        begsr
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C                   eval      *in94 = *off
    C*** END CHG ****** 05/07/07 **********************************************
     C                   move      cmco#         #aco#
     C                   move      cmfnd         #afnd
     C                   move      cmfyr         #afyr
     C                   move      cmcase        #acase
     C                   move      *loval        kkseq#
    C*
     C                   if        @@mode = 'D'
     C                   move      *on           *in09
     C                   move      '1'           ##call
     C                   else
     C                   move      *off          *in09
     C                   move      ' '           ##call
     C                   endif
    C*
     C                   if        *in03 = *on
     C                   exsr      procsi
     C                   endif
    C*
     C                   select
    C*01 PAGE UP
     C                   when      *in01 = *on
     C                   exsr      pgdndo
     C                   goto      screen
    C*02 PAGE DOWN
     C                   when      *in02 = *on
     C                   eval      *in95 = *off
     C                   exsr      pgupdo
     C                   goto      screen
    C*
    C*F1
     C                   when      *inka = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C                   if        not xwwccall
     C*** not called from ww claims
     C                   call      'W0070R'
     C                   parm                    ##call
     C     *inkc         parm      *off          ##kc
     C     *inkl         parm      *off          ##kl
     C                   parm      @@mode        ##mode
     C                   parm      dbrrn         ##rrn
     C                   parm      *blanks       ##sort
     C                   parm      *zeros        ##rrn2
     C                   parm      *zeros        ##xfyr
     C                   parm      q1pgm         ##pgm
    C*
    C*       Enter=Exit to WWC
    C*
     C                   if        *inkc = *off and *inkl = *off
     C                   move      *off          *in11
     C                   endif
     C                   else
     C* if called from ww claims, seton 94 to display F1 not available
     C                   eval      *in94 = *on
     C                   endif                                                  not xwwccall
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F2
     C                   when      *inkb = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F2=Payment History
     C                   call      'W0576R'                             99
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##call
     C                   parm                    #aco#
     C                   parm                    #afnd
     C                   parm                    #afyr
     C                   parm                    #acase
     C                   parm      *blank        #apyst
     C                   parm      *zeros        #asfdt
     C                   parm      *zeros        #astdt
     C                   parm      *blanks       #aspc
     C                   parm      *blanks       #astt
     C                   parm      *blanks       #aspv1
     C                   parm      *blanks       #asrc
     C                   parm      *zeros        #asfei
     C                   parm      *zeros        #aschk
     C                   parm      *zeros        #aseob
     C                   parm      *blanks       #asirq
     C                   parm      *blank        #asgrp
     C***
     C***                clear                   qckey
     C***                move      cmco#         qcco#
     C***                move      cmfnd         qcfnd
     C***                move      cmfyr         qcfyr
     C***                move      cmcase        qccase
    C***
     C***                call      'SC010R'                             99
     C***                parm                    qckey
     C***                parm      'W0070R'      xxpgm
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F3
     C                   when      *inkc = *on
     C                   move      *on           *inlr
     C                   return
    C*
    C*F4
     C                   when      *inkd = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F4=Rsv/Pmt Summary
     C                   call      'WA044R'
     C                   parm      *blanks       ##rtcd
    C*
     C                   if        ##rtcd <> 'F'
     C                   if        *in09 = *off
     C                   move      'C'           ##mode
     C                   endif
     C                   if        *in09 = *on
     C                   move      'D'           ##mode
     C                   endif
     C                   call      'W0070R12'    acdpl                  99
     C                   endif
     C***
     C***                if        *in09 = *off
     C***                move      'C'           ##mode
     C***                endif
     C***                if        *in09 = *on
     C***                move      'D'           ##mode
     C***                endif
     C***                call      'W0070R09'    acdpl                  99
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F5
     C                   when      *inke = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F5=Reserve History
     C*
     C                   call      'W0574R1'                            99
     C                   parm                    ##prds
     C                   parm                    ##call
     C                   parm                    cmco#
     C                   parm                    cmfnd
     C                   parm                    cmss#
     C                   parm                    cmfyr
     C                   parm                    cmcase
     C                   parm      *zeros        ##efdt            8 0
     C                   parm                    ##rtcd
     C                   if        ##rtcd = 'A'
     C                   move      *on           *inkc
     C                   endif
     C
     C***                clear                   qckey
     C***                move      cmco#         qcco#
     C***                move      cmfnd         qcfnd
     C***                move      cmfyr         qcfyr
     C***                move      cmcase        qccase
    C***
     C***                call      'SC015R2'                            99
     C***                parm                    qckey
     C***                parm                    ##rtcd
     C***                if        ##rtcd = 'A'
     C***                move      *on           *inkc
     C***                endif
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F6
     C                   when      *inkf = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F6=Employer Info
     C                   if        *in09 = *off
     C                   move      'C'           ##mode
     C                   endif
     C                   if        *in09 = *on
     C                   move      'D'           ##mode
     C                   endif
     C                   call      'W0070R06'    acdpl                  99
     C***
     C***                eval      srdti = cmacdt
     C***                call      'SRCVTDAT'    dtmdyy
     C***                movel     srdto         ##acdt
    C***
     C***                call      'W0190R'                             99
     C***                parm                    ##prds
     C***                parm                    ##call
     C***                parm                    #aco#
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
     C***                parm                    ##acdt
     C***                parm      '1'           ##rtcd
     C***                if        ##rtcd = 'A'
     C***                move      *on           *inkc
     C***                endif
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F7
     C                   when      *inkg = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F7=Claimant Info
     C                   if        *in09 = *off
     C                   move      'C'           ##mode
     C                   endif
     C                   if        *in09 = *on
     C                   move      'D'           ##mode
     C                   endif
     C                   call      'W0070R07'    acdpl                  99
     C***
    C*** THIS PROCESS WILL APPEAR TO LOOK LIKE A W/W FUNCTION
    C***
     C***                dou       xxou54 = *on
    C***
     C***                call      'W0600R'                             99
     C***                parm      *blanks       ##prds
     C***                parm      *blanks       ##call
     C***                parm      cmfyr         ##fyr
     C***                parm      cmcase        ##case
     C***                parm      *zeros        ##req#
     C***                parm                    ##rtcd
     C***                parm      cmfnd         ##fnd
    C***
    C*** ##REQ# = SELECTION; ##RTCD = F6 ADD
    C***
     C***                if        ##req# <> *zeros
     C***                          or ##rtcd = '6'
     C***                move      'D'           ##mode
     C***                call      'W0100R'                             99
     C***  *inkc         parm                    ##kc
     C***  *inkl         parm                    ##kl
     C***                parm                    ##mode
     C***                parm      dbrrn         ##rrn
     C***                parm                    ##req#
    C***
     C***                if        *inkc = *on
     C***                move      *on           xxou54
     C***                endif
    C***
     C***                else
     C***                move      *on           xxou54
     C***                endif
     C***                enddo
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F8
     C                   when      *inkh = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F8=Accident Info
     C                   if        *in09 = *off
     C                   move      'C'           ##mode
     C                   endif
     C                   if        *in09 = *on
     C                   move      'D'           ##mode
     C                   endif
     C                   call      'W0070R08'    acdpl                  99
     C***
     C***                move      'A'           ##call
     C***                call      'W0080R'                             99
     C***                parm                    ##prds
     C***                parm                    ##call
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
     C***                parm                    ##fein
     C***                parm                    ##mod
     C***                parm                    ##frdt
     C***                parm                    ##todt
     C***                parm                    ##amt
     C***                parm                    ##eob#
     C***                parm      *blanks       ##pnam
     C***                parm      *blanks       ##trcd
     C***  *inkc         parm                    ##kc
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F9
    C                   when      *inki = *on
    C*** beg chg *** 05/05/06 ********************************
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F9=Management Info
     C                   if        *in09 = *off
     C                   move      'C'           ##mode
     C                   endif
     C                   if        *in09 = *on
     C                   move      'D'           ##mode
     C                   endif
     C                   call      'W0070R09'    acdpl                  99
     C***
    C***                call      'W0070R50'                           99
     C***  *inkc         parm                    ##kc
     C***  *inkl         parm                    ##kl
     C***                parm                    #aco#
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
     C***                parm                    ##call
     C***                parm      *blanks       ##eomb            9
    C*** END CHG ****** 05/07/07 **********************************************
      ****
    C****               call      'W8080R'                             99
    C****               parm      *blanks       ##prds
    C****               parm      *blank        ##call
    C****               parm      cmfnd         ##fnd
    C****               parm      cmfyr         ##fyr
    C****               parm      cmcase        ##case
    C**** *inkc         parm                    ##kc
    C*** end chg *** 05/05/06 ********************************
    C*
    C*F10
     C                   when      *inkj = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F10=Check Request
     C                   move      'A'           ##call
     C                   call      'W0080R'                             99
     C                   parm                    ##prds
     C                   parm                    ##call
     C                   parm                    #afnd
     C                   parm                    #afyr
     C                   parm                    #acase
     C                   parm                    ##fein
     C                   parm                    ##mod
     C                   parm                    ##frdt
     C                   parm                    ##todt
     C                   parm                    ##amt
     C                   parm                    ##eob#
     C                   parm      *blanks       ##pnam
     C                   parm      *blanks       ##trcd
     C     *inkc         parm                    ##kc
     C***
     C***                call      'W0576R'                             99
     C***  *inkc         parm                    ##kc
     C***  *inkl         parm                    ##kl
     C***                parm                    ##call
     C***                parm                    #aco#
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
     C***                parm      *blank        #apyst
     C***                parm      *zeros        #asfdt
     C***                parm      *zeros        #astdt
     C***                parm      *blanks       #aspc
     C***                parm      *blanks       #astt
     C***                parm      *blanks       #aspv1
     C***                parm      *blanks       #asrc
     C***                parm      *zeros        #asfei
     C***                parm      *zeros        #aschk
     C***                parm      *zeros        #aseob
     C***                parm      *blanks       #asirq
     C***                parm      *blank        #asgrp
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F11
     C                   when      *inkk = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F11=Reoccuring Comp
     C                   call      'W8080R'
     C                   parm      *blanks       ##prds
     C                   parm      *blank        ##call
     C                   parm      cmfnd         ##fnd
     C                   parm      cmfyr         ##fyr
     C                   parm      cmcase        ##case
     C     *inkc         parm                    ##kc
     C***
     C***                call      'WA044R'
     C***                parm      *blanks       ##rtcd
    C***
     C***                if        ##rtcd <> 'F'
     C***                if        *in09 = *off
     C***                move      'C'           ##mode
     C***                endif
     C***                if        *in09 = *on
     C***                move      'D'           ##mode
     C***                endif
     C***                call      'W0070R12'    acdpl                  99
     C***                endif
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F12
     C                   when      *inkl = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F12=Recovery Request
    C*** THIS PROCESS WILL APPEAR TO LOOK LIKE A W/W FUNCTION
    C***
     C                   dou       xxou54 = *on
    C***
     C                   call      'W0600R'                             99
     C                   parm      *blanks       ##prds
     C                   parm      *blanks       ##call
     C                   parm      cmfyr         ##fyr
     C                   parm      cmcase        ##case
     C                   parm      *zeros        ##req#
     C                   parm                    ##rtcd
     C                   parm      cmfnd         ##fnd
    C***
    C*** ##REQ# = SELECTION; ##RTCD = F6 ADD
    C***
     C                   if        ##req# <> *zeros
     C                             or ##rtcd = '6'
     C                   move      'D'           ##mode
     C                   call      'W0100R'                             99
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      dbrrn         ##rrn
     C                   parm                    ##req#
    C***
     C                   if        *inkc = *on
     C                   move      *on           xxou54
     C                   endif
    C***
     C                   else
     C                   move      *on           xxou54
     C                   endif
     C                   enddo
     C
     C*****              move      *on           *inlr
     C*****              return
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F13
     C***                when      *inkm = *on
    C*** beg chg *** 05/05/06 ********************************
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F13=DARTS Print
     C***                call      'SC960R'
     C***                parm      ' '           ##debug           1
     C***                parm      ' '           ##call
     C***                parm                    cmfnd
     C***                parm                    cmfyr
     C***                parm                    cmcase
     C***
     C***                call      'W0583R'                             99
     C***  *inkc         parm                    ##kc
     C***  *inkl         parm                    ##kl
     C***                parm                    ##call
     C***                parm                    #aco#
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*
    C****               call      'W0574R1'                            99
    C****               parm                    ##prds
    C****               parm                    ##call
    C****               parm                    cmco#
    C****               parm                    cmfnd
    C****               parm                    cmss#
    C****               parm                    cmfyr
    C****               parm                    cmcase
    C****               parm      *zeros        ##efdt
    C****               parm                    ##rtcd
    C****               if        ##rtcd = 'A'
    C****               move      *on           *inkc
    C****               endif
    C*** end chg *** 05/05/06 ********************************
    C*
    C*F14
     C                   when      *inkn = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F14=DARTS
     C                   clear                   qckey
     C                   move      cmco#         qcco#
     C                   move      cmfnd         qcfnd
     C                   move      cmfyr         qcfyr
     C                   move      cmcase        qccase
    C***
     C                   call      'SC010R'                             99
     C                   parm                    qckey
     C                   parm      q1pgm         xxpgm
     C***
     C***                if        *in09 = *off
     C***                move      'C'           ##mode
     C***                endif
     C***                if        *in09 = *on
     C***                move      'D'           ##mode
     C***                endif
     C***                call      'W0070R08'    acdpl                  99
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F15
     C                   when      *inkp = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F15=Dept Letters
     C                   eval      ##dept = 'CS'
    C*
     C                   call      'W4110R'                             99
     C                   parm                    ##prds
     C                   parm      '2'           ##call
     C                   parm                    ##rtcd
     C                   parm                    #afnd
     C                   parm                    #afyr
     C                   parm                    #acase
     C                   parm                    ##dept            2
    C*
     C                   if        ##rtcd = 'A'
     C                   move      *on           *inkc
     C                   endif
     C***
     C***                call      'W0573R'                             99
     C***                parm                    ##call
     C***                parm      ' '           ##pmtf
     C***  *inkc         parm      *off          ##kc
     C***  *inkl         parm      *off          ##kl
     C***                parm      dbrrn         ##rrn
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F16
     C                   when      *inkq = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F16=Legal Info
     C                   if        *in09 = *off
     C                   move      'C'           ##mode
     C                   endif
     C                   if        *in09 = *on
     C                   move      'D'           ##mode
     C                   endif
     C                   call      'W0070R16'    acdpl                  99
     C***
     C***                if        *in09 = *off
     C***                move      'C'           ##mode
     C***                endif
     C***                if        *in09 = *on
     C***                move      'D'           ##mode
     C***                endif
     C***                call      'W0070R07'    acdpl                  99
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F17
     C                   when      *inkr = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F17=Additional Address
     C                   call      'W0573R'                             99
     C                   parm                    ##call
     C                   parm      ' '           ##pmtf
     C     *inkc         parm      *off          ##kc
     C     *inkl         parm      *off          ##kl
     C                   parm      dbrrn         ##rrn
     C***
     C***                if        *in09 = *off
     C***                move      'C'           ##mode
     C***                endif
     C***                if        *in09 = *on
     C***                move      'D'           ##mode
     C***                endif
     C***                call      'W0070R06'    acdpl                  99
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F18
     C                   when      *inks = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F18=EDI
     C                   call      'W0070R5'                            99
     C                   parm                    ##prds
     C                   parm                    ##call
     C                   parm                    #aco#
     C                   parm                    #afnd
     C                   parm                    #afyr
     C                   parm                    #acase
     C                   parm                    ##rtcd
     C                   if        ##rtcd = 'A'
     C                   move      *on           *inkc
     C                   endif
     C***
     C***                if        *in09 = *off
     C***                move      'C'           ##mode
     C***                endif
     C***                if        *in09 = *on
     C***                move      'D'           ##mode
     C***                endif
     C***                call      'W0070R16'    acdpl                  99
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
    C*F19=No Use
     C                   when      *inkt = *on
     C                   eval      *in94 = *on
    C***
     C***                call      'W0070R5'                            99
     C***                parm                    ##prds
     C***                parm                    ##call
     C***                parm                    #aco#
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
     C***                parm                    ##rtcd
     C***                if        ##rtcd = 'A'
     C***                move      *on           *inkc
     C***                endif
    C*
    C*F20=No Use
     C                   when      *inku = *on
     C                   eval      *in94 = *on
    C***
     C***                call      'SC960R'
     C***                parm      ' '           ##call
     C***                parm                    cmfnd
     C***                parm                    cmfyr
     C***                parm                    cmcase
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F21
     C                   when      *inkv = *on
    C*
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F21=Diary Items
     C                   clear                   qckey
     C                   move      cmco#         qcco#
     C                   move      cmfnd         qcfnd
     C                   move      cmfyr         qcfyr
     C                   move      cmcase        qccase
    C***
     C                   call      'SC015R2'                            99
     C                   parm                    qckey
     C                   parm                    ##rtcd
     C                   if        ##rtcd = 'A'
     C                   move      *on           *inkc
     C                   endif
     C***
     C***                if        *in09 = *on
     C***                move      '1'           ##call
     C***                endif
     C***                if        *in09 = *off
     C***                move      ' '           ##call
     C***                endif
    C***
     C***                call      'W0970R'
     C***                parm                    ##call
     C***  *inkc         parm                    ##kc
     C***  *inkl         parm                    ##kl
     C***                parm                    cmss#
     C***                parm      dbrrn         ##rrn
    C***
     C***                if        *inkc = *on
     C***                move      *off          *in90
     C***                move      'D'           @@mode
     C***                endif
    C*** END CHG ****** 05/07/07 **********************************************
    C***
    C*F22
     C                   when      *inkw = *on
    C*** BEG CHG ****** 05/07/07 **********************************************
     C*** F22=Benefit Diary
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         ##acdt
    C***
     C                   call      'W0190R'                             99
     C                   parm                    ##prds
     C                   parm                    ##call
     C                   parm                    #aco#
     C                   parm                    #afnd
     C                   parm                    #afyr
     C                   parm                    #acase
     C                   parm                    ##acdt
     C                   parm      '1'           ##rtcd
     C                   if        ##rtcd = 'A'
     C                   move      *on           *inkc
     C                   endif
     C***
     C***                eval      ##dept = 'CS'
    C***
    C***                call      'W4110R'                             99
    C***                parm                    ##prds
    C***                parm      '2'           ##call
     C***                parm                    ##rtcd
     C***                parm                    #afnd
     C***                parm                    #afyr
     C***                parm                    #acase
     C***                parm                    ##dept            2
    C***
     C***                if        ##rtcd = 'A'
     C***                move      *on           *inkc
     C***                endif
    C*
    C*F23=No Use
     C***                when      *inkx = *on
    C***
     C***                call      'SC014R'
     C***                parm                    xdkey
     C***                parm                    c2seq#
     C***                parm                    cmpnam
     C***                parm      'D'           qqmode
    C*** END CHG ****** 05/07/07 **********************************************
    C*
    C*F24
     C                   when      *inky = *on
     C                   exsr      kydo
    C*
     C                   endsl
    C* F3=Exit to MM
     C                   if        *inkc = *on
     C                   move      *off          *in11
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C*  SRSPEC - Load special attributes
    C*================================================================
     C     srspec        begsr
    C*
     C                   eval      i = 0
     C                   eval      scsp = *blanks
    C*
    C* REINSURANCE
    C*
     C                   call      'WA010R'
     C                   parm                    @@rrn
     C                   parm                    xxfndr
     C                   parm                    xxexre
    C*
     C                   eval      scrret = xxfndr
    C*
     C                   if        xxexre = 'Y'
     C                   eval      i = i + 1
     C                   movea     xcrein        scsp(i)
     C                   endif
    C*
    C*** RECOVERY TYPE
    C*
     C                   if        cmsubr = 'Y'
    C***BEGIN ADD****** 07/13/10 **********************************************
     C                             OR cmsubr = 'R'
    C***END   ADD****** 07/13/10 **********************************************
     C                   eval      i = i + 1
     C                   movea     xcrect        scsp(i)
     C                   endif
    C*
    C*** MULTIPLE CLAIM
    C*
     C                   move      *all'0'       x@case
     C                   move      cmcase        x@case
     C     mcky          chain     pmc01                              99
    C*
     C                   if        *in99 = *off
     C                   eval      i = i + 1
     C                   movea     xcmulc        scsp(i)
     C                   endif
    C*
    C*** CONTROVERTED
    C*
     C                   if        cmcvfl = 'Y'
     C                   eval      i = i + 1
     C                   movea     xccont        scsp(i)
     C                   endif
    C*
    C*** BEG DEL ****** 05/07/07 ****************************************************
    C*** COMPANION CLAIMS
    C***
     C***  cmky          chain     pcc01                              99
    C***
     C***                if        *in99 = *off
     C***                eval      i = i + 1
     C***                movea     xccomc        scsp(i)
     C***                endif
    C*** END DEL ****** 05/07/07 ****************************************************
    C*
     C                   endsr
    C*
    C*================================================================
    C*  SRSTAT - Load status history dates
    C*================================================================
     C     srstat        begsr
     C                   clear                   xhopen
     C     cmky          chain     pch01                              99
    C*
     C                   dow       *in99 = *off
    C*
     C                   eval      srdti = chefdt
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel(p)  srdto         xxdt6
    C*
     C                   select
     C                   when      chocst = 'O'
     C                   if        xhopen <> 'Y'
     C                   move      xxdt6         scopdt
     C                   move      'Y'           xhopen
     C                   else
     C                   move      xxdt6         scrodt
     C                   endif
    C*
     C                   when      chocst = 'C'
     C                   move      xxdt6         sccldt
    C*
     C                   endsl
    C*
     C     cmky          reade     pch01                                  99
     C                   enddo
     C                   endsr
    C*
    C*================================================================
    C*  SRATNY - Load Attorney info
    C*================================================================
     C     sratny        begsr
    C*
     C***                clear                   scdefn
     C***                clear                   scplan
    C*
     C     cmky          chain     pll01                              99
     C                   if        *in99 = *off
    C*
     C                   if        lldefn <> *blanks
     C**                 movel(p)  lldefn        scdefn
     C                   else
    C*
     C                   if        lldefa <> *zeros
     C                   move      lldefa        kkfein
     C                   move      lldefm        kkmod
     C     pdky          chain     ppd01                              98
     C                   if        *in98 = *off
     C**                 movel(p)  pdname        scdefn
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   if        llplan <> *blanks
     C**                 movel(p)  llplan        scplan
     C                   else
    C*
     C                   if        llplaa <> *zeros
     C                   move      llplaa        kkfein
     C                   move      llplam        kkmod
     C     pdky          chain     ppd01                              98
     C                   if        *in98 = *off
     C**                 movel(p)  pdname        scplan
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   endif
     C                   endsr
    C*
    C*================================================================
    C*  SRSTAFF- Load Staff code info
    C*================================================================
     C     srstaf        begsr
    C*
     C                   clear                   scsupr
     C                   clear                   scclex
     C                   clear                   scuman
     C                   clear                   scsdfa
     C                   clear                   scnrse
    C*
     C     cmsupr        chain     pup01                              99
     C                   if        *in99 = *off
     C                   movel(p)  upusr         scsupr
     C                   endif
    C*
     C     cmclex        chain     pup01                              99
     C                   if        *in99 = *off
     C                   movel(p)  upusr         scclex
     C                   endif
     C     cmuman        chain     pup01                              99
     C                   if        *in99 = *off
     C                   movel(p)  upusr         scuman
     C                   endif
     C     cmsdfa        chain     pup01                              99
     C                   if        *in99 = *off
     C                   movel(p)  upusr         scsdfa
     C                   endif
     C     cmnrse        chain     pup01                              99
     C                   if        *in99 = *off
     C                   movel(p)  upusr         scnrse
     C                   endif
     C                   endsr
    C*
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
     C                   call      comapi        compl                  99
B01  C                   if        *in99 = *on
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval       ##data   = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm
     C                   endsr
    C*================================================================
    C*  GETPGT - Get program title for display heading
    C*================================================================
     C     getpgt        begsr
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
     C                   endsr
    C*===============================================================
    C* CHGNUM - SUBROUTINE TO CHANGE NUMBERS TO ALPHA WITHOUT ZEROS
    C*===============================================================
    C*
     C     chgnum        begsr
    C*
     C                   clear                   nm1
    C*
     C     '0'           check     xxnum         xxzero
     C     ' '           checkr    xxnum         xxlen
     C                   eval      xxblnk = xxzero - 1
     C                   movea     xxnum         nm1
    C*
     C                   do        xxblnk        x
     C                   move      ' '           nm1(x)
     C                   enddo
    C*
     C                   clear                   nm2
    C**
    C**         EDTMSK    WHEQ 'X,XXX.XX'
    C*
     C                   eval      w = xxlen
     C                   eval      y = xxlen / 3
     C                   eval      xxlen = xxlen + y
     C                   eval      xxlen = xxlen + 1
     C                   eval      z = xxlen
    C*
     C                   do        xxlen         x
    C*
     C                   if        x = 1
    C*
     C                   clear                   nm2(z)
    C*
     C                   else
    C*
     C                   if        nm1(w) <> *blank
    C*
     C     x             div       4             y
     C                   mvr                     y
    C*
     C                   if        y = *zero
    C*
     C                   if        x = 4
     C                   movea     '.'           nm2(z)
     C                   else
     C                   movea     ','           nm2(z)
     C                   endif
    C*
     C                   else
    C*
     C                   move      nm1(w)        nm2(z)
     C                   eval      w = w - 1
    C*
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   eval      z = z - 1
     *
     C                   enddo
     *
     * LEFT ADJUST AMOUNT
     *
     C                   movea(p)  nm2           xxnum
     C     ' '           check     xxnum         s
     C                   subst(p)  xxnum:s       xxnum
     *
     C                   endsr
    C*================================================================
    C*  BLDLNS - Builds options and functions lines
    C*================================================================
     C     bldlns        begsr
     C                   eval      ol#1 = ol#1 - ol#1
     C                   eval      ol#2 = ol#2 - ol#2
     C                   eval      fl#1 = fl#1 - fl#1
     C                   eval      fl#2 = fl#2 - fl#2
     C                   movea(p)  of            ##of
     C                   movea(p)  ofd           ##ofd
     C                   clear                   ##wwmo
B01  C                   if        q1prms > 0
     C                   move      ##call        ##wwmo
E01  C                   endif
     C                   call      'SROPTFNC'                           99
     C                   parm                    ##wwmo
     C                   parm                    ##of
     C                   parm                    ##ofd
     C                   parm                    ol
     C                   parm                    fl
     C                   parm                    vo
     C                   parm                    vf
     C                   parm      q1pgm         ##pgm
     C*
     C                   exsr      kydo
     C*
     C                   endsr
    C*================================================================
    C*  KYDO   - Process F24; more keys
    C*================================================================
     C     kydo          begsr
     C*
     C                   eval      fl#1 = fl#2 + 1
B01  C                   if        fl#1 > 14
     C                             or fl(fl#1) = *blanks
     C                   eval      fl#1 = 1
E01  C                   endif
     C                   eval      fl#2 = fl#1 + 1
     C                   movel     fl(fl#1)      scfkl1
     C                   movel     fl(fl#2)      scfkl2
     C*
     C*    'gF24'        scan      scfkl2        p                 2 0    05
B01  C*                  if        *in05 = *on
     C*                  movea     scfkl2        fc
     C*                  movea     xf24          fc(p)
     C*                  movea     fc            scfkl2
E01  C*                  endif
     C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C     siky          setgt     psi01                              96
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C     siky          setll     psi01                              96
     C                   endsr
    C*================================================================
    C*  PGDNDO - Process page down; get another page in subfile
    C*================================================================
     C     pgdndo        begsr
     C*
     C*** BEGIN CHG ****** 04/07/11 ************************************
     C*************      do        2             L                 2 0
     C                   eval      svseq# = svseq# + 1
     C                   eval      kkseq# = svseq#
     C                   exsr      setll
     C                   do        2             L                 2 0
     C*** END   CHG ****** 04/07/11 ************************************
     C     cmky          reade(n)  psi01                                  95
     C*
     C                   if        *in95 = *on and
     C                             sccom1 = *blanks and
     C                             sccom2 = *blanks and
     C                             l = 1 and kkseq# <> 1
     C*** BEGIN CHG ****** 04/07/11 ************************************
     C************       eval      svseq# = s1seq#
     C                   eval      svseq# = scseq1
     C*** END   CHG ****** 04/07/11 ************************************
     C                   endif
     C*
     C     L             ifeq      1
     C*** BEGIN DEL ****** 04/07/11 ************************************
     C************       eval      s1seq# = svseq#
     C*** END   DEL ****** 04/07/11 ************************************
     C  n95              eval      sccom1 = sicomm
     C  n95              eval      scseq1 = siseq#
     C   95              eval      sccom1 = *blanks
     C   95              eval      scseq1 = svseq#
     C*
     C                   else
     C  n95              eval      sccom2 = sicomm
     C  n95              eval      scseq2 = siseq#
     C   95              eval      sccom2 = *blanks
     C   95              eval      scseq2 = svseq#+1
     C                   endif
     C*** BEGIN ADD ****** 04/07/11 ************************************
     C                   if        *in95 = *off
     C                   eval      svseq# = siseq#
     C                   endif
     C*** END   ADD ****** 04/07/11 ************************************
     C                   enddo
     C*
     C                   endsr
    C*================================================================
    C*  PGUPDO - Process page up; get page for subfile
    C*================================================================
     C     pgupdo        begsr
     C*
     C*** BEGIN CHG ****** 04/07/11 ************************************
     C***********        eval      svseq# = s1seq# - 3
     C*
     C***********        if        svseq# < 0
     C***********        eval      svseq# = 0
     C***********        endif
     C*
     C**n95******        exsr      pgdndo
     C*
     C                   eval      kkseq# = svseq#
     C*
     C                   exsr      setgt
     C                   do        4             L                 2 0
     C     cmky          readpe(n) psi01                                  95
     C                   if        *in95 = *on
     C                   eval      svseq# = 0
     C                   leave
     C                   endif
     C                   enddo
     C*
     C  n95              eval      svseq# = siseq#-1
     C                   if        svseq# < 0
     C                   eval      svseq# = 0
     C                   endif
     C                   exsr      pgdndo
     C*
     C*** END   CHG ****** 04/07/11 ************************************
     C*
     C                   endsr
    C*================================================================
    C*  PROCSI - Process write to PSI01
    C*================================================================
     C     procsi        begsr
     C                   do        2             L
     C*
     C     L             ifeq      1
     C*** BEGIN CHG ****** 04/07/11 ************************************
     C************       eval      kkseq# = s1seq#
     C                   eval      kkseq# = scseq1
     C*** END   CHG ****** 04/07/11 ************************************
     C*
     C                   else
     C*** BEGIN CHG ****** 04/07/11 ************************************
     C************       eval      kkseq# = s1seq# + 1
     C                   eval      kkseq# = scseq2
     C*** END   CHG ****** 04/07/11 ************************************
     C                   endif
     C*
     C     siky          chain     psi01                              99
     C*
     C   99              eval      siseq# = kkseq#
     C*
     C     L             ifeq      1
     C                   eval      sicomm = sccom1
     C  n99              exsr      update
     C   99              exsr      write
     C*
     C                   else
     C                   eval      sicomm = sccom2
     C  n99              exsr      update
     C   99              exsr      write
     C                   endif
     C*
     C                   enddo
     C                   endsr
    C*================================================================
    C*  WRITE - Write PSI01
    C*================================================================
     C     write         begsr
     C*** BEGIN ADD ****** 04/07/11 ************************************
     C                   if        sicomm <> *blanks
     C*** END   ADD ****** 04/07/11 ************************************
     C                   eval      sico#  = cmco#
     C                   eval      sifnd  = cmfnd
     C                   eval      sifyr  = cmfyr
     C                   eval      sicase = cmcase
     C                   eval      siuser = q1user
     C                   eval      siiusr = q1user
     C                   eval      sipgm = q1pgm
     C                   eval      siipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sitime = dstime
     C                   eval      siitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         sidt
     C                   movel(p)  srdto         siidt
     C                   write     psi01
     C*** BEGIN ADD ****** 04/07/11 ************************************
     C                   endif
     C*** END   ADD ****** 04/07/11 ************************************
     C                   endsr
    C*================================================================
    C*  UPDATE - Update PSI01
    C*================================================================
     C     update        begsr
     C*** BEGIN ADD ****** 04/07/11 ************************************
     C                   if        sicomm = *blanks
     C                   delete    psi01
     c                   else
     C*** END   ADD ****** 04/07/11 ************************************
     C                   eval      siuser = q1user
     C                   eval      sipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sitime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         sidt
     C                   update    psi01
     C*** BEGIN ADD ****** 04/07/11 ************************************
     C                   endif
     C*** END   ADD ****** 04/07/11 ************************************
     C                   endsr
    C*================================================================
    C*  RMVOPT - remove options
    C*================================================================
     C     rmvopt        begsr
     C                   eval      xxopfn = '2'
     C                   eval      xxmis = 'NNN'
     C                   eval      xxnmbr = '23'
     C                   exsr      updofa
     C                   endsr
    C*===============================================================
    C* UPDOFA - Update values in the option/function array
    C*===============================================================
    C*  Before calling this subroutine make the following assignments:
    C*       XXOPFN = '1' if Option, '2' if Function
    C*       XXNMBR = Option/Function number to change
    C*            NOTE: Pass option/function numbers as 2 char fields.
    C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
    C*       XXMIS = 'XXX' where each 'X' is Y or N depending on change rqrd
    C*
    C*  This subroutine when called evaluates array OF for XXOPFN and checks
    C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
    C*  replaces 'XXX' with XXMIS.
    C*
     C     updofa        begsr
     C                   move      *off          found
     C                   eval      j = 1
    C*
    C*  Go through array looking for a match
    C*
B01  C                   dow       found = *off
     C                             and of(j) <> '3'
    C*
B02  C                   if        of(j) = xxopfn
     C                   movel     ofd(j)        dumnum
B03  C                   if        xxnmbr = dumnum
     C                   movel(p)  ofd(j)        dumofd
     C                   move      xxmis         dumofd
     C                   movel     dumofd        ofd(j)
     C                   move      *on           found
E03  C                   endif
E02  C                   endif
    C*
     C                   eval      j = j + 1
E01  C                   enddo
     C                   endsr
** Description--------------MISN
201F1=Work With Claims      YYN
202F2=Payment History       YYN
203F3=Exit                  YYN
204F4=Rsv/Pmt Summary       YYN
205F5=Reserve History       YYN
206F6=Employer Info         YYN
207F7=Claimant Info         YYN
208F8=Accident Info         YYN
209F9=Management Info       YYN
210F10=Check Request        YYN
211F11=Reoccuring Comp      YYN
212F12=Recovery Request     YYN
214F14=DARTS                YYN
215F15=Dept Letters         YYN
216F16=Legal Info           YYN
217F17=Additional Address   YYN
218F18=EDI                  YYN
219F19=                     NNN
220F20=                     NNN
221F21=Diary Items          YYN
222F22=Benefit Diary        YYN
223F23=                     NNN
3
