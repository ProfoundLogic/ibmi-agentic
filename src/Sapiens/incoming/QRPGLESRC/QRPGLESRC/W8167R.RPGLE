    H*====================================================================
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*====================================================================
    H/TITLE NGIF Reporting Extract - D process
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*                                                                   *
    H*===================================================================*
    H*                                                                   *
    H*  Called from: W8167C - Override                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*===================================================================*
    H*  Created on 07/29/2009 by c:62 for Project 114WTX                 *
    H*===================================================================*
    H*  Revisions:                                                       *
    H*                                                                   *
     H* 03/30/15 C:279 Project: 114WPR                                    *
     H*                Modified to use div 0 physical state (or mailing   *
     H*                state, if no physical state is present) when rep-  *
     H*                orting all divisions.                              *
     H*                                                                   *
     H* 06/18/14 C:53  SUPPORT: 85795
     H*                Need to loop through employers has div are split
    H*                                                                   *
     H* 12/27/11 C:062 PROJECT: WC114 78782                               *
     H*                -remove emp#=13 condition                          *
    H*                                                                   *
     H* 04/04/11 C:062 PROJECT: 76321                                     *
     H*                Add processing for employer 13.                    *
    H*                                                                   *
    F*====================================================================
    F*  F i l e s
    F*====================================================================
     F*
     Fwmfnl     if   e           k disk
     Fwdpal7    if   e           k disk
     Fwdpal5    if   e           k disk    rename(ppa01:ppa02)
     Fwdrpl     if   e           k disk
     Fwmcml     if   e           k disk    rename(pcm01:pcm02)
     Fwmcml1    if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmeml21   if   e           k disk
      ******** beg add ********** 03/30/15 ********************************
     fwmeml     if   e           k disk    rename(pem01:pem00)
     f                                     prefix(e_)
      ******** end add ********** 03/30/15 ********************************
     Fwmncl     if   e           k disk
     Fwmrhl     if   e           k disk
     Fwtabl     if   e           k disk    ignore(pnc01:ppa01:pcc01)
     F*
     Fwandp     if a e             disk    usropn
     Fwxndp     o    e             disk    rename(pnd01:pnd02) usropn
     F*
    D*====================================================================
    D*  Definition Specifications
    D*====================================================================
     D*
     D psds          esds                  extname(szq1p)
     D*
     D@@co#            S              3
     D@@medt           S              7
     D@@naic           S              5
     D@@medt7          S              7  0
     D##co#            S              3
     D##fnd            S              3
     D##medt           S              8
     D##dcidC          S             45
     D##dcidD          S             45
     Dreserves         S               n
     Dkkco#            S                   like(fnco#)
     Dkktrdt           S                   like(patrdt)
     Dkkunq#           S                   like(ccunq#)
     Dkkkey            S                   like(b2key)
     Dxxqbdt           S                   like(patrdt)
     Dxxybdt           S                   like(patrdt)
     DmonthDay         S              4  0
     Dmonth            S              2
     Dx                S              3  0
     DcurrDate         S              8  0
     DamtC             S             14  2
     DcountC           S              9  0
     Dbatch#           S              3S 0
     Dgroup            S              3a
     Demployer         S              9a
     Ddivision         S              5a
     Dyear             S              3a
     Dclaim            S              7a
      ******** beg add ********** 03/30/15 ********************************
     d strState        S              2a
      ******** end add ********** 03/30/15 ********************************

     Dxxnaic           S              9s 0
     Dkknaic           S                   like(emrsk#)

     Dxxlcpy           S             12  2 inz(0)
     Dxxsvrc           S             12  2 inz(0)
     Dxxunrc           S             12  2 inz(0)
     Dxxsbrc           S             12  2 inz(0)
     Dxxsirc           S             12  2 inz(0)
     Dxxrlrc           S             12  2 inz(0)
     Dxxotrc           S             12  2 inz(0)
     Dxxdcce           S             12  2 inz(0)
     Dxxderc           S             12  2 inz(0)
     Dxxlosc           S             12  2 inz(0)
     Dxxdcc            S             12  2 inz(0)

     Dqdlcpy           S             12  2 inz(0)
     Dqdsvrc           S             12  2 inz(0)
     Dqdunrc           S             12  2 inz(0)
     Dqdsbrc           S             12  2 inz(0)
     Dqdsirc           S             12  2 inz(0)
     Dqdrlrc           S             12  2 inz(0)
     Dqdotrc           S             12  2 inz(0)
     Dqddcce           S             12  2 inz(0)
     Dqdderc           S             12  2 inz(0)
     Dqdlosc           S             12  2 inz(0)
     Dqddcc            S             12  2 inz(0)

     Dydlcpy           S             12  2 inz(0)
     Dydsvrc           S             12  2 inz(0)
     Dydunrc           S             12  2 inz(0)
     Dydsbrc           S             12  2 inz(0)
     Dydsirc           S             12  2 inz(0)
     Dydrlrc           S             12  2 inz(0)
     Dydotrc           S             12  2 inz(0)
     Dyddcce           S             12  2 inz(0)
     Dydderc           S             12  2 inz(0)

     Didlcpy           S             12  2 inz(0)
     Didsvrc           S             12  2 inz(0)
     Didunrc           S             12  2 inz(0)
     Didsbrc           S             12  2 inz(0)
     Didsirc           S             12  2 inz(0)
     Didrlrc           S             12  2 inz(0)
     Didotrc           S             12  2 inz(0)
     Diddcce           S             12  2 inz(0)
     Didderc           S             12  2 inz(0)
     D*
     D srdti           S              8  0
     D srdto           S             10
     D srerr           S              1
     D srffmt          S              8
     D srtfmt          S              8

     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2

     D                 DS                  INZ
     D  RTYP                   1      8
     D                                     DIM(4)
     D  RT01                   1      2    inz('CO')
     D  RT02                   3      4    inz('ME')
     D  RT03                   5      6    inz('EX')
     D  RT04                   7      8    inz('RE')
     D                 DS                  INZ
     D  RAMT                   1     36  2
     D                                     DIM(4)
     D  SU01                   1      9  2
     D  SU02                  10     18  2
     D  SU03                  19     27  2
     D  SU04                  28     36  2
     D                 DS                  INZ
     D  PAMT                   1     36  2
     D                                     DIM(4)
     D  PA01                   1      9  2
     D  PA02                  10     18  2
     D  PA03                  19     27  2
     D  PA04                  28     36  2
     D*
     D*ie****          s              1    dim(999)
    D*
     D W8166R         uds
     D xxbatch#                       3A
     C*
     D                 ds
     D document                      45
     D  iiCoNumber                    5  0 overlay(document)
     D  ifundReceiver                 1    overlay(document:6)
     D  ifromState                    2    overlay(document:7)
     D  ifromLocation                 2  0 overlay(document:9)
     D  itoState                      2    overlay(document:11)
     D  itoLocation                   2  0 overlay(document:13)
     D  ibatch#                       3  0 overlay(document:15)
     D  ireportingDat                 8  0 overlay(document:18)
     D  ifromDate                     8  0 overlay(document:26)
     D  itoDate                       8  0 overlay(document:34)
     D  iext                          4    overlay(document:42) inz('.txt')
     C*
     D                 ds
     D header                        65
     D  hType                        20    overlay(header)
     D  hiCoNumber                    5  0 overlay(header:21)
     D  hfundReceiver                 1    overlay(header:26)
     D  hfromState                    2    overlay(header:27)
     D  hfromLocation                 2  0 overlay(header:29)
     D  htoState                      2    overlay(header:31)
     D  htoLocation                   2  0 overlay(header:33)
     D  hbatch#                       3  0 overlay(header:35)
     D  hreportingDat                 8  0 overlay(header:38)
     D  hfromDate                     8  0 overlay(header:46)
     D  htoDate                       8  0 overlay(header:54)
     D  hinsType                      3    overlay(header:62)
     D  hreplacement                  1    overlay(header:65)
     C*
     D                 ds
     D trailerD                    1702
     D  dType                        20    overlay(trailerD)
     D  diCoNumber                    5  0 overlay(trailerD:21)
     D  dfundReceiver                 1    overlay(trailerD:26)
     D  dfromState                    2    overlay(trailerD:27)
     D  dfromLocation                 2  0 overlay(trailerD:29)
     D  dtoState                      2    overlay(trailerD:31)
     D  dtoLocation                   2  0 overlay(trailerD:33)
     D  dbatch#                       3  0 overlay(trailerD:35)
     D  dreportingDat                 8  0 overlay(trailerD:38)
     D  dfromDate                     8  0 overlay(trailerD:46)
     D  dtoDate                       8  0 overlay(trailerD:54)
     D  dinsType                      3    overlay(trailerD:62)
     D  drecordCount                  9  0 overlay(trailerD:65)
     D  dtotCurRes1                  15    overlay(trailerD:74)
     D  dtotCurRes2                  15    overlay(trailerD:89)
     D  dtotCurQExp1                 15    overlay(trailerD:104)
     D  dtotCurQExp2                 15    overlay(trailerD:119)
     D  dtotCurQRev                  15    overlay(trailerD:134)
     D  dtotYTDExp1                  15    overlay(trailerD:149)
     D  dtotYTDExp2                  15    overlay(trailerD:164)
     D  dtotYTDRev                   15    overlay(trailerD:179)
     D  dtotITDExp1                  15    overlay(trailerD:194)
     D  dtotITDExp2                  15    overlay(trailerD:209)
     D  dtotITDRev                   15    overlay(trailerD:224)
     D  dFiller                    1464    overlay(trailerD:239)
     D*
     D                 ds
     D amount12                      12
     D  number12                     11  2 overlay(amount12)
     D  sign12                        1    overlay(amount12:12)
     D*
     D                 ds
     D amount13                      13
     D  number13                     12  2 overlay(amount13)
     D  sign13                        1    overlay(amount13:13)
     D*
     D                 ds
     D amount15                      15
     D  number15                     14  2 overlay(amount15)
     D  sign15                        1    overlay(amount15:15)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*
     D detailc       e ds                  extname(wancp)
     D detailD       e ds                  extname(wandp)
    C*--------------------------------------------------------------------
    C*  Parameter Lists                                                  *
    C*--------------------------------------------------------------------
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@medt
     C                   parm                    @@naic
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyym2        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
    C*--------------------------------------------------------------------
    C*  Key Lists                                                        *
    C*--------------------------------------------------------------------
     C*
     C     b2ky          klist
     C                   kfld                    paco#
     C                   kfld                    kkkey
     C*
     C     rtky          klist
     C                   kfld                    rhco#
     C                   kfld                    kkkey
     C*
     C     ccky          klist
     C                   kfld                    kkunq#
     C*
     C     cmky1         klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C*
     C     cmky2         klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    ememp#
     C                   kfld                    emdiv
     C*
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kknaic
     C*
     C     fnky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C*
     C     rhky1         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    kktrdt
     C*
     C     rhky2         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C     rpky          klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C                   kfld                    pachk#
     C                   kfld                    patrdt
     C                   kfld                    paseq#
     C*
     C     paky1         klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    kktrdt
     C*
     C     paky2         klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C*
     C     paky3         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    kktrdt
     C*
     C     paky4         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
    C*====================================================================
     C*  Main Line Logic
    C*====================================================================
     C*
     C                   open      wandp
     C                   open      wxndp
     C*
     C                   time                    dsdttm
     C                   move      @@co#         kkco#
     C                   move      @@medt        @@medt7
     C                   move      @@medt        kktrdt
     C                   eval      countC=3
     C                   eval      amtC=0
     C                   move      xxbatch#      batch#
     C                   eval      batch#=batch#+1
     C*
     c                   exsr      getDates
     C*
     C*  Get Employr record asociated with NAIC #
     C*
     C                   move      @@naic        xxnaic
     C                   movel     xxnaic        kknaic
     C*
     C     emky          chain     pem01
     C*** beg chg *** 06/18/14 **********************************
     c                   dow       not %eof(wmeml21)
     C***************    if        %found
     C*** end chg *** 06/18/14 **********************************
     C     fnky          chain     pfn01                              99
     C*
     c     paky1         setgt     ppa01
     c     paky2         readpe    ppa01
     c                   dow       not %eof(wdpal7)
     c     cmky1         chain     pcm01
     c                   if        %found and cmemp#=ememp# and cmdiv=emdiv
     c     cmss#         chain     pcd01
     c     rpky          chain     prp01
     c                   movel(p)  cmpds#        kkunq#
     c     ccky          chain     pcc01                              99
     c                   if        %found(WMNCL)
     c                   exsr      accTotalsPmt
     c                   endif
     c                   endif
     c     paky2         readpe    ppa01
     c                   enddo
     c*
     c     cmky2         chain     pcm02
     c                   if        %found
     c                   dou       %eof(wmcml)
     c     cmss#         chain     pcd01
     c                   movel(p)  cmpds#        kkunq#
     c     ccky          chain     pcc01                              99
     c                   if        %found(WMNCL)
     c                   exsr      accTotalsRsv
     c                   endif
     c     cmky2         reade     pcm02
     c                   enddo
     C                   endif
     C*** beg chg *** 06/18/14 **********************************
     C     emky          reade     pem01
     C                   enddo
     C****************** endif
     C*** beg chg *** 06/18/14 **********************************
     c*
     c                   exsr      writeWandp
     c                   exsr      writeWxndp
     C                   exsr      genDocID
     C*
     C                   close     wandp
     C                   close     wxndp
     C*
     C                   call      'W8166C1'
     C                   parm      *blanks       ##dcidC
     C                   parm                    ##dcidD
     C*
     C                   move      batch#        xxbatch#
     C*
     C                   seton                                        lr
    C*====================================================================
    C*  genDocID - Generate the document ids
    C*====================================================================
     C     genDocID      begsr
     C*
     C                   move      @@naic        iiCoNumber
     C                   eval      ifundReceiver='D'
     C                   eval      ifromState=fnpst
     C                   eval      ifromLocation=11
      ************* beg chg ******** 03/30/15 ****************************
     C******             eval      itoState=empst
      /free
       Exsr getState;
       itoState = strState;
      /end-free
      ************* end chg ******** 03/30/15 ****************************
     C                   eval      itoLocation=01
     C                   eval      ibatch#=batch#
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         ireportingDat
     C*
     C                   z-add     xxqbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ifromDate
     C*
     C                   z-add     @@medt7       srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         itoDate
     C*
     C                   eval      ##dcidD=document
     C*
     C                   endsr
    C*====================================================================
    C*  getDates - Get Begin Dates
    C*====================================================================
     C     getDates      BEGSR
     C*
     C* Quarter Begin Date
     C*
     C                   eval      xxqbdt=@@medt7
     C                   move      @@medt7       monthDay
     C*
     C                   select
     C                   when      monthDay >=  0101 and monthDay <= 0331
     c                   move      '0101'        xxqbdt
     C                   when      monthDay >=  0401 and monthDay <= 0630
     c                   move      '0401'        xxqbdt
     C                   when      monthDay >=  0701 and monthDay <= 0930
     c                   move      '0701'        xxqbdt
     C                   when      monthDay >=  1001 and monthDay <= 1231
     c                   move      '1001'        xxqbdt
     C                   endsl
     C*
     C* Year Begin Date
     C*
     C                   eval      xxybdt=@@medt7
     C                   move      '0101'        xxybdt
     C*
     C                   ENDSR
    C*====================================================================
    C*  accTotalsRsv - Accumulate Reserve Totals
    C*====================================================================
     C     accTotalsRsv  begsr
     C*
     C                   clear                   ramt
     C                   clear                   pamt
     C*
     c     rhky1         setgt     prh01
     c     rhky2         readpe    prh01                                  99
     c                   dow       not *in99
     c*
     c                   eval      ramt(1)=ramt(1)+rsu(3)
     c                   eval      ramt(2)=ramt(2)+rsu(1)
     c                   eval      ramt(3)=ramt(3)+rsu(2)+rsu(4)
     c                   eval      ramt(4)=ramt(4)+rsu(5)
     c*
     c     rhky2         readpe    prh01                                  99
     c                   enddo
     C*
     c     paky3         setgt     ppa02
     c     paky4         readpe    ppa02                                  99
     c                   dow       not *in99
     c*
     C                   if        patype='V' or patype='S' or patype='R'
     C                   eval      paamt=paamt*-1
     C                   endif
     c*
     c                   select
     c                   when      parecd='CO'
     c                   eval      pamt(1)=pamt(1)+paamt
     c                   when      parecd='EX' or parecd='LE'
     c                   eval      pamt(3)=pamt(3)+paamt
     c                   when      parecd='ME'
     c                   eval      pamt(2)=pamt(2)+paamt
     c                   when      parecd='RE'
     c                   eval      pamt(4)=pamt(4)+paamt
     c                   endsl
     c*
     c     paky4         readpe    ppa02                                  99
     c                   enddo
     c*
     c                   eval      xxlosc=(ramt(1)-pamt(1)) + (ramt(2)-pamt(2))
     c                                    + (ramt(4)-pamt(4))
     c*
     c                   eval      xxdcc=ramt(3)-pamt(3)
     c*
     c                   eval      qdlosc=qdlosc+xxlosc
     c                   eval      qddcc=qddcc+xxdcc
     C*
     C                   endsr
    C*====================================================================
    C*  accTotalsPmt - Accumulate Payment Totals
    C*====================================================================
     C     accTotalsPmt  BEGSR
     C*
     C                   eval      xxlcpy=*zeros
     C                   eval      xxsbrc=*zeros
     C                   eval      xxrlrc=*zeros
     C                   eval      xxotrc=*zeros
     C                   eval      xxdcce=*zeros
     C                   eval      xxderc=*zeros
     C*
     C                   eval      kkkey=pabr2
     C     b2ky          chain     pb201
     C                   if        not %found
     C                   clear                   b2minr
     C                   endif
     C*
     C                   if        patype='V' or patype='S' or patype='R'
     C                   eval      paamt=paamt*-1
     C                   endif
     C*
     C                   if        paretp='C' or paretp='M' or parecd='RE'
     C                   if        pabr2='CQ' or pabr2='EQ' or pabr2='MQ' or
     C                             pabr2='LQ' or pabr2='P1' or pabr2='P2' or
     C                             pabr2='59' or pabr2='60' or pabr2='61' or
     C                             pabr2='62' or pabr2='63'
     C*
     C***                select
     C***                when      b2minr='082'
     C***                eval      xxsvrc=paamt
     C***                when      b2minr='081'
     C***                eval      xxsbrc=paamt
     C***                when      b2minr='085'
     C***                eval      xxsirc=paamt
     C***                other
     C***                eval      xxotrc=paamt
     C***                endsl
     C*
     C                   select
     C                   when      pabr2='CQ' or pabr2='EQ' or pabr2='MQ' or
     C                             pabr2='LQ'
     C                   eval      xxrlrc=paamt
     C                   when      pabr2='P1' or pabr2='P2' or pabr2='59' or
     C                             pabr2='60' or pabr2='61' or pabr2='63'
     C                   eval      xxsbrc=paamt
     C                   when      pabr2='62'
     C                   eval      xxotrc=paamt
     C                   endsl
     C*
     C                   else
     C                   eval      xxlcpy=paamt
     C                   endif
     C                   endif
     C*
     C                   if        (paretp='E' and parecd<>'RE') or paretp='L'
     C                   if        pabr2='CQ' or pabr2='EQ' or pabr2='MQ' or
     C                             pabr2='LQ' or pabr2='P1' or pabr2='P2' or
     C                             pabr2='59' or pabr2='60' or pabr2='61' or
     C                             pabr2='62' or pabr2='63'
     C*** Begin Add *** 04/04/11 *******************************************
     C*** Begin Chg *** 12/27/11 *******************************************
     C**********         if        ememp#=13 and (pabr2='CQ' or pabr2='EQ'
     C**********                      or pabr2='MQ' or pabr2='LQ')
     C                   if        (pabr2='CQ' or pabr2='EQ'
     C                                or pabr2='MQ' or pabr2='LQ')
     C*** End   Chg *** 12/27/11 *******************************************
     C                   eval      xxrlrc=paamt
     C                   else
     C*** End   Add *** 04/04/11 *******************************************
     C                   eval      xxderc=paamt
     C*** Begin Add *** 04/04/11 *******************************************
     C                   endif
     C*** End   Add *** 04/04/11 *******************************************
     C                   else
     C                   eval      xxdcce=paamt
     C                   endif
     C                   endif
     C*
     C                   select
     C                   when      patrdt>=xxqbdt
     C*
     C                   eval      qdlcpy=qdlcpy+xxlcpy
     C                   eval      ydlcpy=ydlcpy+xxlcpy
     C                   eval      idlcpy=idlcpy+xxlcpy
     C*
     C                   eval      qddcce=qddcce+xxdcce
     C                   eval      yddcce=yddcce+xxdcce
     C                   eval      iddcce=iddcce+xxdcce
     C*
     C                   eval      qdderc=qdderc+xxderc
     C                   eval      ydderc=ydderc+xxderc
     C                   eval      idderc=idderc+xxderc
     C*
     C***                eval      qdunrc=qdunrc+xxunrc
     C***                eval      ydunrc=ydunrc+xxunrc
     C***                eval      idunrc=idunrc+xxunrc
     C*
     C                   eval      qdsbrc=qdsbrc+xxsbrc
     C                   eval      ydsbrc=ydsbrc+xxsbrc
     C                   eval      idsbrc=idsbrc+xxsbrc
     C*
     C                   eval      qdrlrc=qdrlrc+xxrlrc
     C                   eval      ydrlrc=ydrlrc+xxrlrc
     C                   eval      idrlrc=idrlrc+xxrlrc
     C*
     C                   eval      qdotrc=qdotrc+xxotrc
     C                   eval      ydotrc=ydotrc+xxotrc
     C                   eval      idotrc=idotrc+xxotrc
     C*
     C                   when      patrdt>=xxybdt and patrdt<xxqbdt
     C*
     C                   eval      ydlcpy=ydlcpy+xxlcpy
     C                   eval      idlcpy=idlcpy+xxlcpy
     C*
     C                   eval      ydotrc=ydotrc+xxotrc
     C                   eval      idotrc=idotrc+xxotrc
     C*
     C                   eval      yddcce=yddcce+xxdcce
     C                   eval      iddcce=iddcce+xxdcce
     C*
     C                   eval      ydderc=ydderc+xxderc
     C                   eval      idderc=idderc+xxderc
     C*
     C***                eval      ydunrc=ydunrc+xxunrc
     C***                eval      idunrc=idunrc+xxunrc
     C*
     C                   eval      ydsbrc=ydsbrc+xxsbrc
     C                   eval      idsbrc=idsbrc+xxsbrc
     C*
     C                   eval      ydrlrc=ydrlrc+xxrlrc
     C                   eval      idrlrc=idrlrc+xxrlrc
     C*
     C                   when      patrdt<xxybdt
     C*
     C                   eval      idlcpy=idlcpy+xxlcpy
     C*
     C                   eval      idotrc=idotrc+xxotrc
     C*
     C                   eval      iddcce=iddcce+xxdcce
     C*
     C                   eval      idderc=idderc+xxderc
     C*
     C***                eval      idunrc=idunrc+xxunrc
     C*
     C                   eval      idsbrc=idsbrc+xxsbrc
     C*
     C                   eval      idrlrc=idrlrc+xxrlrc
     C*
     C                   endsl
     C*
     C                   endsr
    C*====================================================================
    C*  writeWandp - Write to WANDP
    C*====================================================================
     C     writeWandp    BEGSR
     C*
     C                   clear                   pnd01
     C                   eval      ndtype='D'
     C                   move      @@naic        ndicmp
     C                   eval      ndflst=fnpst
     C                   eval      ndflcd='11'
     C                   z-add     @@medt7       srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ndredt
     C                   movel(p)  fnname        ndfnam
     C***                movel(p)  fnmad1        ndfad1
     C***                movel(p)  fnmad2        ndfad2
     C***                movel(p)  fnmcty        ndfcty
     C***                movel(p)  fnmst         ndfst
     C***                movel(p)  fnmzip        ndfzip
     C***                movel(p)  *blanks       ndfcon
     C***                movel(p)  *blanks       ndtitl
     C***                movel(p)  fnph1a        ndphn#
     C***                move      fnph1         ndphn#
     C***                movel     *blanks       ndeadd
     C                   eval      ndfad1='Capitol Associates Building'
     C                   eval      ndfad2='901 North Seventh Street'
     C                   eval      ndfcty='Harrisburg'
     C                   eval      ndfst ='PA'
     C                   eval      ndfzip=171020000
     C                   eval      ndfcon='Vicki Flowers'
     C                   eval      ndtitl='Fiscal Assistant'
     C                   eval      ndphn#=7177838093
     C                   eval      ndeadd='vflowers@state.pa.us'
     C                   eval      ndcaac='O'
     C*
     C                   eval      NDLCPY='000000000000+'
     C                   eval      NDTLRC='000000000000+'
     C                   eval      NDMTRC='000000000000+'
     C                   eval      NDSVRC='000000000000+'
     C                   eval      NDSBRC='000000000000+'
     C                   eval      NDPDRC='000000000000+'
     C                   eval      NDSIRC='000000000000+'
     C                   eval      NDNWRC='000000000000+'
     C                   eval      NDOTRC='000000000000+'
     C                   eval      NDUNRC='000000000000+'
     C                   eval      NDRLRC='000000000000+'
     C                   eval      NDUPCP='000000000000+'
     C                   eval      NDTLAE='000000000000+'
     C                   eval      NDDCCE='000000000000+'
     C                   eval      NDDERC='000000000000+'
     C                   eval      NDAOEX='000000000000+'
     C                   eval      NDAERC='000000000000+'
     C                   eval      NDUPRC='000000000000+'
     C                   eval      NDRES2='000000000000+'
     C                   eval      NDULAE='000000000000+'
     C                   eval      NDCAEX='000000000000+'
     C                   eval      NDMBRV='000000000000+'
     C                   eval      NDECEX='000000000000+'
     C                   eval      NDLEGL='000000000000+'
     C                   eval      NDPEX ='000000000000+'
     C                   eval      NDTMEX='000000000000+'
     C                   eval      NDORU ='000000000000+'
     C                   eval      NDGOE ='000000000000+'
     C                   eval      NDIBF ='000000000000+'
     C                   eval      NDIMF ='000000000000+'
     C                   eval      NDOE1 ='000000000000+'
     C                   eval      NDOE2 ='000000000000+'
     C                   eval      NDOE3 ='000000000000+'
     C                   eval      NDOE4 ='000000000000+'
     C                   eval      NCRFS1='000000000000+'
     C                   eval      NDSPDP='000000000000+'
     C                   eval      NDREDI='000000000000+'
     C                   eval      NDLDRE='000000000000+'
     C                   eval      NDOR1 ='000000000000+'
     C                   eval      NDOR2 ='000000000000+'
     C                   eval      NDOR3 ='000000000000+'
     C                   eval      NDOR4 ='000000000000+'
     C                   eval      NDLOSC='000000000000+'
     C                   eval      NDUNPR='000000000000+'
     C                   eval      NDDCC ='000000000000+'
     C                   eval      NDAOE2='000000000000+'
     C                   eval      NDLSDE='000000000000+'
     C                   eval      NDLSAE='000000000000+'
     C*
     C     1             do        3             x
     C*
     C                   select
     C                   when      x=1
     C                   eval      ndpcov='1'
     C*
     C                   if        qdlcpy<0
     C                   eval      sign13='-'
     C                   eval      number13=qdlcpy*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qdlcpy
     C                   endif
     C                   move      amount13      ndlcpy
     C*
     C                   if        qdotrc<0
     C                   eval      sign13='-'
     C                   eval      number13=qdotrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qdotrc
     C                   endif
     C                   move      amount13      ndotrc
     C*
     C***                if        qdunrc<0
     C***                eval      sign13='-'
     C***                eval      number13=qdunrc*-1
     C***                else
     C***                eval      sign13='+'
     C***                eval      number13=qdunrc
     C***                endif
     C***                move      amount13      ndunrc
     C*
     C                   if        qdsbrc<0
     C                   eval      sign13='-'
     C                   eval      number13=qdsbrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qdsbrc
     C                   endif
     C                   move      amount13      ndsbrc
     C*
     C                   if        qdrlrc<0
     C                   eval      sign13='-'
     C                   eval      number13=qdrlrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qdrlrc
     C                   endif
     C                   move      amount13      ndrlrc
     C*
     C                   if        qddcce<0
     C                   eval      sign13='-'
     C                   eval      number13=qddcce*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qddcce
     C                   endif
     C                   move      amount13      nddcce
     C*
     C                   if        qdderc<0
     C                   eval      sign13='-'
     C                   eval      number13=qdderc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qdderc
     C                   endif
     C                   move      amount13      ndderc
     C*
     C                   if        qdlosc<0
     C                   eval      sign13='-'
     C                   eval      number13=qdlosc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qdlosc
     C                   endif
     C                   move      amount13      ndlosc
     C*
     C                   if        qddcc<0
     C                   eval      sign13='-'
     C                   eval      number13=qddcc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=qddcc
     C                   endif
     C                   move      amount13      nddcc
     C*
     C                   when      x=2
     C                   eval      ndpcov='2'
     C*
     C                   if        ydlcpy<0
     C                   eval      sign13='-'
     C                   eval      number13=ydlcpy*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=ydlcpy
     C                   endif
     C                   move      amount13      ndlcpy
     C*
     C                   if        ydotrc<0
     C                   eval      sign13='-'
     C                   eval      number13=ydotrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=ydotrc
     C                   endif
     C                   move      amount13      ndotrc
     C*
     C***                if        ydunrc<0
     C***                eval      sign13='+'
     C***                eval      number13=ydunrc*-1
     C***                else
     C***                eval      sign13='-'
     C***                eval      number13=ydunrc
     C***                endif
     C***                move      amount13      ndunrc
     C*
     C                   if        ydsbrc<0
     C                   eval      sign13='-'
     C                   eval      number13=ydsbrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=ydsbrc
     C                   endif
     C                   move      amount13      ndsbrc
     C*
     C                   if        ydrlrc<0
     C                   eval      sign13='-'
     C                   eval      number13=ydrlrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=ydrlrc
     C                   endif
     C                   move      amount13      ndrlrc
     C*
     C                   if        yddcce<0
     C                   eval      sign13='-'
     C                   eval      number13=yddcce*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=yddcce
     C                   endif
     C                   move      amount13      nddcce
     C*
     C                   if        ydderc<0
     C                   eval      sign13='-'
     C                   eval      number13=ydderc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=ydderc
     C                   endif
     C                   move      amount13      ndderc
     C*
     C                   eval      sign13='+'
     C                   eval      number13=0
     C                   move      amount13      ndlosc
     C*
     C                   eval      sign13='+'
     C                   eval      number13=0
     C                   move      amount13      nddcc
     C*
     C                   when      x=3
     C                   eval      ndpcov='3'
     C*
     C                   if        idlcpy<0
     C                   eval      sign13='-'
     C                   eval      number13=idlcpy*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=idlcpy
     C                   endif
     C                   move      amount13      ndlcpy
     C*
     C                   if        idotrc<0
     C                   eval      sign13='-'
     C                   eval      number13=idotrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=idotrc
     C                   endif
     C                   move      amount13      ndotrc
     C*
     C***                if        idunrc<0
     C***                eval      sign13='+'
     C***                eval      number13=idunrc*-1
     C***                else
     C***                eval      sign13='-'
     C***                eval      number13=idunrc
     C***                endif
     C***                move      amount13      ndunrc
     C*
     C                   if        idsbrc<0
     C                   eval      sign13='-'
     C                   eval      number13=idsbrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=idsbrc
     C                   endif
     C                   move      amount13      ndsbrc
     C*
     C                   if        idrlrc<0
     C                   eval      sign13='-'
     C                   eval      number13=idrlrc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=idrlrc
     C                   endif
     C                   move      amount13      ndrlrc
     C*
     C                   if        iddcce<0
     C                   eval      sign13='-'
     C                   eval      number13=iddcce*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=iddcce
     C                   endif
     C                   move      amount13      nddcce
     C*
     C                   if        idderc<0
     C                   eval      sign13='-'
     C                   eval      number13=idderc*-1
     C                   else
     C                   eval      sign13='+'
     C                   eval      number13=idderc
     C                   endif
     C                   move      amount13      ndderc
     C*
     C                   eval      sign13='+'
     C                   eval      number13=0
     C                   move      amount13      ndlosc
     C*
     C                   eval      sign13='+'
     C                   eval      number13=0
     C                   move      amount13      nddcc
     C*
     C                   endsl
     C*
     C                   write     pnd01
     C                   enddo
     C*
     C                   endsr
    C*====================================================================
    C*  write_D_Header - Populate and Write D Header Record
    C*====================================================================
     C     write_D_Headerbegsr
     C*
     C                   clear                   header
     C*
     C                   eval      hType='HEADER02'
     C                   move      @@naic        hiCoNumber
     C                   eval      hfundReceiver='D'
     C                   eval      hfromState=fnpst
     C                   eval      hfromLocation=11
     C                   eval      htoState=empst
     C                   eval      htoLocation=01
     C                   eval      hbatch#=batch#
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         hreportingDat
     C*
     C                   z-add     xxqbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         hfromDate
     C*
     C                   z-add     @@medt7       srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         htoDate
     C*
     C                   eval      hinsType='P&C'
     C                   eval      hreplacement='N'
     C*
     C                   endsr
    C*====================================================================
    C*  write_D_Trail - Populate and Write D Trailer Record
    C*====================================================================
     C     write_D_Trail begsr
     C*
     C                   clear                   trailerD
     C*
     C                   eval      dType='TRAILER'
     C                   move      @@naic        diCoNumber
     C                   eval      dfundReceiver='D'
     C                   eval      dfromState=fnpst
     C                   eval      dfromLocation=11
     C                   eval      dtoState=empst
     C                   eval      dtoLocation=01
     C                   eval      dbatch#=batch#
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         dreportingDat
     C*
     C                   z-add     xxqbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dfromDate
     C*
     C                   z-add     @@medt7       srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dtoDate
     C*
     C                   eval      dinsType='P&C'
     C                   eval      drecordCount=Countc
     C*
     C                   if        (qdlosc+qddcc)<0
     C                   eval      number15=(qdlosc+qddcc)*-1
     C                   eval      sign15='-'
     C                   else
     C                   eval      number15=qdlosc+qddcc
     C                   eval      sign15='+'
     C                   endif
     C                   eval      dtotCurRes1=amount15
     C*
     C                   if        (qdlcpy+qdotrc+qdsbrc+qdrlrc+qddcce+qdderc)<0
     C                   eval      number15=(qdlcpy+qdotrc+qdsbrc+
     C                             qdrlrc+qddcce+qdderc)*-1
     C                   eval      sign15='-'
     C                   else
     C                   eval      number15=(qdlcpy+qdotrc+qdsbrc+
     C                             qdrlrc+qddcce+qdderc)
     C                   eval      sign15='+'
     C                   endif
     C                   eval      dtotCurQExp1=amount15
     C*
     C*
     C                   if        (ydlcpy+ydotrc+ydsbrc+ydrlrc+yddcce+ydderc)<0
     C                   eval      number15=(ydlcpy+ydotrc+ydsbrc+
     C                             ydrlrc+yddcce+ydderc)*-1
     C                   eval      sign15='-'
     C                   else
     C                   eval      number15=(ydlcpy+ydotrc+ydsbrc+
     C                             ydrlrc+yddcce+ydderc)
     C                   eval      sign15='+'
     C                   endif
     C                   eval      dtotYTDExp1=amount15
     C*
     C                   if        (idlcpy+idotrc+idsbrc+idrlrc+iddcce+idderc)<0
     C                   eval      number15=(idlcpy+idotrc+idsbrc+
     C                             idrlrc+iddcce+idderc)*-1
     C                   eval      sign15='-'
     C                   else
     C                   eval      number15=(idlcpy+idotrc+idsbrc+
     C                             idrlrc+iddcce+idderc)
     C                   eval      sign15='+'
     C                   endif
     C                   eval      dtotITDExp1=amount15
     C*
     C                   eval      number15=0
     C                   eval      sign15='+'
     C*
     C                   eval      dtotCurRes2=amount15
     C                   eval      dtotCurQExp2=amount15
     C                   eval      dtotCurQRev=amount15
     C                   eval      dtotYTDExp2=amount15
     C                   eval      dtotYTDRev=amount15
     C                   eval      dtotITDExp2=amount15
     C                   eval      dtotITDRev=amount15
     C*
     C                   endsr
    C*====================================================================
    C*  writeWxndp - Write to WXNDP
    C*====================================================================
     C     writeWxndp    BEGSR
     C*
     C                   clear                   pnd02
     c                   exsr      write_D_Header
     C                   movel(p)  header        nddata
     C                   write     pnd02
     C*
     c     1             setll     pnd01
     C                   read      pnd01
     c                   dow       not %eof
     C                   clear                   pnd02
     C                   movel(p)  detailD       nddata
     C                   write     pnd02
     C                   read      pnd01
     c                   enddo
     C*
     C                   clear                   pnd02
     c                   exsr      write_D_Trail
     C                   movel(p)  trailerD      nddata
     C                   write     pnd02
     C*
     C                   endsr
      /free
       //******* beg add ******* 03/30/15 **********************************
       //===================================================================
       // getState
       //===================================================================
       Begsr getState;

       strState = ' ';
       Chain (EMco#:EMfnd:EMemp#:0) pem00;
       If %found(WMEML);
          strState = E_EMpSt;

          If strState = ' ';
             strState = E_EMmSt;
          Endif;
       Endif;

       Endsr;
       //******* end add ******* 03/30/15 **********************************
      /end-free
