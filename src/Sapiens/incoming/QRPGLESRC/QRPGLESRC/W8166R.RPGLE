    H*====================================================================
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*====================================================================
    H/TITLE NGIF Reporting Extract - C process
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*                                                                   *
    H*===================================================================*
    H*                                                                   *
    H*  Called from: W8166C - Override                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*===================================================================*
    H*  Created on 07/29/2009 by c:62 for Project 114WTX                 *
    H*===================================================================*
    H*  Revisions:                                                       *
    H*                                                                   *
     H* 02/16/16 C:298 Project: 114WXC                                    *
     H*                Add condition to populate UDS-C recovery code.     *
    H*                                                                   *
     H* 02/15/16 C:062 Project: 39891                                     *
     H*                Remove code that was removed by 114WPR.            *
    H*                                                                   *
     H* 07/03/15 C:306 Project: 114WTS                                    *
     H*                Expand UDS-C for Reopen Processing                 *
    H*                                                                   *
     H* 03/30/15 C:279 Project: 114WPR                                    *
     H*                Modified to use div 0 physical state (or mailing   *
     H*                state, if no physical state is present) when rep-  *
     H*                orting all divisions.                              *
     H*
     H* 06/18/14 C:53  SUPPORT: 85795
     H*                Need to loop through employers has div are split
    H*                                                                   *
     H* 05/07/13 C:189 PROJECT: WC114 114WIS                              *
     H*                Some Coverage codes are now to be reported based   *
     H*                on the Excess determination                        *
    H*                                                                   *
     H* 07/25/12 C:062 PROJECT: WC114 80337                               *
     H*                -Change reserve selection logic.                   *
    H*                                                                   *
     H* 12/27/11 C:062 PROJECT: WC114 78782                               *
     H*                -Populate expense code with 07.                    *
     H*                -Remove unecessary reserve snapshot.               *
     H*                -remove emp#=13 condition                          *
    H*                                                                   *
     H* 04/04/11 C:062 PROJECT: 76321                                     *
     H*                Add processing for employer 13.                    *
    H*                                                                   *
     H* 02/28/11 C:53  PROJECT: 114WXR                                *
     H*                Additional changes to omit reserves on
    H*                inception-to-date extract                          *
    H*                                                                   *
     H* 12/14/10 C:191 PROJECT: 114WXR                                *
     H*                Add processing for inception-to-date extract   *
     H*                processing                                     *
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
     Fwmchl     if   e           k disk
     Fwtabl     if   e           k disk    ignore(pnc01:ppa01:pcc01)
     F*
     Fwancp     if a e             disk    usropn
     Fwxncp     o    e             disk    rename(pnc01:pnc02) usropn
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
     Dcreate           S               n
     Dreserves         S               n
     DclosedClaim      S               n
     DpmtActivity      S               n
     D*** Begin Add *** 12/27/11 ******************************************
     DanyPmtActivity   S               n
     D*** End   Add *** 12/27/11 ******************************************
     C*** Beg chg   ****** 07/03/15 ************************************
     DisReopened       S               n   Inz(*Off)
     DintOchefdt       S              7  0 Inz(0)
     C*** End chg   ****** 07/03/15 ************************************
     Dkkco#            S                   like(fnco#)
     Dkktrdt           S                   like(patrdt)
     Dkkunq#           S                   like(ccunq#)
     Dkkkey            S                   like(b2key)
     Dxxmbdt           S                   like(patrdt)
     DrsvAmt           S                   like(paamt)
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
     Dstatus           S              1a
      ******** beg add ********** 03/30/15 ********************************
     d strState        S              2a
      ******** end add ********** 03/30/15 ********************************

     Dxxnaic           S              9s 0
     Dkknaic           S                   like(emrsk#)

     Dqdlcpy           S             12  2
     Dqdtlrc           S             12  2
     Dqdsvrc           S             12  2
     Dqdsbrc           S             12  2
     Dqdsirc           S             12  2
     Dqddcce           S             12  2
     Dqdderc           S             12  2

     Dqdlosc           S             12  2
     Dqddcc            S             12  2

     Dydlcpy           S             12  2
     Dydtlrc           S             12  2
     Dydsvrc           S             12  2
     Dydsbrc           S             12  2
     Dydsirc           S             12  2
     Dyddcce           S             12  2
     Dydderc           S             12  2

     Didlcpy           S             12  2
     Didtlrc           S             12  2
     Didsvrc           S             12  2
     Didsbrc           S             12  2
     Didsirc           S             12  2
     Diddcce           S             12  2
     Didderc           S             12  2
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
     D                 DS                  INZ
     D  PCNT                   1     36  0
     D                                     DIM(4)
     D  CN01                   1      9  0
     D  CN02                  10     18  0
     D  CN03                  19     27  0
     D  CN04                  28     36  0
     D*** Begin Add *** 12/27/11 *******************************************
     D                 DS                  INZ
     D  ACNT                   1     36  0
     D                                     DIM(4)
     D  AC01                   1      9  0
     D  AC02                  10     18  0
     D  AC03                  19     27  0
     D  AC04                  28     36  0
     D*** End   Add *** 12/27/11 *******************************************
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
     D trailerC                     602
     D  cType                        20    overlay(trailerC)
     D  ciCoNumber                    5  0 overlay(trailerC:21)
     D  cfundReceiver                 1    overlay(trailerC:26)
     D  cfromState                    2    overlay(trailerC:27)
     D  cfromLocation                 2  0 overlay(trailerC:29)
     D  ctoState                      2    overlay(trailerC:31)
     D  ctoLocation                   2  0 overlay(trailerC:33)
     D  cbatch#                       3  0 overlay(trailerC:35)
     D  creportingDat                 8  0 overlay(trailerC:38)
     D  cfromDate                     8  0 overlay(trailerC:46)
     D  ctoDate                       8  0 overlay(trailerC:54)
     D  cinsType                      3    overlay(trailerC:62)
     D  crecordCount                  9  0 overlay(trailerC:65)
     D  ctotAmt                      15    overlay(trailerC:74)
     D  cfiller                     514    overlay(trailerC:89)
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
     D detailC       e ds                  extname(wancp)
    C*--------------------------------------------------------------------
    C*  Parameter Lists                                                  *
    C*--------------------------------------------------------------------
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@medt
     C                   parm                    @@naic
     C*** BEGIN ADD ****** 12/14/10 ************************************
     C                   parm                    @@itdt            1
     C*** END   ADD ****** 12/14/10 ************************************
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
     C     chky1a        klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C                   kfld                    patrdt
     C*
     C     chky2a        klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C*
     C     chky1b        klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    @@medt7
     C*
     C     chky2b        klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
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
     C                   open      wancp
     C                   open      wxncp
     C*
     C                   time                    dsdttm
     C                   move      @@co#         kkco#
     C                   move      @@medt        @@medt7
     C                   move      @@medt        kktrdt
     C*** BEGIN ADD ****** 12/14/10 ************************************
     C                   move      @@itdt        xxitdt            1
     C*** END   ADD ****** 12/14/10 ************************************
     C                   eval      countC=0
     C                   eval      amtC=0
     C                   move      *off          create
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
     c                   exsr      writeWancp
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
     C*** Begin chg ****** 07/03/15 ************************************
     c                   exsr      Wrtreopntn                                   Reopened
     C*** End chg   ****** 07/03/15 ************************************
     c                   exsr      writeWancp2                                  Closed
     c                   exsr      getReserves
     C*** Begin chg ****** 07/03/15 ************************************
     c****               if        reserves or closedClaim
     c                   if        reserves or closedClaim or IsReopened
     C*** End chg   ****** 07/03/15 ************************************
     C*** BEGIN ADD ****** 02/28/11 ************************************
     C* If inception-to-date extract is requested do not send reserve tx
     C                   if        xxitdt <> 'Y'
     C*** END   ADD ****** 02/28/11 ************************************
     c                   exsr      writeWancp3
     C*** BEGIN ADD ****** 02/28/11 ************************************
     c                   endif
     C*** END   ADD ****** 02/28/11 ************************************
     c                   endif
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
     C                   if        create
     c                   exsr      writeWxncp
     C                   exsr      genDocID
     C*
     C                   close     wancp
     C                   close     wxncp
     C*
     C                   call      'W8166C1'
     C                   parm                    ##dcidC
     C                   parm      *blanks       ##dcidD
     C*
     C                   else
     C*
     C                   close     wancp
     C                   close     wxncp
     C*
     C                   endif
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
     C                   eval      ifundReceiver='C'
     C                   eval      ifromState=fnpst
     C                   eval      ifromLocation=11
      ************* beg chg ******** 03/30/15 ****************************
     C******             eval      itoState=empst
      /free
       Exsr getState;
       itoState = strState;
      /end-free
      ************* end chg ******** 03/30/15 ****************************
     C*** Begin Del *** 02/15/16 ******************************************
     C**********         eval      itoState=empst
     C*** End   Del *** 02/15/16 ******************************************
     C                   eval      itoLocation=01
     C                   eval      ibatch#=batch#
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         ireportingDat
     C*
     C                   z-add     xxmbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ifromDate
     C*
     C                   z-add     @@medt7       srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         itoDate
     C*
     C                   eval      ##dcidC=document
     C*
     C                   endsr
    C*====================================================================
    C*  getDates - Get Begin Dates
    C*====================================================================
     C     getDates      BEGSR
     C*
     C* Quarter Begin Date
     C*
     C                   eval      xxmbdt=@@medt7
     C                   eval      month=%subst(@@medt:4:2)
     C*
     C                   select
     C                   when      month='01'
     c                   move      '0101'        xxmbdt
     C                   when      month='02'
     c                   move      '0201'        xxmbdt
     C                   when      month='03'
     c                   move      '0301'        xxmbdt
     C                   when      month='04'
     c                   move      '0401'        xxmbdt
     C                   when      month='05'
     c                   move      '0501'        xxmbdt
     C                   when      month='06'
     c                   move      '0601'        xxmbdt
     C                   when      month='07'
     c                   move      '0701'        xxmbdt
     C                   when      month='08'
     c                   move      '0801'        xxmbdt
     C                   when      month='09'
     c                   move      '0901'        xxmbdt
     C                   when      month='10'
     c                   move      '1001'        xxmbdt
     C                   when      month='11'
     c                   move      '1101'        xxmbdt
     C                   when      month='12'
     c                   move      '1201'        xxmbdt
     C                   endsl
     C*** BEGIN ADD ****** 12/14/10 ************************************
     C* If inception-to-date extract is requested, reset begining date to 0
     C                   if        xxitdt = 'Y'
     C                   eval      xxmbdt = 0
     C                   endif
     C*** END   ADD ****** 12/14/10 ************************************
     C*
     C                   ENDSR
    C*====================================================================
    C*  getReserves - Get Researves
    C*====================================================================
     C     getReserves   begsr
     C*
     C                   eval      reserves=*off
     C                   clear                   ramt
     C                   clear                   pamt
     C                   clear                   pcnt
     C*** Begin Add *** 12/27/11 *******************************************
     C                   clear                   acnt
     C*** Begin Add *** 12/27/11 *******************************************
     C*
     c     rhky1         setgt     prh01
     c     rhky2         readpe    prh01                                  99
     c                   dow       not *in99
     c*
     c                   if        rhefdt>=xxmbdt
     c                   eval      reserves=*on
     C*** Begin Add *** 07/25/12 ******************************************
     C                   If        rsu(3)<>0
     C                   eval      pcnt(1)=pcnt(1)+1
     C                   EndIf

     C                   If        rsu(1)<>0
     C                   eval      pcnt(2)=pcnt(2)+1
     C                   EndIf

     C                   If        rsu(2)<>0 or rsu(4)<>0
     C                   eval      pcnt(3)=pcnt(3)+1
     C                   EndIf

     C                   If        rsu(5)<>0
     C                   eval      pcnt(4)=pcnt(4)+1
     C                   EndIf
     C*** End   Add *** 07/25/12 ******************************************
     c                   endif
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
     c                   if        patrdt>=xxmbdt
     c                   eval      reserves=*on
     c                   endif
     c*
     C                   if        patype='V' or patype='S' or patype='R'
     C                   eval      paamt=paamt*-1
     C                   endif
     c*
     c                   select
     c                   when      parecd='CO'
     c                   eval      pamt(1)=pamt(1)+paamt
     c                   if        patrdt>=xxmbdt
     c                   eval      pcnt(1)=pcnt(1)+1
     c                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     c                   eval      acnt(1)=acnt(1)+1
     C*** End   Add *** 12/27/11 *******************************************
     c                   when      parecd='EX' or parecd='LE'
     c                   eval      pamt(3)=pamt(3)+paamt
     c                   if        patrdt>=xxmbdt
     c                   eval      pcnt(3)=pcnt(3)+1
     c                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     c                   eval      acnt(3)=acnt(3)+1
     C*** End   Add *** 12/27/11 *******************************************
     c                   when      parecd='ME'
     c                   eval      pamt(2)=pamt(2)+paamt
     c                   if        patrdt>=xxmbdt
     c                   eval      pcnt(2)=pcnt(2)+1
     c                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     c                   eval      acnt(2)=acnt(2)+1
     C*** End   Add *** 12/27/11 *******************************************
     c                   when      parecd='RE'
     c                   eval      pamt(4)=pamt(4)+paamt
     c                   if        patrdt>=xxmbdt
     c                   eval      pcnt(4)=pcnt(4)+1
     c                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     c                   eval      acnt(4)=acnt(4)+1
     C*** End   Add *** 12/27/11 *******************************************
     c                   endsl
     c*
     c     paky4         readpe    ppa02                                  99
     c                   enddo
     C*
     c                   if        (ramt(1)-pamt(1)>0) or (ramt(2)-pamt(2)>0) or
     c                             (ramt(3)-pamt(3)>0) or (ramt(4)-pamt(4)>0)
     c                   eval      reserves=*on
     c                   endif
     C*
     c                   if        pcnt(1)<>0 or pcnt(2)<>0 or
     c                             pcnt(3)<>0 or pcnt(4)<>0
     c                   eval      reserves=*on
     c                   endif
     C*
     C                   endsr
    C*====================================================================
    C*  writeWancp - Write to WANCP
    C*====================================================================
     C     writeWancp    BEGSR
     C*
     c                   move      cmfnd         group
     c                   move      cmemp#        employer
     c                   move      cmdiv         division
     c                   move      cmfyr         year
     c                   move      cmcase        claim
     C*
     C                   eval      kkkey=pabr2
     C     b2ky          chain     pb201
     C                   if        not %found
     C                   clear                   b2minr
     C                   endif
     C*
     C                   clear                   pnc01
     C*
     C                   eval      nctype='C'
     C                   move      @@naic        ncicmp
     C                   eval      nctlst=fnpst
     C                   eval      nctlcd='11'
     C*
     c                   select
     c                   when      parecd='CO'
     C                   eval      nccvcd='965005'
     c                   when      parecd='ME'
     C                   eval      nccvcd='965010'
     c                   when      parecd='RE'
     C                   eval      nccvcd='965025'
     c                   when      parecd='LE'
     C                   eval      nccvcd='965005'
     c                   when      parecd='EX'
     C                   eval      nccvcd='965005'
     c                   endsl
     c*
     c*** Begin Add *** 05/07/13 ******************************************
     C                   If        CMexcs  = 'Y'
     c*
     C                   Select
     c*
     c                   When      nccvcd  = '965005'
     c                   Eval      nccvcd  = '515305'
     c*
     c                   When      nccvcd  = '965010'
     c                   Eval      nccvcd  = '515310'
     c*
     c                   When      nccvcd  = '965025'
     c                   Eval      nccvcd  = '515325'
     C*
     C                   Endsl
     C                   Endif
     c*
     c*** End   Add *** 05/07/13 ******************************************
     C                   eval      ncpol#=ccpol#
     C*** Begin chg *** 01/20/09 *******************************************
     C                   eval      ncicc#=ccicc#
     C                   eval      ncrc#=ccrc#
     C                   eval      ncfc#=group + year + claim
     C**********         eval      ncicc#=ccrc#
     C**********         eval      ncrc#=group + year + claim
     C**********         eval      ncfc#=ccicc#
     C*** End   chg *** 01/20/09 *******************************************
     C                   movel(p)  ccinl1        ncinl1
     C                   movel(p)  ccinl2        ncinl2
     C                   z-add     1             ncclm#
     C                   eval      nccnl1=cmlsn
     C                   eval      nccnl2=cmfsn
     C                   eval      nccad1=cdadr1
     C                   eval      nccad2=cdadr2
     C                   eval      ncccty=cdcty
     C                   eval      nccst=cdst
     C                   move      cdzip         ncczip
     C*
     C     chky1a        setgt     pch01
     C     chky2a        readpe    pch01
     C                   if        not *in99
     C                   eval      status=chocst
     C                   else
     C                   eval      status='O'
     C                   endif
     C*
     C                   movel     *blanks       ncexcd
     C*
     C                   if        paretp='C' or paretp='M' or parecd='RE'
     C                   if        pabr2='CQ' or pabr2='EQ' or pabr2='MQ' or
     C                             pabr2='LQ' or pabr2='P1' or pabr2='P2' or
     C                             pabr2='59' or pabr2='60' or pabr2='61' or
     C                             pabr2='62' or pabr2='63'
     C                   eval      nctrcd=530
     C                   else
     C                   if        status='O'
     C                   eval      nctrcd=310
     C                   else
     C                   eval      nctrcd=320
     C                   endif
     C                   endif
     C                   endif
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
     C                   eval      nctrcd=530
     C                   else
     C*** End   Add *** 04/04/11 *******************************************
     C                   eval      nctrcd=540
     C*** Begin Add *** 04/04/11 *******************************************
     C                   endif
     C*** End   Add *** 04/04/11 *******************************************
     C                   else
     C                   if        status='O'
     C                   eval      nctrcd=410
     C                   else
     C                   eval      nctrcd=420
     C                   endif
     C                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     C                   if        (nctrcd=410 or nctrcd=420) and pabr2='AE'
     C                   movel(p)  '07'          ncexcd
     C                   else
     C*** End   Add *** 12/27/11 *******************************************
     C                   if        paretp='L'
     C                   movel(p)  '04'          ncexcd
     C                   else
     C                   movel(p)  '05'          ncexcd
     C                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     C                   endif
     C*** End   Add *** 12/27/11 *******************************************
     C                   endif
     C*
     C                   eval      srdti=patrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         nctrdt
     C                   if        patype='V' or patype='S' or patype='R'
     C                   eval      paamt=paamt*-1
     C                   endif
     C*
     C                   if        paamt<0
     C                   eval      sign12='-'
     C                   eval      number12=paamt*-1
     C                   else
     C                   eval      sign12='+'
     C                   eval      number12=paamt
     C                   endif
     C*
     C                   eval      nctamt=amount12
     C                   movel     pachk#        ncchk#
     C                   eval      ncpind='F'
     C                   move      pafein        ncpid#
     C                   movel(p)  rppnam        ncpnm1
     C                   movel(p)  *blanks       ncpnm2
     C                   movel(p)  *blanks       ncclcd
     C                   movel(p)  *blanks       ncricd
     C*
     C                   if        nctrcd=530 or nctrcd=540
     C*
     C                   select
     C                   when      pabr2='CQ' or pabr2='EQ' or pabr2='MQ' or
     C                             pabr2='LQ'
     C                   eval      ncricd='9'
     C                   when      pabr2='P1' or pabr2='P2' or pabr2='59' or
     C                             pabr2='60' or pabr2='61' or pabr2='63'
     C                   eval      ncricd='3'
     C                   when      pabr2='62'
     C                   eval      ncricd='7'
     C                   endsl
      /Free
       //***** Beg Add ***** 02/16/16 ********************************************
       Else;
          NCricd = '0';
       //***** End Add ***** 02/16/16 ********************************************
      /End-Free
     c*
     C                   endif
     C*
     C                   movel(p)  'U'           ncsind
     C                   movel(p)  'N'           nc2ifi
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncdol
     C                   eval      nctcom=padesc
     C                   movel     *blanks       ncwicd
     C                   movel(p)  cmpart        ncwpb
     C                   movel(p)  cmnatr        ncwni
     C                   movel(p)  cmcaus        ncwcau
     C                   movel(p)  ccwact        ncwact
     C                   movel(p)  ccwlos        ncwlos
     C                   movel(p)  *blanks       ncwtor
     C                   movel(p)  *blanks       ncwtoc
     C                   movel(p)  ccwtos        ncwtos
     C                   movel(p)  *blanks       ncwvhi
     C                   movel(p)  cctpac        nctpac
     C                   movel(p)  cclclm        nclclm
     C                   z-add     pafrdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncsfdt
     C                   z-add     patodt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncstdt
     C                   eval      ncpdin='N'
     C*
     c                   if        patrdt >= xxmbdt
     C                   write     pnc01
     C                   add       1             countC
     C                   add       paamt         amtC
     C                   eval      create=*on
     c                   endif
     C*
     C                   endsr
    C*====================================================================
    C*  writeWancp2 - Write to WANCP - Closed Claim
    C*====================================================================
     C     writeWancp2   BEGSR
     C*
     C                   eval      closedClaim=*off
     C     chky1b        setgt     pch01
     C     chky2b        readpe    pch01
     C                   if        not *in99
     C                   if        chefdt>=xxmbdt and chefdt<=@@medt7 and
     C                             chocst='C'
     C                   eval      closedClaim=*on
     C                   endif
     C                   endif
     C*
     C                   if        closedClaim
     C*
     c                   move      cmfnd         group
     c                   move      cmemp#        employer
     c                   move      cmdiv         division
     c                   move      cmfyr         year
     c                   move      cmcase        claim
     C*
     C                   clear                   pnc01
     C*
     C                   eval      nctype='C'
     C                   move      @@naic        ncicmp
     C                   eval      nctlst=fnpst
     C                   eval      nctlcd='11'
     c*
     C                   eval      ncpol#=ccpol#
     C                   eval      ncicc#=ccicc#
     C                   eval      ncrc#=ccrc#
     C                   eval      ncfc#=group + year + claim
     C                   movel(p)  ccinl1        ncinl1
     C                   movel(p)  ccinl2        ncinl2
     C                   z-add     1             ncclm#
     C                   eval      nccnl1=cmlsn
     C                   eval      nccnl2=cmfsn
     C                   eval      nccad1=cdadr1
     C                   eval      nccad2=cdadr2
     C                   eval      ncccty=cdcty
     C                   eval      nccst=cdst
     C                   move      cdzip         ncczip
     C                   eval      status=chocst
     C*
     C                   movel     *blanks       ncexcd
     C*
     C                   eval      nctrcd=030
     C                   eval      nccvcd=*blanks
     C*
     C                   eval      srdti=chefdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         nctrdt
     C*
     C                   eval      sign12='+'
     C                   eval      number12=0
     C                   eval      nctamt=amount12
     C*
     C                   movel     *blanks       ncchk#
     C                   eval      ncpind=' '
     C                   move      *blanks       ncpid#
     C                   movel(p)  *blanks       ncpnm1
     C                   movel(p)  *blanks       ncpnm2
     C                   movel(p)  *blanks       ncclcd
      /Free
       //***** Beg Chg ***** 02/16/16 ********************************************
       //C********           movel(p)  *blanks       ncricd
       NCricd = '0';
       //***** End Chg ***** 02/16/16 ********************************************
      /End-free
     C                   movel(p)  'U'           ncsind
     C                   movel(p)  'N'           nc2ifi
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncdol
     C                   eval      nctcom=chexpl
     C                   movel     *blanks       ncwicd
     C                   movel(p)  cmpart        ncwpb
     C                   movel(p)  cmnatr        ncwni
     C                   movel(p)  cmcaus        ncwcau
     C                   movel(p)  ccwact        ncwact
     C                   movel(p)  ccwlos        ncwlos
     C                   movel(p)  *blanks       ncwtor
     C                   movel(p)  *blanks       ncwtoc
     C                   movel(p)  ccwtos        ncwtos
     C                   movel(p)  *blanks       ncwvhi
     C                   movel(p)  cctpac        nctpac
     C                   movel(p)  cclclm        nclclm
     C                   z-add     chefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncsfdt
     C                   z-add     chefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncstdt
     C                   eval      ncpdin='N'
     C*
     C                   write     pnc01
     C                   add       1             countC
     c                   endif
     C*
     C                   endsr
    C*====================================================================
    C*  writeWancp3 - Write to WANCP - Reserves
    C*====================================================================
     C     writeWancp3   BEGSR
     C*
     c                   move      cmfnd         group
     c                   move      cmemp#        employer
     c                   move      cmdiv         division
     c                   move      cmfyr         year
     c                   move      cmcase        claim
     c*
     c     1             do        4             x
     C*
     C                   clear                   pnc01
     C*
     C                   eval      nctype='C'
     C                   move      @@naic        ncicmp
     C                   eval      nctlst=fnpst
     C                   eval      nctlcd='11'
     C*
     c                   select
     c                   when      rtyp(x)='CO'
     C                   eval      nccvcd='965005'
     C                   eval      nctrcd=130
     c                   when      rtyp(x)='ME'
     C                   eval      nccvcd='965010'
     C                   eval      nctrcd=130
     c                   when      rtyp(x)='RE'
     C                   eval      nccvcd='965025'
     C                   eval      nctrcd=130
     c                   when      rtyp(x)='EX' or rtyp(x)='LE'
     C                   eval      nccvcd='965005'
     C                   eval      nctrcd=230
     c                   endsl
     C*
     c*** Begin Add *** 05/07/13 ******************************************
     C                   If        CMexcs  = 'Y'
     C                   Select
     c                   When      nccvcd  = '965005'
     c                   Eval      nccvcd  = '515305'
     c*
     c                   When      nccvcd  = '965010'
     c                   Eval      nccvcd  = '515310'
     c*
     c                   When      nccvcd  = '965025'
     c                   Eval      nccvcd  = '515325'
     C*
     C                   Endsl
     C                   Endif
     c*
     c*** End   Add *** 05/07/13 ******************************************
     C                   eval      ncpol#=ccpol#
     C*** Begin chg *** 01/20/09 *******************************************
     C                   eval      ncicc#=ccicc#
     C                   eval      ncrc#=ccrc#
     C                   eval      ncfc#=group + year + claim
     C**********         eval      ncicc#=ccrc#
     C**********         eval      ncrc#=group + year + claim
     C**********         eval      ncfc#=ccicc#
     C*** End   chg *** 01/20/09 *******************************************
     C                   movel(p)  ccinl1        ncinl1
     C                   movel(p)  ccinl2        ncinl2
     C                   z-add     1             ncclm#
     C                   eval      nccnl1=cmlsn
     C                   eval      nccnl2=cmfsn
     C                   eval      nccad1=cdadr1
     C                   eval      nccad2=cdadr2
     C                   eval      ncccty=cdcty
     C                   eval      nccst=cdst
     C                   move      cdzip         ncczip
     C*
     c                   eval      kkkey=rtyp(x)
     c     rtky          chain     prt01
     c                   if        not %found
     c                   clear                   rttype
     c                   endif
     C*
     C                   eval      srdti=@@medt7
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         nctrdt
     C*
     C                   eval      rsvAmt=ramt(x)-pamt(x)
     C                   eval      pmtActivity=*off
     C                   if        pcnt(x)<>0
     C                   eval      pmtActivity=*on
     C                   endif
     C*** Begin Add *** 12/27/11 *******************************************
     C                   eval      anyPmtActivity=*off
     C                   if        acnt(x)<>0
     C                   eval      anyPmtActivity=*on
     C                   endif
     C*** End   Add *** 12/27/11 *******************************************
     C                   if        (ramt(x)-pamt(x))<0
     C                   eval      sign12='+'
     C                   eval      number12=0
     C                   eval      rsvAmt=0
     C                   else
     C                   eval      sign12='+'
     C                   eval      number12=ramt(x)-pamt(x)
     C                   endif
     C*
     C                   eval      nctamt=amount12
     C                   movel     *blanks       ncchk#
     C                   eval      ncpind=' '
     C                   move      *blanks       ncpid#
     C                   movel(p)  *blanks       ncpnm1
     C                   movel(p)  *blanks       ncpnm2
     C                   movel(p)  *blanks       ncclcd
      /Free
       //***** Beg Chg ***** 02/16/16 ********************************************
       //C**********         movel(p)  *blanks       ncricd
       NCricd = '0';
       //***** End Chg ***** 02/16/16 ********************************************
      /End-free
     C                   movel(p)  'U'           ncsind
     C                   movel(p)  'N'           nc2ifi
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncdol
     C                   eval      nctcom=' '
     C                   movel     *blanks       ncexcd
     C                   movel     *blanks       ncwicd
     C                   movel(p)  cmpart        ncwpb
     C                   movel(p)  cmnatr        ncwni
     C                   movel(p)  cmcaus        ncwcau
     C                   movel(p)  ccwact        ncwact
     C                   movel(p)  ccwlos        ncwlos
     C                   movel(p)  *blanks       ncwtor
     C                   movel(p)  *blanks       ncwtoc
     C                   movel(p)  ccwtos        ncwtos
     C                   movel(p)  *blanks       ncwvhi
     C                   movel(p)  cctpac        nctpac
     C                   movel(p)  cclclm        nclclm
     C                   z-add     rhefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncsfdt
     C                   z-add     rhefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ncstdt
     C                   eval      ncpdin='N'
     C*
     C*
     C*** Begin Chg *** 12/27/11 ******************************************
     C**********         if        rsvAmt>0 or closedClaim or pmtActivity
     C*** Begin Chg *** 07/25/12 ******************************************
     C**********         if        rsvAmt>0 or pmtactivity or
     C**********                   (closedClaim and anyPmtActivity)
     C                   if        rsvAmt>0 or pmtActivity
     C*** End   Chg *** 07/25/12 ******************************************
     C*** End   Chg *** 12/27/11 ******************************************
     C
     C                   write     pnc01
     C                   add       1             countC
     C                   add       rsvAmt        amtC
     C                   eval      create=*on
     C                   endif
     C*
     c                   enddo
     C*
     C                   endsr
    C*====================================================================
    C*  write_C_Header - Populate and Write C Header Record
    C*====================================================================
     C     write_C_Headerbegsr
     C*
     C                   clear                   header
     C*
     C                   eval      hType='HEADER02'
     C                   move      @@naic        hiCoNumber
     C                   eval      hfundReceiver='C'
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
     C                   z-add     xxmbdt        srdti
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
    C*  write_C_Trail - Populate and Write C Trailer Record
    C*====================================================================
     C     write_C_Trail begsr
     C*
     C                   clear                   trailerC
     C*
     C                   eval      cType='TRAILER'
     C                   move      @@naic        ciCoNumber
     C                   eval      cfundReceiver='C'
     C                   eval      cfromState=fnpst
     C                   eval      cfromLocation=11
     C                   eval      ctoState=empst
     C                   eval      ctoLocation=01
     C                   eval      cbatch#=batch#
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         creportingDat
     C*
     C                   z-add     xxmbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cfromDate
     C*
     C                   z-add     @@medt7       srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         ctoDate
     C*
     C                   eval      cinsType='P&C'
     C                   eval      crecordCount=Countc
     C*
     C                   if        amtC<0
     C                   eval      sign15='-'
     C                   eval      number15=amtC*-1
     C                   else
     C                   eval      sign15='+'
     C                   eval      number15=amtC
     C                   endif
     C*
     C                   eval      ctotAmt=amount15
     C*
     C                   endsr
    C*====================================================================
    C*  writeWxncp - Write to WXNCP
    C*====================================================================
     C     writeWxncp    BEGSR
     C*
     C                   clear                   pnc02
     c                   exsr      write_C_Header
     C                   movel(p)  header        ncdata
     C                   write     pnc02
     C*
     c     1             setll     pnc01
     C                   read      pnc01
     c                   dow       not %eof
     C                   clear                   pnc02
     C                   movel(p)  detailC       ncdata
     C                   write     pnc02
     C                   read      pnc01
     c                   enddo
     C*
     C                   clear                   pnc02
     c                   exsr      write_C_Trail
     C                   movel(p)  trailerC      ncdata
     C                   write     pnc02
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
     C*****Begin chg ****** 07/03/15 ************************************
      /Free
        Begsr Wrtreopntn;
         IsReopened = *off;            // Reopened flag
         intOchefdt = *Zero;

         Setgt chky1b pch01;
         ReadPe chky2b pch01;

         If CHocst = 'O';
          intOchefdt = CHefdt;
          ReadPe chky2b pch01;

          If CHocst = 'C';
            If intOchefdt >=xxmbdt and intOchefdt <=@@medt7;
               IsReopened = *On;
            EndIf;

          EndIf;                      //  Chocst ='C' EndIf
        EndIf;                        //  Chocst ='O' EndIF

          If IsReopened =  *On;
           group     =  %Char(CMfnd);
           employer  =  %char(CMemp#);
           division  =  %char(CMdiv);
           year      =  %char(CMfyr);
           claim     =  %char(CMcase);

           clear pnc01;

           NCtype ='C';
           NCicmp = @@naic;
           NCtlst=FNpst;
           NCtlcd='11';

           NCpol#=CCpol#;
           NCicc#=CCicc#;
           NCrc#=CCrc#;
           NCfc#=group + year + claim;

           NCinl1 = CCinl1;
           NCinl2 = CCinl2;
           NCclm# = 1;

           NCcnl1=CMlsn;
           NCcnl2=CMfsn;
           NCcad1=CDadr1;
           NCcad2=CDadr2;
           NCccty=CDcty;
           NCcst=CDst;
           NCczip = %char(CDzip);
           status = CHocst;

           NCexcd = *Blanks;

           NCtrcd = 050;        // Tran-50

           NCcvcd=*blanks;
      /End-Free
     C                   eval      srdti=CHefdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         NCtrdt
      /Free
           sign12 ='+';
           number12 =0;
           NCtamt = amount12;

           NCchk# =  *blanks;
           NCpind = ' ';
           NCpid#  = *Blanks;
           NCpnm1  = *Blanks;
           NCpnm2  = *Blanks;
           NCclcd  = *Blanks;
           // ***** Beg Chg ***** 02/16/16 ********************************************
           // NCricd  = *Blanks;
           NCricd  = '0';
           // ***** End Chg ***** 02/16/16 ********************************************
           NCsind  = 'U';
           NC2ifi  =  'N';
      /End-Free
     C                   z-add     CMacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         NCdol
      /Free
           NCtcom = CHexpl;

           NCwicd = *blanks;
           NCwpb = CMpart;
           NCwni = CMnatr;
           NCwcau = CMcaus;
           NCwact = CCwact;
           NCwlos = CCwlos;
           NCwtor = *blanks;
           NCwtoc = *blanks;
           NCwtos = CCwtos;
           NCwvhi = *blanks;

           NCtpac = CCtpac;
           NClclm = CClclm;
      /End-Free
     C                   z-add     CHefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         NCsfdt
     C                   z-add     CHefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         NCstdt
      /Free
           NCpdin='N';

           write pnc01;
           countC = countC + 1;
          Endif;
         Endsr;
      /End-Free
     C****** End chg********* 07/03/15 ************************************
