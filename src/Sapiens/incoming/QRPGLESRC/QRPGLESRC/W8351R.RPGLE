     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      * ============================================================= *
     H DATFMT(*USA)
    H*
    H/TITLE W8351R - STAT CORRECTION REPORT
    H*****************************************************************
    H*                                                               *
    H*  PROGRAM DESCRIPTION:                                         *
    H*                                                               *
    H*                                                               *
    H*  CALLED FROM: W8351C                                          *
    H*                                                               *
    H*  CALLS TO:                                                    *
    H*                                                               *
    H*  PROGRAMMER: C:87                                             *
    H*  CREATED:    10/26/00              PROJECT: WC591.2           *
    H*****************************************************************
    H* REVISIONS:                                                    *
    H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
    H*****************************************************************
    F*
     Fsmorl     if   e           k disk
     Fsmcol     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwdehl13   if   e           k disk
     Fwmihl     if   e           k disk
     Fwdpal5    if   e           k disk
     Fwdpal     if   e           k disk    Rename(ppa01 : ppa02) Prefix(p2:2)
     Fwacml     if   e           k disk    Rename(pcm01 : pcm02) Prefix(c2:2)
     Fwmcml1    if   e           k disk
     Fwdell     if   e           k disk
     Fwmeml     if   e           k disk
     Fwmrpl     if   e           k disk
     Fwmrhl     if   e           k disk
     Fwtabl     if   e           k disk    Include(pb201 : prt01)
     Fwmupl     if   e           k disk
     Fqsysprt1  o    f  198        printer oflind(*inof)
    
     DcvtToDate        pr              d
     D numDate8                       8  0 Const

     DincFactor        pr            15  4
     D ppco#                          3  0 Const
     D ppfnd                          3  0 Const
     D ppfyr                          3  0 Const
     D ppcase                         7  0 Const
     D pptrdt                         7  0 Const
     D pptodt                         7  0 Const

     DpdFactor         pr            15  4
     D ppco#                          3  0 Const
     D ppfnd                          3  0 Const
     D ppfyr                          3  0 Const
     D ppcase                         7  0 Const
     D pptrdt                         7  0 Const
     D ppamt                          9  2 Const

     D
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D
     D                 ds
     D policy#                       25
     D   xxfnd                        3    overlay(policy#)
     D   xxemp#                       9    overlay(policy#:4)
     D   xxdiv                        5    overlay(policy#:13)
     D
     D                 DS
     D rhamt                   1     90  2 Dim(10)
     D  RHSU01                 1      9  2
     D  rhsu02                10     18  2
     D  rhsu03                19     27  2
     D  rhsu04                28     36  2
     D  rhsu05                37     45  2
     D  rhsu06                46     54  2
     D  rhsu07                55     63  2
     D  rhsu08                64     72  2
     D  rhsu09                73     81  2
     D  rhsu10                82     90  2
     D*
     D                 DS
     D rhcod                   1     20    Dim(10)
     D  rhrt01                 1      2
     D  rhrt02                 3      4
     D  rhrt03                 5      6
     D  rhrt04                 7      8
     D  rhrt05                 9     10
     D  rhrt06                11     12
     D  rhrt07                13     14
     D  rhrt08                15     16
     D  rhrt09                17     18
     D  rhrt10                19     20
     D*
    D*---------------------------------------------------------------------
    D* Key fields - TOP
    D*---------------------------------------------------------------------
     D kkco#           s                   like(elco#)
     D kkfnd           s                   like(elfnd)
     D kkfyr           s                   like(elfyr)
     D kkemp#          s                   like(elemp#)
     D kkdiv           s                   like(eldiv)
     D kkcase          s                   like(cmcase)
     D kkkey           s                   like(b2key)
    D*---------------------------------------------------------------------
    D* Key fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##leng          s              3  0
     D qqagin          s             11
     D qqco#           s              3
     D qqfnd           s              3
     D qqfrdt          s              8
     D qqmode          s              1
     D qqque           s             10
     D qqtodt          s              8
     D rsco#           s              3  0
     D rsfnd           s              3  0
     D rsfrdt          s              8  0
     D rstodt          s              8  0
     D rsagin          s             11  2
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxagin          s             11  2
     D xxfnam          s             60
     D xxconm          s             60
     D xxtitle         s             60
     D xxname          s             40
     D xxfrdt          s              7  0
     D xxkey           s             10
     D xxprt           s              1
     D xxtodt          s              7  0
     D polFrDt         s               d
     D valDate         s               d
     D transDt         s               d
     D deletDt         s               d
     D accDate         s               d
     D claimant        s             46
     D xxInc           s             15  4
     D xxPaid          s             15  4
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*
     D psds          esds                  extname(szq1p)
    C*
    C*  KEY LIST FOR THE OVERRIDE FILE
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt
    C*
    C*   COMPANY FILE
     C     coky          klist
     C                   kfld                    kkco#
    C*
    C*   EMPLOYER DETAIL HISTORY
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
    C*   CLAIM INJURY TYPE HISTORY
     C     ihky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
    C*   CLAIMS CHECKS PAYMENT FILE
     C     paky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
    C*   CLAIM DELETION FILE
     C     c2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
    C*   CLAIM MASTER FILE
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
    C*  EMPLOYER DETAIL FILE
     C     elky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
    C*
    C*  CARRIER/STATE REPORING MASTER FILE
     C     rpky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
    C*
    C*   EMPLOYER MASTER FILE
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
    C*
    C*   KEY LIST FOR THE FUND MASTER FILE.
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
    C*  TABLE FILE KEY
     C     tbky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
    C*   KEY LIST FOR THE USER PARAMETER FILE
     C     upky          klist
     C                   kfld                    q1user
    C*
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfrdt
     C                   parm                    qqtodt
     C                   parm                    qqagin
     C                   parm                    qqque
     C                   parm                    qqmode
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
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
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*---------------------------------------------------------------------
    C* Begin processing
    C*---------------------------------------------------------------------
     C                   time                    dsdttm
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
    C*
     C                   move      qqfrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxfrdt
    C*
     C                   move      qqtodt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtodt
    C*
     C                   move      qqagin        xxagin
    C*
    C*  BATCH OR DEMAND  MODE
    C*
     C                   if        qqmode = 'D'
     C                   move      *on           *in09
     C                   else
     C                   move      *on           *in08
     C                   endif
    C*
     C                   move      qqco#         rsco#
     C                   move      qqfnd         rsfnd
    C*
     C                   move      qqfrdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         rsfrdt
    C*
     C                   move      qqtodt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         rstodt
     C
     C                   move      qqagin        rsagin
    C*
    C*   CHAIN TO THE USER PARAMETER FILE
     C     upky          chain     pup01                              60
    C*
    C*   CHAIN TO THE COMPANY FILE
     C     coky          chain     pco01                              60
     C                   eval       xxconm = *blank
     C                   if        *in60 = *off
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     C                   eval       xxconm = coname
     C                   endif
    C*
    C*   CHAIN TO THE FUND MASTER FILE
     C                   eval       xxfnam = *blank
     C     fnky          chain     pfn01                              60
     C                   if        *in60 = *off
     C
     C                   if        fnanam <> *blank
     C                   movel(p)  fnanam        xxconm
     C                   endif
     C
     C                   movel(p)  fnname        xxfnam
     C
     C                   endif                                                  *in60=*off
     C*
     C*   CENTER COMPANY NAME IN FIELD
     C                   movel(p)  xxconm        ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      60            ##leng
     C
     C                   movel     ##data        xxconm
    C*
    C*   GET THE DISTRIBUTION INFORMATION & PRINT
    C*
     C                   eval       xxtitle = *blank
     C                   move      '1'           xxprt
     C     orky          chain     por01                              60
     C                   if        *in60 = *off
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   movel(p)  ortitl        xxtitle
     C                   endif
     C                   move      *on           *inof
     C*
     C*   CENTER TITLE IN FIELD
     C                   movel(p)  xxtitle       ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      60            ##leng
     C
     C                   movel     ##data        xxtitle
     C*
    C*---------------------------------------------------------------------
     C*  AUDITS
    C*---------------------------------------------------------------------
     C                   except    head
     C                   except    dtl1hd
     C*    read wdehp to find audits processed within date range - print the change
     C*    if it occured after the valuation date of the first report
     C     ehky          chain     peh01                              70
     C                   dow       *in70 = *off
     C
     C                   if        ehfaf = 'A'
     C                             and ehtrdt >= xxfrdt
     C                             and ehtrdt <= xxtodt
     C
     C                   eval       transDt = cvtToDate(ehtrdt)
     C                   eval       polFrDt = cvtToDate(ehpfdt)
     C     polFrDt       adddur    18:*m         valDate
     C                   if        valDate < transDt
     C
     C*                  *PRINT THIS GUY OUT
     C*               policy #
     C                   eval      kkemp# = ehemp#
     C                   eval      kkdiv = ehdiv
     C                   eval      kkfyr = ehfyr
     C     rpky          chain     prp01                              71
     C                   if        *in71 = *off
     C                             and rpcap# <> *blank
     C                   movel(p)  rpcap#        policy#
     C                   else
     C                   move      ehfnd         xxfnd
     C                   move      ehemp#        xxemp#
     C                   move      ehdiv         xxdiv
     C                   endif                                                  *in71=*off
     C*               name
     C                   move      *blanks       xxname
     C     emky          chain     pem01                              72
     C                   if        *in72 = *off
     C                   movel(p)  emname        xxname
     C                   endif                                                  *in72=*off
     C
     C                   if        *inof = *on
     C                   except    head
     C                   except    dtl1hd
     C                   endif
     C                   except    dtl1
     C
     C                   endif                                                  valDate<cvt(ehtrdt)
     C
     C                   endif                                                  elfaf='A'
     C
     C     ehky          reade     peh01                                  70
     C                   enddo                                                  *in70=*off
    C*
    C*---------------------------------------------------------------------
     C*  CLAIM INJURY TYPE CHANGES
    C*---------------------------------------------------------------------
     C                   except    head
     C                   except    dtl2hd
     C*    read wmihp to find changes made within date range - print the change
     C*    if it occured after the valuation date of the first report
     C     ihky          chain     pih01                              70
     C                   dow       *in70 = *off
     C
     C                   if        ihtrdt >= xxfrdt
     C                             and ihtrdt <= xxtodt
     C
     C*                  *chain to get employer
     C                   eval       kkfyr = ihfyr
     C                   eval       kkcase = ihcase
     C     cmky          chain     pcm01                              71
     C                   if        *in71 = *off
     C                   eval       kkemp# = cmemp#
     C                   eval       kkdiv = cmdiv
     C     elky          chain     pel01                              72
     C                   if        *in72 = *off
     C
     C*                  *check if change made after valuation of 1st report
     C                   eval       transDt = cvtToDate(ihtrdt)
     C                   eval       polFrDt = cvtToDate(elpfdt)
     C     polFrDt       adddur    18:*m         valDate
     C                   if        valDate < transDt
     C
     C*                  *PRINT THIS GUY OUT
     C*               claimant
     C                   eval       claimant = %trim(cmfsn) + ' ' +
     C                                         cmmid + ' ' + %trim(cmlsn)
     C*               policy #
     C                   eval      kkemp# = elemp#
     C                   eval      kkdiv = eldiv
     C                   eval      kkfyr = elfyr
     C     rpky          chain     prp01                              71
     C                   if        *in71 = *off
     C                             and rpcap# <> *blank
     C                   movel(p)  rpcap#        policy#
     C                   else
     C                   move      elfnd         xxfnd
     C                   move      elemp#        xxemp#
     C                   move      eldiv         xxdiv
     C                   endif                                                  *in71=*off
     C
     C                   if        *inof = *on
     C                   except    head
     C                   except    dtl2hd
     C                   endif
     C                   except    dtl2
     C
     C                   endif                                                  valDate<cvt(ehtrdt)
     C
     C                   endif                                                  *in72=*off
     C                   endif                                                  *in71=*off
     C
     C                   endif                                                  ihtrdt>=xxfrdt
     C
     C     ihky          reade     pih01                                  70
     C                   enddo                                                  *in70=*off
    C*
    C*---------------------------------------------------------------------
     C*  SDF / SUBRO / DEDUCTIBLE RECOVERIES
    C*---------------------------------------------------------------------
     C                   except    head
     C                   except    dtl3hd
     C*    read wdpap selecting patrdt within date range and patype='R', chain
     C*    to wtb2p with pabr2:  b2plcd='P' : SDF
     C*                          b2plcd='O' : Subro
     C*                          b2plcd='Q' : Deductible
     C     paky          chain     ppa01                              70
     C                   dow       *in70 = *off
     C
     C                   if        patrdt >= xxfrdt
     C                             and patrdt <= xxtodt
     C                             and patype = 'R'
     C
     C*                  *chain to get employer
     C                   eval       kkfyr = pafyr
     C                   eval       kkcase = pacase
     C     cmky          chain     pcm01                              71
     C                   if        *in71 = *off
     C                   eval       kkemp# = cmemp#
     C                   eval       kkdiv = cmdiv
     C     elky          chain     pel01                              72
     C                   if        *in72 = *off
     C
     C*                  *check if change made after valuation of 1st report
     C                   eval       transDt = cvtToDate(patrdt)
     C                   eval       polFrDt = cvtToDate(elpfdt)
     C     polFrDt       adddur    18:*m         valDate
     C                   if        valDate < transDt
     C
     C                   movel(p)  pabr2         kkkey
     C     tbky          chain     pb201                              73
     C                   if        *in73 = *off
     C
     C*                  *determine type of recovery
     C*                  *PRINT THIS GUY OUT
     C*               claimant
     C                   eval       claimant = %trim(cmfsn) + ' ' +
     C                                         cmmid + ' ' + %trim(cmlsn)
     C                   movel     claimant      clmntName        35
     C*               policy #
     C                   eval      kkemp# = elemp#
     C                   eval      kkdiv = eldiv
     C                   eval      kkfyr = elfyr
     C     rpky          chain     prp01                              71
     C                   if        *in71 = *off
     C                             and rpcap# <> *blank
     C                   movel(p)  rpcap#        policy#
     C                   else
     C                   move      elfnd         xxfnd
     C                   move      elemp#        xxemp#
     C                   move      eldiv         xxdiv
     C                   endif                                                  *in71=*off
     C
     C                   eval       xxinc = incFactor(elco# : elfnd : elfyr :
     C                                                 cmcase : patrdt : xxtodt)
     C
     C                   eval       xxpaid = pdFactor(elco# : elfnd : elfyr :
     C                                                 cmcase : patrdt : paamt)
     C
     C                   if        *inof = *on
     C                   except    head
     C                   except    dtl3hd
     C                   endif
     C                   except    dtl3
     C
     C                   endif                                                  *in73=*off
     C
     C                   endif                                                  valDate<cvt(patrdt)
     C
     C                   endif                                                  *in72=*off
     C                   endif                                                  *in71=*off
     C                   endif                                                  patrdt>=xxfrdt
     C
     C     paky          reade     ppa01                                  70
     C                   enddo                                                  *in70=*off
    C*
    C*---------------------------------------------------------------------
     C*  CLAIM DELETIONS
    C*---------------------------------------------------------------------
     C                   except    head
     C                   except    dtl4hd
     C*    read wacmp for deletions with the entered date range - print the deletion
     C*    if it occured after the valuation date of the first report
     C     c2ky          chain     pcm02                              70
     C                   dow       *in70 = *off
     C
     C                   if        c2dldt >= xxfrdt
     C                             and c2dldt <= xxtodt
     C
     C                   eval       kkemp# = c2emp#
     C                   eval       kkdiv = c2div
     C                   eval       kkfyr = c2fyr
     C     elky          chain     pel01                              72
     C                   if        *in72 = *off
     C
     C*                  *check if change made after valuation of 1st report
     C                   eval       accDate = cvtToDate(c2acdt)
     C                   eval       deletDt = cvtToDate(c2dldt)
     C                   eval       polFrDt = cvtToDate(elpfdt)
     C     polFrDt       adddur    18:*m         valDate
     C                   if        valDate < deletDt
     C
     C*                  *PRINT THIS GUY OUT
     C*               claimant
     C                   eval       claimant = %trim(c2fsn) + ' ' +
     C                                         c2mid + ' ' + %trim(c2lsn)
     C*               policy #
     C                   eval      kkemp# = elemp#
     C                   eval      kkdiv = eldiv
     C                   eval      kkfyr = elfyr
     C     rpky          chain     prp01                              71
     C                   if        *in71 = *off
     C                             and rpcap# <> *blank
     C                   movel(p)  rpcap#        policy#
     C                   else
     C                   move      elfnd         xxfnd
     C                   move      elemp#        xxemp#
     C                   move      eldiv         xxdiv
     C                   endif                                                  *in71=*off
     C
     C                   if        *inof = *on
     C                   except    head
     C                   except    dtl4hd
     C                   endif
     C                   except    dtl4
     C
     C                   endif                                                  valDate<cvt(ehtrdt)
     C
     C                   endif                                                  *in72=*off
     C
     C                   endif                                                  c2dldt>=xxfrdt
     C
     C     c2ky          reade     pcm02                                  70
     C                   enddo                                                  *in70=*off
    C*
    C*&&
    C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
          Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   move      *on           *inlr
    
    
    
     Oqsysprt1  e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ordis1              60
     O**********        09   upusnm              67
     O**********e            route          1
     O**********     08      ordis2              60
     O**********e            route          1
     O**********     08      ordis3              60
     O**********e            route          2
     O**********     08      ordis4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O               08      ordis1              60
     O                  09   upusnm              67
     O          e            route          1
     O               08      ordis2              60
     O          e            route          1
     O               08      ordis3              60
     O          e            route          2
     O               08      ordis4              60
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          1
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********e            route          1
     O**********                                 19 'PROGRAM NAME:'
     O**********             orpnam              30
     O**********                                 36 'FILE:'
     O**********             orfil#              38
     O**********                                 46 'COPIES:'
     O**********             orcopy        z     50
     O**********                                 56 'HOLD:'
     O**********             orhold              61
     O**********e            route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      oroutq              30
     O**********        09   qqque               30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 ************************************
    O**   SELECTION CRITERIA
     O          e            route          1
     O                                           20 'COMPANY      :'
     O                       rsco#         z     25
     O          e            route          1
     O                                           20 'GROUP        :'
     O                       rsfnd         z     25
     O          e            route          1
     O                                           20 'DATE RANGE   :'
     O                       rsfrdt              30 '  /  /    '
     O                                         +001 '-'
     O                       rstodt            +001 '  /  /    '
     O          e            route          1
     O                                           17 'AGGRAVATED '
     O                                         +000 'INEQUITY :'
     O                       rsagin        k   +002
     c*** Beg Add ***** 01/28/10 **************************************
     O****      e            route          1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                      +000 'VE DATA SOLUTIONS, INC.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
    O*
     Oqsysprt1  e            head           1 02
     O                       dstime              08 '  :  :  '
     O                       xxconm             129
     O                       dsdate        Y    198
     O          e            head           1
     O                       q1pgm               10
     O                       xxtitle            129
     O                                          193 'Page:'
     O                       page          Z    198
     O          e            head        1  1
     O                                            6 'Group:'
     O                       kkfnd         Z     10
     O                       xxfnam              72
     O          e            head           1
     O                                           12 'Date range: '
     O                       rsfrdt              23 '  /  /    '
     O                                         +001 '-'
     O                       rstodt            +001 '  /  /    '
     O          e            head           2
     O                                           11 'Aggravated '
     O                                         +000 'inequity:'
     O                       rsagin        k   +002
    O*

     O* AUDITS
     O          e            dtl1hd         1
     O                                           12 '***AUDITS***'
     O          e            dtl1hd         1
     O          e            dtl1hd         1
     O                                            3 'Yr'
     O                                           14 'Employer'
     O                                           24 'Policy #'
     O                                           47 'Name'
     O                                           95 'Pol Eff Dt'
     O                                          107 'Last Val Dt'
     O                                          119 'Audit Date'
     O          e            dtl1           1
     O                       ehfyr         Z      3
     O                       ehemp#        Z     14
     O                       policy#             41
     O                       xxname              83
     O                       polFrDt             95
     O                       valDate            107
     O                       transDt            119

     O* CLAIM INJURY TYPE CHANGES
     O          e            dtl2hd         1
     O                                           16 '***CLAIM INJURY '
     O                                         +000 'TYPE CHANGES***'
     O          e            dtl2hd         1
     O                                          122 'Changed'
     O          e            dtl2hd         1
     O                                            3 'Yr'
     O                                           10 'Case#'
     O                                           20 'Claimant'
     O                                           65 'Juris St'
     O                                           74 'Policy #'
     O                                          104 'Last Val Dt'
     O                                          116 'Trans Date'
     O                                          122 'From'
     O                                          125 'To'
     O          e            dtl2           1
     O                       ihfyr         Z      3
     O                       ihcase        Z     10
     O                       claimant            58
     O                       cmjrst              62
     O                       policy#             92
     O                       valDate            104
     O                       transDt            116
     O                       ihfrom             121
     O                       ihto               125

     O* SDF / SUBRO / DEDUCTIBLE RECOVERIES
     O          e            dtl3hd         1
     O                                           25 '***SDF / SUBRO / DEDUCTIB'
     O                                         +000 'LE RECOVERIES***'
     O          e            dtl3hd         1
     O                                          138 'Rsrv'
     O                                          142 'PV1'
     O          e            dtl3hd         1
     O                                            3 'Yr'
     O                                           10 'Case#'
     O                                           20 'Claimant'
     O                                           54 'Juris St'
     O                                           64 'Policy #'
     O                                           89 'Last Val Dt'
     O                                          101 'Trans Date'
     O                                          114 'Description'
     O                                          138 'Code'
     O                                          143 'Code'
     O                                          159 'Amount'
     O                                          179 'Factor'
     O                                          198 'Factor'
     O          e            dtl3           1
     O                       pafyr         Z      3
     O                       pacase        Z     10
     O                       clmntName           47
     O                       cmjrst              51
     O                       policy#             81
     O                       valDate             89
     O                       transDt            101
     O                       padesc             133
     O                       parecd             137
     O                       pabr2              142
     O                       paamt         J    160
     O                       xxInc         1    179
     O                       xxPaid        1    198

     O* CLAIM DELETIONS
     O          e            dtl4hd         1
     O                                           18 '***CLAIM DELETIONS'
     O          e            dtl4hd         1
     O                                          140 'Inj'
     O                                          147 'Loss'
     O          e            dtl4hd         1
     O                                            3 'Yr'
     O                                           10 'Case#'
     O                                           20 'Claimant'
     O                                           65 'Juris St'
     O                                           75 'Policy #'
     O                                          104 'Last Val Dt'
     O                                          116 'Deletion Dt'
     O                                          128 'Accident Dt'
     O                                          135 'Class'
     O                                          141 'Code'
     O                                          147 'Code'
     O                                          158 'Reason'
     O          e            dtl4           1
     O                       c2fyr         Z      3
     O                       c2case        Z     10
     O                       claimant            58
     O                       c2jrst              62
     O                       policy#             92
     O                       valDate            104
     O                       deletDt            116
     O                       accDate            128
     O                       c2clas        Z    135
     O                       c2ncci             140
     O                       c2lscv             146
     O                       c2dltr             156
      *


     P*=====================================================================
     P* cvtToDate - convert number to date data type
     P*  Parameter 1 - 8 numeric date
     P*  Returns - date data type (date will be 01/01/0001 if invalid)
     P*=====================================================================
     PcvtToDate        b                   export
     DcvtToDate        pi              d
     D numDate8                       8  0 Const
     D
     D date1           s               d
     C*
     C                   if        numDate8 < 1000000
     C     *mdy          test(d)                 numDate8               99
     C  n99*mdy          move      numDate8      date1
     C                   else
     C     *usa          test(d)                 numDate8               99
     C  n99*usa          move      numDate8      date1
     C                   endif
     C
     C                   if        *in99
     C     *cymd         test(d)                 numDate8               99
     C  n99*cymd         move      numDate8      date1
     C                   endif
     C
     C                   return    date1
     PCvtToDate        e


     P*=====================================================================
     P* incFactor - (1) calc incurred med & comp up to pmt date, (2) calc
     P*             incurred med & comp up to "to date".  return (2)/(1)
     P*  Parameters:
     P*            ppco#     company
     P*            ppfnd     group
     P*            ppfyr     policy year
     P*            ppcase    case
     P*            pptrdt    payment transaction date
     P*            pptodt    to date
     P*  Returns - factor (2) / (1)
     P*=====================================================================
     PincFactor        b                   export
     DincFactor        pi            15  4
     D ppco#                          3  0 Const
     D ppfnd                          3  0 Const
     D ppfyr                          3  0 Const
     D ppcase                         7  0 Const
     D pptrdt                         7  0 Const
     D pptodt                         7  0 Const
     D
     D inc1            s             11  2 inz(0)
     D inc2            s             11  2 inz(0)
     D factor          s             15  4

     C     rhky          klist
     C                   kfld                    ppco#
     C                   kfld                    ppfnd
     C                   kfld                    ppfyr
     C                   kfld                    ppcase
     C                   kfld                    ppdate            7 0
     C*
     C     rhky1         klist
     C                   kfld                    ppco#
     C                   kfld                    ppfnd
     C                   kfld                    ppfyr
     C                   kfld                    ppcase

     C* calc incurred 1 value
     C                   eval       ppdate = pptrdt
     C     rhky          setll     prh01
     C     rhky1         readpe    prh01                                  80
     C                   dow       *in80 = *off
     C
     C*    Loop through all reserve history entries in record
     C                   do        10            i                 3 0
     C                   clear                   prt01
     C                   movel(p)  rhcod(i)      kkkey
     C     tbky          chain     prt01                              60
     C*
     C                   if        rttype = 'C'
     C                             or rttype = 'M'
     C                   add       rhamt(i)      inc1
     C                   endif
     C
     C                   enddo                                                  10 i
     C
     C     rhky1         readpe    prh01                                  80
     C                   enddo                                                  *in80=*off

     C* calc incurred 2 value
     C                   eval       ppdate = pptodt
     C     rhky          setgt     prh01
     C     rhky1         readpe    prh01                                  80
     C                   dow       *in80 = *off
     C
     C*    Loop through all reserve history entries in record
     C                   do        10            i
     C                   clear                   prt01
     C                   movel(p)  rhcod(i)      kkkey
     C     tbky          chain     prt01                              60
     C*
     C                   if        rttype = 'C'
     C                             or rttype = 'M'
     C                   add       rhamt(i)      inc2
     C                   endif
     C
     C                   enddo                                                  10 i
     C
     C     rhky1         readpe    prh01                                  80
     C                   enddo                                                  *in80=*off

     C                   if        inc1 <> 0
     C                   eval       factor = inc2 / inc1
     C                   else
     C                   eval       factor = 0
     C                   endif

     C                   return    factor
     PincFactor        e


     P*=====================================================================
     P*  pdFactor - (1) calc incurred med & comp up to pmt date, (2) calc
     P*             incurred med & comp up to "to date".  return (1)/(2)
     P*  Parameters:
     P*            ppco#     company
     P*            ppfnd     group
     P*            ppfyr     policy year
     P*            ppcase    case
     P*            pptrdt    payment transaction date
     P*            pptodt    to date
     P*  Returns - factor (2) / (1)
     P*=====================================================================
     PpdFactor         b                   export
     DpdFactor         pi            15  4
     D ppco#                          3  0 Const
     D ppfnd                          3  0 Const
     D ppfyr                          3  0 Const
     D ppcase                         7  0 Const
     D pptrdt                         7  0 Const
     D ppamt                          9  2 Const
     D
     D pd1             s             11  2 inz(0)
     D pd2             s             11  2 inz(0)
     D factor          s             15  4
     D

     C     p2ky          klist
     C                   kfld                    ppco#
     C                   kfld                    ppfnd
     C                   kfld                    ppfyr
     C                   kfld                    ppcase

     C* calc incurred 1 value
     C     p2ky          chain     ppa02                              80
     C                   dow       *in80 = *off
     C                   if        p2trdt < pptrdt
     C
     C                   clear                   prt01
     C                   movel(p)  p2recd        kkkey
     C     tbky          chain     prt01                              60
     C*
     C                   if        rttype = 'C'
     C                             or rttype = 'M'
     C
     C                   select
     C                   when      p2type = 'P'
     C                             or p2type = 'A'
     C                             or p2type = 'M'
     C                             or p2type = 'Z'
     C                   add       p2amt         pd1
     C
     C                   when      p2type = 'V'
     C                             or p2type = 'S'
     C                             or p2type = 'R'
     C                   sub       p2amt         pd1
     C
     C                   endsl
     C
     C                   endif                                                  rttype=C or M
     C
     C                   endif                                                  p2trdt<pptrdt
     C     p2ky          reade     ppa02                                  80
     C                   enddo                                                  *in80=*off

     C* calc incurred 2 value
     C                   eval      pd2 = pd1 + ppamt

     C                   if        pd2 <> 0
     C                   eval       factor = pd1 / pd2
     C                   else
     C                   eval       factor = 0
     C                   endif

     C                   return    factor
     PpdFactor         e
