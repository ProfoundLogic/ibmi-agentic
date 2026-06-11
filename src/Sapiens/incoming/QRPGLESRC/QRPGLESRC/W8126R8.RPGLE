     * (C) Copyright Progressive Data Solutions, Inc., 2001
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *
     *****************************************************************
     *    PROGRAM: W8126R8                                           *
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
      *                                                               *
      * 03/15/05 C:136 Support: 45980  (WC909)                        *
      *                Process out-of-country addresses by entering   *
      *                'FN' for state and 99999 as zip per Clarendon. *
      *                Populate city with leftmost characters of      *
      *                postal/province code.                          *
      *                                                               *
      * 10/04/04 C:15  Project: WC970                                 *
      *                Per Laura at Clarendon, do not report 3rd party*
      *                recoveries.                                    *
      *                                                               *
      * 07/22/03  C:61  support wa099 32376
      *                 Change to use Wxpgl.
     *                                                               *
      * 05/22/03 C:15  Support: WC065/31676                           *
      *                Changed access to WDRPP.                       *
     *                                                               *
      * 07/09/02 C:15  Support: WC122/33284                           *
      *                Added code to prevent amount fields from being *
      *                blank in PMTRP.                                *
     *                                                               *
      * 01/22/02 C:15  Support: WC095/30740                           *
      *                Corrected code that loads the payee name and   *
      *                address in PMTRP.                              *
     *****************************************************************
     h DATFMT(*USA)
     *                                                               *
     Fpmtrp     o  a e             disk
      ******* beg chg ****** 07/22/03 *********************************
     F*********wxpgp     if   e             disk
     Fwxpgl     if   e             disk
      ******* end chg ****** 07/22/03 *********************************
     Fwmcml1    if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmpdl     if   e           k disk
XX   Fwmrpl4    if   e           k disk
     F                                     Rename(prp01:prp02)
     Fwdrpl     if   e           k disk
    F*** Begin add *** 05/22/03 **************************************
     Fwdrpl3    if   e           k disk    Rename(prp01:prp03)
    F*** End add ***** 05/22/03 **************************************
     Fwdpal5    if   e           k disk
     Fwdpalb    if   e           k disk
     F                                     Rename(Ppa01:Ppa02)
     F                                     prefix(p2:2)
     Fwmgcl     if   e           k disk
     Fwmeml     if   e           k disk
XX   Fwmrsl     if   e           k disk
     Fwdell     if   e           k disk
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
     F                                     ignore(ppa01)
     F                                     ignore(pb701)
     F                                     ignore(pbs01)
     F                                     ignore(ptt01)
    D*
    D*
     D Policy#         DS
     D  Pol1                   1      3
     D  Pol2                   4     10
     D  Pol3                  11     13
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
     D xxclm                         15
     D  pafnd                         3  0 overlay(xxclm:1)
     D  xxdsh1                        1    overlay(xxclm:4)
     D  pafyr                         3  0 overlay(xxclm:5)
     D  xxdsh2                        1    overlay(xxclm:8)
     D  pacase                        7  0 overlay(xxclm:9)
     D                 ds
     D tm2                            6  0
     D  zzhh                          2  0 overlay(tm2)
     D  zzmm                          2  0 overlay(tm2:3)
     D  zzss                          2  0 overlay(tm2:5)
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
     D xxco#           s                   like(cmco#)
     D xxdedr          s                   like(xxdeds)
     D ##clm           s                   like(trclm#)
     D svclm           s                   like(xxclm)
     D kktrdt          s                   like(patrdt)
     D grpkey          s             20
     D Efdkey          S             10
     D xxdeds          s             22  2
     D xxelig          s              1
     D xxexpd          s                   like(xxdeds)
     D xxexrv          s                   like(xxdeds)
     D xxinpd          s                   like(xxdeds)
     D xxinrv          s                   like(xxdeds)
     D xxkey           s                   like(b2key)
     D xxmdpd          s                   like(xxdeds)
     D xxmdrv          s                   like(xxdeds)
     D xxsant          s                   like(xxdeds)
     D xxsban          s                   like(xxdeds)
     D xxslvg          s                   like(xxdeds)
     D xxsubr          s                   like(xxdeds)
     D xxuapd          s                   like(xxdeds)
     D xxuarv          s                   like(xxdeds)
     D x               s              3  0
     D xx02            s             10
     D xx03            s             10
     D x7todt          s                   like(cmidt)
     D x8todt          s              8  0
     D Polkey          s             30
     D Editzip         C                    '0     -    '
     D H11A            S             11
      *
     D Datusa          s               D   DATFMT(*USA)
     D Today           s               D   DATFMT(*USA)
     D Tab10           S             10
     *** Begin add *** 07/09/02 *********************************************************
     D Zeros           S             20  2
     *** End add ***** 07/09/02 *********************************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     *  CLAIM MASTER FILE
     *
     C     cmky          klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     *
     *  CLAIMANT DEMOGRAPHICS FILE
     *
     C     cdky          klist
     C                   kfld                    cmss#
     *
     *  PAYMENT FILE
     *
     C     paky          klist
     C                   kfld                    pgco#
     C                   kfld                    pggrp
     *
     C     p2ky          klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C                   kfld                    pachk#
     C                   kfld                    pabasq
     *
     *  PROVIDER DEMOG FILE
     *
     C     pdky          klist
     C                   kfld                    pafein
     C                   kfld                    pamod
     *
     C     Wmeml_Key     Klist
     C                   Kfld                    pgco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Elemp#
     C                   Kfld                    Eldiv
      *
     C     Wmeml_Key3    Klist
     C                   Kfld                    pgco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Cmemp#
     C                   Kfld                    Cmdiv
      *
     C     Poky          Klist
     C                   Kfld                    Grpkey
     C                   Kfld                    Polkey
     C                   Kfld                    Efdkey
     *
     *  KEY LIST FOR THE RATE SET CODE FILE
     *
     C     rsky          klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
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
      *
     C     Wmgcl_Key     Klist
     C                   Kfld                    Pgco#
     C                   Kfld                    Pggrp
      *
     C     Wtabl_key     Klist
     C                   Kfld                    Pgco#
     C                   Kfld                    Tab10
      *
     *  CARRIER REPORTING MASTER FILE
     *
     C     rpky          klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C                   kfld                    pachk#
     C                   kfld                    kktrdt
     *
    C*** Begin add *** 05/22/03 **************************************
     C     r3ky          klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C                   kfld                    paseq#
    C*** End add ***** 05/22/03 **************************************
     *
     *  TABLE FILE
     *
     C     tbky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey
     *
     C     Wmrpl4_Key    Klist
     C                   Kfld                    xxco#
     C                   Kfld                    cmfnd
     C                   Kfld                    cmfyr
     C                   Kfld                    cmemp#
     C                   Kfld                    cmdiv
      *
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
     *
     C     plname        plist
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm      '1'           ##eoj
     *
     C     *Like         Define    Rpcap#        Svpol#
     /eject
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@todt
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
     *  USE GROUP CONTROL FILE
     *
     C                   read      xpg01                                  54
     *
     C                   dow       *in54 = *off
     *
     C                   clear                   ptr01
     * INCLUDE FUND ?
     C     pgflag        cabne     'I'           nxtfnd
     *
     C     Wmgcl_Key     Chain     Wmgcl
      *
     C                   If        not %Found
     C                   Eval      Gcpgid  = '888888'
     C                   Endif
      *
     *
     *  READ THE PAYMENT RECORDS
     *
     C     paky          chain     ppa01                              50
     *
     C                   dow       *in50 = *off
     *
     C                   if        patrdt >= x7frdt
     C                             and patrdt <= x7todt
      *
     C*** Begin add *** 10/04/04 ******************************************
     C                   exsr      Get_Trpycd
      *
     C                   if        trpycd <> *blanks
     C*** End add ***** 10/04/04 ******************************************
     *
     C                   exsr      pymt
     *
     * CHECK FOR POLICY RCD
     *
     C                   Movel(p)  Svpol#        Polkey
     C                   Movel     Pggrp         Grpkey
     *
     C     elky          chain     pel01                              95
     C                   Exsr      Wrtpol
     *
     C                   Eval      Trfyr  = Pafyr
     C                   Eval      Trfnd  = Pafnd
     C                   Eval      Tremp# = Cmemp#
     C                   Eval      Trcase = Cmcase
     C                   write     ptr01
     C                   Clear                   PTR01
      *
     C*** Begin add *** 10/04/04 ******************************************
     C                   Endif
     C*** End add ***** 10/04/04 ******************************************
      *
     C                   Endif
     *
     C     paky          reade     ppa01                                  50
     C                   enddo
     *
     C     nxtfnd        tag
     C                   read      xpg01                                  54
     C                   enddo
     *
     C                   move      *on           *inlr
     **
     /eject
     *===============================================================
     * PYMT  - PROCESS PAYMENT INFO
     *===============================================================
     *
     C     pymt          begsr
     *
     C     cmky          chain     pcm01                              91
     *
     *** IF NEW CASE, CHECK FOR, WRITE CLAIM INFO.
     *
     C                   if        xxclm <> svclm
     C                   movel(p)  xxclm         svclm
     C                   call      'W8126R7'
     C                   parm                    @@co#
     C                   parm                    @@todt
     C                   parm      xxclm         ##clm
     C                   endif
     *
     C                   movel(p)  gcpgid        trpgid
     C                   movel(p)  xxclm         trclm#
     C                   move      1             trclt#
     *
     C*** Begin dlt *** 10/04/04 ******************************************
     C**************     exsr      Get_Trpycd
     C*** End dlt ***** 10/04/04 ******************************************
     *
     C                   Eval      Tr9flg = 'N'
     C     pdky          chain     ppd01                              91
     C                   if        %found and
     C                             pd1099 = 'Y'
     C                   movel(p)  pd1099        tr9flg
     C                   endif
     *
     C                   movel(p)  pafein        trfein
     *
     C                   movel(p)  pachk#        trck#
     C                   clear                   trckdt
     *
     C                   if        patrdt <> *zeros
     C     *CYMD         Movel     patrdt        Datusa
     C                   Movel     Datusa        trckdt
     C                   endif
     *
     C                   If        PaType = 'V' or
     C                             Patype = 'S' or
     C                             PaType = 'R'
    C                   Eval      xxnbr = paamt * -1
     C                   Else
    C                   Eval      xxnbr = paamt
     C                   Endif
     *
     C                   Eval      trckam = %editc(xxnbr:'P')
     C                   Eval      Trckam# = Xxnbr
     *
     *** Begin add *** 07/09/02 *********************************************************
     C                   Eval      trinpd = %editc(Zeros:'P')
     C                   Eval      trmdpd = %editc(Zeros:'P')
     C                   Eval      trexpd = %editc(Zeros:'P')
     *** End add ***** 07/09/02 *********************************************************
     *
     C                   select
     *
     C     paretp        wheneq    'C'
     C                   Eval      trinpd = %editc(xxnbr:'P')
     C                   Eval      TrInpd# = Xxnbr
     *
     C     paretp        wheneq    'M'
     C                   Eval      trmdpd = %editc(xxnbr:'P')
     C                   Eval      Trmdpd# = Xxnbr
     *
     *
     C     paretp        wheneq    'E'
     C     paretp        oreq      'L'
     C                   Eval      trexpd = %editc(xxnbr:'P')
     C                   Eval      Trexpd# = Xxnbr
     *
     C                   endsl
     *
     C                   clear                   trsysd
     C                   if        patrdt <> *zeros
     C     *CYMD         Movel     patrdt        Datusa
     C                   Movel     Datusa        trsysd
     C                   endif
     *
     C                   clear                   tracdt
     C                   if        x7todt <> *zeros
     C     *CYMD         Movel     x7todt        Datusa
     C                   Movel     Datusa        tracdt
     C                   endif
     *
    C*** Begin add *** 05/22/03 **************************************
     C     r3ky          chain     prp03
     C                   If        not %Found
    C*** End add ***** 05/22/03 **************************************
     *
     * V,A,S,Z.
     C                   if        patype = 'V'
     C                             or patype = 'A'
     C                             or patype = 'S'
     C                             or patype = 'Z'
     C     p2ky          chain     ppa02
     C                   eval      kktrdt = p2trdt
     * P,M,R.
     C                   else
     C                   eval      kktrdt = patrdt
     C                   endif
     *
     C     rpky          chain     prp01
    C*** Begin add *** 05/22/03 **************************************
     C                   endif
    C*** End add ***** 05/22/03 **************************************
     *
     * HIT WDRPP
     C                   if        %found
     C                   movel(p)  rppnam        trnam1
     C                   movel(p)  rpadr1        tradr1
     C                   movel(p)  rpadr2        tradr2

      *** beg add ***** 03/15/05 **************************************
     C                   if        RPCTRY = *blanks and
     C                             RPADR3 = *blanks
      *** end add ***** 03/15/05 **************************************
     C                   movel(p)  rpcty         trcity
     C                   movel(p)  rpst          trst
     C                   Eval      H11A   = %Editw(rpzip:Editzip)
     C                   Move      H11A          trzip
      *** beg add ***** 03/15/05 **************************************
     C                   else
     C                   eval      TRCITY = RPADR3
     C                   eval      TRST   = 'FN'
     C                   eval      TRZIP  = '99999'
     C                   endif
      *** end add ***** 03/15/05 **************************************

     C                   endif
     *
     C                   if        trnam1 = *blanks
     C                   if        *in91 = *off
     C                   if        pdfsn = *blanks
     C                   movel(p)  pdname        trnam1
     *
     C                   else
     C                   movel     pdfsn         ##frst
     C                   movel     pdname        ##last
     C                   call      'SRNAME'      plname
     C                   movel     ##last        trnam1
     C                   endif
     *
     *** THE FOLLOWING WILL OVERRIDE THE NAME LOADED ABOVE IF DBA1 = Y
     *
     C                   if        pddba1 = 'Y'
     C                   movel(p)  pdadr1        trnam1
     C                   movel(p)  pdadr2        tradr1
     C                   else
     C                   movel(p)  pdadr1        tradr1
     C                   movel(p)  pdadr2        tradr2
     C                   endif
     *
      *** beg add ***** 03/15/05 **************************************
     C                   if        PDCTRY = *blanks and
     C                             PDADR3 = *blanks
      *** end add ***** 03/15/05 **************************************
     C                   movel(p)  pdcty         trcity
     C                   movel(p)  pdst          trst
     C                   Eval      H11A   = %Editw(pdzip:Editzip)
     C                   Move      H11A          trzip
      *** beg add ***** 03/15/05 **************************************
     C                   else
     C                   eval      TRCITY = PDADR3
     C                   eval      TRST   = 'FN'
     C                   eval      TRZIP  = '99999'
     C                   endif
      *** end add ***** 03/15/05 **************************************
      *
     * NO HIT WMPDP
      *
     C                   else
     C     cdky          chain     pcd01                              61
     C                   movel     cdfsn         ##frst
     C                   movel     cdlsn         ##last
     C                   call      'SRNAME'      plname
     C                   movel     ##last        trnam1
     C                   movel(p)  cdadr1        tradr1
     C                   movel(p)  cdadr2        tradr2

      *** beg add ***** 03/15/05 **************************************
     C                   if        CDCTRY = *blanks and
     C                             CDADR3 = *blanks
      *** end add ***** 03/15/05 **************************************
     C                   movel(p)  cdcty         trcity
     C                   movel(p)  cdst          trst
     C                   Eval      H11A   = %Editw(cdzip:Editzip)
     C                   Move      H11A          trzip
      *** beg add ***** 03/15/05 **************************************
     C                   else
     C                   eval      TRCITY = CDADR3
     C                   eval      TRST   = 'FN'
     C                   eval      TRZIP  = '99999'
     C                   endif
      *** end add ***** 03/15/05 **************************************

     C                   endif
     C                   endif
     *
     C                   endsr
     /eject
     **********************************************************************
     *  Get Payment Transaction Code for Pmtrp
     **********************************************************************
      *
     C     Get_Trpycd    Begsr
      *
     C                   If        Patype = 'R' and Paamt > 0
      *
     C                   movel(p)  pabr2         Tab10
     C     Wtabl_Key     Chain     Pb201
     C                   If        not %Found
     C                   Eval      B2plcd = *blank
     C                   Endif
      *
     C*** Begin add *** 10/04/04 ******************************************
     C                   If        B2plcd = 'O' or
     C                             B2plcd = 'P' or
     C                             B2plcd = 'Q'
     C                   Eval      Trpycd = *blanks
     C                   Endif
     C*** End add ***** 10/04/04 ******************************************
     C*** Begin dlt *** 10/04/04 ******************************************
     C*********          If        B2plcd = 'O' or
     C*********                    B2plcd = 'P'
     C*********          Eval      Trpycd = '12'
     C*********          Endif
      *
     C*********          If        B2plcd = 'Q'
     C*********          Eval      Trpycd = '28'
     C*********          Endif
     C*** End dlt ***** 10/04/04 ******************************************
      *
     C                   If        Paretp = 'C' or Paretp = 'M'
     C                   If        B2plcd <> 'O' and
     C                             B2plcd <> 'P' and
     C                             B2plcd <> 'Q'
     C                   Eval      Trpycd = '29'
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'E' or Paretp = 'L'
     C                   If        B2plcd <> 'O' and
     C                             B2plcd <> 'P' and
     C                             B2plcd <> 'Q'
     C                   Eval      Trpycd = '13'
     C                   Endif
     C                   Endif
      *
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'P' and paamt > 0 or
     C                             Patype = 'M' and paamt > 0 or
     C                             Patype = 'A' and paamt > 0 or
     C                             Patype = 'Z' and paamt > 0
      *
     C                   If        Paretp = 'C' or
     C                             Paretp = 'M'
     C                   If        Pafnal = 'Y'
     C                   Eval      Trpycd = '16'
     C                   Else
     C                   Eval      Trpycd = '17'
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
     C                   Eval      Trpycd = '27'
     C                   Endif
      *
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'V' and paamt > 0 or
     C                             Patype = 'S' and paamt > 0 or
     C                             Patype = 'A' and paamt < 0 or
     C                             Patype = 'M' and paamt < 0
      *
     C                   If        Paretp = 'C' or
     C                             Paretp = 'M'
     C                   Eval      Trpycd = '29'
     C                   Endif
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
     C                   Eval      Trpycd = '30'
     C                   Endif
      *
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'R'
      *
     C*** Begin add *** 10/04/04 ******************************************
     C                   movel(p)  pabr2         Tab10
     C     Wtabl_Key     Chain     Pb201
     C                   If        not %Found
     C                   Eval      B2plcd = *blank
     C                   Endif
      *
     C                   If        B2plcd = 'O' or
     C                             B2plcd = 'P' or
     C                             B2plcd = 'Q'
     C                   Eval      Trpycd = *blanks
     C                   Else
     C*** End add ***** 10/04/04 ******************************************
      *
     C                   If        Paretp = 'C' or
     C                             Paretp = 'M'
      *
     C                   If        Paamt < *zero
     C                   Eval      Trpycd = '33'
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
      *
     C                   If        Paamt < *zero
     C                   Eval      Trpycd = '34'
     C                   Endif
     C                   Endif
      *
     C*** Begin add *** 10/04/04 ******************************************
     C                   Endif
     C*** End add ***** 10/04/04 ******************************************
      *
     C                   Endif
      *
     C                   Endsr
      *
     /eject
     **********************************************************************
     *  Write Policy Master For Clarendon Pre Processing
     **********************************************************************
      *
     C     Wrtpol        Begsr
      *
     C                   call      'W8126R2'
     C                   parm      @@co#         ##co#             3
     C                   parm      @@todt        ##todt            8
     C                   parm      xxpol#        ##pol            15
     C                   Parm      CmDiv         ##Div             5 0
      *
      *
     C                   Endsr
    O*
