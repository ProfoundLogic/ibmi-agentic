    Hcopyright ('(c) Copyright Fiserv, Inc., 2005 - 2006')
    H*****************************************************************
    H/TITLE WA057R - API to process auto assignments
    H*****************************************************************
    H*  Description:
    H*
    H*    CHECK FOR AUTO ASSIGNMENT OF AUDITS PROCESSING.
    H*
    H*  Called From:
    H*    W7231R
    H*
    H*  Calls To:
    H*
    H*    None
    H*
    H*  Created:  03/23/05  C:53  Project:  WC1196
    H*
    H*****************************************************************
    H*  Revisions:
    H*
    H*  09/04/08  C:62  Project: WC1611
    H*                  Use ELNWRW
    H*
    H*  10/02/06  C:33  Project: WC1196
    H*                  Use program to retrieve New/Renew flag
    H*****************************************************************
    H*  Indicators:
    H*
    H*    99 - General File Access
    H*****************************************************************
    H*
     Fwmeml     if   e           k disk
     Fwtgtl     if   e           k disk
     Fwdrrl1    if   e           k disk
     Fwtssl1    if   e           k disk
     Fwdehl3    if   e           k disk
     Fwdell8    if   e           k disk
     Fwtaal1    if   e           k disk
     Fwmhap     uf a e             disk
     F*** begin add *** 09/04/08 *******************************************
     Fwdell     if   e           k disk    rename(pel01:pel02)
     F*** end   add *** 09/04/08 *******************************************
    F*
     D psds          esds                  extname(szq1p)
     D er              s             50    dim(11) ctdata perrcd(1)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kiefdt          s              7
     D kifnd           s              3
     D kifyr           s              3
     D kiprml          s              9
     D kizip           s              9
     D kkbctg          s                   like(ssbctg)
     D kkcnty          s                   like(aacnty)
     D kkco#           s                   like(emco#)
     D kkdiv           s                   like(emdiv)
     D kkefdt          s                   like(ssdt)
     D kkemp#          s                   like(ememp#)
     D kkfnd           s                   like(emfnd)
     D kkfyr           s                   like(elfyr)
     D kkkey           s                   like(gtkey)
     D kknrbs          s                   like(ssnrbs)
     D kkprml          s                   like(ssprml)
     D kkrrst          s                   like(rrst)
     D kkst            s                   like(aast)
     D kktbid          s                   like(gttbid)
     D kktypa          s                   like(aatypa)
     D kkzip           s                   like(aazip)
     D kykey           s                   like(gtkey)
     D kytbid          s                   like(gttbid)
     D qqaco           s              3
     D qqco#           s              3
     D qqdiv           s              5
     D qqemp#          s              9
     D qqerr           s             50
     D qqfnd           s              3
     D qqfyr           s              3
     D qqinfo          s             30
     D qqnrbs          s              1
     D qqtypa          s              2
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxdiv           s                   like(emdiv)
     D xxdt            s                   like(ssdt)
     D xxemp#          s                   like(ememp#)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  10/02/06  **************************************
     DGetRenewFlg      pr                  like(EHNWRW)
     D inCo#                               like(qqCo#)
     D inFnd                               like(qqFnd)
     D inEmp#                              like(qqEmp#)
     D inDiv                               like(qqDiv)
     D inFyr                               like(qqFyr)
     D inDate                              like(xxDt)
     D***  End   Add  ***  10/02/06  **************************************
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    qqtypa
     C                   parm                    qqnrbs
     C                   parm                    qqaco
     C                   parm                    qqerr
     C                   parm                    qqinfo
    C*
     C     kyem          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
    C*
     C     kyrr          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrrst
    C*
     C     kkread        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     *
     C     kygt          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*
     C     kyssst        klist
     C                   kfld                    kkco#
     C                   kfld                    kkbctg
     C                   kfld                    kknrbs
     C                   kfld                    kkprml
    C*
     C     kyss          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbctg
     C                   kfld                    kknrbs
    C*
     C     kyehst        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkefdt
    C*
     C     kyeh          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
    C*
     C     kyel          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr
    C*
     C     kyaast        klist
     C                   kfld                    kkco#
     C                   kfld                    kktypa
     C                   kfld                    kkst
     C                   kfld                    kkcnty
     C***  Begin Del  ***  10/02/06  **************************************
     C**********         kfld                    kkzip
     C***  End   Del  ***  10/02/06  **************************************
     C                   kfld                    kkprml
    C*
     C     kyaa          klist
     C                   kfld                    kkco#
     C                   kfld                    kktypa
     C                   kfld                    kkst
     C                   kfld                    kkcnty
     C***  Begin Del  ***  10/02/06  **************************************
     C**********         kfld                    kkzip
     C***  End   Del  ***  10/02/06  **************************************
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C                   clear                   pem01
     C                   clear                   pel01
     C                   clear                   prr01
     C                   clear                   pgt01
     C                   clear                   pss01
     C                   clear                   peh01
     C                   clear                   qqtypa
     C                   clear                   qqnrbs
     C                   clear                   qqaco
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt
     C*
     C***  Begin Add  ***  10/02/06  **************************************
     C                   eval      qqNRBs = GetRenewFlg(qqCo#:qqFnd:
     C                                             qqEmp#:qqDiv:qqFyr:xxDt)
     C***  End   Add  ***  10/02/06  **************************************
    C*
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqemp#        kkemp#
     C                   clear                   kkdiv
     C                   move      qqfyr         kkfyr
    C*
     C     kyem          chain     pem01                              99
    C*
     C                   dou       *in99 = *on
    C*
     C                   if        emstrp = 'Y'
     C                   move      *on           *in99
     C                   iter
     C                   endif
    C*
     C     kyem          reade     pem01                                  99
     C                   if        *in99 = *on
     C                   movel(p)  er(1)         qqerr
     C                   return
     C                   endif
    C*
     C                   enddo
    C*
     C                   if        emstrp = ' '
     C                   movel(p)  er(1)         qqerr
     C                   return
     C                   endif
    C*
     C                   if        empst <> *blanks
     C                   move      empst         kkrrst
     C                   else
     C                   move      emmst         kkrrst
     C                   endif
    C*
     C     kyrr          chain     prr01                              99
    C*
     C                   if        *in99 = *on
     C                   movel(p)  er(2)         qqerr
     C                   movel     kkfnd         kifnd
     C     kifnd         cat(p)    kkrrst:1      qqinfo
     C                   return
     C                   endif
    C*
     C                   if        rrnaic = 'Y'
     C                   movel(p)  'NAIC'        kktbid
     C                   movel(p)  emnaic        kkkey
    C*
     C                   else
     C                   movel(p)  'SIC'         kktbid
     C                   movel(p)  emsic         kkkey
     C                   endif
    C*
     C     kygt          chain     pgt01                              99
    C*
     C                   if        *in99 = *on
    C*
     C                   if        rrnaic = 'Y'
     C                   movel(p)  er(3)         qqerr
    C*
     C                   else
     C                   movel(p)  er(4)         qqerr
     C                   endif
    C*
     C     kkkey         cat(p)    kktbid:1      qqinfo
     C                   return
     C                   endif
    C*
     C                   if        gtcf01 = *blanks
    C*
     C                   if        rrnaic = 'Y'
     C                   movel(p)  er(5)         qqerr
    C*
     C                   else
     C                   movel(p)  er(6)         qqerr
     C                   endif
    C*
     C     kkkey         cat(p)    kktbid:1      qqinfo
     C                   return
     C                   endif
    C*
     C                   movel(p)  gtcf01        kkbctg
    C*
     C                   movel     xxdt          kkefdt
     C     kyehst        setgt     peh01
     C     kyeh          readpe    peh01                                  99
    C*
     C                   if        *in99 = *on
     C                   movel(p)  er(7)         qqerr
     C                   movel     kkfyr         kifyr
     C                   movel     kkefdt        kiefdt
     C     kifyr         cat(p)    kiefdt:1      qqinfo
     C                   return
     C                   endif
    C*
     C*** begin chg *** 09/04/08 *******************************************
     C**********         move      ehnwrw        kknrbs
     C**********         move      ehnwrw        qqnrbs
     C                   move      qqnrbs        kknrbs
     C*** end   chg *** 09/04/08 *******************************************
    C*
     C                   eval      kkprml = *zeros
    C*
     C     kyel          chain     pel01                              99
    C*
     C                   dou       *in99 = *on
    C*
     C                   if        elfaf = 'A'
     C                   eval      kkprml = kkprml + elaapr
     C                   else
     C                   eval      kkprml = kkprml + elanpr
     C                   endif
    C*
     C     kyel          reade     pel01                                  99
     C                   enddo
    C*
     C     kyssst        setgt     pss01
     C     kyss          readpe    pss01                                  99
    C*
     C                   if        *in99 = *on
     C                   movel(p)  er(8)         qqerr
     C                   movel     kkprml        kiprml
     C     kkbctg        cat(p)    kknrbs:1      qqinfo
     C     qqinfo        cat(p)    kiprml:1      qqinfo
     C                   return
     C                   endif
    C*
     C                   if        kkprml <= ssprmh
     C                   move      sstypa        qqtypa
    C*
     C                   else
     C     kkbctg        cat(p)    kknrbs:1      qqinfo
     C     qqinfo        cat(p)    kiprml:1      qqinfo
     C                   movel(p)  er(9)         qqerr
     C                   return
     C                   endif
    C*
     C                   move      qqdiv         kkdiv
     C     kyem          chain     pem01                              99
    C*
     C                   if        *in99 = *on
     C                   movel(p)  er(10)        qqerr
     C                   return
     C                   endif
    C*
     C                   move      emcnty        kkcnty
     C                   move      qqtypa        kktypa
    C*
     C                   if        empst <> *blanks
     C                   move      empst         kkst
     C                   else
     C                   move      emmst         kkst
     C                   endif
    C*
     C                   if        empzip <> *zeros
     C                   move      empzip        kkzip
     C                   else
     C                   move      emmzip        kkzip
     C                   endif
    C*
     C     kyaast        setgt     paa01
     C     kyaa          readpe    paa01                                  99
    C*
     C                   if        *in99 = *on
     C                   movel(p)  er(11)        qqerr
     C                   movel     kkzip         kizip
     C                   movel     kkprml        kiprml
     C     kktypa        cat(p)    kkst:1        qqinfo
     C     qqinfo        cat(p)    kkcnty:1      qqinfo
     C     qqinfo        cat(p)    kizip:1       qqinfo
     C     qqinfo        cat(p)    kiprml:1      qqinfo
     C                   return
     C                   endif
    C*
     C                   move      aaaco         qqaco
    C*
    C* Write history record
    C*
     C                   eval      haco# = aaco#
     C                   eval      hafnd = kkfnd
     C                   eval      haemp# = kkemp#
     C                   eval      hadiv = kkdiv
     C                   eval      hafyr = kkfyr
     C                   eval      hanac = aaaco
     C                   eval      hatypa = aatypa
     C                   eval      hast = aast
     C                   eval      hacnty = aacnty
     C                   eval      hazip = aazip
     C*
     C*************      eval      hapac =
     C                   eval      hatypb = aatypb
     C                   eval      haprem = kkprml
     C                   eval      aastat = 'A'
     C*
     C                   eval      hauser = q1user
     C                   eval      hapgm = q1pgm
     C                   time                    dsdttm
     C                   eval      hatime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         hadt
     C*
     C                   eval      haiusr = q1user
     C                   eval      haipgm = q1pgm
     C                   eval      haitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         haidt
    C*
     C                   write     pha01
    C*
     C                   return
     C*





     P***  Begin Add  ***  10/02/06  **************************************
     P*====================================================================
     P* Get New/Renewal Flag
     P*====================================================================
     PGetRenewFlg      b
     DGetRenewFlg      pi                  like(EHNWRW)
     D inCo#                               like(qqCo#)
     D inFnd                               like(qqFnd)
     D inEmp#                              like(qqEmp#)
     D inDiv                               like(qqDiv)
     D inFyr                               like(qqFyr)
     D inDate                              like(xxDt)

     D co#             s                   like(EHCO#)
     D fnd             s                   like(EHFND)
     D emp#            s                   like(EHEMP#)
     D div             s                   like(EHDIV)
     D fyr             s                   like(EHFYR)
     D date            s                   like(EHEFDT)
    
     C     keyWDEHL3     klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    fyr
     C                   kfld                    date
    C*
     C     key1WDEHL3    klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    fyr

     C                   move      inCo#         co#
     C                   move      inFnd         fnd
     C                   move      inEmp#        emp#
     C                   move      inDiv         div
     C                   move      inFyr         fyr
     C                   z-add     inDate        date

     C*** begin chg *** 09/04/08 *******************************************
     C**********         clear                   EHNWRW
     C*****keyWDEHL3     setgt     peh01
     C*****key1WDEHL3    readpe    peh01

     C**********         return    EHNWRW
     C                   clear                   ELNWRW
     C     key1WDEHL3    chain     pel02

     C                   return    ELNWRW
     C*** end   chg *** 09/04/08 *******************************************

     PGetRenewFlg      e
     P***  End   Add  ***  10/02/06  **************************************
**  ER
No state reporting division record found
No record match in WDRRP
No NAIC code found in the table
No SIC code found in the table
No business catagory associated with the NAIC code
No business catagory associated with the SIC code
No record match in WDEHP
No record match in WTSSP
Premium amount out of range for WTSSP
No record match in WMEMP
No record match in WTAAP
