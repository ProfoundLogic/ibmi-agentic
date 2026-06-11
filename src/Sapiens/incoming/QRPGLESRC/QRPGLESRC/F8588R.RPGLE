    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
     H/TITLE F8588R
     H**************************************************************************
    H*  F8588R - Coventry EOR Financial Bills Check Issuance
    H*
    H*  Called from: F8587R01 - Coventry EOR Financial Bills with Errors Up
    H*  Calls to   : None
    H*
     H**************************************************************************
    H*  Created on 02/18/2008 by C:062 for Project 129MQN
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
     Ffix4p     uf   e             disk
     Ffix3l1    uf   e           k disk
     Ffixal1    uf   e           k disk
     Fwarpln    o    e             disk
     Fwarpl     if   e           k disk
     F                                     rename(prp01:prp02)
     Ffmcml1    if   e           k disk
     Fwmcml1    if   e           k disk
     F                                     rename(pcm01:pcm02)
     Fwmcil     if   e           k disk
     Ffmpdl     if   e           k disk
     Fwtabl     if   e           k disk
     F                                     ignore(pat01)
     F                                     ignore(psp01)
     F                                     ignore(pnc01)
     F                                     ignore(pna01)
     F                                     ignore(ppa01)
     F                                     ignore(pca01)
     F                                     ignore(prc01)
     F                                     ignore(pb201)
     F                                     ignore(pb701)
     F                                     ignore(pbs01)
     F                                     ignore(ptt01)
     F                                     ignore(pcc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     F                                     ignore(pcn01)
     F                                     ignore(prs01)
     F                                     ignore(plr01)
     F                                     ignore(pvs01)
     F                                     ignore(ptr01)
     F                                     ignore(phy01)
     F                                     ignore(psy01)
     F                                     ignore(pba01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)
     Fwtpsl     if   e           k disk
    D*****************************************************************
    D* Program constants.
    D*****************************************************************
     D comapi          c                   const('FA007R')
     D zeros           c                   const('000000000')
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D data                          50
     D  qxtrdt                        7  0 overlay(data)
     D  qxfrdt                        7  0 overlay(data:8)
     D  qxtodt                        7  0 overlay(data:15)
     D  qxtrcd                        2    overlay(data:22)
     D  qxrecd                        2    overlay(data:24)
     D  qxpv1                         2    overlay(data:26)
     D  qxpv2                         2    overlay(data:28)
    D*
     D                 ds
     D x3femd
     D  xxfein                        9  0        overlay(x3femd:*next)
     D  xxmod                         3  0        overlay(x3femd:*next)
    D*
     D                 ds
     D xachrg
     D  xachrg#                                   like(amouns) overlay(xachrg:2)
     D xafs
     D  xafs#                                     like(amouns) overlay(xafs:2)
     D xappo
     D  xappo#                                    like(amouns) overlay(xappo:2)
     D xapaid
     D  xapaid#                                   like(amouns) overlay(xapaid:2)
     D xasurc
     D  xasurc#                                   like(amouns) overlay(xasurc:2)
     D xaplfe
     D  xaplfe#                                   like(amouns) overlay(xaplfe:2)
     D xa2lnf
     D  xa2lnf#                                   like(amouns) overlay(xa2lnf:2)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D $$appl          s              2
     D $$case          s              7
     D $$cktp          s             10
     D $$co#           s              3
     D $$data          s             50
     D $$fnd           s              3
     D $$fyr           s              3
     D $$mode          s              1
     D $$rtcd          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fyr           s              3  0
     D ##rtcd          s              1
     D ##sub#          s              5  0
     D #1co#           s              3
     D #1fnd           s              3
     D #1sub#          s              5
     D #1wcf#          s              3
     D @@dcid          s             12
     D @@prvc          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D amount          s             12  2
     D amouns          s             12s 2
     D kkacdt          s                   like(cmacdt)
     D kkcase          s                   like(cmcase)
     D kkcntl          s                   like(x4cntl)
     D kkco#           s                   like(cmco#)
     D kkfein          s                   like(pdfein)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkkey           s                   like(pckey)
     D kkmod           s                   like(pdmod)
     D kkptyp          s                   like(ciptyp)
     D kkss#           s                   like(cmss#)
     D kksub#          s                   like(cmsub#)
     D kksys           s                   like(cisys)
     D kkwcf#          s                   like(cmfnd)
     D sign            s              1
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxasce          s              9  2
     D xxclne          s              9  2
     D xxdate          s              8  0
     D xxdateiso       s               d   datfmt(*ISO)
     D xxinpe          s              9  2
     D xxoute          s              9  2
     D xxpall          s                   like(rpamt)
     D xxprpo          s              9  2
     D xxseq#          s                   like(rpseq#)
     D xxsys           s              1
     D xxtall          s                   like(rpamt)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     I/COPY SCOPYWW01I
    I*
     Ipcm02
     I              cmoscl                      cwoscl
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@dcid
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
     C                   parm                    @@prvc
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcym2        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
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
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     cmky1         klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     cmky2         klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     ciky          klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    kkptyp
     C                   kfld                    kksys
    C*
     C     pdky1         klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     pdky2         klist
     C                   kfld                    x4fein
     C                   kfld                    x4mod
    C*
     C     rpky          klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey
    C*
     C     x3ky1         klist
     C                   kfld                    kkcntl
    C*
     C     xaky1         klist
     C                   kfld                    kkcntl
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*
    C*****************************************************************
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   exsr      mainsr
    C*
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
    C*
     C                   call      comapi        compl                  99
     C                   eval      *in16 = *off
    C*
     C     @@rrn2        chain(n)  px401                              98
     C                   if        *in98 = *off
    C*
     C                   eval      kkcntl = x4cntl
     C     x3ky1         chain(n)  px301                              98
     C     xaky1         chain(n)  pxa01                              98
     C                   exsr      xasign
    C*
     C     cmky1         chain     pcm01                              98
     C                   endif
    C*
     C                   if        *in98 = *off
     C     pdky2         chain     ppd01                              98
     C                   endif
    C*
     C                   if        *in98 = *off
    C*
     C                   move      ##co#         #1co#
     C                   move      cmsub#        #1sub#
     C                   move      cmfnd         #1fnd
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
     C                   if        ##rtcd = *blanks
     C                   move      #1wcf#        kkwcf#
     C                   else
     C                   eval      kkwcf# = *zeros
     C                   endif
    C*
     C                   if        @@prvc = 'Y'
     C                   eval      *in16 = *on
     C                   endif
    C*
     C                   exsr      writer
     C                   exsr      updat4
    C*
     C                   endif
    C*
     C                   exsr      pgmend
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* UPDAT4 - Update First Health Bill Header Record (FIX4P) - SINGLE MODE
    C*================================================================
     C     updat4        begsr
    C*
     C     @@rrn2        chain     px401                              98
     C                   if        *in98 = *off
    C*
     C                   movel(p)  'Y'           x4prfl
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x4prdt
    C*
     C                   update    px401
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* XASIGN - XA amounts sign
    C*================================================================
     C     xasign        begsr
    C*
     C                   if        %subst(xachrg:1:1)= '-'
     C                   eval      xachrg#= 0 - xachrg#
     C                   endif
     C                   if        %subst(xafs:1:1)  = '-'
     C                   eval      xafs#  = 0 - xafs#
     C                   endif
     C                   if        %subst(xappo:1:1) = '-'
     C                   eval      xappo# = 0 - xappo#
     C                   endif
     C                   if        %subst(xapaid:1:1)= '-'
     C                   eval      xapaid#= 0 - xapaid#
     C                   endif
     C                   if        %subst(xasurc:1:1)= '-'
     C                   eval      xasurc#= 0 - xasurc#
     C                   endif
     C                   if        %subst(xaplfe:1:1)= '-'
     C                   eval      xaplfe#= 0 - xaplfe#
     C                   endif
     C                   if        %subst(xa2lnf:1:1)= '-'
     C                   eval      xa2lnf#= 0 - xa2lnf#
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
    C* Write the check request
    C*
     C                   if        kkwcf# <> *zeros
    C*
     C                   exsr      wrtrp1
    C*
    C*
    C* BEG DEL ***** 11/28/06 *************************************************
    C*
     C******             exsr      wrtrp2
    C*
    C* END DEL ***** 11/28/06 *************************************************
    C*
    C*
     C***                if        x4ipbf = '1'
     C***                          or x4ipbf = '2'
     C                   exsr      wrtrp3
     C***                endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTRP1 - Write WARPP record  - Provider Check
    C*================================================================
     C     wrtrp1        begsr
    C*
     C     rpky          setgt     prp02
     C     rpky          readpe    prp02                                  99
B01  C                   if        *in99 = *off
     C                   eval      xxseq# = rpseq# + 1
X01  C                   else
     C                   eval      xxseq# = 1
E01  C                   endif
    C*
     C                   clear                   prp01
     C                   eval      rpco#  = cmco#
     C                   eval      rpfnd  = kkwcf#
     C                   eval      rpfyr  = cmfyr
     C                   eval      rpcase = cmcase
     C                   eval      rpseq# = xxseq#
    C*
     C                   time                    dsdttm
     C                   movel     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rptrdt
    C*
     C                   move      xxfein        rpfein
     C                   eval      rpmod = xxmod
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     rpfrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     rptodt
     C                   endif
    C*
B01  C                   if        pddba1 <> 'Y'
B02  C                   if        pdfsn = *blanks
     C                   move      pdname        rppnam
X02  C                   else
     C     pdfsn         cat(p)    pdname:1      rppnam
E02  C                   endif
     C                   move      pdadr1        rpadr1
     C                   move      pdadr2        rpadr2
X01  C                   else
     C                   move      pdadr1        rppnam
     C                   move      pdadr2        rpadr1
     C                   clear                   rpadr2
E01  C                   endif
     C                   move      pdcty         rpcty
     C                   move      pdst          rpst
     C                   move      pdzip         rpzip
    C*
     C                   eval      xxtall = xapaid#
     C                   eval      rpamt  = xxtall
    C*
     C                   select
    C*
     C                   when      %subst(x4cntl:1:2) >= '01' and
     C                             %subst(x4cntl:1:2) <= '05'
     C                   eval      xxsys   = 'H'
    C*
     C                   when      %subst(x4cntl:1:2) >= '06' and
     C                             %subst(x4cntl:1:2) <= '10'
     C                   eval      xxsys   = 'P'
    C*
     C                   when      %subst(x4cntl:1:2) >= '11' and
     C                             %subst(x4cntl:1:2) <= '13'
     C                   eval      xxsys   = 'P'
    C*
     C                   other
     C                   eval      xxsys   = 'P'
    C*
     C                   endsl
    C*
     C     cmky2         chain     pcm02                              99
     C                   movel     'P'           kkptyp
     C                   movel     xxsys         kksys
     C     ciky          chain     pci01                              99
B01  C                   if        *in99 = *off
    C*
B02  C                   if        cmcvfl = 'Y'
     C                   move(p)   cip1cv        rpbr2
X02  C                   else
     C                   move(p)   cibr2         rpbr2
E02  C                   endif
    C*
B02  C                   if        cmltfl = 'I'
     C                   move(p)   cibr7l        rpbr7
X02  C                   else
     C                   move(p)   cibr7n        rpbr7
E02  C                   endif
    C*
     C                   movel(p)  'PD'          rpsts
    C*
X01  C                   else
    C*
B02  C                   if        xxsys = 'P'
     C                   movel(p)  'MD'          rpbr2
X02  C                   else
     C                   movel(p)  'HP'          rpbr2
E02  C                   endif
    C*
B02  C                   if        cmltfl = 'I'
     C                   movel(p)  'LT'          rpbr7
X02  C                   else
     C                   movel(p)  'NL'          rpbr7
E02  C                   endif
    C*
     C                   movel(p)  'PD'          rpsts
    C*
E01  C                   endif
    C*
     C                   if        x4orid =  'GICSPCRPT'
     C                   movel(p)  'PE'          rpsts
E01  C                   endif
    C*
     C                   movel(p)  cmncci        rpncci
     C                   movel(p)  'MP'          rptrcd
     C                   movel(p)  'P'           rptype
     C                   movel(p)  'M'           rpretp
     C                   movel(p)  '11'          rprecd
    C*
     C                   movel(p)  rptrcd        kkkey
     C     tbky          chain     ppc01                              99
B01  C                   if        *in99 = *off
     C                   movel(p)  pcdesc        rpdesc
E01  C                   endif
    C*
     C                   movel     'P'           rpstat
     C                   movel(p)  q1user        rpuser
     C                   movel(p)  q1pgm         rppgm
    C*
     C                   eval      rptime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rpdt
     C                   movel     srdto         rprcvd
    C*
     C***                movel(p)  x4ref#        rprmds
    C*
     C                   movel(p)  rpsts         kkkey
     C     tbky          chain     pps01                              99
B01  C                   if        *in99 = *off
     C                   move(p)   pssrt         rpsort
E01  C                   endif
    C*
     C                   movel(p)  'N'           rpsent
     C                   movel(p)  q1user        rprqst
    C*
    C*  Call API to determine the Bank "Code".
    C*
     C                   move      rpco#         $$co#
     C                   move      rpfnd         $$fnd
     C                   move      rpfyr         $$fyr
     C                   move      rpcase        $$case
     C                   eval      qxtrdt = rptrdt
     C                   eval      qxfrdt = rpfrdt
     C                   eval      qxtodt = rptodt
     C                   movel     rptrcd        qxtrcd
     C                   movel     rprecd        qxrecd
     C                   movel     rpbr2         qxpv1
     C                   movel     rpbr7         qxpv2
    C*
     C                   call      'WA023R1'
     C                   parm      '2'           $$mode
     C                   parm                    $$co#
     C                   parm                    $$fnd
     C                   parm                    $$fyr
     C                   parm                    $$case
     C                   parm      'WC'          $$appl
     C                   parm      data          $$data
     C                   parm      *blanks       $$cktp
     C                   parm      *blanks       $$rtcd
    C*
     C                   movel(p)  $$cktp        rpcktp
    C*
     C                   movel(p)  rpuser        rpiusr
     C                   movel(p)  rppgm         rpipgm
     C                   eval      rpitm = rptime
     C                   eval      rpidt = rpdt
    C*
     C                   if        rpamt > *zeros
     C                   write     prp01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTRP2 - Write WARPP record  - Public Goods Pool Check
    C*================================================================
     C     wrtrp2        begsr
    C*
     C     rpky          setgt     prp02
     C     rpky          readpe    prp02                                  99
B01  C                   if        *in99 = *off
     C                   eval      xxseq# = rpseq# + 1
X01  C                   else
     C                   eval      xxseq# = 1
E01  C                   endif
    C*
     C                   clear                   prp01
     C                   eval      rpco# = cmco#
     C                   eval      rpfnd = kkwcf#
     C                   eval      rpfyr = cmfyr
     C                   eval      rpcase = cmcase
     C                   eval      rpseq# = xxseq#
     C                   time                    dsdttm
    C*
     C                   movel     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rptrdt
    C*
     C                   eval      rpfein = 141777232
    C*
     C                   eval      rpamt = xasurc#
    C*
     C     cmky2         chain     pcm02                              99
     C                   movel     'P'           kkptyp
     C                   movel     xxsys         kksys
     C     ciky          chain     pci01                              99
     C                   if        *in99 = *off
    C*
     C                   if        cmcvfl = 'Y'
     C                   move(p)   cip1cv        rpbr2
     C                   else
     C                   move(p)   cibr2         rpbr2
     C                   endif
    C*
     C                   if        cmltfl = 'I'
     C                   move(p)   cibr7l        rpbr7
     C                   else
     C                   move(p)   cibr7n        rpbr7
     C                   endif
    C*
     C                   movel(p)  'PD'          rpsts
    C*
     C                   else
    C*
     C                   if        xxsys = 'P'
     C                   movel(p)  'MD'          rpbr2
     C                   else
     C                   movel(p)  'HP'          rpbr2
     C                   endif
    C*
     C                   if        cmltfl = 'I'
     C                   movel(p)  'LT'          rpbr7
     C                   else
     C                   movel(p)  'NL'          rpbr7
     C                   endif
    C*
     C                   movel(p)  'PD'          rpsts
    C*
     C                   endif
    C*
     C                   movel(p)  cmncci        rpncci
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     rpfrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     rptodt
     C                   endif
    C*
     C                   movel(p)  'MP'          rptrcd
     C                   movel(p)  'P'           rptype
     C                   movel(p)  'M'           rpretp
     C                   movel(p)  '11'          rprecd
    C*
     C                   movel(p)  rptrcd        kkkey
     C     tbky          chain     ppc01                              99
     C                   if        *in99 = *off
     C                   movel(p)  pcdesc        rpdesc
     C                   endif
    C*
     C                   eval      kkfein = xxfein
     C                   eval      kkmod = xxmod
     C     pdky1         chain     ppd01                              99
     C                   if        *in99 = *off
     C                   if        pddba1 = 'Y'
     C                   movel(p)  pdadr1        rppnam
     C                   movel(p)  pdadr2        rpadr1
     C                   clear                   rpadr2
     C                   else
     C                   if        pdfsn = *blanks
     C                   movel(p)  pdname        rppnam
     C                   else
     C     pdfsn         cat(p)    pdname:1      rppnam
     C                   endif
     C                   movel(p)  pdadr1        rpadr1
     C                   movel(p)  pdadr2        rpadr2
     C                   endif
     C                   movel(p)  pdcty         rpcty
     C                   movel(p)  pdst          rpst
     C                   eval      rpzip = pdzip
     C                   endif
    C*
     C                   movel     'G'           rppgtp
     C                   movel     'P'           rpstat
     C                   movel(p)  q1user        rpuser
     C                   movel(p)  q1pgm         rppgm
     C                   eval      rptime = dstime
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rpdt
     C                   movel     srdto         rprcvd
    C*
     C***                movel(p)  x4ref#        rprmds
    C*
     C                   movel(p)  rpsts         kkkey
     C     tbky          chain     pps01                              99
     C                   if        *in99 = *off
     C                   move(p)   pssrt         rpsort
     C                   endif
    C*
     C                   movel(p)  'N'           rpsent
     C                   movel(p)  q1user        rprqst
    C*
    C*  Call API to determine the Bank "Code".
    C*
     C                   move      rpco#         $$co#
     C                   move      rpfnd         $$fnd
     C                   move      rpfyr         $$fyr
     C                   move      rpcase        $$case
     C                   eval      qxtrdt = rptrdt
     C                   eval      qxfrdt = rpfrdt
     C                   eval      qxtodt = rptodt
     C                   movel     rptrcd        qxtrcd
     C                   movel     rprecd        qxrecd
     C                   movel     rpbr2         qxpv1
     C                   movel     rpbr7         qxpv2
    C*
     C                   call      'WA023R1'
     C                   parm      '2'           $$mode
     C                   parm                    $$co#
     C                   parm                    $$fnd
     C                   parm                    $$fyr
     C                   parm                    $$case
     C                   parm      'WC'          $$appl
     C                   parm      data          $$data
     C                   parm      *blanks       $$cktp
     C                   parm      *blanks       $$rtcd
    C*
     C                   movel(p)  $$cktp        rpcktp
    C*
     C                   movel(p)  rpuser        rpiusr
     C                   movel(p)  rppgm         rpipgm
     C                   eval      rpitm = rptime
     C                   eval      rpidt = rpdt
    C*
     C                   if        rpamt > *zeros
     C                   write     prp01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTRP3 - Write WARPP record  - PATRIOT Check
    C*================================================================
     C     wrtrp3        begsr
    C*
     C     rpky          setgt     prp02
     C     rpky          readpe    prp02                                  99
     C                   if        *in99 = *off
     C                   eval      xxseq# = rpseq# + 1
     C                   else
     C                   eval      xxseq# = 1
     C                   endif
    C*
     C                   clear                   prp01
     C                   eval      rpco# = cmco#
     C                   eval      rpfnd = kkwcf#
     C                   eval      rpfyr = cmfyr
     C                   eval      rpcase = cmcase
     C                   eval      rpseq# = xxseq#
     C                   time                    dsdttm
    C*
     C                   movel     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rptrdt
    C*
     C                   eval      rpfein = 042658593
     C                   eval      rpmod = 1
    C*
     C                   eval      rpamt  = xaplfe# + xa2lnf#
    C*
     C     cmky2         chain     pcm02                              99
     C                   movel     'N'           kkptyp
     C                   movel     xxsys         kksys
     C     ciky          chain     pci01                              99
     C                   if        *in99 = *off
    C*
     C                   if        cmcvfl = 'Y'
     C                   move(p)   cip1cv        rpbr2
     C                   else
     C                   move(p)   cibr2         rpbr2
     C                   endif
    C*
     C                   if        cmltfl = 'I'
     C                   move(p)   cibr7l        rpbr7
     C                   else
     C                   move(p)   cibr7n        rpbr7
     C                   endif
    C*
     C                   movel(p)  'PD'          rpsts
    C*
     C                   else
    C*
     C                   if        xxsys = 'P'
     C                   movel(p)  'MD'          rpbr2
     C                   else
     C                   movel(p)  'HP'          rpbr2
     C                   endif
    C*
     C                   if        cmltfl = 'I'
     C                   movel(p)  'LT'          rpbr7
     C                   else
     C                   movel(p)  'NL'          rpbr7
     C                   endif
    C*
     C                   movel(p)  'PD'          rpsts
     C                   endif
    C*
     C                   movel(p)  cmncci        rpncci
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     rpfrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     rptodt
     C                   endif
    C*
     C                   movel(p)  'MP'          rptrcd
     C                   movel(p)  'P'           rptype
     C                   movel(p)  'M'           rpretp
     C                   movel(p)  '11'          rprecd
    C*
     C                   movel(p)  rptrcd        kkkey
     C     tbky          chain     ppc01                              99
     C                   if        *in99 = *off
     C                   movel(p)  pcdesc        rpdesc
     C                   endif
    C*
     C                   eval      kkfein = xxfein
     C                   eval      kkmod = xxmod
     C     pdky1         chain     ppd01                              99
     C                   if        *in99 = *off
     C                   if        pddba1 = 'Y'
     C                   movel(p)  pdadr1        rppnam
     C                   movel(p)  pdadr2        rpadr1
     C                   clear                   rpadr2
     C                   else
     C                   if        pdfsn = *blanks
     C                   movel(p)  pdname        rppnam
     C                   else
     C     pdfsn         cat(p)    pdname:1      rppnam
     C                   endif
     C                   movel(p)  pdadr1        rpadr1
     C                   movel(p)  pdadr2        rpadr2
     C                   endif
     C                   movel(p)  pdcty         rpcty
     C                   movel(p)  pdst          rpst
     C                   eval      rpzip = pdzip
     C                   endif
    C*
     C                   movel     'P'           rpstat
     C                   movel(p)  q1user        rpuser
     C                   movel(p)  q1pgm         rppgm
     C                   eval      rptime = dstime
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rpdt
     C                   movel     srdto         rprcvd
    C*
     C***                movel(p)  x4ref#        rprmds
    C*
     C                   movel(p)  rpsts         kkkey
     C     tbky          chain     pps01                              99
     C                   if        *in99 = *off
     C                   move(p)   pssrt         rpsort
     C                   endif
    C*
     C                   movel(p)  'N'           rpsent
     C                   movel(p)  q1user        rprqst
    C*
    C*  Call API to determine the Bank "Code".
    C*
     C                   move      rpco#         $$co#
     C                   move      rpfnd         $$fnd
     C                   move      rpfyr         $$fyr
     C                   move      rpcase        $$case
     C                   eval      qxtrdt = rptrdt
     C                   eval      qxfrdt = rpfrdt
     C                   eval      qxtodt = rptodt
     C                   movel     rptrcd        qxtrcd
     C                   movel     rprecd        qxrecd
     C                   movel     rpbr2         qxpv1
     C                   movel     rpbr7         qxpv2
    C*
     C                   call      'WA023R1'
     C                   parm      '2'           $$mode
     C                   parm                    $$co#
     C                   parm                    $$fnd
     C                   parm                    $$fyr
     C                   parm                    $$case
     C                   parm      'WC'          $$appl
     C                   parm      data          $$data
     C                   parm      *blanks       $$cktp
     C                   parm      *blanks       $$rtcd
    C*
     C                   movel(p)  $$cktp        rpcktp
    C*
     C                   movel(p)  rpuser        rpiusr
     C                   movel(p)  rppgm         rpipgm
     C                   eval      rpitm = rptime
     C                   eval      rpidt = rpdt
    C*
     C                   if        rpamt > *zeros
     C                   write     prp01
     C                   endif
    C*
     C                   endsr
