    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE F8282R
     H**************************************************************************
    H*  First Health EOR Financial Process - Transfer program 2
    H*
    H*  Called from: F8281R - First Health EOR Financial Process - Transfer pro
    H*  Calls to   : F8282C - First Health EOR Financial Process - Delete Docum
    H*
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8262R from Client 129
     H**************************************************************************
    H*  Revisions:
    H*
    H*  11/28/06 C: 182 Support: 56004
    H*              Removed the logic to call subroutine WRTRP2 which writes
    H*              checks for public goods pool.
    H*
    H*  10/03/06 C: 079 Support: 55006
    H*              Add service date calculation to all subroutines where
    H*              FAX4P is updated.
    H*
    H*  09/22/06 C: 053 Support: 54967
    H*              Correct data issue.
    H*
    H*  09/18/06 C: 079 Support: 54851
    H*              If X4FDOS/X4TDOS are blank then calculate the from
    H*              and to dates
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
    F*
    F* Import files
    F*1
     Ffax1l1    uf   e           k disk
     Ffax2l1    uf   e           k disk
     Ffax3l1    uf   e           k disk
     Ffax4l1    uf   e           k disk    rename(px401:px402)
     Ffax4l2    uf   e           k disk    rename(px401:px403)
     Ffax4p     uf   e             disk
     Ffax5l1    uf   e           k disk
     Ffax5l2    uf   e           k disk    rename(px501:px502)
     F*** BEG ADD *** 09/18/06 *********************************************
     Ffax5l3    if   e           k disk    rename(px501:px503) prefix(d1:2)
     Ffax5l4    if   e           k disk    rename(px501:px504) prefix(d2:2)
     F*** END ADD *** 09/18/06 *********************************************
     Ffax6l1    uf   e           k disk
     Ffax7l1    uf   e           k disk
     Ffax8l1    uf   e           k disk
     FfaxAl1    uf   e           k disk
     FfaxBl1    uf   e           k disk
    F*
    F* Transmission control file
    F*
     Ffax9p     uf   e             disk
    F*
    F* Header files
    F*
     Ffaeol4    uf a e           k disk    infds(dbfds)
     Ffaeol74   if   e           k disk    rename(peo01:peo02)
     Ffaeal1    if a e           k disk
     Ffaeal37   if   e           k disk    rename(pea01:pea02)
     Ffaesl2    if a e           k disk
     Ffaeel     uf a e           k disk
     Ffaeel4    if   e           k disk    rename(pee01:pee02)
     Ffaehl     if a e           k disk
     Ffaepl     if a e           k disk
     Ffaeul     if a e           k disk
    F*
    F* Detail files
    F*
     Ffdcap     o    e             disk
     Ffddal2    if a e           k disk
     Ffddel     uf a e           k disk
     Ffddel5    if   e           k disk    rename(pde01:pde02)
     Ffddpl     uf a e           k disk
     Ffddhl     uf a e           k disk
     Ffddul     uf a e           k disk
    F*
    F* Check Request file
    F*
     Fwarpln    if a e           k disk
    F*
    F* Miscellaneous files
    F*
     Ffdcalf    if   e           k disk    rename(pca01:pca02)
     Ffdcal19   uf   e           k disk    rename(pca01:pca03)
     Ffmc1l     uf   e           k disk
     Ffmcdl     if   e           k disk
     Ffmcml1    if   e           k disk
     Ffmdil2    uf a e           k disk
     Ffmf1l     uf   e           k disk
     Ffmgzl     if   e           k disk
     Ffmpdl     if   e           k disk
     Fnmnwl     if a e           k disk
     Fststl1    if   e           k disk
     Fwarpl     if   e           k disk    rename(prp01:prp02)
     Fwmcml1    if   e           k disk    rename(pcm01:pcm02)
     Fwmcil     if   e           k disk
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
     Fftbrl1    if   e           k disk
     Fftpxl1    if   e           k disk
    F*
     D bid             s              1    dim(25)
     D bi              s              1    dim(25)
     D dii             s              1    dim(6)
     D dio             s              1    dim(6)
     D nte             s              1    dim(280)
    D*****************************************************************
    D* Program constants.
    D*****************************************************************
     D comapi          c                   const('FA007R')
     D reas1           c                   const('First Health bill upload')
     D reas2           c                   const('Reconsidered by First Health')
     D codeds          c                   const('MEDICINE DISPENSED -
     D                                            BY PHYSICIA')
     D zeros           c                   const('000000000')
     D msg1            c                   const('This pharmacy code was used:')
     D msg2            c                   const('The provider replaced')
     D msg3            c                   const('First Health has adjusted -
     D                                            this bill by using the code')
     D msg4            c                   const('with the modifier')
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D claim#                        20
     D  xxwcf#                        3    overlay(claim#)
     D  xxfyr                         3    overlay(claim#:4)
     D  xxcase                        7    overlay(claim#:7)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D kkcmsv                        11
     D  kkrvcd                        3    inz('000')
     D                                     overlay(kkcmsv)
     D  kkhcpc                        5    overlay(kkcmsv:4)
     D  kkunts                        3    inz('000')
     D                                     overlay(kkcmsv:9)
    D*
     D                 ds
     D xxxref                         4  0
     D  dx1                           1  0 overlay(xxxref)
     D  dx2                           1  0 overlay(xxxref:2)
     D  dx3                           1  0 overlay(xxxref:3)
     D  dx4                           1  0 overlay(xxxref:4)
     D                 ds
     D data                          50
     D  qxtrdt                        7  0 overlay(data)
     D  qxfrdt                        7  0 overlay(data:8)
     D  qxtodt                        7  0 overlay(data:15)
     D  qxtrcd                        2    overlay(data:22)
     D  qxrecd                        2    overlay(data:24)
     D  qxpv1                         2    overlay(data:26)
     D  qxpv2                         2    overlay(data:28)
     D                 ds
     D  nw                     1     50
     D                                     dim(10)
     D  f1nw01                 1      5
     D  f1nw02                 6     10
     D  f1nw03                11     15
     D  f1nw04                16     20
     D  f1nw05                21     25
     D  f1nw06                26     30
     D  f1nw07                31     35
     D  f1nw08                36     40
     D  f1nw09                41     45
     D  f1nw10                46     50
     D                 ds
     D  nf                     1     10
     D                                     dim(10)
     D  f1nf01                 1      1
     D  f1nf02                 2      2
     D  f1nf03                 3      3
     D  f1nf04                 4      4
     D  f1nf05                 5      5
     D  f1nf06                 6      6
     D  f1nf07                 7      7
     D  f1nf08                 8      8
     D  f1nf09                 9      9
     D  f1nf10                10     10
     D                 ds
     D  nt                     1     10
     D                                     dim(10)
     D  f1nt01                 1      1
     D  f1nt02                 2      2
     D  f1nt03                 3      3
     D  f1nt04                 4      4
     D  f1nt05                 5      5
     D  f1nt06                 6      6
     D  f1nt07                 7      7
     D  f1nt08                 8      8
     D  f1nt09                 9      9
     D  f1nt10                10     10
     D                 ds
     D  ni                     1     90  0
     D                                     dim(10)
     D  f1ni01                 1      9  0
     D  f1ni02                10     18  0
     D  f1ni03                19     27  0
     D  f1ni04                28     36  0
     D  f1ni05                37     45  0
     D  f1ni06                46     54  0
     D  f1ni07                55     63  0
     D  f1ni08                64     72  0
     D  f1ni09                73     81  0
     D  f1ni10                82     90  0
     D                 ds
     D  nm                     1     30  0
     D                                     dim(10)
     D  f1nm01                 1      3  0
     D  f1nm02                 4      6  0
     D  f1nm03                 7      9  0
     D  f1nm04                10     12  0
     D  f1nm05                13     15  0
     D  f1nm06                16     18  0
     D  f1nm07                19     21  0
     D  f1nm08                22     24  0
     D  f1nm09                25     27  0
     D  f1nm10                28     30  0
     D                 ds
     D note                         140
     D  xxnot1                       70    overlay(note)
     D  xxnot2                       70    overlay(note:71)
    D*
     D                 ds
     D mcdds                         32
     D  x6mcd1                                    overlay(mcdds:1)
     D  x6mcd2                                    overlay(mcdds:*next)
     D  x6mcd3                                    overlay(mcdds:*next)
     D  x6mcd4                                    overlay(mcdds:*next)
     D  x6mcd5                                    overlay(mcdds:*next)
     D  x6mcd6                                    overlay(mcdds:*next)
     D  x6mcd7                                    overlay(mcdds:*next)
     D  x6mcd8                                    overlay(mcdds:*next)
     D  mcd                           4    dim(8) overlay(mcdds:1)
    D*
     D                 ds
     D rcdds                         16
     D  x6rcd1                                    overlay(rcdds:1)
     D  x6rcd2                                    overlay(rcdds:*next)
     D  x6rcd3                                    overlay(rcdds:*next)
     D  x6rcd4                                    overlay(rcdds:*next)
     D  x6rcd5                                    overlay(rcdds:*next)
     D  x6rcd6                                    overlay(rcdds:*next)
     D  x6rcd7                                    overlay(rcdds:*next)
     D  x6rcd8                                    overlay(rcdds:*next)
     D  rcd                           2    dim(8) overlay(rcdds:1)
    D*
    D*
     D                 ds
     D diads                         30
     D  x8dia1                                    overlay(diads:1)
     D  x8dia2                                    overlay(diads:*next)
     D  x8dia3                                    overlay(diads:*next)
     D  x8dia4                                    overlay(diads:*next)
     D  x8dia5                                    overlay(diads:*next)
     D  dia                           6    dim(5) overlay(diads:1)
    D*
     D                 ds
     D didds                        600
     D  x8d1ds                                    overlay(didds:1)
     D  x8d2ds                                    overlay(didds:*next)
     D  x8d3ds                                    overlay(didds:*next)
     D  x8d4ds                                    overlay(didds:*next)
     D  x8d5ds                                    overlay(didds:*next)
     D  did                         120    dim(5) overlay(didds:1)
    D*
     D                 ds
     D x3femd
     D  xxfein                        9  0        overlay(x3femd:*next)
     D  xxmod                         3  0        overlay(x3femd:*next)
    D*
     D                 ds
     D x5chrg
     D  x5chrg#                                   like(amouns) overlay(x5chrg:2)
     D x5fs
     D  x5fs#                                     like(amouns) overlay(x5fs:2)
     D x5ppo
     D  x5ppo#                                    like(amouns) overlay(x5ppo:2)
     D x5paid
     D  x5paid#                                   like(amouns) overlay(x5paid:2)
    D*
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
     D xapbfe
     D  xapbfe#                                   like(amouns) overlay(xapbfe:2)
     D xa2ts1
     D  xa2ts1#                                   like(amouns) overlay(xa2ts1:2)
     D xa2ts2
     D  xa2ts2#                                   like(amouns) overlay(xa2ts2:2)
     D xa2ts3
     D  xa2ts3#                                   like(amouns) overlay(xa2ts3:2)
     D xa2ts4
     D  xa2ts4#                                   like(amouns) overlay(xa2ts4:2)
     D xa2ts5
     D  xa2ts5#                                   like(amouns) overlay(xa2ts5:2)
     D xa2ts6
     D  xa2ts6#                                   like(amouns) overlay(xa2ts6:2)
     D xa2ts7
     D  xa2ts7#                                   like(amouns) overlay(xa2ts7:2)
     D xa2ts8
     D  xa2ts8#                                   like(amouns) overlay(xa2ts8:2)
     D xa2ts9
     D  xa2ts9#                                   like(amouns) overlay(xa2ts9:2)
     D xa2t10
     D  xa2t10#                                   like(amouns) overlay(xa2t10:2)
     D xa2lnf
     D  xa2lnf#                                   like(amouns) overlay(xa2lnf:2)
    D*
     D                 ds
     D xxnotvds                    8910
     D   xxnotvar                    90    dim(99) overlay(xxnotvds:1)
     D   xxnotxar                   140    dim(62) overlay(xxnotvds:1)
     D   xxnotvc                      1    dim(8910) overlay(xxnotvds:1)
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
     D $#amt           s              9  2
     D $#case          s              7  0
     D $#co#           s              3  0
     D $#fein          s              9  0
     D $#fnd           s              3  0
     D $#from          s              7  0
     D $#fyr           s              3  0
     D $#recd          s              2
     D $#retp          s              1
     D $#rtcd          s              2
     D $#to            s              7  0
     D ##co#           s              3  0
     D ##conm          s             40
     D ##dcid          s             12
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fyr           s              3  0
     D ##lin#          s              9  0
     D ##rrn           s              9  0
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
     D c               s              3  0
     D code            s             13
     D d               s              1  0
     D duplicatbill    s              1
     D hdseq#          s                   like(deseq#)
     D kkacdt          s                   like(cmacdt)
     D kkcntl          s                   like(x5cntl)
     D kkcase          s                   like(cmcase)
     D kkcode          s                   like(brcode)
     D kkds1           s                   like(dids1)
     D kkdseq          s                   like(cadseq)
     D kkeob#          s                   like(eoeob#)
     D kkeobs          s                   like(rpeobs)
     D kkfein          s                   like(pdfein)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkkey           s                   like(pckey)
     D kklin#          s                   like(calin#)
     D kkmcod          s                   like(x7mcod)
     D kxmcod          s                   like(eemcod)
     D kkmod           s                   like(pdmod)
     D kknot1          s                   like(denot1)
     D kknwcd          s                   like(nwnwcd)
     D kkpos           s                   like(pxpos)
     D kkptyp          s                   like(ciptyp)
     D kkrcn           s                   like(eorcn)
     D kkrecd          s                   like(x7recd)
     D kkseq#          s                   like(deseq#)
     D kksinv          s                   like(easinv)
     D kkss#           s                   like(cmss#)
     D kkst            s                   like(stkey)
     D kksub#          s                   like(cmsub#)
     D kksys           s                   like(cisys)
     D kkwcf#          s                   like(cmfnd)
     D kkzipa          s                   like(gzzipa)
     D kk6ln#          s                   like(x6lin#)
     D k1sinv          s                   like(eosinv)
     D l0              s              5  0
     D l2              s              5  0
     D lc              s              3  0
     D ll              s              3  0
     D m               s              3  0
     D mmtype          s              1
     D n               s              3  0
     D odchrg          s                   like(cachrg)
     D odmra           s                   like(camra)
     D odndis          s                   like(candis)
     D odothr          s                   like(caothr)
     D odpaid          s                   like(capaid)
     D ohchrg          s                   like(eochrg)
     D ohinpe          s                   like(eoinpe)
     D ohmra           s                   like(eomra)
     D ohnamt          s                   like(eonamt)
     D ohndis          s                   like(eondis)
     D ohothr          s                   like(eoothr)
     D ohpaid          s                   like(eopaid)
     D originalbill    s              1
     D recon           s              3  0
     D reconerrorfl    s              1
     D reconf          s              1
     D recon#iseven    s              1
     D recrm           s              3  0
     D sign            s              1
     D size            s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D ul              s              3  0
     D v               s              5  0
     D x               s              3  0
     D xxamt           s                   like(eopaid)
     D xxast           s                   like(eoast)
     D xxdate          s              8  0
     D xxdateiso       s               d   datfmt(*ISO)
     D xxdseq          s                   like(cadseq)
     D xxedi           s                   like(st1a4)
     D xxfrdt          s                   like(eofrdt)
     D*xxnote          s            140
     D*xxnot3          s             70
     D*xxnot4          s             70
     D*xxnotv          s           8910    varying
     D xxntwk          s              5
     D xxone           s              1  0
     D xxonea          s              1
     D xxrcne          s              1
     D xxrlin          s                   like(x7rlin)
     D xxseq#          s                   like(rpseq#)
     D xxtodt          s                   like(eotodt)
     D xxtwo           s              2  0
     D xxtwoa          s              2
     D xxtype          s              1
     D xxzip           s              5
     D y               s              3  0
     D yynotvds        s           8910
     D z               s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D/COPY SCOPYWW01I
     Ipcm02
     I              cmoscl                      cwoscl
    I*
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
     C     c1ky          klist
     C                   kfld                    ##co#
    C*
     C     caky          klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
     C                   kfld                    kkdseq
    C*
     C     caky2         klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     caky3         klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
     C                   kfld                    x4eob#
     C                   kfld                    kklin#
    C*
     C     ciky          klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    kkptyp
     C                   kfld                    kksys
    C*
     C     capky         klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
     C     daky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    cadseq
     C                   kfld                    eorcn
    C*
     C     cdky          klist
     C                   kfld                    kkss#
    C*
     C     cmky1         klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     cmky2         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
     C     cmky3         klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     deky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    eorcn
     C                   kfld                    cadseq
    C*
     C     deky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
    C*
     C     deky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
    C*
     C     deky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kknot1
    C*
     C     deky4         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    hdseq#
    C*
     C     deky5         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kxmcod
    C*
     C     diky          klist
     C                   kfld                    kkds1
    C*
     C     eaky1         klist
     C                   kfld                    ##co#
     C                   kfld                    kksinv
    C*
     C     eaky2         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    eorcn
    C*
     C     eeky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    eorcn
    C*
     C     eeky1         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
    C*
     C     eeky2         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    kkseq#
    C*
     C     eeky3         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    hdseq#
    C*
     C     eeky4         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    kxmcod
    C*
     C     eobky         klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
     C                   kfld                    x4fyr
     C                   kfld                    x4case
     C                   kfld                    x4eob#
    C*
     C     eoky1         klist
     C                   kfld                    ##co#
     C                   kfld                    k1sinv
    C*
     C     esky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    eorcn
    C*
     C     f1ky          klist
     C                   kfld                    x4co#
     C                   kfld                    x4sub#
     C                   kfld                    x4fnd
    C*
     C     gzky          klist
     C                   kfld                    x4co#
     C                   kfld                    kkzipa
    C*
     C     nwky          klist
     C                   kfld                    ##co#
     C                   kfld                    kknwcd
    C*
     C     pdky1         klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     pdky2         klist
     C                   kfld                    xxfein
     C                   kfld                    xxmod
    C*
     C     rpky1         klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    x4fyr
     C                   kfld                    x4case
     C                   kfld                    kkeob#
     C                   kfld                    kkeobs
    C*
     C     rpky2         klist
     C                   kfld                    x4co#
     C                   kfld                    kkwcf#
     C                   kfld                    x4fyr
     C                   kfld                    x4case
    C*
     C     stky1         klist
     C                   kfld                    kkst
    C*
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey
    C*
     C     x1ky1         klist
     C                   kfld                    @@dcid
    C*
     C     x2ky1         klist
     C                   kfld                    kkcntl
    C*
     C     x3ky1         klist
     C                   kfld                    kkcntl
    C*
     C     x4ky1         klist
     C                   kfld                    @@dcid
    C*
     C     x4ky2         klist
     C                   kfld                    x4eob#
     C                   kfld                    x4rcn
    C*
     C     x5ky1         klist
     C                   kfld                    kkcntl
    C*
     C     x5ky2         klist
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*** BEG ADD *** 09/18/06 *********************************************
    C*
     C     x5ky3         klist
     C                   kfld                    kkcntl
     C*** END ADD *** 09/18/06 *********************************************
    C*
     C     x6ky1         klist
     C                   kfld                    kkcntl
     C                   kfld                    kk6ln#
    C*
     C     x7ky1         klist
     C                   kfld                    kkcntl
     C                   kfld                    kkmcod
    C*
     C     x8ky1         klist
     C                   kfld                    kkcntl
    C*
     C     xaky1         klist
     C                   kfld                    kkcntl
    C*
     C     xbky1         klist
     C                   kfld                    @@dcid
    C*
     C     brky1         klist
     C                   kfld                    kkcode
    C*
     C     pxky1         klist
     C                   kfld                    kkpos
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*****************************************************************
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   exsr      mainsr
    C*
    C*================================================================
    C* CHKNET - CHECK TO SEE IF THE NETWORK EXISTS
    C*================================================================
     C     chknet        begsr
    C*
    C* If network does not exist then create it
    C*
     C                   movel(p)  'FH'          kknwcd
     C     nwky          chain     pnw01                              99
     C                   if        *in99 = *on
     C                   clear                   pnw01
     C                   z-add     eoeob#        nwco#
     C                   movel(p)  'FH'          nwnwcd
     C                   movel(p)  'First Health'nwname
     C                   movel     'UC'          nwst
     C                   movel(p)  'A'           nwstat
     C                   movel(p)  'FIRSTHE'     nwuser
     C                   movel(p)  'FIRSTHE'     nwiusr
     C                   movel(p)  q1pgm         nwpgm
     C                   time                    dsdttm
     C                   eval      nwtime = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nwdt
     C                   movel     srdto         nwnidt
     C                   write     pnw01
     C                   endif
    C*
    C* If network is not set up in the group then set it up
    C*
     C     f1ky          chain     pf101                              99
     C                   if        *in99 = *off
     C                   eval      x = 1
     C                   movel(p)  'FH'          xxntwk
     C     xxntwk        lookup    nw(x)                                  99
     C                   if        *in99 = *off
     C                   eval      x = 1
     C     *blanks       lookup    nw(x)                                  99
     C                   if        *in99 = *on
     C                   movel(p)  'FH'          nw(x)
     C                   movel(p)  'Y'           nf(x)
     C                   movel(p)  'R'           nt(x)
     C                   eval      ni(x) = 203377189
     C                   eval      nm(x) = 1
     C                   endif
     C                   endif
     C                   update    pf101
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* CVTDIA - REMOVES THE DECIMAL FROM DIAGNOSIS CODES
    C*================================================================
     C     cvtdia        begsr
    C*
     C                   clear                   dio
     C                   eval      x = 1
     C                   eval      y = 1
     C                   eval      z = 1
     C     '.'           lookup    dii(x)                                 99
     C                   if        *in99 = *on
    C*
     C                   if        x = 3
     C                   eval      z = 2
     C                   endif
    C*
     C     1             do        6             y
     C                   if        dii(y) <> '.'
     C                   movea     dii(y)        dio(z)
     C                   eval      z = z + 1
     C                   endif
     C                   if        z > 6
     C                   leave
     C                   endif
     C                   enddo
    C*
     C                   else
     C                   eval      x = 1
     C     ' '           lookup    dii(x)                                 99
     C                   if        *in99 = *on
    C*
     C                   if        x = 3
     C                   eval      z = 2
     C                   endif
    C*
     C     1             do        6             y
     C                   movea     dii(y)        dio(z)
     C                   eval      z = z + 1
     C                   if        z > 6
     C                   leave
     C                   endif
     C                   enddo
    C*
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* ERRCK - ERROR CHECK SUBROUTINE
    C*================================================================
     C     errck         begsr
    C*
    C* Extract the recon number
    C*
     C                   exsr      getrca
    C*
    C* Now that we got the amend suffix let's see if this
    C* is an original bill
    C*
B00  C                   if        *in91 = *off
    C*
    C* Let's check to see if the bill is a duplicate
    C*
     C                   eval      duplicatbill = *off
B0B  C                   if        x4cntl <> *blanks
     C                   movel(p)  x4cntl        k1sinv
     C     eoky1         chain     peo02                              99
B0C  C                   if        *in99 = *off
     C                   eval      *in90 = *on
     C                   eval      *in91 = *on
     C                   eval      *in93 = *on
     C                   eval      duplicatbill = *on
E0C  C                   endif
E0B  C                   endif
    C*
B0A  C                   if        *in93  = *off
B01  C                   if        recon  = *zeros
    C*
    C* Check to see if original bill has already been uploaded
    C*
     C                   eval      kksinv = x4cntl
     C                   eval      %subst(kksinv:13:2) = '00'
     C     eaky1         chain     pea02                              99
B02  C                   if        *in99  = *on
    C*
    C* Check WCMC Claim Master to see if claim number exists
    C*
     C                   movel     'N'           reconf
     C                   exsr      getclm
     C     cmky2         chain     pcm01                              99
B03  C                   if        *in99  = *on
     C                   eval      *in90  = *on
E03  C                   endif
    C*
    C* FEIN/MOD does not exist.
    C*
B04  C                   if        *in90  = *off
    C*
     C                   eval      kkfein = xxfein
     C                   eval      kkmod  = xxmod
     C                   if        *in16  = *on
     C                   eval      kkfein = x4fein
     C                   eval      kkmod  = x4mod
     C                   endif
    C*
    C*
     C     pdky1         chain     ppd01                              99
B06  C                   if        *in99  = *on  or
     C                             *in99  = *off and
     C                             pdstat = 'I'
     C                   eval      *in90  = *on
E06  C                   endif
E04  C                   endif
    C*
     C                   if        *in90  = *off
    C*
    C* Check for Duplicate Payments
    C*
     C                   eval      xxamt  = xapaid#
    C*
     C                   if        xxamt <> *zeros
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     xxfrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     xxtodt
     C                   endif
    C*
    C* Check for duplicate request/payment
    C*
     C                   call      'WA052R'
     C                   parm      ##co#         $#co#
     C                   parm      kkwcf#        $#fnd
     C                   parm      kkfyr         $#fyr
     C                   parm      kkcase        $#case
     C                   parm      xxfrdt        $#from
     C                   parm      xxtodt        $#to
     C                   parm      xxamt         $#amt
     C                   parm      '11'          $#recd
     C                   parm      pdfein        $#fein
     C                   parm      'M'           $#retp
     C                   parm      *blanks       $#rtcd
    C*
     C                   if        $#rtcd <> *blanks
     C                   eval      *in90  = *on
     C                   eval      *in94  = *on
     C                   endif
    C*
     C                   endif
     C                   endif
    C*
S02  C                   else
     C                   eval      *in90  = *on
     C                   eval      *in91  = *on
     C                   eval      *in93  = *on
E02  C                   endif
    C*
S01  C                   else
    C*
    C* Checking if this recon can be uploaded and getting the
    C* next available recon.
     C                   movel     'Y'           reconf
     C                   exsr      getclm
    C*
E01  C                   endif
E0A  C                   endif
S00  C                   else
     C                   movel     '1'           xxrcne
E00  C                   endif
    C*
     C                   endsr
    C*================================================================
    C* GETAST - GET THE ADJUDICATION STATE
    C*================================================================
     C     getast        begsr
    C*
     C                   eval      xxedi = 'N'
    C*
     C                   if        x4ast = 'FL'
     C                   eval      xxedi = 'E'
     C                   movel(p)  x4ast         xxast
    C*
     C                   else
    C*
     C                   movel(p)  'UC'          xxast
     C                   movel(p)  x4ast         kkst
     C     stky1         chain     pst01                              99
     C                   if        *in99 = *off
     C                   eval      xxedi = st1a4
     C                   movel(p)  x4ast         xxast
     C                   endif
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* GETCLM - GET THE CLAIM NUMBER
    C*================================================================
     C     getclm        begsr
    C*
     C                   movel     x4clm#        claim#
    C*
     C                   if        reconf = 'Y'
     C                   move      xxwcf#        kkwcf#
     C                   else
     C                   move      xxwcf#        kkwcf#
     C                   move      xxfyr         kkfyr
     C                   move      xxcase        kkcase
    C*
     C                   move      ##co#         #1co#
     C                   move      kkwcf#        #1wcf#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1wcf#
     C                   parm      *blanks       #1sub#
     C                   parm      *blanks       #1fnd
    C*
     C                   if        ##rtcd = *blanks
     C                   move      #1sub#        kksub#
     C                   move      #1fnd         kkfnd
     C                   else
     C                   eval      kksub# = *zeros
     C                   eval      kkfnd = *zeros
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* GETRCA - GET THE RECON NUMBER PART 1
    C*================================================================
     C     getrca        begsr
    C*
     C                   eval      *in90  = *off
     C                   eval      *in91  = *off
     C                   eval      *in92  = *off
     C                   eval      *in93  = *off
     C                   eval      *in94  = *off
     C                   eval      kkrcn  = *zeros
     C                   eval      recon  = *zeros
     C                   eval      recrm  = *zeros
     C                   eval      reconerrorfl = *off
     C                   eval      originalbill = *off
     C                   eval      recon#iseven = *off
     C                   clear                   xxrcne
    C*
     C                   eval      xxtwoa = %subst(x4cntl:13:2)
     C                   move      xxtwoa        recon
    C*
     C                   select
    C*
     C                   when      recon  = 0
     C                   eval      originalbill = *on
    C*
     C                   when      recon  = 1
     C                   eval      *in90  = *on
     C                   eval      *in91  = *on
     C                   eval      reconerrorfl = *on
    C*
     C                   when      recon  = 2
     C                   eval      recon  = 1
     C                   eval      recon#iseven = *on
    C*
     C                   when      recon  > 2
     C                   eval      recon  = recon / 2
     C                   eval      recrm  = %rem(recon:2)
    C*
     C                   if        recrm  = 0
     C                   eval      recon#iseven = *on
     C                   else
     C                   eval      *in90  = *on
     C                   eval      *in91  = *on
     C                   eval      reconerrorfl = *on
     C                   endif
    C*
     C                   endsl
    C*
     C                   if        recon   > 0       and
     C                             x4cntl <> *blanks
    C*
     C                   movel(p)  x4cntl        k1sinv
     C                   eval      %subst(k1sinv:13:2) = '00'
     C     eoky1         chain     peo02                              99
    C*
     C                   if        %found(faeol74)
    C*
     C                   if        recon  - eorcn = 1
     C                   eval      kkrcn  = recon
     C                   eval      *in92  = *on
     C                   else
     C                   eval      xxrcne = '2'
     C                   eval      *in90  = *on
     C                   eval      *in91  = *on
     C                   eval      reconerrorfl = *on
     C                   endif
    C*
     C                   else
     C                   eval      *in90  = *on
     C                   eval      *in91  = *on
     C                   eval      reconerrorfl = *on
     C                   endif
     C                   endif
    C*
     C                   endsr
     C*** BEG ADD *** 09/18/06 ********************************************
    C*
    C*================================================================
    C* LOADDOS - LOAD THE FROM AND TO DATES OF SERVICE
    C*================================================================
     C     loaddos       begsr
     C*
     C                   if        x4fdos = *blanks
     C*
     C                   eval      kkcntl = x4cntl
     C*
     C     x5ky3         chain     px503                              99
     C*
     C                   if        *in99 = *off
     C                   eval      x4fdos = d1fdat
     C                   endif
     C*
     C     x5ky3         setgt     px504
     C     x5ky3         readpe    px504                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      x4tdos = d2tdat
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C*** END ADD *** 09/18/06 ********************************************
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
    C*
     C                   eval      *in16 = *off
     C                   call      comapi        compl                  99
    C*
B01  C                   if        q1prms > 2
    C*
     C                   if        @@prvc = 'Y'
     C                   eval      *in16 = *on
     C                   endif
    C*
     C     @@rrn2        chain(n)  px401                              98
     C                   if        *in98 = *off
     C     cmky1         chain     pcm01                              98
     C                   endif
    C*
     C                   if        *in98 = *off
    C*
     C                   eval      kkcntl = x4cntl
     C     x2ky1         chain(n)  px201                              98
     C     x3ky1         chain(n)  px301                              98
     C     xaky1         chain(n)  pxa01                              98
     C                   if        %found(faxal1)
     C                   exsr      xasign
     C                   endif
    C*
     C                   if        *in98 = *off
    C*
     C                   if        *in16 = *off
     C     pdky2         chain     ppd01                              98
     C                   else
     C                   eval      kkfein = x4fein
     C                   eval      kkmod  = x4mod
     C     pdky1         chain     ppd01                              98
     C                   endif
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
     C                   eval      kkwcf# = *zeros
     C                   if        ##rtcd = *blanks
     C                   move      #1wcf#        kkwcf#
     C                   endif
    C*
     C                   exsr      getrca
    C*
     C                   exsr      writer
     C                   exsr      updat4
    C*
     C                   endif
     C                   endif
    C*
X01  C                   else
    C*
     C     x4ky1         chain     px402                              98
     C                   if        *in98 = *off
     C                   dow       *in98 = *off
    C*
     C                   eval      kkcntl = x4cntl
     C     x2ky1         chain(n)  px201                              98
     C     x3ky1         chain(n)  px301                              98
     C     xaky1         chain(n)  pxa01                              98
     C                   if        %found(faxal1)
     C                   exsr      xasign
     C                   endif
     C*** BEG ADD *** 10/03/06 ********************************************
     C*
     C* Calculate the from and to dates of service if not provided
     C* so that duplicate payments are searched correctly
     C*
     C                   exsr      loaddos
     C*** END ADD *** 10/03/06 ********************************************
    C*
     C                   exsr      errck
     C                   exsr      updt4a
     C                   if        *in90 = *off
     C                   exsr      writer
     C                   exsr      updt4b
     C                   endif
    C*
     C     x4ky1         reade     px402                                  98
     C                   if        *in98 = *on
     C                   exsr      updat9
     C                   endif
     C                   enddo
     C                   else
     C                   exsr      updat9
     C                   endif
     C                   endif
    C*
     C                   exsr      pgmend
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
     C                   if        %subst(xapbfe:1:1)= '-'
     C                   eval      xapbfe#= 0 - xapbfe#
     C                   endif

     C                   if        %subst(xa2ts1:1:1)= '-'
     C                   eval      xa2ts1#= 0 - xa2ts1#
     C                   endif
     C                   if        %subst(xa2ts2:1:1)= '-'
     C                   eval      xa2ts2#= 0 - xa2ts2#
     C                   endif
     C                   if        %subst(xa2ts3:1:1)= '-'
     C                   eval      xa2ts3#= 0 - xa2ts3#
     C                   endif
     C                   if        %subst(xa2ts4:1:1)= '-'
     C                   eval      xa2ts4#= 0 - xa2ts4#
     C                   endif
     C                   if        %subst(xa2ts5:1:1)= '-'
     C                   eval      xa2ts5#= 0 - xa2ts5#
     C                   endif
     C                   if        %subst(xa2ts6:1:1)= '-'
     C                   eval      xa2ts6#= 0 - xa2ts6#
     C                   endif
     C                   if        %subst(xa2ts7:1:1)= '-'
     C                   eval      xa2ts7#= 0 - xa2ts7#
     C                   endif
     C                   if        %subst(xa2ts8:1:1)= '-'
     C                   eval      xa2ts8#= 0 - xa2ts8#
     C                   endif
     C                   if        %subst(xa2ts9:1:1)= '-'
     C                   eval      xa2ts9#= 0 - xa2ts9#
     C                   endif
     C                   if        %subst(xa2t10:1:1)= '-'
     C                   eval      xa2t10#= 0 - xa2t10#
     C                   endif

     C                   if        %subst(xa2lnf:1:1)= '-'
     C                   eval      xa2lnf#= 0 - xa2lnf#
     C                   endif
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
    C* UPDT4A - Update First Health Bill Header Record (FAX4P) - 1ST UPDATE
    C*================================================================
     C     updt4a        begsr
    C*
     C                   if        *in90  = *on
     C                   if        *in91  = *off
     C                   if        *in94  = *off
     C                   movel(p)  'E'           x4prfl
     C                   else
     C                   movel(p)  'P'           x4prfl
     C                   endif
     C                   else
     C                   if        *in93  = *off
     C                   movel(p)  'R'           x4prfl
     C                   movel     xxrcne        x4rcne
     C                   else
     C                   movel(p)  'I'           x4prfl
     C                   endif
     C                   endif
     C                   else
    C*
     C                   if        *in92  = *off
     C                   if        x4eob# = *zeros
     C     c1ky          chain     pc101                              99
     C                   if        *in99  = *off
     C                   eval      c1eob# = c1eob# + 1
     C                   update    pc101
     C                   endif
     C                   eval      x4eob# = c1eob#
     C                   endif
    C*
     C                   eval      x4co#  = cmco#
     C                   eval      x4sub# = cmsub#
     C                   eval      x4fnd  = cmfnd
     C                   eval      x4fyr  = cmfyr
     C                   eval      x4case = cmcase
     C                   eval      x4fein = pdfein
     C                   eval      x4mod  = pdmod
     C                   movel(p)  cmss#         x4ss#
     C                   eval      x4acdt = cmacdt
     C                   eval      x4rcn  = *zeros
     C                   else
     C                   if        x4eob# = *zeros
     C                   eval      x4co#  = eoco#
     C                   eval      x4sub# = eosub#
     C                   eval      x4fnd  = eofnd
     C                   eval      x4fyr  = eofyr
     C                   eval      x4case = eocase
     C                   eval      x4eob# = eoeob#
     C                   eval      x4rcn  = kkrcn
     C                   eval      x4fein = eofein
     C                   eval      x4mod  = eomod
     C                   movel(p)  eoss#         x4ss#
     C                   eval      x4acdt = eoacdt
     C                   endif
     C                   endif
    C*
     C                   endif
    C*
     C                   if        x4acdt = 0 and x2acdt <> *blanks
     C                   movel     x2acdt        xxdateiso
     C     *cymd         move      xxdateiso     x4acdt
     C                   endif
     C*** BEG ADD *** 09/18/06 ********************************************
     C*
     C* Calculate the from and to dates of service if not provided
     C*
     C                   exsr      loaddos
     C*** END ADD *** 09/18/06 ********************************************
    C*
     C                   update    px402
    C*
     C                   endsr
    C*================================================================
    C* UPDT4B - Update First Health Bill Header Record (FAX4P) - 2ND UPDATE
    C*================================================================
     C     updt4b        begsr
    C*
     C     x4ky2         chain     px403                              99
     C                   if        *in99 = *off
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x4prdt
    C*
     C                   clear                   x4dlus
     C                   clear                   x4dlpg
     C                   eval      x4dltm = *zeros
     C                   eval      x4dldt = *zeros
     C                   clear                   x4dlre
     C                   movel(p)  'Y'           x4prfl
     C*** BEG ADD *** 10/03/06 ********************************************
     C*
     C* Calculate the from and to dates of service if not provided
     C*
     C                   exsr      loaddos
     C*** END ADD *** 10/03/06 ********************************************
    C*
     C                   update    px403
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* UPDAT4 - Update First Health Bill Header Record (FAX4P) - SINGLE MODE
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
     C*** BEG ADD *** 10/03/06 ********************************************
     C*
     C* Calculate the from and to dates of service if not provided
     C*
     C                   exsr      loaddos
     C*** END ADD *** 10/03/06 ********************************************
    C*
     C                   update    px401
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* UPDT5A - Update First Health Bill Detail Record (FAX5P) - 1ST UPDATE
    C*================================================================
     C     updt5a        begsr
    C*
     C                   eval      x5eob# = caeob#
     C                   eval      x5dseq = cadseq
    C*
     C                   update    px501
    C*
     C                   endsr
    C*================================================================
    C* UPDT5B - Update First Health Bill Detail Record (FAX5P) - 2ND UPDATE
    C*================================================================
     C     updt5b        begsr
    C*
     C     x5ky2         chain     px502                              99
     C                   if        *in99 = *off
     C                   movel(p)  'Y'           x5prfl
     C                   update    px502
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* UPDAT9 - Update First Health Bill Transmission File (FAX9P).
    C*================================================================
     C     updat9        begsr
    C*
     C     x4ky1         chain(n)  px402                              99
     C                   if        *in99 = *on
     C     @@rrn         chain     px901                              99
     C                   if        *in99 = *off
    C*
     C                   movel(p)  q1user        x9user
     C                   movel(p)  q1pgm         x9pgm
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x9prdt
     C                   movel     srdto         x9dt
     C                   eval      x9prtm = dstime
    C*
     C                   update    px901
     C                   endif
    C*
    C* Copy document to First Health folder (FIRSTHEA)
    C*
     C                   move      ##co#         $$co#
     C                   call      'F8282C'
     C                   parm                    $$co#
     C                   parm      @@dcid        ##dcid
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* UPDCAP - Update FDCAP record
    C*================================================================
     C     updcap        begsr
    C*
     C                   move      x5lin#        kklin#
    C*
     C     caky3         chain     pca03                              99
     C                   if        *in99 = *off
    C*
     C                   select
    C*
     C                   when      eobtyp = 'N'
     C                   movel(p)  x5prcd        cacode
     C                   movel(p)  x5prcd        cacpt4
     C                   movel(p)  x5prcd        cabprc
    C*
     C                   when      eobtyp = 'H'
     C                   reset                   kkcmsv
     C                   movel     x5rvcd        kkrvcd
     C                   movel     x5prcd        kkhcpc
     C                   movel     kkcmsv        cacode
     C                   move      kkrvcd        casvcd
     C                   movel     x5prcd        cacpt4
    C*
     C                   when      eobtyp = 'X'
     C                   movel(p)  x5ndc         cacode
    C*
     C                   endsl
    C*
     C                   movel(p)  eouser        causer
     C                   movel(p)  eopgm         capgm
     C                   eval      cadt    = eodt
     C                   eval      catime  = eotime
    C*
     C                   eval      odchrg  = cachrg
     C                   eval      odmra   = camra
     C                   eval      odndis  = candis
     C                   eval      odpaid  = capaid
    C*
     C                   eval      cachrg  = x5chrg#
     C                   eval      capaid  = x5paid#
    C*
     C                   if        x5paid# = x5chrg#
     C                   eval      camra   = 0
     C                   eval      candis  = 0
     C                   else
     C                   if        x5paid# = x5fs#
     C                   eval      camra   = cachrg - capaid
     C                   eval      camraa  = x5fs#
     C                   eval      candis  = 0
     C                   else
     C                   if        x5paid# = x5ppo# and x5ppo# < x5fs#
     C                   eval      camra   = cachrg - x5fs#
     C                   eval      candis  = x5fs# - x5ppo#
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   exsr      wrtdap
     C                   exsr      wrtdep
    C*
     C                   update    pca03
    C*
     C                   else
     C                   exsr      wrtcap
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
    C* Get the adjudication state
    C*
     C                   exsr      getast
    C*
    C* Write the header records
    C*
     C                   exsr      wrteop
     C                   exsr      wrteap
     C                   exsr      wrteep
     C                   exsr      wrtesp
     C                   exsr      wrtdip
    C*
     C                   select
     C                   when      eobtyp = 'H'
     C                   exsr      wrteup
     C                   when      eobtyp = 'N'
     C                   exsr      wrtehp
     C                   when      eobtyp = 'X'
     C                   exsr      wrtepp
     C                   endsl
    C*
    C* Write the detail records
    C*
     C                   exsr      wrtdtl
    C*
    C* Calculate From and To Service Dates
    C*
     C                   exsr      frtodt
    C*
    C* Write the check request
    C*
     C                   if        eaplnk = '2' or
     C                             eaplnk = '3'
    C*
     C                   if        eapaid > 0
    C*
     C                   exsr      wrtrp1
    C*
    C* BEG DEL ***** 11/28/06 *********************************************
    C*
     C*****              exsr      wrtrp2
    C*
    C* END DEL ***** 11/28/06 *********************************************
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* FRTODT - Populate From and To dates
    C*================================================================
     C     frtodt        begsr
    C*
     C     eobky         chain     peo01                              99
     C                   if        *in99   = *off
    C*
    C*  Calculates TO and FROM service dates
    C*
     C     eobky         chain(n)  pca03                              99
     C                   dow       *in99  = *off
    C*
     C                   if        eofrdt = 0 and
     C                             eotodt = 0
     C                   eval      eofrdt = cafdat
     C                   eval      eotodt = cafdat
     C                   endif
    C*
     C                   if        cafdat < eofrdt
     C                   eval      eofrdt = cafdat
     C                   endif
    C*
     C                   if        cafdat > eotodt
     C                   eval      eotodt = cafdat
     C                   endif
    C*
     C                   if        catdat > eotodt
     C                   eval      eotodt = catdat
     C                   endif
    C*
     C     eobky         reade(n)  pca03                                  99
    C*
     C                   enddo
    C*
     C                   update    peo01
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTEAP - Write FAEAP record
    C*================================================================
     C     wrteap        begsr
    C*
     C     eaky2         chain     pea01                              99
     C                   if        *in99 = *on
    C*
     C                   clear                   pea01
     C                   eval      eaco#  = eoco#
     C                   eval      easub# = eosub#
     C                   eval      eafnd  = eofnd
     C                   eval      eafyr  = eofyr
     C                   eval      eacase = eocase
     C                   movel     eoss#         eass#
     C                   eval      eaacdt = eoacdt
     C                   eval      eaeob# = eoeob#
     C                   eval      earcn  = eorcn
     C                   eval      eatrdt = eotrdt
    C*
     C                   eval      eachrg = eochrg - ohchrg
     C                   eval      eamra  = eomra  - ohmra
     C                   eval      eaothr = eoothr - ohothr
     C                   eval      eandis = eondis - ohndis
     C                   eval      eapaid = eopaid - ohpaid
     C                   eval      eanamt = eonamt - ohnamt
    C*
     C                   eval      eancam = eanamt
     C                   eval      eaedi  = xxedi
    C*
     C                   movel(p)  'A'           eastat
     C                   movel(p)  eouser        eauser
     C                   movel(p)  eopgm         eapgm
     C                   eval      eatime = eotime
     C                   eval      eadt   = eodt
     C                   movel(p)  eouser        eaiusr
     C                   movel(p)  eopgm         eaipgm
     C                   eval      eaitim = eoitim
     C                   eval      eaidt  = eoidt
    C*
     C                   movel(p)  'H'           eabhrf
     C                   movel(p)  'H'           eanhrf
     C                   movel(p)  eosys         easys
    C*
     C     f1ky          chain(n)  pf101                              99
     C                   if        *in99  = *off
     C                   if        f1outa = 'Y'
     C                   move      '1'           eaplnk
     C                   else
     C                   if        eopaid<= 0
     C                   move      '2'           eaplnk
     C                   else
     C                   move      '3'           eaplnk
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   eval      eafein = eofein
     C                   eval      eamod  = eomod
     C                   movel(p)  x4cntl        easinv
     C                   movel(p)  x4cntl        eacntl
    C*
     C                   if        pddba1 <> 'Y'
     C                   if        pdfsn = *blanks
     C                   move      pdname        eapnam
     C                   else
     C     pdfsn         cat(p)    pdname:1      eapnam
     C                   endif
     C                   move      pdadr1        eaadr1
     C                   move      pdadr2        eaadr2
     C                   else
     C                   move      pdadr1        eapnam
     C                   move      pdadr2        eaadr1
     C                   clear                   eaadr2
     C                   endif
     C                   move      pdcty         eacty
     C                   move      pdst          east
     C                   move      pdzip         eazip
    C*
     C                   eval      eainpe = eoinpe - ohinpe
     C                   eval      eaoca1 = eainpe
    C*
     C                   eval      eapnwc = 'Y'
    C*
     C                   write     pea01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTEEP - Write FAEEP record
    C*================================================================
     C     wrteep        begsr
    C*
     C                   eval      kk6ln# = '****'
    C*
     C     x6ky1         setll     px601
    C*
B01  C                   if        %found(fax6l1)
    C*
     C     eeky1         setll     pee01
     C     eeky1         reade     pee01
B02  C                   dow       not %eof(faeel)
B03  C                   if        eeipgm = 'F8282R'
     C                   delete    pee01
E03  C                   endif
     C     eeky1         reade     pee01
E02  C                   enddo
    C*
     C     x6ky1         reade     px601
B02  C                   dow       not %eof(fax6l1)
    C*
    C*                  each fax6l1 record comes with 8 reason codes
     C                   eval      m = 1
B03  C                   do        8
    C*
B04  C                   if        mcd(m) <> *blanks
     C                   eval      kkmcod  = mcd(m)
    C*
     C                   eval      kxmcod  = kkmcod
     C     eeky4         chain     pee02                              99
B044 C                   if        *in99   = *on
    C*
    C*                  get reason codes description
    C*
     C                   clear                   xxrlin
     C                   clear                   xxnotvds
     C                   eval      v = 1
     C     x7ky1         setll     px701
     C     x7ky1         reade     px701
B05  C                   dow       not %eof(fax7l1)
    C*
B06  C                   if        x7reas <> *blanks and x7rlin > xxrlin
     C                   eval      xxrlin = x7rlin
B07  C                   if        v = 1
     C                   eval      xxnotvds = x7reas
X07  C                   else
     C                   eval      xxnotvds = %trim(xxnotvds) + ' ' +
     C                                        %trim(x7reas)
E07  C                   endif
     C                   eval      v = v + 1
E06  C                   endif
    C*
     C     x7ky1         reade     px701
E05  C                   enddo
    C*
     C                   eval      l0  = 0
     C                   dow       l0 <= %size(xxnotvds)
     C                   eval      l0  = l0 + 70
     C                   if        %subst(xxnotvds:l0:1) <> *blanks
     C                             and %subst(xxnotvds:l0+1:70) <> *blanks
     C                   eval      l2 = l0
     C                   dou       xxnotvc(l2) = ' '
     C                   eval      l2 = l2 - 1
     C                   enddo
     C                   eval      yynotvds = xxnotvds
     C                   eval      xxnotvds = %subst(yynotvds:1:l2)
     C                   eval      %subst(xxnotvds:l0) = %subst(yynotvds:l2)
     C                   else
     C                   if        %subst(xxnotvds:l0+1:70) = *blanks
     C                   leave
     C                   endif
     C                   endif
     C                   enddo
    C*
     C                   eval      v = 1
B05  C                   dow       xxnotxar(v) <> *blanks
    C*
     C                   eval      kkseq#  = *hival
     C     eeky2         setgt     pee01
     C     eeky1         readpe(n) pee01                                  99
B06  C                   if        *in99   = *off
     C                   eval      hdseq#  = eeseq# + 1
X06  C                   else
     C                   eval      hdseq#  = 1
E06  C                   endif
    C*
     C     eeky3         chain     pee01                              99
B06  C                   if        *in99   = *on
    C*
     C                   clear                   pee01
     C                   eval      eeco#   = eoco#
     C                   eval      eesub#  = eosub#
     C                   eval      eefnd   = eofnd
     C                   eval      eefyr   = eofyr
     C                   eval      eecase  = eocase
     C                   eval      eeeob#  = eoeob#
     C                   eval      eercn   = eorcn
     C                   movel     eoss#         eess#
     C                   eval      eeacdt  = eoacdt
     C                   eval      eeseq#  = hdseq#
     C                   movel(p)  xxnotxar(v)   note
     C                   movel(p)  xxnot1        eenot1
     C                   movel(p)  xxnot2        eenot2
     C                   movel(p)  'Y'           eeprnt
    C*
     C                   movel     'A'           eestat
     C                   movel(p)  eouser        eeuser
     C                   movel(p)  eopgm         eepgm
     C                   eval      eetime  = eotime
     C                   eval      eedt    = eodt
     C                   movel(p)  eouser        eeiusr
     C                   movel(p)  eopgm         eeipgm
     C                   eval      eeitim  = eotime
     C                   eval      eeidt   = eodt
    C*
     C                   eval      eemcod  = kxmcod
    C*
     C                   write     pee01
     C                   eval      v = v + 1
    C*
E06  C                   endif
    C*
E05  C                   enddo
    C*
E044 C                   endif
E04  C                   endif
     C                   eval      m = m + 1
E03  C                   enddo
    C*
     C     x6ky1         reade     px601
E02  C                   enddo
    C*
E01  C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTEHP - Write FAEHP record
    C*================================================================
     C     wrtehp        begsr
    C*
     C     eobky         chain     peh01                              99
     C                   if        *in99 = *on
    C*
     C                   clear                   peh01
     C                   eval      ehco#  = eoco#
     C                   eval      ehsub# = eosub#
     C                   eval      ehfnd  = eofnd
     C                   eval      ehfyr  = eofyr
     C                   eval      ehcase = eocase
     C                   eval      eheob# = eoeob#
     C                   movel(p)  eoss#         ehss#
     C                   eval      ehacdt = eoacdt
    C*
     C                   movel(p)  eodia1        ehdia1
     C                   movel(p)  eodia2        ehdia2
     C                   movel(p)  eodia3        ehdia3
     C                   movel(p)  eodia4        ehdia4
     C                   movel     x4acct        ehacct
     C                   movel(p)  x3lic#        ehlic#
     C                   movel(p)  xxedi         ehedi
    C*
     C                   movel(p)  'Y'           ehempl
     C                   movel(p)  'A'           ehstat
    C*
     C                   movel(p)  eouser        ehuser
     C                   movel(p)  eopgm         ehpgm
     C                   eval      ehtime = eotime
     C                   eval      ehdt   = eodt
     C                   movel(p)  eouser        ehiusr
     C                   movel(p)  eopgm         ehipgm
     C                   eval      ehitim = eoitim
     C                   eval      ehidt  = eoidt
    C*
     C                   write     peh01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTEPP - Write FAEPP record
    C*================================================================
     C     wrtepp        begsr
    C*
     C     eobky         chain     pep01                              99
     C                   if        *in99  = *on
    C*
     C                   clear                   pep01
     C                   eval      epco#  = eoco#
     C                   eval      epsub# = eosub#
     C                   eval      epfnd  = eofnd
     C                   eval      epfyr  = eofyr
     C                   eval      epcase = eocase
     C                   movel(p)  eoss#         epss#
     C                   eval      epacdt = eoacdt
     C                   eval      epeob# = eoeob#
    C*
     C                   movel(p)  x3lic#        epphar
     C                   movel(p)  xxedi         epedi
     C                   movel     x4acct        epacct
    C*
     C                   movel(p)  'A'           epstat
    C*
     C                   movel(p)  eouser        epuser
     C                   movel(p)  eopgm         eppgm
     C                   eval      eptime = eotime
     C                   eval      epdt   = eodt
     C                   movel(p)  eouser        epiusr
     C                   movel(p)  eopgm         epipgm
     C                   eval      epitim = eoitim
     C                   eval      epidt  = eoidt
    C*
     C                   write     pep01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTEOP - Write FAEOP record
    C*================================================================
     C     wrteop        begsr
    C*
     C                   time                    dsdttm
    C*
    C* WRITE FIRST EOMB
    C*
     C     eobky         chain     peo01                              99
B01  C                   if        *in99   = *on
    C*
     C                   clear                   peo01
     C                   eval      eoco#   = x4co#
     C                   eval      eofnd   = x4fnd
     C                   eval      eofyr   = x4fyr
     C                   eval      eocase  = x4case
    C*
     C                   movel     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eotrdt
    C*
     C                   eval      eofein = xxfein
     C                   eval      eomod  = xxmod
     C                   if        *in16  = *on
     C                   eval      eofein = x4fein
     C                   eval      eomod  = x4mod
     C                   endif
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     eofrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     eotodt
     C                   endif
    C*
B02  C                   if        pddba1 <> 'Y'
B03  C                   if        pdfsn   = *blanks
     C                   move      pdname        eopnam
S03  C                   else
     C     pdfsn         cat(p)    pdname:1      eopnam
E03  C                   endif
     C                   move      pdadr1        eoadr1
     C                   move      pdadr2        eoadr2
S02  C                   else
     C                   move      pdadr1        eopnam
     C                   move      pdadr2        eoadr1
     C                   clear                   eoadr2
E02  C                   endif
     C                   move      pdcty         eocty
     C                   move      pdst          eost
     C                   move      pdzip         eozip
    C*
     C                   eval      eochk#  = x4eob#
     C                   eval      eoeob#  = x4eob#
     C                   eval      eorcn   = x4rcn
    C*
     C                   select
    C*
     C                   when      %subst(x4cntl:1:2) >= '01' and
     C                             %subst(x4cntl:1:2) <= '05'
     C                   eval      eobtyp  = 'H'
     C                   eval      eosys   = 'H'
    C*
     C                   when      %subst(x4cntl:1:2) >= '06' and
     C                             %subst(x4cntl:1:2) <= '10'
     C                   eval      eobtyp  = 'N'
     C                   eval      eosys   = 'P'
    C*
     C                   when      %subst(x4cntl:1:2) >= '11' and
     C                             %subst(x4cntl:1:2) <= '13'
     C                   eval      eobtyp  = 'X'
     C                   eval      eosys   = 'P'
    C*
     C                   other
     C                   eval      eobtyp  = 'N'
     C                   eval      eosys   = 'P'
    C*
     C                   endsl
    C*
     C                   movel(p)  'FRSTHEALTH'  eouser
     C                   movel(p)  q1pgm         eopgm
     C                   eval      eotime  =     dstime
     C                   eval      srdti   =     dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eodt
     C                   movel(p)  'FRSTHEALTH'  eoiusr
     C                   movel     srdto         eoidt
    C*
B02  C                   if        x4dia1 <> *blanks
     C                   movea     x4dia1        dii
     C                   exsr      cvtdia
     C                   movea(p)  dio           eodia1
E02  C                   endif
    C*
B02  C                   if        x4dia2 <> *blanks
     C                   movea     x4dia2        dii
     C                   exsr      cvtdia
     C                   movea(p)  dio           eodia2
E02  C                   endif
    C*
B02  C                   if        x4dia3 <> *blanks
     C                   movea     x4dia3        dii
     C                   exsr      cvtdia
     C                   movea(p)  dio           eodia3
E02  C                   endif
    C*
B02  C                   if        x4dia4 <> *blanks
     C                   movea     x4dia4        dii
     C                   exsr      cvtdia
     C                   movea(p)  dio           eodia4
E02  C                   endif
    C*
     C                   movel(p)  xxedi         eotape
    C*
     C                   movel     eozip         xxzip
     C                   movel     xxzip         eoflic
     C                   eval      eo#bll = 1
     C                   movel     xxast         eoast
     C                   eval      eosub# = x4sub#
    C*
     C                   movel(p)  xxzip         kkzipa
     C     gzky          setgt     pgz01
     C                   readp     pgz01                                  99
B02  C                   if        *in99 = *off
     C                             and kkzipa <= gztozp
     C                   move      gzzipa        eozipa
S02  C                   else
     C                   clear                   eozipa
E02  C                   endif
    C*
     C                   movel(p)  x4ss#         eoss#
     C                   eval      eoacdt = x4acdt
    C*
     C                   movel(p)  q1pgm         eoipgm
     C                   eval      eoitim = dstime
    C*
     C                   movel(p)  'C'           eoblst
     C                   movel     '2'           eocscd
    C*
     C                   if        x4crdt <> *blanks
     C                   movel     x4crdt        xxdateiso
     C     *cymd         movel     xxdateiso     eocrdt
     C                   endif
    C*
     C                   if        x4prcd <> *blanks
     C                   movel     x4prcd        xxdateiso
     C     *iso          movel     xxdateiso     eobrdt
     C                   endif
    C*
B02  C                   if        eorcn    = *zeros
     C                   movel(p)  x4cntl        eosinv
E02  C                   endif
    C*
     C                   eval      ohchrg  = eochrg
     C                   eval      ohmra   = eomra
     C                   eval      ohothr  = eoothr
     C                   eval      ohndis  = eondis
     C                   eval      ohpaid  = eopaid
     C                   eval      ohnamt  = eonamt
    C*
     C                   eval      eochrg  = xachrg#
     C                   eval      eopaid  = xapaid#
     C                   eval      eonamt  = xa2ts1# + xa2ts2# + xa2ts3# +
     C                                       xa2ts4# + xa2ts5# + xa2ts6# +
     C                                       xa2ts7# + xa2ts8# + xa2ts9# +
     C                                       xa2t10#
    C*
B02  C                   if        xapaid# = xachrg#
     C                   eval      eomra   = 0
     C                   eval      eondis  = 0
S02  C                   else
B03  C                   if        xapaid# = xafs#
     C                   eval      eopaid  = xapaid#
     C                   eval      eomra   = eochrg - eopaid
     C                   eval      eondis  = 0
S03  C                   else
B04  C                   if        xapaid# = xappo# and xappo# < xafs#
     C                   eval      eomra   = eochrg - xafs#
     C                   eval      eondis  = xafs# - xappo#
E04  C                   endif
E03  C                   endif
E02  C                   endif
    C*
    C* Check to see if the network exists
    C*
     C                   movel(p)  'FH'          eontwk
     C                   exsr      chknet
    C*
     C                   movel(p)  x4ss#         kkss#
     C     cdky          chain     pcd01                              99
B02  C                   if        *in99 = *off
     C                   movel(p)  cdlsn         eolsn
     C                   movel(p)  cdfsn         eofsn
     C                   movel(p)  cdmid         eomid
S02  C                   else
     C                   clear                   eolsn
     C                   clear                   eofsn
     C                   clear                   eomid
E02  C                   endif
    C*
     C                   eval      ohinpe = eoinpe
     C                   eval      eoinpe = xasurc#
    C*
     C                   write     peo01
     C     eobky         chain(n)  peo01                              99
S01  C                   else
    C*
    C* First Health HAS RECONSIDERED THIS BILL
    C*
B02  C                   if        *in92  = *on
    C*
     C                   eval      ohchrg = eochrg
     C                   eval      ohmra  = eomra
     C                   eval      ohothr = eoothr
     C                   eval      ohndis = eondis
     C                   eval      ohpaid = eopaid
     C                   eval      ohnamt = eonamt
    C*
     C                   eval      eochrg = xachrg#
     C                   eval      eopaid = xapaid#
     C                   eval      eonamt = xa2ts1# + xa2ts2# + xa2ts3# +
     C                                      xa2ts4# + xa2ts5# + xa2ts6# +
     C                                      xa2ts7# + xa2ts8# + xa2ts9# +
     C                                      xa2t10#
    C*
B02  C                   if        xapaid# = xachrg#
     C                   eval      eomra   = 0
     C                   eval      eondis  = 0
S02  C                   else
B03  C                   if        xapaid# = xafs#
     C                   eval      eomra   = eochrg - eopaid
     C                   eval      eondis  = 0
S03  C                   else
B04  C                   if        xapaid#= xappo# and xappo#< xafs#
     C                   eval      eomra   = eochrg - xafs#
     C                   eval      eondis  = xafs# - xappo#
E04  C                   endif
E03  C                   endif
E02  C                   endif
    C*
     C                   eval      eorcn = kkrcn
E02  C                   endif
    C*
     C                   update    peo01
E01  C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTESP - Write FAESP record
    C*================================================================
     C     wrtesp        begsr
    C*
     C     esky          chain     pes01                              99
     C                   if        *in99   = *on
    C*
    C* Is this a reconsideration
    C*
     C                   if        *in92   = *on
    C*
    C* Write the recon record
    C*
     C                   clear                   pes01
     C                   eval      esco#   = eoco#
     C                   eval      essub#  = eosub#
     C                   eval      esfnd   = eofnd
     C                   eval      esfyr   = eofyr
     C                   eval      escase  = eocase
     C                   eval      eseob#  = eoeob#
     C                   eval      esrcn   = eorcn
     C                   movel     eoss#         esss#
     C                   eval      esacdt  = eoacdt
     C                   movel(p)  'R'           esblst
     C                   movel(p)  'RCN'         esstcd
    C*
     C                   movel(p)  reas2         esreas
     C                   movel(p)  x4ajty        kkcode
     C     brky1         chain     pbr01
     C                   if        %found(ftbrl1)
     C                   movel(p)  brdesc        esreas
     C                   endif
    C*
     C                   if        x4prcd <> *blanks
     C                   movel     x4prcd        xxdateiso
     C     *cymd         movel     xxdateiso     esrrdt
     C                   endif
    C*
     C                   eval      eotime = eotime + 1
     C                   movel     'A'           esstat
     C                   movel(p)  eouser        esuser
     C                   movel(p)  eopgm         espgm
     C                   eval      estime = eotime
     C                   eval      esdt   = eodt
     C                   movel(p)  eouser        esiusr
     C                   movel(p)  eopgm         esipgm
     C                   eval      esitim = eotime
     C                   eval      esidt  = eodt
     C                   eval      eotime = eotime + 1
     C                   write     pes01
     C                   endif
    C*
    C* Now complete the bill
    C*
     C                   clear                   pes01
     C                   eval      esco#  = eoco#
     C                   eval      essub# = eosub#
     C                   eval      esfnd  = eofnd
     C                   eval      esfyr  = eofyr
     C                   eval      escase = eocase
     C                   eval      eseob# = eoeob#
     C                   eval      esrcn  = eorcn
     C                   movel     eoss#         esss#
     C                   eval      esacdt = eoacdt
     C                   movel(p)  'C'           esblst
     C                   movel(p)  reas1         esreas
    C*
     C                   movel     'A'           esstat
     C                   movel(p)  eouser        esuser
     C                   movel(p)  eopgm         espgm
     C                   eval      estime = eotime
     C                   eval      esdt   = eodt
     C                   movel(p)  eouser        esiusr
     C                   movel(p)  eopgm         esipgm
     C                   eval      esidt  = eoidt
     C                   eval      esitim = eotime
    C*
     C                   write     pes01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDIP - Write FMDIP record
    C*================================================================
     C     wrtdip        begsr
    C*
     C     x8ky1         setll     px801
     C     x8ky1         reade     px801
     C                   dow       not %eof(fax8l1)
     C                   do        5             d
     C                   if        dia(d) <> *blanks
     C                   eval      kkds1  = dia(d)
     C     diky          chain     pdi01
     C                   if        not %found(fmdil2)
     C                   clear                   pdi01
     C                   movea     dia(d)        dii
     C                   exsr      cvtdia
     C                   movea(p)  dio           diicd9
     C                   eval      didsc1 = did(d)
     C                   eval      distat = 'A'
     C                   eval      diuser = q1user
     C                   eval      dipgm  = q1pgm
     C                   time                    dsdttm
     C                   eval      ditime = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         didt
     C                   eval      diiusr = q1user
     C                   movel     srdto         diidt
     C                   eval      dilost = 'D'
     C                   eval      dids1  = dia(d)
    C*
     C                   write     pdi01
    C*
     C                   endif
     C                   endif
     C                   enddo
     C     x8ky1         reade     px801
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C* WRTEUP - Write FAEUP record
    C*================================================================
     C     wrteup        begsr
    C*
     C     eobky         chain     peu01                              99
     C                   if        *in99   = *on
    C*
     C                   clear                   peu01
     C                   eval      euco#   = eoco#
     C                   eval      eusub#  = eosub#
     C                   eval      eufnd   = eofnd
     C                   eval      eufyr   = eofyr
     C                   eval      eucase  = eocase
     C                   movel     eoss#         euss#
     C                   eval      euacdt  = eoacdt
     C                   eval      eueob#  = eoeob#
     C                   movel(p)  x4acct        euacct
     C                   movel(p)  x4blty        eublty
    C*
     C                   if        x4addt <> *blanks
     C                   movel     x4addt        xxdateiso
     C     *cymd         move      xxdateiso     euaddt
     C                   endif
    C*
     C                   movel     x4drg         eudrg#
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     euscpf
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     euscpt
     C                   endif
    C*
     C                   movel     x4pats        eupats
    C*
     C                   movel(p)  eodia1        eudia1
     C                   movel(p)  eodia2        eudia2
     C                   movel(p)  eodia3        eudia3
     C                   movel(p)  eodia4        eudia4
    C*
     C                   move      '0'           eupcm
     C                   movel(p)  x4prc1        euprc1
     C                   movel(p)  x4prc2        euprc2
     C                   movel(p)  x4prc3        euprc3
    C*
     C                   if        x4prd1 <> *blanks
     C                   movel     x4prd1        xxdateiso
     C     *cymd         move      xxdateiso     euprd1
     C                   endif
    C*
     C                   if        x4dcdt <> *blanks
     C                   movel     x4dcdt        xxdateiso
     C     *cymd         move      xxdateiso     eudcdt
     C                   endif
    C*
     C                   movel     xxedi         euedi
     C                   movel     x3lic#        eulic#
    C*
     C                   movel     'A'           eustat
     C                   movel(p)  eouser        euuser
     C                   movel(p)  eopgm         eupgm
     C                   eval      eutime = eotime
     C                   eval      eudt   = eodt
     C                   movel(p)  eouser        euiusr
     C                   movel(p)  eopgm         euipgm
     C                   eval      euitim = eoitim
     C                   eval      euidt  = eoidt
    C*
     C                   write     peu01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTCAP - Write FDCAP record
    C*================================================================
     C     wrtcap        begsr
    C*
B01  C                   if        x5eob# = *zeros and
     C                             x5dseq = *zeros
    C*
     C                   eval      kkdseq = *hival
     C     caky          setgt     pca02
     C     caky2         readpe    pca02                                  99
B02  C                   if        *in99  = *off
     C                   eval      xxdseq = cadseq + 1
X02  C                   else
     C                   eval      xxdseq = 1
E02  C                   endif
    C*
     C                   clear                   pca01
     C                   eval      caco#  = eoco#
     C                   eval      cafnd  = eofnd
     C                   eval      cafyr  = eofyr
     C                   eval      cacase = eocase
     C                   movel     '2'           cacscd
    C*
B02  C                   if        x5fdat <> *blanks
     C                   movel     x5fdat        xxdateiso
     C     *cymd         move      xxdateiso     cafdat
E02  C                   endif
    C*
B02  C                   if        x5tdat <> *blanks
     C                   movel     x5tdat        xxdateiso
     C     *cymd         move      xxdateiso     catdat
E02  C                   endif
    C*
     C                   eval      cardat = eotrdt
     C                   eval      cafein = eofein
     C                   eval      camod  = eomod
    C*
B02  C                   select
W02  C                   when      eobtyp = 'N'
     C                   eval      caprty = '1'
W02  C                   when      eobtyp = 'H'
     C                   eval      caprty = '4'
E02  C                   endsl
    C*
     C                   eval      cadseq = xxdseq
     C                   eval      cachk# = eochk#
     C                   eval      caeob# = eoeob#
     C                   eval      cass#  = eoss#
     C                   eval      caacdt = eoacdt
     C                   eval      castat = 'A'
    C*
     C                   eval      carcfl = 'C'
B02  C                   select
W02  C                   when      eobtyp = 'X'
     C                   eval      carcfl = 'X'
W02  C                   when      eosys  = 'H'
     C                   eval      carcfl = 'S'
E02  C                   endsl
    C*
B02  C                   select
    C*
W02  C                   when      eobtyp = 'N'
     C                   movel(p)  x5prcd        cacode
     C                   movel(p)  x5prcd        cacpt4
     C                   movel(p)  x5prcd        cabprc
    C*
W02  C                   when      eobtyp = 'H'
     C                   reset                   kkcmsv
     C                   movel     x5rvcd        kkrvcd
     C                   movel     x5prcd        kkhcpc
     C                   movel     kkcmsv        cacode
     C                   move      kkrvcd        casvcd
     C                   movel     x5prcd        cacpt4
    C*
W02  C                   when      eobtyp = 'X'
     C                   movel(p)  x5ndc         cacode
    C*
E02  C                   endsl
    C*
     C                   movel(p)  eouser        causer
     C                   movel(p)  eopgm         capgm
     C                   eval      cadt   = eodt
     C                   eval      catime = eotime
     C                   movel(p)  eouser        caiusr
     C                   movel(p)  eopgm         caipgm
     C                   eval      caidt  = eoidt
     C                   eval      caitim = eoitim
    C*
     C                   eval      casub# = eosub#
     C                   move      eozipa        cazipa
     C                   movel(p)  eoast         caast
    C*
     C                   eval      odchrg = cachrg
     C                   eval      odmra  = camra
     C                   eval      odothr = caothr
     C                   eval      odndis = candis
     C                   eval      odpaid = capaid
    C*
     C                   eval      cachrg = x5chrg#
     C                   eval      capaid = x5paid#
    C*
B02  C                   if        x5paid#= x5chrg#
     C                   eval      camra  = 0
     C                   eval      candis = 0
X02  C                   else
B03  C                   if        x5paid#= x5fs#
     C                   eval      camra  = cachrg - capaid
     C                   eval      camraa = x5fs#
     C                   eval      candis = 0
X03  C                   else
B04  C                   if        x5paid#= x5ppo# and x5ppo#< x5fs#
     C                   eval      camra  = cachrg - x5fs#
     C                   eval      candis = x5fs# - x5ppo#
E04  C                   endif
E03  C                   endif
E02  C                   endif
    C*
     C                   move      x5lin#        calin#
     C                   eval      carcn = eorcn
    C*
     C                   exsr      wrtdap
     C                   exsr      wrtdep
    C*
     C                   write     pca01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDAP - Write FDDAP record
    C*================================================================
     C     wrtdap        begsr
    C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
    C*
     C     daky          chain     pda01                              99
     C                   if        *in99  = *on
    C*
     C                   clear                   pda01
     C                   eval      daco#  = caco#
     C                   eval      dasub# = casub#
     C                   eval      dafnd  = cafnd
     C                   eval      dafyr  = cafyr
     C                   eval      dacase = cacase
     C                   movel     cass#         dass#
     C                   eval      daacdt = caacdt
     C                   eval      daeob# = caeob#
     C                   eval      darcn# = eorcn
     C                   eval      dadseq = cadseq
     C                   eval      datrdt = cardat
    C*
     C                   eval      dachrg = cachrg - odchrg
     C                   eval      damra  = camra  - odmra
     C                   eval      daothr = caothr - odothr
     C                   eval      dandis = candis - odndis
     C                   eval      dapaid = capaid - odpaid
    C*
     C                   movel     'A'           dastat
     C                   movel(p)  causer        dauser
     C                   movel(p)  capgm         dapgm
     C                   eval      datime = catime
     C                   eval      dadt   = cadt
     C                   movel(p)  causer        daiusr
     C                   movel(p)  capgm         daipgm
     C                   eval      daitim = caitim
     C                   eval      daidt  = caidt
    C*
     C                   write     pda01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDEP - Write FDDEP record
    C*================================================================
     C     wrtdep        begsr
    C*
     C                   eval      kk6ln# = x5lin#
    C*
     C     x6ky1         setll     px601
    C*
     C                   if        %found(fax6l1)
    C*
     C     deky1         setll     pde01
     C     deky1         reade     pde01
     C                   dow       not %eof(fddel)
     C                   if        deipgm = 'F8282R'
     C                   delete    pde01
     C                   endif
     C     deky1         reade     pde01
     C                   enddo
    C*
     C     x6ky1         reade     px601
     C                   dow       not %eof(fax6l1)
    C*
    C*                  each fax6l1 record comes with 8 reason codes
     C                   eval      m = 1
     C                   do        8
    C*
     C                   if        mcd(m) <> *blanks
     C                   eval      kkmcod  = mcd(m)
    C*
     C                   eval      kxmcod  = kkmcod
     C     deky5         chain     pde02                              99
B044 C                   if        *in99   = *on
    C*
    C*                  get reason codes description
    C*
     C                   clear                   xxrlin
     C                   clear                   xxnotvds
     C                   eval      v = 1
     C     x7ky1         setll     px701
     C     x7ky1         reade     px701
     C                   dow       not %eof(fax7l1)
    C*
     C                   if        x7reas <> *blanks and x7rlin > xxrlin
     C                   eval      xxrlin = x7rlin
B07  C                   if        v = 1
     C                   eval      xxnotvds = x7reas
X07  C                   else
     C                   eval      xxnotvds = %trim(xxnotvds) + ' ' +
     C                                        %trim(x7reas)
E07  C                   endif
     C                   eval      v = v + 1
E06  C                   endif
    C*
     C     x7ky1         reade     px701
E05  C                   enddo
    C*
     C                   eval      l0  = 0
     C                   dow       l0 <= %size(xxnotvds)
     C                   eval      l0  = l0 + 70
     C                   if        %subst(xxnotvds:l0:1) <> *blanks
     C                             and %subst(xxnotvds:l0+1:70) <> *blanks
     C                   eval      l2 = l0
     C                   dou       xxnotvc(l2) = ' '
     C                   eval      l2 = l2 - 1
     C                   enddo
     C                   eval      yynotvds = xxnotvds
     C                   eval      xxnotvds = %subst(yynotvds:1:l2)
     C                   eval      %subst(xxnotvds:l0) = %subst(yynotvds:l2)
     C                   else
     C                   if        %subst(xxnotvds:l0+1:70) = *blanks
     C                   leave
     C                   endif
     C                   endif
     C                   enddo
    C*
     C                   eval      v = 1
B05  C                   dow       xxnotxar(v) <> *blanks
    C*
     C                   eval      kkseq# = *hival
     C     deky2         setgt     pde01
     C     deky1         readpe(n) pde01                                  99
     C                   if        *in99  = *off
     C                   eval      hdseq# = deseq# + 1
     C                   else
     C                   eval      hdseq# = 1
     C                   endif
    C*
     C     deky4         chain     pde01                              99
     C                   if        *in99  = *on
    C*
     C                   clear                   pde01
     C                   eval      deco#  = caco#
     C                   eval      desub# = casub#
     C                   eval      defnd  = cafnd
     C                   eval      defyr  = cafyr
     C                   eval      decase = cacase
     C                   movel     cass#         dess#
     C                   eval      deacdt = caacdt
     C                   eval      deeob# = caeob#
     C                   eval      dedseq = cadseq
     C                   eval      deseq# = hdseq#
     C                   eval      hdseq# = hdseq# + 1
     C                   movel(p)  xxnotxar(v)   note
     C                   movel(p)  xxnot1        denot1
     C                   move(p)   xxnot2        denot2
     C                   movel(p)  'Y'           deprnt
    C*
     C                   movel     'A'           destat
     C                   movel(p)  causer        deuser
     C                   movel(p)  capgm         depgm
     C                   eval      detime = catime
     C                   eval      dedt = cadt
     C                   movel(p)  causer        deiusr
     C                   movel(p)  capgm         deipgm
     C                   eval      deitim = caitim
     C                   eval      deidt = caidt
     C                   movel(p)  'U'           deesrc
     C                   eval      dercn = eorcn
     C                   movel(p)  'Y'           deprnt
    C*
     C                   eval      demcod  = kxmcod
    C*
     C                   write     pde01
     C                   eval      v = v + 1
    C*
     C                   endif
    C*
     C                   enddo
    C*
E044 C                   endif
     C                   endif
     C                   eval      m = m + 1
     C                   enddo
    C*
     C     x6ky1         reade     px601
     C                   enddo
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDHP - Write FDDHP record
    C*================================================================
     C     wrtdhp        begsr
    C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C     capky         chain     pdh01                              99
     C                   if        *in99  = *on
    C*
     C                   clear                   pdh01
     C                   eval      dhco#  = caco#
     C                   eval      dhsub# = casub#
     C                   eval      dhfnd  = cafnd
     C                   eval      dhfyr  = cafyr
     C                   eval      dhcase = cacase
     C                   movel     cass#         dhss#
     C                   eval      dhacdt = caacdt
     C                   eval      dheob# = caeob#
     C                   eval      dhdseq = cadseq
    C*
     C                   movel     x5prcd        dhprcd
     C                   movel(p)  x5desc        dhprds
     C                   movel     x5bprc        dhbprc
     C                   movel(p)  *blanks       dhbprd
     C                   if        x5bprc = *blanks
     C                   movel     x5prcd        dhbprc
     C                   movel(p)  x5desc        dhbprd
     C                   endif
    C*
     C                   if        x5pos <> *blanks
     C                   eval      kkpos  = x5pos
     C     pxky1         chain     ppx01
     C                   if        %found(ftpxl1)
     C                   movel     pxpxrf        dhpos
     C                   else
     C                   movel     ' 11'         dhpos
     C                   endif
     C                   endif
    C*
     C                   movel(p)  x5tos         dhtos
     C                   movel(p)  x5mod1        dhmod1
     C                   movel(p)  x5mod2        dhmod2
     C                   movel(p)  x5mod3        dhmod3
     C                   move      'C'           dhrcfl
    C*
     C                   select
     C                   when      x5unts <> *zeros  and
     C                             x5unts <> *blanks
     C                   move      x5unts        dhunts
     C                   other
     C                   eval      dhunts = 1
     C                   endsl
    C*
     C                   movel     x5xref        xxxref
    C*
     C                   if        dx1 = 1
     C                             or dx2 = 1
     C                             or dx3 = 1
     C                             or dx4 = 1
     C                   movel     'X'           dhdxr1
     C                   endif
    C*
     C                   if        dx1 = 2
     C                             or dx2 = 2
     C                             or dx3 = 2
     C                             or dx4 = 2
     C                   movel     'X'           dhdxr2
     C                   endif
    C*
     C                   if        dx1 = 3
     C                             or dx2 = 3
     C                             or dx3 = 3
     C                             or dx4 = 3
     C                   movel     'X'           dhdxr3
     C                   endif
    C*
     C                   if        dx1 = 4
     C                             or dx2 = 4
     C                             or dx3 = 4
     C                             or dx4 = 4
     C                   movel     'X'           dhdxr4
     C                   endif
    C*
     C                   select
     C                   when      dhdxr1 = *blanks
     C                             and dhdxr2 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and ehdia1 <> *blanks
     C                   movel     'X'           dhdxr1
     C                   when      dhdxr1 = *blanks
     C                             and dhdxr2 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and ehdia2 <> *blanks
     C                   movel     'X'           dhdxr2
     C                   when      dhdxr1 = *blanks
     C                             and dhdxr2 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and ehdia3 <> *blanks
     C                   movel     'X'           dhdxr3
     C                   when      dhdxr1 = *blanks
     C                             and dhdxr2 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and dhdxr3 = *blanks
     C                             and ehdia4 <> *blanks
     C                   movel     'X'           dhdxr4
     C                   endsl
    C*
     C                   movel     'A'           dhstat
     C                   movel(p)  causer        dhuser
     C                   movel(p)  capgm         dhpgm
     C                   eval      dhtime = catime
     C                   eval      dhdt = cadt
     C                   movel(p)  causer        dhiusr
     C                   movel(p)  capgm         dhipgm
     C                   eval      dhitim = caitim
     C                   eval      dhidt = caidt
    C*
     C                   write     pdh01
    C*
     C                   else
    C*
     C                   movel     x5prcd        dhprcd
     C                   movel(p)  x5desc        dhprds
     C                   movel     x5bprc        dhbprc
     C                   movel(p)  *blanks       dhbprd
     C                   if        x5bprc = *blanks
     C                   movel     x5prcd        dhbprc
     C                   movel(p)  x5desc        dhbprd
     C                   endif
    C*
     C                   movel(p)  causer        dhuser
     C                   movel(p)  capgm         dhpgm
     C                   eval      dhtime = catime
     C                   eval      dhdt = cadt
    C*
     C                   update    pdh01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDPP - Write FDDPP record
    C*================================================================
     C     wrtdpp        begsr
    C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C     capky         chain     pdp01                              99
     C                   if        *in99  = *on
    C*
     C                   clear                   pdp01
     C                   eval      dpco#  = caco#
     C                   eval      dpsub# = casub#
     C                   eval      dpfnd  = cafnd
     C                   eval      dpfyr  = cafyr
     C                   eval      dpcase = cacase
     C                   movel     cass#         dpss#
     C                   eval      dpacdt = caacdt
     C                   eval      dpeob# = caeob#
     C                   eval      dpdseq = cadseq
     C                   movel(p)  x5rx#         dprx#
    C*
     C                   movel(p)  x5desc        dpdesc
    C*
     C                   movel(p)  x5ndc         dpndc
     C                   movel(p)  x5ndc         dpbndc
    C*
     C                   select
     C                   when      x5unts <> *zeros  and
     C                             x5unts <> *blanks
     C                   move      x5unts        dpunts
     C                   other
     C                   eval      dpunts = 1
     C                   endsl
    C*
     C                   movel     'A'           dpstat
     C                   movel(p)  causer        dpuser
     C                   movel(p)  capgm         dppgm
     C                   eval      dptime = catime
     C                   eval      dpdt = cadt
     C                   movel(p)  causer        dpiusr
     C                   movel(p)  capgm         dpipgm
     C                   eval      dpitim = caitim
     C                   eval      dpidt = caidt
    C*
     C                   write     pdp01
    C*
     C                   else
    C*
     C                   movel(p)  x5desc        dpdesc
    C*
     C                   movel(p)  causer        dpuser
     C                   movel(p)  capgm         dppgm
     C                   eval      dptime = catime
     C                   eval      dpdt = cadt
    C*
     C                   update    pdp01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDTL - Write Detail Records
    C*================================================================
     C     wrtdtl        begsr
    C*
     C                   if        *in92 = *off
     C     x5ky1         chain     px501                              99
     C                   dow       *in99 = *off
    C*
    C*** BEG ADD *** 09/22/06 *****************************
     C                   if        x5chrg = '*************'
     C                   eval      x5chrg = '0000000000000'
     C                   endif
    C*
     C                   if        x5ppo = '*************'
     C                   eval      x5ppo = '0000000000000'
     C                   endif
    C*
     C                   if        x5paid = '*************'
     C                   eval      x5paid = '0000000000000'
     C                   endif
    C*
     C                   if        x5fs = '*************'
     C                   eval      x5fs = '0000000000000'
     C                   endif
    C*** END ADD *** 09/22/06 *****************************
    C*
     C                   exsr      x5sign
    C*
     C                   exsr      wrtcap
     C                   exsr      updt5a
    C*
     C                   select
     C                   when      eobtyp = 'H'
     C                   exsr      wrtdup
     C                   when      eobtyp = 'N'
     C                   exsr      wrtdhp
     C                   when      eobtyp = 'X'
     C                   exsr      wrtdpp
     C                   endsl
    C*
     C                   exsr      updt5b
    C*
     C     x5ky1         reade     px501                                  99
     C                   enddo
    C*
     C                   else
    C*
     C     x5ky1         chain     px501                              99
     C                   dow       *in99 = *off
    C*
    C*** BEG ADD *** 09/22/06 *****************************
     C                   if        x5chrg = '*************'
     C                   eval      x5chrg = '0000000000000'
     C                   endif
    C*
     C                   if        x5ppo = '*************'
     C                   eval      x5ppo = '0000000000000'
     C                   endif
    C*
     C                   if        x5paid = '*************'
     C                   eval      x5paid = '0000000000000'
     C                   endif
    C*
     C                   if        x5fs = '*************'
     C                   eval      x5fs = '0000000000000'
     C                   endif
    C*** END ADD *** 09/22/06 *****************************
    C*
     C                   exsr      x5sign
    C*
     C                   exsr      updcap
     C                   exsr      updt5a
    C*
    C* Let's update or write the detail records
    C*
     C                   select
     C                   when      eobtyp = 'H'
     C                   exsr      wrtdup
     C                   when      eobtyp = 'N'
     C                   exsr      wrtdhp
     C                   when      eobtyp = 'X'
     C                   exsr      wrtdpp
     C                   endsl
    C*
     C                   exsr      updt5b
    C*
     C     x5ky1         reade     px501                                  99
     C                   enddo
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* X5SIGN - X5 amounts sign
    C*================================================================
     C     x5sign        begsr
    C*
     C                   if        %subst(x5chrg:1:1)= '-'
     C                   eval      x5chrg#= 0 - x5chrg#
     C                   endif
     C                   if        %subst(x5fs:1:1)  = '-'
     C                   eval      x5fs#  = 0 - x5fs#
     C                   endif
     C                   if        %subst(x5ppo:1:1) = '-'
     C                   eval      x5ppo# = 0 - x5ppo#
     C                   endif
     C                   if        %subst(x5paid:1:1)= '-'
     C                   eval      x5paid#= 0 - x5paid#
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDUP - Write FDDUP record
    C*================================================================
     C     wrtdup        begsr
    C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C     capky         chain     pdu01                              99
     C                   if        *in99  = *on
    C*
     C                   clear                   pdu01
     C                   eval      duco#  = caco#
     C                   eval      dusub# = casub#
     C                   eval      dufnd  = cafnd
     C                   eval      dufyr  = cafyr
     C                   eval      ducase = cacase
     C                   movel     cass#         duss#
     C                   eval      duacdt = caacdt
     C                   eval      dueob# = caeob#
     C                   eval      dudseq = cadseq
    C*
     C                   if        x5pos <> *blanks
     C                   eval      kkpos  = x5pos
     C     pxky1         chain     ppx01
     C                   if        %found(ftpxl1)
     C                   movel     pxpxrf        dupos
     C                   else
     C                   movel     ' 22'         dupos
     C                   endif
     C                   endif
    C*
     C                   select
     C                   when      x5unts <> *zeros  and
     C                             x5unts <> *blanks
     C                   move      x5unts        duunts
     C                   other
     C                   eval      duunts = 1
     C                   endsl
    C*
     C                   movel     kkrvcd        durvcd
     C                   movel     kkhcpc        durate
    C*
     C                   move      durvcd        dubprc
     C                   move      durate        dubrat
     C                   movel(p)  x5desc        dudesc
     C                   movel(p)  dudesc        dubprd
    C*
     C                   movel     'A'           dustat
     C                   movel(p)  causer        duuser
     C                   movel(p)  capgm         dupgm
     C                   eval      dutime = catime
     C                   eval      dudt   = cadt
     C                   movel(p)  causer        duiusr
     C                   movel(p)  capgm         duipgm
     C                   eval      duitim = caitim
     C                   eval      duidt  = caidt
    C*
     C                   write     pdu01
    C*
     C                   else
    C*
     C                   movel     kkrvcd        durvcd
     C                   movel     kkhcpc        durate
    C*
     C                   movel(p)  causer        duuser
     C                   movel(p)  capgm         dupgm
     C                   eval      dutime = catime
     C                   eval      dudt   = cadt
    C*
     C                   update    pdu01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTRP1 - Write WARPP record  - Provider Check
    C*================================================================
     C     wrtrp1        begsr
    C*
     C     eaky2         chain(n)  pea01                              99
    C*
     C                   eval      kkeob# = eoeob#
     C                   eval      kkeobs = eorcn
     C     rpky1         chain     prp01                              99
     C                   if        *in99 = *on
    C*
     C     rpky2         setgt     prp02
     C     rpky2         readpe    prp02                                  99
     C                   if        *in99 = *off
     C                   eval      xxseq# = rpseq# + 1
     C                   else
     C                   eval      xxseq# = 1
     C                   endif
    C*
     C                   clear                   prp01
     C                   eval      rpco#  = eoco#
     C                   eval      rpfnd  = kkwcf#
     C                   eval      rpfyr  = eofyr
     C                   eval      rpcase = eocase
     C                   eval      rpseq# = xxseq#
     C                   eval      rptrdt = eotrdt
     C                   eval      rpfein = eofein
     C                   eval      rpmod  = eomod
     C                   eval      rpamt  = eapaid
    C*
     C     cmky3         chain     pcm02                              99
     C                   movel     'P'           kkptyp
     C                   movel     eosys         kksys
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
     C                   movel(p)  cists         rpsts
    C*
     C                   else
    C*
     C                   if        eosys = 'P'
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
     C                   if        x4orid =  'GICSPCRPT'
     C                   movel(p)  'PE'          rpsts
     C                   endif
    C*
     C                   movel(p)  cmncci        rpncci
     C                   eval      rpfrdt = eofrdt
     C                   eval      rptodt = eotodt
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
     C                   movel(p)  eopnam        rppnam
     C                   movel(p)  eoadr1        rpadr1
     C                   movel(p)  eoadr2        rpadr2
     C                   movel(p)  eocty         rpcty
     C                   movel(p)  eost          rpst
     C                   eval      rpzip = eozip
     C                   movel     'P'           rpstat
     C                   movel(p)  eouser        rpuser
     C                   movel(p)  eopgm         rppgm
     C                   eval      rptime = eotime
     C                   eval      rpdt = eodt
     C                   eval      rpeob# = eoeob#
     C                   eval      rpeobs = eorcn
     C                   eval      rprcvd = eodt
    C*
     C                   movel(p)  rpsts         kkkey
     C     tbky          chain     pps01                              99
     C                   if        *in99 = *off
     C                   move(p)   pssrt         rpsort
     C                   endif
    C*
     C                   movel(p)  'N'           rpsent
     C                   movel(p)  'FIRSTHE'     rprqst
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
     C                   movel(p)  eaiusr        rpiusr
     C                   movel(p)  eapgm         rpipgm
     C                   eval      rpitm = eaitim
     C                   eval      rpidt = eaidt
    C*
     C                   if        rpamt > *zeros
     C                   write     prp01
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTRP2 - Write WARPP record  - Public Goods Pool Check
    C*================================================================
     C     wrtrp2        begsr
    C*
     C     rpky2         setgt     prp02
     C     rpky2         readpe    prp02                                  99
     C                   if        *in99 = *off
     C                   eval      xxseq# = rpseq# + 1
     C                   else
     C                   eval      xxseq# = 1
     C                   endif
    C*
     C                   clear                   prp01
     C                   eval      rpco# = eoco#
     C                   eval      rpfnd = kkwcf#
     C                   eval      rpfyr = eofyr
     C                   eval      rpcase = eocase
     C                   eval      rpseq# = xxseq#
     C                   eval      rptrdt = eotrdt
     C                   eval      rpfein = 141777232
    C*
     C                   eval      rpamt = eaoca1
    C*
     C     cmky3         chain     pcm02                              99
     C                   movel     'P'           kkptyp
     C                   movel     eosys         kksys
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
     C                   movel(p)  cists         rpsts
    C*
     C                   else
    C*
     C                   if        eosys = 'P'
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
     C                   eval      rpfrdt = eofrdt
     C                   eval      rptodt = eotodt
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
     C                   eval      kkfein = rpfein
     C                   eval      kkmod = *zeros
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
     C                   movel(p)  eouser        rpuser
     C                   movel(p)  eopgm         rppgm
     C                   eval      rptime = eotime
     C                   eval      rpdt = eodt
     C                   eval      rpeob# = eoeob#
     C                   eval      rpeobs = eorcn
     C                   eval      rprcvd = eodt
    C*
     C                   movel(p)  rpsts         kkkey
     C     tbky          chain     pps01                              99
     C                   if        *in99 = *off
     C                   move(p)   pssrt         rpsort
     C                   endif
    C*
     C                   movel(p)  'N'           rpsent
     C                   movel(p)  'FIRSTHE'     rprqst
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
     C                   movel(p)  eaiusr        rpiusr
     C                   movel(p)  eapgm         rpipgm
     C                   eval      rpitm = eaitim
     C                   eval      rpidt = eaidt
    C*
     C                   if        rpamt > *zeros
     C                   write     prp01
     C                   endif
    C*
     C                   endsr
