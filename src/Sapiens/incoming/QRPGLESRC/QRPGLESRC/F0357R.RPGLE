     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h***** Beg Add *** 06/09/20 *******************************************
     h Bnddir('STBNDDIR')
     h***** End Add *** 06/09/20 *******************************************
     h*****************************************************************
     h/TITLE F0357R - Work With Bills Returned from Procura
     h*****************************************************************
     h*  Created: 09/25/14  C:296 Project: 114MPQ Based on F0358R     *
     h*                                                               *
    h*****************************************************************
    h** REVISIONS:
     H*
     H*
     H*  06/09/20 C:295  Project : SY420
     H*                  Mask SSN
     H*
     H*  07/28/16 C:079 Support: 42612/114
     H*                 To modify the length to 32.
     H*
     H*  09/14/15 C:212 Project: 146MGI
     H*                 Change default tab name
     h*
    h*****************************************************************
    h*
      /IF DEFINED (WEB)
      /ENDIF
     ff0357f    cf   e             workstn
      /IF DEFINED (WEB)
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     sfile(scr01s:sfrrn1)
     f                                     sfile(scr02s:sfrrn2)
     f                                     infds(fids01)
    f*
    f*  Physical file definition (for refreshing subfile record)
     ffaeop     if   e             disk
     f                                     rename(peo01:pfrcdf)
    f*
    f*  Primary logical file definition (for displaying items)
VW#1 ffaeol76   if   e           k disk    usropn
     f                                     infds(dbfds)
     f                                     rename(peo01:dbrcdf)
VW#2 ffaeol78   if   e           k disk    usropn
     f                                     infds(dbfds2)
     f                                     rename(peo01:dbrcd2)
VW#3 ffaeol79   if   e           k disk    usropn
     f                                     infds(dbfds3)
     f                                     rename(peo01:dbrcd3)
VW#4 ffaeol80   if   e           k disk    usropn
     f                                     infds(dbfds4)
     f                                     rename(peo01:dbrcd4)
     ffaeoln    if   e           k disk
     f                                     infds(dbfdsc)
     f                                     rename(peo01:dbrcdc)
     ffaeolo    if   e           k disk
     f                                     infds(dbfdsd)
     f                                     rename(peo01:dbrcdd)
     ffaeolp    if   e           k disk
     f                                     infds(dbfdse)
     f                                     rename(peo01:dbrcde)
     ffaeolq    if   e           k disk
     f                                     infds(dbfdsg)
     f                                     rename(peo01:dbrcdg)
     ffaeol25   if   e           k disk
     f                                     infds(dbfdsh)
     f                                     rename(peo01:dbrcdh)
     ffaeol27   if   e           k disk
     f                                     infds(dbfdsi)
     f                                     rename(peo01:dbrcdi)
     ffaeol28   if   e           k disk
     f                                     infds(dbfdsk)
     f                                     rename(peo01:dbrcdk)
     ffaeol30   if   e           k disk
     f                                     infds(dbfdsl)
     f                                     rename(peo01:dbrcdl)
     ffaeolw    if   e           k disk
     f                                     infds(dbfdsm)
     f                                     rename(peo01:dbrcdm)
     ffaeolx    if   e           k disk
     f                                     infds(dbfdsn)
     f                                     rename(peo01:dbrcdn)
     ffaeoly    if   e           k disk
     f                                     infds(dbfdso)
     f                                     rename(peo01:dbrcdo)
     ffaeolz    if   e           k disk
     f                                     infds(dbfdsp)
     f                                     rename(peo01:dbrcdp)
     ffaeol10   if   e           k disk
     f                                     infds(dbfdsq)
     f                                     rename(peo01:dbrcdq)
     ffaeol11   if   e           k disk
     f                                     infds(dbfdsr)
     f                                     rename(peo01:dbrcdr)
     ffaeol12   if   e           k disk
     f                                     infds(dbfdss)
     f                                     rename(peo01:dbrcds)
     ffaeol13   if   e           k disk
     f                                     infds(dbfdst)
     f                                     rename(peo01:dbrcdt)
     ffaeol14   if   e           k disk
     f                                     infds(dbfdsu)
     f                                     rename(peo01:dbrcdu)
     ffaeol15   if   e           k disk
     f                                     infds(dbfdsv)
     f                                     rename(peo01:dbrcdv)
     ffaeol16   if   e           k disk
     f                                     infds(dbfdsw)
     f                                     rename(peo01:dbrcdw)
     ffaeol17   if   e           k disk
     f                                     infds(dbfdsx)
     f                                     rename(peo01:dbrcdx)
     ffaeol18   if   e           k disk
     f                                     infds(dbfdsy)
     f                                     rename(peo01:dbrcdy)
     ffaeol19   if   e           k disk
     f                                     infds(dbfdsz)
     f                                     rename(peo01:dbrcdz)
     ffaeol29   if   e           k disk
     f                                     infds(dbfdt5)
     f                                     rename(peo01:dbrc05)
     ffaeol26   if   e           k disk
     f                                     infds(dbfdt6)
     f                                     rename(peo01:dbrc06)
     ffaeol37   if   e           k disk
     f                                     infds(dbfd07)
     f                                     rename(peo01:dbrc07)
     ffaeol38   if   e           k disk
     f                                     infds(dbfd08)
     f                                     rename(peo01:dbrc08)
     ffaeol39   if   e           k disk
     f                                     infds(dbfd09)
     f                                     rename(peo01:dbrc09)
     ffaeol40   if   e           k disk
     f                                     infds(dbfd10)
     f                                     rename(peo01:dbrc10)
     ffaeol41   if   e           k disk
     f                                     infds(dbfd11)
     f                                     rename(peo01:dbrc11)
     ffaeol42   if   e           k disk
     f                                     infds(dbfd12)
     f                                     rename(peo01:dbrc12)
     ffaeol44   if   e           k disk
     f                                     infds(dbfd14)
     f                                     rename(peo01:dbrc14)
VW#5 ffaeol81   if   e           k disk    usropn
     f                                     infds(dbfds5)
     f                                     rename(peo01:dbrcd5)
VW#1 ffaeol46   if   e           k disk
     f                                     infds(dbfd15)
     f                                     rename(peo01:dbrc15)
VW#2 ffaeol47   if   e           k disk
     f                                     infds(dbfd16)
     f                                     rename(peo01:dbrc16)
VW#3 ffaeol48   if   e           k disk
     f                                     infds(dbfd17)
     f                                     rename(peo01:dbrc17)
VW#4 ffaeol49   if   e           k disk
     f                                     infds(dbfd18)
     f                                     rename(peo01:dbrc18)
VW#5 ffaeol50   if   e           k disk
     f                                     infds(dbfd19)
     f                                     rename(peo01:dbrc19)
VW#1 ffaeol56   if   e           k disk
     f                                     infds(dbfd20)
     f                                     rename(peo01:dbrc20)
VW#2 ffaeol57   if   e           k disk
     f                                     infds(dbfd21)
     f                                     rename(peo01:dbrc21)
VW#3 ffaeol58   if   e           k disk
     f                                     infds(dbfd22)
     f                                     rename(peo01:dbrc22)
VW#4 ffaeol59   if   e           k disk
     f                                     infds(dbfd23)
     f                                     rename(peo01:dbrc23)
VW#5 ffaeol60   if   e           k disk
     f                                     infds(dbfd24)
     f                                     rename(peo01:dbrc24)
VW#1 ffaeol61   if   e           k disk
     f                                     infds(dbfd25)
     f                                     rename(peo01:dbrc25)
VW#2 ffaeol62   if   e           k disk
     f                                     infds(dbfd26)
     f                                     rename(peo01:dbrc26)
VW#3 ffaeol63   if   e           k disk
     f                                     infds(dbfd27)
     f                                     rename(peo01:dbrc27)
VW#4 ffaeol64   if   e           k disk
     f                                     infds(dbfd28)
     f                                     rename(peo01:dbrc28)
VW#5 ffaeol65   if   e           k disk
     f                                     infds(dbfd29)
     f                                     rename(peo01:dbrc29)
    f*
    f*  Provider Demographics File
    f***FMPDL   IF  E           K        DISK
    f*
    f*  Subscriber Master File
    f***FMSMP   IF  E                    DISK
    f*
    f*  Group Master FIle
    f***FMFNP   IF  E                    DISK
    f*
    f*  Generic Table FIle
    f***FTGTP   IF  E                    DISK
    f*
    f*  User Parameter File
    f*
    f*  Claim Master File
     ffmcml1    if   e           k disk
     f                                     infds(dbfcm1)
    f*
    f*  EOMB Status Change Transaction File
     ffaesl     if   e           k disk
     f                                     infds(dbfda)
    ffaeal1    if   e           k disk
    f*
    f*  Pre-Certification Master File
    f***UMPCL11 IF  E           K        DISK
    f***                                           KINFDS DBFDB
    f*  Subscriber Master File
    f***FMSML   IF  E           K        DISK
    f***         PSM01                             KRENAMEPSM02
    f*  WCMC Fund/Group Master File
    f***FMFNL   IF  E           K        DISK
    f***         PFN01                             KRENAMEPFN02
    f*  Generic Table  File
    f***FTGTL   IF  E           K        DISK
    f***         PGT01                             KRENAMEPGT02
    f*  User Parameter File
    f***FMBEL   IF  E           K        DISK
    f***FELGL4  IF  E           K        DISK
    f*WDPALA  IF  E           K        DISK                          UC
    fsmupl     if   e           k disk
     ffmsml     if   e           k disk
     ffmfnl1    if   e           k disk
     ffmfnl     if   e           k disk
     f                                     rename(pfn01:pfn02)
     fimdtl     if   e           k disk
     fimdol1    if   e           k disk
     f*
    d*
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     d hd              s             78    dim(20) ctdata perrcd(1)
     d of              s              1    dim(33) ctdata perrcd(1)
     d ofd             s             35    dim(33) alt(of)
     d grp             s              3    dim(999)
    d*
     d qca             s             60    dim(14) ctdata perrcd(1)
    d*
     d data            s             74    dim(5)
     d dta1            s              1    dim(74)
     d dta2            s              1    dim(74)
     d dta3            s              1    dim(74)
     d dta4            s              1    dim(74)
     d dta5            s              1    dim(74)
     d inf1            s              1    dim(29)
     d inf2            s              1    dim(29)
     d inf3            s              1    dim(29)
     d inf4            s              1    dim(29)
    d*
    d*
     d dbfda           ds
     d dbrrna                397    400b 0
     d dbfdsc          ds
     d dbrrnc                397    400b 0
     d dbfdsd          ds
     d dbrrnd                397    400b 0
     d dbfdse          ds
     d dbrrne                397    400b 0
     d dbfdsg          ds
     d dbrrng                397    400b 0
     d dbfdsh          ds
     d dbrrnh                397    400b 0
     d dbfdsi          ds
     d dbrrni                397    400b 0
     d dbfdsj          ds
     d dbrrnj                397    400b 0
     d dbfdsk          ds
     d dbrrnk                397    400b 0
     d dbfdsl          ds
     d dbrrnl                397    400b 0
     d dbfdsm          ds
     d dbrrnm                397    400b 0
     d dbfdsn          ds
     d dbrrnn                397    400b 0
     d dbfdso          ds
     d dbrrno                397    400b 0
     d dbfdsp          ds
     d dbrrnp                397    400b 0
     d dbfdsq          ds
     d dbrrnq                397    400b 0
     d dbfdsr          ds
     d dbrrnr                397    400b 0
     d dbfdss          ds
     d dbrrns                397    400b 0
     d dbfdst          ds
     d dbrrnt                397    400b 0
     d dbfdsu          ds
     d dbrrnu                397    400b 0
     d dbfdsv          ds
     d dbrrnv                397    400b 0
     d dbfdsw          ds
     d dbrrnw                397    400b 0
     d dbfdsx          ds
     d dbrrnx                397    400b 0
     d dbfdsy          ds
     d dbrrny                397    400b 0
     d dbfdsz          ds
     d dbrrnz                397    400b 0
     d dbfdt1          ds
     d dbrr01                397    400b 0
     d dbfdt2          ds
     d dbrr02                397    400b 0
     d dbfdt3          ds
     d dbrr03                397    400b 0
     d dbfdt4          ds
     d dbrr04                397    400b 0
     d dbfcm1          ds
     d dbrcm1                397    400b 0
     d dbfdt5          ds
     d dbrr05                397    400b 0
     d dbfdt6          ds
     d dbrr06                397    400b 0
     d dbfd07          ds
     d dbrr07                397    400b 0
     d dbfd08          ds
     d dbrr08                397    400b 0
     d dbfd09          ds
     d dbrr09                397    400b 0
     d dbfd10          ds
     d dbrr10                397    400b 0
     d dbfd11          ds
     d dbrr11                397    400b 0
     d dbfd12          ds
     d dbrr12                397    400b 0
     d dbfd13          ds
     d dbrr13                397    400b 0
     d dbfd14          ds
     d dbrr14                397    400b 0
     d dbfd15          ds
     d dbrr15                397    400b 0
     d dbfd16          ds
     d dbrr16                397    400b 0
     d dbfd17          ds
     d dbrr17                397    400b 0
     d dbfd18          ds
     d dbrr18                397    400b 0
     d dbfd19          ds
     d dbrr19                397    400b 0
     d dbfd20          ds
     d dbrr20                397    400b 0
     d dbfd21          ds
     d dbrr21                397    400b 0
     d dbfd22          ds
     d dbrr22                397    400b 0
     d dbfd23          ds
     d dbrr23                397    400b 0
     d dbfd24          ds
     d dbrr24                397    400b 0
     d dbfd25          ds
     d dbrr25                397    400b 0
     d dbfd26          ds
     d dbrr26                397    400b 0
     d dbfd27          ds
     d dbrr27                397    400b 0
     d dbfd28          ds
     d dbrr28                397    400b 0
     d dbfd29          ds
     d dbrr29                397    400b 0
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d                 ds
     d qqcmd                         60
     d  qqst                          2    overlay(qqcmd:38)
     d                 ds
     d qqcmd2                        60
     d  qqst2                         2    overlay(qqcmd2:40)
     d                 ds
     d x1dt                          10
     d  x1dtm                         2    overlay(x1dt)
     d  x1dtd                         2    overlay(x1dt:4)
     d  x1dty                         4    overlay(x1dt:7)
     d                 ds
     d  x2dt                   1      8
     d  x2dtm                  1      2
     d  x2dtd                  3      4
     d  x2dty                  5      8
     d qryslt          ds          5000
    d*
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d slmsgi          c                   const('SYS0001')
     d slmsgf          c                   const('FSMESG')
     d acdpgm          c                   const('F0350R01')
     d comapi          c                   const('FA007R')
     d uspapi          c                   const('F0006R')
     d qrypgm          c                   const('F0350C1')
     d infomx          c                   const(4)
     d viewmx          c                   const(5)
     d sf1pag          c                   const(8)
    d*================================================================
    d*  Program specific constants
    d*================================================================
     d heada           c                   const('Dates of Service')
     d clropt          c                   const('                    -
     d                                            NNN')
     d pciopt          c                   const('1515=Pre-Cert Info  -
     d                                            YYN')
     d clmopt          c                   const('8181=Claim info     -
     d                                            YYN')
     d ediopt          c                   const('1818=EDI Log Info   -
     d                                            YYN')
     d ssqopt          c                   const('1919=Send Subsq 837 -
     d                                            YNN')
     d vckopt          c                   const('2121=Check info     -
     d                                            YYN')
     d delete          c                   const('Deleted')
     d co#             c                   const('(EOCO# *EQ')
     d co#2            c                   const(') *AND (')
     d sub#            c                   const('(EOSUB# *EQ')
     d fnd             c                   const('*AND (EOFND *EQ -
     d                                     %VALUES(')
     d ovrdb1          c                   const('OVRDBF FILE(')
     d ovrdb2          c                   const(') SHARE(*YES)')
    d** **         'SEQONLY(*NO)'
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ##acdt          s              8  0
     d ##acis          s              1
     d ##ar            s              1
     d ##arin          s              1
     d ##ast           s                   like(eoast)
     d ##blst          s              1
     d ##bscl          s              3  0
     d ##bsct          s              1
     d ##btyp          s                   like(eobtyp)
     d ##c#cl          s              3  0
     d ##c#ct          s              1
     d ##call          s              1
     d ##case          s              7  0
     d ##cert          s             15
     d ##ckdl          s              3  0
     d ##ckdt          s              1
     d ##cldl          s              3  0
     d ##cldt          s              1
     d ##cltl          s              3  0
     d ##cltt          s              1
     d ##co#           s              3  0
     d ##conm          s             40
     d ##cont          s              1
     d ##cpaa          s              5
     d ##cpad          s              5
     d ##cs#l          s              3  0
     d ##cs#t          s              1
     d ##dccl          s              3  0
     d ##dcct          s              1
     d ##dcdl          s              3  0
     d ##dcdt          s              1
     d ##depg          s             10
     d ##desc          s             40
     d ##dnsl          s              3  0
     d ##dnst          s              1
     d ##edi           s              1
     d ##eob#          s                   like(eoeob#)
     d ##eobd          s              3  0
     d ##eobi          s              1
     d ##err           s              1
     d ##ext           s              3
     d ##fein          s              9
     d ##fimr          s              5
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##frn           s              5
     d ##frst          s             15
     d ##fsch          s              1
     d ##fyr           s              3  0
     d ##kc            s              1
     d ##key           s             40
     d ##kl            s              1
     d ##last          s             56
     d ##lseq          s              5  0
     d ##lvl           s              3  0
     d ##mcif          s              1
     d ##mciv          s              1
     d ##med           s              1
     d ##meus          s              1  0
     d ##mgid          s              1
     d ##mod           s              3
     d ##mode          s              1
     d ##mss           s              1
     d ##name          s             40
     d ##nerc          s              5
     d ##npwc          s              1
     d ##ntwk          s              1
     d ##nwpa          s              5
     d ##nwst          s              1
     d ##ocst          s              1
     d ##outq          s             10
     d ##outs          s              1
     d ##pay0          s              1
     d ##pbiv          s              1
     d ##pgm2          s             10
     d ##plcl          s              3  0
     d ##plct          s              1
     d ##prvb          s              1
     d ##pvmc          s              1  0
     d ##pvpr          s              1  0
     d ##que           s                   like(##outq)
     d ##rcn           s              3  0
     d ##reas          s             60
     d ##rhab          s              1
     d ##rrn           s              9  0
     d ##rrn2          s              9  0
     d ##rrn3          s              9  0
     d ##rtcd          s              1
     d ##sdcl          s              3  0
     d ##sdct          s              1
     d ##sdts          s              1
     d ##sort          s              2
     d ##ss#           s              9
     d ##stcd          s              3
     d ##sts           s              2
     d ##sub#          s              5  0
     d ##sys           s                   like(eosys)
     d ##tbid          s              4
     d ##term          s              1
     d ##timr          s              5
     d ##trn           s              5
     d ##tset          s              3
     d ##type          s              1
     d ##ur            s              1
     d ##user          s             10
     d ##usnm          s             50
     d ##view          s              1  0
     d ##wcdk          s              1
     d ##wcif          s              1
     d ##wcin          s              1
     d ##wcup          s              1
     d ##1n4           s              1
     d ##1prv          s              1
     d #1case          s              7
     d #1co#           s              3
     d #1eob#          s              9
     d #1err           s              1
     d #1ext           s              3  0
     d #1fein          s              9  0
     d #1fnd           s              3
     d #1fyr           s              3
     d #1key           s             10
     d #1l1            s             78
     d #1l2            s             78
     d #1mod           s              3  0
     d #1mode          s              1
     d #1name          s             40
     d #1pgm           s              8
     d #1rrn           s              9
     d #1rtcd          s              1
     d #1sub#          s              5
     d #1wcf#          s              3
     d #1wcin          s              1
     d #1wdck          s              1
     d #2case          s              7  0
     d #2err           s              1
     d #2fnd           s              3  0
     d #2fyr           s              3  0
     d #2name          s             40
     d #2rrn           s              9  0
     d #2sub#          s              5  0
     d @@call          s              1
     d @@case          s              7  0
     d @@cert          s             15
     d @@eob#          s              9  0
     d @@fnd           s              3  0
     d @@fyr           s              3  0
     d @@rcn           s              3  0
     d @@rrn2          s              9  0
     d @@sfdt          s              8
     d @@sfei          s              9
     d @@smod          s              3
     d @@stdt          s              8
     d @@sub#          s              5  0
     d @3blst          s              1
     d @3kc            s              1
     d @3kl            s              1
     d @3mode          s              1
     d @3pgm           s              8
     d @3reas          s             60
     d @3rrn           s              9  0
     d @3rrn2          s              9  0
     d @3rrn3          s              9  0
     d @3stcd          s              3
     d alchrg          s             10
     d aleob#          s              9
     d alfein          s              9
     d almod           s              3
     d alpaid          s             10
     d hlblst          s              1
     d hlbsds          s             30
     d hlcase          s                   like(eocase)
     d hldate          s              8
     d hlfein          s                   like(eofein)
     d hlfnd           s                   like(eofnd)
     d hlfnnm          s             30
     d hlfrdt          s                   like(eofrdt)
     d hlfyr           s                   like(eofyr)
     d hlinf1          s             29
     d hlinf2          s             29
     d hlinf3          s             29
     d hlinf4          s             29
     d hliusr          s                   like(eouser)
     d hlmod           s                   like(eomod)
     d hlpnam          s                   like(eopnam)
     d hlsdoi          s                   like(eoacdt)
     d hlsmnm          s             30
     d hlsub#          s                   like(eosub#)
     d hltodt          s                   like(eofrdt)
     d hlusnm          s             40
     d kkacdt          s                   like(eoacdt)
     d kkcase          s                   like(cmcase)
     d kkcert          s             15
     d kkcntl          s                   like(eocntl)
     d kkeob#          s                   like(eoeob#)
     d kkext           s              3  0
     d kkfein          s                   like(eofein)
     d kkfnd           s                   like(cmfnd)
     d kkfrdt          s                   like(eofrdt)
     d kkfsn           s                   like(eofsn)
     d kkfyr           s                   like(cmfyr)
     d kkidt           s                   like(esidt)
     d kkitim          s                   like(esitim)
     d kkkey           s             10
     d kklsn           s                   like(eolsn)
     d kkmod           s                   like(eomod)
     d kkpnam          s                   like(eopnam)
     d kkrcn           s                   like(eorcn)
     d kkss#           s                   like(eoss#)
     d kksub#          s                   like(cmsub#)
     d kktbid          s              4
     d kkusr           s                   like(upusr)
     d length          s             15  5
     d qqacdt          s              7  0
     d qqactf          s              1
     d qqcase          s              7
     d qqco#           s              3
     d qqeob#          s              9
     d qqflag          s              1
     d qqfnd           s              3
     d qqfyr           s              3
     d qqmask          s             10
     d qqmod           s              3
     d qqmode          s              1
     d qqnum           s             80
     d qqrcn           s              3
     d qqrtcd          s              1
     d qqsub#          s              5
     d ssbsds          s             30
     d sspdnm          s             20
     d stop            s              1
     d vldopt          s              1
     d x               s              2  0
     d xxcntl          s                   like(eocntl)
     d xxmod           s              3  0
     d x1fein          s              9
     d x1mod           s              3
     d y               s              2  0
     d zzfein          s              9
     d zzmod           s              3
     d #2emp#          s                   like(upemp#)
     d $$fnd           s                   like(upfnd)
     d $$sub#          s                   like(upsub#)
     d svcall          s                   like(@@call)
     d svin09          s                   like(*in09)
     d ##mrem          s                   like(upmrem)
     d ##mrgr          s                   like(upmrgr)
     d ##mrsu          s                   like(upmrsu)
     d xxovrd          s             50
     d cmdstr          s             50
     d z               s              3  0
     d xaco#           s              3
     d xasub#          s              5
     d grperr          s              1
     d first           s              1
     d ##prom          s              1
     d ##preo          s              1
     d ##parn          s              1
     d ##os            s              1
     d ##mgri          s              1
     d ##mcrv          s              1
     d ##freq          s              1
     d ##crac          s              1
     d ##crvw          s              1
     d ##ccod          s              1
     d ##auth          s              1
     d ##arls          s              1
     d optnm           s             25
     d xxcall          s              1
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     d*
     d kkdoct          s                   like(dtdoct)
     d kkdcid          s                   like(dodcid)
     d*
     C/COPY SCOPYWW01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@eob#
     C                   parm                    @@cert
     C                   parm                    @@sfdt
     C                   parm                    @@stdt
     C                   parm                    @@sfei
     C                   parm                    @@smod
     C                   parm                    @@rcn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    #2sub#
     C                   parm                    #2fnd
     C                   parm                    #2fyr
     C                   parm                    #2case
     C                   parm                    ##nwst
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     chgpl         plist
     C                   parm                    qqnum
     C                   parm                    qqmask
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*
    C* PARM LIST FOR DARTS API
    C*
     C     drtspl        plist
     C                   parm      *blanks       #1rtcd
     C                   parm      ##wcin        #1wcin
     C                   parm      ##wcdk        #1wdck
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      *blanks       ##key
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     rstpl         plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr            10
     C                   parm      'M'           ##appl            1
     C                   parm      *off          ##rcds            1
    C*
    C*
     C     rstplf        plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C                   parm                    ##auth
     C                   parm      fnfnd         ##fnd
     C                   parm      *zero         ##emp#            9 0
     C                   parm      fnsub#        ##sub#
    C*
     C     rstpls        plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C                   parm                    ##auth
     C                   parm      *zero         ##fnd
     C                   parm      *zero         ##emp#
     C                   parm      smsub#        ##sub#
    C*
     C     qrypl         plist
     C                   parm                    qryslt
     C*** parameter list for claims secuirty API
     C     cuspl         plist
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      q1user        ##usr
     C                   parm                    #1rtcd
    C*
    C* key lists for eomb transaction file
    C*
     C     key           klist
     C                   kfld                    ##co#
    C*
     C     key1          klist
     C                   kfld                    ##co#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key2          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key3          klist
     C                   kfld                    ##co#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key4          klist
     C                   kfld                    ##co#
     C                   kfld                    kkeob#
    C*
     C     key5          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key6          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key7          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key8          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kkeob#
    C*
     C     key9          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
    C*
     C     key10         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key11         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
    C*
     C     key11a        klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key12         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key13         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key14         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key15         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
    C*
     C     key15a        klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key16         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key17         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key18         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key19         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key20         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key21         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kkeob#
    C*
     C     key22         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
    C*
     C     key23         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key24         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key25         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key26         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkeob#
    C*
     C     key27         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
    C*
     C     key28         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
    C*
     C     key29         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     key30         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
    C*
     C     key31         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
    C*
     C     key32         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
    C*
     C     key33         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     key34         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
    C*
     C     key35         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C     key41         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkcntl
    C*
     C     key42         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkcert
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key43         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkfein
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key44         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key45         klist
     C                   kfld                    ##co#
     C                   kfld                    kkfein
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key46         klist
     C                   kfld                    ##co#
     C                   kfld                    kkeob#
     C                   kfld                    kkcntl
    C*
     C     key48         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key49         klist
     C                   kfld                    ##co#
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
    C*
     C     key50         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key51         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key52         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key53         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkeob#
    C*
     C     key54         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key55         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
    C*
    C*
     C     key56         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key57         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key58         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key59         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkeob#
    C*
     C     key60         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key61         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
    C*
     C     key62         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key63         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key64         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key65         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkeob#
    C*
     C     key66         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key67         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
    C*
    C*
    C* key list for provider demographics file
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     pdky1         klist
     C                   kfld                    kkfein
    C*
    C* key list for claims master file
    C*
     C     cmky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
     C     cmky1         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
    C*
    C* key list for eomb status change transaction file
    C*
     C     esky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     esky2         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkidt
     C                   kfld                    kkitim
    C*
    C* key list for pre-cert master file
    C*
     C     pcky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkcert
     C                   kfld                    kkext
    C*
    C* key list for WCMS payment detail file
    C*
    C*
    C* key list for subscriber master file
    C*
     C     smky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
    C*
    C* key list for WCMC fund/group master file
    C*
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C     fnkey         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C*
     C     fnky3         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
    C*
    C* key list for generic table file
    C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*
    C* key list for user parameter file
    C*
    C*
    C* key list for user parameter file
    C*
    C     upky          klist
    C                   kfld                    kkusr
    C*
    C* key list for Bill Entry Control File
    C*
     C     beky          klist
     C                   kfld                    scast
     C                   kfld                    scsys
     C                   kfld                    scbtyp
    C*
    C*
    C     eaky          klist
    C                   kfld                    ##co#
    C                   kfld                    scsub#
    C                   kfld                    scfnd
    C                   kfld                    scfyr
    C                   kfld                    sccase
    C                   kfld                    sceob#
    C                   kfld                    scrcn
    C     eaky1         klist
    C                   kfld                    ##co#
    C                   kfld                    eosub#
    C                   kfld                    eofnd
    C                   kfld                    eofyr
    C                   kfld                    eocase
    C                   kfld                    eoeob#
    C                   kfld                    eorcn
     C*
     C     doky          klist
     C                   kfld                    kkdcid
     C     dtky          klist
     C                   kfld                    kkdoct
     C*
    C*
    C*  KEY LIST FOR EDI LOG RECORD
    C*
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*
    C*================================================================
    C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
    C*================================================================
     C     assrrn        begsr
    C*
     C                   select
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
     C                   when      ssfein <> *zeros
    C*
     C                   when      sseob# <> *zeros
    C*
     C                   when      sssub# <> *zeros and ssfnd <> *zeros
     C*
     C                   when      sssub# <> *zeros
    C*
     C                   when      ssiusr <> *blanks
    C*
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
    C*
     C                   if        sssub# <> *zeros
     C                             or ssfnd <> *zeros
     C                             or ssfyr <> *zeros
     C                             or sscase <> *zeros
     C                             or sseob# <> *zeros
     C                             or sscert <> *blanks
     C                             or ssfrdt <> *zeros
     C                             or sstodt <> *zeros
     C                             or ssfein <> *zeros
     C                             or ssmod <> *blanks
     C                             or ssbtyp <> *blanks
    C*
     C                   eval      *in11 = *on
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* DATSBR - Date check subrountine
    C*================================================================
     C     datsbr        begsr
    C*
    C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
    C* FORMAT ALL DATES ON SCREEN
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssfrdt
    C*
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sstodt
    C*
     C                   endsr
    C*
    C*================================================================
    C* DLOVRD - Delete file overrides
    C*================================================================
     C     dlovrd        begsr
     C                   eval      length = 60
     C                   call(e)   'QCMDEXC'                              60
     C                   parm                    qca(8)
     C                   parm                    length
     C                   call(e)   'QCMDEXC'                              60
     C                   parm                    qca(9)
     C                   parm                    length
     C                   if        %error
     C                   endif
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(10)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(11)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(12)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(13)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(14)
     C                   parm                    length
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
    C*
     C                   move      'Y'           vldopt
     C                   select
    C*
    C*  Check for user authorization to OPT 2 = change.
    C*
     C                   when      scsel = '2'
     C                             and scauth = 'I'
     C                             and ##rcds = *on
     C                             and *in55 = *on
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
    C*
     C                   when      scauth = 'I'
     C                             and *in55 = *on
    C*
     C                   if        scsel = '3'
     C                             or scsel = '4'
     C                             or scsel = '6'
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   other
    C*
    C* disallow certain options of bill status is delete
    C*
     C                   if        scblst = 'DEL'
     C                   if        scsel = '2'
     C                             or scsel = '4'
     C                             or scsel = '9'
     C                             or scsel = '12'
     C                             or scsel = '13'
     C                             or scsel = '17'
     C                             or scsel = '19'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
    C*
     C                   if        scblst = 'DEL'
     C                             and @@call <> '1'
     C                             and scsel = '7'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck1end
     C                   endif
    C*
    C* DO NOT CALL OPT15=PRE-CERT INFO IF NO PRECERT EXIST FOR BILL
    C*
     C                   if        scsel = '15'
     C     scdbrr        chain     pfrcdf                             98
     C                   if        *in98 = *off
     C                             and eocert = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
     C                   if        *in90 = *off
     C                   if        scsel = '2' or scsel = '4'
     C*
     C     eaky          chain     pea01                              99
     C                   if        *in99=*off and eansnt = 'S'
     C                   seton                                        7590
     C                   endif
     C*
     C                   endif
     C                   endif
    C*
    C* DELETE PROCESSING
    C*
     C                   if        *in90 = *off
     C                   eval      ##rrn = 0
     C                   if        scsel = '4'
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
    C*
     C                   call      'FA701R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       ##outs
    C*
    C* Only chain to WDPAP if Workers Comp is inst and linked case
     C                   if        ##wcin = 'Y'
     C                             and ##rtcd = *blank
     C                             and ##outs <> 'Y'
     C                             and ##npwc <> 'Y'
     C                             and ##wcif = 'Y'
    C*
    C* When there is a check in history only allow option 4=Delete
    C* if the user has the proper security level.
    C*
     C                   move      ##co#         #1co#
     C                   move      scfnd         #1fnd
     C                   move      scsub#        #1sub#
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
     C                   move      sceob#        #1eob#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
     C                   call      'WA047R'
     C                   parm                    #1co#
     C                   parm      #1wcf#        #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    #1eob#
     C                   parm      *blanks       ##rtcd
    C*
     C                   if        ##rtcd <> 'F'
    C*
     C                   call      'FA715R'
     C                   parm      ' '           ##rtcd
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    ##pvmc
     C                   parm                    ##pvpr
     C                   parm                    ##wcup
     C                   parm                    ##cldl
     C                   parm                    ##cldt
     C                   parm                    ##cltl
     C                   parm                    ##cltt
     C                   parm                    ##sdcl
     C                   parm                    ##sdct
     C                   parm                    ##bscl
     C                   parm                    ##bsct
     C                   parm                    ##dcdl
     C                   parm                    ##dcdt
     C                   parm                    ##dccl
     C                   parm                    ##dcct
     C                   parm                    ##c#cl
     C                   parm                    ##c#ct
     C                   parm                    ##fimr
     C                   parm                    ##timr
     C                   parm                    ##frn
     C                   parm                    ##trn
     C                   parm                    ##cs#l
     C                   parm                    ##cs#t
     C                   parm                    ##dnsl
     C                   parm      ' '           ##dnst
     C                   parm                    ##nerc
     C                   parm                    ##meus
     C                   parm                    ##nwpa
     C                   parm                    ##cpaa
     C                   parm                    ##cpad
     C                   parm                    ##plcl
     C                   parm                    ##plct
     C                   parm      0             ##ckdl
     C                   parm      ' '           ##ckdt
    C*
    C*
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
     C                   call      'SA006R'
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##ckdl
     C                   parm                    ##ckdt
     C                   parm      ' '           ##rtcd
    C*
     C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in88 = *on
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
    C*
    C* IF AR IS INSTALLED, CALL WORK WITH EOMB INVOICES PROGRAM
    C*
     C                   if        ##ar = 'Y'
     C                   eval      ##rrn = scdbrr
     C                   move      ' '           ##call
     C                   eval      ##sub# = scsub#
     C                   eval      ##fnd = scfnd
     C                   eval      ##eob# = sceob#
     C                   eval      ##case = sccase
     C                   eval      ##fyr = scfyr
     C                   call      'N0350R1'
     C                   parm                    ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm                    ##co#
     C                   parm                    ##sub#
     C                   parm                    ##fnd
     C                   parm                    ##fyr
     C                   parm                    ##case
     C                   parm                    ##eob#
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                   move      '1'           rstpgm
     C                   clear                   scsel
     C                   move      *on           stop
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*
     C                   movel     '99'          scsel
     C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IF THE CORRESPONDING
    C* GROUP IS EXPIRED  (F9, F19)
    C*
     C                   if        scsel = '9'
     C                             or scsel = '19'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
    C*
     C                   call      'FA729R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    #2err
    C*
     C                   if        #2err = '1'
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
     C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IN CHANGE MODE IF THE
    C* CORRESPONDING GROUP IS EXPIRED.  IF ONE OF THESE OPTIONS IS
    C* PRESSED, THEN GIVE THE USER THE EXPIRED GROUP WINDOW AND TAKE
    C* THEM INTO THEIR SELECTED OPTION IN DISPLAY MODE.
    C*          OPTIONS 2 AND 18
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '2'
     C                             or scsel = '18'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
    C*
     C                   call      'FA729R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    #2err
    C*
     C                   if        #2err = '1'
     C                   call      'FA720R'
     C                   parm      '2'           ##mgid
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
    C*
     C                   if        scsel = '2'
     C                   movel(p)  '5'           scsel
     C                   endif
    C*
     C                   move      'D'           ##mode
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
     C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IF THE
    C* CLAIM INDICATES A 'Y' IN THE DENIED FLAG AND THE USER'S
    C* SECURITY LEVEL AND TYPE DOES NOT PASS THE SECURTY LEVEL AND
    C* TYPE DEFINED INTHE CLAIMS/DARTS CONTROL FILE.
    C*          OPTIONS 9 AND 19
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '9'
     C                             or scsel = '19'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   if        cmdny = 'Y'
     C                   eval      ##dnsl = 0
     C                   call      'FA715R'
     C                   parm      ' '           ##rtcd
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    ##pvmc
     C                   parm                    ##pvpr
     C                   parm                    ##wcup
     C                   parm                    ##cldl
     C                   parm                    ##cldt
     C                   parm                    ##cltl
     C                   parm                    ##cltt
     C                   parm                    ##sdcl
     C                   parm                    ##sdct
     C                   parm                    ##bscl
     C                   parm                    ##bsct
     C                   parm                    ##dcdl
     C                   parm                    ##dcdt
     C                   parm                    ##dccl
     C                   parm                    ##dcct
     C                   parm                    ##c#cl
     C                   parm                    ##c#ct
     C                   parm                    ##fimr
     C                   parm                    ##timr
     C                   parm                    ##frn
     C                   parm                    ##trn
     C                   parm                    ##cs#l
     C                   parm                    ##cs#t
     C                   parm                    ##dnsl
     C                   parm      ' '           ##dnst
     C                   parm                    ##nerc
     C                   parm                    ##meus
     C                   parm                    ##nwpa
     C                   parm                    ##cpaa
     C                   parm                    ##cpad
     C                   parm                    ##plcl
     C                   parm                    ##plct
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
    C*****      UPLVL     IFLT ##DNSL
    C*****      UPTYPE    ORNE ##DNST
     C                   call      'SA006R'
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##dnsl
     C                   parm                    ##dnst
     C                   parm      ' '           ##rtcd
     C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   endif
     C                   endif
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IN CHANGE MODE IF THE
    C* CLAIM INDICATES A 'Y' IN THE DENIED FLAG AND THE USER'S
    C* SECURITY LEVEL AND TYPE DOES NOT PASS THE SECURTY LEVEL AND
    C* TYPE DEFINED INTHE CLAIMS/DARTS CONTROL FILE.
    C*          OPTIONS 2, 17 AND 18
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '2'
     C                             or scsel = '18'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   if        cmdny = 'Y'
     C                   eval      ##dnsl = 0
     C                   call      'FA715R'
     C                   parm      ' '           ##rtcd
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    ##pvmc
     C                   parm                    ##pvpr
     C                   parm                    ##wcup
     C                   parm                    ##cldl
     C                   parm                    ##cldt
     C                   parm                    ##cltl
     C                   parm                    ##cltt
     C                   parm                    ##sdcl
     C                   parm                    ##sdct
     C                   parm                    ##bscl
     C                   parm                    ##bsct
     C                   parm                    ##dcdl
     C                   parm                    ##dcdt
     C                   parm                    ##dccl
     C                   parm                    ##dcct
     C                   parm                    ##c#cl
     C                   parm                    ##c#ct
     C                   parm                    ##fimr
     C                   parm                    ##timr
     C                   parm                    ##frn
     C                   parm                    ##trn
     C                   parm                    ##cs#l
     C                   parm                    ##cs#t
     C                   parm                    ##dnsl
     C                   parm      ' '           ##dnst
     C                   parm                    ##nerc
     C                   parm                    ##meus
     C                   parm                    ##nwpa
     C                   parm                    ##cpaa
     C                   parm                    ##cpad
     C                   parm                    ##plcl
     C                   parm                    ##plct
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
     C                   if        ##lvl < ##dnsl
     C                             or ##type <> ##dnst
    C*
     C                   call      'FA720R'
     C                   parm      '3'           ##mgid
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
    C*
     C                   if        scsel = '2'
     C                   movel(p)  '5'           scsel
     C                   endif
    C*
     C                   move      'D'           ##mode
     C                   goto      ck1end
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   endif
     C                   endif
    C*
    C**CONDITION ABILITY TO A RECON ON THE RECON ALLOWANCE FLAG
    C**RECON*SECURITY LEVEL AND TYPE
    C*
    C*          Option 3 = Copy
    C*             Call F7098 to perform copies
    C*          ------------------------------------------------------
     C                   if        *in90 = *off
     C                   if        scsel = '3'
     C                   call      'F7098R'
     C                   parm      scdbrr        @3rrn
     C                   parm                    @3rrn2
     C                   parm                    @3rrn3
    C*
    C*             Call F0395 to display new EOMB
    C*             ---------------------------------------------------
     C                   move      'D'           @3mode
     C                   move      'P'           @3blst
     C                   call      'F0395R01'
 P1  C                   parm      *inkc         @3kc
 P2  C                   parm      *inkl         @3kl
 P3  C                   parm                    @3mode
 P4  C                   parm                    @3rrn2
 P5  C                   parm                    @3rrn3
 P6  C                   parm                    @3blst
 P7  C                   parm      *blanks       @3stcd
     C                   parm      *blanks       @3reas
     C                   parm      *blanks       @3pgm
     C                   clear                   scsel
     C                   eval      *in90 = *on
     C                   endif
     C                   endif
    C*
    C*
    C* DETERMINE IF THE OPTION2=CHANGE IS A REGULAR CHANGE OR TAPE
    C* SUBMISSION CORRECTION OR EDI CORRECTION AND TAKE THE
    C* APPROPRIATE PATH
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '2'
     C     scdbrr        chain     pfrcdf                             99
     C                   if        *in99 = *off
     C                             and eoblst = 'C'
     C                   if        eotape = 'Y'
     C                             or eotape = 'R'
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   eval      kkrcn = eorcn
     C                   call      'FA731R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm      kkfnd         ##fnd
     C                   parm      kkfyr         ##fyr
     C                   parm      kkcase        ##case
     C                   parm      kkeob#        ##eob#
     C                   parm      kkrcn         ##rcn
     C                   parm                    ##lseq
     C                   parm                    ##sts
    C*
     C                   if        ##sts <> '  '
     C                   if        ##sts = 'SN'
     C                   eval      *in90 = *on
     C                   eval      *in87 = *on
     C                   goto      ck1end
     C                   endif
     C                   if        ##sts = 'TA'
     C                   move      'R'           ##nwst
     C                   else
     C                   move      'E'           ##nwst
     C                   endif
     C                   else
     C                   if        eocntl = *blanks
     C                   move      'R'           ##nwst
     C                   else
     C                   move      'B'           ##nwst
     C                   endif
     C                   endif
     C                   else
     C                   move      'R'           ##nwst
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
    C* DON'T ALLOW A TWO TO CHANGE IF CODE REVIEW PENDED THIS ERROR
    C* DUE TO AN ERROR IN PROCESSING.
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '2'
     C                             and sccrpe = 'E'
    C*
     C                   exsr      mvdb04
     C                   exfmt     scr04
     C                   seton                                        9084
    C*
     C                   endif
     C                   endif
     C                   endsl
      /Free
       // If a document is associated with this eomb the display it.
        If xxclt# = '114' or xxclt# = '129';
          If scSel = '2' and sciDoc <> *blanks;
             exsr dspDoc;
          Endif;
        Endif;
      /End-Free
    C*
     C     ck1end        endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
    C*
    C* PROCESS SUBSET SEARCHES
    C*
     C                   exsr      othfks
     C     *in90         cabeq     *on           ck2end
    C*
    C* CLEAR SUBSET DESCRIPTION FIELDS
    C*
     C                   exsr      sscler
    C*
    C* START OF ERROR CHECKING
    C*
     C                   if        ssfnd <> *zeros
     C                             and sssub# = *zeros
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        sssub# = *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        ssfnd = *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        ssfyr = *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        ssfein = *zeros
     C                             and ssmod <> *blanks
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck2end
     C                   endif
    C*
    C* validate subscriber number
    C*
     C                   if        sssub# <> *zeros
     C                   eval      kksub# = sssub#
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
     C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in75 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* validate group
    C*
     C                   if        ssfnd <> *zeros
     C                   eval      kkfnd = ssfnd
     C                   eval      kksub# = sssub#
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm                    #2err
    C*
     C                   if        #2err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in76 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* validate case
    C*
     C                   if        sscase <> *zeros
     C                   eval      kksub# = sssub#
     C                   eval      kkfnd = ssfnd
     C                   eval      kkfyr = ssfyr
     C                   eval      kkcase = sscase
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *on
     C                   eval      *in90 = *on
     C                   eval      *in77 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* validate provider
    C*
     C                   if        ssfein <> *zeros
     C                             and ssmod <> *blanks
     C                   eval      kkfein = ssfein
     C                   movel     ssmod         kkmod
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck2end
     C                   else
    C*
     C                   endif
     C                   endif
    C*
     C                   if        ssfein <> *zeros
     C                             and ssmod = *blanks
     C                   eval      kkfein = ssfein
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm                    #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck2end
     C                   else
    C*
     C                   endif
     C                   endif
    C*
    C* validate dates of service
    C*
     C                   if        ssfrdt <> *zeros
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssfrdt
     C                   if        srerr = '1'
     C                   eval      *in90 = *on
     C                   eval      *in79 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
     C                   if        sstodt <> *zeros
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sstodt
     C                   if        srerr = '1'
     C                   eval      *in90 = *on
     C                   eval      *in83 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* From Service Date cannot be greater than To Service Date
    C*
     C                   if        ssfrdt > *zeros
     C                             and sstodt > *zeros
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlfrdt
    C*
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hltodt
    C*
     C                   if        hlfrdt > hltodt
     C                   eval      *in90 = *on
     C                   eval      *in84 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   endif
    C*
    C*
    C* validate bill type
    C*
     C                   if        ssbtyp <> *blanks
     C                   movel(p)  ssbtyp        kkkey
     C                   movel     'BTYP'        kktbid
    C*
     C                   call      'FA728R'
     C                   parm                    ##co#
     C                   parm      kktbid        ##tbid
     C                   parm      kkkey         #1key
     C                   parm                    ##desc
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
     C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in89 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* validate initial user
    C*
     C                   if        ssiusr <> *blanks
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      ssiusr        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in81 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* CALL DATE FORMATTING SUBROUTINE
    C*
     C                   exsr      datsbr
    C*
     C     ck2end        endsr
    C*===============================================================
    C* GETGRP - SUBROUTINE TO GET GROUPS USER IS AUTHORIZED TO
    C*===============================================================
    C*
     C     getgrp        begsr
    C*
     C                   eval      x = 1
     C                   move      *off          grperr
    C*
     C     fnky3         chain     pfn02                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   call      'SA008R'      rstplf
    C*
     C                   if        ##rtcd = *off
     C                   move      fnfnd         grp(x)
     C                   eval      x = x + 1
     C                   else
     C                   move      *on           grperr
     C                   endif
    C*
     C     fnky3         reade     pfn02                                  60
    C*
     C                   enddo
    C*
     C                   if        grperr = *on
    C*
     C                   cat       fnd:1         qryslt
    C*
     C                   do        x             z
     C                   cat       grp(z):1      qryslt
     C                   enddo
    C*
     C                   cat       '))':0        qryslt
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================
    C* GETSUB - SUBROUTINE TO GET SUBSCRIBERS USER IS AUTHORIZED TO
    C*===============================================================
     C     getsub        begsr
    C*
     C     ##co#         chain     psm01                              60
    C*
     C                   move      *on           first
     C                   move      ##co#         xaco#
    C*
     C                   dow       *in60 = *off
    C*
     C                   call      'SA008R'      rstpls
    C*
     C                   if        ##rtcd = *off
    C*
     C                   eval      ##sub# = smsub#
     C                   eval      kksub# = smsub#
     C                   move      smsub#        xasub#
    C*
     C                   if        first = *on
     C                   move      *off          first
     C     co#           cat(p)    xaco#:1       qryslt
     C                   cat       co#2:0        qryslt
     C                   cat       sub#:0        qryslt
     C                   cat       xasub#:1      qryslt
     C                   cat       ')':0         qryslt
     C                   else
     C                   cat       '*OR':1       qryslt
     C                   cat       sub#:1        qryslt
     C                   cat       xasub#:1      qryslt
     C                   cat       ')':0         qryslt
     C                   endif
    C*
     C                   clear                   grp
    C*
     C                   if        ##mrgr = 'Y'
     C                   exsr      getgrp
     C                   endif
    C*
     C                   endif
    C*
     C     ##co#         reade     psm01                                  60
    C*
     C                   enddo
    C*
     C                   cat       ')':0         qryslt
    C*
    C*  CALL CL TO CREATE AN OPEN QUERY FILE OVER THE FILES
    C*
     C     ovrdb1        cat(p)    'FAEOL76':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*** Beg   Add *** 07/28/16 **************************************
      /free
       length = %len(%trim(xxovrd));
      /end-free
    C*** End   Add *** 07/28/16 **************************************
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
    C*** Beg   Chg *** 07/28/16 **************************************
     C****************   parm      31            length
     C                   parm                    length
    C*** End   Chg *** 07/28/16 **************************************
    C*
     C     ovrdb1        cat(p)    'FAEOL78':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*** Beg   Add *** 07/28/16 **************************************
      /free
       length = %len(%trim(xxovrd));
      /end-free
    C*** End   Add *** 07/28/16 **************************************
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
    C*** Beg   Chg *** 07/28/16 **************************************
     C*****************  parm      31            length
     C                   parm                    length
    C*** End   Chg *** 07/28/16 **************************************
    C*
     C     ovrdb1        cat(p)    'FAEOL79':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*** Beg   Add *** 07/28/16 **************************************
      /free
       length = %len(%trim(xxovrd));
      /end-free
    C*** End   Add *** 07/28/16 **************************************
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
    C*** Beg   Chg *** 07/28/16 **************************************
     C*****************  parm      31            length
     C                   parm                    length
    C*** End   Chg *** 07/28/16 **************************************
    C*
     C     ovrdb1        cat(p)    'FAEOL80':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*** Beg   Add *** 07/28/16 **************************************
      /free
       length = %len(%trim(xxovrd));
      /end-free
    C*** End   Add *** 07/28/16 **************************************
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
    C*** Beg   Chg *** 07/28/16 **************************************
     C****************   parm      32            length
     C                   parm                    length
    C*** End   Chg *** 07/28/16 **************************************
    C*
     C     ovrdb1        cat(p)    'FAEOL81':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*** Beg   Add *** 07/28/16 **************************************
      /free
       length = %len(%trim(xxovrd));
      /end-free
    C*** End   Add *** 07/28/16 **************************************
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
    C*** Beg   Chg *** 07/28/16 **************************************
     C****************** parm      44            length
     C                   parm                    length
    C*** End   Chg *** 07/28/16 **************************************
    C*
     C                   call      qrypgm        qrypl                  99
    C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   eval      #2sub# = 0
     C                   eval      #2fnd = 0
     C                   eval      #2fyr = 0
     C                   eval      #2case = 0
     C                   if        q1prms > 5
     C                             and @@sub# <> *zeros
     C                             and @@fnd <> *zeros
     C                             and @@fyr <> *zeros
     C                             and @@case <> *zeros
     C                   eval      sssub# = @@sub#
     C                   eval      ssfnd = @@fnd
     C                   eval      ssfyr = @@fyr
     C                   eval      sscase = @@case
    C*
    C* PROTECT THE SUBSET FIELDS FOR SUB#/FND/FYR/CASE
    C*
     C                   eval      *in50 = *on
    C*
    C* SET CASE PARMS FOR CALL TO ADJPGM WHEN SUBSETTED ON CASE
    C*
     C                   eval      #2sub# = sssub#
     C                   eval      #2fnd = ssfnd
     C                   eval      #2fyr = ssfyr
     C                   eval      #2case = sscase
     C                   else
    C*
     C                   eval      *in50 = *off
    C*
     C                   endif
    C*
    C* LOAD EOMB# AND PRE-CERT# INTO SUBSET FIELDS IF THE NUMBER OF
    C* PARMS BEING PASSED IS GREATER THAN 5.
    C*
     C                   if        q1prms > 5
    C*
    C* LOAD EOMB# INFORMATION INTO SSEOB# AND PROTECT IF NOT BLANK.
    C*
     C                   eval      sseob# = @@eob#
    C*
     C                   if        sseob# <> *zeros
    C*
     C                   eval      *in16 = *on
    C*
     C                   endif
    C*
    C* LOAD PRE-CERT INFORMATION INTO SSCERT AND PROTECT IF NOT BLANK.
    C*
     C                   if        q1prms > 6
    C*
     C                   movel(p)  @@cert        sscert
    C*
     C                   if        sscert <> *blanks
    C*
     C                   eval      *in17 = *on
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
     C                   if        q1prms >= 8
     C                   move      @@sfdt        ssfrdt
     C                   move      @@stdt        sstodt
     C                   move      @@sfei        ssfein
     C                   move      @@smod        ssmod
     C                   if        ssfein = *zeros
     C                             or ssmod = '000'
     C                   move      '   '         ssmod
     C                   endif
     C                   if        ssfrdt <> *zeros
     C                             or sstodt <> *zeros
     C                   eval      *in18 = *on
     C                   endif
     C                   if        ssfein <> *zeros
     C                             or ssmod <> *blanks
     C                   endif
     C                   endif
     C*
     C* Check restriction flags and subset and protect.
     C*
     C                   movel(p)  q1user        kkusr
     C     upky          chain     pup01                              99
     C                   if        *in99=*off
     C                   select
     C                   when      upmrgr = 'Y'
     C                             and ##rcds = *off
     C                             and upsub# <> *zeros
     C                             and upfnd <> *zeros
     C                             or *in55 = *off
     C                             and upmrgr = 'Y'
     C                             and upsub# <> *zeros
     C                             and upfnd <> *zeros
     C                   eval      sssub#=upsub#
     C                   eval      ssfnd=upfnd
     C                   seton                                        5152
     C                   when      upmrsu = 'Y'
     C                             and ##rcds = *off
     C                             and upsub# <> *zeros
     C                             or *in55 = *off
     C                             and upmrsu = 'Y'
     C                             and upsub# <> *zeros
     C                   eval      sssub#=upsub#
     C                   seton                                        51
     C                   endsl
     C                   endif
     C*
    C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
    C*
    C*** GET SYSTEM INFO
    C*
     C                   call      'SA001R'
     C                   parm      *blank        ##wcif
     C                   parm      *blank        ##mcif
     C                   parm      *blank        ##wcin
     C                   parm      *blank        ##ar
     C                   parm      *blank        ##ntwk
     C                   parm      *blank        ##ur
     C                   parm      *blank        ##med
     C                   parm      *blank        ##edi
     C                   parm      *blank        ##wcdk
     C                   parm      *blank        ##rhab
     C                   parm      *blank        ##mss
     C                   parm      *blank        ##fsch
     C                   parm      *blank        ##prvb
     C                   parm      *blank        ##npwc
     C                   parm      *blank        ##os
     C                   parm      *blank        ##arls
     C                   parm      *blank        ##ccod
     C                   parm      *blank        ##crvw
     C                   parm      *blank        ##prom
     C                   parm      *blank        ##mcrv
     C                   parm      *blank        ##freq
     C                   parm      *blank        ##crac
     C                   parm      *blank        ##preo
     C                   parm      *blank        ##parn
     C                   parm      *blank        ##mgri
    C*
     C                   eval      *in55 = *off
     C                   if        ##mgri = 'Y'
     C                   eval      *in55 = *on
     C                   endif
     C                   move      *loval        kklsn
     C                   move      *loval        kkfsn
     C                   move      *loval        kkeob#
     C                   move      *loval        kkfein
     C                   move      *loval        kkmod
     C                   move      *loval        kkss#
     C                   move      *loval        kkacdt
     C                   move      *loval        kkcntl
     C                   eval      kkfrdt = *hival
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
    C*
    C* VIEW# 1 (Last Name and First Name)
    C*
     C                   if        *in21 = *on
     C                   movel(p)  scpst1        kklsn
     C                   movel(p)  scpsta        kkfsn
     C                   endif
     C                   if        scpstf <> *zeros
     C                             and *in21 = *on
     C                   if        *in21 = *on
     C                   move      scpstf        srdti
     C                   endif
     C                   if        *in21 = *on
     C                   call      'SRCVTDAT'    dtcymd
     C                   endif
     C                   if        *in21 = *on
     C                   movel     srdto         kkfrdt
     C                   endif
     C                   else
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkfrdt
     C                   endif
     C                   if        *in21 = *on
     C                   eval      kkeob# = *zeros
     C                   endif
    C*
    C* VIEW# 2 (Provider FEIN and MOD)
    C*
     C                   if        *in22 = *on
     C                   movel     scpst2        kkfein
     C                   move      scpstb        kkmod
     C                   endif
    C*
    C* VIEW# 3 (SS# and DOI)
    C*
     C                   if        *in23 = *on
     C                   movel     scpst3        kkss#
     C                   endif
     C                   if        *in23 = *on
     C                   movel     scpstc        srdti
     C                   clear                   srdto
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkacdt
     C                   endif
    C*
    C* VIEW# 4 (EOMB#)
    C*
     C                   if        *in24 = *on
     C                   move      scpst4        kkeob#
     C                   endif
    C*
    C*
    C* VIEW# 5 (Control Number)
    C*
     C                   if        *in25 = *on
     C                   movel(p)  scpst5        kkcntl
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
    C*****
    C***21                MOVELSCLSN     KKLSN     P
    C***21                MOVELSCFSN     KKFSN     P
    C***21                MOVELHLPNAM    KKPNAM    P
    C***22                MOVELSCFEIN    KKFEIN
    C***22                MOVELSCMOD     KKMOD
    C***23                MOVELSCSS#     KKSS#     P
    C*****      *IN23     IFEQ *ON
    C*****                MOVELSCACDT    SRDTI     P
    C*****                MOVE *BLANKS   SRDTO
    C*****                MOVE *BLANKS   SRERR
    C*****                CALL 'SRCVTDAT'DTCYMD
    C*****                MOVELSRDTO     KKACDT
    C*****                ENDIF                           *IN23 = *ON
    C***24                Z-ADDSCEOB#    KKEOB#
    C*
     C                   movel(p)  sclsn         kklsn
     C                   movel(p)  scfsn         kkfsn
     C                   movel(p)  hlpnam        kkpnam
     C                   movel     scfein        kkfein
     C                   movel     scmod         kkmod
     C                   movel(p)  scss#         kkss#
    C*****                MOVE SCACDT    SRDTI     P
    C*****                MOVE *BLANKS   SRDTO
    C*****                MOVE *BLANKS   SRERR
    C*****                CALL 'SRCVTDAT'DTCYMD
    C*****                MOVELSRDTO     KKACDT
     C                   eval      kkacdt = hlsdoi
     C                   eval      kkeob# = sceob#
     C                   movel(p)  sccntl        kkcntl
     C                   eval      kkfrdt = scfrd
     C                   endsr
    C*================================================================
    C* KJDO - F10 Search Subroutine
    C*================================================================
     C     kjdo          begsr
    C*
    C* Indicators for cursor positioning
    C*
     C                   eval      *in30 = *off
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off
     C                   eval      *in33 = *off
     C                   eval      *in34 = *off
     C                   eval      *in35 = *off
     C                   eval      *in36 = *off
     C                   eval      *in37 = *off
     C                   eval      *in38 = *off
    C*
    C* Subscriber Search
    C*
     C                   if        hfld = 'SSSUB#'
     C                             and *in50=*off
     C                             and *in51=*off
     C                   eval      hlsub# = sssub#
     C                   movel     sssmnm        hlsmnm
     C                   eval      ##rrn = 0
     C                   call      'F0055R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
    C*
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                             or ##rrn = *zeros
     C                   eval      sssub# = hlsub#
     C                   movel     hlsmnm        sssmnm
     C                   eval      *in90 = *on
     C                   eval      *in30 = *on
     C                   else
    C**N99*****           Z-ADDSMSUB#    SSSUB#
    C**N99*****           MOVELSMNAME    SSSMNM    P
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm                    ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '1'           ##err
    C*
     C                   movel(p)  ##name        sssmnm
     C                   eval      sssub# = ##sub#
     C                   eval      *in90 = *on
     C                   eval      *in31 = *on
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
    C* Group Search
    C*
     C                   if        hfld = 'SSFND'
     C                             and *in50=*off
     C                             and *in52=*off
     C                   eval      ##rrn = 0
     C                   eval      hlfnd = ssfnd
     C                   movel     ssfnnm        hlfnnm
     C                   call      'F0060R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
    C*
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                             or ##rrn = *zeros
     C                   eval      ssfnd = hlfnd
     C                   movel     hlfnnm        ssfnnm
     C                   eval      *in90 = *on
     C                   eval      *in31 = *on
     C                   else
    C**N99*****           Z-ADDFNFND     SSFND
    C**N99*****           MOVELFNNAME    SSFNNM    P
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm                    #2sub#
     C                   parm                    #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm      '1'           #2err
    C*
     C                   eval      ssfnd = #2fnd
     C                   movel(p)  #2name        ssfnnm
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
    C* Fund Year/Case Search
    C*
     C                   if        hfld = 'SSFYR'
     C                             or hfld = 'SSCASE'
     C                   eval      hlfyr = ssfyr
     C                   eval      hlcase = sscase
     C                   call      'F0269R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##sub#
     C                   parm      *zeros        ##fnd
     C                   parm      *zeros        ##fyr
     C                   parm      *zeros        ##case
     C                   parm      *blanks       ##ss#
     C                   parm      *zeros        ##acdt
     C                   parm      ' '           ##rtcd
    C*
     C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fyr = *zeros
     C                             or ##case = *zeros
     C                   eval      ssfyr = hlfyr
     C                   eval      sscase = hlcase
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   else
     C                   eval      ssfyr = ##fyr
     C                   eval      sscase = ##case
     C                   eval      sssub# = ##sub#
     C                   eval      ssfnd = ##fnd
     C                   eval      kksub# = ##sub#
     C                   eval      kkfnd = ##fnd
     C                   eval      kkfyr = ##fyr
     C                   eval      kkcase = ##case
    C*
    C* get claimaint name
    C*
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   movel(p)  cmlsn         ##last
     C                   movel(p)  cmfsn         ##frst
     C                   move      '1'           ##term
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm                    ##term
     C                   movel(p)  ##last        ssname
     C                   endif
    C*
    C* get subscriber name
    C*
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
     C                   movel(p)  ##name        sssmnm
    C*
    C* get group name
    C*
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm                    #2err
    C*
     C                   movel(p)  #2name        ssfnnm
    C*
    C*
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
     C                   endif
     C                   exsr      datsbr
     C                   endif
    C*
    C* Provider Search
    C*
     C                   if        hfld = 'SSFEIN'
     C                             or hfld = 'SSMOD'
     C                   move      ssfein        hlfein
     C                   move      ssmod         hlmod
     C                   call      'F0032R'
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##fein
     C                   parm      *blanks       ##mod
     C                   parm      *blanks       ##rtcd
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fein = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
     C                   else
     C                   move      ##fein        ssfein
     C                   move      ##mod         ssmod
     C                   eval      *in90 = *on
     C                   eval      *in34 = *on
    C*
     C                   eval      kkfein = ssfein
     C                   movel     ssmod         kkmod
    C**N99*****           MOVELPDNAME    SSPDNM 20 P
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err <> '0'
     C                   movel(p)  #1name        sspdnm
     C                   endif
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
     C                   if        hfld = 'SSBTYP'
    C*
     C                   call      'F2380R'
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##kc
     C                   parm      *blanks       ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      'BTYP'        ##tbid
    C*
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                             or ##rrn = *zeros
     C                   eval      *in90 = *on
     C                   eval      *in37 = *on
     C                   else
     C                   movel     hlbsds        ssbsds
     C                   eval      *in90 = *on
     C                   eval      *in36 = *on
    C*
     C                   call      'FA728R'
     C                   parm                    ##co#
     C                   parm                    ##tbid
     C                   parm      *blanks       #1key
     C                   parm      *blanks       ##desc
     C                   parm                    ##rrn
     C                   parm      '1'           ##err
    C*
     C                   movel     #1key         ssbtyp
     C                   movel     ##desc        scbtds
    C*
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
    C* Initial User Search
    C*
     C                   if        hfld = 'SSIUSR'
     C                   movel(p)  ssiusr        hliusr
     C                   movel(p)  ssusnm        hlusnm
     C                   call      'S0005R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm      *zeros        ##view
     C                   if        ##rrn = *zeros
     C                   movel(p)  hliusr        ssiusr
     C                   movel(p)  hlusnm        ssusnm
     C                   eval      *in90 = *on
     C                   eval      *in36 = *on
     C                   else
    C**N99*****           MOVELUPUSR     SSIUSR    P
    C**N99*****           MOVELUPUSNM    SSUSNM    P
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      ##rrn         ##rrn
     C                   parm      *blanks       ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
     C                   movel(p)  ##user        ssiusr
     C                   movel(p)  ##usnm        ssusnm
     C                   else
     C                   clear                   ssiusr
     C                   clear                   ssusnm
     C                   endif
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*================================================================
    C* KNDO - DARTS Subroutine
    C*================================================================
     C     kndo          begsr
     C                   endsr
    C*================================================================
    C* OPT6 - Detail Charge List
    C*================================================================
     C     opt6          begsr
     C                   move      scacdt        x1dt
     C                   move      x1dtm         x2dtm
     C                   move      x1dtd         x2dtd
     C                   move      x1dty         x2dty
     C                   move      x2dt          srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         qqacdt
     C                   move      scmod         qqmod
     C                   call      'F9951R'
     C                   parm                    scprds
     C                   parm      ' '           ##call
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    scss#
     C                   parm                    qqacdt
     C                   parm                    scfein
     C                   parm                    qqmod
     C                   parm      'Y'           qqmode
     C                   endsr
    C*================================================================
    C* OPT7 - Work with Detail
    C*================================================================
     C     opt7          begsr
    C*
     C                   move      'D'           ##mode
    C*
     C                   exsr      ovride
     C                   move      ' '           ##kl
    C*
     C                   call      'FA730R'
     C                   parm      scast         ##ast
     C                   parm      scsys         ##sys
     C                   parm      scbtyp        ##btyp
     C                   parm                    ##depg
    C*
     C                   move(p)   ##depg        ##pgm
    C*
     C                   clear                   ##pgm2
    C*
     C                   call      'F0390R'
     C                   parm                    ##mode
     C                   parm                    ##kl
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##pgm
     C                   parm                    ##pgm2
     C                   exsr      dlovrd
     C                   endsr
    C*================================================================
    C* OPT8 - Work with Amounts
    C*================================================================
     C     opt8          begsr
     C                   call      'F0396R'
     C                   parm      ' '           ##call
     C                   parm                    scdbrr
     C                   endsr
    C*================================================================
    C* OPT10 - Work with Bill Status History
    C*================================================================
     C     opt10         begsr
    C*
     C                   exsr      ovride
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   move      sceob#        kkeob#
    C*
     C     esky          chain     pes01                              99
     C                   if        *in99 = *off
     C                   eval      ##rrn = dbrrna
     C                   endif
     C                   call      'F0395R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm      scdbrr        ##rrn2
     C                   exsr      dlovrd
     C                   endsr
    C*================================================================
    C* OPT11 - Work with EOB Notes
    C*================================================================
     C     opt11         begsr
    C*
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   move      sceob#        kkeob#
    C*
     C     esky          chain     pes01                              99
     C                   if        *in99 = *off
     C                   eval      ##rrn = dbrrna
     C                   endif
     C                   call      'F0355R'
     C                   parm      '1'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm      scdbrr        ##rrn2
     C                   endsr
    C*================================================================
    C* OPT12 - Print EOMB
    C*================================================================
     C     opt12         begsr
    C*
     C                   move      scdbrr        #1rrn
     C                   call      'SA012R'
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##outq
     C                   parm      *blanks       ##rtcd
    C*
     C                   if        ##rtcd = *blanks
     C                   movel(p)  ##outq        ##que
     C                   endif
     C                   call      'F0340C'
     C                   parm                    #1rrn
     C                   parm                    scast
     C                   parm      'P'           #1mode
     C                   parm                    ##que
    C*
     C                   endsr
    C*================================================================
    C* OPT13 - Display EOMB
    C*================================================================
     C     opt13         begsr
    C*
     C                   clear                   ##que
     C                   move      scdbrr        #1rrn
     C                   call      'F0340C'
     C                   parm                    #1rrn
     C                   parm                    scast
     C                   parm      'D'           #1mode
     C                   parm                    ##que
    C*
    C*
     C                   endsr
    C*================================================================
    C* OPT14 - DARTS
    C*================================================================
     C     opt14         begsr
    C*
     C                   move(p)   ##co#         #1co#
     C                   move(p)   scsub#        #1sub#
     C                   move(p)   scfnd         #1fnd
     C                   move(p)   scfyr         #1fyr
     C                   move(p)   sccase        #1case
    C*
     C                   call      'FA703R'      drtspl
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*================================================================
    C* OPT15 - Pre-Cert Info
    C*================================================================
     C     opt15         begsr
    C*
    C*
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   movel     sccert        kkcert
     C                   eval      kkext = scext
    C*
     C                   call      'FA732R'
     C                   parm      ##co#         ##co#
     C                   parm      kksub#        ##sub#
     C                   parm      kkfnd         ##fnd
     C                   parm      kkfyr         ##fyr
     C                   parm      kkcase        ##case
     C                   parm      kkcert        ##cert
     C                   parm      kkext         #1ext
     C                   parm                    ##rrn
    C*
    C*
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
     C                   call      'U0060R01'
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm      'D'           ##mode
     C                   parm                    ##rrn
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    ##cert
     C                   parm                    ##ext
     C                   parm      ' '           ##ocst
     C                   parm      'Y'           ##cont
     C                   endsr
    C*================================================================
    C* OPT16 - History
    C*================================================================
     C     opt16         begsr
    C*
    C*  Call new history pgm F0391R
    C*
     C                   call      'F0391R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    scfyr
     C                   parm                    sccase
     C                   parm                    sceob#
     C                   endsr
    C*================================================================
    C* OPT20 - Option 20 (Pended Lines)
    C*================================================================
     C     opt20         begsr
    C*
    C*  Call new pended lines pgm F0398R
    C*
     C                   call      'F0398R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    scfyr
     C                   parm                    sccase
     C                   parm                    sceob#
     C                   endsr
    C*================================================================
    C* OPT22 - View Prov Info
    C*================================================================
     C     opt22         begsr
    C*
     C                   movel     scfein        zzfein
     C                   movel     scmod         zzmod
    C*
     C                   call      'F0032R'
     C                   parm      '1'           ##call
     C                   parm      zzfein        ##fein
     C                   parm      zzmod         ##mod
     C                   parm                    ##rtcd
     C                   parm                    ##type
     C                   parm                    ##1n4
     C                   parm      'Y'           ##1prv
     C                   eval      *in90 = *on
     C                   endsr
    C*================================================================
    C* OPT30 - Letters
    C*================================================================
     C     opt30         begsr
    C*
     C                   call      'F0399R'
     C                   parm      @@call        ##call
     C     *inkc         parm      *off          ##kc
     C     *inkl         parm      *off          ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      scdbrr        ##rrn2
     C                   parm      *zeros        ##rrn3
    C*
     C                   endsr
    C*================================================================
    C* OPT81 - Claim info
    C*================================================================
     C     opt81         begsr
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
     C                   call      'FA701R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       ##outs
     C                   if        ##rtcd = *blank
     C                             and ##outs <> 'Y'
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   if        ##rtcd = *blank
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
     C                   call      'WA004R'
     C                   parm                    #1co#
     C                   parm      #1wcf#        #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      *zeros        #2rrn
     C                   if        #2rrn <> 0
     C                   call      'W0070R'
     C                   parm      '1'           ##call
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'M'           ##mode
     C                   parm                    #2rrn
     C                   parm      *blanks       ##sort
     C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   else
     C                   eval      *in85 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endsr
    C*================================================================
    C* OPT82 - Case info
    C*================================================================
     C     opt82         begsr
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   call      'F0260R'
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'L'           ##mode
     C                   parm      dbrcm1        ##rrn
     C                   parm      *blanks       ##ss#
     C                   parm      *zeros        ##acdt
     C                   endif
     C                   endsr
    C*================================================================
    C* OPT99 - Soft Delete of EOMB
    C*================================================================
     C     opt99         begsr
    C*
     C                   eval      scsf1r = sfrrn1
     C                   add       1             sfrrn2               14
     C                   write     scr02s
     C                   eval      *in08 = *on
     C                   eval      scrrn2 = 1
     C                   write     scr02
     C                   exfmt     scr02c
     C                   if        *inkl = *off
     C                   move      'G'           ##mode
    C*
    C*** CALL BILL STATUS HISTORY
    C*
     C                   eval      ##rrn = 0
     C                   eval      ##rrn2 = scdbrr
     C                   clear                   ##stcd
     C                   clear                   ##reas
     C                   movel(p)  q1pgm         #1pgm
     C                   call      'F0395R01'
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm      'A'           #1mode
     C                   parm                    ##rrn
     C                   parm                    ##rrn2
     C                   parm      'D'           ##blst
     C                   parm                    ##stcd
     C                   parm                    ##reas
     C                   parm                    #1pgm
     C                   if        ##kl = *off
     C                             and ##kc = *off
     C                   call      acdpgm        acdpl
     C                   endif
     C                   endif
    C*
     C                   eval      *in14 = *off
     C                   endsr
    C*================================================================
    C* OVRIDE - Overide files based  on adjudication state
    C*================================================================
     C     ovride        begsr
     C                   eval      length = 60
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(2)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(3)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
    C*
     C                   movel(p)  qca(4)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(5)        qqcmd2
     C                   movel(p)  scast         qqst2
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd2
     C                   parm                    length
     C                   movel(p)  qca(6)        qqcmd2
     C                   movel(p)  scast         qqst2
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd2
     C                   parm                    length
     C                   movel(p)  qca(7)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C*
     C                   endsr
    C*================================================================
    C* MVDB04 - Move data base fields to SCR04
    C*================================================================
     C     mvdb04        begsr
    C*
    C** Get standard screen headings
    C*
     C     scdbrr        chain     pfrcdf                             99
    C*
     C                   movel     eospcd        scspcd
     C                   movel     eontwk        scnwcd
     C                   movel     eoflic        scsvzp
     C*
     C                   setoff                                       69
     C                   if        scnwcd <> *blanks
     C                   seton                                        69
     C                   endif
    C*
     C                   movel     *hival        kkidt
     C                   movel     *hival        kkitim
     C     esky2         setgt     pes01
     C     esky          readpe    pes01                                  99
     C                   if        *in99 = *off
     C                   eval      screas = esreas
     C                   else
     C                   eval      screas = *blanks
     C                   endif
    C*
     C                   move      eoco#         #1co#
     C                   move      eosub#        #1sub#
     C                   move      eofnd         #1fnd
     C                   move      eofyr         #1fyr
     C                   move      eocase        #1case
     C                   clear                   ##rtcd
     C                   clear                   #1l1
     C                   clear                   #1l2
    C*
     C                   call      'FA702R'
     C                   parm                    ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    #1l1
     C                   parm                    #1l2
    C*
     C                   movel     #1l1          schd1
     C                   movel     #1l2          schd2
    C*
     C                   clear                   ##rtcd
     C                   clear                   #1l1
    C*
     C                   call      'FA707R'
     C                   parm                    ##rtcd
     C                   parm      scdbrr        @@rrn2
     C                   parm                    #1l1
    C*
     C                   movel     #1l1          schd3
    C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   if        vldopt = 'N'
    C*
     C                   exsr      assrrn
    C*
    C*
     C                   endif
    C*
     C                   movel     eocrpe        sccrpe
     C                   movel     eocntl        sccntl
     C                   eval      scfrd = eofrdt
     C                   movel     eoast         scast
     C                   movel     eosys         scsys
     C                   movel     eobtyp        scbtyp
     C                   movel(p)  eolsn         sclsn
     C                   movel(p)  eofsn         scfsn
     C                   movel(p)  sclsn         ##last
     C                   movel(p)  scfsn         ##frst
     C                   move      '1'           ##term
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm                    ##term
     C                   movel     ##last        scname
    C*
     C                   move      eoeob#        sceob#
     C                   eval      scrcn = eorcn
     C                   movel(p)  eoeob#        qqnum
     C                   clear                   qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         aleob#
    C*
     C                   eval      kkfein = eofein
     C                   eval      kkmod = eomod
    C**N99*****           MOVELPDNAME    SCPNAM    P
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err <> '0'
     C                   movel(p)  #1name        scpnam
     C                   endif
    C*
    C** 'HLPNAM' FIELD IS USED TO HOLD ENTIRE PROVIDER NAME FOR
    C**  USE IN KKPNAM KEY FIELD FOR PAGING (KEYSFL SUBROUTINE)
    C*
     C                   movel     #1name        hlpnam
    C*
     C                   eval      srdti = eofrdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         scfrdt
    C*
     C                   eval      srdti = eotodt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         sctodt
    C*
     C                   eval      srdti = eoacdt
     C                   eval      hlsdoi = eoacdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel     srdto         shacdt
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         scacdt
    C*
     C                   if        eoblst = 'D'
     C                   movel(p)  'DEL'         scblst
     C                   else
     C                   movel(p)  eoblst        scblst
     C                   endif
    C*
      /Free
       If xxclt# = '114' or xxclt# = '129';
          sciDoc = EOiDoc;
       EndIf;
      /End-Free
     C                   move      eofein        scfein
     C                   move      scfein        alfein
     C                   move      eomod         scmod
     C                   move      scmod         almod
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                movel(p)  eoss#         scss#
      /Free
       scSS# = MaskData('SSN':'eoss#':eoss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
    C*
     C                   movel(p)  eochrg        qqnum
     C                   movel(p)  'X,XXX.XX'    qqmask
     C                   call      'SA020R'
     C                   parm                    qqnum
     C                   parm                    qqmask
     C                   movel     qqnum         scchrg
     C                   move      scchrg        alchrg
    C*
     C                   movel(p)  eopaid        qqnum
     C                   movel(p)  'X,XXX.XX'    qqmask
     C                   call      'SA020R'
     C                   parm                    qqnum
     C                   parm                    qqmask
     C                   movel     qqnum         scpaid
     C                   move      scpaid        alpaid
    C*
     C                   eval      scsub# = eosub#
     C                   eval      scfnd = eofnd
     C                   eval      scfyr = eofyr
     C                   eval      sccase = eocase
    C*
     C                   clear                   scdata
    C*
     C                   select
    C*  I had to change the HD#s to accomidate the new Control
    C*  Number view.
    C*
    C*** LOAD VIEW# 1 FIELDS
    C*
     C                   when      hd# = 1
     C                             or hd# = 6
     C                             or hd# = 11
     C                             or hd# = 16
     C                   eval      y = 1
     C                   movea     scname        dta1(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta1(y)
     C                   eval      y = y + 11
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta1(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta1(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta1(y)
     C                   movea(p)  dta1          scdata
    C*
    C*** LOAD VIEW# 2 FIELDS
    C*
     C                   when      hd# = 2
     C                             or hd# = 7
     C                             or hd# = 12
     C                             or hd# = 17
     C                   eval      y = 1
     C                   movea     alfein        dta2(y)
     C                   eval      y = y + 10
     C                   movea     almod         dta2(y)
     C                   eval      y = y + 5
     C                   movea     scname        dta2(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta2(y)
     C                   movea(p)  dta2          scdata
    C*
    C*** LOAD VIEW# 3 FIELDS
    C*
     C                   when      hd# = 3
     C                             or hd# = 8
     C                             or hd# = 13
     C                             or hd# = 18
     C                   eval      y = 1
     C                   movea     scss#         dta3(y)
     C                   eval      y = y + 11
     C                   movea     scacdt        dta3(y)
     C                   eval      y = y + 11
     C                   movea     aleob#        dta3(y)
     C                   eval      y = y + 10
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta3(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta3(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta3(y)
     C                   movea(p)  dta3          scdata
    C*
    C*** LOAD VIEW# 4 FIELDS
    C*
     C                   when      hd# = 4
     C                             or hd# = 9
     C                             or hd# = 14
     C                             or hd# = 19
     C                   eval      y = 1
     C                   movea     aleob#        dta4(y)
     C                   eval      y = y + 11
     C                   movea     scname        dta4(y)
     C                   eval      y = y + 21
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta4(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta4(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta4(y)
     C                   movea(p)  dta4          scdata
    C*
    C*** LOAD VIEW# 5 FIELDS
    C*
     C                   when      hd# = 5
     C                             or hd# = 10
     C                             or hd# = 15
     C                             or hd# = 20
     C                   eval      y = 1
     C                   movea     eocntl        dta5(y)
     C                   eval      y = y + 15
     C                   movea     scname        dta5(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta5(y)
     C                   movea(p)  dta5          scdata
    C*
     C                   endsl
    C*
     C                   select
    C*
    C* LOAD INFO #1
    C*
     C                   when      info# = 1
    C*****                Z-ADD1         X       20
     C                   eval      x = 2
     C                   movea     scfrdt        inf1(x)
     C                   eval      x = x + 12
     C                   movea     sctodt        inf1(x)
    C*****                ADD  14        X
     C                   eval      x = x + 13
     C                   movea     scblst        inf1(x)
     C                   movea     inf1          hlinf1
     C                   move      hlinf1        scdata
    C*
    C* LOAD INFO #2
    C*
     C                   when      info# = 2
     C                   eval      x = 1
     C                   movea     alchrg        inf2(x)
     C                   eval      x = 12
     C                   movea     alpaid        inf2(x)
     C                   eval      x = x + 12
     C                   movea     scbtyp        inf2(x)
     C                   eval      x = x + 3
     C                   movea     eoast         inf2(x)
     C                   movea     inf2          hlinf2
     C                   move      hlinf2        scdata
    C*
    C* LOAD INFO #3
    C*
     C                   when      info# = 3
     C                   eval      x = 2
     C                   movea     eocntl        inf3(x)
     C                   eval      x = x + 20
     C                   movea     scblst        inf3(x)
     C                   movea     inf3          hlinf3
     C                   move      hlinf3        scdata
    C*
    C* LOAD INFO #4
    C*
     C                   when      info# = 4
     C                   eval      x = 2
     C                   movea     eopnam        inf4(x)
     C                   eval      x = x + 24
     C                   movea     ' '           inf4(x)
     C                   eval      x = x + 1
     C                   movea     scblst        inf4(x)
     C                   movea     inf4          hlinf4
     C                   move      hlinf4        scdata
    C*
    C*
     C                   endsl
    C*
     C                   if        info# = 1
     C                   movel     heada         schdla
     C                   else
     C                   clear                   schdla
     C                   endif
    C*
    C* THE FOLLOWING ARE HIDDEN FIELDS THAT ARE NOT DISPLAYED BUT
    C* USED IN THIS PROGARM
    C*
     C                   movel(p)  eocert        sccert
     C                   eval      scext = eoext
    C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
    C*
     C                   eval      *in70 = *off
    C*
     C     *inkj         caseq     *on           kjdo
     C     *inkn         caseq     *on           kndo
     C                   endcs
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
    C*
     C                   if        *in55 = *on
    C*
     C                   move      @@call        svcall
     C                   move      *in09         svin09
    C*
     C                   if        scauth = 'I'
     C                   move      *on           *in09
     C                   move      '1'           @@call
     C                   move      '1'           ##call
     C                   endif
     C                   endif
     C     scsel         caseq     '6'           opt6
     C     scsel         caseq     '7'           opt7
     C     scsel         caseq     '8'           opt8
     C     scsel         caseq     '10'          opt10
     C     scsel         caseq     '11'          opt11
     C     scsel         caseq     '12'          opt12
     C     scsel         caseq     '13'          opt13
     C     scsel         caseq     '14'          opt14
     C     scsel         caseq     '15'          opt15
     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '22'          opt22
     C     scsel         caseq     '16'          opt16
     C     scsel         caseq     '30'          opt30
     C     scsel         caseq     '81'          opt81
     C     scsel         caseq     '82'          opt82
     C     scsel         caseq     '99'          opt99
     C                   endcs
     C                   if        *in55 = *on
     C                   move      svcall        @@call
     C                   move      svin09        *in09
     C                   endif
    C*
    C*                    MOVEL'Y'       VLDOPT
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   close     FAEOL76
     C                   close     FAEOL78
     C                   close     FAEOL79
     C                   close     FAEOL80
     C                   close     FAEOL81
     C                   if        qqflag = '0'
    C* Call CL to release the lock on Data area FDAILYA
     C                   call      'F0350C03'
     C                   endif
     C                   if        stop = *on
     C                   move      *off          stop
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
    C* Call CL to place a lock on Data area FDAILYA
     C                   call      'F0350C02'
     C                   parm      '0'           qqflag
    C*
     C                   if        qqflag = '1'
     C                   move      *on           *inlr
     C                   return
     C                   endif
    C*
     C                   eval      *in12 = *on
     C                   move      *off          stop
    C*
     C                   movel(p)  q1user        scuser
    C*
     C     scuser        chain(n)  pup01                              99
    C*
    C* Check for active user authorization
    C*
     C                   if        *in55 = *on
     C                   call      'SA008R'      rstpl                  99
     C                   move      upmrsu        ##mrsu
     C                   move      upmrgr        ##mrgr
     C                   move      upmrem        ##mrem
    C*
     C                   if        ##rcds = *on
     C                             and ##mrsu = 'Y'
     C                   exsr      getsub
     C                   endif
     C                   endif
    C*
     C                   open      FAEOL76
     C                   open      FAEOL78
     C                   open      FAEOL79
     C                   open      FAEOL80
     C                   open      FAEOL81
    C*
    C* CALL API FOR SYSTEM FLAGS
    C*
     C                   call      'SA001R'
     C                   parm      ' '           ##wcif
     C                   parm      ' '           ##mcif
     C                   parm      ' '           ##wcin
     C                   parm      ' '           ##ar
     C                   parm      ' '           ##ntwk
     C                   parm      ' '           ##ur
     C                   parm      ' '           ##med
     C                   parm      ' '           ##edi
     C                   parm      ' '           ##wcdk
     C                   parm      ' '           ##rhab
     C                   parm                    ##mss
     C                   parm                    ##fsch
     C                   parm                    ##prvb
     C                   parm                    ##npwc
    C*
    C* Determine if OPT15=Pre-Cert Info should be displayed
    C*
     C                   if        ##ur = 'Y'
     C                   movel     pciopt        ofd(15)
     C                   else
     C                   movel     clropt        ofd(15)
     C                   endif
    C*
    C* Determine if OPT81=Claim Info should be displayed
    C*
     C                   if        ##wcif = 'Y'
     C                             and ##npwc <> 'Y'
     C                   movel     clmopt        ofd(23)
     C                   else
     C                   movel     clropt        ofd(23)
     C                   endif
    C*
    C*
    C*
    C** GET INITIAL VIEW FROM CONTROL FILE SFBPF (THEIR ARE 4 POSSIBLE VIEWS)
    C*
     C                   call      'SA009R'
 1   C                   parm      *zeros        ##eobd
 2   C                   parm      ' '           ##eobi
 3   C                   parm      ' '           ##arin
 4   C                   parm      ' '           ##acis
 5   C                   parm      ' '           ##pay0
 6   C                   parm      ' '           ##sdts
 7   C                   parm      ' '           ##mciv
 8   C                   parm      ' '           ##pbiv
     C                   if        ##pbiv = '1'
     C                             or ##pbiv = '2'
     C                             or ##pbiv = '3'
     C                             or ##pbiv = '4'
     C                             or ##pbiv = '5'
     C                   move      ##pbiv        view#
     C                   else
     C                   eval      view# = 1
     C                   endif
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C*
     C                   EVAL      XXCALL =  @@CALL
     C                   EVAL      @@CALL =  *BLANKS
     C                   IF        XXCLT# = '146' or xxclt# = '114'
     C*
     C* Storing value of call param into XXCALL and blanking out as for original Pended w/w
     C*
     C                   SELECT
     C                   WHEN      XXCALL = 'P'                                         PROCURA
     C                   EVAL      OPTNM = 'Pended Procura Bills'
     C                   WHEN      XXCALL = 'K'                                         KARVY
     C                   EVAL      OPTNM = 'Pended Karvy Bills'
     C                   WHEN      XXCALL = 'R'                                         Processed
     C                   EVAL      OPTNM = 'Processed Procura Bills'
     C                   OTHER
     C***** Beg Chg *** 09/14/15 *******************************************
     C                   EVAL      OPTNM = 'Hospital Bills Returned to PPO'
     C*****              EVAL      OPTNM = 'Pended Provider Bills'
     C***** End Chg *** 09/14/15 *******************************************
     C                   ENDSL
     C*
     C                   ELSE                                                           CLT <> 146
     C                   EVAL      OPTNM = 'Pended Provider Bills'
     C                   ENDIF
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
     C                   move      'N'           vldopt
    C*
     C                   if        *in21 = *on
     C     key           reade     dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key           reade     dbrcd2                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key           reade     dbrcd3                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key           reade     dbrcd4                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key           reade     dbrcd5                                 95
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
     C                   move      'N'           vldopt
    C*
     C                   select
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in24 = *on
     C     key35         readpe    dbrcd4                                 95
     C                   endif
    C*
     C                   other
     C                   if        *in21 = *on
     C     key           readpe    dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key           readpe    dbrcd2                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key           readpe    dbrcd3                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key           readpe    dbrcd4                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key           readpe    dbrcd5                                 95
     C                   endif
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
    C*
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setgt     dbrcd2                             96
     C                   endif
     C                   if        *in23 = *on
     C     key3          setgt     dbrcd3                             96
     C                   endif
     C                   if        *in24 = *on
     C     key4          setgt     dbrcd4                             96
     C                   endif
     C                   if        *in25 = *on
     C     key49         setgt     dbrcd5                             96
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
    C*
     C                   select
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in24 = *on
     C     key35         setll     dbrcd4                             96
     C                   endif
    C*
     C                   other
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setll     dbrcd2                             96
     C                   endif
     C                   if        *in23 = *on
     C     key3          setll     dbrcd3                             96
     C                   endif
     C                   if        *in24 = *on
     C     key4          setll     dbrcd4                             96
     C                   endif
     C                   if        *in25 = *on
     C     key49         setll     dbrcd5                             96
     C                   endif
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
    C*
     C                   eval      @@eob# = sceob#
    C*
     C                   if        q1prms > 11
    C*
     C                   eval      @@rcn = scrcn
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SSCLER - Clear subset description fields
    C*================================================================
     C     sscler        begsr
    C*
    C* CLEAR ALL SUBSET DESCRIPTIONS IF CORRESPONDING SUBSET FIELD
    C* IS EMPTY
    C*
     C                   if        sssub# = *zeros
     C                   clear                   sssmnm
     C                   endif
    C*
     C                   if        ssfnd = *zeros
     C                   clear                   ssfnnm
     C                   endif
    C*
     C                   if        ssfyr = *zeros
     C                             and sscase = *zeros
     C                   clear                   ssname
     C                   endif
    C*
     C                   if        ssfein = *zeros
     C                             and ssmod = *blanks
     C                   clear                   sspdnm
     C                   endif
    C*
     C                   if        ssiusr = *blanks
     C                   clear                   ssusnm
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C*** convert MC GRP# to WC FND#
     C                   move      eoco#         #1co#
     C                   move      eofnd         #1fnd
     C                   move      eosub#        #1sub#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C*** Check claim authority
     C                   move      #1wcf#        #1fnd
     C                   move      eofyr         #1fyr
     C                   move      eocase        #1case
     C                   move      *blanks       #1rtcd
     C*
     C                   call      'WA067R'      cuspl
     C* not authorised when rtcd = '2'
     C                   if        #1rtcd = '2'
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        sssub# <> *zeros
     C                             and sssub# <> eosub#
     C                             or sssub# <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssfnd <> *zeros
     C                             and ssfnd <> eofnd
     C                             or ssfnd  <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssfyr <> *zeros
     C                             and ssfyr <> eofyr
     C                             or ssfyr  <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        sscase <> *zeros
     C                             and sscase <> eocase
     C                             or sscase <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssfein <> *zeros
     C                             and ssfein <> eofein
     C                             or ssfein <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   movel     ssmod         xxmod
     C                   if        ssmod <> *blanks
     C                             and xxmod <> eomod
     C                             or ssmod  <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        sseob# <> *zeros
     C                             and sseob# <> eoeob#
     C                             or sseob# <> *zeros
     C                             and eoblst <> 'P'
    C*
     C                   movel(p)  *off          meetss
    C*
     C                   endif
    C*
     C                   if        sscert <> *blanks
     C                             and sscert <> eocert
     C                             or sscert <> *blanks
     C                             and eoblst <> 'P'
    C*
     C                   movel(p)  *off          meetss
    C*
     C                   endif
    C*
    C*
    C* SUBSET DATE OF SERVICE SHOULD FALL WITHIN EOMB SERVICE DATE
    C* RANGE (EOFRDT - EOTODT)
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlfrdt
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hltodt
    C*
    C* SUBSET CRITERIA WHEN ONLY FROM DATE IS ENTERED
    C*
     C                   if        hlfrdt > *zeros
     C                             and hltodt = *zeros
     C                   if        eofrdt < hlfrdt
     C                             and eotodt < hlfrdt
     C                             or  eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
    C* SUBSET CRITERIA WHEN RANGE IS SPECIFIED
    C*
     C                   if        hlfrdt > *zeros
     C                             and hltodt > *zeros
    C*
     C                   if        eotodt < hlfrdt
     C                             or eofrdt > hltodt
     C                             or  eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   endif
    C*
     C                   if        ssbtyp <> *blanks
     C                             and ssbtyp <> eobtyp
     C                             or ssbtyp <> *blanks
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssiusr <> *blanks
     C                             and ssiusr <> eoiusr
     C                             or ssiusr <> *blanks
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
     C                   if        *in55 = *on
    C*
     C                   move      'A'           scauth
    C*
    C* Check Subscriber Restriction
    C*
     C                   if        ##mrsu = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
     C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      *zero         $$fnd
     C                   parm      *zero         #2emp#
     C                   parm      eosub#        $$sub#
    C*
     C                   if        ##rtcd = *on
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
    C*  Check Group restriction
    C*
     C                   if        ##mrgr = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
    C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      eofnd         $$fnd
     C                   parm      *zero         #2emp#
     C                   parm      eosub#        $$sub#
    C*
     C                   if        ##rtcd = *on
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
    C*  Check for Employer Restriction
    C*
     C                   if        ##mrgr = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
     C*
     C     cmky1         chain     pcm01                              99
     C                   if        *in99 = *off
     C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      cmfnd         $$fnd
     C                   parm      cmemp#        #2emp#
     C                   parm      cmsub#        $$sub#
    C*
     C                   if        ##rtcd = *on
     C                   move      *off          meetss
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C* Differentiate between procura and Karvey bills
     C*
     C******             if        xxclt# = '146' or xxclt# = '114'
     C******             if        xxcall ='K' and eombup <> 'K'
     C******             move      *off          meetss
     C******             endif
     C*
     C******             if        xxcall ='P' and eombup <> 'P'
     C******             move      *off          meetss
     C******             endif
     C*
     C*****eaky1         chain     pea01                              99
     C******             if        *IN99 = *OFF
     C******             if        xxcall = 'R' and Eansnt <> 'R'
     C******             move      *off          meetss
     C******             Endif
     C******             Else
     C******             move      *off          meetss
     C******             Endif
     C******             endif
     C*
     C******             if        xxcall ='R' and EANSNT
     C******             move      *off          meetss
     C******             endif
     C*
     C                   endsr
     C*================================================================
     C* DSPDOC - Display Document
     C*================================================================
     C     dspDoc        begsr
     C*
     C                   movel(p)  scidoc        kkdcid
     C     doky          chain     pdo01                              99
     C                   if        *in99 = *off
     C                   if        dofl01 = 'F'
     C*
     C* Call the DSPFAXC Program to display the fax, else
     C*
     C                   movel     doflr         xxflr             8
     C                   movel     dosflr        xxsflr           12
     C     xxflr         cat       '/'           xx9a              9
     C     xx9a          cat(p)    xxsflr        $$fold
     C*
     C                   call      'DSPFAXC'
     C                   parm      scidoc        $$dcid           12
     C                   parm                    $$FOLD           80
     C                   parm      *blanks       $$rtrn            1
     C*
     C                   else
     C* Image Display
     C*
     C                   movel(p)  dodtyp        kkdoct
     C     dtky          chain     pdt01                              99
     C                   if        *in99 = *off
     C                   eval      $$desc = dtdesc
     C                   else
     C                   eval      $$desc = '*N/A*'
     C                   endif
     C*
     C                   call      'IS211C'
     C                   parm      scidoc        $$DCID
     C                   parm      doflr         $$FLR             8
     C                   parm      dosflr        $$SFLR           12
     C                   parm      dodtyp        $$DOCT            8
     C                   parm                    $$DESC           40
     C                   parm      *blanks       $$RTN             6
     C*
     C                   endif
     C                   endif
     C*
     C                   endsr
     C*
     C*
** Column headings
Opt Claimant                EOMB#    Provider       From          To      Sts
Opt Provider       Claimant                EOMB#    From          To      Sts
Opt Soc Sec #     DOI        EOMB#   Provider       From          To      Sts
Opt    EOMB#   Claimant             Provider        From          To      Sts
Opt Control Number Claimant                EOMB#    From          To      Sts
Opt Claimant                EOMB#    Provider    Charge Amt   Paid Amt Typ AST
Opt Provider       Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Soc Sec #     DOI        EOMB#   Provider    Charge Amt   Paid Amt Typ AST
Opt    EOMB#   Claimant             Provider     Charge Amt   Paid Amt Typ AST
Opt Control Number Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Claimant                EOMB#    Provider     Control Number     Sts
Opt Provider       Claimant                EOMB#  Control Number     Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Control Number     Sts
Opt    EOMB#   Claimant             Provider      Control Number     Sts
Opt Control Number Claimant                EOMB#  Control Number     Sts
Opt Claimant                EOMB#    Provider     Provider's Name         Sts
Opt Provider       Claimant                EOMB#  Provider's Name         Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Provider's Name         Sts
Opt    EOMB#   Claimant             Provider      Provider's Name         Sts
Opt Control Number Claimant                EOMB#  Provider's Name         Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN     07/29/99
1044=Delete                 YNN
1055=Display                YYY
1066=Dtl Chrg List          YNN
1077=Work w/ Detail         YYY
1088=Work w/ Amounts        YYN
1099=EOMB Delete            NNN
11010=EOMB status           YYY
11111=EOB/Notes             YYY
11212=Print EOMB            NNN
11313=Display EOMB          NNN
11414=DARTS                 YNN
11515=Pre-Cert Info         YYY
11616=History               NNN
11818=EDI Log Info          NNN
11919=Send Subsq 837        NNN
12020=Pended items          YYY
12121=Check info            NNN
12222=View Prov Info        YYY
13030=Letters               YYN
18181=Claim info            YYN
18282=Case info             YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYN
211F11=Change view          YYN
212F12=Cancel               YYY
215F15=Fold/Truncate        YYN
217F17=Subset               YYY
3
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )   OVRSCOPE(*JOB)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )   OVRSCOPE(*JOB)
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )   OVRSCOPE(*JOB)
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )   OVRSCOPE(*JOB)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  ) OVRSCOPE(*JOB)
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  ) OVRSCOPE(*JOB)
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )   OVRSCOPE(*JOB)
DLTOVR FILE(FTABL)    LVL(*JOB)
DLTOVR FILE(FMPRL)    LVL(*JOB)
DLTOVR FILE(FTPSL)    LVL(*JOB)
DLTOVR FILE(FMSVP)    LVL(*JOB)
DLTOVR FILE(FMSVL1)   LVL(*JOB)
DLTOVR FILE(FMSVL2)   LVL(*JOB)
DLTOVR FILE(FMPRP)    LVL(*JOB)
