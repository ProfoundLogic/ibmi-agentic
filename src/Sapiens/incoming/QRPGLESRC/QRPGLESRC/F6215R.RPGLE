     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  F6215R - 837 Medical EDI Format transmission (IAIABC)
     H*
     H*  Description: This pgm will format the 837 Medical EDI data
     H*               into IAIABC ANSI X12 formatted file.
     H*
     H*  Created    : 06/17/2014  Programmer: 212  Project: MC376
     H*
     H*  Called By  : F6211C
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
    F*
    f* Log Record File
     ffelgl3    uf a e           k disk
    f*
     ffemfl     uf   e           k disk
    f*
     ffeprl1    if   e           k disk
    f*
     ffebll1    if   e           k disk
    f*
     ffecoml1   if   e           k disk
    f*
     ffeexl     if   e           k disk
    f*
     ffaeolg    if   e           k disk
    f*
     ffeehl     if   e           k disk
    f*
     ffeeul     if   e           k disk
    f*
     ffeepl     if   e           k disk
    f*
     ffeedl     if   e           k disk
    f*
     ffedhl     if   e           k disk
    f*
     ffedul     if   e           k disk
    f*
     ffedpl     if   e           k disk
    f*
     ffeddl     if   e           k disk
    f*
     fnmnwl     if   e           k disk
    f*
     ffmc1l     uf   e           k disk
    f*
     fststl1    if   e           k disk
    f*
     fwdf2l     if   e           k disk
     f                                     usropn
    f*
     ffaedl     if   e           k disk    rename(ped01:ped02)
     f                                     prefix(e1:2)
    f*
     ffaehl     if   e           k disk    rename(peh01:peh02)
     f                                     prefix(e2:2)
    f*
     ffaepl     if   e           k disk    rename(pep01:pep02)
     f                                     prefix(e3:2)
    f*
     ffaeul     if   e           k disk    rename(peu01:peu02)
     f                                     prefix(e4:2)
    f*
     ffdddl     if   e           k disk    rename(pdd01:pdd02)
     f                                     prefix(d1:2)
    f*
     ffddhl     if   e           k disk    rename(pdh01:pdh02)
     f                                     prefix(d2:2)
    f*
     ffddpl     if   e           k disk    rename(pdp01:pdp02)
     f                                     prefix(d3:2)
    f*
     ffddul     if   e           k disk    rename(pdu01:pdu02)
     f                                     prefix(d4:2)
    f*
     ffdcal5    if   e           k disk
    f*
     ffddal2    if   e           k disk
    f*
    f* Medical 837 IAIABC ANSI X12 formatted flat file (extract)
     ffe837flp  o    e           k disk
    D*
     d** Procedure definitions
     d*
     d GetSystem       pr                  ExtPgm('SA001R')
     d ##wcif                              like(##rtcd)                         WC INST
     d ##mcif                              like(##rtcd)                         MC INST
     d ##wcin                              like(##rtcd)                         WC INTERFACE
     d ##ar                                like(##rtcd)                         MC AR INST
     d ##ntwk                              like(##rtcd)                         NETWORK INST
     d ##ur                                like(##rtcd)                         PRECERT INST
     d ##med                               like(##rtcd)                         MEDMGMT INST
     d ##edi                               like(##rtcd)                         EDI INST
     d ##wcdk                              like(##rtcd)                         WC DARTS KEY
     d ##rhab                              like(##rtcd)                         REHAB INST
     d ##mss                               like(##rtcd)                         MS SUPPORT
     d ##fsch                              like(##rtcd)                         FEE SCHEDULE
     d ##prvb                              like(##rtcd)                         PROVIDER BILL

     d** Data Structures
     d*
     d LogRec        e ds                  extname(felgl3:*input) inz
     d LogRecPrv       ds                  likeds(LogRec) inz
     d*
     d                 ds
     d DTime                         20  0
     d CDateIso                       8  0 overlay(DTime)
     d CTimeHms                       6  0 overlay(DTime:*next)
     d*
     d** DS - ISA Interchange Control Header
     d ISA             ds                  qualified
     d                                     inz
     d AutQual                             overlay(ISA)
     d                                     like(isa01)
     d AutInfo                             overlay(ISA:*next)
     d                                     like(isa02)
     d SecQual                             overlay(ISA:*next)
     d                                     like(isa03)
     d SecInfo                             overlay(ISA:*next)
     d                                     like(isa04)
     d IntIdQ1                             overlay(ISA:*next)
     d                                     like(isa05)
     d IntSndId                            overlay(ISA:*next)
     d                                     like(isa06)
     d IntIDQ2                             overlay(ISA:*next)
     d                                     like(isa07)
     d IntRcvId                            overlay(ISA:*next)
     d                                     like(isa08)
     d IntDt                               overlay(ISA:*next)
     d                                     like(isa09)
     d IntTm                               overlay(ISA:*next)
     d                                     like(isa10)
     d IntCtlId                            overlay(ISA:*next)
     d                                     like(isa11)
     d IntCtlVr                            overlay(ISA:*next)
     d                                     like(isa12)
     d IntCtlNo                            overlay(ISA:*next)
     d                                     like(isa13)
     d AckReq                              overlay(ISA:*next)
     d                                     like(isa14)
     d UsgInd                              overlay(ISA:*next)
     d                                     like(isa15)
     d CmpSep                              overlay(ISA:*next)
     d                                     like(isa16)
     d*
     d** DS - Functional Group Control Header
     d FGrpH           ds                  qualified
     d                                     inz
     d IdCode                              overlay(FGrpH)
     d                                     like(gs01)
     d AppSndCd                            overlay(FGrpH:*next)
     d                                     like(gs02)
     d AppRcvCd                            overlay(FGrpH:*next)
     d                                     like(gs03)
     d PrepDt                              overlay(FGrpH:*next)
     d                                     like(gs04)
     d PrepTm                              overlay(FGrpH:*next)
     d                                     like(gs05)
     d CtrlNo                              overlay(FGrpH:*next)
     d                                     like(gs06)
     d ResAgyCd                            overlay(FGrpH:*next)
     d                                     like(gs07)
     d VRIIdCd                             overlay(FGrpH:*next)
     d                                     like(gs08)
     d*
     d** DS - Transaction Set Header
     d TSetH           ds                  qualified
     d                                     inz
     d IdCode                              overlay(TSetH)
     d                                     like(st01)
     d CtrlNo                              overlay(TSetH:*next)
     d                                     like(st02)
     d ImpConRf                            overlay(TSetH:*next)
     d                                     like(st03)
     d HlStrCd                             overlay(TSetH:*next)
     d                                     like(bht01)
     d PurpCd                              overlay(TSetH:*next)
     d                                     like(bht02)
     d OTrnIdNo                            overlay(TSetH:*next)
     d                                     like(bht03)
     d TrnSndDt                            overlay(TSetH:*next)
     d                                     like(bht04)
     d TrnSndTm                            overlay(TSetH:*next)
     d                                     like(bht05)
     d TrnTypCd                            overlay(TSetH:*next)
     d                                     like(bht06)
     d*
     d** DS - Submitter Information
     d Sbm             ds                  qualified
     d                                     inz
     d IdCode                              overlay(Sbm)
     d                                     like(nm101)
     d TypQual                             overlay(Sbm:*next)
     d                                     like(nm102)
     d NameL                               overlay(Sbm:*next)
     d                                     like(nm103)
     d NameF                               overlay(Sbm:*next)
     d                                     like(nm104)
     d NameM                               overlay(Sbm:*next)
     d                                     like(nm105)
     d NameP                               overlay(Sbm:*next)
     d                                     like(nm106)
     d NameS                               overlay(Sbm:*next)
     d                                     like(nm107)
     d IdQual                              overlay(Sbm:*next)
     d                                     like(nm108)
     d SndId                               overlay(Sbm:*next)
     d                                     like(nm109)
     d*
     d** DS - Receiver Information
     d Rcv             ds                  qualified
     d                                     inz
     d IdCode                              overlay(Rcv)
     d                                     like(nm101)
     d TypQual                             overlay(Rcv:*next)
     d                                     like(nm102)
     d NameL                               overlay(Rcv:*next)
     d                                     like(nm103)
     d NameF                               overlay(Rcv:*next)
     d                                     like(nm104)
     d NameM                               overlay(Rcv:*next)
     d                                     like(nm105)
     d NameP                               overlay(Rcv:*next)
     d                                     like(nm106)
     d NameS                               overlay(Rcv:*next)
     d                                     like(nm107)
     d IdQual                              overlay(Rcv:*next)
     d                                     like(nm108)
     d RcvId                               overlay(Rcv:*next)
     d                                     like(nm109)
     d*
     d** DS - Insurer Hierarchical Level Information
     d InHL            ds                  qualified
     d                                     inz
     d IdNo                                overlay(InHL)
     d                                     like(hl01)
     d ParentId                            overlay(InHL:*next)
     d                                     like(hl02)
     d LevelCd                             overlay(InHL:*next)
     d                                     like(hl03)
     d ChildCd                             overlay(InHL:*next)
     d                                     like(hl04)
     d DtTmQ                               overlay(InHL:*next)
     d                                     like(dtp01)
     d DtTmFmtQ                            overlay(InHL:*next)
     d                                     like(dtp02)
     d DtTmPrd                             overlay(InHL:*next)
     d                                     like(dtp03)
     d*
     d** DS - Insurer/Self Insured Information
     d InInf           ds                  qualified
     d                                     inz
     d IdCode                              overlay(InInf)
     d                                     like(nm101)
     d TypQual                             overlay(InInf:*next)
     d                                     like(nm102)
     d NameL                               overlay(InInf:*next)
     d                                     like(nm103)
     d NameF                               overlay(InInf:*next)
     d                                     like(nm104)
     d NameM                               overlay(InInf:*next)
     d                                     like(nm105)
     d NameP                               overlay(InInf:*next)
     d                                     like(nm106)
     d NameS                               overlay(InInf:*next)
     d                                     like(nm107)
     d IdQual                              overlay(InInf:*next)
     d                                     like(nm108)
     d Fein                                overlay(InInf:*next)
     d                                     like(nm109)
     d RshpCd                              overlay(InInf:*next)
     d                                     like(nm110)
     d IdCd                                overlay(InInf:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(InInf:*next)
     d                                     like(nm112)
     d CityNm                              overlay(InInf:*next)
     d                                     like(n401)
     d StPrvCd                             overlay(InInf:*next)
     d                                     like(n402)
     d PostCd                              overlay(InInf:*next)
     d                                     like(n403)
     d CtryCd                              overlay(InInf:*next)
     d                                     like(n404)
     d LocQ                                overlay(InInf:*next)
     d                                     like(n405)
     d LocId                               overlay(InInf:*next)
     d                                     like(n406)
     d SubDvCd                             overlay(InInf:*next)
     d                                     like(n407)
     d*
     d** DS - Claim Administrator Information
     d ClAdm           ds                  qualified
     d                                     inz
     d IdCode                              overlay(ClAdm)
     d                                     like(nm101)
     d TypQual                             overlay(ClAdm:*next)
     d                                     like(nm102)
     d NameL                               overlay(ClAdm:*next)
     d                                     like(nm103)
     d NameF                               overlay(ClAdm:*next)
     d                                     like(nm104)
     d NameM                               overlay(ClAdm:*next)
     d                                     like(nm105)
     d NameP                               overlay(ClAdm:*next)
     d                                     like(nm106)
     d NameS                               overlay(ClAdm:*next)
     d                                     like(nm107)
     d IdQual                              overlay(ClAdm:*next)
     d                                     like(nm108)
     d Fein                                overlay(ClAdm:*next)
     d                                     like(nm109)
     d RshpCd                              overlay(ClAdm:*next)
     d                                     like(nm110)
     d IdCd                                overlay(ClAdm:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(ClAdm:*next)
     d                                     like(nm112)
     d CityNm                              overlay(ClAdm:*next)
     d                                     like(n401)
     d StPrvCd                             overlay(ClAdm:*next)
     d                                     like(n402)
     d PostCd                              overlay(ClAdm:*next)
     d                                     like(n403)
     d CtryCd                              overlay(ClAdm:*next)
     d                                     like(n404)
     d LocQ                                overlay(ClAdm:*next)
     d                                     like(n405)
     d LocId                               overlay(ClAdm:*next)
     d                                     like(n406)
     d SubDvCd                             overlay(ClAdm:*next)
     d                                     like(n407)
     d*
     d** DS - Employer Hierarchical Information
     d EmHL            ds                  qualified
     d                                     inz
     d IdNo                                overlay(EmHL)
     d                                     like(hl01)
     d ParentId                            overlay(EmHL:*next)
     d                                     like(hl02)
     d LevelCd                             overlay(EmHL:*next)
     d                                     like(hl03)
     d ChildCd                             overlay(EmHL:*next)
     d                                     like(hl04)
     d*
     d** DS - Employer Information
     d EmInf           ds                  qualified
     d                                     inz
     d IdCode                              overlay(EmInf)
     d                                     like(nm101)
     d TypQual                             overlay(EmInf:*next)
     d                                     like(nm102)
     d NameL                               overlay(EmInf:*next)
     d                                     like(nm103)
     d NameF                               overlay(EmInf:*next)
     d                                     like(nm104)
     d NameM                               overlay(EmInf:*next)
     d                                     like(nm105)
     d NameP                               overlay(EmInf:*next)
     d                                     like(nm106)
     d NameS                               overlay(EmInf:*next)
     d                                     like(nm107)
     d IdQual                              overlay(EmInf:*next)
     d                                     like(nm108)
     d Fein                                overlay(EmInf:*next)
     d                                     like(nm109)
     d RshpCd                              overlay(EmInf:*next)
     d                                     like(nm110)
     d IdCd                                overlay(EmInf:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(EmInf:*next)
     d                                     like(nm112)
     d Addr1                               overlay(EmInf:*next)
     d                                     like(n301)
     d Addr2                               overlay(EmInf:*next)
     d                                     like(n302)
     d CityNm                              overlay(EmInf:*next)
     d                                     like(n401)
     d StPrvCd                             overlay(EmInf:*next)
     d                                     like(n402)
     d PostCd                              overlay(EmInf:*next)
     d                                     like(n403)
     d CtryCd                              overlay(EmInf:*next)
     d                                     like(n404)
     d LocQ                                overlay(EmInf:*next)
     d                                     like(n405)
     d LocId                               overlay(EmInf:*next)
     d                                     like(n406)
     d SubDvCd                             overlay(EmInf:*next)
     d                                     like(n407)
     d*
     d** DS - Claimant Hierarchical Level Information
     d ClmHL           ds                  qualified
     d                                     inz
     d IdNo                                overlay(ClmHL)
     d                                     like(hl01)
     d ParentId                            overlay(ClmHL:*next)
     d                                     like(hl02)
     d LevelCd                             overlay(ClmHL:*next)
     d                                     like(hl03)
     d ChildCd                             overlay(ClmHL:*next)
     d                                     like(hl04)
     d DtTmQ                               overlay(ClmHL:*next)
     d                                     like(dtp01)
     d DtTmFmtQ                            overlay(ClmHL:*next)
     d                                     like(dtp02)
     d DtTmPrd                             overlay(ClmHL:*next)
     d                                     like(dtp03)
     d*
     D** Constants
     d Aster           c                   const('*')
     d Tilde           c                   const('~')
     d*
     d** DS - Claimant Information
     d ClmInf          ds                  qualified
     d                                     inz
     d IdCode                              overlay(ClmInf)
     d                                     like(nm101)
     d TypQual                             overlay(ClmInf:*next)
     d                                     like(nm102)
     d NameL                               overlay(ClmInf:*next)
     d                                     like(nm103)
     d NameF                               overlay(ClmInf:*next)
     d                                     like(nm104)
     d NameM                               overlay(ClmInf:*next)
     d                                     like(nm105)
     d NameP                               overlay(ClmInf:*next)
     d                                     like(nm106)
     d NameS                               overlay(ClmInf:*next)
     d                                     like(nm107)
     d IdQual                              overlay(ClmInf:*next)
     d                                     like(nm108)
     d Fein                                overlay(ClmInf:*next)
     d                                     like(nm109)
     d RshpCd                              overlay(ClmInf:*next)
     d                                     like(nm110)
     d IdCd                                overlay(ClmInf:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(ClmInf:*next)
     d                                     like(nm112)
     d Addr1                               overlay(ClmInf:*next)
     d                                     like(n301)
     d Addr2                               overlay(ClmInf:*next)
     d                                     like(n302)
     d CityNm                              overlay(ClmInf:*next)
     d                                     like(n401)
     d StPrvCd                             overlay(ClmInf:*next)
     d                                     like(n402)
     d PostCd                              overlay(ClmInf:*next)
     d                                     like(n403)
     d CtryCd                              overlay(ClmInf:*next)
     d                                     like(n404)
     d LocQ                                overlay(ClmInf:*next)
     d                                     like(n405)
     d LocId                               overlay(ClmInf:*next)
     d                                     like(n406)
     d SubDvCd                             overlay(ClmInf:*next)
     d                                     like(n407)
     d DtTmPrQ                             overlay(ClmInf:*next)
     d                                     like(dmg01)
     d DtTmDOB                             overlay(ClmInf:*next)
     d                                     like(dmg02)
     d GndrCd                              overlay(ClmInf:*next)
     d                                     like(dmg03)
     d MarSts                              overlay(ClmInf:*next)
     d                                     like(dmg04)
     d RaceCd                              overlay(ClmInf:*next)
     d                                     like(dmg05)
     d CtzCd                               overlay(ClmInf:*next)
     d                                     like(dmg06)
     d CtryCd2                             overlay(ClmInf:*next)
     d                                     like(dmg07)
     d VerfyCd                             overlay(ClmInf:*next)
     d                                     like(dmg08)
     d Quanty                              overlay(ClmInf:*next)
     d                                     like(dmg09)
     d CdLsQCd                             overlay(ClmInf:*next)
     d                                     like(dmg10)
     d IndstCd                             overlay(ClmInf:*next)
     d                                     like(dmg11)
     d RefIdQ                              overlay(ClmInf:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(ClmInf:*next)
     d                                     like(ref02)
     d Desc                                overlay(ClmInf:*next)
     d                                     like(ref03)
     d RefId                               overlay(ClmInf:*next)
     d                                     like(ref04)
     d RefIdQ2                             overlay(ClmInf:*next)
     d                                     like(ref01)
     d RefIdn2                             overlay(ClmInf:*next)
     d                                     like(ref02)
     d Desc2                               overlay(ClmInf:*next)
     d                                     like(ref03)
     d RefId2                              overlay(ClmInf:*next)
     d                                     like(ref04)
     d RefIdQ3                             overlay(ClmInf:*next)
     d                                     like(ref01)
     d RefIdn3                             overlay(ClmInf:*next)
     d                                     like(ref02)
     d Desc3                               overlay(ClmInf:*next)
     d                                     like(ref03)
     d RefId3                              overlay(ClmInf:*next)
     d                                     like(ref04)
     d CFuncCd                             overlay(ClmInf:*next)
     d                                     like(per01)
     d ComNm                               overlay(ClmInf:*next)
     d                                     like(per02)
     d ComNoQ                              overlay(ClmInf:*next)
     d                                     like(per03)
     d ComNo                               overlay(ClmInf:*next)
     d                                     like(per04)
     d ComNoQ2                             overlay(ClmInf:*next)
     d                                     like(per05)
     d ComNo2                              overlay(ClmInf:*next)
     d                                     like(per06)
     d ComNoQ3                             overlay(ClmInf:*next)
     d                                     like(per07)
     d ComNo3                              overlay(ClmInf:*next)
     d                                     like(per08)
     d InqRef                              overlay(ClmInf:*next)
     d                                     like(per09)
     d*
     d** DS - Bill Information
     d*** the repeating DTP, HI, REF information is included under this once
     d BlInf           ds                  qualified
     d                                     inz
     d** CLM segment
     d IdCode                              overlay(BlInf)
     d                                     like(clm01)
     d TotChrg                             overlay(BlInf:*next)
     d                                     like(clm02)
     d FilInd                              overlay(BlInf:*next)
     d                                     like(clm03)
     d ClmTypCd                            overlay(BlInf:*next)
     d                                     like(clm04)
     d FacPOS                              overlay(BlInf:*next)
     d                                     like(clm05a)
     d FacCdQ                              overlay(BlInf:*next)
     d                                     like(clm05b)
     d ClFrqCd                             overlay(BlInf:*next)
     d                                     like(clm05c)
     d AgrmtCd                             overlay(BlInf:*next)
     d                                     like(clm16)
     d ClStsCd                             overlay(BlInf:*next)
     d                                     like(clm17)
     d RespCd                              overlay(BlInf:*next)
     d                                     like(clm18)
     d SbmRsnCd                            overlay(BlInf:*next)
     d                                     like(clm19)
     d DlyRsnCd                            overlay(BlInf:*next)
     d                                     like(clm20)
     d** DTP segment
     d DtTmQ                               overlay(BlInf:*next)
     d                                     like(dtp01)
     d DtTmFmtQ                            overlay(BlInf:*next)
     d                                     like(dtp02)
     d DtTmPrd                             overlay(BlInf:*next)
     d                                     like(dtp03)
     d** CL1 segment
     d AdmTypCd                            overlay(BlInf:*next)
     d                                     like(cl101)
     d AdmSrcCd                            overlay(BlInf:*next)
     d                                     like(cl102)
     d PtStsCd                             overlay(BlInf:*next)
     d                                     like(cl103)
     d NurHmCd                             overlay(BlInf:*next)
     d                                     like(cl104)
     d** CN1 segment
     d CntrTyp                             overlay(BlInf:*next)
     d                                     like(cn101)
     d CntrAmt                             overlay(BlInf:*next)
     d                                     like(cn102)
     d DecFmt                              overlay(BlInf:*next)
     d                                     like(cn103)
     d CntrRef1                            overlay(BlInf:*next)
     d                                     like(cn104)
     d CntrRef2                            overlay(BlInf:*next)
     d                                     like(cn105)
     d VerId                               overlay(BlInf:*next)
     d                                     like(cn106)
     d** AMT segment
     d AmtQCd                              overlay(BlInf:*next)
     d                                     like(amt01)
     d MAmount                             overlay(BlInf:*next)
     d                                     like(amt02)
     d CrDbFlg                             overlay(BlInf:*next)
     d                                     like(amt03)
     d** REF segment
     d RefIdQ                              overlay(BlInf:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(BlInf:*next)
     d                                     like(ref02)
     d Desc                                overlay(BlInf:*next)
     d                                     like(ref03)
     d RefId                               overlay(BlInf:*next)
     d                                     like(ref04)
     d** HI segment
     d HICdLsQ                             overlay(BlInf:*next)
     d                                     like(hi01a)
     d HIIndCd                             overlay(BlInf:*next)
     d                                     like(hi01b)
     d HIDTFmQ                             overlay(BlInf:*next)
     d                                     like(hi01c)
     d HIDTPrd                             overlay(BlInf:*next)
     d                                     like(hi01d)
     d HIMAmnt                             overlay(BlInf:*next)
     d                                     like(hi01e)
     d HIQty                               overlay(BlInf:*next)
     d                                     like(hi01f)
     d HIVerId                             overlay(BlInf:*next)
     d                                     like(hi01g)
     d HIIndCd2                            overlay(BlInf:*next)
     d                                     like(hi01h)
     d HIAdmRes                            overlay(BlInf:*next)
     d                                     like(hi01i)
     d HlthCd1                             overlay(BlInf:*next)
     d                                     like(hi02)
     d HlthCd2                             overlay(BlInf:*next)
     d                                     like(hi03)
     d HlthCd3                             overlay(BlInf:*next)
     d                                     like(hi04)
     d HlthCd4                             overlay(BlInf:*next)
     d                                     like(hi05)
     d HlthCd5                             overlay(BlInf:*next)
     d                                     like(hi06)
     d HlthCd6                             overlay(BlInf:*next)
     d                                     like(hi07)
     d HlthCd7                             overlay(BlInf:*next)
     d                                     like(hi08)
     d HlthCd8                             overlay(BlInf:*next)
     d                                     like(hi09)
     d HlthCd9                             overlay(BlInf:*next)
     d                                     like(hi10)
     d HlthCd10                            overlay(BlInf:*next)
     d                                     like(hi11)
     d HlthCd11                            overlay(BlInf:*next)
     d                                     like(hi12)
     d*
     d** DS - Billing Provider Info
     d BlPrv           ds                  qualified
     d                                     inz
     d IdCode                              overlay(BlPrv)
     d                                     like(nm101)
     d TypQual                             overlay(BlPrv:*next)
     d                                     like(nm102)
     d NameL                               overlay(BlPrv:*next)
     d                                     like(nm103)
     d NameF                               overlay(BlPrv:*next)
     d                                     like(nm104)
     d NameM                               overlay(BlPrv:*next)
     d                                     like(nm105)
     d NameP                               overlay(BlPrv:*next)
     d                                     like(nm106)
     d NameS                               overlay(BlPrv:*next)
     d                                     like(nm107)
     d IdQual                              overlay(BlPrv:*next)
     d                                     like(nm108)
     d Fein                                overlay(BlPrv:*next)
     d                                     like(prbnpi)
     d RshpCd                              overlay(BlPrv:*next)
     d                                     like(nm110)
     d IdCd                                overlay(BlPrv:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(BlPrv:*next)
     d                                     like(nm112)
     d PrCd                                overlay(BlPrv:*next)
     d                                     like(prv01)
     d PrRefIdQ                            overlay(BlPrv:*next)
     d                                     like(prv02)
     d PrRefIdn                            overlay(BlPrv:*next)
     d                                     like(prv03)
     d PrStCd                              overlay(BlPrv:*next)
     d                                     like(prv04)
     d PrSpcInf                            overlay(BlPrv:*next)
     d                                     like(prv05)
     d PrOrgCd                             overlay(BlPrv:*next)
     d                                     like(prv06)
     d Addr1                               overlay(BlPrv:*next)
     d                                     like(prbadr1)
     d Addr2                               overlay(BlPrv:*next)
     d                                     like(prbadr2)
     d CityNm                              overlay(BlPrv:*next)
     d                                     like(prbcty)
     d StPrvCd                             overlay(BlPrv:*next)
     d                                     like(prbst)
     d PostCd                              overlay(BlPrv:*next)
     d                                     like(prbzip)
     d CtryCd                              overlay(BlPrv:*next)
     d                                     like(prbcnty)
     d LocQ                                overlay(BlPrv:*next)
     d                                     like(n405)
     d LocId                               overlay(BlPrv:*next)
     d                                     like(n406)
     d SubDvCd                             overlay(BlPrv:*next)
     d                                     like(n407)
     d RefIdQ                              overlay(BlPrv:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(BlPrv:*next)
     d                                     like(prbfein)
     d Desc                                overlay(BlPrv:*next)
     d                                     like(ref03)
     d RefId                               overlay(BlPrv:*next)
     d                                     like(ref04)
     d RefIdQ2                             overlay(BlPrv:*next)
     d                                     like(ref01)
     d RefIdn2                             overlay(BlPrv:*next)
     d                                     like(prblic)
     d Desc2                               overlay(BlPrv:*next)
     d                                     like(ref03)
     d RefId2                              overlay(BlPrv:*next)
     d                                     like(ref04)
     d*
     d** DS - Rendering Bill Provider Info
     d RndPrv          ds                  qualified
     d                                     inz
     d IdCode                              overlay(RndPrv)
     d                                     like(nm101)
     d TypQual                             overlay(RndPrv:*next)
     d                                     like(nm102)
     d NameL                               overlay(RndPrv:*next)
     d                                     like(prrlsn)
     d NameF                               overlay(RndPrv:*next)
     d                                     like(prrfsn)
     d NameM                               overlay(RndPrv:*next)
     d                                     like(nm105)
     d NameP                               overlay(RndPrv:*next)
     d                                     like(nm106)
     d NameS                               overlay(RndPrv:*next)
     d                                     like(prrlsf)
     d IdQual                              overlay(RndPrv:*next)
     d                                     like(nm108)
     d Fein                                overlay(RndPrv:*next)
     d                                     like(prrnpi)
     d RshpCd                              overlay(RndPrv:*next)
     d                                     like(nm110)
     d IdCd                                overlay(RndPrv:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(RndPrv:*next)
     d                                     like(nm112)
     d PrCd                                overlay(RndPrv:*next)
     d                                     like(prv01)
     d PrRefIdQ                            overlay(RndPrv:*next)
     d                                     like(prv02)
     d PrRefIdn                            overlay(RndPrv:*next)
     d                                     like(prv03)
     d PrStCd                              overlay(RndPrv:*next)
     d                                     like(prv04)
     d PrSpcInf                            overlay(RndPrv:*next)
     d                                     like(prv05)
     d PrOrgCd                             overlay(RndPrv:*next)
     d                                     like(prv06)
     d RefIdQ                              overlay(RndPrv:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(RndPrv:*next)
     d                                     like(prrlic)
     d Desc                                overlay(RndPrv:*next)
     d                                     like(ref03)
     d RefId                               overlay(RndPrv:*next)
     d                                     like(ref04)
     d*
     d** DS - Service Facility Location Info
     d SrvLoc          ds                  qualified
     d                                     inz
     d IdCode                              overlay(SrvLoc)
     d                                     like(nm101)
     d TypQual                             overlay(SrvLoc:*next)
     d                                     like(nm102)
     d NameL                               overlay(SrvLoc:*next)
     d                                     like(prflsn)
     d NameF                               overlay(SrvLoc:*next)
     d                                     like(nm104)
     d NameM                               overlay(SrvLoc:*next)
     d                                     like(nm105)
     d NameP                               overlay(SrvLoc:*next)
     d                                     like(nm106)
     d NameS                               overlay(SrvLoc:*next)
     d                                     like(nm107)
     d IdQual                              overlay(SrvLoc:*next)
     d                                     like(nm108)
     d Fein                                overlay(SrvLoc:*next)
     d                                     like(prfnpi)
     d RshpCd                              overlay(SrvLoc:*next)
     d                                     like(nm110)
     d IdCd                                overlay(SrvLoc:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(SrvLoc:*next)
     d                                     like(nm112)
     d Addr1                               overlay(SrvLoc:*next)
     d                                     like(prfadr1)
     d Addr2                               overlay(SrvLoc:*next)
     d                                     like(prfadr2)
     d CityNm                              overlay(SrvLoc:*next)
     d                                     like(prfcty)
     d StPrvCd                             overlay(SrvLoc:*next)
     d                                     like(prfst)
     d PostCd                              overlay(SrvLoc:*next)
     d                                     like(prfzip)
     d CtryCd                              overlay(SrvLoc:*next)
     d                                     like(prfcnty)
     d LocQ                                overlay(SrvLoc:*next)
     d                                     like(n405)
     d LocId                               overlay(SrvLoc:*next)
     d                                     like(n406)
     d SubDvCd                             overlay(SrvLoc:*next)
     d                                     like(n407)
     d RefIdQ                              overlay(SrvLoc:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(SrvLoc:*next)
     d                                     like(ref02)
     d Desc                                overlay(SrvLoc:*next)
     d                                     like(ref03)
     d RefId                               overlay(SrvLoc:*next)
     d                                     like(ref04)
     d*
     d** DS - Referring Bill Provider Info
     d RefPrv          ds                  qualified
     d                                     inz
     d IdCode                              overlay(RefPrv)
     d                                     like(nm101)
     d TypQual                             overlay(RefPrv:*next)
     d                                     like(nm102)
     d NameL                               overlay(RefPrv:*next)
     d                                     like(prrflsn)
     d NameF                               overlay(RefPrv:*next)
     d                                     like(prrffsn)
     d NameM                               overlay(RefPrv:*next)
     d                                     like(prrfmid)
     d NameP                               overlay(RefPrv:*next)
     d                                     like(nm106)
     d NameS                               overlay(RefPrv:*next)
     d                                     like(prrflsf)
     d IdQual                              overlay(RefPrv:*next)
     d                                     like(nm108)
     d Fein                                overlay(RefPrv:*next)
     d                                     like(prrfnpi)
     d RshpCd                              overlay(RefPrv:*next)
     d                                     like(nm110)
     d IdCd                                overlay(RefPrv:*next)
     d                                     like(nm111)
     d OrgNmL                              overlay(RefPrv:*next)
     d                                     like(nm112)
     d RefIdQ                              overlay(RefPrv:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(RefPrv:*next)
     d                                     like(ref02)
     d Desc                                overlay(RefPrv:*next)
     d                                     like(ref03)
     d RefId                               overlay(RefPrv:*next)
     d                                     like(ref04)
     d*
     d** DS - Bill Level Adjustments and Amounts
     d BilAA           ds                  qualified
     d                                     inz
     d SeqNo                               overlay(BilAA)
     d                                     like(sbr01)
     d RshpCd                              overlay(BilAA:*next)
     d                                     like(sbr02)
     d RefId                               overlay(BilAA:*next)
     d                                     like(sbr03)
     d Name                                overlay(BilAA:*next)
     d                                     like(sbr04)
     d TypCd                               overlay(BilAA:*next)
     d                                     like(sbr05)
     d BenfCd                              overlay(BilAA:*next)
     d                                     like(sbr06)
     d RespCd                              overlay(BilAA:*next)
     d                                     like(sbr07)
     d EmStsCd                             overlay(BilAA:*next)
     d                                     like(sbr08)
     d FlIndCd                             overlay(BilAA:*next)
     d                                     like(sbr09)
     d ClGrpCd                             overlay(BilAA:*next)
     d                                     like(cas01)
     d ClRsnCd                             overlay(BilAA:*next)
     d                                     like(cas02)
     d MAmnt                               overlay(BilAA:*next)
     d                                     like(cas03)
     d Qty                                 overlay(BilAA:*next)
     d                                     like(cas04)
     d AmtQCd                              overlay(BilAA:*next)
     d                                     like(amt01)
     d MAmount                             overlay(BilAA:*next)
     d                                     like(amt02)
     d CrDbFlg                             overlay(BilAA:*next)
     d                                     like(amt03)
     d*
     d** DS - Service Line Information
     d SrvInf          ds                  qualified
     d                                     inz
     d AsgNo                               overlay(SrvInf)
     d                                     like(lx01)
     d PSPrdIdQ                            overlay(SrvInf:*next)
     d                                     like(sv101a)
     d PSPrdId                             overlay(SrvInf:*next)
     d                                     like(sv101b)
     d PSMod1                              overlay(SrvInf:*next)
     d                                     like(sv101c)
     d PSMod2                              overlay(SrvInf:*next)
     d                                     like(sv101d)
     d PSMod3                              overlay(SrvInf:*next)
     d                                     like(sv101e)
     d PSMod4                              overlay(SrvInf:*next)
     d                                     like(sv101f)
     d PSDesc                              overlay(SrvInf:*next)
     d                                     like(sv101g)
     d PSPrdId2                            overlay(SrvInf:*next)
     d                                     like(sv101h)
     d PSAmnt                              overlay(SrvInf:*next)
     d                                     like(sv102)
     d PSUnit                              overlay(SrvInf:*next)
     d                                     like(sv103)
     d PSQty                               overlay(SrvInf:*next)
     d                                     like(sv104)
     d PSFacCd                             overlay(SrvInf:*next)
     d                                     like(sv105)
     d PSSrvCd                             overlay(SrvInf:*next)
     d                                     like(sv106)
     d PSDCod1                             overlay(SrvInf:*next)
     d                                     like(sv107a)
     d PSDCod2                             overlay(SrvInf:*next)
     d                                     like(sv107b)
     d PSDCod3                             overlay(SrvInf:*next)
     d                                     like(sv107c)
     d PSDCod4                             overlay(SrvInf:*next)
     d                                     like(sv107d)
     d PSAmnt2                             overlay(SrvInf:*next)
     d                                     like(sv108)
     d PSResCd                             overlay(SrvInf:*next)
     d                                     like(sv109)
     d PSMPrCd                             overlay(SrvInf:*next)
     d                                     like(sv110)
     d PSResCd2                            overlay(SrvInf:*next)
     d                                     like(sv111)
     d PSResCd3                            overlay(SrvInf:*next)
     d                                     like(sv112)
     d PSRvwCd                             overlay(SrvInf:*next)
     d                                     like(sv113)
     d PSRvwVal                            overlay(SrvInf:*next)
     d                                     like(sv114)
     d PSCStsCd                            overlay(SrvInf:*next)
     d                                     like(sv115)
     d PSAreaCd                            overlay(SrvInf:*next)
     d                                     like(sv116)
     d PSRefIdn                            overlay(SrvInf:*next)
     d                                     like(sv117)
     d PSPostCd                            overlay(SrvInf:*next)
     d                                     like(sv118)
     d PSAmnt3                             overlay(SrvInf:*next)
     d                                     like(sv119)
     d PSCareCd                            overlay(SrvInf:*next)
     d                                     like(sv120)
     d PSAgmtCd                            overlay(SrvInf:*next)
     d                                     like(sv121)
     d ISPrdId                             overlay(SrvInf:*next)
     d                                     like(sv201)
     d ISPrdIdQ                            overlay(SrvInf:*next)
     d                                     like(sv202a)
     d ISPrdId2                            overlay(SrvInf:*next)
     d                                     like(sv202b)
     d ISMod1                              overlay(SrvInf:*next)
     d                                     like(sv202c)
     d ISMod2                              overlay(SrvInf:*next)
     d                                     like(sv202d)
     d ISMod3                              overlay(SrvInf:*next)
     d                                     like(sv202e)
     d ISMod4                              overlay(SrvInf:*next)
     d                                     like(sv202f)
     d ISDesc                              overlay(SrvInf:*next)
     d                                     like(sv202g)
     d ISPrdId3                            overlay(SrvInf:*next)
     d                                     like(sv202h)
     d ISAmnt                              overlay(SrvInf:*next)
     d                                     like(sv203)
     d ISUnit                              overlay(SrvInf:*next)
     d                                     like(sv204)
     d ISQty                               overlay(SrvInf:*next)
     d                                     like(sv205)
     d ISURate                             overlay(SrvInf:*next)
     d                                     like(sv206)
     d ISResCd                             overlay(SrvInf:*next)
     d                                     like(sv208)
     d ISNurCd                             overlay(SrvInf:*next)
     d                                     like(sv209)
     d ISCareCd                            overlay(SrvInf:*next)
     d                                     like(sv210)
     d DSPrdIdQ                            overlay(SrvInf:*next)
     d                                     like(sv301a)
     d DSPrdId                             overlay(SrvInf:*next)
     d                                     like(sv301b)
     d DSMod1                              overlay(SrvInf:*next)
     d                                     like(sv301c)
     d DSMod2                              overlay(SrvInf:*next)
     d                                     like(sv301d)
     d DSMod3                              overlay(SrvInf:*next)
     d                                     like(sv301e)
     d DSMod4                              overlay(SrvInf:*next)
     d                                     like(sv301f)
     d DSDesc                              overlay(SrvInf:*next)
     d                                     like(sv301g)
     d DSPrdId2                            overlay(SrvInf:*next)
     d                                     like(sv301h)
     d DSAmnt                              overlay(SrvInf:*next)
     d                                     like(sv302)
     d DSFacCd                             overlay(SrvInf:*next)
     d                                     like(sv303)
     d DSCavit                             overlay(SrvInf:*next)
     d                                     like(sv304)
     d DSInlay                             overlay(SrvInf:*next)
     d                                     like(sv305)
     d DSQty                               overlay(SrvInf:*next)
     d                                     like(sv306)
     d DSDesc2                             overlay(SrvInf:*next)
     d                                     like(sv307)
     d DSStsCd                             overlay(SrvInf:*next)
     d                                     like(sv308)
     d DSAgmtCd                            overlay(SrvInf:*next)
     d                                     like(sv309)
     d DSResCd                             overlay(SrvInf:*next)
     d                                     like(sv310)
     d DSDCod                              overlay(SrvInf:*next)
     d                                     like(sv311)
     d DRRfIdn                             overlay(SrvInf:*next)
     d                                     like(sv401)
     d DRPrdIdQ                            overlay(SrvInf:*next)
     d                                     like(sv402a)
     d DRPrdId                             overlay(SrvInf:*next)
     d                                     like(sv402b)
     d DRMod1                              overlay(SrvInf:*next)
     d                                     like(sv402c)
     d DRMod2                              overlay(SrvInf:*next)
     d                                     like(sv402d)
     d DRMod3                              overlay(SrvInf:*next)
     d                                     like(sv402e)
     d DRMod4                              overlay(SrvInf:*next)
     d                                     like(sv402f)
     d DRDesc                              overlay(SrvInf:*next)
     d                                     like(sv402g)
     d DRPrdId2                            overlay(SrvInf:*next)
     d                                     like(sv402h)
     d DRRfIdn2                            overlay(SrvInf:*next)
     d                                     like(sv403)
     d DRResCd                             overlay(SrvInf:*next)
     d                                     like(sv404)
     d DRWrtCd                             overlay(SrvInf:*next)
     d                                     like(sv405)
     d DRSrvCd                             overlay(SrvInf:*next)
     d                                     like(sv406)
     d DROrgCd                             overlay(SrvInf:*next)
     d                                     like(sv407)
     d DRDesc2                             overlay(SrvInf:*next)
     d                                     like(sv408)
     d DRResCd2                            overlay(SrvInf:*next)
     d                                     like(sv409)
     d DRResCd3                            overlay(SrvInf:*next)
     d                                     like(sv410)
     d DRUnit                              overlay(SrvInf:*next)
     d                                     like(sv411)
     d DRCost                              overlay(SrvInf:*next)
     d                                     like(sv412)
     d DRSply                              overlay(SrvInf:*next)
     d                                     like(sv413)
     d DRDose                              overlay(SrvInf:*next)
     d                                     like(sv414)
     d DRStsCd                             overlay(SrvInf:*next)
     d                                     like(sv415)
     d DRLocCd                             overlay(SrvInf:*next)
     d                                     like(sv416)
     d DRCareCd                            overlay(SrvInf:*next)
     d                                     like(sv417)
     d DRAuthz                             overlay(SrvInf:*next)
     d                                     like(sv418)
     d SrDtTmQ                             overlay(SrvInf:*next)
     d                                     like(dtp01)
     d SrDTFmtQ                            overlay(SrvInf:*next)
     d                                     like(dtp02)
     d SrDTPrd                             overlay(SrvInf:*next)
     d                                     like(dtp03)
     d PrDtTmQ                             overlay(SrvInf:*next)
     d                                     like(dtp01)
     d PrDTFmtQ                            overlay(SrvInf:*next)
     d                                     like(dtp02)
     d PrDTPrd                             overlay(SrvInf:*next)
     d                                     like(dtp03)
     d PQQtyQ                              overlay(SrvInf:*next)
     d                                     like(qty01)
     d PQQty                               overlay(SrvInf:*next)
     d                                     like(qty02)
     d PQUnit                              overlay(SrvInf:*next)
     d                                     like(qty03)
     d PQFInfo                             overlay(SrvInf:*next)
     d                                     like(qty04)
     d CntrTyp                             overlay(SrvInf:*next)
     d                                     like(cn101)
     d CntrAmt                             overlay(SrvInf:*next)
     d                                     like(cn102)
     d DecFmt                              overlay(SrvInf:*next)
     d                                     like(cn103)
     d CntrRef1                            overlay(SrvInf:*next)
     d                                     like(cn104)
     d CntrRef2                            overlay(SrvInf:*next)
     d                                     like(cn105)
     d VerId                               overlay(SrvInf:*next)
     d                                     like(cn106)
     d RefIdQ                              overlay(SrvInf:*next)
     d                                     like(ref01)
     d RefIdn                              overlay(SrvInf:*next)
     d                                     like(ref02)
     d Desc                                overlay(SrvInf:*next)
     d                                     like(ref03)
     d RefId                               overlay(SrvInf:*next)
     d                                     like(ref04)
     d AmtQCd                              overlay(SrvInf:*next)
     d                                     like(amt01)
     d MAmount                             overlay(SrvInf:*next)
     d                                     like(amt02)
     d CrDbFlg                             overlay(SrvInf:*next)
     d                                     like(amt03)
     d AmtQCd2                             overlay(SrvInf:*next)
     d                                     like(amt01)
     d MAmount2                            overlay(SrvInf:*next)
     d                                     like(amt02)
     d CrDbFlg2                            overlay(SrvInf:*next)
     d                                     like(amt03)
     d AmtQCd3                             overlay(SrvInf:*next)
     d                                     like(amt01)
     d MAmount3                            overlay(SrvInf:*next)
     d                                     like(amt02)
     d CrDbFlg3                            overlay(SrvInf:*next)
     d                                     like(amt03)
     d*
     d** DS - Service Level Adjustments and Amounts
     d SrvAA           ds                  qualified
     d                                     inz
     d IdCode                              overlay(SrvAA)
     d                                     like(svd01)
     d MAmnt                               overlay(SrvAA:*next)
     d                                     like(svd02)
     d PrdIdQ                              overlay(SrvAA:*next)
     d                                     like(svd03a)
     d PrdId                               overlay(SrvAA:*next)
     d                                     like(svd03b)
     d PrMod1                              overlay(SrvAA:*next)
     d                                     like(svd03c)
     d PrMod2                              overlay(SrvAA:*next)
     d                                     like(svd03d)
     d PrMod3                              overlay(SrvAA:*next)
     d                                     like(svd03e)
     d PrMod4                              overlay(SrvAA:*next)
     d                                     like(svd03f)
     d Desc                                overlay(SrvAA:*next)
     d                                     like(svd03g)
     d PrdId2                              overlay(SrvAA:*next)
     d                                     like(svd03h)
     d PrdId3                              overlay(SrvAA:*next)
     d                                     like(svd04)
     d Qty                                 overlay(SrvAA:*next)
     d                                     like(svd05)
     d AsgNo                               overlay(SrvAA:*next)
     d                                     like(svd06)
     d ClGrpCd                             overlay(SrvAA:*next)
     d                                     like(cas01)
     d ClRsnCd                             overlay(SrvAA:*next)
     d                                     like(cas02)
     d ClMAmnt                             overlay(SrvAA:*next)
     d                                     like(cas03)
     d ClQty                               overlay(SrvAA:*next)
     d                                     like(cas04)
     d ClRsnCd2                            overlay(SrvAA:*next)
     d                                     like(cas05)
     d ClMAmnt2                            overlay(SrvAA:*next)
     d                                     like(cas06)
     d ClQty2                              overlay(SrvAA:*next)
     d                                     like(cas07)
     d ClRsnCd3                            overlay(SrvAA:*next)
     d                                     like(cas08)
     d ClMAmnt3                            overlay(SrvAA:*next)
     d                                     like(cas09)
     d ClQty3                              overlay(SrvAA:*next)
     d                                     like(cas10)
     d ClRsnCd4                            overlay(SrvAA:*next)
     d                                     like(cas11)
     d ClMAmnt4                            overlay(SrvAA:*next)
     d                                     like(cas12)
     d ClQty4                              overlay(SrvAA:*next)
     d                                     like(cas13)
     d ClRsnCd5                            overlay(SrvAA:*next)
     d                                     like(cas14)
     d ClMAmnt5                            overlay(SrvAA:*next)
     d                                     like(cas15)
     d ClQty5                              overlay(SrvAA:*next)
     d                                     like(cas16)
     d ClRsnCd6                            overlay(SrvAA:*next)
     d                                     like(cas17)
     d ClMAmnt6                            overlay(SrvAA:*next)
     d                                     like(cas18)
     d ClQty6                              overlay(SrvAA:*next)
     d                                     like(cas19)
     d AmtQCd                              overlay(SrvAA:*next)
     d                                     like(amt01)
     d MAmount                             overlay(SrvAA:*next)
     d                                     like(amt02)
     d CrDbFlg                             overlay(SrvAA:*next)
     d                                     like(amt03)
     d*
     d** DS - Transaction Set Trailer
     d TSetT           ds                  qualified
     d                                     inz
     d NbrSeg                              overlay(TSetT)
     d                                     like(se01)
     d CtrlNo                              overlay(TSetT:*next)
     d                                     like(se02)
     d*
     d** DS - Functional Group Control Trailer
     d FGrpT           ds                  qualified
     d                                     inz
     d NbrTS                               overlay(FGrpT)
     d                                     like(ge01)
     d CtrlNo                              overlay(FGrpT:*next)
     d                                     like(ge02)
     d*
     d** DS - IEA Interchange Control Trailer
     d IEA             ds                  qualified
     d                                     inz
     d NbrFG                               overlay(IEA)
     d                                     like(iea01)
     d CtrlNo                              overlay(IEA:*next)
     d                                     like(iea02)
     d*
     D** Standalone variables
     D*
     d qqco#           s              3
     d qqjst           s              2
     d qqotdt          s              8
     d qqottm          s              6
     d qqtset          s              3
     d*
     d ##Rtcd          s              1    inz
     d*
     d ##Wcif          s                   like(##rtcd)
     d ##Mcif          s                   like(##rtcd)
     d ##Wcin          s                   like(##rtcd)
     d ##Ar            s                   like(##rtcd)
     d ##Ntwk          s                   like(##rtcd)
     d ##Ur            s                   like(##rtcd)
     d ##Med           s                   like(##rtcd)
     d ##Edi           s                   like(##rtcd)
     d ##Wcdk          s                   like(##rtcd)
     d ##Rhab          s                   like(##rtcd)
     d ##Mss           s                   like(##rtcd)
     d ##Fsch          s                   like(##rtcd)
     d ##Prvb          s                   like(##rtcd)
     d*
     d kkco#           s                   like(lgco#)
     d kksub#          s                   like(lgsub#)
     d kkfnd           s                   like(lgfnd)
     d kkfyr           s                   like(lgfyr)
     d kkcase          s                   like(lgcase)
     d kkeob#          s                   like(lgeob#)
     d kkrcn           s                   like(lgrcn)
     d kklseq          s                   like(lglseq)
     d kkdseq          s                   like(dhdseq)
     d kkline          s                   like(dhline)
     d kkjst           s                   like(lgjst)
     d kkotdt          s                   like(lgotdt)
     d kkottm          s                   like(lgottm)
     d kktset          s              3
     d*
     d kkkey           s                   like(stkey)
     d*
     d curseg          s                   like(Fl837data)
     d*
     d GrpId           s              3  0 inz(0)
     d EmpId           s              3  0 inz(0)
     d ClmId           s              3  0 inz(0)
     d*
     d FirstRead       s               n   inz(*off)
     d HeaderWrt       s               n   inz(*off)
     d GrpChg          s               n   inz(*off)
     d EmpChg          s               n   inz(*off)
     d ClmChg          s               n   inz(*off)
     d BilChg          s               n   inz(*off)
     d*
     d nbrsegwrt       s              3  0 inz
     d nbrTSet         s              3  0 inz
     d nbrFGrp         s              3  0 inz
     d*
     d isa01           s              2    inz
     d isa02           s             10    inz
     d isa03           s              2    inz
     d isa04           s             10    inz
     d isa05           s              2    inz
     d isa06           s             12    inz
     d isa07           s              2    inz
     d isa08           s             12    inz
     d isa09           s              6    inz
     d isa10           s              4    inz
     d isa11           s              1    inz
     d isa12           s              3    inz
     d isa13           s              9    inz
     d isa14           s              1    inz
     d isa15           s              1    inz
     d isa16           s              1    inz
     d gs01            s              3    inz
     d gs02            s             12    inz
     d gs03            s             12    inz
     d gs04            s              8    inz
     d gs05            s              6    inz
     d gs06            s              9    inz
     d gs07            s              1    inz
     d gs08            s             15    inz
     d st01            s              3    inz
     d st02            s              9    inz
     d st03            s             15    inz
     d bht01           s              4    inz
     d bht02           s              2    inz
     d bht03           s              9    inz
     d bht04           s              8    inz
     d bht05           s              6    inz
     d bht06           s              2    inz
     d nm101           s              2    inz
     d nm102           s              1    inz
     d nm103           s             40    inz
     d nm104           s             40    inz
     d nm105           s             40    inz
     d nm106           s              4    inz
     d nm107           s              4    inz
     d nm108           s              2    inz
     d nm109           s             12    inz
     d nm110           s              1    inz
     d nm111           s              1    inz
     d nm112           s              1    inz
     d hl01            s              2    inz
     d hl02            s              2    inz
     d hl03            s              2    inz
     d hl04            s              2    inz
     d dtp01           s              3    inz
     d dtp02           s              3    inz
     d dtp03           s             17    inz
     d n401            s             40    inz
     d n402            s              2    inz
     d n403            s              9    inz
     d n404            s              3    inz
     d n405            s              1    inz
     d n406            s              1    inz
     d n407            s              1    inz
     d n301            s             40    inz
     d n302            s             40    inz
     d dmg01           s              3    inz
     d dmg02           s              8    inz
     d dmg03           s              1    inz
     d dmg04           s              1    inz
     d dmg05           s              1    inz
     d dmg06           s              1    inz
     d dmg07           s              1    inz
     d dmg08           s              1    inz
     d dmg09           s              1    inz
     d dmg10           s              1    inz
     d dmg11           s              1    inz
     d ref01           s              2    inz
     d ref02           s             50    inz
     d ref03           s              1    inz
     d ref04           s              1    inz
     d per01           s              2    inz
     d per02           s              1    inz
     d per03           s              2    inz
     d per04           s             10    inz
     d per05           s              1    inz
     d per06           s              1    inz
     d per07           s              1    inz
     d per08           s              1    inz
     d per09           s              1    inz
     d clm01           s             30    inz
     d clm02           s             12    inz
     d clm03           s              1    inz
     d clm04           s              1    inz
     d clm05a          s              4    inz
     d clm05b          s              2    inz
     d clm05c          s              1    inz
     d clm16           s              1    inz
     d clm17           s              1    inz
     d clm18           s              1    inz
     d clm19           s              2    inz
     d clm20           s              1    inz
     d cl101           s              1    inz
     d cl102           s              1    inz
     d cl103           s              1    inz
     d cl104           s              1    inz
     d cn101           s              2    inz
     d cn102           s              1    inz
     d cn103           s              1    inz
     d cn104           s              1    inz
     d cn105           s              1    inz
     d cn106           s              1    inz
     d amt01           s              2    inz
     d amt02           s             18    inz
     d amt03           s              1    inz
     d hi01a           s              3    inz
     d hi01b           s             30    inz
     d hi01c           s              3    inz
     d hi01d           s              1    inz
     d hi01e           s             12    inz
     d hi01f           s              1    inz
     d hi01g           s              1    inz
     d hi01h           s              1    inz
     d hi01i           s              1    inz
     d hi02            s              1    inz
     d hi03            s              1    inz
     d hi04            s              1    inz
     d hi05            s              1    inz
     d hi06            s              1    inz
     d hi07            s              1    inz
     d hi08            s              1    inz
     d hi09            s              1    inz
     d hi10            s              1    inz
     d hi11            s              1    inz
     d hi12            s              1    inz
     d se01            s              3    inz
     d se02            s              4    inz
     d ge01            s              3    inz
     d ge02            s              9    inz
     d iea01           s              3    inz
     d iea02           s              9    inz
     d prv01           s              2    inz
     d prv02           s              3    inz
     d prv03           s             50    inz
     d prv04           s              1    inz
     d prv05           s              1    inz
     d prv06           s              1    inz
     d sbr01           s              1    inz
     d sbr02           s              1    inz
     d sbr03           s              1    inz
     d sbr04           s              1    inz
     d sbr05           s              1    inz
     d sbr06           s              1    inz
     d sbr07           s              1    inz
     d sbr08           s              1    inz
     d sbr09           s              1    inz
     d svd01           s              2    inz
     d svd02           s             10    inz
     d svd03a          s              1    inz
     d svd03b          s              2    inz
     d svd03c          s              2    inz
     d svd03d          s              2    inz
     d svd03e          s              2    inz
     d svd03f          s              2    inz
     d svd03g          s              2    inz
     d svd03h          s              2    inz
     d svd04           s              3    inz
     d svd05           s              1    inz
     d svd06           s              1    inz
     d cas01           s              2    inz
     d cas02           s              3    inz
     d cas03           s             18    inz
     d cas04           s              1    inz
     d cas05           s              3    inz
     d cas06           s             10    inz
     d cas07           s              1    inz
     d cas08           s              3    inz
     d cas09           s             10    inz
     d cas10           s              1    inz
     d cas11           s              3    inz
     d cas12           s             10    inz
     d cas13           s              1    inz
     d cas14           s              3    inz
     d cas15           s             10    inz
     d cas16           s              1    inz
     d cas17           s              3    inz
     d cas18           s             10    inz
     d cas19           s              1    inz
     d lx01            s              9    inz
     d sv101a          s              2    inz
     d sv101b          s              5    inz
     d sv101c          s              2    inz
     d sv101d          s              2    inz
     d sv101e          s              2    inz
     d sv101f          s              2    inz
     d sv101g          s              1    inz
     d sv101h          s              1    inz
     d sv102           s             10    inz
     d sv103           s              2    inz
     d sv104           s              6    inz
     d sv105           s              1    inz
     d sv106           s              1    inz
     d sv107a          s              1    inz
     d sv107b          s              1    inz
     d sv107c          s              1    inz
     d sv107d          s              1    inz
     d sv108           s              1    inz
     d sv109           s              1    inz
     d sv110           s              1    inz
     d sv111           s              1    inz
     d sv112           s              1    inz
     d sv113           s              1    inz
     d sv114           s              1    inz
     d sv115           s              1    inz
     d sv116           s              1    inz
     d sv117           s              1    inz
     d sv118           s              1    inz
     d sv119           s              1    inz
     d sv120           s              1    inz
     d sv121           s              1    inz
     d sv201           s              3    inz
     d sv202a          s              2    inz
     d sv202b          s              5    inz
     d sv202c          s              2    inz
     d sv202d          s              2    inz
     d sv202e          s              2    inz
     d sv202f          s              2    inz
     d sv202g          s              1    inz
     d sv202h          s              1    inz
     d sv203           s             10    inz
     d sv204           s              2    inz
     d sv205           s              6    inz
     d sv206           s              1    inz
     d sv208           s              1    inz
     d sv209           s              1    inz
     d sv210           s              1    inz
     d sv301a          s              2    inz
     d sv301b          s              5    inz
     d sv301c          s              2    inz
     d sv301d          s              2    inz
     d sv301e          s              2    inz
     d sv301f          s              2    inz
     d sv301g          s              1    inz
     d sv301h          s              1    inz
     d sv302           s             10    inz
     d sv303           s              1    inz
     d sv304           s              1    inz
     d sv305           s              1    inz
     d sv306           s              1    inz
     d sv307           s              1    inz
     d sv308           s              1    inz
     d sv309           s              1    inz
     d sv310           s              1    inz
     d sv311           s              1    inz
     d sv401           s             13    inz
     d sv402a          s              2    inz
     d sv402b          s             11    inz
     d sv402c          s              2    inz
     d sv402d          s              2    inz
     d sv402e          s              2    inz
     d sv402f          s              2    inz
     d sv402g          s              1    inz
     d sv402h          s              1    inz
     d sv403           s             10    inz
     d sv404           s              1    inz
     d sv405           s              2    inz
     d sv406           s              1    inz
     d sv407           s              1    inz
     d sv408           s              1    inz
     d sv409           s              1    inz
     d sv410           s              1    inz
     d sv411           s              1    inz
     d sv412           s              1    inz
     d sv413           s              1    inz
     d sv414           s              1    inz
     d sv415           s              1    inz
     d sv416           s              1    inz
     d sv417           s              1    inz
     d sv418           s              1    inz
     d qty01           s              2    inz
     d qty02           s              6    inz
     d qty03           s              1    inz
     d qty04           s              1    inz
     d*
     d rd8frmdt        s              8    inz
     d rd8todt         s              8    inz
     d ICD10Ind        s               n   inz(*off)
     d loopidx         s              2  0 inz
     d ISACtlNo        s              9  0 inz
     d*
    C*
     c     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqjst
     C                   parm                    qqtset
     C                   parm                    qqotdt
     C                   parm                    qqottm
      /free
        //================================================================
        //mainline
        //================================================================

        // initialisation routine
         exsr srinit;

        // verify edi control flag value
         getsystem(##wcif:##mcif:##wcin:##ar:##ntwk:##ur:##med:
                   ##edi:##wcdk:##rhab:##mss:##fsch:##prvb);
         if ##edi <> 'Y';
           exsr pgmend;
         endif;

        // retrieve the edi control record
         clear kktset;
         chain(n) (kkco#:kkjst:kktset) pex01;
         if not %found(feexl);
           exsr pgmend;
         endif;

        // retrieve transmission management record
         kktset = '837';
         chain(n) (kkco#:kkjst:kktset:kkotdt:kkottm) pmf01;
         if not %found(femfl);
           exsr pgmend;
         endif;

        // if this is an original request (mfstat = a), then clear
        // kkotdt and kkottm to get new log records
         if mfstat = 'A';
           kkotdt = *zeros;
           kkottm = *zeros;
         endif;

        // process 837 log records

         setll (kkco#:kkjst:kkotdt:kkottm) plg01;
         reade (kkco#:kkjst:kkotdt:kkottm) plg01;

        // set first time read indicator on
         firstread = *on;
        // set the id counters to be used to determine parentage
         grpid = 0;
         empid = 0;
         clmid = 0;

         dow not %eof(felgl3);
           if (mfstat = 'R' or mfstat = 'A') and
              lgsts = 'EX';

             if firstread = *on;

              // save this record read
               logrecprv = logrec;

              // switch on header write indicator
               headerwrt = *on;

              // isa header
               exsr isaheader;

              // functional group header
               exsr funcgrphdr;

              // transaction set header
               exsr transethdr;

              // submitter informaion
               exsr sbminfo;

              // receiver informaion
               exsr rcvinfo;

             endif;

            // determine type of bill and read respective files
             kkco# = lgco#;
             kksub# = lgsub#;
             kkfnd = lgfnd;
             kkfyr = lgfyr;
             kkcase = lgcase;
             kkeob# = lgeob#;
             kkrcn = lgrcn;
             kklseq = lglseq;

             chain(n) (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#) peo01;
             if %found(faeolg);
               select;

               when eobtyp = 'N';
                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) peh01;

                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#) peh02;

               when eobtyp = 'H';
                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) peu01;

                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#) peu02;

               when eobtyp = 'X';
                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) pep01;

                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#) pep02;

               when eobtyp = 'D';
                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) ped01;

                chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#) ped02;

               endsl;
             endif;

            // determine what has changed in this record compared to last
            // a) insurer (group/fnd)
            // b) employer (employee)
            // c) claimant (case)

             grpchg = *off;
             empchg = *off;
             clmchg = *off;
             bilchg = *off;

             select;
               when firstread = *on;
                 grpchg = *on;
                 empchg = *off;
                 clmchg = *off;
                 bilchg = *off;
                 firstread = *off;
               when lgfnd <> logrecprv.lgfnd;
                 grpchg = *on;
                 empchg = *off;
                 clmchg = *off;
                 bilchg = *off;
               when lgemp# <> logrecprv.lgemp#;
                 grpchg = *off;
                 empchg = *on;
                 clmchg = *off;
                 bilchg = *off;
               when lgcase <> logrecprv.lgcase;
                 grpchg = *off;
                 empchg = *off;
                 clmchg = *on;
                 bilchg = *off;
               when lgeob# <> logrecprv.lgeob#;
                 grpchg = *off;
                 empchg = *off;
                 clmchg = *off;
                 bilchg = *on;
               other;
             endsl;

            // determine what all information (loops) should be written
            // the hierarchy is as below :
            // a) insurer (group/fnd)
            //    |
            // b) |_employer
            //    |
            // c) |___claimant (case)
            //    |
            // d) |_____bills

             if grpchg = *on;
               exsr newgrp;
             endif;

             if grpchg = *on or empchg = *on;
               exsr newemp;
             endif;

             if grpchg = *on or empchg = *on or
                clmchg = *on;
               exsr newclm;
             endif;

             if grpchg = *on or empchg = *on or
                clmchg = *on or bilchg = *on;
               exsr newbil;
             endif;

             logrecprv = logrec;

            // set status of log record
             if mfstat <> 'R';
               lgsts = 'SN';
               update plg01 %fields(lgsts);
             endif;

           endif;
           reade (kkco#:kkjst:kkotdt:kkottm) plg01;

         enddo;

        // if headers were written and processing done above, process below
         if headerwrt = *on;

           headerwrt = *off;

          // transaction set trailer
           exsr transettrl;

          // functional group trailer
           exsr funcgrptrl;

          // iea trailer
           exsr ieatrailer;

          // update transmission management record
           kktset = '837';
           chain (kkco#:kkjst:kktset:kkotdt:kkottm) pmf01;
           if %found(femfl);
             mfbch# = tseth.ctrlno;
             update pmf01 %fields (mfbch#);
           endif;

          // update record
           setgt kkco# pc101;
           readpe kkco# pc101;
           if not %eof(fmc1l);
             c1tsetid# += 1;
             update pc101 %fields (c1tsetid#);
           endif;

         endif;

         *inlr = *on;

        //==================================================================
        // end of mainline
        //==================================================================

        //================================================================
        // ISAHeader - process ISA interchange control header segment
        //================================================================
         begsr isaheader;

           clear isa;

          // get the values

           isa.autqual   = '00';
           isa.autinfo   = '          ';
           isa.secqual   = '00';
           isa.secinfo   = '          ';
           isa.intidq1   = 'ZZ';
           isa.intsndid  = exsein;
           isa.intidq2   = 'ZZ';
           isa.intrcvid  = exrein;
           isa.intdt     = %subst(%editc(cdateiso:'X'):3:6);
           isa.inttm     = %subst(%editc(ctimehms:'X'):1:4);
           isa.intctlid  = '^';
           isa.intctlvr  = '501';
           isactlno      = isactlno + 1;
           isa.intctlno  = %editc(isactlno:'X');
           isa.ackreq    = '1';
           if mftstf = 'Y';
             isa.usgind = 'T';
           else;
             isa.usgind = 'P';
           endif;
           isa.cmpsep    = ':';

          // build the segment to be written

           clear curseg;

           curseg = 'ISA' + aster;
           curseg = %trim(curseg) + %trim( isa.autqual ) + aster;
           curseg = %trim(curseg) +      ( isa.autinfo ) + aster;
           curseg = %trim(curseg) + %trim( isa.secqual ) + aster;
           curseg = %trim(curseg) +      ( isa.secinfo ) + aster;
           curseg = %trim(curseg) + %trim( isa.intidq1 ) + aster;
           curseg = %trim(curseg) + %trim( isa.intsndid) + aster;
           curseg = %trim(curseg) + %trim( isa.intidq2 ) + aster;
           curseg = %trim(curseg) + %trim( isa.intrcvid) + aster;
           curseg = %trim(curseg) + %trim( isa.intdt   ) + aster;
           curseg = %trim(curseg) + %trim( isa.inttm   ) + aster;
           curseg = %trim(curseg) + %trim( isa.intctlid) + aster;
           curseg = %trim(curseg) + %trim( isa.intctlvr) + aster;
           curseg = %trim(curseg) + %trim( isa.intctlno) + aster;
           curseg = %trim(curseg) + %trim( isa.ackreq  ) + aster;
           curseg = %trim(curseg) + %trim( isa.usgind  ) + aster;
           curseg = %trim(curseg) + %trim( isa.cmpsep  ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // FuncGrpHdr - process GS functional group control header segment
        //================================================================
         begsr funcgrphdr;

           clear fgrph;

          // get the values

           fgrph.idcode      = '837';
           fgrph.appsndcd    = isa.intsndid;
           fgrph.apprcvcd    = isa.intrcvid;
           fgrph.prepdt      = %editc(cdateiso:'X');
           fgrph.preptm      = %editc(ctimehms:'X');
           fgrph.resagycd    = 'X';

           setgt kkco# pc101;
           readpe(n) kkco# pc101;
           if not %eof(fmc1l);
             fgrph.ctrlno = %editc(c1tsetid#:'X');
           endif;

           kkkey = kkjst;
           chain(n) kkkey pst01;
           if %found(ststl1);
             fgrph.vriidcd  = stedirel;
           endif;

          // build the segment to be written

           clear curseg;

           curseg = 'FG' + aster;
           curseg = %trim(curseg) + %trim(fgrph.idcode  ) + aster;
           curseg = %trim(curseg) + %trim(fgrph.appsndcd) + aster;
           curseg = %trim(curseg) + %trim(fgrph.apprcvcd) + aster;
           curseg = %trim(curseg) + %trim(fgrph.prepdt  ) + aster;
           curseg = %trim(curseg) + %trim(fgrph.preptm  ) + aster;
           curseg = %trim(curseg) + %trim(fgrph.ctrlno  ) + aster;
           curseg = %trim(curseg) + %trim(fgrph.resagycd) + aster;
           curseg = %trim(curseg) + %trim(fgrph.vriidcd ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           nbrfgrp += 1;

         endsr;

        //================================================================
        // TranSetHdr - process transaction set header segment
        //================================================================
         begsr transethdr;

           clear tseth;

          // get the values

           setgt kkco# pc101;
           readpe(n) kkco# pc101;
           if not %eof(fmc1l);
             tseth.ctrlno = %editc(c1tsetid#:'X');
           endif;

           kkkey = kkjst;
           chain(n) kkkey pst01;
           if %found(ststl1);
             tseth.impconrf = stedirel;
           endif;

           tseth.idcode     = '837';
           tseth.hlstrcd    = '0080';
           tseth.purpcd     = '00';
           tseth.otrnidno   = %trim(tseth.ctrlno);
           tseth.trnsnddt   = %editc(cdateiso:'X');
           tseth.trnsndtm   = %editc(ctimehms:'X');
           tseth.trntypcd   = 'RP';

          // build the segment to be written

          // ST segment
           clear curseg;

           curseg = 'ST' + aster;
           curseg = %trim(curseg) + %trim( tseth.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( tseth.ctrlno  ) + aster;
           curseg = %trim(curseg) + %trim( tseth.impconrf) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // BHT segment
           clear curseg;

           curseg = 'BHT' + aster;
           curseg = %trim(curseg) + %trim( tseth.hlstrcd ) + aster;
           curseg = %trim(curseg) + %trim( tseth.purpcd  ) + aster;
           curseg = %trim(curseg) + %trim( tseth.otrnidno) + aster;
           curseg = %trim(curseg) + %trim( tseth.trnsnddt) + aster;
           curseg = %trim(curseg) + %trim( tseth.trnsndtm) + aster;
           curseg = %trim(curseg) + %trim( tseth.trntypcd) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           nbrtset += 1;

         endsr;

        //================================================================
        // SbmInfo - process submitter information segment
        // Loop Id : 1000A
        //================================================================
         begsr sbminfo;

           clear sbm;

          // get the values

           sbm.idcode       = '41';
           sbm.typqual      = '2';
           sbm.namel        = *blanks;
           sbm.namef        = *blanks;
           sbm.namem        = *blanks;
           sbm.namep        = *blanks;
           sbm.names        = *blanks;
           sbm.idqual       = '46';
           sbm.sndid        = exsein;

          // build the segment to be written

           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( sbm.idcode    ) + aster;
           curseg = %trim(curseg) + %trim( sbm.typqual   ) + aster;
           curseg = %trim(curseg) + %trim( sbm.namel     ) + aster;
           curseg = %trim(curseg) + %trim( sbm.namef     ) + aster;
           curseg = %trim(curseg) + %trim( sbm.namem     ) + aster;
           curseg = %trim(curseg) + %trim( sbm.namep     ) + aster;
           curseg = %trim(curseg) + %trim( sbm.names     ) + aster;
           curseg = %trim(curseg) + %trim( sbm.idqual    ) + aster;
           curseg = %trim(curseg) + %trim( sbm.sndid     ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // RcvInfo - process receiver information segment
        // Loop Id : 1000B
        //================================================================
         begsr rcvinfo;

           clear rcv;

          // get the values

           rcv.idcode       = '40';
           rcv.typqual      = '2';
           rcv.namel        = *blanks;
           rcv.namef        = *blanks;
           rcv.namem        = *blanks;
           rcv.namep        = *blanks;
           rcv.names        = *blanks;
           rcv.idqual       = '46';
           rcv.rcvid        = exrein;

          // build the segment to be written

           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( rcv.idcode    ) + aster;
           curseg = %trim(curseg) + %trim( rcv.typqual   ) + aster;
           curseg = %trim(curseg) + %trim( rcv.namel     ) + aster;
           curseg = %trim(curseg) + %trim( rcv.namef     ) + aster;
           curseg = %trim(curseg) + %trim( rcv.namem     ) + aster;
           curseg = %trim(curseg) + %trim( rcv.namep     ) + aster;
           curseg = %trim(curseg) + %trim( rcv.names     ) + aster;
           curseg = %trim(curseg) + %trim( rcv.idqual    ) + aster;
           curseg = %trim(curseg) + %trim( rcv.rcvid     ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // NewGrp - process when a group changes
        //================================================================
         begsr newgrp;

         // increment group id
           grpid += 1;

         // loop 2000a : insurer hierarchical level info
           exsr inshlinfo;

         // loop 2010aa : insurer/self insured info
           exsr insinfo;

         // loop 2010ab : claim admin info
           exsr clmadminfo;

         endsr;

        //================================================================
        // NewEmp - process when an employer changes
        //================================================================
         begsr newemp;

         // increment employer id
           empid += 1;

         // loop 2000b : employer hierarchical level info
           exsr emphlinfo;

         // loop 2010ba : employer info
           exsr empinfo;

         endsr;

        //================================================================
        // NewClm - process when a claimant changes
        //================================================================
         begsr newclm;

         // increment claimant id
           clmid += 1;

         // loop 2000c : claimant hierarchical level info
           exsr clmhlinfo;

         // loop 2010ca : claimant info
           exsr clminfo;

         endsr;

        //================================================================
        // NewBil - process when a bill changes
        //================================================================
         begsr newbil;

           select;

             when eobtyp = 'N';
              chain(n)
              (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) pdh01;
              kkdseq = dhdseq;
              kkline = dhline;

             when eobtyp = 'H';
              chain(n)
              (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) pdu01;
              kkdseq = dudseq;
              kkline = duline;

             when eobtyp = 'X';
              chain(n)
              (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) pdp01;
              kkdseq = dpdseq;
              kkline = dpline;

             when eobtyp = 'D';
              chain(n)
              (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn:kklseq) pdd01;
              kkdseq = dddseq;
              kkline = ddline;

           endsl;

           chain(n) (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn) pbl01;

           chain(n) (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkdseq) pca01;

           chain(n)
           (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kklseq:kkline) pdp01;

           chain(n)
           (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kklseq) pco01;

           chain(n) (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkrcn) ppr01;

         // loop 2300 : billing info
           exsr bilinfo;

         // loop 2310a : billing provider info
           exsr bilprov;

         // loop 2310b : rendering bill provider info
           exsr rndprov;

         // loop 2310d : service facility location info
           exsr srvlocinfo;

         // loop 2310e : referring provider info
           exsr refprov;

         // loop 2320 : bill level adjustments and amounts
           exsr biladj;

         // loop 2400 : service line info
           exsr srvinfo;

         // loop 2430 : service line adjustments and amounts
           exsr srvadj;

         endsr;

        //================================================================
        // InsHLInfo - process insurer hl info
        // Loop Id : 2000A
        //================================================================
         begsr inshlinfo;

           clear inhl;

          // get the values

           inhl.idno        = %char(grpid);
           inhl.parentid    = *blanks;
           inhl.levelcd     = '20';
           inhl.childcd     = '1';
           inhl.dttmq       = '582';
           inhl.dttmfmtq    = 'RD8';
           monitor;
             rd8frmdt = %char(%date(mfrfdt:*cymd):*iso0);
           on-error;
             rd8frmdt = '00010101';
           endmon;
           monitor;
             rd8todt = %char(%date(mfrtdt:*cymd):*iso0);
           on-error;
             rd8todt = '00010101';
           endmon;
           inhl.dttmprd     = rd8frmdt + '-' + rd8todt;

          // HL segment

           // build the segment to be written
           clear curseg;

           curseg = 'HL' + aster;
           curseg = %trim(curseg) + %trim( inhl.idno     ) + aster;
           curseg = %trim(curseg) + %trim( inhl.parentid ) + aster;
           curseg = %trim(curseg) + %trim( inhl.levelcd  ) + aster;
           curseg = %trim(curseg) + %trim( inhl.childcd  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // DTP segment

           // build the segment to be written
           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( inhl.dttmq    ) + aster;
           curseg = %trim(curseg) + %trim( inhl.dttmfmtq ) + aster;
           curseg = %trim(curseg) + %trim( inhl.dttmprd  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // InsInfo - process insurer/self-insured info
        // Loop Id : 2010AA
        //================================================================
         begsr insinfo;

           clear ininf;

          // NM1 segment

           // get the values

           ininf.idcode = 'CA';
           if not %open(wdf2l);
             open wdf2l;
           endif;
           chain(n) (kkco#:kkfnd) pf201;
           if %found(wdf2l);
             if f2tpa = 'Y';
               ininf.idcode = 'IR';
             endif;
           endif;
           if %open(wdf2l);
             close wdf2l;
           endif;

           ininf.typqual    = '2';
           ininf.namel      = lginnm;
           ininf.namef      = *blanks;
           ininf.namem      = *blanks;
           ininf.namep      = *blanks;
           ininf.names      = *blanks;
           ininf.idqual     = 'FI';
           ininf.fein       = lginfe;
           ininf.rshpcd     = *blanks;
           ininf.idcd       = *blanks;
           ininf.orgnml     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( ininf.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( ininf.typqual ) + aster;
           curseg = %trim(curseg) + %trim( ininf.namel   ) + aster;
           curseg = %trim(curseg) + %trim( ininf.namef   ) + aster;
           curseg = %trim(curseg) + %trim( ininf.namem   ) + aster;
           curseg = %trim(curseg) + %trim( ininf.namep   ) + aster;
           curseg = %trim(curseg) + %trim( ininf.names   ) + aster;
           curseg = %trim(curseg) + %trim( ininf.idqual  ) + aster;
           curseg = %trim(curseg) + %trim( ininf.fein    ) + aster;
           curseg = %trim(curseg) + %trim( ininf.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim( ininf.idcd    ) + aster;
           curseg = %trim(curseg) + %trim( ininf.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N4 segment

           // get the values

           ininf.citynm     = *blanks;
           ininf.stprvcd    = *blanks;
           ininf.postcd     = lginzp;
           ininf.ctrycd     = *blanks;
           ininf.locq       = *blanks;
           ininf.locid      = *blanks;
           ininf.subdvcd    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'N4' + aster;
           curseg = %trim(curseg) + %trim( ininf.citynm  ) + aster;
           curseg = %trim(curseg) + %trim( ininf.stprvcd ) + aster;
           curseg = %trim(curseg) + %trim( ininf.postcd  ) + aster;
           curseg = %trim(curseg) + %trim( ininf.ctrycd  ) + aster;
           curseg = %trim(curseg) + %trim( ininf.locq    ) + aster;
           curseg = %trim(curseg) + %trim( ininf.locid   ) + aster;
           curseg = %trim(curseg) + %trim( ininf.subdvcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //==================================================================
        // ClmAdmInfo - process claim administrator info
        // Loop Id : 2010AB
        //==================================================================
         begsr clmadminfo;

           clear cladm;

          // get the values

           cladm.idcode     = 'CX';
           cladm.typqual    = '2';
           cladm.namel      = lgcanm;
           cladm.namef      = *blanks;
           cladm.namem      = *blanks;
           cladm.namep      = *blanks;
           cladm.names      = *blanks;
           cladm.idqual     = 'FI';
           cladm.fein       = lgcafe;
           cladm.rshpcd     = *blanks;
           cladm.idcd       = *blanks;
           cladm.orgnml     = *blanks;
           cladm.citynm     = *blanks;
           cladm.stprvcd    = *blanks;
           cladm.postcd     = lgcazp;
           cladm.ctrycd     = *blanks;
           cladm.locq       = *blanks;
           cladm.locid      = *blanks;
           cladm.subdvcd    = *blanks;

          // NM1 segment

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( cladm.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( cladm.typqual ) + aster;
           curseg = %trim(curseg) + %trim( cladm.namel   ) + aster;
           curseg = %trim(curseg) + %trim( cladm.namef   ) + aster;
           curseg = %trim(curseg) + %trim( cladm.namem   ) + aster;
           curseg = %trim(curseg) + %trim( cladm.namep   ) + aster;
           curseg = %trim(curseg) + %trim( cladm.names   ) + aster;
           curseg = %trim(curseg) + %trim( cladm.idqual  ) + aster;
           curseg = %trim(curseg) + %trim( cladm.fein    ) + aster;
           curseg = %trim(curseg) + %trim( cladm.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim( cladm.idcd    ) + aster;
           curseg = %trim(curseg) + %trim( cladm.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N4 segment

           // build the segment to be written
           clear curseg;

           curseg = 'N4' + aster;
           curseg = %trim(curseg) + %trim( cladm.citynm  ) + aster;
           curseg = %trim(curseg) + %trim( cladm.stprvcd ) + aster;
           curseg = %trim(curseg) + %trim( cladm.postcd  ) + aster;
           curseg = %trim(curseg) + %trim( cladm.ctrycd  ) + aster;
           curseg = %trim(curseg) + %trim( cladm.locq    ) + aster;
           curseg = %trim(curseg) + %trim( cladm.locid   ) + aster;
           curseg = %trim(curseg) + %trim( cladm.subdvcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // EmpHLInfo - process employer hierarchical information
        // Loop Id : 2000B
        //================================================================
         begsr emphlinfo;

           clear emhl;

          // get the values

           emhl.idno        = %char(empid);
           emhl.parentid    = %char(grpid);
           emhl.levelcd     = 'EM';
           emhl.childcd     = '1';

          // build the segment to be written

           clear curseg;

           curseg = 'HL' + aster;
           curseg = %trim(curseg) + %trim( emhl.idno     ) + aster;
           curseg = %trim(curseg) + %trim( emhl.parentid ) + aster;
           curseg = %trim(curseg) + %trim( emhl.levelcd  ) + aster;
           curseg = %trim(curseg) + %trim( emhl.childcd  ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // EmpInfo - process employer info
        // Loop Id : 2010BA
        //================================================================
         begsr empinfo;

           clear eminf;

          // get the values

           eminf.idcode     = '36';
           eminf.typqual    = '2';
           eminf.namel      = lgemnm;
           eminf.namef      = *blanks;
           eminf.namem      = *blanks;
           eminf.namep      = *blanks;
           eminf.names      = *blanks;
           eminf.idqual     = 'FI';
           eminf.fein       = lgemfe;
           eminf.rshpcd     = *blanks;
           eminf.idcd       = *blanks;
           eminf.orgnml     = *blanks;
           eminf.addr1      = lgema1;
           eminf.addr2      = lgema2;
           eminf.citynm     = lgemct;
           eminf.stprvcd    = lgemst;
           eminf.postcd     = lgemzp;
           eminf.ctrycd     = lgerct;
           eminf.locq       = *blanks;
           eminf.locid      = *blanks;
           eminf.subdvcd    = *blanks;

          // NM1 segment

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( eminf.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( eminf.typqual ) + aster;
           curseg = %trim(curseg) + %trim( eminf.namel   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.namef   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.namem   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.namep   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.names   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.idqual  ) + aster;
           curseg = %trim(curseg) + %trim( eminf.fein    ) + aster;
           curseg = %trim(curseg) + %trim( eminf.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim( eminf.idcd    ) + aster;
           curseg = %trim(curseg) + %trim( eminf.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N3 segment

           // build the segment to be written
           clear curseg;

           curseg = 'N3' + aster;
           curseg = %trim(curseg) + %trim( eminf.addr1   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.addr2   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N4 segment

           // build the segment to be written
           clear curseg;

           curseg = 'N4' + aster;
           curseg = %trim(curseg) + %trim( eminf.citynm  ) + aster;
           curseg = %trim(curseg) + %trim( eminf.stprvcd ) + aster;
           curseg = %trim(curseg) + %trim( eminf.postcd  ) + aster;
           curseg = %trim(curseg) + %trim( eminf.ctrycd  ) + aster;
           curseg = %trim(curseg) + %trim( eminf.locq    ) + aster;
           curseg = %trim(curseg) + %trim( eminf.locid   ) + aster;
           curseg = %trim(curseg) + %trim( eminf.subdvcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // ClmHLInfo - process claimant hl info
        // Loop Id : 2000C
        //================================================================
         begsr clmhlinfo;

           clear clmhl;

          // get the values

           clmhl.idno        = %char(clmid);
           clmhl.parentid    = %char(empid);
           clmhl.levelcd     = 'CL';
           clmhl.childcd     = '0';
           clmhl.dttmq       = '558';
           clmhl.dttmfmtq    = 'D8';
           monitor;
             clmhl.dttmprd = %char(%date(lgacdt:*cymd):*iso0);
           on-error;
             clmhl.dttmprd = '00010101';
           endmon;

          // HL segment

           // build the segment to be written
           clear curseg;

           curseg = 'HL' + aster;
           curseg = %trim(curseg) + %trim( clmhl.idno     ) + aster;
           curseg = %trim(curseg) + %trim( clmhl.parentid ) + aster;
           curseg = %trim(curseg) + %trim( clmhl.levelcd  ) + aster;
           curseg = %trim(curseg) + %trim( clmhl.childcd  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // DTP segment

           // build the segment to be written
           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( clmhl.dttmq    ) + aster;
           curseg = %trim(curseg) + %trim( clmhl.dttmfmtq ) + aster;
           curseg = %trim(curseg) + %trim( clmhl.dttmprd  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // ClmInfo - process claimant info
        // Loop Id : 2010CA
        //================================================================
         begsr clminfo;

           clear clminf;

          // NM1 segment

           // get the values

           clminf.idcode     = 'CC';
           clminf.typqual    = '1';
           clminf.namel      = lgclln;
           clminf.namef      = lgclfn;
           clminf.namem      = lgclmi;
           clminf.namep      = *blanks;
           clminf.names      = lgclsx;
           clminf.idqual     = '34';
           clminf.fein       = lgss#;
           clminf.rshpcd     = *blanks;
           clminf.idcd       = *blanks;
           clminf.orgnml     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim(clminf.idcode  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.typqual ) + aster;
           curseg = %trim(curseg) + %trim(clminf.namel   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.namef   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.namem   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.namep   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.names   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.idqual  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.fein    ) + aster;
           curseg = %trim(curseg) + %trim(clminf.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.idcd    ) + aster;
           curseg = %trim(curseg) + %trim(clminf.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N3 segment

           // get the values

           clminf.addr1      = lgcla1;
           clminf.addr2      = lgcla2;

           // build the segment to be written
           clear curseg;

           curseg = 'N3' + aster;
           curseg = %trim(curseg) + %trim(clminf.addr1   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.addr2   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N4 segment

          // get the values

           clminf.citynm     = lgemct;
           clminf.stprvcd    = lgemst;
           clminf.postcd     = lgemzp;
           clminf.ctrycd     = lgecty;
           clminf.locq       = *blanks;
           clminf.locid      = *blanks;
           clminf.subdvcd    = *blanks;

          // build the segment to be written
           clear curseg;

           curseg = 'N4' + aster;
           curseg = %trim(curseg) + %trim(clminf.citynm  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.stprvcd ) + aster;
           curseg = %trim(curseg) + %trim(clminf.postcd  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.ctrycd  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.locq    ) + aster;
           curseg = %trim(curseg) + %trim(clminf.locid   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.subdvcd ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // DMG segment

           // get the values

           monitor;
             clminf.dttmdob = %char(%date(lgdtob:*cymd):*iso0);
           on-error;
             clminf.dttmdob = '00010101';
           endmon;
           clminf.dttmprq    = 'D8';
           clminf.gndrcd     = lgsex;
           clminf.marsts     = *blanks;
           clminf.racecd     = *blanks;
           clminf.ctzcd      = *blanks;
           clminf.ctrycd2    = *blanks;
           clminf.verfycd    = *blanks;
           clminf.quanty     = *blanks;
           clminf.cdlsqcd    = *blanks;
           clminf.indstcd    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'DMG' + aster;
           curseg = %trim(curseg) + %trim(clminf.dttmprq ) + aster;
           curseg = %trim(curseg) + %trim(clminf.dttmdob ) + aster;
           curseg = %trim(curseg) + %trim(clminf.gndrcd  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.marsts  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.racecd  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.ctzcd   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.ctrycd2 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.verfycd ) + aster;
           curseg = %trim(curseg) + %trim(clminf.quanty  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.cdlsqcd ) + aster;
           curseg = %trim(curseg) + %trim(clminf.indstcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment 1 : claim administrator details

           // get the values

           clminf.refidq     = 'Y1';
           clminf.refidn     = lgcac#;
           clminf.desc       = *blanks;
           clminf.refid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(clminf.refidq  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.refidn  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.desc    ) + aster;
           curseg = %trim(curseg) + %trim(clminf.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment 2 : replacement claim details

           // get the values

           clminf.refidq2    = '9E';
           clminf.refidn2    = lgcac#;
           clminf.desc2      = *blanks;
           clminf.refid2     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(clminf.refidq2 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.refidn2 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.desc2   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.refid2  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment 3 : jurisdiction assigned details

           // get the values

           clminf.refidq3    = 'Y4';
           clminf.refidn3    = lgscl#;
           clminf.desc3      = *blanks;
           clminf.refid3     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(clminf.refidq3 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.refidn3 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.desc3   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.refid3  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // PER segment

           // get the values
           clminf.cfunccd    = 'CT';
           clminf.comnm      = *blanks;
           clminf.comnoq     = 'TE';
           clminf.comno      = %editc(lgcphn#:'X');
           clminf.comnoq2    = *blanks;
           clminf.comno2     = *blanks;
           clminf.comnoq3    = *blanks;
           clminf.comno3     = *blanks;
           clminf.inqref     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'PER' + aster;
           curseg = %trim(curseg) + %trim(clminf.cfunccd ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comnm   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comnoq  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comno   ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comnoq2 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comno2  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comnoq3 ) + aster;
           curseg = %trim(curseg) + %trim(clminf.comno3  ) + aster;
           curseg = %trim(curseg) + %trim(clminf.inqref  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // BilInfo - process bill info
        // Loop Id : 2300
        //================================================================
         begsr bilinfo;

           clear blinf;

          // CLM segment

           // get the values

           select;
           when eobtyp = 'N';
             blinf.idcode = ehacct;
           when eobtyp = 'H';
             blinf.idcode = euacct;
           when eobtyp = 'X';
             blinf.idcode = epacct;
           when eobtyp = 'D';
             blinf.idcode = edacct;
           endsl;

           blinf.totchrg    = %char(lgchrg);
           blinf.filind     = *blanks;
           blinf.clmtypcd   = *blanks;
           blinf.facpos     = %trim(blfac) + ':';
           blinf.faccdq     = blfqual;
           blinf.clfrqcd    = blcfreq;
           blinf.agrmtcd    = blpagr;
           blinf.clstscd    = *blanks;
           blinf.respcd     = *blanks;
           blinf.sbmrsncd   = lgmtc;
           blinf.dlyrsncd   = *blanks;

           // build the segment to be written

           clear curseg;

           curseg = 'CLM' + aster;
           curseg = %trim(curseg) + %trim( blinf.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.totchrg ) + aster;
           curseg = %trim(curseg) + %trim( blinf.filind  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.clmtypcd) + aster;
           curseg = %trim(curseg) + %trim( blinf.facpos  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.faccdq  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.clfrqcd ) + aster;
           curseg = %trim(curseg) + %trim( blinf.agrmtcd ) + aster;
           curseg = %trim(curseg) + %trim( blinf.clstscd ) + aster;
           curseg = %trim(curseg) + %trim( blinf.respcd  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.sbmrsncd) + aster;
           curseg = %trim(curseg) + %trim( blinf.dlyrsncd) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // DTP segments : repeats

           // DTP 1 : date insurer received bill

           blinf.dttmq      = '050';
           blinf.dttmfmtq   = 'D8';
           monitor;
             blinf.dttmprd = %char(%date(lgcrdt:*cymd):*iso0);
           on-error;
             blinf.dttmprd = '00010101';
           endmon;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // DTP 2 : date and time of admission

           blinf.dttmq      = '435';
           blinf.dttmfmtq   = 'D8';
           monitor;
             blinf.dttmprd = %char(%date(euaddt:*cymd):*iso0);
           on-error;
             blinf.dttmprd = '00010101';
           endmon;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // DTP 3 : date and time of discharge

           blinf.dttmq      = '096';
           blinf.dttmfmtq   = 'D8';
           monitor;
             blinf.dttmprd = %char(%date(eudcdt:*cymd):*iso0);
           on-error;
             blinf.dttmprd = '00010101';
           endmon;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // DTP 4 : service dates range

           blinf.dttmq      = '472';
           blinf.dttmfmtq   = 'RD8';
           blinf.dttmprd    = blsvdt;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // DTP 5 : date of prescription

           blinf.dttmq      = '471';
           blinf.dttmfmtq   = 'D8';
           monitor;
             blinf.dttmprd = %char(%date(dprxfd:*cymd):*iso0);
           on-error;
             blinf.dttmprd = '00010101';
           endmon;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // DTP 6 : date of bill

           blinf.dttmq      = '434';
           blinf.dttmfmtq   = 'D8';
           monitor;
             blinf.dttmprd = %char(%date(lgcsdt:*cymd):*iso0);
           on-error;
             blinf.dttmprd = '00010101';
           endmon;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // DTP 7 : date insurer paid bill

           blinf.dttmq      = '666';
           blinf.dttmfmtq   = 'D8';
           blinf.dttmprd    =  blpddt;

           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmq   ) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmfmtq) + aster;
           curseg = %trim(curseg) + %trim( blinf.dttmprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // CL1 segment

           if eobtyp = 'H' and
              (euicn = 'IM' or euicn = 'IP' or
               euicn = 'IR' or euicn = 'IS');

             blinf.admtypcd   = euadty;
             blinf.admsrccd   = *blanks;
             blinf.ptstscd    = *blanks;
             blinf.nurhmcd    = *blanks;

             clear curseg;

             curseg = 'CL1' + aster;
             curseg = %trim(curseg) + %trim( blinf.admtypcd) + aster;
             curseg = %trim(curseg) + %trim( blinf.admsrccd) + aster;
             curseg = %trim(curseg) + %trim( blinf.ptstscd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.nurhmcd ) + aster;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

          // CN1 segment

           blinf.cntrtyp    = euctcd;
           blinf.cntramt    = *blanks;
           blinf.decfmt     = *blanks;
           blinf.cntrref1   = *blanks;
           blinf.cntrref2   = *blanks;
           blinf.verid      = *blanks;

           clear curseg;

           curseg = 'CN1' + aster;
           curseg = %trim(curseg) + %trim( blinf.cntrtyp ) + aster;
           curseg = %trim(curseg) + %trim( blinf.cntramt ) + aster;
           curseg = %trim(curseg) + %trim( blinf.decfmt  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.cntrref1) + aster;
           curseg = %trim(curseg) + %trim( blinf.cntrref2) + aster;
           curseg = %trim(curseg) + %trim( blinf.verid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // AMT segment

           blinf.amtqcd     = 'TP';
           blinf.mamount    = %char(lgpaid);
           blinf.crdbflg    = *blanks;

           clear curseg;

           curseg = 'AMT' + aster;
           curseg = %trim(curseg) + %trim( blinf.amtqcd  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.mamount ) + aster;
           curseg = %trim(curseg) + %trim( blinf.crdbflg ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segments : repeats

           // REF 1 : unique bill id number

           blinf.refidq     = 'DD';
           blinf.refidn     = blblid;
           blinf.desc       = *blanks;
           blinf.refid      = *blanks;

           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim( blinf.refidq  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.refidn  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.desc    ) + aster;
           curseg = %trim(curseg) + %trim( blinf.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // REF 2 : record transmission tracking number

           blinf.refidq     = '2I';
           blinf.refidn     = blblid;
           blinf.desc       = *blanks;
           blinf.refid      = *blanks;

           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim( blinf.refidq  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.refidn  ) + aster;
           curseg = %trim(curseg) + %trim( blinf.desc    ) + aster;
           curseg = %trim(curseg) + %trim( blinf.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // HI segments : repeats

           // get icd10 indicator value
           icd10ind = *off;
           select;
           when eobtyp = 'D';
             if e1icdind = 'y';
               icd10ind = *on;
             endif;
           when eobtyp = 'N';
             if e2icdind = 'y';
               icd10ind = *on;
             endif;
           when eobtyp = 'H';
             if e4icdind = 'Y';
               icd10ind = *on;
             endif;
           endsl;

           // HI 1 : institutional bill principal diagnosis

           if eobtyp = 'H';

             if icd10ind = *on;
               blinf.hicdlsq    = 'ABK';
             else;
               blinf.hicdlsq    = 'BK:';
             endif;
             blinf.hiindcd    = bldia1;
             blinf.hidtfmq    = *blanks;
             blinf.hidtprd    = *blanks;
             blinf.himamnt    = *blanks;
             blinf.hiqty      = *blanks;
             blinf.hiverid    = *blanks;
             blinf.hiindcd2   = *blanks;
             blinf.hiadmres   = 'U';
             blinf.hlthcd1    = *blanks;
             blinf.hlthcd2    = *blanks;
             blinf.hlthcd3    = *blanks;
             blinf.hlthcd4    = *blanks;
             blinf.hlthcd5    = *blanks;
             blinf.hlthcd6    = *blanks;
             blinf.hlthcd7    = *blanks;
             blinf.hlthcd8    = *blanks;
             blinf.hlthcd9    = *blanks;
             blinf.hlthcd10   = *blanks;
             blinf.hlthcd11   = *blanks;

             clear curseg;

             curseg = 'HI' + aster;
             curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd1 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd2 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd3 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd4 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd5 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd6 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd7 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd8 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd9 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd10) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd11) + aster;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 2 : institutional bill admitting diagnosis

           if eobtyp = 'H' and
              (euicn = 'IM' or euicn = 'IP' or
               euicn = 'IR' or euicn = 'IS');

             if icd10ind = *on;
               blinf.hicdlsq    = 'ABJ';
             else;
               blinf.hicdlsq    = 'BJ:';
             endif;
             blinf.hiindcd    = bladdg;
             blinf.hidtfmq    = *blanks;
             blinf.hidtprd    = *blanks;
             blinf.himamnt    = *blanks;
             blinf.hiqty      = *blanks;
             blinf.hiverid    = *blanks;
             blinf.hiindcd2   = *blanks;
             blinf.hiadmres   = *blanks;
             blinf.hlthcd1    = *blanks;
             blinf.hlthcd2    = *blanks;
             blinf.hlthcd3    = *blanks;
             blinf.hlthcd4    = *blanks;
             blinf.hlthcd5    = *blanks;
             blinf.hlthcd6    = *blanks;
             blinf.hlthcd7    = *blanks;
             blinf.hlthcd8    = *blanks;
             blinf.hlthcd9    = *blanks;
             blinf.hlthcd10   = *blanks;
             blinf.hlthcd11   = *blanks;

             clear curseg;

             curseg = 'HI' + aster;
             curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd1 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd2 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd3 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd4 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd5 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd6 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd7 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd8 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd9 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd10) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd11) + aster;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 3 : institutional bill other diagnosis

           if eobtyp = 'H';

             // this segment consists of similar attributes for different
             // diagnostic codes. The segment is built in loop processing
             // each diagnostic code in differnt cycle of loop

             clear curseg;
             curseg = 'HI' + aster;
             loopidx = 1;

             for loopidx = 1 to 7;

               select;
               when loopidx = 1;
                 blinf.hiindcd    = bldia2;
               when loopidx = 2;
                 blinf.hiindcd    = bldia3;
               when loopidx = 3;
                 blinf.hiindcd    = bldia4;
               when loopidx = 4;
                 blinf.hiindcd    = bldia5;
               when loopidx = 5;
                 blinf.hiindcd    = bldia6;
               when loopidx = 6;
                 blinf.hiindcd    = bldia7;
               when loopidx = 7;
                 blinf.hiindcd    = bldia8;
               endsl;

               if icd10ind = *on;
                 blinf.hicdlsq    = 'ABF';
               else;
                 blinf.hicdlsq    = 'BF:';
               endif;
               blinf.hidtfmq    = *blanks;
               blinf.hidtprd    = *blanks;
               blinf.himamnt    = *blanks;
               blinf.hiqty      = *blanks;
               blinf.hiverid    = *blanks;
               blinf.hiindcd2   = *blanks;
               blinf.hiadmres   = 'U';

               // build segment
               curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;

             endfor;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 4 : outpatient reason for visit

           if eobtyp = 'H' and
              (euicn = 'HO' or euicn = 'HS' or
               euicn = 'AO' or euicn = 'AS');

             if icd10ind = *on;
               blinf.hicdlsq    = 'APR';
             else;
               blinf.hicdlsq    = 'PR:';
             endif;
             blinf.hiindcd    = bloutr1;
             blinf.hidtfmq    = *blanks;
             blinf.hidtprd    = *blanks;
             blinf.himamnt    = *blanks;
             blinf.hiqty      = *blanks;
             blinf.hiverid    = *blanks;
             blinf.hiindcd2   = *blanks;
             blinf.hiadmres   = *blanks;

             clear curseg;

             curseg = 'HI' + aster;
             curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 5 : non-institutional diagnosis codes

           if (eobtyp = 'N' or eobtyp = 'D') and
              (bldia1 <> *blanks or bldia2 <> *blanks or
               bldia3 <> *blanks or bldia4 <> *blanks);

             // this segment consists of similar attributes for different
             // diagnostic codes. The segment is built in loop processing
             // each diagnostic code in differnt cycle of loop

             clear curseg;
             curseg = 'HI' + aster;
             loopidx = 1;

             for loopidx = 1 to 4;

               if icd10ind = *on;
                 blinf.hicdlsq    = 'ABF';
               else;
                 blinf.hicdlsq    = 'BF:';
               endif;

               select;
               when loopidx = 1;
                 blinf.hiindcd    = bldia1;

                 if icd10ind = *on;
                   blinf.hicdlsq    = 'ABK';
                 else;
                   blinf.hicdlsq    = 'BK:';
                 endif;
               when loopidx = 2;
                 blinf.hiindcd    = bldia2;
               when loopidx = 3;
                 blinf.hiindcd    = bldia3;
               when loopidx = 4;
                 blinf.hiindcd    = bldia4;
               endsl;

               blinf.hidtfmq    = *blanks;
               blinf.hidtprd    = *blanks;
               blinf.himamnt    = *blanks;
               blinf.hiqty      = *blanks;
               blinf.hiverid    = *blanks;
               blinf.hiindcd2   = *blanks;
               blinf.hiadmres   = *blanks;

               // build segment
               curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;

             endfor;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 6 : institutional bill principal procedure

           if eobtyp = 'H' and
              (euicn = 'IM' or euicn = 'IP' or
               euicn = 'IR' or euicn = 'IS');

             if icd10ind = *on;
               blinf.hicdlsq    = 'BBR';
             else;
               blinf.hicdlsq    = 'BR:';
             endif;
             blinf.hiindcd    = blprc1;
             blinf.hidtfmq    = 'D8';
             blinf.dttmprd    =  blppdt1;
             blinf.hidtprd    = *blanks;
             blinf.himamnt    = *blanks;
             blinf.hiqty      = *blanks;
             blinf.hiverid    = *blanks;
             blinf.hiindcd2   = *blanks;
             blinf.hiadmres   = *blanks;
             blinf.hlthcd1    = *blanks;
             blinf.hlthcd2    = *blanks;
             blinf.hlthcd3    = *blanks;
             blinf.hlthcd4    = *blanks;
             blinf.hlthcd5    = *blanks;
             blinf.hlthcd6    = *blanks;
             blinf.hlthcd7    = *blanks;
             blinf.hlthcd8    = *blanks;
             blinf.hlthcd9    = *blanks;
             blinf.hlthcd10   = *blanks;
             blinf.hlthcd11   = *blanks;

             clear curseg;

             curseg = 'HI' + aster;
             curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd1 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd2 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd3 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd4 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd5 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd6 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd7 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd8 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd9 ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd10) + aster;
             curseg = %trim(curseg) + %trim( blinf.hlthcd11) + aster;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 7 : institutional bill other procedure

           if eobtyp = 'H' and
              (euicn = 'IM' or euicn = 'IP' or
               euicn = 'IR' or euicn = 'IS');

             // this segment consists of similar attributes for different
             // diagnostic codes. The segment is built in loop processing
             // each diagnostic code in differnt cycle of loop

             clear curseg;
             curseg = 'HI' + aster;
             loopidx = 1;

             for loopidx = 1 to 5;

               select;
               when loopidx = 1;
                 blinf.hiindcd    = blprc2;
                 blinf.hidtprd    = blppdt2;
               when loopidx = 2;
                 blinf.hiindcd    = blprc3;
                 blinf.hidtprd    = blppdt3;
               when loopidx = 3;
                 blinf.hiindcd    = blprc4;
                 blinf.hidtprd    = blppdt4;
               when loopidx = 4;
                 blinf.hiindcd    = blprc5;
                 blinf.hidtprd    = blppdt5;
               when loopidx = 5;
                 blinf.hiindcd    = blprc5;
                 blinf.hidtprd    = blppdt6;
               endsl;

               if icd10ind = *on;
                 blinf.hicdlsq    = 'BBQ';
               else;
                 blinf.hicdlsq    = 'BQ:';
               endif;

               blinf.hidtfmq    = 'D8';
               blinf.himamnt    = *blanks;
               blinf.hiqty      = *blanks;
               blinf.hiverid    = *blanks;
               blinf.hiindcd2   = *blanks;
               blinf.hiadmres   = *blanks;

               // build segment
               curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;

             endfor;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 8 : condition codes

           if eobtyp = 'H' and
              (euicn = 'IM' or euicn = 'IP' or
               euicn = 'IR' or euicn = 'IS');

             // this segment consists of similar attributes for different
             // diagnostic codes. The segment is built in loop processing
             // each diagnostic code in differnt cycle of loop

             clear curseg;
             curseg = 'HI' + aster;
             loopidx = 1;

             for loopidx = 1 to 7;

               select;
               when loopidx = 1;
                 blinf.hiindcd    = blcoc1;
               when loopidx = 2;
                 blinf.hiindcd    = blcoc2;
               when loopidx = 3;
                 blinf.hiindcd    = blcoc3;
               when loopidx = 4;
                 blinf.hiindcd    = blcoc4;
               when loopidx = 5;
                 blinf.hiindcd    = blcoc5;
               when loopidx = 6;
                 blinf.hiindcd    = blcoc6;
               when loopidx = 7;
                 blinf.hiindcd    = blcoc7;
               endsl;

               blinf.hicdlsq    = 'BG:';
               blinf.hidtfmq    = *blanks;
               blinf.hidtprd    = *blanks;
               blinf.himamnt    = *blanks;
               blinf.hiqty      = *blanks;
               blinf.hiverid    = *blanks;
               blinf.hiindcd2   = *blanks;
               blinf.hiadmres   = *blanks;

               // build segment
               curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
               curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
               curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;

             endfor;

             curseg = %trim(curseg) + tilde;

             // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

           // HI 9 : diagnosis related group info

           if eobtyp = 'H' and
              (euicn = 'IM' or euicn = 'IP' or
               euicn = 'IR' or euicn = 'IS');

             blinf.hicdlsq    = 'DR:';
             blinf.hiindcd    = bldrg#;
             blinf.hidtfmq    = *blanks;
             blinf.hidtprd    = *blanks;
             blinf.himamnt    = *blanks;
             blinf.hiqty      = *blanks;
             blinf.hiverid    = *blanks;
             blinf.hiindcd2   = bldrg#;
             blinf.hiadmres   = *blanks;

             clear curseg;

             curseg = 'HI' + aster;
             curseg = %trim(curseg) + %trim( blinf.hicdlsq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtfmq ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hidtprd ) + aster;
             curseg = %trim(curseg) + %trim( blinf.himamnt ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiqty   ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiverid ) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiindcd2) + aster;
             curseg = %trim(curseg) + %trim( blinf.hiadmres) + aster;

             curseg = %trim(curseg) + tilde;

            // write to extract file
             fl837data = curseg;
             exsr wrtextract;

           endif;

         endsr;

        //================================================================
        // BilProv - billing provider info
        // Loop Id : 2310A
        //================================================================
         begsr bilprov;

           clear blprv;

          // NM1 segment : billing provider name

           // get the values

           blprv.idcode     = '85';
           if prbfsn <> *blanks;
             blprv.typqual = '1';    // person
           else;
             blprv.typqual = '2';    // non person
           endif;
           blprv.namel      = prblsn;
           blprv.namef      = prbfsn;
           blprv.namem      = prbmid;
           blprv.namep      = *blanks;
           blprv.names      = *blanks;
           blprv.idqual     = 'XX';
           blprv.fein       = prbnpi;
           blprv.rshpcd     = *blanks;
           blprv.idcd       = *blanks;
           blprv.orgnml     = *blanks;

           // build the segment to be written

           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( blprv.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.typqual ) + aster;
           curseg = %trim(curseg) + %trim( blprv.namel   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.namef   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.namem   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.namep   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.names   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.idqual  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.fein    ) + aster;
           curseg = %trim(curseg) + %trim( blprv.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.idcd    ) + aster;
           curseg = %trim(curseg) + %trim( blprv.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // NM1 segment : billing provider specialty info

           // get the values

           blprv.prcd       = 'BI';
           blprv.prrefidq   = 'PXC';
           blprv.prrefidn   = prbtaxo;
           blprv.prstcd     = *blanks;
           blprv.prspcinf   = *blanks;
           blprv.prorgcd    = *blanks;

           // build the segment to be written

           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( blprv.prcd    ) + aster;
           curseg = %trim(curseg) + %trim( blprv.prrefidq) + aster;
           curseg = %trim(curseg) + %trim( blprv.prrefidn) + aster;
           curseg = %trim(curseg) + %trim( blprv.prstcd  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.prspcinf) + aster;
           curseg = %trim(curseg) + %trim( blprv.prorgcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // NM1 segment : billing provider address

           // get the values

           blprv.addr1      = prbadr1;
           blprv.addr2      = prbadr2;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim( blprv.addr1   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.addr2   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // N4 segment

           // get the values

           blprv.citynm     = prbcty;
           blprv.stprvcd    = prbst;
           blprv.postcd     = prbzip;
           blprv.ctrycd     = prbcnty;
           blprv.locq       = *blanks;
           blprv.locid      = *blanks;
           blprv.subdvcd    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'N4' + aster;
           curseg = %trim(curseg) + %trim( blprv.citynm  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.stprvcd ) + aster;
           curseg = %trim(curseg) + %trim( blprv.postcd  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.ctrycd  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.locq    ) + aster;
           curseg = %trim(curseg) + %trim( blprv.locid   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.subdvcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment 1 : billing provider tax indentification number

           // get the values

           blprv.refidq     = 'TJ';
           blprv.refidn     = prbfein;
           blprv.desc       = *blanks;
           blprv.refid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim( blprv.refidq  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.refidn  ) + aster;
           curseg = %trim(curseg) + %trim( blprv.desc    ) + aster;
           curseg = %trim(curseg) + %trim( blprv.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment 2 : billing provider state license number

           // get the values

           blprv.refidq2    = '0B';
           blprv.refidn2    = prblic;
           blprv.desc2      = *blanks;
           blprv.refid2     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim( blprv.refidq2 ) + aster;
           curseg = %trim(curseg) + %trim( blprv.refidn2 ) + aster;
           curseg = %trim(curseg) + %trim( blprv.desc2   ) + aster;
           curseg = %trim(curseg) + %trim( blprv.refid2  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // RndProv - rendering bill provider info
        // Loop Id : 2310B
        //================================================================
         begsr rndprov;

           clear rndprv;

          // NM1 segment : rendering bill provider name

          // get the values

           rndprv.idcode     = '82';
           if prrfsn <> *blanks;
             rndprv.typqual = '1';    // person
           else;
             rndprv.typqual = '2';    // non person
           endif;
           rndprv.namel      = prrlsn;
           rndprv.namef      = prrfsn;
           rndprv.namem      = *blanks;
           rndprv.namep      = *blanks;
           rndprv.names      = prrlsf;
           rndprv.idqual     = 'XX';
           rndprv.fein       = prrnpi;
           rndprv.rshpcd     = *blanks;
           rndprv.idcd       = *blanks;
           rndprv.orgnml     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim(rndprv.idcode  ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.typqual ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.namel   ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.namef   ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.namem   ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.namep   ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.names   ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.idqual  ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.fein    ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.idcd    ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // NM1 segment : rendering bill provider specialty info

           // get the values

           rndprv.prcd       = 'PE';
           rndprv.prrefidq   = 'PXC';
           rndprv.prrefidn   = prrtaxo;
           rndprv.prstcd     = *blanks;
           rndprv.prspcinf   = *blanks;
           rndprv.prorgcd    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim(rndprv.prcd    ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.prrefidq) + aster;
           curseg = %trim(curseg) + %trim(rndprv.prrefidn) + aster;
           curseg = %trim(curseg) + %trim(rndprv.prstcd  ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.prspcinf) + aster;
           curseg = %trim(curseg) + %trim(rndprv.prorgcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment

           // get the values

           rndprv.refidq     = '0B';
           rndprv.refidn     = prrlic;
           rndprv.desc       = *blanks;
           rndprv.refid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(rndprv.refidq  ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.refidn  ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.desc    ) + aster;
           curseg = %trim(curseg) + %trim(rndprv.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // SrvLocInfo - service facility location info
        // Loop Id : 2310D
        //================================================================
         begsr srvlocinfo;

           clear srvloc;

          // NM1 segment : service facility location name

           // get the values

           srvloc.idcode     = '77';
           srvloc.typqual    = '2';
           srvloc.namel      = prflsn;
           srvloc.namef      = *blanks;
           srvloc.namem      = *blanks;
           srvloc.namep      = *blanks;
           srvloc.names      = *blanks;
           srvloc.idqual     = 'XX';
           srvloc.fein       = prfnpi;
           srvloc.rshpcd     = *blanks;
           srvloc.idcd       = *blanks;
           srvloc.orgnml     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim(srvloc.idcode  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.typqual ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.namel   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.namef   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.namem   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.namep   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.names   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.idqual  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.fein    ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.idcd    ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // NM1 segment : service facility location address

           // get the values

           srvloc.addr1      = prfadr1;
           srvloc.addr2      = prfadr2;

           // build the segment to be written
           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim(srvloc.addr1   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.addr2   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

           // N4 segment

           // get the values

           srvloc.citynm     = prfcty;
           srvloc.stprvcd    = prfst;
           srvloc.postcd     = prfzip;
           srvloc.ctrycd     = prfcnty;
           srvloc.locq       = *blanks;
           srvloc.locid      = *blanks;
           srvloc.subdvcd    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'N4' + aster;
           curseg = %trim(curseg) + %trim(srvloc.citynm  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.stprvcd ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.postcd  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.ctrycd  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.locq    ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.locid   ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.subdvcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment

           // get the values

           srvloc.refidq     = '0B';
           srvloc.refidn     = prflic;
           srvloc.desc       = *blanks;
           srvloc.refid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(srvloc.refidq  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.refidn  ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.desc    ) + aster;
           curseg = %trim(curseg) + %trim(srvloc.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // RefProv - referring provider info
        // Loop Id : 2310E
        //================================================================
         begsr refprov;

           clear refprv;

          // NM1 segment : referring provider name

           // get the values

           refprv.idcode     = 'DN';
           refprv.typqual    = '1';
           refprv.namel      = prrflsn;
           refprv.namef      = prrffsn;
           refprv.namem      = prrfmid;
           refprv.namep      = *blanks;
           refprv.names      = prrflsf;
           refprv.idqual     = 'XX';
           refprv.fein       = prrfnpi;
           refprv.rshpcd     = *blanks;
           refprv.idcd       = *blanks;
           refprv.orgnml     = *blanks;

           // build the segment to be written

           clear curseg;

           curseg = 'NM1' + aster;
           curseg = %trim(curseg) + %trim(refprv.idcode  ) + aster;
           curseg = %trim(curseg) + %trim(refprv.typqual ) + aster;
           curseg = %trim(curseg) + %trim(refprv.namel   ) + aster;
           curseg = %trim(curseg) + %trim(refprv.namef   ) + aster;
           curseg = %trim(curseg) + %trim(refprv.namem   ) + aster;
           curseg = %trim(curseg) + %trim(refprv.namep   ) + aster;
           curseg = %trim(curseg) + %trim(refprv.names   ) + aster;
           curseg = %trim(curseg) + %trim(refprv.idqual  ) + aster;
           curseg = %trim(curseg) + %trim(refprv.fein    ) + aster;
           curseg = %trim(curseg) + %trim(refprv.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim(refprv.idcd    ) + aster;
           curseg = %trim(curseg) + %trim(refprv.orgnml  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment

           // get the values

           refprv.refidq     = '0B';
           refprv.refidn     = prrflic;
           refprv.desc       = *blanks;
           refprv.refid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(refprv.refidq  ) + aster;
           curseg = %trim(curseg) + %trim(refprv.refidn  ) + aster;
           curseg = %trim(curseg) + %trim(refprv.desc    ) + aster;
           curseg = %trim(curseg) + %trim(refprv.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // BilAdj - bill level adjustments and amounts
        // Loop Id : 2320
        //================================================================
         begsr biladj;

           clear bilaa;

          // SBR segment : subscriber info

           // get the values

           bilaa.seqno       = 'P';
           bilaa.rshpcd      = *blanks;
           bilaa.refid       = *blanks;
           bilaa.name        = *blanks;
           bilaa.typcd       = *blanks;
           bilaa.benfcd      = *blanks;
           bilaa.respcd      = *blanks;
           bilaa.emstscd     = *blanks;
           bilaa.flindcd     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'SBR' + aster;
           curseg = %trim(curseg) + %trim( bilaa.seqno   ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.rshpcd  ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.refid   ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.name    ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.typcd   ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.benfcd  ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.respcd  ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.emstscd ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.flindcd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // CAS segment

           // get the values

           bilaa.clgrpcd     = lgbag1;
           bilaa.clrsncd     = lgbar1;
           bilaa.mamnt       = bladjamt1;
           bilaa.qty         = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'CAS' + aster;
           curseg = %trim(curseg) + %trim( bilaa.clgrpcd ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.clrsncd ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.mamnt   ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.qty     ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // AMT segment

           // get the values

           bilaa.amtqcd      = 'C4';
           bilaa.mamount     = blprpaid;
           bilaa.crdbflg     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'AMT' + aster;
           curseg = %trim(curseg) + %trim( bilaa.amtqcd  ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.mamount ) + aster;
           curseg = %trim(curseg) + %trim( bilaa.crdbflg ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // SrvInfo - service line information
        // Loop Id : 2400
        //================================================================
         begsr srvinfo;

           clear srvinf;

          // LX segment : service line info

           // get the values

           srvinf.asgno = %editc(calin#:'X');

           // build the segment to be written
           clear curseg;

           curseg = 'LX' + aster;
           curseg = %trim(curseg) + %trim(srvinf.asgno   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // SV1 segment : professional service info

           if (dhprcd >= 'A0001' and dhprcd <= 'Z9999');
             srvinf.psprdidq = 'HC';
           else;
             srvinf.psprdidq = 'ER';
           endif;
           srvinf.psprdid    = dhbprc;
           srvinf.psmod1     = *blanks;
           srvinf.psmod2     = *blanks;
           srvinf.psmod3     = *blanks;
           srvinf.psmod4     = *blanks;
           srvinf.psdesc     = *blanks;
           srvinf.psprdid2   = *blanks;
           srvinf.psamnt     = %char(dhchrg);
           if (dhprcd >= '00100' and dhprcd <= '01999');
             srvinf.psunit = 'MJ';
           else;
             srvinf.psunit = 'UN';
           endif;
           srvinf.psqty      = %char(dhunts);
           srvinf.psfaccd    = *blanks;
           srvinf.pssrvcd    = *blanks;
           srvinf.psdcod1    = dhdxr1;
           srvinf.psdcod2    = dhdxr2;
           srvinf.psdcod3    = dhdxr3;
           srvinf.psdcod4    = dhdxr4;
           srvinf.psamnt2    = *blanks;
           srvinf.psrescd    = *blanks;
           srvinf.psmprcd    = *blanks;
           srvinf.psrescd2   = *blanks;
           srvinf.psrescd3   = *blanks;
           srvinf.psrvwcd    = *blanks;
           srvinf.psrvwval   = *blanks;
           srvinf.pscstscd   = *blanks;
           srvinf.psareacd   = *blanks;
           srvinf.psrefidn   = *blanks;
           srvinf.pspostcd   = *blanks;
           srvinf.psamnt3    = *blanks;
           srvinf.pscarecd   = *blanks;
           srvinf.psagmtcd   = *blanks;

           // build the segment to be written

           clear curseg;

           curseg = 'SV1' + aster;
           curseg = %trim(curseg) + %trim(srvinf.psprdidq) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psprdid ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psmod1  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psmod2  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psmod3  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psmod4  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psdesc  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psprdid2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psamnt  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psunit  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psqty   ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psfaccd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pssrvcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psdcod1 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psdcod2 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psdcod3 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psdcod4 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psamnt2 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psrescd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psmprcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psrescd2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psrescd3) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psrvwcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psrvwval) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pscstscd) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psareacd) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psrefidn) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pspostcd) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psamnt3 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pscarecd) + aster;
           curseg = %trim(curseg) + %trim(srvinf.psagmtcd) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // SV2 segment : institutional service info

           // get the values

           srvinf.isprdid    = %editc(dubrvc:'X');
           if (dubrat >= 'A0001' and dhprcd <= 'Z9999');
             srvinf.isprdidq   = 'HC';
           else;
             srvinf.isprdidq   = 'ER';
           endif;
           srvinf.isprdid2   = dubrat;
           srvinf.ismod1     = dumod1;
           srvinf.ismod2     = dumod2;
           srvinf.ismod3     = dumod3;
           srvinf.ismod4     = dumod4;
           srvinf.isdesc     = *blanks;
           srvinf.isprdid3   = *blanks;
           srvinf.isamnt     = %char(duchrg);
           if dubrat <> *blanks;
             srvinf.isunit     = 'UN';
           else;
             srvinf.isunit     = 'DA';
           endif;
           srvinf.isqty      = %char(duunts);
           srvinf.isurate    = *blanks;
           srvinf.isrescd    = *blanks;
           srvinf.isnurcd    = *blanks;
           srvinf.iscarecd   = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'SV2' + aster;
           curseg = %trim(curseg) + %trim(srvinf.isprdid ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isprdidq) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isprdid2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.ismod1  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.ismod2  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.ismod3  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.ismod4  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isdesc  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isprdid3) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isamnt  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isunit  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isqty   ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isurate ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isrescd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.isnurcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.iscarecd) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // SV3 segment : dental service info

           // get the values

           if (ddprcd >= 'D0001' and ddprcd <= 'D9999');
             srvinf.dsprdidq = 'AD';
           else;
             srvinf.dsprdidq = 'HC';
           endif;
           srvinf.dsprdid    = ddbprc;
           srvinf.dsmod1     = *blanks;
           srvinf.dsmod2     = *blanks;
           srvinf.dsmod3     = *blanks;
           srvinf.dsmod4     = *blanks;
           srvinf.dsdesc     = *blanks;
           srvinf.dsprdid2   = *blanks;
           srvinf.dsamnt     = %char(ddchrg);
           srvinf.dsfaccd    = *blanks;
           srvinf.dscavit    = *blanks;
           srvinf.dsinlay    = *blanks;
           srvinf.dsqty      = *blanks;
           srvinf.dsdesc2    = *blanks;
           srvinf.dsstscd    = *blanks;
           srvinf.dsagmtcd   = *blanks;
           srvinf.dsrescd    = *blanks;
           srvinf.dsdcod     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'SV3' + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsprdidq) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsprdid ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsmod1  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsmod2  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsmod3  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsmod4  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsdesc  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsprdid2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsamnt  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsfaccd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dscavit ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsinlay ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsqty   ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsdesc2 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsstscd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsagmtcd) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsrescd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.dsdcod  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // SV4 segment : prescription drug service info

           // get the values

           srvinf.drrfidn    = dprx#;
           srvinf.drprdidq   = 'N4';
           srvinf.drprdid    = dpbndc;
           srvinf.drmod1     = *blanks;
           srvinf.drmod2     = *blanks;
           srvinf.drmod3     = *blanks;
           srvinf.drmod4     = *blanks;
           srvinf.drdesc     = *blanks;
           srvinf.drprdid2   = *blanks;
           srvinf.drrfidn2   = %char(dpchrg);
           srvinf.drrescd    = *blanks;
           srvinf.drwrtcd    = dprxdi;
           srvinf.drsrvcd    = *blanks;
           srvinf.drorgcd    = *blanks;
           srvinf.drdesc2    = *blanks;
           srvinf.drrescd2   = *blanks;
           srvinf.drrescd3   = *blanks;
           srvinf.drunit     = *blanks;
           srvinf.drcost     = *blanks;
           srvinf.drsply     = *blanks;
           srvinf.drdose     = *blanks;
           srvinf.drstscd    = *blanks;
           srvinf.drloccd    = *blanks;
           srvinf.drcarecd   = *blanks;
           srvinf.drauthz    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'SV4' + aster;
           curseg = %trim(curseg) + %trim(srvinf.drrfidn ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drprdidq) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drprdid ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drmod1  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drmod2  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drmod3  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drmod4  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drdesc  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drprdid2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drrfidn2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drrescd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drwrtcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drsrvcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drorgcd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drdesc2 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drrescd2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drrescd3) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drunit  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drcost  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drsply  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drdose  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drstscd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drloccd ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drcarecd) + aster;
           curseg = %trim(curseg) + %trim(srvinf.drauthz ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // DTP segment 1 : service dates

           // get the values

           srvinf.srdttmq    = '472';
           srvinf.srdtfmtq   = 'RD8';
           monitor;
             rd8frmdt = %char(%date(cafdat:*cymd):*iso0);
           on-error;
             rd8frmdt = '00010101';
           endmon;
           monitor;
             rd8todt = %char(%date(catdat:*cymd):*iso0);
           on-error;
             rd8todt = '00010101';
           endmon;
           srvinf.srdtprd    = rd8frmdt + '-' + rd8todt;

           // build the segment to be written
           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim(srvinf.srdttmq ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.srdtfmtq) + aster;
           curseg = %trim(curseg) + %trim(srvinf.srdtprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // DTP segment 2 : prescription dates

           // get the values

           srvinf.prdttmq    = '471';
           srvinf.prdtfmtq   = 'D8';
           monitor;
             srvinf.prdtprd = %char(%date(dprxfd:*cymd):*iso0);
           on-error;
             srvinf.prdtprd = '00010101';
           endmon;

           // build the segment to be written
           clear curseg;

           curseg = 'DTP' + aster;
           curseg = %trim(curseg) + %trim(srvinf.prdttmq ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.prdtfmtq) + aster;
           curseg = %trim(curseg) + %trim(srvinf.prdtprd ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // QTY segment : prescription quantity info

           // get the values

           if dprxdy <> 0;
             srvinf.pqqtyq = 'QB';
             srvinf.pqqty  = %char(dprxdy);
           else;
             srvinf.pqqtyq = 'SP';
             srvinf.pqqty  = %char(dpunts);
           endif;
           srvinf.pqunit     = *blanks;
           srvinf.pqfinfo    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'QTY' + aster;
           curseg = %trim(curseg) + %trim(srvinf.pqqtyq  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pqqty   ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pqunit  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.pqfinfo ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // CN1 segment : contract info

           // get the values

           srvinf.cntrtyp    = euctcd;
           srvinf.cntramt    = *blanks;
           srvinf.decfmt     = *blanks;
           srvinf.cntrref1   = *blanks;
           srvinf.cntrref2   = *blanks;
           srvinf.verid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'CN1' + aster;
           curseg = %trim(curseg) + %trim(srvinf.cntrtyp ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.cntramt ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.decfmt  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.cntrref1) + aster;
           curseg = %trim(curseg) + %trim(srvinf.cntrref2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.verid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // REF segment : treatment authorization no. per line of service

           // get the values

           srvinf.refidq     = 'G1';
           srvinf.refidn     = *blanks;
           srvinf.desc       = *blanks;
           srvinf.refid      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'REF' + aster;
           curseg = %trim(curseg) + %trim(srvinf.refidq  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.refidn  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.desc    ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.refid   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // AMT segment 1 : pharmacy dispensing fee paid amount

           // get the values

           srvinf.amtqcd     = 'D7';
           srvinf.mamount    = *blanks;
           srvinf.crdbflg    = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'AMT' + aster;
           curseg = %trim(curseg) + %trim(srvinf.amtqcd  ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.mamount ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.crdbflg ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // AMT segment 2 : pharmacy billed amount

           // get the values

           srvinf.amtqcd2    = 'PB';
           srvinf.mamount2   = %char(dpchrg);
           srvinf.crdbflg2   = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'AMT' + aster;
           curseg = %trim(curseg) + %trim(srvinf.amtqcd2 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.mamount2) + aster;
           curseg = %trim(curseg) + %trim(srvinf.crdbflg2) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // AMT segment 3 : line item tax charge amount

           // get the values

           srvinf.amtqcd3    = 'TJ';
           srvinf.mamount3   = dptax;
           srvinf.crdbflg3   = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'AMT' + aster;
           curseg = %trim(curseg) + %trim(srvinf.amtqcd3 ) + aster;
           curseg = %trim(curseg) + %trim(srvinf.mamount3) + aster;
           curseg = %trim(curseg) + %trim(srvinf.crdbflg3) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // SrvAdj - service line adjustments and amounts
        // Loop Id : 2430
        //================================================================
         begsr srvadj;

           clear srvaa;

          // SVD segment : service line adjudication

           // get the values

           srvaa.idcode = 'XX';
           select;
           when eobtyp = 'N';
             srvaa.mamnt = %char(dhpaid);
             select;
               when dhprcd >= 'D0001' and dhprcd <= 'D9999';
                 srvaa.prdid = 'AD';
               when dhprcd >= 'A0001' and dhprcd <= 'Z9999';
                 srvaa.prdid = 'HC';
               other;
                 srvaa.prdid = 'ER';
             endsl;
           when eobtyp = 'H';
             srvaa.mamnt = %char(dupaid);
             select;
               when durate >= 'D0001' and durate <= 'D9999';
                 srvaa.prdid = 'AD';
               when durate >= 'A0001' and durate <= 'Z9999';
                 srvaa.prdid = 'HC';
               other;
                 srvaa.prdid = 'ER';
             endsl;
           when eobtyp = 'X';
             srvaa.mamnt = %char(dppaid);
             srvaa.prdid  = 'N4';
           when eobtyp = 'D';
             srvaa.mamnt = %char(ddpaid);
             srvaa.prdid = 'AD';
           endsl;
           srvaa.prdidq = *blanks;
           select;
           when eobtyp = 'N';
             srvaa.prmod1 = dhmod1;
             srvaa.prmod2 = dhmod2;
             srvaa.prmod3 = dhmod3;
             srvaa.prmod4 = dhmod4;
           when eobtyp = 'H';
             srvaa.prmod1 = dumod1;
             srvaa.prmod2 = dumod2;
             srvaa.prmod3 = dumod3;
             srvaa.prmod4 = dumod4;
           endsl;
           srvaa.desc        = 'XX';
           srvaa.prdid2      = 'XX';
           if eobtyp = 'H';
             srvaa.prdid3 = %editc(durvcd:'X');
           endif;
           srvaa.qty         = *blanks;
           srvaa.asgno       = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'SVD' + aster;
           curseg = %trim(curseg) + %trim( srvaa.idcode  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.mamnt   ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prdidq  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prdid   ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prmod1  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prmod2  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prmod3  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prmod4  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.desc    ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prdid2  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.prdid3  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.qty     ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.asgno   ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // CAS segment

           // get the values

           srvaa.clgrpcd     = coslag1;
           select;
           when eobtyp = 'N';
             srvaa.clrsncd = dheob1;
             srvaa.clmamnt = %char(dhchrg - dhpaid);
           when eobtyp = 'H';
             srvaa.clrsncd = dueob1;
             srvaa.clmamnt = %char(duchrg - dupaid);
           when eobtyp = 'X';
             srvaa.clrsncd = dpeob1;
             srvaa.clmamnt = %char(dpchrg - dppaid);
           when eobtyp = 'D';
             srvaa.clrsncd = ddeob1;
             srvaa.clmamnt = %char(ddchrg - ddpaid);
           endsl;
           srvaa.clqty       = *blanks;
           srvaa.clrsncd2    = *blanks;
           srvaa.clmamnt2    = *blanks;
           srvaa.clqty2      = *blanks;
           srvaa.clrsncd3    = *blanks;
           srvaa.clmamnt3    = *blanks;
           srvaa.clqty3      = *blanks;
           srvaa.clrsncd4    = *blanks;
           srvaa.clmamnt4    = *blanks;
           srvaa.clqty4      = *blanks;
           srvaa.clrsncd5    = *blanks;
           srvaa.clmamnt5    = *blanks;
           srvaa.clqty5      = *blanks;
           srvaa.clrsncd6    = *blanks;
           srvaa.clmamnt6    = *blanks;
           srvaa.clqty6      = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'CAS' + aster;
           curseg = %trim(curseg) + %trim( srvaa.clgrpcd ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clrsncd ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clmamnt ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clqty   ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clrsncd2) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clmamnt2) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clqty2  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clrsncd3) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clmamnt3) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clqty3  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clrsncd4) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clmamnt4) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clqty4  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clrsncd5) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clmamnt5) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clqty5  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clrsncd6) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clmamnt6) + aster;
           curseg = %trim(curseg) + %trim( srvaa.clqty6  ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

          // AMT segment

           // get the values

           srvaa.amtqcd      = 'C4';
           setgt (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkdseq) pda01;
           readpe(n) (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkdseq) pda01;
           if %found(fddal2);
            // default with this value
             srvaa.mamount = %char(dapaid);
            // now check if there is a record for previous DARCN#
             if darcn# > 0;
               chain(n)
                (kkco#:kksub#:kkfnd:kkfyr:kkcase:kkeob#:kkdseq:darcn#-1)
                pda01;
               if %found(fddal2);
                 srvaa.mamount = %char(dapaid);
               endif;
             endif;
           endif;
           srvaa.crdbflg     = *blanks;

           // build the segment to be written
           clear curseg;

           curseg = 'AMT' + aster;
           curseg = %trim(curseg) + %trim( srvaa.amtqcd  ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.mamount ) + aster;
           curseg = %trim(curseg) + %trim( srvaa.crdbflg ) + aster;

           curseg = %trim(curseg) + tilde;

           // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // TranSetTrl - process transaction set trailer segment
        //================================================================
         begsr transettrl;

           clear tsett;

          // get the values

           tsett.nbrseg      = %char(nbrsegwrt+1);
           tsett.ctrlno      = 'ST02';

          // build the segment to be written

          // SE segment
           clear curseg;

           curseg = 'SE' + aster;
           curseg = %trim(curseg) + %trim( tsett.nbrseg   ) + aster;
           curseg = %trim(curseg) + %trim( tsett.ctrlno   ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // FuncGrpTrl - process GE functional group control trailer segment
        //================================================================
         begsr funcgrptrl;

           clear fgrpt;

          // get the values

           fgrpt.nbrts       = %char(nbrtset);
           fgrpt.ctrlno      = fgrph.ctrlno;

          // build the segment to be written

          // GE segment
           clear curseg;

           curseg = 'GE' + aster;
           curseg = %trim(curseg) + %trim( fgrpt.nbrts    ) + aster;
           curseg = %trim(curseg) + %trim( fgrpt.ctrlno   ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // IEATrailer - process IEA interchange control trailer segment
        //================================================================
         begsr ieatrailer;

           clear iea;

          // get the values

           iea.nbrfg         = %char(nbrfgrp);
           iea.ctrlno        = isa.intctlno;

          // build the segment to be written

          // IEA segment
           clear curseg;

           curseg = 'IEA' + aster;
           curseg = %trim(curseg) + %trim( iea.nbrfg    ) + aster;
           curseg = %trim(curseg) + %trim( iea.ctrlno   ) + aster;

           curseg = %trim(curseg) + tilde;

          // write to extract file
           fl837data = curseg;
           exsr wrtextract;

         endsr;

        //================================================================
        // WrtExtract - write to the extract file
        //================================================================
         begsr wrtextract;

           write p837fl;

         // count the number of segments written
           nbrsegwrt += 1;

         endsr;

        //================================================================
        // PgmEnd - end of progam subroutine
        //================================================================
         begsr pgmend;

           *inlr = *on;
           return;

         endsr;
        //================================================================
        // SrInit - initialization subroutine
        //================================================================
         begsr srinit;

          // process entry parameters
           kkco# = %dec(qqco#:3:0);
           kkjst = qqjst;
           kkotdt = %dec(%char(%date(qqotdt:*usa0):*cymd0):7:0);
           kkottm = %dec(qqottm:6:0);
           kktset = qqtset;

           dtime = %dec(%timestamp());

         endsr;
        //===============================================================
      /end-free
