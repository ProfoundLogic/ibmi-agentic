     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/Define CONVERT
      //****************************************************************
      //  F8193R - 837 Mapping from Holding Files to Prov Bills Files
      //****************************************************************
      //                                                               *
      //  Called from: Various                                         *
      //  Calls to   : None                                            *
      //                                                               *
      //****************************************************************
      //  Created on 10/20/2017 by C:079       Project: 114MJB         *
      //****************************************************************
      //  Revisions:                                                   *
      //                                                               *
      //  04/05/19 C:079 Support: 78417/114                            *
      //                 Use suffix when accessing claims/line records *
      //                                                               *
      //****************************************************************
      //  Indicators used:                                             *
      //                                                               *
      //****************************************************************
      //
      // Image Cross reference files
      //
     foptimptl1 if   e           k disk    usropn
      //
      // Holding Files
      //
     ffafghl    if   e           k disk
     ffahdrl    if   e           k disk
     ffaclmp    uf   e             disk
     f                                     INFDS(DBFDS1)
     ffaclml2   uf   e           k disk    Rename(pfaclm:pfaclm3)
     ffaclml    uf   e           k disk    Rename(pfaclm:pfaclm2)
     ffaclml7   uf   e           k disk    Rename(pfaclm:pfaclm4)
     ffalinl    uf   e           k disk
     ffalinl2   uf   e           k disk    Rename(pfalin:pfalin2)
      //
      // Header files
      //
     ffmeml1    if   e           k disk
     ffmcmp     if   e             disk
     ffmcml1    if   e           k disk    Rename(pcm01:pcm02)
     ffmcmls    if   e           k disk    Rename(pcm01:pcm03)
     ffmcml6    uf   e           k disk    Rename(pcm01:pcm04)
     ffaeop     uf   e             disk    Rename(peo01:peo02)
     ffaeol4    uf a e           k disk
     F                                     INFDS(DBFDS2)
     ffaeal1    uf a e           k disk
     ffaeal37   if   e           k disk
     F                                     Rename(pea01:pea02)
     fftbsl     if   e           k disk
     ffaesl2    if a e           k disk
     ffaedl     if a e           k disk
     ffaehl     if a e           k disk
     ffaeul     if a e           k disk
      //
      // Detail files
      //
     ffdddl     if a e           k disk
     ffddhl     if a e           k disk
     ffddul     if a e           k disk
      //
      // Miscellaneous files
      //
     ffdcal19   uf a e           k disk
     ffdcalf    if   e           k disk
     F                                     RENAME(pca01:pca02)
     ffmc1l     uf   e           k disk
     ffmcdl     uf   e           k disk
     ffmpdl     uf a e           k disk
     ffmp1l     uf a e           k disk
     fststl     if   e           k disk
     ffmbel     if   e           k disk
     ffmf1l     if   e           k disk
      //  Fee Schedule Base Control File
     fssylf     if   e           k disk
     f                                     ignore(ssy01)
     f                                     ignore(spc01)
     f                                     ignore(swc01)
     f                                     ignore(src01)
     f                                     ignore(sdr01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
     f                                     ignore(sgl01)
     f                                     ignore(sp201)
     fqsysprt1  o    f  132        Printer OflInd(*Inof)
     fqsysprt2  o    f  132        Printer OflInd(*Inoa) Formlen(60) Formofl(56)
      // copy structure
      /copy sprDate
      /copy fprProcCd
      // Program constants.
     d*        ========  Report Title lines  =========
     d Titl1           C                   Const('837 Bill Import -
     d                                     Error Listing')
     d Titl2           C                   Const('837 Mass Adjudication')
     d Titl3           C                   Const('837 Mass Adjudication -
     d                                     Summary Report')
     d AdjBillCK       pr                  ExtPgm('FA735R')
     d  xxRRN                         9
     d  xxCfMr                        1
     d  xxRRN          s              9
     d  xxCfMr         s              1    Inz('N')
     d Comapi          pr                  ExtPgm('FA007R')
     d  outCo#                        3  0
     d  outFnd                        3  0
     d  outCoNm                      40
     d  outFNam                      40
     d  outSub#                       5  0
     d  outFyr                        3  0
     d  outCo#         s              3  0
     d  outFnd         s              3  0
     d  outCoNm        s             40
     d  outFNam        s             40
     d  outSub#        s              5  0
     d  outFyr         s              3  0
     d WrtFmcmp        pr                  ExtPgm('FA003R')
     d  inRtCd_                       1
     d  inSS#_                        9
     d  inAcDt_                       7
     d  inCo#_                        3
     d  inSub#_                       5
     d  inFnd_                        3
     d  inFyr_                        3
     d  inCase_                       7
     d  inEmp#_                       9
     d  inDiv_                        5
     d  inAcTm_                       6
     d  inDtRp_                       7
     d  inDtLw_                       7
     d  inAcDs_                      60
     d  inmCon_                       1
     d  inEntD_                       7
     d  inRcvD_                       7
     d  inMMI_                        7
     d  in1A1_                        1
     d  in10A1_                      10
     d  inLtFl_                       1
     d  insCl#_                      25
     d  inMcTm_                       4
     d  inStWk_                       2
     d  inDeDt_                       7
     d  inDeEm_                       2
     d  inCmNu_                       3
     d  inCmSu_                       3
     d  inClAd_                       3
     d  inRRN_                        9  0
     d  inLsN_                       30
     d  inFsN_                       15
     d  inMid_                        1
     d  inDtRe_                       7
     d  inClSu_                       3
     d  in#Dep_                       3
     d  inOcDs_                      40
     d  inHiDt_                       7  0
     d  inEsts_                       2
     d  inWkPt_                       2
     d  inPrex_                       1
     d  inAcD2_                      60
     d  inCvFl_                       1
     d  inMCA_                        1
     d  inMMIp_                       5  2
     d  inOdis_                       1
     d  inYrLx_                       4  0
     d  inDbDt_                       7  0
     d  inHrDy_                       3  0
     d  inDyWk_                       1  0
     d  inClFa_                       3
     d  inDny_                        1
     d  inImp_                        1
     d  inJrSt_                       2
     d  inPost_                       1
     d  inRtCd         s              1
     d  inSS#          s              9
     d  inAcDt         s              7
     d  inCo#          s              3
     d  inSub#         s              5
     d  inFnd          s              3
     d  inFyr          s              3
     d  inCase         s              7
     d  inEmp#         s              9
     d  inDiv          s              5
     d  inAcTm         s              6
     d  inDtRp         s              7
     d  inDtLw         s              7
     d  inAcDs         s             60
     d  inmCon         s              1
     d  inEntD         s              7
     d  inRcvD         s              7
     d  inMMI          s              7
     d  in10A1         s             10
     d  in1A1          s              1
     d  inLtFl         s              1
     d  insCl#         s             25
     d  inMcTm         s              4
     d  inStWk         s              2
     d  inDeDt         s              7
     d  inDeEm         s              2
     d  inCmNu         s              3
     d  inCmSu         s              3
     d  inClAd         s              3
     d  inRRN          s              9  0
     d  inLsN          s             30
     d  inFsN          s             15
     d  inMid          s              1
     d  inDtRe         s              7
     d  inClSu         s              3
     d  in#Dep         s              3
     d  inOcDs         s             40
     d  inHiDt         s              7  0
     d  inEsts         s              2
     d  inWkPt         s              2
     d  inPrex         s              1
     d  inAcD2         s             60
     d  inCvFl         s              1
     d  inMCA          s              1
     d  inMMIp         s              5  2
     d  inOdis         s              1
     d  inYrLx         s              4  0
     d  inDbDt         s              7  0
     d  inHrDy         s              3  0
     d  inDyWk         s              1  0
     d  inClFa         s              3
     d  inDny          s              1
     d  inImp          s              1
     d  inJrSt         s              2
     d  inPost         s              1
     d WrtFmcdp        pr                  ExtPgm('FA002R')
     d  inRtCd_                       1
     d  inSS#_                        9
     d  inAcDt_                       7
     d  inCo#_                        3
     d  inSub#_                       5
     d  inFnd_                        3
     d  inFyr_                        3
     d  inCase_                       7
     d  inLsn_                       30
     d  inFsn_                       15
     d  inMid_                        1
     d  inAdr1_                      40
     d  inAdr2_                      40
     d  inCty_                       25
     d  inSt_                         2
     d  inZip_                        9
     d  inDtOb_                       7
     d  inSex_                        1
     d  inArea_                       3
     d  inPhn#_                       7
     d  inMsts_                       1
     d  inWara_                       3
     d  inWph#_                       7
     d  inCtry_                       3
     d  inAdr3_                      40
     d  inCara_                       3
     d  inCphn_                       7
     d  inAdr1         s             40
     d  inAdr2         s             40
     d  inCty          s             25
     d  inSt           s              2
     d  inZip          s              9
     d  inDtOb         s              7
     d  inSex          s              1
     d  inArea         s              3
     d  inPhn#         s              7
     d  inMsts         s              1
     d  inWara         s              3
     d  inWph#         s              7
     d  inCtry         s              3
     d  inAdr3         s             40
     d  inCara         s              3
     d  inCphn         s              7
      //================================================================
      // Standalone fields
      //================================================================
     d dtCCYYMMDD      s              8  0
     d sys_date        s               D   datfmt(*USA)
     d sys_time        s               T
     d xxKey           s             10A
     d xxDt8           s              8  0
     d xxDate          s              8  0
     d xxTime          s              6  0
     d xxLength        s              2  0
     d xxPos           s              2  0
     d xxClaim         s             35
     d xxClmErr        s              1n
     d xxProvErr       s              1n
     d xxErrorMsg      s            122
     d xxClaim#        s             50
     d SmChrg          s             11  2
     d SmMRA           s             11  2
     d SmOthr          s             11  2
     d SmOth2          s             11  2
     d SmNdis          s             11  2
     d SmPaid          s             11  2
     d  zzRRN          s              9  0
     d Title           s             40
     d SmEOBCnt        s              9  0 inz(0)
     d xxRcFl          s              1
     D outdesc         s             25
     D inRvCd          s              3
     D inMed#          s             10
      //================================================================
      //  User Defined data structures
      //================================================================
     d                 ds
     d  claim#                 1     20
     d  group                  1      3
     d  year                   4      6
     d  case                   7     13
     d                 ds
     d  kkCmSv                 1     11
     d  kkRvCd                 1      3    inz('000')
     d  kkHcPc                 4      8
     d  kkunts                 9     11    inz('000')

     d  EOclaim#       ds            15
     d   EOfnd
     d   EOdash1                      1    inz('-')
     d   EOfyr
     d   EOdash2                      1    inz('-')
     d   EOcase
      //
      //
     d                 ds
     d  value                  1     11
     d  sign                   1      1
     d  Amount                 2     11  2
     d pdds          esds                  extname(SZQ1P)
      //
     d dbfds1          ds
     d  DBrrn1               397    400b 0
      //
     d dbfds2          ds
     d  DBrrn2               397    400b 0
      * EOMB Batch Reprint Submit
     dChkImgr          pr                  ExtPgm('CHKIMGR')
     d isImageInst_                   1
     d isImageInst     s              1
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltIDx                    3
     d   outCltID      s              3
      //================================================================
      //  Plist definitions
      //================================================================
     c     *entry        Plist
     c                   Parm                    @@Mode            1
     c                   Parm                    @@BchCntl         9
     c                   Parm                    @@Rrn             9 0
     c                   Parm                    @@DcId           12
     c                   Parm                    @@Rrn2            9 0
     c                   Parm                    @@RtCd            1
      //================================================================
      //  Klist definitions
      //================================================================
      //
     c     beKy          Klist
     c                   Kfld                    EOast
     c                   Kfld                    EOsys
     c                   Kfld                    EObTyp
      //
     c     c1Ky          Klist
     c                   Kfld                    kkCo#
      //
     c     eobKy1        Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkSub#
     c                   Kfld                    kkFnd
     c                   Kfld                    kkFyr
     c                   Kfld                    kkCase
     c                   Kfld                    kkEob#
      //
     c     p1Ky          Klist
     C                   KFLD                    PDfein
     c                   Kfld                    PDmod
      //
     c     stKy          Klist
     c                   Kfld                    kkSt             10
      //
     c     opKy          Klist
     c                   Kfld                    kkSdoc
      //
      //
      //================================================================
      //  *Like DEFN statements
      //================================================================
      //
     c     *Like         Define    CMco#         kkCo#
     c     *Like         Define    CMsub#        kkSub#
     c     *Like         Define    CMfnd         kkFnd
     c     *Like         Define    CMfyr         kkFyr
     c     *Like         Define    CMcase        kkCase
     c     *Like         Define    EOeob#        kkEob#
     c     *Like         Define    EOss#         kkSs#
     C     *Like         DEFINE    PDfein        kkFein
     c     *Like         Define    PDmod         kkMod
     c     *Like         Define    CAdSeq        kkDseq
     c     *Like         Define    CAdSeq        XXDSEQ
     c     *Like         Define    EOast         XXAST
     c     *Like         Define    EOiDoc        kkSdoc
     c     *Like         Define    OPdcId        kkdcId
     c     *Like         Define    EOsys         xxSys
     c     *Like         Define    EObTyp        xxBtyp
     c     *Like         Define    CMss#         xxSS#
     c     *Like         Define    CMacDt        xxAcDt
     c     *Like         Define    CMfnd         xxFnd
     c     *Like         Define    CMfyr         xxFyr
     c     *Like         Define    CMcase        xxCase
     c     *Like         Define    CAlin#        kkLin#
     c     *Like         Define    CMsCl#        xxScl#
     c     *Like         Define    EOnDis        xxNdis
     c     *Like         Define    EOntwk        xxNtwk
     c     *Like         Define    CLMpAcct      kkPacct
       //
       // Execute Main Subroutine
       //
      /Free
       ExSr PgmStr;
       ExSr MainSr;
       ExSr PgmEnd;
       //
       // Subroutines
       //
       //================================================================
       // GetProv - Get the Provider
       //================================================================
       Begsr GetProv;
          xxProvErr = *off;
          If HDbpTaxId <> *Zeros;
             Monitor;
             kkFein = %Dec(HDbpTaxId:9:0);
             On-Error;
             xxProvErr = *on;
             xxErrorMsg = 'Billing Provider TaxID has invalid data:' +
                 %Subst(HDbpTaxId:1:12) + ' Patient Account#: ' + CLMpAcct;
             Exsr SrWrtRep;
             Endmon;
             Chain (kkFein:kkMod) ppd01;
             If Not %Found (fmpdl);
                xxProvErr = *on;
                xxErrorMsg = 'Provider not found:' + %Subst(HDbpTaxId:1:12)+
                   ' Patient Account#: ' + CLMpAcct;
                Exsr SrWrtRep;
             Endif;
          Else;
             xxProvErr = *on;
             xxErrorMsg = 'Provider not found:' + %Subst(HDbpTaxId:1:12) +
                 ' Patient Account#: ' + CLMpAcct;
             Exsr SrWrtRep;
          Endif;
       Endsr;
       //================================================================
       // GetClm - Get the Claim
       //================================================================
       Begsr GetClm;
          xxClmErr = *off;
          xxFnd = 0;
          xxFyr = 0;
          xxCase = 0;
          // If Image mode then called from Image Queue and claim RRN is
          // being passed in.
          Select;
          When @@Mode = 'I';
             Chain @@Rrn pcm01;
             If %Found(fmcmp);
                kkCo# = CMco#;
                kkSub#= CMsub#;
                kkFnd = CMfnd;
                kkFyr = CMfyr;
                kkCase= CMcase;
             Endif;
          //
          // If All mode then claim number may be in the holding files
          //
          When @@Mode = 'A';
             xxClaim# = *blanks;
             If CLMpatPcQu = 'Y4';
                xxClaim# = CLMpatClm#;
             Else;
                If CLMpiRiQ = '1W';
                   xxClaim# = CLMpiPcCn;
                Else;
                   xxClaim# = CLMsubSpi;
                Endif;
             Endif;
                Monitor;
                xxFnd = %Dec(%Subst(xxClaim#:1:3):3:0);
                On-Error;
                xxFnd = 0;
                Endmon;
                Monitor;
                xxFyr = %Dec(%Subst(xxClaim#:4:3):3:0);
                On-Error;
                xxFyr = 0;
                Endmon;
                Monitor;
                xxCase = %Dec(%Subst(xxClaim#:7:7):7:0);
                On-Error;
                xxCase = 0;
                Endmon;
             Chain (outCo#:outSub#:xxFnd:xxFyr:xxCase) pcm02;
             If %Found(fmcml1);
                kkCo# = CMco#;
                kkSub#= CMsub#;
                kkFnd = CMfnd;
                kkFyr = CMfyr;
                kkCase= CMcase;
          // Check by SS# and Accident Date
             Else;
                If CLMpiRiQ = 'SY';
                   xxSS# = CLMpiPcCn;
                   Monitor;
                   xxDt8 = %Dec(CLMaD:8:0);
                   On-Error;
                   xxDt8 = 0;
                   Endmon;
                   dtCCYYMMDD = dtMDYY2(xxDt8);
                   xxAcDt = dtCYMD(dtCCYYMMDD);
                   Chain (outCo#:xxSS#:xxAcDt) pcm03;
                   If %Found (fmcmls);
                      kkCo# = CMco#;
                      kkSub#= CMsub#;
                      kkFnd = CMfnd;
                      kkFyr = CMfyr;
                      kkCase= CMcase;
                   Else;
                      xxClmErr = *on;
                   Endif;
                Else;
                   xxClmErr = *on;
                Endif;
             Endif;
             If xxClmErr = *on;
                xxErrorMsg = 'Claim not found: ' + %Subst(xxClaim#:1:15) +
                      ' Patient account#: ' + CLMpAcct;
                Exsr SrWrtRep;
             Endif;
          When @@Mode = 'T';
             Chain (CLMco#:CLMsub#:CLMfnd:CLMfyr:CLMcase) pcm02;
             kkCo#  = CLMco#;
             kkSub# = CLMsub#;
             kkFnd  = CLMfnd;
             kkFyr  = CLMfyr;
             kkCase = CLMcase;
             kkEOB# = CLMeob#;
          Endsl;
       EndSr;
       //================================================================
       // CrtClm - Create the Claim (Mass Mode)
       //================================================================
       Begsr CrtClm;
          //
          // Check to see if the Claim Exists
          //
             if CLMpatClm# <> *blanks;
                xxScl#= CLMpatClm#;
             Else;
                if CLMpipCcn <> *blanks;
                   xxScl# = CLMpipCcn;
                Else;
                   xxScl# = CLMsubSpi;
                Endif;
             Endif;
             Chain (outCo#:outSub#:xxScl#) pcm04;
             If %Found(fmcml6);
                kkCo# = CMco#;
                kkSub#= CMsub#;
                kkFnd = CMfnd;
                kkFyr = CMfyr;
                kkCase= CMcase;
             Endif;
          //
          // If the Claim does not exist then create in the test group
          //
             If not %Found(fmcml6);
                // Get the next available case number in group 777
                kkCo# = outCo#;
                Chain c1Ky pc101;
                If %found(fmc1l);
                   inRtCd = ' ';
                   If CLMpiRiQ = 'SY';
                      inSS# = CLMpiPcCn;
                   Else;
                      Setgt *hival pcd01;
                      readp(n) pcd01;
                      If %Found(Fmcdl);
                         Monitor;
                         inSS# = %Char(%Dec(CDss#:9:0) + 1);
                         On-Error;
                         inSS# = *Zeros;
                         Endmon;
                      Endif;
                   Endif;
                   Monitor;
                   xxDt8 = %Dec(CLMaD:8:0);
                   On-Error;
                   xxDt8 = 0;
                   Endmon;
                   dtCCYYMMDD = dtMDYY2(xxDt8);
                   inAcDt = %Char(dtCYMD(dtCCYYMMDD));
                   inCo#  = %Char(outCo#);
                   Evalr inSub# = %Char(outSub#);
                   inFnd  = '777';
                   sys_date = %date();
                   inFyr = %Subst(%Char(Dateto7(sys_date)):1:3);
                   Evalr inCase = (%Char(C1LCS# + 1));
                   C1lCs# = C1lCs# + 1;
                   Update pc101;
                   Chain (outCo#:outSub#:777) pem01;
                   If %Found (fmeml1);
                      Evalr inEmp# = %Trim(%Char(EMemp#));
                      Evalr inDiv  = %Trim(%Char(EMdiv));
                   ENdif;
                   inAcTm = ' ';
                   inDtRp = ' ' ;
                   inDtLw = ' ' ;
                   inAcDs = ' ' ;
                   inMcon = ' ';
                   inEntD = ' ';
                   inRcvD = ' ';
                   inMMI  = ' ';
                   in1A1  = ' ';
                   in10A1 = ' ';
                   inLtFl = 'M';
                   inScl# = xxScl#;
                   inMcTm = ' ';
                   inStWk = ' ';
                   inDeDt = ' ';
                   inDeEm = ' ';
                   inCmNu = ' ';
                   inClAd = ' ';
                   inRrn  = 0;
                   If CLMsubIrc <> '18';
                      inLsN = CLMpatLsN;
                      infsN = CLMpatFsN;
                      inMid  = ' ';
                   Else;
                      inLsN = CLMsubLsn;
                      inFsn = CLMsubFsn;
                      inMid = CLMsubMid;
                   Endif;
                   inDtRe = ' ';
                   inClSu = ' ';
                   in#Dep = ' ';
                   inOcDs = ' ';
                   inHiDt = 0;
                   inEsts = ' ';
                   inWkPt = ' ';
                   inPrEx = ' ';
                   inAcD2 = ' ';
                   inCvFl = ' ';
                   inMca  = ' ';
                   inMmIp = 0;
                   inOdis = ' ';
                   inYrLx = 0;
                   inDbDt = 0;
                   inHrDy = 0;
                   inDYWk = 0;
                   inClFa = ' ';
                   inDny  = ' ';
                   inImp  = ' ';
                   inJrSt = CLMsubSt;
                   inPost = ' ';
                   Callp WrtFmcmp(inRtCd:inSS#:inAcDt:inCo#:inSub#:inFnd:
                          inFyr:inCase:inEmp#:inDiv:inAcTm:inDtRp:inDtLw:
                          inAcDs:inmCon:inEntD:inRcvD:inMMI:in1A1:in10A1:inLtFl:
                          inScl#:inMcTm:inStWk:inDeDt:inDeEm:inCmNu:inCmSu:
                          inClad:inRRN:inLsN:inFsN:inMid:inDtRe:inClSu:
                          in#Dep:inOcDs:inHiDt:inEsts:inWkPt:inPrex:inAcD2:
                          inCvFl:inMCA:inMMIp:inOdis:inYrLx:inDbDt:inHrDy:
                          inDyWk:inClFa:inDny:inImp:inJrSt:inPost);
                   if inRtCd = 'W';
                      kkFnd = %Dec(inFnd:3:0);
                      kkFyr = %Dec(inFyr:3:0);
                      kkCase = %Dec(inCase:7:0);
                      Chain (outCo#:outSub#:kkFnd:kkFyr:kkCase) pcm02;
                   Endif;
                   inAdr1 = CLMpatAdr1;
                   inAdr2 = CLMpatAdr2;
                   inCty  = CLMpatCty;
                   inSt   = CLMpatSt;
                   inZip  = CLMpatPost;
                   inDtoB = ' ';
                   inSex  = ' ';
                   inArea = ' ';
                   inPhn# = ' ';
                   inMsts = ' ';
                   inWara = ' ';
                   inWph# = ' ';
                   inCtry = ' ';
                   inAdr3 = ' ';
                   inCara = ' ';
                   inCphn = ' ';
                   Callp WrtFmcdp(inRtCd:inSS#:inAcDt:inCo#:inSub#:inFnd:
                          inFyr:inCase:inLsn:inFsn:inMid:inAdr1:inAdr2:
                          inCty:inSt:inZip:inDtOb:inSex:inArea:inPhn#:
                          inMsts:inWara:inWph#:inCtry:inAdr3:inCara:inCphn);
                Endif;
             Endif;
          //***** Need to find out what fields to use to determine this
          //
          // If the Claim does not exist then create in the test group
          //***** Need to find out what fields to use to create the claim
          //
       EndSr;
       //================================================================
       // CrtProv - Create the Provider (Mass Mode)
       //================================================================
       Begsr CrtProv;
          //
          // Check to see if the Provider exists and if not then create
          //
             Monitor;
             kkFein = %Dec(HDbpTaxId:9:0);
             kkMod = *Zeros;
             On-Error;
             kkFein = *zeros;
             Endmon;
             Chain (kkFein:kkmod) ppd01;
             If Not %Found(fmpdl) and HDBpTaxId <> *blanks;
                Monitor;
                PDfein = %Dec(HDBpTaxId:9:0);
                On-Error;
                PDfein = 0;
                Endmon;
                PDMod  = 0;
                PDname = HDbpNm;
                PDfsN  = HDbpFnm;
                PDadr1 = HDbpAdr1;
                PDadr2 = HDbpAdr2;
                PDcty = HDbpCty;
                PDst = HDbpSt;
                Monitor;
                PDzip = %Dec(HDbpPost:9:0);
                On-Error;
                PDzip = 0;
                Endmon;
                if xxBtyp = 'H';
                   PDtype = '4';
                Else;
                   PDtype = '1';
                Endif;
                PDparn = 'P';
                PDdbA1 = 'N';
                PDwcCe = 'N';
                PDstat = 'A';
                PDuser = Q1user;
                PDpgm  = Q1pgm;
                sys_time = %time();
                PDdt   = Dateto7(sys_date);
                PDtime = TimeTo6(SystemTime());
                Write ppd01;
              //
              // Write to Provider Auxilary File
              //
                P1fein = PDfein;
                P1Mod  = 0;
                P1name = PDname;
                P1fsN  = PDfsN;
                P1adr1 = PDadr1;
                P1adr2 = PDadr2;
                P1cty = PDcty;
                P1st = PDst;
                P1zip = PDzip;
                P1stat = 'A';
                P1user = Q1user;
                P1pgm  = Q1pgm;
                sys_time = %time();
                P1dt   = Dateto7(sys_date);
                P1time = TimeTo6(SystemTime());
                P1St = HDbpSt;
                Write pp101;
             Endif;
       EndSr;
       //================================================================
       // GetAst - Get the Adjudication State
       //================================================================
       Begsr GetAst;
       // Initially set UCR flag to S=Service code for hosp or C=CPT
          If xxBtyp = 'H';
             xxRcFl = 'S';
          Else;
             xxRcFl = 'C';
          Endif;
       // Determine adjudication state
       // Use the Claim State if Extra-Terrestrial
            xxAst = 'UC';
            kkSt = CMast;
            Chain stKy pst01;
            If %Found(ststl) and STinst = 'Y';
               xxAst = CMAST;
               If xxBtyp <> 'H';
               Select;
               When STfsTy = 'S';
                  xxRcFl = 'C';
               When STfsTy = 'U';
                  xxRcFl = 'Y';
               Other;
                  xxRcFl = 'C';
               Endsl;
               Endif;
       // Use the Provider State
            Else;
               Chain p1Ky pp101;
               If %Found(fmp1l) and P1st <> *Blanks;
                  xxKey = P1st;
                  Chain xxkey pst01;
                  If %Found(ststl);
                     xxAst = P1st;
                  Endif;
               Endif;
            Endif;
       Endsr;
       //================================================================
       // LoadKeys - Load keys for reading files
       //================================================================
       Begsr LoadKeys;
          Monitor;
          kkFein = %Dec(HDBpTaxId:9:0);
          On-Error;
          kkFein = 0;
          Endmon;
          kkMod  = 0;
       Endsr;
       //================================================================
       // MainSr - Main Subroutine
       //================================================================
       Begsr MainSr;
          Select;
        // Single EOB Mode with an image called from Image Queue
          When @@Mode = 'I';
        //** Chain @@Rrn faclmp;
        //** If %Found(faclmp);
        //
        // Get the 837 Header Record
        //
                clear pfahdr;
                Chain (CLMiCntl) fahdrl;
                ExSr LoadKeys;
                ExSr GetClm;
                ExSr ChkAm1;
                ExSr UpdFaclmp1;
                ExSr Writer;
                ExSr UpdFaclmp2;
                ExSr PendBill;
        //***Endif;
        //
        // All EOB Mode
        //
          When @@Mode = 'A';
             Setll @@BchCntl pfaclm2;
             ReadE @@BchCntl pfaclm2;
             Dow Not %Eof(faclml);
        //
        // Get the 837 Header Record
        //
                clear pfahdr;
                Chain (CLMiCntl) fahdrl;
                ExSr GetClm;
                if xxClmErr = *On;
                   // Set Claim to have error status and read next record
                   CLMprFl = 'E';
                   Update pfaclm2;
                   ReadE @@BchCntl pfaclm2;
                   Iter;
                Endif;
                ExSr GetProv;
                if xxProvErr = *On;
                   // Set Claim to have error status and read next record
                   CLMprFl = 'E';
                   Update pfaclm2;
                   ReadE @@BchCntl pfaclm2;
                   Iter;
                Endif;
                ExSr ChkAm1;
                ExSr UpdFaclmp1;
                ExSr Writer;
                ExSr UpdFaclmp2;
                ExSr PendBill;
                ReadE @@BchCntl pfaclm2;
             EndDo;
        // Mass EOB Mode
          When @@Mode = 'M';
             Setll @@BchCntl pfaclm2;
             ReadE @@BchCntl pfaclm2;
             Dow Not %Eof(faclml);
        //
        // Get the 837 Header Record
        //
                clear pfahdr;
                Chain (CLMiCntl) fahdrl;
                ExSr CrtClm;
                ExSr CrtProv;
                ExSr ChkAm1;
                ExSr UpdFaclmp1;
                ExSr Writer;
                ExSr UpdFaclmp2;
                ExSr PendBill;
                ExSr Adjudicate;
                ReadE @@BchCntl pfaclm2;
             EndDo;
       // Print the totals
             ExSr SrWrtRepSum;
        //
        // Transfer from error queue mode
        //
          When @@Mode = 'T';
             Chain @@RRN faclmp;
        //
        // Get the 837 Header Record
        //
                clear pfahdr;
                Chain (CLMiCntl) fahdrl;
                ExSr GetClm;
                ExSr Getprov;
                ExSr ChkAm1;
                ExSr UpdFaclmp1;
                ExSr Writer;
                ExSr UpdFaclmp2;
                ExSr PendBill;
          EndSl;
       Endsr;
       //================================================================
       // PgmStr - Open Files, Access Image Document, Etc.
       //================================================================
       Begsr PgmStr;
         //
         // Get the system date and for the report
         //
          sys_date = %date();
          xxDate= Dateto8(sys_date);
          sys_time = %time();
          xxTime = TimeTo6(SystemTime());
         //
         // Set overlow indicator on initially
         //
         *inOA = *on;
         *inOF = *on;
         //
         // Check the Image File
         //
          ChkImGr(isImageInst);
         //
         // Called from Image Queue
         //
          If @@mode = 'I';
           //
           // Access the xref File to Get the Document ID
           //
             If isImageInst= 'I';
                Open optimptl1;
                kkSdoc = @@DcID;
                Chain opKy pop01;
                If %Found(optimptl1);
                   xxpos = %Scan('.':OPdcid);
                   If xxpos <> 0;
                      kkpAcct = %Subst(OPdcid:1:xxpos-1);
                   Else;
                      kkpAcct = OPdcId;
                   Endif;
                   Chain kkpAcct pfaclm4;
                   If %Found (faclml7);
                      @@BchCntl = CLMiCntl;
                   Endif;
                Endif;
                Close optimptl1;
             Endif;
          Endif;
         //
         // Get the Functional Group Header Record
         //
          Chain @@BchCntl pfafgh;
          If %Found(fafghl);
             Select;
             When %Subst(FGHverIdCd:1:10) = '005010X222';
                xxSys = 'P';
                xxBtyp = 'N';
             When %Subst(FGHverIdCd:1:10) = '005010X223';
                xxSys = 'H';
                xxBtyp = 'H';
             Endsl;
         // End the program if not found
          Else;
             Exsr PgmEnd;
          Endif;
         //
         // Get the client number
         //
          GetClientID(outCltID);
         //
         // Get the company number and subscriber number
         //
         outCo# = 0;
         outFnd = 0;
         outConm = *Blanks;
         outFnam = *Blanks;
         outSub# = 0;
         outFyr = 0;
         Callp ComApi(outCo#:outFnd:outConm:outFnam:outSub#:outFyr);
         // In Mass mode initialize summary fields
          If @@mode = 'M';
             SmChrg = 0;
             SmMRA  = 0;
             SmNdis = 0;
             SmOthr = 0;
             SmOth2 = 0;
             SmPaid = 0;
          Endif;
       Endsr;
       //================================================================
       // PgmEnd - End of program processing
       //================================================================
       Begsr PgmEnd;
         *inLr = *On;
         Return;
       Endsr;
       //================================================================
       // Adjudicate - Adjudicate the Bill
       //================================================================
       Begsr Adjudicate;
          If CLMprFl = 'Y';
             Evalr xxRRN = %Char(DBrrn2);
             zzRRN = DBrrn2;
        // Determine greatest PPO savings by using all networks on the group
             Chain (outCo#:outSub#:EOfnd) Pf101;
             If %Found(fmf1l);
                xxNdis = 0;
                xxNtwk = *blanks;
        // F1nw01
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw01 <> *blanks;
                      EOntwk = F1nw01;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw02
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw02 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw02;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw03
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw03 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw03;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw04
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw04 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw04;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw05
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw05 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw05;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw06
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw06 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw06;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw07
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw07 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw07;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw08
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw08 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw08;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw09
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw09 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw09;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
        // F1nw10
                Chain eobKy1 peo01;
                If %Found (faeol4);
                   If F1nw10 <> *blanks;
                      Chain eobKy1 peo01;
                      EOntwk = F1nw10;
                      Update peo01;
                      Callp AdjBillCK (xxRRN:xxCfMr);
                      Chain(n) eobKy1 peo01;
                      If EOnDis > xxNdis;
                         xxNdis = EOnDis;
                         xxNtwk = EOntwk;
                      Endif;
                   Else;
                      Update peo01;
                   Endif;
                Endif;
             Endif;
          // Perform final adjudication using highest discount
             Chain eobKy1 peo01;
             If %Found (faeol4);
                EOntwk = xxNtwk;
                Update peo01;
             Endif;
             Callp AdjBillCK (xxRRN:xxCfMr);
             Chain(n) zzRRN peo02;
          // Accumulate totals
             If %Found (faeop);
                SmChrg = SmChrg + EOchrg;
                SmMRA  = SmMRA  + EOmra;
                SmNdis = SmNdis + EOnDis;
                SmOthr = SmOthr + EOothr;
                SmOth2 = SmOth2 + EOoth2;
                SmPaid = SmPaid + EOPaid;
                Exsr SrWrtRep2;
                Feod faeop;
             Endif;
          Endif;
       Endsr;
       //================================================================
       // PendBill - Pend the bill
       //================================================================
       Begsr PendBill;
        // If All mode without an Image then pend to 837 pend type
             ESco# = EOco#;
             ESco# = EOco#;
             ESsub# = EOsub#;
             ESfnd  = EOfnd;
             ESfyr  = EOfyr;
             EScase  = EOcase;
             ESeob#  = EOeob#;
             ESrcn   = EOrcn;
             ESss#   = EOss#;
             ESacDt  = EOacDt;
             if @@Mode = 'A' or @@Mode = 'T' or @@Mode = 'I';
                ESblSt  = 'P';
                ESstCd  = '837';
             Endif;
             if @@Mode = 'M';
                ESblSt  = 'C';
             Endif;
             Chain (kkCo#:ESblst:ESstCd) pbs01;
             If %Found(ftbsl);
                ESreas = BSdesc;
             Else;
                ESreas = 'Imported by 837 Import Process';
             EndIf;
             ESstat = 'A';
             ESuser = Q1user;
             ESpgm  = Q1pgm;
             ESdt = EOdt;
             EStime = EOtime;
             ESiUsr = EOuser;
             ESiPgm = EOpgm;
             ESiDt = EOdt;
             ESiTim = EOtime;
             Write pes01;
       Endsr;
       //================================================================
       // UpdFaclmp1 - Update Claim (Bill) Header(FACLMP) - 1st Update
       //================================================================
       Begsr updFaclmp1;
         If CLMeob# = *Zeros;
            Chain c1Ky pc101;
            If %found(fmc1l);
               C1eob# = C1eob# + 1;
               Update pc101;
            Endif;
         Endif;
         CLMco# = CMco#;
         CLMsub# = CMsub#;
         CLMfnd  = CMfnd;
         CLMfyr  = CMfyr;
         CLMcase  = CMcase;
         CLMss#   = CMss#;
         CLMacDt  = CMacDt;
         If @@Mode <> 'T';
            CLMeob#  = C1eob#;
            kkEob#   = C1eob#;
         Endif;
         If @@Mode <> 'T';
            kkEob#   = CLMeob#;
         Endif;
         Select;
         When @@Mode = 'A' or @@Mode = 'M';
            Update pfaclm2;
         When @@Mode = 'I';
            Update pfaclm4;
         Other;
            Update pfaclm;
         Endsl;
       Endsr;
       //================================================================
       // UpdFaclmp2 -  Update Claim(Bill) Header(FaclmP) - 2nd Update
       //================================================================
       //
       Begsr UpdFaclmp2;
          Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pfaclm3;
          If %Found(faclml2);
             CLMco# = EOco#;
             CLMfnd = EOfnd;
             CLMfyr = EOfyr;
             CLMcase = EOcase;
             CLMfein = EOfein;
             CLMmod  = EOmod;
             CLMss# = EOss#;
             CLMacDt = EOacDt;
             CLMrcn  = *Zeros;
             CLMprFl = 'Y';
             sys_date = %date();
             CLMprDt = Dateto7(sys_date);
             CLMprTm = TimeTo6(SystemTime());
             Update pfaclm3;
          Endif;
         //
       Endsr;
       //================================================================
       // UpdFalinp1 - Update Claim(Bill) Detail(Falinp) - 1st Update
       //================================================================
       Begsr UpdFalinp1;
          LINco#  = CAco#;
          LINsub# = CAsub#;
          LINfnd  = CAfnd;
          LINfyr  = CAfyr;
          LINcase = CAcase;
          LINeob# = CAeob#;
          LINdseq = CAdseq;
          Update pfalin;
       Endsr;
       //================================================================
       // UpdFalinp2 - Update Claim(Bill) Detail(Falinp) - 2nd Update
       //================================================================
       Begsr UpdFalinp2;
          Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pfalin2;
          If %Found(falinl2);
             LINprFl = 'C';
             Update pfalin2;
          Endif;
       Endsr;
       //================================================================
       // WRITER - Write data base record and after processing
       //================================================================
       begsr Writer;
       //
       // Get the adjudication state
       //
          ExSr GetAst;
       //
       // Write the header records
       //
          ExSr WrtEop;                                                        //FAEOP
          ExSr WrtEap;                                                        //FAEAP
          Select;
          When EObTyp = 'D';
          //***  ExSr WrtEdp;                                                 //FAEDP
          When EObTyp = 'H';
           ExSr WrtEup;                                                       //FAEUP
         When EObTyp = 'N';
           ExSr WrtEhp;                                                       //FAEHP
         EndSl;
         //
         // Write the detail records
         //
         ExSr WrtDtl;
         //
       Endsr;
       //================================================================
       // WrtEop - Write FAEOP record
       //================================================================
       Begsr WrtEop;
         //
         // Write first EOMB
         //
         Chain eobky1 peo01;
         If Not %Found(faeol4) or @@Mode = 'T';
            Clear peo01;
            EOco# = CLMco#;
            EOsub# = CLMsub#;
            EOfnd = CLMfnd;
            EOfyr = CLMfyr;
            EOcase = CLMcase;
            Select;
            When @@Mode <> 'T';
               If CLMpIrIq = 'SY';
               EOss# = CLMPIPCCN;
               Endif;
               Monitor;
               xxDt8 = %Dec(CLMaD:8:0);
               On-Error;
               xxDt8 = 0;
               Endmon;
               dtCCYYMMDD = dtMDYY2(xxDt8);
               EOacDt = dtCYMD(dtCCYYMMDD);
               If @@Mode = 'A' or @@Mode = 'I';
                  EOss# = CMss#;
                  EOacDt = CMacDt;
               Endif;
               EOfein = kkFein;
               EOmod  = kkMod;
               If @@Mode = 'M';
                  EOfein = PDfein;
                  EOmod = PDmod;
               Endif;
            When @@Mode = 'T';
               EOss# = CLMss#;
               EOacDt = CLMacDt;
               EOfein = CLMfein;
               EOmod  = CLMmod;
            Endsl;
            If CLMrfRupQu = '0B';
               EOrLic = CLMrfUpid;
            Endif;
            Monitor;
            xxDt8 = %Dec(CLMitDt:8:0);
            On-Error;
            xxDt8 = 0;
            Endmon;
            dtCCYYMMDD = dtMDYY2(xxDt8);
            EOfrDt = dtCYMD(dtCCYYMMDD);
            Monitor;
            xxDt8 = %Dec(CLMlsDt:8:0);
            On-Error;
            xxDt8 = 0;
            Endmon;
            dtCCYYMMDD = dtMDYY2(xxDt8);
            EOtoDt = dtCYMD(dtCCYYMMDD);
           // Billing Provider Information
           Select;
           When @@Mode <> 'T';
              If HDbpEnIdCd = '85' and HDbpEnQu = '2';
                 If HDbpFnm = *blanks;
                    EOpNam = %Trim(HDbpNm);
                 Else;
                    EOpNam = %Trim(HDbpFnm) + ' ' + %Trim(HDbpNm);
                 Endif;

                 If EOpNam > *blanks and HDbpSuff > *blanks;
                    EOpNam = %Trim(EOPnam) + ', ' + %Trim(HDbpSuff);
                 Endif;
              Endif;
              // For 114MJB we are not loading the NPI number
              //** If HDbpEnIdCd = '85' and HDbpNmQu = 'XX';
              //**   EObNpi = HDbpNmId;
              //** Endif;
              EOadr1 = HDbpAdr1;
              EOadr2 = HDbpAdr2;
              EOcty = HDbpCty;
              EOst = HDbpSt;
            When @@Mode = 'T';
               Chain (CLMfein:CLMmod) ppd01;
               If %Found (fmpdl);
                  If PDfsn = *Blanks;
                     EOpNam = PDname;
                  Else;
                     EOpNam = %Trim(PDfsN) + ' ' + %Trim(PDname) ;
                  Endif;
                  EOadr1 = PDadr1;
                  EOadr2 = PDadr2;
                  EOcty = PDcty;
                  EOst  = PDst;
                  EOzip = PDzip;
               Endif;
            Endsl;
            Monitor;
            EOzip = %Dec(HDbpPost:9:0);
            On-Error;
            EOzip = 0;
            Endmon;
            EOarea = *zeros;
            EOphn = *zeros;
            EOeob1 = *blanks;
            If @@mode <> 'M';
               EOblSt = 'P';
            Endif;
            If @@mode = 'M';
               EOblSt = 'C';
            Endif;
            EOcsCd = '1  ';
            EOeob# = CLMeob#;
            EOrcn = CLMrcn;
            EOsys = xxSys;
            EObTyp = xxBtyp;
            EOiDoc = @@dcId;
            EOcont = CLMctCd;
            Monitor;
            EOFfei = %Dec(CLMsf2ndId:9:0);
            On-Error;
            EOfFei = 0;
            Endmon;
            EOfNam = CLMsfNm;
            Monitor;
            EOfZip = %Dec(CLMsfPost:9:0);
            On-Error;
            EOfZip = 0;
            Endmon;
            EObLic = *blanks;
            EObTyC = *blanks;
            EOrNam = CLMrnpNm;
            EOrFei = *zeros;
            EOrFnm = CLMrnpFnm;
            EOrLnm = CLMrnpNm;
            EOblId = *Blanks;
            EOfLic = CLMsfPost;
            EO#bll = 1;
            EOast = xxAst;
            EOtape = 'N';
            Chain beKy pbe01;
            If %Found(fmbel);
               EOtape = BEedi;
               EOrevI = BErevI;
            Endif;
            Monitor;
            xxDt8 = %Dec(HDbhtCrDt:8:0);
            On-Error;
            xxDt8 = 0;
            Endmon;
            dtCCYYMMDD = dtMDYY2(xxDt8);
            EOcSdt = dtCYMD(dtCCYYMMDD);
            Monitor;
            xxDt8 = %Dec(HDbhtCrDt:8:0);
            On-Error;
            xxDt8 = 0;
            Endmon;
            dtCCYYMMDD = dtMDYY2(xxDt8);
            EOcrDt = dtCYMD(dtCCYYMMDD);
            EOsInv = CLMpAcct;

            If CLMpatClm# <> *blanks;
               EOcid# = CLMpatClm#;
            Else;
               EOcid# = CLMsubSpi;
            Endif;

            Monitor;
            EOchrg = %Dec(CLMch:9:2);
            On-Error;
            EOchrg = 0;
            Endmon;
            EOntwk = *Blanks;
            EOspCd = PDspCd;
            kkSS# = EOss#;
            EOlsn = CMlsN;
            EOfsn = CMfsN;
            EOmid = CMmid;
            EOnDsc = *Blanks;
            EOmbUp = 'A';
            EOuser = Q1user;
            EOpgm = Q1pgm;
            sys_date = %date();
            sys_time = %time();
            EOtrDT = Dateto7(sys_date);
            EOdt = Dateto7(sys_date);
            EOtime = TimeTo6(SystemTime());
            EOiUsr = Q1user;
            EOiPgm = EOpgm;
            EOiDt = EOdt;
            EOiTim = EOtime;
            Write peo01;
            @@Rrn2 = DBrrn2;
            SmEOBcnt = SmEOBcnt + 1;
          Endif;
       Endsr;
       //================================================================
       // UpdEOdates -  Update FAEOP EOfrDt and EOtoDt
       //================================================================
       Begsr UpdEOdates;
          Chain eobky1 peo01;
          If %Found (Faeol4);
             Setll eobKy1 pca01;
             Reade eobKy1 pca01;
             Dow Not %Eof (fdcal19);
                If EOfrDt = *Zeros;
                   EOfrDt = CAfDat;
                   EOtoDt = CAtDat;
                Endif;
                If CAfDat < EOfrDt;
                   EOfrDt = CAfDat;
                Endif;
                If CAtDat > EOtoDt;
                   EOtoDt = CAtDat;
                Endif;
                Reade eobKy1 pca01;
             Enddo;
             Update peo01;
          Endif;
       Endsr;
       //================================================================
       // WRTEAP - Write FAEAP record
       //================================================================
       Begsr WRTEAP;
          Chain eobky1 PEA01;
          If Not %Found(faeal1) or @@Mode = 'T';
             Clear pea01;
             EAco# = EOco#;
             EAsub# = EOsub#;
             EAfnd = EOfnd;
             EAfyr = EOfyr;
             EAcase = EOcase;
             EAss# = EOss#;
             EAacDt = EOacDt;
             EAeob# = EOeob#;
             EArcn  = EOrcn;
             EABtyp = EOBtyp;
             EAtrDt = EOtrDt;
             If EOblSt = 'P';
                EAepFl = 'E';
             Endif;
             EAchrg = EOchrg;
             EAedi = EOtape;
             EAbHrf = 'H';
             EAnHrf = 'H';
             EAsys = EOsys;
             EAfein = EOfein;
             EAmod = EOmod;
             EAsinv = EOsInv;
             EApNam = EOpNam;
             EAadr1 = EOadr1;
             EAadr2 = EOadr2;
             EActy = EOcty;
             EAst = EOst;
             EAzip = EOzip;
             EAnSnt = 'A';
             EAstat = 'A';
             EAuser = Q1user;
             EApgm  = Q1pgm;
             EAdt = EOdt;
             EAtime = EOtime;
             EAiUsr = EOuser;
             EAiPgm = EApgm;
             EAiDt = EAdt;
             EAiTim = EAtime;
             Write pea01;
          Endif;
       Endsr;
       //================================================================
       // WrtEdp - Write FAEDP record *** COMMENTED OUT FOR NOW ***
       //================================================================
       //***Begsr WrtEdp;
       //***   kkEOB# = EOeob#;
       //***   Chain eobKy1 ped01;
       //***   If %Found(faedl);
       //***
       //*** Clear ped01;
       //*** EDco#  = EOco#;
       //*** EDsub# = EOsub#;
       //*** EDfnd  = EOfnd;
       //*** EDfyr  = EOfyr;
       //*** EDcase = EOcase;
       //*** EDeob# = EOeob#;
       //*** EDss#  = EOss#;
       //*** EDacDt = EOacDt;
       //*** EDlic# = U1lic#;
       //*** EDedi = EOtape;
       //*** EDstat = 'A';
       //*** EDpPln = U1pPln;
       //*** EDacct = U1acct;
       //*** EDfLic = U1rLic;
       //*** EDrfLc = U1rfLc;
       //*** EDatLc = U1atLc;
       //*** EDicd1001 = U1icd1;
       //*** EDicd1002 = U1icd2;
       //*** EDicd1003 = U1icd3;
       //*** EDicd1004 = U1icd4;
       //*** EDicdInd = 'Y';
       //*** EDpoTx = U1poTx;
       //*** EDrLsn = U1rNam;
       //*** EDrFei = U1rFei;
       //*** EDrFsn = U1sfNm;
       //*** EDrAd1 = U1sAd1;
       //*** EDrAd2 = U1sAd2;
       //*** EDrCty = U1sCty;
       //*** EDrSt = U1sSt;
       //*** EDrZip = %Dec(U1sZip:9:0);
       //*** EDtNpi = U1tNpi;
       //*** EDuser = Q1user;
       //*** EDpgm  = Q1pgm;
       //*** EDdt = EOdt;
       //*** EDtime = EOtime;
       //*** EDiUsr = EOuser;
       //*** EDiPgm = EOpgm;
       //*** EDiDt = EOdt;
       //*** EDiTim = EOtime;
       //*** Write ped01;
       //***   Endif;
       //***Endsr;
       //================================================================
       // WrtEhp - Write FAEHP record
       //================================================================
       Begsr WrtEhp;
          kkEob# = EOeob#;
          Chain eobky1 peh01;
          If Not %Found(faehl) or @@Mode = 'T';
             Clear peh01;
             EHco#  = EOco#;
             EHsub# = EOsub#;
             EHfnd  = EOfnd;
             EHfyr  = EOfyr;
             EHcase = EOcase;
             EHeob# = EOeob#;
             EHss#  = EOss#;
             EHacDt = EOacDt;
             EHacct = CLMPacct;
            // Load ICD-10 Codes
             Select;
             When CLMdg1Q = 'ABK';
                EHicd1001 = CLMdg1;
                EHicd1002 = CLMdg2;
                EHicd1003 = CLMdg3;
                EHicd1004 = CLMdg4;
                EHicdE    = CLMdg5;
                EHicdF    = CLMdg6;
                EHicdG    = CLMdg7;
                EHicdH    = CLMdg8;
                EHicdI    = CLMdg9;
                EHicdJ    = CLMdg10;
                EHicdK    = CLMdg11;
                EHicdL    = CLMdg12;
                EHicdInd = 'Y';
            // Load ICD-9 Codes
             When CLMdg1Q = 'BK';
                EHdia1 = CLMdg1;
                EHdia2 = CLMdg2;
                EHdia3 = CLMdg3;
                EHdia4 = CLMdg4;
                EHicdInd = 'N';
             Endsl;
             EHlic# = *blanks;
             EHedi = EOtape;
             EHempl = 'Y';
             EHstat = 'A';
             Chain beKy pbe01;
             If %Found(fmbel);
               EHpPln = BEpPln;
             Endif;
             EHfLic = *Blanks;
             If CLMrfRupQu = '0B';
                EHrfLc = CLMrfUpId;
             Endif;
             EhatLc = *Blanks;
             EHrNpi = CLMrnPnmId;
             if CLMrnp2Pqu = '0B';
                EHrnLc = CLMrnp2Pid;
             Endif;
             EHrFei = *Zeros;
             EHrLsn = CLMrnpNm;
             EHrFsn = CLMrnpFnm;
             EHrAd1 = CLMsfAdr1;
             EHrAd1 = CLMsfAdr2;
             EHrCty = CLMsfCty;
             EHrSt = CLMsfSt;
             Monitor;
             EHrZip = %Dec(CLMsfPost:9:0);
             On-Error;
             EHrZip = 0;
             Endmon;
             EHuser = Q1user;
             EHpgm  = Q1pgm;
             EHdt = EOdt;
             EHtime = EOtime;
             EHiUsr = EOuser;
             EHiPgm = EOpgm;
             EHiDt = EOdt;
             EHiTim = EOtime;
             Write peh01;
          Endif;
       Endsr;
       //================================================================
       // WrtEup - Write FAEUP record
       //================================================================
       Begsr WrtEup;
          kkEob# = EOeob#;
          Chain eobky1 peu01;
          If Not %Found(faeul) or @@Mode = 'T';
             CLear peu01;
             EUco#  = EOco#;
             EUsub# = EOsub#;
             EUfnd  = EOfnd;
             EUfyr  = EOfyr;
             EUcase = EOcase;
             EUeob# = EOeob#;
             EUss#  = EOss#;
             EUacDt = EOacDt;
             EUpcM = '0';
             EUmcar = *Blanks;
             Monitor;
             xxDt8 = %Dec(%Subst(CLMscFt:1:8):8:0);
             On-Error;
             xxDt8 = 0;
             Endmon;
             dtCCYYMMDD = dtMDYY2(xxDt8);
             EUscPf = dtCYMD(dtCCYYMMDD);
             Monitor;
             xxDt8 = %Dec(%Subst(CLMscFt:10:8):8:0);
             On-Error;
             xxDt8 = 0;
             Endmon;
             dtCCYYMMDD = dtMDYY2(xxDt8);
             EUscPt = dtCYMD(dtCCYYMMDD);
             EUacct = CLMpAcct;
             If CLMfQu = 'A';
                EUblTy = CLMfC + CLMfCd;
             Endif;
             Monitor;
             xxDt8 = %Dec(%Subst(CLMadDtTm:1:8):8:0);
             On-Error;
             xxDt8 = 0;
             Endmon;
             dtCCYYMMDD = dtMDYY2(xxDt8);
             EUadDt = dtCYMD(dtCCYYMMDD);
             Monitor;
             EUadHr = %Dec(%Subst(CLMadDtTm:9:4):4:0);
             On-Error;
             EUadHr = 0;
             Endmon;
             If CLMprDgT = 'ABK';
                EUicd1001 = CLMprDg;
                EUicd1002 = CLMdg2;
                EUicd1003 = CLMdg3;
                EUicd1004 = CLMdg4;
                EUicd1005 = CLMdg5;
                EUicd1006 = CLMdg6;
                EUicd1007 = CLMdg7;
                EUicd1008 = CLMdg8;
                EUicd1009 = CLMdg9;
                EUicd1010 = CLMdg10;
                EUicdInd = 'Y';
             Endif;
             If CLMprDgT = 'BF';
                EUdia1 = CLMprDg;
                EUdia2 = CLMdg2;
                EUdia3 = CLMdg3;
                EUdia4 = CLMdg4;
                EUdia5 = CLMdg5;
                EUdia6 = CLMdg6;
                EUdia7 = CLMdg7;
                EUdia8 = CLMdg8;
                EUdia9 = CLMdg9;
             Endif;
             If CLMprinPq = 'BBR';
             // Procedure Codes
                EUprc1001 = CLMprinPc;
                EUprc1002 = CLMpr2;
                EUprc1003 = CLMpr3;
                EUprc1004 = CLMpr4;
                EUprc1005 = CLMpr5;
                EUprc1006 = CLMpr6;
             // Procedure Dates
                Monitor;
                xxDt8 = %Dec(CLMprinDt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD1 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr2Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD2 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr3Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD3 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr4Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD4 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr5Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD5 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr6Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD6 = dtCYMD(dtCCYYMMDD);
             Endif;
             If CLMprDgT = 'BR';
             // Procedure Codes
                EUprc1 = CLMprinPc;
                EUprc2 = CLMpr2;
                EUprc3 = CLMpr3;
                EUprc4 = CLMpr4;
                EUprc5 = CLMpr5;
                EUprc6 = CLMpr6;
             // Procedure Dates
                Monitor;
                xxDt8 = %Dec(CLMprinDt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD1 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr2Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD2 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr3Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD3 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr4Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD4 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr5Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD5 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(CLMpr6Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUprD6 = dtCYMD(dtCCYYMMDD);
             Endif;
             If CLMadDgt = 'ABK';
                EUaDia1001 = CLMadDg;
             Endif;
             If CLMadDgt = 'BK';
                EUadDi = CLMadDg;
             Endif;
             If CLMecDgT1 = 'ABN';
                EUcau1001 = CLMecDg1;
                EUcau1002 = CLMecDg2;
                EUcau1003 = CLMecDg3;
             Endif;
             If CLMecDgT1 = 'BN';
                EUcau1 = CLMecDg1;
                EUcau2 = CLMecDg2;
                EUcau3 = CLMecDg3;
             Endif;
             EUlic# = *blanks;
             EUedi = EOtape;
             If CLMop2ndQu = '0B';
                EUoLic = CLMop2NDId;
             Endif;
             EUschd = *Blanks;
             EUiCn = *Blanks;
             EUdcDt = 0;
             Monitor;
             EUdcHr = %Dec(CLMdH:4:0);
             On-Error;
             EUdcHr = 0;
             Endmon;
             EUadTy = CLMadTyp;
             EUdCn = CLMopCcNbr;
             If CLMprinPc <> *Blanks or CLMpr2 <> *Blanks or CLMpr3 <> *Blanks
                 or CLMpr4 <> *Blanks or CLMpr5 <> *Blanks or CLMpr6 <> *Blanks;
                EUpcM = '9';
             Endif;

             // Occurance Span and Dates

             If CLMos1Qu  = 'BI';
                EUoScd = CLMoS1Cd;
             Endif;
             If CLMoS1Dtq = 'RD8';
                Monitor;
                xxDt8 = %Dec(%Subst(CLMoS1DtP:1:8):8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoSfd = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(%Subst(CLMoS1DtP:10:8):8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoStd = dtCYMD(dtCCYYMMDD);
             Endif;

             If CLMos2Qu  = 'BI';
                EUoSc2 = CLMoS2Cd;
             Endif;
             If CLMoS2Dtq = 'RD8';
                Monitor;
                xxDt8 = %Dec(%Subst(CLMoS2DtP:1:8):8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoFd2 = dtCYMD(dtCCYYMMDD);
                Monitor;
                xxDt8 = %Dec(%Subst(CLMoS2DtP:10:8):8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoTd2 = dtCYMD(dtCCYYMMDD);
             Endif;

             // Occurance Codes and Dates

             If CLMocc1Qu = 'BH';
                EUoCd1 = CLMocc1Cd;
             Endif;
             If CLMocc1Dq = 'D8';
                Monitor;
                xxDt8 = %Dec(CLMocc1Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoDt1 = dtCYMD(dtCCYYMMDD);
             Endif;

             If CLMocc3Qu = 'BH';
                EUoCd3 = CLMocc3Cd;
             Endif;
             If CLMocc3Dq = 'D8';
                Monitor;
                xxDt8 = %Dec(CLMocc3Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoDt3 = dtCYMD(dtCCYYMMDD);
             Endif;

             If CLMocc4Qu = 'BH';
                EUoCd4 = CLMocc4Cd;
             Endif;
             If CLMocc4Dq = 'D8';
                Monitor;
                xxDt8 = %Dec(CLMocc4Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoDt4 = dtCYMD(dtCCYYMMDD);
             Endif;

             If CLMocc2Qu = 'BH';
                EUoCd2 = CLMocc2Cd;
             Endif;
             If CLMocc2Dq = 'D8';
                Monitor;
                xxDt8 = %Dec(CLMocc2Dt:8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                EUoDt2 = dtCYMD(dtCCYYMMDD);
             Endif;

             // Value Codes and Amounts

             If CLMvC1Qu  = 'BE';
                EUvC01 = CLMvC1;
                EUvA01 = %Dec(CLMvCa1:11:2);
             Endif;

             If CLMvC2Qu  = 'BE';
                EUvC02 = CLMvC2;
                EUvA02 = %Dec(CLMvCa2:11:2);
             Endif;

             If CLMvC3Qu  = 'BE';
                EUvC03 = CLMvC3;
                EUvA03 = %Dec(CLMvCa3:11:2);
             Endif;

             If CLMvC4Qu  = 'BE';
                EUvC04 = CLMvC4;
                EUvA04 = %Dec(CLMvCa4:11:2);
             Endif;

             If CLMvC5Qu  = 'BE';
                EUvC05 = CLMvC5;
                EUvA05 = %Dec(CLMvCa5:11:2);
             Endif;

             If CLMvC6Qu  = 'BE';
                EUvC06 = CLMvC6;
                EUvA06 = %Dec(CLMvCa6:11:2);
             Endif;

             If CLMvC7Qu  = 'BE';
                EUvC07 = CLMvC7;
                EUvA07 = %Dec(CLMvCa7:11:2);
             Endif;

             If CLMvC8Qu  = 'BE';
                EUvC08 = CLMvC8;
                EUvA08 = %Dec(CLMvCa8:11:2);
             Endif;

             If CLMvC9Qu  = 'BE';
                EUvC09 = CLMvC9;
                EUvA09 = %Dec(CLMvCa9:11:2);
             Endif;

             If CLMvC10Qu  = 'BE';
                EUvC10 = CLMvC10;
                EUvA10 = %Dec(CLMvCa10:11:2);
             Endif;

             If CLMvC11Qu  = 'BE';
                EUvC11 = CLMvC11;
                EUvA11 = %Dec(CLMvCa11:11:2);
             Endif;

             If CLMvC12Qu  = 'BE';
                EUvC12 = CLMvC12;
                EUvA12 = %Dec(CLMvCa12:11:2);
             Endif;

             EUpPln = *Blanks;
             Monitor;
             EUdrg# = %Dec(CLMdrg:5:0);
             On-Error;
             EUdrg# = 0;
             Endmon;
             EUfLic = *Blanks;
             If CLMat2ndQu = '0B';
                EUatLc = CLMat2ndId;
             Endif;
             EUaNpi = CLMatpNmId;
             EUrLsn = CLMrnpNm;
             EUrFsn = CLMrnpFnm;
             EUrAd1 = *Blanks;
             EUrAd2 = *Blanks;
             EUrCty = *Blanks;
             EUrSt = *Blanks;
             EUrZip = *Zeros;
             EUrFei = *Zeros;
             EUadSc = CLMadSc;
             EUdcSt = *Blanks;
             If CLMprvDgT = 'APR';
                EUpRea10 = CLMprvRea;
             Endif;
             If CLMprvDgT = 'PR';
                EUpRea = CLMprvRea;
             Endif;
             EUimSt = *Blanks;
             EUstat = 'A';
             EUuser = Q1user;
             EUpgm  = Q1pgm;
             EUdt = EOdt;
             EUtime = EOtime;
             EUiUsr = EOuser;
             EUiPgm = EOpgm;
             EUiDt = EOdt;
             EUiTim = EOtime;
             Write peu01;
          Endif;
       Endsr;
       //================================================================
       // WrtCap - Write FDCAP record
       //================================================================
       Begsr WrtCap;
          If LINeob# = *Zeros and LINdSeq = *Zeros;
             kkDseq = *HiVal;
             SetGt (EOco#:EOsub#:EOfnd:EOfyr:EOcase:kkDseq) pca02;
             ReadPe (EOco#:EOsub#:EOfnd:EOfyr:EOcase) pca02;
             If %Found(fdcalf);
                xxDseq = CAdSeq + 1;
             Else;
                xxDseq = 1;
             Endif;
             Monitor;
             kkLin# = %Dec(LINlin#:9:0);
             On-Error;
             kkLin# = 0;
             Endmon;
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:kkLin#) pca01;
             If Not %Found(fdcal19);
                Clear pca01;
                CAco# = EOco#;
                CAsub# = EOsub#;
                CAfnd = EOfnd;
                CAfyr = EOfyr;
                CAcase = EOcase;
                CAeob# = EOeob#;
                CArcn  = EOrcn;
                CAss#  = EOss#;
                CAacDt = EOacDt;
                CAstat = 'A';
                CAcsCd = EOcsCd;
                Monitor;
                xxDt8 = %Dec(%Subst(LINsvDt:1:8):8:0);
                On-Error;
                xxDt8 = 0;
                Endmon;
                dtCCYYMMDD = dtMDYY2(xxDt8);
                CAfDat = dtCYMD(dtCCYYMMDD);
                If LINtfQ = 'D8';
                   CAtDat = CAfDat;
                Endif;
                If LINtfQ = 'RD8';
                   Monitor;
                   xxDt8 = %Dec(%Subst(LINsvDt:10:8):8:0);
                   On-Error;
                   xxDt8 = 0;
                   Endmon;
                   dtCCYYMMDD = dtMDYY2(xxDt8);
                   CAtDat = dtCYMD(dtCCYYMMDD);
                Endif;
                CArDat = EOtrDt;
                CAfein = EOfein;
                CAmod = EOmod;
                Select;
                when EObTyp = 'N';
                CAprTy = '1';
                When EObTyp = 'H';
                CAprTy = '4';
                When EObTyp = 'D';
                CAprTy = 'D';
                EndSl;
                If LINsvcUnit <> *Zeros;
                Monitor;
                CAunts = %Dec(LINsvcUnit:5:2);
                On-Error;
                CAunts = 0;
                Endmon;
                Else;
                CAunts = 1;
                Endif;
                CAdSeq = xxDseq;
                If EOblSt = 'P';
                CAchk# = 0;
                Else;
                CAchk# = EOchk#;
                Endif;
                Select;
                When EObTyp = 'D';
                   CArcFl = xxRcFL;
                When EObTyp = 'H';
                   CArcFl = 'S';
                When EObTyp = 'N';
                   CArcFl = xxRcFl;
                EndSl;
                CAzipA = EOzipA;
                CAaSt = EOaSt;
                Monitor;
                CAchrg = %Dec(LINchrg:9:2);
                On-Error;
                CAchrg = 0;
                Endmon;
                Monitor;
                CAlin# = %Dec(LINlin#:9:0);
                On-Error;
                CALin# = 0;
                Endmon;
                Monitor;
                CAmSeq = %Dec(LINlin#:5:0);
                On-Error;
                CAmSeq = 0;
                Endmon;
                Select;
                When EObTyp = 'N';
                CAcode = LINcode;
                CAcpt4 = LINcode;
                CAbPrc = LINcode;
                When EObTyp = 'D';
                CAcode = LINcode;
                CAcpt4 = LINcode;
                CAbPrc = LINcode;
                When EObTyp = 'H';
                Reset kkcmSv;
                kkRvCd = LINrvCd;
                kkhcpc = LINcode;
                Monitor;
                CAsvCd = %Dec(KKrvCd:4:0);
                On-Error;
                CAsvCd = 0;
                Endmon;
                CAcode = KKcmSv;
                EndSl;
                CAstat = 'A';
                CAuser = Q1user;
                CApgm  = Q1pgm;
                CAdt = EOdt;
                CAtime = EOtime;
                CAiUsr = EOuser;
                CAiPgm = EOpgm;
                CAiDt = EOdt;
                CAiTim = EOtime;
                Write pca01;
           // Update EOfrDt and EOtoDt if zero
                if EOfrDt = 0;
                   Exsr UpdEODates;
                Endif;
             Endif;
          Endif;
       Endsr;
       //================================================================
       // WrtDdp - Write FDDDP record *** COMMENTED OUT FOR NOW ***
       //================================================================
       //*** Begsr WrtDdp;
       //***   kkEob# = CAeob#;
       //***   kkDseq = CAdSeq;
       //***   Chain CApKy1 pdd01;
       //***   If %Found(fdddl);
       //***
       //*** Clear pdd01;
       //*** DDco# = CAco#;
       //*** DDsub# = CAsub#;
       //*** DDfnd = CAfnd;
       //*** DDfyr = CAfyr;
       //*** DDcase = CAcase;
       //*** DDss# = CAss#;
       //*** DDacDt = CAacDt;
       //*** DDeob# = CAeob#;
       //*** DDdSeq = CAdSeq;
       //*** If U2unts = *Zeros;
       //***    DDunts = 1;
       //*** Else;
       //***    DDunts = U2unts;
       //*** Endif;
       //*** DDprCd = U2code;
       //*** DDprDs = U2cDsc;
       //*** DDbPrC = U2code;
       //*** DDbPrD = U2cDsc;
       //*** DDpos = U2pos;
       //*** If DDpos = *Blanks or DDpos = '00';
       //***    DDpos =  '11 ';
       //*** Endif;
       //*** DDrcFl = 'C';
       //*** xxDxr = %Dec(U2dXr:4:0);
       //*** If dx1 = 1 or dx2 = 1 or dx3 = 1 or dx4 = 1;
       //***    DDdXr1 = 'X';
       //*** Endif;
       //*** If dx1 = 2 or dx2 = 2 or dx3 = 2 or dx4 = 2;
       //***    DDdXr2 = 'X';
       //*** Endif;
       //*** If dx1 = 3 or dx2 = 3 or dx3 = 3 or dx4 = 3;
       //***    DDdXr3 = 'X';
       //*** Endif;
       //*** If dx1 = 4 or dx2 = 4 or dx3 = 4 or dx4 = 4;
       //***    DDdXr4 = 'X';
       //*** Endif;
       //*** Select;
       //*** When DDdXr1 = *Blanks and DDdXr2 = *Blanks and DDdXr3 = *Blanks
       //***   and DDdXr4 = *Blanks and EDdia1 <> *blanks;
       //***    DDdXr1 = 'X';
       //*** When DDdXr1 = *Blanks and DDdXr2 = *Blanks and DDdXr3 = *Blanks
       //***   and DDdXr4 = *Blanks and EDdia2 <> *blanks;
       //***    DDdXr2 = 'X';
       //*** When DDdXr1 = *Blanks and DDdXr2 = *Blanks and DDdXr3 = *Blanks
       //***   and DDdXr4 = *Blanks and EDdia3 <> *blanks;
       //***    DDdXr3 = 'X';
       //*** When DDdXr1 = *Blanks and DDdXr2 = *Blanks and DDdXr3 = *Blanks
       //***   and DDdXr4 = *Blanks and EDdia4 <> *blanks;
       //***    DDdXr4 = 'X';
       //*** EndSl;
       //*** DDpart = U2too#;
       //*** If U2bPrc <> *Blanks;
       //***    DDbPrc = U2bPrc;
       //*** Endif;
       //*** DDmod1 = U2mod;
       //*** DDstat = 'A';
       //*** DDuser = Q1user;
       //*** DDpgm  = Q1pgm;
       //*** DDdt = CAdt;
       //*** DDtime = CAtime;
       //*** DDiUsr = CAuser;
       //*** DDiPgm = CApgm;
       //*** DDiDt = CAdt;
       //*** DDiTim = CAtime;
       //*** Write pdd01;
       //***   Endif;
       //***Endsr;
       //================================================================
       // WrtDhp - Write FDDHP record
       //================================================================
       Begsr WrtDhp;
          kkEob# = CAeob#;
          kkDseq = CAdSeq;
          Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pdh01;
          If Not %Found(fddhl) or @@Mode = 'T';
             Clear pdh01;
             DHco#  = CAco#;
             DHsub# = CAsub#;
             DHfnd  = CAfnd;
             DHfyr  = CAfyr;
             DHcase = CAcase;
             DHss#  = CAss#;
             DHacDt = CAacDt;
             DHeob# = CAeob#;
             DHdSeq = CAdSeq;
             DHprCd = LINcode;
             If LINdesc <> *blanks;
                DHprDs = LINdesc;
             Else;
                Callp GetProcCdDes('N':DHprCd:xxAst:
                        CAfDat:'H':inMed#:outDesc);
                DHprDs = outDesc;
             Endif;
             DHbPrc = DHprCd;
             DHbPrD = DHprDs;
             If LINpos <> *Blanks;
                DHpos = '0' + LINpos;
             Else;
                If CLMfQu = 'B';
                   DHpos = '0'+ CLMfC;
                Endif;
             Endif;
             DHmod1 = LINmod1;
             DHmod2 = LINmod2;
             DHmod3 = LINmod3;
             DHmod4 = LINmod4;
             DHrcFl = 'C';
             If LINsvcUnit = *Zeros;
                DHunts = 1;
             Else;
                Monitor;
                DHunts = %Dec(LINsvcUnit:5:2);
                On-Error;
                DHunts = 0;
                Endmon;
             Endif;
             DHduCd = LINunBas;
              // Diagnosis Cross Reference Logic
             If LINdgPtr1 = '1' or LINdgPtr2 = '1' or LINdgPtr3 = '1'
               or LINdgPtr4 = '1';
                 DHdXr1 = 'X';
             Endif;
             If LINdgPtr1 = '2' or LINdgPtr2 = '2' or LINdgPtr3 = '2'
               or LINdgPtr4 = '2';
                 DHdXr2 = 'X';
             Endif;
             If LINdgPtr1 = '3' or LINdgPtr2 = '3' or LINdgPtr3 = '3'
               or LINdgPtr4 = '3';
                 DHdXr3 = 'X';
             Endif;
             If LINdgPtr1 = '4' or LINdgPtr2 = '4' or LINdgPtr3 = '4'
               or LINdgPtr4 = '4';
                 DHdXr4 = 'X';
             Endif;
             If LINdgPtr1 = '5' or LINdgPtr2 = '5' or LINdgPtr3 = '5'
               or LINdgPtr4 = '5';
                 DHdXrE = 'X';
             Endif;
             If LINdgPtr1 = '6' or LINdgPtr2 = '6' or LINdgPtr3 = '6'
               or LINdgPtr4 = '6';
                 DHdXrF = 'X';
             Endif;
             If LINdgPtr1 = '7' or LINdgPtr2 = '7' or LINdgPtr3 = '7'
               or LINdgPtr4 = '7';
                 DHdXrG = 'X';
             Endif;
             If LINdgPtr1 = '8' or LINdgPtr2 = '8' or LINdgPtr3 = '8'
               or LINdgPtr4 = '8';
                 DHdXrH = 'X';
             Endif;
             If LINdgPtr1 = '9' or LINdgPtr2 = '9' or LINdgPtr3 = '9'
               or LINdgPtr4 = '9';
                 DHdXrI = 'X';
             Endif;
             If LINdgPtr1 = '10' or LINdgPtr2 = '10' or LINdgPtr3 = '10'
               or LINdgPtr4 = '10';
                 DHdXrJ = 'X';
             Endif;
             If LINdgPtr1 = '11' or LINdgPtr2 = '11' or LINdgPtr3 = '11'
               or LINdgPtr4 = '11';
                 DHdXrK = 'X';
             Endif;
             If LINdgPtr1 = '12' or LINdgPtr2 = '12' or LINdgPtr3 = '12'
               or LINdgPtr4 = '12';
                 DHdXrL = 'X';
             Endif;
             DHnpiR = LINrnNmId;
             If LINrnUpQu = '0B';
                DHrnLc = LINrnUpId;
             Endif;
             DHblFc = *Blanks;
             DHstat = 'A';
             DHuser = Q1user;
             DHpgm  = Q1pgm;
             DHdt = CAdt;
             DHtime = CAtime;
             DHiUsr = CAuser;
             DHiPgm = CApgm;
             DHiDt = CAdt;
             DHiTim = CAtime;
             Write pdh01;
          Endif;
       Endsr;
       //================================================================
       // WrtDtl - Write Detail Records
       //================================================================
       Begsr WrtDtl;
          //*** Beg Chg *** 04/05/19 ***********************************
          //***Setll (CLMiCntl:CLMgCtl:CLMtsCntl:CLMbhtTid:CLMpAcct) falinl;
          Setll (CLMiCntl:CLMgCtl:CLMtsCntl:CLMbhtTid:CLMpAcct:
                 CLMinsID:CLMpaSfx) falinl;
          //*** End Chg *** 04/05/19 ***********************************
          If %Equal(falinl);
          //*** Beg Chg *** 04/05/19 ***********************************
          //**Reade (CLMiCntl:CLMgCtl:CLMtsCntl:CLMbhtTid:CLMpAcct) pfalin;
            Reade (CLMiCntl:CLMgCtl:CLMtsCntl:CLMbhtTid:CLMpAcct:CLMinsID:
                   CLMpaSfx) pfalin;
          //*** End Chg *** 04/05/19 ***********************************
            Dow not %Eof(falinl);
               ExSr ChkAm2;
               ExSr WrtCap;                                                   //FDCAP
               ExSr UpdFalinp1;
               Select;
               When EObTyp = 'D';
               //***  ExSr WrtDdp;                                            //FADDP
               When EObTyp = 'H';
                 ExSr WrtDup;                                                 //FADUP
               When EObTyp = 'N';
                 ExSr WrtDhp;                                                 //FADHP
               EndSl;
               ExSr UpdFalinp2;
          //*** Beg Chg *** 04/05/19 ***********************************
          //***ReadE (CLMiCntl:CLMgCtl:CLMtsCntl:CLMbhtTid:CLMpAcct) pfalin;
               ReadE (CLMiCntl:CLMgCtl:CLMtsCntl:CLMbhtTid:CLMpAcct:
                      CLMinsID:CLMpaSfx) pfalin;
          //*** End Chg *** 04/05/19 ***********************************
            EndDo;
          EndIf;
       Endsr;
       //================================================================
       // WrtDup - Write FDDUP record
       //================================================================
       Begsr WrtDup;
          kkEob# = CAeob#;
          kkDseq = CAdSeq;
          Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pdu01;
          If Not %Found(fddul) or @@Mode = 'T';
             Clear pdu01;
             DUco#  = CAco#;
             DUSub# = CASub#;
             DUfnd  = CAfnd;
             DUfyr  = CAfyr;
             DUcase = CAcase;
             DUss#  = CAss#;
             DUacDt = CAacDt;
             DUeob# = CAeob#;
             DUdSeq = CAdSeq;
             Monitor;
             DUrvCd = %Dec(LINRvCd:3:0);
             On-Error;
             DUrvCd = 0;
             Endmon;
             DUrate = LINcode;
             // Place of service is based on Type of Bill
             Select;
             When %Subst(EUblTy:1:2) = '11';
                DUpos = '021';
             When %Subst(EUblTy:1:2) = '13';
                DUpos = '022';
             When %Subst(EUblTy:1:2) = '83';
                DUpos = '024';
             Other;
                DUpos = '022';
             Endsl;
             DUunts = CAunts;
             DUbPrc = DUrvCd;
             DUbRat = DUrate;

            // Load the description  from the HCPCS code or Revenue Code

             If LINdesc <> *blanks;
                DUdesc = LINdesc;
             Else;
                Callp GetProcCdDes('H':DUrate:xxAst:
                        CAfDat:'H':inMed#:outDesc);
                DUdesc = outDesc;
             Endif;
             If DUdesc = *blanks;
                inRvCd = %Char(DUrvCd);
                Callp GetProcCdDes('H':inRvCd:xxAst:
                        CAfDat:'R':inMed#:outDesc);
                DUdesc = outDesc;
             Endif;
             DUbPrD = DUdesc;

             DUdUcd = LINunBas;
             DUpMod = LINmod1;
             DUmod2 = LINmod2;
             DUmod3 = LINmod3;
             DUmod4 = LINmod4;
             // Service Code and Description
             DUpScd = *Blanks;
             DUscDs = *Blanks;
             DUstat = 'A';
             DUuser = Q1user;
             DUpgm  = Q1pgm;
             DUdt = CAdt;
             DUtime = CAtime;
             DUiUsr = CAuser;
             DUiPgm = CApgm;
             DUiDt = CAdt;
             DUiTim = CAtime;
             Write pdu01;
          Endif;
       Endsr;
       //================================================================
       // ChkAm1 - Check Amounts in upload file and convert to zero If blanks
       //================================================================
       Begsr ChkAm1;
          If CLMch = *Blanks;
             CLMch = *zeros;
          Endif;
       Endsr;
       //================================================================
       // ChkAm2 - Check Amounts in upload file and convert to zero If blanks
       //================================================================
       Begsr ChkAm2;
          If LINchrg = *Blanks;
             LINchrg = *Zeros;
          Endif;
       Endsr;
       //==============================================================
       // SRwrtRep - Error report                                           Sent" status
       //==============================================================
       Begsr SrWrtRep;
          If *inOF = *on;
             Except Head;
             *inOF = *off;
          Endif;
          Except Detail;
       Endsr;
       //==============================================================
       // SRwrtRep2 - Mass Adjudication Report                              Sent" status
       //==============================================================
       Begsr SrWrtRep2;
          If *inOA = *on;
             title = titl2;
             Except Head2;
             Except Head3;
             *inOA = *off;
          Endif;
          Except Detail2;
       Endsr;
       //==============================================================
       // SRwrtRep2 - Mass Adjudication Summary Report                      Sent" status
       //==============================================================
       Begsr SrWrtRepSum;
          *inOA = *on;
          title = titl3;
          Except Head2;
          Except Head4;
          Except Summary;
          *inOA = *off;
       Endsr;
      /End-free
     O*=========================================================================
     O*      R   E   P   O   R   T   S
     O*=========================================================================
     O*
     Oqsysprt1  E            Head           1 06
     O                       xxTime               8 '  :  :  '
     O                       Titl1               70
     O                       xxDate             128 '  /  /    '
     O          E            Head           1
     O                                           56 'Batch ID:'
     O                       FGHCntl             70
     O          E            Head           2
     O                       Q1pgm               10
     O                       Q1user             116
     O                       Page2         Z    128
     O                                          123 'Page'
     O          E            Head           2
     O                                           17 'ERROR DESCRIPTION'
     O          E            Detail         1
     O                       xxErrorMsg         125
     O*
     Oqsysprt2  E            Head2          1 06
     O                       xxTime               8 '  :  :  '
     O                       Title               89
     O                       xxDate             128 '  /  /    '
     O          E            Head2          1
     O                                           58 'Batch ID:'
     O                       FGHCntl             70
     O          E            Head2          1
     O                                           60 'Submitter: '
     O                       HDsbName           120
     O          E            Head2          2
     O                       Q1pgm               10
     O                       Q1user             116
     O                       Page2         Z    128
     O                                          123 'Page'
     O          E            Head3          2
     O                                           17 'EOB#'
     O                                           33 'Charges'
     O                                           49 'MRA'
     O                                           65 'Network'
     O                                           81 'ClaimCheck'
     O                                           97 'Other'
     O                                          113 'Paid'
     O          E            Detail2        1
     O                       EOeob#        4     17
     O                       EOchrg        j     34
     O                       EOmra         j     50
     O                       EOnDis        j     66
     O                       EOoth2        j     82
     O                       EOothr        j     98
     O                       EOpaid        j    114
     O          E            Detail2        2
     O                                           26 'Claim#:'
     O                       EOclaim#            42
     O                                           49 'FEIN:'
     O                       EOfein              59
     O                                           75 'Provider Name:'
     O                       EOpnam             116
     O          E            Head4          2
     O                                           19 'Count'
     O                                           33 'Charges'
     O                                           49 'MRA'
     O                                           65 'Network'
     O                                           81 'ClaimCheck'
     O                                           97 'Other'
     O                                          113 'Paid'
     O          E            Summary        1
     O                                            9 'Totals:'
     O                       SmEOBcnt      2     19
     O                       Smchrg        j     34
     O                       Smmra         j     50
     O                       SmnDis        j     66
     O                       Smoth2        j     82
     O                       Smothr        j     98
     O                       Smpaid        j    114
     O*_______________________________________________________________
