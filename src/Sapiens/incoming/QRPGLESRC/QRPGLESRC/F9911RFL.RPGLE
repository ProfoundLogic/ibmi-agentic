     //*********************************************************************
     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     //*********************************************************************
     //    PROGRAM: F9911RFL
     //    DESCRIPTION: FL Medical EDI Extract Revision Part 1
     //*********************************************************************
     //    PROGRAMMER NOTES: THE CONTROL# CNTL IS MADE UP OF
     //                      CCCYYJJJNNNNN
     //                      CCC=Submitter#, YY=Year, JJJ=Julian Date
     //                      NNNNN=The calculated sequence number
     //*********************************************************************
     //    CALLED FROM:
     //      F9811CFL - CREATE WORKFILE
     //****************************************************************
     //  CREATED: 08/13/15 C:212 : Project - FL092
     //  Copied from F9811RFL
     //*********************************************************************
     //  REVISIONS:
     //
      //  07/18/23 C:342 Project: NY060
      //                 NY Compliance: CARC/RARC Update
     //
      //  04/18/18 C:079 Project: MC421
      //                 Add value of A to EOtape selection
     //
      //  04/22/17 C:309 Support: 114/46582
      //                 Initialize DateOfOPSRV with Blanks instead of Zeros
     //
      //  03/17/17 C:079 Support: 114/45467
      //                 For DME send blanks in Days Supply
     //
      //  01/27/17 C:079 Support: 114/45389
      //                 Fix Proc string assignment
     //
      //  12/08/16 C:212 Retrofitted 44639 changes
      //  12/07/16 C:079 Support: 114/44639
      //                 Fix relative record number issue
     //
      //  12/06/16 C:212 Retrofitted 42888 changes
      //  09/14/16 C:079 Support: 114/42888
      //                 Add SQL processing
      //
      //  11/30/16 C:212 Support: 44466
      //                 Remove hyphen from NDC code for rev. F
      //
      //  09/16/16 C:079 Project: FL091 Phase 2
      //                 Make ICD type indicator flexible length
      //
      //  09/15/16 C:212 Added Support: 114/39393 changes (01/27/16)
      //                 earlier missing in base
      //
      //  09/01/16 C:309 Project : FL091 Phase 2
      //                 Adding logic for some additional reference codes,
      //                 from DHdxrE to DHdxrL.
    H*
     H* 01/27/16 C:079 Support: 114/39393
     H*                Handle Withdrawals
     //
      //
     //******************************************************************
     // INDICATOR USAGE:
     //
     //  99   -   GENERAL CHAIN
     //******************************************************************
     Ffeppfle   o  a f  700        disk
     Ffexpfle   o  a f  500        disk
     Ffehpfle   o  a f  700        disk
     Ffedpfle   o  a f  500        disk
     F*** Beg Chg *** 01/27/16 **************************************
     F*********faeol     uf   e             disk
     F*** Beg Chg *** 09/14/16 ************************************
     F****faeol     uf   e             disk    infds(DBfds)
     F*** Beg Chg *** 12/07/16 ************************************
     F*****faeol31   uf   e             disk    infds(DBfds)
     Ffaeol14   uf   e           k disk    infds(DBfds)
     F*** End Chg *** 12/07/16 ************************************
     F*** End Chg *** 09/14/16 ************************************
     F*** End Chg *** 01/27/16 **************************************
     Ffaeol4    uf   e           k disk    rename(peo01:peo02)
     Ffteol     if   e           k disk    rename(peo01:peo03)
     Fsmcol     if   e           k disk
     Ffmc1l     if   e           k disk
     Ffmf1l     if   e           k disk
     Ffdf2l     if   e           k disk
     Fsmorl     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffdcal5    if   e           k disk
     Ffdcal28   if   e           k disk    rename(pca01:pca28)
     Ffmcml1    if   e           k disk
     Ffmcdl     if   e           k disk
     Ffmpdl     if   e           k disk
     Ffmsml     if   e           k disk
     Ffmdil     if   e           k disk
     Ffaehl     uf   e           k disk
     Ffaeul     uf   e           k disk
     Ffaedl     uf   e           k disk
     Ffaepl     uf   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffddel4    if   e           k disk
     Ffmtsp     uf a e           k disk
     Ffaesl3    if   e           k disk
     Ffaeal1    uf   e           k disk
     Fftpmlfl   if   e           k disk
     Ffmpllfl   if   e           k disk
     Fststl     if   e           k disk
     Ffteblfl   if   e           k disk
     Ffmrxl     if   e           k disk
     Ffmicd10l1 if   e           k disk
     Fqsysprt   o    f  198        printer oflind(*inoa)
     F*** Beg Add *** 01/27/16 ********************************************
     Ffaesp     uf   e           k disk    rename(pes01:pes02)
     F*** End Add *** 01/27/16 ********************************************
     D*
     D getDiag         PR             7
     D  icd9                          5
     D*
     D getDiag10       PR             8
     D  icd10                         7
     D  indtyp                        1
      //=====================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D  dsmd                          4  0 overlay(dsdttm:7)
     D  dscy                          4  0 overlay(dsdttm:11)
     //
     D                 ds
     D xxdt                           8  0
     D  xxmm                          2  0 overlay(xxdt)
     D  xxdd                          2  0 overlay(xxdt:3)
     D  xxyy                          4  0 overlay(xxdt:5)
     D                 ds
     D xxcnt                          8  0
     D  xxmmdd                        4  0 overlay(xxcnt)
     D  xxyr                          4  0 overlay(xxcnt:5)
     //
     // Year prior to current date's year; used to calc Julian date
     D                 ds
     D zzdt                           8  0
     D  zzmm                          2  0 overlay(zzdt)
     D  zzdd                          2  0 overlay(zzdt:3)
     D  zzyy                          4  0 overlay(zzdt:5)
     //
     // Data Structure for Claim Control Number
     D                 ds
     D cntl                          13
     D  ccc                           3    overlay(cntl)
     D  yy                            2    overlay(cntl:4)
     D  jjj                           3    overlay(cntl:6)
     D  sssss                         5    overlay(cntl:9)
     D                 ds
     D icd9ref                        4
     D  icd9ref1                      1    overlay(icd9ref:1)
     D  icd9ref2                      1    overlay(icd9ref:2)
     D  icd9ref3                      1    overlay(icd9ref:3)
     D  icd9ref4                      1    overlay(icd9ref:4)
     D                 ds
     D @@blty                         3
     D  @@blt1                        1    overlay(@@blty:1)
     D  @@blt2                        1    overlay(@@blty:2)
     D  @@blt3                        1    overlay(@@blty:3)
     D***** Beg Chg *** 11/30/16 **********************************
     //
     // Data Structure for NDC Code
     D                 ds
     D @xxndc                        11
     D  ndc5                          5    overlay(@xxndc)
     D  ndc4                          4    overlay(@xxndc:6)
     D  ndc2                          2    overlay(@xxndc:10)
     //
     // Data Structure for NDC Code
     D*                ds
     D*@xxndc                        13
     D* ndc5                          5    overlay(@xxndc)
     D* hyphen1                       1    overlay(@xxndc:6)
     D* ndc4                          4    overlay(@xxndc:7)
     D* hyphen2                       1    overlay(@xxndc:11)
     D* ndc2                          2    overlay(@xxndc:12)
     //
     D***** End Chg *** 11/30/16 **********************************
     //
     D***** Beg Chg *** 11/30/16 **********************************
     D***** Beg add *** 09/01/16 **********************************
     //
     // Data Structure for NDC Code 2
     D                 ds
     D xxndc_ds                      11
     D  ndc_5                         5    overlay(xxndc_ds)
     D  ndc_4                         4    overlay(xxndc_ds:6)
     D  ndc_2                         2    overlay(xxndc_ds:10)

     D*                ds
     D*xxndc_ds                      13
     D* ndc_5                         5    overlay(xxndc_ds)
     D* hyp1                          1    overlay(xxndc_ds:6)
     D* ndc_4                         4    overlay(xxndc_ds:7)
     D* hyp2                          1    overlay(xxndc_ds:11)
     D* ndc_2                         2    overlay(xxndc_ds:12)
     D***** End add *** 09/01/16 **********************************
     D***** End Chg *** 11/30/16 **********************************
     //
     D sys_date        s               D   datfmt(*USA)
     //
     // External Data Structure
     D psds          esds                  extname(szq1p)
     D*** Beg Add *** 09/14/16 ****************************************
     D dsFAEOP       e ds                  extname(FAEOP)
     D*** End Add *** 09/14/16 ****************************************
     //---------------------------------------------------------------------
     // Stand Alone Fields - TOP
     //---------------------------------------------------------------------
     D accdt           s              8
     D admissdt        s              8
     D admisshr        s              2
     D certifictn      s              1
     D billedproc      s              5
     D billedmod       s              2
     D billedmod1      s              2
     D billedmod2      s              2
     D billedmod3      s              2
     D billedmod4      s              2
     D dispaswrit      s              1
     D drugsuppind     s              1
     D empdob          s              8
     D empsex          s              1
     D extinjcd1       s              8
     D extinjcd2       s              8
     D extinjcd3       s              8
     D hcpcs           s                   like(dphcpc)
     D billrcvdt       s              8
     D billpaydt       s              8
     D cntr            s              5  0
     D datefilled      s              8
     D dateofsrvfrm    s              8
     D dateofsrvto     s              8
     D dateofopsrv     s              8
     D*** Beg Chg *** 03/17/17 ***********************************
     D***** dayssupply      s              3S 0
     D dayssupply      s              3A
     D*** End Chg *** 03/17/17 ***********************************
     D detchrg         s             11
     D detpaid         s             11
     D detseq#         s              3
     D d1              s              2  0
     D d2              s              2  0
     D digits          c                   '0123456789'
     D digibk          c                   ' 0123456789'
     D dischrghr       s              2
     D ecnt            s              1  0
     D emplid          s              9
     D eobcode1        s              2
     D eobcode2        s              2
     D eobcode3        s              2
     D facilzip        s              9
     D fill2           s              2    inz(*blanks)
     D fill32          s             32    inz(*blanks)
     D fill90head      s            235    inz(*blanks)
     D fill10head      s            259    inz(*blanks)
     D fill09head      s            344    inz(*blanks)
     D fill11head      s             50    inz(*blanks)
     D fill166         s            166    inz(*blanks)
     D fill10trhdr     s            263    inz(*blanks)
     D fill09det       s            378    inz(*blanks)
     D fill237         s            237    inz(*blanks)
     D fill10trtrl     s            291    inz(*blanks)
     D fill90dtl       s            390    inz(*blanks)
     D fill263         s            263    inz(*blanks)
     D fill291         s            291    inz(*blanks)
     D fill90trhdr     s            413    inz(*blanks)
     D fill10det       s            391    inz(*blanks)
     D fill90trtrl     s            441    inz(*blanks)
     D first09         s              1
     D first10         s              1
     D first11         s              1
     D first90         s              1
     D firstnm         s             15
     D formid          s              2
     D insurcode       s              5
     D insurtaxid      s              9
     D insurzip        s              9
     D strICDA         s              8
     D strICDB         s              8
     D strICDC         s              8
     D strICDD         s              8
     D strICDE         s              8
     D strICDF         s              8
     D strICDG         s              8
     D strICDH         s              8
     D strICDI         s              8
     D strICDJ         s              8
     D strICDK         s              8
     D strICDL         s              8
     D strResubCode    s             11
     D*** Beg Chg *** 09/16/16 *************************************
     D*** isICD10         s              2
     D isICD10         s              1
     D*** End Chg *** 09/16/16 *************************************
     D*** Beg Add *** 09/16/16 *************************************
     D isICD102A       s              2
     D*** End Add *** 09/16/16 *************************************
     D insintfile#     s             30
     D juldat          s              5
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D kkast           s                   like(eost)
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D kkdseq          s                   like(cadseq)
     D @@dseq          s                   like(cadseq)
     D@@units          s              3  0
     D kkmseq          s                   like(camseq)
     D kkrcn           s                   like(eorcn)
     D kkkey           s                   like(stkey)
     D kkdiag          s                   like(diicd9)
     D kkdiag10        s              7    inz
     D kkind           s              1    inz
     D kkfein          s                   like(eofein)
     D kkmod           s                   like(eomod)
     D kkss#           s                   like(eoss#)
     D kkesrc          s                   like(deesrc)
     D kkprt           s              1
     D kkndc           s                   like(rxndc)
     D lastnm          s             30
     D license#        s             13
     D operlicense     s             13
     D lostTime        s              2
     D middin          s              1
     D neworrefill     s              1
     D numclm09        s              6  0
     D numclm10        s              6  0
     D numclm11        s              6  0
     D numclm90        s              6  0
     D otherprcd_a     s              8
     D otherprcd_b     s              8
     D otherprcd_c     s              8
     D otherprcd_d     s              8
     D otherprcd_e     s              8
     D paidproc        s              5
     D paidmod         s              2
     D paidmod1        s              2
     D paidmod2        s              2
     D paidmod3        s              2
     D paidmod4        s              2
     D paycode         s              2
     D payplan         s              2
     D pharmacy#       s             10
     D placesrv        s              2
     D placetmt        s              2
     D prescbrlic      s             13
     D princprcd       s              5
     D procmth         s              1
     D provtaxid       s              9
     D qqco#           s              3
     D qqsub#          s              5
     D qqfrdt          s              8
     D qqproc          s              1
     D qqque           s             10
     D qqtodt          s              8
     D qqtpin          s              1
     D qtyofmed        s              5S 0
     D recflg          s              1
     D reportreas      s              2
     D revind          s              2
     D revcode         s              4
     D servcocode      s              5
     D servcotaxid     s              9
     D servcozip       s              9
     D skpdtl10        s              1
     D subloc          s             20
     D srdays          s              5  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srfrdc          s              8  0
     D srterm          s              1
     D srtfmt          s              8
     D srtodc          s              8  0
     D stmtcovfrmdt    s              8
     D stmtcovtomdt    s              8
     D statemntdt      s              8
     D submdt          s              8
     D submid          s              3
     D submtid         s              9
     D submzip         s              9
     D totpay          s             11  2
     D totpaysupp      s             11  2
     D imptotpay       s             11  2
     D totchrg         s             11  2
     D tpin            s              1
     D tranhdrmkr      s              3
     D trantlrmkr      s              3
     D typeadm         s              1
     D typebill        s              3
     D units           s              3  0
     D @units          s              3  0 INZ
     D units90         s              7  0
     D x               s              2  0
     D xqfrdt          s              8  0
     D xqtodt          s              8  0
     D xxconm          s             40
     D xxdupovrind     s              1
     D xxemplppind     s              1
     D purchrent       s              1
     D schdunschdind   s              1
     D xxeob           s              2
     D xxesrc          s                   like(deesrc)
     D xxfnnm          s             40
     D xxfrdt          s              7  0
     D xxfrom          s              8  0
     D xxkey           s                   like(pmkey)
     D xxn             s              2  0
     D xxndc           s                   like(dhndc)
     D xxndcwk         s                   like(dhndc)
     D xxproc          s              9
     D xxprt           s              1
     D xxto            s              8  0
     D xxtodt          s              7  0
     D xxunit          s              7  0
     D xxunts          s              2  0
     D xxsub#          s                   like(smsub#)
     D xxsbnm          s                   like(smname)
     D xxco#           s                   like(c1co#)
     D x1rcn           s                   like(eorcn)
     D zipserv         s              9
     D emppayment      s                   like(*in99)
     Dxxtotpay         s             11  0 inz(0)
     Dfac_license      s             13
     Dfac_npi#         s             10
     Dinspayment       s             11
     D strNDC2nd       s             13
    D*** Beg Add ***** 01/27/16 *******************************************
     d WrtBillStatus   pr                  ExtPgm('F0395R01')
     d   ##kc                         1
     d   ##kl                         1
     d   ##mode                       1
     d   ##rrn                        9  0
     d   ##rrn2                       9  0
     d   ##blSt                       1
     d   ##stCd                       3
     d   ##reas                      60
     d   ##pgm                        8
     d   ##ltr#                       9  0
     d   ##edi                        1
     D
     d   ##kc          s              1
     d   ##kl          s              1
     d   ##mode        s              1
     d   ##rrn         s              9  0
     d   ##rrn2        s              9  0
     d   ##blSt        s              1
     d   ##stCd        s              3
     d   ##reas        s             60
     d   ##pgm         s              8
     d   ##ltr#        s              9  0
     d   ##edi         s              1
     d
     D DBfds           ds
     D DBrrn                 397    400B 0
    D*** End Add ***** 01/27/16 *******************************************
     D*** Beg Add *** 09/14/16 ****************************************
     D sqlStmtStr      s            700a
     D qqfnds          s            360A
     D strFnds         s            360A
     D strProc         s             20A
     D*** Beg Chg *** 04/18/18 *********************************************
     D*** strTape         s             20A
     D strTape         s             30A
     D*** End Chg *** 04/18/18 *********************************************
     D moreRows        s               n   inz(*on)
     D qqRevi          s              1A
     D qqOsub          s              1A
     D*** End Add *** 09/14/16 ****************************************

     //---------------------------------------------------------------------
     // Stand Alone Fields - BOTTOM
     //---------------------------------------------------------------------
    
     // Entry Parameters
     //
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfrdt
     C                   parm                    qqtodt
     C                   parm                    qqque
     C                   parm                    qqproc
     C                   parm                    qqtpin
     C*** Beg Add *** 09/14/16 **********************************
     C                   parm                    qqFnds
     C                   parm                    qqRevi
     C                   parm                    qqOsub
     C*** End Add *** 09/14/16 **********************************
     //
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     //
      //
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      //
     // Key List for the Company Master File
     //
     C     cokey         klist
     C                   kfld                    kkco#
     //
     // Key List for the Company AUXILIARY Master File
     //
     C     c1ky          klist
     C                   kfld                    kkco#
     //
     // Key List for the FUND AUXILIARY Master File
     //
     C     f1key         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     //
     //
     C     eoky1         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     //
     // Key List for the Output Routing Master File
     //
     C     orkey         klist
     C                   kfld                    q1pgm
     C                   kfld                    kkprt
     //
     // Key List for the SUBCRIBER   File
     //
     C     smkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     //
     // Key List for the Claim Master File
     //
     C     cmkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     //
     //
     // Key List for the Claimant Demographics Master File
     //
     C     cdkey         klist
     C                   kfld                    kkss#
     //
     // Key List for the Provider Master File
     //
     C     pdkey         klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     //
     // Key List for the GROUP STATE INFORMATION FILE
     //
     C     f2key         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkast
     //
     // Key List for the TAPE SUBMISSION SEQ# FILE
     //
     C     tskey         klist
     C                   kfld                    kkco#
     //
     // Key List for the DIAGNOSIS CODE FILE
     //
     C     diky          klist
     C                   kfld                    kkdiag
     //
     // Key List for the EOMB Header FILES
     //
     C     hdky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     //
     // Key List for the EOMB DETAIL CHARGE FILE
     //
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     //
     // Key List for the CLAIM DETAIL FILE
     //
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkmseq
     //
     // Key List for the EOMB DETAIL FILES
     //
     C     dtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     //
     // Key List for the EOMB DETAIL EOB CODE FILE
     //
     C     deky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkesrc
     //
     //  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     //
     C     esky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     //
     C     esky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     //
     //  EOMB Header Amount file
     //
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     //
     //  Procedure code modifier table file
     //
     C     pmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     //
     //* Key List for State Code Table File
     //
     C     stky          klist
     C                   kfld                    kkkey
     //
     //* Key List for State EOB Code Table file
     //
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C     rxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkndc
      //=====================================================================
     // MAIN PROGRAM BEGINS
      //=====================================================================
     //
      /FREE
       exsr pgmstr;
      //
      //  BEGIN READING BATCH
      //
       //
       //*** Beg Del *** 09/14/16 **************************************
       //*** if ##clt# = '051';
       //***  read peo03;
       //***  *IN99 = %EOF;
       //*** else;
       //***  read peo01;
       //***  *IN99 = %EOF;
       //*** endif;
      //
       //*** if *in99 = *on;
       //***  except errms2;
       //***  exsr pgmend;
       //*** endif;
       //*** End Del *** 09/14/16 **************************************
       //*** Beg Add *** 09/14/16 **************************************
      /end-free
     C* Form the SQL statement
     C*
     C                   if        qqProc = 'A'
     C                   eval      strProc = '''N'',''H'',''D'',''X'''
     C                   eval      strProc = %trimr(strProc)
     C*
     C                   else
     C                   eval      strProc = qqProc
     C*
     C                   if        strProc = 'P'
     C                   eval      strProc = 'N'
     C                   endif
     C*
     C*** Beg Chg *** 01/27/17 *********************************************
     C****************** eval      strProc = ''' + strProc + '''
     C                   eval      strProc = '''' + %trimr(strProc) + ''''
     C*** End Chg *** 01/27/17 *********************************************
     C*
     C                   endif
     C*
     C                   Select
     C
     C*
     C     qqOsub        WhenEq    'Y'
     C                   eval      strFnds = qqFnds
     C                   eval      strFnds = %trim(strFnds)
     c                   eval      sqlStmtStr = 'Select * ' +
     c                             'from FAEOP where EOfnd in ' +
     c                             '('+ %trimr(strFnds) + ')' + ' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EObtyp in ' +
     c                             '('+ %trimr(strProc) + ')' + ' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOwcck >= '  + %editc(xxfrdt:'X')
     c                             + ' and EOwcck <= ' + %editc(xxtodt:'X')
     c                             + ' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOcntl = '' '' and '
     c                   eval      sqlStmtStr=%trim(sqlStmtStr) +
     c                             ' EOtape = ''E'' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOsub# = ' + %editc(kkSub#:'X') + ' and '
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOblst = ''C'' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOast = ''FL'' and '
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOrevi = ' + '''' + %trim(qqRevi) + ''''
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' Order by EOco#,EOsub#,EOfnd,EOfyr, EOcase,'
     c                             + 'EOtrdt, EOfein, EOmod, EOeob#'
     c*
     c     qqOsub        whenNe    'Y'
     C*** Beg Chg *** 04/18/18 ***************************************
     C**************     eval      strTape = '''W'',''R'',''C'',''B'',''D'''
      /Free
                         strTape = '''W'',''R'',''C'',''B'',''D'',''A''';
      /End-free
     C*** End Chg *** 04/18/18 ***************************************
     c                   eval      sqlStmtStr = 'Select * ' +
     c                             'from FAEOP where '
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EObtyp in ' +
     c                             '('+ %trimr(strProc) + ')' + ' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOwcck >= '  + %editc(xxfrdt:'X')
     c                             + ' and EOwcck <= ' + %editc(xxtodt:'X')
     c                             + ' and'
     c                   eval      sqlStmtStr=%trim(sqlStmtStr) +
     c                             ' EOtape in ' +
     c                             '('+ %trimr(strTape) + ')' + ' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOsub# = ' + %editc(kkSub#:'X') + ' and '
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOblst = ''C'' and'
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOast = ''FL'' and '
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' EOrevi = ' + '''' + %trim(qqRevi) + ''''
     c                   eval      sqlStmtStr = %trim(sqlStmtStr) +
     c                             ' Order by EOco#,EOsub#,EOfnd,EOfyr, EOcase,'
     c                             + 'EOtrdt, EOfein, EOmod, EOeob#'
     c*
     c                   EndSl

     c/exec sql
     c+ prepare stmt from :sqlStmtStr
     c/end-exec

     c/exec sql
     c+ declare eobcursor cursor for stmt
     c/end-exec

     c/exec SQL
     c+ open eobcursor
     c/end-exec
     C*** End Add *** 09/14/16 **************************************
     C*** Beg Add *** 09/14/16 **************************************

     c/exec SQL
     c+ fetch next from eobcursor
     c+   into :dsFAEOP
     c/end-exec

     c                   if        sqlstt <> '00000'
     c                   eval      moreRows = *off
     c                   except    errms2
     c                   exsr      pgmend
     c                   endif
     c*
     c/free
       //*** End Add *** 09/14/16 **************************************
      //
       //*** Beg Chg *** 09/14/16 **************************************
       //*** dow *in99 = *off;
       dow moreRows = *on;
       //*** End Chg *** 09/14/16 **************************************
       //*** Beg Del *** 09/14/16 **************************************
       //*** if ##clt# = '051';
       //*** chain eoky1 peo02;
       //*** *IN99 = NOT %FOUND;
       //*** endif;
       //*** if *in99=*off;
       //*** End Del *** 09/14/16 **************************************
          //
          // If check date is < recon date then batch has not updated check
          // date so don't include this EOB
          //  Get data from various files, group, company, etc.
          //
           exsr getFiledata;
          //
           select;
            //
           when eobtyp = 'N';
             exsr medsr;
            //
           when eobtyp = 'D';
             exsr dentsr;
            //
           when eobtyp = 'X';
             exsr pharsr;
            //
           when eobtyp = 'H';
             exsr hossr;
             //
           endsl;
          //
       //*** Beg Del *** 09/14/16 **************************************
       //*** endif;
       //*** End Del *** 09/14/16 **************************************
        //
       //*** Beg Del *** 09/14/16 **************************************
       //***  if ##clt# = '051';
       //***    read peo03;
       //***    *IN99 = %EOF;
       //***  else;
       //***    read peo01;
       //***    *IN99 = %EOF;
       //***  endif;
       //*** End Del *** 09/14/16 **************************************
       //*** Beg Add *** 09/14/16 **************************************
      /end-free
     c***
     c/exec SQL
     c+ fetch next from eobcursor
     c+   into :dsFAEOP
     c/end-exec

     c                   if        sqlstt <> '00000'
     c                   eval      moreRows = *off
     c                   endif
     c/free
       //*** End Add *** 09/14/16 **************************************
        //
       enddo;
      //
       exsr pgmend;
      /END-FREE
      /Free
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
      /End-free
      //=====================================================================
      /FREE
       begsr checkRecon;
         //=====================================================================
        //
        // Only select esrrdt where status is 'R'
        //
         if eorcn > 0;
          //
           setgt esky1 pes01;
           readpe esky2 pes01;
           *IN60 = %EOF;
           dow *in60 = *off;
            //
             if esblst = 'R';
               leave;
             endif;
            //
             readpe esky2 pes01;
             *IN60 = %EOF;
            //
           enddo;
          //
         endif;
        //
       endsr;
       //=====================================================================
       // SUBROUTINE DENTSR
       //=====================================================================
       begsr dentsr;
         //
         // First time through write the transmission header info initialize
         // claim counter for DWC11 claims
         //
         if first11 = 'Y';
           numclm11 = 0;
           exsr lodthdr;
           except tranHdr11;
           first11 = *blank;
         endif;
         //
         // Sum up the number of claims in the transmission for the trailer
         //
      /END-FREE
     C                   add       1             numclm11
      //
      // Load header info for the current EOB
      //
      /FREE
         exsr lodbhdr;
         //
         except billHdr11;
         //
         chain caky pca01;
         *IN99 = NOT %FOUND;
         chain caky pca01;
         *IN99 = NOT %FOUND;
         //
         dow not *in99;
           //
          If emppayment = *off;
             exsr loddtl11;
             except billDtl11;
          Endif;
           //
           reade caky pca01;
           *IN99 = %EOF;
           //
         enddo;
         //
       endsr;
      //
       //=====================================================================
       // SUBROUTINE gencntlnum
       //=====================================================================
       begsr gencntlnum;
        //
      /END-FREE
     C                   move      c1subm        ccc
     C                   move      xxyy          yy
     C                   move      srdays        jjj
      /FREE
         cntr = cntr + 1;
      /END-FREE
     C                   move      cntr          sssss
     C                   movel     cntl          eocntl
     C                   movel     dsdate        eosbdt
     //
      /FREE
       endsr;
       //=====================================================================
      // SUBROUTINE getFiledata
       //=====================================================================
       begsr getFiledata;
         //
         kkco#  = eoco#;
         kksub# = eosub#;
         kkfnd  = eofnd;
         kkast  = eoast;
         kkfyr  = eofyr;
         kkcase = eocase;
         kkeob# = eoeob#;
         kkrcn  = eorcn;
         kkss#  = eoss#;
         kkfein = eofein;
         kkmod  = eomod;
        //
        If EOEPMT = 'Y';
          emppayment = *on;
        Else;
          emppayment = *off;
        Endif;
        // Get the fund auxiliary record
        //
         chain f1key pf101;
         *IN99 = NOT %FOUND;
        //
        // Get the Claim Master Record
        //
         chain cmkey pcm01;
         *IN99 = NOT %FOUND;
        //
        // Get the Claimant Demographic Record
        //
         chain cdkey pcd01;
         *IN99 = NOT %FOUND;
        //
        // Get the Provider Master Record
        //
         chain pdkey ppd01;
         *IN99 = NOT %FOUND;
        //
        // Get the group state information
        //
         chain f2key pf201;
         *IN99 = NOT %FOUND;
         //
       endsr;
       //=====================================================================
       // SUBROUTINE HOSSR
       //=====================================================================
       begsr hossr;
         //
         // First time through write the transmission header info initialize
         // claim counter for DWC90 claims
         //
         if first90 = 'Y';
           numclm90 = 0;
           exsr lodthdr;
           except tranHdr90;
           first90 = *blank;
         endif;
         //
         // Sum up the number of claims in the transmission for the trailer
         //
      /END-FREE
     C                   add       1             numclm90
      //
      // Load header info for the current EOB
      //
      /FREE
         exsr lodbhdr;
         //
         except billHdr90;
         //
         chain caky pca01;
         *IN99 = NOT %FOUND;
         //
         dow not *in99;
           //
          If emppayment = *off;
             exsr loddtl90;
             except billDtl90;
          Endif;
           //
           reade caky pca01;
           *IN99 = %EOF;
           //
         enddo;
         //
       endsr;
      //
       //=====================================================================
       // SUBROUTINE loadbhdr - Load bill header info
       //=====================================================================
       begsr lodbhdr;
        //
        // Initialize fields
        //
      /END-FREE
     C                   move      '1'           recflg
     C                   move      '00'          formid
     C                   move      *blanks       lostTime
     C                   move      '00000'       insurcode
     C                   move      '000000000'   insurtaxid
     C                   move      *blanks       insurzip
     C                   move      '00000'       servcocode
     C                   move      '000000000'   servcotaxid
     C                   move      *blanks       servcozip
     C                   move      *blanks       emplid
     C                   move      *zeros        typebill
     C                   move      *zeros        accdt
     C                   move      *blanks       license#
     C                   move      '000000000'   provtaxid
     C                   move      *blanks       zipserv
     C                   move      *blanks       lastnm
     C                   move      *blanks       firstnm
     C                   move      *blanks       middin
     C                   move      *zeros        admissdt
     C                   move      *zeros        admisshr
     C                   move      *blanks       dischrghr
     C                   move      *zeros        stmtcovfrmdt
     C                   move      *zeros        stmtcovtomdt
     C                   move      *blanks       placetmt
     C                   move      *zeros        billrcvdt
     C                   move      *zeros        billpaydt
     C                   move      *blanks       reportreas
     C                   move      *blanks       payplan
     C                   move      *blanks       paycode
     C                   move      *zeros        statemntdt
     C                   movel     *zeros        totpay
     C                   movel     *zeros        totchrg
     C                   move      *blanks       strICDA
     C                   move      *blanks       strICDB
     C                   move      *blanks       strICDC
     C                   move      *blanks       strICDD
     C                   move      *blanks       strICDE
     C                   move      *blanks       strICDF
     C                   move      *blanks       strICDG
     C                   move      *blanks       strICDH
     C                   move      *blanks       strICDI
     C                   move      *blanks       strICDJ
     C                   move      *blanks       strICDK
     C                   move      *blanks       strICDL
     C                   move      *blanks       isICD10
     C                   move      *blank        procmth
     C                   move      *blanks       princprcd
     C                   move      *blanks       otherprcd_a
     C                   move      *blanks       otherprcd_b
     C                   move      *blanks       otherprcd_c
     C                   move      *blanks       otherprcd_d
     C                   move      *blanks       otherprcd_e
     C                   move      *blanks       extinjcd1
     C                   move      *blanks       extinjcd2
     C                   move      *blanks       extinjcd3
     C                   move      *zeros        typeadm
     C                   move      *blanks       facilzip
     C                   move      *blanks       insintfile#
     C                   move      *blanks       subloc
     C                   move      *blanks       xxemplppind
     C                   move      *blanks       xxdupovrind
     C                   move      *blanks       fac_license
     C                   move      *blanks       fac_npi#
     C                   move      *blanks       strResubCode
     //
     // Determine Report Reason Code
     //
      /FREE
         select;
           //
         when eotape = 'E' and eocntl = *blanks;
           reportreas = '00';
           //
         when eotape = 'W' and eocntl <> *blanks;
           reportreas = '01';
           //
         when eotape = 'R' and eocntl <> *blanks
               or
               eotape = 'D' and eocntl = *blanks;
           reportreas = '02';
           //
         when eotape = 'C' and eocntl <> *blanks
               and eorcn > 0;
           reportreas = '03';
         when eotape = 'B' and eocntl <> *blanks;
           reportreas = '03';
           //
         other;
           reportreas = '99';
           //
         endsl;
         //*** Beg Del *** 09/14/16 ************************************
        //
        // Build the control number and increment it
        //
         //*** if eocntl = *blanks;
         //***  exsr gencntlnum;
         //*** else;
         //***  cntl = eocntl;
         //*** endif;
         //*** End Del *** 09/14/16 ************************************
         //
         // Duplicate Override Indicator
         //
         if eotape = 'D';
           xxdupovrind = 'Y';
         endif;
        //
        // Update the bill header files FAEOP/FAEAP
        //
         exsr updathdr;
        //
        // Load the Lost time info
        //
         if cmltfl = 'I';
           lostTime = 'LT';
         else;
           lostTime = 'MO';
         endif;
        //
        // Load Insurer header info
        //
         if f2fl01 = 'Y';
      /END-FREE
     C                   move      eosemp        insurcode
     C                   move      eoefid        insurtaxid
     C                   movel     eoezip        insurzip
      /FREE
           if %subst(insurzip:6:4) = '0000';
      /END-FREE
     C                   movel     eoezip        xxzip             5
     C                   movel(P)  xxzip         insurzip
      /FREE
           endif;
         else;
      /END-FREE
     C                   move      f25n01        insurcode
     C                   move      f29n01        insurtaxid
     C                   move      f29n02        insurzip
      /FREE
           if %subst(insurzip:6:4) = '0000';
      /END-FREE
     C                   movel     f29n02        xxzip             5
     C                   movel(P)  xxzip         insurzip
      /FREE
           endif;
         endif;
        //
        // Load Service Co/TPA info
        //
        If emppayment = *off;
      /END-FREE
     C                   move      f25n02        servcocode
     C                   move      f29n03        servcotaxid
     C                   move      f29n04        servcozip
      /FREE
        Else;
           servcocode = *blanks;
           servcotaxid = *blanks;
           servcozip   = *blanks;
         Endif;
         if %subst(servcozip:6:4) = '0000';
      /END-FREE
     C                   movel     f29n04        xxzip             5
     C                   movel(P)  xxzip         servcozip
      /FREE
         endif;
         //
         // If info for service company is missing then load blanks
         //
         if servcocode = *zeros;
           servcocode = *blanks;
         endif;
         if servcotaxid = *zeros;
           servcotaxid = *blanks;
         endif;
         if servcozip   = '00000    ';
           servcozip   = *blanks;
         endif;
        //
        // Load Employee info
        //
      /END-FREE
     C                   movel     cmss#         emplid
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         accdt
      //
      // Last name
      //
      /FREE
         flag = *blanks;
         string = cdlsn;
         patt1  = cdlsn;
      /END-FREE
     C                   call      'F9716CFL'
     C                   parm                    string           40
     C                   parm                    patt1            15
     C                   parm                    flag              1
      /FREE
         lastnm = string;
         //
         // First name
         //
         flag = *blanks;
         string = cdfsn;
         patt1  = cdfsn;
      /END-FREE
     C                   call      'F9716CFL'
     C                   parm                    string
     C                   parm                    patt1
     C                   parm                    flag
      /FREE
         firstnm = string;
         //
         // Middle Initial
         //
         flag = *blanks;
         string = cdmid;
         patt1  = cdmid;
      /END-FREE
     C                   call      'F9716CFL'
     C                   parm                    string
     C                   parm                    patt1
     C                   parm                    flag
      /FREE
         middin = string;
         if ##clt# = '010';
           //
           d1 = 0;
           middin = ' ';
      /END-FREE
     C     ' '           scan      firstnm       d1
      //
      /FREE
           select;
           when d1 > 0 and d1 < (%len(firstnm) - 1);
             //
             if %subst(firstnm:d1+2:1) =  '.' or
                   %subst(firstnm:d1+2:1) =  ' ';
               middin = %subst(firstnm:d1+1:1);
               %subst(firstnm:d1) = *blanks;
             endif;
             //
           when d1 > 0 and d1 = (%len(firstnm) - 1);                          //last pos in field
             //
             if %subst(firstnm:d1+1:1) <> '.' and
                   %subst(firstnm:d1+1:1) <> ' ';
               middin = %subst(firstnm:d1+1:1);
               %subst(firstnm:d1) = *blanks;
             endif;
             //
           endsl;
         endif;                                                               //##clt# = 010
        //
        // Employee Date of Birth
        //
         if cddtob <> *zeros;
      /END-FREE
     C                   z-add     cddtob        srdti
     C                   call      'SRCVTDAT'    dtyymd
      /FREE
          If emppayment = *off;
      /END-FREE
     C                   movel     srdto         empdob
      //                  eval      empdob = cddtob
      /FREE
          Else;
      /END-FREE
     C                   movel     *blanks       empdob
      /FREE
          Endif;
         endif;
        //
        // Employee Gender
        //
        If emppayment = *off;
           if cdsex <> *blanks;
             empsex = cdsex;
           else;
             empsex = 'U';
           endif;
        Else;
      /END-FREE
     C                   movel     *blanks       empsex
      /FREE
        Endif;
         //
         if eoepmt = 'Y';
           xxemplppind = 'E';
         endif;
         if eotodt > EOCRDT or eotodt > EOWCCK;
           xxemplppind = 'P';
         endif;
        //
        // Load Provider info
        //
        If emppayment = *off;
      /END-FREE
     C                   move      eofein        provtaxid
      /FREE
        Else;
      /END-FREE
     C                   movel     *blanks       provtaxid
      /FREE
        Endif;
         If eoflic <> *blanks;
      /END-FREE
     C                   movel     eoflic        facilZip
      /FREE
         else;
      /END-FREE
     C                   move      eozip         facilZip
      /FREE
         endif;
         if %subst(facilzip:6:4) = '0000';
           If eoflic <> *blanks;
      /END-FREE
     C                   movel     eoflic        xxzip
      /FREE
           else;
      /END-FREE
     C                   movel     eozip         xxzip             5
      /FREE
           endif;
      /END-FREE
     C                   movel(P)  xxzip         facilzip
      /FREE
         endif;
        If emppayment = *off;
      /END-FREE
     C                   movel     eoflic        zipserv
      /FREE
           if %subst(zipserv:6:4) = '0000';
      /END-FREE
     C                   movel     eoflic        xxzip             5
     C                   movel(P)  xxzip         zipserv
      /FREE
           endif;
        Else;
      /END-FREE
     C                   movel     *blanks       zipserv
      /FREE
        Endif;
         //
        // Load Date Insurer Received Bill
        //
         exsr checkRecon;
        if eorcn > 0 and esrrdt <> 0
              and esrrdt <= eowcck;
      /END-FREE
     C                   z-add     esrrdt        srdti
      /FREE
         else;
      /END-FREE
     C                   z-add     eocrdt        srdti
      /FREE
         endif;
        //
      /END-FREE
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         billrcvdt
      //
     // Load Date Insurer Paid Bill
     //
      //
      // USE THE CHECK MAILING DATE TO REPORT DATE PAID IF NOT BLANK
     C
      /FREE
         EXSR GETMDT;
         //
         IF #1CMDT <> *BLANKS;
           //
      /END-FREE
     C                   MOVE      #1CMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         BILLPAYDT
      //
      /FREE
         ELSE;
           //
           //
      /END-FREE
     C                   z-add     eowcck        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         billpaydt
      //
      //
      /FREE
         ENDIF;                                                               //#1CMDT
         //
         //
        // Load Total Charged/Paid
        //
      /END-FREE
     C                   z-add     eopaid        totpay
     C                   z-add     eochrg        totchrg
     C                   movel(P)  eoeob#        insintfile#
      //
     // Load Header info for Physician Bills
     //
      /FREE
         select;
           //
         when eobtyp = 'N';
           //
           // Load Form id
           //
      /END-FREE
     C                   move      '09'          formid
      //
      /FREE
           chain hdky peh01;
           *IN99 = NOT %FOUND;
           //
           if not *in99;
             //
             // Load Provider License Number
             //
            If emppayment = *off;
      /END-FREE
     C                   movel(P)  ehflic        license#
      /FREE
            Else;
      /END-FREE
     C                   movel     *Blanks       license#
      /FREE
             Endif;
             //
             // Load Payment Plan
             //
            If emppayment = *off;
               if eoepmt <> 'Y';
      /END-FREE
     C                   move      ehppln        payplan
      /FREE
               endif;
            Else;
      /END-FREE
     C                   movel     *Blanks       payplan
      /FREE
             Endif;
             //
             // Load diagnosis codes
             //
            If emppayment = *off;
              if EHICDInd <> 'Y';
                 strICDA = getDiag(ehdia1);
                 strICDB = getDiag(ehdia2);
                 strICDC = getDiag(ehdia3);
                 strICDD = getDiag(ehdia4);
               //***** Beg add *** 09/01/16 **********************************
                 strICDE = getDiag(ehicdE);
                 strICDF = getDiag(ehicdF);
                 strICDG = getDiag(ehicdG);
                 strICDH = getDiag(ehicdH);
                 strICDI = getDiag(ehicdI);
                 strICDJ = getDiag(ehicdJ);
                 strICDK = getDiag(ehicdK);
                 strICDL = getDiag(ehicdL);
               //***** End add *** 09/01/16 **********************************
                 isICD10 = '9';
              else;
                 kkind = 'D';
                 strICDA = getDiag10(EHICD1001:kkind);
                 strICDB = getDiag10(EHICD1002:kkind);
                 strICDC = getDiag10(EHICD1003:kkind);
                 strICDD = getDiag10(EHICD1004:kkind);
               //***** Beg add *** 09/01/16 **********************************
                 strICDE = getDiag10(EHICDE:kkind);
                 strICDF = getDiag10(EHICDF:kkind);
                 strICDG = getDiag10(EHICDG:kkind);
                 strICDH = getDiag10(EHICDH:kkind);
                 strICDI = getDiag10(EHICDI:kkind);
                 strICDJ = getDiag10(EHICDJ:kkind);
                 strICDK = getDiag10(EHICDK:kkind);
                 strICDL = getDiag10(EHICDL:kkind);
               //***** End add *** 09/01/16 **********************************
                 isICD10 = '0';
               endif;
            Else;
               strICDA = *Blanks;
               strICDB = *Blanks;
               strICDC = *Blanks;
               strICDD = *Blanks;
             //***** Beg add *** 09/01/16 **********************************
               strICDE = *Blanks;
               strICDF = *Blanks;
               strICDG = *Blanks;
               strICDH = *Blanks;
               strICDI = *Blanks;
               strICDJ = *Blanks;
               strICDK = *Blanks;
               strICDL = *Blanks;
             //***** End add *** 09/01/16 **********************************
             Endif;

             // Load the Resubmission Code
             //***** Beg add *** 09/01/16 **********************************
             If EHrrcd <> *blanks;
                strReSubCode = EHrrcd;
             EndIf;
             //***** End add *** 09/01/16 **********************************

             // Load the EDI Status with S to indicate this EOB was sent
             // unless it was Withdrawn.
             if eotape <> 'W';
               ehedi  = 'S';
           //** Beg Add ** 01/27/16 ************************************    ***
             Else;
               ehedi  = 'N';
           //*** End Add ** 01/27/16 ************************************   ***
             endif;
             ehuser = q1user;
             ehpgm  = q1pgm;
      /END-FREE
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ehdt
     C                   move      dstime        ehtime
      /FREE
             update peh01;
           endif;                                                             //if not *in99
           //
          // Load Header info for Hospital Bills
           //
         when eobtyp = 'H';
           //
           // Load Form id
           //
      /END-FREE
     C                   move      '90'          formid
      //
      /FREE
           chain hdky peu01;
           *IN99 = NOT %FOUND;
           //
           if not *in99;
             //
             // Load type of bill
             //
             typebill = eublty;
             //
             // Load license number
             //
            If emppayment = *off;
      /END-FREE
     C                   movel(p)  euflic        license#
      /FREE
            Else;
      /END-FREE
     C                   movel     *Blanks       license#
      /FREE
             Endif;
            //
            // Load Operating Physicians License Number
            //
      /END-FREE
     C                   movel(p)  euolic        operlicense
   
      // Load Administration Facility License Number
   
     C                   movel(p)  eufalc        fac_license
   
       // Load Administration Facility License Number
   
     C                   movel(p)  eobnpi        fac_npi#
   
       // Load admission/Discharg Date/hour

     C                   z-add     euaddt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         admissdt

     C                   movel     eublty        @@blty
      /FREE
             if (@@blt1 = '1' or @@blt1 = '2')
                   and (@@blt2 = '1' or @@blt2 = '2'
                   or @@blt2 = '8');
      /END-FREE
     C                   move      euadhr        admisshr
      /FREE
             else;
      /END-FREE
     C                   move      euadhr        admisshr
      /FREE
               if euadhr = 0;
      /END-FREE
     C                   move      *blanks       admisshr
      /FREE
               endif;
             endif;
             //
             if eudchr <> 99;
      /END-FREE
     C                   move      eudchr        dischrghr
      /FREE
             endif;
             //
             // Load Date Statement Covers from/to
             //
      /END-FREE
     C                   z-add     euscpf        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         stmtcovfrmdt
      //
     C                   z-add     euscpt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         stmtcovtomdt
      //
      // Load Diagnosis Codes
      //
      /FREE
            If emppayment = *off;
              if EUICDInd <> 'Y';
                 strICDA = getDiag(eudia1);
                 strICDB = getDiag(eudia2);
                 strICDC = getDiag(eudia3);
                 strICDD = getDiag(eudia4);
                 strICDE = getDiag(eudia5);
                 strICDF = getDiag(eudia6);
                 strICDG = getDiag(eudia7);
                 strICDH = getDiag(eudia8);
                 isICD10 = '9';
              else;
                 kkind = 'D';
                 strICDA = getDiag10(EUICD1001:kkind);
                 strICDB = getDiag10(EUICD1002:kkind);
                 strICDC = getDiag10(EUICD1003:kkind);
                 strICDD = getDiag10(EUICD1004:kkind);
                 strICDE = getDiag10(EUICD1005:kkind);
                 strICDF = getDiag10(EUICD1006:kkind);
                 strICDG = getDiag10(EUICD1007:kkind);
                 strICDH = getDiag10(EUICD1008:kkind);
                 isICD10 = '0';
               endif;
               //
            Else;
               strICDA = *Blanks;
               strICDB = *Blanks;
               strICDC = *Blanks;
               strICDD = *Blanks;
               strICDE = *Blanks;
               strICDF = *Blanks;
               strICDG = *Blanks;
               strICDH = *Blanks;
             Endif;
            //
            // Load External Cause of Injury
            //
            If emppayment = *off;
              if EUICDInd <> 'Y';
                 extinjcd1 = getDiag(eucau1);
                 extinjcd2 = getDiag(eucau2);
                 extinjcd3 = getDiag(eucau3);
              else;
                 kkind = 'D';
                 extinjcd1 = getDiag10(EUCAU1001:kkind);
                 extinjcd2 = getDiag10(EUCAU1002:kkind);
                 extinjcd3 = getDiag10(EUCAU1003:kkind);
              endif;
            Else;
               extinjcd1 = *Blanks;
               extinjcd2 = *Blanks;
               extinjcd3 = *Blanks;
             Endif;

             // Load Procedure Code Info
            If emppayment = *off;
              If EUICDInd = 'Y';
                 if eupcm <> *blanks;
                   procmth = eupcm;
                   if procmth <> '9';
                     princprcd = euprc1001;
                     otherprcd_a = euprc1002;
                     otherprcd_b = euprc1003;
                     otherprcd_c = euprc1004;
                     otherprcd_d = euprc1005;
                     otherprcd_e = euprc1006;
                   else;
                     kkind = 'O';
                     princprcd = getdiag10(euprc1001:kkind);
                     otherprcd_a = getdiag10(euprc1002:kkind);
                     otherprcd_b = getdiag10(euprc1003:kkind);
                     otherprcd_c = getdiag10(euprc1004:kkind);
                     otherprcd_d = getdiag10(euprc1005:kkind);
                     otherprcd_e = getdiag10(euprc1006:kkind);
                   endif;
                 endif;
              Endif;
            Endif;

             // Load Procedure Code Info
             //
            If EUICDInd <> 'Y';
               if euprc1 <> *blanks;
                 procmth = eupcm;
                 if procmth <> '9';
                   princprcd = euprc1;
                 else;
                   princprcd = getdiag(euprc1);
                 endif;
               endif;
             Endif;

            If emppayment = *off;
               princprcd = princprcd;
             Else;
               princprcd = *blanks;
             endif;

            If EUICDInd <> 'Y';
               if euprc2 <> *blanks;
                 if procmth <> '9';
                   otherprcd_a = euprc2;
                   otherprcd_b = euprc3;
                   otherprcd_c = euprc4;
                   otherprcd_d = euprc5;
                   otherprcd_e = euprc6;
                 else;
                   otherprcd_a = getdiag(euprc2);
                   otherprcd_b = getDiag(euprc3);
                   otherprcd_c = getDiag(euprc4);
                   otherprcd_d = getDiag(euprc5);
                   otherprcd_e = getDiag(euprc6);
                 endif;
               endif;
             Endif;

            If emppayment = *off;
               otherprcd_a = otherprcd_a;
               otherprcd_b = otherprcd_b;
               otherprcd_c = otherprcd_c;
               otherprcd_d = otherprcd_d;
               otherprcd_e = otherprcd_e;
             else;
               otherprcd_a = *Blanks;
               otherprcd_b = *Blanks;
               otherprcd_c = *Blanks;
               otherprcd_d = *Blanks;
               otherprcd_e = *Blanks;
             Endif;
            //
             // Load Type of Admission
             //
             typeadm = euadty;
             //
             // Load Payment Plan
             //
            If emppayment = *off;
               if eoepmt <> 'Y';
      /END-FREE
     C                   move      euppln        payplan
      /FREE
               endif;
            Else;
      /END-FREE
     C                   movel     *Blanks       payplan
      /FREE
             Endif;
            //
            // Load Scheduled/Unscheduled Indicator
            If emppayment = *off;
              //
               schdunschdind = euschd;
            Else;
      /END-FREE
     C                   movel     *Blanks       schdunschdind
      /FREE
             Endif;
            //
            // Load Implant Total Paid
            //
             exsr getdwc90imtp;
            //
             // Update the EDI flag to indicate the EOB has been sent to the state
             // unless it was Withdrawn.
             if eotape <> 'W';
               euedi = 'S';
           //** Beg Add ** 01/27/16 ************************************    ***
             Else;
               euedi  = 'N';
           //*** End Add ** 01/27/16 ************************************   ***
             endif;
             euuser = q1user;
             eupgm  = q1pgm;
      /END-FREE
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eudt
     C                   move      dstime        eutime
      //
      /FREE
             update peu01;
           endif;
           //
          // Load Header info for Pharmacy Bills
           //
         when eobtyp = 'X';
           //
           chain hdky pep01;
           *IN99 = NOT %FOUND;
           if not *in99;
             // Load Form id
      /END-FREE
     C                   move      '10'          formid
      //
      // Load Statement Date
      //
     C                   z-add     eocsdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         statemntdt
      /FREE
            If emppayment = *off;
               if eoepmt <> 'Y';
      /END-FREE
     C                   move      epppln        payplan
      /FREE
               endif;
            Else;
      /END-FREE
     C                   movel     *Blanks       payplan
      /FREE
             Endif;
            //
            // Pharmacists license number
            //
            If emppayment = *off;
               if epplic <> *blanks;
                 license# = epplic;
               endif;
            Else;
      /END-FREE
     C                   movel     *Blanks       license#
      /FREE
             Endif;
            //
             // Load Payments
             //
             exsr getdwc10pmts;
             //
             // Update the EDI flag to indicate the EOB has been sent to the state
             //
             if eotape <> 'W';
               epedi = 'S';
          //** Beg Add ** 01/27/16 ***************************************  ***
             Else;
               epedi  = 'N';
          //** End Add ** 01/27/16 **************************************   ***
             endif;
             //
             epuser = q1user;
             eppgm  = q1pgm;
             //
      /END-FREE
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         epdt
     C                   move      dstime        eptime
      //
      /FREE
             update pep01;
             //
           endif;
           //
          // Load Header info for Dental Bills
           //
         when eobtyp = 'D';
           //
           // Load Form id
           //
      /END-FREE
     C                   move      '11'          formid
      //
      /FREE
           chain hdky ped01;
           *IN99 = NOT %FOUND;
           //
           // Load Place of treatment
           //
           if not *in99;
             placetmt = edpotx;
            If emppayment = *off;
      /END-FREE
     C                   movel(P)  edflic        license#
      /FREE
            Else;
      /END-FREE
     C                   movel     *Blanks       license#
      /FREE
             Endif;

       // Load Diagnosis Codes

            If emppayment = *off;
              if EDICDInd <> 'Y';
                 strICDA = getDiag(eddia1);
                 strICDB = getDiag(eddia2);
                 strICDC = getDiag(eddia3);
                 strICDD = getDiag(eddia4);
             //*** Beg Chg *** 09/16/16 *******************************
             //**isICD10 = 'B';
                 isICD102A = 'B ';
             //*** End Chg *** 09/16/16 *******************************
              else;
                 kkind = 'D';
                 strICDA = getDiag10(EDICD1001:kkind);
                 strICDB = getDiag10(EDICD1002:kkind);
                 strICDC = getDiag10(EDICD1003:kkind);
                 strICDD = getDiag10(EDICD1004:kkind);
             //*** Beg Chg *** 09/16/16 *******************************
             //**isICD10 = 'AB';
                 isICD102A = 'AB';
             //*** End Chg *** 09/16/16 *******************************
               endif;
            Else;
               strICDA = *Blanks;
               strICDB = *Blanks;
               strICDC = *Blanks;
               strICDD = *Blanks;
             Endif;

             // Load Payment Plan
             //
            If emppayment = *off;
               if eoepmt <> 'Y';
                 paycode = edppln;
               endif;
            Else;
               payplan = *Blanks;
               paycode = *Blanks;
             Endif;
             //
             // Update the EDI flag to indicate the EOB has been sent to the state
             // unless it was Withdrawn.
             if eotape <> 'W';
               ededi = 'S';
          //** Beg Add ** 01/27/16 *******************************          ***
             Else;
               ededi  = 'N';
          //** End Add ** 01/27/16 *******************************          ***
             endif;
             eduser = q1user;
             edpgm  = q1pgm;
      /END-FREE
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eddt
     C                   move      dstime        edtime
      //
      /FREE
             update ped01;
           endif;
         endsl;
         //
         // Do not send payment plan if this is an employee payment
         //
         if xxemplppind = 'E';
           payplan = ' ';
         endif;
         //
       endsr;
       //=====================================================================
       // SUBROUTINE getdwc10pmts - Loads header info for pharmacy bills
       //=====================================================================
       begsr getdwc10pmts;
         //
         // Initialize
         //
      /END-FREE
     C                   move      *zeros        totpay
     C                   move      *zeros        totpaysupp
      //
      /FREE
         chain caky pca01;
         *IN99 = NOT %FOUND;
         //
         dow not *in99;
           //
           kkdseq = cadseq;
           //
           chain dtky pdp01;
           *IN99 = NOT %FOUND;
           //
           // Accumulate capaid
           //
           if dprxpt = 'S'  and *in99 = '0';
             totpaysupp = totpaysupp + capaid;
           else;
             totpay     = totpay     + capaid;
           endif;
           //
           reade caky pca01;
           *IN99 = %EOF;
           //
         enddo;
         //
       endsr;
       //=====================================================================
       // SUBROUTINE getdwc90imtp - Loads header info for pharmacy bills
       //=====================================================================
       begsr getdwc90imtp;
         //
         // Initialize
         //
      /END-FREE
     C                   move      *zeros        imptotpay
      //
      /FREE
         setll caky pdu01;
         *IN99 = %EQUAL;
         reade caky pdu01;
         *IN99 = %EOF;
         //
         dow not *in99;
           //
           if durvcd = 278;
             //
             kkdseq = dudseq;
             //
             chain dtky pca01;
             *IN99 = NOT %FOUND;
             //
             // Accumulate capaid
             //
             imptotpay = imptotpay + capaid;
             //
           endif;
           //
           reade caky pdu01;
           *IN99 = %EOF;
           //
         enddo;
         //
        If emppayment = *off;
           //
           xxtotpay  = imptotpay * 100;
         Else;
           xxtotpay   = *zeros;
           //
        Endif;
         //
       endsr;
       //=====================================================================
       // SUBROUTINE loddtl09 - Load Physician Detail
       //=====================================================================
       begsr loddtl09;
         //
         // Initialize
         //
      /END-FREE
     C                   move      *zeros        detseq#
     C                   move      *zeros        placesrv
     C                   move      *blanks       icd9ref
     C                   move      *zeros        billedproc
     C                   move      *blanks       billedmod
     C                   move      *blanks       billedmod1
     C                   move      *blanks       billedmod2
     C                   move      *blanks       billedmod3
     C                   move      *blanks       billedmod4
     C                   move      *blanks       paidmod1
     C                   move      *blanks       paidmod2
     C                   move      *blanks       paidmod3
     C                   move      *blanks       paidmod4
     C                   move      *blanks       @XXNDC
     C***** Beg Add *** 09/01/16 *******************************************
     C                   move      *blanks       xxndc_ds
     C***** End Add *** 09/01/16 *******************************************
     C                   move      *zeros        paidproc
     C                   move      *blanks       paidmod
     C                   move      *zeros        detchrg
     C                   move      *zeros        units
     C                   move      *zeros        detpaid
     C                   move      *zeros        dateofsrvfrm
     C                   move      *zeros        dateofsrvto
     C                   move      *blanks       eobcode1
     C                   move      *blanks       eobcode2
     C                   move      *blanks       eobcode3
     C                   move      *blanks       strNDC2nd
      //
      /FREE
         kkdseq = @@dseq;
         //
         chain dtky pdh01;
         *IN99 = NOT %FOUND;
         //
         // Load from the database
         //
      /END-FREE
     C                   move      '2'           recflg
     C                   move      camseq        detseq#
     C                   move      dhpos         placesrv
      //
      // Load the diagnosis cross references
      //
      /FREE
         exsr loddiagxref;
         //
         // Load the procedure codes and modifiers
         //
      /END-FREE
     C                   move      dhbprc        billedproc
      //
      /FREE
         if dhmod1 <> *blanks;
      /END-FREE
     C                   move      dhmod1        billedmod
      /FREE
         else;
           if dhmod2 <> *blanks;
      /END-FREE
     C                   move      dhmod2        billedmod
      /FREE
           else;
             if dhmod3 <> *blanks;
      /END-FREE
     C                   move      dhmod3        billedmod
      /FREE
             else;
               if dhmod4 <> *blanks;
      /END-FREE
     C                   move      dhmod4        billedmod
      /FREE
               endif;
             endif;
           endif;
         endif;
         //
         if dhmod1 <> *blanks;
      /END-FREE
     C                   move      dhmod1        billedmod1
     C                   move      dhmod1        paidmod1
      /FREE
         endif;
         //
         if dhmod2 <> *blanks;
      /END-FREE
     C                   move      dhmod2        billedmod2
     C                   move      dhmod2        paidmod2
      /FREE
         endif;
         //
         if dhmod3 <> *blanks;
      /END-FREE
     C                   move      dhmod3        billedmod3
     C                   move      dhmod3        paidmod3
      /FREE
         endif;
         //
         if dhmod4 <> *blanks;
      /END-FREE
     C                   move      dhmod4        billedmod4
     C                   move      dhmod4        paidmod4
      /FREE
         endif;
         //
         //
      /END-FREE
     C                   move      dhprcd        paidproc
     C                   move      billedmod     paidmod
     C                   move      @@chrg        detchrg
      /FREE
         units = @@units;
      /END-FREE
      /free
       If units = 0;
         units = dhunts;
       EndIf;
      /end-free
     C                   move      @@paid        detpaid
      //
     // Load Service Dates
     //
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofsrvfrm
     //
     C                   z-add     catdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofsrvto
     //
      /FREE
         xxndc   = dhndc;
        //
         if xxndc  <> *blanks;
          //
           xxndcwk = %trim(dhndc);
           d1 = 0;
           d2 = 0;
      /END-FREE
     C     digibk        check     xxndcwk       d1                             only digits or blank
      //
      /FREE
           if d1 = 0;
      /END-FREE
     C     digits        check     xxndcwk       d1                             only digits
      //
      /FREE
             if d1 > 0;
      /END-FREE
     C     ' '           check     xxndcwk:d1    d2                             no blanks in middle
      //
      /FREE
               if d2 = 0;
                 xxndc   = *zeros;
                 d2 = (%len(xxndcwk) + 1) - (d1 - 1);                         //start position
                 //
                 if d2 > 0 or d2 <= %len(xxndcwk);
                   %subst(xxndc:d2) = %trim(xxndcwk);                         //right just/zero fill
                 else;
                   xxndc = xxndcwk;                                           //just move
                 endif;                                                       //d2 > 0 or d2 <= %len
                 //
               endif;                                                         //d2 = 0
             endif;                                                           //d1 > 0
           endif;                                                             //d1 = 0

       //**** Beg Chg *** 11/30/16 *******************************************
           ndc5 = %subst(xxndc:1:5);
           ndc4 = %subst(xxndc:6:4);
           ndc2 = %subst(xxndc:10:2);

           //*** ndc5 = %subst(xxndc:1:5);
           //*** hyphen1 = '-';
           //*** ndc4 = %subst(xxndc:6:4);
           //*** hyphen2 = '-';
           //*** ndc2 = %subst(xxndc:10:2);
       //**** End Chg *** 11/30/16 *******************************************

         endif;                                                               //dhndc <> ' '
        //
       //**** Beg Add *** 09/01/16 *******************************************
        // Load NDC Code 2

         xxndc   = dhndc2;
        //
         if xxndc  <> *blanks;
          //
           xxndcwk = %trim(dhndc2);
           d1 = 0;
           d2 = 0;
      /END-FREE
     C     digibk        check     xxndcwk       d1                             only digits or blank
      //
      /FREE
           if d1 = 0;
      /END-FREE
     C     digits        check     xxndcwk       d1                             only digits
      //
      /FREE
             if d1 > 0;
      /END-FREE
     C     ' '           check     xxndcwk:d1    d2                             no blanks in middle
      //
      /FREE
               if d2 = 0;
                 xxndc   = *zeros;
                 d2 = (%len(xxndcwk) + 1) - (d1 - 1);                         //start position
                 //
                 if d2 > 0 or d2 <= %len(xxndcwk);
                   %subst(xxndc:d2) = %trim(xxndcwk);                         //right just/zero fill
                 else;
                   xxndc = xxndcwk;                                           //just move
                 endif;                                                       //d2 > 0 or d2 <= %len
                 //
               endif;                                                         //d2 = 0
             endif;                                                           //d1 > 0
           endif;                                                             //d1 = 0

       //**** Beg Chg *** 11/30/16 *******************************************
           ndc_5 = %subst(xxndc:1:5);
           ndc_4 = %subst(xxndc:6:4);
           ndc_2 = %subst(xxndc:10:2);

           //*** ndc_5 = %subst(xxndc:1:5);
           //*** hyp1 = '-';
           //*** ndc_4 = %subst(xxndc:6:4);
           //*** hyp2 = '-';
           //*** ndc_2 = %subst(xxndc:10:2);
       //**** End Chg *** 11/30/16 *******************************************

         endif;                                                               //dhndc <> ' '
         //
         strNDC2nd = xxndc_ds;
         //
       //**** End Add *** 09/01/16 *******************************************
        //
        // Load the EOB codes
        //
         exsr lodeobcodes;
        //
       endsr;
       //=====================================================================
       // SUBROUTINE loddtl10 - Load Pharmacy Detail
       //=====================================================================
       begsr loddtl10;
         //
         // Initialize
         //
      /END-FREE
     C                   move      *zeros        detseq#
     C                   z-add     *zeros        qtyofmed
     C*** Beg Chg *** 03/17/17 ************************************
     C*****************  z-add     *zeros        dayssupply
     C                   movel     *blanks       dayssupply
     C*** End Chg *** 03/17/17 ************************************
     C                   move      *blanks       xxndc
     C                   move      *blanks       @xxndc
     C***** Beg Add *** 09/01/16 *******************************************
     C                   move      *blanks       xxndc_ds
     C                   move      *blanks       strNDC2nd
     C***** End Add *** 09/01/16 *******************************************
     C                   move      *blanks       pharmacy#
     C                   move      *blank        neworrefill
     C                   move      *blanks       datefilled
     C                   move      *blank        certifictn
     C                   move      *blanks       prescbrlic
     C                   move      *zeros        detchrg
     C                   move      *blanks       eobcode1
     C                   move      *blanks       eobcode2
     C                   move      *blanks       eobcode3
     C                   move      *zeros        detpaid
     C                   move      'N'           skpdtl10
      //
      /FREE
         kkdseq = cadseq;
         //
         chain dtky pdp01;
         *IN99 = NOT %FOUND;
         //
         // Load from the database
         //
      /END-FREE
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
     //
     // Load Drugs/Equipment/Supplies Indicator
     //
     C                   move      dprxpt        drugsuppind
      //
     // Load Quantity of Medication
     //
     C                   z-add     dpunts        qtyofmed
      //
     // Load Number of Days Supply
     //
      //*** Beg Chg *** 03/17/17 ********************************
     C***************    z-add     dprxdy        dayssupply
     C                   move      dprxdy        dayssupply
      //*** End Chg *** 03/17/17 ********************************
      //*** Beg Add *** 03/17/17 ********************************
      /free
         if dprxpt = 'S';
            dayssupply = *blanks;
         endif;
      /end-free
      //*** End Add *** 03/17/17 ********************************
      //
     // Load NDC code
     //
      /FREE
         xxndc   = dpndc;
         if xxndc <> *Blanks;
           xxndcwk = %trim(dpndc);
           d1 = 0;
           d2 = 0;
      /END-FREE
     C     digibk        check     xxndcwk       d1                             only digits or blank
      //
      /FREE
           if d1 = 0;
      /END-FREE
     C     digits        check     xxndcwk       d1                             only digits
      //
      /FREE
             if d1 > 0;
      /END-FREE
     C     ' '           check     xxndcwk:d1    d2                             no blanks in middle
      //
      /FREE
               if d2 = 0;
                 xxndc   = *zeros;
                 d2 = (%len(xxndcwk) + 1) - (d1 - 1);                         //start position
                 //
                 if d2 > 0 or d2 <= %len(xxndcwk);
                   %subst(xxndc:d2) = %trim(xxndcwk);                         //right just/zero fill
                 else;
                   xxndc = xxndcwk;                                           //just move
                 endif;                                                       //d2 > 0 or d2 <= %len
                 //
               endif;                                                         //d2 = 0
             endif;                                                           //d1 > 0
           endif;                                                             //d1 = 0
           //
       //**** Beg Chg *** 11/30/16 *******************************************
           ndc5 = %subst(xxndc:1:5);
           ndc4 = %subst(xxndc:6:4);
           ndc2 = %subst(xxndc:10:2);

           //*** ndc5 = %subst(xxndc:1:5);
           //*** hyphen1 = '-';
           //*** ndc4 = %subst(xxndc:6:4);
           //*** hyphen2 = '-';
           //*** ndc2 = %subst(xxndc:10:2);
       //**** End Chg *** 11/30/16 *******************************************
           //
         endif;                                                               //xxndc <> *Blanks
         //
       //****** Beg Add *** 09/01/16 *****************************************************
       //
      // Load NDC code 2
      //
         xxndc = dpndc2;
         if xxndc <> *Blanks;
           xxndcwk = %trim(dpndc2);
           d1 = 0;
           d2 = 0;
      /END-FREE
     C     digibk        check     xxndcwk       d1                             only digits or blank

      /FREE
           if d1 = 0;
      /END-FREE
     C     digits        check     xxndcwk       d1                             only digits

      /FREE
             if d1 > 0;
      /END-FREE
     C     ' '           check     xxndcwk:d1    d2                             no blanks in middle

      /FREE
               if d2 = 0;
                 xxndc   = *zeros;
                 d2 = (%len(xxndcwk) + 1) - (d1 - 1);                         //start position
                 //
                 if d2 > 0 or d2 <= %len(xxndcwk);
                   %subst(xxndc:d2) = %trim(xxndcwk);                         //right just/zero fill
                 else;
                   xxndc = xxndcwk;                                           //just move
                 endif;                                                       //d2 > 0 or d2 <= %len
                 //
               endif;                                                         //d2 = 0
             endif;                                                           //d1 > 0
           endif;                                                             //d1 = 0
           //

       //**** Beg Chg *** 11/30/16 *******************************************
           ndc_5 = %subst(xxndc:1:5);
           ndc_4 = %subst(xxndc:6:4);
           ndc_2 = %subst(xxndc:10:2);

           //*** ndc_5 = %subst(xxndc:1:5);
           //*** hyp1 = '-';
           //*** ndc_4 = %subst(xxndc:6:4);
           //*** hyp2 = '-';
           //*** ndc_2 = %subst(xxndc:10:2);
       //**** End Chg *** 11/30/16 *******************************************
           //
         endif;                                                               //xxndc <> *Blanks
         //
         strNDC2nd = xxndc_ds;
         //
       // ****** End Add *** 09/01/16 *****************************************************
        // Load Pharmacy number
        //
      /END-FREE
     C                   movel     dprx#         pharmacy#
      //
     // Load New or Refill
     //
     C                   movel     dpnwrf        neworrefill
     //
     // Load Purchase Rental Indicator
     //
     C                   move      dpprin        purchrent
     //
     // Load Date Filled
     //
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         datefilled
     //
     // Load Dispensed as Written
     //
     C                   move      dpcert        dispaswrit
      //
     // Load Certification
     //
     C                   movel     dpcert        certifictn
      //
     // Load Prescribers license
     //
      /FREE
         prescbrlic = %trim(dplic#);
         //
        // Load Charges
        //
      /END-FREE
     C                   move      cachrg        detchrg
      //
     // Load Amount paid
     //
     C                   move      capaid        detpaid
      //
     // Load the EOB codes
     //
      /FREE
         exsr lodeobcodes;
        //
        // Load HCPCS Code(HCPCS)
        //
      /END-FREE
     C                   move      dphcpc        hcpcs
     //
      /FREE
       endsr;
       //=====================================================================
       // SUBROUTINE loddtl11 - Load Dental Detail
       //=====================================================================
       begsr loddtl11;
         //
         // Initialize
         //
      /END-FREE
     C                   move      *zeros        detseq#
     C                   move      *zeros        billedproc
     C                   move      *zeros        paidproc
     C                   move      *zeros        detchrg
     C                   move      *zeros        detpaid
     C                   move      *zeros        dateofsrvfrm
     C                   move      *blanks       eobcode1
     C                   move      *blanks       eobcode2
     C                   move      *blanks       eobcode3
      //
      /FREE
         kkdseq = cadseq;
         //
         chain dtky pdd01;
         *IN99 = NOT %FOUND;
         //
         // Load from the database
         //
      /END-FREE
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
      //
      // Load the procedure codes
      //
     C                   move      ddbprc        billedproc
      //
     C                   move      ddprcd        paidproc
     C                   move      cachrg        detchrg
     C                   move      capaid        detpaid
      //
     // Load Service Dates
     //
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofsrvfrm
     //
     // Load the EOB codes
     //
      /FREE
         exsr lodeobcodes;
       endsr;
       //=====================================================================
       // SUBROUTINE loddtl90 - Load Hospital Detail
       //=====================================================================
       begsr loddtl90;
         //
         // Initialize
         //
      /END-FREE
     C                   move      *zeros        detseq#
     C                   move      *zeros        revcode
     C                   move      *zeros        billedproc
     C***** Beg Chg *** 04/22/17 ****************************
     C*****              move      *zeros        dateofopsrv
     C                   move      *blanks       dateofopsrv
     C***** End Chg *** 04/22/17 ****************************
     C                   move      *blanks       billedmod
     C                   move      *blanks       billedmod2
     C                   move      *blanks       billedmod3
     C                   move      *blanks       billedmod4
     C                   move      *zeros        paidproc
     C                   move      *blanks       paidmod1
     C                   move      *blanks       paidmod2
     C                   move      *blanks       paidmod3
     C                   move      *blanks       paidmod4
     C                   move      *zeros        detchrg
     C                   move      *zeros        units
     C                   move      *blanks       eobcode1
     C                   move      *blanks       eobcode2
     C                   move      *blanks       eobcode3
     C                   move      *zeros        inspayment
     C                   move      *zeros        detpaid
      //
      /FREE
         kkdseq = cadseq;
         //
         chain dtky pdu01;
         *IN99 = NOT %FOUND;
         //
         // Load from the database
         //
      /END-FREE
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
      //
      // Load the procedure/rev codes and modifiers
      //
     C                   move      dubprc        revcode
     C                   move      dubrat        billedproc
     C                   move      dupmod        billedmod
     C                   move      durate        paidproc
     C                   move      dupmod        paidmod1
     C                   move      dumod2        billedmod2
     C                   move      dumod3        billedmod3
     C                   move      dumod4        billedmod4
     C                   move      dumod2        paidmod2
     C                   move      dumod3        paidmod3
     C                   move      dumod4        paidmod4
      //
      // Load the amounts
      //
     C                   move      cachrg        detchrg
     C                   move      capaid        detpaid
     C                   move      detpaid       inspayment
     C                   z-add     duunts        units90
     //
     // Load the EOB codes
     //
      /FREE
         exsr lodeobcodes;
        //
        // Load the date of service for outpatient bills
        //
         if %subst(eublty:1:2) = '13';
      /END-FREE
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofopsrv
      /FREE
         endif;
        //
       endsr;
      //
      //***************************************************************
      // LODDIAGXREF - Subroutine to load diagnosis cross references
      //***************************************************************
      //
       begsr lodDiagxref;
         //
         // Load the 1st cross reference in the leftmost blank position
         //
         if dhdxr1 = 'X';
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C*************      move      '1'           icd9ref1
     C                   move      'A'           icd9ref1
     C*** End Chg *** 09/16/16 **************************************
      /FREE
         endif;
         //
         // Load the 2nd cross reference in the leftmost blank position
         //
         if dhdxr2 = 'X';
           //
           if icd9ref1 <> ' ';
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C***************    move      '2'           icd9ref2
     C                   move      'B'           icd9ref2
     C*** End Chg *** 09/16/16 **************************************
      /FREE
           else;
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C***************    move      '2'           icd9ref1
     C                   move      'B'           icd9ref1
     C*** End Chg *** 09/16/16 **************************************
      /FREE
           endif;
           //
         endif;
         //
         // Load the 3th cross reference in the leftmost blank position
         //
         if dhdxr3 = 'X';
           //
           if icd9ref1 <> ' ';
             //
             if icd9ref2 <> ' ';
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C****************   move      '3'           icd9ref3
     C                   move      'C'           icd9ref3
     C*** End Chg *** 09/16/16 **************************************
      /FREE
             else;
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C****************   move      '3'           icd9ref2
     C                   move      'C'           icd9ref2
     C*** End Chg *** 09/16/16 **************************************
      /FREE
             endif;
             //
           else;
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C****************   move      '3'           icd9ref1
     C                   move      'C'           icd9ref1
     C*** End Chg *** 09/16/16 **************************************
      /FREE
           endif;
           //
         endif;
         //
         // Load the 4th cross reference in the leftmost blank position
         //
         if dhdxr4 = 'X';
           //
           if icd9ref1 <> ' ';
             //
             if icd9ref2 <> ' ';
               //
               if icd9ref3 <> ' ';
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C***************    move      '4'           icd9ref4
     C                   move      'D'           icd9ref4
     C*** End Chg *** 09/16/16 **************************************
      /FREE
               else;
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C****************   move      '4'           icd9ref3
     C                   move      'D'           icd9ref3
     C*** End Chg *** 09/16/16 **************************************
      /FREE
               endif;
               //
             else;
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C****************   move      '4'           icd9ref2
     C                   move      'D'           icd9ref2
     C*** End Chg *** 09/16/16 **************************************
      /FREE
             endif;
             //
           else;
      /END-FREE
     C*** Beg Chg *** 09/16/16 **************************************
     C***************    move      '4'           icd9ref1
     C                   move      'D'           icd9ref1
     C*** End Chg *** 09/16/16 **************************************
      /FREE
           endif;
           //
         endif;
         //
         // ***** Beg add *** 09/01/16 *************************
        // Load 5th cross reference in the leftmost blank position
         If dhdxrE = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';
                      // nothing needs to be done, max 4 codes
                     Else;
                        icd9ref4 = 'E';
                     EndIf;
                  Else;
                     icd9ref3 = 'E' ;
                  Endif;
               Else;
                  icd9ref2 ='E';
               Endif;
            Else;
               icd9ref1 ='E';
            Endif;
         Endif;

        // Load 6th cross reference in the leftmost blank position
         If dhdxrF = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'F';
                     EndIf;
                  Else;
                     icd9ref3 = 'F' ;
                  Endif;
               Else;
                  icd9ref2 ='F';
               Endif;
            Else;
               icd9ref1 ='F';
            Endif;
         Endif;

        // Load 7th cross reference in the leftmost blank position
         If dhdxrG = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'G';
                     EndIf;
                  Else;
                     icd9ref3 = 'G';
                  Endif;
               Else;
                  icd9ref2 = 'G';
               Endif;
            Else;
               icd9ref1 = 'G';
            Endif;
         Endif;

        // Load 8th cross reference in the leftmost blank position
         If dhdxrH = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'H';
                     EndIf;
                  Else;
                     icd9ref3 = 'H';
                  Endif;
               Else;
                  icd9ref2 = 'H';
               Endif;
            Else;
               icd9ref1 = 'H';
            Endif;
         Endif;

        // Load 9th cross reference in the leftmost blank position
         If dhdxrI = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'I';
                     EndIf;
                  Else;
                     icd9ref3 = 'I';
                  Endif;
               Else;
                  icd9ref2 = 'I';
               Endif;
            Else;
               icd9ref1 = 'I';
            Endif;
         Endif;

        // Load 10th cross reference in the leftmost blank position
         If dhdxrJ = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'J';
                     EndIf;
                  Else;
                     icd9ref3 = 'J';
                  Endif;
               Else;
                  icd9ref2 = 'J';
               Endif;
            Else;
               icd9ref1 = 'J';
            Endif;
         Endif;

        // Load 11th cross reference in the leftmost blank position
         If dhdxrK = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'K';
                     EndIf;
                  Else;
                     icd9ref3 = 'K';
                  Endif;
               Else;
                  icd9ref2 = 'K';
               Endif;
            Else;
               icd9ref1 = 'K';
            Endif;
         Endif;

        // Load 12th cross reference in the leftmost blank position
         If dhdxrL = 'X';
            If icd9ref1 <> ' ';
               If icd9ref2 <> ' ';
                  If icd9ref3 <> ' ';
                     If icd9ref4 <> ' ';

                     Else;
                        icd9ref4 = 'L';
                     EndIf;
                  Else;
                     icd9ref3 = 'L';
                  Endif;
               Else;
                  icd9ref2 = 'L';
               Endif;
            Else;
               icd9ref1 = 'L';
            Endif;
         Endif;
         // ***** End add *** 09/01/16 *************************
       endsr;
       //
      //***************************************************************
      // LODEOBCODES - Subroutine to load EOB codes
      //***************************************************************
      //
       begsr lodEOBcodes;
         //
         clear ecnt;
         //
         chain dtky pde01;
         *IN99 = NOT %FOUND;
         //
         dow not *in99 and ecnt <= 4;
           //
           // Select an EOBR code or note. For notes report S20 as the Code.
           //
           //*** Beg Chg *** 07/18/23 ****************************
           //*** if deeob <> *blanks and deprnt <> 'N'
           if deeobcd <> *blanks and deprnt <> 'N'
           //*** End Chg *** 07/18/23 ****************************
                 and destat <> 'I';
            //
            //
      /END-FREE
      /free

       //*** Beg Chg *** 07/18/23 ****************************
       //*** if deeob < '500' or deeob > '599';
       if deeobcd < '500' or deeobcd > '599';
       //*** End Chg *** 07/18/23 ****************************

      /end-free
     //
     //
      //
      /FREE
             //*** Beg Chg *** 07/18/23 ****************************
             //*** if deeob <> *blanks;
             //***  kkkey = deeob;
             if deeobcd <> *blanks;
               kkkey = deeobcd;
             //*** End Chg *** 07/18/23 ****************************
             else;
             endif;
             //
             // Use the state cross reference. If blank then use 20=Unique code
             //
             if eoast = 'FL';
               chain ebky peb01;
               *IN99 = NOT %FOUND;
             endif;
             //
             xxeob=*blanks;
             if *in99 = *off and eoast = 'FL';
               if ebxref <> *blanks;
      /END-FREE
     C                   move      ebxref        xxeob
      /FREE
               else;
                 xxeob=%subst(ebkey:2:2);
               endif;
             endif;
             //
             if eobcode1 = *blanks;
      /END-FREE
     C                   move      xxeob         eobcode1
      /FREE
             else;
               //
               if eobcode2 = *blanks;
      /END-FREE
     C                   move      xxeob         eobcode2
      /FREE
               else;
                 //
                 if eobcode3 = *blanks;
      /END-FREE
     C                   move      xxeob         eobcode3
      /FREE
                 else;
                   //
                 endif;
                 //
               endif;
               //
             endif;
             //
      /END-FREE
     C                   add       1             ecnt
      //
      /FREE
           endif;
          //
      /END-FREE
      /free

       endif;

      /end-free
     //
      //
      /FREE
           reade dtky pde01;
           *IN99 = %EOF;
         enddo;
         //
         // Report code 07 when charges = payments
         //
         if eobcode1 = *blanks and
               eobcode2 = *blanks and
               eobcode3 = *blanks;
           select;
             //
           when eobtyp = 'N';
             if eospcd = 'ASC' or
                   %subst(ehflic:1:3) = 'ASC';
               eobcode1 = '91';
             else;
               eobcode1 = '90';
             endif;
             //
           when eobtyp = 'H';
             eobcode1 = '92';
             //
           when eobtyp = 'D' or eobtyp = 'X';
             eobcode1 = '90';
             //
           endsl;
           //
         endif;
         //
       endsr;
      //***************************************************************
      // LODTHDR - Subroutine to load transmission header record fields
      //***************************************************************
      //
       begsr lodthdr;
        //
      /END-FREE
     C                   move      *blanks       tranhdrmkr
     C                   move      '000'         submid
     C                   move      *blanks       submzip
     C                   move      *zeros        submtid
     C                   move      *blanks       formid
     C                   move      *blanks       revind
     C                   move      *zeros        submdt
     //
     C                   move      'HD1'         tranhdrmkr
     //
      /FREE
         chain c1ky pc101;
         *IN99 = NOT %FOUND;
        //
         if not *in99;
      /END-FREE
     C                   move      c1subm        submid
     C                   move      c1zip         submzip
      /FREE
           if %subst(submzip:6:4) = '0000';
      /END-FREE
     C                   movel     c1zip         xxzip             5
     C                   movel(P)  xxzip         submzip
      /FREE
           endif;
      /END-FREE
     C                   move      c1fein        submtid
     C                   eval      revind = 'F'
     C                   move      qqtpin        tpin
      /FREE
         endif;
         //
        // Load form id
         //
         select;
           //
         when eobtyp = 'N';
      /END-FREE
     C                   move      '09'          formid
      //
      /FREE
         when eobtyp = 'X';
      /END-FREE
     C                   move      '10'          formid
      //
      /FREE
         when eobtyp = 'D';
      /END-FREE
     C                   move      '11'          formid
      //
      /FREE
         when eobtyp = 'H';
      /END-FREE
     C                   move      '90'          formid
      //
      /FREE
         endsl;
         //
        // Load submitted date
        //
      /END-FREE
     C                   time                    dsdttm
     C                   movel     dscy          submdt
     C                   move      dsmd          submdt
     //
      /FREE
       endsr;
      //***************************************************************
      // LODTTLR - Subroutine to load transmission trailer record fields
      //***************************************************************
      //
       begsr lodttlr;
        //
      /END-FREE
     C                   move      'TR1'         trantlrmkr
     //
      /FREE
         if numclm09 > 0;
           except tranTrl09;
         endif;
        //
         if numclm10 > 0;
           except tranTrl10;
         endif;
        //
         if numclm11 > 0;
           except tranTrl11;
         endif;
        //
         if numclm90 > 0;
           except tranTrl90;
         endif;
        //
       endsr;
       //=====================================================================
       // SUBROUTINE MEDSR - FORM DWC-9
       //=====================================================================
       begsr medsr;
         //
         // First time through write the transmission header info initialize
         // claim counter for DWC9 claims
         //
         if first09 = 'Y';
           numclm09 = 0;
           exsr lodthdr;
           except tranHdr09;
           first09 = *blank;
         endif;
         //
         // Sum up the number of claims in the transmission for the trailer
         //
      /END-FREE
     C                   add       1             numclm09
      //
      // Load header info for the current EOB
      //
      /FREE
         exsr lodbhdr;
         //
         except billHdr09;
         //
         chain caky pca28;
         *IN99 = NOT %FOUND;
         //
         dow not *in99;
           kkmseq = camseq;
      /END-FREE
     C                   Call      'FA751R'
     C                   Parm                    kkco#
     C                   Parm                    kksub#
     C                   Parm                    kkfnd
     C                   Parm                    kkfyr
     C                   Parm                    kkcase
     C                   Parm                    kkeob#
     C                   Parm                    kkmseq
     C                   Parm      *ZEROS        @@chrg           11 2
     C                   Parm      *ZEROS        @@paid           11 2
     C                   Parm      *ZEROS        @@DSEQ
     C                   Parm      *ZEROS        @@units
      //
      /FREE
          If emppayment = *off;
             exsr loddtl09;
             except billDtl09;
          Endif;
           //
           setgt caky1 pca28;
           reade caky pca28;
           *IN99 = %EOF;
           //
         enddo;
         RESET @units;
         //
       endsr;
      //
       //=====================================================================
      // SUBROUTINE UPDDBR
       //=====================================================================
       begsr upddbr;
        //
        //  Update control number counter file
        //
      /END-FREE
     C                   movel     yy            juldat
     C                   move      jjj           juldat
     //
      /FREE
         chain tskey pts01;
         *IN99 = NOT %FOUND;
        //
         if *in99 = *off;
           tsseq# = cntr;
      /END-FREE
     C                   move      juldat        tsrndt
      /FREE
           update pts01;
          //
         else;
           tsco#  = eoco#;
           tsseq# = cntr;
      /END-FREE
     C                   move      juldat        tsrndt
      /FREE
           write pts01;
          //
         endif;
        //
       endsr;
      //
       //=====================================================================
      // SUBROUTINE PGMEND
       //=====================================================================
       begsr pgmend;
         //
         // Write the trailer record(s)
         //
         exsr lodttlr;
         //
         if cntr > 0;
           exsr upddbr;
         endif;
         //
         *inlr = *on;
         return;
         //
       endsr;
       //
       //***************************************************************
       // GETMDT - Subroutine to retrieve the check mailing date       *
       //***************************************************************
       //
       BEGSR GETMDT;
         //
         // USE THE CHECK MAILING DATE IF NOT BLANK
         //
         IF @@WCIF = 'Y'
         AND @@WCIN = 'Y'
         AND @@NPWC <> 'Y';
           //
      /END-FREE
     C                   MOVEL     KKSUB#        ##SUB#            5
     C                   MOVEL     KKFND         #1FND             3
      //
      // DETEMINE IF STANDALONE ACCOUNT
      //
     C                   MOVE      KKCO#         #1CO#             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
      //
      /FREE
           IF ##OUTS = 'N';
             //
             // GET WCMS FUND NUMBER
             //
      /END-FREE
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
      //
     C                   Z-ADD     EOWCCK        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXWCCK            8 0
      //
     C                   MOVE      XXWCCK        #1TRDT
      //
     C                   Z-ADD     EOWCCK        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXWCCK
     C
      /FREE
             IF SRERR = '1';
             ELSE;
      /END-FREE
     C                   CALL      'WA050R'
     C                   PARM                    #1CO#
     C                   PARM      ##WCF#        #1FND
     C                   PARM                    #1TRDT            8
     C                   PARM      *BLANKS       #1CMDT            8
      //
      //
      /FREE
             ENDIF;                                                           //SRERR
           ENDIF;                                                             //##OUTS=N
         ENDIF;                                                               //@@WCIF
       ENDSR;
      //
       //=====================================================================
      // SUBROUTINE PGMSTR
       //=====================================================================
       begsr pgmstr;
        //
        // Set-up and Initialize entry fields, convert dates, etc.
        //
      /END-FREE
     C                   move      qqco#         kkco#
     C                   move      qqsub#        kksub#
     //
     C                   move      qqfrdt        xqfrdt
     C                   move      qqtodt        xqtodt
     //
      /FREE
         srdti = xqfrdt;
      /END-FREE
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxfrdt
     //
      /FREE
         srdti = xqtodt;
      /END-FREE
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtodt
     //
      /FREE
         srdti = xxfrdt;
      /END-FREE
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxfrom
     //
      /FREE
         srdti = xxtodt;
      /END-FREE
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxto
     //
      /FREE
         cntr = 0;
        //
      /END-FREE
     C                   time                    dsdttm
     //
      /FREE
         first09 = 'Y';
         first10 = 'Y';
         first11 = 'Y';
         first90 = 'Y';
        //
         select;
          //
         when qqproc = 'P';
      /END-FREE
     C                   movel     'PHYSI'       xxproc
     C                   move      'CIAN'        xxproc
      /FREE
           *in90 = *on;
          //
         when qqproc = 'H';
      /END-FREE
     C                   movel     'HOSPI'       xxproc
     C                   move      'TAL '        xxproc
      /FREE
           *in91 = *on;
          //
         when qqproc = 'D';
      /END-FREE
     C                   movel     'DENTA'       xxproc
     C                   move      'L   '        xxproc
      /FREE
           *in92 = *on;
          //
         endsl;
        //
        // Get the current date
        //
      /END-FREE
     C                   move      *date         sys_date
     //
     // Get the Julian Date
     //
      /FREE
         xxdt = dsdate;
         zzmm = 12;
         zzdd = 31;
         if xxyy <> 0;
           zzyy = xxyy - 1;
         else;
           zzyy = 99;
         endif;
        //
        // Call sub-program SRDDIF8 to Calc Julian Date
        //
      /END-FREE
     C                   move      zzdt          srfrdc
     C                   move      xxdt          srtodc
     //
      /FREE
         srdays = 0;
      /END-FREE
     C                   move      ' '           srerr
     C                   move      ' '           srterm
     C                   call      'SRDDIF8'
     C                   parm                    srfrdc
     C                   parm                    srtodc
     C                   parm                    srdays
     C                   parm                    srerr
     C                   parm                    srterm
     //
     //
     //  GET NEXT AVAILABLE SEQUENCE # TO BE USED IN CONTROL RECORD
     //
      /FREE
         chain tskey pts01;
         *IN80 = NOT %FOUND;
         if *in80 = *on;
           cntr = 0;
         else;
      /END-FREE
     C                   move      tsseq#        cntr
      /FREE
         endif;
        //
        //  GET COMPANY NAME
        //
         chain cokey pco01;
         *IN60 = NOT %FOUND;
         if *in60 = *on;
           clear xxconm;
         else;
      /END-FREE
     C                   movel     coname        xxconm
      /FREE
         endif;
        //
        //  GET COMPANY AUXILIARY RECORD FOR AUDIT NUMBERS
        //  IF NON EXIST - PRINT ERROR MESSAGE AND END PROGRAM
        //
         chain c1ky pc101;
         *IN60 = NOT %FOUND;
         if *in60 = *on;
           except errms1;
           exsr pgmend;
         endif;
        //
        //  GET SUBCRIBER NAME
        //
         chain smkey psm01;
         *IN60 = NOT %FOUND;
         if *in60 = *on;
           clear xxsbnm;
         else;
      /END-FREE
     C                   movel     smname        xxsbnm
      /FREE
         endif;
        //
        // Get the Routing Information and Print Routing Page
        //
        //
      /END-FREE
     C                   move      *off          *in99
      //
      // Retrieve client id
      //
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
      //
      //  Check to see if WCMS is installed
      //
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1        WC INST
     C                   PARM                    @@MCIF            1        MC INST
     C                   PARM                    @@WCIN            1        WC INTERFACE
04   C                   PARM                    @@AR              1        MC AR INST
05   C                   PARM                    @@NTWK            1        NETWORK INST
06   C                   PARM                    @@UR              1        PRECERT INST
07   C                   PARM                    @@MED             1        MEDMGMT INST
08   C                   PARM                    @@EDI             1        EDI INST
09   C                   PARM                    @@WCDK            1        WC DARTS KEY
10   C                   PARM                    @@RHAB            1        REHAB INST
11   C                   PARM                    @@MSS             1        MS SUPPORT
12   C                   PARM                    @@FSCH            1        FEE SCHEDULE
13   C                   PARM                    @@PRVB            1        PROVIDER BILLS
14   C                   PARM                    @@NPWC            1        NON-PDS WC  LS
      //
      /FREE
       endsr;
       //=====================================================================
       // SUBROUTINE PHARSR
       //=====================================================================
       begsr pharsr;
         //
         //
         // First time through write the transmission header info initialize
         // claim counter for DWC10 claims
         //
         if first10 = 'Y';
           numclm10 = 0;
           exsr lodthdr;
           except tranHdr10;
           first10 = *blank;
         endif;
         //
         // Sum up the number of claims in the transmission for the trailer
         //
      /END-FREE
     C                   add       1             numclm10
      //
      // Load header info for the current EOB
      //
      /FREE
         exsr lodbhdr;
         except billHdr10;
         //
         chain caky pca01;
         *IN99 = NOT %FOUND;
         //
         dow not *in99;
           //
          If emppayment = *off;
             exsr loddtl10;
             if skpdtl10 = 'N';
               except billDtl10;
             endif;
          Endif;
           //
           reade caky pca01;
           *IN99 = %EOF;
           //
         enddo;
         //
       endsr;
      //
      //
       //=====================================================================
       // SUBROUTINE splitphar - Split out Drugs and Supplies
       //=====================================================================
       begsr splitphar;
         //
         totpay = 0;
         totpaysupp = 0;
         //
         chain caky pca01;
         *IN99 = NOT %FOUND;
         //
         dow not *in99;
           //
           kkdseq = cadseq;
           //
           chain dtky pdp01;
           *IN99 = NOT %FOUND;
           //
           kkndc = dpndc;
           chain rxky prx01;
           *IN99 = NOT %FOUND;
           //
           if *in99 = *off;
             totpay = totpay + capaid;
             //
           else;
             totpaysupp = totpaysupp + capaid;
             //
           endif;
           //
           reade caky pca01;
           *IN99 = %EOF;
           //
         enddo;
       endsr;
      //
       //=====================================================================
       // SUBROUTINE updathdr - Update the Bill Header Files
       //=====================================================================
       begsr updathdr;
         //
       //*** Beg Add ** 09/14/16 ********************************************  ***
      /end-free
     C*** Beg Chg ** 12/07/16 ********************************************  ***
     C**** EOky1         Chain     pEO02                              99
     C     EOky1         Chain     pEO01                              99
     C*** End Chg ** 12/07/16 ********************************************  ***
     C                   If        *in99 = *off
    C*
    C* Build the control number and increment it
    C*
     C                   if        eocntl = *blanks
     C                   exsr      gencntlnum
     C                   else
     C                   eval      cntl = eocntl
     C                   endif
     c/free
       //*** End Add ** 09/14/16 ********************************************  ***
         if eotape <> 'W';
           eotape = 'S';
         //** Beg Add ** 01/27/16 ************************************      ***
         Else;
           eotape = 'N';
           eocntl = *blanks;
           eosbdt = *blanks;
         //** Write bill status history record
             ##mode = 'H';
             ##rrn2 = DBrrn;
             ##blSt = 'E';
             ##stCd = 'WDR';
             WrtBillStatus(##kl:##kc:##mode:##rrn:##rrn2:##blSt:##stCd:
                           ##reas:##pgm:##ltr#:##edi);
         //*** End Add ** 01/27/16 **************                           ***
         endif;
        //
         eopgm  = q1user;
         //
      /END-FREE
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eodt
     C                   move      dstime        eotime
     //
      /FREE
         if eosbdt = *blanks;
      /END-FREE
     C     *ymd          move      sys_date      eosbdt
      /FREE
         endif;
        //** Beg Add ** 01/27/16 ************************************       ***
         if        eocntl = *blanks;
           eval      eosbdt = *blanks;
         Endif;
        //** End Add ** 01/27/16 ************************************       ***
        //
       //*** Beg Chg ** 09/14/16 ********************************************  ***
       //***  if ##clt# = '051';
       //***    update peo02;
       //***  else;
       //***    update peo01;
       //***  endif;

       //*** Beg Chg ** 12/07/16 ********************************************  ***
         //*** update peo02;
         update peo01;
       //*** End Chg ** 12/07/16 ********************************************  ***
       //*** End Chg ** 09/14/16 ********************************************  ***
        //
         chain eaky pea01;
         *IN99 = NOT %FOUND;
    
         if not *in99;
          //
           if eotape <> 'W';
             eaedi  = 'S';
           endif;
           eacntl = eocntl;
           //
      /END-FREE
     C                   time                    dsdttm
      //
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         eaesdt
      //
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
      //
     C                   movel     dstime        eatime
      /FREE
           eapgm  = q1user;
           //
           update pea01;
           //
         endif;
       //*** Beg Add ** 09/14/16 ********************************************
      /end-free
     c                   Endif
      /free
       //*** End Add ** 09/14/16 ********************************************
        //
       endsr;
       //=====================================================================
       // DWC09 Output File
       //=====================================================================
       // DWC09 Transmission Header
       //
      /END-FREE
     Ofeppfle   eadd         tranHdr09
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill263            300
      // DWC09 Bill Header
     O          eadd         billHdr09
     O                       cntl                13
     O                       recflg              14
     O                       formid              16
     O                       insurcode           21
     O                       insurtaxid          30
     O                       insurzip            39
     O                       servcocode          44
     O                       servcotaxid         53
     O                       servcozip           62
     O                       emplid              71
     O                       accdt               79
     O                       lastnm             109
     O                       firstnm            124
     O                       middin             125
     O                       empdob             133
     O                       empsex             134
     O                       license#           147
     O                       provtaxid          156
     O                       zipserv            165
     O                       billrcvdt          173
     O                       billpaydt          181
     O                       totpay             192
     O                       reportreas         194
     O                       payplan            196
     O                       strICDA            204
     O                       strICDB            212
     O                       strICDC            220
     O                       strICDD            228
     O                       insintfile#        258
     O                       subloc             278
     O                       xxemplppind        279
     O                       xxdupovrind        280
     O                       isICD10            281
     O                       strICDE            289
     O                       strICDF            297
     O                       strICDG            305
     O                       strICDH            313
     O                       strICDI            321
     O                       strICDJ            329
     O                       strICDK            337
     O                       strICDL            345
     O                       strResubCode       356
     O                       fill09head         700
      // DWC09 Bill Detail
     O          eadd         billDtl09
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       placesrv            19
     O                       icd9ref             23
     O                       billedproc          28
     O                       billedmod1          30
     O                       billedmod2          32
     O                       billedmod3          34
     O                       billedmod4          36
     O                       paidproc            41
     O                       paidmod1            43
     O                       paidmod2            45
     O                       paidmod3            47
     O                       paidmod4            49
     O                       detchrg             60
     O                       units               63
     O                       detpaid             74
     O                       dateofsrvfrm        82
     O                       dateofsrvto         90
     O                       @xxndc             103
     O                       eobcode1           105
     O                       eobcode2           107
     O                       eobcode3           109
     O                       strNDC2nd          122
     O                       fill09det          500
      // DWC09 Transmission Trailer
     O          eadd         tranTrl09
     O                       trantlrmkr           3
     O                       numclm09             9
     O                       fill291            300
      //=====================================================================
      // DWC10 Output File
      //=====================================================================
      // DWC10 Transmission Header
      //
     Ofexpfle   eadd         tranHdr10
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill10trhdr        300
      // DWC10 Bill Header
     O          eadd         billHdr10
     O                       cntl                13
     O                       recflg              14
     O                       formid              16
     O                       insurcode           21
     O                       insurtaxid          30
     O                       insurzip            39
     O                       servcocode          44
     O                       servcotaxid         53
     O                       servcozip           62
     O                       emplid              71
     O                       accdt               79
     O                       lastnm             109
     O                       firstnm            124
     O                       middin             125
     O                       empdob             133
     O                       empsex             134
     O                       billrcvdt          142
     O                       billpaydt          150
     O                       totpay             161
     O                       totpaysupp         172
     O                       reportreas         174
     O                       insintfile#        204
     O                       subloc             224
     O                       payplan            226
     O                       xxemplppind        227
     O                       license#           240
     O                       xxdupovrind        241
     O                       fill10head         500
      // DWC10 Bill Detail
     O          eadd         billDtl10
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       drugsuppind         18
     O                       qtyofmed            23
     O                       dayssupply          26
     O                       @xxndc              39
     O                       neworrefill         40
     O                       purchrent           41
     O                       datefilled          49
     O                       dispaswrit          50
     O                       prescbrlic          63
     O                       detchrg             74
     O                       eobcode1            76
     O                       eobcode2            78
     O                       eobcode3            80
     O                       hcpcs               85
     O                       detpaid             96
     O                       strNDC2nd          109
     O                       fill10det          500
      // DWC10 Transmission Trailer
     O          eadd         tranTrl10
     O                       trantlrmkr           3
     O                       numclm10             9
     O                       fill10trtrl        300
      //=====================================================================
      // DWC11 Output File
      //=====================================================================
      // DWC11 Transmission Header
      //
     Ofedpfle   eadd         tranHdr11
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill263            300
      // DWC11 Bill Header
     O          eadd         billHdr11
     O                       cntl                13
     O                       recflg              14
     O                       formid              16
     O                       insurcode           21
     O                       insurtaxid          30
     O                       insurzip            39
     O                       servcocode          44
     O                       servcotaxid         53
     O                       servcozip           62
     O                       emplid              71
     O                       accdt               79
     O                       lastnm             109
     O                       firstnm            124
     O                       middin             125
     O                       empdob             133
     O                       empsex             134
     O                       license#           147
     O                       provtaxid          156
     O                       zipserv            165
     O                       placetmt           167
     O                       billrcvdt          175
     O                       billpaydt          183
     O                       totpay             194
     O                       reportreas         196
     O                       paycode            198
     O                       insintfile#        228
     O                       subloc             248
     O                       xxemplppind        249
     O                       xxdupovrind        250
     O*** Beg Chg *** 09/16/16 ******************************
     O****************       isICD10            252
     O                       isICD102A          252
     O*** End Chg *** 09/16/16 ******************************
     O                       strICDA            260
     O                       strICDB            268
     O                       strICDC            276
     O                       strICDD            284
     O                       fill11head         500
      // DWC11 Bill Detail
     O          eadd         billDtl11
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       dateofsrvfrm        25
     O                       billedproc          30
     O                       paidproc            35
     O                       detchrg             46
     O                       detpaid             57
     O                       eobcode1            59
     O                       eobcode2            61
     O                       eobcode3            63
     O                       fill237            300
      // DWC11 Transmission Trailer
     O          eadd         tranTrl11
     O                       trantlrmkr           3
     O                       numclm11             9
     O                       fill291            300
      //=====================================================================
      // DWC90 Output File
      //=====================================================================
      // DWC90 Transmission Header
      //
     Ofehpfle   eadd         tranHdr90
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill90trhdr        500
      // DWC90 Bill Header
     O          eadd         billHdr90
     O                       cntl                13
     O                       recflg              14
     O                       formid              16
     O                       insurcode           21
     O                       insurtaxid          30
     O                       insurzip            39
     O                       servcocode          44
     O                       servcotaxid         53
     O                       servcozip           62
     O                       typebill            65
     O                       emplid              74
     O                       accdt               82
     O                       lastnm             112
     O                       firstnm            127
     O                       middin             128
     O                       empdob             136
     O                       empsex             137
     O                       license#           150
     O                       operlicense        163
     O                       admissdt           171
     O                       admisshr           173
     O                       stmtcovfrmdt       181
     O                       stmtcovtomdt       189
     O                       dischrghr          191
     O                       provtaxid          200
     O                       facilzip           209
     O                       billrcvdt          217
     O                       billpaydt          225
     O                       totpay             236
     O                       strICDA            244
     O                       strICDB            252
     O                       strICDC            260
     O                       strICDD            268
     O                       strICDE            276
     O                       strICDF            284
     O                       strICDG            292
     O                       strICDH            300
     O                       extinjcd1          308
     O                       extinjcd2          316
     O                       extinjcd3          324
     O                       princprcd          332
     O                       otherprcd_a        340
     O                       otherprcd_b        348
     O                       otherprcd_c        356
     O                       otherprcd_d        364
     O                       otherprcd_e        372
     O                       reportreas         374
     O                       payplan            376
     O                       typeadm            377
     O                       insintfile#        407
     O                       subloc             427
     O                       xxemplppind        428
     O                       xxdupovrind        429
     O                       schdunschdind      430
     O                       xxtotpay           441
     O                       fac_license        454
     O                       fac_npi#           464
     O                       isICD10            465
     O                       fill90head         700
      // DWC90 Bill Detail
     O          eadd         billDtl90
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       revcode             21
     O                       billedproc          26
     O                       billedmod           28
     O                       billedmod2          30
     O                       billedmod3          32
     O                       billedmod4          34
     O                       paidproc            39
     O                       paidmod1            41
     O                       paidmod2            43
     O                       paidmod3            45
     O                       paidmod4            47
     O                       units90             54
     O                       detchrg             65
     O                       eobcode1            67
     O                       eobcode2            69
     O                       eobcode3            71
     O                       dateofopsrv         79
     O                       inspayment          90
     O                       fill90dtl          500
      // DWC90 Transmission Trailer
     O          eadd         tranTrl90
     O                       trantlrmkr           3
     O                       numclm90             9
     O                       fill90trtrl        500
     //
     Oqsysprt   e            route          1 06
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      //
     O          e            route          1 34
     O                                           18 'COMPANY  :'
     O                       xxco#         z     25
     O          e            route          1
     O                                           18 'SUBSCRIBER:'
     O                       xxsub#        z     27
     O          e            route          1
     O                                           18 'FROM DATE:'
     O                       xxfrom              30 '  /  /    '
     O          e            route          1
     O                                           18 'TO DATE  :'
     O                       xxto                30 '  /  /    '
     O          e            route            60
     O                                              '(C) COPYRIGHT PROGRESSIV'
     O                                         +  0 'E DATA SOLUTIONS, INC.'
     O                       *year             +  1
     O          e            head           1 06
     O                       dstime              08 '  :  :  '
     O                       xxconm             120
     O                       dsdate        y    198
     O          e            head           1
     O                       q1pgm               10
     O                       ortitl             129
     O                                          192 'PAGE:'
     O                       page          z    198
     O          e            head           1
     O                                           88 'FROM DATE:'
     O                       xxfrom              98 '  /  /    '
     O                                          108 'TO DATE:'
     O                       xxto               118 '  /  /    '
     O          e            head           1
     O                       xxproc             102
     O          e            head           1
     O                                           11 'SUBSCRIBER:'
     O                       xxsub#        z     18
     O                       xxsbnm              60
     O          e            errms1      3  1
     O                                           33 '*** THERE IS NO COMPANY'
     O                                           54 'AUXILIARY RECORD FOR'
     O                                           67 'THIS COMPANY'
     O          e            errms1         1
     O                                           29 'ENTER ONE, THEN'
     O                                           50 'RE-RUN THIS JOB. ***'
     O          e            errms2      3  1
     O                                           33 '*** THERE ARE NO EOMB R'
     O                                           53 'ECORDS FOR THIS FUND'
     O                                           75 ' WITHIN SELECTED DATES'
      //=====================================================================
      // Procedure getDiag - Returns diagnosis code with decimal
      //=====================================================================
     P
     P getDiag         B
     D getDiag         PI             7
     D  icd9                          5
      //
      /FREE
       kkdiag = icd9;
       chain diky pdi01;
       *IN99 = NOT %FOUND;
       if not *in99;
         return dids1;
       else;
         return *blanks;
       endif;

      /END-FREE
     P getDiag         E
      //=====================================================================
      // Procedure getDiag10 - Returns diagnosis code 10 with decimal
      //=====================================================================
     P
     P getDiag10       B
     D getDiag10       PI             8
     D  icd10                         7
     D  indtyp                        1
      //
      /FREE
       kkdiag10 = icd10;
       kkind = indtyp;
       chain (kkdiag10:kkind) fmicd10l1;
       if %found(fmicd10l1);
         return icfcode;
       else;
         return *blanks;
       endif;

      /END-FREE
     P getDiag10       E
