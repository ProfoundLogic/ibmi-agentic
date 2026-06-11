    H**********************************************************************
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H*    PROGRAM: F9711RAFL
    H*    DESCRIPTION: FL Med EDI Extract.
    H**********************************************************************
    H*    PROGRAMMER NOTES: THE CONTROL# CNTL IS MADE UP OF
    H*                      CCCYYJJJNNNNN
    H*                      CCC=Submitter#, YY=Year, JJJ=Julian Date
    H*                      NNNNN=The calculated sequence number
    H**********************************************************************
    H*    CALLED FROM:
    H*      F9711CFL - CREATE WORKFILE
    H*****************************************************************
    H*  CREATED: 03/14/07 C: 79 PROJ: FL067
    H**********************************************************************
    H*  REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  11/28/07 C:198 SUPPORT:60885
     H*                 Changed imptotpay field to have leading zeros.
     H*
     H** 09/27/07  C:198  Support: 60235
     H**                  Add edit to check status of FL EDI
     H**                  extract to prevent deletion when EOAST = 'FL'
     H**                  and EOTAPE <> ' ' or 'E' or 'W', and display
     H**                  an error message. Also restrict change of fields
     H**                  EHEDI, EUEDI, EPEDI, EAEDI, EDEDI and EOTAPE
     H**                  to 'S' to only when EOTAPE <> 'W'.
     H*
     H*  09/20/07 C:183 PROJECT:FL053
     H*                 Removed changes for Dental Detail and added logic
     H*                 for "Unit" field change in Output Sepcs change in
     H*                 loddtl09 subroutine.
     H*
     H*  09/10/07 C:079 PROJECT:FL070
     H*                 Correct the fields used for license number
     H*
     H*  08/22/07 C:079 Support: 59634
     H*                 Correct the key list for the fund auxiliary         apping
     H*
     H*  08/08/07 C:079 SUPPORT:59477
     H*                 For notes we cannot default an EOBR code
     H*
     H*  08/08/07 C:183 PROJECT:FL053
     H*                 CHANGED IN ORDER TO ACCESS NEW LOGICAL FDCAL28.
     H*
     H*  08/03/07 C:189 PROJECT:FL071
     H*             When a bill is marked as an employee payment type bill,
     H*             only certain fields are Required
     H*
     H*  07/25/07 C:194 SEQ 57325
     H*                 Medical EDI Reconsideration received
     H*                 date issue.
     H*
     H*
     H*  06/08/07 C:182 PROJECT:FL070
     H*                 For DWC90 populated Facility Zip code from
     H*                 EOFLIC if not blank.
     H*
     H*  06/04/07 C:188 PROJECT:FL067
     H*                 Set default EOBR codes on detail lines.
     H*
     H*  05/14/07 C:182 PROJECT:FL067
     H*                 Incorporated the chnages required to address issue
     H*                 regarding NDC code on DWC09 & DWC10.
     H*
     H*  05/09/07 C:182 PROJECT:FL067
     H*                 Incorporated the chnages required to address issue
     H*                 regarding payment code on emp. payment.
     H*
     H*  05/07/07 C:182 PROJECT:FL067
     H*                 Incorporated the chnages required to address issue
     H*                 regarding NDC code,EOB codes & Ext. cause of injury.
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 FTEBLFL to be used only if the adjudication state (FAEOP/EOAST = FL).
     H*
     H*  03/10/06  C:079  Project: 51MGU
     H*                   For Employee Payments dont load Payment Plan
     H*
    H*  06/21/04 C:62 PROJECT 51MFP
    H*                APPLY 51MDL MOD
     H*
    H*******************************************************************
    H* INDICATOR USAGE:
    H*
    H*  99   -   GENERAL CHAIN
    H*******************************************************************
     Ffeppfl    o  a f  300        disk
     Ffexpfl    o  a f  300        disk
     Ffehpfl    o  a f  450        disk
     Ffedpfl    o  a f  300        disk
    F*** BEGIN CHG *** 06/21/04 *******************************************
     Ffaeol     uf   e             disk
     Ffaeol4    uf   e           k disk    rename(peo01:peo02)
     Ffteol     if   e           k disk    rename(peo01:peo03)
    F*** END   CHG *** 06/21/04 *******************************************
     Fsmcol     if   e           k disk
     Ffmc1l     if   e           k disk
     Ffmf1l     if   e           k disk
     Ffdf2l     if   e           k disk
     Fsmorl     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffdcal5    if   e           k disk
    F*** BEG ADD *** 08/08/07 *******************************************
     Ffdcal28   if   e           k disk    rename(pca01:pca28)
    F*** END ADD *** 08/08/07 *******************************************
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
    F*
     Fqsysprt   o    f  198        printer oflind(*inoa)
     Ffmtsp     uf a e           k disk
     Ffaesl3    if   e           k disk
     Ffaeal1    uf   e           k disk
    F*
     Fftpmlfl   if   e           k disk
     Ffmpllfl   if   e           k disk
     Fststl     if   e           k disk
     Ffteblfl   if   e           k disk
     Ffmrxl     if   e           k disk
    F*
     D getDiag         PR             7
     D  icd9                          5
     P*=====================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D  dsmd                          4  0 overlay(dsdttm:7)
     D  dscy                          4  0 overlay(dsdttm:11)
    D*
     D                 ds
     D xxdt                           8  0
     D  xxmm                          2  0 overlay(xxdt)
     D  xxdd                          2  0 overlay(xxdt:3)
     D  xxyy                          4  0 overlay(xxdt:5)
     D                 ds
     D xxcnt                          8  0
     D  xxmmdd                        4  0 overlay(xxcnt)
     D  xxyr                          4  0 overlay(xxcnt:5)
    D*
    D* Year prior to current date's year; used to calc Julian date
     D                 ds
     D zzdt                           8  0
     D  zzmm                          2  0 overlay(zzdt)
     D  zzdd                          2  0 overlay(zzdt:3)
     D  zzyy                          4  0 overlay(zzdt:5)
    D*
    D* Data Structure for Claim Control Number
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
    D*** BEG ADD ***** 05/07/07 **************************************
    D*
    D* Data Structure for NDC Code
     D                 ds
     D @xxndc                        13
     D  ndc5                          5    overlay(@xxndc)
     D  hyphen1                       1    overlay(@xxndc:6)
     D  ndc4                          4    overlay(@xxndc:7)
     D  hyphen2                       1    overlay(@xxndc:11)
     D  ndc2                          2    overlay(@xxndc:12)
    D*** END ADD ***** 05/07/07 **************************************
    D*
     D sys_date        s               D   datfmt(*USA)
    D*
    D* External Data Structure
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
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
     D*
     D dispaswrit      s              1
     D*
     D drugsuppind     s              1
     D*
    D*** BEG CHG ***** 08/03/07 **************************************
     D****** empdob          s              8  0
     D empdob          s              8
    D*** END CHG ***** 08/03/07 **************************************
     D*
     D empsex          s              1
     D*
    D*** BEG CHG ***** 05/07/07 **************************************
     D*
     D***** extinjcd1       s                   like(eucau1)
     D extinjcd1       s              6
     D***** extinjcd2       s                   like(eucau2)
     D extinjcd2       s              6
     D***** extinjcd3       s                   like(eucau3)
     D extinjcd3       s              6
     D*
    D*** END CHG ***** 05/07/07 **************************************
     D*
     D hcpcs           s                   like(dphcpc)
     D billrcvdt       s              8
     D billpaydt       s              8
     D cntr            s              5  0
     D datefilled      s              8
     D dateofsrvfrm    s              8
     D dateofsrvto     s              8
     D dateofopsrv     s              8
     D dayssupply      s              3S 0
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
     D*eobcode4        s              2
     D facilzip        s              9
     D fill2           s              2    inz(*blanks)
     D fill32          s             32    inz(*blanks)
     D fill90head      s             27    inz(*blanks)
     D fill10head      s             59    inz(*blanks)
     Dfill09head       s             20    inz(*blanks)
     D fill11head      s             50    inz(*blanks)
     D fill166         s            166    inz(*blanks)
     D fill10trhdr     s            263    inz(*blanks)
     D fill09det       s            195    inz(*blanks)
     D fill237         s            237    inz(*blanks)
     D fill10trtrl     s            291    inz(*blanks)
     D fill90dtl       s            390    inz(*blanks)
     D fill263         s            263    inz(*blanks)
     D fill291         s            291    inz(*blanks)
     D fill90trhdr     s            413    inz(*blanks)
     D fill10det       s            204    inz(*blanks)
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
     D icd91           s              6
     D icd92           s              6
     D icd93           s              6
     D icd94           s              6
     D icd95           s              6
     D icd96           s              6
     D icd97           s              6
     D icd98           s              6
     D insintfile#     s             30
     D juldat          s              5
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D*** BEG CHG *** 08/22/07 *********************************************
     D******** kkst            s                   like(eost)
     D kkast           s                   like(eost)
     D*** END CHG *** 08/22/07 *********************************************
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D kkdseq          s                   like(cadseq)
      *** BEG ADD *** 08/08/07 *******************************************
     D kkmseq          s                   like(camseq)
      *** END ADD *** 08/08/07 *******************************************
     D kkrcn           s                   like(eorcn)
     D kkkey           s                   like(stkey)
     D kkdiag          s                   like(diicd9)
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
     D otherprcd       s              5
     D otherprcd_b     s              5
     D otherprcd_c     s              5
     D otherprcd_d     s              5
     D otherprcd_e     s              5
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
     D*** BEG ADD *** 09/20/07 *********************************************
     D @units          s              3  0 INZ
     D*** END ADD *** 09/20/07 *********************************************
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
    D*** BEG ADD ***** 08/03/07 **************************************
     D emppayment      s                   like(*in99)
    D*** END ADD ***** 08/03/07 **************************************
    D*** Beg Add ***** 11/28/07 **************************************
     Dxxtotpay         s             11  0 inz(0)
    D*** End Add ***** 11/28/07 **************************************
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C* Entry Parameters
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfrdt
     C                   parm                    qqtodt
     C                   parm                    qqque
     C                   parm                    qqproc
     C                   parm                    qqtpin
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
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
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
     C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
    C* Key List for the Company Master File
    C*
     C     cokey         klist
     C                   kfld                    kkco#
    C*
    C* Key List for the Company AUXILIARY Master File
    C*
     C     c1ky          klist
     C                   kfld                    kkco#
    C*
    C* Key List for the FUND AUXILIARY Master File
    C*
     C     f1key         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
    C*
    C*** BEGIN ADD *** 06/21/04 *******************************************
    C*
     C     eoky1         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
    C*** END   ADD *** 06/21/04 *******************************************
    C*
    C* Key List for the Output Routing Master File
    C*
     C     orkey         klist
     C                   kfld                    q1pgm
     C                   kfld                    kkprt
    C*
    C* Key List for the SUBCRIBER   File
    C*
     C     smkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
    C*
    C* Key List for the Claim Master File
    C*
     C     cmkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
    C*
    C* Key List for the Claimant Demographics Master File
    C*
     C     cdkey         klist
     C                   kfld                    kkss#
    C*
    C* Key List for the Provider Master File
    C*
     C     pdkey         klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
    C* Key List for the GROUP STATE INFORMATION FILE
    C*
     C     f2key         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C*** BEG CHG *** 08/22/07 *********************************************
     C***************    kfld                    kkst
     C                   kfld                    kkast
     C*** END CHG *** 08/22/07 *********************************************
    C*
    C* Key List for the TAPE SUBMISSION SEQ# FILE
    C*
     C     tskey         klist
     C                   kfld                    kkco#
    C*
    C* Key List for the DIAGNOSIS CODE FILE
    C*
     C     diky          klist
     C                   kfld                    kkdiag
    C*
    C* Key List for the EOMB Header FILES
    C*
     C     hdky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C* Key List for the EOMB DETAIL CHARGE FILE
    C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
      *** BEG ADD *** 08/08/07 *******************************************
    C* Key List for the CLAIM DETAIL FILE
    C*
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkmseq
    C*
      *** END ADD *** 08/08/07 *******************************************
    C* Key List for the EOMB DETAIL FILES
    C*
     C     dtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
    C* Key List for the EOMB DETAIL EOB CODE FILE
    C*
     C     deky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkesrc
    C*
    C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
    C*
     C     esky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
    C*
     C     esky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C*  EOMB Header Amount file
    C*
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
    C*
    C*  Procedure code modifier table file
    C*
     C     pmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
    C** Key List for State Code Table File
    C*
     C     stky          klist
     C                   kfld                    kkkey
    C*
    C** Key List for State EOB Code Table file
    C*
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C     rxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkndc
     C*=====================================================================
    C* MAIN PROGRAM BEGINS
     C*=====================================================================
    C*
     C                   exsr      pgmstr
    C*
    C*  BEGIN READING BATCH
    C*
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C*
     C                   if        ##clt# = '051'
     C                   read      peo03                                  99
     C                   else
    C*** END   ADD *** 06/21/04 *******************************************
     C                   read      peo01                                  99
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   endif
    C*** END   ADD *** 06/21/04 *******************************************
    C*
     C                   if        *in99 = *on
     C                   except    errms2
     C                   exsr      pgmend
     C                   endif
    C*
     C                   dow       *in99 = *off
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   if        ##clt# = '051'
     C     eoky1         chain     peo02                              99
     C                   endif
     C                   if        *in99=*off
    C*** END   ADD *** 06/21/04 *******************************************
    C*
    C* If check date is < recon date then batch has not updated check
    C* date so don't include this EOB
    C*  Get data from various files, group, company, etc.
    C*
     C                   exsr      getFiledata
    C*
     C                   select
    C*
     C                   when      eobtyp = 'N'
     C                   exsr      medsr
    C*
     C                   when      eobtyp = 'D'
     C                   exsr      dentsr
    C*
     C                   when      eobtyp = 'X'
     C                   exsr      pharsr
    C*
     C                   when      eobtyp = 'H'
     C                   exsr      hossr
     C*
     C                   endsl
    C*
     C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   endif
     C*** END   ADD *** 06/21/04 *******************************************
    C*
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   if        ##clt# = '051'
     C                   read      peo03                                  99
     C                   else
    C*** END   ADD *** 06/21/04 *******************************************
     C                   read      peo01                                  99
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   endif
    C*** END   ADD *** 06/21/04 *******************************************
    C*
     C                   enddo
    C*
     C                   exsr      pgmend
     C*=====================================================================
     C     checkRecon    begsr
     C*=====================================================================
    C*
    C* Only select esrrdt where status is 'R'
    C*
     C                   if        eorcn > 0
    C*
     C     esky1         setgt     pes01
     C     esky2         readpe    pes01                                  60
     C                   dow       *in60 = *off
    C*
     C                   if        esblst = 'R'
     C                   leave
     C                   endif
    C*
     C     esky2         readpe    pes01                                  60
    C*
     C                   enddo
    C*
     C                   endif
    C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE DENTSR
     C*=====================================================================
     C     dentsr        begsr
     C*
     C* First time through write the transmission header info initialize
     C* claim counter for DWC11 claims
     C*
     C                   if        first11 = 'Y'
     C                   eval      numclm11 = 0
     C                   exsr      lodthdr
     C                   except    tranHdr11
     C                   eval      first11 = *blank
     C                   endif
     C*
     C* Sum up the number of claims in the transmission for the trailer
     C*
     C                   add       1             numclm11
     C*
     C* Load header info for the current EOB
     C*
     C                   exsr      lodbhdr
     C*
     C                   except    billHdr11
     C*
      *** BEG CHG *** 08/08/07 *******************************************
     C*****caky          chain     pca01                              99
      *** BEG CHG *** 09/20/07 *******************************************
     C*****caky          chain     pca28                              99
     C     caky          chain     pca01                              99
      *** END CHG *** 09/20/07 *******************************************
      *** END CHG *** 08/08/07 *******************************************
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
      *** BEG ADD *** 08/08/07 *******************************************
      *** BEG DLT *** 09/20/07 *******************************************
     C***********        Eval      kkmseq = camseq
     C***********        Call      'FA751R'
     C***********        Parm                    kkco#
     C***********        Parm                    kksub#
     C***********        Parm                    kkfnd
     C***********        Parm                    kkfyr
     C***********        Parm                    kkcase
     C***********        Parm                    kkeob#
     C***********        Parm                    kkmseq
     C***********        Parm      *ZEROS        @@chrg           11 2
     C***********        Parm      *ZEROS        @@paid           11 2
      *
      *** END DLT *** 09/20/07 *******************************************
      *** END ADD *** 08/08/07 *******************************************
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   exsr      loddtl11
     C                   except    billDtl11
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
      *** BEG CHG *** 08/08/07 *******************************************
     C*****caky          reade     pca01                                  99
      *** BEG CHG *** 09/20/07 *******************************************
     C*****caky1         setgt     pca28
     C*****caky          reade     pca28                                  99
     C     caky          reade     pca01                                  99
      *** END CHG *** 09/20/07 *******************************************
      *** END CHG *** 08/08/07 *******************************************
     C*
     C                   enddo
     C*
     C                   endsr
    C*
     C*=====================================================================
     C* SUBROUTINE gencntlnum
     C*=====================================================================
     C     gencntlnum    begsr
    C*
     C                   move      c1subm        ccc
     C                   move      xxyy          yy
     C                   move      srdays        jjj
     C                   eval      cntr = cntr + 1
     C                   move      cntr          sssss
     C                   movel     cntl          eocntl
     C                   movel     dsdate        eosbdt
    C*
     C                   endsr
     C*=====================================================================
    C* SUBROUTINE getFiledata
     C*=====================================================================
     C     getFiledata   begsr
     C*
     C                   eval      kkco#  = eoco#
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd  = eofnd
     C*** BEG CHG *** 08/22/07 *********************************************
     C**************     eval      kkst   = eost
     C                   eval      kkast  = eoast
     C*** END CHG *** 08/22/07 *********************************************
     C                   eval      kkfyr  = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   eval      kkrcn  = eorcn
     C                   eval      kkss#  = eoss#
     C                   eval      kkfein = eofein
     C                   eval      kkmod  = eomod
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    C                   If        EOEPMT = 'Y'
    C                   Eval      emppayment = *on
    C                   Else
    C                   Eval      emppayment = *off
    C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C* Get the fund auxiliary record
    C*
     C     f1key         chain     pf101                              99
    C*
    C* Get the Claim Master Record
    C*
     C     cmkey         chain     pcm01                              99
    C*
    C* Get the Claimant Demographic Record
    C*
     C     cdkey         chain     pcd01                              99
    C*
    C* Get the Provider Master Record
    C*
     C     pdkey         chain     ppd01                              99
    C*
    C* Get the group state information
    C*
     C     f2key         chain     pf201                              99
     C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE HOSSR
     C*=====================================================================
     C     hossr         begsr
     C*
     C* First time through write the transmission header info initialize
     C* claim counter for DWC90 claims
     C*
     C                   if        first90 = 'Y'
     C                   eval      numclm90 = 0
     C                   exsr      lodthdr
     C                   except    tranHdr90
     C                   eval      first90 = *blank
     C                   endif
     C*
     C* Sum up the number of claims in the transmission for the trailer
     C*
     C                   add       1             numclm90
     C*
     C* Load header info for the current EOB
     C*
     C                   exsr      lodbhdr
     C*
     C                   except    billHdr90
     C*
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   exsr      loddtl90
     C                   except    billDtl90
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C     caky          reade     pca01                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C*
     C*=====================================================================
     C* SUBROUTINE loadbhdr - Load bill header info
     C*=====================================================================
     C     lodbhdr       begsr
    C*
    C* Initialize fields
    C*
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
    C*** BEG ADD ***** 05/09/07 *******************************************
     C                   move      *blanks       paycode
    C*** END ADD ***** 05/09/07 *******************************************
     C                   move      *zeros        statemntdt
     C                   movel     *zeros        totpay
     C                   movel     *zeros        totchrg
     C                   move      *blanks       icd91
     C                   move      *blanks       icd92
     C                   move      *blanks       icd93
     C                   move      *blanks       icd94
     C                   move      *blank        procmth
     C                   move      *blanks       princprcd
     C                   move      *blanks       otherprcd
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
    C*
    C* Determine Report Reason Code
    C*
     C                   select
     C*
     C                   when      eotape = 'E' and eocntl = *blanks
     C                   eval      reportreas = '00'
     C*
     C                   when      eotape = 'W' and eocntl <> *blanks
     C                   eval      reportreas = '01'
     C*
     C                   when      eotape = 'R' and eocntl <> *blanks
     C                             or
     C                             eotape = 'D' and eocntl = *blanks
     C                   eval      reportreas = '02'
     C*
     C                   when      eotape = 'C' and eocntl <> *blanks
     C                             and eorcn > 0
     C                   eval      reportreas = '03'
     C                   when      eotape = 'B' and eocntl <> *blanks
     C                   eval      reportreas = '03'
     C*
     C                   other
     C                   eval      reportreas = '99'
     C*
     C                   endsl
    C*
    C* Build the control number and increment it
    C*
     C                   if        eocntl = *blanks
     C                   exsr      gencntlnum
     C                   else
     C                   eval      cntl = eocntl
     C                   endif
     C*
     C* Duplicate Override Indicator
     C*
     C                   if        eotape = 'D'
     C                   eval      xxdupovrind = 'Y'
     C                   endif
    C*
    C* Update the bill header files FAEOP/FAEAP
    C*
     C                   exsr      updathdr
    C*
    C* Load the Lost time info
    C*
     C                   if        cmltfl = 'I'
     C                   eval      lostTime = 'LT'
     C                   else
     C                   eval      lostTime = 'MO'
     C                   endif
    C*
    C* Load Insurer header info
    C*
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   if        f2fl01 = 'Y'
     C                   move      eosemp        insurcode
     C                   move      eoefid        insurtaxid
     C                   movel     eoezip        insurzip
     C                   if        %subst(insurzip:6:4) = '0000'
     C                   movel     eoezip        xxzip             5
     C                   movel(P)  xxzip         insurzip
     C                   endif
     C                   else
    C*** END   ADD *** 06/21/04 *******************************************
     C                   move      f25n01        insurcode
     C                   move      f29n01        insurtaxid
     C                   move      f29n02        insurzip
     C                   if        %subst(insurzip:6:4) = '0000'
     C                   movel     f29n02        xxzip             5
     C                   movel(P)  xxzip         insurzip
     C                   endif
    C*** BEGIN ADD *** 06/21/04 *******************************************
     C                   endif
    C*** END   ADD *** 06/21/04 *******************************************
    C*
    C* Load Service Co/TPA info
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   move      f25n02        servcocode
     C                   move      f29n03        servcotaxid
     C                   move      f29n04        servcozip
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   eval      servcocode = *blanks
     c                   eval      servcotaxid = *blanks
     C                   eval      servcozip   = *blanks
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        %subst(servcozip:6:4) = '0000'
     C                   movel     f29n04        xxzip             5
     C                   movel(P)  xxzip         servcozip
     C                   endif
     C*
     C* If info for service company is missing then load blanks
     C*
     C                   if        servcocode = *zeros
     C                   eval      servcocode = *blanks
     C                   endif
     C                   if        servcotaxid = *zeros
     c                   eval      servcotaxid = *blanks
     C                   endif
     C                   if        servcozip   = '00000    '
     C                   eval      servcozip   = *blanks
     C                   endif
    C*
    C* Load Employee info
    C*
     C                   movel     cmss#         emplid
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         accdt
     C*
     C* Last name
     C*
     C                   eval      flag = *blanks
     C                   eval      string = cdlsn
     C                   eval      patt1  = cdlsn
     C                   call      'F9716CFL'
     C                   parm                    string           40
     C                   parm                    patt1            15
     C                   parm                    flag              1
     C                   eval      lastnm = string
     C*
     C* First name
     C*
     C                   eval      flag = *blanks
     C                   eval      string = cdfsn
     C                   eval      patt1  = cdfsn
     C                   call      'F9716CFL'
     C                   parm                    string
     C                   parm                    patt1
     C                   parm                    flag
     C                   eval      firstnm = string
     C*
     C* Middle Initial
     C*
     C                   eval      flag = *blanks
     C                   eval      string = cdmid
     C                   eval      patt1  = cdmid
     C                   call      'F9716CFL'
     C                   parm                    string
     C                   parm                    patt1
     C                   parm                    flag
     C                   eval      middin = string
     C                   if        ##clt# = '010'
     C*
     C                   eval      d1 = 0
     C                   eval      middin = ' '
     C     ' '           scan      firstnm       d1
     C*
     C                   select
     C                   when      d1 > 0 and d1 < (%len(firstnm) - 1)
     C*
     C                   if        %subst(firstnm:d1+2:1) =  '.' or
     C                             %subst(firstnm:d1+2:1) =  ' '
     C                   eval      middin = %subst(firstnm:d1+1:1)
     C                   eval      %subst(firstnm:d1) = *blanks
     C                   endif
     C*
     C                   when      d1 > 0 and d1 = (%len(firstnm) - 1)          last pos in field
     C*
     C                   if        %subst(firstnm:d1+1:1) <> '.' and
     C                             %subst(firstnm:d1+1:1) <> ' '
     C                   eval      middin = %subst(firstnm:d1+1:1)
     C                   eval      %subst(firstnm:d1) = *blanks
     C                   endif
     C*
     C                   endsl
     C                   endif                                                  ##clt# = 010
    C*
    C* Employee Date of Birth
    C*
     C                   if        cddtob <> *zeros
     C                   z-add     cddtob        srdti
     C                   call      'SRCVTDAT'    dtyymd
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   movel     srdto         empdob
     C*                  eval      empdob = cddtob
    C*** BEG ADD ***** 08/03/07 **************************************
    C                   Else
     C                   movel     *blanks       empdob
    C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C                   endif
    C*
    C* Employee Gender
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        cdsex <> *blanks
     C                   eval      empsex = cdsex
     C                   else
     C                   eval      empsex = 'U'
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    C                   Else
     C                   movel     *blanks       empsex
    C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C                   if        eoepmt = 'Y'
     C                   eval      xxemplppind = 'E'
     C                   endif
     C                   if        eotodt > EOCRDT or eotodt > EOWCCK
     C                   eval      xxemplppind = 'P'
     C                   endif
    C*
    C* Load Provider info
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   move      eofein        provtaxid
    C*** BEG ADD ***** 08/03/07 **************************************
    C                   Else
     C                   movel     *blanks       provtaxid
    C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*** BEG ADD ***** 06/08/07 **************************************
     C                   If        eoflic <> *blanks
     C                   movel     eoflic        facilZip
     C                   else
     C*** END ADD ***** 06/08/07 **************************************
     C                   move      eozip         facilZip
     C*** BEG ADD ***** 06/08/07 **************************************
     C                   endif
     C*** END ADD ***** 06/08/07 **************************************
     C                   if        %subst(facilzip:6:4) = '0000'
     C*** BEG ADD ***** 06/08/07 **************************************
     C                   If        eoflic <> *blanks
     C                   movel     eoflic        xxzip
     C                   else
     C*** END ADD ***** 06/08/07 **************************************
     C                   movel     eozip         xxzip             5
     C*** BEG ADD ***** 06/08/07 **************************************
     C                   endif
     C*** END ADD ***** 06/08/07 **************************************
     C                   movel(P)  xxzip         facilzip
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   movel     eoflic        zipserv
     C                   if        %subst(zipserv:6:4) = '0000'
     C                   movel     eoflic        xxzip             5
     C                   movel(P)  xxzip         zipserv
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    C                   Else
     C                   movel     *blanks       zipserv
    C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
    C* Load Date Insurer Received Bill
    C*
    C*** BEG ADD ***** 07/25/07 **************************************
     C                   exsr      checkRecon
    C                   if        eorcn > 0 and esrrdt <> 0
    C                             and esrrdt <= eowcck
    C*** END ADD ***** 07/25/07 **************************************
    C*** BEG DEL ***** 07/25/07 **************************************
     C**********         if        reportreas = '3' and esrrdt >= eowcck
    C*** END DEL ***** 07/25/07 **************************************
     C                   z-add     esrrdt        srdti
     C                   else
     C                   z-add     eocrdt        srdti
     C                   endif
    C*
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         billrcvdt
     C*
    C* Load Date Insurer Paid Bill
    C*
     C*
     C* USE THE CHECK MAILING DATE TO REPORT DATE PAID IF NOT BLANK
     C
     C                   EXSR      GETMDT
     C*
     C     #1CMDT        IFNE      *BLANKS
     C*
     C                   MOVE      #1CMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         BILLPAYDT
     C*
     C                   ELSE
     C*
     C*
     C                   z-add     eowcck        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         billpaydt
     C*
     C*
     C                   ENDIF                                                  #1CMDT
     C*
     C*
    C* Load Total Charged/Paid
    C*
     C                   z-add     eopaid        totpay
     C                   z-add     eochrg        totchrg
     C                   movel(P)  eoeob#        insintfile#
     C*
    C* Load Header info for Physician Bills
    C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C* Load Form id
     C*
     C                   move      '09'          formid
     C*
     C     hdky          chain     peh01                              99
     C*
     C                   if        not *in99
     C*
     C* Load Provider License Number
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   movel(P)  ehflic        license#
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       license#
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C* Load Payment Plan
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        eoepmt <> 'Y'
     C                   move      ehppln        payplan
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       payplan
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C* Load diagnosis codes
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   eval      icd91 = getDiag(ehdia1)
     C                   eval      icd92 = getDiag(ehdia2)
     C                   eval      icd93 = getDiag(ehdia3)
     C                   eval      icd94 = getDiag(ehdia4)
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   eval      icd91 = *Blanks
     C                   eval      icd92 = *Blanks
     C                   eval      icd93 = *Blanks
     C                   eval      icd94 = *Blanks
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C* Load the EDI Status with S to indicate this EOB was sent
     C***** BEG ADD ** 09/27/07 ***********************************************
     C* unless it was Withdrawn.
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/07 ***********************************************
     C*
     C                   eval      ehedi  = 'S'
     C*
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   endif
     C*
     C***** END ADD ** 09/27/07 ***********************************************
     C                   eval      ehuser = q1user
     C                   eval      ehpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ehdt
     C                   move      dstime        ehtime
     C*
     C                   update    peh01
     C*
     C                   endif                                                  if not *in99
     C*
    C* Load Header info for Hospital Bills
     C*
     C                   when      eobtyp = 'H'
     C*
     C* Load Form id
     C*
     C                   move      '90'          formid
     C*
     C     hdky          chain     peu01                              99
     C*
     C                   if        not *in99
     C*
     C* Load type of bill
     C*
     C                   eval      typebill = eublty
     C*
     C* Load license number
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   movel(p)  euflic        license#
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       license#
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
    C*
    C* Load Operating Physicians License Number
    C*
     C                   movel(p)  euolic        operlicense
    C*
    C*
     C*
     C* Load admission/Discharg Date/hour
     C*
     C                   z-add     euaddt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         admissdt
     C*
     C                   move      euadhr        admisshr
     C*
     C                   if        eudchr <> 99
     C                   move      eudchr        dischrghr
     C                   endif
     C*
     C* Load Date Statement Covers from/to
     C*
     C                   z-add     euscpf        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         stmtcovfrmdt
     C*
     C                   z-add     euscpt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         stmtcovtomdt
     C*
     C* Load Diagnosis Codes
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   eval      icd91 = getDiag(eudia1)
     C                   eval      icd92 = getDiag(eudia2)
     C                   eval      icd93 = getDiag(eudia3)
     C                   eval      icd94 = getDiag(eudia4)
    C*
    C*
     C                   eval      icd95 = getDiag(eudia5)
     C                   eval      icd96 = getDiag(eudia6)
     C                   eval      icd97 = getDiag(eudia7)
     C                   eval      icd98 = getDiag(eudia8)
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   eval      icd91 = *Blanks
     C                   eval      icd92 = *Blanks
     C                   eval      icd93 = *Blanks
     C                   eval      icd94 = *Blanks
    C*
     C                   eval      icd95 = *Blanks
     C                   eval      icd96 = *Blanks
     C                   eval      icd97 = *Blanks
     C                   eval      icd98 = *Blanks
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
    C* Load External Cause of Injury
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
    C*
     C                   eval      extinjcd1 = getDiag(eucau1)
     C                   eval      extinjcd2 = getDiag(eucau2)
     C                   eval      extinjcd3 = getDiag(eucau3)
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   eval      extinjcd1 = *Blanks
     C                   eval      extinjcd2 = *Blanks
     C                   eval      extinjcd3 = *Blanks
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
    C*
     C*
     C* Load Procedure Code Info
     C*
     C*
     C                   if        euprc1 <> *blanks
     C                   eval      procmth = eupcm
     C                   if        procmth <> '9'
     C                   eval      princprcd = euprc1
     C                   else
     C                   eval      princprcd = getdiag(euprc1)
     C                   endif
     C                   endif
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
     C                   eval      princprcd = princprcd
     C                   Else
     C                   eval      princprcd = *blanks
     C                   endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C                   if        euprc2 <> *blanks
     C                   if        procmth <> '9'
     C                   eval      otherprcd = euprc2
    C*
    C*
     C                   eval      otherprcd_b = euprc3
     C                   eval      otherprcd_c = euprc4
     C                   eval      otherprcd_d = euprc5
     C                   eval      otherprcd_e = euprc6
    C*
    C*
     C                   else
     C                   eval      otherprcd = getdiag(euprc2)
    C*
    C*
     C                   eval      otherprcd_b = getDiag(euprc3)
     C                   eval      otherprcd_c = getDiag(euprc4)
     C                   eval      otherprcd_d = getDiag(euprc5)
     C                   eval      otherprcd_e = getDiag(euprc6)
    C*
    C*
     C                   endif
     C                   endif
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
     C                   eval      otherprcd =  otherprcd
    C*
    C*
     C                   eval      otherprcd_b = otherprcd_b
     C                   eval      otherprcd_c = otherprcd_c
     C                   eval      otherprcd_d = otherprcd_d
     C                   eval      otherprcd_e = otherprcd_e
     C                   else
     C                   eval      otherprcd =  *Blanks
    C*
     C                   eval      otherprcd_b = *Blanks
     C                   eval      otherprcd_c = *Blanks
     C                   eval      otherprcd_d = *Blanks
     C                   eval      otherprcd_e = *Blanks
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
     C* Load Type of Admission
     C*
     C                   eval      typeadm = euadty
     C*
     C* Load Payment Plan
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        eoepmt <> 'Y'
     C                   move      euppln        payplan
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       payplan
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
    C* Load Scheduled/Unscheduled Indicator
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
    C*
     C                   eval      schdunschdind = euschd
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       schdunschdind
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
    C* Load Implant Total Paid
    C*
    C*** BEG CHG ***** 05/07/07 **************************************
     C*****************  exsr      getdwc10pmts
     C                   exsr      getdwc90imtp
    C*** BEG CHG ***** 05/07/07 **************************************
    C*
     C*
     C* Update the EDI flag to indicate the EOB has been sent to the state
     C***** BEG ADD ** 09/27/07 ***********************************************
     C* unless it was Withdrawn.
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/07 ***********************************************
     C*
     C                   eval      euedi = 'S'
     C*
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   endif
     C*
     C***** END ADD ** 09/27/07 ***********************************************
     C                   eval      euuser = q1user
     C                   eval      eupgm  = q1pgm
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eudt
     C                   move      dstime        eutime
     C*
     C                   update    peu01
     C*
     C                   endif
     C*
    C* Load Header info for Pharmacy Bills
     C*
     C                   when      eobtyp = 'X'
     C*
     C     hdky          chain     pep01                              99
     C*
     C                   if        not *in99
     C*
     C* Load Form id
     C*
     C                   move      '10'          formid
     C*
     C* Load Statement Date
     C*
     C                   z-add     eocsdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         statemntdt
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        eoepmt <> 'Y'
     C                   move      epppln        payplan
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       payplan
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
    C* Pharmacists license number
    C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        epplic <> *blanks
     C                   eval      license# = epplic
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       license#
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
    C*
     C* Load Payments
     C*
     C                   exsr      getdwc10pmts
     C*
     C* Update the EDI flag to indicate the EOB has been sent to the state
     C*
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/07 ***********************************************
     C                   eval      epedi = 'S'
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/07 ***********************************************
     C*
     C                   eval      epuser = q1user
     C                   eval      eppgm  = q1pgm
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         epdt
     C                   move      dstime        eptime
     C*
     C                   update    pep01
     C*
     C                   endif
     C*
    C* Load Header info for Dental Bills
     C*
     C                   when      eobtyp = 'D'
     C*
     C* Load Form id
     C*
     C                   move      '11'          formid
     C*
     C     hdky          chain     ped01                              99
     C*
     C* Load Place of treatment
     C*
     C                   if        not *in99
     C*
     C                   eval      placetmt = edpotx
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   movel(P)  edflic        license#
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   movel     *Blanks       license#
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C* Load Payment Plan
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   if        eoepmt <> 'Y'
     C*                  eval      payplan = edppln
     C                   eval      paycode = edppln
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Else
     C                   eval      payplan = *Blanks
     C                   eval      paycode = *Blanks
     C                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C* Update the EDI flag to indicate the EOB has been sent to the state
     C***** BEG ADD ** 09/27/07 ***********************************************
     C* unless it was Withdrawn.
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/07 ***********************************************
     C*
     C                   eval      ededi = 'S'
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/07 ***********************************************
     C*
     C                   eval      eduser = q1user
     C                   eval      edpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eddt
     C                   move      dstime        edtime
     C*
     C                   update    ped01
     C*
     C                   endif
     C*
     C                   endsl
     C*
    C*** BEG ADD *** 03/10/06 *********************************************
     C*
     C* Do not send payment plan if this is an employee payment
     C*
     C                   if        xxemplppind = 'E'
     C                   eval      payplan = ' '
     C                   endif
    C*** END ADD *** 03/10/06 *********************************************
     C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE getdwc10pmts - Loads header info for pharmacy bills
     C*=====================================================================
     C     getdwc10pmts  begsr
     C*
     C* Initialize
     C*
     C                   move      *zeros        totpay
     C                   move      *zeros        totpaysupp
     C*
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdp01                              99
     C*
     C* Accumulate capaid
     C*
     C                   if        dprxpt = 'S'  and *in99 = '0'
     C                   eval      totpaysupp = totpaysupp + capaid
     C                   else
     C                   eval      totpay     = totpay     + capaid
     C                   endif
     C*
     C     caky          reade     pca01                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C*** BEG ADD ***** 05/07/07 **************************************
     C*=====================================================================
     C* SUBROUTINE getdwc90imtp - Loads header info for pharmacy bills
     C*=====================================================================
     C     getdwc90imtp  begsr
     C*
     C* Initialize
     C*
     C                   move      *zeros        imptotpay
     C*
     C     caky          setll     pdu01                                  99
     C     caky          reade     pdu01                                  99
     C*
     C                   dow       not *in99
     C*
     C                   if        durvcd = 278
     C*
     C                   eval      kkdseq = dudseq
     C*
     C     dtky          chain     pca01                              99
     C*
     C* Accumulate capaid
     C*
     C                   eval      imptotpay = imptotpay + capaid
     C*
     C                   endif
     C*
     C     caky          reade     pdu01                                  99
     C*
     C                   enddo
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
     C*
    C*** Beg Chg ***** 11/28/07 **************************************
     C****************** Eval      imptotpay = imptotpay
     C                   Eval      xxtotpay  = imptotpay * 100
    C*** End Chg ***** 11/28/07 **************************************
     C                   Else
    C*** Beg Chg ***** 11/28/07 **************************************
     C****************** Eval      imptotpay  = *zeros
     C                   Eval      xxtotpay   = *zeros
    C*** End Chg ***** 11/28/07 **************************************
     C*
    c                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C                   endsr
    C*** END ADD ***** 05/07/07 **************************************
     C*=====================================================================
     C* SUBROUTINE loddtl09 - Load Physician Detail
     C*=====================================================================
     C     loddtl09      begsr
     C*
     C* Initialize
     C*
     C                   move      *zeros        detseq#
     C                   move      *zeros        placesrv
     C                   move      *blanks       icd9ref
     C                   move      *zeros        billedproc
     C                   move      *blanks       billedmod
    C*** BEG ADD ***** 05/07/07 **************************************
     C                   move      *blanks       billedmod1
     C                   move      *blanks       billedmod2
     C                   move      *blanks       billedmod3
     C                   move      *blanks       billedmod4
     C                   move      *blanks       paidmod1
     C                   move      *blanks       paidmod2
     C                   move      *blanks       paidmod3
     C                   move      *blanks       paidmod4
    C*** END ADD ***** 05/07/07 **************************************
    C*** BEG ADD ***** 05/14/07 **************************************
     C                   move      *blanks       @XXNDC
    C*** END ADD ***** 05/14/07 **************************************
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
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdh01                              99
     C*
     C* Load from the database
     C*
     C                   move      '2'           recflg
      *** BEG CHG *** 08/08/07 *******************************************
     C**********         move      calin#        detseq#
     C                   move      camseq        detseq#
      *** END CHG *** 08/08/07 *******************************************
     C                   move      dhpos         placesrv
     C*
     C* Load the diagnosis cross references
     C*
     C                   exsr      loddiagxref
     C*
     C* Load the procedure codes and modifiers
     C*
     C                   move      dhbprc        billedproc
     C*
     C                   if        dhmod1 <> *blanks
     C                   move      dhmod1        billedmod
     C                   else
     C                   if        dhmod2 <> *blanks
     C                   move      dhmod2        billedmod
     C                   else
     C                   if        dhmod3 <> *blanks
     C                   move      dhmod3        billedmod
     C                   else
     C                   if        dhmod4 <> *blanks
     C                   move      dhmod4        billedmod
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   if        dhmod1 <> *blanks
     C                   move      dhmod1        billedmod1
     C                   move      dhmod1        paidmod1
     C                   endif
     C*
     C                   if        dhmod2 <> *blanks
     C                   move      dhmod2        billedmod2
     C                   move      dhmod2        paidmod2
     C                   endif
     C*
     C                   if        dhmod3 <> *blanks
     C                   move      dhmod3        billedmod3
     C                   move      dhmod3        paidmod3
     C                   endif
     C*
     C                   if        dhmod4 <> *blanks
     C                   move      dhmod4        billedmod4
     C                   move      dhmod4        paidmod4
     C                   endif
     C*
     C*
     C                   move      dhprcd        paidproc
     C                   move      billedmod     paidmod
      *** BEG CHG *** 08/08/07 *******************************************
     C************       move      cachrg        detchrg
     C                   move      @@chrg        detchrg
      *** END CHG *** 08/08/07 *******************************************
      *** BEG CHG *** 09/20/07 *******************************************
     C***********        z-add     dhunts        units
     C                   Eval      @units = @units + 1
     C                   Eval      units = @units
      *** END CHG *** 09/20/07 *******************************************
      *** BEG CHG *** 08/08/07 *******************************************
     C***********        move      capaid        detpaid
     C                   move      @@paid        detpaid
      *** END CHG *** 08/08/07 *******************************************
     C*
    C* Load Service Dates
    C*
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofsrvfrm
    C*
     C                   z-add     catdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofsrvto
    C*
     C                   eval      xxndc   = dhndc
    C*
     C                   if        xxndc  <> *blanks
    C*
     C                   eval      xxndcwk = %trim(dhndc)
     C                   eval      d1 = 0
     C                   eval      d2 = 0
     C     digibk        check     xxndcwk       d1                             only digits or blank
     C*
     C                   if        d1 = 0
     C     digits        check     xxndcwk       d1                             only digits
     C*
     C                   if        d1 > 0
     C     ' '           check     xxndcwk:d1    d2                             no blanks in middle
     C*
     C                   if        d2 = 0
     C                   eval      xxndc   = *zeros
     C                   eval      d2 = (%len(xxndcwk) + 1) - (d1 - 1)          start position
     C*
     C                   if        d2 > 0 or d2 <= %len(xxndcwk)
     C                   eval      %subst(xxndc:d2) = %trim(xxndcwk)            right just/zero fill
     C                   else
     C                   eval      xxndc = xxndcwk                              just move
     C                   endif                                                  d2 > 0 or d2 <= %len
     C*
     C                   endif                                                  d2 = 0
     C                   endif                                                  d1 > 0
     C                   endif                                                  d1 = 0
     C*
    C*** BEG ADD ***** 05/07/07 **************************************
     C*
     C                   eval      ndc5 = %subst(xxndc:1:5)
     C                   eval      hyphen1 = '-'
     C                   eval      ndc4 = %subst(xxndc:6:4)
     C                   eval      hyphen2 = '-'
     C                   eval      ndc2 = %subst(xxndc:10:2)
     C*
    C*** END ADD ***** 05/07/07 **************************************
     C*
     C                   endif                                                  dhndc <> ' '
    C*
    C* Load the EOB codes
    C*
     C                   exsr      lodeobcodes
    C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE loddtl10 - Load Pharmacy Detail
     C*=====================================================================
     C     loddtl10      begsr
     C*
     C* Initialize
     C*
     C                   move      *zeros        detseq#
     C                   z-add     *zeros        qtyofmed
     C                   z-add     *zeros        dayssupply
     C                   move      *blanks       xxndc
     C*** BEG ADD ***** 05/14/07 **************************************
     C                   move      *blanks       @xxndc
     C*** END ADD ***** 05/14/07 **************************************
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
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdp01                              99
     C*
     C* Load from the database
     C*
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
    C*
    C*
    C* Load Drugs/Equipment/Supplies Indicator
    C*
     C                   move      dprxpt        drugsuppind
    C*
    C*
     C*
    C* Load Quantity of Medication
    C*
     C                   z-add     dpunts        qtyofmed
     C*
    C* Load Number of Days Supply
    C*
     C                   z-add     dprxdy        dayssupply
     C*
    C* Load NDC code
    C*
     C                   eval      xxndc   = dpndc
     C                   if        xxndc <> *Blanks
     C                   eval      xxndcwk = %trim(dpndc)
     C                   eval      d1 = 0
     C                   eval      d2 = 0
     C     digibk        check     xxndcwk       d1                             only digits or blank
     C*
     C                   if        d1 = 0
     C     digits        check     xxndcwk       d1                             only digits
     C*
     C                   if        d1 > 0
     C     ' '           check     xxndcwk:d1    d2                             no blanks in middle
     C*
     C                   if        d2 = 0
     C                   eval      xxndc   = *zeros
     C                   eval      d2 = (%len(xxndcwk) + 1) - (d1 - 1)          start position
     C*
     C                   if        d2 > 0 or d2 <= %len(xxndcwk)
     C                   eval      %subst(xxndc:d2) = %trim(xxndcwk)            right just/zero fill
     C                   else
     C                   eval      xxndc = xxndcwk                              just move
     C                   endif                                                  d2 > 0 or d2 <= %len
     C*
     C                   endif                                                  d2 = 0
     C                   endif                                                  d1 > 0
     C                   endif                                                  d1 = 0
     C*
    C*** BEG ADD ***** 05/07/07 **************************************
     C*
     C                   eval      ndc5 = %subst(xxndc:1:5)
     C                   eval      hyphen1 = '-'
     C                   eval      ndc4 = %subst(xxndc:6:4)
     C                   eval      hyphen2 = '-'
     C                   eval      ndc2 = %subst(xxndc:10:2)
     C*
    C*** END ADD ***** 05/07/07 **************************************
     C*
     C                   endif                                                  xxndc <> *Blanks
     C*
    C* Load Pharmacy number
    C*
     C                   movel     dprx#         pharmacy#
     C*
    C* Load New or Refill
    C*
     C                   movel     dpnwrf        neworrefill
    C*
    C*
    C* Load Purchase Rental Indicator
    C*
     C                   move      dpprin        purchrent
    C*
     C*
    C* Load Date Filled
    C*
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         datefilled
    C*
    C* Load Dispensed as Written
    C*
     C                   move      dpcert        dispaswrit
    C*
    C*** END ADD ***** 03/01/07 **************************************
    C*
     C*
    C* Load Certification
    C*
     C                   movel     dpcert        certifictn
     C*
    C* Load Prescribers license
    C*
     C                   eval      prescbrlic = %trim(dplic#)
     C*
    C* Load Charges
    C*
     C                   move      cachrg        detchrg
     C*
    C* Load Amount paid
    C*
     C                   move      capaid        detpaid
     C*
    C*
    C* Load the EOB codes
    C*
     C                   exsr      lodeobcodes
    C*
    C*
    C* Load HCPCS Code(HCPCS)
    C*
     C                   move      dphcpc        hcpcs
    C*
    C*
    C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE loddtl11 - Load Dental Detail
     C*=====================================================================
     C     loddtl11      begsr
     C*
     C* Initialize
     C*
     C                   move      *zeros        detseq#
     C                   move      *zeros        billedproc
     C                   move      *zeros        paidproc
     C                   move      *zeros        detchrg
     C                   move      *zeros        detpaid
     C                   move      *zeros        dateofsrvfrm
     C                   move      *blanks       eobcode1
     C                   move      *blanks       eobcode2
     C                   move      *blanks       eobcode3
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdd01                              99
     C*
     C* Load from the database
     C*
     C                   move      '2'           recflg
      *** BEG CHG *** 08/08/07 *******************************************
     C***********        move      calin#        detseq#
      *** BEG CHG *** 09/20/07 *******************************************
     C***********        move      camseq        detseq#
     C                   move      calin#        detseq#
      *** END CHG *** 09/20/07 *******************************************
      *** END CHG *** 08/08/07 *******************************************
     C*
     C* Load the procedure codes
     C*
     C                   move      ddbprc        billedproc
     C*
     C                   move      ddprcd        paidproc
      *** BEG CHG *** 08/08/07 *******************************************
     C*************      move      cachrg        detchrg
     C*************      move      capaid        detpaid
      *** BEG CHG *** 09/20/07 *******************************************
     C*************      move      @@chrg        detchrg
     C*************      move      @@paid        detpaid
     C                   move      cachrg        detchrg
     C                   move      capaid        detpaid
      *** END CHG *** 09/20/07 *******************************************
      *** END CHG *** 08/08/07 *******************************************
     C*
    C* Load Service Dates
    C*
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofsrvfrm
    C*
    C* Load the EOB codes
    C*
     C                   exsr      lodeobcodes
     C*
     C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE loddtl90 - Load Hospital Detail
     C*=====================================================================
     C     loddtl90      begsr
     C*
     C* Initialize
     C*
     C                   move      *zeros        detseq#
     C                   move      *zeros        revcode
     C                   move      *zeros        billedproc
     C                   move      *zeros        dateofopsrv
     C                   move      *blanks       billedmod
     C                   move      *zeros        detchrg
     C                   move      *zeros        units
     C                   move      *blanks       eobcode1
     C                   move      *blanks       eobcode2
     C                   move      *blanks       eobcode3
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdu01                              99
     C*
     C* Load from the database
     C*
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
     C*
     C* Load the procedure/rev codes and modifiers
     C*
     C                   move      dubprc        revcode
     C                   move      dubrat        billedproc
     C                   move      dupmod        billedmod
     C*
     C* Load the amounts
     C*
     C                   move      cachrg        detchrg
     C                   z-add     duunts        units90
    C*
    C* Load the EOB codes
    C*
     C                   exsr      lodeobcodes
    C*
    C* Load the date of service for outpatient bills
    C*
     C                   if        %subst(eublty:1:2) = '13'
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         dateofopsrv
     C                   endif
    C*
     C                   endsr
     C*
    C****************************************************************
    C* LODDIAGXREF - Subroutine to load diagnosis cross references
    C****************************************************************
    C*
     C     lodDiagxref   begsr
     C*
     C* Load the 1st cross reference in the leftmost blank position
     C*
     C                   if        dhdxr1 = 'X'
     C                   move      '1'           icd9ref1
     C                   endif
     C*
     C* Load the 2nd cross reference in the leftmost blank position
     C*
     C                   if        dhdxr2 = 'X'
     C*
     C                   if        icd9ref1 <> ' '
     C                   move      '2'           icd9ref2
     C                   else
     C                   move      '2'           icd9ref1
     C                   endif
     C*
     C                   endif
     C*
     C* Load the 3th cross reference in the leftmost blank position
     C*
     C                   if        dhdxr3 = 'X'
     C*
     C                   if        icd9ref1 <> ' '
     C*
     C                   if        icd9ref2 <> ' '
     C                   move      '3'           icd9ref3
     C                   else
     C                   move      '3'           icd9ref2
     C                   endif
     C*
     C                   else
     C                   move      '3'           icd9ref1
     C                   endif
     C*
     C                   endif
     C*
     C* Load the 4th cross reference in the leftmost blank position
     C*
     C                   if        dhdxr4 = 'X'
     C*
     C                   if        icd9ref1 <> ' '
     C*
     C                   if        icd9ref2 <> ' '
     C*
     C                   if        icd9ref3 <> ' '
     C                   move      '4'           icd9ref4
     C                   else
     C                   move      '4'           icd9ref3
     C                   endif
     C*
     C                   else
     C                   move      '4'           icd9ref2
     C                   endif
     C*
     C                   else
     C                   move      '4'           icd9ref1
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C****************************************************************
    C* LODEOBCODES - Subroutine to load EOB codes
    C****************************************************************
    C*
     C     lodEOBcodes   begsr
     C*
     C                   clear                   ecnt
     C*
     C     dtky          chain     pde01                              99
     C*
     C                   dow       not *in99 and ecnt <= 4
     C*
     C* Select an EOBR code or note. For notes report S20 as the Code.
     C*
     C                   if        deeob <> *blanks and deprnt <> 'N'
     C                             and destat <> 'I'
     C*** BEG DEL *** 08/08/07 *********************************************
     C**********************       or deeob = *blanks and denot1 <> *blanks
     C**********************       and deprnt <> 'N' and destat <> 'I'
     C*** END DEL *** 08/08/07 *********************************************
     C*
     C                   if        deeob <> *blanks
     C                   eval      kkkey = deeob
     C                   else
     C*** BEG DEL *** 06/04/07 *********************************************
     C*****              eval      kkkey = 'S20'
     C*** END DEL *** 06/04/07 *********************************************
     C*** BEG ADD *** 06/04/07 *********************************************
     C*** END ADD *** 06/04/07 *********************************************
     C                   endif
     C*
     C* Use the state cross reference. If blank then use 20=Unique code
     C*
     C*** BEG ADD *** 04/23/07 *********************************************
     C                   if        eoast = 'FL'
     C*** END ADD *** 04/23/07 *********************************************
     C     ebky          chain     peb01                              99
     C*** BEG ADD *** 03/24/05 *********************************************
     C                   endif
     C*** END ADD *** 03/24/05 *********************************************
     C*
     C                   if        *in99 = *off and ebxref <> *blanks
     C*** BEG ADD *** 04/23/07 *********************************************
     C                             and eoast = 'FL'
     C*** END ADD *** 04/23/07 *********************************************
     C                   move      ebxref        xxeob
    C*** BEG DEL *** 05/07/07 *********************************************
     C**********         else
     C*** BEG DEL *** 08/08/07 *********************************************
     C*****************  move      '20'          xxeob
     C*** END DEL *** 08/08/07 *********************************************
    C*** END DEL *** 05/07/07 *********************************************
     C                   endif
     C*
     C                   if        eobcode1 = *blanks
     C                   move      xxeob         eobcode1
     C                   else
     C*
     C                   if        eobcode2 = *blanks
     C                   move      xxeob         eobcode2
     C                   else
     C*
     C                   if        eobcode3 = *blanks
     C                   move      xxeob         eobcode3
     C                   else
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   add       1             ecnt
     C*
     C                   endif
     C*
     C     dtky          reade     pde01                                  99
     C                   enddo
     C*
     C* Report code 07 when charges = payments
     C*
    C*** BEG DEL ***** 05/07/07 **************************************
     C*******            if        cachrg = capaid
     C*******
     C*******            select
     C*******
     C*******            when      eobcode1 = *blanks
     C*******            eval      eobcode1 = '07'
     C*******
     C*******            when      eobcode2 = *blanks
     C*******            eval      eobcode2 = '07'
     C*******
     C*******            when      eobcode3 = *blanks
     C*******            eval      eobcode3 = '07'
     C*******
     C*******            endsl
     C*******
     C*******            endif
    C*** END DEL ***** 05/07/07 **************************************
     C*
     C*** BEG ADD *** 06/04/07 *********************************************
     C*
     C                   if        eobcode1 = *blanks and
     C                             eobcode2 = *blanks and
     C                             eobcode3 = *blanks
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C                   if        eospcd = 'ASC' or
     C                             %subst(ehflic:1:3) = 'ASC'
     C*** BEG CHG *** 06/04/07 *********************************************
     C****************** eval      eobcode1 = 'D91'
     C                   eval      eobcode1 = '91'
     C*** END CHG *** 06/04/07 *********************************************
     C                   else
     C*** BEG CHG *** 06/04/07 *********************************************
     C*****************  eval      eobcode1 = 'D90'
     C                   eval      eobcode1 = '90'
     C*** END CHG *** 06/04/07 *********************************************
     C                   endif
     C*
     C                   when      eobtyp = 'H'
     C*** BEG CHG *** 06/04/07 *********************************************
     C****************   eval      eobcode1 = 'D92'
     C                   eval      eobcode1 = '92'
     C*** END CHG *** 06/04/07 *********************************************
     C*
     C                   when      eobtyp = 'D' or eobtyp = 'X'
     C*** BEG CHG *** 06/04/07 *********************************************
     C*****************  eval      eobcode1 = 'D90'
     C                   eval      eobcode1 = '90'
     C*** END CHG *** 06/04/07 *********************************************
     C*
     C                   endsl
     C*
     C                   endif
     C*
     C*** END ADD *** 06/04/07 *********************************************
     C*
     C                   endsr
    C****************************************************************
    C* LODTHDR - Subroutine to load transmission header record fields
    C****************************************************************
    C*
     C     lodthdr       begsr
    C*
     C                   move      *blanks       tranhdrmkr
     C                   move      '000'         submid
     C                   move      *blanks       submzip
     C                   move      *zeros        submtid
     C                   move      *blanks       formid
     C                   move      *blanks       revind
     C                   move      *zeros        submdt
    C*
     C                   move      'HD1'         tranhdrmkr
    C*
     C     c1ky          chain     pc101                              99
    C*
     C                   if        not *in99
     C                   move      c1subm        submid
     C                   move      c1zip         submzip
     C                   if        %subst(submzip:6:4) = '0000'
     C                   movel     c1zip         xxzip             5
     C                   movel(P)  xxzip         submzip
     C                   endif
     C                   move      c1fein        submtid
     C                   move      'D '          revind
     C                   move      qqtpin        tpin
     C                   endif
     C*
    C* Load form id
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C                   move      '09'          formid
     C*
     C                   when      eobtyp = 'X'
     C                   move      '10'          formid
     C*
     C                   when      eobtyp = 'D'
     C                   move      '11'          formid
     C*
     C                   when      eobtyp = 'H'
     C                   move      '90'          formid
     C*
     C                   endsl
     C*
    C* Load submitted date
    C*
     C                   time                    dsdttm
     C                   movel     dscy          submdt
     C                   move      dsmd          submdt
    C*
     C                   endsr
    C****************************************************************
    C* LODTTLR - Subroutine to load transmission trailer record fields
    C****************************************************************
    C*
     C     lodttlr       begsr
    C*
     C                   move      'TR1'         trantlrmkr
    C*
     C                   if        numclm09 > 0
     C                   except    tranTrl09
     C                   endif
    C*
     C                   if        numclm10 > 0
     C                   except    tranTrl10
     C                   endif
    C*
     C                   if        numclm11 > 0
     C                   except    tranTrl11
     C                   endif
    C*
     C                   if        numclm90 > 0
     C                   except    tranTrl90
     C                   endif
    C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE MEDSR - FORM DWC-9
     C*=====================================================================
     C     medsr         begsr
     C*
     C* First time through write the transmission header info initialize
     C* claim counter for DWC9 claims
     C*
     C                   if        first09 = 'Y'
     C                   eval      numclm09 = 0
     C                   exsr      lodthdr
     C                   except    tranHdr09
     C                   eval      first09 = *blank
     C                   endif
     C*
     C* Sum up the number of claims in the transmission for the trailer
     C*
     C                   add       1             numclm09
     C*
     C* Load header info for the current EOB
     C*
     C                   exsr      lodbhdr
     C*
     C                   except    billHdr09
     C*
      *** BEG CHG *** 08/08/07 *******************************************
     C*****caky          chain     pca01                              99
     C     caky          chain     pca28                              99
      *** END CHG *** 08/08/07 *******************************************
     C*
     C                   dow       not *in99
      *** BEG ADD *** 08/08/07 *******************************************
     C                   Eval      kkmseq = camseq
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
      *** END ADD *** 08/08/07 *******************************************
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   exsr      loddtl09
     C                   except    billDtl09
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
      *** BEG CHG *** 08/08/07 *******************************************
     C*****caky          reade     pca01                                  99
     C     caky1         setgt     pca28
     C     caky          reade     pca28                                  99
      *** END CHG *** 08/08/07 *******************************************
     C*
     C                   enddo
      *** BEG ADD *** 09/20/07 *******************************************
     C                   RESET                   @units
      *** END ADD *** 09/20/07 *******************************************
     C*
     C                   endsr
    C*
     C*=====================================================================
    C* SUBROUTINE UPDDBR
     C*=====================================================================
     C     upddbr        begsr
    C*
    C*  Update control number counter file
    C*
     C                   movel     yy            juldat
     C                   move      jjj           juldat
    C*
     C     tskey         chain     pts01                              99
    C*
     C                   if        *in99 = *off
     C                   eval      tsseq# = cntr
     C                   move      juldat        tsrndt
     C                   update    pts01
    C*
     C                   else
     C                   eval      tsco#  = eoco#
     C                   eval      tsseq# = cntr
     C                   move      juldat        tsrndt
     C                   write     pts01
    C*
     C                   endif
    C*
     C                   endsr
    C*
     C*=====================================================================
    C* SUBROUTINE PGMEND
     C*=====================================================================
     C     pgmend        begsr
     C*
     C* Write the trailer record(s)
     C*
     C                   exsr      lodttlr
     C*
     C                   if        cntr > 0
     C                   exsr      upddbr
     C                   endif
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain orkey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   eval      *inlr = *on
     C                   return
     C*
     C                   endsr
     C*
     C*** BEG ADD ***** 01/10/07 ****************************************
     C****************************************************************
     C* GETMDT - Subroutine to retrieve the check mailing date       *
     C****************************************************************
     C*
     C     GETMDT        BEGSR
     C*
     C* USE THE CHECK MAILING DATE IF NOT BLANK
     C*
     C     @@WCIF        IFEQ      'Y'
     C     @@WCIN        ANDEQ     'Y'
     C     @@NPWC        ANDNE     'Y'
     C*
     C                   MOVEL     KKSUB#        ##SUB#            5
     C                   MOVEL     KKFND         #1FND             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      KKCO#         #1CO#             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C* GET WCMS FUND NUMBER
     C*
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C*
     C                   Z-ADD     EOWCCK        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXWCCK            8 0
     C*
     C                   MOVE      XXWCCK        #1TRDT
     C*
     C                   Z-ADD     EOWCCK        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXWCCK
     C
     C     SRERR         IFEQ      '1'
     C                   ELSE
     C*
     C*
     C                   CALL      'WA050R'
     C                   PARM                    #1CO#
     C                   PARM      ##WCF#        #1FND
     C                   PARM                    #1TRDT            8
     C                   PARM      *BLANKS       #1CMDT            8
     C*
     C*
     C                   ENDIF                                                  SRERR
     C*
     C                   ENDIF                                                  ##OUTS=N
     C*
     C                   ENDIF                                                  @@WCIF
     C*
     C                   ENDSR
    C*
     C*=====================================================================
    C* SUBROUTINE PGMSTR
     C*=====================================================================
     C     pgmstr        begsr
    C*
    C* Set-up and Initialize entry fields, convert dates, etc.
    C*
     C                   move      qqco#         kkco#
     C                   move      qqsub#        kksub#
    C*
     C                   move      qqfrdt        xqfrdt
     C                   move      qqtodt        xqtodt
    C*
     C                   eval      srdti = xqfrdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxfrdt
    C*
     C                   eval      srdti = xqtodt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtodt
    C*
     C                   eval      srdti = xxfrdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxfrom
    C*
     C                   eval      srdti = xxtodt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxto
    C*
     C                   eval      cntr = 0
    C*
     C                   time                    dsdttm
    C*
     C                   eval      first09 = 'Y'
     C                   eval      first10 = 'Y'
     C                   eval      first11 = 'Y'
     C                   eval      first90 = 'Y'
    C*
     C                   select
    C*
     C                   when      qqproc = 'P'
     C                   movel     'PHYSI'       xxproc
     C                   move      'CIAN'        xxproc
     C                   eval      *in90 = *on
    C*
     C                   when      qqproc = 'H'
     C                   movel     'HOSPI'       xxproc
     C                   move      'TAL '        xxproc
     C                   eval      *in91 = *on
    C*
     C                   when      qqproc = 'D'
     C                   movel     'DENTA'       xxproc
     C                   move      'L   '        xxproc
     C                   eval      *in92 = *on
    C*
     C                   endsl
    C*
    C* Get the current date
    C*
     C                   move      *date         sys_date
    C*
    C* Get the Julian Date
    C*
     C                   eval      xxdt = dsdate
     C                   eval      zzmm = 12
     C                   eval      zzdd = 31
     C                   if        xxyy <> 0
     C                   eval      zzyy = xxyy - 1
     C                   else
     C                   eval      zzyy = 99
     C                   endif
    C*
    C* Call sub-program SRDDIF8 to Calc Julian Date
    C*
     C                   move      zzdt          srfrdc
     C                   move      xxdt          srtodc
    C*
     C                   eval      srdays = 0
     C                   move      ' '           srerr
     C                   move      ' '           srterm
     C                   call      'SRDDIF8'
     C                   parm                    srfrdc
     C                   parm                    srtodc
     C                   parm                    srdays
     C                   parm                    srerr
     C                   parm                    srterm
    C*
    C*
    C*  GET NEXT AVAILABLE SEQUENCE # TO BE USED IN CONTROL RECORD
    C*
     C     tskey         chain     pts01                              80
     C                   if        *in80 = *on
     C                   eval      cntr = 0
     C                   else
     C                   move      tsseq#        cntr
     C                   endif
    C*
    C*  GET COMPANY NAME
    C*
     C     cokey         chain     pco01                              60
     C                   if        *in60 = *on
     C                   clear                   xxconm
     C                   else
     C                   movel     coname        xxconm
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
     C                   endif
    C*
    C*  GET COMPANY AUXILIARY RECORD FOR AUDIT NUMBERS
    C*  IF NON EXIST - PRINT ERROR MESSAGE AND END PROGRAM
    C*
     C     c1ky          chain     pc101                              60
     C                   if        *in60 = *on
     C                   except    errms1
     C                   exsr      pgmend
     C                   endif
    C*
    C*  GET SUBCRIBER NAME
    C*
     C     smkey         chain     psm01                              60
     C                   if        *in60 = *on
     C                   clear                   xxsbnm
     C                   else
     C                   movel     smname        xxsbnm
     C                   endif
    C*
    C* Get the Routing Information and Print Routing Page
    C*
     C                   move      '0'           xxprt
     C     orkey         chain     por01                              61
     C                   if        *in61 = *off
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
    C*
     C                   move      *off          *in99
     C*
     C* Retrieve client id
     C*
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
     C*
     C*  Check to see if WCMS is installed
     C*
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
     C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE PHARSR
     C*=====================================================================
     C     pharsr        begsr
     C*
     C*
     C* First time through write the transmission header info initialize
     C* claim counter for DWC10 claims
     C*
     C                   if        first10 = 'Y'
     C                   eval      numclm10 = 0
     C                   exsr      lodthdr
     C                   except    tranHdr10
     C                   eval      first10 = *blank
     C                   endif
     C*
     C* Sum up the number of claims in the transmission for the trailer
     C*
     C                   add       1             numclm10
     C*
     C* Load header info for the current EOB
     C*
     C                   exsr      lodbhdr
     C                   except    billHdr10
     C*
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   If        emppayment = *off
    C*** END ADD ***** 08/03/07 **************************************
     C                   exsr      loddtl10
     C                   if        skpdtl10 = 'N'
     C                   except    billDtl10
     C                   endif
    C*** BEG ADD ***** 08/03/07 **************************************
    c                   Endif
    C*** END ADD ***** 08/03/07 **************************************
     C*
     C     caky          reade     pca01                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C*
    C*
     C*=====================================================================
     C* SUBROUTINE splitphar - Split out Drugs and Supplies
     C*=====================================================================
     C     splitphar     begsr
     C*
     C                   eval      totpay = 0
     C                   eval      totpaysupp = 0
     C*
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdp01                              99
     C*
     C                   eval      kkndc = dpndc
     C     rxky          chain     prx01                              99
     C*
     C                   if        *in99 = *off
     C                   eval      totpay = totpay + capaid
     C*
     C                   else
     C                   eval      totpaysupp = totpaysupp + capaid
     C*
     C                   endif
     C*
     C     caky          reade     pca01                                  99
     C*
     C                   enddo
     C                   endsr
    C*
     C*=====================================================================
     C* SUBROUTINE updathdr - Update the Bill Header Files
     C*=====================================================================
     C     updathdr      begsr
     C*
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/07 ***********************************************
     C                   eval      eotape = 'S'
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/07 ***********************************************
    C*
     C                   eval      eopgm  = q1user
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eodt
     C                   move      dstime        eotime
    C*
     C                   if        eosbdt = *blanks
     C     *ymd          move      sys_date      eosbdt
     C                   endif
    C*
     C                   if        ##clt# = '051'
     C                   update    peo02
     C                   else
     C                   update    peo01
     C                   endif
    C*
     C     eaky          chain     pea01                              99
    C
     C                   if        not *in99
    C*
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/07 ***********************************************
     C                   eval      eaedi  = 'S'
     C***** BEG ADD ** 09/27/07 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/07 ***********************************************
     C                   eval      eacntl = eocntl
     C*
     C                   time                    dsdttm
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         eaesdt
     C*
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C*
     C                   movel     dstime        eatime
     C                   eval      eapgm  = q1user
     C*
     C                   update    pea01
     C*
     C                   endif
    C*
     C                   endsr
     O*=====================================================================
     O* DWC09 Output File
     O*=====================================================================
     O* DWC09 Transmission Header
     O*
     Ofeppfl    eadd         tranHdr09
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill263            300
     O* DWC09 Bill Header
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
     O                       icd91              204
     O                       icd92              212
     O                       icd93              220
     O                       icd94              228
     O                       insintfile#        258
     O                       subloc             278
     O                       xxemplppind        279
     O                       xxdupovrind        280
     O                       fill09head         300
     O* DWC09 Bill Detail
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
    O*** BEG CHG **** 05/07/07 ***************************************
     O*******                xxndc              103
     O                       @xxndc             103
    O*** END CHG **** 05/07/07 ***************************************
     O                       eobcode1           105
     O                       eobcode2           107
     O                       eobcode3           109
     O                       fill09det          300
     O* DWC09 Transmission Trailer
     O          eadd         tranTrl09
     O                       trantlrmkr           3
     O                       numclm09             9
     O                       fill291            300
     O*=====================================================================
     O* DWC10 Output File
     O*=====================================================================
     O* DWC10 Transmission Header
     O*
     Ofexpfl    eadd         tranHdr10
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill10trhdr        300
     O* DWC10 Bill Header
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
     O                       fill10head         300
     O* DWC10 Bill Detail
     O          eadd         billDtl10
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       drugsuppind         18
     O                       qtyofmed            23
     O                       dayssupply          26
    O*** BEG CHG **** 05/07/07 ***************************************
     O*******                xxndc               39
     O                       @xxndc              39
    O*** END CHG **** 05/07/07 ***************************************
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
     O                       fill10det          300
     O* DWC10 Transmission Trailer
     O          eadd         tranTrl10
     O                       trantlrmkr           3
     O                       numclm10             9
     O                       fill10trtrl        300
     O*=====================================================================
     O* DWC11 Output File
     O*=====================================================================
     O* DWC11 Transmission Header
     O*
     Ofedpfl    eadd         tranHdr11
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill263            300
     O* DWC11 Bill Header
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
     O                       fill11head         300
     O* DWC11 Bill Detail
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
     O* DWC11 Transmission Trailer
     O          eadd         tranTrl11
     O                       trantlrmkr           3
     O                       numclm11             9
     O                       fill291            300
     O*=====================================================================
     O* DWC90 Output File
     O*=====================================================================
     O* DWC90 Transmission Header
     O*
     Ofehpfl    eadd         tranHdr90
     O                       tranhdrmkr           3
     O                       submid               6
     O                       submzip             15
     O                       submtid             24
     O                       formid              26
     O                       revind              28
     O                       tpin                29
     O                       submdt              37
     O                       fill90trhdr        450
     O* DWC90 Bill Header
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
     O                       icd91              244
     O                       icd92              252
     O                       icd93              260
     O                       icd94              268
     O                       icd95              276
     O                       icd96              284
     O                       icd97              292
     O                       icd98              300
     O                       extinjcd1          308
     O                       extinjcd2          316
     O                       extinjcd3          324
     O                       princprcd          329
     O                       otherprcd          334
     O*                      reportreas         254
     O                       otherprcd_b        339
     O                       otherprcd_c        344
     O                       otherprcd_d        349
     O                       otherprcd_e        354
     O                       reportreas         356
     O                       payplan            358
     O                       typeadm            359
     O                       insintfile#        389
     O                       subloc             409
     O                       xxemplppind        410
     O                       xxdupovrind        411
     O                       schdunschdind      412
    C*** Beg Chg ***** 11/28/07 **************************************
    O*** BEG CHG ***** 08/03/07 **************************************
     O**********             imptotpay          423
     O**********             imptotpay     Z    423
     O                       xxtotpay           423
    O*** END CHG ***** 08/03/07 **************************************
    C*** End Chg ***** 11/28/07 **************************************
     O                       fill90head         450
     O* DWC90 Bill Detail
     O          eadd         billDtl90
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       revcode             21
     O                       billedproc          26
     O                       billedmod           28
     O                       units90             35
     O                       detchrg             46
     O                       eobcode1            48
     O                       eobcode2            50
     O                       eobcode3            52
     O                       dateofopsrv         60
     O                       fill90dtl          450
     O* DWC90 Transmission Trailer
     O          eadd         tranTrl90
     O                       trantlrmkr           3
     O                       numclm90             9
     O                       fill90trtrl        450
    O*
     Oqsysprt   e            route          1 06
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
     O**********             xxfnnm              60
     O**********e            route          1
     O**********             ordis1              60
     O**********e            route          1
     O**********             ordis2              60
     O**********e            route          1
     O**********             ordis3              60
     O**********e            route          2
     O**********             ordis4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O                       xxfnnm              60
     O          e            route          1
     O                       ordis1              60
     O          e            route          1
     O                       ordis2              60
     O          e            route          1
     O                       ordis3              60
     O          e            route          2
     O                       ordis4              60
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********e            route          2
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
     O**********             oroutq              30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 ************************************
     O*
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
    O****************
     O*** BEGIN CHG ***  01/28/10  ************************************
     O****      e            route            60
     O***                                           '(C) COPYRIGHT PROGRESSIV'
     O***                                      +  0 'E DATA SOLUTIONS, INC.'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
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
     P*=====================================================================
     P* Procedure getDiag - Returns diagnosis code with decimal
     P*=====================================================================
     P
     P getDiag         B
     D getDiag         PI             7
     D  icd9                          5
     C*
     C                   eval      kkdiag = icd9
     C     diky          chain     pdi01                              99
     C                   if        not *in99
     C                   return    dids1
     C                   else
     C                   return    *blanks
     C                   endif
     C
     P getDiag         E
