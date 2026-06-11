    H**********************************************************************
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H*    PROGRAM: F9711RFL
    H*    DESCRIPTION: Create Florida Medical EDI workfile and report
    H**********************************************************************
    H*    PROGRAMMER NOTES: THE CONTROL# CNTL IS MADE UP OF
    H*                      CCCYYJJJNNNNN
    H*                      CCC=Submitter#, YY=Year, JJJ=Julian Date
    H*                      NNNNN=The calculated sequence number
    H**********************************************************************
    H*    CALLED FROM:
    H*      F9711CFL - CREATE WORKFILE
    H*    CALLS TO:
    H*      F9716CFL - CONVERT NAMES TO ALL UPPERCASE
    H*****************************************************************
    H** CREATED: 04/15/04 C: 79 PROJ: FL046
    H**********************************************************************
    H** REVISIONS:
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 FTEBLFL to be used only if the adjudication state (FAEOP/EOAST = FL).
     H**
     H** 01/10/07  C:182  Project: MC275
     H**                  Check Mailing date
     H**
     H** 09/27/06  C:181  Project: 10MJE
     H**                  Add edit to check status of FL EDI
     H**                  extract to prevent deletion when EOAST = 'FL'
     H**                  and EOTAPE <> ' ' or 'E' or 'W', and display
     H**                  an error message. Also restrict change of fields
     H**                  EHEDI, EUEDI, EPEDI, and EDEDI, to 'S' to only
     H**                  when EOTAPE <> 'W'.
     H**
     H** 01/25/06  C:079  Support: 50887
     H**                  Temporarily remove call to checkrecon.  It was
     H**                  apparently looping forever
     H**
     H** 01/08/06  C:163  Project: 10MIJ
     H**                  Strip Middle Initial-FL EDI
     H**
     H** 01/10/06  C:163  Project: MC270
     H**                  Rx New field - "Medical Supplies"
     H**
     H** 01/09/06  C:079  Project: FL052
     H**                  On employee payments dont send payment plan
     H**
     H** 01/04/06  C:163  Project: 10MIE
     H**                  FL EDI Employee Payments
     H**
     H** 11/11/05  C:163  Project: FL052
     H**                  FL EDI Changes 2005
     H**
     H** 07/26/05  C:079  Project: 135MER
     H**                  Blank fill service co/tpa Taxid and zip
     H**
     H** 03/24/05  C:079  Support: MC048 46015
     H**                  If a line item only contains notes report S20
     H**
     H** 03/07/05  C:079  Support: MC010 45991
     H**                  Allow bypassing of EOB if previously accepted
     H**
     H** 02/22/05  C:079  Support: FLMNT
     H**                  Report diag code if PCM = 9
     H**
     H** 02/05/05  C:079  Project: FL046
     H**                  Break out Drugs and Supplies
     H**
     H** 01/25/05  C:079  Support: MC048 45577
     H**                  Names in upper case
     H**
     H** 08/24/04  C:079  Support: MC010 43618
     H**                  Report billed fields for DWC90
    H**********************************************************************
    H* INDICATOR USAGE:
    H*
    H*  99   -   GENERAL CHAIN
    H*******************************************************************
     Ffeppfl    o  a f  300        disk
     Ffexpfl    o  a f  250        disk
     Ffehpfl    o  a f  350        disk
     Ffedpfl    o  a f  300        disk
     Ffaeol     uf   e             disk
     Fsmcol     if   e           k disk
     Ffmc1l     if   e           k disk
     Ffmf1l     if   e           k disk
     Ffdf2l     if   e           k disk
     Fsmorl     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffdcal5    if   e           k disk
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
     F*** BEG ADD *** 02/05/05 *********************************************
     Ffmrxl     if   e           k disk
     F*** END ADD *** 02/05/05 *********************************************
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
     D*** BEG ADD *** 11/11/05 *********************************************
     D certifictn      s              1
     D*** END ADD *** 11/11/05 *********************************************
     D billedproc      s              5
     D billedmod       s              2
     D billrcvdt       s              8
     D billpaydt       s              8
     D cntr            s              5  0
     D*** BEG ADD *** 11/11/05 *********************************************
     D datefilled      s              8
     D*** END ADD *** 11/11/05 *********************************************
     D dateofsrvfrm    s              8
     D dateofsrvto     s              8
     D dateofopsrv     s              8
     D*** BEG ADD *** 11/11/05 *********************************************
     D dayssupply      s              3S 0
     D*** END ADD *** 11/11/05 *********************************************
     D detchrg         s             11
     D detpaid         s             11
     D detseq#         s              3
     D*** BEG ADD *** 11/11/05 *********************************************
     D d1              s              2  0
     D d2              s              2  0
     D digits          c                   '0123456789'
     D digibk          c                   ' 0123456789'
     D*** END ADD *** 11/11/05 *********************************************
     D dischrghr       s              2
     D ecnt            s              1  0
     D emplid          s             10
     D eobcode1        s              2
     D eobcode2        s              2
     D eobcode3        s              2
     D*** BEG DEL *** 11/11/05 *********************************************
     D*eobcode4        s              2
     D*** END DEL *** 11/11/05 *********************************************
     D facilzip        s              9
     D fill2           s              2    inz(*blanks)
     D fill32          s             32    inz(*blanks)
     D*** BEG CHG *** 11/11/05 *********************************************
     D*fill49          s             49    inz(*blanks)
     D fill41          s             41    inz(*blanks)
     D*fill24          s             24    inz(*blanks)
     D fill20          s             20    inz(*blanks)
     D*fill36          s             36    inz(*blanks)
     D fill34          s             34    inz(*blanks)
     D*fill58          s             58    inz(*blanks)
     D fill56          s             56    inz(*blanks)
     D*** END CHG *** 11/11/05 *********************************************
     D fill166         s            166    inz(*blanks)
     D fill213         s            213    inz(*blanks)
     D*** BEG CHG *** 11/11/05 *********************************************
     D*fill216         s            216    inz(*blanks)
     D fill205         s            205    inz(*blanks)
     D*** END CHG *** 11/11/05 *********************************************
     D fill237         s            237    inz(*blanks)
     D fill241         s            241    inz(*blanks)
     D fill240         s            240    inz(*blanks)
     D fill263         s            263    inz(*blanks)
     D fill291         s            291    inz(*blanks)
     D fill313         s            313    inz(*blanks)
     D*** BEG ADD *** 11/11/05 *********************************************
     D fill153         s            153    inz(*blanks)
     D*** END ADD *** 11/11/05 *********************************************
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
     D insintfile#     s             30
     D juldat          s              5
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D kkdseq          s                   like(cadseq)
     D kkrcn           s                   like(eorcn)
     D kkkey           s                   like(stkey)
     D kkdiag          s                   like(diicd9)
     D kkfein          s                   like(eofein)
     D kkmod           s                   like(eomod)
     D kkss#           s                   like(eoss#)
     D kkesrc          s                   like(deesrc)
     D kkprt           s              1
     D*** BEG ADD *** 02/05/05 *********************************************
     D kkndc           s                   like(rxndc)
     D*** END ADD *** 02/05/05 *********************************************
     D lastnm          s             30
     D license#        s             13
     D lostTime        s              2
     D middin          s              1
     D*** BEG ADD *** 11/11/05 *********************************************
     D neworrefill     s              1
     D*** END ADD *** 11/11/05 *********************************************
     D numclm09        s              6  0
     D numclm10        s              6  0
     D numclm11        s              6  0
     D numclm90        s              6  0
     D otherprcd       s              5
     D paidproc        s              5
     D paidmod         s              2
     D payplan         s              2
     D*** BEG ADD *** 11/11/05 *********************************************
     D pharmacy#       s             10
     D*** END ADD *** 11/11/05 *********************************************
     D placesrv        s              2
     D placetmt        s              2
     D*** BEG ADD *** 11/11/05 *********************************************
     D prescbrlic      s             13
     D*** END ADD *** 11/11/05 *********************************************
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
     D*** BEG ADD *** 11/11/05 *********************************************
     D qtyofmed        s              5S 0
     D*** END ADD *** 11/11/05 *********************************************
     D recflg          s              1
     D reportreas      s              2
     D revind          s              2
     D revcode         s              4
     D servcocode      s              5
     D servcotaxid     s              9
     D servcozip       s              9
     D*** BEG ADD *** 01/10/06 *********************************************
     D skpdtl10        s              1
     D*** END ADD *** 01/10/06 *********************************************
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
     D
     D submid          s              3
     D submtid         s              9
     D submzip         s              9
     D totpay          s             11  2
     D*** BEG ADD *** 02/05/05 *********************************************
     D totpaysupp      s             11  2
     D*** END ADD *** 02/05/05 *********************************************
     D totchrg         s             11  2
     D tpin            s              1
     D tranhdrmkr      s              3
     D trantlrmkr      s              3
     D typeadm         s              1
     D typebill        s              3
     D units           s              3  0
     D units90         s              7  0
     D x               s              2  0
     D xqfrdt          s              8  0
     D xqtodt          s              8  0
     D xxconm          s             40
     D*** BEG ADD *** 11/11/05 *********************************************
     D xxdupovrind     s              1
     D xxemplppind     s              1
     D*** END ADD *** 11/11/05 *********************************************
     D xxeob           s              2
     D xxesrc          s                   like(deesrc)
     D xxfnnm          s             40
     D xxfrdt          s              7  0
     D xxfrom          s              8  0
     D xxkey           s                   like(pmkey)
     D*** BEG ADD *** 11/11/05 *********************************************
     D xxn             s              2  0
     D xxndc           s                   like(dhndc)
     D xxndcwk         s                   like(dhndc)
     D*** END ADD *** 11/11/05 *********************************************
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
     D*
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
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
     C*** BEG ADD *** 01/10/07 ****************************************
     C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*** END ADD *** 01/10/07 ****************************************
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
     C*** BEG ADD *** 02/05/05 *********************************************
     C     rxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkndc
     C*** END ADD *** 02/05/05 *********************************************
     C*=====================================================================
    C* MAIN PROGRAM BEGINS
     C*=====================================================================
    C*
     C                   exsr      pgmstr
    C*
    C*  BEGIN READING BATCH
    C*
     C                   read      peo01                                  99
    C*
     C                   if        *in99 = *on
     C                   except    errms2
     C                   exsr      pgmend
     C                   endif
    C*
     C                   dow       *in99 = *off
    C*
    C* If check date is < recon date then batch has not updated check
    C* date so don't include this EOB
    C*
     C*** BEG DEL *** 01/25/06 *********************************************
     C*****************  if        eotape = 'C'
     C*****************  exsr      checkRecon
    C*****************
     C*****************  if        eowcck < esrrdt
     C*****************  iter
     C*****************  endif
    C*****************
     C*****************  endif
     C*** END DEL *** 01/25/06 *********************************************
    C*
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
     C                   read      peo01                                  99
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
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
     C                   exsr      loddtl11
     C                   except    billDtl11
     C*
     C     caky          reade     pca01                                  99
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
     C                   eval      kkfyr  = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   eval      kkrcn  = eorcn
     C                   eval      kkss#  = eoss#
     C                   eval      kkfein = eofein
     C                   eval      kkmod  = eomod
    C*
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
     C                   exsr      loddtl90
     C                   except    billDtl90
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
     C                   move      *zeros        typeadm
     C                   move      *blanks       facilzip
     C                   move      *blanks       insintfile#
     C                   move      *blanks       subloc
     C*** BEG ADD *** 11/11/05 *********************************************
     C                   move      *blanks       xxemplppind
     C                   move      *blanks       xxdupovrind
     C*** END ADD *** 11/11/05 *********************************************
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
     C*** BEG CHG *** 11/11/05 *********************************************
     C                   when      eotape = 'R' and eocntl <> *blanks
     C                             or
     C                             eotape = 'D' and eocntl = *blanks
     C*** END CHG *** 11/11/05 *********************************************
     C                   eval      reportreas = '02'
     C*
     C                   when      eotape = 'C' and eocntl <> *blanks
     C                             and eorcn > 0
     C                   eval      reportreas = '03'
     C*** BEG ADD *** 03/07/05 *********************************************
     C                   when      eotape = 'B' and eocntl <> *blanks
     C                   eval      reportreas = '03'
     C*** END ADD *** 03/07/05 *********************************************
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
     C*** BEG ADD *** 11/11/05 *********************************************
     C*
     C* Duplicate Override Indicator
     C*
     C                   if        eotape = 'D'
     C                   eval      xxdupovrind = 'Y'
     C                   endif
     C*** END ADD *** 11/11/05 *********************************************
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
     C                   move      f25n01        insurcode
     C                   move      f29n01        insurtaxid
     C                   move      f29n02        insurzip
     C                   if        %subst(insurzip:6:4) = '0000'
     C                   movel     f29n02        xxzip             5
     C                   movel(P)  xxzip         insurzip
     C                   endif
    C*
    C* Load Service Co/TPA info
    C*
     C                   move      f25n02        servcocode
     C                   move      f29n03        servcotaxid
     C                   move      f29n04        servcozip
     C                   if        %subst(servcozip:6:4) = '0000'
     C                   movel     f29n04        xxzip             5
     C                   movel(P)  xxzip         servcozip
     C                   endif
     C*** BEG ADD *** 07/26/05 *********************************************
     C*
     C* If info for service company is missing then load blanks
     C*
     C                   if        servcocode = *zeros
     C                   eval      servcocode = *blanks
     C                   endif
     C                   if        servcotaxid = *zeros
     C                   eval      servcotaxid = *blanks
     C                   endif
     C                   if        servcozip   = '00000    '
     C                   eval      servcozip   = *blanks
     C                   endif
     C*** END ADD *** 07/26/05 *********************************************
    C*
    C* Load Employee info
    C*
     C                   movel     cmss#         emplid
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         accdt
     C*** BEG ADD *** 01/25/05 *********************************************
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
     C*** END ADD *** 01/25/05 *********************************************
     C*** BEG ADD *** 01/08/06 *********************************************
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
     C*** END ADD *** 01/08/06 *********************************************
     C*** BEG ADD *** 11/11/05 *********************************************
     C*
     C* Employee Prepayment Indicator
     C*
     C*** BEG ADD *** 01/04/06 *********************************************
     C                   if        eoepmt = 'Y'
     C                   eval      xxemplppind = 'E'
     C                   endif
     C*** END ADD *** 01/04/06 *********************************************
     C                   if        eotodt > EOCRDT or eotodt > EOWCCK
     C                   eval      xxemplppind = 'P'
     C                   endif
     C*
     C*** END ADD *** 11/11/05 *********************************************
     C*** BEG DEL *** 01/25/05 *********************************************
     C*****************  movel(P)  cdlsn         lastnm
     C*****************  movel(P)  cdfsn         firstnm
     C*****************  movel     cdmid         middin
     C*** END DEL *** 01/25/05 *********************************************
    C*
    C* Load Provider info
    C*
     C                   move      eofein        provtaxid
     C                   move      eozip         facilZip
     C                   if        %subst(facilzip:6:4) = '0000'
     C                   movel     eozip         xxzip             5
     C                   movel(P)  xxzip         facilzip
     C                   endif
     C                   movel     eoflic        zipserv
     C                   if        %subst(zipserv:6:4) = '0000'
     C                   movel     eoflic        xxzip             5
     C                   movel(P)  xxzip         zipserv
     C                   endif
     C*
    C* Load Date Insurer Received Bill
    C*
     C                   if        reportreas = '3' and esrrdt >= eowcck
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
     C*** BEG ADD *** 01/10/07 ****************************************
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
     C*** END ADD *** 01/10/07 ****************************************
     C*
     C                   z-add     eowcck        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         billpaydt
     C*
     C*** BEG ADD *** 01/10/07 ****************************************
     C*
     C                   ENDIF                                                  #1CMDT
     C*
     C*** END ADD *** 01/10/07 ****************************************
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
     C                   movel(P)  ehflic        license#
     C*
     C* Load Payment Plan
     C*
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   if        eoepmt <> 'Y'
     C*** END ADD *** 01/09/06 *********************************************
     C                   move      ehppln        payplan
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   endif
     C*** END ADD *** 01/09/06 *********************************************
     C*
     C* Load diagnosis codes
     C*
     C                   eval      icd91 = getDiag(ehdia1)
     C                   eval      icd92 = getDiag(ehdia2)
     C                   eval      icd93 = getDiag(ehdia3)
     C                   eval      icd94 = getDiag(ehdia4)
     C*
     C* Load the EDI Status with S to indicate this EOB was sent
     C* unless it was Withdrawn.
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/06 ***********************************************
     C                   eval      ehedi  = 'S'
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/06 ***********************************************
     C*
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
     C                   movel(p)  euflic        license#
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
     C                   eval      icd91 = getDiag(eudia1)
     C                   eval      icd92 = getDiag(eudia2)
     C                   eval      icd93 = getDiag(eudia3)
     C                   eval      icd94 = getDiag(eudia4)
     C*
     C* Load Procedure Code Info
     C*
     C*
     C                   if        euprc1 <> *blanks
     C                   eval      procmth = eupcm
     C*** BEG ADD *** 02/22/05 *********************************************
     C                   if        procmth <> '9'
     C*** END ADD *** 02/22/05 *********************************************
     C                   eval      princprcd = euprc1
     C*** BEG ADD *** 02/22/05 *********************************************
     C                   else
     C                   eval      princprcd = getdiag(euprc1)
     C                   endif
     C*** END ADD *** 02/22/05 *********************************************
     C                   endif
     C*
     C                   if        euprc2 <> *blanks
     C*** BEG ADD *** 02/22/05 *********************************************
     C                   if        procmth <> '9'
     C*** END ADD *** 02/22/05 *********************************************
     C                   eval      otherprcd = euprc2
     C*** BEG ADD *** 02/22/05 *********************************************
     C                   else
     C                   eval      otherprcd = getdiag(euprc2)
     C                   endif
     C*** END ADD *** 02/22/05 *********************************************
     C                   endif
     C*
     C* Load Type of Admission
     C*
     C                   eval      typeadm = euadty
     C*
     C* Load Payment Plan
     C*
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   if        eoepmt <> 'Y'
     C*** END ADD *** 01/09/06 *********************************************
     C                   move      euppln        payplan
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   endif
     C*** END ADD *** 01/09/06 *********************************************
     C*
     C* Update the EDI flag to indicate the EOB has been sent to the state
     C* unless it was Withdrawn.
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/06 ***********************************************
     C*
     C                   eval      euedi = 'S'
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/06 ***********************************************
     C*
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
     C*** BEG ADD *** 11/11/05 *********************************************
     C*
     C* Load Payment Plan
     C*
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   if        eoepmt <> 'Y'
     C*** END ADD *** 01/09/06 *********************************************
     C                   move      epppln        payplan
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   endif
     C*** END ADD *** 01/09/06 *********************************************
     C*
     C*** END ADD *** 11/11/05 *********************************************
     C*** BEG ADD *** 01/10/06 *********************************************
     C*
     C* Load Payments
     C*
     C                   exsr      getdwc10pmts
     C*** END ADD *** 01/10/06 *********************************************
     C*
     C* Update the EDI flag to indicate the EOB has been sent to the state
     C* unless it was Withdrawn.
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/06 ***********************************************
     C*
     C                   eval      epedi = 'S'
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/06 ***********************************************
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
     C                   movel(P)  edflic        license#
     C*
     C* Load Payment Plan
     C*
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   if        eoepmt <> 'Y'
     C*** END ADD *** 01/09/06 *********************************************
     C                   eval      payplan = edppln
     C*** BEG ADD *** 01/09/06 *********************************************
     C                   endif
     C*** END ADD *** 01/09/06 *********************************************
     C*
     C* Update the EDI flag to indicate the EOB has been sent to the state
     C* unless it was Withdrawn.
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/06 ***********************************************
     C*
     C                   eval      ededi = 'S'
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/06 ***********************************************
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
     C                   endsr
     C*** BEG ADD *** 01/10/06 *********************************************
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
     C                   if        dprxpt = 'S'
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
     C*** END ADD *** 01/10/06 *********************************************
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
     C*** BEG DEL *** 11/11/05 *********************************************
     C****************** move      *blanks       eobcode4
     C*** END DEL *** 11/11/05 *********************************************
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdh01                              99
     C*
     C* Load from the database
     C*
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
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
     C                   move      dhprcd        paidproc
     C                   move      billedmod     paidmod
     C                   move      cachrg        detchrg
     C                   z-add     dhunts        units
     C                   move      capaid        detpaid
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
     C*** BEG ADD *** 11/11/05 *********************************************
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
     C                   endif                                                  dhndc <> ' '
     C*
     C*** END ADD *** 11/11/05 *********************************************
    C*
    C* Load the EOB codes
    C*
     C                   exsr      lodeobcodes
    C*
     C                   endsr
     C*** BEG ADD *** 11/11/05 *********************************************
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
     C*** BEG ADD *** 01/10/06 *********************************************
     C                   move      'N'           skpdtl10
     C*** END ADD *** 01/10/06 *********************************************
     C*
     C                   eval      kkdseq = cadseq
     C*
     C     dtky          chain     pdp01                              99
     C*** BEG ADD *** 01/10/06 *********************************************
     C                   if        dprxpt = 'S'
     C                   eval      skpdtl10 = 'Y'
     C                   else
     C*** END ADD *** 01/10/06 *********************************************
     C*
     C* Load from the database
     C*
     C                   move      '2'           recflg
     C                   move      calin#        detseq#
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
    C* Load Pharmacy number
    C*
     C                   movel     dprx#         pharmacy#
     C*
    C* Load New or Refill
    C*
     C                   movel     dpnwrf        neworrefill
     C*
    C* Load Date Filled
    C*
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         datefilled
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
     C*** BEG ADD *** 01/10/06 *********************************************
     C                   endif                                                  dprxpt='S'
     C*** END ADD *** 01/10/06 *********************************************
    C*
     C                   endsr
     C*** END ADD *** 11/11/05 *********************************************
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
     C                   move      calin#        detseq#
     C*
     C* Load the procedure codes
     C*
     C                   move      ddbprc        billedproc
     C*
     C                   move      ddprcd        paidproc
     C                   move      cachrg        detchrg
     C                   move      capaid        detpaid
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
     C*** BEG CHG *** 08/24/04 *********************************************
     C****************** move      durvcd        revcode
     C****************** move      durate        billedproc
     C                   move      dubprc        revcode
     C                   move      dubrat        billedproc
     C*** END CHG *** 08/24/04 *********************************************
     C                   move      *blanks       billedmod
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
     C*** BEG ADD *** 03/24/05 *********************************************
     C                             or deeob = *blanks and denot1 <> *blanks
     C                             and deprnt <> 'N' and destat <> 'I'
     C*** END ADD *** 03/24/05 *********************************************
     C*
     C*** BEG ADD *** 03/24/05 *********************************************
     C                   if        deeob <> *blanks
     C*** END ADD *** 03/24/05 *********************************************
     C                   eval      kkkey = deeob
     C*** BEG ADD *** 03/24/05 *********************************************
     C                   else
     C                   eval      kkkey = 'S20'
     C                   endif
     C*** END ADD *** 03/24/05 *********************************************
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
     C                   else
     C                   move      '20'          xxeob
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
     C*** BEG DEL *** 11/11/05 *********************************************
     C****************** if        eobcode4 = *blanks
     C****************** move      xxeob         eobcode4
     C****************** endif
     C*** END DEL *** 11/11/05 *********************************************
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
     C                   if        cachrg = capaid
     C*
     C                   select
     C*
     C                   when      eobcode1 = *blanks
     C                   eval      eobcode1 = '07'
     C*
     C                   when      eobcode2 = *blanks
     C                   eval      eobcode2 = '07'
     C*
     C                   when      eobcode3 = *blanks
     C                   eval      eobcode3 = '07'
     C*
     C*** BEG DEL *** 11/11/05 *********************************************
     C****************** when      eobcode3 = *blanks
     C****************** eval      eobcode3 = '07'
     C*** END DEL *** 11/11/05 *********************************************
     C*
     C                   endsl
     C*
     C                   endif
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
     C*** BEG CHG *** 11/11/05 *********************************************
     C****************** move      'B '          revind
     C                   move      'C '          revind
     C*** END CHG *** 11/11/05 *********************************************
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
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
     C                   exsr      loddtl09
     C                   except    billDtl09
     C*
     C     caky          reade     pca01                                  99
     C*
     C                   enddo
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
       xxPrt = '0';
       Chain orKey POR01;
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
     C                   CALL      'WA050R'
     C                   PARM                    #1CO#
     C                   PARM      ##WCF#        #1FND
     C                   PARM                    #1TRDT            8
     C                   PARM      *BLANKS       #1CMDT            8
     C*
     C                   ENDIF                                                  ##OUTS=N
     C*
     C                   ENDIF                                                  @@WCIF
     C*
     C                   ENDSR
     C*
     C*** END ADD ***** 01/10/07 ****************************************
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
     C*** BEG ADD *** 01/08/06 *********************************************
     C*
     C* Retrieve client id
     C*
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
     C*
     C*** END ADD *** 01/08/06 *********************************************
     C*** BEG ADD *** 01/10/07 *********************************************
     C*
     C*  Check to see if WCMS is installed
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1        WC INST
     C                   PARM                    @@MCIF            1        MC INST
     C                   PARM                    @@WCIN            1        WC INTERFACE
     C*
     C*** END ADD *** 01/10/07 *********************************************
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
     C*** BEG ADD *** 02/05/05 *********************************************
     C*** BEG DEL *** 01/10/06 *********************************************
     C****************** exsr      splitphar
     C*** END DEL *** 01/10/06 *********************************************
     C*** END ADD *** 02/05/05 *********************************************
     C*
     C                   except    billHdr10
     C*** BEG ADD *** 11/11/05 *********************************************
     C*
     C     caky          chain     pca01                              99
     C*
     C                   dow       not *in99
     C*
     C                   exsr      loddtl10
     C*** BEG CHG *** 01/10/06 *********************************************
     C                   if        skpdtl10 = 'N'
     C                   except    billDtl10
     C                   endif
     C*** END CHG *** 01/10/06 *********************************************
     C*
     C     caky          reade     pca01                                  99
     C*
     C                   enddo
     C*** END ADD *** 11/11/05 *********************************************
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
     C* Show EOB as submitted unless it was Withdrawn.
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/06 ***********************************************
     C*
     C                   eval      eotape = 'S'
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/06 ***********************************************
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
     C                   update    peo01
    C*
     C     eaky          chain     pea01                              99
    C*
     C                   if        not *in99
    C*
    C* Show EOB as submitted unless it was Withdrawn.
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   if        eotape <> 'W'
     C***** END ADD ** 09/27/06 ***********************************************
     C*
     C                   eval      eaedi  = 'S'
     C*
     C***** BEG ADD ** 09/27/06 ***********************************************
     C                   endif
     C***** END ADD ** 09/27/06 ***********************************************
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
     O                       fill2               18
     O                       insurcode           23
     O                       insurtaxid          32
     O                       insurzip            41
     O                       servcocode          46
     O                       servcotaxid         55
     O                       servcozip           64
     O                       emplid              74
     O                       accdt               82
     O                       license#            95
     O                       provtaxid          104
     O                       zipserv            113
     O                       lastnm             143
     O                       firstnm            158
     O                       middin             159
     O                       billrcvdt          167
     O                       billpaydt          175
     O                       totpay             186
     O                       reportreas         188
     O                       payplan            190
     O                       icd91              196
     O                       icd92              202
     O                       icd93              208
     O                       icd93              214
     O                       insintfile#        244
     O                       subloc             264
     O*** BEG CHG *** 11/11/05 *********************************************
     O********************** fill36             300
     O                       xxemplppind        265
     O                       xxdupovrind        266
     O                       fill34             300
     C*** END CHG *** 11/11/05 *********************************************
     O* DWC09 Bill Detail
     O          eadd         billDtl09
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       placesrv            19
     O                       icd9ref             23
     O                       billedproc          28
     O                       billedmod           30
     O                       paidproc            35
     O                       paidmod             37
     O                       detchrg             48
     O                       units               51
     O                       detpaid             62
     O                       dateofsrvfrm        70
     O                       dateofsrvto         78
     O                       eobcode1            80
     O                       eobcode2            82
     O                       eobcode3            84
     O*** BEG CHG *** 11/11/05 *********************************************
     O********************** eobcode4            86
     O********************** fill216            300
     O                       xxndc               95
     O                       fill205            300
     O*** END CHG *** 11/11/05 *********************************************
     O* DWC09 Transmission Trailer
     O          eadd         tranTrl09
     O                       trantlrmkr           3
     O                       numclm09             9
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
     O                       fill213            250
     O* DWC10 Bill Header
     O          eadd         billHdr10
     O                       cntl                13
     O                       recflg              14
     O                       formid              16
     O                       fill2               18
     O                       insurcode           23
     O                       insurtaxid          32
     O                       insurzip            41
     O                       servcocode          46
     O                       servcotaxid         55
     O                       servcozip           64
     O                       emplid              74
     O                       accdt               82
     O                       lastnm             112
     O                       firstnm            127
     O                       middin             128
     O                       billrcvdt          136
     O                       billpaydt          144
     O*** BEG CHG *** 02/05/05 *********************************************
     O********************   totchrg            155
     O********************   totpay             166
     O                       totpay             155
     O                       totpaysupp         166
     O*** END CHG *** 02/05/05 *********************************************
     O                       reportreas         168
     O                       statemntdt         176
     O                       insintfile#        206
     O                       subloc             226
     O*** BEG CHG *** 11/11/05 *********************************************
     O                       payplan            228
     O                       xxemplppind        229
     O                       xxdupovrind        230
     O********************** fill24             250
     O                       fill20             250
     O*** END CHG *** 11/11/05 *********************************************
     O*** BEG ADD *** 11/11/05 *********************************************
     O* DWC10 Bill Detail
     O          eadd         billDtl10
     O                       cntl                13
     O                       recflg              14
     O                       detseq#             17
     O                       qtyofmed            22
     O                       dayssupply          25
     O                       xxndc               36
     O                       pharmacy#           46
     O                       neworrefill         47
     O                       datefilled          55
     O                       certifictn          56
     O                       prescbrlic          69
     O                       detchrg             80
     O                       eobcode1            82
     O                       eobcode2            84
     O                       eobcode3            86
     O                       detpaid             97
     O                       fill153            250
     O*** END ADD *** 11/11/05 *********************************************
     O* DWC10 Transmission Trailer
     O          eadd         tranTrl10
     O                       trantlrmkr           3
     O                       numclm10             9
     O                       fill241            250
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
     O                       fill2               18
     O                       insurcode           23
     O                       insurtaxid          32
     O                       insurzip            41
     O                       servcocode          46
     O                       servcotaxid         55
     O                       servcozip           64
     O                       emplid              74
     O                       accdt               82
     O                       license#            95
     O                       provtaxid          104
     O                       zipserv            113
     O                       lastnm             143
     O                       firstnm            158
     O                       middin             159
     O                       placetmt           161
     O                       billrcvdt          169
     O                       billpaydt          177
     O                       totpay             188
     O                       reportreas         190
     O                       payplan            192
     O                       insintfile#        222
     O                       subloc             242
     O*** BEG CHG *** 11/11/05 *********************************************
     O                       xxemplppind        243
     O                       xxdupovrind        244
     O********************** fill58             300
     O                       fill56             300
     O*** END CHG *** 11/11/05 *********************************************
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
     O                       fill313            350
     O* DWC90 Bill Header
     O          eadd         billHdr90
     O                       cntl                13
     O                       recflg              14
     O                       formid              16
     O                       fill2               18
     O                       insurcode           23
     O                       insurtaxid          32
     O                       insurzip            41
     O                       servcocode          46
     O                       servcotaxid         55
     O                       servcozip           64
     O                       typebill            67
     O                       emplid              77
     O                       accdt               85
     O                       license#            98
     O                       lastnm             128
     O                       firstnm            143
     O                       middin             144
     O                       admissdt           152
     O                       admisshr           154
     O                       stmtcovfrmdt       162
     O                       stmtcovtomdt       170
     O                       dischrghr          172
     O                       provtaxid          181
     O                       facilzip           190
     O                       billrcvdt          198
     O                       billpaydt          206
     O                       totpay             217
     O                       icd91              223
     O                       icd92              229
     O                       icd93              235
     O                       icd93              241
     O*** BEG DEL *** 11/11/05 *********************************************
     O********************** procmth            242
     O*** END DEL *** 11/11/05 *********************************************
     O                       princprcd          247
     O                       otherprcd          252
     O                       reportreas         254
     O                       payplan            256
     O                       typeadm            257
     O                       insintfile#        287
     O*** BEG CHG *** 11/11/05 *********************************************
     O********************** subloc             301
     O********************** fill49             350
     O                       subloc             307
     O                       xxemplppind        308
     O                       xxdupovrind        309
     O                       fill41             350
     O*** END CHG *** 11/11/05 *********************************************
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
     O                       fill240            300
     O* DWC90 Transmission Trailer
     O          eadd         tranTrl90
     O                       trantlrmkr           3
     O                       numclm90             9
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
     O***** End Del ***** 08/26/14 ************************************
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
     O***** Beg Del ***** 08/26/14 ************************************
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
     O***** End Del ***** 08/26/14 ************************************
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
