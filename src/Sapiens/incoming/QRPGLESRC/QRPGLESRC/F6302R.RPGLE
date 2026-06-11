     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*                                                               *
     H*    NCCI Medical Data Call Extract                             *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/02/2009 by C:227    Project MC331              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  07/18/25  C:342  Project MC451                               *
     H*                   NCCI Med Data Call Enhancements             *
     H*                                                               *
     H*  07/05/16  C:079  Support: 42246/114                          *
     H*                   Add logic to expand icd10 to 8 characters   *
     H*                                                               *
     H*  05/24/16  C:307  Support: 40844                              *
     H*                   Add logic  that loads U2ntWk                *
     H*                                                               *
     H*  03/16/15  C:280  Project: MC393                              *
     H*                   Add logic to process icd10 codes            *
     H*                                                               *
     H*  01/06/15  C:296  Project: 114MQA                             *
     H*                   Change logic that excludes groups.          *
     H*                   Change logic that selects the state         *
     H*                   according                                   *
     H*                                                               *
     H*  01/05/15  C:296  Support: 87377                              *
     H*                   Add leading zeroes in case/claim number.    *
     H*                                                               *
     H*  05/29/13  C:079  Support: MC114/82487                        *
     H*                   Correct how policy number is loaded         *
     H*                   Correct how claim number is loaded          *
     H*                                                               *
     H*  01/11/13  C:287  Project: SY266_QRY. Added ACTGRP directives *
     H*                                                               *
     H*  05/05/11  C:235  Project 114MZL                              *
     H*                   Exclude Groups from NCCI Medical Data Call  *
     H*                                                               *
     H*  05/03/11  C:251  Project 133MKP                              *
     H*                   Pass Pharmacy number for PMOA               *
     H*                                                               *
     H*  04/13/11  C:257  Support 76032                               *
     H*                   Data Format for Submission Time             *
     H*                                                               *
     H*  12/17/10  C:227  Support 75228  WC114                        *
     H*                   Medical data call issue in NJ for group 600 *
     H*                                                               *
     H*  11/09/10  C:227  Support 74812                               *
     H*                   Remove decimal positions from units field & *
     H*                   round them to nearest whole number          *
     H*                                                               *
     H*  11/03/10  C:227  Project 114MTN Priss# 8                     *
     H*                   Prefix leading zero in revenue code field.  *
     H*                                                               *
     H*  10/26/10  C:227  Project 114MTN Priss# 7                     *
     H*                   Change in mod to report only two modifiers  *
     H*                                                               *
     H*  09/17/10  C:227  Project 114MTN Priss# 6                     *
     H*                   Change in mod to report ' ' instead of '00' *
     H*                   Changed service date and to/from date logic *
     H*                                                               *
     H*  07/02/10  C:227  Support 73328                               *
     H*                   Changed u2pol# to remove '-'                *
     H*                                                               *
     H*  07/13/09  C:227  Project MC331                               *
     H*                   Changed to correct Birth Year               *
     H*                                                               *
     H*****************************************************************
    F* Submission Control File
     Ffeu1p     uf a e             disk
    F*
    F* Medical Data Call Record File
     Ffeu2p     uf a e             disk
    F*
    F* Medical Data Call Log Record File
     Ffenlp     uf a e             disk
     Ffenll1    if   e           k disk    rename(pnl01:pnl02) prefix(n1:2)
     Ffenll3    if   e           k disk    rename(pnl01:pnl04) prefix(n2:2)
    F*
    F* Medical Data Call Submission Control File
     Ffeu9l1    uf a e           k disk
    F*
    F* Medical Data Call Extract Flat File
     Ffeu0p     o    e             disk
    F*
    F* EOMB Header Amount Transaction File
     Ffaeal1    if   e           k disk
     Ffaeal14   if   e           k disk    rename(pea01:pea02) prefix(e1:2)
    F*
    F* Claim file
     Fwmcml1    if   e           k disk    usropn
    F*
    F* Employer Detail file
     Fwdell1    if   e           k disk    usropn
    F*
    F* Group Auxillary file
     Fwdf2l     if   e           k disk    usropn
     C*** BEG ADD *** 05/05/11 ****************************************
     Fwmcxl     if   e           k disk    usropn
     C*** END ADD *** 05/05/11 ****************************************
    F*
    F* State Code table file
     Fststl1    if   e           k disk
    F*
    F* State Code table file
     Fftgtl     if   e           k disk
    F*
    F* Claimant Demographic file
     Ffmcdl     if   e           k disk
    F*
    F* Claim file
     Ffmcml2    if   e           k disk    rename(pcm01:pcm02) prefix(c1:2)
    F*
    F* Detail Charge File
     Ffdcal19   if   e           k disk
    F*
    F* Payee Demographics File
     Ffmpdl     if   e           k disk
    F*
    F* EOMB Transaction File
     Ffaeol4    if   e           k disk
    F*
    F* Physician Transaction File
     Ffddhl     if   e           k disk
    F*
    F* Hospital Transaction File
     Ffddul     if   e           k disk
    F*
    F* Dental Transaction File
     Ffdddl     if   e           k disk
    F*
    F* Pharmacy Transaction File
     Ffddpl     if   e           k disk
    F*
    F* Carrier Reporting Master File
     Fwmrpl4    if   e           k disk    usropn
    F*
    F* HCFA Header File
     Ffaehl     if   e           k disk
    F*
    F* Hospital Header File
     Ffaeul     if   e           k disk
    F*
    F* Company Auxillary File
     Ffmc1l     if   e           k disk
    F*
     Ffmdil     if   e           k disk
     F*** Beg Add *** 05/03/11 ****************************************
     Ffaepl     if   e           k disk
     F*** End Add *** 05/03/11 ****************************************
     F*** Beg Add *** 01/06/15 ****************************************
     Fwxssell   if   e           k disk
     Fwxgsl     if   e           k disk
     F*** End Add *** 01/06/15 ****************************************
     F*** Beg Add *** 03/16/15 ****************************************
     FFMICD10L1 if   e           k disk
    F*
    F* ICD10-CM/PCS Code by code/ind
    F*
     F*** End Add *** 03/16/15 ****************************************
     F*** Beg Add *** 07/18/25 ***************************************
     Ffaedl     if   e           k disk
     F*** End Add *** 07/18/25 ***************************************
     F*
     D getDiag         PR             7
     D  icd9                          5
     D*
     D*** Beg Chg *** 07/05/16 *****************************************
     D*** getDiag10       PR             7
     D***  icd10                         7
     D getDiag10       PR             8
     D  icd10                         7
     D*** End Chg *** 07/05/16 *****************************************
     D*
     D feu1pds       e ds                  extname(feu1p)
     D feu2pds       e ds                  extname(feu2p)
    D*================================================================
    D*  Variables
    D*================================================================
     D icd91           s              6
     D icd92           s              6
     D icd93           s              6
     D icd94           s              6
     D*** Beg Add *** 03/16/15 ****************************************
     D*** Beg Chg *** 07/05/16 ****************************************
     D**** icd101          s              7
     D**** icd102          s              7
     D**** icd103          s              7
     D**** icd104          s              7
     D**** icd105          s              7
     D icd101          s              8
     D icd102          s              8
     D icd103          s              8
     D icd104          s              8
     D icd105          s              8
     D*** End Chg *** 07/05/16 ****************************************
     D*** End Add *** 03/16/15 ****************************************
     D qqco#           s              3
     D qqfdt           s              8
     D qqtdt           s              8
     D qqmth           s              2
     D qqqtr           s              1
     D qqyear          s              4
     D qqftcd          s              1
     D qqfile          s             30
     D qqlgfl          s             30
     D qqsbdt          s                   like(u9sbdt)
     D qqsbtm          s                   like(u9sbtm)
    D*  Date/Time Data Structure.
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*** BEG CHG *** 04/13/11 ****************************************
     D                 ds
     D @@sbtm                         6
     D  @@sbtm1                1      1
     D  @@sbtm2                2      6
     D*** END CHG *** 04/13/11 ****************************************
     D psds          esds                  extname(szq1p)
     D*
     D CurDate         s             10
     D @@jrst          s                   like(cmjrst)
     D @@co#           s                   like(eaco#)
     D @@sub           s                   like(easub#)
     D @@fnd           s                   like(eafnd)
     D @@wfnd          s                   like(eafnd)
     D @@fyr           s                   like(cmfyr)
     D @@emp#          s                   like(cmemp#)
     D @@div           s                   like(cmdiv)
     D @@case          s                   like(cmcase)
     D @@tbid          s                   like(gttbid)
     D @@ssn#          s                   like(eass#)
     D @@eob#          s                   like(eaeob#)
     D kkeob#          s                   like(nleob#)
     D @@fein          s                   like(eafein)
     D @@mod           s                   like(eamod)
     D @@btyp          s                   like(eobtyp)
     D @@seq           s                   like(cadseq)
     D @@lin#          s                   like(calin#)
     D @stkey          s                   like(stkey)
     D @@zip           s                   like(eazip)
     D @@paid          s                   like(eapaid)
     D @@wcpd          s                   like(eawcpd)
     D @@epdt          s                   like(eaepdt)
     D @@rcn           s                   like(nlrcn)
     D kkdiag          s                   like(diicd9)
     D*** Beg Add *** 03/16/15 ****************************************
     D kxdiag          s                   like(iccode)
     D*** Beg Add *** 03/16/15 ****************************************
     D kkfile          s                   like(u9file)
     D*** BEG ADD *** 09/17/10 ****************************************
     D*** BEG DEL *** 10/26/10 ****************************************
     D*kkmod1****      s                   like(dhmod1)
     D*kkmod2****      s                   like(dhmod1)
     D*kkmod3****      s                   like(dhmod1)
     D*kkmod4****      s                   like(dhmod1)
     D*** END DEL *** 10/26/10 ****************************************
     D*** BEG ADD *** 10/26/10 ****************************************
     D                 ds
     D kkmod                   1      8    dim(2)
     D  kkmod1                 1      4
     D  kkmod2                 5      8
     D                 ds
     D pmod                    1      8    dim(4)
     D  dhmod1                 1      2
     D  dhmod2                 3      4
     D  dhmod3                 5      6
     D  dhmod4                 7      8
     D                 ds
     D hmod                    1      4    dim(2)
     D  dupmod                 1      2
     D  dumod2                 3      4
     D*** END ADD *** 10/26/10 ****************************************
     D*** END ADD *** 09/17/10 ****************************************
     D isWC            s              1
     D cnt             s              9  0
    C*================================================================
    C*  Main Process Loop
    C*================================================================
     c*
     C                   exsr      srinit
     C                   eval      *inlr = *on
     C                   return
     c*
    C*================================================================
    C*  SRINIT - Define all KLIST etc.
    C*================================================================
     C     srinit        begsr
     C*
     C* Entry Parameters
     C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfdt
     C                   parm                    qqtdt
     C                   parm                    qqmth
     C                   parm                    qqqtr
     C                   parm                    qqyear
     C                   parm                    qqftcd
     C                   parm                    qqfile
     C                   parm                    qqlgfl
     C*
     C     dtyymd        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtymd         plist
     C                   parm                    srdti
     C                   parm      '*YMD'        srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*** BEG ADD *** 07/13/09 *********************************************
    C*
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*** END ADD *** 07/13/09 *********************************************
     C*
     C* Key List for FAEAL1
     C*
     C     eaky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@eob#
     C                   kfld                    @@rcn
     C*
     C* Key List for Claim Jurisdiction State
     C*
     C     cmky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@wfnd
     C                   kfld                    @@fyr
     C                   kfld                    @@case
     C*
     C* Key List for Company Auxillary File
     C*
     C     c1ky          klist
     C                   kfld                    @@co#
     C*
     C* Key List for FENLL1
     C*
     C     nlky          klist
     C                   kfld                    kkeob#
     C*
     C     n1ky          klist
     C                   kfld                    qqlgfl
     C*
     C     n2ky          klist
     C                   kfld                    qqlgfl
     C                   kfld                    qqsbdt
     C                   kfld                    qqsbtm
     C*
     C* Key List for STSTP
     C*
     C     stky          klist
     C                   kfld                    @stkey
     C*
     C* Key List for WDF2L
     C*
     C     f2ky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@wfnd
     C*
     C* Key List for WDELL1
     C*
     C     elky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@wfnd
     C                   kfld                    @@fyr
     C                   kfld                    @@emp#
     C                   kfld                    @@div
     C*
     C* Key List for FTGTL
     C*
     C     gtky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@tbid
     C                   kfld                    @stkey
     C*
     C* Key List for FMCDL
     C*
     C     cdky          klist
     C                   kfld                    @@ssn#
     C*
     C* Key List for FMCML2
     C*
     C     cmky1         klist
     C                   kfld                    @@co#
     C                   kfld                    @@sub
     C                   kfld                    @@fnd
     C                   kfld                    @@ssn#
     C                   kfld                    @@fyr
     C                   kfld                    @@case
     C*
     C* Key List for FDCAL19
     C*
     C     caky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@sub
     C                   kfld                    @@fnd
     C                   kfld                    @@fyr
     C                   kfld                    @@case
     C                   kfld                    @@eob#
     C*
     C* Key List for FDDHL
     C*
     C     dhky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@sub
     C                   kfld                    @@fnd
     C                   kfld                    @@fyr
     C                   kfld                    @@case
     C                   kfld                    @@eob#
     C                   kfld                    @@seq
     C*
     C* Key List for FMPDL
     C*
     C     pdky          klist
     C                   kfld                    @@fein
     C                   kfld                    @@mod
     C*
     C* Key List for FEU9L1
     C*
     C     u9ky          klist
     C                   kfld                    kkfile
     C*
     C* Key List for the Diagnosis code File
     C*
     C     diky          klist
     C                   kfld                    kkdiag
     C*
     C*** BEG ADD *** 05/05/11 ****************************************
     C* Key List for Claim/DARTS Control Definition File
     C*
     C     cxky          klist
     C                   kfld                    @@co#
     C                   kfld                    ##fnd             3 0
     C*
     C*** END ADD *** 05/05/11 ****************************************
     C*** Beg Add *** 05/03/11 ****************************************
     C*
     C* Key List for FAEPL
     C*
     C     epky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@sub
     C                   kfld                    @@fnd
     C                   kfld                    @@fyr
     C                   kfld                    @@case
     C                   kfld                    @@eob#
     C*
     C*** End Add *** 05/03/11 ****************************************
     C*** BEG ADD *** 01/06/15 ****************************************
     C* Key List for state file.
     C*
     C     stFlKey       klist
     C                   kfld                    @@co#
     C                   kfld                    @@jrst
     C*
     C*** END ADD *** 01/06/15 ****************************************
     C*
     C* Current Date and Time
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C*** BEG ADD *** 12/17/10 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       @@CLT             3
     C*** END ADD *** 12/17/10 ****************************************
     C*
     C                   call      'SA001R'
     C                   parm                    ##wcif            1
     C                   parm                    ##mcif            1
     C                   parm                    ##wcin            1
     C                   parm      *blank        ##ar              1
     C                   parm      *blank        ##ntwk            1
     C                   parm      *blank        ##ur              1
     C                   parm      *blank        ##med             1
     C                   parm      *blank        ##edi             1
     C                   parm      *blank        ##wcdk            1
     C                   parm      *blank        ##rhab            1
     C                   parm      *blank        ##mss             1
     C                   parm      *blank        ##fsch            1
     C                   parm      *blank        ##prvb            1
     C                   parm      *blank        ##npwc            1
     C                   parm      *blank        ##os              1
     C                   parm      *blank        ##arls            1
     C                   parm      *blank        ##ccod            1
     C                   parm      *blank        ##crvw            1
     C                   parm      *blank        ##prom            1
     C                   parm      *blank        ##mcrv            1
     C                   parm      *blank        ##freq            1
     C                   parm      *blank        ##crac            1
     C                   parm      *blank        ##preo            1
     C                   parm      *blank        ##parm            1
     C                   parm      *blank        ##mgri            1
     C                   parm      *blank        ##kcif            1
     C                   parm      *blank        ##curs            1
     C                   parm      *blank        ##pcib            1
     C                   parm      *blank        ##clt#            1
     C                   parm      *blank        ##ucpf            1
     C                   parm      *blank        ##dcf             1
     C                   parm      *blank        ##autb            1
     C                   parm      *blank        ##deif            1
     C                   parm      *blank        ##aure            1
     C                   parm      *blank        ##cred            1
     C                   parm      *blank        ##idrt            1
     C                   parm      *blank        ##cprv            1
     C                   parm      *blank        ##eft             1
     C                   parm      *blank        ##govr            1
     C                   parm      *blank        ##ncdc            1
     C
     C*
     C                   movel     qqco#         @@co#
     C*
     C                   if        ##ncdc <> 'Y'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C*
     C                   if        ##wcif = 'Y' and ##npwc <> 'Y'
     C                   open      wmcml1
     C                   open      wdell1
     C                   open      wdf2l
     C                   open      wmrpl4
     C*** BEG ADD *** 05/05/11 ****************************************
     C                   open      wmcxl
     C*** END ADD *** 05/05/11 ****************************************
     C                   eval      isWC = 'Y'
     C                   else
     C                   eval      isWC = 'N'
     C                   endif
     C*
     C                   select
     C*
     C                   when      qqftcd = 'O'
     C                   exsr      SROrigSub
     C*
     C                   when      qqftcd = 'R'
     C                   exsr      SRReplSub
     C*
     C                   when      qqftcd = 'D'
     C                   exsr      SRDelSub
     C*
     C                   endsl
     C*Populate Flat File
     C                   exsr      SRFlatFl
     C*
     C                   if        ##wcif = 'Y' and ##npwc <> 'Y'
     C                   close     wmcml1
     C                   close     wdell1
     C                   close     wdf2l
     C                   close     wmrpl4
     C*** BEG ADD *** 05/05/11 ****************************************
     C                   close     wmcxl
     C*** END ADD *** 05/05/11 ****************************************
     C                   endif
     C                   endsr
    C*================================================================
    C*  SRORIGSUB - NCCI Original Submission Subroutine
    C*================================================================
     C     SROrigSub     begsr
     C* Write Control Record
     C                   exsr      SRwrite
     C* Read EOMB Transaction File
     C                   read      pea01                                  66
     C                   dow       *in66 = *off
     C                   movel     easub#        ##sub#            5
     C                   movel     eafnd         #1fnd             3
     C* Get Worker's Compensation Group and claim jurisdiction state
     C                   exsr      getwcg
     C* Get Claim Jurisdiction State.
     C                   eval      @@fyr = eafyr
     C                   eval      @@case = eacase
     C                   if        isWC = 'Y'
     C     cmky          chain     pcm01                              67
     C                   if        *in67 = *off
     C                   move      cmjrst        @@jrst
     C                   movel     cmjrst        @stkey
     C                   eval      @@div = cmdiv
     C                   eval      @@emp# = cmemp#
     C                   endif
     C                   endif
     C                   movel     @@jrst        @stkey
     C***** BEG CHG ***** 01/06/15 ******************************
     C*****stky          chain     pst01                              67
     C*****              if        *in67 = *off
     C*****              if        stncdc = 'Y'
      /Free
        chain stFlKey pss01;
          if %found(wxssell);
             if SSieFl = 'I';
      /End-Free
     C***** END CHG ***** 01/06/15 ******************************
     C* Set Transaction Code
     C                   eval      kkeob# = eaeob#
     C                   if        qqftcd = 'O'
     C                   movel     '01'          trancd
     C     nlky          setll     pnl02
     C     nlky          reade     pnl02                                  55
     C                   dow       *in55 = *off
     C                   if        n1prfl <> 'F'
     C                   movel     '03'          trancd            2
     C                   leave
     C                   endif
     C     nlky          reade     pnl02                                  55
     C                   enddo
     C                   endif
     C* Get Policy/carrier Information
     C                   exsr      SRPolInfo
     C                   endif
     C                   endif
     C                   read      pea01                                  66
     C                   enddo
     C                   exsr      SRUpdRcds
     C                   endsr
    C*================================================================
    C*  SRREPLSUB - NCCI File Replacement Subroutine
    C*================================================================
     C     SRReplSub     begsr
     C* Write Control Record
     C                   exsr      SRwrite
     C                   eval      kkfile = qqlgfl
     C     u9ky          chain(n)  pu901
     C                   if        %found
     C                   eval      qqsbdt = u9sbdt
     C                   eval      qqsbtm = u9sbtm
     C                   endif
     C* Read Log File
     C     n2ky          setll     pnl04
     C     n2ky          reade     pnl04                                  55
     C                   dow       *in55 = *off
     C                   eval      @@eob# = n2eob#
     C                   eval      @@rcn = n2rcn
     C     eaky          chain     pea02                              66
     C                   if        *in66 = *off
     C                   movel     e1sub#        ##sub#            5
     C                   movel     e1fnd         #1fnd             3
     C* Get Worker's Compensation Group and claim jurisdiction state
     C                   exsr      getwcg
     C* Get Claim Jurisdiction State.
     C                   eval      @@fyr = e1fyr
     C                   eval      @@case = e1case
     C                   if        isWC = 'Y'
     C     cmky          chain     pcm01                              67
     C                   if        *in67 = *off
     C                   move      cmjrst        @@jrst
     C                   movel     cmjrst        @stkey
     C                   eval      @@div = cmdiv
     C                   eval      @@emp# = cmemp#
     C                   endif
     C                   endif
     C                   movel     @@jrst        @stkey
     C***** BEG CHG ***** 01/06/15 ******************************
     C*****stky          chain     pst01                              67
     C*****              if        *in67 = *off
     C*****              if        stncdc = 'Y'
      /Free
        chain stFlKey pss01;
          if %found(wxssell);
             if SSieFl = 'I';
      /End-Free
     C***** END CHG ***** 01/06/15 ******************************
     C* Set Transaction Code
     C                   eval      kkeob# = e1eob#
     C                   movel     n2trcd        trancd
     C* Get Policy/carrier Information
     C                   exsr      SRPolInfo
     C                   endif
     C                   endif
     C                   endif
     C     n2ky          reade     pnl04                                  55
     C                   enddo
     C                   exsr      SRUpdRcds
     C                   endsr
    C*================================================================
    C*  SRDELSUB - NCCI File Deletion Subroutine
    C*================================================================
     C     SRDelSub      begsr
     C* Write Control Record
     C                   exsr      SRwrite
     C                   endsr
    C*================================================================
    C*  SRUPDRCDS - Update Record Count & Process Status
    C*================================================================
     C     SRUpdRcds     begsr
     C                   movel     'P'           u9prfl
     C                   movel     *all'0'       u1rcds
     C                   move      cnt           u1rcds
     C                   read      pu101                                  56
     C                   if        *in56 = *off
     C                   movel     *all'0'       u1rcds
     C                   move      cnt           u1rcds
     C                   update    pu101
     C                   endif
     C*                  if        qqftcd = 'O'
     C                   eval      kkfile = qqfile
     C*                  elseif    qqftcd = 'R'
     C*                  eval      kkfile = qqlgfl
     C*                  endif
     C     u9ky          chain     pu901                              56
     C                   if        *in56 = *off
     C                   movel     'P'           u9prfl
     C                   eval      u9rcds = cnt
     C                   update    pu901
     C                   endif
     C                   endsr
    C*================================================================
    C*  SRFLATFL  - Update Flat File
    C*================================================================
     C     SRFlatFl      begsr
     C     *start        setll     feu1p
     C                   read      feu1p                                  56
     C                   dow       *in56 = *off
     C                   eval      u0data = feu1pds
     C                   write     pu001
     C                   read      feu1p                                  56
     C                   enddo
     C     *start        setll     feu2p
     C                   read      feu2p                                  56
     C                   dow       *in56 = *off
     C                   eval      u0data = feu2pds
     C                   write     pu001
     C                   read      feu2p                                  56
     C                   enddo
     C                   endsr
    C*================================================================
    C*  SRWRITE - Write Control Record
    C*================================================================
     C     SRWrite       begsr
     C* Write Submission Control Record - FEU1P
     C                   movel     'SUBCTRLREC'  u1rcty
     C                   movel     qqftcd        u1sbty
     C                   if        qqftcd = 'D'
     C                   movel     'R'           u1sbty
     C                   endif
     C                   move      *all'0'       u1carr
     C     c1ky          chain     pc101
     C                   if        %found
     C                   move      c110a4        u1carr
     C                   endif
     C                   if        qqqtr <> *blank
     C                   movel     qqqtr         u1qtr
     C                   else
     C                   select
     C                   when      qqmth = '01' or qqmth = '02'
     C                             or qqmth = '03'
     C                   movel     '1'           u1qtr
     C                   when      qqmth = '04' or qqmth = '05'
     C                             or qqmth = '06'
     C                   movel     '2'           u1qtr
     C                   when      qqmth = '07' or qqmth = '08'
     C                             or qqmth = '09'
     C                   movel     '3'           u1qtr
     C                   when      qqmth = '10' or qqmth = '11'
     C                             or qqmth = '12'
     C                   movel     '4'           u1qtr
     C                   endsl
     C                   endif
     C                   move      qqyear        u1year
     C                   if        qqftcd = 'O'
     C                   movel(p)  qqfile        u1sbid
     C                   else
     C                   movel(p)  qqlgfl        u1sbid
     C                   endif
     C                   eval      srdti = dsdate
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         u1sbdt
     C                   move      dstime        u1sbtm
     C*** BEG CHG *** 04/13/11 ****************************************
     C                   move      dstime        @@sbtm
     C                   if        @@sbtm1 <> '0'
     C                   eval      u1sbtm  = @@sbtm
     C                   else
     C                   eval      u1sbtm  = @@sbtm2
     C                   endif
     C*** END CHG *** 04/13/11 ****************************************
     C                   move      *all'0'       u1rcds
     C                   move      *blanks       u1fill
     C                   write     pu101
     C* Write Submission Control File - FEU9P
     C* New record would not be written in FEU9P for Deletion
     C                   if        qqftcd <> 'D'
     C                   if        qqftcd = 'O'
     C                   movel(p)  qqfile        u9file
     C                   eval      u9ofil = *blanks
     C                   else
     C                   movel(p)  qqfile        u9file
     C                   movel(p)  qqlgfl        u9ofil
     C                   endif
     C                   movel     u1sbdt        u9sbdt
     C                   move      dstime        u9sbtm
     C                   movel     qqmth         u9mth
     C                   move      u1qtr         u9qtr
     C                   move      qqyear        u9year
     C                   move      *all'0'       u9carr
     C                   movel     qqftcd        u9ftcd
     C                   movel     q1user        u9user
     C                   movel     q1user        u9iusr
     C                   movel     q1pgm         u9ipgm
     C                   movel     q1pgm         u9pgm
     C                   eval      srdti = dsdate
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         u9idt
     C                   movel     srdto         u9dt
     C                   movel     dstime        u9itim
     C                   movel     dstime        u9time
     C                   write     pu901
     C                   endif
     C     enCR          endsr
    C*================================================================
    C*  GETWCG - Subroutine to retrieve the Claim Jurisdiction State
    C*================================================================
     C*
     C     getwcg        begsr
     C*
     C                   if        ##wcif = 'Y' and ##wcin = 'Y'
     C                             and ##npwc <> 'Y'
     C*
     C* Determine if Standalone Account
     C*
     C                   move      qqco#         #1co#             3
     C                   call      'FA701R'
     C                   parm      ' '           ##rtcd            1
     C                   parm                    #1co#
     C                   parm                    ##sub#
     C                   parm                    #1fnd
     C                   parm      ' '           ##outs            1
     C*
     C     ##outs        ifeq      'N'
     C*
     C* Get WCMS Fund Number
     C*
     C                   move      *blanks       ##wcf#            3
     C                   call      'FA700R'
     C                   parm      ' '           ##rtcd
     C                   parm                    #1co#
     C                   parm                    ##wcf#
     C                   parm                    ##sub#
     C                   parm                    #1fnd
     C*
     C                   movel     ##wcf#        #1fnd
     C                   movel     ##wcf#        @@wfnd
     C                   endif                                                  ##OUTS=N
     C*
     C                   endif
     C*
     C*** BEG ADD *** 05/05/11 ****************************************
     C                   movel     #1fnd         ##fnd
     C     cxky          chain     pcx01                              67
     C*** END ADD *** 05/05/11 ****************************************
     C*** BEG ADD *** 01/06/15 ****************************************
      /Free
       chain cxky pgs01;
         if %found(wxgsl);
            *in67 = *off;
         endif;
      /End-Free
     C*** END ADD *** 01/06/15 ****************************************
     C                   endsr
    C*================================================================
    C*  SRPOLINFO- Get Policy/Carrier Information
    C*================================================================
     C     SRPolinfo     begsr
     C                   if        qqftcd = 'O'
     C                   eval      @@ssn# = eass#
     C                   eval      @@sub = easub#
     C                   eval      @@fnd = eafnd
     C                   eval      @@fyr = eafyr
     C                   eval      @@fein = eafein
     C                   eval      @@mod = eamod
     C                   eval      @@eob# = eaeob#
     C                   eval      @@zip = eazip
     C                   eval      @@paid = eapaid
     C                   eval      @@epdt = eaepdt
     C                   eval      @@wcpd = eawcpd
     C                   else
     C                   eval      @@ssn# = e1ss#
     C                   eval      @@sub = e1sub#
     C                   eval      @@fnd = e1fnd
     C                   eval      @@fyr = e1fyr
     C                   eval      @@fein = e1fein
     C                   eval      @@mod = e1mod
     C                   eval      @@eob# = e1eob#
     C                   eval      @@zip = e1zip
     C                   eval      @@paid = e1paid
     C                   eval      @@epdt = e1epdt
     C                   eval      @@wcpd = e1wcpd
     C                   endif
     C                   exsr      SRInitHD
     C* Carrier Code
     C                   if        isWC = 'Y'
     C     f2ky          chain     pf201                              67
     C                   if        *in67 = *off
     C                   move      f2car#        u2carr
     C                   endif
     C* Policy Effective date
     C     elky          chain     pel01                              67
     C                   if        *in67 = *off
     C                   eval      srdti = elpfdt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         u2efdt
     C                   endif
     C* Policy Identifier Number
     C     elky          chain     prp01                              67
     C                   if        *in67 = *off
     C*** Beg Add *** 05/29/13 ****************************************
     C                             and       rpcap# <> *blanks
     C*** End Add *** 05/29/13 ****************************************
     C                   movel     rpcap#        u2pol#
     C                   else
     C*** BEG CHG *** 07/02/10 ****************************************
     C*******            eval      u2pol# = %editC(@@wfnd:'X') + '-' +
     C*******                      %editC(@@emp#:'X') + '-' + %editC(@@fyr:'X')
     C                   eval      u2pol# = %editC(@@wfnd:'X') +
     C                             %editC(@@emp#:'X') + %editC(@@fyr:'X')
     C*** BEG CHG *** 07/02/10 ****************************************
     C                   endif
     C                   endif
     C* Claim Number
     C**                 eval      u2clm# = qqco# + %editC(@@wfnd:'X')
     C**                           + %editC(@@fyr:'X') + %editC(@@case:'X')
     C*** BEG ADD *** 12/17/10 ****************************************
     C                   if        @@clt = '114' and @@wfnd = 600
     C*** Beg Add *** 05/29/13 ****************************************
     C                             and cmacl# <> *blanks
     C*** End Add *** 05/29/13 ****************************************
     C                   movel(p)  cmacl#        u2clm#
     C                   else
     C*** END ADD *** 12/17/10 ****************************************
     C                   eval      u2clm# = %char(@@case)
     C*** Beg Add *** 01/05/15 ****************************************
     C                   eval      U2Clm# = %editc(%dec(U2Clm#:12:0):'X')
     C                   eval      U2Clm# = %subst((U2Clm#):6:7)
     C*** End Add *** 01/05/15 ****************************************
     C*** BEG ADD *** 12/17/10 ****************************************
     C                   endif
     C*** END ADD *** 12/17/10 ****************************************
     C* Transaction Code
     C                   movel     trancd        u2trcd
     C* Jurisdiction Code
     C                   eval      @@tbid = 'JSCD'
     C     gtky          chain     pgt01                              67
     C                   if        *in67 = *off
     C                   movel     gtdesc        u2jscd
     C                   endif
     C* Claimant Gender
     C     cdky          chain     pcd01                              67
     C                   if        *in67 = *off
     C                   if        cdsex = 'M'
     C                   movel     '1'           u2sex
     C                   elseif    cdsex = 'F'
     C                   movel     '2'           u2sex
     C                   else
     C                   movel     '3'           u2sex
     C                   endif
     C* Claimant Birth Year
     C                   eval      srdti = cddtob
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C*** BEG CHG *** 07/13/09 *********************************************
     C******             call      'SRCVTDAT'    dtymd
     C******             eval      u2byr = %subst(srdto:1:4)
     C                   call      'SRCVTDAT'    dtcymd2
     C                   eval      u2byr = %subst(srdto:5:4)
     C*** END CHG *** 07/13/09 *********************************************
     C                   endif
     C* Accident/Injury Date
     C     cmky1         chain     pcm02                              67
     C                   if        *in67 = *off
     C                   eval      srdti = c1acdt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         u2acdt
     C                   endif
     C* Transaction Date
     C                   if        @@paid > 0
     C                   move      @@wcpd        u2trdt
     C                   elseif    @@paid <=0
     C                   move      @@epdt        u2trdt
     C                   endif
     C* Bill Identification Number
     C                   eval      u2eob# = %char(@@eob#)
     C* Provider Type Code
     C     pdky          chain     ppd01                              67
     C                   if        *in67 = *off
     C                   movel     pdtaxo        u2prty
     C                   endif
     C* Provider Identification Number
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                eval      u2fein = %editC(@@fein:'X') +
     C                   eval      u2pid# = %editC(@@fein:'X') +
     C*** End Chg *** 07/18/25 ***************************************
     C                             %editC(@@mod:'X')
     C* Provider Postal Code
     C     caky          chain     peo01                              67
     C                   if        *in67 = *off
     C                   if        eoflic <> *blanks
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                movel     eoflic        u2zip
     C                   movel     eoflic        u2zip9
     C*** End Chg *** 07/18/25 ***************************************
     C                   else
     C                   movel     @@zip         u2zip
     C                   endif
     C                   movel     eobtyp        @@btyp
     C                   else
     C                   movel     @@zip         u2zip
     C                   endif
     C* Line Identification Number
     C     caky          setll     pca01
     C     caky          reade     pca01                                  68
     C                   dow       *in68 = *off
     C                   exsr      SRInitDT
     C                   eval      u2lin# = %char(calin#)
     C                   eval      @@seq = cadseq
     C* Service Date, Service From Date, Service To Date
     C*** BEG ADD *** 09/17/10 ****************************************
     C                   if        cafdat = catdat
     C                   move      *all'0'       u2stdt
     C                   move      *all'0'       u2sfdt
     C*** END ADD *** 09/17/10 ****************************************
     C                   eval      srdti = cafdat
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         u2srdt
     C*** BEG ADD *** 09/17/10 ****************************************
     C                   else
     C                   move      *all'0'       u2srdt
     C*** END ADD *** 09/17/10 ****************************************
     C*** BEG DEL *** 09/17/10 ****************************************
     C***********        if        cafdat = catdat
     C*** END DEL *** 09/17/10 ****************************************
     C                   move      *all'0'       u2stdt
     C                   move      *all'0'       u2sfdt
     C*** BEG DEL *** 09/17/10 ****************************************
     C***********        else
     C*** END DEL *** 09/17/10 ****************************************
     C                   eval      srdti = eofrdt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         u2sfdt
     C                   eval      srdti = eotodt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         u2stdt
     C                   endif
     C* Network Service Code
     C                   if        candis = 0
     C                   move      'N'           u2ntwk
     C                   elseif    candis > 0
     C                   move      'Y'           u2ntwk
     C                   endif
     C*** Beg Add *** 05/24/16 ****************************************
     C                   if        EOFLG8 <> ' '
     C                   eval      u2ntwk =  EOFLG8
     C                   endif
     C*** End Add *** 05/24/16 ****************************************
     C
     C* Amount Charged by Provider
     C                   move      cachrg        u2chrg
     C* Amount Paid
     C                   move      capaid        u2paid
     C* Quantity/Number of Units per Procedure Code
     C*** BEG CHG *** 11/09/10 ****************************************
     C***********        move      caunts        u2unts
     C                   z-add     0             xxunts            7 0
     C                   eval(h)   xxunts = caunts
     C                   move      xxunts        u2unts
     C*** END ADD *** 11/09/10 ****************************************
     C                   select
     C                   when      eobtyp = 'N'
     C                   exsr      SRPhy
     C                   when      eobtyp = 'H'
     C                   exsr      SRHos
     C                   when      eobtyp = 'D'
     C                   exsr      SRDent
     C                   when      eobtyp = 'X'
     C                   exsr      SRPhar
     C                   endsl
     C* Transaction Code
     C                   movel     trancd        u2trcd
     C*** BEG ADD *** 05/05/11 ****************************************
     C*** BEG CHG *** 01/06/15 ****************************************
     C*****              if        cxexnm <> 'Y'
     C                   if        GSieFl = 'I'
     C*** END CHG *** 01/06/15 ****************************************
     C*** END ADD *** 05/05/11 ****************************************
     C                   write     pu201
     C                   eval      cnt = cnt + 1
     C*** BEG ADD *** 05/05/11 ****************************************
     C                   endif
     C*** END ADD *** 05/05/11 ****************************************
     C     caky          reade     pca01                                  68
     C                   enddo
     C                   exsr      SRLogRec
     C                   endsr
    C*================================================================
    C*  SRLOGREC - Write Log Record
    C*================================================================
     C     SRLogRec      begsr
     C*
     C                   eval      nlfile = qqfile
     C                   if        qqftcd = 'O'
     C                   eval      nleob# = eaeob#
     C                   eval      nlrcn = earcn
     C                   else
     C                   eval      nleob# = e1eob#
     C                   eval      nlrcn = e1rcn
     C                   endif
     C*
     C                   movel     qqqtr         nlqtr
     C                   movel     qqyear        nlyear
     C                   eval      srdti = dsdate
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         nlsbdt
     C*** BEG CHG *** 04/13/11 ****************************************
     C***********        movel     dstime        nlsbtm
     C                   move      dstime        nlsbtm
     C*** END CHG *** 04/13/11 ****************************************
     C                   movel     trancd        nltrcd
     C                   eval      nlprfl = 'P'
     C*
     C                   movel     q1user        nliusr
     C                   movel     q1user        nluser
     C                   movel     q1pgm         nlipgm
     C                   movel     q1pgm         nlpgm
     C                   eval      srdti = dsdate
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nlidt
     C                   movel     srdto         nldt
     C                   movel     dstime        nlitim
     C                   movel     dstime        nltime
     C*
     C                   write     pnl01
     C                   endsr
    C*================================================================
    C*  SRPHY - Physician Bill Detail
    C*================================================================
     C     SRPhy         begsr
     C     dhky          chain     pdh01                              67
     c*
b01  C                   if        *in67 = *off
     c*
     C*** Beg Add *** 07/18/25 ***************************************
     C                   movel     dhnpir        u2pid#
     C*** End Add *** 07/18/25 ***************************************
     C* Paid Procedure Code
     C                   movel     dhprcd        u2prcd
     C* Paid Procedure Code Modifier
     C*** BEG CHG *** 09/17/10 ****************************************
     C***********        eval      u2mod = %trim(dhmod1) + %trim(dhmod2)
     C***********                  + %trim(dhmod3) + %trim(dhmod4)
     C                   eval      kkmod1 = *blanks
     C                   eval      kkmod2 = *blanks
     C*** BEG DEL *** 10/26/10 ****************************************
     C***********        eval      kkmod3 = *blanks
     C***********        eval      kkmod4 = *blanks
     C***********        if        dhmod1 = '00' or dhmod1 = ' 0'
     C***********                  or dhmod1 = '0 '
     C***********        eval      kkmod1 = '  '
     C***********        else
     C***********        eval      kkmod1 = dhmod1
     C***********        endif
     C***********        if        dhmod2 = '00' or dhmod2 = ' 0'
     C***********                  or dhmod2 = '0 '
     C***********        eval      kkmod2 = '  '
     C***********        else
     C***********        eval      kkmod2 = dhmod2
     C***********        endif
     C***********        if        dhmod3 = '00' or dhmod3 = ' 0'
     C***********                  or dhmod3 = '0 '
     C***********        eval      kkmod3 = '  '
     C***********        else
     C***********        eval      kkmod3 = dhmod3
     C***********        endif
     C***********        if        dhmod4 = '00' or dhmod4 = ' 0'
     C***********                  or dhmod4 = '0 '
     C***********        eval      kkmod4 = '  '
     C***********        else
     C***********        eval      kkmod4 = dhmod4
     C***********        endif
     C***********        eval      u2mod = %trim(kkmod1) + %trim(kkmod2)
     C***********                  + %trim(kkmod3) + %trim(kkmod4)
     C*** END DEL *** 10/26/10 ****************************************
     C*** BEG ADD *** 10/26/10 ****************************************
     C                   z-add     1             xxcnt             1 0
     C                   z-add     1             i                 1 0
b02  C                   dow       i <= 4
b03  C                   if        pmod(i) <> *blanks
b04  C                   if        pmod(i) = '00' or pmod(i) = ' 0'
     C                             or pmod(i) = '0 '
     C                   movel     *blanks       kkmod(xxcnt)
x04  C                   else
     C                   movel     pmod(i)       kkmod(xxcnt)
     C                   eval      xxcnt = xxcnt + 1
e04  C                   endif
e03  C                   endif
     C                   eval      i = i + 1
b03  C                   if        xxcnt > 2
     C                   leave
e03  C                   endif
e02  C                   enddo
     C*
     C                   movea     kkmod         u2mod
     C*
     C*** END ADD *** 10/26/10 ****************************************
     C*** END CHG *** 09/17/10 ****************************************
     C* Place of Service
     C                   move      dhpos         @@pos             2
     C                   movel     @@pos         u2pos
     C* Secondary Procedure Code
     C*** BEG CHG *** 09/17/10 ****************************************
     C*******            movel     dhbprc        u2bprc
     C                   move      *blanks       u2bprc
     C*** END CHG *** 09/17/10 ****************************************
     C* Primary Diagnostic Code
     C*
     C     caky          chain     peh01                              69
     C*
b02  C                   if        *in69 = *off
     C*
     C*** Beg Chg *** 03/16/15 ****************************************
     C*
b03  C                   if        EHICDIND <> 'Y'
     C*
     C*** End Chg *** 03/16/15 ****************************************
     C*
b04  C                   if        *in69 = *off
     C                   eval      icd91 = getDiag(ehdia1)
     C                   eval      icd92 = getDiag(ehdia2)
     C                   eval      icd93 = getDiag(ehdia3)
     C                   eval      icd94 = getDiag(ehdia4)
e04  C                   endif
     C*
b04  C                   if        icd91 <> *blanks
     C                   movel     icd91         u2pdia
e04  C                   endif
b04  C                   if        icd92 <> *blanks and u2pdia = *blanks
     C                   movel     icd92         u2pdia
x04  C                   else
     C                   movel     icd92         u2sdia
e04  C                   endif
b04  C                   if        icd93 <> *blanks
b05  C                   if        u2pdia = *blanks
     C                   movel     icd93         u2pdia
x05  C                   elseif    u2sdia = *blanks
     C                   movel     icd93         u2sdia
e05  C                   endif
e04  C                   endif
b04  C                   if        icd94 <> *blanks
b05  C                   if        u2pdia = *blanks
     C                   movel     icd94         u2pdia
x05  C                   elseif    u2sdia = *blanks
     C                   movel     icd94         u2sdia
e05  C                   endif
e04  C                   endif
     C*
     C*** Beg Chg *** 03/16/15 ****************************************
     C*
x03  C                   Else
     C*
b04  C                   if        *in69 = *off
     C                   eval      icd101 = getDiag10(EHicd1001)
     C                   eval      icd102 = getDiag10(EHicd1002)
     C                   eval      icd103 = getDiag10(EHicd1003)
     C                   eval      icd104 = getDiag10(EHicd1004)
e04  C                   endif
     C*
b04  C                   if        icd101 <> *blanks
     C                   movel     icd101        u2pdia
e04  C                   endif
     C*
b04  C                   if        icd102 <> *blanks and u2pdia = *blanks
     C                   movel     icd102        u2pdia
x04  C                   else
     C                   movel     icd102        u2sdia
e04  C                   endif
     C*
b04  C                   if        icd103 <> *blanks
b05  C                   if        u2pdia = *blanks
     C                   movel     icd103        u2pdia
x05  C                   elseif    u2sdia = *blanks
     C                   movel     icd103        u2sdia
e05  C                   endif
e04  C                   endif
b04  C                   if        icd105 <> *blanks
b05  C                   if        u2pdia = *blanks
     C                   movel     icd104        u2pdia
x05  C                   elseif    u2sdia = *blanks
     C                   movel     icd104        u2sdia
e05  C                   endif
     C*
e04  C                   endif
     C*
e03  C                   endif
     C*
     C*** End Chg *** 03/16/15 ****************************************
     C*
e02  C                   endif
     C*
e01  C                   endif
     C*
     C                   endsr
    C*================================================================
    C*  SRHOS - Hospital Bill Detail
    C*================================================================
     C     SRHos         begsr
     C     dhky          chain     pdu01                              67
     C                   if        *in67 = *off
     C* Paid Procedure Code
     C                   if        durate <> *blanks
     C                   movel     durate        u2prcd
     C                   else
     C*** BEG CHG *** 11/03/10 ****************************************
     C***********        movel     durvcd        u2prcd
     C                   z-add     0             ##rvcd
     C                   move      durvcd        ##rvcd            4 0
     C                   movel     ##rvcd        u2prcd
     C*** END CHG *** 11/03/10 ****************************************
     C                   endif
     C* Paid Procedure Code Modifier
     C*** BEG CHG *** 09/17/10 ****************************************
     C***********        eval      u2mod = %trim(dupmod) + %trim(dumod2)
     C*** BEG DEL *** 10/26/10 ****************************************
     C***********        eval      kkmod1 = ' '
     C***********        eval      kkmod2 = ' '
     C***********        if        dupmod = '00' or dupmod = '0 '
     C***********                  or dupmod = ' 0'
     C***********        eval      kkmod1 = *blanks
     C***********        else
     C***********        eval      kkmod1 = dupmod
     C***********        endif
     C***********        if        dumod2 = '00' or dumod2 = '0 '
     C***********                  or dumod2 = ' 0'
     C***********        eval      kkmod2 = ' '
     C***********        else
     C***********        eval      kkmod2 = dumod2
     C***********        endif
     C***********        eval      u2mod = %trim(kkmod1) + %trim(kkmod2)
     C*** END DEL *** 10/26/10 ****************************************
     C*** BEG ADD *** 10/26/10 ****************************************
     C                   eval      kkmod1 = *blanks
     C                   eval      kkmod2 = *blanks
     C                   z-add     1             xxcnt             1 0
     C                   z-add     1             i                 1 0
     C                   dow       i <= 2
     C                   if        hmod(i) <> *blanks
     C                   if        hmod(i) = '00' or hmod(i) = ' 0'
     C                             or hmod(i) = '0 '
     C                   movel     *blanks       kkmod(xxcnt)
     C                   else
     C                   movel     hmod(i)       kkmod(xxcnt)
     C                   eval      xxcnt = xxcnt + 1
     C                   endif
     C                   endif
     C                   eval      i = i + 1
     C                   if        xxcnt > 2
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   movea     kkmod         u2mod
     C*** END CHG *** 09/17/10 ****************************************
     C* Place of Service
     C                   move      dupos         @@pos
     C                   movel     @@pos         u2pos
     C* Secondary Procedure Code
     C*** BEG CHG *** 09/17/10 ****************************************
     C*******            if        dubrat <> *blanks
     C*******            movel     dubrat        u2bprc
     C*******            else                                                   dubrat<>*blanks
     C*******            movel     dubprc        u2bprc
     C*******            endif                                                  dubrat<>*blanks
     C                   move      *blanks       u2bprc
     C*** END CHG *** 09/17/10 ****************************************
     C* Primary Diagnostic Code
     C*
     C     caky          chain     peu01                              69
     C*
     C                   if        *in69 = *off
     C*** Beg Add *** 07/18/25 ***************************************
     C                   movel     eunpi1        u2pid#
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*** Beg Chg *** 03/16/14 ****************************************
     C*
     C                   if        EUICDIND <> 'Y'
     C*
     C*** End Chg *** 03/16/14 ****************************************
     C*
     C                   eval      icd91 = getDiag(eudia1)
     C                   eval      icd92 = getDiag(eudia2)
     C                   movel     icd91         u2pdia
     C                   movel     icd92         u2sdia
     C*
     C*** Beg Chg *** 03/16/14 ****************************************
     C*
     C                   Else
     C*
     C                   eval      icd101 = getDiag10(EUicd1001)
     C                   eval      icd102 = getDiag10(EUicd1002)
     C                   movel     icd101        u2pdia
     C                   movel     icd102        u2sdia
     C*
     C                   endif
     C*
     C*** End Chg *** 03/16/14 ****************************************
     C*
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C*  SRDENT - Dental Bill Detail
    C*================================================================
     C     SRDent        begsr
     C     dhky          chain     pdd01                              67
     C                   if        *in67 = *off
     C*** Beg Add *** 07/18/25 ***************************************
     C     epky          chain     ped01                              67
     C                   if        *in67 = *off
     C                   movel     edtnpi        u2pid#
     C                   endif
     C*** End Add *** 07/18/25 ***************************************
     C* Paid Procedure Code
     C                   movel     ddprcd        u2prcd
     C* Paid Procedure Code Modifier
     C                   move      *blanks       u2mod
     C* Place of Service
     C                   movel     '11'          u2pos
     C* Secondary Procedure Code
     C*** BEG CHG *** 09/17/10 ****************************************
     C*******            movel     ddbprc        u2bprc
     C                   move      *blanks       u2bprc
     C*** END CHG *** 09/17/10 ****************************************
     C                   endif
     C                   endsr
    C*================================================================
    C*  SRPHAR - Pharmacy Bill Detail
    C*================================================================
     C     SRPhar        begsr
     C     dhky          chain     pdp01                              67
     C                   if        *in67 = *off
     C*** Beg Add *** 07/18/25 ***************************************
     C     epky          chain     pep01
     C                   if        %found
     C                   movel     epdnpi        u2pid#
     C                   endif
     C*** End Add *** 07/18/25 ***************************************
     C* Paid Procedure Code
     C                   if        dpndc <> *blanks
     C                   movel     dpndc         u2prcd
     C                   else
     C                   movel     dphcpc        u2prcd
     C                   endif
     C* Paid Procedure Code Modifier
     C                   move      *blanks       u2mod
     C* Place of Service
     C                   movel     '01'          u2pos
     C* Secondary Procedure Code
     C*** BEG CHG *** 09/17/10 ****************************************
     C*******            if        dpbndc <> *blanks
     C*******            movel     dpbndc        u2bprc
     C*******            else                                                   dpbndc<>*blanks
     C*******            movel     dphcpc        u2bprc
     C*******            endif                                                  dpbndc<>*blanks
     C                   move      *blanks       u2bprc
     C*** END CHG *** 09/17/10 ****************************************
     C                   endif
     C*
     C*** Beg Chg *** 05/03/11 ****************************************
     C                   if        @@clt  = '133'
     C     epky          chain     pep01
     C                   if        %found
     C                             and eontwk = 'PMOA'
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                eval      u2fein = epphar
     C                   eval      u2pid# = epphar
     C*** End Chg *** 07/18/25 ***************************************
     C                   endif
     C                   endif
     C*** End Chg *** 05/03/11 ****************************************
     C*
     C                   endsr
    C*================================================================
    C*  SRINITHD - Initialize Header Fields
    C*================================================================
     C     SRInitHd      begsr
     C                   move      *all'0'       u2carr
     C                   move      *blanks       u2pol#
     C                   move      *all'0'       u2efdt
     C                   move      *blanks       u2clm#
     C                   move      *all'0'       u2jscd
     C                   move      *blank        u2sex
     C                   move      *all'0'       u2byr
     C                   move      *all'0'       u2acdt
     C                   move      *blank        u2eob#
     C                   move      *all'0'       u2sfdt
     C                   move      *all'0'       u2stdt
     C                   move      *blank        u2prty
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                move      *blank        u2fein
     C                   move      *blank        u2pid#
     C*** End Chg *** 07/18/25 ***************************************
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                move      *all'0'       u2zip
     C                   move      *all'0'       u2zip9
     C*** End Chg *** 07/18/25 ***************************************
     C                   move      *blank        u2ntwk
     C                   move      *blank        u2pos
     C                   move      *all'0'       u2trcd
     C                   move      *all'0'       u2trdt
     C                   endsr
    C*================================================================
    C*  SRINITDT - Initialize Detail Fields
    C*================================================================
     C     SRInitDT      begsr
     C                   move      *blank        u2lin#
     C                   move      *all'0'       u2srdt
     C                   move      *blank        u2prcd
     C                   move      *blank        u2mod
     C                   move      *all'0'       u2chrg
     C                   move      *all'0'       u2paid
     C                   move      *blank        u2pdia
     C                   move      *blank        u2sdia
     C                   move      *all'0'       u2unts
     C                   move      *blank        u2bprc
     C*** BEG DEL *** 07/13/09 *********************************************
     C*******            move      *all'0'       u2byr
     C*** END DEL *** 07/13/09 *********************************************
     C                   move      *blank        u2fill
     C                   endsr
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
     C*** Beg Add *** 03/16/15 *********************************************
     P*=====================================================================
     P* Procedure getDiag10 - Returns diagnosis code with decimal
     P*=====================================================================
     P
     P getDiag10       B
     D*** Beg Chg *** 07/05/16 *******************************************
     D***** getDiag10       PI             7
     D getDiag10       PI             8
     D*** End Chg *** 07/05/16 *******************************************
     D  icd10                         7
     C*
     C     icd10         chain     fmicd10l1                          99
     C*
     C                   if        not *in99
     C                   return    icfcode
     C                   else
     C                   return    *blanks
     C                   endif
     C
     P getDiag10       E
     C*** End Add *** 03/16/15 *********************************************
