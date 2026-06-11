     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H*    NCCI Medical Data Call Report Program                      *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/2016 by C:308    Project C114MWK            *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 6/24/16 C: 079 Proj: 114MWK                                   *
     H*                Change to write all groups to FMNCP            *
     H*                                                               *
     H*****************************************************************
    F*
    F* Medical Data Call Record File
     Ffev2p     uf a e             disk
     Ffev2l2    if   e           k disk    rename(pv201:pv202)
    F*
    F* Medical Data Call Log Record File
     Ffenlp     uf a e             disk
     Ffenll1    if   e           k disk    rename(pnl01:pnl02) prefix(n1:2)
     Ffenll3    if   e           k disk    rename(pnl01:pnl04) prefix(n2:2)
    F*
    F* Medical Data Call Extract Flat File
     Ffev0p     o    e             disk
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
     Fwmcxl     if   e           k disk    usropn
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
     Ffaepl     if   e           k disk
     Fwxgsl     if   e           k disk    usropn
     FFMICD10L1 if   e           k disk
    F*
    Ffmncp     uf a e           k disk
    Ffmncl3    if   e           k disk    Rename(pnc01:pnc02)
    F*
    F* ICD10-CM/PCS Code by code/ind
     F*
     D getDiag         PR             7
     D  icd9                          5
     D*
     D getDiag10       PR             8
     D  icd10                         7
     D*
    D*================================================================
    D*  Variables
    D*================================================================
     D icd91           s              6
     D icd92           s              6
     D icd93           s              6
     D icd94           s              6
     D icd101          s              8
     D icd102          s              8
     D icd103          s              8
     D icd104          s              8
     D icd105          s              8
     D qqco#           s              3
     D qqfdt           s              8
     D qqfdt5          s              5  0
     D qqtdt           s              8
     D qqtdt5          s              5  0
     D qqmth           s              2
     D qqqtr           s              1
     D qqyear          s              4
     D qqfile          s             30
     D qqfdt8          s              8  0
     D qqfdt7          s              7
     D qqtdt8          s              8  0
     D qqtdt7          s              7
     D SbtDATE         s              7  0
     D MMYYDte6        s              6
     D MMDte2          s              2
     D YYDte4          s              4
     D
    D*  Date/Time Data Structure.
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D @@sbtm                         6
     D  @@sbtm1                1      1
     D  @@sbtm2                2      6
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
     D kxdiag          s                   like(iccode)
     D isProcess       s              1a
     D newGroup        s                   like(eafnd)
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
     C                   parm                    qqfile
     C                   parm                    qqqtr
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
    C*
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C* Key List for FAEAL1
     C*
     C     eaky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@eob#
     C                   kfld                    @@rcn
     C*
     C* Key List for FMNCL3
     C     fnky          klist
     C                   kfld                    eaCO#
     C                   kfld                    eafnd
     C                   kfld                    qqfdt5
     C                   kfld                    qqtdt5
     C*
     C* Key List for FMNCL1
     C     fv2ky         klist
     C                   kfld                    qqfdt5
     C                   kfld                    qqtdt5
     C                   kfld                    eaFnd
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
     C* Key List for the Diagnosis code File
     C*
     C     diky          klist
     C                   kfld                    kkdiag
     C*
     C* Key List for Claim/DARTS Control Definition File
     C*
     C     cxky          klist
     C                   kfld                    @@co#
     C                   kfld                    ##fnd             3 0
     C*
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
     C* Key List for state file.
     C*
     C     stFlKey       klist
     C                   kfld                    @@co#
     C                   kfld                    @@jrst
     C*
     C*
     C* Current Date and Time
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       @@CLT             3
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
     C                   open      wmcxl
     C                   open      wxgsl
     C                   eval      isWC = 'Y'
     C                   else
     C                   eval      isWC = 'N'
     C                   endif
     C*
     C                   exsr      SROrigSub
     C*
     C                   if        ##wcif = 'Y' and ##npwc <> 'Y'
     C                   close     wmcml1
     C                   close     wdell1
     C                   close     wdf2l
     C                   close     wmrpl4
     C                   close     wmcxl
     C                   close     wxgsl
     C                   endif
     C                   endsr
    C*================================================================
    C*  SRORIGSUB - NCCI Original Submission Subroutine
    C*================================================================
     C     SROrigSub     begsr

      //*** Beg Add 06/24/16 **************************************
      //** Write all group records to FMNCP
      /Free
                      qqfdt8 = %Dec(qqfdt:8:0);
                      qqfdt7 = %char(%date(qqfdt8:*iso):*cymd0);

                      qqtdt8 = %Dec(qqtdt:8:0);
                      qqtdt7 = %char(%date(qqtdt8:*iso):*cymd0);
                      SbtDATE = %Dec(%char(%date(dsdate:*USA):*cymd0):7:0);


                         Read pgs01;
                         dow not %eof(wxgsl);
                           if GSieFl = 'I';
                             exsr srWrite;
                           endif;
                           Read pgs01;
                         enddo;
                         Close wxgsl;
                         Open wxgsl;
      /End-Free
      //*** End Add 06/24/16 **************************************
     C                   eval      newGroup = 0
     C* Read EOMB Transaction File
     C                   read      pea01                                  66
      //*** Beg Del 06/24/16 **************************************
      //***/Free
      //***  qqfdt8 = %Dec(qqfdt:8:0);
      //***  qqfdt7 = %char(%date(qqfdt8:*iso):*cymd0);

      //***  qqtdt8 = %Dec(qqtdt:8:0);
      //***  qqtdt7 = %char(%date(qqtdt8:*iso):*cymd0);

      //***  SbtDATE = %Dec(%char(%date(dsdate:*USA):*cymd0):7:0);
      //***/End-Free
     C                   dow       *in66 = *off
     C                   movel     easub#        ##sub#            5
     C                   movel     eafnd         #1fnd             3
     C* Get Worker's Compensation Group and claim jurisdiction state
     C                   exsr      getwcg
     C                   if        *in50 = *On  and GSieFl = 'I'
     C
     C                   movel     qqfdt7        qqfdt5
     C                   movel     qqtdt7        qqtdt5
     C
     C                   if        EAfnd <> newGroup
     C                   eval      newGroup = eafnd
     C                   eval      isProcess = 'Y'
     C
     C*** Beg Del *** 06/24/16 ***********************************
     C*****
     C**Get the FMNCP records using Co# and Grp
     C*****fnky          chain     pnc02                              96
     C*****              if        *in96 = *On
     C*****              exsr      SrWrite
     C*****              else
     C*****              eval      isProcess = 'N'
     C*****              endif
     C*** End Del *** 06/24/16 ***********************************
     C*
     C                   endif
     C* Get Claim Jurisdiction State.
     C                   if        isProcess = 'Y'
     C*
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
     C* Set Transaction Code
     C                   eval      kkeob# = eaeob#
     C                   movel     '01'          trancd            2
     C* Get Policy/carrier Information
     C                   exsr      SRPolInfo
     C                   Endif
     C                   endif
     C                   read      pea01                                  66
     C                   enddo
     C                   exsr      SRUpdRcds
     C                   endsr
    C*================================================================
    C*  SRREPLSUB - NCCI File Replacement Subroutine
    C*================================================================
     C     SRReplSub     begsr
     C                   endsr
    C*================================================================
    C*  SRDELSUB - NCCI File Deletion Subroutine
    C*================================================================
     C     SRDelSub      begsr
     C                   endsr
    C*================================================================
    C*  SRUPDRCDS - Update Record Count & Process Status
    C*================================================================
     C     SRUpdRcds     begsr
     C                   endsr
     C*================================================================
     C*  SRFLATFL  - Update Flat File
     C*================================================================
     C     SRFlatFl      begsr
     C                   endsr
    C*================================================================
    C*  SRWRITE - Write Control Record
    C*================================================================
     C     SRWrite       begsr
     C*** Beg Chg *** 06/24/16 ****************************************
    C************       move      EACO#         NCCO#
    C************       move      EAFND         NCFND
    C                   move      GSCO#         NCCO#
    C                   move      GSFND         NCFND
     C*** End Chg *** 06/24/16 ****************************************
    C                   movel     qqfdt7        qqfdt5
    C                   move      qqfdt5        NCRPYMTHF
    C                   movel     qqtdt7        qqtdt5
    C                   move      qqtdt5        NCRPYMTHT
     C                   Exsr      getQtrSr
     C                   movel     SbtDATE       NCSBDT
    C                   move      'P'           NCPRST
    C                   move      'E'           NCEXTDTA
     C*** Beg Chg *** 06/24/16 ****************************************
    C****************   move      EASTAT        NCSTAT
    C                   move      'A'           NCSTAT
     C*** End Chg *** 06/24/16 ****************************************
    C                   move      q1pgm         NCPGM
    C                   move      q1user        NCUSER
    C                   move      SbtDATE       NCDT
    C                   move      dstime        NCTIME
    C                   move      q1pgm         NCIPGM
    C                   move      q1user        NCIUSR
    C                   move      SbtDATE       NCIDT
    C                   move      dstime        NCITIM
    C                   Write     pnc01
     C                   endsr
    C*================================================================
    C*  GETQTR - Subroutine to retrieve NCRPLVL & NCRPMTH values
    C*================================================================
     C*
     C     getqtrsr      begsr
     C                   movel     qqtdt8        MMYYDte6
     C                   move      MMYYDte6      MMDte2
     C                   movel     MMYYDte6      YYDte4
     C     MMDte2        CAT       YYDte4:0      MMYYDte6
      /Free
        NCRPMTH = %Dec(MMYYDte6:6:0);
      /End-Free
     C                   If        qqqtr = 'Y'
     C                   Movel     'Q'           NCRPLVL
     C                   Else
     C                   Movel     'M'           NCRPLVL
     C                   endif
     C                   endsr
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
     C                   movel     #1fnd         ##fnd
     C     cxky          chain     pcx01                              67
      /Free
       *in50 = *off;
       chain cxky pgs01;
         if %found(wxgsl);
            *in67 = *off;
            *in50 = *on;
         endif;
      /End-Free
     C                   endsr
    C*================================================================
    C*  SRPOLINFO- Get Policy/Carrier Information
    C*================================================================
     C     SRPolinfo     begsr
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
     C                   exsr      SRInitHD
     C* Carrier Code
     C                   if        isWC = 'Y'
     C     f2ky          chain     pf201                              67
     C                   if        *in67 = *off
     C                   move      f2car#        v2carr
     C                   endif
     C* Policy Effective date
     C     elky          chain     pel01                              67
     C                   if        *in67 = *off
     C                   eval      srdti = elpfdt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         v2efdt
     C                   endif
     C* Policy Identifier Number
     C     elky          chain     prp01                              67
     C                   if        *in67 = *off
     C                             and       rpcap# <> *blanks
     C                   movel     rpcap#        v2pol#
     C                   else
     C                   eval      v2pol# = %editC(@@wfnd:'X') +
     C                             %editC(@@emp#:'X') + %editC(@@fyr:'X')
     C                   endif
     C                   endif
     C* Claim Number
     C                   if        @@clt = '114' and @@wfnd = 600
     C                             and cmacl# <> *blanks
     C                   movel(p)  cmacl#        v2clm#
     C                   else
     C                   eval      v2clm# = %char(@@case)
     C                   eval      v2Clm# = %editc(%dec(v2Clm#:12:0):'X')
     C                   eval      v2Clm# = %subst((v2Clm#):6:7)
     C                   endif
     C* Transaction Code
     C                   movel     trancd        v2trcd
     C* Jurisdiction Code
     C                   eval      @@tbid = 'JSCD'
     C     gtky          chain     pgt01                              67
     C                   if        *in67 = *off
     C                   movel     gtdesc        v2jscd
     C                   endif
     C* Claimant Gender
     C     cdky          chain     pcd01                              67
     C                   if        *in67 = *off
     C                   if        cdsex = 'M'
     C                   movel     '1'           v2sex
     C                   elseif    cdsex = 'F'
     C                   movel     '2'           v2sex
     C                   else
     C                   movel     '3'           v2sex
     C                   endif
     C* Claimant Birth Year
     C                   eval      srdti = cddtob
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtcymd2
     C                   eval      v2byr = %subst(srdto:5:4)
     C                   endif
     C* Accident/Injury Date
     C     cmky1         chain     pcm02                              67
     C                   if        *in67 = *off
     C                   eval      srdti = c1acdt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         v2acdt
     C                   endif
     C* Transaction Date
     C                   if        @@paid > 0
     C                   move      @@wcpd        v2trdt
     C                   elseif    @@paid <=0
     C                   move      @@epdt        v2trdt
     C                   endif
     C* Bill Identification Number
     C                   eval      v2eob# = %char(@@eob#)
     C* Provider Type Code
     C     pdky          chain     ppd01                              67
     C                   if        *in67 = *off
     C                   movel     pdtaxo        v2prty
     C                   endif
     C* Provider Identification Number
     C                   eval      v2fein = %editC(@@fein:'X') +
     C                             %editC(@@mod:'X')
     C* Provider Postal Code
     C     caky          chain     peo01                              67
     C                   if        *in67 = *off
     C                   if        eoflic <> *blanks
     C                   movel     eoflic        v2zip
     C                   else
     C                   movel     @@zip         v2zip
     C                   endif
     C                   movel     eobtyp        @@btyp
     C                   else
     C                   movel     @@zip         v2zip
     C                   endif
     C* Line Identification Number
     C     caky          setll     pca01
     C     caky          reade     pca01                                  68
     C                   dow       *in68 = *off
     C                   exsr      SRInitDT
     C                   eval      v2lin# = %char(calin#)
     C                   eval      @@seq = cadseq
     C* Service Date, Service From Date, Service To Date
     C                   if        cafdat = catdat
     C                   move      *all'0'       v2stdt
     C                   move      *all'0'       v2sfdt
     C                   eval      srdti = cafdat
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         v2srdt
     C                   else
     C                   move      *all'0'       v2srdt
     C                   move      *all'0'       v2stdt
     C                   move      *all'0'       v2sfdt
     C                   eval      srdti = eofrdt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         v2sfdt
     C                   eval      srdti = eotodt
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         v2stdt
     C                   endif
     C* Network Service Code
     C                   if        candis = 0
     C                   move      'N'           v2ntwk
     C                   elseif    candis > 0
     C                   move      'Y'           v2ntwk
     C                   endif
     C* Amount Charged by Provider
     C                   move      cachrg        v2chrg
     C* Amount Paid
     C                   move      capaid        v2paid
     C* Quantity/Number of Units per Procedure Code
     C                   z-add     0             xxunts            7 0
     C                   eval(h)   xxunts = caunts
     C                   move      xxunts        v2unts
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
     C                   movel     trancd        v2trcd
     C                   if        GSieFl = 'I'
     C                   Movel     qqfdt7        qqfdt5
     C                   Movel     qqfdt5        V2RPYMTHF
     C                   Movel     qqtdt7        qqtdt5
     C                   Movel     qqtdt5        V2RPYMTHT
     C                   Eval      V2FND = EAfnd
     C                   Move      'E'           V2EXTDTA
     C                   write     pv201
     C                   eval      cnt = cnt + 1
     C                   endif
     C     caky          reade     pca01                                  68
     C                   enddo
     C                   endsr
    C*================================================================
    C*  SRPHY - Physician Bill Detail
    C*================================================================
     C     SRPhy         begsr
     C     dhky          chain     pdh01                              67
     c*
b01  C                   if        *in67 = *off
     c*
     C* Paid Procedure Code
     C                   movel     dhprcd        v2prcd
     C* Paid Procedure Code Modifier
     C                   eval      kkmod1 = *blanks
     C                   eval      kkmod2 = *blanks
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
     C                   movea     kkmod         v2mod
     C* Place of Service
     C                   move      dhpos         @@pos             2
     C                   movel     @@pos         v2pos
     C* Secondary Procedure Code
     C                   move      *blanks       v2bprc
     C* Primary Diagnostic Code
     C*
     C     caky          chain     peh01                              69
     C*
b02  C                   if        *in69 = *off
     C*
b03  C                   if        EHICDIND <> 'Y'
     C*
b04  C                   if        *in69 = *off
     C                   eval      icd91 = getDiag(ehdia1)
     C                   eval      icd92 = getDiag(ehdia2)
     C                   eval      icd93 = getDiag(ehdia3)
     C                   eval      icd94 = getDiag(ehdia4)
e04  C                   endif
     C*
b04  C                   if        icd91 <> *blanks
     C                   movel     icd91         v2pdia
e04  C                   endif
b04  C                   if        icd92 <> *blanks and v2pdia = *blanks
     C                   movel     icd92         v2pdia
x04  C                   else
     C                   movel     icd92         v2sdia
e04  C                   endif
b04  C                   if        icd93 <> *blanks
b05  C                   if        v2pdia = *blanks
     C                   movel     icd93         v2pdia
x05  C                   elseif    v2sdia = *blanks
     C                   movel     icd93         v2sdia
e05  C                   endif
e04  C                   endif
b04  C                   if        icd94 <> *blanks
b05  C                   if        v2pdia = *blanks
     C                   movel     icd94         v2pdia
x05  C                   elseif    v2sdia = *blanks
     C                   movel     icd94         v2sdia
e05  C                   endif
e04  C                   endif
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
     C                   movel     icd101        v2pdia
e04  C                   endif
     C*
b04  C                   if        icd102 <> *blanks and v2pdia = *blanks
     C                   movel     icd102        v2pdia
x04  C                   else
     C                   movel     icd102        v2sdia
e04  C                   endif
     C*
b04  C                   if        icd103 <> *blanks
b05  C                   if        v2pdia = *blanks
     C                   movel     icd103        v2pdia
x05  C                   elseif    v2sdia = *blanks
     C                   movel     icd103        v2sdia
e05  C                   endif
e04  C                   endif
b04  C                   if        icd105 <> *blanks
b05  C                   if        v2pdia = *blanks
     C                   movel     icd104        v2pdia
x05  C                   elseif    v2sdia = *blanks
     C                   movel     icd104        v2sdia
e05  C                   endif
     C*
e04  C                   endif
     C*
e03  C                   endif
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
     C                   movel     durate        v2prcd
     C                   else
     C                   z-add     0             ##rvcd
     C                   move      durvcd        ##rvcd            4 0
     C                   movel     ##rvcd        v2prcd
     C                   endif
     C* Paid Procedure Code Modifier
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
     C                   movea     kkmod         v2mod
     C* Place of Service
     C                   move      dupos         @@pos
     C                   movel     @@pos         v2pos
     C* Secondary Procedure Code
     C                   move      *blanks       v2bprc
     C* Primary Diagnostic Code
     C*
     C     caky          chain     peu01                              69
     C*
     C                   if        *in69 = *off
     C*
     C                   if        EUICDIND <> 'Y'
     C*
     C                   eval      icd91 = getDiag(eudia1)
     C                   eval      icd92 = getDiag(eudia2)
     C                   movel     icd91         v2pdia
     C                   movel     icd92         v2sdia
     C*
     C                   Else
     C*
     C                   eval      icd101 = getDiag10(EUicd1001)
     C                   eval      icd102 = getDiag10(EUicd1002)
     C                   movel     icd101        v2pdia
     C                   movel     icd102        v2sdia
     C*
     C                   endif
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
     C* Paid Procedure Code
     C                   movel     ddprcd        v2prcd
     C* Paid Procedure Code Modifier
     C                   move      *blanks       v2mod
     C* Place of Service
     C                   movel     '11'          v2pos
     C* Secondary Procedure Code
     C                   move      *blanks       v2bprc
     C                   endif
     C                   endsr
    C*================================================================
    C*  SRPHAR - Pharmacy Bill Detail
    C*================================================================
     C     SRPhar        begsr
     C     dhky          chain     pdp01                              67
     C                   if        *in67 = *off
     C* Paid Procedure Code
     C                   if        dpndc <> *blanks
     C                   movel     dpndc         v2prcd
     C                   else
     C                   movel     dphcpc        v2prcd
     C                   endif
     C* Paid Procedure Code Modifier
     C                   move      *blanks       v2mod
     C* Place of Service
     C                   movel     '01'          v2pos
     C                   move      *blanks       v2bprc
     C                   endif
     C*
     C                   if        @@clt  = '133'
     C     epky          chain     pep01
     C                   if        %found
     C                             and eontwk = 'PMOA'
     C                   eval      v2fein = epphar
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C*  SRINITHD - Initialize Header Fields
    C*================================================================
     C     SRInitHd      begsr
     C                   move      *all'0'       v2carr
     C                   move      *blanks       v2pol#
     C                   move      *all'0'       v2efdt
     C                   move      *blanks       v2clm#
     C                   move      *all'0'       v2jscd
     C                   move      *blank        v2sex
     C                   move      *all'0'       v2byr
     C                   move      *all'0'       v2acdt
     C                   move      *blank        v2eob#
     C                   move      *all'0'       v2sfdt
     C                   move      *all'0'       v2stdt
     C                   move      *blank        v2prty
     C                   move      *blank        v2fein
     C                   move      *all'0'       v2zip
     C                   move      *blank        v2ntwk
     C                   move      *blank        v2pos
     C                   move      *all'0'       v2trcd
     C                   move      *all'0'       v2trdt
     C                   endsr
    C*================================================================
    C*  SRINITDT - Initialize Detail Fields
    C*================================================================
     C     SRInitDT      begsr
     C                   move      *blank        v2lin#
     C                   move      *all'0'       v2srdt
     C                   move      *blank        v2prcd
     C                   move      *blank        v2mod
     C                   move      *all'0'       v2chrg
     C                   move      *all'0'       v2paid
     C                   move      *blank        v2pdia
     C                   move      *blank        v2sdia
     C                   move      *all'0'       v2unts
     C                   move      *blank        v2bprc
     C                   move      *blank        v2fill
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
     P*=====================================================================
     P* Procedure getDiag10 - Returns diagnosis code with decimal
     P*=====================================================================
     P
     P getDiag10       B
     D getDiag10       PI             8
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
