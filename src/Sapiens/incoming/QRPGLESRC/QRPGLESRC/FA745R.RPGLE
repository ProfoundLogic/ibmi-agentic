    H*****************************************************************
    H*
    H*(C) Copyright Progressive Data Solutions, Inc., 1997-2007
    H*
     H dftactgrp(*no) actgrp(*caller)
    H/TITLE FA745R - Provider Bill Error API
    H*
    H*****************************************************************
    H*                                                               *
    H*  CREATED: 02/16/07                                            *
    H*           C:188 PROJECT: 10MJC - EDI Edits (P2P Import)       *
    H*                                                               *
    H*****************************************************************
    H* REVISIONS:                                                    *
    H*                                                               *
     H*  06/21/07 C:188 Project: 10MKR                                *
    H*             Add edits: 1) Rx HCPC code (DPHCPC)               *
    H*                        2) Purchase/Rental Indicator (DPPRIN)  *
    H*                                                               *
    H*****************************************************************
    H* INDICATOR USAGE                                               *
    H*                                                               *
    H*****************************************************************
     Ffdelp     o    e           k disk
     Ffaeop     if   e             disk
     Ffdcal5    if   e           k disk
     Ffaeul     if   e           k disk
    Ffddhl     if   e           k disk
    Ffddul     if   e           k disk
    Ffdddl     if   e           k disk
    Ffddpl     if   e           k disk
     F*
     Fftpsl     if   e           k disk
     Fftabl     if   e           k disk    ignore(PPS01)
    F*** BEG DEL *** 06/21/07 ****************************************
     F***** ststp     if   e           k disk
     F***** ftedp     if   e           k disk
     F***** fmsvl1    if   e           k disk
    F*** END DEL *** 06/21/07 ****************************************
     Ffmrxl     if   e           k disk
     Fftgtl     if   e           k disk
    F*** BEG DEL *** 06/21/07 ****************************************
     F***** ftgtp     if   e             disk    rename(pgt01:pgt02)
    F*** END DEL *** 06/21/07 ****************************************
    F*** BEG ADD *** 06/21/07 ****************************************
     Ffmprl     if   e           k disk
    F*** END ADD *** 06/21/07 ****************************************
     F*
    D*
     D*================================================================
     D*  Field Declarations
     D*================================================================
    D*
     D psds          esds                  extname(szq1p)
    D*
     D kkco#           S                   like(EOCO#)
     D kksub#          S                   like(EOSUB#)
     D kkfnd           S                   like(EOFND)
     D kkfyr           S                   like(EOFYR)
     D kkcase          S                   like(EOCASE)
     D kkeob#          S                   like(EOEOB#)
     D kkdseq          S                   like(cadseq)
     D kkpscd          S                   like(pspos)
     D kkkey           s                   like(cskey)
     D k1efdt          s                   like(srdto )
     D kktbid          s                   like(gttbid)
     D kkkey1          s                   like(gtkey)
     D kkndc           s                   like(dpndc)
     D ERRcode         s                   like(elcode)
     D ERRfld          s                   like(elfld)
     D ERRvalu         s                   like(elvalu)
    D*** BEG ADD *** 06/21/07 ****************************************
     D kkprcd          s                   like(prcpt4)
     D kkfdat          s                   like(prefdt)
    D*** END ADD *** 06/21/07 ****************************************
     D ##co#           S              3  0
     D ##fnd           S              3  0
     D ##conm          S             40
     D ##fnam          S             40
     D ##sub#          S              5  0
     D ##fyr           S              3  0
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm:1)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
    D comapi          C                   const('FA007R')
    D*
     Dk1cmsv           ds
     D k1rvcd                  1      3
     D k1rate                  4      8
     D k1unts                  9     11
    D*
     C*================================================================
     C*  KLIST/PLIST definitions
     C*================================================================
    C*
    C* KEY LIST FOR DETAIL CHARGE FILE
    C*
     C     caky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
    C*
    C* KEY LIST FOR TRANSACTION DETAIL FILES
    C*
     C     ddky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    kkdseq
    C*
    C* KEY LIST FOR PLACE OF SERVICE
    C*
     C     psky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkpscd
     C*
     C* GENERIC TABLE KEY
     C*
     C     tbky          klist
     C                   kfld                    ##CO#
     C                   kfld                    kkkEY
     C*
     C* GENERIC TABLE KEY
     C*
     C     gtky          klist
     C                   kfld                    ##CO#
     C                   kfld                    kktbid
     C                   kfld                    kkkey1
     C*
     C     gtky2         klist
     C                   kfld                    ##CO#
     C                   kfld                    kktbid
     C*
     C*  Key list for procedure code file
     C*
     C     rxky          klist
     C                   kfld                    ##CO#
     C                   kfld                    kkndc
     C*
     C* Key list for Service Codes File
     C*
     C     svky1         klist
     C                   kfld                    ##CO#
     C                   kfld                    k1cmsv
     C                   kfld                    k1efdt
     C*
     C     svky2         klist
     C                   kfld                    ##CO#
     C                   kfld                    k1cmsv
     C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
     C*
     C     datck2        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      *blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*  KEY LIST FOR PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
    C*** END ADD *** 03/14/07 ****************************************
     C*
     C     *entry        plist
     C                   parm                    @@mode            1
     C                   parm                    @@KL              1
     C                   parm                    @@rrn             9 0
     C                   parm                    @@pgm            10
     C                   parm                    @@pgm2           10
     C*
     C*================================================================
     C*  Main
     C*================================================================
    C*
     C* Initialize and get FAEOP record from RRN passed to program.
     C*
     C                   exsr      srinit
    C                   exsr      geteop
    C*
    C* Loop through detail. Check for errors based on bill type, write errors to FDELP.
    C*
     C                   if        *in90 = *off
    C                   exsr      getcap
     C                   endif
    C*
     C                   dow       *in90 = *off
     C                   clear     *all          pel01
    C*
     C                   select
     C                   when      eobtyp = 'N'
     C                   exsr      edtphy
     C                   when      eobtyp = 'H'
     C                   exsr      edthos
     C                   when      eobtyp = 'D'
     C                   exsr      edtden
     C                   when      eobtyp = 'X'
     C                   exsr      edtpha
     C                   endsl
    C*
     C     caky          reade     pca01                                  90
     C                   enddo
    C*
     C                   eval      *inlr = *on
    C*
     C*================================================================
     C* srinit - Initialization
     C*================================================================
    C*
     C     srinit        begsr
    C*
    C                   call      comapi        compl
    C                   eval      kkco# = ##co#
    C*
     C                   endsr
    C*
     C*================================================================
     C* geteop - Get FAEOP
     C*================================================================
    C*
     C     geteop        begsr
    C*
     C     @@rrn         chain     PEO01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* getcap - Get FDCAP
     C*================================================================
    C*
     C     getcap        begsr
     C*
     C     caky          chain     pca01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* getdhp - Get FDDHP - Physican
     C*================================================================
    C*
     C     getdhp        begsr
    C*
     C                   eval      kkdseq = cadseq
     C*
     C     ddky          chain     pdh01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* geteup - Get FAEUP- Hospital Header
     C*================================================================
    C*
     C     geteup        begsr
    C*
     C     caky          chain     peu01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* getdup - Get FDDUP - Hospital
     C*================================================================
    C*
     C     getdup        begsr
    C*
     C                   eval      kkdseq = cadseq
     C*
     C     ddky          chain     pdu01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* getddp - Get FDDHP - Dental
     C*================================================================
    C*
     C     getddp        begsr
    C*
     C                   eval      kkdseq = cadseq
     C*
     C     ddky          chain     pdd01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* getdpp - Get FDDPP - Pharmacy
     C*================================================================
    C*
     C     getdpp        begsr
    C*
     C                   eval      kkdseq = cadseq
     C*
     C     ddky          chain     pdp01                              90
    C*
     C                   endsr
    C*
     C*================================================================
     C* edtphy - Edit Physician billtype
     C*================================================================
    C*
     C     edtphy        begsr
    C*
    C* Get physician detail
    C*
     C                   exsr      getdhp
    C*
    C* From-date must not be zero, validate.
    C*
     C                   if        cafdat = *zeros
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'CAFDAT'
     C                   eval      ERRvalu = *blanks
     C                   exsr      wrterr
     C                   else
     C*
     C                   eval      srdti = cafdat
     C                   eval      srdto = *blanks
     C                   eval      srerr = *blanks
     C                   call      'SRCVTDAT'    datck2
     C                   if        srerr =  '1'
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'CAFDAT'
     C                   move      cafdat        ERRvalu
     C                   exsr      wrterr
     C                   else
    C*
    C* From-date cannot be less than accident date
    C*
     C                   if        cafdat < eoacdt
     C                   eval      ERRcode = 'D02'
     C                   eval      ERRfld = 'CAFDAT'
     C                   move      cafdat        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C                   endif
    C*
    C* Validate place of service
    C*
     C                   eval      kkco# = ##co#
     C                   move      dhpos         kkpscd
     C     psky          chain     pps01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and PSSTAT = 'I')
     C                   eval      ERRcode = 'P01'
     C                   eval      ERRfld = 'DHPOS'
     C                   eval      ERRvalu = dhpos
     C                   exsr      wrterr
     C                   endif
    C*
    C* Procedure code cannot be blanks
     C*
     C                   if        dhprcd = *blanks
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DHPRCD'
     C                   eval      ERRvalu = dhprcd
     C                   exsr      wrterr
     C                   endif
     C*
     C*  Alabama specific
     C*
     C                   if        eoast = 'AL'
     C*
     C                   select
     C*
     C*   If surgery code, body part S1-S5 is required
     C*
     C                   when      dhprcd >= '10000' and dhprcd <= '69999' and
     C                             dharea <> 'S1' and
     C                             dharea <> 'S2' and
     C                             dharea <> 'S3' and
     C                             dharea <> 'S4' and
     C                             dharea <> 'S5'
     C                   eval      ERRcode = 'A01'
     C                   eval      ERRfld = 'DHAREA'
     C                   eval      ERRvalu = dharea
     C                   exsr      wrterr
     C*
     C*   If this is an ANES code, body part A1-A4 is required
     C*
     C                   when      dhprcd >= '00100' and dhprcd <= '01999' and
     C                             dharea <> 'A1' and
     C                             dharea <> 'A2' and
     C                             dharea <> 'A3' and
     C                             dharea <> 'A4'
     C                   eval      ERRcode = 'A01'
     C                   eval      ERRfld = 'DHAREA'
     C                   eval      ERRvalu = dharea
     C                   exsr      wrterr
     C*
     C                   when      dhprcd = '97752' and
     C                             dharea <> '01' and
     C                             dharea <> '02' and
     C                             dharea <> '03' and
     C                             dharea <> '04' and
     C                             dharea <> '05' and
     C                             dharea <> '06' and
     C                             dharea <> '7A' and
     C                             dharea <> '7B' and
     C                             dharea <> '7C' and
     C                             dharea <> '7D' and
     C                             dharea <> '7E' and
     C                             dharea <> '7F' and
     C                             dharea <> '7G'
     C                   eval      ERRcode = 'A01'
     C                   eval      ERRfld = 'DHAREA'
     C                   eval      ERRvalu = dharea
     C                   exsr      wrterr
     C*
     C                   endsl
     C*
     C                   endif
     C*
     C* Validate modifier 1
    C*
     C                   if        dhmod1 <> *blanks
     C                   eval      kkkey = dhmod1
     C     tbky          chain     ppm01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and pmstat = 'I')
     C                   eval      ERRcode = 'M01'
     C                   eval      ERRfld = 'DHMOD1'
     C                   eval      ERRvalu = dhmod1
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C* Validate modifier 2
    C*
     C                   if        dhmod2 <> *blanks
     C                   eval      kkkey = dhmod2
     C     tbky          chain     ppm01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and pmstat = 'I')
     C                   eval      ERRcode = 'M01'
     C                   eval      ERRfld = 'DHMOD2'
     C                   eval      ERRvalu = dhmod2
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C* Validate modifier 3
    C*
     C                   if        dhmod3 <> *blanks
     C                   eval      kkkey = dhmod3
     C     tbky          chain     ppm01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and pmstat = 'I')
     C                   eval      ERRcode = 'M01'
     C                   eval      ERRfld = 'DHMOD3'
     C                   eval      ERRvalu = dhmod3
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C* Validate modifier 4
    C*
     C                   if        dhmod4 <> *blanks
     C                   eval      kkkey = dhmod4
     C     tbky          chain     ppm01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and pmstat = 'I')
     C                   eval      ERRcode = 'M01'
     C                   eval      ERRfld = 'DHMOD4'
     C                   eval      ERRvalu = dhmod4
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C* One of the diagnostic cross-references field must be selected with an 'X'
    C*
     C                   if        DHDXR1 <> 'X'  and
     C                             DHDXR2 <> 'X'  and
     C                             DHDXR3 <> 'X'  and
     C                             DHDXR4 <> 'X'
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DHDXR1'
     C                   eval      ERRvalu = dhdxr1
     C                   exsr      wrterr
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DHDXR2'
     C                   eval      ERRvalu = dhdxr2
     C                   exsr      wrterr
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DHDXR3'
     C                   eval      ERRvalu = dhdxr3
     C                   exsr      wrterr
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DHDXR4'
     C                   eval      ERRvalu = dhdxr4
     C                   exsr      wrterr
     C                   endif
     C*
     C* Charge Amount must be greater than zero
     C*
     C                   if        cachrg <= 0
     C                   eval      ERRcode = 'B01'
     C                   eval      ERRfld = 'CACHRG'
     C                   move      cachrg        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C*
     C* Units of the charges must be greater than zero
     C*
     C                   if        caunts < 1
     C                   eval      ERRcode = 'U01'
     C                   eval      ERRfld = 'CAUNTS'
     C                   move      caunts        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C*
     C* If procedure code = 96370, must have NDC code
     C*
     C                   if        dhprcd = '96370' and dhndc = *blanks
     C                   eval      ERRcode = 'N01'
     C                   eval      ERRfld = 'DHNDC'
     C                   eval      ERRvalu = dhndc
     C                   exsr      wrterr
     C                   endif
     C*
     C* Validate NDC code if present
     C*
     C                   if        dhndc <> *blanks
     C                   eval      kkndc = dhndc
     C     rxky          chain     prx01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'C03'
     C                   eval      ERRfld = 'DHNDC'
     C                   eval      ERRvalu = dhndc
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*
     C*================================================================
     C* edthos - Edit Hospital billtype
     C*================================================================
    C*
     C     edthos        begsr
    C*
    C* Get hospital header
    C*
     C                   exsr      geteup
    C*
    C* Get hospital detail
    C*
     C                   exsr      getdup
    C*
    C* From-date must not be zero and a valid date.
    C*
     C                   if        cafdat = *zeros
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'CAFDAT'
     C                   move      cafdat        ERRvalu
     C                   exsr      wrterr
     C                   else
     C*
     C                   eval      srdti = cafdat
     C                   eval      srdto = *blanks
     C                   eval      srerr = *blanks
     C                   call      'SRCVTDAT'    datck2
     C                   if        srerr =  '1'
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'CAFDAT'
     C                   move      cafdat        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C                   endif
    C*
    C* Validate place of service
    C*
     C                   eval      kkco# = ##co#
     C                   move      dupos         kkpscd
     C     psky          chain     pps01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and PSSTAT = 'I')
     C                   eval      ERRcode = 'P01'
     C                   eval      ERRfld = 'DUPOS'
     C                   eval      ERRvalu = dupos
     C                   exsr      wrterr
     C                   endif
    C*
    C* Must have at least one: Revenue code / HCPCS code
    C*
     C                   if        durvcd = *zeros and durate = *blanks
     C                   eval      ERRcode = 'C04'
     C                   eval      ERRfld = 'DURVCD'
     C                   move      durvcd        ERRvalu
     C                   exsr      wrterr
     C                   endif
    C*
    C* Validate procedure modifier
    C*
     C                   if        dupmod <> *blanks
     C                   eval      kkkey = dupmod
     C     tbky          chain     PPM01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'M01'
     C                   eval      ERRfld = 'DUPMOD'
     C                   eval      ERRvalu = dupmod
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C* Charge Amount must be greater than zero
     C*
     C                   if        cachrg <= 0
     C                   eval      ERRcode = 'B01'
     C                   eval      ERRfld = 'CACHRG'
     C                   move      cachrg        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C*
     C* Units of the charges must be greater than zero
     C*
     C                   if        caunts < 1
     C                   eval      ERRcode = 'U01'
     C                   eval      ERRfld = 'CAUNTS'
     C                   move      caunts        ERRvalu
     C                   exsr      wrterr
     C                   endif
    C*
    C*   Florida specific
    C*
     C                   if        eoast = 'FL'                                 beg FL spec
    C*
     C                   if        euicn >= 'AO' and euicn <= 'HS' and
     C                             durvcd = *zeros and durate = *blanks
     C                   eval      ERRcode = 'C04'
     C                   eval      ERRfld = 'DURATE'
     C                   move      durate        ERRvalu
     C                   exsr      wrterr
     C                   else
     C                   if        euicn >= 'IM' and euicn <= 'IS' and
     C                             durvcd = *zeros
     C                   eval      ERRcode = 'C04'
     C                   eval      ERRfld = 'DURVCD'
     C                   move      durvcd        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C                   endif
    C*
     C                   endif                                                  end FL specific
    C*
     C                   endsr
    C*
     C*================================================================
     C* edtden - Edit Dental billtype
     C*================================================================
    C*
     C     edtden        begsr
    C*
    C* Get dental detail
    C*
     C                   exsr      getddp
    C*
    C* From-date must not be zero and a valid date.
    C*
     C                   if        cafdat = *zeros
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'CAFDAT'
     C                   move      cafdat        ERRvalu
     C                   exsr      wrterr
     C                   else
     C*
     C                   eval      srdti = cafdat
     C                   eval      srdto = *blanks
     C                   eval      srerr = *blanks
     C                   call      'SRCVTDAT'    datck2
     C                   if        srerr =  '1'
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'CAFDAT'
     C                   move      cafdat        ERRvalu
     C                   exsr      wrterr
     C                   else
    C*
    C* Validate place of service
    C*
     C                   eval      kkco# = ##co#
     C                   move      ddpos         kkpscd
     C     psky          chain     pps01                              99
     C                   if        (*in99 = *on) or
     C                             (*in99 = *off and PSSTAT = 'I')
     C                   eval      ERRcode = 'P01'
     C                   eval      ERRfld = 'DDPOS'
     C                   eval      ERRvalu = ddpos
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C                   endif
    C*
    C* Procedure code cannot be blanks
     C*
     C                   if        ddprcd = *blanks
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DDPRCD'
     C                   eval      ERRvalu = ddprcd
     C                   exsr      wrterr
     C                   endif
    C*
     C* Charge Amount must be greater than zero
     C*
     C                   if        cachrg <= 0
     C                   eval      ERRcode = 'B01'
     C                   eval      ERRfld = 'CACHRG'
     C                   move      cachrg        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C*
     C* One of the diagnostic cross-references field must be selected with an 'X'
    C*
     C                   if        DDDXR1 <> 'X'  and
     C                             DDDXR2 <> 'X'  and
     C                             DDDXR3 <> 'X'  and
     C                             DDDXR4 <> 'X'
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DDDXR1'
     C                   eval      ERRvalu = dddxr1
     C                   exsr      wrterr
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DDDXR2'
     C                   eval      ERRvalu = dddxr2
     C                   exsr      wrterr
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DDDXR3'
     C                   eval      ERRvalu = dddxr3
     C                   exsr      wrterr
     C                   eval      ERRcode = 'X01'
     C                   eval      ERRfld = 'DDDXR4'
     C                   eval      ERRvalu = dddxr4
     C                   exsr      wrterr
     C                   endif
    C*
     C                   endsr
    C*
     C*================================================================
     C* edtpha - Edit Pharmacy billtype
     C*================================================================
    C*
     C     edtpha        begsr
    C*
    C* Get pharmacy detail
    C*
     C                   exsr      getdpp
    C*
    C* Fill-date must not be zero and a valid date.
    C*
     C                   if        dprxfd = *zeros
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPRXFD'
     C                   move      dprxfd        ERRvalu
     C                   exsr      wrterr
     C                   else
     C*
     C                   eval      srdti = dprxfd
     C                   eval      srdto = *blanks
     C                   eval      srerr = *blanks
     C                   call      'SRCVTDAT'    datck2
     C                   if        srerr =  '1'
     C                   eval      ERRcode = 'D01'
     C                   eval      ERRfld = 'DPRXFD'
     C                   move      dprxfd        ERRvalu
     C                   exsr      wrterr
     C                   else
    C*
    C* Fill-date cannot be less accident date
    C*
     C                   if        dprxfd < eoacdt
     C                   eval      ERRcode = 'X02'
     C                   eval      ERRfld = 'DPRXFD'
     C                   move      dprxfd        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C                   endif
    C*
     C* Validate NDC code
     C*
     C                   if        dpndc = *blanks
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPNDC'
     C                   eval      ERRvalu = dpndc
     C                   exsr      wrterr
     C                   else
     C                   eval      kkndc = dpndc
     C     rxky          chain     prx01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'C03'
     C                   eval      ERRfld = 'DPNDC'
     C                   eval      ERRvalu = dpndc
     C                   exsr      wrterr
     C                   endif
     C                   endif
    C*
     C* Charge Amount must be greater than zero
     C*
     C                   if        cachrg <= 0
     C                   eval      ERRcode = 'B01'
     C                   eval      ERRfld = 'CACHRG'
     C                   move      cachrg        ERRvalu
     C                   exsr      wrterr
     C                   endif
    C*
     C* Rx Metric Qty must be greater than zero
     C*
     C                   if        dprxfd <= 0
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPRXMQ'
     C                   move      dprxmq        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C*
     C*** BEG ADD *** 06/21/07 ****************************************
     C*
     C* Validate Rx HCPC Code
     C*
     C                   if        dprxpt = 'S'                                 dprxpt = 'S'
     C*
     C                   if        dphcpc = *blanks                             cannot be blank
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPHCPC'
     C                   eval      ERRvalu = dphcpc
     C                   exsr      wrterr
     C                   else
     C*
     C                   movel(p)  dphcpc        kkprcd                         validate
     C                   z-add     dprxfd        srdti
     C                   move      *blanks       SRDTO
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkfdat
     C*
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  99
     C                   if        *in99 = *on or dphcpc < 'A0001'
     C                             or dphcpc > 'Z9999'
     C                   eval      ERRcode = 'C01'
     C                   eval      ERRfld = 'DPHCPC'
     C                   eval      ERRvalu = dphcpc
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C                   else                                                   dprxpt <> 'S'
     C                   if        dphcpc <> *blanks                            must be blank
     C                   eval      ERRcode = 'K01'
     C                   eval      ERRfld = 'DPHCPC'
     C                   eval      ERRvalu = dphcpc
     C                   exsr      wrterr
     C                   endif
     C                   endif                                                  end dprxpt
     C
     C*
     C* Validate Purchase / Rental Indicator
     C*
     C                   if        dprxpt = 'S'                                 dpprin = 'S'
     C*
     C                   if        dpprin = *blanks                             cannot be blank
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPPRIN'
     C                   eval      ERRvalu = dpprin
     C                   exsr      wrterr
     C                   else
     C*
     C                   eval      kkkey1 = dpprin                              validate
     C                   eval      kktbid = 'FLPR'
     C     gtky          chain     pgt01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'C05'
     C                   eval      ERRfld = 'DPPRIN'
     C                   eval      ERRvalu = dpprin
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C*
     C                   else                                                   dprxpt <> 'S'
     C                   if        dpprin <> *blanks                            must be blank
     C                   eval      ERRcode = 'K01'
     C                   eval      ERRfld = 'DPPRIN'
     C                   eval      ERRvalu = dpprin
     C                   exsr      wrterr
     C                   endif
     C                   endif                                                  end dprxpt
     C*
     C*** END ADD *** 06/21/07 ****************************************
     C*
     C* FLORIDA specific edits
     C*
     C                   if        eoast = 'FL'                                 beg FL specific
     C*
     C*   RX prescription number cannot be blank
     C*
     C                   if        dprx# = *blanks
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPRX#'
     C                   eval      ERRvalu = dprx#
     C                   exsr      wrterr
     C                   endif
     C*
     C*   Days supply cannot be zero
     C*
     C                   if        dprxdy <= *zeros
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPRXDY'
     C                   move      dprxdy        ERRvalu
     C                   exsr      wrterr
     C                   endif
     C*
     C*   Validate new/refill code
     C*
     C                   if        dpnwrf = *blanks and dprxpt = 'D'
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPNWRF'
     C                   eval      ERRvalu = dpnwrf
     C                   exsr      wrterr
     C                   else
     C                   if        dpnwrf <> *blanks
     C                   eval      kkkey1 = dpnwrf
     C                   eval      kktbid = 'FLFI'
     C     gtky          chain     PGT01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'F02'
     C                   eval      ERRfld = 'DPNWRF'
     C                   eval      ERRvalu = dpNWRF
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C                   endif
     C*
     C*   Validate certification code
     C*
     C                   if        dpcert = *blanks and dprxpt = 'D'
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPCERT'
     C                   eval      ERRvalu = dpcert
     C                   exsr      wrterr
     C                   else
     C                   if        dpcert <> *blanks
     C                   eval      kkkey1 = dpcert
     C                   eval      kktbid = 'FLCT'
     C     gtky          chain     PGT01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'F01'
     C                   eval      ERRfld = 'DPCERT'
     C                   eval      ERRvalu = dpcert
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C                   endif
     C*
     C*   Validate payment type
     C*
     C                   if        dprxpt = *blanks
     C                   eval      ERRcode = 'F03'
     C                   eval      ERRfld = 'DPRXPT'
     C                   eval      ERRvalu = dprxpt
     C                   exsr      wrterr
     C                   else
     C                   if        dprxpt <> *blanks
     C                   eval      kkkey1 = dprxpt
     C                   eval      kktbid = 'FLPT'
     C     gtky          chain     PGT01                              99
     C                   if        *in99 = *on
     C                   eval      ERRcode = 'F03'
     C                   eval      ERRfld = 'DPCERT'
     C                   eval      ERRvalu = dpcert
     C                   exsr      wrterr
     C                   endif
     C                   endif
     C                   endif
    C*
    C*   Prescriber's license number cannot be blank.
    C*
     C                   if        dplic# = *blanks
     C                   eval      ERRcode = 'Z01'
     C                   eval      ERRfld = 'DPLIC#'
     C                   eval      ERRvalu = dplic#
     C                   exsr      wrterr
     C                   endif
     C*
     C                   endif                                                  end FL specific
     C*
     C                   endsr
     C*
     C*================================================================
     C* wrterr - Write error to FDELP
     C*================================================================
     C*
     C     wrterr        begsr
     C*
     C                   eval      eleob# = caeob#
     C                   eval      eldseq = cadseq
     C                   eval      ellin# = calin#
     C                   eval      elast  = eoast
     C                   eval      elbtyp = eobtyp
     C                   eval      elcode = ERRcode
     C                   eval      elfld  = ERRfld
     C                   eval      elvalu = ERRvalu
     C                   eval      elnval = *blanks
     C                   eval      elprfl = *blanks
     C                   eval      elprdt = *loval
     C                   eval      elupld = *loval
     C                   eval      elstat = CASTAT
     C                   eval      eluser = *blanks
     C                   eval      elpgm  = *blanks
     C                   eval      eltime = *loval
     C                   eval      eldt   = *loval
     C                   eval      eliUSR = q1user
     C                   eval      elipgm = q1pgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   eval      srdto = *blanks
     C                   eval      srerr = *blanks
     C                   call      'SRCVTDAT'    dtcymd
     C                   eval      elitim = dstime
     C                   movel     srdto         elidt
     C*
     C                   write     pel01
     C*
     C                   endsr
     C*
