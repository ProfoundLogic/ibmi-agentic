     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H* ***************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO MAINTAINING THE TREATING**
     H**     PHYSICIAN HISTORY FILE.                                 **
    H*
     H Bnddir('WTBNDDIR')
     H* CREATED BY:   220                                             *
     H* PROJECT:      25WPV             03/26/10                      *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* ***************************************************************
    H*
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw4330f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmtpp     uf a e             disk
     F                                     rename(ptp01:dbrcdf)
     Fsmcol     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmpdl     if   e           k disk
     Fwmcml1    if   e           k disk
     F                                     infds(dbfds)
     Fwmupl     if   e           k disk
     Fwmeml     if   e           k disk
      *  System Control
     Fssylf     if   e           k disk
     F                                     ignore(spc01)
     F                                     ignore(sfb01)
     F                                     ignore(src01)
     F                                     ignore(sdr01)
     F                                     ignore(sbc01)
     F                                     ignore(scs01)
     F                                     ignore(sgl01)
     F                                     ignore(sp201)
     F*  GENERIC TABLE FILE
     Fwtgtl     if   e           k disk
     F*  CO#,FND,FYR,CASE
     Fwmtpl     uf a e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Treating Physician History'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
     D xcbc01          c                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D xcbc02          c                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D xcexre          c                   CONST('EXCS REIN ')
    D*---------------------------------------------------------------------
     D dbfds           ds
     D  dbrrn                397    400B 0
    D*---------------------------------------------------------------------
     D                 ds
     D  cmss#                  1      9
     D  ss1                    1      3
     D  ss2                    4      5
     D  ss3                    6      9
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
     C                   parm                    qqseq#            5
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     tpky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    sccase
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     syky          klist
     C                   kfld                    xxsyk             2
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     emky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C*
     C*  CLAIM MASTER DEMOGRAPHIC FILE
     C*
     C     cdky          klist
     C                   kfld                    xxss#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     coky          klist
     C                   kfld                    xxco#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C*
     C     upky          klist
     C                   kfld                    q1user
     C*
     C     gtky          klist
     C                   kfld                    xxco#             3 0
     C                   kfld                    xxtbid
     C                   kfld                    xxkey            10
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHICS FILE
     C*
     C     pdkey         klist
     C                   kfld                    xxfein
     C                   kfld                    xxmod
     C*
      *
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
     C*  IF THE PHYSICIAN FEIN ENTERED, VALIDATE
     C*
     C     scphyf        ifne      *zero
     C                   move      scphyf        xxfein
     C                   move      scphym        xxmod
     C     pdkey         chain     ppd01                              22
     C     *in22         ifeq      *off
     C     pdstat        andne     'A'
     C     pdstat        andne     *blanks
     C                   seton                                        2290
     C                   goto      err1nd
     C                   endif
     C                   endif
     C*
     C* SKIP THIS EDIT IF THE FEIN SEARCH WAS CHOSEN.
     C* CAN NOT ENTER BOTH FEIN/MOD AND PHYSICIAN NAME
     C*
     C     *in32         ifeq      *off
     C     scphyf        ifne      *zeros
     C     scpnam        andne     *blank
     C                   seton                                        4790
     C                   goto      err1nd
     C                   endif
     C                   endif
     C*
     C* PHYSICIAN/HEALTHCARE PROVIDER IS REQUIRED.
     C*
     C     scphyf        ifeq      *zeros
     C     scpnam        andeq     *blank
     C                   seton                                        3190
     C                   endif
     C*
     C                   movel     cmco#         xxco#
     C                   movel     'PHCD'        xxtbid
     C                   movel     scprol        xxkey
     C     gtky          chain     pgt01                              60
     C     *in60         ifeq      *on
     C     scprol        andne     *blanks
     C                   seton                                        4390      POS CUSR
     C                   goto      err1nd
     C                   endif
      *
     C                   if        scedat <> 'Y' and scedat <> 'N'
     C
     C                   seton                                        4590      POS CUSR
     C                   goto      err1nd
     C                   endif
     C*
     C*  FROM DATE IS REQUIRED
     C*
     C     scfrdt        ifeq      *zero
     C                   seton                                        2790
     C                   goto      err1nd
     C                   endif
     C*
     C*  FROM DATE MUST BE A VALID DATE
     C*
     C     scfrdt        ifne      *zeros
     C*
     C                   z-add     scfrdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scfrdt
     C*
     C     srerr         ifeq      *ON
     C                   seton                                        2890
     C                   goto      err1nd
     C                   endif
     C                   endif
     C*
     C*  TO DATE MUST BE A VALID DATE
     C*
     C     sctodt        ifne      *zeros
     C*
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sctodt
     C*
     C     srerr         ifeq      *ON
     C                   seton                                          2990
     C                   goto      err1nd
     C                   endif
     C                   endif
     C*
     C*  FROM DATE MAY NOT BE GREATER THAN TO DATE
     C*
     C                   z-add     scfrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxfrom
     C*
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxto
     C*
     C     sctodt        ifgt      *zero
     C     xxfrom        andgt     xxto
     C                   seton                                          3090
     C                   goto      err1nd
     C                   endif
     C*
     C                   if        @@mode = 'A'
     C     tpky          setgt     ptp01
     C     tpky          readpe    ptp01                                  60
     C                   move      tpseq#        xxseq#            5 0
     C                   endif
     C*
     C*  TO DATE(END DATE) Should not be blan in more than one record.
     C*
     C     tpky          chain     ptp01                              60
     C*
     C                   dow       *in60 = *off
     C                   if        tptodt= *zeros and sctodt = *zeros
     C                             and @@mode = 'A'
     C                             and tpedat = 'Y'
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C*
     C                   movel     srdto         tptodt
     C*
     C                   movel     q1user        tpuser
     C                   movel     q1pgm         tppgm
     C                   z-add     dstime        tptime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tpdt
     C*
     C                   update    ptp01
     C                   goto      err1nd
     C                   endif
     C*
     C     tpky          reade     ptp01                                  60
     C                   enddo
     C*
     C*
     C     err1nd        endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   z-add     tpphyf        scphyf
     C                   z-add     tpphym        scphym
     C                   movel     tpprol        scprol
     C                   movel     tpphyn        scpnam
     C                   movel     tpphya        scadr1
     C                   movel     tpphyc        sccty
     C                   movel     tpphys        scst
     C                   movel     tpphyz        sczip1
     C                   move      tpphyr        scphyr
     C                   movel     tpphyp        scppn1
     C                   move      tpphyp        scppn2
     C                   movel     tpfxar        scfxar
     C                   movel     tpfax#        scfax1
     C                   move      tpfax#        scfax2
     C*
     C                   z-add     tpfrdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scfrdt
     C*
     C* Default to 'N' for client = 025 else 'Y'
     C*
     C                   movel     tpedat        scedat
     C*
     C                   if        @@mode = 'A'
     C                   if        ##clt# = '025'
     C                   movel     'N'           scedat
     C                   movel     *zeros        sctodt
     C                   else
     C                   movel     'Y'           scedat
     C                   endif                                                   client
     C                   endif                                                   @@mode
     C*
     C                   z-add     tptodt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   if        scedat = 'Y'
     C                   movel     srdto         sctodt
     C                   else                                                    scedat = 'Y'
     C                   movel     *blanks       sctodt
     C                   endif                                                   scedat = 'Y'
     C*
     C                   movel     tpclin        scclin
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     qqco#         tpco#
     C                   movel     qqfnd         tpfnd
     C                   movel     qqfyr         tpfyr
     C                   movel     qqcase        tpcase
     C*
     C                   z-add     scphyf        tpphyf
     C                   z-add     scphym        tpphym
     C                   movel     scprol        tpprol
     C                   movel     scpnam        tpphyn
     C                   movel     scadr1        tpphya
     C                   movel     sccty         tpphyc
     C                   movel     scst          tpphys
     C                   movel     sczip1        tpphyz
     C                   move      scphyr        tpphyr
     C                   movel     scppn1        tpphyp
     C                   move      scppn2        tpphyp
     C                   movel     scfxar        tpfxar
     C                   movel     scfax1        tpfax#
     C                   move      scfax2        tpfax#
     C*
     C                   z-add     scfrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tpfrdt
     C*
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C*
     C                   if        scedat = 'Y'
     C*
     C                   movel     srdto         tptodt
     C                   endif
     C*
     C                   movel     scedat        tpedat
     C                   movel     scclin        tpclin
     C*
     C                   if        @@mode = 'A'
     C*
     C                   movel     'A'           tpstat
     C*
     C                   eval      tpseq# = xxseq# + 1
     C*
     C                   endif
     C*
     C* Populate maint fields
     C*
     C                   movel     q1user        tpuser
     C                   movel     q1user        tpiusr
     C                   movel     q1pgm         tppgm
     C                   movel     q1pgm         tpipgm
     C                   z-add     dstime        tptime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tpdt
     C                   movel     srdto         tpidt
     C*
     C
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
      * F10 = TABLE SEARCH
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     *like         define    cdss#         xxss#
     C     *like         define    tpphyf        xxfein
     C     *like         define    tpphym        xxmod
     C     *like         define    tpdt          xxto
     C     *like         define    tpdt          xxfrom
     C*
     C                   clear                   scedn
     C                   clear                   scfnd
     C                   clear                   xcfyr
     C                   clear                   sccase
     C                   clear                   xxseq#
     C*
     C*  Retrieve client ID
     C*
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
     C
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     upky          chain     pup01                              60
     C     *in60         ifeq      '1'
     C                   move      *zero         UPCO#
     C                   endif
     C                   move      UPCO#         XXCO#
     C*
     C                   movel     qqfyr         xxfyr             3 0
     C                   z-add     xxfyr         xcfyr             3 0
     C                   movel     qqfnd         scfnd
     C                   movel     qqcase        sccase            7 0
     C                   movel     qqco#         xxco#             3 0
     C                   movel     qqfnd         xxfnd             3 0
     C                   movel     qqfyr         xxfyr             3 0
     C                   movel     qqcase        xxcase            7 0
     C*
     C*  GET THE CLAIMS MASTER (SOC SEC NUMBER)
     C*
     C     cmky          chain     pcm01                              60
     C                   move      cmss#         xxss#
     C                   z-add     dbrrn         @1rrn             9 0
     C*
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt
     C*
     C*  GET THE CLAIMANT NAME
     C*
     C     cdky          chain     pcd01                              60
     C     *in60         ifeq      '0'
     C                   move      *blanks       qqfmn            15
     C                   move      *blanks       qqfsn            15
     C                   move      *blanks       qqmid            56
     C                   move      '1'           qqterm            1
     C                   movel     cdfsn         qqfsn
     C                   movel     cdmid         qqmid
     C                   call      'SRFRLS'
     C                   parm                    qqmid
     C                   parm                    qqfsn
     C                   parm                    qqterm
     C                   movel     qqmid         qqfmn
     C*
     C                   move      '1'           qqterm
     C                   move      *blanks       qqlsn            56
     C                   movel     cdlsn         qqlsn
     C                   call      'SRNAME'
     C                   parm                    qqlsn
     C                   parm                    qqfmn
     C                   parm                    qqterm
     C                   movel     qqlsn         scclnm
     C                   else
     C                   move      *blanks       scclnm
     C                   move      *zeros        scss#
     C                   endif
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     coky          chain     pco01                              60
     C                   movel(P)  coname        srdfld
     C                   call      'SRCENTER'
     C                   parm                    srdfld           80
     C                   parm      40            srdlen            3 0
     C                   movel     srdfld        scconm
     C     fnky          chain     pfn01                              60
     C                   movel     xxfnd         scfnd
     C                   movel     fnname        scname
     C     xcfyr         ifeq      *zero
     C                   z-add     upfyr         xcfyr
     C                   endif
     C*
     C                   movel(P)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data           80
     C                   parm      40            ##leng            3 0
     C                   movel     ##data        scprds
     C*
     C                   exsr      first
     C*
     C*  Reset the view indicator, if necessary
     C*
     C     @@mode        ifeq      'D'
     C     @@mode        oreq      'C'
     C                   seton                                        3248
     C                   endif
     C*
     C     @@mode        ifeq      'A'
     C                   setoff                                       32
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmtpp
     C                   endsr
     C************************************************************
     C*  SUBROUTINE TO GET PHYSICIAN/HOSPITAL NAME AND ADDRESS   *
     C************************************************************
     C     getnm         begsr
     C*
     C*   FORMAT THE PROVIDER LAST NAME, FIRST
     C*
     C                   movel     *blanks       xxatnm           30
     C     xxfein        ifeq      *zero
     C                   movel     tpphyn        xxatnm
     C                   movel     tpphya        scadr1
     C                   movel     tpphyc        sccty
     C                   movel     tpphys        scst
     C                   movel     tpphyz        sczip1
     C                   move      tpphyz        sczip2
     C                   move      tpphyr        scphyr
     C                   movel     tpphyp        scppn1
     C                   move      tpphyp        scppn2
     C                   movel     tpfxar        scfxar
     C                   movel     tpfax#        scfax1
     C                   move      tpfax#        scfax2
     C*
     C                   else
     C     pdkey         chain     ppd01                              60
     C     *in60         ifeq      *OFF
     C                   movel     pdname        xxatnm
     C     pdfsn         ifne      *blank
     C                   cat       ',':0         xxatnm
     C                   cat       pdfsn:1       xxatnm
     C                   endif
     C*
     C     pddba1        ifeq      'Y'
     C                   movel     pdadr2        scadr1
     C                   else
     C                   movel     pdadr1        scadr1
     C                   endif
     C                   movel     pdcty         sccty
     C                   movel     pdst          scst
     C                   movel     pdzip         sczip1
     C                   move      pdzip         sczip2
     C                   z-add     pdarea        scphyr
     C                   movel     pdphn#        scppn1
     C                   move      pdphn#        scppn2
     C                   movel     pdfxar        scfxar
     C                   movel     pdfax#        scfax1
     C                   move      pdfax#        scfax2
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr                                                   getnm
     C*===============================================================
     C* FIRST  - SUBROUTINE FOR LOADING SCREEN HEADER FIELDS
     C*===============================================================
     C*
     C     first         begsr
     C*
     C                   move      qqfnd         xxfnd
     C                   move      cmemp#        svemp#            9 0
     C                   move      cmdiv         svdiv             5 0
     C                   move      cmemp#        xxemp#            9 0
     C                   move      cmdiv         xxdiv             5 0
     C                   move      qqcase        svcase            7 0
     C                   move      qqcase        xxcase
     C*
     C     emky          chain     pem01                              60
     C*
     C* CONCATENATE EMP #, DIV, & NAME
     C*
     C                   call      'WA005R'
     C                   parm                    cmemp#
     C                   parm                    cmdiv
     C                   parm                    emname
     C     scedn         parm                    xxedn            80
     C*
     C*  GET EXCESS REINSURANCE INFO
     C*
     C     cmexcs        ifne      'Y'
     C     @1rrn         ifne      *zero
     C                   call      'WA010R'
     C                   parm                    @1rrn             9 0          FOR WMCMP
     C                   parm                    xxfndr           13 2
     C                   parm                    xxexre            1
     C                   endif
     C                   endif
     C*
     C     xxexre        ifeq      'Y'
     C     cmexcs        oreq      'Y'
     C                   movel(P)  xcexre        scmsg1
     C                   move      *on           *in69
     C                   endif
     C*
     C* GET SYSTEM CONTROL FILE
     C*
     C                   movel(P)  'SY'          xxsyk             2
     C     syky          chain     ssy01                              60
     C     *in60         ifeq      *off
     C*
     C* BAR CODE INSTALLED
     C*
     C     sybcif        ifeq      'Y'
     C                   call      'BA001R'
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqbcfl            1
     C                   parm                    qqtrtp            1
     C                   parm                    qqtusr           10
     C                   parm                    qqrtcd            1
     C*
     C     qqbcfl        ifeq      'Y'
     C                   move      *on           *in68
     C     qqtrtp        ifeq      'I'
     C                   movel(P)  xcbc02        scmsg2
     C                   else
     C                   movel(P)  xcbc01        scmsg2
     C                   cat       qqtusr:1      scmsg2
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   endif                                                  *IN60
     C*
     C*  CLAIMANT SS#
     C*
     C     cmss#         ifne      *blank
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****ss1           cat(p)    '-':0         scss#
     C*****scss#         cat       ss2:0         scss#
     C*****scss#         cat       '-':0         scss#
     C*****scss#         cat       ss3:0         scss#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   endif
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     cmcvt#        ifne      *blank
     C                   movel     cmcvt#        sccvt
     C                   move      *on           *in64
     C                   endif
     C*
     C*  FYR-CASE
     C*
     C                   move(p)   cmfyr         xafyr             3
     C                   move(p)   cmcase        xacase            7
     C     xafyr         cat(p)    '-':0         scyrcs
     C     scyrcs        cat       xacase:0      scyrcs
     C*
     C*  AGENGY CLAIM# (OR, SATE CLAIM#)
     C*
     C                   move      *blanks       scscl#
     C                   movel     cmacl#        scscl#
     C*
     C                   endsr                                                   First
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     kjdo          begsr
     C*
     C                   select
     C     hfld          wheneq    'SCPHYF'
     C     hfld          oreq      'SCPHYM'
      *
     C                   call      'W0030R'
     C                   parm      '2'           qqcall
     C                   parm                    qafein            9
     C                   parm                    qamod             3
     C                   parm                    qqrtcd
      *
     C     qqrtcd        ifeq      'A'
     C                   move      *on           *inkc
     C                   endif
     C*
     C     qqrtcd        ifeq      'B'
     C     qafein        oreq      *zero
     C                   move      *off          *in32
     C                   endif
     C*
     C     qqrtcd        ifeq      '*'
     C                   move      qafein        scphyf
     C                   move      qamod         scphym
     C                   move      scphyf        xxfein
     C                   move      scphym        xxmod
     C     pdkey         chain     ppd01                              60
     C     *in60         ifeq      *off
     C                   exsr      getnm
     C                   movel     xxatnm        scpnam
     C                   seton                                          3290
     C                   endif                                                  *in60
     C                   endif                                                  QQRTCD = '*'
     C*
     C     hfld          wheneq    'SCPROL'
     C                   movel     'PHCD'        xxtbid
     C                   move      *blank        xxkey
     C                   call      'W2493R'
     C                   parm                    qqprds           40
     C                   parm                    qqcall            1
     C                   parm                    xxtbid            4
     C                   parm                    xxkey            10
     C                   parm                    qqrtcd            1
      *
     C     xxkey         ifeq      *blank
     C                   seton                                          4190     POS CUSR
     C                   endif
      *
     C     qqrtcd        ifeq      '*'
     C                   movel     xxkey         scprol
     C     gtky          chain     pgt01                              60
     C     *IN60         ifeq      *OFF
     C     gtstat        andeq     'A'
     C                   movel     gtkey         scprol
     C                   seton                                          4290     POS CUSR
     C                   endif
     C                   endif
     C*
     C                   other
     C*
     C                   endsl
     C*
     C                   endsr                                                    kjdo
     C*
