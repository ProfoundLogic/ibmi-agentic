     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE - Policy Year Info
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    Display specific information about a selected employer/div/yr. *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU                                                           *
    H*    W0612R                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    W0045R02                                                       *
    H*                                                                   *
    H*  REWRITE: 06/09/00      C:33      PROJECT: WC696                  *
    H*********************************************************************
    H*  Revisions                                                        *
     H*                                                                   *
     H* 04/07/21 C:180 Project: WC1986                                    *
     H*                Add GetEftStatus to replace old methods            *
     H*                                                                   *
     H* 10/22/10 C:255 Support: SEQ73572                                  *
     H*                Checked object WA073R in library list instead      *
     H*                Call Stack.                                        *
     H*                                                                   *
     H* 07/27/10 C:257 Support: SEQ73572                                  *
     H*                If API WA073R exist in call stack then use         *
     H*                file WMEFL6 instead of WMDBL3                      *
     H*                                                                   *
     H* 12/11/08 C:33  Project: WC1769                                    *
     H*                Add 6th and 7th assessments.                       *
    H*                                                                   *
    H* 03/26/07 C:136 Support: WA136 53281                               *
    H*                Prevent bad date error.                            *
    H*                                                                   *
     H* 03/08/06 C: 132 PROJECT: WC071 - 51160                            *
     H*                - Change *DATE to use The Current Date from DSDTTM *
     H*                  Data Structure DSDATE.                           *
    H*                                                                   *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
    H*                                                                   *
    H*  02/02/05 C:136 WC1227                                            *
    H*           Add 4th and 5th assessments to the assessment total.    *
    H*                                                                   *
    H*  01/13/04 C:136 WC1116                                            *
    H*           Use expiration date instead of DBSTAT to determine      *
    H*           whether an account is ACH or not. Use vendor history    *
    H*           record to determine if account has a vendor or not.     *
    H*                                                                   *
    H*  06/26/03 C:132 WC1126                                            *
    H*           Add check for WMDBP/DBSTAT = "A", to verify ACH account *
    H*                                                                   *
    H*  05/23/03 C:132 WC1126                                            *
    H*           ADD Account Type to screen, which is either ACH account *
    H*           or a Payroll Vendor account.                            *
    H*                                                                   *
    H*  12/04/01 C:136 WA121 Supp# 30116                                 *
    H*           Assessments displayed incorrectly. Data fields being    *
    H*           used to total assessments were wrong.                   *
    H*                                                                   *
    H*  10/24/01 C:136 WA081 Supp# 29413                                 *
    H*           Add F-14 for Darts Access                               *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0612f1   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwtabl     if   e           k disk    include(ptt01:pbs01)
     Fwtavl     if   e           k disk    rename(pav01:pav00)
      ***** BEG ADD ***** 05/23/03 ****************************************
     Fwmvpl     if   e           k disk
     F*** beg chg  ******  01/13/04  **************************************
     F************ wmdbl     if   e           k disk
     Fwmdbl3    if   e           k disk
     Fwdvhl1    if   e           k disk
     F*** end chg  ******  01/13/04  **************************************
      ***** END ADD ***** 05/23/03 ****************************************
     F***** BEG ADD ******* 07/27/10 **********************************
     Fwmefl6    if   e           k disk
     F***** END ADD ******* 07/27/10 **********************************

     D/COPY SCOPYPR01D

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@prds          s             40
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@co#           s              3  0
     D @@fnd           s              3  0
     D @@emp#          s              9  0
     D @@div           s              5  0
     D @@fyr           s              3  0
     D @@rrn           s              9  0
     D isEft           s               N   inz(*off)
      ***** BEG ADD ***** 05/23/03 ****************************************
     D ACHtxt          c                   const('ACH-Starting period = ')
     D VDRtxt          c                   const('Payroll-Vendor = ')
     D Acctxt          s             60    Inz
     D xavdr#          s              5    Inz
     D xadbsd          s              3    Inz
     D xwrka37         s             35    Inz
      ***** END ADD ***** 05/23/03 ****************************************
     D optnm           c                   const('Policy Year Info')
     D comapi          c                   const('WA001R')
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D areaCode        s              3
     D phone#          s              7
     D xxkey           s                   like(TTKEY)
     D cxReason        s             65
     D need_emp#       s              1
     D restartPgm      s              1
     D error           s              1
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D PF_rrn          s              9  0
     *** BEG ADD *** 07/27/10 **********************************************************************
     D XXDATE          s                   like(EFEFDT)
     *** END ADD *** 07/27/10 **********************************************************************
     *** beg add *** 10/24/01 **********************************************************************
     D qcpgm           s             10
     D  qckey          ds            40
     D    qcco#                1      3
     D    qcfnd                4      6
     D    qcemp#               7     15
     D    qcdiv               16     20
     *** end add *** 10/24/01 **********************************************************************
      *
     D*** beg add  ******  01/13/04  **************************************
     D sysDate         s               d
     D kkefdt          s              7  0
     D*** end add  ******  01/13/04  **************************************
     d*** BEG ADD *** 04/07/21 ****************************************
     d strCo#          S              3a
     d strEftType      S              1a
     d strFnd          S              3a
     d strEftKey       S             30a
     d strDate         S              7a
     d strPre          S              1a   inz
     d strRrn          S              9a   inz
     d strEobTyp       S              3    inz
     d strCkTp         S             10    inz
     d strFyr          S              3    inz
     d strEnv          S              1    inz
     d*** END ADD *** 04/07/21 ****************************************

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     DWA002R           PR                  extpgm('WA002R')
     D ##co#                               like(EMCO#)  const
     D #@fnd                               like(EMFND)  const
     D ##emp#                              like(EMEMP#) const
     D ##div                               like(EMDIV)  const
     D ##fyr                               like(ELFYR)  const
     D ##agt#                         5  0
     D ##agt_name                    60
     D ##prod                         5  0
     D ##prod_nm                     60
     D ##area_cd                      3  0
     D ##phone#                       7  0
     D ##plan                         1
     D ##asOfDate                     8  0              const

     DW0045R02         PR                  extpgm('W0045R02')
     D ##kc                           1
     D ##kl                           1    const
     D ##call                         1    const
     D ##PF_rrn                       9  0 const
     D ##FN_rrn                       9  0 const

     Dacct_curr        PR             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D yr                                  like(ELFYR)  const

     D/copy SPRDATE
     D/copy SPRMESSAGE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
     D/copy SPRWRKWTH

     D/copy WPRGROUP
     D/copy WPREMPLR
     d*** BEG ADD *** 04/07/21 ****************************************
     d GetEFTStatus    Pr             1n
     d inCo#                          3
     d inType                         1
     d inFnd                          3
     d inKey                         30
     d inDate                         7
     d inPre                          1
     d inRrn                          9    options(*nopass)
     d inEobType                      3    options(*nopass)
     d inCkTp                        10    options(*nopass)
     d inFyr                          3    options(*nopass)
     d inEnv                          1    options(*nopass)
     d*** END ADD *** 04/07/21 ****************************************

    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      ***** BEG ADD ***** 07/27/10 ****************************************
    C     DACYMD        plist
    C                   parm                    SRDTI             8 0
    C                   parm      *BLANKS       SRFFMT            8
    C                   parm      '*CYMD'       SRTFMT            8
    C                   parm                    SRDTO            10
    C                   parm                    SRERR             1
      ***** END ADD ***** 07/27/10 ****************************************
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     table_key     klist
     C                   kfld                    ##co#
     C                   kfld                    xxkey

      ***** BEG ADD ***** 05/23/03 ****************************************
      * Payroll Vendor File
     C     vend_key      klist
     C                   kfld                    ##co#
     C                   kfld                    elvdr#
      *
      * Direct Debit Employer File
     C     ddbt_key      klist
     C                   kfld                    ##co#
     C                   kfld                    @@fnd
     C                   kfld                    @@emp#
     C                   kfld                    @@div
      ***** END ADD ***** 05/23/03 ****************************************

      ***** beg add ***** 01/13/04 ****************************************
     C     dbky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv

     C     dbky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    kkefdt

     c     vhky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr

     c     vhky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C                   kfld                    kkefdt
      ***** end add ***** 01/13/04 ****************************************

     C                   exsr      getcof
     C                   exsr      pgmstr
     C*
     C                   dou       restartPgm = *off
     C*
     C                   eval      restartPgm = *off
     C*
     C                   if        need_emp# = 'Y'
     C                   exsr      criteria
     C                   endif
     C*
     C                   if        not *inkc
    C*
B01  C                   dou       *inkc or *inkl or not *in90
    C*
     C                   exsr      predsp
    C*
     C                   exfmt     scr01
    C*
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
    C*
B02  C                   if        not *inkc and not *inkl
    C*
     C                   exsr      othfks
     C  n90              exsr      errck1
    C*
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
    C*
E02  C                   endif
    C*
E01  C                   enddo
    C*
E02  C                   endif
    C*
     C                   exsr      pgmend
     C*
     C                   enddo
    C*
     C                   eval      *inlr = *on
    C*
    C*====================================================================
    C* CRITERIA - Get the selection criteria from scrcrt
    C*====================================================================
    C*
     C     criteria      begsr
    C*
     C                   dou       *in90 = *off
     C*
     C                   exfmt     scrcrt
     C*
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
     C*
     C                   if        not *inkc and not *inkl
     C*
     C                   exsr      othfks
     C*
     C                   if        not *in90
     C                   exsr      err_scrcrt
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo
     C*
     C                   if        *in90
     C                   eval      need_emp# = 'N'
     C                   exsr      pgmend
     C                   return
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERR_scrcrt - Error checking for the selection criteria screen
    C*====================================================================
    C*
     C     err_scrcrt    begsr
     C*
     C  n90              if        ValidEmp(scemp#:scdiv:EM_rrn:##fnd)
     C                              = *off
     C                   seton                                        71  90
     C                   endif
     C*
     C  n90              if        ValidEmpYr(scyr:scemp#:scdiv:EL_rrn:
     C                                        ##fnd) = *off
     C                   seton                                        72  90
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* GRP_INFO - Move group information data base fields to screen fields
    C*====================================================================
    C*
     C     grp_info      begsr
     C*
     C                   eval      scfnd      = ##fnd
     C                   eval      scfnam     = GetGrpName(##fnd:FN_rrn:##co#)
     C                   eval      scname_fnd = scfnam
    C*
     C                   endsr
    C*
    C*====================================================================
    C*  FINISH - Final processing for file update/display
    C*====================================================================
    C*
     C     FINISH        begsr
    C*
     C                   exsr      prsctl
    C*
     C                   endsr
    C*
    C*====================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*====================================================================
    C*
     C     GETCOF        begsr
    C*
     C                   move      *on           *in91
    C*
     C                   exsr      dscapi
    C*
B01  C                   if        *in91
     C                   eval      ##conm = pdsnam
     C                   eval      ##conm = centerFld(##conm)
E01  C                   endif
    C*
     C                   eval      scconm = ##conm
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KC_KL_do - Return *inkc & *inkl values to calling program, if needed
    C*====================================================================
    C*
     C     kc_kl_do      begsr
     C*
     C                   if        %parms >= 3
     C                   eval      @@kc = *inkc
     C                   endif
     C*
     C                   if        %parms >= 4
     C                   eval      @@kl = *inkl
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KGDO   - Processing for F7
    C*====================================================================
    C*
     C     KGDO          begsr
    C*
     C                   callp     W0045R02(*inkc:*inkl:'D':PF_rrn:FN_rrn)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
    C*
     C                   select
     C*____________________________________________________________________
     C*
     C                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     C*
     C                   eval      EM_rrn = empSrch(*inkc:*inkl:error:##fnd)
    C*
     C     EM_rrn        chain     wmemp                              60
    C*
     C                   select
     C                   when      *inkl = *on or *in60
     C                   eval      *in81 = *on
     C                   other
     C                   eval      *in82 = *on
     C                   eval      scemp# = EMEMP#
     C                   eval      scdiv  = EMDIV
     C                   endsl
     C*____________________________________________________________________
     C*
     C                   when      hfld = 'SCYR'
     C*
     C                   eval      EL_rrn = empYrSrch(*inkc:*inkl:error:scemp#:
     C                                              scdiv:##fnd)
    C*
     C     EL_rrn        chain     wdelp                              60
    C*
     C                   select
     C                   when      error = '2'
     C                   eval      *in71 = *on
     C                   when      *inkl = *on or *in60
     C                   eval      *in82 = *on
     C                   other
     C                   eval      scyr   = ELFYR
     C                   endsl
     C*____________________________________________________________________
     C*
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C*  KMDO   - Process F13; user parameters
    C*====================================================================
    C*
     C     kmdo          begsr
    C*
     C                   call      'W0006R'                             99
    C*
     C                   exsr      getcof
     C                   exsr      grp_info
    C*
     C                   endsr
    C*
     *** beg add *** 10/24/01 **********************************************************************
      *===============================================================
      * KNDO - SUBROUTINE FOR F14
      *===============================================================
      *
      *  F14=Darts
      *
     C     kndo          begsr
      *
      *  Establish key
      *
     C                   move      *blanks       qckey
     C                   move      ##co#         qcco#
     C                   move      scfnd         qcfnd
     C                   move      scemp#        qcemp#
     C                   move      scdiv         qcdiv
      *
      *  Call Darts entry program
      *
     C                   call      'SC010R'
     C                   parm                    qckey
     C                   parm      'W0050R'      qcpgm
      *
     C                   endsr                                                  kndo
     *** end add *** 10/24/01 **********************************************************************
    C*====================================================================
    C* LOAD_EL - Load WDELP information
    C*====================================================================
    C*
     C     load_EL       begsr
    C*
     C     EL_rrn        chain     wdelp                              60
    C*
     C                   eval      ##co#  = ELCO#
     C                   eval      ##fnd  = ELFND
     C                   eval      scemp# = ELEMP#
     C                   eval      scdiv  = ELDIV
     C                   eval      scyr   = ELFYR
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkg         caseq     *on           kgdo                     90
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
      *** beg add *** 10/24/01 ******************************************************************
     C     *inkn         caseq     *on           kndo
      *** end add *** 10/24/01 ******************************************************************
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C*
     C                   if        not *inkc and need_emp# = 'Y'
     C*
     C                   eval      restartPgm = *on
     C*
     C                   if        *inkl = *off
     C                   clear                   scemp#
     C                   clear                   scdiv
     C                   clear                   scyr
     C                   endif
     C*
     C                   endif
    C*
     C                   exsr      kc_kl_do
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   if        q1prms >= 1 and @@prds <> *blanks
     C                   eval      scprds = @@prds
     C                   else
     C                   eval      scprds = optnm
     C                   endif
    C*
     C                   eval      scprds     = centerFld(scprds)
     C                   eval      scprds_crt = scprds
    C*
     C                   clear                   xxcall
     C                   if        q1prms >= 2
     C                   eval      xxcall = @@call
     C                   endif
     C*
     C                   if        %parms >= 5 and @@co# <> *zero
     C                   move      @@co#         ##co#
     C                   endif
     C*
     C                   if        %parms >= 6 and @@fnd <> *zero
     C                   move      @@fnd         ##fnd
     C                   endif
     C*
     C                   eval      need_emp# = 'Y'
     C*
     C                   if        %parms >= 7 and @@emp# <> *zero
     C                   move      @@emp#        scemp#
     C                   move      @@div         scdiv
     C                   move      @@fyr         scyr
     C                   eval      need_emp# = 'N'
     C                   endif
     C*
     C                   if        %parms >= 10 and @@rrn <> 0
     C                   eval      EL_rrn = @@rrn
     C                   exsr      load_EL
     C                   eval      need_emp# = 'N'
     C                   endif
     C*
     C                   exsr      grp_info
     C*** BEG ADD ***** 07/27/10 ************************************
     C                   movel     'WA073R'      xxpgm            10
     C*** BEG CHG ***** 10/22/10 ************************************
     C*******            call      'SRCHKPGM'
     C*******            parm                    xxpgm
     C*******            parm                    xxnach            1
     C*
     C                   MOVE      '*PGM'        XXTYP             7
     C                   call      'SRCHKOBJ'
     C                   parm                    XXPGM
     C                   parm                    XXTYP
     C                   parm                    XXNACH            1
     C*** BEG CHG ***** 10/22/10 ************************************
    C*
     C                   time                    dsdttm
     c                   Z-ADD     DSDATE        SRDTI
     c                   CALL      'SRCVTDAT'    DACYMD
     c                   MOVEL     SRDTO         xxdate
     C*** END ADD ***** 07/27/10 ************************************
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C*
     C                   exsr      grp_info
     C*
     C                   eval      EL_rrn = GetEmpYrRR(scyr:scemp#:scdiv:
     C                                                 scfnd:##co#)
     C*
     C     EL_rrn        chain     WDELP                              60
     C*
     C                   eval      scedn = EmpDivName(ELEMP#:ELDIV:EM_rrn:
     C                                                ELFND:ELCO#)
     C*
     C     EM_rrn        chain     WMEMP                              60
     C*
     C                   eval      scdiv_cnt = DivCount(ELCO#:ELFND:ELEMP#)
     C*
     C                   eval      scprv_emp# = GetPrvEmp#(ELCO#:ELFND:ELEMP#:
     C                                                     ELDIV)
     C                   eval      *in51 = scprv_emp# <> *blanks
     C*
     C                   eval      *in50 = acct_curr(ELCO#:ELFND:ELEMP#:
     C                                               ELDIV:ELFYR)
     C*
      ***** BEG ADD ***** 05/23/03 ****************************************
      * Check for Payroll Vendor and display if have one
     c                   eval      acctxt = *blanks
     c                   eval      *in69 = *off
      *** beg chg ***** 01/13/04 ***************************************
     c****************   if        *in60 = *off AND elvdr# > *zero
     c*****vend_key***   chain     PVP01                              61
     c****************   if        not *in61
     c                   if        not *in60

      ***** BEG CHG ***** 03/08/06 *******************************************************
     c*************      move      *date         sysDate
     c                   TIME                    DSDTTM
      *** beg chg ***** 03/26/07 *********************************************************
     c*****************  move      DSDATE        sysDate                        mmddyyyy
     c     *usa          move      DSDATE        sysDate                        mmddyyyy
      *** end chg ***** 03/26/07 *********************************************************
      ***** END CHG ***** 03/08/06 *******************************************************
     c     *cymd         move      sysDate       kkefdt
     c     vhky1         setgt     pvh01
     c     vhky          readpe    pvh01
     c                   if        not %eof and vhexdt = 0 or
     c                             not %eof and vhexdt <> 0 and kkefdt < vhexdt
      *** end chg ***** 01/13/04 ***************************************
     c                   eval      *in69 = *on
     c                   eval      xwrka37 = vpname
      *** beg chg ***** 01/13/04 ***************************************
     c****************   move      vpvdr#        xavdr#
     c                   eval      xavdr# = %triml(%editc(vhvdr#:'4'))
      *** end chg ***** 01/13/04 ***************************************
     c                   eval      ACCTXT = vdrtxt + xavdr# + ' ' + xwrka37
     c                   eval      scactp = acctxt
      *** beg chg ***** 01/13/04 ***************************************
     c*****************  endif
     c*****************  endif
     c                   else
      *** end chg ***** 01/13/04 ***************************************
      * Check for ACH Direct Debit record if don't have Payroll Vendor
      ***** BEG CHG ***** 06/26/03 *************************************
      *** beg chg ***** 01/13/04 ***************************************
     c****************** if        *in60 = *off AND elvdr# = *zero
     c*****ddbt_key***** chain     PDB01                              61
     c****************** if        not *in61
     c****************** if        not *in61 AND DBSTAT = 'A'                   Active ACH Acct ?
     C*** BEG CHG ***** 07/27/10 ************************************
     C*
     C* check if new ACH module is installed ('WC1806')
     C*
     C*** BEG CHG ***** 10/22/10 ************************************
     C*******            if        xxnach = *on
     *** Begin Delete 04/07/21 **************************************
     C**                 if        xxnach = *off
     C*** END CHG ***** 10/22/10 ************************************
     C**   dbky          chain     pef01                              62
     C**   *in62         doweq     *off
     C**   xxdate        iflt      efexdt
     C**   xxdate        andge     efefdt
     C**   efexdt        oreq      *zero
     C**   xxdate        andge     efefdt
     C**
     c**                 clear                   xxkey
     c**                 move      elbscd        xadbsd
     c**                 movel(P)  xadbsd        xxkey
     c**   table_key     chain     pbs01                              62
     c**                 if        not *IN62 AND bsdbsd > *zero
     c**                 eval      *in69 = *on
     c**                 clear                   xadbsd
     c**                 move      bsdbsd        xadbsd
     c**                 eval      ACCtxt = ACHtxt + ' ' + xadbsd
     c**                 eval      scactp = acctxt
     c**                 endif
     C**                 leave
     C**                 endif
     C**   dbky          reade     pef01                                       62
     C**                 enddo
     C**                 else
     C*** END CHG ***** 07/27/10 ************************************
     c**   dbky1         setgt     pdb01
     c**   dbky          readpe    pdb01
     c**                 if        not %eof and dbexdt = 0 or
     c**                           not %eof and dbexdt <> 0 and kkefdt < dbexdt
      *** end chg ***** 01/13/04 ***************************************
      ***** END CHG ***** 06/26/03 *************************************
     *** End   Delete 04/07/21 **************************************
      *** Begin Add *** 04/07/21 *****************************************
       strCo# = %Editc(ELco#:'X');
       strEftType = 'E';
       strFnd = %Editc(ELfnd:'X');
       strEftKey = %Editc(ELemp#:'X') + %Editc(ELdiv:'X');
       strDate =%Editc(xxDate:'X');
       strFyr = %Editc(ELfyr:'X');
       isEft = GetEftStatus(strCo#:strEftType:strfnd:strEftKey:strDate:
                            strPre:strRrn:strEobTyp:strCkTp:strFyr);
       If isEft = *On;
      *** End   Add *** 04/07/21 *****************************************
     c                   clear                   xxkey
     c                   move      elbscd        xadbsd
     c                   movel(P)  xadbsd        xxkey
     c     table_key     chain     pbs01                              62
     c                   if        not *IN62 AND bsdbsd > *zero
     c                   eval      *in69 = *on
     c                   clear                   xadbsd
     c                   move      bsdbsd        xadbsd
     c                   eval      ACCtxt = ACHtxt + ' ' + xadbsd
     c                   eval      scactp = acctxt
     c                   endif
      ** Begin Add    04/07/21 **************************************
       Endif;
      ** End   Add    04/07/21 **************************************

      ** Begin Delete 04/07/21 **************************************
     c**                 endif
     C*** beg add ***** 07/27/10 ************************************
     c**                 endif
     C*** end add ***** 07/27/10 ************************************
     *** End   Delete 04/07/21 **************************************

     c                   endif
      *** beg add ***** 01/13/04 ***************************************
     c                   endif                                                  not *in60
      *** end add ***** 01/13/04 ***************************************
      ***** END ADD ***** 05/23/03 ****************************************
     C*
     C                   eval      scfyr      = ELFYR
     C                   eval      scpfdt     = dtMDYY(ELPFDT)
     C                   eval      scptdt     = dtMDYY(ELPTDT)
     C                   eval      scBillSchd = GetBillSch(EL_rrn)
     C*
     C                   eval      sccxdt = dtMDYY(GetCxDate(ELCO#:ELFND:
     C                                        ELEMP#:ELDIV:cxReason))
     C                   eval      *in52 = sccxdt <> *zero
     C                   eval      sccx_desc = cxReason
     C*
     C                   if        ELFAF = 'A'
     C                   eval      scprem   = ELAAPR
     C                   eval      scstatus = 'Audited'
     C                   eval      *in55    = *on
     C                   else
     C                   eval      scprem   = ELANPR
     C                   eval      scstatus = 'Estimated'
     C                   eval      *in55    = *off
     C                   endif
     C*
     C                   eval      scprem     = scprem - ELPRTD
     C                   eval      scexpconst = ELPRTD
     C                   eval      sctax      = ELTXDU
      *** beg chg **** 12/04/01 ********************************************************************
     C****************   eval      scassess   = EL1APR + EL2APR
      *** beg chg **** 02/02/05 ********************************************************************
     C****************   eval      scassess   = EL1AAM + EL2AAM + EL3AAM
     C                   eval      scassess   = EL1AAM + EL2AAM + EL3AAM +
     C                                          EL4AAM + EL5AAM
     C***  Begin Add  ***  12/11/08  **************************************
     C                                        + EL6AAM + EL7AAM
     C***  End   Add  ***  12/11/08  **************************************
      *** end chg **** 02/02/05 ********************************************************************
      *** end chg **** 12/04/01 ********************************************************************
     C                   eval      sctotal    = scprem + scexpconst + sctax
     C                                        + scassess
     C*
     C***** beg add ***** 07/07/05 ****************************************
     C                   eval      EMASTS = ELASCD
     C***** end add ***** 07/07/05 ****************************************
     C*
     C                   clear                   AVDESC
     C                   eval      xxkey = EMASTS
     C     table_key     chain     pav00                              60
     C                   eval      scact_sts = EMASTS + ' ' + AVDESC
     C*
     C                   eval      scctst = ELCTST
     C*
     C                   eval      scprem_co = GetPremFin(EL_rrn:
     C                                cvtToDate(dtMDYY(ELPFDT)):PF_rrn)
     C                   eval      *in53 = scprem_co <> *blanks
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
     C                   endsr
    C*





    P*====================================================================
    P* acct_curr - Determine if employer is account current
    P*====================================================================
    P*
     Pacct_curr        b
    P*
     Dacct_curr        PI             1
     D co#                                 like(EMCO#)  const
     D grp                                 like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D yr                                  like(ELFYR)  const

     D ##name_agt      s             60
     D ##name_pro      s             60
     D ##areaCode      s              3  0
     D ##phone#        s              7  0
     D ##plan          s              1
     D ##asOfDate      s              8  0
     D usa_date        s               d   datfmt(*usa)
     D acct_curr2      s                   like(*in99)

     C                   eval      ##plan = '*'
     C*
      ***** BEG CHG ***** 03/08/06 *******************************************************
     c*************      move      *date         usa_date
     c                   TIME                    DSDTTM
     c     *usa          move      DSDATE        usa_date                       mmddyyyy
      ***** END CHG ***** 03/08/06 *******************************************************

     C                   move      usa_date      ##asOfDate
     C*
     C                   callp     WA002R(co#:grp:emp#:div:yr:scagt#:
     C                                    ##name_agt:scprod:##name_pro:
     C                                    ##areaCode:##phone#:##plan:
     C                                    ##asOfDate)
     C*
     C                   move      ##areaCode    areaCode
     C                   move      ##phone#      phone#
     C*
     C                   if        ##areaCode <> *zero
     C                   eval      scphn# = '(' + areaCode + ') '
     C                   endif
     C*
     C                   if        ##areaCode <> *zero or ##phone# <> *zero
     C                   eval      scphn# = %trim(scphn#) + ' ' +
     C                                      %subst(phone#:1:3) + '-' +
     C                                      %subst(phone#:4:4)
     C                   endif
     C*
     C                   eval      scname_agt = ##name_agt
     C                   eval      scname_pro = ##name_pro
     C                   eval      acct_curr2 = ##plan = 'A'
     C*
     C                   return    acct_curr2

     Pacct_curr        e





