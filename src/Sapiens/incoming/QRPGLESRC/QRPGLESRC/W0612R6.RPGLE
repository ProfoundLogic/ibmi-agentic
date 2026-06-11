     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE - A/R Summary
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    List all A/R code summary for a selected employer/div/yr.      *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU                                                           *
    H*    W0612R                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  REWRITE: 06/09/00      C:33      PROJECT: WC696                  *
    H*********************************************************************
    H*  REVSIONS:                                                        *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
    H*                                                                   *
    H*  10/25/01 C:136 WA081 Supp# 29415                                 *
    H*           Add reverse image on activity status if there is one,   *
    H*           otherwise, do not display the field.                    *
    H*                                                                   *
    H*  10/24/01 C:136 WA081 Supp# 29413                                 *
    H*           Add F-14 for Darts Access                               *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0612f6   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtttp     if   e             disk    rename(ptt01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtabl     if   e           k disk    rename(ptt01:dbrcdf)
     F                                     infds(dbfds) include(ptt01)
     Fwmeal14   if   e           k disk    rename(pea01:pea14)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwtctp     if   e             disk    rename(pct01:pct0p)
     Fwtavl     if   e           k disk    rename(pav01:pav00)
     Fwmeal6    if   e           k disk    rename(pea01:pea06)

     D srchCtgrys      s             10    dim(5) ctdata perrcd(1)
     D srchCodes       s             10    dim(10) ctdata perrcd(1)

     D hd              s             78    dim(1) ctdata perrcd(1)
     *** beg chg *** 10/24/01 **********************************************************************
     D************************ of              s              1    dim(11) ctdata perrcd(1)
     D************************ ofd             s             35    dim(11) alt(of)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
     *** end chg *** 10/24/01 **********************************************************************

     D lda             ds          1024
     D  sspodt_fr                     8  0
     D  sspodt_to                     8  0
     D  sstrdt_fr                     8  0
     D  sstrdt_to                     8  0
     D  ssctyp_1                      1
     D  ssctyp_2                      1
     D  ssctyp_3                      1
     D  ssctyp_4                      1
     D  ssctyp_5                      1
     D  ssarcd_1                      2
     D  ssarcd_2                      2
     D  ssarcd_3                      2
     D  ssarcd_4                      2
     D  ssarcd_5                      2
     D  ssarcd_6                      2
     D  ssarcd_7                      2
     D  ssarcd_8                      2
     D  ssarcd_9                      2
     D  ssarcd_10                     2
     *** beg add *** 10/24/01 **********************************************************************
     D  qckey          ds            40
     D    qcco#                1      3
     D    qcfnd                4      6
     D    qcemp#               7     15
     D    qcdiv               16     20
     *** end add *** 10/24/01 **********************************************************************

    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'A/R Detail Summary'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0612R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@co#           s              3  0
     D @@fnd           s              3  0
     D @@emp#          s              9  0
     D @@div           s              5  0
     D @@fyr           s              3  0
     D @@rrn           s              9  0
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##call          s              1
     D ##rrn           s              9  0
     D kkco#           s                   like(TTCO#)
     D kkkey           s                   like(TTKEY)
     D kkfnd           s                   like(EAFND)
     D kkemp#          s                   like(EAEMP#)
     D kkdiv           s                   like(EADIV)
     D kkfyr           s                   like(EAFYR)
     D kkpodt          s                   like(EAPODT)
     D kkarcd          s                   like(EAARCD)
     D need_emp#       s              1
     D firstRead       s              1
     D readBack        s              1
     D balance         s             13  2
     D priorAmt        s                   like(scamt)
     D error           s              1
     D xxkey           s                   like(TTKEY)
     D rrn             s              9  0
     D flag            s              1
     D validOpt        s              1
     D loadLDA         s              1
     D srchCtgry       s                   like(*in60)
     D srchCode        s                   like(*in60)
     D x               s              3  0
     D db_rrn          s              9  0
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D CT_rrn          s              9  0
     D ARCode          s             10
     D null_date       s               d   inz
     *** beg add *** 10/24/01 *****************************************************************
     D qcpgm           s             10
     *** end add *** 10/24/01 *****************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     DWA002R           PR                  extpgm('WA002R')
     D ##co#                               like(EMCO#)  const
     D #@fnd                               like(EMFND)  const
     D ##emp#                              like(EMEMP#) const
     D ##div                               like(EMDIV)  const
     D ##fyr                               like(ELFYR)  const
     D ##agt#                         5  0              const
     D ##agt_name                    60                 const
     D ##prod                         5  0              const
     D ##prod_nm                     60                 const
     D ##area_cd                      3  0              const
     D ##phone#                       7  0              const
     D ##plan                         1
     D ##asOfDate                     8  0              const

     DsrChkPgm         PR                  extpgm('SRCHKPGM')
     D ##pgm                         10    const
     D ##LoadLDA                      1

     DaccumTotal       PR            11  2
     D key                                 like(TTKEY)  const

     Dacct_curr        PR             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D yr                                  like(ELFYR)  const

     DcurrentBal       PR            11  2
     D fromDate                       7  0 const
     D toDate                         7  0 const

     DmeetSubset       PR             1
     D type                           1    const options(*nopass)

     DsflBalance       PR            13  2
     D rrn                            9  0 const

     D/copy SPRDATE
     D/copy SPRMESSAGE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
     D/copy SPRWRKWTH

     D/copy WPRGROUP
     D/copy WPREMPLR
     D/copy WPRAR

     C/copy SCOPYWW41C

     C     *dtaara       define    *lda          lda

    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
    C*
     C     key           klist
     C                   kfld                    kkco#
    C*
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
     C     table_key     klist
     C                   kfld                    ##co#
     C                   kfld                    xxkey

    C*====================================================================
    C* CALC_BAL - Calculate header balance
    C*====================================================================
    C*
     C     calc_bal      begsr
    C*
     C** **              eval      sccurr = currentBal(0:dtCYMD(*date))
     C** **              eval      scprev = currentBal(0:dtCYMD(sspodt_fr))
    C*
     C                   eval      *in54 = sspodt_to <> *zero
     C*
     C                   eval      scbal_fr = sspodt_fr
     C                   eval      scbal_to = sspodt_to
    C*
     C                   endsr
    C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
    C*
     C     chkssm        begsr
     C*
     C                   eval      *in11 = (sspodt_to <> *zeros  or
     C                                      ssctyp_1  <> *blanks or
     C                                      ssctyp_2  <> *blanks or
     C                                      ssctyp_3  <> *blanks or
     C                                      ssctyp_4  <> *blanks or
     C                                      ssctyp_5  <> *blanks or
     C                                      ssarcd_1  <> *blanks or
     C                                      ssarcd_2  <> *blanks or
     C                                      ssarcd_3  <> *blanks or
     C                                      ssarcd_4  <> *blanks or
     C                                      ssarcd_5  <> *blanks or
     C                                      ssarcd_6  <> *blanks or
     C                                      ssarcd_7  <> *blanks or
     C                                      ssarcd_8  <> *blanks or
     C                                      ssarcd_9  <> *blanks or
     C                                      ssarcd_10 <> *blanks)
     C*
     C                   exsr      calc_bal
     C*
     C                   endsr
    C*
    C*====================================================================
    C* CRITERIA - Get the selection criteria from scrcrt
    C*====================================================================
    C*
     C     criteria      begsr
    C*
     C                   eval      scprds_crt = optnm
     C                   eval      scprds_crt = centerFld(scprds_crt)
    C*
     C                   dou       *in90 = *off
     C*
     C                   exfmt     scrcrt
     C*
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
     C*
     C                   if        *inkc = *off and *inkl = *off
     C*
     C                   exsr      othfks
     C*
     C                   if        *in90 = *off
     C                   exsr      err_scrcrt
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo
     C*
     C     *inkc         caseq     *on           kcdo                     90
     C     *inkl         caseq     *on           kldo                     90
     C                   endcs
     C*
     C                   if        *in90 = *on
     C                   eval      need_emp# = 'N'
     C                   exsr      pgmend
     C                   return
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*====================================================================
    C*
     C     errck1        begsr
     C*
     C                   eval      validOpt = *on
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*====================================================================
    C*
     C     errck2        begsr
     C*
     C                   exsr      othfks
     C*
     C                   if        sspodt_fr <> *zero
     C                   eval      sspodt_fr  = dtCheck(sspodt_fr)
     C  n90              if        sspodt_fr  = *zero
     C                   seton                                        71  90
     C                   endif
     C                   endif
    C*
     C                   if        sspodt_to <> *zero
     C                   eval      sspodt_to  = dtCheck(sspodt_to)
     C  n90              if        sspodt_to  = *zero
     C                   seton                                        72  90
     C                   endif
     C                   endif
     C*
     C  n90              if        cvtToDate(sspodt_fr) > cvtToDate(sspodt_to)
     C                   seton                                        73  90
     C                   endif
    C*
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
     C                   eval      scfnd    = ##fnd
     C                   eval      scfnam   = GetGrpName(##fnd:FN_rrn:##co#)
     C                   eval      scfnam_2 = scfnam
    C*
     C                   endsr
    C*
    C*====================================================================
    C* INZSST - Initialize the subset fields
    C*====================================================================
    C*
     C     inzsst        begsr
     C*
     C                   if        need_emp# = 'Y'
     C                   exsr      criteria
     C                   endif
     C*
     C                   clear                   sspodt_fr
     C                   clear                   sspodt_to
     C                   clear                   ssctyp_1
     C                   clear                   ssctyp_2
     C                   clear                   ssctyp_3
     C                   clear                   ssctyp_4
     C                   clear                   ssctyp_5
     C                   clear                   ssarcd_1
     C                   clear                   ssarcd_2
     C                   clear                   ssarcd_3
     C                   clear                   ssarcd_4
     C                   clear                   ssarcd_5
     C                   clear                   ssarcd_6
     C                   clear                   ssarcd_7
     C                   clear                   ssarcd_8
     C                   clear                   ssarcd_9
     C                   clear                   ssarcd_10
     C*
     C                   callp     srChkPgm('W0612R':loadLDA)
     C                   if        loadLDA = *on
     C                   in        lda
     C                   endif
     C*
     C                   exsr      loadHeader
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KC_KL_do - Return *inkc & *inkl values to calling program, if needed
    C*====================================================================
    C*
     C     kc_kl_do      begsr
     C*
     C                   if        %parms >=2
     C                   eval      @@kc = *inkc
     C                   endif
     C*
     C                   if        %parms >=3
     C                   eval      @@kl = *inkl
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
    C*
     C     keyinz        begsr
     C*
     C                   eval      kkco#  = ##co#
     C                   eval      kkfnd  = ##fnd
     C                   eval      kkemp# = scemp#
     C                   eval      kkdiv  = scdiv
     C                   eval      kkfyr  = scyr
     C*
     C                   clear                   kkpodt
     C                   clear                   kkkey
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
    C*
     C     keyset        begsr
     C*
     C                   clear                   kkkey
     C*
     C                   select
     C                   when      *in21
     C                   eval      kkkey = scpst1
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
    C*
     C     keysfl        begsr
     C*
     C                   eval      kkkey = sckey
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
    C*
     C     kjdo          begsr
     C*
     C     hfld          lookup    srchCtgrys                             60
     C                   eval      srchCtgry = *in60
     C*
     C     hfld          lookup    srchCodes                              60
     C                   eval      srchCode = *in60
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
     C                   when      srchCtgry = *on
     C*
     C                   eval      x = 1
     C     hfld          lookup    srchCtgrys(x)                          60
     C*
     C                   eval      CT_rrn = ARCatSrch(*inkc:*inkl)
     C*
     C                   eval      *in81 = *on
     C*
     C     CT_rrn        chain     pct0p                              60
     C*
     C                   if        *in60 = *off
     C                   exsr      loadCtgry
     C                   eval      *in(x + 70 + 1) = *on
     C                   else
     C                   eval      *in(x + 70) = *on
     C                   endif
     C*____________________________________________________________________
    C*
     C                   when      srchCode = *on
     C*
     C                   eval      x = 1
     C     hfld          lookup    srchCodes(x)                           60
     C*
     C                   eval      ARCode = ARCodeSrch(*inkc:*inkl)
     C*
     C                   eval      *in82 = *on
     C*
     C                   if        ARCode <> *blanks
     C                   exsr      loadCode
     C                   eval      *in(x + 70 + 1) = *on
     C                   else
     C                   eval      *in(x + 70) = *on
     C                   endif
     C*____________________________________________________________________
    C*
     C                   endsl
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
    C* LOAD_bal - Load balance when adding a new subfile screen
    C*====================================================================
    C*
     C     load_bal      begsr
    C*
     C                   if        firstRead = *on
     C                   eval      balance   = sflBalance(db_rrn)
     C                   eval      firstRead = *off
     C                   eval      readBack  = *off
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* loadCode - Load A/R codes after search
    C*====================================================================
    C*
     C     loadCode      begsr
    C*
     C                   select
     C                   when      hfld = 'SSARCD_1'
     C                   eval      ssarcd_1  = ARCode
     C                   when      hfld = 'SSARCD_2'
     C                   eval      ssarcd_2  = ARCode
     C                   when      hfld = 'SSARCD_3'
     C                   eval      ssarcd_3  = ARCode
     C                   when      hfld = 'SSARCD_4'
     C                   eval      ssarcd_4  = ARCode
     C                   when      hfld = 'SSARCD_5'
     C                   eval      ssarcd_5  = ARCode
     C                   when      hfld = 'SSARCD_6'
     C                   eval      ssarcd_6  = ARCode
     C                   when      hfld = 'SSARCD_7'
     C                   eval      ssarcd_7  = ARCode
     C                   when      hfld = 'SSARCD_8'
     C                   eval      ssarcd_8  = ARCode
     C                   when      hfld = 'SSARCD_9'
     C                   eval      ssarcd_9  = ARCode
     C                   when      hfld = 'SSARCD_10'
     C                   eval      ssarcd_10 = ARCode
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* loadCtgry - Load A/R catagories after search
    C*====================================================================
    C*
     C     loadCtgry     begsr
    C*
     C                   select
     C                   when      hfld = 'SSCTYP_1'
     C                   eval      ssctyp_1  = CTCTYP
     C                   when      hfld = 'SSCTYP_2'
     C                   eval      ssctyp_2  = CTCTYP
     C                   when      hfld = 'SSCTYP_3'
     C                   eval      ssctyp_3  = CTCTYP
     C                   when      hfld = 'SSCTYP_4'
     C                   eval      ssctyp_4  = CTCTYP
     C                   when      hfld = 'SSCTYP_5'
     C                   eval      ssctyp_5  = CTCTYP
     C                   endsl
    C*
     C                   endsr
    C*
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
    C* loadHeader - Load information to the header fields
    C*====================================================================
    C*
     C     loadHeader    begsr
     C*
     C                   exsr      grp_info
     C                   exsr      keyinz
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
     C                   eval      scprv_emp# = GetPrvEmp#(ELCO#:ELFND:ELEMP#:
     C                                                     ELDIV)
     C                   eval      *in51 = scprv_emp# <> *blanks
     C*
     C                   eval      sccxdt = dtMDYY(GetCxDate(ELCO#:ELFND:
     C                                                       ELEMP#:ELDIV))
     C                   eval      *in52 = sccxdt <> *zero
     C*
     C                   eval      scprem_co = GetPremFin(EL_rrn)
     C                   eval      *in53 = scprem_co <> *blanks
     C*
     C                   eval      scdiv_cnt = DivCount(ELCO#:ELFND:ELEMP#)
     C*
     C***** beg add ***** 07/07/05 ****************************************
     C                   eval      EMASTS = ELASCD
     C***** end add ***** 07/07/05 ****************************************
     C                   clear                   AVDESC
     C                   eval      xxkey = EMASTS
     C     table_key     chain     pav00                              60
     C                   eval      scact_sts = EMASTS + ' ' + AVDESC
      *** beg add *** 10/25/01 *********************************************************************
     C                   eval      *in55 = scact_sts <> *blanks
      *** end add *** 10/25/01 *********************************************************************
     C*
     C                   eval      *in50 = acct_curr(ELCO#:ELFND:ELEMP#:
     C                                               ELDIV:ELFYR)
     C*
     C                   exsr      calc_bal
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   eval      sckey = TTKEY
     C*
     C                   if        validOpt = *off
     C                   if        readBack = *off
     C                   eval      balance = balance + scamt
     C                   else
     C                   eval      balance = balance - priorAmt
     C                   eval      readBack = *off
     C                   endif
     C                   endif
     C*
     C                   if        validOpt = *off
     C                   eval      scbal    = balance
     C                   eval      priorAmt = scamt
     C                   endif
     C*
     C                   select
     C                   when      view# = 1 and info# = 1
     C                   eval      scdata = ' ' + %subst(TTKEY:1:2) + '   '
     C                                    + TTTYPE + '  ' + TTDESC
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
    C*
     C     othfks        begsr
    C*
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
     C*
     C     *inkj         caseq     *on           kjdo                     90
      *** beg add *** 10/24/01 ******************************************************************
     C     *inkn         caseq     *on           kndo
      *** end add *** 10/24/01 ******************************************************************
     C                   endcs
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHOPT - Process other options
    C*====================================================================
    C*
     C     othopt        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     pgmend        begsr
     C*
     C                   if        *inkc=*off and need_emp#='Y'
     C                   eval      rstpgm = *on
     C                   if        *inkl = *off
     C                   clear                   scemp#
     C                   clear                   scdiv
     C                   clear                   scyr
     C                   endif
     C                   exsr      inzsst
     C                   exsr      keyinz
     C                   exsr      clrget
     C                   endif
    C*
     C                   exsr      kc_kl_do
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
     C*
     C                   if        %parms >= 4 and @@co# <> *zero
     C                   move      @@co#         ##co#
     C                   endif
     C*
     C                   if        %parms >= 5 and @@fnd <> *zero
     C                   move      @@fnd         ##fnd
     C                   endif
     C*
     C                   eval      need_emp# = 'Y'
     C*
     C                   if        %parms >= 6 and @@emp# <> *zero
     C                   move      @@emp#        scemp#
     C                   move      @@div         scdiv
     C                   move      @@fyr         scyr
     C                   eval      need_emp# = 'N'
     C                   endif
     C*
     C                   if        %parms >= 9 and @@rrn <> 0
     C                   eval      EL_rrn = @@rrn
     C                   exsr      load_EL
     C                   eval      need_emp# = 'N'
     C                   endif
     C*
     C                   exsr      grp_info
     C*
     C                   endsr
    C*
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
    C*
     C     read          begsr
     C*
     C                   eval      validOpt = *off
    C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C                   eval      db_rrn = dbrrn
     C                   endsl
    C*
     C                   exsr      load_bal
    C*
     C                   endsr
    C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
    C*
     C     readp         begsr
     C*
     C                   eval      validOpt = *off
    C*
     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C                   eval      db_rrn = dbrrn
     C                   endsl
    C*
     C                   eval      readBack = *on
    C*
     C                   exsr      load_bal
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
    C*
     C     setgt         begsr
    C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   eval      firstRead = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SETLL  - Set lower limits on file
    C*====================================================================
    C*
     C     setll         begsr
    C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   eval      firstRead = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*====================================================================
    C*
     C     sf1upd        begsr
     C                   endsr
    C*
    C*====================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*====================================================================
    C*
     C     slctrd        begsr
     C                   endsr
    C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
    C*
     C     subset        begsr
     C*
     C                   eval      meetss = meetSubset
     C*
     C                   if        meetss = *on
     C                   eval      scamt  = accumTotal(TTKEY)
     C                   eval      meetss = scamt <> *zero
     C                   endif
     C*
     C                   endsr
    C*





    P*====================================================================
    P* accumTotal - Accumulate the total amount for an A/R code
    P*====================================================================
    P*
     PaccumTotal       b
    P*
     DaccumTotal       PI            11  2
     D key                                 like(TTKEY)  const

     D total           s             11  2

     C     eaky14        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkarcd
    C*
     C     eaky14_1      klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkarcd
     C                   kfld                    kkpodt

     C                   clear                   total
     C*
     C                   eval      kkarcd = key
     C                   eval      kkpodt = dtCYMD(sspodt_fr)
     C*
     C     eaky14_1      setll     pea14
     C     eaky14        reade     pea14                                  60
     C*
     C                   dow       not *in60 and
     C                              (sspodt_to = *zero or
     C                               EAPODT <= dtCYMD(sspodt_to))
     C*
     C                   eval      total = total + ARValue(EAAMT:EAARTY)
     C*
     C     eaky14        reade     pea14                                  60
     C*
     C                   enddo

     C                   return    total

     PaccumTotal       e




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

     D ##agt_name      s             60
     D ##prod_nm       s             60
     D ##plan          s              1
     D ##asOfDate      s              8  0
     D usa_date        s               d   datfmt(*usa)
     D acct_curr2      s                   like(*in99)

     C                   eval      ##plan = '*'
     C*
     C*PUIMOD
     C*                  move      *date         usa_date
     C                   EVAL      usa_date = %Date()
     C                   move      usa_date      ##asOfDate
     C*
     C                   callp     WA002R(co#:grp:emp#:div:yr:
     C                                    0:##agt_name:0:##prod_nm:
     C                                    0:0:##plan:##asOfDate)
     C*
     C                   eval      acct_curr2 = ##plan = 'A'
     C*
     C                   return    acct_curr2

     Pacct_curr        e





    P*====================================================================
    P* currentBal - Calculate the current balance
    P*====================================================================
    P*
     PcurrentBal       b
    P*
     DcurrentBal       PI            11  2
     D fromDate                       7  0 const
     D toDate                         7  0 const

     D balance         s             11  2

     C     eaky6         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
    C*
     C     eaky6_1       klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    fromDate

     C                   clear                   balance
     C*
     C     eaky6_1       setll     pea06
     C     eaky6         reade     pea06                                  60
     C*
     C                   dow       not *in60 and EAPODT <= toDate
     C*
     C                   if        meetSubset('2') = *on
     C                   eval      balance = balance + ARValue(EAAMT:EAARTY)
     C                   endif
     C*
     C     eaky6         reade     pea06                                  60
     C*
     C                   enddo

     C                   return    balance

     PcurrentBal       e





    P*====================================================================
    P* meetSubset - Determine if the records meet the subset criteria
    P*====================================================================
    P*
     PmeetSubset       b
    P*
     DmeetSubset       PI             1
     D type                           1    const options(*nopass)

     D meetSS          s              1


     C                   eval      meetSS = *on
     C** **
     C** **              if        sspodt_to <> *zero and
     C** **                        (cvtToDate(sspodt_fr) >
     C** **                           cvtToDate(EAPODT) or
     C** **                         cvtToDate(sspodt_to) <
     C** **                           cvtToDate(dtMDYY(EAPODT)))
     C** **              eval      meetss = *off
     C** **              endif
     C*
     C                   if        %parms >= 1 and type = '2'
     C                   clear                   TTCTYP
     C                   clear                   TTKEY
     C                   eval      xxkey = EAARCD
     C     table_key     chain     dbrcdf                             60
     C                   endif
     C*
     C                   if        (ssctyp_1 <> *blanks  or
     C                              ssctyp_2 <> *blanks  or
     C                              ssctyp_3 <> *blanks  or
     C                              ssctyp_4 <> *blanks  or
     C                              ssctyp_5 <> *blanks) and
     C                             (ssctyp_1 <> TTCTYP   and
     C                              ssctyp_2 <> TTCTYP   and
     C                              ssctyp_3 <> TTCTYP   and
     C                              ssctyp_4 <> TTCTYP   and
     C                              ssctyp_5 <> TTCTYP)
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        (ssarcd_1  <> *blanks  or
     C                              ssarcd_2  <> *blanks  or
     C                              ssarcd_3  <> *blanks  or
     C                              ssarcd_4  <> *blanks  or
     C                              ssarcd_5  <> *blanks  or
     C                              ssarcd_6  <> *blanks  or
     C                              ssarcd_7  <> *blanks  or
     C                              ssarcd_8  <> *blanks  or
     C                              ssarcd_9  <> *blanks  or
     C                              ssarcd_10 <> *blanks) and
     C                             (ssarcd_1  <> TTKEY    and
     C                              ssarcd_2  <> TTKEY    and
     C                              ssarcd_3  <> TTKEY    and
     C                              ssarcd_4  <> TTKEY    and
     C                              ssarcd_5  <> TTKEY    and
     C                              ssarcd_6  <> TTKEY    and
     C                              ssarcd_7  <> TTKEY    and
     C                              ssarcd_8  <> TTKEY    and
     C                              ssarcd_9  <> TTKEY    and
     C                              ssarcd_10 <> TTKEY )
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   return    meetSS

     PmeetSubset       e





    P*====================================================================
    P* sflBalance - Calculate the balance prior to the first 'displayed'
    P*====================================================================
    P*
     PsflBalance       b
    P*
     DsflBalance       PI            13  2
     D saveRRN                        9  0  const

     D balance         s             13  2
     D DB_rrn          s              9  0

     C                   clear                   balance
     C                   clear                   EAAMT
     C*
     C                   select
     C                   when      *in21
     C     key           chain     dbrcdf                             60
     C                   eval      DB_rrn = dbrrn
     c                   endsl
     C*
     C                   dow       not *in60 and DB_rrn <> saveRRN
     C*
     C                   if        meetSubset <> *off
     C                   eval      balance = balance + accumTotal(TTKEY)
     C                   endif
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 60
     C                   eval      DB_rrn = dbrrn
     c                   endsl
     C*
     C                   enddo
     C*
     C                   return    balance

     PsflBalance       e





** srchCtgrys
SSCTYP_1
SSCTYP_2
SSCTYP_3
SSCTYP_4
SSCTYP_5
** srchCodes
SSARCD_1
SSARCD_2
SSARCD_3
SSARCD_4
SSARCD_5
SSARCD_6
SSARCD_7
SSARCD_8
SSARCD_9
SSARCD_10
** Column headings
Code Typ Desc                                        Amount            Balance
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change info           NNN
212F12=Cancel               YYY
214F14=Darts                YYN
217F17=Subset               YYY
3
