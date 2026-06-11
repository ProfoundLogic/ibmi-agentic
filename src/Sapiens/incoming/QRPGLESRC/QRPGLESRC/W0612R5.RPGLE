     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE - A/R Historical Balance
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    List A/R information across years with a balance for a         *
    H*    selected employer/div.                                         *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU                                                           *
    H*    W0050R                                                         *
    H*    W1210R02                                                       *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    W0612R                                                         *
    H*    W0612R3                                                        *
    H*    W0612R4                                                        *
    H*                                                                   *
    H*  REWRITE: 06/09/00      C:33      PROJECT: WC696                  *
    H*********************************************************************
    H* REVSIONS:                                                         *
    H*                                                                   *
    H* 05/22/08 C:136 Project: 93WHI                                     *
    H*                Display verbiage if VIP Member                     *
    H*                                                                   *
     H* 09/14/07 C:65  PROJECT: Supp# 60035                               *
     H*                Add a call to CalcTotals after subset screen.      *
    H*                Also, clear all total fields as well.              *
    H*                                                                   *
    H* 03/26/07 C:136 Support: WA136 53281                               *
    H*                Populate LDA so that the A/R Inquiry programs      *
    H*                called by this program can have the same subset    *
    H*                data as this one.                                  *
    H*                                                                   *
     H* 03/08/06 C: 132 PROJECT: WC071 - 51160                            *
     H*                - Change *DATE to use The Current Date from DSDTTM *
     H*                  Data Structure DSDATE.                           *
    H*                                                                   *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
    H*                                                                   *
    H* 02/19/04 C:136 Project: WC1033 (Return Premium)                   *
    H*                Add processing for EAARTY = 'T' (true void)        *
    H*                transaction.                                       *
    H*                                                                   *
    H* 12/19/01 C:136 WA091 Supp# 30426                                  *
    H*                DARTS not called correctly.  Corrected the values  *
    H*                being loaded to QCKEY.                             *
    H*                                                                   *
    H* 10/25/01 C:136 WA081 Supp# 29415                                  *
    H*                Add reverse image on activity status if there is one,
    H*                otherwise, do not display the field.               *
    H*                                                                   *
    H* 10/24/01  C:136 WA081 Supp# 29413                                 *
    H*                 Add F-14 for Darts Access                         *
    H*                                                                   *
    H* 10/08/01  C:35   PROJECT: WC696                                   *
    H*                  CHANGE TO CALL W0612R DIRECTLY, SINCE PROJECT    *
    H*                  HAS BEEN RELEASED ON BASE TAPE.                  *
    H*                                                                   *
    H*********************************************************************
     Fw0612f5   cf   e             workstn infds(fids01)
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdelp     if   e             disk    rename(pel01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdell5    if   e           k disk    rename(pel01:dbrcdf)
     F                                     infds(dbfds)

     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwmeal6    if   e           k disk    rename(pea01:pea06)
     Fwtabl     if   e           k disk    include(ptt01)
     Fwtavl     if   e           k disk    rename(pav01:pav00)

     D hd              s             78    dim(1) ctdata perrcd(1)
     *** beg chg *** 10/24/01 **********************************************************************
     D************************ of              s              1    dim(13) ctdata perrcd(1)
     D************************ ofd             s             35    dim(13) alt(of)
     D of              s              1    dim(14) ctdata perrcd(1)
     D ofd             s             35    dim(14) alt(of)
     *** end chg *** 10/24/01 **********************************************************************
     *** beg add *** 10/24/01 **********************************************************************
     D  qckey          ds            40
     D    qcco#                1      3
     D    qcfnd                4      6
     D    qcemp#               7     15
     D    qcdiv               16     20
     *** end add *** 10/24/01 **********************************************************************
      *
      ***** BEG ADD ***** 03/08/06 *******************************************************
      *
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
      ***** END ADD ***** 03/08/06 *******************************************************
      *** beg add ***** 03/26/07 *********************************************************
     D lda             ds          1024
     D  sspodt_fr                     8  0 inz(0)
     D  sspodt_to                     8  0 inz(0)
     D  sstrdt_fr                     8  0 inz(0)
     D  sstrdt_to                     8  0 inz(0)
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
     D  Ssctype_1                     1
     D  Ssctype_2                     1
     D  Ssctype_3                     1
     D  Ssctype_4                     1
     D  Ssctype_5                     1
      *** end add ***** 03/26/07 *********************************************************
      *
    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'A/R Historical Balance'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   9

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D EMCO#_a         s              3
     D EMFND_a         s              3
     D EMEMP#_a        s              9
     D EMDIV_a         s              5
     D scfyr_a         s              3
     D kkfyr           s                   like(ELFYR)
     D***** beg add ***** 07/07/05 ****************************************
     D fgfyr           s                   like(*IN90)
     D***** end add ***** 07/07/05 ****************************************
     D xxkey           s                   like(AVKEY)
     D ctgy            s                   like(TTCTYP)
     D cx_reason       s             65
     D balanceDt       s              8  0
     D need_emp#       s              1
     D error           s              1
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     *** beg add *** 10/24/01 *****************************************************************
     D qcpgm           s             10
     *** end add *** 10/24/01 *****************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     **** BEGIN CHG **** 10/08/01 ********************************************
     D********** W0612R           PR                  extpgm('W0612C')
     DW0612R           PR                  extpgm('W0612R')
     **** END   CHG **** 10/08/01 ********************************************
     D ##kc                           1
     D ##kl                           1
     D ##call                         1    const
     D #@co#2                         3    const
     D #@fnd2                         3    const
     D ##emp#2                        9    const
     D ##div2                         5    const
     D ##fyr2                         3    const
     D ##date                         8    const
     D ##amount                       9    const
     D ##chk#                         7    const
     D ##rtcd                         1    const
     D ##rrn                          9  0 const

     DW0612R3          PR                  extpgm('W0612R3')
     D ##kc                           1
     D ##kl                           1
     D ##call                         1                 const
     D #@co#                               like(EMCO#)  const
     D #@fnd                               like(EMFND)  const
     D ##emp#                              like(EMEMP#) const
     D ##div                               like(EMDIV)  const
     D ##fyr                               like(ELFYR)  const
     D ##rrn                          9  0              const

     DW0612R4          PR                  extpgm('W0612R4')
     D ##kc                           1
     D ##kl                           1
     D ##call                         1                 const
     D #@co#                               like(EMCO#)  const
     D #@fnd                               like(EMFND)  const
     D ##emp#                              like(EMEMP#) const
     D ##div                               like(EMDIV)  const
     D ##fyr                               like(ELFYR)  const
     D ##rrn                          9  0              const

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

     Dacct_curr        PR             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D yr                                  like(ELFYR)  const

     DAR_value         PR             9  2
     D AR_amount                      9  2  const
     D AR_type                        1     const

     Dcategory         PR             1
     D co#                                 like(EACO#)  const
     D code                                like(EAARCD) const

     DmeetSubset       PR             1
     D type                           1    const options(*nopass)

     D/copy SPRDATE
     D/copy SPRMESSAGE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
     D/copy SPRWRKWTH

     D/copy WPRGROUP
     D/copy WPREMPLR

     C/COPY SCOPYWW41C

      *** beg add ***** 03/26/07 ******************************************
     C     *dtaara       define    *lda          lda
      *** end add ***** 03/26/07 ******************************************
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
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
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
    C*
     C     key1          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C                   kfld                    kkfyr
    C*
     C     table_key     klist
     C                   kfld                    EMCO#
     C                   kfld                    xxkey

    C*====================================================================
    C* calcTotals - Calculate total amounts for header and footer
    C*====================================================================
    C*
     C     calcTotals    begsr
    C*
     C                   clear                   scttl_bill
     C                   clear                   scttl_paid
     c*** begin add ***  09/14/07  **************************************
     C                   clear                   scttl_prem
     C                   clear                   scttl_expc
     C                   clear                   scttl_dep
     C                   clear                   scttl_fees
     c*** end add *****  09/14/07  **************************************
     C*
     C     key           chain     pea06                              60
     C*
     C                   dow       not *in60
     C*
     C                   if        meetSubset = *on
     C*
     C                   eval      ctgy = category(EACO#:EAARCD)
     C*
     C                   if        ctgy = 'D'
     C                   select
     C                   when      EAARTY = 'P' or EAARTY = 'I' or
     C                             EAARTY = 'Q'
     C                   eval      scttl_paid = scttl_paid + EAAMT
     C                   other
     C                   eval      scttl_bill = scttl_bill + EAAMT
     C                   endsl
     C                   endif
     C*
     C                   select
     C                   when      ctgy = 'P'
     C                   eval      scTtl_Prem = scTtl_Prem +
     C                                         AR_value(EAAMT:EAARTY)
     C                   when      ctgy = 'E'
     C                   eval      scTtl_Expc = scTtl_Expc +
     C                                         AR_value(EAAMT:EAARTY)
     C                   when      ctgy = 'D'
     C                   eval      scTtl_Dep  = scTtl_Dep  +
     C                                         AR_value(EAAMT:EAARTY)
     C                   other
     C                   eval      scTtl_Fees = scTtl_Fees +
     C                                         AR_value(EAAMT:EAARTY)
     C                   endsl
     C*
     C                   endif
     C*
     C     key           reade     pea06                                  60
     C*
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
    C*
     C     chkssm        begsr
    C*
     C                   eval      *in11 = (ssBalDue <> *blank)
    C*
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
     C*
     C                   exsr      loadHeader
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*====================================================================
    C*
     C     errck1        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*====================================================================
    C*
     C     errck2        begsr
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
     C                   exsr      showBal
     c*** begin add ***  09/14/07  **************************************
     C                   exsr      calcTotals
     c*** end add *****  09/14/07  **************************************
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
     C                   clear                   sspodt_fr
     C                   clear                   sspodt_to
     C*
     C                   if        need_emp# = 'Y'
     C                   exsr      criteria
     C                   endif
     C*
     C                   if        %parms >= 1 and @@call = '2'
     C                   eval      ssBalDue = 'Y'
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
    C*
     C     kjdo          begsr
    C*
     C                   select
     C*____________________________________________________________________
     C*
     C                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     C*
     C                   eval      EM_rrn = empSrch(*inkc:*inkl:error:##fnd)
    C*
     C     EM_rrn        chain     pem0p                              60
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
     C                   endsl
    C*
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
     C                   eval      kkfyr = *hival
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
    C*
     C     keyset        begsr
     C*
     C                   select
     C                   when      *in21
     C                   move      scpst1        kkfyr
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
    C*
     C     keysfl        begsr
    C*
     C                   eval      kkfyr = scfyr
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
      *** beg chg **** 12/19/01 ********************************************************************
     C****************   move      ##co#         qcco#
     C****************   move      scfnd         qcfnd
     C****************   move      scemp#        qcemp#
     C****************   move      scdiv         qcdiv
     C                   move      emco#         qcco#
     C                   move      emfnd         qcfnd
     C                   move      ememp#        qcemp#
     C                   move      emdiv         qcdiv
      *** end chg **** 12/19/01 ********************************************************************
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
    C* LOADAMTS - Load amounts for screen fields
    C*====================================================================
    C*
     C     loadAmts      begsr
    C*
     C                   clear                   scprem
     C                   clear                   scexpc
     C                   clear                   scdep
     C                   clear                   scfees
     C*
     C***** beg add ***** 07/07/05 ****************************************
     C                   if        fgfyr = *OFF
     C                   clear                   AVDESC
     C                   eval      xxkey = ELASCD
     C     table_key     chain     pav00                              60
     C                   eval      scact_sts = ELASCD + ' ' + AVDESC
     C                   eval      *in53 = scact_sts <> *blanks
     C                   eval      fgfyr = *ON
     C                   endif
     C***** end add ***** 07/07/05 ****************************************
     C*
     C                   eval      kkfyr = ELFYR
     C*
     C     key1          chain     pea06                              60
     C*
     C                   dow       not *in60
     C*
     C                   if        meetSubset('2') = *on
     C*
     C                   eval      ctgy = category(EACO#:EAARCD)
     C*
     C                   select
     C                   when      ctgy = 'P'
     C                   eval      scprem = scprem + AR_value(EAAMT:EAARTY)
     C                   when      ctgy = 'E'
     C                   eval      scexpc = scexpc + AR_value(EAAMT:EAARTY)
     C                   when      ctgy = 'D'
     C                   eval      scdep  = scdep  + AR_value(EAAMT:EAARTY)
     C                   other
     C                   eval      scfees = scfees + AR_value(EAAMT:EAARTY)
     C                   endsl
     C*
     C                   endif
     C*
     C     key1          reade     pea06                                  60
     C*
     C                   enddo
    C*
     C                   endsr
    C*
    C*====================================================================
    C* LOADHEADER - Load header fields
    C*====================================================================
    C*
     C     loadHeader    begsr
     C*
     C     EM_rrn        chain     pem0p                              60
     C*
     C                   if        not *in60
     C                   eval      ##co# = EMCO#
     C                   eval      ##fnd = EMFND
     C                   endif
    C*
     C                   exsr      grp_info
     C*
     C                   eval      scedn = EmpDivName(EMEMP#:EMDIV:EM_rrn:
     C                                                EMFND:EMCO#)
     C*
     C                   eval      scprv_emp# = GetPrvEmp#(EMCO#:EMFND:EMEMP#:
     C                                                     EMDIV)
     C                   eval      *in51 = scprv_emp# <> *blanks
     C*
     C                   eval      sccxdt = dtMDYY(GetCxDate(EMCO#:EMFND:
     C                                            EMEMP#:EMDIV:cx_reason))
     C                   eval      sccx_desc = cx_reason
     C                   eval      *in52 = sccxdt <> *zero
     C*
     C                   eval      scdiv_cnt = DivCount(EMCO#:EMFND:EMEMP#)
     C*
     C***** beg del ***** 07/07/05 ****************************************
     C****************** clear                   AVDESC
     C****************** eval      xxkey = EMASTS
     C*****table_key**** chain     pav00                              60
     C****************** eval      scact_sts = EMASTS + ' ' + AVDESC
     C***** end del ***** 07/07/05 ****************************************
      *** beg add *** 10/25/01 *********************************************************************
     C                   eval      *in53 = scact_sts <> *blanks
      *** end add *** 10/25/01 *********************************************************************
     C*
     C                   callp     sndPgmMsg('CPF9898':'*STATUS':'QCPFMSG':
     C                             '*LIBL':
     C                             'Calculating.  This may take a few -
     C                             moments...')
     C*
     C                   exsr      showBal
     C                   exsr      calcTotals
      *
      *** beg add ***** 05/22/08 *******************************************************
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3
     c                   if        clientID = '093' and
     c                             EMVIP = 'Y'
     c                   eval      *in59 = *on
     c                   else
     c                   eval      *in59 = *off
     c                   endif
      *** end add ***** 05/22/08 *******************************************************
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
    C*
     C                   eval      scfyr  = ELFYR
     C*
     C                   clear                   scfaf
     C                   if        ELFAF = 'A'
     C                   eval      scfaf  = ELFAF
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OPT7 - Process option 7
    C*====================================================================
    C*
     C     opt7          begsr
    C*
     C                   move      EMCO#         EMCO#_a
     C                   move      EMFND         EMFND_a
     C                   move      EMEMP#        EMEMP#_a
     C                   move      EMDIV         EMDIV_a
     C                   move      scfyr         scfyr_a
     C*
     C                   callp     W0612R(*inkc:*inkl:'1':EMCO#_a:EMFND_a:
     C                                    EMEMP#_a:EMDIV_a:scfyr_a:' ':' ':
     C                                    ' ':' ':scdbrr)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OPT15 - Process option 15
    C*====================================================================
    C*
     C     opt15         begsr
     C*
     C                   callp     W0612R3(*inkc:*inkl:'1':0:0:0:0:0:scdbrr)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OPT16 - Process option 16
    C*=================================================================
    C*
     C     opt16         begsr
     C*
     C                   callp     W0612R4(*inkc:*inkl:'1':0:0:0:0:0:scdbrr)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
    C*
     C     othfks        begsr
     C*
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
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
    C*
      *** beg add ***** 03/26/07 ******************************************
     c                   out       lda
      *** end add ***** 03/26/07 ******************************************
     C     scsel         caseq     '7'           opt7
     C     scsel         caseq     '15'          opt15
     C     scsel         caseq     '16'          opt16
     C                   endcs
    C*
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
     C                   eval      need_emp# = 'Y'
     C*
     C                   if        %parms >= 4 and @@rrn <> *zero
     C                   eval      EM_rrn = @@rrn
     C                   eval      need_emp# = 'N'
     C                   eval      @@rrn  = *zero
     C                   endif
     C*
     C                   exsr      loadHeader
     C*
     C                   endsr
    C*
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
    C*
     C     read          begsr
    C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C  n95              exsr      loadAmts
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
    C*
     C     readp         begsr
    C*
     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C  n95              exsr      loadAmts
     C                   endsl
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
    C* showBal - Load fields for 'Balance through:'
    C*====================================================================
    C*
     C     showBal       begsr
    C*
     C                   if        sspodt_to <> *zero
     C                   eval      balanceDt = sspodt_to
     C                   else
      ***** BEG CHG ***** 03/08/06 *******************************************************
     C**************     eval      balanceDt = *date
     C                   TIME                    DSDTTM
     C                   move      DSDATE        balanceDT                      mmddyyyy
      ***** END CHG ***** 03/08/06 *******************************************************
     C                   endif
     C*
     C                   eval      scbal_to = '  Balances through: ' +
     C                                        %trim(alphaUSA(cvtToDate(
     C                                                       balanceDt)))
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*====================================================================
    C*
     C     slctrd        begsr
    C*
     C                   eval      @@rrn = scdbrr
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
    C*
     C     subset        begsr
     C*
     C                   eval      meetss = meetSubset('1')
     C*
     C                   if        meetss = *on
    C*
     C                   select
    C*
     C                   when       ssBalDue = 'Y' and
     C                              (scprem = *zero and scexpc = *zero and
     C                               scdep  = *zero and scfees = *zero)
     C                   eval       meetss = *off
    C*
     C                   endsl
     C*
     C                   endif
    C*
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

     D ##agt_name      s             60
     D ##prod_nm       s             60
     D ##plan          s              1
     D ##asOfDate      s              8  0
     D usa_date        s               d   datfmt(*usa)
     D acct_curr2      s                   like(*in99)

     C                   eval      ##plan = '*'
     C*
      ***** BEG CHG ***** 03/08/06 *******************************************************
     C*************      move      *date         usa_date
     C                   TIME                    DSDTTM
     C     *usa          move      DSDATE        usa_date                        mmddyyyy
      ***** END CHG ***** 03/08/06 *******************************************************
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
    P* AR_value - Determine if A/R amount is positive or negative
    P*====================================================================
    P*
     PAR_value         b
    P*
     DAR_value         PI             9  2
     D AR_amount                      9  2  const
     D AR_type                        1     const

     D amount          s              9  2

     C                   select
      *** beg chg ***** 02/19/04 ***********************************************
     C*****************  when      AR_type = 'P' or AR_type = 'Q' or
     C*****************            AR_type = 'I'
     C                   when      AR_type = 'P' or AR_type = 'Q' or
     C                             AR_type = 'I' or AR_type = 'T'
      *** end chg ***** 02/19/04 ***********************************************
     C                   eval      amount = AR_amount * -1
     C                   other
     C                   eval      amount = AR_amount
     C                   endsl
     C*
     C                   return    amount

     PAR_value         e





    P*====================================================================
    P* category - Get the category for the A/R code
    P*====================================================================
    P*
     Pcategory         b
    P*
     Dcategory         PI             1
     D co#                                 like(EACO#)  const
     D code                                like(EAARCD) const

     D ctgy            s                   like(TTCTYP)

     C                   clear                   TTCTYP
     C                   eval      xxkey = code
     C     table_key     chain     ptt01                              60
     C                   eval      ctgy = TTCTYP
     C*
     C                   return    ctgy

     Pcategory         e





    P*====================================================================
    P* meetSubset - Determine if the records meet the subset criteria
    P*====================================================================
    P*
     PmeetSubset       b
    P*
     DmeetSubset       PI             1
     D type                           1    const options(*nopass)

     D meetSS          s              1
     D testPODT        s              1
     D testAcct        s              1

     D co#             s                   like(ELCO#)
     D group           s                   like(ELFND)
     D emp#            s                   like(ELEMP#)
     D div             s                   like(ELDIV)
     D yr              s                   like(ELFYR)

     C                   eval      meetSS   = *on
     C                   eval      testPODT = *on
     C                   eval      testAcct = *on
     C*
     C                   eval      co#   = EACO#
     C                   eval      group = EAFND
     C                   eval      emp#  = EAEMP#
     C                   eval      div   = EADIV
     C                   eval      yr    = EAFYR
     C*
     C                   if        %parms >= 1 and
     C                             (type = '1')
     C                   eval      testPODT = *off
     C                   eval      co#   = ELCO#
     C                   eval      group = ELFND
     C                   eval      emp#  = ELEMP#
     C                   eval      div   = ELDIV
     C                   eval      yr    = ELFYR
     C                   endif
     C*
     C                   if        %parms >= 1 and
     C                             (type = '2')
     C                   eval      testAcct = *off
     C                   endif
     C*
     C                   select
     C*
     C                   when      testPODT = *on and
     C                             (EAPODT < dtCYMD(sspodt_fr) or
     C                              EAPODT > dtCYMD(balanceDt))
     C                   eval      meetSS = *off
    C*
     C                   when      testAcct = *on and
     C                             acct_curr(co#:group:emp#:div:yr)
     C                              = *on
     C                   eval       meetss = *off
     C*
     C                   endsl
     C*
     C                   return    meetSS

     PmeetSubset       e





** Column headings
Opt   Yr    Premium Due   Expense Const     Deposit Due        Fees Due    Adt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
1077=A/R Detail             YYY
11515=Category detail       YYY
11616=Category summary      YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
214F14=Darts                YYN
217F17=Subset               YYY
3
