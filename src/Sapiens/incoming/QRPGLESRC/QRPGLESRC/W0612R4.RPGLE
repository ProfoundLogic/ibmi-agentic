     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE - A/R Category Summary
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    Display category inormation about a selected employer/div/yr.  *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU                                                           *
    H*    W0612R                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    W0612R1                                                        *
    H*                                                                   *
    H*  REWRITE: 06/09/00      C:33      PROJECT: WC696                  *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H* 07/07/06 C:33  Seq#: WA136/53498                                  *
    H*                Load subset from W0612R at the appropriate time.   *
    H*                                                                   *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
    H*                                                                   *
    H* 02/19/04 C:136 Project: WC1033 (Return Premium)                   *
    H*                Add processing for EAARTY = 'T' (true void)        *
    H*                transaction.                                       *
    H*                                                                   *
    H*  06/12/03 C:136 Support: 37546                                    *
    H*                 Accumulate billings and payments correctly.       *
    H*                                                                   *
    H*  11/14/01 C:142 WC969                                             *
    H*           Change subset screen to have exclude categories as      *
    H*           well as include categories.                             *
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
     Fw0612f4   cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtttp     if   e             disk    rename(ptt01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtttl1    if   e           k disk    rename(ptt01:dbrcdf)
     F                                     infds(dbfds)

     Fwmeal14   if   e           k disk    rename(pea01:pea14)
     F                                     infds(eafds14)
     Fwtctl     if   e           k disk    rename(pct01:pct00)
     Fwtavl     if   e           k disk    rename(pav01:pav00)
     *** Beg Add *** 11/14/01 *****************************************************************
     Fwmexl     if   e           k disk
     *** End Add *** 11/14/01 *****************************************************************
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwtctp     if   e             disk    rename(pct01:pct0p)
      /eject
     D srchCtgrys      s             10    dim(5) ctdata perrcd(1)
     *** Beg Add *** 11/14/01 *****************************************************************
     D srchCtgrysX     s             10    dim(5) ctdata perrcd(1)
     *** End Add *** 11/14/01 *****************************************************************
     D srchCodes       s             10    dim(10) ctdata perrcd(1)

     D hd              s             78    dim(2) ctdata perrcd(1)
     *** beg chg *** 10/24/01 **********************************************************************
     D************************ of              s              1    dim(12) ctdata perrcd(1)
     D************************ ofd             s             35    dim(12) alt(of)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
     *** end chg *** 10/24/01 **********************************************************************
     *** beg add *** 10/24/01 **********************************************************************
     D  qckey          ds            40
     D    qcco#                1      3
     D    qcfnd                4      6
     D    qcemp#               7     15
     D    qcdiv               16     20
     *** end add *** 10/24/01 **********************************************************************
     D eafds14         ds
     D  earrn14              397    400b 0

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
     *** Beg Add *** 11/14/01 *****************************************************************
     D  Ssctype_1                     1
     D  Ssctype_2                     1
     D  Ssctype_3                     1
     D  Ssctype_4                     1
     D  Ssctype_5                     1
     *** End Add *** 11/14/01 *****************************************************************

    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'A/R Category Summary'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   2
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
     D ##fnd           s              3  0
     D ##fnam          s             40
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     *** Beg Add *** 11/14/01 *****************************************************************
     D First           s              1
     *** End Add *** 11/14/01 *****************************************************************
     D kkco#           s                   like(TTCO#)
     D kkctyp          s                   like(TTCTYP)
     D kktype          s                   like(TTTYPE)
     D kkkey           s                   like(TTKEY)
     D kkfnd           s                   like(EAFND)
     D kkfyr           s                   like(EAFYR)
     D kkemp#          s                   like(EAEMP#)
     D kkdiv           s                   like(EADIV)
     D kkarcd          s                   like(EAARCD)
     D xxkey           s                   like(TTKEY)
     D need_emp#       s              1
     D billings        s             11  2
     D payments        s             11  2
     D balance         s             13  2
     D totalBill       s             13  2
     D totalPaid       s             13  2
     D length          s              3  0
     D remainder       s              3  0
     D dots            s              1  0
     D error           s              1
     D loadLDA         s              1
     D srchCtgry       s                   like(*in60)
     *** Beg Add *** 11/14/01 *****************************************************************
     D srchCtgryx      s                   like(*in60)
     *** End Add *** 11/14/01 *****************************************************************
     D srchCode        s                   like(*in60)
     D balanceDt       s              8  0
     D thruDt          s              8  0
     D x               s              3  0
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D CT_rrn          s              9  0
     D ARCode          s             10
     D sysDate         s               d
     D dateField       s               d   inz datfmt(*usa)
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

     DW0612R1          PR                  extpgm('W0612R1')
     D ##prds                        40                 const
     D ##call                         1                 const
     D ##kc                           1
     D ##kl                           1
     D #@co#                               like(EMCO#)  const
     D #@fnd                               like(EMFND)  const
     D ##emp#                              like(EMEMP#) const
     D ##div                               like(EMDIV)  const
     D ##fyr                               like(ELFYR)  const
     D ##rrn                          9  0              const

     DsrChkPgm         PR                  extpgm('SRCHKPGM')
     D ##pgm                         10    const
     D ##LoadLDA                      1

     Dacct_curr        PR             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D yr                                  like(ELFYR)  const

     DmeetSubset       PR             1
     D type                           1    const options(*nopass)

     D/copy SPRDATE
     D/copy SPRMESSAGE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
     D/copy SPRWRKWTH

     D/copy WPRGROUP
     D/copy WPREMPLR
     D/copy WPRAR

      /eject
     C/COPY SCOPYWW41C

      /eject
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
     C                   kfld                    kkctyp
    C*
     C     key1_1        klist
     C                   kfld                    kkco#
     C                   kfld                    kkctyp
     C                   kfld                    kktype
     C                   kfld                    kkkey
    C*
     C     eaky14        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkarcd
    C*
     C     ctky          klist
     C                   kfld                    kkco#
     C                   kfld                    TTCTYP
    C*
     C     table_key     klist
     C                   kfld                    ##co#
     C                   kfld                    xxkey
    C*
     *** Beg Add *** 11/14/01 *****************************************************************
     C     Wmexl_Key     klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     *** End Add *** 11/14/01 *****************************************************************

      /eject
    C*====================================================================
    C* ACCUMULATE - Accumulate the amounts for each categroy
    C*====================================================================
    C*
     C     accumulate    begsr
     C*
     C                   eval      *in97    = *off
     C                   eval      kkctyp   = TTCTYP
     C*
     C                   exsr      clrPayBill
     C*
     C                   dow       not *in97
     C*
     C                   if        meetSubset = *on
     C*
     C                   eval      kkarcd = TTKEY
     C*
     C     eaky14        chain     pea14                              60
     C*
     C                   dow       not *in60
     C** **                         and sysDate >= CvtToDate(EAPODT)
     C*
     C                   if        meetSubset('2') = *on
     C*
     C                   select
      *** beg chg ***** 02/19/04 ***********************************************
     C*****************  when      EAARTY = 'P' or EAARTY = 'I' or
     C*****************            EAARTY = 'Q'
     C                   when      EAARTY = 'P' or EAARTY = 'I' or
     C                             EAARTY = 'Q' or EAARTY = 'T'
      *** end chg ***** 02/19/04 ***********************************************
     C                   eval      payments = payments + EAAMT
      *** beg add ***** 06/12/03 ***********************************************
     C                   when      EAARTY = 'R' or EAARTY = 'N' or
     C                             EAARTY = 'V'
     C                   eval      payments = payments - EAAMT
      *** end add ***** 06/12/03 ***********************************************
     C                   other
     C                   eval      billings = billings + EAAMT
     C                   endsl
     C*
     C                   endif
     C*
     C     eaky14        reade     pea14                                  60
     C*
     C                   enddo
     C*
     c                   endif
     C*
     C     key1          reade     dbrcdf                                 97
     C*
     C                   enddo
     C*
     C                   eval      balance = balance + billings - payments
     C*
     C     key1          setgt     dbrcdf
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* CALC_BAL - Calculate header balance
    C*====================================================================
    C*
     C     calc_bal      begsr
    C*
     C                   eval      *in54 = *off
    C*
     C                   if        sspodt_to <> *zero
     C                   eval      *in54     = *on
     C                   eval      thruDt = sspodt_to
     C                   else
     C*PUIMOD
     C*                  eval      thruDt = *date
     C                   Eval      thruDt = %Int(%Char(%Date():*USA0))
     C                   endif
    C*
     C** **              eval      sccurr = currentBal(0:dtCYMD(balanceDt))
     C*
     C                   eval      scbal_to = '      Balances as of ' +
     C                                        %trim(alphaMDY(cvtToDate(
     C                                                       thruDt)))
     C** **                                   + ':'
     C*
     C                   if        *in54
     C*
     C                   eval      dateField = cvtToDate(sspodt_fr)
     C                   subdur    1:*d          dateField
     C                   move      dateField     balanceDt
    C*
     C** **              eval      scprev = currentBal(0:dtCYMD(balanceDt))
     C*
     C                   eval      scbal_fr = '      Balances after ' +
     C                                        %trim(alphaMDY(cvtToDate(
     C                                                       sspodt_fr)))
     C** **                                   + ':'
    C*
     C                   endif
    C*
     C                   endsr
    C*
      /eject
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
     *** Beg Add *** 11/14/01 *****************************************************************
     C                                      Ssctype_1 <> *blanks or
     C                                      Ssctype_2 <> *blanks or
     C                                      Ssctype_3 <> *blanks or
     C                                      Ssctype_4 <> *blanks or
     C                                      Ssctype_5 <> *blanks)
     C*
     C                   exsr      calc_bal
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* CLRPAYBILL - Clear payment and billing fields
    C*====================================================================
    C*
     C     clrPayBill    begsr
    C*
     C                   eval      billings = *zero
     C                   eval      payments = *zero
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
     *** Beg Add *** 11/14/01 *****************************************************************
     C                   Eval      *in41 = *off
     C                   movea     '0000000000'  *in(61)
     *** End Add *** 11/14/01 *****************************************************************
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
     C                   endsr
    C*
      /eject
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
      /eject
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
      /eject
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
     *** Beg Add *** 11/14/01 *****************************************************************
     C                   Clear                   Ssctype_1
     C                   Clear                   Ssctype_2
     C                   Clear                   Ssctype_3
     C                   Clear                   Ssctype_4
     C                   Clear                   Ssctype_5
     *** End Add *** 11/14/01 *****************************************************************
     C*
     C***  Begin Del  ***  07/07/06  **************************************
     C**********         exsr      loadHeader
     C***  End   Del  ***  07/07/06  **************************************
     C*
     C                   callp     srChkPgm('W0612R':loadLDA)
     C                   if        loadLDA = *on
     C                   in        lda
     C                   endif
     C*
     C***  Begin Add  ***  07/07/06  **************************************
     C                   exsr      loadHeader
     C***  End   Add  ***  07/07/06  **************************************
     C*
     C                   endsr
    C*
      /eject
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
      /eject
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
    C*
     C     keyinz        begsr
     C*
     C                   clear                   kkctyp
     C                   clear                   kktype
     C                   clear                   kkkey
     C*
     C                   eval      kkco#  = ##co#
     C                   eval      kkfnd  = ##fnd
     C                   eval      kkemp# = scemp#
     C                   eval      kkdiv  = scdiv
     C                   eval      kkfyr  = scyr
     C*
     C                   eval      balance   = *zero
     C                   eval      totalBill = *zero
     C                   eval      totalPaid = *zero
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
    C*
     C     keyset        begsr
     C*
     C                   clear                   kkctyp
     C                   clear                   kktype
     C                   clear                   kkkey
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
    C*
     C     keysfl        begsr
     C*
     C                   eval      kkctyp = scctyp
     C                   eval      kktype = sctype
     C                   eval      kkkey  = sckey
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KGDO - Processing for F7
    C*====================================================================
    C*
     C     kgdo          begsr
     C*
     C                   callp     W0612R1(' ':'D':*inkc:*inkl:##co#:##fnd:
     C                                     scemp#:scdiv:scyr:EL_rrn)
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
    C*
     C     kjdo          begsr
     C*
     C     hfld          lookup    srchCtgrys                             60
     C                   eval      srchCtgry = *in60
     C*
     *** Beg Add *** 11/14/01 *****************************************************************
     C     hfld          lookup    srchCtgrysX                            60
     C                   eval      srchCtgryX = *in60
     *** End Add *** 11/14/01 *****************************************************************
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
     *** Beg Add *** 11/14/01 *****************************************************************
     C*
     C                   when      srchCtgryx = *on
     C*
     C                   eval      x = 1
     C     hfld          lookup    srchCtgrysx(x)                         60
     C*
     C                   eval      CT_rrn = ARCatSrch(*inkc:*inkl)
     C*
     C                   eval      *in41 = *on
     C*
     C     CT_rrn        chain     pct0p                              60
     C*
     C                   if        *in60 = *off
     C                   exsr      loadCtgryx
     C                   eval      *in(x + 60 + 1) = *on
     C                   else
     C                   eval      *in(x + 60) = *on
     C                   endif
     *** End Add *** 11/14/01 *****************************************************************
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
      /eject
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
      /eject
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
      /eject
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
      /eject
     *** Beg Add *** 11/14/01 *****************************************************************
    C*====================================================================
    C* loadCtgryx - Load A/R catagories excluded after search
    C*====================================================================
    C*
     C     LoadCtgryx    Begsr
    C*
     C                   Select
     C                   When      Hfld = 'SSCTYPE_1'
     C                   Eval      Ssctype_1  = CTCTYP
     C                   When      Hfld = 'SSCTYPE_2'
     C                   Eval      Ssctype_2  = CTCTYP
     C                   When      Hfld = 'SSCTYPE_3'
     C                   Eval      Ssctype_3  = CTCTYP
     C                   When      Hfld = 'SSCTYPE_4'
     C                   Eval      Ssctype_4  = CTCTYP
     C                   When      Hfld = 'SSCTYPE_5'
     C                   Eval      Ssctype_5  = CTCTYP
     C                   Endsl
    C*
     C                   Endsr
    C/eject
     *** End Add *** 11/14/01 *****************************************************************
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
     C     EL_rrn        chain     wdelp                              60
     C*
     C                   eval      scedn = EmpDivName(ELEMP#:ELDIV:EM_rrn:
     C                                                ELFND:ELCO#)
     C*
     C     EM_rrn        chain     wmemp                              60
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
     C*
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
    C* loadTotals - Load the total line
    C*====================================================================
    C*
     C     loadTotals    begsr
     C*
     C                   if        not *in59
     C*
     C                   eval      *in59  = *on
     C                   eval      *in95  = *off
     C*
     C                   eval      scctyp = *hival
     C                   eval      sctype = *hival
     C                   eval      sckey  = *hival
     C*
     C                   eval      scdata = 'Total . . . . . . . :'
     C*
     C                   eval      scbill    = totalBill
     C                   eval      scpaid    = totalPaid
     C                   eval      scbal     = balance
     C*
     C                   else
     C*
     C                   eval      *in59  = *off
     C*
     C                   endif
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   if        not *in59
     C*
     C                   eval      scctyp = TTCTYP
     C                   eval      sctype = TTTYPE
     C                   eval      sckey  = TTKEY
     C*
     C                   eval      scdata = '. . . . . . . . . . :'
     C                   eval      dots   = 0
     C                   eval      CTDESC = *blanks
     C*
     C     ctky          chain     pct00                              60
     C*
     C     ' '           checkr    CTDESC        length
     C                   div       2             remainder
     C                   mvr                     remainder
     C*
     C                   if        length > 15 and length < 20
     C                   eval      length = 19
     C                   endif
     C*
     C                   if        remainder = 0 and length <> 20
     C                   eval      dots = 1
     C                   endif
     C*
     C                   if        CTDESC <> *blank
     C                   eval      %subst(scdata:1:length + dots) =
     C                               %subst(CTDESC:1:length)
     C                   endif
     C*
     C                   eval      scbill = billings
     C                   eval      scpaid = payments
     C*
     C                   select
     C                   when      info# = 1
     C                   eval      scbal = balance
     C                   when      info# = 2
     C                   eval      scbal = billings - payments
     C                   endsl
     C*
     C                   eval      totalBill = totalBill + billings
     C                   eval      totalPaid = totalPaid + payments
     C*
     C                   endif
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
    C*
     C     othfks        begsr
    C*
     *** Beg Add *** 11/14/01 *****************************************************************
     C                   Eval      *in41 = *off
     C                   movea     '0000000000'  *in(61)
     *** End Add *** 11/14/01 *****************************************************************
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
     C*
     C     *inkg         caseq     *on           kgdo
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
      /eject
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
     C*
     C     *usa          move      *date         sysDate
     C*
     C                   eval      *in59   = *off
     C                   eval      balance = *zero
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
     C                   exsr      keyinz
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
    C*
     C     read          begsr
     C*
     C                   exsr      clrPayBill
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C  n95              exsr      accumulate
     C   95              exsr      loadTotals
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
    C*
     C     readp         begsr
     C*
     C                   exsr      clrPayBill
    C*
     C                   if        *in21 = *on
     C     key           readpe    dbrcdf                                 95
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
    C*
     C     setgt         begsr
    C*
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SETLL  - Set lower limits on file
    C*====================================================================
    C*
     C     setll         begsr
    C*
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
    C*
    C*  This should only be executed when the subfile is set to the top
    C*
     C                   eval      balance   = *zero
     C                   eval      totalBill = *zero
     C                   eval      totalPaid = *zero
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
     *** Beg Add *** 11/14/01 *****************************************************************
     C                   If        First <> 'N'
     C     Wmexl_Key     Chain     Wmexl
     C                   If        %Found and
     C                             Exedfl = 'Y'
     C                   Eval      Ssctype_1 = 'D'
     C                   Endif
     C                   Eval      First = 'N'
     C                   Endif
     *** End Add *** 11/14/01 *****************************************************************
     C                   eval      meetss = meetSubset('1')
     C*
     C                   if        billings = *zero and payments = *zero
     C                   eval      meetss  = *off
     C                   endif
     C*
     C                   if        *in59
     C                   eval      meetss = *on
     C                   endif
     C*
     C                   endsr
    C*





      /eject
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
     C                   eval      usa_date=%date()
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





      /eject
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
     C*
     C                   if        (%parms >= 1 and type = '2') and
     C                             (cvtToDate(sspodt_fr) >
     C                               cvtToDate(EAPODT) or
     C                              cvtToDate(thruDt) <
     C                               cvtToDate(dtMDYY(EAPODT)))
     C                   eval      meetss = *off
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
     *** Beg Add *** 11/14/01 *****************************************************************
     C                   Else
     C                   If        (Ssctype_1 <> *Blanks  And
     C                              Ssctype_1 =  TTCTYP)  Or
     C                             (Ssctype_2 <> *Blanks  And
     C                              Ssctype_2 =  TTCTYP)  Or
     C                             (Ssctype_3 <> *Blanks  And
     C                              Ssctype_3 =  TTCTYP)  Or
     C                             (Ssctype_4 <> *Blanks  And
     C                              Ssctype_4 =  TTCTYP)  Or
     C                             (Ssctype_5 <> *Blanks  And
     C                              Ssctype_5 =  TTCTYP)
     C                   Eval      Meetss = *Off
     C                   Endif
     *** End Add *** 11/14/01 *****************************************************************
     C                   endif
     C*
     C                   return    meetSS

     PmeetSubset       e





** srchCtgrys
SSCTYP_1
SSCTYP_2
SSCTYP_3
SSCTYP_4
SSCTYP_5
** srchCtgrysx
SSCTYPE_1
SSCTYPE_2
SSCTYPE_3
SSCTYPE_4
SSCTYPE_5
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
                                Billed              Paid              Balance
                                Billed              Paid           Difference
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
207F7=Policy info           YYY
209F9=Change info           YYY
212F12=Cancel               YYY
214F14=Darts                YYN
217F17=Subset               YYY
3
