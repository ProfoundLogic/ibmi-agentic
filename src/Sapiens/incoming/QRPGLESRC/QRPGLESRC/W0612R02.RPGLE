     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H debug(*yes) bnddir('WTBNDDIR')
    H*********************************************************************
    H/TITLE - A/R Detail - Display
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    Display detail about particular A/R transactions.              *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W0612R                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  CREATED: 06/09/00      C:33      PROJECT: WC696                  *
    H*********************************************************************
    H*  REVISIONS:                                                       *
     h* 04/11/22 C:340 Project: WC2663
     h*                Invoice Cloud - Add Invoice Type ID
     h*
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
     Fw0612f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmeap     uf a e             disk    rename(pea01:dbrcdf)

     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwtctl     if   e           k disk    rename(pct01:pct00)
     Fwtabl     if   e           k disk    include(ptt01)
     Fwtavl     if   e           k disk    rename(pav01:pav00)
      *** beg add ***  7/07/05 *********************************************************************
     Fwdelp     if   e             disk    rename(pel01:pel0p)
      *** end add ***  7/07/05 *********************************************************************
    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'A/R Detail'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     *** beg add *** 10/24/01 **********************************************************************
     D  qckey          ds            40                                         DARTS parm
     D    qcco#                1      3
     D    qcfnd                4      6
     D    qcemp#               7     15
     D    qcdiv               16     20
     *** end add *** 10/24/01 **********************************************************************
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
     D xxkey           s                   like(TTKEY)
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     *** beg add *** 10/24/01 *****************************************************************
     D qcpgm           s             10                                         DARTS parm
     *** end add *** 10/24/01 *****************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     Dacct_curr        PR             1
     D co#                                 like(EACO#)  const
     D group                               like(EAFND)  const
     D emp#                                like(EAEMP#) const
     D div                                 like(EADIV)  const
     D yr                                  like(EAFYR)  const

     D/copy SPRDATE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
      //*** Beg Add *** 04/11/22 *************************************
     d/Copy sPrModule
      //*** End Add *** 04/11/22 *************************************

     D/copy WPRGROUP
     D/copy WPREMPLR

     C/copy SCOPYMT41C
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
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
     C     table_key     klist
     C                   kfld                    ##co#
     C                   kfld                    xxkey
    C*
     C     ctky          klist
     C                   kfld                    EACO#
     C                   kfld                    TTCTYP
    C*
    C*====================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*====================================================================
    C*
     C     dltpre        begsr
     C                   endsr
    C*
    C*====================================================================
    C* DSPLYR - Processing after displaying information
    C*====================================================================
    C*
     C     dsplyr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     errck1        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
    C*
     C     errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
    C*
     C     errck3        begsr
     C                   endsr
    C*
    C*====================================================================
    C* GRP_INFO - Move group information data base fields to screen fields
    C*====================================================================
    C*
     C     grp_info      begsr
     C*
     C                   eval      scfnd    = EAFND
     C                   eval      scfnam   = GetGrpName(EAFND:FN_rrn:EACO#)
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
     C                   move      EAFND         qcfnd
     C                   move      EAEMP#        qcemp#
     C                   move      EADIV         qcdiv
      *
      *  Call Darts entry program
      *
     C                   call      'SC010R'
     C                   parm                    qckey
     C                   parm      'W0050R'      qcpgm
      *
     C                   endsr                                                  kndo
     *** beg add *** 10/24/01 **********************************************************************
    C*====================================================================
    C* loadHeader - Load information to the header fields
    C*====================================================================
    C*
     C     loadHeader    begsr
     C*
     C                   exsr      grp_info
     C*
     C                   eval      EL_rrn = GetEmpYrRR(EAFYR:EAEMP#:EADIV:
     C                                                 EAFND:EACO#)
     C*
     C                   eval      scedn = EmpDivName(EAEMP#:EADIV:EM_rrn:
     C                                                EAFND:EACO#)
     C     EM_rrn        chain     wmemp                              60
     C*
     C                   eval      scprv_emp# = GetPrvEmp#(EACO#:EAFND:EAEMP#:
     C                                                     EADIV)
     C                   eval      *in51 = scprv_emp# <> *blanks
     C*
     C                   eval      sccxdt = dtMDYY(GetCxDate(EACO#:EAFND:
     C                                                       EAEMP#:EADIV))
     C                   eval      *in52 = sccxdt <> *zero
     C*
     C                   eval      scprem_co = GetPremFin(EL_rrn)
     C                   eval      *in53 = scprem_co <> *blanks
      *** beg add *** 10/25/01 *********************************************************************
     C                   eval      *in54 = scact_sts <> *blanks
      *** end add *** 10/25/01 *********************************************************************
     C*
     C                   eval      scdiv_cnt = DivCount(EACO#:EAFND:EAEMP#)
     C*
      *** beg add ***  7/07/05 *********************************************************************
     C     EL_rrn        chain     wdelp                              60
     C                   eval      EMASTS = ELASCD
      *** end add ***  7/07/05 *********************************************************************
     C*
     C                   clear                   AVDESC
     C                   eval      xxkey = EMASTS
     C     table_key     chain     pav00                              60
     C                   eval      scact_sts = EMASTS + ' ' + AVDESC
      *** beg add *** 10/25/01 *********************************************************************
     C                   eval      *in54 = scact_sts <> *blanks
      *** end add *** 10/25/01 *********************************************************************
     C*
     C                   eval      *in50 = acct_curr(EACO#:EAFND:EAEMP#:
     C                                               EADIV:EAFYR)
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   exsr      loadHeader
     C*
     C                   clear                   TTCTYP
     C                   eval      xxkey = EAARCD
     C     table_key     chain     ptt01                              60
     C                   clear                   CTDESC
     C     ctky          chain     pct00                              60
     C*
     C                   eval      scpodt     = dtMDYY(EAPODT)
     C                   eval      sctrdt     = dtMDYY(EATRDT)
     C                   eval      scarcd     = EAARCD
     C                   eval      scarty     = EAARTY
     C                   eval      scar_desc  = EADESC
     C                   eval      scctyp     = TTCTYP
     C                   eval      sccat_desc = CTDESC
     C                   eval      scamt      = EAAMT
     C                   eval      sclate     = EALATE
     C                   eval      sc_EAUSER  = EAUSER
     C                   eval      sc_EATIME  = EATIME
     C                   eval      sc_EAPGM   = EAPGM
     C*
       //*** Beg Add *** 04/11/22 *************************************
           SCInvTypID = EAInvTypId;
       //*** End Add *** 04/11/22 *************************************
     C                   endsr
    C*
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
    C*
     C     mvscdb        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
    C*
     C     otherr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     othfks        begsr
     C     *inkn         caseq     *on           kndo
     C                   endcs
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     pgmend        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
    C*
      //*** Beg Add *** 04/11/22 *************************************
           isInvType = IsModEnabled('S/R Online') or
                       IsModEnabled('Third Party Payment') ;
      //*** End Add *** 04/11/22 *************************************
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PREOTH - Pre-processing for other modes
    C*====================================================================
    C*
     C     preoth        begsr
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
    C*
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
    C*
     C                   exsr      mvscdb
    C*
     C                   update    dbrcdf
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
    C*
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
    C*
     C                   exsr      mvscdb
    C*
     C                   write     dbrcdf
    C*
     C                   feod      wmeap
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
     D co#                                 like(EACO#)  const
     D group                               like(EAFND)  const
     D emp#                                like(EAEMP#) const
     D div                                 like(EADIV)  const
     D yr                                  like(EAFYR)  const

     D ##co#           s                   like(EACO#)
     D ##group         s                   like(EAFND)
     D ##emp#          s                   like(EAEMP#)
     D ##div           s                   like(EADIV)
     D ##yr            s                   like(EAFYR)
     D ##agt#          s              5  0
     D ##agt_name      s             60
     D ##prod          s              5  0
     D ##prod_nm       s             60
     D ##area_cd       s              3  0
     D ##phone#        s              7  0
     D ##plan          s              1
     D acct_curr2      s                   like(*in99)

     C                   call      'WA002R'                             99
     C                   parm      co#           ##co#
     C                   parm      group         ##group
     C                   parm      emp#          ##emp#
     C                   parm      div           ##div
     C                   parm      yr            ##yr
     C                   parm                    ##agt#
     C                   parm                    ##agt_name
     C                   parm                    ##prod
     C                   parm                    ##prod_nm
     C                   parm                    ##area_cd
     C                   parm                    ##phone#
     C                   parm                    ##plan
     C*
     C                   eval      acct_curr2 = ##plan = 'A'
     C*
     C                   return    acct_curr2

     Pacct_curr        e
