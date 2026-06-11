     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
    H*********************************************************************
    H/TITLE - A/R Detail
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    List all A/R transactions for a selected employer/div/yr.      *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU                                                           *
    H*    W0051R                                                         *
    H*    W0090R                                                         *
    H*    W0110R                                                         *
    H*    W0162R                                                         *
    H*    W0612R5                                                        *
    H*    W1210R02                                                       *
    H*    W4700R                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    W0612R02                                                       *
    H*    W0612R2                                                        *
    H*    W0612R3                                                        *
    H*    W0612R4                                                        *
    H*    W0612R6                                                        *
    H*    W0614R                                                         *
    H*                                                                   *
    H*  REWRITE: 06/09/00      C:33      PROJECT: WC696                  *
    H*********************************************************************
     H* REVISIONS:                                                        *
    H*                                                                   *
    H* 11/19/09 C:33  Project: 25WQI                                     *
    H*                Allow CLT025, to toggle views to show Statement#   *
    H*                                                                   *
    H* 05/22/08 C:136 Project: 93WHI                                     *
    H*                Display verbiage if VIP Member                     *
    H*                                                                   *
     H* 03/08/06 C: 132 PROJECT: WC071 - 51160                            *
     H*                - Change *DATE to use The Current Date from DSDTTM *
     H*                  Data Structure DSDATE.                           *
    H*                                                                   *
     H* 09/23/05 C: 62 PROJECT: WC099 48335                               *
     H*                -Prevent date overflow error                       *
     H*                -Prevent date error.                               *
    H*                                                                   *
     H* 07/07/05 C: 58 PROJECT: WC1205                                    *
     H*              - Access activity status code from employer          *
     H*                detail WDELP rather than master WMEMP.             *
    H*                                                                   *
     H* 06/18/04 C:61  Support wa107 42942                                *
    H*                Recompile with default activation group *NEW.      *
    H*                                                                   *
    H*  03/12/04 C:136 Support: WA065 41722                              *
    H*                 Prevent decimal data error by initializing lda    *
    H*                 data structure. Reinitialize it after call to     *
    H*                 employer search.                                  *
    H*                                                                   *
    H* 02/19/04 C:136 Project: WC1033 (Return Premium)                   *
    H*                Add processing for EAARTY = 'T' (true void)        *
    H*                transaction.                                       *
    H*                                                                   *
     H* 06/11/02 C:61  Project wc577                                      *
    H*                Load default subset for group passed in parm.      *
     H*                                                                   *
     H* 01/21/02 C:136 Support WA046 Support 30346                        *
     H*          Pgm loses the default subset when reprompted from SCRCRT *
     H*          in the Accounting Menu.                                  *
     H*                                                                   *
     H* 12/07/01 C:61  PROJECT WC696                                      *
     H*                Enable F14=Darts in select mode.                   *
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
     H***** DFTACTGRP(*NO)
      /ENDIF
     Fw0612f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmeap     if   e             disk    rename(pea01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmeal6    if   e           k disk    rename(pea01:dbrcdf)
     F                                     infds(dbfds)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwtctp     if   e             disk    rename(pct01:pct0p)
     Fwtabl     if   e           k disk    include(ptt01)
     Fwtavl     if   e           k disk    rename(pav01:pav00)
     *** Beg Add *** 11/14/01 *****************************************************************
     Fwmexl     if   e           k disk
     *** End Add *** 11/14/01 *****************************************************************

     D srchCtgrys      s             10    dim(5) ctdata perrcd(1)
     *** Beg Add *** 11/14/01 *****************************************************************
     D srchCtgrysX     s             10    dim(5) ctdata perrcd(1)
     *** End Add *** 11/14/01 *****************************************************************
     D srchCodes       s             10    dim(10) ctdata perrcd(1)

     D***  Begin Chg  ***  11/19/09  **************************************
     D*hd*******       s             78    dim(2) ctdata perrcd(1)
     D hd              s             78    dim(3) ctdata perrcd(1)
     D***  End   Chg  ***  11/19/09  **************************************
     *** beg chg *** 10/24/01 **********************************************************************
     D************************ of              s              1    dim(16) ctdata perrcd(1)
     D************************ ofd             s             35    dim(16) alt(of)
     D of              s              1    dim(17) ctdata perrcd(1)
     D ofd             s             35    dim(17) alt(of)
     *** end chg *** 10/24/01 **********************************************************************

     D lda             ds          1024
      *** beg chg ***** 03/12/04 ***********************************************
     D***************  sspodt_fr                     8  0
     D***************  sspodt_to                     8  0
     D***************  sstrdt_fr                     8  0
     D***************  sstrdt_to                     8  0
     D  sspodt_fr                     8  0 inz(0)
     D  sspodt_to                     8  0 inz(0)
     D  sstrdt_fr                     8  0 inz(0)
     D  sstrdt_to                     8  0 inz(0)
      *** end chg ***** 03/12/04 ***********************************************
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
      *
    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'A/R Detail'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0612R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D***  Begin Chg  ***  11/19/09  **************************************
     D*infomx          c                   2
     D infomx          s              1  0 inz(2)
     D***  End   Chg  ***  11/19/09  **************************************
     D viewmx          c                   1
     D sf1pag          c                   9
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@co#           s              3
     D @@fnd           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fyr           s              3
     D @@date          s              8
     D @@amount        s              9
     D @@chk#          s              7
     D @@rtcd          s              1
     D @@rrn           s              9  0
     D***  Begin Add  ***  11/19/09  **************************************
     D @@NInv          s             10  0
     D***  End   Add  ***  11/19/09  **************************************
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##call          s              1
     D ##rrn           s              9  0
     D kkco#           s                   like(EACO#)
     D kkfnd           s                   like(EAFND)
     D kkemp#          s                   like(EAEMP#)
     D kkdiv           s                   like(EADIV)
     D kkfyr           s                   like(EAFYR)
     D kkpodt          s                   like(EAPODT)
     D kkdt            s                   like(EADT)
     D kktime          s                   like(EATIME)
     D xxrtcd          s                   like(@@rtcd)
     D xxkey           s                   like(TTKEY)
     D need_emp#       s              1
     D firstRead       s              1
     *** Beg Add *** 11/14/01 *****************************************************************
     D first           s              1
     *** End Add *** 11/14/01 *****************************************************************
     D readBack        s              1
     D balance         s             11  2
     D priorAmt        s                   like(scamt)
     D EACHK#_a        s              7
     D numOfZeros      s             15  0
     D error           s              1
     D x8date          s              8  0
     D balanceDt       s              8  0
     D srchCtgry       s                   like(*in60)
     *** Beg Add *** 11/14/01 *****************************************************************
     D srchCtgryx      s                   like(*in60)
     *** End Add *** 11/14/01 *****************************************************************
     D srchCode        s                   like(*in60)
     D x               s              3  0
     D flag            s              1
     D validOpt        s              1
     D rrn             s              9  0
     D db_rrn          s              9  0
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D CT_rrn          s              9  0
     D ARCode          s             10
     D dateField       s               d   inz datfmt(*usa)
     d*** begin add *** 09/23/05 *******************************************
     D dateTest        s               d   inz(d'0001-01-01') datfmt(*usa)
     d*** end   add *** 09/23/05 *******************************************
     D null_date       s               d   inz
     *** beg add *** 10/24/01 *****************************************************************
     D qcpgm           s             10
     *** end add *** 10/24/01 *****************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     DW0614R           PR                  extpgm('W0614R')
     D ##call                         1                 const
     D ##kc                           1
     D ##kl                           1
     D #@fnd                               like(EMFND)  const
     D ##emp#                              like(EMEMP#) const
     D ##div                               like(EMDIV)  const
     D ##fyr                               like(ELFYR)  const

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

     DW0612R6          PR                  extpgm('W0612R6')
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

     Dget@@Call        PR             1
     D @@call2                             like(@@call) const

      ***** BEG ADD ******* 12/07/01 **********************************

     DsrChkPgm         PR                  extpgm('SRCHKPGM')
     D program                       10    const
     D flag                           1
      ***** END ADD ******* 12/07/01 **********************************

     Dacct_curr        PR             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D yr                                  like(ELFYR)  const

     DAR_value         PR             9  2
     D AR_amount                      9  2  const
     D AR_type                        1     const

     DcurrentBal       PR            11  2
     D fromDate                       7  0 const
     D toDate                         7  0 const

     DmeetSubset       PR             1
     D type                           1    const options(*nopass)

     DsflBalance       PR            11  2
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
     C                   parm                    @@date
     C                   parm                    @@amount
     C                   parm                    @@chk#
     C                   parm                    @@rtcd
     C                   parm                    @@rrn
     C***  Begin Add  ***  11/19/09  **************************************
     C                   parm                    @@NInv
     C***  End   Add  ***  11/19/09  **************************************
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
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
    C*
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkpodt
     C                   kfld                    kkdt
     C                   kfld                    kktime
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
    C* CALC_BAL - Calculate header balance
    C*====================================================================
    C*
     C     calc_bal      begsr
    C*
     C                   eval      *in54 = *off
    C*
     C                   if        sspodt_to <> *zero
     c*** begin del *** 09/23/05 *******************************************
     C**********         eval      *in54     = *on
     c*** end   del *** 09/23/05 *******************************************
     C                   eval      balanceDt = sspodt_to
     C                   else
      ***** BEG CHG ***** 03/08/06 *******************************************************
     C**************     eval      balanceDt = *date
     C                   TIME                    DSDTTM
     C                   move      DSDATE        balanceDT                      mmddyyyy
      ***** END CHG ***** 03/08/06 *******************************************************
     C                   endif
     c*** begin add *** 09/23/05 *******************************************
     C                   if        sspodt_fr <> *zero
     C                   eval      *in54     = *on
     C                   endif
     c*** end   add *** 09/23/05 *******************************************
    C*
     C                   eval      sccurr = currentBal(0:dtCYMD(balanceDt))
     C*
     C                   eval      scbal_to = '       Balance as of ' +
     C                                        %trim(alphaMDY(cvtToDate(
     C                                                       balanceDt)))
     C                                        + ':'
     C*
     C                   if        *in54
     C*
     C                   eval      dateField = cvtToDate(sspodt_fr)
     c*** begin add *** 09/23/05 *******************************************
     C                   if        dateField > dateTest
     c*** end   add *** 09/23/05 *******************************************
     C                   subdur    1:*d          dateField
     c*** begin add *** 09/23/05 *******************************************
     C                   endif
     c*** end   add *** 09/23/05 *******************************************
     C                   move      dateField     balanceDt
    C*
     C                   eval      scprev = currentBal(0:dtCYMD(balanceDt))
     C*
     c*** begin add *** 09/23/05 *******************************************
     C                   if        sspodt_fr = 0
     C                   eval      scbal_fr = '    Balance prior to ' +
     C                                        '01/01/0001 :'
     C                   else
     c*** end   add *** 09/23/05 *******************************************
     C                   eval      scbal_fr = '    Balance prior to ' +
     C                                        %trim(alphaMDY(cvtToDate(
     C                                                       sspodt_fr)))
     C                                        + ':'
     c*** begin add *** 09/23/05 *******************************************
     c                   endif
     c*** end   add *** 09/23/05 *******************************************
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
     C                                      sstrdt_to <> *zeros  or
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
     C                                      Ssctype_5 <> *blanks or
     *** End Add *** 11/14/01 *****************************************************************
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
      /eject
    C*====================================================================
    C* ClearLDA - Clear the Local Data Area
    C*====================================================================
    C*
     C     clearLDA      begsr
    C*
     C                   clear                   lda
      *** beg add **** 01/21/02 ********************************************************************
     C                   If        First <> 'N'
     C     Wmexl_Key     Chain     Wmexl
     C                   If        %Found and
     C                             Exedfl = 'Y'
     C                   Eval      Ssctype_1 = 'D'
     C                   Endif
     C                   Eval      First = 'N'
     C                   Endif
      *** end add **** 01/21/02 ********************************************************************
     C                   out       lda
    C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* CRITERIA - Get the selection criteria from scrcrt
    C*====================================================================
    C*
     C     criteria      begsr
    C*
     C                   eval      scprds_crt = optnm
     C                   eval      scprds_crt = centerFld(scprds_crt)
     C*
      /If Defined(WEB)
      /free
       clear fl;
       fl(1) = 'F3=Exit   F10=Search   F13=Chg user parms';
       AV_KEYS_DS.ARRAY = fl;
       AV_KEYS = AV_KEYS_DS.KEYS;
      /End-free
      /Endif
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
     *** beg add *** 03/12/04 *****************************************************************
     c                   exsr      clearLDA
     *** beg add *** 03/12/04 *****************************************************************
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
      /If Defined(WEB)
     C                   exsr      bldlns
      /Endif
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
     C                   if        sstrdt_fr <> *zero
     C                   eval      sstrdt_fr  = dtCheck(sstrdt_fr)
     C  n90              if        sstrdt_fr  = *zero
     C                   seton                                        74  90
     C                   endif
     C                   endif
    C*
     C                   if        sstrdt_to <> *zero
     C                   eval      sstrdt_to = dtCheck(sstrdt_to)
     C  n90              if        sstrdt_to = *zero
     C                   seton                                        75  90
     C                   endif
     C                   endif
     C*
     C  n90              if        cvtToDate(sstrdt_fr) > cvtToDate(sstrdt_to)
     C                   seton                                        76  90
     C                   endif
     C*
     C  n90              out       lda
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
     C*
      *** beg add **** 01/21/02 ********************************************************************
     C     Wmexl_Key     Chain     Wmexl
     C                   If        %Found and
     C                             Exedfl = 'Y'
     C                   Eval      Ssctype_1 = 'D'
     C                   Endif
     C                   Eval      First = 'Y'
      *** end add **** 01/21/02 ********************************************************************
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
      *** beg del **** 01/21/02 ********************************************************************
     C****************   exsr      clearLDA
      *** end del **** 01/21/02 ********************************************************************
     C*
     C                   if        need_emp# = 'Y'
     C                   exsr      criteria
     C                   endif
     C*
     C                   clear                   sspodt_fr
     C                   clear                   sspodt_to
     C                   clear                   sstrdt_fr
     C                   clear                   sstrdt_to
     C                   clear                   ssctyp_1
     C                   clear                   ssctyp_2
     C                   clear                   ssctyp_3
     C                   clear                   ssctyp_4
     C                   clear                   ssctyp_5
     *** Beg Add *** 11/14/01 *****************************************************************
      *** beg del **** 01/21/02 ***************************************************************
     C*****************  Clear                   Ssctype_1
      *** end del **** 01/21/02 ***************************************************************
     C                   Clear                   Ssctype_2
     C                   Clear                   Ssctype_3
     C                   Clear                   Ssctype_4
     C                   Clear                   Ssctype_5
     *** End Add *** 11/14/01 *****************************************************************
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
     C                   exsr      loadHeader
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
     C                   select
     C                   when      *inkc
     C                   eval      xxrtcd = 'A'
     C                   when      *inkl
     C                   eval      xxrtcd = 'B'
     C                   endsl
     C*
     C                   if        %parms >= 12
     C                   eval      @@rtcd = xxrtcd
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
     C                   eval      kkco#  = ##co#
     C                   eval      kkfnd  = ##fnd
     C                   eval      kkemp# = scemp#
     C                   eval      kkdiv  = scdiv
     C                   eval      kkfyr  = scyr
     C*
     C                   clear                   kkpodt
     C                   clear                   kkdt
     C                   clear                   kktime
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
     C                   clear                   kkpodt
     C                   clear                   kkdt
     C                   clear                   kktime
     C*
     C                   select
     C                   when      *in21
     C                   eval      kkpodt = dtCYMD(numDate(scpst1))
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
     C                   eval      kkpodt = scpodt
     C                   eval      kkdt   = scdt
     C                   eval      kktime = sctime
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
    C/EJECT
    C*====================================================================
    C* KHDO - Processing for F8
    C*====================================================================
    C*
     C     khdo          begsr
     C*
     C                   callp     W0612R6(*inkc:*inkl:'1':##co#:##fnd:
     C                                     scemp#:scdiv:scyr:EL_rrn)
     C*
     C                   endsr
    C/eject
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
      *** beg add ***** 03/12/04 ***********************************************
     C                   eval      scemp# = ELEMP#
     C                   eval      scdiv  = ELDIV
      *** end add ***** 03/12/04 ***********************************************
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
    C*====================================================================
    C* KPDO - Processing for F15
    C*====================================================================
    C*
     C     kpdo          begsr
     C*
     C                   callp     W0612R3(*inkc:*inkl:'1':##co#:##fnd:
     C                                     scemp#:scdiv:scyr:EL_rrn)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KQDO - Processing for F16
    C*====================================================================
    C*
     C     kqdo          begsr
     C*
     C                   callp     W0612R4(*inkc:*inkl:'1':##co#:##fnd:
     C                                     scemp#:scdiv:scyr:EL_rrn)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KTDO - Processing for F19
    C*====================================================================
    C*
     C     ktdo          begsr
     C*
     C                   callp     W0614R('1':*inkc:*inkl:##fnd:scemp#:
     C                                    scdiv:scyr)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* LOAD_bal - Load balance when adding a new subfile screen
    C*====================================================================
    C*
     C     load_bal      begsr
    C*
     C                   if        firstRead = *on
     C                   eval      balance   = sflBalance(db_rrn)
     C                   eval      readBack  = *off
     C                   endif
    C*
     C                   endsr
    C*
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
    C/eject
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
    C/eject
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
     *** Beg Add ***  7/07/05 *****************************************************************
     C                   eval      EMASTS = ELASCD
     *** End Add ***  7/07/05 *****************************************************************
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
     C                   eval      xxopfn = '2'
     C                   eval      xxnmbr = '19'
     C                   if        not *in50
     C                   eval      xxmis  = 'NNN'
     C                   else
     C                   eval      xxmis  = 'YYY'
     C                   endif
     C                   exsr      updofa
     C*
     C                   exsr      calc_bal
     C                   exsr      bldlns
     *
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
     *
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   eval      firstRead = *off
     C*
     C                   eval      scamt2 = EAAMT
     C                   eval      scchk# = EACHK#
     C                   eval      scpodt = EAPODT
     C                   eval      scdt   = EADT
     C                   eval      sctime = EATIME
     C***  Begin Add  ***  11/19/09  **************************************
     C                   eval      scNInv = EANINV
     C***  End   Add  ***  11/19/09  **************************************
     C*
     C                   eval      scamt   = AR_value(EAAMT:EAARTY)
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
     C                   move      EACHK#        EACHK#_a
     C     '0'           check     EACHK#_a      numOfZeros
     C                   if        numOfZeros = 0
     C                   eval      numOfZeros = 8
     C                   endif
     C                   eval      %subst(EACHK#_a:1:numOfZeros - 1) = '       '
     C*
     C                   select
     C                   when      view# = 1 and info# = 1
     C                   eval      scdata = alphaMDY(cvtToDate(
     C                                        dtMDYY(EAPODT))) + ' ' +
     C                                      EAARCD + '  ' + EAARTY + ' ' +
     C                                      %subst(EADESC:1:22) + ' ' +
     C                                      EACHK#_a
     C                   when      view# = 1 and info# = 2
     C                   eval      scdata = alphaMDY(cvtToDate(
     C                                        dtMDYY(EAPODT))) + ' ' +
     C                                      alphaMDY(cvtToDate(
     C                                        dtMDYY(EATRDT))) + ' ' +
     C                                      EAARCD + '  ' + EAARTY + ' ' +
     C                                      %subst(EADESC:1:22)
     C***  Begin Add  ***  11/19/09  **************************************
     C                   when      view# = 1 and info# = 3
     C                   eval      scdata = alphaMDY(cvtToDate(
     C                                        dtMDYY(EAPODT))) + ' ' +
     C                                      EAARCD + '  ' + EAARTY + ' ' +
     C                                      %subst(EADESC:1:20) +
     C                                      %editc(EANINV:'Z')
     C***  End   Add  ***  11/19/09  **************************************
     C                   endsl
     C*
     C                   endsr
    C/eject
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
     C     *inkh         caseq     *on           khdo
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
      *** beg add *** 10/24/01 ******************************************************************
     C     *inkn         caseq     *on           kndo
      *** end add *** 10/24/01 ******************************************************************
     C     *inkp         caseq     *on           kpdo
     C     *inkq         caseq     *on           kqdo
     C     *inkt         caseq     *on           ktdo
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
    C/eject
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
      *
     C                   exsr      kc_kl_do
     C*
     C                   exsr      clearLDA
     C*
     C                   endsr
    C*
      /eject
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
     C*** beg add **** 01/21/02 ********************************************************************
     C                   eval      First = 'Y'
     C*** end add **** 01/21/02 ********************************************************************
      ******* beg del ****** 06/11/02 *********************************
     C***********        exsr      clearLDA
      ******* end del ****** 06/11/02 *********************************
      ***** BEG ADD ******* 12/07/01 **********************************
      *
      * Enable F14=Darts if called from Post Cash and Adjustments
      *
     C                   callp     srChkPgm('W0110R':flag)
     C                   if        flag = *on
     C                   eval      xxopfn = '2'
     C                   eval      xxnmbr = '14'
     C                   eval      xxmis  = 'YYY'
     C                   exsr      updofa
     C                   endif
      *
      ***** END ADD ******* 12/07/01 **********************************
     C*
     C                   if        %parms >= 3
     C                   eval      @@call = get@@Call(@@call)
     C                   endif
     C*
     C                   clear                   xxrtcd
     C*
     C                   if        %parms >= 12
     C                   eval      xxrtcd = @@rtcd
     C                   endif
     C*
     C                   if        xxrtcd = 'R'
     C                   eval      xxopfn = '2'
     C                   eval      xxnmbr = '03'
     C                   eval      xxmis  = 'NNN'
     C                   exsr      updofa
     C                   endif
     C*
     C                   if        %parms >= 4 and @@co# <> *blank
     C                   move      @@co#         ##co#
     C                   endif
     C*
     C                   if        %parms >= 5 and @@fnd <> *blank
     C                   move      @@fnd         ##fnd
      ******* beg add ****** 06/11/02 *********************************
     C                   move      @@fnd         kkfnd
      ******* end add ****** 06/11/02 *********************************
     C                   endif
     C*
     C                   eval      need_emp# = 'Y'
     C*
     C                   if        %parms >= 6 and @@emp# <> *blank
     C                   move      @@emp#        scemp#
     C                   move      @@div         scdiv
     C                   move      @@fyr         scyr
     C                   eval      need_emp# = 'N'
     C                   endif
     C*
     C                   if        %parms >= 13 and @@rrn <> 0
     C                   eval      EL_rrn = @@rrn
     C                   exsr      load_EL
     C                   eval      need_emp# = 'N'
     C                   endif
      *
      ******* beg add ****** 06/11/02 *********************************
     C                   exsr      clearLDA
      ******* end add ****** 06/11/02 *********************************
     C*
     C                   exsr      grp_info
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C                   call      'RTVCLTID'
     C                   parm                    clientID
     C*
     C                   select
     C                   when      clientID = '025'
     C                   eval      infomx = 3
     C                   eval      info# = 3
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
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
    C/eject
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
     C*
     C                   eval      @@rtcd = 'B'
     C*
     C                   if        %parms >= 8
     C                   eval      x8date = dtMDYY(scpodt)
     C                   move      x8date        @@date
     C                   move      scamt2        @@amount
     C                   move      scchk#        @@chk#
     C                   endif
     C*
     C                   if        %parms >= 13
     C                   eval      @@rrn = scdbrr
     C                   endif
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C                   if        %parms >= 14
     C                   eval      @@NInv = scNInv
     C                   endif
     C***  End   Add  ***  11/19/09  **************************************
     C*
     C                   endsr
    C/eject
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
    C*
     C     subset        begsr
     *** Beg Add *** 11/14/01 *****************************************************************
     *** beg del **** 01/21/02 ****************************************************************
     C*******            If        First <> 'N'
     C*****Wmexl_Key     Chain     Wmexl
     C*******            If        %Found and
     C*******                      Exedfl = 'Y'
     C*******            Eval      Ssctype_1 = 'D'
     C*******            Endif
     C*******            Eval      First = 'N'
     C*******            Endif
     *** end del **** 01/21/02 ****************************************************************
     *** End Add *** 11/14/01 *****************************************************************
     C*
     C                   eval      meetss = meetSubset
     C*
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




      /eject
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





      /eject
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

     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
    C*
     C     eaky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    fromDate

     C                   clear                   balance
     C*
     C     eaky1         setll     dbrcdf
     C     eaky          reade     dbrcdf                                 60
     C*
     C                   dow       not *in60 and EAPODT <= toDate
     C*
     C                   if        meetSubset('1') = *on
     C                   eval      balance = balance + AR_value(EAAMT:EAARTY)
     C                   endif
     C*
     C     eaky          reade     dbrcdf                                 60
     C*
     C                   enddo

     C                   return    balance

     PcurrentBal       e





      /eject
    P*====================================================================
    P* get@@Call - Determine the value of @@call based on whether this
    P*             program was called from a program prior to project
    C*             WC696 (REWRITE)
    P*====================================================================
    P*
     Pget@@Call        b
    P*
     Dget@@Call        PI             1
     D @@call2                        1    const

     D call            s              1

      ***** BEG DEL ******* 12/07/01 **********************************
     D******srChkPgm         PR                  extpgm('SRCHKPGM')
     D****** program                       10    const
     D****** flag                           1
      ***** END DEL ******* 12/07/01 **********************************

     C                   eval      call = @@call2
    C*
     C                   callp     srChkPgm('W0051R':flag)
     C                   if        flag = *on and call = '2'
     C                   eval      call = '1'
     C                   endif
    C*
     C                   callp     srChkPgm('W0090R':flag)
     C                   if        flag = *on and call = '2'
     C                   eval      call = '1'
     C                   endif
    C*
     C                   callp     srChkPgm('W0091R02':flag)
     C                   if        flag = *on and call = '2'
     C                   eval      call = '1'
     C                   endif
    C*
     C                   callp     srChkPgm('W0162R':flag)
     C                   if        flag = *on and call = '2'
     C                   eval      call = '1'
     C                   endif
    C*
     C                   callp     srChkPgm('W4700R':flag)
     C                   if        flag = *on and call = '2'
     C                   eval      call = '1'
     C                   endif


     C                   return    call

     Pget@@Call        e





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
     D testPODT        s              1

     C                   eval      meetSS   = *on
     C                   eval      testPODT = *on
     C*
     C                   if        %parms >= 1 and type = '1'
     C                   eval      testPODT = *off
     C                   endif
     C*
     C                   if        testPODT = *on
     C*
     C                   if        sspodt_to <> *zero and
     C                             (cvtToDate(sspodt_fr) >
     C                                cvtToDate(EAPODT) or
     C                              cvtToDate(sspodt_to) <
     C                                cvtToDate(dtMDYY(EAPODT)))
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endif
     C*
     C                   if        sstrdt_to <> *zero and
     C                             (cvtToDate(sstrdt_fr) >
     C                                cvtToDate(dtMDYY(EATRDT)) or
     C                              cvtToDate(sstrdt_to) <
     C                                cvtToDate(dtMDYY(EATRDT)))
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   clear                   TTCTYP
     C                   eval      xxkey = EAARCD
     C     table_key     chain     ptt01                              60
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
     C                             (ssarcd_1  <> EAARCD   and
     C                              ssarcd_2  <> EAARCD   and
     C                              ssarcd_3  <> EAARCD   and
     C                              ssarcd_4  <> EAARCD   and
     C                              ssarcd_5  <> EAARCD   and
     C                              ssarcd_6  <> EAARCD   and
     C                              ssarcd_7  <> EAARCD   and
     C                              ssarcd_8  <> EAARCD   and
     C                              ssarcd_9  <> EAARCD   and
     C                              ssarcd_10 <> EAARCD)
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   return    meetSS

     PmeetSubset       e





      /eject
    P*====================================================================
    P* sflBalance - Calculate the balance prior to the first 'displayed'
    P*====================================================================
    P*
     PsflBalance       b
    P*
     DsflBalance       PI            11  2
     D saveRRN                        9  0  const

     D balance         s             11  2
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
     C                   if        meetSubset('1') = *on
     C                   eval      balance = balance + AR_value(EAAMT:EAARTY)
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
Opt  Post Dt Cd Tp Description             Check#       Amount         Balance
Opt  Post Dt Trans Dt Cd Tp Description                 Amount         Balance
Opt  Post Dt Cd Tp Description         Statement#       Amount         Balance
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Policy info           YYY
208F8=Summary               YYY
209F9=Change info           YYY
212F12=Cancel               YYY
214F14=Darts                YYN
215F15=Category detail      YYY
216F16=Category summary     YYY
217F17=Subset               YYY
219F19=Account current      YYY
3
