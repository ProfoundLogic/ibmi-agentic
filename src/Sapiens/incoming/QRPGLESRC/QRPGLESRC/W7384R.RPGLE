     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
    H*
    H/TITLE W7384R - A/R DETAIL REPORT - BUILD WORKFILE
    H*********************************************************************
    H*  DESCRIPTION:  This program reads teh A/R file and determines     *
    H*                A/R date (posting or transaction) is to be used    *
    H*                on the type of account.  Only transactions that    *
     H*                meet the selection criteria entered on the propmpt *
     H*                screen will be processed.  This program writes to  *
    H*                the workfile (WXEAP) which is used by W7383R to    *
    H*                calculate previous and current balances based on   *
    H*                selection criteria and sort the data by the sort   *
    H*                selected.                                          *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W7381C                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    NONE                                                           *
    H*                                                                   *
    H*  REWRITE:  08/01/00   C:33   PROJECT: WC804                       *
    H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
     H* 11/06/23 C:291 Project: WC2934                                    *
     H*                Add Batch Type parameter to entry parms and        *
     H*                include Batch Type in selection criteria to select *
     H*                records.                                           *
     H*                                                                   *
     H* 04/07/21 C:180 Project: WC1986                                    *
     H*                Add GetEftStatus to replace old methods            *
     H*
     H* 11/09/15 C:062 Support: 38255(132)                                *
     H*                If group range passed, use it instead of group     *
     H*                select.                                            *
     H*                                                                   *
     H* 05/28/14 C:279 Project: 129WSV                                    *
     H*                Modified to use group selection.                   *
     H*                                                                   *
     H* 10/22/10 C:255 Support: SEQ73572                                  *
     H*                Checked object WA073R in library list instead      *
     H*                Call Stack.                                        *
     H*                                                                   *
     H* 09/09/10  C:257  Support: 73822                                   *
     H*                  Fix for group restriction: Not to print all      *
     H*                  employee report when user is restricted to       *
     H*                  one group.                                       *
     H*                                                                   *
     H* 07/27/10 C:257 Support: SEQ73572                                  *
     H*                If API WA073R exist in call stack then use         *
     H*                file WMEFL6 instead of WMDBL3                      *
     H*                                                                   *
     H* 01/04/10 C:190 Project: 25WRE                                     *
     H*                Load District#.                                    *
     H*                                                                   *
     H* 06/26/08 C:136 Support: WA093 64076                               *
     H*                Exclude billing type codes if report is run to     *
     H*                include commissionable codes only per GW           *
     H*                                                                   *
     H* 06/26/07  C:189 Support: 58273                                    *
     H*                 If the restrictions have been set to allow for    *
     H*                 multiple groups, the flag used also must be set.  *
     H*                                                                   *
    H* 12/07/04 C:136 Project: 129WHU                                    *
    H*                Incorporate FCC mods to base.                      *
    H*                Add for selection of ACH Direct Debit, Vendor, or  *
    H*                or Direct Bill transactions.                       *
    H*                                                                   *
    H* 09/07/04  c:53 Project wc1050                                     *
    H*                Add aging rpt processing.                          *
    H*                                                                   *
    H* 07/16/02 C:136 SUPPORT WA048 33364                                *
    H*                The workfile build ends prematurely if one of the  *
    H*                groups being processed doesn't have a policy year  *
    H*                greater than the from policy year passed in.       *
    H*                                                                   *
    H* 04/15/02  C:35   PROJECT: WC970                                   *
    H*                  Add Category Type to the workfile.               *
    H*                                                                   *
    H* 04/04/02  C:35   PROJECT: SUPPORT WA099 #29710                    *
    H*                  Change selection routine to be more efficient.   *
    H*                                                                   *
     H* 04/18/01 C:61  SUPPORT WA112 27044                                *
     H*                Select by FND and FYR range before write to        *
    H*                work file.                                         *
    H*                                                                   *
    H*********************************************************************
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
     F*** Begin Add *** 11/09/15 *******************************************************************
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     F*** End   Add *** 11/09/15 *******************************************************************
     Fwmeal8    if   e           k disk    rename(pea01:pea08)
     Fwtbsp     if   e           k disk    rename(pbs01:pbs0p)
     Fwxeap     o    e             disk    rename(pea01:pea0px)
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     Fwmupl     if   e           k disk
      **** END   ADD **** 04/04/02 *****************************************************************
      **** BEG ADD *** 09/07/04 *****************************************   ************************
     Fwmeapa    o    e             disk    rename(pea01:pea0pa)
     Fwxebl1    uf a e           k disk
     Fwmemp     if   e             disk
     Fwdell     if   e           k disk
     Fwmrsl     if   e           k disk
      **** END ADD *** 09/07/04 *****************************************   ************************
      *** beg add ***** 12/07/04 *******************************************************************
     fwmdbl3    if   e           k disk
     fwdvhl1    if   e           k disk
     F***** BEG ADD ******* 07/27/10 **********************************
     Fwmefl6    if   e           k disk
     F***** END ADD ******* 07/27/10 **********************************
      ****** beg add ******* 05/28/14 *******************************************************
     fwxgsl     if   e           k disk    rename(pgs01:pgs00)
      ****** end add ******* 05/28/14 *******************************************************
      *** Beg add ***** 11/06/23 *******************************************************************
     fwdeal4    if   e           k disk    rename(pea01:pea04)
     f                                     prefix(L4_)
      *** End add ***** 11/06/23 *******************************************************************

     D/copy sprstring
      *** end add ***** 12/07/04 *******************************************************************
     D/copy sprdate
     D/copy wprAR
     D/copy wprgroup
     D/copy wpremplr
     D/copy wpragent
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     D psds          esds                  extname(szq1p)
      **** END   ADD **** 04/04/02 *****************************************************************
      **** BEG   ADD **** 06/26/07 *****************************************************************
     D grpflg          s              1    inz
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

     DUsrRstFlg        PR             1
     D user                          10    const
      **** END   ADD **** 06/26/07 *****************************************************************

     DmeetSelect       PR             1

     DWA002R           PR                  extpgm('WA002R')
     D ##co#                               like(EACO#)  const
     D #@fnd                               like(EAFND)  const
     D ##emp#                              like(EAEMP#) const
     D ##div                               like(EADIV)  const
     D ##fyr                               like(EAFYR)  const
     D ##agt#                         5  0              const
     D ##agt_name                    60                 const
     D ##prod                         5  0              const
     D ##prod_nm                     60                 const
     D ##area_cd                      3  0              const
     D ##phone#                       7  0              const
     D ##plan                         1
     D ##asOfDate                     8  0              const
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     DSA008R           PR                  extpgm('SA008R')
     D                                3  0 const
     D                               10    const
     D                                1    const
     D                                1
     D                                1          options(*nopass)
     D                                3  0 const options(*nopass)
     D                                9  0 const options(*nopass)
      **** END   ADD **** 04/04/02 *****************************************************************
    
     D categories      s              1    dim(10) ascend
     D codes           s              2    dim(30) ascend
      *** Beg add ***** 11/06/23 **************************************************
     D batchTypes      s              1    dim(20)
      *** End add ***** 11/06/23 **************************************************
    
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@fnd_fr        s              3
     D @@fnd_to        s              3
     D @@fyr_fr        s              3
     D @@fyr_to        s              3
     D @@ARdt_fr       s              8
     D @@ARdt_to       s              8
     D @@sort          s              1
     D @@agt#          s              5
     D @@emp#          s              9
     D @@div           s              5
     D @@plan          s              1
     D @@ctyp          s             10
     D @@ARCode        s              1
     D @@arcd          s             60
     D kkco#           s                   like(EACO#)
     D kkfnd           s                   like(EAFND)
     D kkfnd_fr        s                   like(EAFND)
     D kkfnd_to        s                   like(EAFND)
     D kkemp#          s                   like(EAEMP#)
     D kkdiv           s                   like(EADIV)
     D kkfyr_fr        s                   like(EAFYR)
     D kkfyr_to        s                   like(EAFYR)
     D kkagt#          s              5  0
     D x7ardt_fr       s                   like(EAPODT)
     D x7ardt_to       s                   like(EAPODT)
     D agent#          s              5  0
     D selfRptEst      s                   like(BSSEFL)
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     D Restrictions    s              1    inz('0')
      **** END   ADD **** 04/04/02 *****************************************************************
     D chkTestGrp      s              1    inz('0')
     D ##plan          s              1
     D ##agt_name      s             60
     D ##prod_nm       s             60
     D EAFND_a         s              3
     D EAFYR_a         s              3
     D EAEMP#_a        s              9
     D EADIV_a         s              5
     D agent#_a        s              5
     D rrn             s              9  0
     D y               s              3  0
      **** BEGIN ADD **** 04/15/02 *****************************************************************
     D category        s              1
      **** END   ADD **** 04/15/02 *****************************************************************
     D*** Begin Add *** 11/09/15 ***********************************************
     D groupSelect     s               n   Inz(*On)
     D toGroup         s                   like(EAFND)
     D*** End   Add *** 11/09/15 ***********************************************
      **** BEG ADD *** 09/07/04 *****************************************   ************************
     D rrn1            s              9  0
     D @@arpt          s              1
     D usa_date        s               d   datfmt(*usa)
     D ##asOfDate      s              8  0
     D xxrndt          s              7  0
     D xxendt          s              7  0
     D xxasof          s              7  0
     D*ed              s              7  0 dim(5)
     D*cd              s              7  0 dim(5)
     D*
     D                 ds                  inz
     D  ed                     1     35  0
     D                                     dim(5) ascend
     D    emefd1               1      7  0
     D    emefd2               8     14  0
     D    emefd3              15     21  0
     D    emefd4              22     28  0
     D    emefd5              29     35  0
     D                 ds                  inz
     D  cd                     1     35  0
     D                                     dim(5) ascend
     D    emcad1               1      7  0
     D    emcad2               8     14  0
     D    emcad3              15     21  0
     D    emcad4              22     28  0
     D    emcad5              29     35  0
      **** END ADD *** 09/07/04 *****************************************   ************************
      *
      *** beg add ***** 12/07/07 *******************************************************************
     d @@btyp          s              1
     D sysDate         s               d   datfmt(*usa)
     D kkcurDate       s              7  0
      *** end add ***** 12/07/04 *******************************************************************
      *** Beg add ***** 11/06/23 *******************************************************************
     d @@BatchTyp      s             20
     d chrBatchTyp     s              1
      *** End add ***** 11/06/23 *******************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     f1ky          klist
     C                   kfld                    kkco#
    C*
     C     f1ky_1        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
     C     f1ky_2        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr_fr
    C*
     C     f1ky_3        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd_fr
     C                   kfld                    kkfyr_fr
    C*
     C     f1ky_4        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd_fr
    C*
     C     eaky8         klist
     C                   kfld                    kkco#
     C                   kfld                    F1FND
     C                   kfld                    F1FYR
    C*
     C     eaky8_1       klist
     C                   kfld                    kkco#
     C                   kfld                    F1FND
     C                   kfld                    F1FYR
     C                   kfld                    kkemp#
    C*
     C     eaky8_2       klist
     C                   kfld                    kkco#
     C                   kfld                    F1FND
     C                   kfld                    F1FYR
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     C     upky          klist
     C                   kfld                    q1user
      **** END   ADD **** 04/04/02 *****************************************************************
      **** BEG ADD *** 09/07/04 *****************************************   ************************
    C*
     C     ebky          klist
     C                   kfld                    eaco#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eaemp#
     C                   kfld                    eadiv
    C*
     C     elky          klist
     C                   kfld                    eaco#
     C                   kfld                    eafnd
     C                   kfld                    eaemp#
     C                   kfld                    eadiv
     C                   kfld                    eafyr
    C*
     C     rsky          klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
      **** END ADD *** 09/07/04 *****************************************   ************************
    C*
      *** beg add ***** 12/07/04 *******************************************************************
      *
      * Vendor History
      *
     c     vhky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eaemp#
     c                   kfld                    eadiv
     c                   kfld                    eafyr

     c     vhky1         klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eaemp#
     c                   kfld                    eadiv
     c                   kfld                    eafyr
     c                   kfld                    kkcurDate
      *
      * Direct Debit History
      *
     c     dbky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eaemp#
     c                   kfld                    eadiv

     c     dbky1         klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eaemp#
     c                   kfld                    eadiv
     c                   kfld                    kkcurDate
      *** end add ***** 12/07/04 *******************************************************************
    C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd_fr
     C                   parm                    @@fnd_to
     C                   parm                    @@fyr_fr
     C                   parm                    @@fyr_to
     C                   parm                    @@ARdt_fr
     C                   parm                    @@ARdt_to
     C                   parm                    @@agt#
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@sort
     C                   parm                    @@plan
     C                   parm                    @@ctyp
     C                   parm                    @@ARCode
     C                   parm                    @@arcd
      **** BEG ADD *** 09/07/04 *****************************************   ************************
     C                   parm                    @@arpt
      **** END ADD *** 09/07/04 *****************************************   ************************
      *** beg add ***** 12/07/04 *******************************************************************
     C                   parm                    @@btyp
      *** end add ***** 12/07/04 *******************************************************************
      *** Beg add ***** 11/06/23 *******************************************************************
     C                   parm                    @@BatchTyp
      *** End add ***** 11/06/23 *******************************************************************
      *** beg add **** 01/04/10 ************************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3

     C     cltid         ifeq      '025'
     C                   move      *on           *in97
     C                   endif
      *** end add **** 01/04/10 ************************************************

     C                   exsr      initialize
     C*
      ******** beg chg ********* 05/28/14 **************************************
      * Move this code to subroutine SRMAIN

     C*****f1ky_3        setll     pf100
     C*****f1ky          reade     pf100                                  60
     C***60*             eval      F1FND = *zero
    C*
     C******             eval      kkfnd = F1FND
     C*
     C******             dow       not *in60 and F1FND <= kkfnd_to
     C*
     C******             if        F1FYR > kkfyr_to or
     C******                        groupAuth(F1CO#:F1FND) = *off or
     C******                        (chkTestGrp = *on and
     C******                         TestGrp(F1CO#:F1FND) = *on)
     C******             exsr      getNextFnd
     C******             endif
     C*
     C******             if        not *in60
     C*
     C******             select
     C******             when      @@div <> *blank
     C*****eaky8_2       chain     pea08                              60
     C******             when      kkemp# <> *zero
     C*****eaky8_1       chain     pea08                              60
     C******             other
     C*****eaky8         chain     pea08                              60
     C******             endsl
    C*
     C******             dow       not *in60
     C*
     C******             if        meetSelect = *on
     C******             exsr      writeWXEAP
     C******             endif
     C*
     C******             select
     C******             when      @@div <> *blank
     C*****eaky8_2       reade     pea08                                  60
     C******             when      kkemp# <> *zero
     C*****eaky8_1       reade     pea08                                  60
     C******             other
     C*****eaky8         reade     pea08                                  60
     C******             endsl
    C*
     C******             enddo
     C*
     C******             endif
     C*
     C*****f1ky_1        reade     pf100                                  60
     C*
     C***60**            exsr      getNextFnd
    C*
     C*******            enddo

      /free
       //*** Begin Add *** 11/09/15 ***********************************************
       //
       // If a group range is passed in then bypass the group select processing
       If groupSelect = *Off;
          Setll (kkCo#:kkFnd_Fr) pfn00;
          ReadE (kkCo#) pfn00;
          Dow not %eof(WMFNL);
             If (toGroup <> 0 and FNfnd <= toGroup) Or toGroup = 0;
                kkFnd_Fr = FNfnd;
                kkFnd_To = FNfnd;
                Exsr srMain;
             Else;
                Leave;
             EndIf;
             ReadE (kkCo#) pfn00;
          Enddo;
       Else;
       //*** End   Add *** 11/09/15 ***********************************************
          Setll kkCo# pgs00;
          Reade kkCo# pgs00;
          Dow not %eof;
             If GSIEFL = 'I';
                kkFnd_Fr = GSfnd;
                kkFnd_To = GSfnd;
                Exsr srMain;
             Endif;
             Reade kkCo# pgs00;
          Enddo;
       //*** Begin Add *** 11/09/15 ***********************************************
       EndIf;
       //*** End   Add *** 11/09/15 ***********************************************
      /end-free
      ******** end chg ********* 05/28/14 **************************************
    C*
     C                   move      *on           *inlr
    C*____________________________________________________________________
    C*
      ******** beg del ********* 05/28/14 **************************************
    C*====================================================================
    C* GetNextFnd - Get the next group
    C*====================================================================
    C*
     C*****GetNextFnd    begsr
     C*
     C*****              dou       *in60 or (F1FYR <= kkfyr_to and
     C*****                                  groupAuth(F1CO#:F1FND) = *on)
    C*
     C*****              eval      kkfnd = F1FND
     C*
     C*****f1ky_1        setgt     pf100
     C*****f1ky          reade     pf100                                  60
    C*
     C*****              if        not *in60
     C*
     C*****              eval      kkfnd = F1FND
    C*
     C*****f1ky_2        setll     pf100
     C*****f1ky_1        reade     pf100                                  60
     C*
      *** beg add *** 07/16/02 *********************************************************************
     C*****              if        *in60
     C*****f1ky_1        setgt     pf100
     C*****f1ky          reade     pf100                                  60
     c*****              eval      kkfnd = F1FND
     C*****              endif
      *** end add *** 07/16/02 *********************************************************************
     C*
     C*****              endif
     C*
     C*****              eval      *in60 = not (not *in60 and
     C*****                                     F1FND <= kkfnd_to)
    C*
     C*****              enddo
    C*
     C*****              endsr
    C*
      ******** end del ********* 05/28/14 **************************************
    C*====================================================================
    C* initialize - Initialize entry fields
    C*====================================================================
     C     initialize    begsr
    C*
     C                   move      @@co#         kkco#
     C*** Begin Add *** 11/09/15 ***********************************************
     C                   If        @@fnd_fr = 'NG'
     C                   Eval      @@fnd_fr = '000'
     C                   Eval      groupSelect = *Off
     C                   EndIf
     C*** End   Add *** 11/09/15 ***********************************************
     C                   move      @@fnd_fr      kkfnd_fr
     C                   move      @@fnd_to      kkfnd_to
     C*** Begin Add *** 11/09/15 *****************************************
     C                   z-add     kkFnd_to      toGroup
     C*** End   Add *** 11/09/15 ***********************************************
     C                   move      @@fyr_fr      kkfyr_fr
     C                   move      @@fyr_to      kkfyr_to
     C                   move      @@agt#        kkagt#
     C                   move      @@emp#        kkemp#
     C                   move      @@div         kkdiv
     C                   movea     @@ctyp        categories
     C                   movea     @@arcd        codes
      *** Beg add ***** 11/06/23 **********************************************
     C                   movea     @@BatchTyp    batchTypes
      *** End add ***** 11/06/23 **********************************************
     C                   eval      x7ARdt_fr= dtCYMD(numDate(@@ARdt_fr))
     C                   eval      x7ARdt_to= dtCYMD(numDate(@@ARdt_to))
     C*
     C                   if        kkfnd_to = *zero
     C                   eval      kkfnd_to = *hival
     C                   endif
     C*
     C                   if        kkfyr_to = *zero
     C                   eval      kkfyr_to = *hival
     C                   endif
     C*
     C                   if        kkfnd_fr <> kkfnd_to
     C                   eval      chkTestGrp = *on
     C                   endif
    C*
      **** BEGIN ADD **** 06/26/07 *****************************************************************
     C     upky          chain     pup01                              60
     c                   eval      grpflg = UsrRstFlg(q1user)
     C
     c                   select
     c     grpflg        wheneq    'M'
     c                   eval      restrictions = *on
     c
     c     grpflg        wheneq    'S'
     c                   eval      restrictions = *on
     c
     c     grpflg        wheneq    'Y'
     c                   eval      restrictions = *on
     c                   eval      kkfnd_fr = upfnd
     C                   eval      kkfnd_to = upfnd
     C                   endsl
     C*
     C******* beg add ****** 09/09/10 *********************************
     C********           If        grpflg  = 'Y' or grpflg  = ' '
     C********                     and  UPXEMP = 'Y'
     C                   If        grpflg = 'Y' and UPXEMP = 'Y' or
     C                             grpflg = ' ' and UPXEMP = 'Y'
     C******* end add ****** 09/09/10 *********************************
     C*
     C                   eval      kkemp#   = upemp#
     C                   Endif
     C*
     C                   if        *in60 = *off
     C**********         if        upxemp = 'Y' and upemp# <> 0
     C**********         eval      kkemp# = upemp#
     C**********         endif                                                  if upxemp = 'Y' and
     C**********         if        upxfnd = 'Y' and upfnd <> 0
     C**********         eval      kkfnd_fr = upfnd
     C**********         eval      kkfnd_to = upfnd
     C**********         endif                                                  if upxfnd = 'Y' and
     C                   if        upxemp = 'Y' or upxfnd = 'Y'
     C                   callp     SA008R(upco#:q1user:'W':Restrictions)
     C                   endif                                                  if upxemp = 'Y' or
     C                   endif                                                  if *in60 = *off
      **** END   ADD **** 06/26/07 *****************************************************************
    C*
      **** beg add ****** 12/07/04 *****************************************************************
     C     *usa          move      *date         sysDate
     C     *cymd         move      sysDate       kkcurDate
      **** end add ****** 12/07/04 *****************************************************************
    C*
     C*** BEG ADD ***** 07/27/10 ************************************
     C                   movel     'WA073R'      xxpgm            10
     C*** BEG CHG ***** 10/22/10 ************************************
     C*******            call      'SRCHKPGM'
     C*******            parm                    xxpgm
     C*******            parm                    xxnach            1
     C*
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1
     C*** END CHG ***** 10/22/10 ************************************
    C*
     C*** END ADD ***** 07/27/10 ************************************
     C                   endsr
      /free
       //****** beg add ******* 05/28/14 *************************************
       //==================================================================
       // SRMAIN
       //==================================================================
       Begsr srMain;

       Setll f1ky_3 pf100;
       Reade f1ky pf100;
       If %eof(WDF1L);
          F1fnd = *zero;
       Endif;
       kkFnd = F1fnd;

       Dow not %eof(WDF1L) and F1fnd <= kkfnd_to;
          If F1fyr > kkfyr_to or groupAuth(F1co#:F1fnd) = *off or (chkTestGrp =
           *on and TestGrp(F1co#:F1fnd) = *on);
             leaveSr;
          Endif;

          Select;
          When @@div <> *blank;
             Setll eaky8_2 pea08;
             Reade eaky8_2 pea08;
          When kkemp# <> *zero;
             Setll eaky8_1 pea08;
             Reade eaky8_1 pea08;
          Other;
             Setll eaky8 pea08;
             Reade eaky8 pea08;
          Endsl;

          Dow not %eof(WMEAL8);
             If meetSelect = *on;
                Exsr writeWXEAP;
             Endif;

             Select;
             When @@div <> *blank;
                Reade eaky8_2 pea08;
             When kkemp# <> *zero;
                Reade eaky8_1 pea08;
             Other;
                Reade eaky8 pea08;
             Endsl;
          Enddo;

          Reade f1ky_1 pf100;
       Enddo;

       Endsr;

       //****** end add ******* 05/28/14 *************************************
      /end-free
    C*====================================================================
    C* writeWXEAP - Write records to WXEAP
    C*====================================================================
    C*
     C     writeWXEAP    begsr
     C*
     C                   eval      EAAGT# = agent#
      **** BEGIN ADD **** 04/15/02 *****************************************************************
     C                   eval      EACTYP = category
      **** END   ADD **** 04/15/02 *****************************************************************
      **** BEGIN ADD **** 01/04/10 *****************************************************************
     C                   if        *in97 = *on
     C     rrn           chain     pem01                              98
     C                   eval      EASEMP = EMSEMP
     C                   endif                                                  *in97 = *on
      **** END   ADD **** 01/04/10 *****************************************************************
    C*
     C                   move      EAFND         EAFND_a
     C                   move      EAFYR         EAFYR_a
     C                   move      EAEMP#        EAEMP#_a
     C                   move      EADIV         EADIV_a
     C                   move      agent#        agent#_a
    C*
    C* Determine sort
    C*
     C                   select
     C                   when      @@sort = 'A'
     C                   eval      EASORT = EAFND_a + EAFYR_a +
     C                                      agent#_a + EAEMP#_a + EADIV_a
     C                   when      @@sort = 'E'
     C                   eval      EASORT = EAFND_a + EAFYR_a +
     C                                      EAEMP#_a + EADIV_a
     C                   when      @@sort = 'N'
     C                   eval      EASORT = EAFND_a + EAFYR_a +
     C                                      getEmpName(EAEMP#:EADIV:rrn:
     C                                                 EAFND:EACO#) +
     C                                      EAEMP#_a + EADIV_a
     C                   endsl
     C*
     C*  Determine date to be used for comparison and load to EAxxDT
    C*
     C                   eval      EAxxDT = GetARDate(EACO#:EAFND:EAFYR:
     C                                                EAEMP#:EADIV:EAARTY:
     C                                                EAPODT:EATRDT)
    C*
      //*** Beg Add ***** 11/06/23 ********************************************
      //Batch Type and Batch Descripion
        Clear EABTyp;
        Clear EABDesc;
        If EABul1 <> 0;
           Chain (EAco#:EAbul1:EAfnd:EAemp#:EAdiv:EAfyr:EApodt) pea04;
           EABTyp = L4_EAACHF;
           Select;
           When EABTyp = ' ';
              EABDesc = 'Standard Batch';
           When EABTyp = 'A';
              EABDesc =  'EFT';
           When EABTyp = 'S';
              EABDesc = 'S/R ';
           When EABTyp = 'L';
              EABDesc = 'Lockbox';
           When EABTyp = 'D';
              EABDesc = 'Deductible';
           When EABTyp = 'R';
              EABDesc = 'Retrospective';
           When EABTyp = 'B';
              EABDesc = 'CiCenter S/R Billing';
           When EABTyp = 'T';
              EABDesc = '3rd Party Payment';
           When EABTyp = 'U';
              EABDesc = 'PayGo Billing';
           Endsl;
        Endif;
        EAbat# = EABul1;
      //*** End Add ***** 11/06/23 ********************************************
     C                   write     pea0px
      **** BEG ADD *** 09/07/04 *****************************************   ************************
     C                   if        @@arpt = 'Y'

     C                   if        EAxxDT >= x7ARdt_fr and EAxxDT <= x7ARdt_to
     C                   write     pea0pa
     C                   endif
    C*
     C     ebky          chain     peb01
    C*
     c                   if        not %found
     C                   eval      ebco#  = eaco#
     C                   eval      ebfnd  = eafnd
     C                   eval      ebfyr  = eafyr
     C                   eval      ebemp# = eaemp#
     C                   eval      ebdiv  = eadiv
     C                   eval      ebsrt1 = *blanks
     C                   eval      ebsrt2 = *blanks
     C                   eval      ebenam = getEmpName(EAEMP#:EADIV:rrn:
     C                                                 EAFND:EACO#)
     C     rrn           chain     pem01                              98
     C                   eval      ebagt# = GetEmpAgt(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR)
     C                   eval      ebanam = *blanks
     C                   eval      ebrgn# = emrgn#
     C                   eval      ebchap = emchap
     C                   eval      ##plan = '*'
     C                   move      *date         usa_date
     C                   move      usa_date      ##asOfDate
     C                   callp     WA002R(eaco#:eafnd:eaemp#:eadiv:eafyr:
     C                                    0:##agt_name:0:##prod_nm:
     C                                    0:0:##plan:##asOfDate)
     C                   if        ##plan = 'A'
     C                   eval      ebacur = 'Y'
     C                   else
     C                   eval      ebacur = *blanks
     C                   endif
    C*
     C                   eval      ebuser = q1user
     C                   eval      ebpgm  = q1pgm
     C                   eval      ebtime = eatime
     C                   eval      ebdt   = kkcurDate
      *
      *  LOAD ORIGINAL INCEPTION / POLICY EFFECTIVE / CANCEL DATES
      *
     C                   move      ED(1)         EBINDT
     C                   move      *ZEROS        EBPFDT
     C                   move      *ZEROS        EBCNDT
      *
     C     *cymd         move      usa_date      xxrndt
     C                   eval      xxendt = x7ardt_to
      *
     C     xxrndt        ifgt      xxendt
     C                   move      xxrndt        xxasof
     C                   else
     C                   move      xxendt        xxasof
     C                   endif
      *
     C                   z-add     5             ix                3 0
     C     ix            dowgt     *zero
      *
     C                   if        ed(ix) <> *zero
     C                             and ed(ix) <= xxasof
     C                   leave
     C                   endif
      *
     C                   sub       1             ix
     C                   enddo
      *
     C                   if        ix > *zero
     C                   if        cd(ix) <= xxasof
     C                   move      cd(ix)        ebcndt
     C                   endif
     C                   endif
      *
     C     elky          chain     pel01                              97
     C                   if        *in97 = *on
     C                   eval      ebpfdt = *zeros

     C                   else
     C                   eval      ebpfdt = elpfdt

     C     rsky          chain     prs01                              97
     C                   if        *in97 = *off
     C                   eval      ebjrst = rsjrst
     C                   else
     C                   eval      ebjrst = *blanks
     C                   endif

     C                   endif

     C                   write     peb01
     C                   endif                                                  not %found
    C*
     C                   endif                                                  @@arpt = 'Y'
      **** END ADD *** 09/07/04 *****************************************   ************************
    C*
     C                   endsr
    C*====================================================================
    C* meetSelect - Determine if the record meets the selection criteria
    C*====================================================================
     PmeetSelect       b

     DmeetSelect       PI             1

      **** BEGIN DEL **** 04/15/02 *****************************************************************
     D**** category         s              1
      **** END   DEL **** 04/15/02 *****************************************************************
     DfoundARcd        s              1
     Dmeet             s              1

      **** BEGIN CHG **** 04/04/02 *****************************************************************
     C**********         eval      meet = empAuth(EACO#:EAFND:EAEMP#)
    C**********
     C**********         eval      agent# = GetEmpAgt(EACO#:EAFND:EAEMP#:
     C**********                            EADIV:EAFYR)
     C**********         if        kkagt# <> *zero and kkagt# <> agent#
     C**********         eval      meet = *off
     C**********         endif
    C**********
     C**********         eval      ##plan = GetEmpPlan(EACO#:EAFND:EAEMP#:
     C**********                            EADIV:EAFYR:x7ardt_to)
    C**********
     C**********         select
     C**********         when      @@plan = 'E' and ##plan = 'A'
     C**********         eval      meet = *off
     C**********         when      @@plan = 'C' and ##plan <> 'A'
     C**********         eval      meet = *off
     C**********         endsl
    C**********
    C*  Determine if the A/R category is to be written.
    C**********
     C**********         if        @@ctyp <> *blanks
     C**********         eval      y = 1
     C**********         eval      category = GetARCat(EAARCD:rrn:EACO#)
     C*****category      lookup    categories(y)                          60
     C**n60              eval      meet = *off
     C**********         endif
    C**********
    C*  Determine if the A/R code is to be written.
    C**********
     C**********         if        @@arcd <> *blanks
     C**********         eval      y = 1
     C**** EAARCD        lookup    codes(y)                               60
     C**********         eval      foundARcd = *in60
     C**********         endif
     C**********
     C**********         select
     C**********         when      @@ARcode = 'E' and foundARcd = *on
     C**********         eval      meet = *off
     C**********         when      @@ARcode = 'I' and foundARcd = *off
     C**********         eval      meet = *off
     C**********         when      @@ARcode = 'C' and
     C**********                    getARComm(EAARCD:rrn:EACO#) = 'Y'
     C**********         eval      meet = *off
     C**********         endsl
      **********
     ***** BEG ADD ***** 04/18/01 ****************************************
      **********
      * Select by Group and FYR
      **********
     C**********         if        (EAFND < kkfnd_fr) or
     C**********                   (EAFND > kkfnd_to) or
     C**********                   (EAFYR < kkfyr_fr) or
     C**********                   (EAFYR > kkfyr_to)
     C**********         eval      meet = *off
     C**********         endif
      **********
     ***** END ADD ***** 04/18/01 ****************************************
      **********
      *
     C                   eval      meet = *on
      *
     C                   if        Restrictions = *on
     C                   eval      meet = empAuth(EACO#:EAFND:EAEMP#)
     C                   endif                                                  if Restrictions
    C*
     C                   if        meet = *on
     C                   eval      agent# = GetEmpAgt(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR)
     C                   if        kkagt# <> *zero and kkagt# <> agent#
     C                   eval      meet = *off
     C                   endif
     C                   endif                                                  if meet = *on
    C*
     C                   if        meet = *on and @@plan <> 'A'
     C                   eval      ##plan = GetEmpPlan(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR:x7ardt_to)
    C*
     C                   select
     C                   when      @@plan = 'E' and ##plan = 'A'
     C                   eval      meet = *off
     C                   when      @@plan = 'C' and ##plan <> 'A'
     C                   eval      meet = *off
     C                   endsl
     C                   endif                                                  if meet = *on and ##
    C*
    C*  Determine if the A/R category is to be written.
    C*
      **** BEGIN CHG **** 04/15/02 *****************************************************************
     C****************** if        meet = *on and @@ctyp <> *blanks
     C****************** eval      y = 1
     C****************** eval      category = GetARCat(EAARCD:rrn:EACO#)
     C                   eval      category = GetARCat(EAARCD:rrn:EACO#)
     C                   if        meet = *on and @@ctyp <> *blanks
     C                   eval      y = 1
      **** END   CHG **** 04/15/02 *****************************************************************
     C     category      lookup    categories(y)                          60
     C  n60              eval      meet = *off
     C                   endif                                                  if meet = *on and @@
    C*
    C*  Determine if the A/R code is to be written.
    C*
     C                   if        meet = *on
     C                   if        @@arcd <> *blanks
     C                   eval      y = 1
     C     EAARCD        lookup    codes(y)                               60
     C                   eval      foundARcd = *in60
     C                   endif
     C*
     C                   select
     C                   when      @@ARcode = 'E' and foundARcd = *on
     C                   eval      meet = *off
     C                   when      @@ARcode = 'I' and foundARcd = *off
     C                   eval      meet = *off
      *** beg chg ***** 06/26/08 ***********************************************
     C***********        when      @@ARcode = 'C' and
     C                   when      @@ARcode = 'C'
     C                   if         getARComm(EAARCD:rrn:EACO#) = 'Y'
     C                   eval      meet = *off
     c                   endif
      *
      * Exclude billing type codes if running for commissionable codes only
      *
     c                   if        EAARTY = 'A' or
     c                             EAARTY = 'B' or
     c                             EAARTY = 'D' or
     c                             EAARTY = 'F' or
     c                             EAARTY = 'H' or
     c                             EAARTY = 'Q'
     C                   eval      meet = *off
     c                   endif
      *** end chg ***** 06/26/08 ***********************************************
     C                   endsl
     C                   endif                                                  if meet = *on

      //*** Beg Add ***** 11/06/23 ********************************************
       Clear chrBatchTyp;
       If meet = *On and @@BatchTyp <> *Blanks;
          Chain (EAco#:EAbul1:EAfnd:EAemp#:EAdiv:EAfyr:EApodt) pea04;
          If %found(Wdeal4);
             If L4_EAACHF = ' ';
                chrBatchTyp = '#';
             Else;
                chrBatchTyp = L4_EAACHF;
             Endif;
          Else;
             meet = *Off;
          Endif;
          If meet = *On;
             y = %Lookup(chrBatchTyp:batchTypes);
             If y = 0;
                meet = *Off;
             Endif;
          Endif;
       Endif;
      //*** End Add ***** 11/06/23 ********************************************
      *
      * Select by Group and FYR
      *
     C                   if        meet = *on
     C                   if        (EAFND < kkfnd_fr) or
     C                             (EAFND > kkfnd_to) or
     C                             (EAFYR < kkfyr_fr) or
     C                             (EAFYR > kkfyr_to)
     C                   eval      meet = *off
     C                   endif
     C                   endif                                                  if meet = *on
      *
      **** END   CHG **** 04/04/02 *****************************************************************
      *** beg add ***** 12/07/04 *******************************************************************
      *
      *  Select transaction based upon billing type.
      *
     C                   if        meet = *on

     c                   select
     c                   when      @@btyp = 'A'
     C*** BEG CHG ***** 07/27/10 ************************************
     C*
     C* check if new ACH module is installed ('WC1806')
     C*
      **** BEG   CHG **** 10/22/10 *********************************************
     C*******            if        xxnach = *on
      *** Beg change *** 04/07/21 **********************************************
     C**                 if        xxnach = *off
      **** END   CHG **** 10/22/10 *********************************************
     C**   dbky          chain     pef01                              62
     C**   *in62         doweq     *off
     C**   kkcurDate     iflt      efexdt
     C**   kkcurDate     andge     efefdt
     C**   efexdt        oreq      *zero
     C**   kkcurDate     andge     efefdt
     C**
     C**                 eval      meet = *off
     C**                 leave
     C**                 endif
     C**   dbky          reade     pef01                                       62
     C**                 enddo
     C**                 else
     C*** END CHG ***** 07/27/10 ************************************
     c**   dbky1         setgt     pdb01
     c**   dbky          readpe    pdb01
     c**                 if        %eof or
     c**                           not %eof and kkcurDate >= dbexdt
     C**                 eval      meet = *off
     C**                 endif                                                  %eof
     C*** BEG CHG ***** 07/27/10 ************************************
     C**                 endif                                                  %eof
       strCo# = %Editc(EAco#:'X');
       strEftType = 'E';
       strFnd = %Editc(EAfnd:'X');
       strEftKey = %Editc(EAemp#:'X') + %Editc(EAdiv:'X');
       strDate =%Editc(kkCurDate:'X');
       strFyr = %Editc(EAfyr:'X');
       meet = GetEftStatus(strCo#:strEftType:strfnd:strEftKey:
                           strDate:strPre:strRrn:strEobTyp:
                           strCktp:strFyr);
     C*** END CHG ***** 07/27/10 ************************************
      *** End change *** 04/07/21 **********************************************

     c                   when      @@btyp = 'P'
     c     vhky1         setgt     pvh01
     c     vhky          readpe    pvh01
     c                   if        %eof
     C                   eval      meet = *off
     C                   endif                                                  %eof

     c                   when      @@btyp = 'D'
     c     vhky1         setgt     pvh01
     c     vhky          readpe    pvh01
     c                   if        not %eof
     c                   eval      meet = *off
     c                   endif                                                  not %eof

     c     dbky1         setgt     pdb01
     c     dbky          readpe    pdb01
     c                   if        not %eof and dbexdt = 0 or
     c                             not %eof and kkcurDate < dbexdt
     c                   eval      meet = *off
     c                   endif                                                  not %eof

     c                   endsl

     C                   endif                                                  if meet = *on
      *** end add ***** 12/07/04 **********************************************
     C*
     C                   return    meet

     PmeetSelect       e
