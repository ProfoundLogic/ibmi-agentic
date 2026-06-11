     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H/TITLE W7871R - WORKFILE BUILD - AGENT ACTIVITY REPORT
    H*********************************************************************
    H*                                                                   *
    H* CALLED BY: W7871C - OVERRIDE                                      *
    H*                                                                   *
    H* CALLS TO:  NONE                                                   *
    H*                                                                   *
    H* CREATED:   11/08/02  C:61  PROJECT WC1073                         *
    H* Converted: 10/10/03  C:33  Project: WC1017                        *
     *********************************************************************
     H* Revised:
     H*                                                                   *
     H* 02/28/18  C:33  Support: 64498                                    *
     H*                 Get group information when using group select     *
     H*                                                                   *
     H* 01/09/17 C:306 Project: WC2441                                    *
     H*                 - Implement the group selection functinality.     *
     H*                 - Changed the parameter list                      *
     H*                                                                   *
     H* 09/21/16 C:136 Support: 43065                                     *
     H*                Pass agt# to Agent API to retrieve correct         *
     H*                producer                                           *
     H*                                                                   *
     H* 06/15/16 C:136 Support: 42041                                     *
     H*                Add group restriction check                        *
     H*                                                                   *
     H* 10/17/13  C:65  Project: WC2115/SY266 Redmine #3534               *
     H*                 Fix the renewal type coding to convert 'F' and    *
     H*                 'G' types to 'p' and 'q'.                         *
     H*                                                                   *
     H* 11/06/12  C:33  Project: WC2115                                   *
     H*                 -Implement renewal and new types.                 *
     H*                                                                   *
     H* 05/28/10 C:136 SUPPORT: WA133 72856                               *
     H*                ADD HARBOR EXPOSURE FEE TO PREMIUM FOR LUBA        *
     H*                                                                   *
     H* 06/02/09  C:33  Support                                           *
     H*                 Process TEST group if only group selected.        *                        *
     H*                                                                   *
     H* 06/01/09  C:62  Project: 133WGX                                   *
     H*                 FQ Flag processing                                *                        *
     H*                                                                   *
     H* 05/28/09  C:33  Project: 133WGV                                   *
     H*                 Use a new file for quote status.                  *                        *
     H*                                                                   *
    H* 05/27/09 C:62  Project: 133WGW                                    *
    H*                Add parms to call of WA002R.                       *
     H*                                                                   *
    H* 12/05/07 C:33  Seq#: WA133/61167                                  *
    H*                NOT needed after V7R1.                             *
     H*                                                                   *
    H* 10/19/07 C:193 Project: WC1561                                    *
    H*                Add a new Quote Tracking type 'S' CLOSED.          *
     H*                                                                   *
    H* 07/12/07 C:33  Seq#: WA045/58805                                  *
    H*                Correctly process agent reads.                     *
     H*                                                                   *
    H* 01/30/07 C:09  Project: WA045 56915                               *
    H*                Correct code added on 08/19/05.                    *
     H*                                                                   *
     H* 10/24/06  C:33  Project: 132WFE   WA133/55470                     *
    H*                 Pass a unique parm for agent/producer variables.  *
     H*                                                                   *
     H* 10/11/06  C:132  Project:  wa093 - 55162
     H*           Save Agt Number before Callp, since returns zero Agt#.
     H*                                                                   *
    H* 10/04/06 C:09  Project: WA129 54816                               *
    H*                Use a unique indicator for primary WAQTP read      *
     H*                                                                   *
     H* 10/03/06  C:58  Project:  71WHI
     H*           Get premium from WDEHL1, Premium history file.
     H*                                                                   *
     H* 05/01/06  C:33  Project: 132WFE                                   *
     H*                 Add:                                              *
     H*                  Governing Class Code, Experience Modifier (all   *
     H*                  three) and effective dates, Schedule Debit/Credit*
     H*                  Factor, and Commission Rate Percentage.          *
    H*
    H* 08/19/05    C:132  Project: WA133 48605
    H*           - When request single agent FEIN, and put in
    H*             zeros for MOD, then only do the all zero mod
    H*           - If MOD is blanks and FEIN is not, do all mods
    H*
    H*  7/06/05    C:58     Project: WC1310
    H*             Update new - Auto update to lost flag
    H*
    H* 11/18/04 C:33  Project: 132WCB                                    *
    H*                Add new type                                       *
      *
      * 07/20/04 C:61  Support WA093 43252
      *                Expand Quote Type Description field.
      *
      * 01/12/04 C:61  Support WA121 40676 WC1017
      *                Correct issue with selection of types.
    H*                                                                   *
    H* 10/10/03 C:33  Project: WC1017                                    *
    H*                Add new types                                      *
    H*                                                                   *
    H* 05/14/03 C:61  PROJECT WC577                                      *
    H*                If list all modifiers for agency if modifier       *
    H*                selection left blank.                              *
    H*********************************************************************
     *
     * System Company
     Fsmcol     if   e           k disk
     *
     * User Parameter
     Fwmupl     if   e           k disk
     Fwmupl1    if   e           k disk    rename(pup01:pup02)
     *
     * Group Master
     Fwmfnl     if   e           k disk
     *
     * Agent Assignment
     Fwmaal2    if   e           k disk
     *
     * Agent Master
     Fwmagl     if   e           k disk
     *
     * Agent Quote
     Fwaqtl1    if   e           k disk
     Fwaqtl15   if   e           k disk    rename(pqt01:pqt15)
     *
     * Rating State
     Fwmrsl     if   e           k disk
     *
     *  Employer Detail
     Fwdell2    if   e           k disk
     Fwdell2q   if   e           k disk    rename(pel01:pel01q)
     *
     *  County Table
     Fwtcnl1    if   e           k disk
     *
     *  Region
     Fwmrel     if   e           k disk
     *
     *  Chapter Code
     Fwmcpl     if   e           k disk
     *
     *  Employer Master
     Fwmeml     if   e           k disk
     Fwmemlq    if   e           k disk    rename(pem01:pem01q)
     *
     * Quote Type Inclusion File
     Fwxq2l1    if   e           k disk    prefix(xq2_)
     *
    F*   Premium History File
     Fwdehl1    if   e           k disk
     Fwtqsl1    if   e           k disk
    F*
     * Report Out File
     Fwfq2p     o    e             disk
     *
     F***** Beg Add ***** 01/09/17 *****************************************
     * Fund Work File - CO#/FND
     Fwxgsl     if   e           k disk
     F***** End Add ***** 01/09/17 *****************************************
     *
     *
     D #ofTypes        c                   15
     D rqstdTypes      s              1    dim(999)
     D tp              s              1    dim(#ofTypes) ctdata perrcd(1)
     D  tpd            s             35    dim(#ofTypes) alt(tp)
     *
     *
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@asrt          s              1
     D @@co#           s              3
     D @@dtyp          s              1
     D @@fein          s              9
     D @@ffnd          s              3
     D @@frdt          s              8
     D @@mod           s              3
     D @@pqb           s              1
     D @@tfnd          s              3
     D @@todt          s              8
     D @@fqfl          s              1
     D j1              s              3  0 inz
     D kkkey           s                   like(cnkey)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D sysdt7          s                   like(fndt)
     D xxbal           s                   like(q2bal)
     D xxco#           s                   like(coco#)
     D xxfein          s                   like(aafein)
     D xxffnd          s                   like(fnfnd)
     D xxfnd           s                   like(fnfnd)
     D xxfrdt          s                   like(fndt)
     D xxmod           s                        like(aamod) INZ
     D xxtfnd          s                   like(fnfnd)
     D xxtodt          s                   like(fndt)
     D xxtype          s                   like(QTTYPE)
     D xxwrpr          s              9  0
     D x1usnm          s             50
     D name60          s             60
     D xxKey           s                   like(QSKEY)
     D agent#          s                   like(AAAGT#)
     D producer        s                   like(AAAGT#)
      ** Beg Add **** 01/09/17 **************************************
     D @@GrpSlct       s              1
      ** End Add **** 01/09/17 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     *
     D/copy sPrDate
     D/copy sPrString
      *** beg add ***** 06/15/16 *****************
     D/copy wPrGroup
      *** end add ***** 06/15/16 *****************

     D GetAgtInfo      pr                  extPgm('WA002R')
     D  co#                                like(ELCO#)  const
     D  fnd                                like(ELFND)  const
     D  emp#                               like(ELEMP#) const
     D  div                                like(ELDIV)  const
     D  fyr                                like(ELFYR)  const
     D  agt#                               like(AAAGT#) const
     D  agtNm                        60                 const
     D  producer                           like(AAAGT#)
     D  prodNm                       60
     D  agtAreaCode                   3  0              const
     D  agtPhone#                     7  0              const
     D  plan                          1                 const
     D  asOfDate                      8  0              const
     D  newRate                       5  2
     D  renewRate                     5  2
     D  rate#                         3  0
     D  agentEmail                   60                 const
     D  ctrlAgtFlag                   1
     D GetAgtInfoC     pr                  extPgm('WA002C')
     D  co#                                like(ELCO#)  const
     D  fnd                                like(ELFND)  const
     D  emp#                               like(ELEMP#) const
     D  div                                like(ELDIV)  const
     D  fyr                                like(ELFYR)  const
     D  agt#                               like(AAAGT#) const
     D  agtNm                        60                 const
     D  producer                           like(AAAGT#)
     D  prodNm                       60
     D  agtAreaCode                   3  0              const
     D  agtPhone#                     7  0              const
     D  plan                          1                 const
     D  asOfDate                      8  0              const
     D  newRate                       5  2
     D  renewRate                     5  2
     D  rate#                         3  0
     D  agentEmail                   60                 const
     D  ctrlAgtFlag                   1
     *
     *=================================================================
     * KLIST
     *=================================================================
     *
     * Rating State
     *
     C     rsky          klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     *
     *  Employer Detail
     *
     C     elky1         klist
     C                   kfld                    xxco#
     C                   kfld                    qtfnd
     C                   kfld                    qtemp#
     C                   kfld                    qtdiv
     C                   kfld                    qtefdt
     *
     C     elky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qtfnd
     C                   kfld                    qtemp#
     C                   kfld                    qtdiv
     *
     *  County
     *
     C     cnky          klist
     C                   kfld                    xxco#
     C                   kfld                    kkkey
     *
     *  Region
     *
     C     reky          klist
     C                   kfld                    xxco#
     C                   kfld                    emfnd
     C                   kfld                    emrgn#
     *
     *  Chapter Code
     *
     C     cpky          klist
     C                   kfld                    xxco#
     C                   kfld                    qtfnd
     C                   kfld                    qtchap
     *
     *  Employer Master
     *
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    qtfnd
     C                   kfld                    qtemp#
     C                   kfld                    qtdiv
     *
     *
     *   KEY LIST FOR THE QUOTE FILE
     *
     C     qtky1         klist
     C                   kfld                    xxco#
     C                   kfld                    fnfnd
     C                   kfld                    aaagt#
     *
     C     qtky2         klist
     C                   kfld                    xxco#
     C                   kfld                    fnfnd
     C                   kfld                    xxtype
     C                   kfld                    aaagt#
     *
     *  Agent Assignment Master
     *
     C     aaky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     *
     C     aaky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfein
     *
     C     aaky2         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfein
     C                   kfld                    xxmod
     *
     *  Agent Master
     *
     C     agky          klist
     C                   kfld                    xxco#
     C                   kfld                    aafein
     C                   kfld                    aamod
     *
     *  Company
     *
     C     cokey         klist
     C                   kfld                    xxco#
     *
     *  Group
     *
     C     fnky          klist
     C                   kfld                    xxco#
     C*
     C*   KEY LIST FOR EMPLOYER DETAIL HISTORY FILE
     C     ehkey         klist
     C                   kfld                    qtco#
     C                   kfld                    qtfnd
     C                   kfld                    qtemp#
     C                   kfld                    qtdiv
     C                   kfld                    elfyr
     C*
     C     keyWTQSL1     klist
     C                   kfld                    QTCO#
     C                   kfld                    xxKey
     *
     *=================================================================
     *  PLIST
     *=================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@ffnd
     C                   parm                    @@tfnd
     C                   parm                    @@frdt
     C                   parm                    @@todt
     C                   parm                    @@dtyp
     C                   parm                    @@pqb
     C                   parm                    @@asrt
     C                   parm                    @@fein
     C                   parm                    @@mod
     C                   parm                    @@fqfl
      ** Beg Add **** 01/09/17 **************************************
     C                   parm                    @@GrpSlct
      ** End Add **** 01/09/17 **************************************
     *
     *==============================================================
     *  BEGIN
     *==============================================================
     *
     * Initialize
     *
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3
     *
     C                   move      @@co#         xxco#
     C                   move      @@ffnd        xxffnd
     C                   move      @@tfnd        xxtfnd
     *
     C                   move      @@frdt        srdti
     C                   eval      xxFrDt = DtCYMD(srDtI)
     *
     C                   move      @@todt        srdti
     C                   eval      xxToDt = DtCYMD(srDtI)
     *
     C                   move      @@fein        xxfein
     C                   if        @@mod = '  0' or
     C                             @@mod = ' 00' or
     C                             @@mod = '000' or
     C                             @@mod = ' 0 ' or
     C                             @@mod = '00 ' or
     C                             @@mod = '0  ' or
     C                             @@mod = '0 0'
     C                   MOVE      '000'         @@mod
     C                   endif

     C                   if        @@mod <> *BLANK
     C                   move      @@mod         xxmod
     C                   endif

     C                   reset                   j1
     C     *loval        setll     wxq2l1
     C                   dou       %eof
     C                   read      wxq2l1
     C                   if        not %eof
     C                   eval      j1 = j1 + 1
     C                   eval      rqstdTypes(j1) = xq2_Q2QTYP
     C                   endif
     C                   enddo
     *
     *
     *  GET USER NAME
     *
     C                   clear                   x1usnm
     C     q1user        chain     pup01                              60
     C                   if        *in60 = *off
     C                   movel     upusnm        x1usnm
     C                   endif
     C                   time                    dsdttm
     C                   eval      sysDt7 = DateTo7(SystemDate)
     *
     *  GET COMPANY NAME
     *
     C     cokey         chain     pco01                              60
     *
     *   PROCESS GROUP(S)
     *
      *** beg add ***** 01/09/17 *****************
     c                   If        @@GrpSlct <> 'Y'
      *** end add ***** 01/09/17 *****************
     c     fnky          setll     wmfnl
     c                   if        %equal
     c     fnky          reade     wmfnl
     c                   endif
     c                   dow       not %eof(wmfnl)
    
      *** beg add ***** 06/15/16 *****************
      /free
       If GroupAuth(xxCo#:FNfnd) = *off;
          Reade xxCo# wmfnl;
          Iter;
       Endif;
      /end-free
      *** end add ***** 06/15/16 *****************
    
     C                   if        fntest <> 'Y'
     C                             and fnfnd >= xxffnd
     C                             and fnfnd <= xxtfnd
     C                             or (FNFND = xxFFnd and xxFFnd = xxTFnd)
     C                   exsr      srfnd
     C                   endif
     *
     C     fnky          reade     wmfnl
     C                   enddo
      *** beg add ***** 01/09/17 *****************
     C                   Else
     c     fnky          setll     wxgsl
     c                   read      wxgsl
     c                   Dow       not %eof(wxgsl)

     c                   If        GSieFl ='I'
     c                   move      GSfnd         fnfnd
       //***  Begin Add  ***  02/28/18  ***********************************
      /Free
       Chain (GSco#:GSfnd) wmfnl;
      /End-free
       //***  End   Add  ***  02/28/18  ***********************************
     C                   exsr      srfnd
     c                   EndIf
     c
     c                   read      wxgsl
     c                   EndDo
     c
     c                   EndIf
      *** end add ***** 01/09/17 *****************
     *
     *
     C                   move      *on           *inlr
     *
     *
     *
     *================================================================
     * SRFND - Processing for Group
     *================================================================
     C     srfnd         begsr
     *
     C                   eval      xxfnd = fnfnd
     *
     C                   if        fnanam <> *blanks
     C                   movel     fnanam        coname
     C                   endif
     *
     * For each Agent
     *
     C                   select

     c                   when      xxfein <> *ZERO and
     c                             @@mod  <> *BLANKS
     c     aaky2         setll     WMAAL2
     c                   if        %equal(WMAAL2)
     c     aaky2         reade     WMAAL2
     c                   endif

     c                   when      xxfein <> *ZERO and
     c                              @@mod = *BLANKS
     c     aaky1         setll     WMAAL2
     c                   if        %equal(WMAAL2)
     c     aaky1         reade     WMAAL2
     c                   endif

     c                   other
     c     aaky          setll     WMAAL2
     c                   if        %equal(WMAAL2)
     c     aaky          reade     WMAAL2
     c                   endif
     c                   endsl

     c                   dow       not %eof(WMAAL2)
     *
     *  Get Agent Commitment balance
     *
     C                   exsr      agtcmt
     *
     *  READ QUOTE FILE
     C*
     C     qtky1         chain     pqt01                              75
     *
     C                   dow       *in75 = *off
     C                   exsr      srqte
     C     qtky1         reade     pqt01                                  75
     *
     C                   enddo
     *
     C                   select

     c                   when      xxfein <> *ZERO and
     c                             @@mod  <> *BLANKS
     c     aaky2         reade     WMAAL2

     c                   when      xxfein <> *ZERO and
     c                              @@mod = *BLANKS
     c     aaky1         reade     WMAAL2

     c                   other
     c     aaky          reade     WMAAL2
     c                   endsl

     C                   enddo
     *
     C                   endsr
     *
     *==================================================================
     * SRQTE - READ QUOTE FILE
     *==================================================================
     C     srqte         begsr
     C*** begin add ***  10/17/13  **************************************
      /free

       //* Convert the old quote types into the new renewal quote types.
       Select;
       When QTType = 'F';
          QTType = 'p';
       When QTType = 'G';
          QTType = 'q';
       Endsl;

      /end-free
     C*** end add *****  10/17/13  **************************************
     *
     * ASSUME INCLUSION
     *
     C                   eval      *in90 = *off
     *
     * CHECK FOR DATE EXCLUSION
     *
     C                   select
     *
     * Type change date
     *
     C                   when      @@dtyp = 'T'
     C                   if        qtwkdt < xxfrdt
     C                             or qtwkdt > xxtodt
     C                   eval      *in90 = *on
     C                   endif
     *
     * Effective date
     *
     C                   when      @@dtyp = 'E'
     C                   if        qtefdt < xxfrdt
     C                             or qtefdt > xxtodt
     C                   eval      *in90 = *on
     C                   endif
     *
     * Original Submission date
     *
     C                   when      @@dtyp = 'O'
     C                   if        qtordt < xxfrdt
     C                             or qtordt > xxtodt
     C                   eval      *in90 = *on
     C                   endif
     *
     * New Business date
     *
     C                   when      @@dtyp = 'N'
     C                   if        qtnbdt < xxfrdt
     C                             or qtnbdt > xxtodt
     C                   eval      *in90 = *on
     C                   endif
     *
     C                   endsl
     *
     * Quote Type found in inclusion file
     *
     C                   if        *in90 = *off
     C     QTTYPE        lookup    rqstdTypes                             99
     C                   eval      *in90 = not %found
     C                   endif
     *
     * Include all Pended, Quoted, Bound regardless of date range = 'Y'
     *
      /Free
       If @@pqb = 'Y'
        and (Upper(QTTYPE)='P' or Upper(QTTYPE)='Q' or Upper(QTTYPE)='B');
          *in90 = *off;
       Endif;
      /End-free
     *
     C                   if        *in90 = *off
     C                   exsr      srout
     C                   endif
     *
     C                   endsr
     *
     *================================================================
     *  SROUT - Write record to Out File
     *================================================================
     C     srout         begsr
     *
     C                   clear                   pq201
     C                   eval      q2co# = coco#
     C                   movel(p)  coname        q2conm
     C                   eval      q2fnd = fnfnd
     C                   movel(p)  fnname        q2fnnm
     C                   eval      q2emp# = qtemp#
     C                   eval      q2div = qtdiv
     *
     C     emky          chain     pem01                              61
     C* Always chain to the quote environment.  If the employer is found in
     C*  the QUOTE environment and in the LIVE environment, it would
     C*  indicate the employer has been renewed to QUOTE.  If that is the
     C*  case, the employer master information can not be changed and should
     C*  be the same as the LIVE environment.
     C     emky          chain     pem01q                             62
     *
b1   C                   if        *in61 = *off
     C                             or *in62 = *off
b2   C                   if        (@@fqfl='F' and emiusr<>'WCEXPRESS') or
     C                             (@@fqfl='W' and emiusr='WCEXPRESS')
     C                   leavesr
e2   C                   endif
     *
     * Rate Set Code / Description / Rating State
     *
     C     elky1         setgt     pel01
     C     elky2         readpe    pel01                                  61
     C     elky1         setgt     pel01q
     C     elky2         readpe    pel01q                                 62
     *
b3   C                   if        not *in61 or not *in62
     C                   movel(p)  elrtcd        q2rtcd
     C     rsky          chain     prs01                              60
b4   C                   if        *in60 = *off
     C                   movel(p)  rsdesc        q2rsds
     C                   movel(p)  rsjrst        q2jrst
e4   C                   endif
     *
     C                   eval      Q2EXDT = DtMDYY(ELPTDT)
      *
      * Governing Class Code
      *
     C                   eval      Q2GOVC = ELGOVC
      *
      * Experience Modifiers
      *
     C                   eval      Q2EXM1 = ELEXM1
     C                   eval      Q2EXD1 = DtMDYY(ELEMD1)
     C                   eval      Q2EXM2 = ELEXM2
     C                   eval      Q2EXD2 = DtMDYY(ELEMD2)
     C                   eval      Q2EXM3 = ELEXM3
     C                   eval      Q2EXD3 = DtMDYY(ELEMD3)
      *
      * Schedule Debit/Credit
      *
     C                   eval      Q2DBCR = ELDBCR
     C                   eval      Q2SDEF = DtMDYY(ELSDEF)
     C                   eval      Q2SDEX = DtMDYY(ELSDEX)
      *
      * Save Agent number before CALLP
     C                   EVAL      Q2AGT#  = AAAGT#
      *** beg add ***** 09/21/16 ***************************
     c                   Eval      agent#  = AAagt#
      *** end add ***** 09/21/16 ***************************
      *
      * Agent Rate
      *
     C* Change agent variable    (AAAGT# -> agent#)
     C* Change producer variable (AAAGT# -> producer)
     C                   if        not *in62
     C                   callp     GetAgtInfoC(ELCO#:ELFND:ELEMP#:ELDIV:
     C                                        ELFYR:agent#:name60:Q2PROD:
     C                                        Q2PNAM:000:0000000:'*':
     C                                        00000000:Q2NWRT:Q2RNRT:
     C                                        Q2RT#:name60:Q2CAFL
     C                                       )
     C                   else
     C                   callp     GetAgtInfo(ELCO#:ELFND:ELEMP#:ELDIV:
     C                                        ELFYR:agent#:name60:Q2PROD:
     C                                        Q2PNAM:000:0000000:'*':
     C                                        00000000:Q2NWRT:Q2RNRT:
     C                                        Q2RT#:name60:Q2CAFL
     C                                       )
     C                   endif
     *
e3   C                   endif
     *
     * Region
     *
     C                   eval      q2rgn# = emrgn#
     C     reky          chain     pre01                              60
     C                   if        *in60 = *off
     C                   movel     redesc        q2reds
     C                   endif
     *
     * Employer County / Description
     *
     C                   move      emcnty        q2cnty
     *
     C                   if        empst <> *blanks
     C     empst         cat       emcnty:0      kkkey
     *
     C                   else
     *
     C     emmst         cat       emcnty:0      kkkey
     C                   endif
     *
     C     cnky          chain     pcn01                              60
     C                   if        *in60 = *off
     C                   movel(p)  cndesc        q2cnds
     C                   endif
     *
     * Employer Underwriter
     *
     C                   movel     emuwtr        q2euwt
     C     emuwtr        chain     pup02                              60
     C                   if        *in60 = *off
     C                   movel     upusnm        q2euwn
     C                   endif
     *
     C                   else
     C                   leavesr
     C                   endif
     *
     * Agent
      *
      * If Q2AGT# already loaded before callP, bypass load here
     C                   if        q2agt# = *zeros
     C                   eval      q2agt# = aaagt#
     C                   endif
     *
     C                   eval      q2fein = aafein
     C                   eval      q2amod = aamod
     *
     C                   eval      Q2AEDT = DtMDYY(AAEFDT)
     *
     C                   eval      q2cmt = aacmmt
     *
     C                   eval      Q2CMDT = DtMDYY(AACMDT)
     *
     * Agent Underwriter
     *
     C                   movel     aauwtr        q2auwt
     C     aauwtr        chain     pup02                              60
     C                   if        *in60 = *off
     C                   movel     upusnm        q2auwn
     C                   endif
     *
     *
     C     agky          chain     pag01                              60
     *
     C                   if        *in60 = *off
     C                   movel(p)  agname        q2agnm
     *
     * Marketing Rep
     *
     C                   if        agmkrp <> *blanks
     C                   movel(p)  agmkrp        q2mkrp
     C     agmkrp        chain     pup02                              60
     C                   if        *in60 = *off
     C                   movel(p)  upusnm        q2mkrn
     C                   endif
     C                   endif
     *
     C                   endif
     *
     * QUOTE TYPE
     *
     C                   move      QTTYPE        Q2QTYP
     *
     C                   eval      xxKey = QTTYPE
     C     keyWTQSL1     chain     pqs01
     C                   if        %found
     C                   eval      Q2QSRT = QSAARO
     C                   eval      Q2QDES = QSAARH
     C                   else
     C                   eval      j1 = 1
     C     QTTYPE        lookup    tp(j1)                                 60
     C                   if        *in60 = *on
     C                   eval      Q2QSRT = j1
     C                   eval      Q2qdes = tpd(j1)
     C                   endif
     C                   endif
     *
     * Account Name
     *
     C                   movel(p)  qtname        q2enam
     *
     * Employer experience modifier
     *
     C                   eval      q2emod = qtmod
     *
     C                   eval      q2auls = qtauls
     * Chapter
     *
     C                   eval      q2chap = qtchap
     C     cpky          chain     pcp01                              60
     C                   if        *in60 = *off
     C                   movel(p)  cpdesc        q2chds
     C                   endif
     *
     * Effective date
     *
     C                   eval      Q2EFDT = DtMDYY(QTEFDT)
     *
     * Type Change date
     *
     C                   eval      Q2WKDT = DtMDYY(QTWKDT)
     *
     * Original Submission date
     *
     C                   eval      Q2ORDT = DtMDYY(QTORDT)
     *
     * New Business date
     *
     C                   eval      Q2NBDT = DtMDYY(QTNBDT)
     *
     * Quote Fax date
     *
     C                   eval      Q2FXDT = DtMDYY(QTFXDT)
     *
     C                   movel(p)  qtcarr        q2carr

     C                   if        *in61 = *off or *in62 = *off
     C     ehkey         setgt     peh01
     C     ehkey         readpe    peh01                                  99
     C                   dow       *in99 = *off and ehfaf = 'A'
     C     ehkey         readpe    peh01                                  99
     C                   enddo
     C                   if        *in99 = *off and ehfaf <> 'A'
     C                   eval      qtprem = ehwnmp + ehprtd
     c                   if        cltid = '133'
     c                   eval      qtprem = qtprem + ehwhxm
     c                   endif
     C                   endif
     C                   endif
     C*
     C* Assign FQ Flag
     C*
     C                   if        emiusr='WCEXPRESS'
     C                   eval      q2fqfl='Y'
     C                   else
     C                   eval      q2fqfl=' '
     C                   endif

     C                   eval      q2prem = qtprem
     C                   movel(p)  qtcomm        q2com1
     C                   movel(p)  qtcom2        q2com2
     *
     C                   movel     q1user        q2user
     C                   movel     q1pgm         q2pgm
     C                   time                    dsdttm
     C                   eval      Q2TIME = TimeTo6(SystemTime)
     C                   eval      Q2DT = sysDt7
     *
     C                   eval      q2bal = xxbal
     *
     C                   write     pq201
     *
     C                   endsr
     *
     *================================================================
     * AGTCMT - GET WRITTEN PREMIUM FOR AGT / GROUP
     *================================================================
     C     agtcmt        begsr
     *
     C                   eval      xxbal = 0
     C                   eval      xxwrpr = 0
     C                   move      'W'           xxtype
     C     qtky2         chain     pqt15                              50
     *
     C                   dow       *in50 = *off
     *
     * Check date exclusion
     *
     C                   select
     *
     C                   when      @@pqb = 'Y'
     C                   eval      xxwrpr = xxwrpr + qtprem
     *
     * Type change date
     *
     C                   when      @@dtyp = 'T'
     C                   if        qtwkdt >= xxfrdt
     C                             and qtwkdt <= xxtodt
     C                   eval      xxwrpr = xxwrpr + qtprem
     C                   endif
     *
     * Effective date
     *
     C                   when      @@dtyp = 'E'
     C                   if        qtefdt >= xxfrdt
     C                             and qtefdt <= xxtodt
     C                   eval      xxwrpr = xxwrpr + qtprem
     C                   endif
     *
     * Original Submission date
     *
     C                   when      @@dtyp = 'O'
     C                   if        qtordt >= xxfrdt
     C                             and qtordt <= xxtodt
     C                   eval      xxwrpr = xxwrpr + qtprem
     C                   endif
     *
     * New Business date
     *
     C                   when      @@dtyp = 'N'
     C                   if        qtnbdt >= xxfrdt
     C                             and qtnbdt <= xxtodt
     C                   eval      xxwrpr = xxwrpr + qtprem
     C                   endif
     *
     C                   endsl
     *
     C     qtky2         reade     pqt15                                  50
     C                   enddo
     *
     C                   eval      xxbal = aacmmt - xxwrpr
     *
     C                   endsr
     *
     *
**  TYPE TABLE
LSUBMISSIONS NOT WRITTEN (LOST)
DSUBMISSIONS NOT WRITTEN (DECLINED)
USUBMISSIONS NOT WRITTEN (SUBMITTED)                     11/18/04
WSUBMISSIONS WRITTEN
XACCOUNTS TRANSFERED (OUT)
TACCOUNTS TRANSFERED (IN)
PACCOUNTS PENDING
FRENEWALS PENDING                                        10/10/03
QACCOUNTS QUOTED
GRENEWALS QUOTED                                         10/10/03
BACCOUNTS BOUND
MACCOUNTS BOUND W/MONEY
CACCOUNTS CANCELLED
RACCOUNTS REINSTATED
SACCOUNTS CLOSED                                         10/19/07
