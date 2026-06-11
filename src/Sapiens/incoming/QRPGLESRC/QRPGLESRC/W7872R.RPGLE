     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
    H/TITLE W7872R - REPORT - AGENT ACTIVITY REPORT - AGENT SORT
    H*********************************************************************
    H*                                                                   *
    H* CALLED BY: W7871C - OVERRIDE                                      *
    H*                                                                   *
    H* CALLS TO:  NONE                                                   *
    H*                                                                   *
    H* CREATED:   11/08/02  C:61  PROJECT WC1073                         *
    H* Converted: 10/10/03  C:33  Project: WC1017                        *
    H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
     H* 01/09/17 C:306 Project: WC2441                                    *
     H*                 - Implement the group selection functinality.     *
     H*                 - Changed the parameter list                      *
     H*                 - Changed the logic to create report on the       *
     H*                   basis of groups that were selected on           *
     H*                   group select screen                             *
     H*                                                                   *
     H* 08/05/15 C:307 Project: SY354                                     *
     H*                -Adding 'Route To:'                                *
     H*                                                                   *
     H* 08/26/14 C:298 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
     H* 11/06/12  C:33  Project: WC2115                                   *
     H*                 -Implement renewal and new types.                 *
    H*                                                                   *
     H* 04/26/10 C:248 SUPPORT                                            *
     H*                AGENT ACTIVITY REPORT.                             *
     H*                THE AGENT SORT Y/N NEEDS TO BE CHANGED TO READ     *
     H*                "PAGE BREAK BY AGENT" Y OR N.                      *
     H*                                                                   *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
    H* 11/23/09 C:062 Support: WA133/70352                               *
    H*                Move controlling agent flag to detail.             *
    H*                                                                   *
    H* 08/27/09 C:227 Support: WA133/69666                               *
    H*                Changed the length of variable.                    *
    H*                                                                   *
    H* 06/01/09 C:62  Project: 133WGX                                    *
    H*                Add FQ Flag.                                       *
    H*                                                                   *
    H* 05/27/09 C:62  Project: 133WGW                                    *
    H*                Add Primary sort.                                  *
    H*                                                                   *
    H* 08/26/05 C:33  Seq#: WA133/48605                                  *
    H*                Allow for Mod to be blank.                         *
      *
      * 07/20/04 C:61  Support WA093 43252
      *                Expand Quote Type Description field.
    H*                                                                   *
    H* 10/10/03 C:33  Project: WC1017                                    *
    H*                Add new types                                      *
    H*********************************************************************
     *
     * System Company
     Fsmcol     if   e           k disk
     *
     * System Output Routing
     Fsmorl     if   e           k disk
     *
     * User Parameter
     Fwmupl     if   e           k disk
     *
     * Quote Type Inclusion File
     Fwxq2l1    if   e           k disk
     *
     * Quote Type Exclusion File
     Fwxq2l2    if   e           k disk    rename(xq201:xq202)
     *
     * Agent Activity Report Work File
     Fwfq2l     if   e           k disk
     * Agent Activity Report Work File - Agent Name Order
     Fwfq2l2    if   e           k disk    rename(pq201:pq202)
     *
     *  Detail Output
      ** Beg Chg **** 01/09/17 **************************************
     F***qsysprt   o    f  198        printer oflind(*inoa)
     Fqsysprt   o    f  240        printer oflind(*inoa)
      ** End Chg **** 01/09/17 **************************************
     F                                     usropn
     *
     *  Summary Output
      ** Beg Chg **** 01/09/17 **************************************
     F***qsysprt1  o    f  198        printer oflind(*inob)
     Fqsysprt1  o    f  240        printer oflind(*inob)
      ** End Chg **** 01/09/17 **************************************
     F                                     usropn
     *
     F***** Beg Add ***** 01/09/17 *****************************************
     * Fund Work File - CO#/FND
     Fwxgsl     if   e           k disk
     F***** End Add ***** 01/09/17 *****************************************
     *
     *
     D xtitl0          c                   const('*** AGENT ACTIVITY -
     D                                     REPORT - AGENT SORT-
     D                                      ***')
     *
     D xtitl1          c                   const('*** AGENT ACTIVITY -
     D                                     REPORT SUMMARY - AG-
     D                                     ENT SORT ***')
     *
     D tdat            c                   const('Type change date')
     D edat            c                   const('Effective date')
     D odat            c                   const('Original submission-
     D                                      date')
     D ndat            c                   const('New business date')
     D pSortAA         c                   const('Agent Name')
     D pSortAN         c                   const('Agent #')
     D pFQ             c                   const('FQ')
     D pWC             c                   const('WC')
     D pAll            c                   const('*All')
     D pFQDesc         c                   const('FastQuote')
     D pWCDesc         c                   const('WCMS')
     D pAllDesc        c                   const('All Employers')
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     *
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##leng          s              3  0
     D @@asrt          s              1
     D @@co#           s              3
     D @@dsb           s              1
     D @@dtyp          s              1
     D @@fein          s              9
     D @@ffnd          s              3
     D @@frdt          s              8
     D @@mod           s              3
     D @@outq          s             10
     D @@pSrt          s              2
     D @@fqfl          s              1
     D @@pfb           s              1
     D @@pqb           s              1
     D @@tfnd          s              3
     D @@todt          s              8
     D agtSelection    s             15
     D kkco#           s                   like(q2co#)
     D kkfnd           s                   like(q2fnd)
     D kkprt           s              1
     D line            s            198
     D lr$sub          s             13  0
     D lr#sub          s              6  0
     D lractv          s              6  0
     D lrcnt           s              9  0
     D lrhit           s              6  2
     D lrnwrt          s              6  0
     D lrprem          s             13  0
     D lrtotw          s              6  0
     D lrwrit          s              6  0
     D l1cnt           s              7  0
     D l1prem          s             13  0
     D l2$sub          s             13  0
     D l2#sub          s              6  0
     D l2actv          s              6  0
     D l2cnt           s              7  0
     D l2hit           s              6  2
     D l2nwrt          s              6  0
     D l2prem          s             13  0
     D l2totw          s              6  0
     D l2writ          s              6  0
     D l3$sub          s             13  0
     D l3#sub          s              6  0
     D l3actv          s              6  0
     D l3cnt           s              7  0
     D l3hit           s              6  2
     D l3nwrt          s              6  0
     D l3prem          s             13  0
     D l3totw          s              6  0
     D l3writ          s              6  0
     D meetl1          s              1
     D meetl2          s              1
     D meetl3          s              1
     D meetpr          s              1
     D meetss          s              1
     D n               s              3  0
     D rqstdTypes      s             40
     D pSortDesc       s             30
     D pFQFlag         s              4
     D pFQFlagDesc     s             30
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D svaedt          s                   like(q2aedt)
     D svagnm          s                   like(q2agnm)
     D svagt#          s                   like(q2agt#)
     D svamod          s                   like(q2amod)
     D svbal           s                   like(q2bal)
     D svcafl          s                   like(q2cafl)
     D caflYesNo       s              3
     D svcmdt          s                   like(q2cmdt)
     D svcmt           s                   like(q2cmt)
     D svco#           s                   like(q2co#)
     D svconm          s                   like(q2conm)
     D svfein          s                   like(q2fein)
     D svfnd           s                   like(q2fnd)
     D svfnnm          s                   like(q2fnnm)
     D svmkrn          s                   like(q2mkrn)
     D svqsrt          s                   like(q2qsrt)
     D svqdes          s                   like(q2qdes)
     D svqtyp          s                   like(q2qtyp)
     D x               s              3  0 inz(1)
     D xxdata          s             60
     D x0titl          s                   like(ortitl)
     D x1co#           s              3  0
     D x1date          s             25
     D x1fein          s              9  0
     D x1ffnd          s              3  0
     D x1frdt          s              8  0
     D x1mod           s              3  0
     D x1tfnd          s              3  0
     D x1titl          s                   like(ortitl)
     D x1todt          s              8  0
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D***  Begin Add  ***  11/06/12  **************************************
     DstrRenew         s             20    inz
     D***  End   Add  ***  11/06/12  **************************************
      ** Beg Add **** 01/09/17 **************************************
     D @@GrpSlct       s              1a
     D strSelGrp       s             48a
      ** End Add **** 01/09/17 **************************************
     D*
     D/copy SPrRtvBrnd
     D/copy sPrString
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     *
     *===================================================================
     *  KLIST
     *===================================================================
     *
     *   Output Routing
     *
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    kkprt
     *
     *===================================================================
     *  *LIKE DEFN
     *===================================================================
     *
     *   ENTRY PARAMETERS
     *
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
     C                   parm                    @@pfb
     C                   parm                    @@dsb
     C                   parm                    @@outq
     C                   parm                    @@pSrt
     C                   parm                    @@fqfl
      ** Beg Add **** 01/09/17 **************************************
     C                   parm                    @@GrpSlct
      ** End Add **** 01/09/17 **************************************
     *
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     *   Initialize
     *
     C                   time                    dsdttm
     C                   move      *all'_'       line
     C                   move      *on           meetpr
     *
     *  PGMSTR executes first
     *
     C                   exsr      pgmstr
      /Free
       //***** Beg Add ***** 01/09/17 ***********************************
      // Making the list of selected groups which needs to be printed on the
      // report.
        Exsr MakeGrpLst;
       //***** End Add ***** 01/09/17 ***********************************
      /End-Free
     *
     *   Print Routing Page
     *
     C                   exsr      srrte
     *
     *  Read 1st record
     *
     C                   exsr      read1
     *
     *  Initialize break criteria
     *
     C                   exsr      izl1bk
     C                   exsr      izl2bk
     C                   exsr      izl3bk
     *
     C                   dow       *in91 = *off
     *
     *  If subset criteria met
     *
     C                   move      *on           meetss
     C                   exsr      subset
     *
     C                   if        meetss = *on
     *
     *  Test for break in report
     *
     C                   move      *on           meetl1
     C                   move      *on           meetl2
     C                   move      *on           meetl3
     C                   exsr      l1tst
     C                   exsr      l2tst
     C                   exsr      l3tst
     *
     *  L1 Break
     *
     C                   if        meetl1 = *off
     C                             or meetl2 = *off
     C                             or meetl3 = *off
     *
     C                   if        l1cnt <> *zero
     C                   exsr      l1bk
     C                   endif
     *
     C                   exsr      izl1bk
     C                   endif
     *
     *  L2 Break
     *
     C                   if        meetl2 = *off
     C                             or meetl3 = *off
     *
     C                   if        l2cnt <> *zero
     C                   exsr      l2bk
     C                   endif
     *
     C                   exsr      izl2bk
     C                   endif
     *
     *  L3 Break
     *
     C                   if        meetl3 = *off
     *
     C                   if        l3cnt <> *zero
     C                   exsr      l3bk
     C                   endif
     *
     C                   exsr      izl3bk
     C                   endif
     *
     *  Write detail record
     *
     C                   exsr      srdet
     C                   move      *off          meetpr
     *
     *  Accumulate Totals
     *
     C                   eval      l1cnt = l1cnt + 1
     C                   eval      l2cnt = l2cnt + 1
     C                   eval      l3cnt = l3cnt + 1
     C                   eval      lrcnt = lrcnt + 1
     C                   exsr      acctot
     *
     C                   endif
     *
     *  Read next record
     *
     C                   exsr      readn
     *
     C                   enddo
     *
     *  Last Record processing
     *
     C                   if        meetpr = *on
     C                             or lrcnt = *zero
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     C                   eval      *inoa = *on
     C                   eval      *in92 = *on
     C                   except    head
     C                   except    norec
     C                   endif
     *
     C                   if        @@dsb = 'S'
     C                             or @@dsb = 'B'
     C                   eval      *inob = *on
     C                   eval      *in92 = *on
     C                   except    heads
     C                   except    norecs
     C                   endif
     *
     C                   else
     *
     C                   if        l1cnt <> *zero
     C                   exsr      l1bk
     C                   endif
     *
     C                   if        l2cnt <> *zero
     C                   exsr      l2bk
     C                   endif
     *
     C                   if        l3cnt <> *zero
     C                   exsr      l3bk
     C                   endif
     *
     C                   exsr      lrbk
     *
     C                   endif
     *
     *  End of Processing
     *
     C     end           tag
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If @@dsb = 'D' or @@dsb = 'B';
          kkPrt = '0';
          Chain orKy POR01;
          If %found;
             Except route;
          Endif;
       Endif;
       If @@dsb = 'S' or @@dsb = 'B';
          kkPrt = '1';
          Chain orKy POR01;
          If %found;
             Except route1;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************

      /End-free
     C                   move      *on           *inlr
      /Free
       //***** Beg Add ***** 01/09/17 ***********************************
       BegSr MakeGrpLst;
        *in13 = *off;
        If @@GrpSlct = 'Y';
           *in13 = *on;

           setll (x1co#:*loval) wxgsl;
           read wxgsl;
           Dow not %eof(wxgsl);
              If GSieFl = 'I';
                 If strSelGrp = *blanks;
                    eval strSelGrp = %editc(GSfnd:'X');
                 Else;
                    eval strSelGrp =%trim(strSelGrp) + ',' +
                     %editc(GSfnd:'X');
                 Endif;
              Endif;
           read wxgsl;
           EndDo;
        EndIf;

       EndSr;
       //***** End Add ***** 01/09/17 ***********************************
      /End-free
     *
     *****************************************************************
     *
     *****************************************************************
     * PGMSTR - Set-up and Initialize
     *****************************************************************
     *
     C     pgmstr        begsr
     *
     *  Entry parm processing
     *
     C                   move      @@co#         x1co#
     C                   move      @@ffnd        x1ffnd
     C                   move      @@tfnd        x1tfnd
     C                   move      @@frdt        x1frdt
     C                   move      @@todt        x1todt
     C                   move      @@fein        x1fein
     C                   move      @@mod         x1mod
     *
     C                   select
     *
     C                   when      @@dsb = 'D'
     C                   open      qsysprt
     *
     C                   when      @@dsb = 'S'
     C                   open      qsysprt1
     *
     C                   other
     C                   open      qsysprt
     C                   open      qsysprt1
     *
     C                   endsl
     *
     C                   select
     *
     C                   when      @@dtyp = 'T'
     C                   move(p)   tdat          x1date
     *
     C                   when      @@dtyp = 'E'
     C                   move(p)   edat          x1date
     *
     C                   when      @@dtyp = 'O'
     C                   move(p)   odat          x1date
     *
     C                   when      @@dtyp = 'N'
     C                   move(p)   ndat          x1date
     *
     C                   endsl
     *
     C     *loval        setll     wxq2l2
     C                   read      wxq2l2
     C     *loval        setll     wxq2l1
     C                   read      wxq2l1
     C                   select
     C                   when      %eof(wxq2l2)
     C                   eval      rqstdTypes = '*ALL'
     C                   other
     C                   dow       not %eof(wxq2l1)
      /Free
       If x > 39;
          %subst(rqstdTypes:38:3) = '...';
          Leave;
       Endif;
      /End-free
     C                   eval      %subst(rqstdTypes:x:1) = Q2QTYP
     C                   eval      x = x + 2
     C                   read      wxq2l1
     C                   enddo
     C                   endsl
     *
     C                   select
     C                   when      x1Fein = *zero
     C                   eval      agtSelection = '*ALL'
     C                   when      @@mod = *blank
     C                   eval      agtSelection = @@Fein + ' *ALL'
     C                   other
     C                   eval      agtSelection = @@Fein + ' ' + @@Mod
     C                   endsl
     C*
     C* Assign Primary sort Description
     C*
     C                   if        @@pSrt='AA'
     c                   eval      pSortDesc=pSortAA
     c                   else
     c                   eval      pSortDesc=pSortAN
     c                   endif
     C*
     C* Assign FQ Flag Description
     C*
     c                   select
     C                   when      @@fqfl='F'
     c                   eval      pFQFlag=pFQ
     c                   eval      pFQFlagDesc=pFQDesc
     c                   when      @@fqfl='W'
     c                   eval      pFQFlag=pWC
     c                   eval      pFQFlagDesc=pWCDesc
     c                   other
     c                   eval      pFQFlag=pAll
     c                   eval      pFQFlagDesc=pAllDesc
     c                   endsl
     *
     C                   eval      *in93 = *off
     *
     C                   endsr
     *
     *****************************************************************
     * READ1 - Read first record from file
     *****************************************************************
     *
     C     read1         begsr
     C                   if        @@psrt='AA'
     C                   read      pq202                                  91
     c                   else
     C                   read      pq201                                  91
     C                   endif
     C                   endsr
     *
     *****************************************************************
     * READN - Read next record from file
     *****************************************************************
     *
     C     readn         begsr
     *
     C                   if        @@psrt='AA'
     C                   read      pq202                                  91
     c                   else
     C                   read      pq201                                  91
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * SUBSET - Check if member of records processed subset
     *****************************************************************
     C     subset        begsr
     *
     *  MEETSS = *OFF, if not to be processed
     *
     *
     C                   endsr
     *
     *****************************************************************
     * L1TST - First Level Break TEST criteria
     *****************************************************************
     *
     C     l1tst         begsr
     *
     *  MEETL1 = *OFF if L1BK processing to proceed
     *
     *  Quote Type
     *
     C                   if        q2qsrt <> svqsrt
     C                             or q2qtyp <> svqtyp
     C                   move      *off          meetl1
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * L1BK  - First Level Break action
     *****************************************************************
     *
     C     l1bk          begsr
     *
     *  Quote Type
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     C                   except    l1tot
     C                   eval      *in93 = *off
     C                   endif
     *
     C                   if        @@dsb = 'S'
     C                             or @@dsb = 'B'
     C                   if        *inob = *on
     C                   except    heads
     C                   endif
     C                   if        *inob = *on
     C                   eval      *inob = *off
     C                   endif
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * IZL1BK - Initialize Level 1 break criteria
     *****************************************************************
     *
     C     izl1bk        begsr
     *
     *  Quote type
     *
     C                   move      q2qtyp        svqtyp
     C                   movel(p)  q2qdes        svqdes
     C                   eval      svqsrt = q2qsrt
     *
     C                   eval      l1cnt = 0
     C                   eval      l1prem = 0
     *
     C                   endsr
     *
     *****************************************************************
     * L2TST - Second Level Break TEST criteria
     *****************************************************************
     *
     C     l2tst         begsr
     *
     *  MEETL2 = *OFF if L2BK processing to proceed
     C                   if        @@psrt='AA'
     *
     *  Agent Name
     *
     C                   if        q2agnm <> svagnm
     C                   move      *off          meetl2
     C                   endif
     C*
     C                   else
     *
     *  Agent
     *
     C                   if        q2agt# <> svagt#
     C                             or q2fein <> svfein
     C                             or q2amod <> svamod
     C                   move      *off          meetl2
     C                   endif
     c                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * L2BK  - Second Level Break Action
     *****************************************************************
     *
     C     l2bk          begsr
     *
     *  Hit Ratio
     *
     C                   eval      l2totw = l2writ + l2nwrt
     *
     C                   if        l2totw <> 0
     C                   eval(h)   l2hit = l2writ / l2totw
     C                   endif
     *
     *  Agent totals
     *
     C                   eval      *in94 = *on
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     C                   except    head
     C                   except    l2tot
     C                   endif
     *
     C                   if        @@dsb = 'S'
     C                             or @@dsb = 'B'
     C                   except    l2tots
     C                   endif
     *
     C                   eval      *in94 = *off
     *
     *  PAGE BREAK AFTER
     *
     C                   eval      *inoa = *on
     C                   eval      *inob = *on
     *
     C                   endsr
     *
     *****************************************************************
     * IZL2BK - Initialize Level 2 break criteria
     *****************************************************************
     *
     C     izl2bk        begsr
     *
     *  Agent
     *
     C                   eval      svagt# = q2agt#
     C                   eval      svfein = q2fein
     C                   eval      svamod = q2amod
     C                   movel(p)  q2agnm        svagnm
     C                   eval      svaedt = q2aedt
     C                   eval      svcmt = q2cmt
     C                   eval      svcmdt = q2cmdt
     C                   eval      svbal = q2bal
     C                   movel(p)  q2mkrn        svmkrn
     *
     C                   eval      l2cnt = 0
     C                   eval      l2prem = 0
     C                   eval      l2nwrt = 0
     C                   eval      l2writ = 0
     C                   eval      l2totw = 0
     C                   eval      l2hit = 0
     C                   eval      l2actv = 0
     C                   eval      l2#sub = 0
     C                   eval      l2$sub = 0
     *
     C                   endsr
     *
     *****************************************************************
     * L3TST - Third Level Break TEST criteria
     *****************************************************************
     *
     C     l3tst         begsr
     *
     *  MEETL3 = *OFF if L3BK processing to proceed
     *
     *  Company / Group
     *
     C                   if        q2co# <> svco#
     C                             or q2fnd <> svfnd
     C                   move      *off          meetl3
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * L3BK  - Third Level Break Action
     *****************************************************************
     *
     C     l3bk          begsr
     *
     *  Hit Ratio
     *
     C                   eval      l3totw = l3writ + l3nwrt
     *
     C                   if        l3totw <> 0
     C                   eval(h)   l3hit = l3writ / l3totw
     C                   endif
     *
     *  Group totals
     *
     C                   eval      *in95 = *on
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     C                   except    head
     C                   except    l3tot
     C                   endif
     *
     C                   if        @@dsb = 'S'
     C                             or @@dsb = 'B'
     C                   except    heads
     C                   except    l3tots
     C                   endif
     *
     C                   eval      *in95 = *off
     *
     *  PAGE BREAK AFTER
     *
     C                   eval      *inoa = *on
     C                   eval      *inob = *on
     *
     C                   endsr
     *
     *****************************************************************
     * IZL3BK - Initialize Level 3 break criteria
     *****************************************************************
     *
     C     izl3bk        begsr
     *
     *  Company / Group
     *
     C                   eval      svco# = q2co#
     C                   movel(p)  q2conm        svconm
     C                   eval      svfnd = q2fnd
     C                   movel(p)  q2fnnm        svfnnm
     *
     C                   eval      l3cnt = 0
     C                   eval      l3prem = 0
     C                   eval      l3nwrt = 0
     C                   eval      l3writ = 0
     C                   eval      l3totw = 0
     C                   eval      l3hit = 0
     C                   eval      l3actv = 0
     C                   eval      l3#sub = 0
     C                   eval      l3$sub = 0
     *
     C                   endsr
     *
     *****************************************************************
     * SRDET - Write Detail Record
     *****************************************************************
     *
     C     srdet         begsr
     *
     C                   eval      *in71 = *off
     C                   eval      *in72 = *off
     C                   eval      *in73 = *off
     C                   eval      *in74 = *off
     C                   eval      *in75 = *off
     C                   eval      *in76 = *off
     C                   eval      *in77 = *off
     C                   eval      *in78 = *off
     C                   eval      svcafl = q2cafl
     C*
     C                   if        svcafl = 'Y'
     C                   eval      caflYesNo='Yes'
     C                   else
     C                   eval      caflYesNo='No '
     C                   endif
     *
     C                   eval      *in71 = q2efdt = *zero
     C                   eval      *in72 = q2wkdt = *zero
     C                   eval      *in73 = q2ordt = *zero
     C                   eval      *in74 = q2nbdt = *zero
     C                   eval      *in75 = q2fxdt = *zero
     C                   eval      *in76 = q2com1 = *blanks
     C                   eval      *in77 = q2com2 = *blanks
     C                   eval      *in78 = q2prod = *zeros
     *
     C   oa              if        q2conm <> *blanks
     C                   movel(p)  q2conm        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        coname
     C                   endif
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     C                   if        *inoa = *on
     C                   eval      *in93 = *off
     C                   endif
     C                   if        *inoa = *on
     C                   except    head
     C                   endif
     C                   if        *inoa = *on
     C                   eval      *inoa = *off
     C                   endif
     C                   if        *in93 = *off
      /Free
       //***  Begin Add  ***  11/06/12  ***********************************
       Select;
       When Q2qTyp = Lower(Q2qTyp);
          strRenew = '* * Renewals * *';
       Other;
          Clear strRenew;
       Endsl;
       //***  End   Add  ***  11/06/12  ***********************************
      /End-free
     C                   except    qtyp
     C                   endif
     C                   eval      *in93 = *on
     C                   except    det
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * ACCTOT - Accumulate Totals
     *****************************************************************
     *
     C     acctot        begsr
     *
     *  Premium
     *
     C                   eval      l1prem = l1prem + q2prem
     C                   eval      l2prem = l2prem + q2prem
     C                   eval      l3prem = l3prem + q2prem
     C                   eval      lrprem = lrprem + q2prem
     *
     *  Hit Ratios
     *
     C                   if        q2qtyp <> 'C'
     C                             and q2qtyp <> 'W'
     C                             and q2qtyp <> 'R'
     C                             and q2qtyp <> 'T'
     C                             and q2qtyp <> 'X'
     C                   eval      l2nwrt = l2nwrt + 1
     C                   eval      l3nwrt = l3nwrt + 1
     C                   eval      lrnwrt = lrnwrt + 1
     C                   endif
     *
     C                   if        q2qtyp = 'W'
     C                   eval      l2writ = l2writ + 1
     C                   eval      l3writ = l3writ + 1
     C                   eval      lrwrit = lrwrit + 1
     C                   endif
     *
     *  # active accounts
     *
     C                   if        q2qtyp = 'W'
     C                             or q2qtyp = 'R'
     C                   eval      l2actv = l2actv + 1
     C                   eval      l3actv = l3actv + 1
     C                   eval      lractv = lractv + 1
     C                   endif
     *
     *  Submissions
     *
     C                   if        q2qtyp <> 'C'
     C                             and q2qtyp <> 'R'
     C                   eval      l2#sub = l2#sub + 1
     C                   eval      l3#sub = l3#sub + 1
     C                   eval      lr#sub = lr#sub + 1
     C                   eval      l2$sub = l2$sub + q2prem
     C                   eval      l3$sub = l3$sub + q2prem
     C                   eval      lr$sub = lr$sub + q2prem
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * LRBK - Last Record Processing
     *****************************************************************
     *
     C     lrbk          begsr
     *
     *  Hit Ratio
     *
     C                   eval      lrtotw = lrwrit + lrnwrt
     *
     C                   if        lrtotw <> 0
     C                   eval(h)   lrhit = lrwrit / lrtotw
     C                   endif
     *
     C                   eval      *in92 = *on
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     C                   except    head
     C                   except    lrtot
     C                   endif
     *
     C                   if        @@dsb = 'S'
     C                             or @@dsb = 'B'
     C                   except    heads
     C                   except    lrtots
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     * SRRTE - Print routing page
     *****************************************************************
     *
     C     srrte         begsr
     *
     C     x1co#         chain     pco01                              60
     *
     C                   movel(p)  coname        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        coname
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     *
     C     q1user        chain     pup01                              60
     *
     *   Output Routing - detail report 0
     *
     C                   if        @@dsb = 'D'
     C                             or @@dsb = 'B'
     *
     C                   move      '0'           kkprt
     C     orky          chain     por01                              60
     *
     C                   if        *in60 = *off
     *
     * Left Justify Title in Field
     *
     C                   if        ortitl <> *blanks
     C                   move(p)   ortitl        xxdata
     C     ' '           check     xxdata        n
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      x0titl
     C                   endif
     C                   endif
     *
     *  User Name if no Distribution specified in routing
     *
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
     *
      ***** Beg Del ***** 08/26/14 **********************************
     C******             except    route
      ***** End Del ***** 08/26/14 **********************************
     *
     C                   else
     *
     C                   movel(p)  xtitl0        x0titl
     C                   endif
     *
     *  Center Title in Field
     *
     C                   movel(p)  x0titl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        x0titl
     *
     C                   endif
     *
     *   Output Routing - Summary report 1
     *
     C                   if        @@dsb = 'S'
     C                             or @@dsb = 'B'
     C                   move      '1'           kkprt
     C     orky          chain     por01                              60
     *
     C                   if        *in60 = *off
     *
     * Left Justify Title in Field
     *
     C                   if        ortitl <> *blanks
     C                   move(p)   ortitl        xxdata
     C     ' '           check     xxdata        n
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      x1titl
     C                   endif
     C                   endif
     *
     *  User Name if no Distribution specified in routing
     *
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
     *
      ***** Beg Del ***** 08/26/14 **********************************
     C***********        except    route1
      ***** End Del ***** 08/26/14 **********************************
     *
     C                   else
     *
     C                   movel(p)  xtitl1        x1titl
     C                   endif
     *
     *  Center Title in Field
     *
     C                   movel(p)  x1titl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        x1titl
     *
     C                   endif
     *
     C                   eval      *inoa = *on
     C                   eval      *inob = *on
     *
     C                   endsr
     *
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14  **********************************
     O*******                                     8 'XXXXXXXX'
     O*******                                    20 'XXXXXX'
     O*******                                    34 'XX      XX'
     O*******                                    45 'XXXXXXXXXX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XXX     XX'
     O*******                                    77 'XXXXXX'
     O*******   e            route          1
     O*******                                     9 'XXXXXXXXX'
     O*******                                    21 'XXXXXXXX'
     O*******                                    34 'XX      XX'
     O*******                                    45 'XXXXXXXXXX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XXXX    XX'
     O*******                                    78 'XXXXXXXX'
     O*******   e            route          1
     O*******                                    10 'XX      XX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XXXX    XX'
     O*******                                    79 'XX      XX'
     O*******   e            route          1
     O*******                                    10 'XX      XX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX XX   XX'
     O*******                                    71 'XX'
     O*******   e            route          1
     O*******                                     9 'XXXXXXXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX XX   XX'
     O*******                                    71 'XX'
     O*******   e            route          1
     O*******                                     8 'XXXXXXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX  XX  XX'
     O*******                                    71 'XX'
     O*******   e            route          1
     O*******                                     4 'XXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX  XX  XX'
     O*******                                    79 'XX    XXXX'
     O*******   e            route          1
     O*******                                     5 'XXXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX   XX XX'
     O*******                                    79 'XX    XXXX'
     O*******   e            route          1
     O*******                                     6 'XX XXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX   XX XX'
     O*******                                    79 'XX      XX'
     O*******   e            route          1
     O*******                                     7 'XX  XXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX    XXXX'
     O*******                                    79 'XX      XX'
     O*******   e            route          1
     O*******                                     8 'XX   XXX'
     O*******                                    21 'XXXXXXXX'
     O*******                                    33 'XXXXXXXX'
     O*******                                    41 'XX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XX    XXXX'
     O*******                                    78 'XXXXXXXX'
     O*******   e            route          3
     O*******                                     9 'XX    XXX'
     O*******                                    20 'XXXXXX'
     O*******                                    32 'XXXXXX'
     O*******                                    41 'XX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XX     XXX'
     O*******                                    77 'XXXXXX'
     *
     O*******   e            route          1
     O*******                                    15 'ROUTE TO:'
     O*******                ordis1              56
     *
     O*******   e            route          1
     O*******                ordis2              56
     *
     O*******   e            route          1
     O*******                ordis3              56
     *
     O*******   e            route          1
     O*******                ordis4              56
      ***** End Del ***** 08/26/14  **********************************
      ***** Beg Add ***** 08/05/15  **********************************
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O                       ordis1              56
     *
     O          e            route          1
     O                       ordis2              56
     *
     O          e            route          1
     O                       ordis3              56
     *
     O          e            route          1
     O                       ordis4              56
     *
      ***** End Add ***** 08/05/15  **********************************
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       x0titl              80
     *
      ***** Beg Del ***** 08/26/14  **********************************
     O*******   e            route          2
     O*******                                    19 'PROGRAM NAME:'
     O*******                orpnam              30
     O*******                                    36 'FILE:'
     O*******                orfil#              38
     O*******                                    46 'COPIES:'
     O*******                orcopy        z     50
     O*******                                    56 'HOLD:'
     O*******                orhold              61
     *
     O*******   e            route          2
     O*******                                    19 'OUTPUT QUEUE:'
     O*******                oroutq              30
     O*******                                    37 'FORMS:'
     O*******                orform              48
     O*******                                    56 'SAVE:'
     O*******                orsave              61
      ***** End Del ***** 08/26/14  **********************************
     *
     O          e            route       2  2
     O                                           24 'REPORT SELECTIONS:'
     *
     O          e            route          2
      ***** Beg chg ***** 01/09/17 ***********************************
     O*                                          35 'Group range . . . . . :'
     O*                      x1ffnd            +001
     O*                                        +001 '-'
     O*                      x1tfnd            +001

     O              N13                          35 'Group range . . . . . :'
     O              N13      x1ffnd            +001
     O              N13                        +001 '-'
     O              N13      x1tfnd            +001

     O               13                          35 'Groups. . . . . . . . :'
     O               13      strSelGrp         +  1
      ***** End chg ***** 01/09/17 ***********************************
     *
     O          e            route          2
     O                                           35 'Type of date range    :'
     O                       x1frdt            +001 '  /  /    '
     O                                         +001 '-'
     O                       x1todt            +001 '  /  /    '
     *
     O          e            route          2
     O                                           35 'Type of date  . . . . :'
     O                       @@dtyp            +001
     O                       x1date            +002
     *
     O          e            route          2
     O                                           35 'Quote types . . . . . :'
     O                       rqstdTypes        +001
     *
     O          e            route          1
     O                                           35 'Include all Pended,    '
     O          e            route          2
     O                                           35 '  Quoted, Bound  (Y/N):'
     O                       @@pqb             +001
     *
     O          e            route          2
     O                                           35 'Page Break by Agent (Y/N):'
     O                       @@asrt            +001
     O          e            route          2
     O                                           35 'Primary sort   (AA/AN):'
     O                       @@psrt            +001
     O                       pSortDesc         +001
     *
     O          e            route          2
     O                                           35 'Agent selection . . . :'
     O                       agtSelection      +001
     O          e            route          2
     O                                           35 'Type of Quotes (FQ/WC):'
     O                       pFQFlag           +001
     O                       pFQFlagDesc       +001
     *
     O          e            route          2
     O                                           35 'Output type    (P/F/B):'
     O                       @@pfb             +001
     *
     O          e            route          2
     O                                           35 'Report type    (D/S/B):'
     O                       @@dsb             +001
     *
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     *
     O          e            head           1 03
     O                       dstime               8 '  :  :  '
     O                       coname             119
     O                       dsdate        y    198
     *
     O          e            head           1
     O                       q1pgm               10
     O                       x0titl             129
     O                                          192 'PAGE:'
     O                       page          z    198
     *
     O          e            head        1  1
      ***** Beg chg ***** 01/09/17 ***********************************
     O***                                      +005 'Group range:'
     O***                    x1ffnd            +001
     O***                                      +001 '-'
     O***                    x1tfnd            +001
     O
     O              N13                        +005 'Group range:'
     O              N13      x1ffnd            +001
     O              N13                        +001 '-'
     O              N13      x1tfnd            +001
     O
     O               13                          12 'Groups:'
     O               13      strSelGrp         +  1
      ***** Beg chg ***** 01/09/17 ***********************************
     O                       x1date            +001
     O                                         +001 'range:'
     O                       x1frdt            +001 '  /  /    '
     O                                         +001 '-'
     O                       x1todt            +001 '  /  /    '
     O                                         +010 'Quote type:'
     O                       rqstdTypes        +001
     O                                         +010 'Include all Pended,'
     O                                         +001 'Quoted, Bound (Y/N):'
     O                       @@pqb             +001
     *
     O          e            head           1
     O                                         +005 'Page Break by Agent (Y/N):'
     O                       @@asrt            +001
     O                                         +010 'Agent selection:'
     O                       agtSelection      +001
     O                                         +005 'Output type (P/F/B):'
     O                       @@pfb             +001
     O                                         +005 'Report type (D/S/B):'
     O                       @@dsb             +001
     O                                         +005 'Primary Sort(AA/AN):'
     O                       @@psrt            +001
     O                                         +005 'Type of Quote(FQ/WC):'
     O                       pFQFlag           +001
     *
     O          e   n92      head        1  1
     O                                         +000 'Group:'
     O                       svfnd             +001
     O                       svfnnm            +002
     *
     O          e   n92n95   head           1
     O                                         +000 'Agent:'
     O                       svagt#            +001
     O                       svagnm            +002
     O                                         +004 'Commitment Amount:'
     O                       svcmt         j   +001
     O                                         +004 'Commitment Date:'
     O                       svcmdt            +001 '  /  /    '
     O                                         +004 'Commitment Balance:'
     O                       svbal         j   +001
     *
     O          e   n92n95   head           2
     O                                         +014 'Marketing Rep:'
     O                       svmkrn            +001
     *
     O          e   n92n94n95head           1
     O                                         +000 'EMPLOYER DIV '
     O                                         +043 'FQ/WC'
     O                                         +002 'EFFECTIVE'
     O                                         +004 'TYPE CHG'
     O                                         +004 'ORIG SUB'
     O                                         +004 'NEW BUS '
     O                                         +004 'QTE FAX '
     O                                         +036 'CTRL'
     *
     O          e            head           0
     O                       line              +000
     *
     O          e   n92n94n95head           1
     O                                         +000 '  PRODUCER   '
     O                                         +053 'DATE'
     O                                         +  8 'DATE'
     O                                         +  8 'DATE'
     O                                         +  8 'DATE'
     O                                         +  8 'DATE'
     O                                         +  7 'CARRIER       '
     O                                         +  5 'PREMIUM'
     O                                         +006 'AGT'
     *
     *
     O          e   n93      qtyp        1  2
     O                       q2qtyp            +000
     O                                         +001 '-'
     O                       q2qdes            +001
     O***  Begin Add  ***  11/06/12  **************************************
     O                       strRenew          +005
     O***  End   Add  ***  11/06/12  **************************************
     *
     O          e            det            1
     O                       q2emp#        z   +000
     O                       q2div         z   +001
     O                       q2enam            +001
     O                       q2fqfl            +002
     O              n71      q2efdt            +004 '  /  /    '
     O              n72      q2wkdt            +002 '  /  /    '
     O              n73      q2ordt            +002 '  /  /    '
     O              n74      q2nbdt            +002 '  /  /    '
     O              n75      q2fxdt            +002 '  /  /    '
     O                       q2carr            +002
     O                       q2prem        j   +002
     O                       caflYesNo         +006
     *
     O          e   n78      det            1
     O                       q2prod        z   +002
     O                       q2pnam            +001
     *
     O          e   n76      det            1
     O                       q2com1            +010
     *
     O          e   n77      det            1
     O                       q2com2            +010
     *
     *
     O          e            l1tot       1  1
     O                                         +083 'Count:'
     O                       l1cnt         j   +001
     O                                         +010 '            Total:'
     O                       l1prem        j   +001
     *
     *
     O          e            l2tot       2  2
     O                                         +000 'Totals for Agent:'
     O                       svagt#            +001
     O                       svagnm            +002
     *
     O          e            l2tot          2
     O                                         +010 'Number of accounts:'
     O                       l2cnt         j   +001
     *
     O          e            l2tot          2
     O                                         +010 'Total premium . . :'
     O                       l2prem        j   +001
     *
     O          e            l2tot          2
     O                                         +010 'HIT RATIO . . . . : ('
     O                       l2writ        1   +001
     O                                         +001 '/'
     O                       l2totw        1   +001
     O                                         +001 ') ='
     O                       l2hit         1   +001
     *
     *
     O          e            l3tot       2  2
     O                                         +000 'Totals for Group:'
     O                       svfnd             +001
     O                       svfnnm            +002
     *
     O          e            l3tot          2
     O                                         +010 'Number of accounts:'
     O                       l3cnt         j   +001
     *
     O          e            l3tot          2
     O                                         +010 'Total premium . . :'
     O                       l3prem        j   +001
     *
     O          e            l3tot          2
     O                                         +010 'HIT RATIO . . . . : ('
     O                       l3writ        1   +001
     O                                         +001 '/'
     O                       l3totw        1   +001
     O                                         +001 ') ='
     O                       l3hit         1   +001
     *
     *
     O          e            lrtot       2  2
     O                                         +000 'Totals for Report:'
     *
     O          e            lrtot          2
     O                                         +010 'Number of accounts:'
     O                       lrcnt         j   +001
     *
     O          e            lrtot          2
     O                                         +010 'Total premium . . :'
     O                       lrprem        j   +001
     *
     O          e            lrtot          2
     O                                         +010 'HIT RATIO . . . . : ('
     O                       lrwrit        1   +001
     O                                         +001 '/'
     O                       lrtotw        1   +001
     O                                         +001 ') ='
     O                       lrhit         1   +001
     *
     *
     O          e            norec       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
     *
     *
     *
     Oqsysprt1  e            route1         1 06
      ***** Beg Del ***** 08/26/14 **********************************
     O*******                                     8 'XXXXXXXX'
     O*******                                    20 'XXXXXX'
     O*******                                    34 'XX      XX'
     O*******                                    45 'XXXXXXXXXX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XXX     XX'
     O*******                                    77 'XXXXXX'
     O*******   e            route1         1
     O*******                                     9 'XXXXXXXXX'
     O*******                                    21 'XXXXXXXX'
     O*******                                    34 'XX      XX'
     O*******                                    45 'XXXXXXXXXX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XXXX    XX'
     O*******                                    78 'XXXXXXXX'
     O*******   e            route1         1
     O*******                                    10 'XX      XX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XXXX    XX'
     O*******                                    79 'XX      XX'
     O*******   e            route1         1
     O*******                                    10 'XX      XX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX XX   XX'
     O*******                                    71 'XX'
     O*******   e            route1         1
     O*******                                     9 'XXXXXXXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX XX   XX'
     O*******                                    71 'XX'
     O*******   e            route1         1
     O*******                                     8 'XXXXXXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX  XX  XX'
     O*******                                    71 'XX'
     O*******   e            route1         1
     O*******                                     4 'XXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX  XX  XX'
     O*******                                    79 'XX    XXXX'
     O*******   e            route1         1
     O*******                                     5 'XXXXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX   XX XX'
     O*******                                    79 'XX    XXXX'
     O*******   e            route1         1
     O*******                                     6 'XX XXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX   XX XX'
     O*******                                    79 'XX      XX'
     O*******   e            route1         1
     O*******                                     7 'XX  XXX'
     O*******                                    22 'XX      XX'
     O*******                                    34 'XX      XX'
     O*******                                    41 'XX'
     O*******                                    52 'XX'
     O*******                                    67 'XX    XXXX'
     O*******                                    79 'XX      XX'
     O*******   e            route1         1
     O*******                                     8 'XX   XXX'
     O*******                                    21 'XXXXXXXX'
     O*******                                    33 'XXXXXXXX'
     O*******                                    41 'XX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XX    XXXX'
     O*******                                    78 'XXXXXXXX'
     O*******   e            route1         3
     O*******                                     9 'XX    XXX'
     O*******                                    20 'XXXXXX'
     O*******                                    32 'XXXXXX'
     O*******                                    41 'XX'
     O*******                                    56 'XXXXXXXXXX'
     O*******                                    67 'XX     XXX'
     O*******                                    77 'XXXXXX'
     *******
     O*******   e            route1         1
     O*******                                    15 'ROUTE TO:'
     O*******                ordis1              56
     *******
     O*******   e            route1         1
     O*******                ordis2              56
     *******
     O*******   e            route1         1
     O*******                ordis3              56
     *******
     O*******   e            route1         1
     O*******                ordis4              56
      ***** End Del ***** 08/26/14 **********************************
      ***** Beg Add ***** 08/05/15 **********************************
     O          e            route1         1
     O                                           15 'ROUTE TO:'
     O                       ordis1              56
     *******
     O          e            route1         1
     O                       ordis2              56
     *******
     O          e            route1         1
     O                       ordis3              56
     *******
     O          e            route1         1
     O                       ordis4              56
      ***** End Add ***** 08/05/15 **********************************
     *
     O          e            route1         2
     O                                           19 'REPORT TITLE:'
     O                       x1titl              80
     *
      ***** Beg Del ***** 08/26/14 **********************************
     O*******   e            route1         2
     O*******                                    19 'PROGRAM NAME:'
     O*******                orpnam              30
     O*******                                    36 'FILE:'
     O*******                orfil#              38
     O*******                                    46 'COPIES:'
     O*******                orcopy        z     50
     O*******                                    56 'HOLD:'
     O*******                orhold              61
     *******
     O*******   e            route1         2
     O*******                                    19 'OUTPUT QUEUE:'
     O*******                oroutq              30
     O*******                                    37 'FORMS:'
     O*******                orform              48
     O*******                                    56 'SAVE:'
     O*******                orsave              61
      ***** End Del ***** 08/26/14 **********************************
     *
     O          e            route1      2  2
     O                                           24 'REPORT SELECTIONS:'
     *
     O          e            route1         2
      ***** Beg chg ***** 01/09/17 ***********************************
     O***                                        35 'Group range . . . . . :'
     O***                    x1ffnd            +001
     O***                                      +001 '-'
     O***                    x1tfnd            +001
     O
     O              N13                          35 'Group range . . . . . :'
     O              N13      x1ffnd            +001
     O              N13                        +001 '-'
     O              N13      x1tfnd            +001
     O
     O               13                          35 'Groups. . . . . . . . :'
     O               13      strSelGrp         +  1
      ***** End chg ***** 01/09/17 ***********************************
     *
     O          e            route1         2
     O                                           35 'Type of date range    :'
     O                       x1frdt            +001 '  /  /    '
     O                                         +001 '-'
     O                       x1todt            +001 '  /  /    '
     *
     O          e            route1         2
     O                                           35 'Type of date  . . . . :'
     O                       @@dtyp            +001
     O                       x1date            +002
     *
     O          e            route1         2
     O                                           35 'Quote type  . . . . . :'
     O                       rqstdTypes        +001
     *
     O          e            route1         1
     O                                           35 'Include all Pended,    '
     O          e            route1         2
     O                                           35 '  Quoted, Bound  (Y/N):'
     O                       @@pqb             +001
     *
     O          e            route1         2
     O                                           35 'Page Break by Agent (Y/N):'
     O                       @@asrt            +001
     O          e            route1         2
     O                                           35 'Primary sort   (AA/AN):'
     O                       @@psrt            +001
     O                       pSortDesc         +001
     *
     O          e            route1         2
     O                                           35 'Agent selection . . . :'
     O                       agtSelection      +001
     O          e            route1         2
     O                                           35 'Type of Quotes (FQ/WC):'
     O                       pFQFlag           +001
     O                       pFQFlagDesc       +001
     *
     O          e            route1         2
     O                                           35 'Output type    (P/F/B):'
     O                       @@pfb             +001
     *
     O          e            route1         2
     O                                           35 'Report type    (D/S/B):'
     O                       @@dsb             +001
     *
     O          e            route1         1 59
     O                       #brnd1
     O          e            route1         1
     O                       #brnd2
     *
     *
     O          e            heads          1 03
     O                       dstime               8 '  :  :  '
     O                       coname             119
     O                       dsdate        y    198
     *
     O          e            heads          1
     O                       q1pgm               10
     O                       x1titl             129
     O                                          192 'PAGE:'
     O                       page1         z    198
     *
     O          e            heads       1  1
      ***** Beg chg ***** 01/09/17 ***********************************
     O***                                      +005 'Group range:'
     O***                    x1ffnd            +001
     O***                                      +001 '-'
     O***                    x1tfnd            +001
     O
     O              N13                        +005 'Group range:'
     O              N13      x1ffnd            +001
     O              N13                        +001 '-'
     O              N13      x1tfnd            +001
     O
     O               13                          12 'Groups:'
     O               13      strSelGrp         +  1
      ***** End chg ***** 01/09/17 ***********************************
     O                       x1date            +001
     O                                         +001 'range:'
     O                       x1frdt            +001 '  /  /    '
     O                                         +001 '-'
     O                       x1todt            +001 '  /  /    '
     O                                         +010 'Quote type:'
     O                       rqstdTypes        +001
     O                                         +010 'Include all Pended,'
     O                                         +001 'Quoted, Bound (Y/N):'
     O                       @@pqb             +001
     *
     O          e            heads          1
     O                                         +005 'Page Break by Agent (Y/N):'
     O                       @@asrt            +001
     O                                         +010 'Agent selection:'
     O                       agtSelection      +001
     O                                         +005 'Output type (P/F/B):'
     O                       @@pfb             +001
     O                                         +005 'Report type (D/S/B):'
     O                       @@dsb             +001
     O                                         +005 'Primary Sort(AA/AN):'
     O                       @@psrt            +001
     O                                         +005 'Type of Quote(FQ/WC):'
     O                       pFQFlag           +001
     *
     O          e   n92      heads       1  1
     O                                         +000 'Group:'
     O                       svfnd             +001
     O                       svfnnm            +002
     *
     O          e   n92n95   heads          2
     O                                         +000 'Agent:'
     O                       svagt#            +001
     O                       svagnm            +002
     O                                         +004 'Commitment Amount:'
     O                       svcmt         j   +001
     O                                         +004 'Commitment Date:'
     O                       svcmdt            +001 '  /  /    '
     O                                         +004 'Commitment Balance:'
     O                       svbal         j   +001
     *
     *
     O          e            heads          0
     O                       line              +000
     *
     O          e   n92      heads          2
     *
     *
     O          e            l2tots      2  2
     O                                         +000 'Totals for Agent  . . :'
     O                       svagt#            +001
     O                       svagnm            +002
     *
     O          e            l2tots         1
     O                                         +010 'Agent effective date  :'
     O                       svaedt            +001 '  /  /    '
     *
     O          e            l2tots         1
     O                                         +010 'Number active accounts:'
     O                       l2actv        j   +001
     *
     O          e            l2tots         1
     O                                         +010 'Total book of business:'
     O                       l2prem        j   +001
     *
     O          e            l2tots         1
     O                                         +010 '# submissions written :'
     O                       l2#sub        j   +001
     *
     O          e            l2tots         1
     O                                         +010 '$ submissions written :'
     O                       l2$sub        j   +001
     *
     O          e            l2tots         1
     O                                         +010 'HIT RATIO  . . . . . .:'
     O                                         +001 '('
     O                       l2writ        1   +001
     O                                         +001 '/'
     O                       l2totw        1   +001
     O                                         +001 ') ='
     O                       l2hit         1   +001
     *
     O          e            l2tots         1
     O                                         +010 'Marketing rep  . . . .:'
     O                       svmkrn            +001
     *
     *
     O          e            l3tots      2  2
     O                                         +000 'Totals for Group:'
     O                       svfnd             +001
     O                       svfnnm            +002
     *
     O          e            l3tots         1
     O                                         +010 'Number active accounts:'
     O                       l3actv        j   +001
     *
     O          e            l3tots         1
     O                                         +010 'Total book of business:'
     O                       l3prem        j   +001
     *
     O          e            l3tots         1
     O                                         +010 '# submissions written :'
     O                       l3#sub        j   +001
     *
     O          e            l3tots         1
     O                                         +010 '$ submissions written :'
     O                       l3$sub        j   +001
     *
     O          e            l3tots         1
     O                                         +010 'HIT RATIO  . . . . . .:'
     O                                         +001 '('
     O                       l3writ        1   +001
     O                                         +001 '/'
     O                       l3totw        1   +001
     O                                         +001 ') ='
     O                       l3hit         1   +001
     *
     *
     O          e            lrtots      2  2
     O                                         +000 'Totals for Report:'
     *
     O          e            lrtots         1
     O                                         +010 'Number active accounts:'
     O                       lractv        j   +001
     *
     O          e            lrtots         1
     O                                         +010 'Total book of business:'
     O                       lrprem        j   +001
     *
     O          e            lrtots         1
     O                                         +010 '# submissions written :'
     O                       lr#sub        j   +001
     *
     O          e            lrtots         1
     O                                         +010 '$ submissions written :'
     O                       lr$sub        j   +001
     *
     O          e            lrtots         1
     O                                         +010 'HIT RATIO  . . . . . .:'
     O                                         +001 '('
     O                       lrwrit        1   +001
     O                                         +001 '/'
     O                       lrtotw        1   +001
     O                                         +001 ') ='
     O                       lrhit         1   +001
     *
     *
     O          e            norecs      2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
     *
     *
