
      // ------------------------------------------------------------------
      // FLDATA - J.H. Fletcher demo data access
      //
      // Screen-facing contract (see GJA-911):
      //   - dates are formatted here and passed out as char(10) YYYY-MM-DD
      //   - coded values are accompanied by their description text, so every
      //     screen labels the same code identically
      //   - numerics stay numeric; presentation is the screen's job
      // ------------------------------------------------------------------

       dcl-ds fl_cust_t qualified template;
         custno   packed(6:0);
         name     char(40);
         city     char(30);
         state    char(2);
         region   char(10);
         mgr      char(30);
         status   char(1);
       end-ds;

       dcl-ds fl_mach_t qualified template;
         ser      char(8);
         model    char(10);
         modeldsc char(40);
         custno   packed(6:0);
         custname char(40);
         siteno   packed(6:0);
         sitename char(40);
         sitetype char(1);
         built    char(10);
         shipped  char(10);
         hours    packed(9:0);
         status   char(1);
         statusd  char(10);
         lastsvc  char(10);
         lastmod  char(10);
         modtype  char(1);
         modtyped char(10);
         modflag  char(12);
       end-ds;

       dcl-ds fl_optn_t qualified template;
         code     char(10);
         descr    char(40);
       end-ds;

       dcl-ds fl_part_t qualified template;
         part     char(15);
         descr    char(40);
         assy     char(20);
         qty      packed(5:0);
         stock    packed(7:0);
         price    packed(11:2);
         lead     packed(3:0);
         status   char(1);
         statusd  char(12);
         superby  char(15);
         optn     char(10);
         dupes    int(10);
       end-ds;

       dcl-ds fl_svc_t qualified template;
         ser      char(8);
         svcdate  char(10);
         svctype  char(1);
         svctyped char(12);
         tech     char(30);
         descr    char(60);
         downhrs  packed(5:1);
         cost     packed(11:2);
       end-ds;

       dcl-ds fl_sum_t qualified template;
         custno   packed(6:0);
         custname char(40);
         region   char(10);
         machines int(10);
         sites    int(10);
         spendcur packed(13:2);
         spendpri packed(13:2);
         trendpct packed(5:1);
         modcand  int(10);
         insight  char(240);
       end-ds;

       dcl-pr fl_getCustomer varchar(80);
         custno like(fl_cust_t.custno) const;
         customer likeds(fl_cust_t);
         found ind;
       end-pr;

       dcl-pr fl_getMachine varchar(80);
         serial char(8) const;
         machine likeds(fl_mach_t);
         found ind;
       end-pr;

       dcl-pr fl_listMachines varchar(80);
         custno like(fl_cust_t.custno) const;
         machines likeds(fl_mach_t) dim(500);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_listOptions varchar(80);
         serial char(8) const;
         options_ likeds(fl_optn_t) dim(50);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_listPartsForMachine varchar(80);
         serial char(8) const;
         search varchar(40) const;
         parts likeds(fl_part_t) dim(500);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_findDuplicates varchar(80);
         part char(15) const;
         parts likeds(fl_part_t) dim(50);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_listServiceEvents varchar(80);
         serial char(8) const;
         events likeds(fl_svc_t) dim(200);
         limit int(10) const;
         returned int(10);
       end-pr;

      // ---- duplicate review (A7) -------------------------------------
      // The detector writes clusters to FLDUPC/FLDUPM; the review utility
      // reads them, shows the blast radius, and applies a decision.
       dcl-ds fl_dupc_t qualified template;
         id       packed(9:0);
         run      packed(9:0);
         sig      char(60);
         cnt      packed(3:0);
         conf     packed(3:0);
         reason   char(150);
         status   char(1);
         statusd  char(14);
         survivor char(15);
         user     char(18);
         revdate  char(10);
         note     char(120);
         val      packed(13:2);
         bom      packed(5:0);
         topdesc  char(40);
       end-ds;

       dcl-ds fl_dupm_t qualified template;
         part     char(15);
         descr    char(40);
         created  char(10);
         status   char(1);
         statusd  char(12);
         super    char(15);
         stock    packed(7:0);
         price    packed(11:2);
         val      packed(11:2);
         bom      packed(5:0);
         ord      packed(5:0);
         role     char(1);
         roled    char(12);
         why      char(80);
       end-ds;

       dcl-ds fl_dupsc_t qualified template;
         parts    int(10);
         clusters int(10);
         involved int(10);
         val      packed(13:2);
         bomlines int(10);
         c80      int(10);
         cnew     int(10);
         cmerged  int(10);
         crej     int(10);
         cdef     int(10);
       end-ds;

       dcl-pr fl_scanDuplicates varchar(80);
         run packed(9:0);
         created int(10);
         skipped int(10);
       end-pr;

       dcl-pr fl_dupScorecard varchar(80);
         sc likeds(fl_dupsc_t);
       end-pr;

       dcl-pr fl_listClusters varchar(80);
         statusFilter char(1) const;
         clusters likeds(fl_dupc_t) dim(500);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_getCluster varchar(80);
         id packed(9:0) const;
         hdr likeds(fl_dupc_t);
         found ind;
       end-pr;

       dcl-pr fl_listClusterMembers varchar(80);
         id packed(9:0) const;
         members likeds(fl_dupm_t) dim(50);
         limit int(10) const;
         returned int(10);
       end-pr;

      // mergeList names ONLY the members being folded into the survivor.
      // Anything unmarked is left alone and recorded as excluded - a cluster
      // often contains parts that are genuinely separate items.
       dcl-pr fl_resolveCluster varchar(80);
         id packed(9:0) const;
         survivor char(15) const;
         mergeList char(15) dim(50) const options(*varsize);
         mergeCount int(10) const;
         decision char(1) const;
         note char(120) const;
         user char(18) const;
       end-pr;

      // Customer lookup row: carries fleet size and spend so the picker is
      // useful for choosing, not just for finding.
       dcl-ds fl_clkp_t qualified template;
         custno   packed(6:0);
         name     char(40);
         city     char(30);
         state    char(2);
         region   char(10);
         mgr      char(30);
         machines int(10);
         sites    int(10);
         spendcur packed(13:2);
         spendpri packed(13:2);
         trendpct packed(5:1);
       end-ds;

       dcl-pr fl_findCustomers varchar(80);
         search varchar(40) const;
         customers likeds(fl_clkp_t) dim(200);
         limit int(10) const;
         returned int(10);
       end-pr;

      // Free-text machine lookup: matches on serial fragment, customer name,
      // site name or model, so a rep who only has the caller's company name
      // can still find the machine.
       dcl-pr fl_findMachines varchar(80);
         search varchar(40) const;
         machines likeds(fl_mach_t) dim(500);
         limit int(10) const;
         returned int(10);
       end-pr;

      // Demo entry points: both list screens open populated rather than empty,
      // and they pick the most interesting record rather than a hardcoded one.
       dcl-pr fl_defaultCustomer varchar(80);
         custno like(fl_cust_t.custno);
       end-pr;

       dcl-pr fl_defaultMachine varchar(80);
         serial char(8);
       end-pr;

       dcl-pr fl_getFleetSummary varchar(80);
         custno like(fl_cust_t.custno) const;
         summary likeds(fl_sum_t);
       end-pr;

      // ---- A4 production schedule board ------------------------------
      // The board answers three questions: what is on the floor, when will
      // it actually ship, and how much of that answer can be trusted.
      //
      // Projected ship is COMPUTED here rather than stored, because a stored
      // projection is just another number nobody maintains. The walk is:
      // remaining standard hours at each work centre, divided by that
      // centre's capacity share (its daily hours split across the orders
      // queued on it), accumulated over working days, then pushed out by any
      // open material shortage whose expected date lands later.

       dcl-ds fl_wctr_t qualified template;
         code     char(6);
         descr    char(40);
         seq      packed(3:0);
         cap      packed(5:1);
         crew     packed(3:0);
         shifts   packed(1:0);
         status   char(1);
         statusd  char(10);
         wip      int(10);
         queued   int(10);
         loadhrs  packed(7:1);
         loaddays packed(5:1);
         loadpct  packed(5:0);
         // Queued work in working weeks. Percent-of-a-month was unreadable
         // once the real loads came out between 150% and 406%: every bar
         // pinned at full width and told the eye nothing.
         weeks    packed(5:1);
         late     int(10);
       end-ds;

       dcl-ds fl_wo_t qualified template;
         wo       packed(8:0);
         serial   char(8);
         model    char(10);
         modeldsc char(40);
         custno   packed(6:0);
         custname char(40);
         siteno   packed(6:0);
         sitename char(40);
         otype    char(1);
         otyped   char(16);
         status   char(1);
         statusd  char(12);
         prio     char(1);
         priod    char(8);
         opened   char(10);
         promised char(10);
         started  char(10);
         compl    char(10);
         wctr     char(6);
         wctrd    char(40);
         pctrptd  packed(3:0);
         pctroute packed(3:0);
         stdhrs   packed(7:1);
         acthrs   packed(7:1);
         remhrs   packed(7:1);
         ops      int(10);
         opsdone  int(10);
         projectd char(10);
         slipdays packed(5:0);
         promdays packed(5:0);
         projdays packed(5:0);
         shorts   int(10);
         shortlat int(10);
         value    packed(11:2);
         descr    char(40);
         note     char(60);
         flag     char(10);
         dqcount  int(10);
         dqhigh   int(10);
       end-ds;

       dcl-ds fl_op_t qualified template;
         wo       packed(8:0);
         seq      packed(3:0);
         wctr     char(6);
         wctrd    char(40);
         descr    char(40);
         stdhrs   packed(5:1);
         acthrs   packed(5:1);
         varpct   packed(5:0);
         status   char(1);
         statusd  char(12);
         started  char(10);
         compl    char(10);
         shorts   int(10);
         shortpar char(15);
         shortqty packed(5:0);
         shortdue char(10);
         shortpo  char(8);
         shortven char(30);
         shortdsc char(60);
       end-ds;

       dcl-ds fl_shop_t qualified template;
         orders   int(10);
         wip      int(10);
         late     int(10);
         risk     int(10);
         ontrack  int(10);
         hold     int(10);
         shorts   int(10);
         shortval packed(13:2);
         value    packed(13:2);
         remhrs   packed(9:1);
         capday   packed(7:1);
         weeks    packed(5:1);
         dqfind   int(10);
         dqorders int(10);
         dqhigh   int(10);
         dqtrust  int(10);
         dqpct    packed(3:0);
       end-ds;

      // One data-confidence finding. Severity is the screen's sort key and
      // the trust calculation only counts H, because a missing standard-hour
      // value degrades a projection while an impossible date invalidates it.
       dcl-ds fl_dq_t qualified template;
         sev      char(1);
         sevd     char(8);
         cat      char(26);
         wo       packed(8:0);
         subject  char(22);
         finding  char(110);
         impact   char(90);
       end-ds;

       dcl-pr fl_listWorkCentres varchar(80);
         lanes likeds(fl_wctr_t) dim(50);
         limit int(10) const;
         returned int(10);
       end-pr;

      // view: A=all live  L=past promised  R=projected late  S=short
      //       H=on hold   C=complete       *=everything
      // wctr blank means every work centre.
       dcl-pr fl_listWorkOrders varchar(80);
         view char(1) const;
         wctr char(6) const;
         orders likeds(fl_wo_t) dim(300);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_getWorkOrder varchar(80);
         wo packed(8:0) const;
         order likeds(fl_wo_t);
         found ind;
       end-pr;

       dcl-pr fl_listOperations varchar(80);
         wo packed(8:0) const;
         ops likeds(fl_op_t) dim(100);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_shopSummary varchar(80);
         summary likeds(fl_shop_t);
       end-pr;

      // The data-confidence scan. Every finding is produced by querying the
      // data, never from a list of known problems, so it keeps working when
      // the data changes.
       dcl-pr fl_scanSchedule varchar(80);
         findings likeds(fl_dq_t) dim(400);
         limit int(10) const;
         returned int(10);
       end-pr;

       dcl-pr fl_defaultWorkOrder varchar(80);
         wo packed(8:0);
       end-pr;

      // Quote a value as a JSON string, quotes included. The board, the work
      // order detail and the confidence screen all deliver a secondary list
      // as one JSON string field, because a second subfile in the same RDF
      // format compiles to malformed DDS. Shared rather than written three
      // times, slightly differently.
       dcl-pr fl_jsonStr varchar(300);
         v varchar(250) const;
       end-pr;

      // Repair a number on its way into a JSON payload. %CHAR on a packed
      // value drops the leading zero - 0.0 comes out as ".0" and -0.5 as
      // "-.5" - and neither is valid JSON. JSON.parse then throws and the
      // entire payload renders as nothing, with no error anywhere. Pass the
      // %CHAR output through here rather than taking the number, so each
      // caller keeps its own precision.
       dcl-pr fl_jsonNum varchar(24);
         numText varchar(24) const;
       end-pr;
