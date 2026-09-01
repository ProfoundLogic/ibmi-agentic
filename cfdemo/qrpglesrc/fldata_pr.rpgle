
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
