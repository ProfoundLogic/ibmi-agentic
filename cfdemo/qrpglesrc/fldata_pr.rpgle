
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
