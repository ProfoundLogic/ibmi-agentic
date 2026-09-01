**free

/////////////////////
// Customer lookup - pick a customer to show their fleet        GJA-913
//
// Called from Fleet 360. Returns the selected customer number, or zero if
// the user cancels.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flcusteo workstn sfile(clkpsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_search char(40) const;
  p_custno like(fl_cust_t.custno);
end-pi;

dcl-ds customers likeds(fl_clkp_t) dim(200);
dcl-s numCust int(10) inz(0);
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
dcl-s wantSearch like(ssearch);

p_custno = 0;
wantSearch = p_search;

dow action <> 'EXIT';

  exsr findCustomers;

  if numCust > 0;
    soptdesc = 'Double-click a row, or type 5';
    exsr loadSFL;
  else;
    soptdesc = '';
    exsr clearSFL;
  endif;

  ssearch = wantSearch;
  scount = numCust;
  action = '';
  exfmt custlkup;

  if action = 'EXIT';
    leave;
  endif;

  if ssearch <> wantSearch;
    wantSearch = ssearch;
    iter;
  endif;

  if numCust > 0;
    selrrn = 0;
    sopt = '';
    readc clkpsfl;
    dow not %eof(flcusteo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update clkpsfl;
      readc clkpsfl;
    enddo;
    if selrrn <> 0;
      chain selrrn clkpsfl;
      if %trim(sopt) = '5';
        p_custno = scustno;
        leave;
      else;
        msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5.';
      endif;
    endif;
  endif;

enddo;

*inlr = *on;

begsr findCustomers;
  msg = '';
  numCust = 0;
  err = fl_findCustomers(%trim(wantSearch) : customers : %elem(customers) : numCust);
  if err <> '';
    msg = err;
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write custlkup;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numCust;
    sopt      = '';
    scustno   = customers(i).custno;
    scustname = customers(i).name;
    scity     = customers(i).city;
    sstate    = customers(i).state;
    sregion   = customers(i).region;
    smgr      = customers(i).mgr;
    smachcnt  = customers(i).machines;
    ssitecnt  = customers(i).sites;
    sspend    = customers(i).spendcur;
    strend    = customers(i).trendpct;
    rrn += 1;
    write clkpsfl;
  endfor;
endsr;
