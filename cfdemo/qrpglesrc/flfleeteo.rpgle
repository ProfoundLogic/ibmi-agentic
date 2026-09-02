**free

/////////////////////
// Fleet 360 - installed base by customer (EJS)          GJA-913
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flfleeteo workstn sfile(machsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pr flfleet1eo extpgm;
  serial char(8) const;
end-pr;

dcl-pr flparteo extpgm;
  serial char(8) const;
end-pr;

dcl-pr flcusteo extpgm;
  search char(40) const;
  custno like(fl_cust_t.custno);
end-pr;

dcl-ds summary likeds(fl_sum_t);
dcl-ds machines likeds(fl_mach_t) dim(500);
dcl-s numMach int(10) inz(0);
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
dcl-s wantCust like(fl_cust_t.custno) inz(0);
dcl-s pickedCust like(fl_cust_t.custno);

// open on a populated fleet rather than an empty form
if wantCust = 0;
  err = fl_defaultCustomer(wantCust);
  if err <> '';
    msg = err;
  endif;
endif;

dow action <> 'EXIT';

  if wantCust <> 0;
    exsr loadCustomer;
  endif;

  if numMach = 0;
    soptdesc = '';
  else;
    soptdesc = 'Double-click=detail   7=parts';
  endif;

  if numMach > 0;
    exsr loadSFL;
  else;
    exsr clearSFL;
  endif;

  scustno = wantCust;
  action = '';
  exfmt fleetctl;

  select;
    when action = 'EXIT';
      leave;
    when action = 'LOOKUP';
      // Open unfiltered - seeding it with the customer already on screen
      // would just return the account the user is moving away from.
      pickedCust = 0;
      flcusteo(*blanks : pickedCust);
      if pickedCust <> 0;
        wantCust = pickedCust;
      endif;
      iter;
  endsl;

  // customer changed on screen
  if scustno <> wantCust;
    wantCust = scustno;
    iter;
  endif;

  // subfile selection
  if numMach > 0;
    selrrn = 0;
    sopt = '';
    readc machsfl;
    dow not %eof(flfleeteo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update machsfl;
      readc machsfl;
    enddo;
    if selrrn <> 0;
      chain selrrn machsfl;
      select;
        when %trim(sopt) = '5';
          flfleet1eo(sserial);
        when %trim(sopt) = '7';
          flparteo(sserial);
        other;
          msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5 or 7.';
      endsl;
    endif;
  endif;

enddo;

*inlr = *on;

begsr loadCustomer;
  msg = '';
  clear summary;
  numMach = 0;
  err = fl_getFleetSummary(wantCust : summary);
  if err <> '';
    msg = err;
    clear summary;
    leavesr;
  endif;
  scustname = summary.custname;
  sregion   = summary.region;
  smachcnt  = summary.machines;
  ssitecnt  = summary.sites;
  sspendcur = summary.spendcur;
  sspendpri = summary.spendpri;
  strendpct = summary.trendpct;
  smodcand  = summary.modcand;
  sinsight  = summary.insight;
  err = fl_listMachines(wantCust : machines : %elem(machines) : numMach);
  if err <> '';
    msg = err;
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write fleetctl;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numMach;
    sopt      = '';
    sserial   = machines(i).ser;
    smodel    = machines(i).model;
    smodeldsc = machines(i).modeldsc;
    ssite     = machines(i).sitename;
    sbuilt    = machines(i).built;
    shours    = machines(i).hours;
    sstatusd  = machines(i).statusd;
    slastsvc  = machines(i).lastsvc;
    smodflag  = machines(i).modflag;
    rrn += 1;
    write machsfl;
  endfor;
endsr;
