**free

/////////////////////
// Machine detail and service history (EJS)              GJA-913
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flfleet1eo workstn sfile(svcsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_serial char(8) const;
end-pi;

dcl-ds machine likeds(fl_mach_t);
dcl-ds events likeds(fl_svc_t) dim(200);
dcl-ds opts likeds(fl_optn_t) dim(50);
dcl-ds cust likeds(fl_cust_t);
dcl-s numOpts int(10) inz(0);
dcl-s optList varchar(200);
dcl-s found2 ind;
dcl-s i2 int(10);
dcl-s numEvents int(10) inz(0);
dcl-s found ind;
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);

msg = '';
err = fl_getMachine(p_serial : machine : found);
if err <> '';
  msg = err;
elseif not found;
  msg = 'Machine ' + %trim(p_serial) + ' not found.';
else;
  sserial   = machine.ser;
  smodel    = machine.model;
  smodeldsc = machine.modeldsc;
  scustname = machine.custname;
  ssitename = machine.sitename;
  sbuilt    = machine.built;
  sshipped  = machine.shipped;
  shours    = machine.hours;
  sstatusd  = machine.statusd;
  slastsvc  = machine.lastsvc;
  slastmod  = machine.lastmod;
  smodtyped = machine.modtyped;
  smodflag  = machine.modflag;
  // options fitted - what makes this machine different from its model
  optList = '';
  if fl_listOptions(p_serial : opts : %elem(opts) : numOpts) = '';
    for i2 = 1 to numOpts;
      if optList <> '';
        optList += ', ';
      endif;
      optList += %trimr(opts(i2).descr);
    endfor;
  endif;
  soptions = optList;

  if fl_getCustomer(machine.custno : cust : found2) = '' and found2;
    sregion = cust.region;
  endif;

  err = fl_listServiceEvents(p_serial : events : %elem(events) : numEvents);
  if err <> '';
    msg = err;
  endif;
endif;

exsr loadSFL;

action = '';
exfmt machdetail;

*inlr = *on;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write machdetail;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numEvents;
    ssvcdate  = events(i).svcdate;
    ssvctyped = events(i).svctyped;
    stech     = events(i).tech;
    sdescr    = events(i).descr;
    sdownhrs  = events(i).downhrs;
    scost     = events(i).cost;
    rrn += 1;
    write svcsfl;
  endfor;
endsr;
