**free

/////////////////////
// Work with Customers: Edit Credit Limit
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('CUST');

dcl-f wrkcust2do workstn handler('PROFOUNDUI(HANDLER)');

/copy custr_pr.rpgle

dcl-pi *n;
  custno like(cust_rec.custno) const;
end-pi;

dcl-ds customer likeds(cust_rec);
dcl-s customerFound ind;
dcl-s error varchar(210);
dcl-s saved ind inz(*off);

// Load customer record.
error = cust_get(custno : customer : customerFound);
if not customerFound;
  if error = '';
    error = 'Customer ' + %char(custno) + ' not found.';
  endif;
  exsr endError;
endif;

exsr loadFields;

dow not *in03;

  sfkeys = 'F3=Cancel  Enter=Save';
  exfmt custdetail;

  if *in03;
    leave;
  endif;

  // Validate credit limit.
  if slimit < 0;
    sfkeys = 'Credit limit cannot be negative.';
    exfmt custdetail;
    if *in03;
      leave;
    endif;
    iter;
  endif;

  // Save credit limit.
  error = cust_update_climit(custno : slimit);
  if error <> '';
    sfkeys = error;
    exfmt custdetail;
    if *in03;
      leave;
    endif;
    iter;
  endif;

  saved = *on;
  leave;

enddo;

*inlr = *on;

begsr loadFields;
  scustno = customer.custno;
  sname = customer.cname;
  stype = customer.ctype;
  sstatus = customer.cstatus;
  slimit = customer.climit;
  sbalance = customer.cbalance;
  slastord = %char(%date(customer.clastord) : *usa);
  screated = %char(%date(customer.ccreated) : *usa);
  saddr1 = customer.caddr1;
  saddr2 = customer.caddr2;
  scity = customer.ccity;
  sstate = customer.cstate;
  szip = customer.czip;
  semail = customer.cemail;
  sphone = customer.cphone;
endsr;

// Displays error window and ends the program.
begsr endError;
  serrattr = x'A0';
  serrwinmsg = error;
  exfmt errorwin;
  *inlr = *on;
  return;
endsr;
