**free

/////////////////////
// Work with Customers: Edit Credit Limit (EJS Screen Mode)
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('CUST');

dcl-f wrkcust2eo workstn handler('PROFOUNDUI(HANDLER)');

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
  msg = error;
  action = 'EXIT';
  exfmt custdetail;
  *inlr = *on;
  return;
endif;

exsr loadFields;

dow action <> 'EXIT';

  action = '';
  exfmt custdetail;

  if action = 'EXIT';
    leave;
  endif;

  msg = '';

  if action = 'SAVE';
    // Validate credit limit.
    if slimit < 0;
      msg = 'Credit limit cannot be negative.';
      iter;
    endif;

    // Save credit limit.
    error = cust_update_climit(custno : slimit);
    if error <> '';
      msg = error;
      iter;
    endif;

    saved = *on;
    leave;
  endif;

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
