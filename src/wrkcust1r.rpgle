**free

/////////////////////
// Work with Customers: Customer Detail
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('CUST');

dcl-f wrkcust1d workstn;

/copy custr_pr.rpgle

dcl-pi *n;
  custno like(cust_rec.custno) const;
  mode char(1) const options(*nopass);
end-pi;

dcl-ds customer likeds(cust_rec);
dcl-s customerFound ind;
dcl-s error varchar(210);
dcl-s editMode ind;

if %parms() >= 2 and mode = 'E';
  editMode = *on;
else;
  editMode = *off;
endif;

// Initial load
error = cust_get(custno : customer : customerFound);
if not customerFound;
  if error = '';
    error = 'Customer ' + %char(custno) + ' not found.';
  endif;
  exsr endError;
endif;

exsr populateScreenFields;

dow not *in03;
  if editMode;
    *in30 = *on;
    sfkeys = 'F3=Cancel  Enter=Save';
  else;
    *in30 = *off;
    sfkeys = 'F3=Exit';
  endif;

  exfmt custdetail;

  if *in03;
    leave;
  endif;

  if editMode;
    customer.cname = sname;
    customer.ctype = stype;
    customer.cstatus = sstatus;
    customer.climit = slimit;
    customer.caddr1 = saddr1;
    customer.caddr2 = saddr2;
    customer.ccity = scity;
    customer.cstate = sstate;
    customer.czip = szip;
    customer.cemail = semail;
    customer.cphone = sphone;

    error = cust_update(customer);
    if error = '';
      leave;
    else;
      exsr showError;
    endif;
  else;
    // In display mode, Enter refreshes the data from the DB
    error = cust_get(custno : customer : customerFound);
    if customerFound;
      exsr populateScreenFields;
    endif;
  endif;
enddo;

*inlr = *on;

begsr populateScreenFields;
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

// Displays error window but does not end the program.
begsr showError;
  serrattr = x'A0';
  serrwinmsg = error;
  exfmt errorwin;
endsr;

