**free

/////////////////////
// Work with Customers: Customer Detail
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('CUST');

dcl-f wrkcust1do workstn handler('PROFOUNDUI(HANDLER)');

/copy custr_pr.rpgle

dcl-pi *n;
  custno like(cust_rec.custno) const;
  p_mode char(1) const options(*nopass);
end-pi;

dcl-ds customer likeds(cust_rec);
dcl-s customerFound ind;
dcl-s error varchar(210);
dcl-s mode char(1);

if %parms() >= 2;
  mode = p_mode;
else;
  mode = 'D';
endif;

dow not *in03;
  exsr showScreen;
enddo;

*inlr = *on;

// Loads customer record and displays screen.
begsr showScreen;

  error = cust_get(custno : customer : customerFound);
  if customerFound;
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

    if mode = 'E';
      *in30 = *on; // Enable editing.
      sfkeys = 'F3=Exit  Enter=Save';
    else;
      *in30 = *off; // Protect fields from editing.
      sfkeys = 'F3=Exit';
    endif;
    exfmt custdetail;

    // If in edit mode and user pressed Enter (not F3), update customer.
    if mode = 'E' and not *in03;
      exsr updateCustomer;
    endif;
  else;
    if error = '';
      error = 'Customer ' + %char(custno) + ' not found.';
    endif;
    exsr endError;
  endif;

endsr;

// Updates customer record from screen fields.
begsr updateCustomer;
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
  if error <> '';
    exsr endError;
  else;
    *inlr = *on;
    return;
  endif;
endsr;

// Displays error window and ends the program.
begsr endError;
  serrattr = x'A0';
  serrwinmsg = error;
  exfmt errorwin;
  *inlr = *on;
  return;
endsr;
