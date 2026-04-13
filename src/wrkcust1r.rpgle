**free

/////////////////////
// Work with Customers: Customer Detail
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('CUST');

dcl-f wrkcust1d workstn;
dcl-f custp usage(*update) keyed;

/copy custr_pr.rpgle

dcl-pi *n;
  p_custno like(cust_rec.custno) const;
end-pi;

dcl-ds customer likeds(cust_rec);
dcl-s customerFound ind;
dcl-s error varchar(210);
dcl-s original_name like(sname);

// Load the data ONCE before the loop starts
exsr loadCustomer;
original_name = sname;

dow not *in03 and not *in12;
  *in30 = *on; // Protect fields off = Editable (because 30 is UL, N30 is PR)
  sfkeys = 'F3=Exit  F12=Cancel  Enter=Save';
  
  exfmt custdetail;

  if not *in03 and not *in12;
    exsr loadCustomer; 

    exsr saveChanges;
  endif;
enddo;

*inlr = *on;

begsr loadCustomer;
  error = cust_get(p_custno : customer : customerFound);
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
  else;
    if error = '';
      error = 'Customer ' + %char(p_custno) + ' not found.';
    endif;
    exsr endError;
  endif;
endsr;

begsr saveChanges;
  chain p_custno cusmstr;
  if %found(custp);
    
    // Map screen fields to file fields
    cname = sname;
    ctype = stype;
    cstatus = sstatus;
    
    climit = (slimit - sbalance) * (10 / 100); 

    caddr1 = saddr1;
    caddr2 = saddr2;
    ccity = scity;
    cstate = sstate;
    czip = szip;
    cemail = semail;
    cphone = sphone;

    if (original_name <> sname) and (original_name = sname);
      update cusmstr;
    endif;

    // After save, we should exit.
    *in03 = *on;
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