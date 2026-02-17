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
  editMode ind const options(*nopass);
end-pi;

dcl-ds customer likeds(cust_rec);
dcl-s customerFound ind;
dcl-s error varchar(210);
dcl-s isEditMode ind inz(*off);
dcl-s done ind inz(*off);

if %parms() >= 2 and editMode;
  isEditMode = *on;
endif;

dow not done;
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

    if isEditMode;
      *in30 = *on;
      sfkeys = 'F3=Exit  Enter=Update';
    else;
      *in30 = *off; // Protect fields from editing.
      sfkeys = 'F3=Exit';
    endif;

    exfmt custdetail;

    if *in03;
      done = *on;
      return;
    endif;

    if isEditMode;
      exsr copyScreenToCustomer;
      error = cust_update(customer);
      if error = '';
        done = *on;
        return;
      else;
        exsr displayError;
      endif;
    endif;
  else;
    if error = '';
      error = 'Customer ' + %char(custno) + ' not found.';
    endif;
    done = *on;
    exsr endError;
  endif;

endsr;

// Displays error window and ends the program.
begsr endError;
  serrattr = x'A0';
  serrwinmsg = error;
  exfmt errorwin;
endsr;

begsr copyScreenToCustomer;
  customer.cname = sname;
  customer.ctype = %upper(stype);
  customer.cstatus = %upper(sstatus);
  customer.climit = slimit;
  customer.caddr1 = saddr1;
  customer.caddr2 = saddr2;
  customer.ccity = scity;
  customer.cstate = %upper(sstate);
  customer.czip = szip;
  customer.cemail = semail;
  customer.cphone = sphone;
endsr;

begsr displayError;
  serrattr = x'A0';
  serrwinmsg = error;
  exfmt errorwin;
  if *in03;
    done = *on;
  endif;
endsr;
