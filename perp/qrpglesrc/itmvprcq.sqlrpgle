**free

// ---------------------------------------------------------------------
// Module:  itmvprcq  (item-vendor price history query service)
// Purpose: Implements itmvprcq_history -- see itmvprcq_pr.rpgle for the
//          prototype and its docstring.
// Epic:    PERP-5 (PERP-32)
// ---------------------------------------------------------------------

ctl-opt nomain;

// Real commitment control against PERPJRN -- per DDL_STYLE_GUIDE Sec.7.
exec sql set option closqlcsr = *endmod;

/copy itmvprcq_pr.rpgle

// Host-variable names prefixed hc_ -- the SQLRPGLE precompiler collects
// host variables at module scope, not subprocedure scope, so an
// unprefixed name here could collide with a future second exported
// procedure in this module (SQL0314).
dcl-proc itmvprcq_history export;
  dcl-pi *n int(10);
    hc_company char(3)     const;
    hc_item    varchar(25) const;
    hc_rows    likeds(itmvprcq_row) dim(200);
    hc_errmsg  varchar(80);
  end-pi;

  dcl-s hc_count int(10) inz(0);
  dcl-ds hc_row likeds(itmvprcq_row);

  hc_errmsg = '';

  exec sql declare hc1 cursor for
    select vendor_code, vendor_name,
           char(effective_from, iso),
           case when effective_to is null then ''
                else char(effective_to, iso) end,
           unit_price, currency_code
      from perpdemo.item_vendor_price_history
     where company_code = :hc_company and item_number = :hc_item
     order by effective_from;
  exec sql open hc1;
  if sqlcode < 0;
    hc_errmsg = 'itmvprcq_history open: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return 0;
  endif;

  dow hc_count < %elem(hc_rows);
    exec sql fetch hc1 into :hc_row;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    hc_count += 1;
    hc_rows(hc_count) = hc_row;
  enddo;
  exec sql close hc1;

  return hc_count;

end-proc;
