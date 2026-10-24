**free

// ---------------------------------------------------------------------
// Module:  lotrecon  (lot vs item-balance reconciliation service)
// Purpose: Implements lotrecon_run -- see lotrecon_pr.rpgle for the
//          prototype and its docstring.
// Epic:    PERP-8 (PERP-44)
// ---------------------------------------------------------------------

ctl-opt nomain;

// Real commitment control against PERPJRN -- per DDL_STYLE_GUIDE Sec.7.
exec sql set option closqlcsr = *endmod;

/copy lotrecon_pr.rpgle

// Host variables prefixed lr_ -- the SQLRPGLE precompiler collects host
// variables at module scope, not subprocedure scope (DDL_STYLE_GUIDE
// Sec.13), so an unprefixed name here could collide with a future
// second exported procedure in this module.
dcl-proc lotrecon_run export;
  dcl-pi *n int(10);
    lr_company      char(3)     const;
    lr_reconciledby varchar(18) const;
    lr_rows         likeds(lotrecon_row) dim(200);
    lr_errmsg       varchar(80);
  end-pi;

  dcl-s lr_count  int(10) inz(0);
  dcl-s lr_item   varchar(25);
  dcl-s lr_qtyoh  packed(15:4);
  dcl-s lr_lotsum packed(15:4);

  lr_errmsg = '';

  // Lot sum via a correlated subquery, not a JOIN + GROUP BY -- an
  // item with zero lot rows must still be scanned (lot sum 0 vs
  // whatever qty_on_hand drifted to), which an inner JOIN would drop
  // and a plain GROUP BY would need a LEFT JOIN + null-handling for
  // anyway. COALESCE covers the "no lot rows yet" case.
  exec sql declare lr1 cursor for
    select i.item_number, i.qty_on_hand,
           coalesce((select sum(l.qty_on_hand)
                       from perpdemo.item_lot l
                      where l.company_code = i.company_code
                        and l.item_number  = i.item_number
                        and l.is_active    = 'Y'), 0)
      from perpdemo.item i
     where i.company_code   = :lr_company
       and i.lot_controlled = 'Y'
       and i.is_active      = 'Y'
     order by i.item_number;
  exec sql open lr1;
  if sqlcode < 0;
    lr_errmsg = 'lotrecon_run open: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return 0;
  endif;

  dow lr_count < %elem(lr_rows);
    exec sql fetch lr1 into :lr_item, :lr_qtyoh, :lr_lotsum;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;

    if lr_qtyoh <> lr_lotsum;
      exec sql
        insert into perpdemo.reconciliation_log
          (company_code, item_number, item_qty_before, lot_sum_before,
           qty_after, reconciled_by, notes)
          values (:lr_company, :lr_item, :lr_qtyoh, :lr_lotsum, :lr_lotsum,
                  :lr_reconciledby, 'Auto-repaired: lot is source of truth');

      exec sql
        update perpdemo.item
           set qty_on_hand = :lr_lotsum,
               updated_at  = current_timestamp,
               updated_by  = :lr_reconciledby
         where company_code = :lr_company and item_number = :lr_item;

      lr_count += 1;
      lr_rows(lr_count).item      = lr_item;
      lr_rows(lr_count).qtybefore = lr_qtyoh;
      lr_rows(lr_count).lotsum    = lr_lotsum;
      lr_rows(lr_count).qtyafter  = lr_lotsum;
    endif;
  enddo;
  exec sql close lr1;

  return lr_count;

end-proc;
