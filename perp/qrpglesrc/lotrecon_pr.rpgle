**free

// ---------------------------------------------------------------------
// Prototypes: lotrecon  (lot vs item-balance reconciliation service)
// Module:     perp
// Purpose:    Detects drift between item.qty_on_hand and
//             SUM(item_lot.qty_on_hand) for lot-controlled items
//             (Option C balances -- denormalized by design, see
//             item_lot.table.sql), and repairs it. Lot is the source
//             of truth: item.qty_on_hand is set to match the lot sum.
//             Every repair is logged to reconciliation_log. Callers:
//             a future scheduled/CoderFlow job, smoke-test caller
//             lotrcnsmk.
// Epic:       PERP-8 (PERP-44)
// ---------------------------------------------------------------------

// One reconciliation result row -- mirrors reconciliation_log's
// before/after columns.
dcl-ds lotrecon_row qualified template;
  item      varchar(25);
  qtybefore packed(15:4);
  lotsum    packed(15:4);
  qtyafter  packed(15:4);
end-ds;

// lotrecon_run -- scan every lot-controlled, active item for one
// company; for each where item.qty_on_hand <> SUM(item_lot.qty_on_hand),
// log the discrepancy to reconciliation_log and repair item.qty_on_hand
// to match the lot sum (lot is the source of truth). Returns the number
// of discrepancies found and repaired (0 = clean, no drift -- check
// errmsg to tell a genuinely clean run apart from a failed one: errmsg
// is blank when 0 legitimately means "no drift found").
//
// Does NOT commit or rollback -- per DDL_STYLE_GUIDE.md Sec.13, leaf
// service programs never issue commitment-control statements; the
// caller commits (or rolls back on error) after this returns.
//
// Raises no messages itself; SQL diagnostics are returned in errmsg as
// 'SQLCODE=... SQLSTATE=...' for the caller to log/display.
dcl-pr lotrecon_run int(10);
  company      char(3)      const;
  reconciledby varchar(18)  const;
  resultRows   likeds(lotrecon_row) dim(200);
  errmsg       varchar(80);
end-pr;
