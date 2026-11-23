**free

// ---------------------------------------------------------------------
// Prototypes: itmvprcq  (item-vendor price history query service)
// Module:     perp
// Purpose:    Effective-dated price history for one item across all of
//             its vendors -- the pricing-over-time comparative graph
//             demo. Callers: future graphing UI, smoke-test caller
//             itmvprcqsmk.
// Epic:       PERP-5 (PERP-32)
// ---------------------------------------------------------------------

// One price-history row. ISO date strings (not native RPG date fields)
// per the PERP-3 finding: nullable DATE columns are simpler handled as
// CHAR(10) ISO strings across the embedded-SQL boundary.
dcl-ds itmvprcq_row qualified template;
  vendor   varchar(10);
  vendorNm varchar(60);
  efffrm   varchar(10);
  effto    varchar(10);
  price    packed(15:4);
  currency varchar(20);
end-ds;

// itmvprcq_history -- fetch up to %elem(histRows) price-history rows for
// (company, item) across all vendors, ordered by effective_from ascending
// (oldest first, matching a left-to-right time-series graph). Returns the
// number of rows fetched (0 on error or no history; check errmsg to tell
// the two apart -- errmsg is blank when 0 legitimately means "no history
// yet"). Raises no messages itself; SQL diagnostics are returned in
// errmsg as 'SQLCODE=... SQLSTATE=...' for the caller to log/display.
dcl-pr itmvprcq_history int(10);
  company   char(3)      const;
  item      varchar(25)  const;
  histRows  likeds(itmvprcq_row) dim(200);
  errmsg    varchar(80);
end-pr;
