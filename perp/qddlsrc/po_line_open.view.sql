-- ---------------------------------------------------------------------------
-- View:    po_line_open  (system name PO_LINE_OP, auto-derived)
-- Module:  perp
-- Purpose: Expose po_line with a computed open_qty column
--          (ordered_qty - received_qty). This is the workaround for the
--          "no GENERATED ALWAYS AS (expression) computed columns" gotcha
--          documented in DDL_STYLE_GUIDE.md Sec.9 — RPG reads either
--          po_line directly and derives open_qty in code, or joins this
--          view where open_qty is already computed.
-- Epic:    PERP-7 (PERP-37)
-- ---------------------------------------------------------------------------

CREATE VIEW po_line_open AS
  SELECT company_code,
         po_number,
         line_number,
         item_number,
         ordered_qty,
         received_qty,
         (ordered_qty - received_qty)          AS open_qty,
         uom_code,
         unit_price,
         (ordered_qty * unit_price)            AS extended_price,
         expected_receipt_date,
         status_code,
         source_requisition_number,
         source_requisition_line_number,
         is_active
    FROM po_line;

-- LABEL ON TABLE caps at 50 chars on DB2 for i (DDL_STYLE_GUIDE.md Sec.2).
LABEL ON TABLE po_line_open IS
  'PERP po_line with open_qty + extended_price';
