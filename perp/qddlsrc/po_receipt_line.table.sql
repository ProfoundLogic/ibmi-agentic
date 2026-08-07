-- ---------------------------------------------------------------------------
-- Table:   po_receipt_line  (auto-derived short name)
-- Module:  perp
-- Purpose: Purchase order receipt line. Records what was actually received
--          against one po_line, in BOTH the vendor's UOM (as entered) and
--          the item's inventory UOM (as posted to item.qty_on_hand /
--          item_lot), plus the conversion factor snapshot used at receipt
--          time (same snapshot idiom as po_line.unit_price).
--
--          po_number is denormalized from the parent po_receipt row.
--          po_receipt only carries po_number, not po_line's line_number,
--          so without this column a direct FK to po_line would need a
--          three-table join at insert time just to resolve one value.
--          Storing it here (like buyer_code on po_header, unit_price on
--          po_line) makes the FK to po_line direct and gives rcventr a
--          single INSERT instead of a lookup-then-insert. item_number is
--          denormalized from po_line the same way, for a direct FK to
--          item and so item_lot upserts don't need an extra join either.
--
--          lot_number is nullable. The ticket's stated rule --
--          "if item.lot_controlled = 'Y' then lot_number IS NOT NULL" --
--          cannot be a DB2 for i CHECK constraint: a CHECK expression may
--          only reference columns of the table being defined, and
--          lot_controlled lives on item, a different table. Enforced in
--          rcventr (PERP-43) before INSERT instead; see
--          DDL_STYLE_GUIDE.md Sec.17 for the general rule.
-- Epic:    PERP-8 (PERP-42)
-- ---------------------------------------------------------------------------

-- 'po_receipt_line' is 16 chars -- DB2 will auto-derive a short name (same
-- shape as po_line_schedule/requisition_line). FOR SYSTEM NAME omitted per
-- DDL_STYLE_GUIDE.md Sec.2; confirm the real short name via DSPOBJD after
-- build before referencing it in any native/CL-level command.
CREATE TABLE po_receipt_line (

  -- Composite key (per-company, per-receipt) ---------------------------------
  company_code                FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  receipt_number               FOR COLUMN RCPNBR  BIGINT        NOT NULL,
  line_number                   FOR COLUMN LINNBR  INTEGER       NOT NULL,

  -- Denormalized back-link to po_line (see header note) ----------------------
  po_number                     FOR COLUMN PONBR   BIGINT        NOT NULL,
  po_line_number                 FOR COLUMN POLNNBR INTEGER       NOT NULL,
  item_number                     FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,

  -- Quantities: vendor UOM as entered, inventory UOM as posted, and the
  -- conversion factor snapshot used to compute the latter from the former.
  qty_received_vendor_uom          FOR COLUMN QTYVUOM DECIMAL(15,4) NOT NULL,
  qty_received_inventory_uom       FOR COLUMN QTYIUOM DECIMAL(15,4) NOT NULL,
  uom_conversion_factor             FOR COLUMN CONVFCT DECIMAL(15,6) NOT NULL DEFAULT 1,

  -- Lot number -- required by rcventr when item.lot_controlled = 'Y',
  -- nullable here since DB2 for i can't cross-table CHECK it (see header).
  lot_number                         FOR COLUMN LOTNBR  VARCHAR(20),

  -- 'notes' auto-derives to a valid <=10-char system name; explicit
  -- FOR COLUMN NOTES raises SQL0612 (same finding as po_line_schedule) --
  -- omit.
  notes                                VARCHAR(240)  NOT NULL DEFAULT '',

  -- Standard audit block ----------------------------------------------------
  created_at         FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by         FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at         FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by         FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active          FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, receipt_number, line_number),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT porcl_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT porcl_linnbr_ck  CHECK (line_number > 0),
  CONSTRAINT porcl_qtyvuom_ck CHECK (qty_received_vendor_uom > 0),
  CONSTRAINT porcl_qtyiuom_ck CHECK (qty_received_inventory_uom > 0),
  CONSTRAINT porcl_convfct_ck CHECK (uom_conversion_factor > 0),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT porcl_rcpt_fk FOREIGN KEY (company_code, receipt_number)
    REFERENCES po_receipt (company_code, receipt_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT porcl_poln_fk FOREIGN KEY (company_code, po_number, po_line_number)
    REFERENCES po_line (company_code, po_number, line_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT porcl_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE po_receipt_line IS
  'PERP purchase order receipt line';

LABEL ON COLUMN po_receipt_line (
  company_code                IS 'Company code (FK to po_receipt)',
  receipt_number               IS 'Receipt number (FK to po_receipt)',
  line_number                   IS 'Line number within receipt (PK)',
  po_number                     IS 'Source PO number (FK to po_line, denorm)',
  po_line_number                 IS 'Source PO line number (FK to po_line)',
  item_number                     IS 'Item received (FK to item, denorm)',
  qty_received_vendor_uom          IS 'Qty received, vendor UOM (as entered)',
  qty_received_inventory_uom       IS 'Qty received, inventory UOM (as posted)',
  uom_conversion_factor             IS 'Vendor-to-inventory UOM factor snapshot',
  lot_number                         IS 'Lot number, required if item lot-controlled',
  notes                                IS 'Receipt line notes',
  is_active                          IS 'Active flag (Y/N, soft delete)'
);
