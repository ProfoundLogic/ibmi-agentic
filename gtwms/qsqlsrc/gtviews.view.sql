--  GT Warehouse Mobile -- views
--
--  All derived values live here rather than in computed columns, because
--  GENERATED ALWAYS AS (expression) does not work on this platform. Keeping
--  them in views also means the "available in bulk" rule is defined once and
--  every screen that shows it agrees.
--
--  Views are read from embedded SQL only, never opened as externally-described
--  files, so long column names are fine.

--  Total on-hand and allocated for an item across the whole building.
CREATE OR REPLACE VIEW GTVITEMOH AS
SELECT b.sku,
       SUM(b.qty_on_hand)                       AS qty_on_hand,
       SUM(b.qty_allocated)                     AS qty_allocated,
       SUM(b.qty_on_hand - b.qty_allocated)     AS qty_available,
       COUNT(*)                                 AS location_count
  FROM GTINVBAL b
 GROUP BY b.sku;

--  Available in bulk racking -- the second number on the Inventory location
--  screen, and the source of the replenishment suggestion.
CREATE OR REPLACE VIEW GTVBULK AS
SELECT b.sku,
       SUM(b.qty_on_hand - b.qty_allocated)     AS qty_bulk_avail,
       COUNT(*)                                 AS bulk_locations
  FROM GTINVBAL b
  JOIN GTLOCATION l ON l.location_id = b.location_id
 WHERE l.location_type = 'BULK'
   AND l.active_flag   = 'Y'
 GROUP BY b.sku;

--  Pick faces sitting below their minimum that have bulk stock to pull from.
--  Drives the Replenish shortcut and Supervisor View's alert list.
CREATE OR REPLACE VIEW GTVREPLEN AS
SELECT b.sku,
       b.location_id,
       l.zone,
       b.qty_on_hand,
       l.min_qty,
       l.max_qty,
       l.max_qty - b.qty_on_hand                AS qty_suggested,
       COALESCE(k.qty_bulk_avail, 0)            AS qty_bulk_avail
  FROM GTINVBAL   b
  JOIN GTLOCATION l ON l.location_id = b.location_id
  LEFT JOIN GTVBULK k ON k.sku = b.sku
 WHERE l.location_type = 'PICK'
   AND l.active_flag   = 'Y'
   AND l.min_qty       > 0
   AND b.qty_on_hand   < l.min_qty
   AND COALESCE(k.qty_bulk_avail, 0) > 0;

--  Receipt progress, for the receiving home list and Supervisor View.
--  A receipt is "confirmed" line by line; this rolls those counts up.
CREATE OR REPLACE VIEW GTVRCPPRG AS
SELECT h.receipt_id,
       h.po_number,
       h.vendor_name,
       h.door_id,
       h.receipt_status,
       h.sscc,
       h.expected_ts,
       COUNT(d.line_no)                                                AS line_count,
       SUM(CASE WHEN d.line_status <> 'OPEN' THEN 1 ELSE 0 END)        AS lines_done,
       SUM(CASE WHEN d.line_status IN ('SHRT','OVER','DAMG')
                THEN 1 ELSE 0 END)                                     AS lines_variance,
       SUM(d.photo_count)                                              AS photo_count,
       SUM(d.qty_expected)                                             AS qty_expected,
       SUM(d.qty_received)                                             AS qty_received
  FROM GTRCPTHDR h
  LEFT JOIN GTRCPTDTL d ON d.receipt_id = h.receipt_id
 GROUP BY h.receipt_id, h.po_number, h.vendor_name, h.door_id,
          h.receipt_status, h.sscc, h.expected_ts;

--  Cycle count variance rollup, for the variance review and for the
--  supervisor's approval queue.
CREATE OR REPLACE VIEW GTVCNTVAR AS
SELECT c.count_id,
       c.location_id,
       c.count_status,
       c.assigned_to,
       COUNT(d.line_no)                                                AS line_count,
       SUM(CASE WHEN d.qty_counted <> d.qty_expected
                THEN 1 ELSE 0 END)                                     AS lines_variance,
       SUM(d.qty_expected)                                             AS qty_expected,
       SUM(d.qty_counted)                                              AS qty_counted,
       SUM(d.qty_counted - d.qty_expected)                             AS qty_variance
  FROM GTCOUNTHDR c
  LEFT JOIN GTCOUNTDTL d ON d.count_id = c.count_id
 GROUP BY c.count_id, c.location_id, c.count_status, c.assigned_to;
