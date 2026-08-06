--  GT Warehouse Mobile -- indexes
--
--  SQL indexes rather than DDS logical files, per the project's DDL-first rule.
--  Only access paths the application actually walks are created; primary keys
--  already cover lookups whose leading column matches.

--  Barcode -> item is the hot path: every scan in the entire application hits
--  it. Covered by GTBARCODE's primary key, so the index here is the reverse
--  direction -- "show me every barcode for this item" on the item detail screen.
CREATE INDEX GTBARSKUX ON GTBARCODE (sku, barcode_level);

--  "What is in this location" -- GTINVBAL's key is (sku, location_id), so a
--  location-first path is needed for the Inventory location screen.
CREATE INDEX GTINVLOCX ON GTINVBAL (location_id, sku);

--  Locations by zone and type, for the bulk-availability view and for
--  Supervisor View's replenishment alerts.
CREATE INDEX GTLOCZONX ON GTLOCATION (zone, location_type, location_id);
CREATE INDEX GTLOCTYPX ON GTLOCATION (location_type, location_id);

--  Item search by department, and description search on the item lookup screen.
CREATE INDEX GTITMDEPX ON GTITEM (department, sku);

--  Movement history: by item for the item screen, by timestamp for Supervisor
--  View, and by source document to pull back everything one receipt posted.
CREATE INDEX GTMOVSKUX ON GTMOVEMENT (sku, moved_ts);
CREATE INDEX GTMOVTSX  ON GTMOVEMENT (moved_ts);
CREATE INDEX GTMOVDOCX ON GTMOVEMENT (source_doc, movement_type);

--  Receipts open today, ordered for the receiving home screen.
CREATE INDEX GTRCPSTSX ON GTRCPTHDR (receipt_status, expected_ts);
CREATE INDEX GTRCPSCCX ON GTRCPTHDR (sscc);

--  Counts assigned to an operator.
CREATE INDEX GTCNTASGX ON GTCOUNTHDR (assigned_to, count_status);

--  Image retrieval for the carousel: one seek per screen, ordered by sequence.
CREATE INDEX GTIMGREFX ON GTIMAGE (ref_type, ref_key, seq_no);

--  Operator badge scan on the sign-on screen.
CREATE INDEX GTOPRBDGX ON GTOPERATOR (badge_barcode);
