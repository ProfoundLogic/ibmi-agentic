-- =====================================================================
--  Receiving views.
--
--  Everything the receiving screens display is derived HERE rather than in
--  RPG, for the same reason the item views are: GENERATED ALWAYS AS
--  (expression) does not work on this box, so a view is where computed and
--  formatted values belong. It also means the line list, the progress bar and
--  the post transaction all agree by construction.
-- =====================================================================

-- ---------------------------------------------------------------------
--  GTVRCVLIN -- one row per receipt line, ready to render.
--
--  Carries the item's description and the ONE image the line list shows as a
--  thumbnail, so the subfile needs no second query per row. The thumbnail is
--  the item's lowest-sequence CATALOGUE image (img_group 0): a receiving line
--  should show what the product is meant to look like, not the damage
--  photograph somebody just took of it.
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW GTVRCVLIN AS
SELECT d.receipt_id,
       d.line_no,
       d.sku,
       i.description_en,
       i.description_fr,
       i.department,
       i.uom,
       i.case_pack,
       d.qty_expected,
       d.qty_received,
       d.qty_received - d.qty_expected                      AS qty_variance,
       d.reason_code,
       d.line_status,
       d.photo_count,
       d.scanned_barcode,
       --  Status is stored, not derived, because the operator's intent
       --  matters: a line deliberately confirmed at zero is not the same as
       --  one nobody has touched. This is the DISPLAY ordering only.
       CASE d.line_status WHEN 'OPEN' THEN 0
                          WHEN 'DAMG' THEN 1
                          WHEN 'SHRT' THEN 2
                          WHEN 'OVER' THEN 3
                          ELSE 4 END                        AS sort_group,
       COALESCE((SELECT MIN(v.image_id)
                   FROM GTVITEMIMG v
                  WHERE v.sku = d.sku
                    AND v.img_group = 0), 0)                AS thumb_id
  FROM GTRCPTDTL d
  JOIN GTITEM i ON i.sku = d.sku;


-- ---------------------------------------------------------------------
--  GTVRCVOPEN -- receipts a receiver can actually work on, newest first.
--
--  Posted receipts are excluded rather than filtered in the program: the home
--  screen's list and its count then cannot disagree.
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW GTVRCVOPEN AS
SELECT p.receipt_id,
       p.po_number,
       p.vendor_name,
       p.door_id,
       p.receipt_status,
       p.sscc,
       p.expected_ts,
       p.line_count,
       p.lines_done,
       p.lines_variance,
       p.photo_count,
       p.qty_expected,
       p.qty_received
  FROM GTVRCPPRG p
 WHERE p.receipt_status IN ('OPEN', 'PART');
