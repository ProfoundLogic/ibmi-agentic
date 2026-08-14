-- =====================================================================
--  Cycle Count views.
--
--  BLIND COUNTING is the operationally correct behaviour: the counter must not
--  see what the system expects, or the number they write down is the system's
--  number rather than what is on the shelf.
--
--  This view CARRIES qty_expected, because the variance review needs it. The
--  counting screen simply never selects it -- and its display file has no field
--  to hold it, so the expected quantity never reaches the browser at all. That
--  is the difference between blind and hidden: hidden is a CSS rule somebody can
--  turn off in dev tools.
-- =====================================================================

-- ---------------------------------------------------------------------
--  GTVCNTLIN -- one row per count line, ready to render.
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW GTVCNTLIN AS
SELECT d.count_id,
       d.line_no,
       d.sku,
       i.description_en,
       i.description_fr,
       i.department,
       i.uom,
       i.case_pack,
       d.qty_expected,
       d.qty_counted,
       d.qty_counted - d.qty_expected                AS qty_variance,
       ABS(d.qty_counted - d.qty_expected)           AS qty_variance_abs,
       d.counted_flag,
       d.reason_code,
       d.photo_count,
       --  A variance only counts as one once the line has actually been
       --  counted: an uncounted line is not a discrepancy, it is unfinished.
       CASE WHEN d.counted_flag = 'Y'
             AND d.qty_counted <> d.qty_expected
            THEN 'Y' ELSE 'N' END                    AS has_variance,
       COALESCE((SELECT MIN(v.image_id)
                   FROM GTVITEMIMG v
                  WHERE v.sku = d.sku
                    AND v.img_group = 0), 0)         AS thumb_id
  FROM GTCOUNTDTL d
  JOIN GTITEM     i ON i.sku = d.sku;


-- ---------------------------------------------------------------------
--  GTVCNTOPEN -- counts a counter can work on, with progress.
--
--  Posted counts are excluded in the view rather than in the program, so the
--  list and its count cannot disagree.
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW GTVCNTOPEN AS
SELECT h.count_id,
       h.location_id,
       h.count_status,
       h.assigned_to,
       o.full_name                                   AS assigned_name,
       l.zone,
       l.location_type,
       COUNT(d.line_no)                              AS line_count,
       SUM(CASE WHEN d.counted_flag = 'Y' THEN 1 ELSE 0 END)     AS lines_counted,
       SUM(CASE WHEN d.counted_flag = 'Y'
                 AND d.qty_counted <> d.qty_expected
                THEN 1 ELSE 0 END)                   AS lines_variance,
       SUM(d.photo_count)                            AS photo_count
  FROM GTCOUNTHDR h
  JOIN GTLOCATION l ON l.location_id = h.location_id
  LEFT JOIN GTCOUNTDTL d ON d.count_id = h.count_id
  LEFT JOIN GTOPERATOR o ON o.operator_id = h.assigned_to
 WHERE h.count_status IN ('OPEN', 'CNTD', 'APPR')
 GROUP BY h.count_id, h.location_id, h.count_status, h.assigned_to,
          o.full_name, l.zone, l.location_type;
