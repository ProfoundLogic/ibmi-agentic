-- =====================================================================
--  Inventory & Movement views.
--
--  The bulk-versus-pick-face story (design doc §8.3) is the operational value
--  of this application, so both numbers are derived HERE rather than in RPG:
--  what is in the location you are standing in front of, and what is available
--  in bulk racking elsewhere. One definition, so the location screen, the
--  replenish suggestion and the move screen cannot disagree.
-- =====================================================================

-- ---------------------------------------------------------------------
--  GTVINVLOC -- one row per item in a location, ready to render.
--
--  Carries the item's description, its catalogue thumbnail and the bulk
--  availability, so the subfile needs no per-row query. `needs_replen` is the
--  Replenish shortcut's own condition, evaluated once here: a pick face below
--  its minimum with bulk stock to pull from.
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW GTVINVLOC AS
SELECT b.location_id,
       l.zone,
       l.location_type,
       l.min_qty,
       l.max_qty,
       b.sku,
       i.description_en,
       i.description_fr,
       i.department,
       i.uom,
       i.case_pack,
       b.qty_on_hand,
       b.qty_allocated,
       b.qty_on_hand - b.qty_allocated              AS qty_available,
       COALESCE(k.qty_bulk_avail, 0)                AS qty_bulk_avail,
       COALESCE(k.bulk_locations, 0)                AS bulk_locations,
       CASE WHEN l.location_type = 'PICK'
             AND l.min_qty > 0
             AND b.qty_on_hand < l.min_qty
             AND COALESCE(k.qty_bulk_avail, 0) > 0
            THEN 'Y' ELSE 'N' END                   AS needs_replen,
       --  How much to pull to fill the face back to its maximum, never more
       --  than bulk can actually supply.
       CASE WHEN l.max_qty > b.qty_on_hand
            THEN CASE WHEN l.max_qty - b.qty_on_hand > COALESCE(k.qty_bulk_avail, 0)
                      THEN COALESCE(k.qty_bulk_avail, 0)
                      ELSE l.max_qty - b.qty_on_hand END
            ELSE 0 END                              AS qty_suggested,
       COALESCE((SELECT MIN(v.image_id)
                   FROM GTVITEMIMG v
                  WHERE v.sku = b.sku
                    AND v.img_group = 0), 0)        AS thumb_id
  FROM GTINVBAL   b
  JOIN GTLOCATION l ON l.location_id = b.location_id
  JOIN GTITEM     i ON i.sku         = b.sku
  LEFT JOIN GTVBULK k ON k.sku       = b.sku;


-- ---------------------------------------------------------------------
--  GTVLOCSUM -- the location header: what is in it, and how full it is.
--
--  Locations with nothing in them still have to appear: scanning an empty
--  location and being told it is empty is a valid answer, and a LEFT JOIN from
--  GTLOCATION is what makes that possible.
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW GTVLOCSUM AS
SELECT l.location_id,
       l.zone,
       l.location_type,
       l.aisle,
       l.bay,
       l.level_id,
       l.capacity_units,
       l.min_qty,
       l.max_qty,
       l.active_flag,
       COUNT(b.sku)                                 AS item_count,
       COALESCE(SUM(b.qty_on_hand), 0)              AS qty_on_hand,
       COALESCE(SUM(b.qty_allocated), 0)            AS qty_allocated,
       --  Percentage full, capped at 100 so a bar cannot render past its own
       --  track. A location with no capacity on file reports 0 rather than
       --  dividing by zero.
       CASE WHEN l.capacity_units > 0
            THEN CASE WHEN COALESCE(SUM(b.qty_on_hand), 0) >= l.capacity_units
                      THEN 100
                      ELSE INTEGER((COALESCE(SUM(b.qty_on_hand), 0) * 100)
                                   / l.capacity_units) END
            ELSE 0 END                              AS pct_full
  FROM GTLOCATION l
  LEFT JOIN GTINVBAL b ON b.location_id = l.location_id
 GROUP BY l.location_id, l.zone, l.location_type, l.aisle, l.bay, l.level_id,
          l.capacity_units, l.min_qty, l.max_qty, l.active_flag;
