--  GT Warehouse Mobile -- Putaway: what is staged, and where it should go.
--
--  Receiving posts everything it accepts into the staging location STAGE01 and
--  flips the pallet to RECV (see gtrclr.sqlrpgle). Putaway is the other half of
--  that sentence: staged stock is not in a pick face or a bulk bay yet, so it
--  cannot be picked and it is standing in the way of the next truck.
--
--  Two views, because the two questions are different:
--
--    GTVPUTSTG  what is waiting -- one row per staged SKU, with the pallet it
--               arrived on and the imagery, so the work list reads like the
--               rest of the application.
--
--    GTVPUTSUG  where it could go -- EVERY candidate destination, ranked, not
--               just the winner. The screen shows the operator the suggestion
--               AND lets them see it was a choice; RPG takes rank 1 and the
--               operator can override by scanning any location.
--
--  The ranking is the whole opinion of this application about putaway, and it
--  lives HERE rather than in the two programs that ask, so the suggestion on
--  the list and the suggestion on the confirm screen can never disagree:
--
--    1  the SKU's own pick face, if it has room. Replenishing the face the
--       pickers already use beats creating a second home for the same item.
--    2  a bulk bay this SKU is already in, if the bay has room. Consolidating
--       beats scattering.
--    3  an empty pick face. Only when the item has nowhere it belongs yet.
--
--  Room is computed per type because the two are measured differently: a pick
--  face has a max_qty for that one SKU, a bulk bay has capacity_units shared
--  across everything standing in it.
--
--  DOES IT FIT COMES BEFORE WHERE IT BELONGS. Preference alone sent a staged
--  pallet of 864 shirts to its own pick face, which had room for 36, while a
--  bulk bay with room for 1227 sat at rank 2. The face is the right ANSWER to
--  the wrong QUESTION: a putaway has to physically fit somewhere first, and
--  only then prefer the place it belongs. Found by opening the screen and
--  reading the numbers on it -- both views were internally consistent and both
--  were wrong together, which is exactly what a shared derivation buys you and
--  exactly what it cannot protect you from.
--
--  SORT_KEY carries the whole ordering so the two programs do not each write
--  their own ORDER BY and drift apart: fits first, then preference. Both just
--  `ORDER BY sort_key, room DESC`.

CREATE OR REPLACE VIEW GTVPUTSTG AS
SELECT b.sku,
       b.location_id                              AS stage_location,
       l.zone                                     AS stage_zone,
       b.qty_on_hand                              AS qty_staged,
       i.description_en,
       i.description_fr,
       i.department,
       i.uom,
       i.case_pack,
       COALESCE((SELECT MIN(v.image_id) FROM GTVITEMIMG v
                  WHERE v.sku = b.sku AND v.img_group = 0), 0) AS image_id,
       --  The pallet it came in on, when there is one. Receiving parks the
       --  pallet at the staging location, so this is how a scanned SSCC finds
       --  its way back to a work-list row.
       COALESCE((SELECT MIN(p.sscc) FROM GTPALLET p
                  WHERE p.location_id = b.location_id
                    AND p.pallet_status = 'RECV'), '')         AS sscc
  FROM GTINVBAL b
  JOIN GTLOCATION l ON l.location_id = b.location_id
  JOIN GTITEM     i ON i.sku         = b.sku
 WHERE l.location_type = 'STAG'
   AND b.qty_on_hand   > 0;

CREATE OR REPLACE VIEW GTVPUTSUG AS
--  1 -- the SKU's own pick face, with room left against its max.
SELECT b.sku,
       l.location_id,
       l.location_type,
       l.zone,
       l.aisle,
       l.bay,
       l.level_id,
       CAST(l.max_qty - b.qty_on_hand AS DECIMAL(11,2)) AS room,
       b.qty_on_hand                                    AS qty_there,
       CAST(1 AS SMALLINT)                              AS rank_no,
       COALESCE((SELECT SUM(s.qty_staged) FROM GTVPUTSTG s
                  WHERE s.sku = b.sku), 0)              AS qty_staged,
       CAST(CASE WHEN l.max_qty - b.qty_on_hand >=
                      COALESCE((SELECT SUM(s.qty_staged) FROM GTVPUTSTG s
                                 WHERE s.sku = b.sku), 0)
                 THEN 1 ELSE 11 END AS SMALLINT)        AS sort_key
  FROM GTINVBAL   b
  JOIN GTLOCATION l ON l.location_id = b.location_id
 WHERE l.location_type = 'PICK'
   AND l.active_flag   = 'Y'
   AND l.max_qty       > b.qty_on_hand

UNION ALL

--  2 -- a bulk bay this SKU already occupies, with room against the bay's
--       shared capacity rather than against one SKU's maximum.
SELECT b.sku,
       l.location_id,
       l.location_type,
       l.zone,
       l.aisle,
       l.bay,
       l.level_id,
       CAST(l.capacity_units -
            (SELECT COALESCE(SUM(x.qty_on_hand), 0) FROM GTINVBAL x
              WHERE x.location_id = l.location_id) AS DECIMAL(11,2)) AS room,
       b.qty_on_hand                                    AS qty_there,
       CAST(2 AS SMALLINT)                              AS rank_no,
       COALESCE((SELECT SUM(s.qty_staged) FROM GTVPUTSTG s
                  WHERE s.sku = b.sku), 0)              AS qty_staged,
       CAST(CASE WHEN l.capacity_units -
                      (SELECT COALESCE(SUM(x.qty_on_hand), 0) FROM GTINVBAL x
                        WHERE x.location_id = l.location_id) >=
                      COALESCE((SELECT SUM(s.qty_staged) FROM GTVPUTSTG s
                                 WHERE s.sku = b.sku), 0)
                 THEN 2 ELSE 12 END AS SMALLINT)        AS sort_key
  FROM GTINVBAL   b
  JOIN GTLOCATION l ON l.location_id = b.location_id
 WHERE l.location_type = 'BULK'
   AND l.active_flag   = 'Y'
   AND l.capacity_units >
       (SELECT COALESCE(SUM(x.qty_on_hand), 0) FROM GTINVBAL x
         WHERE x.location_id = l.location_id)

UNION ALL

--  3 -- an empty pick face, for an item with no home yet. Joined to the staged
--       list rather than to GTITEM: this arm is a cross product, and 408 items
--       against every empty face would be a large answer to a question nobody
--       asked. Staged SKUs are a handful.
SELECT s.sku,
       l.location_id,
       l.location_type,
       l.zone,
       l.aisle,
       l.bay,
       l.level_id,
       CAST(l.max_qty AS DECIMAL(11,2))                 AS room,
       CAST(0 AS DECIMAL(11,2))                         AS qty_there,
       CAST(3 AS SMALLINT)                              AS rank_no,
       s.qty_staged                                     AS qty_staged,
       CAST(CASE WHEN l.max_qty >= s.qty_staged
                 THEN 3 ELSE 13 END AS SMALLINT)        AS sort_key
  FROM GTVPUTSTG  s
 CROSS JOIN GTLOCATION l
 WHERE l.location_type = 'PICK'
   AND l.active_flag   = 'Y'
   AND l.max_qty       > 0
   AND NOT EXISTS (SELECT 1 FROM GTINVBAL x
                    WHERE x.location_id = l.location_id);
