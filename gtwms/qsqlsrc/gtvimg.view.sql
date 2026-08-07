--  GT Warehouse Mobile -- item to image mapping.
--
--  An item's imagery comes from TWO places, and this view is what makes them
--  look like one list to every program that reads it:
--
--    C  catalogue -- generated per product FAMILY, not per item: 51 families,
--       102 images, shared across 408 item variants. A "1 kg" and a "2 kg" bag
--       of the same rice are the same photograph in any real catalogue too.
--
--    P  photo     -- taken on the device by an operator standing in front of
--       the actual stock, stored against THAT SKU alone. Damage, a relabelled
--       carton, a pallet that does not look like the catalogue shot: the
--       things a stock photograph can never show.
--
--  IMG_GROUP orders them, catalogue first, so the carousel still opens on the
--  clean product shot with the operator's photos appended after it.
--
--  The demo-data generator lays items out sequentially, exactly 8 variants per
--  family, and the SKU carries that sequence: GROC-000001 .. TOYS-000408.
--  Family index is therefore ((seq - 1) / 8) + 1.
--
--  That derivation lives HERE and nowhere else. If the generator ever changes
--  its variant count, this view is the single place to correct -- which is why
--  it is a view rather than the same arithmetic copied into four programs.

CREATE OR REPLACE VIEW GTVITEMIMG AS
SELECT i.sku,
       g.image_id,
       g.seq_no,
       g.caption_en,
       g.caption_fr,
       g.byte_size,
       CAST('C' AS CHAR(1)) AS IMG_SOURCE,
       CAST(0 AS SMALLINT)  AS IMG_GROUP
  FROM GTITEM i
  JOIN GTIMAGE g
    ON g.ref_type = 'ITEM'
   AND g.ref_key  = 'FAM' ||
       RIGHT(DIGITS(((CAST(SUBSTR(i.sku, 6, 6) AS INTEGER) - 1) / 8) + 1), 4)

UNION ALL

--  Operator photos key on the SKU itself. A SKU never looks like 'FAMnnnn', so
--  the two arms cannot claim each other's rows and an item picks up only its
--  own photographs.
SELECT i.sku,
       g.image_id,
       g.seq_no,
       g.caption_en,
       g.caption_fr,
       g.byte_size,
       CAST('P' AS CHAR(1)) AS IMG_SOURCE,
       CAST(1 AS SMALLINT)  AS IMG_GROUP
  FROM GTITEM i
  JOIN GTIMAGE g
    ON g.ref_type = 'ITEM'
   AND g.ref_key  = i.sku;

--  RIGHT(), not SUBSTR(x, 6, 4): DIGITS() of an INTEGER returns TEN characters,
--  so the value sits at position 7 and a fixed offset of 6 silently produced
--  '0000' for single-digit families. That matched 42 of 51 families by
--  coincidence and left 72 items with no image -- the kind of near-miss that
--  looks like it works until someone scans the wrong item.
