--  GT Warehouse Mobile -- item to image mapping.
--
--  Imagery is generated per product FAMILY, not per item: 51 families, 102
--  images, shared across 408 item variants. A "1 kg" and a "2 kg" bag of the
--  same rice are the same photograph in any real catalogue too.
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
       g.byte_size
  FROM GTITEM i
  JOIN GTIMAGE g
    ON g.ref_type = 'ITEM'
   AND g.ref_key  = 'FAM' ||
       RIGHT(DIGITS(((CAST(SUBSTR(i.sku, 6, 6) AS INTEGER) - 1) / 8) + 1), 4);

--  RIGHT(), not SUBSTR(x, 6, 4): DIGITS() of an INTEGER returns TEN characters,
--  so the value sits at position 7 and a fixed offset of 6 silently produced
--  '0000' for single-digit families. That matched 42 of 51 families by
--  coincidence and left 72 items with no image -- the kind of near-miss that
--  looks like it works until someone scans the wrong item.
