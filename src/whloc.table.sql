-- WHLOC - Warehouse Location Master
-- 12 (row) x 60 (bay) x 4 (shelf) coordinate grid per DC.
-- Primary key (WHITEM, WHDC) means each SKU lives in one location per
-- distribution centre; the picker workflow joins on this to surface
-- row/bay/shelf for every pick line.
CREATE TABLE WHLOC (
  WHITEM   CHAR(15)     NOT NULL DEFAULT '',
  WHDC     CHAR(4)      NOT NULL DEFAULT '',
  WHROW    DECIMAL(3,0) NOT NULL DEFAULT 0,
  WHBAY    DECIMAL(3,0) NOT NULL DEFAULT 0,
  WHSHELF  DECIMAL(2,0) NOT NULL DEFAULT 0,
  WHQOH    DECIMAL(9,0) NOT NULL DEFAULT 0,
  PRIMARY KEY (WHITEM, WHDC)
) RCDFMT WHLOCR;

LABEL ON TABLE WHLOC IS 'Item -> warehouse coordinate master';
