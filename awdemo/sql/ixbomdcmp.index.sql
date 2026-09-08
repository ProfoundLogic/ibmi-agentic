--  System name pinned to match BOMSAMPLE so short-name references still resolve.
CREATE INDEX IX_BOM_DETAIL_BY_COMPONENT FOR SYSTEM NAME IX_BO00001
  ON BOM_DETAIL (COMPONENT_ITEM_ID);
