-- PICKMBATDP - MegaBatch -> Component Batch mapping
-- A "MegaBatch" groups several real pick batches under one umbrella batch
-- number for picking only (shipment publishing stays per-component batch).
-- See PICKBATR.SQLRPGLE inline notes from RBM 12/03/2025.
-- Schema inferred from the SQL inserts/selects:
--   insert into pickmbatdp (select :newBatchNum, batch from batchArray);
--   select pickbat from pickmbatdp where pickmbat = :newBatchNum;
CREATE TABLE PICKMBATDP (
  PICKMBAT  DECIMAL(8,0) NOT NULL DEFAULT 0,
  PICKBAT   DECIMAL(8,0) NOT NULL DEFAULT 0,
  PRIMARY KEY (PICKMBAT, PICKBAT)
) RCDFMT PICKMBATDR;
