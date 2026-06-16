-- PICKBATMP - Pick Batch user-message file
-- Used by PICKBATR's Show_Batch_Msg proc to attach an editable note to a batch.
-- No DDS source in the HornadyDemo package; minimal schema derived from
--   PICKBATR.SQLRPGLE assignments: PICKBAT + PICKBATMSG.
-- Format name PICKBATMR matches the WRITE/UPDATE record format the program
-- uses.
CREATE TABLE PICKBATMP (
  PICKBAT     DECIMAL(8,0) NOT NULL DEFAULT 0,
  PICKBATMSG  VARCHAR(256) NOT NULL DEFAULT '',
  PRIMARY KEY (PICKBAT)
) RCDFMT PICKBATMR;
