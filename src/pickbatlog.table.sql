-- PICKBATLOG - Pick Batch audit / event log
-- Inferred from PICKBATLR2.SQLRPGLE insert statements
-- (LOGTS, LOGBAT, LOGGRP, LOGMSG, LOGUSR, LOGJOB, LOGJOB#).
-- No DDS source in the HornadyDemo package; types sized to fit the
-- :in_Batch / :wk_ExistingGrp / :ps_Job / :ps_Job# host variables that
-- PICKBATLR2 binds (PICKBAT-shaped 8,0 -- shipment group 7,0 -- job# 6,0).
CREATE TABLE PICKBATLOG (
  LOGTS    TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  LOGBAT   DECIMAL(8,0) NOT NULL DEFAULT 0,
  LOGGRP   DECIMAL(7,0) NOT NULL DEFAULT 0,
  LOGMSG   VARCHAR(256) NOT NULL DEFAULT '',
  LOGUSR   CHAR(10)     NOT NULL DEFAULT '',
  LOGJOB   CHAR(10)     NOT NULL DEFAULT '',
  LOGJOB#  DECIMAL(6,0) NOT NULL DEFAULT 0
) RCDFMT PICKLOGR;
