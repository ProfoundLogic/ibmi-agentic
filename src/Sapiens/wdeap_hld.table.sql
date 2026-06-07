--
-- WDEAP_HLD - REVERSE-ENGINEERED placeholder (trigger-DS stub).
-- SPRtrigger.rpgle uses extName(wdeap_hld) twice; underscore in the
-- name requires SQL CREATE TABLE rather than DDS CRTPF.
-- Single placeholder column is enough to satisfy the compile.
--
CREATE TABLE WDEAP_HLD (
  EHCO#   DECIMAL(3,0)  NOT NULL DEFAULT 0,
  EHDUMMY CHAR(10)      NOT NULL DEFAULT ''
);
