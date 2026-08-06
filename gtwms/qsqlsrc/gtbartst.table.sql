--  Results table for the GTBAR unit test (gtbartst.sqlrpgle).
--  Not part of the application -- it exists so barcode parsing can be
--  verified from outside the program rather than by reading a joblog.
--  Barcode correctness is exactly the thing that must not be wrong on stage.

BEGIN
    DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN END;
    CALL QSYS2.QCMDEXC('ADDLIBLE LIB(TIGERPOC) POSITION(*FIRST)');
END;

DROP TABLE IF EXISTS GTBARTST;

CREATE TABLE GTBARTST (
    test_no     FOR COLUMN GTBTNO   SMALLINT     NOT NULL,
    test_name   FOR COLUMN GTBTNAM  VARCHAR(40)  NOT NULL,
    payload     FOR COLUMN GTBTPAY  VARCHAR(120) NOT NULL DEFAULT '',
    kind        FOR COLUMN GTBTKND  CHAR(6)      NOT NULL DEFAULT '',
    symbology   FOR COLUMN GTBTSYM  CHAR(12)     NOT NULL DEFAULT '',
    is_gs1      FOR COLUMN GTBTGS1  CHAR(1)      NOT NULL DEFAULT 'N',
    check_ok    FOR COLUMN GTBTCHK  CHAR(1)      NOT NULL DEFAULT 'N',
    sku         FOR COLUMN GTBTSKU  CHAR(15)     NOT NULL DEFAULT '',
    location_id FOR COLUMN GTBTLOC  CHAR(10)     NOT NULL DEFAULT '',
    sscc        FOR COLUMN GTBTSCC  CHAR(18)     NOT NULL DEFAULT '',
    gtin        FOR COLUMN GTBTGTN  CHAR(14)     NOT NULL DEFAULT '',
    lot         FOR COLUMN GTBTLOT  CHAR(20)     NOT NULL DEFAULT '',
    expiry      FOR COLUMN GTBTEXP  CHAR(6)      NOT NULL DEFAULT '',
    qty_count   FOR COLUMN GTBTQTY  DECIMAL(8,0) NOT NULL DEFAULT 0,
    descr       FOR COLUMN GTBTDSC  VARCHAR(60)  NOT NULL DEFAULT '',
    expected    FOR COLUMN GTBTEXPK CHAR(6)      NOT NULL DEFAULT '',
    PRIMARY KEY (test_no)
)
RCDFMT GTBARTSTR;

CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTBARTST) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');
