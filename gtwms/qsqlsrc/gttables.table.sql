--  GT Warehouse Mobile -- table definitions
--
--  Modern SQL DDL throughout: CREATE TABLE with declarative constraints, long
--  SQL names with short FOR COLUMN system names so RPG stays readable, and
--  journalling started on every table as it is created.
--
--  Conventions and platform notes:
--    * FOR SYSTEM NAME / FOR COLUMN are only specified where the SQL name is
--      longer than 10 characters or differs -- specifying one redundantly is
--      rejected with SQL7029.
--    * Audit columns that use DEFAULT USER must be VARCHAR(18).
--    * GENERATED ALWAYS AS (expression) does not work on this platform, so all
--      derived values live in views (gtviews.view.sql) or in RPG.
--      GENERATED ALWAYS AS IDENTITY is fine and is used for surrogate keys.
--    * Tables are dropped and recreated on every build. Demo data is generated,
--      so there is nothing to preserve.
--
--  The library is named exactly once, to put it on the job's library list.
--  Every CL command afterwards resolves through *LIBL. See gtjrn.table.sql.

BEGIN
    DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN END;
    CALL QSYS2.QCMDEXC('ADDLIBLE LIB(TIGERPOC) POSITION(*FIRST)');
END;

--  Dropped children-first so referential constraints never block a rebuild.
DROP TABLE IF EXISTS GTIMAGE;
DROP TABLE IF EXISTS GTCOUNTDTL;
DROP TABLE IF EXISTS GTCOUNTHDR;
DROP TABLE IF EXISTS GTMOVEMENT;
DROP TABLE IF EXISTS GTRCPTDTL;
DROP TABLE IF EXISTS GTRCPTHDR;
DROP TABLE IF EXISTS GTPALLET;
DROP TABLE IF EXISTS GTINVBAL;
DROP TABLE IF EXISTS GTBARCODE;
DROP TABLE IF EXISTS GTLOCATION;
DROP TABLE IF EXISTS GTITEM;
DROP TABLE IF EXISTS GTOPERATOR;
DROP TABLE IF EXISTS GTREASON;


-- =====================================================================
--  GTREASON -- bilingual reason codes.
--  photo_required drives the "a photo is mandatory here" rule declaratively
--  rather than hard-coding thresholds into RPG.
-- =====================================================================
CREATE TABLE GTREASON (
    reason_code     FOR COLUMN GTRSNCD   CHAR(4)      NOT NULL,
    reason_type     FOR COLUMN GTRSNTP   CHAR(4)      NOT NULL,
    reason_desc_en  FOR COLUMN GTRSNEN   VARCHAR(40)  NOT NULL DEFAULT '',
    reason_desc_fr  FOR COLUMN GTRSNFR   VARCHAR(40)  NOT NULL DEFAULT '',
    photo_required  FOR COLUMN GTRSNPHO  CHAR(1)      NOT NULL DEFAULT 'N',
    needs_approval  FOR COLUMN GTRSNAPP  CHAR(1)      NOT NULL DEFAULT 'N',
    sort_order      FOR COLUMN GTRSNSEQ  SMALLINT     NOT NULL DEFAULT 0,
    CONSTRAINT GTREASON_PK PRIMARY KEY (reason_code),
    CONSTRAINT GTREASON_PH CHECK (photo_required IN ('Y','N')),
    CONSTRAINT GTREASON_AP CHECK (needs_approval IN ('Y','N'))
)
RCDFMT GTREASONR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTREASON) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTOPERATOR -- warehouse operators. Shared devices badge in and out, so
--  operator identity lives here rather than in the IBM i user profile.
-- =====================================================================
CREATE TABLE GTOPERATOR (
    operator_id     FOR COLUMN GTOPRID   CHAR(8)      NOT NULL,
    badge_barcode   FOR COLUMN GTOPRBDG  VARCHAR(20)  NOT NULL DEFAULT '',
    full_name       FOR COLUMN GTOPRNAM  VARCHAR(40)  NOT NULL DEFAULT '',
    lang_pref       FOR COLUMN GTOPRLNG  CHAR(2)      NOT NULL DEFAULT 'EN',
    default_zone    FOR COLUMN GTOPRZON  CHAR(4)      NOT NULL DEFAULT '',
    large_touch     FOR COLUMN GTOPRTCH  CHAR(1)      NOT NULL DEFAULT 'N',
    operator_role   FOR COLUMN GTOPRROL  CHAR(4)      NOT NULL DEFAULT 'OPER',
    active_flag     FOR COLUMN GTOPRACT  CHAR(1)      NOT NULL DEFAULT 'Y',
    CONSTRAINT GTOPERATOR_PK PRIMARY KEY (operator_id),
    CONSTRAINT GTOPERATOR_LN CHECK (lang_pref IN ('EN','FR')),
    CONSTRAINT GTOPERATOR_TC CHECK (large_touch IN ('Y','N'))
)
RCDFMT GTOPERR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTOPERATOR) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTITEM -- item master. Bilingual descriptions sit side by side and the
--  RPG program picks the column matching the operator's language.
-- =====================================================================
CREATE TABLE GTITEM (
    sku             FOR COLUMN GTSKU     CHAR(15)      NOT NULL,
    description_en  FOR COLUMN GTDSCEN   VARCHAR(60)   NOT NULL DEFAULT '',
    description_fr  FOR COLUMN GTDSCFR   VARCHAR(60)   NOT NULL DEFAULT '',
    department      FOR COLUMN GTDEPT    CHAR(4)       NOT NULL DEFAULT '',
    uom             FOR COLUMN GTUOM     CHAR(3)       NOT NULL DEFAULT 'EA',
    case_pack       FOR COLUMN GTCASEPK  INTEGER       NOT NULL DEFAULT 1,
    weight_kg       FOR COLUMN GTWEIGHT  DECIMAL(9,3)  NOT NULL DEFAULT 0,
    length_cm       FOR COLUMN GTLENGTH  DECIMAL(7,1)  NOT NULL DEFAULT 0,
    width_cm        FOR COLUMN GTWIDTH   DECIMAL(7,1)  NOT NULL DEFAULT 0,
    height_cm       FOR COLUMN GTHEIGHT  DECIMAL(7,1)  NOT NULL DEFAULT 0,
    retail_price    FOR COLUMN GTRETAIL  DECIMAL(9,2)  NOT NULL DEFAULT 0,
    item_status     FOR COLUMN GTITMSTS  CHAR(1)       NOT NULL DEFAULT 'A',
    created_by      FOR COLUMN GTITMCBY  VARCHAR(18)   NOT NULL DEFAULT USER,
    created_ts      FOR COLUMN GTITMCTS  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    CONSTRAINT GTITEM_PK PRIMARY KEY (sku),
    CONSTRAINT GTITEM_ST CHECK (item_status IN ('A','I','D'))
)
RCDFMT GTITEMR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTITEM) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTBARCODE -- every barcode that resolves to an item. One item has many:
--  a retail UPC, a case ITF-14, sometimes an EAN. This is the hot path --
--  every single scan in the application lands here.
--
--  barcode_level: EA = consumer unit, CS = case, PL = pallet.
-- =====================================================================
CREATE TABLE GTBARCODE (
    barcode         FOR COLUMN GTBARCD   VARCHAR(20)   NOT NULL,
    sku             FOR COLUMN GTBARSKU  CHAR(15)      NOT NULL,
    symbology       FOR COLUMN GTBARSYM  CHAR(12)      NOT NULL DEFAULT '',
    barcode_level   FOR COLUMN GTBARLVL  CHAR(2)       NOT NULL DEFAULT 'EA',
    pack_qty        FOR COLUMN GTBARQTY  INTEGER       NOT NULL DEFAULT 1,
    CONSTRAINT GTBARCODE_PK PRIMARY KEY (barcode),
    CONSTRAINT GTBARCODE_FK FOREIGN KEY (sku)
        REFERENCES GTITEM (sku) ON DELETE CASCADE,
    CONSTRAINT GTBARCODE_LV CHECK (barcode_level IN ('EA','CS','PL'))
)
RCDFMT GTBARCODER;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTBARCODE) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTLOCATION -- warehouse locations.
--  location_type: PICK = pick face, BULK = bulk racking, STAG = staging,
--                 RETN = returns, DAMG = damage hold.
--  min_qty / max_qty drive the replenishment suggestion on the pick face.
-- =====================================================================
CREATE TABLE GTLOCATION (
    location_id     FOR COLUMN GTLOCID   CHAR(10)      NOT NULL,
    zone            FOR COLUMN GTLOCZON  CHAR(4)       NOT NULL DEFAULT '',
    location_type   FOR COLUMN GTLOCTYP  CHAR(4)       NOT NULL DEFAULT 'PICK',
    aisle           FOR COLUMN GTLOCASL  CHAR(3)       NOT NULL DEFAULT '',
    bay             FOR COLUMN GTLOCBAY  CHAR(3)       NOT NULL DEFAULT '',
    level_id        FOR COLUMN GTLOCLVL  CHAR(2)       NOT NULL DEFAULT '',
    capacity_units  FOR COLUMN GTLOCCAP  INTEGER       NOT NULL DEFAULT 0,
    min_qty         FOR COLUMN GTLOCMIN  DECIMAL(11,2) NOT NULL DEFAULT 0,
    max_qty         FOR COLUMN GTLOCMAX  DECIMAL(11,2) NOT NULL DEFAULT 0,
    active_flag     FOR COLUMN GTLOCACT  CHAR(1)       NOT NULL DEFAULT 'Y',
    CONSTRAINT GTLOCATION_PK PRIMARY KEY (location_id),
    CONSTRAINT GTLOCATION_TY CHECK (location_type IN ('PICK','BULK','STAG','RETN','DAMG')),
    CONSTRAINT GTLOCATION_AC CHECK (active_flag IN ('Y','N'))
)
RCDFMT GTLOCATNR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTLOCATION) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTINVBAL -- on-hand balance by item and location. The table every
--  inventory move updates, under commitment control, both sides at once.
-- =====================================================================
CREATE TABLE GTINVBAL (
    sku             FOR COLUMN GTINVSKU  CHAR(15)      NOT NULL,
    location_id     FOR COLUMN GTINVLOC  CHAR(10)      NOT NULL,
    qty_on_hand     FOR COLUMN GTINVQOH  DECIMAL(11,2) NOT NULL DEFAULT 0,
    qty_allocated   FOR COLUMN GTINVQAL  DECIMAL(11,2) NOT NULL DEFAULT 0,
    stock_status    FOR COLUMN GTINVSTS  CHAR(4)       NOT NULL DEFAULT 'OK',
    last_count_ts   FOR COLUMN GTINVCTS  TIMESTAMP,
    updated_by      FOR COLUMN GTINVUBY  VARCHAR(18)   NOT NULL DEFAULT USER,
    updated_ts      FOR COLUMN GTINVUTS  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    CONSTRAINT GTINVBAL_PK PRIMARY KEY (sku, location_id),
    CONSTRAINT GTINVBAL_FI FOREIGN KEY (sku)
        REFERENCES GTITEM (sku) ON DELETE CASCADE,
    CONSTRAINT GTINVBAL_FL FOREIGN KEY (location_id)
        REFERENCES GTLOCATION (location_id) ON DELETE CASCADE,
    CONSTRAINT GTINVBAL_QT CHECK (qty_on_hand >= 0)
)
RCDFMT GTINVBALR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTINVBAL) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTPALLET -- pallet licence plates. sscc holds the SSCC-18 read from the
--  GS1-128 pallet label, check digit included and valid.
-- =====================================================================
CREATE TABLE GTPALLET (
    sscc            FOR COLUMN GTPLTSCC  CHAR(18)      NOT NULL,
    pallet_status   FOR COLUMN GTPLTSTS  CHAR(4)       NOT NULL DEFAULT 'OPEN',
    location_id     FOR COLUMN GTPLTLOC  CHAR(10)      NOT NULL DEFAULT '',
    vendor_id       FOR COLUMN GTPLTVND  CHAR(8)       NOT NULL DEFAULT '',
    arrived_ts      FOR COLUMN GTPLTATS  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    CONSTRAINT GTPALLET_PK PRIMARY KEY (sscc),
    CONSTRAINT GTPALLET_ST CHECK (pallet_status IN ('OPEN','RECV','PUTA','CLSD'))
)
RCDFMT GTPALLETR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTPALLET) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTRCPTHDR -- receipt header, one per inbound pallet.
-- =====================================================================
CREATE TABLE GTRCPTHDR (
    receipt_id      FOR COLUMN GTRCPID   CHAR(10)      NOT NULL,
    po_number       FOR COLUMN GTRCPPO   CHAR(12)      NOT NULL DEFAULT '',
    vendor_id       FOR COLUMN GTRCPVND  CHAR(8)       NOT NULL DEFAULT '',
    vendor_name     FOR COLUMN GTRCPVNM  VARCHAR(40)   NOT NULL DEFAULT '',
    sscc            FOR COLUMN GTRCPSCC  CHAR(18)      NOT NULL DEFAULT '',
    door_id         FOR COLUMN GTRCPDOR  CHAR(4)       NOT NULL DEFAULT '',
    receipt_status  FOR COLUMN GTRCPSTS  CHAR(4)       NOT NULL DEFAULT 'OPEN',
    expected_ts     FOR COLUMN GTRCPETS  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    received_by     FOR COLUMN GTRCPRBY  VARCHAR(18)   NOT NULL DEFAULT '',
    received_ts     FOR COLUMN GTRCPRTS  TIMESTAMP,
    signature_img   FOR COLUMN GTRCPSIG  INTEGER       NOT NULL DEFAULT 0,
    CONSTRAINT GTRCPTHDR_PK PRIMARY KEY (receipt_id),
    CONSTRAINT GTRCPTHDR_ST CHECK (receipt_status IN ('OPEN','PART','POST'))
)
RCDFMT GTRCPTHDRR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTRCPTHDR) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTRCPTDTL -- expected and received quantities, one row per item on the
--  receipt. line_status: OPEN untouched, MTCH matched, SHRT short,
--  OVER overage, DAMG damaged.
-- =====================================================================
CREATE TABLE GTRCPTDTL (
    receipt_id      FOR COLUMN GTRCDID   CHAR(10)      NOT NULL,
    line_no         FOR COLUMN GTRCDLIN  INTEGER       NOT NULL,
    sku             FOR COLUMN GTRCDSKU  CHAR(15)      NOT NULL,
    qty_expected    FOR COLUMN GTRCDQEX  DECIMAL(11,2) NOT NULL DEFAULT 0,
    qty_received    FOR COLUMN GTRCDQRC  DECIMAL(11,2) NOT NULL DEFAULT 0,
    reason_code     FOR COLUMN GTRCDRSN  CHAR(4)       NOT NULL DEFAULT '',
    scanned_barcode FOR COLUMN GTRCDBAR  VARCHAR(20)   NOT NULL DEFAULT '',
    line_status     FOR COLUMN GTRCDSTS  CHAR(4)       NOT NULL DEFAULT 'OPEN',
    photo_count     FOR COLUMN GTRCDPHC  SMALLINT      NOT NULL DEFAULT 0,
    updated_by      FOR COLUMN GTRCDUBY  VARCHAR(18)   NOT NULL DEFAULT '',
    updated_ts      FOR COLUMN GTRCDUTS  TIMESTAMP,
    CONSTRAINT GTRCPTDTL_PK PRIMARY KEY (receipt_id, line_no),
    CONSTRAINT GTRCPTDTL_FH FOREIGN KEY (receipt_id)
        REFERENCES GTRCPTHDR (receipt_id) ON DELETE CASCADE,
    CONSTRAINT GTRCPTDTL_FI FOREIGN KEY (sku)
        REFERENCES GTITEM (sku) ON DELETE CASCADE,
    CONSTRAINT GTRCPTDTL_ST CHECK (line_status IN ('OPEN','MTCH','SHRT','OVER','DAMG'))
)
RCDFMT GTRCPTDTLR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTRCPTDTL) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTMOVEMENT -- the audit spine. Every receipt line, every move, every
--  count adjustment writes one row here inside the same unit of work as
--  the balance it changed.
--
--  from_location / to_location are deliberately NOT foreign keys: a receipt
--  has no from-location and an adjustment has no to-location, and blank
--  would never satisfy the constraint.
-- =====================================================================
CREATE TABLE GTMOVEMENT (
    movement_id     FOR COLUMN GTMOVID   INTEGER       NOT NULL
                        GENERATED ALWAYS AS IDENTITY (START WITH 1 INCREMENT BY 1),
    movement_type   FOR COLUMN GTMOVTYP  CHAR(4)       NOT NULL,
    sku             FOR COLUMN GTMOVSKU  CHAR(15)      NOT NULL,
    from_location   FOR COLUMN GTMOVFRM  CHAR(10)      NOT NULL DEFAULT '',
    to_location     FOR COLUMN GTMOVTO   CHAR(10)      NOT NULL DEFAULT '',
    qty             FOR COLUMN GTMOVQTY  DECIMAL(11,2) NOT NULL DEFAULT 0,
    reason_code     FOR COLUMN GTMOVRSN  CHAR(4)       NOT NULL DEFAULT '',
    source_doc      FOR COLUMN GTMOVDOC  CHAR(10)      NOT NULL DEFAULT '',
    moved_by        FOR COLUMN GTMOVUBY  VARCHAR(18)   NOT NULL DEFAULT USER,
    moved_ts        FOR COLUMN GTMOVTS   TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    CONSTRAINT GTMOVEMENT_PK PRIMARY KEY (movement_id),
    CONSTRAINT GTMOVEMENT_FI FOREIGN KEY (sku)
        REFERENCES GTITEM (sku) ON DELETE CASCADE,
    CONSTRAINT GTMOVEMENT_TY CHECK (movement_type IN ('RECV','MOVE','ADJC','PUTA','PICK'))
)
RCDFMT GTMOVER;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTMOVEMENT) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTCOUNTHDR -- a cycle count assignment against one location.
-- =====================================================================
CREATE TABLE GTCOUNTHDR (
    count_id        FOR COLUMN GTCNTID   CHAR(10)      NOT NULL,
    location_id     FOR COLUMN GTCNTLOC  CHAR(10)      NOT NULL,
    count_status    FOR COLUMN GTCNTSTS  CHAR(4)       NOT NULL DEFAULT 'OPEN',
    assigned_to     FOR COLUMN GTCNTASG  CHAR(8)       NOT NULL DEFAULT '',
    counted_by      FOR COLUMN GTCNTCBY  VARCHAR(18)   NOT NULL DEFAULT '',
    counted_ts      FOR COLUMN GTCNTCTS  TIMESTAMP,
    approved_by     FOR COLUMN GTCNTABY  VARCHAR(18)   NOT NULL DEFAULT '',
    approved_ts     FOR COLUMN GTCNTATS  TIMESTAMP,
    CONSTRAINT GTCOUNTHDR_PK PRIMARY KEY (count_id),
    CONSTRAINT GTCOUNTHDR_FL FOREIGN KEY (location_id)
        REFERENCES GTLOCATION (location_id) ON DELETE CASCADE,
    CONSTRAINT GTCOUNTHDR_ST CHECK (count_status IN ('OPEN','CNTD','APPR','POST'))
)
RCDFMT GTCNTHDRR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTCOUNTHDR) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTCOUNTDTL -- blind count lines. qty_expected is populated when the
--  count is generated but is never sent to the counting screen; the
--  operator sees it only on the variance review.
-- =====================================================================
CREATE TABLE GTCOUNTDTL (
    count_id        FOR COLUMN GTCNDID   CHAR(10)      NOT NULL,
    line_no         FOR COLUMN GTCNDLIN  INTEGER       NOT NULL,
    sku             FOR COLUMN GTCNDSKU  CHAR(15)      NOT NULL,
    qty_expected    FOR COLUMN GTCNDQEX  DECIMAL(11,2) NOT NULL DEFAULT 0,
    qty_counted     FOR COLUMN GTCNDQCT  DECIMAL(11,2) NOT NULL DEFAULT 0,
    counted_flag    FOR COLUMN GTCNDCFL  CHAR(1)       NOT NULL DEFAULT 'N',
    reason_code     FOR COLUMN GTCNDRSN  CHAR(4)       NOT NULL DEFAULT '',
    photo_count     FOR COLUMN GTCNDPHC  SMALLINT      NOT NULL DEFAULT 0,
    CONSTRAINT GTCOUNTDTL_PK PRIMARY KEY (count_id, line_no),
    CONSTRAINT GTCOUNTDTL_FH FOREIGN KEY (count_id)
        REFERENCES GTCOUNTHDR (count_id) ON DELETE CASCADE,
    CONSTRAINT GTCOUNTDTL_FI FOREIGN KEY (sku)
        REFERENCES GTITEM (sku) ON DELETE CASCADE,
    CONSTRAINT GTCOUNTDTL_CF CHECK (counted_flag IN ('Y','N'))
)
RCDFMT GTCNTDTLR;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTCOUNTDTL) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');


-- =====================================================================
--  GTIMAGE -- every image in the system: item catalogue photos, receiving
--  damage photos, count variance photos, and captured signatures.
--
--  ref_type + ref_key is deliberately generic so that one table, one upload
--  endpoint and one viewer component serve all of them:
--      ITEM  -> ref_key = sku
--      RCPT  -> ref_key = receipt_id + line_no
--      CNT   -> ref_key = count_id + line_no
--      SIGN  -> ref_key = receipt_id
--
--  thumb_data is generated on the client alongside the full image, so IBM i
--  never has to manipulate pixels. List screens request the thumbnail only.
--
--  Journaled to GTJRNIMG, NOT GTJRN -- BLOBs would otherwise swamp the
--  transactional receiver that Supervisor View reads its audit trail from.
-- =====================================================================
CREATE TABLE GTIMAGE (
    image_id        FOR COLUMN GTIMGID   INTEGER       NOT NULL
                        GENERATED ALWAYS AS IDENTITY (START WITH 1 INCREMENT BY 1),
    ref_type        FOR COLUMN GTIMGRTP  CHAR(4)       NOT NULL,
    ref_key         FOR COLUMN GTIMGRKY  CHAR(20)      NOT NULL,
    seq_no          FOR COLUMN GTIMGSEQ  SMALLINT      NOT NULL DEFAULT 1,
    mime_type       FOR COLUMN GTIMGMIM  VARCHAR(30)   NOT NULL DEFAULT 'image/jpeg',
    width_px        FOR COLUMN GTIMGW    INTEGER       NOT NULL DEFAULT 0,
    height_px       FOR COLUMN GTIMGH    INTEGER       NOT NULL DEFAULT 0,
    byte_size       FOR COLUMN GTIMGSZ   INTEGER       NOT NULL DEFAULT 0,
    image_data      FOR COLUMN GTIMGDTA  BLOB(4M)      NOT NULL,
    thumb_data      FOR COLUMN GTIMGTHB  BLOB(512K)    NOT NULL,
    sha256_hex      FOR COLUMN GTIMGSHA  CHAR(64)      NOT NULL DEFAULT '',
    caption_en      FOR COLUMN GTIMGCEN  VARCHAR(60)   NOT NULL DEFAULT '',
    caption_fr      FOR COLUMN GTIMGCFR  VARCHAR(60)   NOT NULL DEFAULT '',
    captured_by     FOR COLUMN GTIMGCBY  VARCHAR(18)   NOT NULL DEFAULT USER,
    captured_ts     FOR COLUMN GTIMGCTS  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    committed_flag  FOR COLUMN GTIMGCMT  CHAR(1)       NOT NULL DEFAULT 'N',
    CONSTRAINT GTIMAGE_PK PRIMARY KEY (image_id),
    CONSTRAINT GTIMAGE_RT CHECK (ref_type IN ('ITEM','RCPT','CNT','SIGN','DMG'))
)
RCDFMT GTIMAGER;
CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTIMAGE) JRN(*LIBL/GTJRNIMG) IMAGES(*BOTH)');
