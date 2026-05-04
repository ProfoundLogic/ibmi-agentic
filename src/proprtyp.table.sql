CREATE TABLE PROPRTYP (
  PGUID    INTEGER       NOT NULL GENERATED ALWAYS AS IDENTITY
                                  (START WITH 1, INCREMENT BY 1),
  PSTRNO   CHAR(10)      NOT NULL DEFAULT '',
  PSTRNAM  CHAR(50)      NOT NULL DEFAULT '',
  PUNIT    CHAR(10)      NOT NULL DEFAULT '',
  PBORO    CHAR(20)      NOT NULL DEFAULT '',
  PCITY    CHAR(30)      NOT NULL DEFAULT '',
  PSTATE   CHAR(2)       NOT NULL DEFAULT '',
  PZIP     CHAR(10)      NOT NULL DEFAULT '',
  PBEDS    SMALLINT      NOT NULL DEFAULT 0,
  PBATHS   DECIMAL(3,1)  NOT NULL DEFAULT 0,
  PSQFT    INTEGER       NOT NULL DEFAULT 0,
  PRENT    DECIMAL(9,2)  NOT NULL DEFAULT 0,
  PKITCHN  CHAR(20)      NOT NULL DEFAULT '',
  PLAUNDR  CHAR(20)      NOT NULL DEFAULT '',
  PHEAT    CHAR(1)       NOT NULL DEFAULT 'N',
  PUTILS   CHAR(1)       NOT NULL DEFAULT 'N',
  PPETS    CHAR(20)      NOT NULL DEFAULT '',
  PAVAILD  DATE                   DEFAULT NULL,
  PCREATED TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
  CONSTRAINT PROPRTYP_PK PRIMARY KEY (PGUID)
);

LABEL ON TABLE PROPRTYP IS 'Properties';

LABEL ON COLUMN PROPRTYP (
  PGUID    IS 'Property            ID',
  PSTRNO   IS 'Street              Number',
  PSTRNAM  IS 'Street              Name',
  PUNIT    IS 'Unit/Apt',
  PBORO    IS 'Borough',
  PCITY    IS 'City',
  PSTATE   IS 'State',
  PZIP     IS 'Zip Code',
  PBEDS    IS 'Bedrooms',
  PBATHS   IS 'Bathrooms',
  PSQFT    IS 'Square              Feet',
  PRENT    IS 'Monthly             Rent',
  PKITCHN  IS 'Kitchen             Level',
  PLAUNDR  IS 'Laundry',
  PHEAT    IS 'Heat                Included',
  PUTILS   IS 'Utilities           Included',
  PPETS    IS 'Pet                 Policy',
  PAVAILD  IS 'Available           Date',
  PCREATED IS 'Created             Timestamp'
);

LABEL ON COLUMN PROPRTYP (
  PGUID    TEXT IS 'Property ID (auto-generated identity)',
  PSTRNO   TEXT IS 'Street Number',
  PSTRNAM  TEXT IS 'Street Name',
  PUNIT    TEXT IS 'Unit or Apartment Number',
  PBORO    TEXT IS 'Borough',
  PCITY    TEXT IS 'City',
  PSTATE   TEXT IS 'State Code',
  PZIP     TEXT IS 'Zip/Postal Code',
  PBEDS    TEXT IS 'Number of Bedrooms',
  PBATHS   TEXT IS 'Number of Bathrooms',
  PSQFT    TEXT IS 'Square Footage',
  PRENT    TEXT IS 'Monthly Rent',
  PKITCHN  TEXT IS 'Kitchen Appointment Level (BASIC/STANDARD/PREMIUM)',
  PLAUNDR  TEXT IS 'Laundry (IN_UNIT/HOOKUPS/BUILDING/NONE)',
  PHEAT    TEXT IS 'Heat Included Y/N',
  PUTILS   TEXT IS 'Utilities Included Y/N',
  PPETS    TEXT IS 'Pet Policy',
  PAVAILD  TEXT IS 'Available Date',
  PCREATED TEXT IS 'Created Timestamp'
);
