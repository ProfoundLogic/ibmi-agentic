CREATE TABLE TENANTP (
  TGUID    INTEGER       NOT NULL GENERATED ALWAYS AS IDENTITY
                                  (START WITH 1, INCREMENT BY 1),
  TFNAME   CHAR(30)      NOT NULL DEFAULT '',
  TMNAME   CHAR(20)      NOT NULL DEFAULT '',
  TLNAME   CHAR(30)      NOT NULL DEFAULT '',
  TDOB     DATE                   DEFAULT NULL,
  TSSN4    CHAR(4)       NOT NULL DEFAULT '',
  TADDR1   CHAR(40)      NOT NULL DEFAULT '',
  TADDR2   CHAR(40)      NOT NULL DEFAULT '',
  TCITY    CHAR(30)      NOT NULL DEFAULT '',
  TSTATE   CHAR(2)       NOT NULL DEFAULT '',
  TZIP     CHAR(10)      NOT NULL DEFAULT '',
  TPHONE   CHAR(15)      NOT NULL DEFAULT '',
  TEMAIL   CHAR(60)      NOT NULL DEFAULT '',
  THHSIZE  SMALLINT      NOT NULL DEFAULT 0,
  TINCOME  DECIMAL(11,2) NOT NULL DEFAULT 0,
  TEMPLOY  CHAR(60)      NOT NULL DEFAULT '',
  TECNAME  CHAR(60)      NOT NULL DEFAULT '',
  TECPHN   CHAR(15)      NOT NULL DEFAULT '',
  TCREATED TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
  CONSTRAINT TENANTP_PK PRIMARY KEY (TGUID)
);

LABEL ON TABLE TENANTP IS 'Tenants';

LABEL ON COLUMN TENANTP (
  TGUID    IS 'Tenant              ID',
  TFNAME   IS 'First               Name',
  TMNAME   IS 'Middle              Name',
  TLNAME   IS 'Last                Name',
  TDOB     IS 'Date of             Birth',
  TSSN4    IS 'SSN                 Last 4',
  TADDR1   IS 'Address             Line 1',
  TADDR2   IS 'Address             Line 2',
  TCITY    IS 'City',
  TSTATE   IS 'State',
  TZIP     IS 'Zip Code',
  TPHONE   IS 'Phone',
  TEMAIL   IS 'Email',
  THHSIZE  IS 'Household           Size',
  TINCOME  IS 'Annual              Income',
  TEMPLOY  IS 'Employer',
  TECNAME  IS 'Emergency           Contact Name',
  TECPHN   IS 'Emergency           Contact Phone',
  TCREATED IS 'Created             Timestamp'
);

LABEL ON COLUMN TENANTP (
  TGUID    TEXT IS 'Tenant ID (auto-generated identity)',
  TFNAME   TEXT IS 'First Name',
  TMNAME   TEXT IS 'Middle Name',
  TLNAME   TEXT IS 'Last Name',
  TDOB     TEXT IS 'Date of Birth',
  TSSN4    TEXT IS 'Last 4 of SSN',
  TADDR1   TEXT IS 'Current Address Line 1',
  TADDR2   TEXT IS 'Current Address Line 2',
  TCITY    TEXT IS 'City',
  TSTATE   TEXT IS 'State Code',
  TZIP     TEXT IS 'Zip/Postal Code',
  TPHONE   TEXT IS 'Phone Number',
  TEMAIL   TEXT IS 'Email Address',
  THHSIZE  TEXT IS 'Household Size',
  TINCOME  TEXT IS 'Annual Income',
  TEMPLOY  TEXT IS 'Employer',
  TECNAME  TEXT IS 'Emergency Contact Name',
  TECPHN   TEXT IS 'Emergency Contact Phone',
  TCREATED TEXT IS 'Created Timestamp'
);
