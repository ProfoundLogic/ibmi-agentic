-- ---------------------------------------------------------------------------
-- Seed:    010_code_master
-- Module:  perp
-- Purpose: Populate code_master with the system lookups that other epics
--          need to be able to reference. Idempotent — DELETE by
--          code_type first, then INSERT. Safe to re-run.
-- Epic:    PERP-2 (PERP-15)
-- ---------------------------------------------------------------------------

DELETE FROM code_master
 WHERE code_type IN
   ('REQSTATUS','POSTATUS','RCPSTATUS','CURRENCY',
    'PRIORITY','APPRSRC','USERROLE');

-- REQSTATUS — requisition status ------------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('REQSTATUS','DRAFT',    'Draft',     'Draft',    10),
  ('REQSTATUS','SUBMITTED','Submitted', 'Submitted',20),
  ('REQSTATUS','APPROVED', 'Approved',  'Approved', 30),
  ('REQSTATUS','REJECTED', 'Rejected',  'Rejected', 40),
  ('REQSTATUS','CONVERTED','Converted to PO','Converted',50),
  ('REQSTATUS','CANCELLED','Cancelled', 'Cancelled',60);

-- POSTATUS — purchase order status ----------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('POSTATUS','DRAFT',    'Draft',            'Draft',    10),
  ('POSTATUS','OPEN',     'Open',             'Open',     20),
  ('POSTATUS','PARTIAL',  'Partially received','Partial', 30),
  ('POSTATUS','RECEIVED', 'Fully received',   'Received', 40),
  ('POSTATUS','CLOSED',   'Closed',           'Closed',   50),
  ('POSTATUS','CANCELLED','Cancelled',        'Cancelled',60);

-- RCPSTATUS — receipt status ----------------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('RCPSTATUS','DRAFT', 'Draft',  'Draft', 10),
  ('RCPSTATUS','POSTED','Posted', 'Posted',20),
  ('RCPSTATUS','VOIDED','Voided', 'Voided',30);

-- CURRENCY — currencies (single value for now) ----------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('CURRENCY','USD','US Dollar','USD',10);

-- PRIORITY — request / order priority -------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('PRIORITY','LOW',     'Low',      'Low',     10),
  ('PRIORITY','NORMAL',  'Normal',   'Normal',  20),
  ('PRIORITY','HIGH',    'High',     'High',    30),
  ('PRIORITY','CRITICAL','Critical', 'Critical',40);

-- APPRSRC — approval source -----------------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('APPRSRC','HUMAN',         'Human approver',          'Human', 10),
  ('APPRSRC','CODERFLOW',     'CoderFlow AI approver',   'AI',    20),
  ('APPRSRC','AUTO_THRESHOLD','Auto-approved (threshold)','Auto', 30);

-- USERROLE — application role --------------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('USERROLE','BUYER',    'Buyer',              'Buyer',   10),
  ('USERROLE','RECEIVER', 'Receiver',           'Receiver',20),
  ('USERROLE','APPROVER', 'Approver',           'Approver',30),
  ('USERROLE','REQUESTER','Requester',          'Requester',40),
  ('USERROLE','ADMIN',    'System administrator','Admin',  50);
