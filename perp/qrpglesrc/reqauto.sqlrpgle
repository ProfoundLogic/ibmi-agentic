**free

// ---------------------------------------------------------------------
// Module:  reqauto  (CoderFlow auto-approval hook)
// Purpose: See reqauto_pr.rpgle for the exported procedure contracts.
//          Callers commit/rollback the unit-of-work; this module never
//          issues COMMIT/ROLLBACK (DDL_STYLE_GUIDE.md Sec.13).
// Epic:    PERP-6 (PERP-36)
// ---------------------------------------------------------------------

ctl-opt nomain;

exec sql set option closqlcsr = *endmod;

/copy reqauto_pr.rpgle

// ---------------------------------------------------------------------
// Host-variable naming: prefixed per subprocedure (sc_/ev_) so the
// SQLRPGLE precompiler's module-wide (not subprocedure-scoped) host
// variable collection doesn't raise SQL0314 (DDL_STYLE_GUIDE.md Sec.13).
// ---------------------------------------------------------------------

// reqauto_score — no-op placeholder scorer (see prototype for contract).
dcl-proc reqauto_score export;
  dcl-pi *n packed(5:2);
    sc_company  char(3)      const;
    sc_reqnbr   int(20)      const;
    sc_errmsg   varchar(80);
  end-pi;

  dcl-s sc_cnt int(10);

  sc_errmsg = '';

  exec sql
    select count(*) into :sc_cnt
      from perpdemo.requisition_header
     where company_code = :sc_company and requisition_number = :sc_reqnbr;
  if sc_cnt = 0;
    sc_errmsg = 'reqauto_score: requisition not found';
    return -1;
  endif;

  // Placeholder score. Not a function of the requisition's content --
  // a real CoderFlow scoring call will replace this body. Chosen high
  // enough to clear the demo's default 85% threshold so the happy path
  // is exercisable end-to-end before real scoring exists.
  return 92.50;

end-proc;

// reqauto_evaluate — score + conditionally auto-approve (see prototype).
dcl-proc reqauto_evaluate export;
  dcl-pi *n int(10);
    ev_company  char(3)      const;
    ev_reqnbr   int(20)      const;
    ev_errmsg   varchar(80);
  end-pi;

  dcl-s ev_status   varchar(20);
  dcl-s ev_total    packed(15:2);
  dcl-s ev_conf     packed(5:2);
  dcl-s ev_thresh   packed(5:2);
  dcl-s ev_maxamt   packed(15:2);
  dcl-s ev_thrTxt   varchar(256);
  dcl-s ev_maxTxt   varchar(256);
  dcl-s ev_notes    varchar(500);

  ev_errmsg = '';

  exec sql
    select status_code, total_estimated_cost
      into :ev_status, :ev_total
      from perpdemo.requisition_header
     where company_code = :ev_company and requisition_number = :ev_reqnbr;
  if sqlcode <> 0;
    ev_errmsg = 'reqauto_evaluate: requisition not found';
    return -1;
  endif;

  if ev_status <> 'SUBMITTED';
    ev_errmsg = 'reqauto_evaluate: status is ' + %trim(ev_status)
              + ', expected SUBMITTED';
    return -1;
  endif;

  ev_conf = reqauto_score(ev_company : ev_reqnbr : ev_errmsg);
  if ev_conf < 0;
    return -1;
  endif;

  exec sql
    update perpdemo.requisition_header
       set confidence_pct = :ev_conf,
           updated_at     = current_timestamp,
           updated_by     = user
     where company_code = :ev_company and requisition_number = :ev_reqnbr;
  if sqlcode < 0;
    ev_errmsg = 'reqauto_evaluate: confidence_pct update failed SQLCODE='
              + %char(sqlcode);
    return -1;
  endif;

  exec sql
    select config_value into :ev_thrTxt
      from perpdemo.company_config
     where company_code = :ev_company
       and config_key    = 'approval.auto_threshold';
  if sqlcode <> 0;
    ev_errmsg = 'reqauto_evaluate: approval.auto_threshold not configured';
    return -1;
  endif;

  exec sql
    select config_value into :ev_maxTxt
      from perpdemo.company_config
     where company_code = :ev_company
       and config_key    = 'approval.auto_max_amount';
  if sqlcode <> 0;
    ev_errmsg = 'reqauto_evaluate: approval.auto_max_amount not configured';
    return -1;
  endif;

  ev_thresh = %dec(%trim(ev_thrTxt) : 5 : 2);
  ev_maxamt = %dec(%trim(ev_maxTxt) : 15 : 2);

  if ev_conf >= ev_thresh and ev_total <= ev_maxamt;
    // Placeholder explanation -- a real confidence rationale string
    // will come from CoderFlow once real scoring lands.
    ev_notes = 'Auto-approved by CoderFlow: confidence ' + %char(ev_conf)
             + '% >= threshold ' + %char(ev_thresh) + '%, total '
             + %char(ev_total) + ' <= max ' + %char(ev_maxamt) + '.';
    exec sql
      update perpdemo.requisition_header
         set status_code           = 'APPROVED',
             approved_by           = 'CODERFLOW',
             approved_at           = current_timestamp,
             approval_source_code  = 'CODERFLOW',
             approval_source_type  = 'APPRSRC',
             approval_notes        = :ev_notes,
             updated_at            = current_timestamp,
             updated_by            = user
       where company_code = :ev_company and requisition_number = :ev_reqnbr;
    if sqlcode < 0;
      ev_errmsg = 'reqauto_evaluate: auto-approve update failed SQLCODE='
                + %char(sqlcode);
      return -1;
    endif;
    return 1;
  endif;

  return 0;

end-proc;
