**free

// ---------------------------------------------------------------------
// Module:  docseq  (document sequence service)
// Purpose: Return the next document number for a (company_code,
//          document_type) pair.
//
//          Strategy: UPDATE ... SET current_number = current_number+1
//          under commitment control (row-locked by DB2). If the row
//          did not exist (SQLCODE=100 on the UPDATE), INSERT it at 1.
//          Otherwise read current_number back into :out — the row is
//          locked to this unit-of-work so no other job can bump it
//          between the UPDATE and the SELECT.
//
//          Callers commit/rollback the unit-of-work; this module
//          never issues COMMIT / ROLLBACK.
//
// Callers: requisition, PO, and PO-receipt entry programs.
// Epic:    PERP-2 (PERP-19)
// ---------------------------------------------------------------------

ctl-opt nomain;

// Real commitment control against PERPJRN — per DDL_STYLE_GUIDE §7.
// (Absence of SET OPTION COMMIT = *NONE is deliberate.)
exec sql set option closqlcsr = *endmod;

/copy docseq_pr.rpgle

// ---------------------------------------------------------------------
// Host-variable naming: the SQLRPGLE precompiler does NOT respect
// subprocedure scope when collecting host variables. Parameter names
// are unique per procedure ('nx_' vs 'pk_' prefix) to avoid the
// SQL0314 "host variable not unique" error that fires when two
// subprocedures share a parm name across the module.
// ---------------------------------------------------------------------

// docseq_next — bump and return the next document number.
dcl-proc docseq_next export;
  dcl-pi *n int(20);
    nx_company     char(3)      const;
    nx_doctype     varchar(20)  const;
    nx_errmsg      varchar(80);
  end-pi;

  dcl-s nx_number int(20) inz(0);

  nx_errmsg = '';

  // Bump — acquires row lock under commit(*chg).
  exec sql
    update perpdemo.document_sequence
       set current_number = current_number + 1,
           updated_at     = current_timestamp,
           updated_by     = user
     where company_code  = :nx_company
       and document_type = :nx_doctype;

  if sqlcode = 100;
    // No row yet — seed at 1.
    exec sql
      insert into perpdemo.document_sequence
        (company_code, document_type, current_number, description)
        values (:nx_company, :nx_doctype, 1, :nx_doctype);
    if sqlcode < 0;
      nx_errmsg = 'docseq_next insert: SQLCODE=' + %char(sqlcode)
                + ' SQLSTATE=' + sqlstate;
      return 0;
    endif;
    return 1;
  endif;

  if sqlcode < 0;
    nx_errmsg = 'docseq_next update: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return 0;
  endif;

  // Row was updated — read it back.
  exec sql
    select current_number
      into :nx_number
      from perpdemo.document_sequence
     where company_code  = :nx_company
       and document_type = :nx_doctype;
  if sqlcode < 0;
    nx_errmsg = 'docseq_next select: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return 0;
  endif;

  return nx_number;

end-proc;

// docseq_peek — read the current high-water mark WITHOUT bumping.
dcl-proc docseq_peek export;
  dcl-pi *n int(20);
    pk_company     char(3)      const;
    pk_doctype     varchar(20)  const;
    pk_errmsg      varchar(80);
  end-pi;

  dcl-s pk_number int(20) inz(0);

  pk_errmsg = '';

  exec sql
    select current_number
      into :pk_number
      from perpdemo.document_sequence
     where company_code  = :pk_company
       and document_type = :pk_doctype;

  if sqlcode = 100;
    return 0;
  endif;
  if sqlcode < 0;
    pk_errmsg = 'docseq_peek: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return 0;
  endif;
  return pk_number;

end-proc;
