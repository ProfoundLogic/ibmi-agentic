**free

// ---------------------------------------------------------------------
// Prototypes:  reqauto  (CoderFlow auto-approval hook)
// Module:      perp
// Purpose:     Integration point for CoderFlow. Scores a submitted
//              requisition and auto-approves it when the score and
//              total both pass the company's configured thresholds.
// Epic:        PERP-6 (PERP-36)
// ---------------------------------------------------------------------

// reqauto_score — no-op placeholder scorer. Returns a fixed confidence_pct
// (0-100) for the given requisition. Today this is a stand-in for the
// real CoderFlow scoring call that will replace it; it does not inspect
// the requisition's content. Returns -1 and fills errmsg if the
// requisition is not found.
dcl-pr reqauto_score packed(5:2);
  company   char(3)      const;
  reqnbr    int(20)      const;
  errmsg    varchar(80);
end-pr;

// reqauto_evaluate — score a requisition and auto-approve it if the score
// exceeds company_config('approval.auto_threshold') AND
// total_estimated_cost is under company_config('approval.auto_max_amount').
// Always writes confidence_pct back to the requisition, whether or not
// it clears the bar. On auto-approval, stamps approved_by='CODERFLOW',
// approved_at=now, approval_source_code='CODERFLOW', and a placeholder
// approval_notes explanation (a real explanation string will come from
// CoderFlow later).
//
// Returns: 1  = auto-approved
//          0  = scored but not auto-approved (stays SUBMITTED)
//          -1 = error (requisition not found, missing config, or a
//               non-DRAFT/SUBMITTED status); errmsg is filled
dcl-pr reqauto_evaluate int(10);
  company   char(3)      const;
  reqnbr    int(20)      const;
  errmsg    varchar(80);
end-pr;
