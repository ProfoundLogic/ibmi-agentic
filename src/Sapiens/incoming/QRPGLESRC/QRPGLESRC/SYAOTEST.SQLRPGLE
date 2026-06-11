     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE   SYAOCVTR Convert files and selected funds
    H*=========================================================================
    H*                   = WORKERS COMPENSATION =
    H*=========================================================================
    H*
    H*                       DESCRIPTION:
    H*  Called by SYAOCVT
    H*
    H*
    H* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    H*=========================================================================
      * Revisions:
      *
      * 03/01/19  C:62  Project: 152WAD
      *                 -Add flag to determine if going to copy Q Files
      *                 -Add flag to determine run type
      *                 -Add flag to determine copy all payees
      *                 -Add flag to determine copy all ss#s
      *
      * 10/02/18  C:79  Support: 99/70482
      *                 Do not duplicate triggers on crtdupobj
      *
      * 09/15/17  C:33  Project: 146WGV
      *                -Update AO flag (SYFL11 pos 2)
      *                -Inactive user parm records with no x-ref
      *                -Copy PDS* user parms
      *                -Clear production files
      *
      * 08/28/17  C:33  Project: 146WGV
      *                 Convert SDCMP records
      *
      * 07/31/17  C:33  Project: 146WGV
      *                 Convert users in USER and SMSLP
    H*=========================================================================
    H*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
     D nextClm#        s              7  0
     D nextEmp#        s              9  0
     D nextQte#        s              9  0
     D nextInv#        s             10  0
     D fmLIb           s             10
     D toLib           s             10
     D strSql          s            500
     d psds          esds                  extname(szq1p)
    D*
    C*=========================================================
    C*=========================================================
    C*=========================================================
     C     *entry        plist
     C                   parm                    inFnd             3 0

     /free

       // Update WMFNP
       tolib = 'WINNER';
       fmlib = 'PDSFIL';
             // Create Alias
             strSQL = 'Create Alias QTEMP/WMFNP_FRM ' +
                'For ' + %trim(fmLIb) + '/WMFNP(WMFNP)';
             Exec SQL execute immediate :strSQL;
             // Let's get the next available counters from convert library
             Exec SQL Select FNnacs, FNnaen, FNnaqn, FNninv
                 Into :nextClm#, :nextEmp#, :nextQte#, :nextInv#
                 From WMFNP_FRM
                 Where FNfnd = :inFnd;
             Exec SQL execute immediate :strSQL;
             // Now let's apply the counter values to the production library
             strSQL = 'Update ' + %trim(toLIb) + '/WMFNP ' +
                'Set FNnacs = ' + %EditC(nextClm#:'X') + ', ' +
                '    FNnaen = ' + %EditC(nextEmp#:'X') + ', ' +
                '    FNnaqn = ' + %EditC(nextQte#:'X') + ', ' +
                '    FNninv = ' + %EditC(nextInv#:'X') + ' ' +
                'Where FNfnd = ' + %EditC(inFnd:'X');
             Exec SQL execute immediate :strSQL;

       *inlr = *on;

     /end-free
