     H*=====================================================================
     H* W4020R - ChangeFEIN stub (PHASE 3 SCAFFOLDING).
     H* No-op. Real program would launch the FEIN-change dialog.
     H*=====================================================================
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)

     D ChangeFEIN      PR                  extpgm('W4020R')
     D @@kl                           1
     D @@lc                           1
     D @@mode                         1
     D @@rrn2                         9  0
     D ChangeFEIN      PI
     D @@kl                           1
     D @@lc                           1
     D @@mode                         1
     D @@rrn2                         9  0
      /free
       // Stub - leave parms unchanged
       *inLR = *On;
       return;
      /end-free
