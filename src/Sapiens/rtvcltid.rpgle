     H*=====================================================================
     H* RTVCLTID - GetClientID stub (PHASE 3 SCAFFOLDING).
     H* Returns 'STB' as a fake 3-char client ID.
     H*=====================================================================
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)

     D GetClientID     PR                  extpgm('RTVCLTID')
     D   outCltID                     3
     D GetClientID     PI
     D   outCltID                     3
      /free
       outCltID = 'STB';
       *inLR = *On;
       return;
      /end-free
