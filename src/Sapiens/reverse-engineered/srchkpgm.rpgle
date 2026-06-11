     H*=====================================================================
     H* SRCHKPGM - ChkStack stub (PHASE 3 SCAFFOLDING).
     H* Always returns qqFound='0'. Real program walks the call stack.
     H*=====================================================================
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)

     D ChkStack        PR                  extpgm('SRCHKPGM')
     D  qqPgmName                    10    const
     D  qqFound                       1
     D ChkStack        PI
     D  qqPgmName                    10    const
     D  qqFound                       1
      /free
       qqFound = '0';
       *inLR = *On;
       return;
      /end-free
