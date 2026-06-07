     H*=====================================================================
     H* STRTVDTA - Retrieve Secured Data stub (PHASE 3 SCAFFOLDING).
     H* Returns blank data. Real program reads encrypted user/pwd store.
     H*=====================================================================
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)

     D StRtvDta        PR                  extpgm('STRTVDTA')
     D  inCompany#                    3  0
     D  inType                        1
     D  inTypeKey                    50
     D  inNumber1                     1
     D  outData1                    256
     D  inNumber2                     1
     D  outData2                    256
     D StRtvDta        PI
     D  inCompany#                    3  0
     D  inType                        1
     D  inTypeKey                    50
     D  inNumber1                     1
     D  outData1                    256
     D  inNumber2                     1
     D  outData2                    256
      /free
       outData1 = ' ';
       outData2 = ' ';
       *inLR = *On;
       return;
      /end-free
