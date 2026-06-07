     H*=====================================================================
     H* STWRTDTA - Write Secured Data stub (PHASE 3 SCAFFOLDING).
     H* No-op. Real program writes encrypted user/pwd store.
     H*=====================================================================
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)

     D StWrtDta        PR                  extpgm('STWRTDTA')
     D  inCompany#                    3  0
     D  inType                        1
     D  inTypeKey                    50
     D  inNumber1                     1
     D  inData1                     256
     D  inNumber2                     1
     D  inData2                     256
     D StWrtDta        PI
     D  inCompany#                    3  0
     D  inType                        1
     D  inTypeKey                    50
     D  inNumber1                     1
     D  inData1                     256
     D  inNumber2                     1
     D  inData2                     256
      /free
       *inLR = *On;
       return;
      /end-free
