     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
     d inJson          s          32000a   Varying
     d inCo#           s              3  0 Inz(777)
     d inFnd           s              3  0 Inz(999)
     d inEmp#          s              9  0 Inz(697)
     d inDiv           s              5  0 Inz(0)
     d inInv#          s             10  0 Inz(13)
     d/copy wPrPmtSvc

                 inJson = '{' +
                       '"type":"billing",' +
                       '"invoice":"'  + %EditC(inInv#:'X') + '",' +
                       '"company":"'  + %EditC(inCo#:'X') + '",'  +
                       '"group":"'    + %EditC(inFnd:'X') + '",'  +
                       '"employer":"' + %EditC(inEmp#:'X') + '",'  +
                       '"division":"' + %EditC(inDiv:'X') + '",'  +
                       '"active":"true"' +
                       '}';
                 ThirdPartyInvoicing(inJson);

                 inJson = '{' +
                       '"type":"billing",' +
                       '"invoice":"7",' +
                       '"company":"777",'  +
                       '"group":"999",'  +
                       '"employer":"697",'  +
                       '"division":"00000",'  +
                       '"active":"true"' +
                       '}';
                 ThirdPartyInvoicing(inJson);

                 FinalizeThirdPartyInvoicing(inCo#:inFnd);

                 *inLR = *On;
