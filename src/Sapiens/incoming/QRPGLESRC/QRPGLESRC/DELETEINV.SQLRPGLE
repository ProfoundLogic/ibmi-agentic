     h DFTACTGRP(*no) ACTGRP('QILE')
     h option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR')

     d/copy wprPmtSvc

     D @@co#           s              3a
     D @@fnd           s              3a
     D @@emp#          s              9a
     D @@div           s              5a
     D @@inv#          s             10a

     d chrJson         s          64000a   Varying Inz
     d decCo#          s              3  0
     d decFnd          s              3  0
     d invoiceType     s             20          Inz('billing')

     C     *entry        Plist
     C                   Parm                    @@co#
     C                   Parm                    @@fnd
     C                   Parm                    @@emp#
     C                   Parm                    @@div
     C                   Parm                    @@inv#

          decCo# = %Dec(@@co#:3:0);
          decFnd = %Dec(@@Fnd:3:0);

          // Inactivate Invoice by calling Third Party Invoicing
          chrJson = '{' +
                 '"type":"'     + %Trim(invoiceType)      + '   ",' +
                 '"invoice":"'  + @@inv#      + '   ",' +
                 '"company":"'  + @@co#       + '   ",' +
                 '"group":"'    + @@fnd       + '   ",' +
                 '"employer":"' + @@emp#      + '   ",' +
                 '"division":"' + @@div       + '   ",' +
                 '"active":"false",'  +
                 '"delete":"true"'   +
                 '}';
          ThirdPartyInvoicing(chrJson);

          FinalizeThirdPartyInvoicing(decCo#:decFnd);

          *inLR = *On;
