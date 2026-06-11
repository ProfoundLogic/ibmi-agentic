     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)

    H/TITLE SA044R - API to Receive SS# and Mask Type
     H*********************************************************************
     H*  Created:  08/14/12    C:282   Project: WC2030
    H*********************************************************************
     H* REVISED:
     H*
     H* 10/25/12 C:282 PROJECT WC2030 Priss 24
     H*                Changing outSS# size from 9A to 11A.
    H*********************************************************************
     d inSS#           s              9
     d inMask          s              1
      *** Beg Chg *** 10/25/12 ******************************************
     d***** outSS#          s              9
     d outSS#          s             11
      *** End Chg *** 10/25/12 ******************************************

     d                 ds
     d  intSS#                 1      9
     d  xxSS1                  1      3
     d  xxSS2                  4      5
     d  xxSS3                  6      9

     c     *Entry        Plist
     c                   Parm                    inSS#
     c                   Parm                    inMask
     c                   Parm                    outSS#

      /free
       intSS# = inSS#;
       outSS# = *blanks;
       If inMask = 'Y';
       // *** Beg Chg *** 10/25/12 ******************************************
          // outSS# = '*********';
          outSS# = '***' + '-' + '**' + '-' + '****';
       // *** End Chg *** 10/25/12 ******************************************
       ElseIf inMask = 'N';
       // *** Beg Chg *** 10/25/12 ******************************************
          // outSS# = inSS#;
          outSS# = xxSS1 + '-' + xxSS2 + '-' + xxSS3;
       // *** End Chg *** 10/25/12 ******************************************
       ElseIf inMask = '4';
       // *** Beg Chg *** 10/25/12 ******************************************
          // outSS# = '*****' + xxSS3;
          outSS# = '***' + '-' + '**'  + '-' +  xxSS3;
       // *** End Chg *** 10/25/12 ******************************************
       EndIf;

       *InLR = *On;

      /end-free
