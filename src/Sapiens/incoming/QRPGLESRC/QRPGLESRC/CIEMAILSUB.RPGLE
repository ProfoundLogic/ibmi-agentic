     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
      * Test program for STEMAILSUB
     d CiEmailSub      pr
     d  rrnType                       2
     d  rrn                           9  0
     d  subject                     255
     d CiEmailSub      pi
     d  rrnType                       2
     d  rrn                           9  0
     d  subject                     255
      /free
       subject = 'Test CIEMAILSUB return';
       Return;
      /end-free
