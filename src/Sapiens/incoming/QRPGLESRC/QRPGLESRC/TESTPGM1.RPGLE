     f*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     D*** begin add ***  04/10/14  *********************************************
     D*
     D*
     d x@RRN           s              9  0 inz(1694)
     D x#Type          c                   const('Policy Set')
     D x#Mode          c                   const('E')
     d*
     D/copy sPrNotice
     d*
     D
      /free

          //* Send the policy set to the notification data queue.
          SendNotice(x@RRN:x#Mode:x#Type);

      /end-free
     c*
     c                   seton                                        lr
