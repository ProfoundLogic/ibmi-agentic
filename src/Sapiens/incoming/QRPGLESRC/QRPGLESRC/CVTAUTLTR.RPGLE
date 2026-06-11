     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*bnddir('WTBNDDIR')
      *================================================================
     H* Auto Claim Letters Conversion
     H*
     H* CREATED: 11/25/2013 C: 282 Project: 129WSC
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H*****************************************************************
      * Auto Claim Letters Definition File
     fwaclp     uf a e             disk    rename(pcl01:pcl0p)
      *
     d EOF             s               n   inz( *OFF )
      *
      *
      /Free
       Setll *Start waclp;
       DoW Not EOF;
          Read pcl0p;
          If %EOF(waclp);
             Leave;
          Endif;                   // %EOF(waclp)

         If %Trim(CLpgm)  = 'CVTAUTLTR' And
            %Trim(CLipgm) = 'CVTAUTLTR';
            Iter;
         Else;
            Select;
            When CLltfl = 'I';

               // Update the record in Auto Claim Letter file.
               CLltto = 'I';
               CLltfl = 'M';
               CLpgm  = 'CVTAUTLTR';
               Update pcl0p;

               // Write new record in Auto Claim Letter file.
               CLltto = 'I';
               CLltfl = 'R';
               CLpgm  = 'CVTAUTLTR';
               CLipgm  = 'CVTAUTLTR';
               Write pcl0p;

            When CLltfl = 'M';
               CLltto = 'M';
               CLltfl = 'R';
               CLpgm  = 'CVTAUTLTR';
               Update pcl0p;

            Other;
            EndSl;
         EndIf;

       EndDo;
       *InLr = *On;
      /End-Free
