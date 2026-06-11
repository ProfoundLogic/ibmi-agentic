     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

     Fpuiconf1  if   e             disk    rename(puiconf1:conf1)
     F                                     prefix(dta:3)
     Fpuiconf2  o    e             disk    rename(puiconf2:conf2)
     F                                     prefix(dta:3)

     D*entry
     Dpuisetup3a       pr                  extpgm('PUISETUP3A')
     Dpuisetup3a       pi

     Dseq#             s              6  2 inz
     Di1               s              3  0 inz

      /Free
       Setll 1 conf1;
       Dou %eof(puiconf1);
          Read conf1;
          If not %eof(puiconf1);
             dtaConf2 = dtaConf1;
             Write conf2;
             If %scan('SetEnv PUI_UPLOAD_CAP 100':dtaConf1) <> 0;
                dtaConf2 = 'SetEnv PUI_ATRIUM_CUSTOM 1';
                Write conf2;
             Endif;
          Endif;
       Enddo;
       *inlr = *on;
      /End-free

