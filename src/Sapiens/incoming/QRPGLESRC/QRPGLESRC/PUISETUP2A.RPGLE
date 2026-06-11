     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

     Fqclsrc    o    e             disk    rename(qclsrc:source)

     D*entry
     Dpuisetup2a       pr                  extpgm('PUISETUP2A')
     D inLIBL                       202
     Dpuisetup2a       pi
     D inLIBL                       202

     DValidLib         pr             1n
     D inLib                         10    const
     DWriteRcd         pr
     D inSource                     100    const

     Dseq#             s              6  2 inz
     Di1               s              3  0 inz

      /Free
       WriteRcd('PGM');
       WriteRcd('CHGLIBL LIBL(*NONE) CURLIB(*SAME)');
       For i1 = 3 to 193 by 10;
          If ValidLib(%subst(inLibl:i1:10));
             WriteRcd('ADDLIBLE LIB(' + %trim(%subst(inLibl:i1:10)) +
              ') POSITION(*LAST)');
          Endif;
       Endfor;
       WriteRcd('ENDPGM');
       *inlr = *on;
      /End-free

     PValidLib         b
     DValidLib         pi             1n
     D inLib                         10    const

      /Free
       Select;
       When inLib = *blank;
          Return *off;
       When %check('.$@#1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ ':inLib) <> 0;
          Return *off;
       Other;
          Return *on;
       Endsl;
      /End-free

     PValidLib         e

     PWriteRcd         b
     DWriteRcd         pi
     D inSource                     100    const

      /Free
       SRCDTA = '          '  + inSource;
       SRCSEQ = SRCSEQ + 10;
       Write source;
      /End-free

     PWriteRcd         e
