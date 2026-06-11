     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('STBNDDIR')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     fSIHUBTST  cf   e             workstn ExtFile('SIHUBTST')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')

     D psDs          esds                  extName(SZQ1P)
      *entry
     DTestLinks        pr                  extpgm('SIHUBTST')
     DTestLinks        pi

      /Free
       Exfmt rcdHubTst;
       *inlr = *on;
       Return;
      /End-free
