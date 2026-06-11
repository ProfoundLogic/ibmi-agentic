     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR10302  - GA - Georgia PEO Master Endorsement
      //===============================================================*
      // Revisions:                                                   *
      *                                                               *
      * 02/27/20 C:136 Project: WC2549 (Not commented)                *
      *          Changed to pasthr call to SRPEOCLT                   *
      //===============================================================*
     D qqForm          s             15A   Inz('WC100302  ')

     D SR10302         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR10302         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     d PeoClt          pr                  extpgm('SRPEOCLT')
     D  ##Co#                         3
     D  ##Fnd                         3
     D  ##Emp#                        9
     D  ##Div                         5
     D  ##Fyr                         3
     D  ##Mode                        1
     D  ##Form                       15

       PeoClt(qqCo#:qqFnd:qqEmp#:qqDiv:qqFyr:qqMode:qqForm);
       *Inlr = *on;
       Return;
