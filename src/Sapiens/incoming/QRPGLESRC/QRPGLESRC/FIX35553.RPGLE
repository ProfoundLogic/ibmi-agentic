     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H
     H*****************************************************************
     H* Data Correction program for RMLA
     H*****************************************************************
     H*                                                               *
     fWMEML     UF   E           K Disk
      *
     d W0050R01        pr                  EXTPGM('W0050R01')
     d inemco#                        3  0
     d inemfnd                        3  0
     d inememp#                       9  0
     d inemdiv                        5  0
     d inqte                          1
     d
     d inCompany       S              3
     d inGroup         S              3
     d inEmco#         S              3  0
     d inEmfnd         S              3  0
     d inEmemp#        S              9  0
     d inEmdiv         S              5  0
     d inQte           S              1
     d
     C     *Entry        Plist
     C                   Parm                    inCompany
     C                   Parm                    inGroup
     C
      /Free
       // Intialize the key value.
       EMco# = %Dec(inCompany:3:0);
       EMfnd = %Dec(inGroup:3:0);

       // EMCO# = 90 ;        // Company code for RMLA
       // EMFND = 605 ;       // Claim only group

       Setll (EMco#:EMfnd) WMEML;
       Reade (EMCO#:EMfnd) WMEML;
       Dow Not %Eof(WMEML);

          inEmco# =  EMco#;
          inEmfnd =  EMfnd;
          inEmemp# = EMemp#;
          inEmdiv =  EMdiv;
          inQte  =   'Q';

          // W0050R01 is a Divsion deletion program //

          W0050R01(inemco#:inemfnd:inememp#:inemdiv:inqte);

          Delete PEM01;
          Reade (EMco#:EMfnd) WMEML;
       EndDo;
       *Inlr = *On;

      /End-Free
