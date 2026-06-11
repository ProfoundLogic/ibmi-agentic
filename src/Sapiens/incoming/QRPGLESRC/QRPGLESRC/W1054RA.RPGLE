     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE W1054RA - Wrapper for W/W Endorsement Forms
      // Called from Quote Transfer (WTPOLQTFR)
      /Free
       Dcl-Pr W1054RA ExtPgm('W1054RA');
        inRRN Char(9) Const;
       End-Pr;
       Dcl-Pi W1054RA;
        inRRN Char(9) Const;
       End-Pi;
       Dcl-Pr W1054R ExtPgm('W1054R');
        inKl Char(1) Const;
        inKc Char(1) Const;
        inCall Char(1) Const;
        inRRN Packed(9:0) Const;
        inEnv Char(1) Const;
       End-Pr;
       Dcl-S outRRN Packed(9:0);
       outRRN = %Int(inRRN);
       Callp W1054R ('0':'0':' ':outRRN:'Q');
       Return;
      /End-Free
