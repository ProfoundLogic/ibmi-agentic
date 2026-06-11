     HCopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
      *---------------------------------------------------------------------
      *  Description:  Fix class effective date WMEPP/EPEFDT
      *
      *
      *  Created: 12/20/18   Programmer C:180   Support:75539
      *---------------------------------------------------------------------
      *  Revisions:
     FBADCLASS  IP   E             DISK    RENAME(BADCLASS:BADREC) PREFIX(B_)
     FWMEPP     UF   E             DISK
     D RRN             S              9  0 INZ

       RRN = B_Sel0001;
       Chain RRN WMEPP;
       If %found(WMEPP) and B_EPEFDT < B_ELPFDT;
          EPEFDT = B_ELPFDT;
          Update PEP01;
       Endif;
