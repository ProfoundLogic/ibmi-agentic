     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     h bnddir('STBNDDIR')
     H nomain
     H*=====================================================================
     H* Created: 01/15/16   C:33   Project: SY285
     H*=====================================================================
     H* Revisions:
     H*
     H* 07/16/24 C:340  Project: WC2972
     H*                 Recomplie only due to change in SPRDASHBRD.
     H*
     H* 12/06/16  C:33  Project: SY388
     H*                 Add iFrames
     H*=====================================================================

     d/copy sPrString
     d/copy sPrDashBrd

     p*====================================================================
     p* Get the Widget Height
     p*====================================================================
     pGetWidgetHeight  b                   export
     dGetWidgetHeight  pi             5  0
     d inType                        15    const
     d inSize                         5  0 const

     d decSize         s                   like(inSize)
     d strType         s             15

      /Free
       decSize = (conBlockHgt * inSize) + (inSize - 1) * conSpacing;
       strType = Lower(inType);

       Select;
       When strType = 'panel';
          Return decSize;
       When strType = 'chart';
          Return decSize + conDiffHgt;
       When strType = 'grid';
          Return (inSize*8) + (inSize-1);
       //***  Begin Add  ***  12/06/16  ***********************************
       When strType = 'iframe';
          Return decSize;
       When strType = 'itab';
          Return decSize + conTabDiffHgt;
       //***  End   Add  ***  12/06/16  ***********************************
       Endsl;
      /End-free
     pGetWidgetHeight  e

     p*====================================================================
     p* Get the Widget Left posistion
     p*====================================================================
     pGetWidgetLeft    b                   export
     dGetWidgetLeft    pi             5  0
     d inType                        15    const
     d inPos                          5  0 const

     d decPos          s                   like(inPos)
     d strType         s             15

      /Free
       decPos = conFirstLft + (conBlockWth*(inPos-1)) + ((inPos-1)*conSpacing);
       strType = Lower(inType);

       Select;
       When strType = 'panel';
          Return decPos;
       When strType = 'chart';
          Return decPos + conDiffLft;
       When strType = 'grid';
          Return decPos;
       Endsl;
      /End-free
     pGetWidgetLeft    e

     p*====================================================================
     p* Get the Widget Top posistion
     p*====================================================================
     pGetWidgetTop     b                   export
     dGetWidgetTop     pi             5  0
     d inType                        15    const
     d inPos                          5  0 const

     d decPos          s                   like(inPos)
     d strType         s             15

      /Free
       decPos = conFirstTop + (conBlockHgt*(inPos-1)) + ((inPos-1)*conSpacing);
       strType = Lower(inType);

       Select;
       When strType = 'panel';
          Return decPos;
       When strType = 'chart';
          Return decPos + conDiffTop;
       When strType = 'grid';
          Return decPos;
       Endsl;
      /End-free
     pGetWidgetTop     e

     p*====================================================================
     p* Get the Widget Width
     p*====================================================================
     pGetWidgetWidth   b                   export
     dGetWidgetWidth   pi             5  0
     d inType                        15    const
     d inSize                         5  0 const

     d decSize         s                   like(inSize)
     d strType         s             15

      /Free
       decSize = (conBlockWth * inSize) + (inSize - 1) * conSpacing;
       strType = Lower(inType);

       Select;
       When strType = 'panel';
          Return decSize;
       When strType = 'chart';
          Return decSize + conDiffWth;
       When strType = 'grid';
          Return decSize + conDiffWth;
       //***  Begin Add  ***  12/06/16  ***********************************
       When strType = 'iframe';
          Return decSize;
       //***  End   Add  ***  12/06/16  ***********************************
       Endsl;
      /End-free
     pGetWidgetWidth   e
