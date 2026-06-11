     hCopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title Return Output Type for Single Endorsement Print
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
     fstspl     if   e           k disk    rename(psp01:psp00)
      *Entry
     d w7054r          pr
     d  outPutType                    1
     d w7054r          pi
     d  outPutType                    1
     d  inProcessType  s            100    inz('Policy Page')
      /Free
       Chain inProcessType psp00;
       Select;
       When not(%found);
          outPutType = 'P';
       When SPsplO = 'Y' and SPscrO = 'Y';
          outPutType = 'B';
       When SPsplO = 'N' and SPscrO = 'Y';
          outPutType = 'S';
       When SPsplO = 'Y' and SPscrO = 'N';
          outPutType = 'P';
       When SPsplO = 'N' and SPscrO = 'N';
          outPutType = 'N';
       Endsl;
       Return;
      /End-Free
