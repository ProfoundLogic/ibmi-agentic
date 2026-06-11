     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')

     h/title LODWTXFP - Load data retention file
      *---------------------------------------------------------------------
     h*  Created on: 08/27/15 by C:33  for 36819
      *---------------------------------------------------------------------
      * Revisions:
      *
      *
      *---------------------------------------------------------------------

     fwtxfp     o    e             disk    rename(pxf01:pxf0p)

     d#ofEntry         c                   10
     dfileEntries      s             13    dim(#ofEntry) ctdata perrcd(1)

     dfileInfo         ds
     d clt                            3a
     d file                          10a

     d intCount        s              3  0

     d psds          esds                  extname(szq1p)

     d/copy sPrDate

      /free

       // main processing
       Exsr Process;

       *inlr = *On;

       //==================================================================
       // Process record in compile time array
       //==================================================================

       Begsr Process;

        // Writing record in file WTXFP from Compile time array.
        For intCount = 1 to #ofEntry;
           fileInfo = fileEntries(intCount);
           If file = *blanks;
              Iter;
           Endif;
              XFclt  = clt;
              XFfile = file;
           Monitor;
              Write pxf0p;
              Feod WTXFP;
           On-Error;
              Iter;
           Endmon;
        Endfor;

       Endsr;

      /end-free

**
   SMBUP
   SSYPF
   SMUPP
   WMUPP
   SMSLP
   MMOPP
   SMORP
   GLOBA00001
