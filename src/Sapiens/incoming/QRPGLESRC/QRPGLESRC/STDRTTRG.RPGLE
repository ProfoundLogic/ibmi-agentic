     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title STDRTTRG - Darts Trigger Process
     h dftname(STDRTTRG)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program Will process darts triggers            *
      *                                                                    *
      *  Called from: iSeries Triggers                                     *
      *                                                                    *
      *====================================================================*
      *  Created on 08/25/2017 by C:062 for Project: WC2501                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *====================================================================*
     d psds          esds                  extname(szq1p)

      * work variables
     d outCo#          s              3    inz
     d outFnd          s              3    inz
     d outLibrary      s             10

      * Trigger Buffer data structure
     d tgbufds         ds
     d  tgfile                       10
     d  tglib                        10
     d  tgmbr                        10
     d  teven                         1
     d  tgtrgtime                     1
     d  tgcmtlvl                      1
     d  tgreserve1                    3
     d  tgccsid                      10i 0
     d  tgreserve2                    8
     d  tgbfrofs                     10i 0
     d  tgbfrlen                     10i 0
     d  tgbfrnulof                   10i 0
     d  tgbfrnulln                   10i 0
     d  tgaftofs                     10i 0
     d  tgaftlen                     10i 0
     d  tgaftnulof                   10i 0
     d  tgaftnulln                   10i 0

      * Buffer length and pointers to before & after images
     d  tgbuflen       s             10i 0

      * file pointers
     d ptrSDCMPbfr     s               *   inz
     d ptrSDCMPaft     s               *   inz

     d SDCMP_before  e ds                  extname(SDCMP)
     d                                     prefix(pre)
     d                                     based(ptrSDCMPbfr)
     d SDCMP_after   e ds                  extname(SDCMP)
     d                                     prefix(pos)
     d                                     based(ptrSDCMPaft)

     d ProcessTrigger  pr
     d outTrgName                    10    const
     d outCo##                        3
     d outFnd#                        3
     d outPtrAfter                     *   options(*nopass)
     d outPtrBefore                    *   options(*nopass)

     c     *entry        plist
     c                   parm                    tgbufds
     c                   parm                    tgbuflen

      /free

       // process triggers based on file insert/update operation
       Select;
       When tgFile = 'SDCMP';
          ptrSDCMPbfr = %addr(tgbufds) + tgbfrofs;
          ptrSDCMPaft = %addr(tgbufds) + tgaftofs;
          outCo# = '000';
          outFnd = '000';

          // Insert
          Select;
          When tEven = '1';
             ProcessTrigger('RECDRYADD':outCo#:outFnd:
              ptrSDCMPaft);
          // Update
          When tEven = '3';
             ProcessTrigger('RECDRYCOMP':outCo#:outFnd:
              ptrSDCMPaft:ptrSDCMPbfr);
          EndSL;
       Endsl;

       *inlr = *on;
       Return;

