     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title STAUTODRY - Auto Diary Trigger Processing (Real time)
     h dftname(STAUTODRY)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program Will process auto diary triggers       *
      *                                                                    *
      *  Called from: iSeries Triggers                                     *
      *                                                                    *
      *  Created on 05/22/2015 by C:279 for Project: 114WKA                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  04/10/17 C:136 Project: 129WUL                                    *
      *                 Add triggers for Exp Mod Date and Policy Address   *
      *                 Change                                             *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
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
     d ptrWMCMPbfr     s               *   inz
     d ptrWMCMPaft     s               *   inz
     d ptrWMCDPbfr     s               *   inz
     d ptrWMCDPaft     s               *   inz
     d ptrWDPAPbfr     s               *   inz
     d ptrWDPAPaft     s               *   inz
     d ptrWAMCPbfr     s               *   inz
     d ptrWAMCPaft     s               *   inz
     d ptrWDEHPbfr     s               *   inz
     d ptrWDEHPaft     s               *   inz
     d ptrWMSHPbfr     s               *   inz
     d ptrWMSHPaft     s               *   inz
     d ptrWXELPbfr     s               *   inz
     d ptrWXELPaft     s               *   inz
      *** beg add ***** 04/10/17 *******************
     d ptrWMEMPbfr     s               *   inz
     d ptrWMEMPaft     s               *   inz
      *** end add ***** 04/10/17 *******************

     d WMCMP_before  e ds                  extname(WMCMP)
     d                                     prefix(bfr)
     d                                     based(ptrWMCMPbfr)
     d WMCMP_after   e ds                  extname(WMCMP)
     d                                     prefix(aft)
     d                                     based(ptrWMCMPaft)
     d WMCDP_before  e ds                  extname(WMCDP)
     d                                     prefix(bfr)
     d                                     based(ptrWMCDPbfr)
     d WMCDP_after   e ds                  extname(WMCDP)
     d                                     prefix(aft)
     d                                     based(ptrWMCDPaft)
     d WDPAP_after   e ds                  extname(WDPAP)
     d                                     prefix(aft)
     d                                     based(ptrWDPAPaft)
     d WAMCP_after   e ds                  extname(WAMCP)
     d                                     prefix(aft)
     d                                     based(ptrWAMCPaft)
     d WDEHP_after   e ds                  extname(WDEHP)
     d                                     prefix(aft)
     d                                     based(ptrWDEHPaft)
     d WMSHP_after   e ds                  extname(WMSHP)
     d                                     prefix(aft)
     d                                     based(ptrWMSHPaft)
     d WXELP_after   e ds                  extname(WXELP)
     d                                     prefix(aft)
     d                                     based(ptrWXELPaft)
      *** beg add ***** 04/10/17 *******************
     d WMEMP_before  e ds                  extname(WMEMP)
     d                                     prefix(bfr)
     d                                     based(ptrWMEMPbfr)
     d WMEMP_after   e ds                  extname(WMEMP)
     d                                     prefix(aft)
     d                                     based(ptrWMEMPaft)
      *** end add ***** 04/10/17 *******************

     d RtvQteLib       pr                  extpgm('RTVQTELIB')
     d @@env                          4    const
     d @@lib                         10    const

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
       When tgFile = 'WMCMP';
          ptrWMCMPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWMCMPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftCMco#:'X');
          outFnd = %editc(aftCMfnd:'X');

          Select;
          // Insert
          When tEven = '1';
             // Do nothing. Removing Insert Trigger from WMCMP

          // Update
          When tEven = '3';
             ProcessTrigger('NEWCLAIM':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('CLMCHG02':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('CLMCHG03':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('CLMCHG04':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('LOWRSV':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('LARGERSV':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('CLMNCCI':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('CLMNPC':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
             ProcessTrigger('TOTALINC':outCo#:outFnd:ptrWMCMPaft:ptrWMCMPbfr);
          Endsl;

       When tgFile = 'WMCDP';
          ptrWMCDPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWMCDPaft = %addr(tgbufds) + tgaftofs;
          outCo# = '000';
          outFnd = '000';
          // Update
          If tEven = '3';
             ProcessTrigger('CLMCHG01':outCo#:outFnd:ptrWMCDPaft:ptrWMCDPbfr);
          Endif;

       When tgFile = 'WDPAP';
          ptrWDPAPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWDPAPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftPAco#:'X');
          outFnd = %editc(aftPAfnd:'X');
          // Insert
          If tEven = '1';
             ProcessTrigger('LARGEPMT':outCo#:outFnd:ptrWDPAPaft);
             ProcessTrigger('PAYMENT':outCo#:outFnd:ptrWDPAPaft);
          Endif;

       When tgFile = 'WAMCP';
          ptrWAMCPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWAMCPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftMCco#:'X');
          outFnd = %editc(aftMCfnd:'X');
          // Insert
          If tEven = '1';
             ProcessTrigger('MULTIEMP':outCo#:outFnd:ptrWAMCPaft);
          Endif;

       When tgFile = 'WDEHP';
          ptrWDEHPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWDEHPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftEHco#:'X');
          outFnd = %editc(aftEHfnd:'X');
          // Insert
          If tEven = '1';
             ProcessTrigger('AUDPREM$':outCo#:outFnd:ptrWDEHPaft);
             ProcessTrigger('AUDPREM':outCo#:outFnd:ptrWDEHPaft);
             ProcessTrigger('CALCPREM':outCo#:outFnd:ptrWDEHPaft);
             ProcessTrigger('POLCONC':outCo#:outFnd:ptrWDEHPaft);
            //*** beg add ***** 04/10/17 **************************
             ProcessTrigger('EXPMODDATE':outCo#:outFnd:ptrWDEHPaft);
            //*** end add ***** 04/10/17 **************************
          Endif;

       When tgFile = 'WMSHP';
          ptrWMSHPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWMSHPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftSHco#:'X');
          outFnd = %editc(aftSHfnd:'X');
          // Insert
          If tEven = '1' or tEven = '3';
             RtvQteLib('*LIV':outLibrary);
             If tgLib = outLibrary;
                ProcessTrigger('POLPANEL':outCo#:outFnd:ptrWMSHPaft);
             Endif;
          Endif;

       When tgFile = 'WXELP';
          ptrWXELPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWXELPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftELco#:'X');
          outFnd = %editc(aftELfnd:'X');
          // Insert
          If tEven = '1';
             RtvQteLib('*QTE':outLibrary);
             If tgLib = outLibrary;
                ProcessTrigger('QTEENTRY':outCo#:outFnd:ptrWXELPaft);
                ProcessTrigger('QTERVW':outCo#:outFnd:ptrWXELPaft);
             Endif;
          Endif;

       //*** beg add ***** 04/10/17 ***********************
       When tgFile = 'WMEMP';
          ptrWMEMPbfr = %addr(tgbufds) + tgbfrofs;
          ptrWMEMPaft = %addr(tgbufds) + tgaftofs;
          outCo# = %editc(aftEMco#:'X');
          outFnd = %editc(aftEMfnd:'X');
          // Update
          If tEven = '3';
             ProcessTrigger('POLADDRCHG':outCo#:outFnd:
              ptrWMEMPaft:ptrWMEMPbfr);
          Endif;
       //*** end add ***** 04/10/17 ***********************

       Endsl;

       *inlr = *on;
       Return;

