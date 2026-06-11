     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*********************************************************************
     H/TITLE API - SA016R - System API to Retrieve Import/Export Settings *
     H*********************************************************************
     H* Description:  This program will be used to retrieve               *
     H*               import/export settings.                             *
     H*                                                                   *
     H*  Called From:                                                     *
     H*                                                                   *
     H*                                                                   *
     H*  Calls To:    None                                                *
     H*                                                                   *
     H*********************************************************************
     H* Created on 04/12/2017 by C:180  for  Project MC325                *
     H*********************************************************************
     H* Revisions:                                                        *
     H* 07/25/18   C:180   Project: 146MIC                                *
     H*                    Add reopened status parameter.                 *
      //==================================================================*
     fSMIEP     if   e             disk
     d SA016R          PR
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                1
     d                                5  0
     d                                1
     d                                1
     d                                1
     d                                1
      *** Begin Add *** 07/25/18 *****************************************
     d                                1
      *** End   Add *** 07/25/18 *****************************************
     d SA016R          PI
     d  @@CCHG                        1
     d  @@CIFS                        1
     d  @@CSTS                        1
     d  @@CDNY                        1
     d  @@CCVFL                       1
     d  @@CGRP                        1
     d  @@CEMP                        1
     d  @@EIFS                        1
     d  @@EAGT                        1
     d  @@EEMP                        1
     d  @@FIFS                        1
     d  @@FTYP                        1
     d  @@UDUP                        1
     d  @@UDRC                        1
     d  @@UREL                        1
     d  @@UPCL                        1
     d  @@UPDN                        1
     d  @@UDPN                        1
     d  @@CLDAYS                      5  0
     d  @@INCZRO                      1
     d  @@CRCD                        1
     d  @@CDTH                        1
     d  @@RTCD                        1
      *** Begin Add *** 07/25/18 *****************************************
     d  @@RSTS                        1
      *
      *
     D PSDS          ESDS                  extname(SZQ1P)
      *
      *** End   Add *** 07/25/18 *****************************************
      /Free
       Read SMIEP;
       If not %eof(SMIEP);
          @@CCHG = IEcchg;
          @@CIFS = IEcifs;
          @@CSTS = IEcsts;
          @@CDNY = IEcdny;
          @@CCVFL= IEccvfl;
          @@CGRP = IEcgrp;
          @@CEMP = IEcemp;
          @@EIFS = IEeifs;
          @@EAGT = IEeagt;
          @@EEMP = IEeemp;
          @@FIFS = IEfifs;
          @@FTYP = IEftyp;
          @@UDUP = IEudup;
          @@UDRC = IEudrc;
          @@UREL = IEurel;
          @@UPCL = IEupcl;
          @@UPDN = IEupdn;
          @@UDPN = IEudpn;
          @@CLDAYS = IEcldays;
          @@INCZRO  = IEinczro;
          @@CRCD   = IEcrcd;
          @@CDTH   = IEcdth;
          @@RTCD   = '1';
       //*** Begin Add *** 07/25/18 *************************************
          If Q1PRMS > 23;
             @@RSTS = IErsts;
          Endif;
       //*** End   Add *** 07/25/18 *************************************
       Else;
          @@RTCD = '0';
       Endif;

       *inlr = *on;
      /End-Free
