      //========================================================================*
     H/TITLE POPMIST - Populate State field in FTMIP messages file
      //copyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
      // StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      //*********************************************************************
      //*********************************************************************
      //
      //  Description:  This program populate the State field in the         "Y"
      //  Fee Schedule message file FTMIP from MIMSG# position 1-2
      //
      //  Called from: SYPOSTPGM2 - Postload conversion program - Processing Program
      //  Calls to:    None
      //
      //
      //  Created on 01/13/2015 by C:079 for Project SY359
      //*********************************************************************
      // Revisions:
      //
      // 06/16/20 C: 079 Support: PHW89430
      //*********************************************************************

     FFtmip     uf   e             disk

     //=========================================================================
     //  Definition Specs
     //=========================================================================
     d PSDS          ESDS                  EXTNAME(SZQ1P)
     d/copy sPrDate

     //=========================================================================
     //  KLIST definitions
     //=========================================================================

     //=========================================================================
      // Main Line processing
     //=========================================================================
      /Free
       Read pMI01;
       Dow Not %Eof;
       //*** Beg Add *** 06/15/20 ******************************************
         If MIst = *blanks;
       //*** End Add *** 06/15/20 ******************************************
            MIst = %subst(MImsg#:1:2);
            MIPgm = Q1Pgm;
            MIuser = Q1User;
            MIDt = DateTo7(systemDate());
            MITime = TimeTo6(SystemTime);
            Update pMI01;
       //*** Beg Add *** 06/15/20 ******************************************
         Endif;
       //*** End Add *** 06/15/20 ******************************************
       Read pMI01;
       enddo;

       *inlr = *on;

       //**  End of Program
      /End-Free
