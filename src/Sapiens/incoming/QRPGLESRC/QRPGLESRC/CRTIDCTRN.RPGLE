     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     h/title CRTIDCTRNO - Create IDC Log Records by from/to date
     h dftname(CRTIDCTRN)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      // Created   : 03/12/25                                         *
      // Programmer: C:079                                            *
      // Project   : 140WCG - Create IDC Log Records by from/to date  *
      *===============================================================*
      // Revisions:                                                   *
      //                                                              *
      //------------------------------------------------------------- *
      // NOTE: Please add comments to the top of Revisions.           *
      //------------------------------------------------------------- *
      *===============================================================*
       // File declarations
       // Payment File
     fwdpal2    if   e           k Disk
     f                                     infds(dbfds)
      *===============================================================*
       // Procedure declaration
     d Main            pr

     d/copy sPrDate
     d/copy wPrIdc
     d/copy wPrUsrPrms

      *===============================================================*
       // Data structures

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d dbrrn1                397    400b 0
      *===============================================================*
       // Array declaration

      *===============================================================*
       // Standalone variables

     d inFrDate        s              7a
     d inToDate        s              7a
     d inRrn1          s              9s 0
     d intFrDate       s                   Like(PAtrDt)
     d intToDate       s                   Like(PAtrDt)
     d intCo#          s                   Like(PAco#)
      *===============================================================*
      *entry
     c     *entry        plist
     c                   parm                    inFrDate
     c                   parm                    inToDate

      *===============================================================*
       // Main program processing

       // If the incoming date are valid then perform processing
       intFrDate = %Dec(inFrDate:7:0);
       intToDate = %Dec(inToDate:7:0);
       If (dtMdYY(intFrDate) > 0 and dtMdYY(intToDate) > 0)
        and intToDate >= intFrDate;
          main();
       Endif;

       *inlr = *on;
       Return;

       //===================================================================
       // Main - Mainline Procedure
       //===================================================================
     p Main            b
     d Main            pi
       intCo# = UserCo#(Q1user);
       // Read through payments according to date range passed in and call
       // WriteTxLog to write transactional log records in WMIDCTR
       Setll (intCo#:intFrDate) ppa01;
       Reade (intCo#) ppa01;
       Dow not %EOF(WDPAL2);
          If PAtrDt > intToDate;
             Leave;
          Endif;
          If PAreTp = 'C' and PAtrDt >= intFrDate and PAtrDt <= intToDate;
             If PAfrdt > 0 and PAtoDt > 0;
                inRrn1 = DbRrn1;
                WriteTxLog(inRrn1);
             Endif;
          Endif;
          Reade (intCo#) ppa01;
       Enddo;

     p Main            e

