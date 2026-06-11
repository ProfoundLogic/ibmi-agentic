     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('STBNDDIR')
     H dftactgrp(*no)
     //********************************************************************
    H/TITLE LODWTP3P - Load data for WTP3P - Third Party Payment Processes
     //********************************************************************
     // Created: 07/01/19    C:65    Project: SY407                       *
     //********************************************************************
      // Revisions:                                                        *
      //                                                                   *
      // 03/11/21  C:33  Project: WC2663                                   *
      //                 Add Third Party Payments                          *
      //********************************************************************
     Fwtp3l     uf a e           k disk    rename(pp301:pp300)

     D#ofRcds          c                   100
     DaddKey           s            100    dim(#ofRcds) ctdata perrcd(1)
     Dstatus           s              1    dim(#ofRcds) ctdata perrcd(1)
     Di1               s              3  0
     DsysTime          s                   like(P3time)
     DsysDate          s                   like(P3dt)
     DintCo#           s              3  0

     D psds          esds                  extname(szq1p)

     D/copy sPrDate
     d/copy sprUsrPrms

      /Free
       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       intCo# = GetUserInfo(q1user);

       For i1 = 1 to #ofRcds;
          Chain (intCo#:addKey(i1)) pp300;
          Select;
          When not %found and addKey(i1) <> *blanks;
             Exsr WriteRecord;
          When %found and P3Stat <> 'A' and status(i1) = 'A';
             Exsr ActivateRecord;
          Endsl;
       Endfor;

       *inlr = *on;

       //====================================================================
       // Write records to WTP3P
       //====================================================================
       Begsr WriteRecord;

         P3Co#    = intCo#;
         P3PrType = addKey(i1);
         P3Stat   = status(i1);

         P3user = q1User;
         P3pgm  = q1Pgm;
         P3time = sysTime;
         P3dt   = sysDate;
         P3iUsr = P3user;
         P3iPgm = P3pgm;
         P3iDt  = P3dt;

         Write pp300;

       Endsr;

       //====================================================================
       // Update records in WTP3P to be active.
       //====================================================================
       Begsr ActivateRecord;

         P3Stat   = status(i1);

         P3user = q1User;
         P3pgm  = q1Pgm;
         P3time = sysTime;
         P3dt   = sysDate;

         Update pp300;

       Endsr;
      /End-free
**  100 A  - All qualifying output
S/R Online
Third Party Payments
**  1 A  - status
A                                                           S/R Online Connect
A                                                           Third Party Payments
