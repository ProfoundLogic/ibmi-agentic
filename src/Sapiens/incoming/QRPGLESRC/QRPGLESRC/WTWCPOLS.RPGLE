     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTWCPOLS - WCPOLS Schedule Program
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program reads thru the WCPOLS schedule file
     h*                and submits WCPOLS job.
     h*
     h*  Called from: Job Schedule
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 12/21/2020 by C:062 for Project: WC2761
     h**********************************************************************
     h*  Revisions:                                                        *
     h*                                                                    *
     h* 03/14/23 C:062 Ticket: 1076163                                     *
     h*                Don't send ETR data if NCCI submission              *
     h*                                                                    *
     h* 03/08/22 C:340 Project: WC2843                                     *
     h*                Start using new Logical - WMTML1                    *
     h*                Use Carrier specific Data provider and FEIN         *
     h*
     h*  11/02/21 C:65  Project:  Phw#97470 (Amwins)
     h*                 Add a set lower limit to ensure reads over group
     h*                 master file access records for submissions after 1st.
     h*
     h**********************************************************************
     fstabl     if   e           k disk    Rename(pst01:pst00)
     fwtwpl3    if   e           k disk    Rename(pwp01:pwp03)
     fwdf2l     if   e           k disk    Rename(pf201:pf200)
     fwmfnl     if   e           k disk    Rename(pfn01:pfn00)
     f*** Change Begin - 03/08/22 *****************************************
     f*** wmtml     if   e           k disk    Rename(ptm01:ptm00)
     fwmtml1    if   e           k disk    Rename(ptm01:ptm00)
     f*** Change End   - 03/08/22 *****************************************
     fwxpgl     uf a e           k disk    Rename(xpg01:xpg00) UsrOpn

     d psds          esds                  extname(szq1p)

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Check IFS Object
     d ChkIfsObj       pr                  extpgm('CHKIFSOBJ')
     d  inIfsName                   256
     d  outFound                       n

     d* Submit Job
     d SubmitJob       pr                  extpgm('W7417C')
     d inCompany#                     3
     d inGroup#                       3
     d inOutputType                   1
     d inInitialize                   3
     d inDevice                      10
     d inVolume                      10
     d inTape                        10
     d outTotalRcds                   9
     d outTotalHdrs                   9
     d outReturnCode                  1
     d inDirectory                  251
     d inDocID                      101
     d inER                           1
     d inState                        2
     d inEMail                       31
     d inContactName                 25
     d inContPhone#                  10
     d inContactExt                   6
     d inContactFax                  10
     d inContAddress                102
     d outTotalNew                    9
     d outTotalRenew                  9
     d outTotalCancel                 9
     d outTotalRein                   9
     d outTotalEndors                 9
     d inPEEP                         1
     d inOldCar#Data                  1
     d inMember                      10
      *** Change Begin - 03/08/22 *****************************************
     d inDataProvider                 1
     d inFein                         9
      *** Change End   - 03/08/22 *****************************************
     d arrDocuments    s            256    Dim(350) Inz

     d* Submit Job
     d CreateMember    pr                  extpgm('W7417C5')
     d inFlag                         1    Const
     d outMember                     10
     d inCopy                         1    Const

     D                 DS
     D contactAddress          1    101
     D  TMadr1                 1     60
     D  TMcty                 61     90
     D  TMst                  91     92
     D  TMzip                 93    101  0

     d ##CltId         s              3

     d inCompany#      s              3
     d inGroup#        s              3
     d inOutputType    s              1
     d inInitialize    s              3
     d inDevice        s             10
     d inVolume        s             10
     d inTape          s             10
     d outTotalRcds    s              9
     d outTotalHdrs    s              9
     d outReturnCode   s              1
     d inDirectory     s            251
     d inDocID         s            101
     d inER            s              1
     d inState         s              2
     d inEMail         s             31
     d inContactName   s             25
     d inContPhone#    s             10
     d inContactExt    s              6
     d inContactFax    s             10
     d inContAddress   s            102
     d outTotalNew     s              9
     d outTotalRenew   s              9
     d outTotalCancel  s              9
     d outTotalRein    s              9
     d outTotalEndors  s              9
     d inPEEP          s              1
     d inOldCar#Data   s              1
     d inMember        s             10
      *** Change Begin - 03/08/22 *****************************************
     d inDataProvider  s              1
     d inFein          s              9
      *** Change End   - 03/08/22 *****************************************

     d inFlag          s              1
     d outMember       s             10

     d carrier#        s                   Like(WPcar#)
     d state           s              4
     d state#          s              5  0
     d key             s                   Like(STkey)
     d ccyymmdd        s              8  0
     d hhmmss          s              6  0
     d hhmm            s              4  0
     d extPos          s              5  0 Inz
     d extension       s              4
     d inIfsName       s            256
     d position        s              3  0
     d outFound        s               n
     d sequence#       s              2  0 Inz(1)
     d sequence2       s              3  0 Inz(1)
     d uniqueFile      s               n   Inz(*Off)

     d/copy sPrDate
     d/copy sPrString
     d/copy sPrRunCmd

      /free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       return;

      /end-free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       Read pwp03;
       DoW Not %Eof(wtwpl3);

          If WPstat = 'I';
             Read pwp03;
             Iter;
          EndIf;

          inCompany#     = %EditC(WPco#:'X');
          inGroup#       = %EditC(WPfnd:'X');
          inOutputType   = 'F';

          inInitialize   = ' ';
          inDevice       = ' ';
          inVolume       = ' ';
          inTape         = ' ';

          outTotalRcds   = '000000000';
          outTotalHdrs   = '000000000';
          outReturnCode  = ' ';

          inDirectory    = WPdir + '$';
          inER           = WPer;
          inState        = WPst;

      *** Change Begin - 03/08/22 *****************************************
      //*** Chain (WPco#:'P') ptm00;
      //*** If %Found(wmtml) And WPst <> ' ';
      //***    inEMail        = TMmail;
      //***    inContactName  = TMname;
      //***    inContPhone#   = %EditC(TMphn#:'X');
      //***    inContactExt   = %EditC(TMext#:'X');
      //***    inContactFax   = %EditC(TMfax#:'X');
      //***    inContAddress  = contactAddress + '$';
      //*** Else;
      //***    inEMail        = ' ';
      //***    inContactName  = ' ';
      //***    inContPhone#   = ' ';
      //***    inContactExt   = ' ';
      //***    inContactFax   = ' ';
      //***    inContAddress  = ' ';
      //*** EndIf;
      *** Change End   - 03/08/22 *****************************************

          outTotalNew    = '000000000';
          outTotalRenew  = '000000000';
          outTotalCancel = '000000000';
          outTotalRein   = '000000000';
          outTotalEndors = '000000000';

          inPEEP         = WPpeep;
          inOldCar#Data  = ' ';
          inMember       = ' ';

          Select;
          When WPfnd <> 0;
             // Get the carrier #
             Chain (WPco#:WPfnd) pf200;
             carrier# = F2car#;

             // No member is sent, just processing one group
             inMember = ' ';
          When WPcar# <> 0;
             // Assign carrier #;
             carrier# = WPcar#;

             // Create member and return name
             CreateMember('C':outMember:'N');

             // Select groups that match the carrier #
             UpdateWXPGP(outMember);

             // Populate Member
             inMember = outMember;
          Endsl;

      *** Change Begin - 03/08/22 *****************************************
          //*** Begin Add *** 03/14/23 ************************************
          If WPst <> ' ';
          //*** End   Add *** 03/14/23 ************************************
            Chain (WPco#:carrier#) ptm00;
            If Not %Found(wmtml1);
               Chain (WPco#:0) ptm00;
               If  %Found(wmtml1);
                   inEMail        = TMmail;
                   inContactName  = TMname;
                   inContPhone#   = %EditC(TMphn#:'X');
                   inContactExt   = %EditC(TMext#:'X');
                   inContactFax   = %EditC(TMfax#:'X');
                   inContAddress  = contactAddress + '$';
                   inDataProvider = TMDType;
                   inFein         = %Char(TMFein);
               EndIf;
            Else;
               inEMail        = TMmail;
               inContactName  = TMname;
               inContPhone#   = %EditC(TMphn#:'X');
               inContactExt   = %EditC(TMext#:'X');
               inContactFax   = %EditC(TMfax#:'X');
               inContAddress  = contactAddress + '$';
               inDataProvider = TMDType;
               inFein         = %Char(TMFein);
            EndIf;
          //*** Begin Add *** 03/14/23 ************************************
          Else;
            inEMail        = ' ';
            inContactName  = ' ';
            inContPhone#   = ' ';
            inContactExt   = ' ';
            inContactFax   = ' ';
            inContAddress  = ' ';
            inDataProvider = ' ';
            inFein         = ' ';
          EndIf;
          //*** End   Add *** 03/14/23 ************************************
      *** Change End   - 03/08/22 *****************************************

          // Assign state value and state #
          If WPst = ' ';
             state  = 'NCCI';
             state# = 99;
          Else;
             state = WPst;
             key = WPst;
             Chain (key) pst00;
             state# = %Dec(STnSt:5:0);
          EndIf;

          // Get date and time
          ccyymmdd = DateToYYMD(systemDate());
          hhmmss   = TimeTo6(systemTime());
          hhmm     = %Dec(%Subst(%EditC(hhmmss:'X'):1:4):4:0);

          // Build Document ID
          // See if there is an extension
          extPos = %Scan('.':WPfile);
          If extPos = 0;
             WPfile = %Trim(WPfile) + '.txt';
          EndIf;

          // Replace Carrier# Token
          WPfile = %ScanRpl('#Carrier#':%EditC(carrier#:'X'):WPfile);

          // Replace Group Token
          WPfile = %ScanRpl('#Group#':%EditC(WPfnd:'X'):WPfile);

          // Replace State Token
          WPfile = %ScanRpl('#State#':%Trim(state):WPfile);

          // Replace State # Token
          WPfile = %ScanRpl('#StateNumber#':%EditC(state#:'X'):WPfile);

          // Replace Date Token
          WPfile = %ScanRpl('#Date#':%EditC(ccyymmdd:'X'):WPfile);

          // Replace Date/Time Token
          WPfile = %ScanRpl('#DateTime#':%EditC(ccyymmdd:'X') +
                   %EditC(hhmm:'X'):WPfile);

          // Check to see if same document was used in this run
          position = %LookUp(WPfile:arrDocuments);
          If position <> 0;
             // Document found already used this run
             extPos = %Scan('.':WPfile);
             WPfile = %Subst(WPfile:1:extPos-1) + '~' +
               %EditC(sequence2:'X') + %Subst(WPfile:extPos);
             sequence2 += 1;
          EndIf;
          position = %LookUp(' ':arrDocuments);
          arrDocuments(position) = WPfile;

          inDocID = WPfile;

          // Make sure it doesn't already exist.
          uniqueFile  = *Off;
          sequence# = 1;
          extPos = %Scan('.':WPfile);
          DoU uniqueFile;
             inIfsName = %Trim(inDirectory) + '/' + inDocID;
             outFound  = *Off;
             ChkIfsObj(inIfsName:outFound);
             If outFound = *On;
                // See if there is an extension
                inDocID = %Subst(WPfile:1:extPos-1) + '~' +
                  %EditC(sequence#:'X') + %Subst(WPfile:extPos);
                sequence# += 1;
             Else;
                uniqueFile = *On;
             EndIf;
          EndDo;

          // Add $ at end of file name
          %Subst(inDocID:101:1) = '$';

          // Submit the scheudled entry
      *** Change Begin - 03/08/22 *****************************************
      //*** SubmitJob(inCompany#:inGroup#:inOutputType:
      //*** inInitialize:inDevice:inVolume:inTape:outTotalRcds:
      //*** outTotalHdrs:outReturnCode:inDirectory:inDocID:inER:
      //*** inState:inEMail:inContactName:inContPhone#:inContactExt:
      //*** incontactFax:inContAddress:outTotalNew:outTotalRenew:
      //*** outTotalCancel:outTotalRein:outTotalEndors:inPEEP:
      //*** inOldCar#Data:inMember);

          SubmitJob(inCompany#:inGroup#:inOutputType:
            inInitialize:inDevice:inVolume:inTape:outTotalRcds:
            outTotalHdrs:outReturnCode:inDirectory:inDocID:inER:
            inState:inEMail:inContactName:inContPhone#:inContactExt:
            incontactFax:inContAddress:outTotalNew:outTotalRenew:
            outTotalCancel:outTotalRein:outTotalEndors:inPEEP:
            inOldCar#Data:inMember:inDataProvider:inFein);

      *** Change End   - 03/08/22 *****************************************

          // Delay job for one minute
          runCmd('DLYJOB DLY(60)':255);

          Read pwp03;
       Enddo;

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Get the client id
       GetClientID(##CltId);

      /end-free
     p ProgramStart    e

       //===================================================================
       // UpdateWXPGP - Update WXPGP by selecting the groups that match
       //               the carrier #.
       //===================================================================
     p UpdateWXPGP     b
     d UpdateWXPGP     pi
     d  inMember                     10

      /free

       // Override to new Member
       runCmd('OvrDBF File(WXPGL) Mbr(' + %Trim(inMember) + ')':255);

       // Open WXPGP
       Open wxpgl;

       // Include groups that match the carrier #;
       //*** begin chg ***  11/02/21  ***********************************
       //*****Read pfn00;
       Setll (WPCo#) pfn00;
       Reade (WPCo#) pfn00;
       //*** end chg *****  11/02/21  ***********************************
       DoW Not %Eof(wmfnl);
          Chain (FNco#:FNfnd) xpg00;
          If %Found(wxpgl);
             PGflag = 'E';
             If FNtest <> 'Y';
                Chain (FNco#:FNfnd) pf200;
                If %Found(wdf2l) And F2car# = WPcar#;
                   PGflag = 'I';
                Else;
                   PGflag = 'E';
                EndIf;
             EndIf;
             Update xpg00;
          Else;
             PGco# = FNco#;
             PGgrp = FNfnd;
             PGname = FNname;
             PGflag = 'E';
             If FNtest <> 'Y';
                Chain (FNco#:FNfnd) pf200;
                If %Found(wdf2l) And F2car# = WPcar#;
                   PGflag = 'I';
                Else;
                   PGflag = 'E';
                EndIf;
             EndIf;
             Write xpg00;
          EndIf;
       //*** begin chg ***  11/02/21  ***********************************
       //*****   Read pfn00;
          Reade (WPCo#) pfn00;
       //*** end chg *****  11/02/21  ***********************************
       EndDo;

       // Close WXPGL
       Close wxpgl;

       // Remove overrides
       runCmd('DLTOVR File(WXPGL)':255);

      /end-free

     p UpdateWXPGP     e
