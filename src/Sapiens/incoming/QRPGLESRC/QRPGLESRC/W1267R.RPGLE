     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)
     H Bnddir('WTBNDDIR')
     H*
     H/TITLE W1267R - E-Pay File Extract Process Preparation
     H*****************************************************************
     H*  Program Description: Exports VPAY Export Data                *
     H*                                                               *
     H*  Called From: W1267C - E-Pay File Extract Process             *
     H*  Calls To   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 09/02/2020 by c:62 for Project WC2749             *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     Fwcvpayf   if   e             Disk    Rename(pvp01:pvp0p) usropn
     Fwcvpayout uf a e             Disk    Rename(pvp01:pvp0O)

      *entry
     DRequestPgm       pr                  ExtPgm('W1267R')
     D                              256
     D                               80
     DRequestPgm       pi
     D outPath                      256
     D outFolder                     80

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D documentID      s             30

     d/Copy sPrDate
     d/Copy wPrEPay

      /free

       // Clear Out file in case the clear failed
       Read pvp0O;
       DoW Not %Eof(wcvpayout);
          Delete pvp0O;
          Read pvp0O;
       EndDo;

       // Open the holding file
       Open wcvpayf;

       // Create Header Record
       CrtVPayHeaderRecord();

       // Copy Details
       Read pvp0p;
       DoW Not %Eof(wcvpayf);
          Write pvp0O;
          Feod wcvpayout;
          Read pvp0p;
       EndDo;

       // Create Trailer Records

       // Header Trailer Record
       CrtVPayTrailerRecord('HR000');

       // Payment Trailer Record
       CrtVPayTrailerRecord('VR001');

       // Payee Trailer Record
       CrtVPayTrailerRecord('VR002');

       // Claim Trailer Record
       CrtVPayTrailerRecord('VR003');

       // Header Trailer Record
       CrtVPayTrailerRecord('TR999');

       // Get the folder
       outFolder = GetEPayOutFolderForFiles('VPAY':DateTo7(systemDate()));

       // Create the file path
       documentID = GenerateDocumentID();
       outPath    = %Trim(outFolder) + '/' + documentID;

       // Create V-Pay History Entry
       WriteVPayHist(documentID);

       // Open the holding file
       Close wcvpayf;

       *inLR = *On;
    P*//----------------------------------------------------------------------
    P* GenerateDocumentID - Generate Document ID
     P*
     P* Returns the generated Document ID
     P*//-------------------------------------------------------------------
     P GenerateDocumentID...
     P                 B
     D GenerateDocumentID...
     D                 PI            30

     D documentID      s             30    Inz(' ')

      /free

       documentID = 'VPAY' + %Char(JobEntSys():*iso0) + '.TXT';

       Return documentID;

      /end-free

     P GenerateDocumentID...
     P                 E

      /end-free
