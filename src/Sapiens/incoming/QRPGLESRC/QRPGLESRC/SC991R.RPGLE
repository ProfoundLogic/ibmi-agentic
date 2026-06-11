     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE SC991R- Darts Image/Notes Extract - Driver
     H*****************************************************************
     H*    Called From: SC991C - Darts Image/Notes Extract - Override *
     H*    Calls to:    SC965C - Note Print                           *
     H*                 SC992R - Copy Documents                       *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Image/Notes Extract History File
     Fsdehp1    o  a e             Disk    Rename(peh01:peh0p1)
     Fsxdel     uf   e           k Disk    Rename(pde01:pde00)
     F*
     d**********************************************************************
     d* System Information
     d**********************************************************************
     d psds          esds                  extname(szq1p)

     d**********************************************************************
     d* Prototypes
     d**********************************************************************

     d* Copy Documents
     d CopyDocuments   pr                  ExtPgm('SC992R')
     d  inKey                        40
     d  inSeq#                        5    Const
     d  inFromdt                      8
     d  inToDate                      8
     d  inDryType                    10
     d  inPath                      255
     d  outPath                     255
     d  outRtCd                       1

     d* Create Directory
     d CrtDirectory    pr                  ExtPgm('SMDIFS')
     d  path80                       80
     d  returnCode                    1

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d clientID                       3

     d* Main Processing
     d Main            pr

     d* Print Notes
     d PrintNotes      pr                  extpgm('SC965C')
     d  inKey                        40    Const
     d  inSeq#                        5    Const
     d  inPath                      256
     d  inDocSuffix                  24

     d* Program Start
     d ProgramStart    pr

     d* Write History
     d WriteSDEHP1     pr

     d/copy sPrCompany
     d/copy sPrDate
     d/copy wPrUsrPrms
     d**********************************************************************
     d* Entry Parameters
     d**********************************************************************
     d inFromDt        s              8
     d inToDate        s              8
     d inDryType       s             10
     d inPath          s            255
     d outPath         s            255
     d outRtCd         s              1
     d inDate          s              7
     d inTime          s              6

     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d clientID        s              3    inz('000')
     d userOutQ        s             10    inz(' ')
     d fromDate        s               d   DatFmt(*usa)
     d toDate          s               d   DatFmt(*usa)
     d dateCvt         s               d   DatFmt(*usa)
     d docSuffix       s             24
     d remarkDate      s             10
     d path80          s             80
     d path            s            256
     d returnCode      s              1

     d**********************************************************************
     d* Constants
     d**********************************************************************
     d docExists       c                   0                                    Exists
     d docAccess       c                   4                                    Access
     d streamFile      c                   '*STMF'
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

     c     *Entry        Plist
     c                   Parm                    inFromDt
     c                   Parm                    inToDate
     c                   Parm                    inDryType
     c                   Parm                    inPath
     c                   Parm                    inDate
     c                   Parm                    inTime

      /free

       // Main Procesing
       Main();

       *inlr = *On;

      /end-free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D error           s               n   Inz(*Off)

      /free

       // Start Of Program
       ProgramStart();

       // Process Darts
       Read pde00;
       DoW Not %Eof(sxdel);
          If CMstat <> 'P' And CMstat <> 'X';

             // Create Key Directory
             path80     = %Trim(inPath) + '/' + %Trim(CMkey);
             returnCode = ' ';
             CrtDirectory(path80:returnCode);

             // Copy Documents (DTM/Images)
             outPath = ' ';
             outRtCd = ' ';
             CopyDocuments(CMkey:%EditC(CMseq#:'X'):inFromDt:inToDate:inDryType:
              inPath:outPath:outRtCd);

             // Don't print remark if no document
             If outRtCd = 'P';

                // Call Program to print the notes
                dateCvt    = %Date(CMiDt:*CYMD);
                remarkDate = %Char(dateCvt);
                docSuffix  = %EditC(CMseq#:'X') + '-C ' + %Trim(CMrmCd) + ' ' +
                   %Subst(remarkDate:1:2) + '-' + %Subst(remarkDate:4:2) + '-' +
                   %Subst(remarkDate:7:4);

                // Remove the Data Center Prefix since PrintNotes adds it
                If %Subst(path80:2:3) = 'PDS';
                   path    = %Trim(%Subst(inPath:9)) + '/' + %Trim(CMkey) + '/';
                Else;
                   path    = %Trim(inPath) + '/' + %Trim(CMkey) + '/';
                EndIf;

                PrintNotes(CMkey:%EditC(CMseq#:'X'):path:docSuffix);

                CMstat = 'P';
                CMuncn = outPath;
                Update pde00;
             EndIf;
          EndIf;

          Read pde00;
       EndDo;

      /end-free
     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

     d suffix          s              3    Inz(' ')

      /free

       // Get the client id
       GetClientID(clientID);

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Write History Record
       WriteSDEHP1();

      /end-free
     p ProgramStart    e
       //===================================================================
       // WriteSDEHP1 - Write to Darts Extract
       //===================================================================
     p WriteSDEHP1     b
     d WriteSDEHP1     pi

      /free

       Clear peh0p1;

       EHtrDt    = %Dec(inDate:7:0);
       EHtrTm    = %Dec(inTime:6:0);
       EHfrDt    = %Dec(inFromDt:8:0);
       EHtoDt    = %Dec(inToDate:8:0);
       EHdryType = inDryType;
       EHpath    = inPath;

       EHstat    = 'S';
       EHuser    = Q1user;
       EHpgm     = Q1pgm;
       EHdt      = DateTo7(SystemDate());
       EHtime    = Timeto6(SystemTime());
       EHiUsr    = Q1user;
       EHiPgm    = Q1pgm;
       EHiDt     = DateTo7(SystemDate());
       EHitim    = Timeto6(SystemTime());
*****
       Write peh0p1;

      /end-free
     p WriteSDEHP1     e
