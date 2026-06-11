     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE SC997R- Image Transfer - Driver                          *
     H*****************************************************************
     H*    Called From: SC997C - Image Transfer Override              *
     H*    Calls to:    SC998R - Image Transfer                       *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Image/Notes Extract History File
     Fwxgsl     if   e           k Disk
     Fsxdel     uf   e           k Disk    Rename(pde01:pde00)
     Fsmmdl     if   e           k DISK    Rename(pmd01:pmd00)
     Fsdehp1    o  a e             Disk    Rename(peh01:peh0p1)
     Fqsysprt   o    f  132        PRINTER Oflind(*inof)
     F*
     d**********************************************************************
     d* System Information
     d**********************************************************************
     d psds          esds                  extname(szq1p)

     d**********************************************************************
     d* Prototypes
     d**********************************************************************

     d* Transfer Objects
     d TransfObj       pr                  ExtPgm('SC998R')
     d  inKey                        40
     D  inSeq#                        5    Const
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

     d* Check Non-Image Optical
     d ChkNonImgOpt    pr                  ExtPgm('SC871R')
     d key                           40
     d chrSeq#                        5    Const
     d document                      12
     d folder                        80
     d returnCode                     1

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr

     d* Write History
     D WriteSDEHP1     pr

     d* Stat API - Determine object size
     d Stat            pr            10I 0 ExtProc('stat')
     d path                            *   value options(*string)
     d buf                             *   value

     d p_statds        s               *
     d statds          ds                  Based(p_statds)
     d st_mode                       10U 0
     d st_ino                        10U 0
     d st_nlink                       5U 0
     d st_pad                         2A
     d st_uid                        10U 0
     d st_gid                        10U 0
     d st_size                       10I 0
     d st_atime                      10I 0
     d st_mtime                      10I 0
     d st_ctime                      10I 0
     d st_dev                        10U 0
     d st_blksize                    10U 0
     d st_allocsize                  10U 0
     d st_objtype                    12A
     d st_codepage                    5U 0
     d st_reserved1                  62A
     d st_ino_gen_id                 10U 0

     D FileStats       ds                  likeds(statds)

     D filename        s           1000A   varying
     D ObjSize         s             10I 0 inz
     D TotBytes        s             10I 0 inz
     D TotGigaBytes    s             10I 0 inz


     d/copy sPrCompany
     d/copy sPrDate
     d/copy wPrUsrPrms
     d**********************************************************************
     d* Entry Parameters
     d**********************************************************************
     d inFromDt        s              8
     d inToDate        s              8
     D inDryType       s             10
     d inPath          s            255
     D inMode          S              1

     d**********************************************************************
     d* Transfer Parameters
     d**********************************************************************
     d outPath         s            255
     d outRtCd         s              1



     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d clientID        s              3    inz('000')
     d path80          s             80
     d returnCode      s              1
     d Time6           s               T   INZ
     d outObj          s             80
     D Flr             S             80    Inz
     d WinPos          s              5  0 Inz
     d chrSeq#         s              5    Inz
     d**********************************************************************
     d* Constants
     d**********************************************************************
     d ErrMsg1         c                   '** Error ** Size could -
     d                                     not be determined.'



     c     *Entry        Plist
     c                   Parm                    inFromDt
     c                   Parm                    inToDate
     C                   PARM                    inDryType
     c                   Parm                    inPath
     c                   Parm                    inMode

      /free

       // Main Procesing
       Main();

       *inlr = *On;

      /end-free
     OQSYSPRT   E            HDG            1 02
     O                                              'PGM:SC997R'
     O                       UDATE         Y   +002
     O                       Time6             +001
     O                                           76 'IMAGE TRANSFER REPORT'
     O                                          124 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG         2
     O                                           11 'OBJECT NAME'
     O                                          100 'OBJECT SIZE'

     O          E            DTL         1
     O                       outObj              80
     O              N90      ObjSize       1B   100
     O               90      errmsg1            130
     O          E            TOT         2
     O                                           48 'Total'
     O                                         +001 'Gigabytes'
     O                       TotGigabytes  1   +001
     O                                           86 'Bytes'
     O                       TotBytes      1    100
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D error           s               n   Inz(*Off)

      /free

       // Start Of Program
       ProgramStart();

       // Process fund
       Read PGS01;
       Dow not %eof(wxgsl);
          If GSiefl = 'I';
             // Process Darts
             setll *loval pde00;
             Read pde00;
             DoW Not %Eof(sxdel);
                If CMstat <> 'P' And CMstat <> 'X';
                   If %editc(GSco#:'X') = %subst(CMKEY:1:3) and
                      %editc(GSfnd:'X') = %subst(CMKEY:4:3);

                      Select;
                      When inMode = 'T';
                         Exsr Transfer;
                      When inMode = 'R';
                         Exsr Report;
                      Endsl;


                   EndIf;
                EndIf;

                Read pde00;
             EndDo;
          EndIf;
          Read PGS01;
       EndDo;

       //Report Mode - Print totals
       *in77 = *on;
       *in90 = *off;
       If inMode = 'R';
          If *inof = *on;
             Except Hdg;
          Endif;
          Except Tot;
       Endif;

       //===================================================================
       // Transfer Mode
       //===================================================================
       Begsr Transfer;

       // Transfer Objects
       outPath = ' ';
       outRtCd = ' ';
       TransfObj(CMkey:%EditC(CMseq#:'X'):inPath:outPath:outRtCd);

       //Update to processed status.
       If outRtCd = 'P';
          CMstat = outRtCd;
          CMuncN = outPath;
          Update pde00;
       EndIf;
       Endsr;
       //===================================================================
       // Report Mode
       //===================================================================
       Begsr Report;

       //Print size of selected objects
       If CMFL05 <> 'O' and CMuncN <> *blanks;

          //Check not already in Win Server
          WinPos = %scan(%trim(inPath):CMuncN);

          If WinPos = *Zero;
             filename = %Trim(CMuncN);
             exsr PrintObj;
          Endif;
       Endif;

       //Print size of additional objects
       Setll (CMkey:CMseq#) pmd00;
       If %equal(smmdl);
          ReadE (CMkey:CMseq#) pmd00;

          Dow not %Eof(smmdl);
             If MDuncp <> *blanks and MDoflr <> *blanks and MDoid <> *blanks;
                chrSeq# = %EditC(MDseq#:'X');
                Flr = MDoflr;
                ChkNonImgOpt(MDkey:chrSeq#:MDoid:Flr:returnCode);

                //Process if not Optical
                If returnCode = '1';

                   //Check not already in Win Server
                   WinPos = %scan(%trim(inPath):MDuncP);

                   If WinPos = *Zero;
                      filename = %trim(MDuncP);
                      exsr PrintObj;
                   Endif;
                Endif;
             Endif;

             ReadE (CMkey:CMseq#) pmd00;
          EndDo;
       EndIf;
       Endsr;
       //===================================================================
       // Print Object
       //===================================================================
       Begsr PrintObj;
         If *inof = *on;
            Except Hdg;
         Endif;

         If *in77 = *off;      //Detail time
            outObj = filename;

            If stat(filename:%addr(FileStats))< 0 ;
               *in90 = *On;
            Else;
               *in90 = *Off;
               p_statds = %addr(FileStats);
               ObjSize = st_size;
               TotBytes = TotBytes + ObjSize;
               If TotBytes > 0999999999;
                  TotGigaBytes = TotGigaBytes + 1;
                  TotBytes = TotBytes - 1000000000;
               Endif;
            Endif;

            Except Dtl;
         Else;
            Except Tot;
         Endif;
       Endsr;
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

       // Print report headings
       Time6 = %time();
       *in77 = *off;

       If inMode = 'R';
          Except Hdg;
       Endif;

      /end-free
     p ProgramStart    e
       //===================================================================
       // WriteSDEHP1 - Write to Darts Extract
       //===================================================================
     p WriteSDEHP1     b
     d WriteSDEHP1     pi

      /free

       Clear peh0p1;

       EHtrDt    = %Dec(%date():*cymd);
       EHtrTm    = %int(%char(%time():*ISO0));
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

       Write peh0p1;

      /end-free
     p WriteSDEHP1     e
