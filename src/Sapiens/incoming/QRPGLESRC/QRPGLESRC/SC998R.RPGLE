     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *========================================================================
      *========================================================================
    H/TITLE SC998R - Image Transfer
      *========================================================================
      * Description: This program transfers the Image Objects
      *
      * Directory for output: /QNTC/WINSERVER/XXXXXXXXXXXXXXXXXXX
      *                                         inPath
      *
      *     Claim Key  40A
      *     RtnCode     1A
      *
      *  Read/Loop all records for the key (SDCMP) and each applicable
      *  document tranfer object/doc to win server.
      *
      *=====================================================================
      * Created on 10/05/2018 by c:180 for Project 129WVD
      *                                Cloned from SC992R.
      *========================================================================
     * Revisions:
     *
      *========================================================================
     Fsdcmla    uf   e           k Disk    Rename(pcm01:pcm0a)
     Fimdolc    uf   e           k Disk
     Fsmmdl     uf   e           k DISK    Rename(pmd01:pmd00)
     d**********************************************************************
     d* Prototypes
     d**********************************************************************
     d* Transfer IFS Objects
     d TrfIfsObj       pr                  ExtPgm('TRFIFSC')
     d frmObj                      1024
     d toObj                       1024
     d returnCode2                    2

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d clientID                       3

     d* Get Document Name
     d GetDocName      pr            80
     d inUncPath                    255

     d* Get Path Name
     d GetPathName     pr           175
     d inUncPath                    255

     d* Get Root Directory
     d GetRootDir      pr                  ExtPgm('RTVDOCROOT')
     d root                          10

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr

     d* Create Directory
     d CreateFlr       pr                  ExtPgm('SMDIFS')
     d                               80
     d                                1

     d* Check Non-Image Optical
     d ChkNonImgOpt    pr                  ExtPgm('SC871R')
     d key                           40
     d chrSeq#                        5    Const
     d document                      12
     d folder                        80
     d returnCode                     1

     d Stat            pr            10I 0 ExtProc('stat')
     d                                 *   value options(*string)
     d                                 *   value

     d/copy sPrCompany
     d/copy sPrDate
     d/copy wPrUsrPrms

     d**********************************************************************
     d* Entry Parameters
     d**********************************************************************
     d inKey           s             40
     d inSeq#          s              5
     d inPath          s            255
     d outPath         s            255
     d outRtCd         s              1

     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d sequence#       s              5  0
     d returnCode      s              1    Inz
     d returncode2     s              2    Inz
     d frmObj          s           1024
     d toObj           s           1024
     d clientID        s              3
     d dftDir          s             80
     d newDir          s             80
     d root            s             10    Inz
     d PrvDir          s             80    Inz
     d Flr             s             80    Inz
     d IntPos          s              5  0 Inz
     d BegPos          s              5  0 Inz
     d WinPos          s              5  0 Inz
     d Path            s            255    Inz
     d chrSeq#         s              5    Inz
     d XXuncN          s            255    Inz
     d p_statds        s               *

     d**********************************************************************
     d* Data Structures
     d**********************************************************************
     d psds          esds                  extname(szq1p)

     d statds          DS                  Based(p_statds)
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

     d**********************************************************************
     d* Constants
     d**********************************************************************
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     c*=================================================================================
     C     *Entry        Plist
     C                   Parm                    inKey
     C                   Parm                    inSeq#
     C                   Parm                    inPath
     C                   Parm                    outPath
     C                   Parm                    outRtCd
     C*
      /free

       ProgramStart();

       Main();

       *inLR = *On;
       Return;

      /end-free
       //===================================================================
       // Main - Main Processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       Chain (inKey:sequence#) pcm0a;
       If %Found(sdcmla);

          If CMFL05 <> 'O' and CMuncN <> *blanks;

             //Build the "TO" directory
             WinPos = %scan(%trim(inPath):CMuncN);

             If WinPos = *Zero and stat(%Trim(CMuncN):%addr(FileStats)) >= 0;
                newDir  = %Trim(inPath) + %Trim(GetPathName(CMuncN));

                If newDir <> PrvDir;
                   XXuncN = CMuncN;
                   exsr CrtFolders;
                   PrvDir = newDir;
                Endif;


                // Transfer Object
                FrmObj = CMuncN;
                ToObj =  %Trim(inPath) + %Trim(CMuncN);
                TrfIfsObj(FrmObj:ToObj:ReturnCode2);
                If ReturnCode2 = *blanks;
                   CMuncN = ToObj;
                   Update PCM0a;
                   OutPath = ToObj;
                   OutRtCd = 'P';

                //Update Image Path
                   If CMimgf = 'Y' or CMimgf = 'I';
                      Setll (CMkey:CMseq#) PDO01;
                      Reade (CMkey:CMseq#) PDO01;
                      Dow not %eof(IMdolc);
                         If CMidoc = DOdcid and DOuncN <> *blanks;
                            DOuncN = %Trim(inPath) + %Trim(DOuncN);
                            Update PDO01;
                         Endif;
                         Reade (CMkey:CMseq#) PDO01;
                      Enddo;
                   Endif;
                Endif;

             Endif;
          Endif;

          //Let's process additional objects
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
                      //Build the "TO" directory
                      WinPos = %scan(%trim(inPath):MDuncP);

                      If WinPos = *Zero and
                         stat(%Trim(MDuncP):%addr(FileStats)) >= 0;
                         newDir  = %Trim(inPath) + %Trim(GetPathName(MDuncP));

                         If newDir <> PrvDir;
                            XXuncN = MDuncP;
                            exsr CrtFolders;
                            PrvDir = newDir;
                         Endif;

                         FrmObj = MDuncP;
                         ToObj =  %Trim(inPath) + %Trim(MDuncP);
                         TrfIfsObj(FrmObj:ToObj:ReturnCode2);
                         If ReturnCode2 = *blanks;
                            MDuncP = ToObj;
                            Update pmd00;
                         Endif;

                      Endif;
                   Endif;
                Endif;
                ReadE (CMkey:CMseq#) pmd00;
             EndDo;
          EndIf;

       EndIf;

       //===================================================================
       // Create Folders
       //===================================================================
       Begsr CrtFolders;
       ReturnCode = *blanks;

       // Get Folders
       Path = %Trim(inPath) + %Trim(XXuncN);
       IntPos = %scan('/':Path);
       BegPos = IntPos;

       // Create one folder at a time.
       If intPos > 0;
          Dou intPos = *Zeros;
             If intPos > 1;
                Flr = %Subst(Path:BegPos:intPos-1);
                CreateFlr(Flr:ReturnCode);
             Endif;
             intPos = %scan('/':Path  :intPos + 1);
          Enddo;
       Endif;
       Endsr;
      /end-free
     p Main            e
       //===================================================================
       // GetDocName - Extract the Document name from the UNC path
       //===================================================================
     p GetDocName      b
     d GetDocName      pi            80
     d  inUncPath                   255

     d  intPos         s              5  0
     d  DocPos         s              5  0 INZ
     d  DocName        s             80    INZ
     d  Path           s            255

      /free

       Path = inUncPath;
       intPos = %scan('/':Path);
       If intPos > 0;
          Dou intPos = *Zeros;
             DocPos = intPos + 1;
             intPos = %scan('/':Path  :intPos + 1);
          Enddo;
          If DocPos > *zeros;
             DocName  = %subst(Path:DocPos);
          Endif;
       Endif;

       Return DocName;

      /end-free

     p GetDocName      e
       //===================================================================
       // GetPathName - Extract the Path name from the UNC path
       //===================================================================
     p GetPathName     b
     d GetPathName     pi           175
     d  inUncPath                   255

     d  intPos         s              5  0
     d  PathName       s            255    INZ
     d  Path           s            255
     d  PathLen        s              5  0

      /free

       Path = inUncPath;
       intPos = %scan('/':Path      );
       If intPos > 0;
          Dou intPos = *Zeros;
             PathLen = intPos -1 ;
             intPos = %scan('/':Path  :intPos + 1);
          Enddo;
          If PathLen > *zeros;
             PathName  = %subst(Path:1:PathLen);
          Endif;
       Endif;

       Return PathName;

      /end-free

     p GetPathName     e
       //===================================================================
      *// ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

     d suffix          s              3    Inz(' ')

      /free

       // Get the client id
       GetClientID(clientID);

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Initialize Fields
       sequence# = %Dec(inSeq#:5:0);

       // Get Root Directory
       GetRootDir(root);

       // Get Default Windows Server Path
       dftDir  = %trim(inPath) + '/' + %trim(root);


      /end-free
     p ProgramStart    e
