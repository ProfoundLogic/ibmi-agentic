     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *========================================================================
      *========================================================================
    H/TITLE SC992R - Darts Document Extract
      *========================================================================
      * Description: This program processes the Darts Extract Objects
      *
      * Directory for output: /DARTS_EXTRACT/XXXXXXXXXXXXXXXXXXX
      *                                         inPath
      *
      *     Claim Key  40A
      *     RtnCode     1A
      *
      *  Read/Loop all records for the key (SDCMP)
      *      Each:
      *           If doc on major remark, copy the doc from current location
      *              to the IFS directory (Darts Key)
      *              DocID will be DartKey *CAT Seq# *CAT Blanks for Major
      *
      *           IF doc is or is not on major remark, check the more objects
      *              (SMMDP) to see if any exist, and if so, loop for each:
      *              copy the object from the current loc to the IFS directory
      *              (see above) docID is DartKey *CAT Seq# *CAT unique MD value
      *              loop for all
      *
      *=====================================================================
      * Created on 07/18/2017 by c:62 for Project 81WLL
      *========================================================================
     * Revisions:
     *
      *========================================================================
     Fsdcmla    if   e           k Disk    Rename(pcm01:pcm0a)
     Fsmoll1    if   e           k Disk
     Fsmmdl     if   e           k Disk    Rename(pmd01:pmd00)
     Fsxdep     o    e             Disk    Rename(pde01:pde0p)
     d**********************************************************************
     d* Prototypes
     d**********************************************************************
     d* Check Image Optical
     d ChkImgOpt       pr                  ExtPgm('IS977R')
     d key                           40
     d chrSeq#                        5    Const
     d document                      12
     d folder                        80
     d returnCode                     1

     d* Check Non-Image Optical
     d ChkNonImgOpt    pr                  ExtPgm('SC871R')
     d key                           40
     d chrSeq#                        5    Const
     d document                      12
     d folder                        80
     d returnCode                     1

     d* Check Object Optical
     d ChkObjOpt       pr                  ExtPgm('IS977R1')
     d key                           40
     d chrSeq#                        5    Const
     d systemName                    20
     d objectPath                    60
     d objectID                      40
     d optFlag                        1
     d returnCode                     1
     d

     d* Copy Objects
     d CopyObject      pr                  ExtPgm('CPYOBJC')
     d fromDir                       40
     d fromObj                      132
     d toDir                         40
     d toObj                        132
     d returnCode3                    2

     d* Copy IFS Objects
     d CopyIfsObject   pr                  ExtPgm('CPYIFSC')
     d fromPath                    1024
     d toPath                      1024
     d returnCode3                    2

     d* Convert To PDF
     d CvtToPDF        pr                  ExtPgm('SC951C2')
     d  spoolName                    10
     d  jobNum                        6
     d  jobUser                      10
     d  jobName                      10
     d  spoolNum2                     6
     d  storePath                   255
     d  returnCode                    1

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d clientID                       3

     d* Get client ID
     d GetDocument     pr            80
     d uncN                         255

     d* Get Root Directory
     d GetRootDir      pr                  ExtPgm('RTVDOCROOT')
     d root                          10

     d* Main Processing
     d Main            pr

     d* Print Document
     d PrintDoc        pr                  ExtPgm('SC951C1')
     d  document                     12
     d  folder                       80
     d  key                          40
     d  chrSeq#                       5
     d  optFlag                       1
     d  returnCode                    1
     d  outQueue                     10
     d  copies                        3
     d  spoolName                    10
     d  spoolNum                      5
     d  jobName                      10
     d  jobNum                        6
     d  jobUser                      10
     d  uncn                        255
     d  fsf                           1

     d* Program Start
     d ProgramStart    pr

     d* Virtual Optical Documents
     d VrtOptDoc       pr                  ExtPgm('VRTOPTDC')
     d directory                     80
     d returnInfo                   272

     d* Write SXDEP for Additonal Objects
     d WriteSXDEP      pr

     d/copy sPrCompany
     d/copy sPrDate
     d/copy wPrUsrPrms
     d**********************************************************************
     d* Entry Parameters
     d**********************************************************************
     d inKey           s             40
     d inSeq#          s              5
     d inFromDt        s              7
     d inToDate        s              7
     d inDryType       s             10
     d inPath          s            255
     d outPath         s            255
     d outRtCd         s              1

     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d sequence#       s              5  0
     d chrSeq#         s              5
     d first           s               n   Inz(*On)
     d aSeq#           s              5  0 Inz(0)
     d aSeqChr         s              5    Inz(' ')
     d returnCode      s              1    Inz(' ')
     d xxDcID          s             12
     d x1DcID          s             12
     d xxFold          s             80
     d x1Fold          s             80
     d toObj           s            132
     d optFlag         s              1    Inz(' ')
     d returncode2     s              1    Inz(' ')
     d returnCode3     s              2    Inz(' ')
     d outQueue        s             10    Inz('QPRINT')
     d copies          s              3    Inz('001')
     d spoolName       s             10    Inz(' ')
     d spoolNum        s              5    Inz('00000')
     d spoolNum2       s              6
     d jobName         s             10    Inz(' ')
     d jobNum          s              6    Inz('000000')
     d jobUser         s             10    Inz(' ')
     d storePath       s            255
     d fromDir         s             80
     d fromObj         s            132
     d toDir           s             80
     d returnCoe3      s              2
     d clientID        s              3
     d dftDir          s             80
     d newDir          s             80
     d dotPos          s              5  0
     d root            s             10    Inz(' ')
     d remarkDate      s             10
     d dateCvt         s               d   DatFmt(*usa)
     d fromPath        s           1024
     d toPath          s           1024

     d**********************************************************************
     d* Data Structures
     d**********************************************************************
     d psds          esds                  extname(szq1p)

     D returnInfo      ds
     D  rtCd                   1      8
     D  msgID                  9     15
     D  rsv                   16     16
     D  msg                   17    272

     d**********************************************************************
     d* Constants
     d**********************************************************************
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D rootFlr         c                   CONST('\DARTS_EXTRACT')
     D rPgm            c                   CONST('RtvDocRoot')
     c*=================================================================================
     C     *Entry        Plist
     C                   Parm                    inKey
     C                   Parm                    inSeq#
     C                   Parm                    inFromDt
     C                   Parm                    inToDate
     C                   Parm                    inDryType
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
       // GetDocument - Extract the Document from the UNCN/P
       //===================================================================
     p GetDocument     b
     d GetDocument     pi            80
     d  inUncN                      255

     d  wrkUncN        s            255
     d  pos            s              5  0

      /free

       wrkUncN = inUncN;
       DoU pos = 0;
          pos     = %Scan('/':wrkUncN);
          If pos <> 0;
             wrkUncN = %Subst(wrkUncN:pos+1);
          EndIf;
       EndDo;

       Return %Subst(wrkUncN:1:80);

      /end-free

     p GetDocument     e
       //===================================================================
       // Main - Main Processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       Chain (inKey:sequence#) pcm0a;
       If %Found(sdcmla);

          // Prepare Remark Date which will be used in the naming
          dateCvt    = %Date(CMiDt:*CYMD);
          remarkDate = %Char(dateCvt);

          // Build the "TO" directory Name
          dftDir  = inPath;
          newDir  = %Trim(inPath) + '/' + %Trim(CMkey);

          // Set Values
          chrSeq# = %EditC(CMseq#:'X');

          If CMdcID <>  ' ';
             // Check if Image or Not
             If CMimgF = 'Y' Or CMimgF = 'I';
                xxDcID = CMiDoc;
                x1DcID = CMiDoc;
             Else;
                xxDcID = CMdcID;
                x1DcID = CMdcID;
             EndIf;

             x1Fold = CMfold;
             xxFold = CMfold;

             //Check to see if in Optical
             returnCode = ' ';
             If CMfl05 = 'O';
                If CMfl05 = 'O';
                   ChkImgOpt(CMkey:chrSeq#:CMdcid:CMfold:returnCode);
                Else;
                   ChkNonImgOpt(CMkey:chrSeq#:CMdcid:CMfold:returnCode);
                EndIf;

                If returnCode <> ' ';
                   outRtCd = 'K';
                EndIf;
             Endif;

             // No Errors Encountered So Far.
             If returnCode = ' ';
                // For All Optical and Non-Optical
                // Copy from QDLS to the "New" name on IFS
                If first = *On;
                   returnInfo = ' ';
                   VrtOptDoc(newDir:returnInfo);
                   If returnInfo <> ' ' and msgID <> 'CPFA0A0';
                      outPath = dftDir;
                   Else;
                      outPath = newDir;
                   EndIf;
                   first = *Off;
                EndIf;
             EndIf;

             // Call pgm to copy to IFS directory
             If CMuncN <> ' ';
                toObj = %EditC(CMseq#:'X') + '-M ' + %Trim(CMrmCd) + ' ' +
                  %Subst(remarkDate:1:2) + '-' + %Subst(remarkDate:4:2) + '-' +
                  %Subst(remarkDate:7:4) + '_' + %Trim(GetDocument(CMuncN));
             Else;
                toObj = %EditC(CMseq#:'X') + '-M ' + %Trim(CMrmCd) + ' ' +
                  %Subst(remarkDate:1:2) + '-' + %Subst(remarkDate:4:2) + '-' +
                  %Subst(remarkDate:7:4) + '_' + %Trim(xxDcId);
             EndIf;

             If CMimgF = ' ';
                PrintDoc(x1DcID:x1Fold:CMkey:chrSeq#:optFlag:returnCode:
                 outQueue:copies:spoolName:spoolNum:jobName:jobNum:jobUser:
                 CMuncN:CMifsP);
                If returnCode2 <> ' ';
                   outPath    = '**Error ' + returncode;
                   outRtCd    = returnCode;
                Else;
                   spoolNum2  = '0' + spoolNum;
                   returnCode = ' ';
                   dotPos = %Scan('.':toObj);
                   If dotPos <> 0;
                      toObj = %Subst(toObj:1:dotPos) + 'PDF';
                   Else;
                      toObj = %Trim(toObj) + '.PDF';
                   EndIf;

                   storePath = %Trim(outPath) + '\' + %Trim(toObj);

                   CvtToPDF(spoolName:jobNum:jobUser:jobName:spoolNum2:
                    storePath:returncode);

                   If returncode <> ' ';
                      outRtCd = 'M';
                      outPath = ' ';
                   Else;
                      outRtCd = 'P';
                      outPath = storePath;
                   EndIf;
                EndIf;
             Else;
                toDir       = newDir;
                If CMuncN <> ' ';
                   fromPath = CMuncN;
                   toPath   = %Trim(newDir) + '\' + toObj;
                   returnCode3 = ' ';
                   CopyIfsObject(fromPath:toPath:returnCode3);
                Else;
                   fromDir     = '\' + %trim(root) + '\' + %Trim(x1Fold);
                   fromObj     = x1DcID;
                   returnCode3 = ' ';
                   CopyObject(fromDir:fromObj:toDir:toObj:returnCode3);
                EndIf;

                If returnCode <> ' ';
                   If returncode3 = '99';
                      outPath = %Trim(toDir) + '\' + %Trim(toObj);
                      CMiUsr  = '99';
                   Else;
                      outPath = '**Error ' + returnCode3;
                   EndIf;
                   outRtCd = 'J';
                Else;
                   outPath    = %Trim(toDir) + '\' + %Trim(toObj);
                   outRtCd = 'P';
                EndIf;
             EndIf;
          EndIf;

          // Let's populate additional objects
          Chain (CMkey:CMseq#) pmd00;
          If %Found(smmdl);
             DoU %Eof(smmdl);
                Chain (CMkey:CMseq#:MDoFlr:MDoID) pol01;
                If %Found(smoll1);
                   ChkObjOpt(CMkey:chrSeq#:OLsysN:OLoFlr:OLoID:OLoptF:
                    returnCode);
                EndIf;

                // If Building Directory for the first time
                If first = *On;
                   returnInfo = ' ';
                   VrtOptDoc(newDir:returnInfo);
                   If returnInfo <> ' ' and msgID <> 'CPFA0A0';
                      outPath = dftDir;
                   Else;
                      outPath = newDir;
                   EndIf;
                   first = *Off;
                EndIf;

                // Build Names
                aSeq#  += 1;
                aSeqChr = %EditC(aSeq#:'X');
                toObj = %EditC(CMseq#:'X') + '-' + aSeqChr + '-O ' +
                  %Trim(CMrmCd) + ' ' + %Subst(remarkDate:1:2) +
                  '-' + %Subst(remarkDate:4:2) +
                  '-' + %Subst(remarkDate:7:4) +
                  '_' + %Trim(MDoID);

                // Copy Object
                toDir = newDir;
                If MDuncP <> ' ';
                   fromPath = MDuncP;
                   toPath   = %Trim(newDir) + '\' + toObj;
                   returnCode3 = ' ';
                   CopyIfsObject(fromPath:toPath:returnCode3);
                Else;
                   fromDir     = '\' + %trim(root) + '\' + %Trim(MDOflr);
                   fromObj     = MDoID;
                   returnCode3 = ' ';
                   CopyObject(fromDir:fromObj:toDir:toObj:returnCode3);
                EndIf;
                If returnCode3 <> ' ';
                   CMstat = 'X';
                   CMiUsr = returnCode3;
                   If returncode3 = '99';
                      CMuncN  = %Trim(toDir) + '\' + %Trim(toObj);
                   Else;
                      outPath = '**Error ' + returnCode3;
                   EndIf;
                   If outRtCd <> 'P';
                      outRtCd = 'J';
                   EndIf;
                Else;
                   outPath    = %Trim(toDir) + '\' + %Trim(toObj);
                   outRtCd = 'P';
                EndIf;

                // Update SXDEP
                WriteSXDEP();

                // Read Next Object
                ReadE (CMkey:CMseq#) pmd00;
             EndDo;
          EndIf;
       EndIf;

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

       // Initialize Fields
       sequence# = %Dec(inSeq#:5:0);

       // Get Root Directory
       GetRootDir(root);

      /end-free
     p ProgramStart    e
       //===================================================================
       // WriteSXDEP - Write SXDEP
       //===================================================================
     p WriteSXDEP      b
     d WriteSXDEP      pi

      /free

       CMpgm  = 'SC952R';
       CMuser = 'MDRCD';
       CMdt   = DateTo7(systemDate());
       CMtime = TimeTo6(systemTime());

       Write pde0p;

      /end-free

     p WriteSXDEP      e
