     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR':'QC2LE')
     H/TITLE STRTVDTA - Secured Data retrieve Data
     H*=====================================================================
     H* Created: 09/29/22 Project: WC2838  C: 291
     H*=====================================================================
     H* Revisions:
     H*
     H*=====================================================================

      //Secured Key File
       Dcl-F smskp usage(*input) Rename(psk01:psk00) usropn;
      //Secured Storage File
       Dcl-F smssl usage(*input:*update:*output) keyed usropn;

      * System includes
     d/copy QSYSINC/QRPGLESRC,QUSEC
     d/copy QSYSINC/QRPGLESRC,QC3CCI
     d/copy sPrDate

      //entry parameters
       Dcl-Pi *N;
        @@Co#      Packed(3:0);
        @@Type     Char(1);
        @@TypeKey  Char(50);
        @@Number1  Char(1);
        @@Data1    Char(256);
        @@Number2  Char(1)   Options(*Nopass);
        @@Data2    Char(256) Options(*Nopass);
        @@Number3  Char(1)   Options(*Nopass);
        @@Data3    Char(256) Options(*Nopass);
        @@Number4  Char(1)   Options(*Nopass);
        @@Data4    Char(256) Options(*Nopass);
        @@Number5  Char(1)   Options(*Nopass);
        @@Data5    Char(256) Options(*Nopass);
        @@Number6  Char(1)   Options(*Nopass);
        @@Data6    Char(256) Options(*Nopass);
        @@Number7  Char(1)   Options(*Nopass);
        @@Data7    Char(256) Options(*Nopass);
        @@Number8  Char(1)   Options(*Nopass);
        @@Data8    Char(256) Options(*Nopass);
        @@Number9  Char(1)   Options(*Nopass);
        @@Data9    Char(256) Options(*Nopass);
       End-Pi;

       // Prototypes
       Dcl-Pr DecryptDataAPI ExtProc('Qc3DecryptData');
        inArg       Char(65535) Const options(*varsize);
        inArgLen    Int(10:0) Const;
        inAlg       Char(65535) Const options(*varsize);
        inAlgFmt    Char(8) Const;
        inKey       Char(65535) Const options(*varsize);
        inKeyFmt    Char(8) Const;
        inCryptoPro Char(1) Const;
        inCryptoDev Char(10) Const;
        outArg      Char(65535);
        outBuffLen  Int(10:0) Const;
        outLen      Int(10:0);
        outError    Likeds(QUSEC);
       End-Pr;

       Dcl-Pr GetSuffix  ExtPgm('SA030R');
        outsuffix Char(3);
       End-Pr;

      //Local variable
       Dcl-S outsuffix Char(3) Inz;
       Dcl-S index     Packed(3);
       Dcl-S i         Packed(3);
       Dcl-S i1        Packed(3);
       Dcl-S int#OfParms Packed(3);
       Dcl-S inSSFLD   Char(256);
       Dcl-S encryptLen Int(10);
       Dcl-S cryptoPro  Char(1);
       Dcl-S cryptoDev  Char(10);
       Dcl-s inFldArr  Char(256) dim(9) inz;
       Dcl-s NumArr    Char(1)   dim(9) inz;
       Dcl-s outFldArr Char(256) dim(9) inz;

       //Data Structures
       Dcl-Ds  keyDesc Qualified;
        QC3D020000 likeDs(QC3D020000);
        QC3Key char(1024);
       End-Ds;

       Dcl-Ds stdError Qualified;
        QUSEC likeDs(QUSEC);
        outError Char(1024);
       End-Ds;

       Dcl-Ds  SSFldDs;
        SSFld Char(2304);
        SSFld1 overlay(SSFld);
        SSFld2 overlay(SSFld:*next);
        SSFld3 overlay(SSFld:*next);
        SSFld4 overlay(SSFld:*next);
        SSFld5 overlay(SSFld:*next);
        SSFld6 overlay(SSFld:*next);
        SSFld7 overlay(SSFld:*next);
        SSFld8 overlay(SSFld:*next);
        SSFld9 overlay(SSFld:*next);
        SSFldArr char(256) dim(9) Overlay(SSFld) inz;
       End-Ds;

       // Retrieve client suffix
       GetSuffix(outSuffix);

       // No of Paramaters Passed
       int#OfParms = %Parms;
       If int#OfParms > 3;
          index = (int#OfParms - 3);
       Else;
          index = 1;
       Endif;

       For i = 1 to index;
          Select;
          When i = 1;
             i1 = 1;
             NumArr(i1) =  @@Number1;
          When i = 2;
             inFldArr(i1) =  @@Data1;
          When i = 3;
             i1 += 1;
             NumArr(i1) =  @@Number2;
          When i = 4;
             inFldArr(i1) =  @@Data2;
          When i = 5;
             i1 += 1;
             NumArr(i1) =  @@Number3;
          When i = 6;
             inFldArr(i1) =  @@Data3;
          When i = 7;
             i1 += 1;
             NumArr(i1) =  @@Number4;
          When i = 8;
             inFldArr(i1) =  @@Data4;
          When i = 9;
             i1 += 1;
             NumArr(i1) =  @@Number5;
          When i = 10;
             inFldArr(i1) =  @@Data5;
          When i = 11;
             i1 += 1;
             NumArr(i1) =  @@Number6;
          When i = 12;
             inFldArr(i1) =  @@Data6;
          When i = 13;
             i1 += 1;
             NumArr(i1) =  @@Number7;
          When i = 14;
             inFldArr(i1) =  @@Data7;
          When i = 15;
             i1 += 1;
             NumArr(i1) =  @@Number8;
          When i = 16;
             inFldArr(i1) =  @@Data8;
          When i = 17;
             i1 += 1;
             NumArr(i1) =  @@Number9;
          When i = 18;
             inFldArr(i1) =  @@Data9;
          Endsl;
       Endfor;

       // Open SMSKP file
       If not %Open(SMSKP);
          Open SMSKP;
       Endif;
       // Read first (only) record to get encrypted file key
       Read psk00;
       if %Eof(smskp);
          Close smskp;
       endif;

       // Open SMSSL
       If not %Open(SMSSL);
          Open SMSSL;
       Endif;

       CipherSetup(SKkey);

       Clear outFldArr(i);
       Chain (@@Co#:@@Type:@@TypeKey) pss01;
       If %Found(smssl);
       // Repeat loop for number of Number/data parameter pairs passed in
          Eval(h) index = index/2;
          For i = 1 to index;
             If NumArr(i) <> *Blank;
                QC3IV = SSIV;
                If SSfldArr(i) <> ' ';
                   inSSFld = SSFldArr(i);
                   encryptlen = %Len(%Trim(inSSFld));
                   outFldArr(i) = DecryptData(inSSFld: encryptLen);
                EndIf;
             Endif;
          Endfor;

          For i = 1 to index;
             Select;
             When i = 1;
                @@Data1 = outFldArr(i);
             When i = 2;
                @@Data2 = outFldArr(i);
             When i = 3;
                @@Data3 = outFldArr(i);
             When i = 4;
                @@Data4 = outFldArr(i);
             When i = 5;
                @@Data5 = outFldArr(i);
             When i = 6;
                @@Data6 = outFldArr(i);
             When i = 7;
                @@Data7 = outFldArr(i);
             When i = 8;
                @@Data8 = outFldArr(i);
             When i = 9;
                @@Data9 = outFldArr(i);
             Endsl;
          Endfor;
       Endif;

       Close SMSSL;
       *Inlr = *On;

       //===================================================
       // CipherSetup() using Block Cipher Algorithms
       //===================================================
       Dcl-Proc CipherSetup;
       Dcl-Pi *N;
        inSecretKey Char(32);
       End-Pi;

       //Intialize QC3D0200 data structure
       QC3D0200 = *AllX'00';
       QC3BCA = 22;
       QC3MODE = '1';
       QC3PO = '1';
       QC3EKS = 0;
       QC3BL = 16;
       // Initialize the Key Data Structure QC3D020000
       keyDesc.QC3D020000 = *AllX'00';
       keyDesc.QC3D020000.QC3KT = 22;         //22=AES
       keyDesc.QC3D020000.QC3KF = '0';
       keyDesc.QC3D020000.QC3KSL = %len(%trim(inSecretKey));
       keyDesc.QC3Key = %trim(inSecretKey);
       // Initialize the Rest of the Parameters
       stdError.QUSEC = *AllX'00';
       stdError.QUSEC.QUSBPRV = 1040;
       cryptoPro = '0';
       cryptoDev = *BLANKS;

       End-Proc;

       //===================================================
       // DecryptData - Decrypt data
       //===================================================
       Dcl-Proc DecryptData;
       Dcl-Pi *N Char(65535);
        inArgData VarChar(65535) const options(*varsize);
        inArgLen Int(10:0);
       End-Pi;
       Dcl-S svOutData Char(65535);
       Dcl-S svOutDataLen Int(10:0) Inz(0);

       DecryptDataAPI(inArgData:inArgLen:
            QC3D0200   :'ALGD0200':
            keyDesc    :'KEYD0200':
            cryptoPro:cryptoDev:
            svOutData:%size(svOutData):svOutDataLen:
            stdError);
        Return svOutData;
       End-Proc;
