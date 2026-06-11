     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR':'QC2LE')
     H/TITLE STWRTDTA - Secured Data Write Program
     H*=====================================================================
     H* Created: 09/21/22 Project: WC2838  C: 291
     H*=====================================================================
     H* Revisions:
     H*
     H*=====================================================================

      //Secured Key File
       Dcl-F smskp usage(*input) Rename(psk01:psk00) usropn;
      //Secured Storage File
       Dcl-F smssl usage(*input:*update:*output) keyed usropn;

      * System includes
      /Copy QSYSINC/QRPGLESRC,QUSEC
      /Copy QSYSINC/QRPGLESRC,QC3CCI
      /copy sPrDate

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
       Dcl-Pr EncryptDataAPI ExtProc('Qc3EncryptData');
        argIn         Char(65535) Const options(*varsize);
        argInLen      Int(10:0) Const;
        argInFmt      Char(8) Const;
        argAlg        Char(65535) Const options(*varsize);
        argAlgFmt     Char(8) Const;
        argKey        Char(65535) Const options(*varsize);
        argKeyFmt     Char(8) Const;
        argCryptoPro  Char(1) Const;
        argCryptoDev  Char(10) Const;
        argOut        Char(65535);
        argOutBuffLen Int(10:0) Const;
        argOutLen     Int(10:0);
        argError      likeds(QUSEC);
       End-Pr;

       Dcl-Pr EncryptData  Char(65535);
        argData Varchar(65535) const options(*varsize) ;
        argOutLen Int(10:0);
       End-Pr;

       Dcl-Pr GenPRN  ExtProc('Qc3GenPRNs');
        PRNData    Char(1);
        PRNDataLen Int(10:0) Const;
        PRNType    Char(1) Const;
        PRNParity  Char(1) Const;
        errCod     Char(1);
       End-Pr;

       Dcl-Pr GetSuffix  ExtPgm('SA030R');
        outSuffix Char(3);
       End-Pr;

       //Program Status Data Structure
       Dcl-Ds SZQ1P ext psds;
       End-Ds;

      //Local variable
       Dcl-S outsuffix Char(3) Inz;
       Dcl-S index     Packed(3);
       Dcl-S i         Packed(3);
       Dcl-S i1        Packed(3);
       Dcl-S int#OfParms Packed(3);
       Dcl-S inSSFLD   Char(256);
       Dcl-S cryptoPro  Char(1);
       Dcl-S cryptoDev  Char(10);
       Dcl-S encryptLen Int(10);
       Dcl-S FldArr    Char(256) dim(9) inz;
       Dcl-S NumArr    Char(1)   dim(9) inz;

      //Data Structures
       Dcl-Ds  keyDesc Qualified;
        QC3D020000 likeDs(QC3D020000);
        QC3Key Char(1024);
       End-Ds;

       Dcl-Ds stdError Qualified;
        QUSEC likeDs(QUSEC);
        outError Char(1024);
       End-Ds;

       Dcl-Ds  SSFldDs;
        SSFld Char(2304);
        SSFld1 Overlay(SSFld);
        SSFld2 Overlay(SSFld:*next);
        SSFld3 Overlay(SSFld:*next);
        SSFld4 Overlay(SSFld:*next);
        SSFld5 Overlay(SSFld:*next);
        SSFld6 Overlay(SSFld:*next);
        SSFld7 Overlay(SSFld:*next);
        SSFld8 Overlay(SSFld:*next);
        SSFld9 Overlay(SSFld:*next);
        SSFldArr Char(256) Dim(9) Overlay(SSFld) inz;
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
             FldArr(i1) =  @@Data1;
          When i = 3;
             i1 += 1;
             NumArr(i1) =  @@Number2;
          When i = 4;
             FldArr(i1) =  @@Data2;
          When i = 5;
             i1 += 1;
             NumArr(i1) =  @@Number3;
          When i = 6;
             FldArr(i1) =  @@Data3;
          When i = 7;
             i1 += 1;
             NumArr(i1) =  @@Number4;
          When i = 8;
             FldArr(i1) =  @@Data4;
          When i = 9;
             i1 += 1;
             NumArr(i1) =  @@Number5;
          When i = 10;
             FldArr(i1) =  @@Data5;
          When i = 11;
             i1 += 1;
             NumArr(i1) =  @@Number6;
          When i = 12;
             FldArr(i1) =  @@Data6;
          When i = 13;
             i1 += 1;
             NumArr(i1) =  @@Number7;
          When i = 14;
             FldArr(i1) =  @@Data7;
          When i = 15;
             i1 += 1;
             NumArr(i1) =  @@Number8;
          When i = 16;
             FldArr(i1) =  @@Data8;
          When i = 17;
             i1 += 1;
             NumArr(i1) =  @@Number9;
          When i = 18;
             FldArr(i1) =  @@Data9;
          Endsl;
       Endfor;

       // Open SMSKP file
       If not %Open(smskp);
          Open smskp;
       Endif;
       // Read first (only) record to get encrypted file key
       Read psk00;

       // Open SMSSL
       If not %Open(SMSSL);
          Open SMSSL;
       endif;

       CipherSetup(SKkey);

       GenPRN( QC3IV :16 :'0' :'0' :QUSEC);
       Chain (@@Co#:@@Type:@@TypeKey) pss01;
       // Repeat loop for number of Number/data parameter pairs passed in
       Eval(h) index = index/2;
       For i = 1 to index;
          If NumArr(i) <> *Blank and FldArr(i) <> *Blank;
             inSSFld     = FldArr(i);
             encryptLen  = 0;
             SSFldArr(i) = EncryptData(inSSFld: encryptLen);
          Endif;
       Endfor;

       // Write to secured storage file
       SSuser = q1user;
       SSpgm  = q1pgm;
       SStime = Timeto6(SystemTime());
       SSdt   = Dateto7(SystemDate());
       If Not %Found(SMSSL);
          SSco#  = @@Co#;
          SSType = @@Type;
          SSkey  = @@TypeKey;
          SSIV = QC3IV;
          SSiusr = q1user;
          SSipgm = q1pgm;
          SSitim = Timeto6(SystemTime());
          SSidt  = Dateto7(SystemDate());
          Write pss01;
       Else;
          SSIV = QC3IV;
          Update pss01;
       Endif;

       close Smssl;
       close Smskp;
       *Inlr = *On;

       //===================================================
       // CipherSetup() using Block Cipher Algorithms
       //===================================================
       Dcl-Proc CipherSetup;
       Dcl-Pi *N;
        inSecretKey Char(32);
       End-Pi;

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
       // EncryptData - Encrypt data
       //===================================================
       Dcl-Proc EncryptData;
       Dcl-Pi *N Char(65535);
        argData VarChar(65535) const options(*varsize);
        argOutLen Int(10:0);
       End-Pi;
       Dcl-S svOutData Char(65535);
       Dcl-S svBytes   Char(52);
       Dcl-S length Int(10);

       length = %len(%Trim(argData));

       EncryptDataAPI(argData:%len(%Trim(argData)):'DATA0100':
                   QC3D0200:'ALGD0200':
                   keyDesc:'KEYD0200':
                   cryptoPro:cryptoDev:
                   svOutData:%size(svOutData):argOutLen:
                   stdError);

        If stdError.QUSEC.QUSBAVL > 0;
           svBytes = 'Error: ' + stdError.QUSEC.QUSEI;
           svOutData = *Blanks;
        Endif;
        Return svOutData;
       End-Proc;

