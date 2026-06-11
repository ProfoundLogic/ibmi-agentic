     hCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     hOption(*NoDebugIO:*SrcStmt)
     hDftactgrp(*No) Actgrp(*Caller)
     hBnddir('WTBNDDIR')
      //======================================================
      // Called from WXARIFS which created QTEMP/DOCLIST
      // Loops through the records and calls WXARIMPC2 to
      //  submit a job for each file.
      //======================================================
      // Revisions:
      //
      // Created for 150WEU on 06/08/20 in V20R2
      //======================================================
      /Free
       Dcl-F DOCLIST Usage(*Input) Rename(pdl01:pdl0p);
       Dcl-F SDACL Usage(*Input) Keyed Rename(pac01:pac00);
       Dcl-S strKey Char(10) Inz('AR_Import_');
       Dcl-S outFolder Char(256);
       Dcl-S outFileName Char(256);
       Dcl-S outMode Char(10) Inz('AUTOJOB');
       Dcl-Pr Submitter ExtPgm('WXARIMPC2');
        outFolder Char(256);
        outFileName Char(256);
        outMode Char(10) Const;
       End-Pr;
       // Return if A/R Import is not set up.
       Chain strKey pac00;
       If Not(%Found);
          *Inlr = *On;
          Return;
       EndIf;
       // The files were moved to the output folder by WXARIFS
       outFolder = ACArOutFld;
       Dou %Eof(DOCLIST);
          Read pdl0p;
          If %Eof;
             Leave;
          Endif;
         // LSTIFS also lists directories (*DIR). We just want to
         //  process the files (*STMF). Submit a job for each
         //  file dropped into the 'hot' folder.
          If %Subst(DLitem:1:5) = '*STMF';
             outFileName = %Subst(DLitem:13);
             %Subst(outFolder:256:1) = '$';
             %Subst(outFileName:256:1) = '$';
             Submitter(outFolder:outFileName:outMode);
          Endif;
       Enddo;
       *Inlr = *On;
       Return;
      /End-Free
