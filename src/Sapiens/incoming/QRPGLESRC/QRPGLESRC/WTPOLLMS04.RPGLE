     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio: *srcstmt) decedit('0.')
      //--------------------------------------------------------------------
      //
      // Description: Response program for claim handling instructions
      //              - Activation group should be *CALLER
      //              - This process should not lock any file
      //
      // Called from: Ajax calls
      // Calls to:
      //
      // Created on 09/04/2014 by C:279
      //--------------------------------------------------------------------
      //Revisions:
      //
      //--------------------------------------------------------------------
     fwdchl     uf a e           k disk    rename(pch01:pch00)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)

      /copy sprUsrPrms
      /copy sPrAtrium
      /copy sPrString
      /copy WprEmpLr
      /copy sPrDate
      /copy CGIDEV2/qrpglesrc,prototypeb
      /copy CGIDEV2/qrpglesrc,usec
      /copy sCopyCGI01

     d psds          esds                  extname(szq1p)

     d err             ds                  qualified
     d bytesProv                     10i 0 inz(0)
     d bytesAvail                    10i 0 inz(0)

      * Number of variables
     d nbrVars         s             10i 0

      * Saved query string
     d savedquerystring...
     d                 s          32767    varying

      * work variables
     d bigCharVar      s          32767A
     d CCSID           s             10i 0 inz(0)
     d CRLF            c                   X'0d25'
     d charVar         s            500a   varying
     d errMsg          s            500a   varying
     d intDiv          s              3  0 inz
     d intDate         s              7  0
     d intRrn          s              9  0
     d inCmd           s           2750    inz
     d length          s             10i 0
     d strJobName      s             20A
     d strJobUser      s             10    inz
     d strUser         s             10    inz
     d strEnvironment  s             10    inz
     d strJobNumber    s              6    inz
     d strRrn          s              9A
     d strType         s             15A
     d strInst         s          32000A

      /include yajl_h
      /copy sCopyCGI02

      /free
       //-----------------------------------------------------------------------
       // Subroutine - Process
       //-----------------------------------------------------------------------
       Begsr Process;

       Select;
       When strType = 'UpdateOnly' or strType = 'UpdateAll' or strType =
        'UpdateAllCx';
          Exsr ProcUpd;

       When strType = 'DisplayEmp';
          Exsr ProcEmp;

       When strType = 'DisplayClaim';
          Exsr ProcClm;
       Endsl;

       Endsr;
       //-----------------------------------------------------------------------
       // ProcUpd -  Process Update Claim Handling Instructions
       //-----------------------------------------------------------------------
       Begsr ProcUpd;

       Chain intRrn pel0p;
       If %found(wdelp);
          intDiv = ELdiv;
       Endif;

       intDate = DateTo7(SystemDate);
       Exsr ProcUpdWrtInst;

       If strType = 'UpdateAll' or strType = 'UpdateAllCx';
          Setll (ELco#:ELfnd:ELemp#) pem00;
          Reade (ELco#:ELfnd:ELemp#) pem00;
          Dow not %eof(wmeml);
             If strType='UpdateAllCx' or GetCxDate(ELco#:ELfnd:ELemp#:EMdiv)=0
              or GetCxDate(ELco#:ELfnd:ELemp#:EMdiv) > intDate;
                intDiv = EMdiv;
                Exsr ProcUpdWrtInst;
             Endif;
             Reade (ELco#:ELfnd:ELemp#) pem00;
          Enddo;
       Endif;

       Endsr;
       //-----------------------------------------------------------------------
       // ProcUpdWrtInst -  Process Update/Write Claim Handling Instructions
       //-----------------------------------------------------------------------
       Begsr ProcUpdWrtInst;

       Chain (ELco#:ELfnd:ELemp#:intDiv) pch00;
       CHclmInst = strInst;
       CHuser = q1User;
       CHpgm  = q1Pgm;
       CHtime = TimeTo6(SystemTime());
       CHdt   = DateTo7(SystemDate);

       If not %found(wdchl);
          CHco#  = ELco#;
          CHfnd  = ELfnd;
          CHemp# = ELemp#;
          CHdiv  = intDiv;
          CHiusr = q1User;
          CHipgm = q1Pgm;
          CHitim = CHtime;
          CHidt  = CHdt;

          write pch00;
       Else;
          update pch00;
       Endif;
       unlock wdchl;

       Endsr;
       //-----------------------------------------------------------------------
       // ProcEmp -  Process Display Employer
       //-----------------------------------------------------------------------
       Begsr ProcEmp;

       Chain intRrn pel0p;

       clear CHclmInst;

       Chain(n) (ELco#:ELfnd:ELemp#:ELdiv) pch00;
       If %found;
          ProcInst();
       Endif;

       Endsr;
       //-----------------------------------------------------------------------
       // ProcClm -  Process Display Claim
       //-----------------------------------------------------------------------
       Begsr ProcClm;

       Chain intRrn pcm0p;

       clear CHclmInst;

       Chain(n) (CMco#:CMfnd:CMemp#:CMdiv) pch00;
       If %found;
          ProcInst();
       Endif;

       Endsr;

      /end-free
      *-------------------------------------------------------------------------
      * Procedure name: ProcInst
      * Purpose:        Process Instructions
      * Returns:
      *-------------------------------------------------------------------------
     p ProcInst        b
     d ProcInst        pi
      /free

       yajl_addChar('instructions':CHclmInst);

      /end-free

     p ProcInst        e
      *-------------------------------------------------------------------------
      * Procedure name: getQueryString
      * Purpose:        Get the valkues passed in the query string
      * Returns:
      *-------------------------------------------------------------------------
     p getQueryString  b
     d getQueryString  pi
     d number6         s              6  0 inz

      /free

       nbrVars =  zhbgetinput(savedquerystring:qusec);

       strJobName   = ZhbGetVar('jobName');
       strJobNumber = ZhbGetVar('jobNumber');
       number6      = %int(%trim(strJobNumber));
       strJobNumber = %editc(number6 : 'X');
       strJobUser   = ZhbGetVar('jobUser');
       strUser      = ZhbGetVar('user');
       strRrn       = ZhbGetVar('rrn');
       intRrn       = %int(%trim(strRrn));
       strType      = ZhbGetVar('callType');
       strInst      = ZhbGetVar('instructions');

      /end-free
     p getQueryString  e

      /copy sCopyCGI03
