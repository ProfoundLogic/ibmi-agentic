     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio: *srcstmt) decedit('0.')
     h*** bnddir('WTBNDDIR':'YAJL')
     H BNDDIR('WTBNDDIR':'YAJL':'QC2LE':'CGIDEV2/TEMPLATE2')
      //--------------------------------------------------------------------
      //
      // Description: Response program for determining deleted records
      //              - Activation group should be *CALLER
      //              - This process should not lock any file
      //
      // Called from: Ajax calls
      // Calls to:
      //
      // Created on 09/21/2015 by C:33  WC2302
      //--------------------------------------------------------------------
      //Revisions:
      //
      //--------------------------------------------------------------------
     fwdelp     if   e             disk    rename(pel01:pel0p) usropn
     fwmcmp     if   e             disk    rename(pcm01:pcm0p) usropn

      /copy SprUsrPrms
      /copy SprAtrium
      /copy SprString
      /copy WprEmplr
      /copy CGIDEV2/qrpglesrc,prototypeb
      /copy CGIDEV2/qrpglesrc,usec
      /copy sCopyCGI01

     D psds          esds                  extname(szq1p)

     d intRrn          s              9  0
     D strRcdExists    s              5    inz
     d strRrn          s              9A
     d strType         s              3A
     D intCo#          s              3  0 inz

     D CCSID           S             10i 0 inz(0)
     D charVar         s            500a   varying
     D bigCharVar      s          32767A
     D length          s             10i 0
     D CRLF            C                   X'0d25'
     D errMsg          s            500a   varying
     D flgRes          s              1    inz
     D flgFirst        s              1    inz
     D httpStatus      s              3p 0
     D index           S              5I 0
     D startPos        S              5I 0
     D endPos          S              5I 0
     D len             S              5I 0
      *
     D strJobName      S             20A
     D strJobUser      S             10    inz
     D strUser         S             10    inz
     D strEnvironment  S             10    inz
     D strJobNumber    S              6    inz
     D strCommand      s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     D err             ds                  qualified
     D   bytesProv                   10i 0 inz(0)
     D   bytesAvail                  10i 0 inz(0)
      *
      * Number of variables
     DnbrVars          s             10i 0
      * Saved query string
     Dsavedquerystring...
     D                 s          32767    varying
     D*--------------------------------------------------
     D* Procedure name: AddArea
     D* Purpose:        Add the area information to the JSON
     D* Returns:
     D*--------------------------------------------------
     D AddArea         PR
      *

     D*--------------------------------------------------
     D* Procedure name: AddFields
     D* Purpose:        Add all fields for an Area and their Security
     D* Returns:
     D*--------------------------------------------------
     D AddFields       PR

     D*--------------------------------------------------
     D* Procedure name: checkSec
     D* Purpose:        Check the security level for a field
     D* Returns:
     D*--------------------------------------------------
     D checkSec        PR


      *
      /include yajl_h
      /copy sCopyCGI02
      /Free

       //-----------------------------------------------------------------------
       // Subroutine - Process
       //-----------------------------------------------------------------------
       Begsr Process;

        strRcdExists = 'true';
        Select;
        When strType = 'CLM';
           If not %open(WMCMP);
              Open WMCMP;
           Endif;
           Chain intRRN WMCMP;
        When strType = 'POL';
           If not %open(WDELP);
              Open WDELP;
           Endif;
           Chain intRRN WDELP;
        When strType = 'QTE';
           strCommand = 'OVRDBF FILE(WDELP) TOFILE(' +
            %trim(RtvQuoteLib('*QTE')) + '/WDELP)';
           QCMDEXC(strCommand:%len(strCommand));
           If not %open(WDELP);
              Open WDELP;
           Endif;
           Chain intRRN WDELP;
        Endsl;
        If not %found;
           strRcdExists = 'false';
        Endif;

        yajl_beginObj();
        yajl_addChar('Record_Exists':strRcdExists);
        yajl_endObj();

        If %open(WMCMP);
           Close WMCMP;
        Endif;
        If %open(WDELP);
           Close WDELP;
           strCommand = 'DLTOVR FILE(WMEMP)';
           Callp(e) QCMDEXC(strCommand:%len(strCommand));
        Endif;

       Endsr;
      /End-free

     P*--------------------------------------------------
     P* Procedure name: getQueryString
     P* Purpose:        Get the valkues passed in the query string
     P* Returns:
     P*--------------------------------------------------
     P getQueryString  B
     D getQueryString  PI
     D number6         S              6  0 inz
      /Free

       nbrVars =  zhbgetinput(savedquerystring:qusec);

       strJobName  = ZhbGetVar('jobName');
       strJobNumber  = ZhbGetVar('jobNumber');
       number6 = %int(%trim(strJobNumber));
       strJobNumber = %editc(number6 : 'X');
       strJobUser  = ZhbGetVar('jobUser');
       strUser  = ZhbGetVar('user');
       strRRN   = ZhbGetVar('rrn');
       intRrn   = %int(%trim(strRrn));
       strType  = ZhbGetVar('environment');

      /End-free
     P getQueryString  E

      /copy sCopyCGI03
