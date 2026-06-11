     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE STAJXPRC - AJAX request processor
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Detail box AJAX request processor       *
     H*                                                               *
     H* CALLED BY: AJAX request thru HTTP server                      *
     H*                                                               *
     H* CALLS TO: Request handler process in query string             *
     H*                                                               *
     H* CREATED: 04/16/12  C:191 PROJECT: Transform                   *
     H*                                                               *
     H*  To compile program:                                          *
     H*    MODULE(STAJXPRC SPIATRIUM)                                 *
     H*    BNDSRVPGM(QTCP/QTMHCGI)                                    *
     H*    TGTRLS(*CURRENT)                                           *
     H*    Compile option - DEFINE(ALLOW_DEBUG) should be removed     *
     H*                     when creating a deployment version        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/26/14  C:65  Project: Support                              *
     H*                 Amend the 3/20/14 fix to ensure if a job # is *
     H*                 passed with less than 6 digits and no leading *
     H*                 blanks then do not include next parm's info.  *
     H*                                                               *
     H* 03/20/14  C:65  Project: Support                              *
     H*                 Convert job # passed in from query string to  *
     H*                 numeric value and then reconvert to alpha     *
     H*                 but padded with 0's to avoid using a job #    *
     H*                 that is not 0 padded.                         *
     H*                                                               *
     H* 03/12/14  C:62  Support: WC114 84996                          *
     H*                 If unable to retrieve job info then try again *
     H*                                                               *
     H*****************************************************************
      *
     D/copy sPrAtrium
      * API Declarations
      * Standard Out API
     D QtmhWrStout     PR                  ExtProc('QtmhWrStout')
     D   DataVar                  32767A   Options(*varsize) Const
     D   DataVarLen                  10I 0 Const
     D   ErrorCode                 8000A   Options(*varsize)

      * Get Environment Variable API
     D QtmhGetEnv      PR                  ExtProc('QtmhGetEnv')
     D   EnvValue                 32767
     D   EnvBufLen                   10I 0
     D   EnvValueLen                 10I 0
     D   EnvName                     30
     D   EnvNameLen                  10I 0
     D   ErrorCode                 8000A   Options(*varsize)

      * Error Data Structure
     D ErrCode         DS                  Qualified
     D   BytesProv                   10I 0 Inz(0)
     D   BytesAvail                  10I 0

      * Variables used for Get Environment Variable (QtmhGetEnv)
     D EnvVal          S          32767A
     D EnvBufLen       S             10I 0 Inz(%size(EnvVal))
     D EnvValLen       S             10I 0
     D EnvName         S             30A
     D EnvNameLen      S             10I 0

     dSetInitLibl      pr                  extPgm('M0002C')
     d inApp                          3    const
     d inMetL                        10    const
     d inOVR1                        70    const
     d inOVR2                        70    const
     d inOVR3                        70    const
     d inOVR4                        70    const
     d inOVR5                        70    const
     d inRTCD                         1    const
      *
     dRunCommand       pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const
      *
     dCheckObj         pr                  extPgm('SRCHKOBJ')
     d inName                        10    const
     d inType                         7    const
     d inRtcd                         1
      *
     dAllowDebug       pr                  extPgm('STDBGMSG')
     d outReply                       1
      *
     d CallRequestRrn  pr                  ExtPgm(strProgram)
     d   inRRN                        9  0
     d   outJSON                  32767
     d   outRtnCode                   1A

     d CallRequestGen  pr                  ExtPgm(strProgram)
     d   inGenParm                  512
     d   outJSON                  32767
     d   outRtnCode                   1A

     d CallRequestBth  pr                  ExtPgm(strProgram)
     d   inRRN                        9  0
     d   inGenParm                  512
     d   outJSON                  32767
     d   outRtnCode                   1A

     D psds          esds                  extname(szq1p)

     D CRLF            C                   x'0D25'
     D QueryString     S           1024A   varying
     D JSON            S          32767A   varying

     D intRRN          S              9  0 inz
     D strRRN          S              9    inz
     D strProgram      S             10    inz
     D strJobName      S             10    inz
     D strUserName     S             10    inz
     D strJobNumber    S              6    inz
     D*** begin add ***  03/20/14  *********************************************
     D number6         S              6  0 inz
     D*** end add *****  03/20/14  *********************************************
     D strPreSetLibl   S              3    inz
     D strGenericParm  S            512    inz
     D inJSON          S          32767A
     D strUserLibl     S           2750A   varying
     D intSPos         S              5  0
     D intEPos         S              5  0
     D intLen          S              5  0
     D isError         S               n   inz
     D inCmd           s           2750    inz
     D RtnCode         s              1
     D strDebug        s             10    inz
     D flgRtnCd        s              1    inz
     D dtDate          s               d   inz
     D noRrnFound      s               n   inz
     D errormsg        s            256    inz
     D*** Begin Add *** 03/12/14 *************************************************
     D jumpCount       s              3  0 inz
     D*** End   Add *** 03/12/14 *************************************************
      /if defined(ALLOW_DEBUG)
     D DebugInfo       S           5000A   varying
      /endif

      /Free
           Clear JSON;
        /if defined(ALLOW_DEBUG)
           ExSr ChkDebug;
        /endif
        //*** Begin Add *** 03/12/14 *************************************************
          DoW jumpCount<5;
        //*** End   Add *** 03/12/14 *************************************************
          ExSr GetQueryString;
          ExSr RtvValues;
          If not isError;
            ExSr SetLibl;
        //*** Begin Add *** 03/12/14 *************************************************
            Leave;
          Else;
            inCmd = 'DLYJOB DLY(1)';
            RunCommand(inCmd:%len(inCmd));
            jumpCount=jumpCount+1;
        //*** End   Add *** 03/12/14 *************************************************
          EndIf;
        //*** Begin Add *** 03/12/14 *************************************************
          EndDo;
        //*** End   Add *** 03/12/14 *************************************************
          ExSr BldJSON;

          // Write JSON to Standard Out
          QtmhWrStout(JSON: %Len(JSON): ErrCode);

          *InLr = *On;

         //***********************************************************************
         // GetQueryString - Subroutine to retrieve qury string values
         //***********************************************************************
         BegSr GetQueryString;
          EnvName = 'QUERY_STRING';
          EnvNameLen = %Len(%Trim(EnvName));
          QtmhGetEnv(EnvVal : EnvBufLen : EnvValLen :
                     EnvName : EnvNameLen : ErrCode);
          QueryString = %SubSt(EnvVal : 1 : EnvValLen);
         EndSr;

         //***********************************************************************
         // RtvValues - Retrieve Values from query string
         // Query String will be in the following format (for RRN based call)
         // 'rrn=xxxxxxxxx&pgm=aaaaaaaaaa&jobnm=mmmmmmmmmm&jobu=uuuuuuuuuu&jobn=nnnnnn
         //  1234567890123456789012345678901234567890123456789012345678901234567890123
         //           1         2         3         4         5         6         7
         //***********************************************************************
         BegSr RtvValues;

          isError = *On;
          strPreSetLibl = ' ';
          //Retrieve RRN
          If %Len(QueryString) >= 13 and
             %SubSt(QueryString : 1 : 4) = 'rrn=';
             strRRN = %SubSt(QueryString :5 :9);
          Else;
             strRRN = '000000000';
             LeaveSr;
          EndIf;
          monitor;
            intRRN = %Int(strRRN);
          On-error;
            intRRN = 0;
            LeaveSr;
          EndMon;

          //Retrieve Program/procedure name
          If %Len(QueryString) >= 27 and
             %SubSt(QueryString :15 :4) = 'pgm=';
             strProgram = %SubSt(QueryString :19);
          Else;
             strProgram = ' ';
             LeaveSr;
          EndIf;

          //Retrieve JobName
          If %Len(QueryString) >= 30;
            // Set start position
            intSPos = %Scan('&jobnm=':QueryString)+7;
           // Check for pre-set library list request
            intEPos = %Scan('&jobu=':QueryString);
            If intEPos = 0;
              strPreSetLibl = %SubSt(QueryString :intSPos);
              // request is probably for pre-set libl - Generic param may follow now
              isError = *Off;
            Endif;

            If strPreSetLibl = *Blanks;
              If intSPos > 0;
                intLen = intEPos - intSPos;
                strJobName = %SubSt(QueryString :intSPos :intLen);
              Else;
                strJobName = ' ';
                LeaveSr;
              EndIf;
            EndIf;
          EndIf;

          // User name
          If %Len(QueryString) >= 40 And strPreSetLibl = *Blanks;
            // End position for Jobname is start for job user
            intSPos = intEPos+6;
            intEPos = %Scan('&jobn=':QueryString);
            If intEPos = 0;
              LeaveSr;
            Endif;

            If intSPos > 0;
              intLen = intEPos - intSPos;
              strUserName = %SubSt(QueryString :intSPos :intLen);
            Else;
              strUserName = ' ';
              LeaveSr;
            EndIf;
          EndIf;

          // Job Number
          If %Len(QueryString) >= 50 And strPreSetLibl = *blanks;
            // End position for User Name is start for job number
            intSPos = intEPos+6;

            If intSPos > 0;
              strJobNumber = %SubSt(QueryString :intSPos);
              //*** begin add ***  03/26/14  *********************************************
              If %scan('&' : strJobNumber) > 1;
                  strJobNumber = %Subst(strJobNumber : 1 :
                                  %scan('&' : strJobNumber) - 1);
              Endif;
              //*** begin add ***  03/26/14  *********************************************
              //*** begin add ***  03/20/14  *********************************************
              number6 = %int(%trim(strJobNumber));
              strJobNumber = %editc(number6 : 'X');
              //*** end add *****  03/20/14  *********************************************
            Else;
              strJobNumber = ' ';
              LeaveSr;
            EndIf;
          EndIf;

          strGenericParm = ' ';
          If %Len(QueryString) >= 30;
            // Look for generic parameter
            // Set start position
            intSPos = %Scan('&GenParm=':QueryString);
            If intSPos > 0;
              intSPos = intSPos + 9;
              // Set end position
              intEPos = %Scan('*END':QueryString:intSPos);
              intLen = intEPos - intSPos;
              If intSPos > 10;
                strGenericParm = %SubSt(QueryString :intSPos :intLen);
              EndIf;
            EndIf;
          EndIf;

          isError = *Off;
         EndSr;
         //***********************************************************************
         // SetLibl - Set library list for AJAX call
         //***********************************************************************
         BegSr SetLibl;
          Monitor;
           If strPreSetLibl <> *blanks;
            // Set preset libl
              SetInitLibl(strPreSetLibl:' ':' ':' ':' ':' ':' ':' ');
           Else;
            // Get job libl and set
              strUserLibl = RtvUserLibl(strJobName:strUserName:strJobNumber);
              If strUserLibl = 'X' or strUserLibl = ' ';
              //Error default to LIV libl
                SetInitLibl('LIV':' ':' ':' ':' ':' ':' ':' ');
                LeaveSr;
              EndIf;
              inCmd = 'CHGLIBL LIBL(' + %Trim(strUserLibl) + ')';
              RunCommand(inCmd:%len(inCmd));
           EndIf;
          On-error;
           // send error response
            isError = *On;
            errormsg ='"RtnCode": "Error setting library list for querystring'
             + '"';
          EndMon;
         EndSr;
         //***********************************************************************
         // BldJSON - Build JSON
         //***********************************************************************
         BegSr BldJSON;
          // Build JSON Output
          JSON = 'Content-Type: text/plain' + CRLF + CRLF;
          JSON += '{ ';
          If isError = *On;
           // send error response
            If errormsg <> *Blanks;
              JSON += errormsg;
            Else;
             JSON += '"RtnCode": "Invalid Query String Value'
              + %Trim(QueryString) + '"';
            Endif;
          Else;
            Monitor;
              Select;
              When intRRN <> 0 And strGenericParm <> *Blanks;
                CallRequestBth(intRRN:strGenericParm:inJSON:RtnCode);
              When intRRN <> 0;
                CallRequestRrn(intRRN:inJSON:RtnCode);
              When strGenericParm <> *Blanks;
                CallRequestGen(strGenericParm:inJSON:RtnCode);
              Endsl;
              JSON += '"RtnCode": "' + %TrimR(RtnCode);
      /if defined(ALLOW_DEBUG)
              // debug data
              DebugInfo = q1job+';' + %editc(q1jobn:'X') + ';' + q1cusr + ';';
              DebugInfo += %TrimR(strPreSetLibl) + ';' + %TrimR(strUserLibl);
              JSON += '", "DebugInfo": "' + %TrimR(DebugInfo);
      /Endif
              If inJSON <> *blanks;
                JSON += '", ' + %TrimR(inJSON);
              EndIf;
            On-Error;
              JSON += '"RtnCode": "Call failed: PGM='
               + %Trim(strProgram) + ', RRN=' + %Trim(strRRN) + '" ';
            EndMon;
          EndIf;

          JSON += ' }';
         EndSr;
      /if defined(ALLOW_DEBUG)
         //***********************************************************************
         // ChkDebug - Hold for debug if needed
         //***********************************************************************
         BegSr ChkDebug;
          Monitor;
           strDebug = %subst(q1user:1:4) + %Char(%Dec(%Date:*YMD));
           CheckObj(strDebug:'*DTAARA':flgRtnCd);
           If flgRtnCd = '0';
             AllowDebug(flgRtnCd);
           EndIf;
          On-Error;
          EndMon;
         EndSr;
      /Endif
      /End-Free
