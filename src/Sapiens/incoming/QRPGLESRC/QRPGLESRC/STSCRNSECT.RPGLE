     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio: *srcstmt) decedit('0.')
      //--------------------------------------------------------------------
      //
      // Description: Response program to serve List of secured areas on display
      //              - Activation group should be *CALLER
      //              - This process should not lock any file
      //              - This process should only read files, no updates/writes
      //                should be made
      //
      // Called from: Ajax calls
      // Calls to:
      //
      // Created on 05/19/2014 by C:302
      //--------------------------------------------------------------------
      //Revisions:
      //
      // 09/18/17  C:33  Support: 49587
      //                 Expand json return
      //--------------------------------------------------------------------
     fsmasl     if   e           k disk    rename(pas01:pas00) usropn
     fsdasl     if   e           k disk    rename(pas01:pds00) USROPN
     f                                     prefix(d)
     fsaasl     if   e           k disk    rename(pas01:pas0l)  usropn
      /copy sprUsrPrms
      /copy sPrAtrium
      /copy sPrString
      /copy CGIDEV2/qrpglesrc,prototypeb
      /copy CGIDEV2/qrpglesrc,usec

     D QtmhWrStout     PR                  extproc('QtmhWrStout')
     D   DtaVar                  100000a   options(*varsize) const
     D   DtaVarLen                   10I 0 const
     D   ErrorCode                 8000A   options(*varsize)

     DCheckSecurity    pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTYP                         1    const
     D inCLVL                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
     D inCLV2                         3    const
     D inCLV3                         3    const
     D inCLV4                         3    const
     D inCLV5                         3    const
     D inCLV6                         3    const

     D psds          esds                  extname(szq1p)

     D strDspf         s             10    inz
     D strFormat       s             10    inz
     D strFields       s          32767a   inz varying
     D strAccess       s              4    inz
     D intCo#          s              3  0 inz
     D

     D CCSID           S             10i 0 inz(0)
     D charVar         s            500a   varying
     D***  Begin Chg  ***  09/18/17  **************************************
     D*bigCharVar      s          32767A
     D bigCharVar      s          65535A
     D***  End   Chg  ***  09/18/17  **************************************
     D length          s             10i 0
     D CRLF            C                   X'0d25'
     D errMsg          s            500a   varying
     D flgRes          s              1    inz
     D flgFirst        s              1    inz
     D httpStatus      s              3p 0
     D strSecType      s              1    inz
     D strSecLevel     s              3    inz
     D strUserName     s             50    inz
     D strOutQ         s             10    inz
     D strStaffCd      s              3    inz
     D intSecLevel     s              3  0 inz
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
     DstrDisplayFiles  S           1000A
     DstrFormats       S           1000A
     D inCmd           s           2750    inz

     D err             ds                  qualified
     D   bytesProv                   10i 0 inz(0)
     D   bytesAvail                  10i 0 inz(0)
      *
     D displays        DS            10    QUALIFIED
     D                                     DIM(50)
     D formats         DS            10    QUALIFIED
     D                                     DIM(50)
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

     dSetInitLibl      pr                  extPgm('M0002C')
     d inApp                          3    const
     d inMetL                        10    const
     d inOVR1                        70    const
     d inOVR2                        70    const
     d inOVR3                        70    const
     d inOVR4                        70    const
     d inOVR5                        70    const
     d inRTCD                         1    const

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

     D*--------------------------------------------------
     D* Procedure name: JSON_Start
     D* Purpose:        Start JSON formating
     D* Returns:
     D*--------------------------------------------------
     D JSON_Start      PR

     D*--------------------------------------------------
     D* Procedure name: JSON_Send
     D* Purpose:        Resolve and send the JSON string
     D* Returns:
     D*--------------------------------------------------
     D JSON_Send       PR

     D*--------------------------------------------------
     D* Procedure name: JSON_Finish
     D* Purpose:        Finish the JSON string
     D* Returns:
     D*--------------------------------------------------
     D JSON_Finish     PR

     D*--------------------------------------------------
     D* Procedure name: getQueryString
     D* Purpose:        Get the valkues passed in the query string
     D* Returns:
     D*--------------------------------------------------
     D getQueryString  PR

     D*--------------------------------------------------
     D* Procedure name: isValidParms
     D* Purpose:        Validate minimum values passed
     D* Returns:
     D*--------------------------------------------------
     D isValidParms    PR              N

     D*--------------------------------------------------
     D* Procedure name: SetLibraryList
     D* Purpose:        Set the jobs labrary list
     D* Returns:
     D*--------------------------------------------------
     D SetLibraryList  PR


     dRunCommand       pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const
      *

      *
      /include yajl_h
      /Free

       getQueryString();
       JSON_Start();

       If not isValidParms();
          yajl_endObj();
          JSON_Send();
          Return;
       Endif;

       If flgFirst <> 'Y';
          flgFirst = 'Y';
         // inCmd = 'DLYJOB DLY(5)';
          //RunCommand(inCmd:%len(inCmd));
       Endif;

       SetLibraryList();
       intCo# = GetUserInfo(Q1user :strUserName :strOutQ :strStaffCd
        :strSecType :intSecLevel);
       strSecLevel = %Editc(intSeclevel:'X');

       If not %open(smasl);
          Open(e) smasl;
       Endif;
       If not %open(sdasl);
          Open(e) sdasl;
       Endif;
       If not %open(saasl);
          Open(e) saasl;
       Endif;
       For index = 1 TO 50;
           If displays(index) = *blanks;
              Leave;
           Endif;

           //Process request
           Setll (intCo#:displays(index):formats(index)) pas00;
           Reade (intCo#:displays(index):formats(index)) pas00;
           yajl_beginArray(%trim(displays(index)) + '.'
                        + %trim(formats(index)));
           Dow not %EOF(smasl);

              Chain (intCo#:ASarea:strEnvironment) pas0l;
              If Not %Found;
                 Select;
                    When strEnvironment = 'L';
                       yajl_endArray();
                       yajl_addChar('RtnCode' : 'X');
                       yajl_addChar('errorMessage' :
                                'No security information exists for ' +
                                ASAREA + ' in Live');
                       JSON_Finish();
                       JSON_Send();
                       return;
                    When strEnvironment = 'Q';
                       strEnvironment = 'L';
                       Chain (intCo#:ASarea:strEnvironment) pas0l;
                       If not(%found);
                          yajl_endArray();
                          yajl_addChar('RtnCode' : 'X');
                          yajl_addChar('errorMessage' :
                           'No security information exists for ' +
                            ASAREA + ' in Quote or Live');
                          JSON_Finish();
                          JSON_Send();
                          return;
                       Endif;
                 Endsl;
              Endif;

              AddArea();
              Reade (intCo#:displays(index):formats(index)) pas00;

           Enddo;

           yajl_endArray();

       Endfor;

       yajl_endObj();

       JSON_Send();

       *INLR = *ON;

       Return;
      /End-free
     P*--------------------------------------------------
     P* Procedure name: AddArea
     P* Purpose:        Add the area information to the JSON
     P* Returns:
     P*--------------------------------------------------
     P AddArea         B
     D AddArea         PI
      /Free
          yajl_beginObj();
          yajl_addChar('areaid' : ASarea );
          AddFields();
          yajl_endObj();
      /End-free
     P AddArea         E

     P*--------------------------------------------------
     P* Procedure name: AddFields
     P* Purpose:        Add all fields for an Area and their Security
     P* Returns:
     P*--------------------------------------------------
     P AddFields       B
     D AddFields       PI
      /Free
          yajl_beginArray('fields');
          Setll (ASco#:ASarea) pds00;
          Reade (ASco#:ASarea) pds00;

          Dow not %EOF(sdasl);
             yajl_beginObj();
             yajl_addChar('field_id' : %trim(DAsfld) );
             checkSec();
             yajl_addChar('field_access': strAccess );
             yajl_endObj();
             Reade (ASco#:ASarea) pds00;
          Enddo;
          yajl_endArray();
      /End-free
     P AddFields       E

     P*--------------------------------------------------
     P* Procedure name: checkSec
     P* Purpose:        Check the security level for a field
     P* Returns:
     P*--------------------------------------------------
     P checkSec        B
     D checkSec        PI
      /Free

       //Check VIEW security level first
       CheckSecurity(strSecType:strSeclevel:ASvst1:%Editc(ASvsl1:'X'):'1'
        :flgRes:ASvst2:ASvst3:ASvst4:ASvst5:ASvst6:%Editc(ASvsl2:'X')
        :%Editc(ASvsl3:'X'):%Editc(ASvsl4:'X'):%Editc(ASvsl5:'X')
        :%Editc(ASvsl6:'X'));
       If flgRes = '0';
          strAccess = 'VIEW';
          //Then EDIT security level if allowed in VIEW
          CheckSecurity(strSecType:strSeclevel:ASest1:%Editc(ASesl1:'X'):'1'
           :flgRes:ASest2:ASest3:ASest4:ASest5:ASest6:%Editc(ASesl2:'X')
           :%Editc(ASesl3:'X'):%Editc(ASesl4:'X'):%Editc(ASesl5:'X')
           :%Editc(ASesl6:'X'));
          If flgRes = '0';
            strAccess = 'EDIT';
          Endif;
       Else;
          strAccess = 'HIDE';
       Endif;

      /End-free
     P checkSec        E

     P*--------------------------------------------------
     P* Procedure name: JSON_Start
     P* Purpose:        Start JSON formating
     P* Returns:
     P*--------------------------------------------------
     P JSON_Start      B
     D JSON_Start      PI
      /Free
         yajl_genOpen(*Off);  // use *ON for easier to read JSON
         yajl_beginObj();
      /End-free
     P JSON_Start      E

     P*--------------------------------------------------
     P* Procedure name: JSON_Send
     P* Purpose:        Resolve and send the JSON string
     P* Returns:
     P*--------------------------------------------------
     P JSON_Send       B
     D JSON_Send       PI
     D JSON            S          32767A   varying
      /Free
          charVar = 'Content-Type: text/plain' + CRLF + CRLF;
          length = %len(%trim(charvar));
          QtmhWrStout( charVar : length: err );

          yajl_copyBuf( CCSID
                      : %addr(bigCharVar)
                      : %size(bigCharVar)
                      : length );
          yajl_genClose();
          // outJSON = %SUBST(bigCharVar:2);
          QtmhWrStout( bigCharVar : length: err );
      /End-free
     P JSON_Send       E

     P*--------------------------------------------------
     P* Procedure name: JSON_Finish
     P* Purpose:        Finish the JSON string
     P* Returns:
     P*--------------------------------------------------
     P JSON_Finish     B
     D JSON_Finish     PI
      /Free
          yajl_endArray();
      /End-free
     P JSON_Finish     E

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
       strDisplayFiles  = ZhbGetVar('displayFiles');
       strFormats  = ZhbGetVar('formats');
       strEnvironment  = ZhbGetVar('environment');

       startPos = 1;
       index = 1;
       Dou endPos = 0;
          endPos = %scan(',': strDisplayFiles:startPos);
          If endPos = 0;
             If %SubSt(strDisplayFiles:startPos:1) > *Blanks;
                displays(index) =  %trim(%SubSt(strDisplayFiles:startPos));
             Endif;
             Leave;
          Endif;
          len = endPos - startPos;
          displays(index) = %SubSt(strDisplayFiles:startPos:len);
          index += 1;
          startPos = endPos + 1;
       Enddo;

       startPos = 1;
       index = 1;
       Dou endPos = 0;
          endPos = %scan(',': strFormats:startPos);
          If endPos = 0;
             If %SubSt(strFormats:startPos:1) > *Blanks;
                formats(index) =  %trim(%SubSt(strFormats:startPos));
             Endif;
             Leave;
          Endif;
          len = endPos - startPos;
          formats(index) = %SubSt(strFormats:startPos:len);
          index += 1;
          startPos = endPos + 1;
       Enddo;

      /End-free
     P getQueryString  E

     P*--------------------------------------------------
     P* Procedure name: isValidParms
     P* Purpose:        Validate minimum values passed
     P* Returns:
     P*--------------------------------------------------
     P isValidParms    B
     D isValidParms    PI              N


     D* Local fields
     D retField        S               N

      /Free
       //  If the environment flag is not passed, or there is garbage in it,
       //  then default it to Live 'L'
       If strEnvironment = *Blanks;
          yajl_addChar('RtnCode' : 'X');
          yajl_addChar('errorMessage' :'No Enviroment flag passed, may need' +
                    ' to update the IFS deployment');
          return *off;
       Endif;
       If displays(1) = *Blanks;
          yajl_addChar('RtnCode' : 'X');
          yajl_addChar('errorMessage' :'No displays passed');
          return *off;
       Endif;
       If formats(1) = *Blanks;
          yajl_addChar('RtnCode' : 'X');
          yajl_addChar('errorMessage' :'No formats passed');
          return *off;
       Endif;

       Return *on;

      /End-free
     P isValidParms    E


     P*--------------------------------------------------
     P* Procedure name: SetLibraryList
     P* Purpose:        Set the jobs labrary list
     P* Returns:
     P*--------------------------------------------------
     P SetLibraryList  B
     D SetLibraryList  PI
     D inCmd           s           2750    inz
     D strUserLibl     S           2750A   varying

      /Free
         // SetLibl - Set library list for AJAX call
         // Get job libl and set
         strUserLibl = RtvUserLibl(strJobName:strJobUser:strJobNumber);
         If strUserLibl = 'X' or strUserLibl = ' ';
         //Error default to LIV libl
           SetInitLibl('LIV':' ':' ':' ':' ':' ':' ':' ');
         Else;
            inCmd = 'CHGLIBL LIBL(' + %Trim(strUserLibl) + ')';
            RunCommand(inCmd:%len(inCmd));
         EndIf;
      /End-free
     P SetLibraryList  E

