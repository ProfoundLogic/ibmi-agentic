     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H nomain
    H*********************************************************************
     H*  Created:
    H*           03/01/12  C:191 Project: Transform
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
     H*  08/04/21  C:33   Project: WC2820                                 *
    H*                  -Add BuildUNCUrl                                 *
    H*                  -Update ElementClose for # to return             *
    H*                                                                   *
     H*  07/21/19  C:33   Project: SY406                                  *
    H*                   Add ElementOpen, ElementClose                   *
    H*                                                                   *
     H*  08/06/18  C:33   Project: SY390                                  *
     H*                   Add BuildGiantJSON, StartGiantElement and       *
    H*                    EndGiantElement                                *
    H*                                                                   *
     H*  09/18/17  C:33   Support: 49587                                  *
     H*                   User open SDCMLA.                               *
    H*                                                                   *
     H*  04/07/16  C:65   38610                                           *
     H*                   Update BuildPDFUrl so it can handle UNCN docs.  *
    H*                                                                   *
     H*  03/14/16  C:33   SY285                                           *
     H*                   Add BuildBigJSON.                               *
    H*                                                                   *
     H*  11/25/15  C:33   38610                                           *
     H*                   Add BuildPDFUrl.                                *
    H*                                                                   *
     H*  10/07/13  C:33   Seq#: WA114/83731                               *
     H*                   Excecpt characters in JSON string.              *
    H*                                                                   *
     H*  08/29/13  C:180  Support  WA133 83307                            *
     H*                   Fix URL not found error during quote search.    *
     H*
    H*  08/23/13  C:279  Redmine: 3378
    H*                   Add Procedure CreateBigQteURL.                  *
    H*                                                                   *
    H*  04/04/13  C:191  Redmine: 1791
    H*                   Add "INQ -" to tab name when inquiry mode       *
    H*                                                                   *
    H*  12/14/12  C:33   Project: Transform
    H*                   Add AtriumJob                                   *
    H*                                                                   *
    H*  10/10/12  C:33   Project: Transform
    H*                   Add LaunchNewTab                                *
    H*                                                                   *
    H*           07/23/12  C:191 Project: Transform
    H*                           Add CreateBigURL function               *
    H*********************************************************************
     Fspiatrium cf   e             workstn Handler('PROFOUNDUI(HANDLER)')
     F                                     usropn
     fsdcmla    uf   e           k disk    rename(pcm01:scm0a)
     f                                     prefix(s2:2)
     F                                     usropn
     D*
     D/copy sPrAtrium
     D***  Begin Add  ***  08/04/21  *******************************
     D/copy sPrPrprty
     D***  End   Add  ***  08/04/21  *******************************
     D/copy sPrString
     dstrElementCloseChar...
     d                 s            500    varying inz
     d xxFold          s            195
     d xxDocid         s             60    inz(' ')
     d y               s              3  0
     d fold            s              1    DIM(255)

      * Get directory path
     dRtvDirPath       pr                  extpgm('DTGETDIR')
     d inFlr                         80    const
     d outDir                        80
     d inUncn                       255    const
     d inFSF                          1    const

     D psds          esds                  extname(szq1p)

     D*=====================================================================
     D* JobInfoAPI - This is internal procedure used in this module to invoke
     D*              job information API to retrieve library list of a job
     D*=====================================================================
     DJobInfoAPI       PR                  ExtPgm('QUSRJOBI')
     D RcvVar                     32767    Options(*VarSize)
     D LengthRcv                     10I 0 Const
     D FormatName                     8A   Const
     D QualJobName                   26    Const
     D InternalJobId                 16    Const
     D Error_ds                      15
     D*=====================================================================
     D* GetObjLib  - Get the object library list
     D*=====================================================================
     DGetObjLib        PR                  ExtPgm('RTVOBJLIB')
     D Object                        10    Const
     D Type                           7    Const
     D Library                       10

     P*=====================================================================
     P* AtriumJob - Determines if the job is an Atrium job                  name
     P*   Parm 1 = Job Name (10A)      {optional}
     P*   Parm 2 = Job user name (10A) {optional}
     P*   Parm 3 = Job number (6A)     {optional}
     P*   Returns = *on if Atrium, otherwise *off
     P*=====================================================================
     PAtriumJob        b                   Export
     DAtriumJob        pi             1n
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
      *
     DdsJOBI0400       ds         32767
     D bytesRtn                      10i 0
     D bytesAvl                      10i 0
     D jobName                       10a
     D userId                        10a
     D jobNumber                      6a
     D intJobId                      16a
     D jobStatus                     10a
     D jobType                        1a
     D jobSubType                     1a
      *
     DdsError          ds            15
     D bytesProvided                 10i 0 inz(%size(dsError))
     D bytesAvail                    10i 0
     D errorId                        7
      *
     DisAtriumJob      s              1n
     DstrQualJob       s             26a   inz('*')
     DstrUserLibl      s           2750a
     DstrJobName       s             10    inz('*')
     DstrJobUsrName    s             10    inz
     DstrJobNumber     s              6    inz
      *
      /free
       Monitor;
        strUserLibl = ' ';
        // Build qualified job name from input values
        If %parms >= 1 and inJobName <> *blanks;
           strJobName = inJobName;
           If %parms >= 2 and inJobUsrName <> *blanks;
              strJobUsrName = inJobUsrName;
           Endif;
           If %parms >= 3 and inJobNumber <> *blanks;
              strJobNumber = inJobNumber;
           Endif;
        Endif;

        strQualJob = strJobName + strJobUsrName + strJobNumber;

        // Call job information API to get job library list
        JobInfoAPI(dsJOBI0400:%Size(dsJOBI0400):'JOBI0400'
         :strQualJob:*blanks:dsError);

        Select;
        When jobType = 'B' and jobSubType = 'D';
           isAtriumJob = *on;
        Other;
           isAtriumJob = *off;
        Endsl;
       On-Error;
          isAtriumJob = *off;
       EndMon;

       Return isAtriumJob;
      /end-free
     PAtriumJob        e

     P*=====================================================================
     P* CreateURL - Create a URL that can be launched as Atrium tab
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 12  = Parameters for program call (50 A) each - optional
     P*          Returns = Character string with Tab name in first 50 and URL after that
     P*=====================================================================
     PCreateURL        B                   Export
     DCreateURL        PI           720
     D inProgram                     10    const
     D inTabName                     50    const
     D inParm01                      50    options(*nopass) const
     D inParm02                      50    options(*nopass) const
     D inParm03                      50    options(*nopass) const
     D inParm04                      50    options(*nopass) const
     D inParm05                      50    options(*nopass) const
     D inParm06                      50    options(*nopass) const
     D inParm07                      50    options(*nopass) const
     D inParm08                      50    options(*nopass) const
     D inParm09                      50    options(*nopass) const
     D inParm10                      50    options(*nopass) const
     D*
     D conPrefix       c                   const('/profoundui/auth/start?pgm=')
     D conLaunchPgm    c                   const('STLAUNCH')
     D conLen1         c                   const(10)
     D conLength       c                   const(550)
     D conBlank        c                   const('%20')
     D conEnd          c                   const('*End')
     D*
     D strURL          s            720    inz
     D strProgram      s             32    inz
     D strLibrary      s             10    inz(' ')
     D strParm         s            550    inz
     D strJobInf       s             26    inz
     D strTabName      s             50    inz
     D strMode         s             50    inz
     D intLen          s              5  0 inz
     D*
      /Free
         GetObjLib('STLAUNCH':'*PGM':strLibrary);
         // strLibrary = q1plib;
         strJobInf = q1job + q1cusr + %EditC(q1jobn:'X');

         strParm = '(';
         If %Parms > 2;
            strParm = %trim(strParm) + '''' + %Trim(inParm01) + '''';
         EndIf;
         If %Parms > 3;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm02) + '''';
            strMode = %Trim(inParm02);
         EndIf;
         If %Parms > 4;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm03) + '''';
         EndIf;
         If %Parms > 5;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm04) + '''';
         EndIf;
         If %Parms > 6;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm05) + '''';
         EndIf;
         If %Parms > 7;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm06) + '''';
         EndIf;
         If %Parms > 8;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm07) + '''';
         EndIf;
         If %Parms > 9;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm08) + '''';
         EndIf;
         If %Parms > 10;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm09) + '''';
         EndIf;
         If %Parms > 11;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm10) + '''';
         EndIf;
         strParm = %trim(strParm) + ')' + conEnd;

         For intLen =  %len(%trim(inProgram)) to conLen1+1;
           strProgram = %trim(inProgram) + conBlank;
         EndFor;

         strTabName = inTabName;
         If strMode = 'D';
           strTabName = 'INQ - ' +  %trim(strTabName);
         Endif;
         //strUrl = inTabName + conPrefix
         strUrl = strTabName + conPrefix
          + %trim(strlibrary) + '/' + conLaunchPgm
          + '&p1=' + %trim(strProgram) + '&l1=' + %char(conLen1)
          + '&p2=' + %trim(strJobInf) +  '&l2=' + %char(%len(strJobInf))
          + '&p3=' + %trim(strParm) + '&l3=' + %char(conLength);

         If %Trim(inTabName) = 'DEBUG';
           strUrl = %Trim(strUrl) + '&p4=D&l4=1';
         EndIf;

         return strUrl;
      /End-Free
     PCreateURL        E
      *
     P*=====================================================================
     P* CreateQteURL - Create a URL that can be launched as Atrium tab for "other" environment
     P*          "other" meaning - if you are in Live environment, URL will take you to Quote Env
     P*                          - if you are in Quote environment, URL will take you to Live Env
     P*          NOTE: To remain in current environment (whether quote or live) use CreateURL
     P*
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 12  = Parameters for program call (50 A) each - optional
     P*          Returns = Character string with Tab name in first 50 and URL after that
     P*=====================================================================
     PCreateQteURL     B                   Export
     DCreateQteURL     PI           720
     D inProgram                     10    const
     D inTabName                     50    const
     D inParm01                      50    options(*nopass) const
     D inParm02                      50    options(*nopass) const
     D inParm03                      50    options(*nopass) const
     D inParm04                      50    options(*nopass) const
     D inParm05                      50    options(*nopass) const
     D inParm06                      50    options(*nopass) const
     D inParm07                      50    options(*nopass) const
     D inParm08                      50    options(*nopass) const
     D inParm09                      50    options(*nopass) const
     D inParm10                      50    options(*nopass) const
     D*
     D conPrefix       c                   const('/profoundui/auth/start?pgm=')
     D conLaunchPgm    c                   const('STLAUNCH')
     D conLen1         c                   const(10)
     D conLength       c                   const(550)
     D conBlank        c                   const('%20')
     D conEnd          c                   const('*End')
     D*
     D strURL          s            720    inz
     D strProgram      s             32    inz
     D strLibrary      s             10    inz
     D strParm         s            550    inz
     D strJobInf       s             26    inz
     D strTabName      s             50    inz
     D strMode         s             50    inz
     D intLen          s              5  0 inz
     D*
      /Free
         GetObjLib('STLAUNCH':'*PGM':strLibrary);
         // strLibrary = q1plib;
         strJobInf = q1job + q1cusr + %EditC(q1jobn:'X');

         strParm = '(';
         If %Parms > 2;
            strParm = %trim(strParm) + '''' + %Trim(inParm01) + '''';
         EndIf;
         If %Parms > 3;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm02) + '''';
            strMode = %Trim(inParm02);
         EndIf;
         If %Parms > 4;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm03) + '''';
         EndIf;
         If %Parms > 5;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm04) + '''';
         EndIf;
         If %Parms > 6;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm05) + '''';
         EndIf;
         If %Parms > 7;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm06) + '''';
         EndIf;
         If %Parms > 8;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm07) + '''';
         EndIf;
         If %Parms > 9;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm08) + '''';
         EndIf;
         If %Parms > 10;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm09) + '''';
         EndIf;
         If %Parms > 11;
            strParm = %trim(strParm) + ' ''' + %Trim(inParm10) + '''';
         EndIf;
         strParm = %trim(strParm) + ')' + conEnd;

         For intLen =  %len(%trim(inProgram)) to conLen1+1;
           strProgram = %trim(inProgram) + conBlank;
         EndFor;

         strTabName = inTabName;
         If strMode = 'D';
           strTabName = 'INQ - ' +  %trim(strTabName);
         Else;
           strTabName = %trim(strTabName);
         Endif;
         //strUrl = inTabName + conPrefix
         strUrl = strTabName + conPrefix
          + %trim(strlibrary) + '/' + conLaunchPgm
          + '&p1=' + %trim(strProgram) + '&l1=' + %char(conLen1)
          + '&p2=' + %trim(strJobInf) +  '&l2=' + %char(%len(strJobInf))
          + '&p3=' + %trim(strParm) + '&l3=' + %char(conLength);

         strUrl = %Trim(strUrl) + '&p4=Q&l4=1';

         return strUrl;
      /End-Free
     PCreateQteURL     E

     P*=====================================================================
     P* ElementClose - This will use close the number of elements requested (1 is default)
     P*                 to return the characters required to close the elements.
     P*                 The element closer characters will be consturcted/destructed by:
     P*                  StartJsonArray/EndJsonArray
     P*                  StartJsonElement/EndJsonElement
     P*                  StartGiantElement/EndGiantElement
     P*                  ElementOpen
     P*  parm 1 - accepts the number of elements to close (10a - optional) {default to *SINGLE}
     P*            - *SINGLE
     P*            - *ALL
     P*=====================================================================
     PElementClose     b                   export
     DElementClose     pi           500    varying
     D inElements                    10    options(*nopass) const varying

     Ddec#OfElements   s             10i 0 inz(1)
     DstrElements      s             10    inz('*SINGLE')
     DstrChars         s            500    inz varying
      /Free
       If %parms >= 1;
          strElements = inElements;
       Endif;
       Select;
       When Upper(strElements) = '*SINGLE';
       When Upper(strElements) = '*ALL';
          dec#OfElements = %len(strElementCloseChar);
       //***  Begin Add  ***  08/04/21  *************************
       Other;
          Monitor;
             dec#OfElements = %int(strElements);
          On-error;
          Endmon;
       //***  End   Add  ***  08/04/21  *************************
       Endsl;
       Select;
       When dec#OfElements <= 0;
          Return *blanks;
       When %len(strElementCloseChar) <= 0;
          Return *blanks;
       Endsl;
       strChars = %subst(strElementCloseChar:1:dec#OfElements);
       // Remove returned characters
       If dec#OfElements = %len(strElementCloseChar);
          Reset strElementCloseChar;
       Else;
          strElementCloseChar = %subst(strElementCloseChar:dec#OfElements+1:
           %len(strElementCloseChar)-dec#OfElements);
       Endif;
       Return strChars;
      /End-free
     PElementClose     e

     P*=====================================================================
     P* ElementOpen - This will use character passed in and add it to strElementCloseChar
     P*                and return it to be added to the json.
     P*  parm 1 - accepts the character to open the json element (1a)
     P*=====================================================================
     PElementOpen      b                   export
     DElementOpen      pi             1
     D inChar                         1    const

      /Free
       Select;
       When inChar = '{';
          strElementCloseChar = '}' + strElementCloseChar;
       When inChar = '[';
          strElementCloseChar = ']' + strElementCloseChar;
       Endsl;
       Return inChar;
      /End-free
     PElementOpen      e



     p*=====================================================================
     p* LaunchNewTab - Launch new tab
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - URL (720A)
     p*  Parm4 - Optional Link text (25A)
     p*  Parm5 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm6 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - link click Response
     P*=====================================================================
     PLaunchNewTab     B                   Export
     DLaunchNewTab     PI              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inURL                        720a   const
     D inLinkText                    25a   const options(*nopass)
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
      /Free
       Open SPIATRIUM;

       scMessage  = inMessage;
       scPnlTitle = inTitle;
       scURL      = inURL;
       scLinkText = 'Click to Open';
       scPLeft    = 350;
       scPTop     = 215;

       If %parms >= 4 and inLinkText <> *Blanks;
          scLinkText = inLinkText;
       EndIf;

       If %parms >= 5 And inLeft > 0;
          scPLeft = inLeft;
       EndIf;

       If %parms >= 6 And inTop > 0;
          scPTop = inTop;
       Endif;

       ExFmt rcdLnchTab;

       Close SPIATRIUM;

       Return scLinkClck;
      /End-Free
     PLaunchNewTab     E

     P*=====================================================================
     P* RtvUserLibl - Retrieves User library list for provided qualified jobname
     P*               Job should be active and user should have authority to the job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = Library list (32767A)
     P*=====================================================================
     PRtvUserLibl      B                   Export
     DRtvUserLibl      PI          2750
     D inJobName                     10    const
     D inJobUsrName                  10    const
     D inJobNumber                    6    const
      *
     D ds_job10700     ds         32767
     D  BytesRtn                     10I 0
     D  BytesAvl                     10I 0
     D  JobName                      10A
     D  UserId                       10A
     D  JobNumber                     6A
     D  IntJobId                     16A
     D  JobStatus                    10A
     D  JobType                       1A
     D  JobSubType                    1A
     D  Reserved                      2A
     D  NbrSysLibs                   10I 0
     D  NbrPrdLibs                   10I 0
     D  NbrCurLibs                   10I 0
     D  NbrUsrLibs                   10I 0
     D  Filler                    32685    Varying
      *
     D ds_Error        ds            15
     D  BytesProvided                10I 0 inz(%size(ds_Error))
     D  BytesAvail                   10I 0
     D  ErrorId                       7
      *
     D Position        S             10I 0 Inz(0)
     D LibsToSkip      S             10I 0 Inz(0)
     D idx             S             10I 0 Inz(0)
     D LibraryList     S          32767A   Varying
     D strQualJob      S             26A
     D strUserLibl     S           2750A
      *
      /free
       Monitor;
        strUserLibl = ' ';
        // Build qualified job name from input values
        strQualJob = inJobName+inJobUsrName+inJobNumber;

        // Call job information API to get job library list
        JobInfoAPI(ds_job10700:%Size(ds_job10700):'JOBI0700'
                   :strQualJob : *Blanks : ds_Error);

        // Position to user portion of library list
        LibsToSkip = NbrSysLibs + NbrPrdLibs + NbrCurLibs;
        Position   = (LibsToSkip * 11) + 81;

        // Retrieve user library list
        LibraryList = %SubSt(ds_job10700:Position:NbrUsrLibs * 11);
        strUserLibl = %Trim(LibraryList);
       On-Error;
        strUserLibl = 'X';
       EndMon;

       Return strUserLibl;
      /end-free
     PRtvUserLibl      E

     P*=====================================================================
     P* BuildBigJSON - Build JSON string for respons
     P*          Parm 1 = JSON string (32767 varsize)
     P*          Parm 2 = Field ID (50A)
     P*          Parm 3 = Field Value (5000A)
     P*=====================================================================
     PBuildBigJSON     B                   Export
     DBuildBigJSON     PI
     D inJSON                     32767a   Options(*VarSize)
     D inFieldID                     50    const
     D inFieldVal                  5000    const
     D*
     D strJSON         s          32767a   varying
     D strFieldVal     s                   like(inFieldVal)
     D lastChar        s              1    inz
      /Free
       If inFieldId = *blanks;
         Return;
       EndIf;

       strJSON = %TrimR(inJSON);
       lastChar = *Blanks;
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;

       Monitor;
       strFieldVal = %trim(RmvExceptJSON(%TrimR(inFieldVal)));
       On-Error;
          strFieldVal = %TrimR(inFieldVal);
       Endmon;
       strJSON += '"' + %TrimR(inFieldId) + '": "' + %TrimR(strFieldVal) + '"';
       inJSON = strJSON;
       Return;
      /End-free
     PBuildBigJSON     E

     P*=====================================================================
     P* BuildGiantJSON - Build JSON string for response to web interfac
     P*          Parm 1 = JSON string (8500000a varsize)
     P*          Parm 2 = Field ID (256a)
     P*          Parm 3 = Field Value (32767a varsize)
     P*=====================================================================
     PBuildGiantJSON   B                   Export
     DBuildGiantJSON   PI
     D inJSON                          a   len(8500000) varying
     D inFieldID                    256a                      const
     D inFieldVal                      a   len(32767) varying const
     D inExcept                       1n   options(*nopass)   const
     D*
     D strJSON         s                   like(inJSON)
     D strFieldVal     s                   like(inFieldVal)
     D lastChar        s              1    inz
     D isExcept        s              1n   inz('1')
      /Free
       If %parms >= 4;
          isExcept = inExcept;
       Endif;

       If inFieldId = *blanks;
         Return;
       EndIf;

       strJSON = %TrimR(inJSON);
       lastChar = *Blanks;
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;

       Monitor;
       Select;
       When isExcept;
          strFieldVal = %trim(RmvExceptJSON(%TrimR(inFieldVal)));
       Other;
          strFieldVal = %trimR(inFieldVal);
       Endsl;
       On-Error;
          strFieldVal = %TrimR(inFieldVal);
       Endmon;
       strJSON += '"' + %TrimR(inFieldId) + '": "' + %TrimR(strFieldVal) + '"';
       inJSON = strJSON;
       Return;
      /End-free
     PBuildGiantJSON   E

     P*=====================================================================
     P* BuildJSON - Build JSON string for respons
     P*          Parm 1 = JSON string (32767 varsize)
     P*          Parm 2 = Field ID (50A)
     P*          Parm 3 = Field Value (512A)
     P*=====================================================================
     PBuildJSON        B                   Export
     DBuildJSON        PI
     D inJSON                     32767a   Options(*VarSize)
     D inFieldID                     50    const
     D inFieldVal                   512    const
     D*
     D strJSON         s          32767a   varying
     D strFieldVal     s                   like(inFieldVal)
     D lastChar        s              1    inz
      /Free
       If inFieldId = *blanks;
         Return;
       EndIf;

       strJSON = %TrimR(inJSON);
       lastChar = *Blanks;
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;

       Monitor;
       strFieldVal = %trim(RmvExceptJSON(%TrimR(inFieldVal)));
       On-Error;
          strFieldVal = %TrimR(inFieldVal);
       Endmon;
       strJSON += '"' + %TrimR(inFieldId) + '": "' + %TrimR(strFieldVal) + '"';
       inJSON = strJSON;
       Return;
      /End-free
     PBuildJSON        E

       //===================================================================
       // View/Print PDF
       //===================================================================
     pBuildPDFUrl      b                   Export
     dBuildPDFUrl      pi           720
     d inDocId                       12                     const
     d inFold                        80                     const
     d inImgTitle                    50    options(*nopass) const
     d inKey                         40    options(*nopass) const
     d inSeq#                         5  0 options(*nopass) const
     d inUser                        10    options(*nopass) const

     d strDir          s             80
     d strImgFlr       s             50
     d strImgLnk       s            720
     d strImgTitle     s             50    inz('PDF Viewer')
     d strKey          s             40    inz
     d strSeq#         s              5    inz('00000')
     d xxSeq#          s              5  0 inz(0)
     d strUser         s             10

      /Free
       strUser = q1User;
       If %parms >= 3 and inImgTitle <> *blank;
          strImgTitle = inImgTitle;
       Endif;
       If %parms >= 4 and inKey <> *blank;
          strKey = inKey;
       Endif;
       If %parms >= 5 and inSeq# <> *zero;
          strSeq# = %Editc(inSeq#:'X');
          xxSeq# = inSeq#;
       Endif;
       If %parms >= 6 and inUser <> *blank;
          strUser = inUser;
       Endif;

       RtvDirPath(inFold:strDir:*blanks:*blanks);
       strImgFlr = strDir;

       xxFold  = strImgFlr;
       xxDocid = inDocid;

       If not %open(sdcmla);
          Open sdcmla;
       Endif;

       Chain(n) (strKey:xxSeq#) scm0a;
       If %found and S2UNCN <> *blanks;
          Exsr movearr;
          y = 255;
          Dow y > 1;
             If fold(y) = '/';
                xxFold  = %trim(%subst(S2Uncn:1:y-1));
                xxDocid = %trim(%subst(S2Uncn:y+1));
                Leave;
             Endif;
             y = y - 1;
          Enddo;
       Endif;

       strImgLnk = strImgTitle + '/iDartsWeb/remarks/attachments.do?method=get-
       AttachmentFile&key='+%trim(strKey)+'&sequence='+%trim(strSeq#)+'&docId-
       ='+%trim(xxDocId)+'&folder='+%trim(xxFold)+'^P&user='+%trim(strUser);

       If %open(sdcmla);
          Close sdcmla;
       Endif;

       Return strImgLnk;

      /End-free
     C**************************************************************************************
     C     movearr       BEGSR
     C                   Movea     S2Uncn        fold
     C                   ENDSR                                                  movearr subr
     C**************************************************************************************
     p BuildPDFUrl     e



       //***  Begin Add  ***  08/04/21  *********************************
       //===================================================================
       // Download file from UNC path
       //===================================================================
     pBuildUNCUrl      b                   Export
     dBuildUNCUrl      pi           720
     d inPath                       256    varying const

     DconPrefix        c                   '/iWAFileDownload/?filePath='
     DstrAddress       s            670    varying
     DstrIAccessURL    s            256    varying

      /Free
       strIAccessURL = %trim(IAccessURL());
       strAddress = strIAccessURL + conPrefix + inPath;
       Return strAddress;
      /End-free
     p BuildUNCUrl     e
       //***  End   Add  ***  08/04/21  *********************************

     P*=====================================================================
     P* StartJSONarray - Start JSON array contruct
     P*=====================================================================
     PStartJSONarray   B                   Export
     DStartJSONarray   PI
     D inJSON                     32767a   Options(*VarSize)
     D*
     D strJSON         s          32767a   varying
     D lastChar        s              1    inz
      /Free
       strJSON = %TrimR(inJSON);
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;
       strJSON += '[';
       inJSON = strJSON;
       strElementCloseChar = ']' + strElementCloseChar;
       Return;
      /End-free
     PStartJSONarray   E
     P*=====================================================================
     P* EndJSONarray - Start JSON array contruct
     P*=====================================================================
     PEndJSONarray     B                   Export
     DEndJSONarray     PI
     D inJSON                     32767a   Options(*VarSize)
     D*
     D strJSON         s          32767a   varying
      /Free
       strJSON = %TrimR(inJSON);
       strJSON += ']';
       inJSON = strJSON;
       Monitor;
          strElementCloseChar = %subst(strElementCloseChar:2:
           %len(strElementCloseChar)-1);
       On-error;
       Endmon;
       Return;
      /End-free
     PEndJSONarray     E
     P*=====================================================================
     P* StartJSONeleme - Start JSON array contruct
     P*=====================================================================
     PStartJSONelemnt  B                   Export
     DStartJSONelemnt  PI
     D inJSON                     32767a   Options(*VarSize)
     D*
     D strJSON         s          32767a   varying
     D lastChar        s              1    inz
      /Free
       strJSON = %TrimR(inJSON);
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;
       strJSON += '{';
       inJSON = strJSON;
       strElementCloseChar = '}' + strElementCloseChar;
       Return;
      /End-free
     PStartJSONelemnt  E
     P*=====================================================================
     P* EndJSONarray - Start JSON array contruct
     P*=====================================================================
     PEndJSONelemnt    B                   Export
     DEndJSONelemnt    PI
     D inJSON                     32767a   Options(*VarSize)
     D*
     D strJSON         s          32767a   varying
      /Free
       strJSON = %TrimR(inJSON);
       strJSON += '}';
       inJSON = strJSON;
       Monitor;
          strElementCloseChar = %subst(strElementCloseChar:2:
           %len(strElementCloseChar)-1);
       On-error;
       Endmon;
       Return;
      /End-free
     PEndJSONelemnt    E

     P*=====================================================================
     P* StartGiantElement - Start JSON element contruct
     P*=====================================================================
     PStartGiantElement...
     P                 B                   Export
     DStartGiantElement...
     D                 PI
     D inJSON                          a   len(8500000) varying
     D*
     D strJSON         s               a   len(8500000) varying
     D lastChar        s              1    inz
      /Free
       strJSON = %TrimR(inJSON);
       If strJSON <> *Blanks;
        //get Last char
         lastChar = %Subst(strJSON:%Len(strJSON):1);
       Endif;
       If strJSON<>*blanks And lastChar<>'{' And lastChar<>'['
        And lastChar<>':';
          strJSON += ', ';
       EndIf;
       strJSON += '{';
       inJSON = strJSON;
       strElementCloseChar = '}' + strElementCloseChar;
       Return;
      /End-free
     PStartGiantElement...
     P                 e

     P*=====================================================================
     P* EntGiantElement - End JSON element contruct
     P*=====================================================================
     PEndGiantElement  B                   Export
     DEndGiantElement  PI
     D inJSON                          a   len(8500000) varying
     D*
     D strJSON         s               a   len(8500000) varying
      /Free
       strJSON = %TrimR(inJSON);
       strJSON += '}';
       inJSON = strJSON;
       Monitor;
          strElementCloseChar = %subst(strElementCloseChar:2:
           %len(strElementCloseChar)-1);
       On-error;
       Endmon;
       Return;
      /End-free
     PEndGiantElement  E

     P*=====================================================================
     P* CreateBigURL - Create a Big URL that can be launched as Atrium tab
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 5  = Parameters for program call (50 A) each - optional
     P*          Parm 6 - Parm 8  = Parameters for program call (1024 A) each - optional
     P*          Returns = Character string with Tab name in first 50 and URL after that
     P*=====================================================================
     PCreateBigURL     B                   Export
     DCreateBigURL     PI          3500
     D inProgram                     10    const
     D inTabName                     50    const
     D inParms1                      50    options(*nopass) const
     D inParms2                      50    options(*nopass) const
     D inParms3                      50    options(*nopass) const
     D inParml1                    1024    options(*nopass) const
     D inParml2                    1024    options(*nopass) const
     D inParml3                    1024    options(*nopass) const
     D*
     D conPrefix       c                   const('/profoundui/auth/start?pgm=')
     D conLaunchPgm    c                   const('STLAUNCH')
     D conLen1         c                   const(10)
     D conLength       c                   const(3250)
     D conBlank        c                   const('%20')
     D conEnd          c                   const('*End')
     D*
     D strURL          s           3500    inz
     D strProgram      s             32    inz
     D strLibrary      s             10    inz
     D strParm         s           3250    inz
     D strJobInf       s             26    inz
     D intLen          s              5  0 inz
     D*
      /Free
         GetObjLib('STLAUNCH':'*PGM':strLibrary);
         // strLibrary = q1plib;
         strJobInf = q1job + q1cusr + %EditC(q1jobn:'X');

         strParm = '(';
         If %Parms > 2;
            strParm = %trim(strParm) + '''' + %Trim(inParms1) + '''';
         EndIf;
         If %Parms > 3;
            strParm = %trim(strParm) + ' ''' + %Trim(inParms2) + '''';
         EndIf;
         If %Parms > 4;
            strParm = %trim(strParm) + ' ''' + %Trim(inParms3) + '''';
         EndIf;
         If %Parms > 5;
            strParm = %trim(strParm) + ' ''' + %Trim(inParml1) + '''';
         EndIf;
         If %Parms > 6;
            strParm = %trim(strParm) + ' ''' + %Trim(inParml2) + '''';
         EndIf;
         If %Parms > 7;
            strParm = %trim(strParm) + ' ''' + %Trim(inParml3) + '''';
         EndIf;
         strParm = %trim(strParm) + ')' + conEnd;

         For intLen =  %len(%trim(inProgram)) to conLen1+1;
           strProgram = %trim(inProgram) + conBlank;
         EndFor;

         strUrl = inTabName + conPrefix
          + %trim(strlibrary) + '/' + conLaunchPgm
          + '&p1=' + %trim(strProgram) + '&l1=' + %char(conLen1)
          + '&p2=' + %trim(strJobInf) +  '&l2=' + %char(%len(strJobInf))
          + '&p3=' + %trim(strParm) + '&l3=' + %char(conLength);

         If %Trim(inTabName) = 'DEBUG';
           strUrl = %Trim(strUrl) + '&p4=D&l4=1';
         EndIf;

         return strUrl;
      /End-Free
     PCreateBigURL     E
     P*=====================================================================
     P* CreateGenURL - Create a General URL that can be used in Iframe or Elsewhere
     P*          Parm 1 = URL Prefix                (50 A)
     P*          Parm 2 - Parm 12  = Parameters for program call (50 A) each - optional
     P*          Returns = Character string with Tab name in first 50 and URL after that
     P*=====================================================================
     PCreateGenURL     B                   Export
     DCreateGenURL     PI           720
     D inPrefix                      50    const
     D inParm01                      50    options(*nopass) const
     D inParm02                      50    options(*nopass) const
     D inParm03                      50    options(*nopass) const
     D inParm04                      50    options(*nopass) const
     D inParm05                      50    options(*nopass) const
     D inParm06                      50    options(*nopass) const
     D inParm07                      50    options(*nopass) const
     D inParm08                      50    options(*nopass) const
     D inParm09                      50    options(*nopass) const
     D inParm10                      50    options(*nopass) const
     D inParm11                      50    options(*nopass) const
     D*
     D strURL          s            720    inz
     D*
      /Free

         strUrl = inPrefix;
         If %Parms > 1;
            strUrl = %trim(strUrl) + '?' + %Trim(inParm01);
         EndIf;
         If %Parms > 2;
            strUrl = %trim(strUrl) + %Trim(inParm02);
         EndIf;
         If %Parms > 3;
            strUrl = %trim(strUrl) + %Trim(inParm03);
         EndIf;
         If %Parms > 4;
            strUrl = %trim(strUrl) + %Trim(inParm04);
         EndIf;
         If %Parms > 5;
            strUrl = %trim(strUrl) + %Trim(inParm05);
         EndIf;
         If %Parms > 6;
            strUrl = %trim(strUrl) + %Trim(inParm06);
         EndIf;
         If %Parms > 7;
            strUrl = %trim(strUrl) + %Trim(inParm07);
         EndIf;
         If %Parms > 8;
            strUrl = %trim(strUrl) + %Trim(inParm08);
         EndIf;
         If %Parms > 9;
            strUrl = %trim(strUrl) + %Trim(inParm09);
         EndIf;
         If %Parms > 10;
            strUrl = %trim(strUrl) + %Trim(inParm10);
         EndIf;
         If %Parms > 11;
            strUrl = %trim(strUrl) + %Trim(inParm11);
         EndIf;

         return strUrl;
      /End-Free

     PCreateGenUrl     E

     P*=====================================================================
     P* CreateBigQteURL - Create a Big URL that can be launched as Atrium tab for quote environment
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 5  = Parameters for program call (50 A) each - optional
     P*          Parm 6 - Parm 8  = Parameters for program call (1024 A) each - optional
     P*          Returns = Character string with Tab name in first 50 and URL after that
     P*=====================================================================
     PCreateBigQteURL  B                   Export
     DCreateBigQteURL  PI          3500
     D inProgram                     10    const
     D inTabName                     50    const
     D inParms1                      50    options(*nopass) const
     D inParms2                      50    options(*nopass) const
     D inParms3                      50    options(*nopass) const
     D inParml1                    1024    options(*nopass) const
     D inParml2                    1024    options(*nopass) const
     D inParml3                    1024    options(*nopass) const
     D*
     D conPrefix       c                   const('/profoundui/auth/start?pgm=')
     D conLaunchPgm    c                   const('STLAUNCH')
     D conLen1         c                   const(10)
     D conLength       c                   const(3250)
     D conBlank        c                   const('%20')
     D conEnd          c                   const('*End')
     D*
     D strURL          s           3500    inz
     D strProgram      s             32    inz
     D strLibrary      s             10    inz
     D strParm         s           3250    inz
     D strJobInf       s             26    inz
     D intLen          s              5  0 inz
     D*
      /Free
         GetObjLib('STLAUNCH':'*PGM':strLibrary);
         // strLibrary = q1plib;
         strJobInf = q1job + q1cusr + %EditC(q1jobn:'X');

         strParm = '(';
         If %Parms > 2;
            strParm = %trim(strParm) + '''' + %Trim(inParms1) + '''';
         EndIf;
         If %Parms > 3;
            strParm = %trim(strParm) + ' ''' + %Trim(inParms2) + '''';
         EndIf;
         If %Parms > 4;
            strParm = %trim(strParm) + ' ''' + %Trim(inParms3) + '''';
         EndIf;
         If %Parms > 5;
            strParm = %trim(strParm) + ' ''' + %Trim(inParml1) + '''';
         EndIf;
         If %Parms > 6;
            strParm = %trim(strParm) + ' ''' + %Trim(inParml2) + '''';
         EndIf;
         If %Parms > 7;
            strParm = %trim(strParm) + ' ''' + %Trim(inParml3) + '''';
         EndIf;
         strParm = %trim(strParm) + ')' + conEnd;

         For intLen =  %len(%trim(inProgram)) to conLen1+1;
           strProgram = %trim(inProgram) + conBlank;
         EndFor;

         strUrl = inTabName + conPrefix
          + %trim(strlibrary) + '/' + conLaunchPgm
          + '&p1=' + %trim(strProgram) + '&l1=' + %char(conLen1)
          + '&p2=' + %trim(strJobInf) +  '&l2=' + %char(%len(strJobInf))
          + '&p3=' + %trim(strParm) + '&l3=' + %char(conLength);

         strUrl = %Trim(strUrl) + '&p4=Q&l4=1';

         return strUrl;
      /End-Free
     PCreateBigQteURL  E
