     D*=====================================================================
     D* Atrium related procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* AtriumJob - Determines if the job is an Atrium job                  name
     P*   Parm 1 = Job Name (10A)      {optional}
     P*   Parm 2 = Job user name (10A) {optional}
     P*   Parm 3 = Job number (6A)     {optional}
     P*   Returns = *on if Atrium, otherwise *off
     P*=====================================================================
     DAtriumJob        pr             1n
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
       //===================================================================
       // View/Print PDF
       //===================================================================
     dBuildPDFUrl      pr           720
     d inDocId                       12                     const
     d inFold                        80                     const
     d inImgTitle                    50    options(*nopass) const
     d inKey                         40    options(*nopass) const
     d inSeq#                         5  0 options(*nopass) const
     d inUser                        10    options(*nopass) const

       //===================================================================
       // Download file from UNC path
       //===================================================================
     dBuildUNCUrl      pr           720
     d inPath                       256    varying const

     P*=====================================================================
     P* CreateURL - Create a URL that can be launched as Atrium tab
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 12  = Parameters for program call (50 A) each - optional
     P*          Returns = Character string URL
     P*=====================================================================
     DCreateURL        PR           720
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
      *
     P*=====================================================================
     P* CreateQteURL - Create a URL that can be launched as Atrium tab for quote environment
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 12  = Parameters for program call (50 A) each - optional
     P*          Returns = Character string URL
     P*=====================================================================
     DCreateQteURL     PR           720
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

      //***  Begin Add  ***  07/21/19  ************************************
     P*=====================================================================
     P* ElementClose - This will use close the number of elements requested  (1 is default)
     P*                 to return the characters required to close the elements.
     P*                 The element closer characters will be consturcted/destructed by:
     P*                  StartJsonArray/EndJsonArray
     P*                  StartJsonElement/EndJsonElement
     P*                  StartGiantElement/EndGiantElement
     P*  parm 1 - accepts the number of elements to close (10.0 - optional) {default to all}
     P*=====================================================================
     DElementClose     pr           500    varying
     D inElements                    10    options(*nopass) const varying

     P*=====================================================================
     P* ElementOpen - This will use character passed in and add it to strElementCloseChar
     P*                and return it to be added to the json.
     P*  parm 1 - accepts the character to open the json element (1a)
     P*=====================================================================
     DElementOpen      pr             1
     D inChar                         1    const
      //***  End   Add  ***  07/21/19  ************************************

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
     DLaunchNewTab     PR              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inURL                        720a   const
     D inLnkTxt                      25a   const options(*nopass)
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
     P*=====================================================================
     P* RtvUserLibl - Retrieves User library list for provided qualified jobname
     P*               Job should be active and user should have authority to the job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = Library list (32767A)
     P*=====================================================================
     DRtvUserLibl      PR          2750
     D inJobName                     10    const
     D inJobUsrName                  10    const
     D inJobNumber                    6    const
     P*=====================================================================
     P* BuildBigJSON - Build JSON string for respons
     P*          Parm 1 = JSON string (32767 varsize)
     P*          Parm 2 = Field ID (50A)
     P*          Parm 3 = Field Value (5000A)
     P*=====================================================================
     DBuildBigJSON     PR
     D inJSON                     32767a   Options(*VarSize)
     D inFieldID                     50    const
     D inFieldVal                  5000    const

     P*=====================================================================
     P* BuildGiantJSON - Build JSON string for response to web interfac
     P*          Parm 1 = JSON string (8500000a varsize)
     P*          Parm 2 = Field ID (256a)
     P*          Parm 3 = Field Value (32767a varsize)
     P*=====================================================================
     DBuildGiantJSON   pr
     D inJSON                          a   len(8500000) varying
     D inFieldID                    256a                      const
     D inFieldVal                      a   len(32767) varying const
     D inExcept                       1n   options(*nopass)   const

     P*=====================================================================
     P* BuildJSON - Build JSON string for respons
     P*          Parm 1 = JSON string (32767 varsize)
     P*          Parm 2 = Field ID (50A)
     P*          Parm 3 = Field Value (512A)
     P*=====================================================================
     DBuildJSON        PR
     D inJSON                     32767    Options(*VarSize)
     D inFieldID                     50    const
     D inFieldVal                   512    const

     P*=====================================================================
     P* StartJSONarray - Start JSON array contruct
     P*=====================================================================
     DStartJSONarray   PR
     D inJSON                     32767a   Options(*VarSize)

     P*=====================================================================
     P* EndJSONarray - End   JSON array contruct
     P*=====================================================================
     DEndJSONarray     PR
     D inJSON                     32767a   Options(*VarSize)

     P*=====================================================================
     P* StartJSONelemnt- Start JSON array element
     P*=====================================================================
     DStartJSONelemnt  PR
     D inJSON                     32767a   Options(*VarSize)

     P*=====================================================================
     P* EndJSONelemnt- End   JSON array contruct
     P*=====================================================================
     DEndJSONelemnt    PR
     D inJSON                     32767a   Options(*VarSize)

     P*=====================================================================
     P* StartGiantElement - Start JSON element contruct
     P*=====================================================================
     DStartGiantElement...
     D                 pr
     D inJSON                          a   len(8500000) varying

     P*=====================================================================
     P* EntGiantElement - End JSON element contruct
     P*=====================================================================
     DEndGiantElement  pr
     D inJSON                          a   len(8500000) varying

     P*=====================================================================
     P* CreateBigURL - Create a Big URL that can be launched as Atrium tab
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 5  = Parameters for program call (50 A) each - optional
     P*          Parm 6 - Parm 8  = Parameters for program call (1024 A) each - optional
     P*          Returns = Character string URL
     P*=====================================================================
     DCreateBigURL     PR          3500
     D inProgram                     10    const
     D inTabName                     50    const
     D inParms1                      50    options(*nopass) const
     D inParms2                      50    options(*nopass) const
     D inParms3                      50    options(*nopass) const
     D inParml1                    1024    options(*nopass) const
     D inParml2                    1024    options(*nopass) const
     D inParml3                    1024    options(*nopass) const
     P*=====================================================================
     P* CreateGenURL - Create a General URL
     P*          Parm 1 = URL Prefix                (50 A)
     P*          Parm 2 - Parm 12  = Parameters for program call (50 A) each - optional
     P*          Returns = Character string URL
     P*=====================================================================
     DCreateGenURL     PR           720
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
     P*=====================================================================
     P* CreateBigQteURL - Create a Big URL that can be launched as Atrium tab for quote environment
     P*          Parm 1 = Program name to be called (10 A)
     P*          Parm 2 = Tab name to be opened     (50 A)
     P*          Parm 3 - Parm 5  = Parameters for program call (50 A) each - optional
     P*          Parm 6 - Parm 8  = Parameters for program call (1024 A) each - optional
     P*          Returns = Character string URL
     P*=====================================================================
     DCreateBigQteURL  PR          3500
     D inProgram                     10    const
     D inTabName                     50    const
     D inParms1                      50    options(*nopass) const
     D inParms2                      50    options(*nopass) const
     D inParms3                      50    options(*nopass) const
     D inParml1                    1024    options(*nopass) const
     D inParml2                    1024    options(*nopass) const
     D inParml3                    1024    options(*nopass) const
