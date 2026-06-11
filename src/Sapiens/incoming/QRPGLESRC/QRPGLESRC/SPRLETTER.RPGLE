      *********************************************************************
      * REVISIONS:
      *
      * 01/23/25 C:65  Project: SY446 (Design and Compose revamp)
      *                Add prototype for ListDocs procedure.
      *
      * 08/13/21 C:65  Project: WC2278
      *                Add RRN to OutputWindow to allow email recipients to
      *                to resolve to actual addresses.
      *
      * 03/23/17 C:136 Support: 148WAC
      *                Add optional parm (inMode) to OutputWindow
      *                procedure to not display the window
      *
      * 12/01/16 C:136 Project: SY371
      *                Add optional parm (inCnlHIde) to OutputWindow
      *                procedure to hide the cancel button
      *
      * 07/28/16 C:65  Project: SY371
      *                Add a procedure for displaying an "Output Choices"
      *                window.
      *
      * 04/01/16 C:65  Project: SY371
      *                Add interactive browser launch procedure.
     *
     *
      *********************************************************************
     D*=====================================================================
     D* Documents/Letters processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* DocWindow - Display and process popup window for policy/claim documents.
    P*
    P*   inRRN     - passed in for validation                  (required)
    P*   inCatg    - passed in for validation                  (required)
    P*   inMode    - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DDocWindow        PR
     D inRRN                          9  0 const
     D inCatg                         5    const
     D inMode                         1    const options(*nopass)
    D*
     D*** begin add ***  04/01/16  *************************************************
    P*====================================================================
    P* LaunchDoc - Generate XML file, build URL, and display to screen for
    P*             launching a browser window for interactive docs.
    P*
    P*  LaunchDoc - returns error message or blank for successful launch.
    P*   inXML     - passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DLaunchDoc        PR           250
     D inXML                         60    const
     D*** end add *****  04/01/16  *************************************************
      ***  begin add ***  07/28/16  ******************************************
     P*=====================================================================
     P* OutputWindow - Display a window for output choiuces for Scriptura
     P*                enabled processes.  Returns off for cancel, on for
     P*                submit.
     P*
     P*          Parm  1 = Type of Process               (100A) (required)
     P*          Parm  2 = Mass Process Flag             (  1A) (required)
     P*
     P*=====================================================================
     DOutputWindow     PR             1n
     D inType                       100    const
     D inMass                         1    const
     D inCnlHide                      1    const  options(*nopass)
     D inMode                         1    const  options(*nopass)
     D*** begin add ***  08/13/21  *******************************************
     D inRRN                          9  0 const  options(*nopass)
     D*** end add *****  08/13/21  *******************************************
     D*
      ***  end add *****  07/28/16  ******************************************
      *** begin add ***  01/23/25  ******************************************
     P*=====================================================================
     P* ListDocs - Call Scriptura server and request a list of documents
     P*            from the D&C Cloud.  Returns on if document is in cloud
     P*            along with documentID in optional parameter.
     P*
     P*          Parm  1 = Document Name                 (100A) (required)
     P*          Parm  2 = Document ID                   (100A) (optional)
     P*
     P*=====================================================================
     DListDocs         PR             1n
     D inDoc                        100    const
     D outID                        100    options(*nopass)
      *** end add *****  01/23/25  ******************************************
