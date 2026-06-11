      *=====================================================================
      * Application Security Routines - Procedure Definitions
      *=====================================================================
     **********************************************************************************************
      * REVISIONS:                                                                                 *
      *
      * 04/04/23 C:344 Project: WC2430
      *                Add ChkUsrAut for User Authority Checking
      *
      * 05/09/22 C:079 Project: 129WWV
      *                Add Procedure for Application Configuration Security
      *                                                                                            *
      * 05/15/18 C:291 Project: 150WCR                                                             *
      *                Add CheckSecLevel prototype                                                 *
      *                                                                                            *
      * 04/29/13 C:136 Project: SY307                                                              *
      *                Add CheckOFSec prototype                                                    *
      *                                                                                            *
      * 04/17/12 C:33  Project: WC2115                                                             *
      *                Add routines:                                                               *
      *                - DisplaySS#                                                                *
      *                - DisplayFEIN                                                               *
      *                                                                                            *
      * 10/19/11 C:061 project 139IQJ                                                              *
      *                 Created.                                                                   *
      *                                                                                            *
     **********************************************************************************************

     P*====================================================================
     P* Edit the FEIN for All, None or Last 4
      *  Parameter 1 - FEIN
      *  Parameter 2 - Add dash (1/0)
      *  Parameter 3 - Type of editing ('*' will use system defined edits)
      *  Returns - FEIN with edit applied
     P*====================================================================
     DDisplayFEIN      pr            10
     D inSS#                          9    const
     D inAddDash                      1n   const options(*nopass)
     D inDisplayType                  1    const options(*nopass)

     P*====================================================================
     P* Edit the SS# for All, None or Last 4
      *  Parameter 1 - SS#
      *  Parameter 2 - Add dash (1/0)
      *  Parameter 3 - Type of editing ('*' will use system defined edits)
      *  Returns - SS# with edit applied
     P*====================================================================
     DDisplaySS#       pr            11
     D inSS#                          9    const
     D inAddDash                      1n   const options(*nopass)
     D inDisplayType                  1    const options(*nopass)

      *=====================================================================
      * New_KeyCode - Generate New Key Code
      *  Parameter 1 - timestamp (z)
      *  Returns - 12 char - Key Code
      *=====================================================================
     d New_KeyCode     pr            12a
     d   timedate                      z   Const


      *=====================================================================
     * On_PDSDEV - return *on if process is being run on PDSDEV machine
     *             Otherwise, return *off
      *=====================================================================
     D On_PDSDEV       pr              n


      *=====================================================================
      * Valid_KeyCode - Check to verify Key Code is valid
      *  Parameter 1 - 12 char Key Code
      *  Returns - *on if valid; otherwise *off
      *=====================================================================
     d Valid_KeyCode   pr              n
     d   @@KeyCode                   12a   Const
     d   @@Term                       1a   Options( *nopass )
      *=====================================================================
      * CheckOFSec  - Implement application security on
      *               CompSuite toolbar selections and application
      *               buttons
      *  Parameter 1 - Program ID (10A)
      *  Parameter 2 - Option/Funtion Type (1A)
      *  Parameter 3 - Option/Funtion Number (1A)
      *  Parameter 4 - Quote/Live flag
      *  Parameter 5 - returns message if user not allowed
      *  Returns - *on if user is allowed to access the action
      *=====================================================================
     d CheckOFSec      pr              n
     d  inPgmID                      10    Const
     d  inType                        1    Const
     d  inNumber                      2  0 Const
     d  inEnv                         1
     d  outMessage                  100
      *=====================================================================
      * CheckSecLevel - Application level security
      *  Parameter 1  - Application ID (10A)
      *  Parameter 2  - Security Type  (1A)
      *  Parameter 3  - Security Level (1A)
      *  Returns - *on if user is allowed to access the action
      *=====================================================================
     d CheckSecLevel   pr              n
     d  inAppID                      10    Const
     d  inType                        1    Const options(*nopass)
     d  inNumber                      3  0 Const options(*nopass)
      *=====================================================================
      * UserAccessWdw - Application Security User Access
      *  Parameter 1  - Display File Name (10A)
      *  Parameter 2  - Progam Name (50A)
      *  Parameter 3  - Tab Name (50A)
      *  Returns - Nothing is returned
      *=====================================================================
     d UserAccessWdw   pr
     d  inDspFileName                10    Const
     d  inPgmName                    50    Const
     d  inTabName                    50    Const
      *=====================================================================
      * ChkUsrAut - Check User Authority
      *  Parameter 1  - Application ID (10A)
      *=====================================================================
     d ChkUsrAut       pr              n
     d  inApplID                     10    Const
      *=====================================================================
