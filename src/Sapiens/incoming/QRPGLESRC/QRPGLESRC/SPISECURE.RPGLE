     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
      **************************************************************************
      * REVISIONS:
      *
      * 08/21/24 C:300 Support: 117388
      *                WC2910 changes (07/04/23) required only for Group Configuration
      *                Security ('GrpCfgAut').
      *                For other screens like A/R batches, Return Funds Batches and
      *                Policy Endorsement Forms -- User not required (999/*) authority
      *                to access, If NO Sec Level/Type available in SDACP file
      *
      * 07/04/23 C:300 Project: WC2910, Support: 112941
      *                Changes in CheckSecLevel procedure for Group Configuration Security
      *                Allow access, If NO Sec Level/Type available in SDACP file
      *                and user has all authority (999/*)
      *
      * 04/10/23 C:344 Project: WC2430
      *                Add Procedure for Checking User Authority.
      *
      * 05/09/22 C:079 Project: 129WWV
      *                Add Procedure for Application Configuration Security
      *
      * 05/15/18 C:291 Project: 150WCR
      *                Add CheckSecLevel procedure.
      *
      * 04/29/13 C:136 Project: SY307
      *                Implement Application Security into
      *                CompSuite with CheckSecurity subprocedure
      *
      * 04/17/12 C:33  Project: WC2115
      *                Add routines:
      *                - DisplaySS#
      *                - DisplayFEIN
      *
      * 10/19/11 C:061 project 139IQJ
      *                 Created.
      *
      **************************************************************************
     fspisecure cf   e             workstn extfile('SPISECURE') UsrOpn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflUsrAcc:rrnUsrAcc)
      * Key Code Used
     fsdkcl     if a e           k disk    usropn
     fsdofl1    if   e           k disk    usropn rename(POf01 : POf0l1)
     fsdacl     if   e           k disk    usropn rename(pac01:pac00)
     f*** Beg Add *** 05/09/22 *************************************************
     fsdtbl1    uf a e           k disk    usropn infds(dbfdsTb)
     fsdtbp     uf   e             disk    usropn Rename(ptb01:ptb02)
     f*** End Add *** 05/09/22 *************************************************

     D/copy sprdate
     D/copy sprsecure
     D/copy wprusrprms
     d*** Beg Add *** 05/09/22 *************************************************
     d/copy sprgenpop
     d/copy sprstring
     d*** End Add *** 05/09/22 *************************************************

     d Check_KeyCode   pr             1n
     d   @@KeyCode                   12a   Const
     d   @@DateTime                    z   Const

     d CheckPDSDEV     pr                  extpgm('SAPDSDEV')
     d  Return_val                    1a

     DEditSS#FEIN      pr             9
     D inSS#FEIN                      9    const
     D inDisplayType                  1    const options(*nopass)

     d Get_Code        pr            12a
     d  dattim                         z   const

     D Write_SDKCP     pr
     d   @@KeyCode                   12a   Const

     d CheckAuth       pr                  extpgm('SRCCSC')
     d inUTYP                         1    const
     d inULVL                         3    const
     d inCTYP                         1    const
     d inCLVL                         3    const
     d inEXEC                         1    const
     d outRes                         1
     d inCTP2                         1    const
     d inCTP3                         1    const
     d inCTP4                         1    const
     d inCTP5                         1    const
     d inCTP6                         1    const
     d inCLV2                         3    const
     d inCLV3                         3    const
     d inCLV4                         3    const
     d inCLV5                         3    const
     d inCLV6                         3    const
     D inCTP7                         1    const options(*nopass)
     D inCTP8                         1    const options(*nopass)
     D inCTP9                         1    const options(*nopass)
     D inCTP0                         1    const options(*nopass)
     D inCLV7                         3    const options(*nopass)
     D inCLV8                         3    const options(*nopass)
     D inCLV9                         3    const options(*nopass)
     D inCLV0                         3    const options(*nopass)

     d*** Beg Add *** 05/09/22 *************************************************
     dUserAccFilter    pr
     d inMode                        10    const options(*nopass)
     d
     d dbfdsTb         ds
     d  dbrrnTb              397    400b 0
     d*** End Add *** 05/09/22 *************************************************
     d psds          esds                  extname(szq1p)
      *=====================================================================
      * Check_KeyCode - Check to verify Key Code is valid at given time
      *  Parameter 1 - 12 char - Key Code
      *  Parameter 2 - Z - timestamp
      *  Returns - *in - '1' valid; otherwise '0'
      *=====================================================================
     p Check_KeyCode   b
     d Check_KeyCode   PI              n
     d   @@KeyCode                   12a   Const
     d   @@DateTime                    z   Const

     d datetime        s               z
     d ix              s              3s 0
     d rtv             s               n

      /free

           // Determine if KeyCode is Valid

           rtv = *off;
           datetime = @@DateTime;
           datetime -= %minutes(4);
           for ix = 1 to 10;
              if (@@KeyCode <> *blanks)
                        and (@@KeyCode = Get_Code(datetime));
                  rtv = *on;
                  leave;
              endif;
              datetime += %minutes(1);
           endfor;

           return rtv;

      /end-free

     p Check_KeyCode   e

     PDisplayFEIN      b                   export
     P*====================================================================
     P* Edit the FEIN for All, None or Last 4
      *  Parameter 1 - FEIN
      *  Parameter 2 - Add dash (1/0)
      *  Parameter 3 - Type of editing ('*' will use system defined edits)
      *  Returns - FEIN with edit applied
     P*====================================================================
     DDisplayFEIN      pi            10
     D inFEIN                         9    const
     D inAddDash                      1n   const options(*nopass)
     D inDisplayType                  1    const options(*nopass)

     DdisplayType      s              1    inz('A')
     DaddDash          s              1n   inz('1')

     DoutFEIN          s             10

      /free
       If %parms >= 2;
          addDash = inAddDash;
       Endif;

       If %parms >= 3 and inDisplayType <> *blank;
          displayType = inDisplayType;
       Endif;

       outFEIN = EditSS#FEIN(inFEIN:displayType);

       Select;
       When addDash;
          Return %subst(outFEIN:1:2) + '-' + %subst(outFEIN:3:7);
       Other;
          Return outFEIN;
       Endsl;

      /end-free

     PDisplayFEIN      e

     PDisplaySS#       b                   export
     P*====================================================================
     P* Edit the SS# for All, None or Last 4
      *  Parameter 1 - SS#
      *  Parameter 2 - Add dash (1/0)
      *  Parameter 3 - Type of editing ('*' will use system defined edits)
      *  Returns - SS# with edit applied
     P*====================================================================
     DDisplaySS#       pi            11
     D inSS#                          9    const
     D inAddDash                      1n   const options(*nopass)
     D inDisplayType                  1    const options(*nopass)

     DdisplayType      s              1    inz('A')
     DaddDash          s              1n   inz('1')

     DoutSS#           s             11

      /free
       If %parms >= 2;
          addDash = inAddDash;
       Endif;

       If %parms >= 3 and inDisplayType <> *blank;
          displayType = inDisplayType;
       Endif;

       outSS# = EditSS#FEIN(inSS#:displayType);

       Select;
       When addDash;
          Return %subst(outSS#:1:3) + '-' + %subst(outSS#:4:2) + '-'
           + %subst(outSS#:6:4);
       Other;
          Return outSS#;
       Endsl;
      /end-free

     PDisplaySS#       e

     PEditSS#FEIN      b
     P*====================================================================
     P* Edit the SS# or FEIN for All, None or Last 4
      *  Parameter 1 - SS# or FEIN
      *  Parameter 3 - Type of editing ('*' will use system defined edits)
      *  Returns - SS#/FEIN with edit applied
     P*====================================================================
     DEditSS#FEIN      pi             9
     D inSS#FEIN                      9    const
     D inDisplayType                  1    const options(*nopass)

     DdisplayAll       c                   'A'
     DdisplayNone      c                   'N'
     DdisplayLast4     c                   '4'
     DeditCharacters   c                   '*********'
     DdisplayType      s              1    inz('4')

     DoutSS#FEIN       s              9

      /free
       If %parms >= 2 and inDisplayType <> *blank;
          displayType = inDisplayType;
       Endif;

       outSS#FEIN = inSS#FEIN;

       Select;
       When displayType = displayAll;
          // no changes;
       When displayType = displayNone;
          outSS#FEIN = editCharacters;
       When displayType = displayLast4;
          %subst(outSS#FEIN:1:5) = editCharacters;
       Endsl;

       Return outSS#FEIN;

      /end-free

     PEditSS#FEIN      e

      *====================================================================
      * Get_Code - convert time stamp to code
      *====================================================================
     P Get_Code        b
     D Get_Code        pi            12a
     D  dattim                         z   const

     d num             c                   '0123456789'
     d let             c                   'WKGZLFERVJ'
     d N_val15         s             15s 0
     d C_val15         s             15a
     d C_val12         s             12a
     d C_val11         s             12a

     d yyyy            s              4  0
     d mm              s              2  0
     d dd              s              2  0
     d hh              s              2  0
     d nn              s              2  0

      /free

        yyyy = %subdt(dattim:*years);
        mm  = %subdt(dattim:*months);
        dd  = %subdt(dattim:*days);
        hh  = %subdt(dattim:*hours);
        nn  = %subdt(dattim:*minutes);

        N_val15 = (yyyy * 149) +
                  ((mm * 104707) * 10007) +
                  ((dd * 73681) * ((nn + 3)**2)) +
                  (hh * 13001) +
                  ((nn + hh + 3) * 102433) +
                  ((mm + dd + hh + nn) * 3299931317) +
                  ((nn + 3) * 104729);

        C_val15 = %char(N_val15);

        // Translate selected digits into numbers

        %subst(C_val15 : (%rem(%int(yyyy * nn):12) + 1) : 1) =
          %xlate(num: let: %subst(C_val15: (%rem(hh:12) + 1) : 1) );

        %subst(C_val15 : (%rem(%int(mm * nn):12) + 1) : 1) =
          %xlate(num: let: %subst(C_val15: (%rem(hh:12) + 1) : 1) );

        %subst(C_val15 : (%rem(%int(dd * nn):12) + 1) : 1) =
          %xlate(num: let: %subst(C_val15: (%rem(dd:12) + 1) : 1) );

        %subst(C_val15 : (%rem(%int(hh * nn):12) + 1) : 1) =
          %xlate(num: let: %subst(C_val15: (%rem(hh:12) + 1) : 1) );

        %subst(C_val15 : (%rem(%int(nn):12) + 1) : 1) =
          %xlate(num: let: %subst(C_val15: (%rem(nn:12) + 1) : 1) );

        // Return Key Code

        C_val12 = %trim(C_val15) + 'R63U734Y7D34';
        return C_val12;

      /end-free

     p Get_Code        e


      *=====================================================================
      * New_KeyCode - Generate New Key Code
      *  Parameter 1 - timestamp (z)
      *  Returns - 12 char - Key Code
      *=====================================================================
     *
     *  Some programs are secured with a Key Code such that they may
     *  only be run by StoneRiver Employees.
     *
     *  The Key Codes are based upon system time and are only valid for
     *  a few minutes from the time entered.
     *
     *  Program SS030R uses this procedure to generate valid Key Codes.
     *  SS030R must only be run from PDSDEV.
      *
      *  Procedure Valid_KeyCode (described below) may be used by program
      *  at client site to determine if KeyCode entered is valid.
      *
      *=====================================================================
     p New_KeyCode     b                   Export
     d New_KeyCode     pi            12a
     d   datetime                      z   Const

     d xxcode          s             12a

      /free

           // Generate new KeyCode (If at PDSDEV)

             If On_PDSDEV();
               xxcode = Get_Code(datetime);
             else;
               xxcode = *BLANKS;
             endif;

      /end-free

     c                   return    xxcode

     p New_KeyCode     E


      *=====================================================================
     * On_PDSDEV - return *on if process is being run on PDSDEV machine
     *             otherwise return *off
      *=====================================================================
     p On_PDSDEV       b                   Export
     d On_PDSDEV       pi              n

     d rtv             s              1a

     c                   callp     CheckPDSDEV(rtv)

     c                   if        rtv = 'Y'
     c                   return    *on
     c                   else
     c                   return    *off
     c                   endif

     p On_PDSDEV       E


      *=====================================================================
      * Valid_KeyCode - Check to verify Key Code is valid
      *  Parameter 1 - 12 char - Key Code
      *  Parameter 2 - 1 char - 'T'erminate validity of KeyCode
      *  Returns - *on if valid; otherwise *off
      *=====================================================================
      *
      *   Used to secure so that only StoneRiver personel can run
      *   process. Key Codes generated by New_KeyCode (described above) are
      *   only valid for 10 minutes.
      *
      *   If KeyCode is valid, return value 'Y' is returned.
      *   If KeyCode is not valid, value 'N' is returned.
      *
      *   If second parameter is passed as 'T', and valid Key Code was
      *   passed; then Key Code is immediately expired (SDKCP record
      *   written).
      *
      *   A KeyCode that was valid as of the time the current process
      *   entered the system is considered valid (Unless it has been
      *   expired by call of Valid_KeyCode with second parm = 'T').
      *
      *=====================================================================
     p Valid_KeyCode   b                   export
     d Valid_KeyCode   PI              n
     d   @@KeyCode                   12a   Const
     d   @@Term                       1a   Options( *nopass )

     d ix              s              3s 0
     d rtv             s               n

      /free

        // Determine if KeyCode is Valid

        rtv = *off;
        open sdkcl;
        chain (@@KeyCode) pkc01;
        if not %found;

            // Check if KeyCode valid at present time
            // or at time job entered system.
            if Check_KeyCode(@@KeyCode:%timestamp()) or
               Check_KeyCode(@@KeyCode:JobEntSys());
                rtv = *on;
                if (%parms >= 2) and (@@term = 'T');
                   callp write_SDKCP(@@KeyCode);
                Endif;
            endif;
        endif;
        close sdkcl;
        return rtv;

      /end-free

     p Valid_KeyCode   e


      *====================================================================
      * Write_SDKCP
      *====================================================================
     P Write_SDKCP     b
     D Write_SDKCP     pi
     d   @@KeyCode                   12a   Const

      /free

         clear pkc01;
         kccode = @@KeyCode;
         kcuser = q1user;
         kcpgm  = q1pgm;
         kcdt   = %int(%char(%date(): *cymd0));
         kctime = %int(%char(%time(): *hms0));
         write pkc01;

      /end-free

     p Write_SDKCP     e
      *==================================================================
      * CheckOFDSec   - Compare user and application security settings
      *                 with the old SDOFP (Option/Function Security) file.
      *                 Return *on if user is authorized access.
      *==================================================================
     p CheckOFSec      b                   export
     d CheckOFSec      pi              n
     d  inPgmID                      10    const
     d  inType                        1    const
     d  inNumber                      2  0 const
     d  inEnv                         1
     d  outMessage                  100

     d clientID        s              3
     d flgEnv          s              1
     d outResult       s              1
     d userLevl        s              3  0
     d userType        s              1
     d userAllowed     s               n   inz(*on)
     d userNotAllowed  s               n   inz(*off)

     c     keySDOFL1     klist
     c                   kfld                    inPgmID
     c                   kfld                    inEnv
     c                   kfld                    inType
     c                   kfld                    inNumber

      /Free
       userLevl = GetUsrLvl(Q1USER);
       userType = GetUsrType(Q1USER);

       Open SDOFL1;
       Chain keySDOFL1 POf0l1;
       If inEnv = 'Q' and not(%found);
          inEnv = 'L';
          Chain keySDOFL1 POf0l1;
       Endif;
       Close SDOFL1;
       If %found;
          CheckAuth(userType :
                    %editc(userLevl : 'X') :
                    OFty01 :
                    %editc(OFlvl : 'X') :
                          '1' :
                          outResult :
                          OFty02 :
                          OFty03 :
                          OFty04 :
                          OFty05 :
                          OFty06 :
                          %editc(OFlv02 : 'X') :
                          %editc(OFlv03 : 'X') :
                          %editc(OFlv04 : 'X') :
                          %editc(OFlv05 : 'X') :
                          %editc(OFlv06 : 'X') );
          If outResult = *on;
             outMessage = 'You do not have sufficient authority ' +
              'to access this action.';
             Return userNotAllowed;
          Else;
             outMessage = *blanks;
             Return userAllowed;
          Endif;
       Else;
          Return userAllowed;
       Endif;

      /End-free

     p CheckOFSec      e

      *==================================================================
      * CheckSecLevel - Compare user and application security settings.
      *                 Return *on if user is authorized access.
      *==================================================================
     p CheckSecLevel   b                   export
     d CheckSecLevel   pi              n
     d  inAppID                      10    const
     d  inType                        1    const options(*nopass)
     d  inLevel                       3  0 const options(*nopass)

     d outResult       s              1
     d userLevl        s              3  0
     d userType        s              1
     d userAllowed     s               n   inz(*on)
     d userNotAllowed  s               n   inz(*off)
     D keySDACL        s             10

      /Free

       keySDACL = inAppId;

       If %parms > 2 and inType <> *blank and inLevel <> 0;
          userLevl = inLevel;
          userType = inType;
       Else;
          userLevl = GetUsrLvl(Q1USER);
          userType = GetUsrType(Q1USER);
       Endif;

       Open sdacl;
       Chain keySDACL pac00;
       Close sdacl;

       If %found;
          CheckAuth(userType :
                    %editc(userLevl : 'X') :
                    ACstp1 :
                    %editc(ACslv1: 'X') :
                          '1' :
                          outResult :
                          ACstp2 :
                          ACstp3 :
                          ACstp4 :
                          ACstp5 :
                          ACstp6 :
                          %editc(ACslv2 : 'X') :
                          %editc(ACslv3 : 'X') :
                          %editc(ACslv4 : 'X') :
                          %editc(ACslv5 : 'X') :
                          %editc(ACslv6 : 'X') :
                          ACstp7 :
                          ACstp8 :
                          ACstp9 :
                          ACstp0 :
                          %editc(ACslv7 : 'X') :
                          %editc(ACslv8 : 'X') :
                          %editc(ACslv9 : 'X') :
                          %editc(ACslv0 : 'X') );
          If outResult = *on;
             Return userNotAllowed;
          Else;
             Return userAllowed;
          Endif;
       Else;
       //*** Beg Chg *** 08/21/24 *************************************
       //*** Beg Add *** 07/04/23 *************************************
       //***    If userLevl = 999 and usertype = '*';
       //***       Return userAllowed;
       //***    Else;
       //*** End Add *** 07/04/23 *************************************
       //***    Return userNotAllowed;
       //*** Beg Add *** 07/04/23 *************************************
       //***    Endif;
       //*** End Add *** 07/04/23 *************************************

       // Record not found in SDACP file

          If inAppId = 'GrpCfgAut';               // Group Configuration screens
             If userLevl = 999 and usertype = '*';
                Return userAllowed;
             Else;
                Return userNotAllowed;
             Endif;
          Endif;

          If inAppId <> 'GrpCfgAut';              // other screens
             Return userAllowed;
          Endif;
       //*** End Chg *** 08/21/24 *************************************
       Endif;

      /End-free

     p CheckSecLevel   e
       //*** Beg Add *** 05/09/22 ***************************************** ****
      *==================================================================
      * UserAccessWdw - Give users access to application config tabs
      *==================================================================
     p UserAccessWdw   b                   export
     d UserAccessWdw   pi
     d  inDspFileName                10    const
     d  inPgmName                    50    const
     d  inTabName                    50    const
     d*
     d  intCo#         s                   Like(TBco#)
       dcl-s xxDelResp ind;
       dcl-s strDelMessage char(256);
       dcl-s strUserName Like(TBUsrName);
     d*
     d* Mainline
       // Open files (User Controlled)
       If Not %Open(SDTBL1);
         Open SDTBL1;
       Endif;

       If Not %Open(SDTBP);
         Open SDTBP;
       Endif;

       If Not %Open(SPISECURE);
         Open SPISECURE;
       Endif;

       intCo# = UserCo#(Q1user);
       scCo# = intCo#;
       scFile = inDspFileName;
       scPgmName = inPgmName;
       scTabName = inTabName;

       DoU ExitNow = *On;

       // Load the User Access subfile
       Exsr LoadUserAccess;

       Readc sflUsrAcc;

       Select;
          //* Do nothing - Catch tab click event
          When tabClick=*On;
             tabClick = *Off;

          // Close processing.
          When btnClose = *on;
             ExitNow = *on;
             btnCLose = *off;

          // Delete processing.
          When delUser = *on;
             strDelMessage = 'Confirm delete of ' + %Trim(scUser) +
             ' from: ' + %Trim(inPgmName) + ' / ' + %Trim(inTabName);
             xxDelresp = PopTwoBtn(strDelMessage:'Delete User':'Delete':
              'Cancel');
             If xxDelresp = *on;
                Chain scTBrrn ptb02;
                If %Found(SDTBP);
                   Delete ptb02;
                Endif;
             Endif;
             delUser = *off;
       Endsl;
       Enddo;

       //=======================================================================
       // Close User Access files and return to the calling program
       //=======================================================================
          If %Open(SDTBL1);
             Close SDTBL1;
          Endif;

          If %Open(SDTBP);
             Close SDTBP;
          Endif;

          If %Open(SPISECURE);
             Close SPISECURE;
          Endif;
          Return;
       //=======================================================================
       // Load and display the User Access Grid
       //=======================================================================
       Begsr LoadUserAccess;

        // Clear the User Access subfile
        rrnUsrAcc = 0;
        clrUsrAcc = *On;
        dspUsrAcc = *Off;
        ctlUsrAcc = *Off;
        Write rcdUsrAcc;
        dspUsrAcc = *On;
        ctlUsrAcc = *On;
        clrUsrAcc = *Off;
        delUser = *off;
        disUser = *on;

        // Set the flexible heading for the grid
        strHeading = %Trim(inPgmName) + ' / ' + %Trim(inTabName);

        // Set the User choices selection criteria field

        UsrChoices = 'upstat <> ''I'' and upusr not in +
          (select TBuserprf from sdtbp where tbfile = ' + '''' +
          %Trim(scFile) + '''' + ' and tbpgmname = ' + '''' +
          %Trim(scPgmName) + '''' +  ' and + tbTabName = ' + '''' +
          %Trim(scTabname) + '''' + ')';

        // Load an extra record to the subfile for adding records
        indBlnkRcd = *on;
        isVisDel = *off;
        disUser = *off;
        scUser    = *blanks;
        scUserPrf    = *blanks;
        scTbRRN   = 0;
        rrnUsrAcc += 1;
        Write sflUsrAcc;
        isVisDel = *on;
        indBlnkRcd = *off;

        // Load records into User Access subfile
        Setll (intCo#:inDspFileName:inPgmName:inTabName) ptb01;
        Dou %Eof(SDTBL1);
           Reade (intCo#:inDspFileName:inPgmName:inTabName) ptb01;

           Select;
            When %Eof(SDTBL1);
               Iter;
           Endsl;

           scUser = TBusrName;
           scUserPrf = TBuserPrf;
           scTbRRN   = dbrrnTb;
           rrnUsrAcc = rrnUsrAcc + 1;
           Write sflUsrAcc;
        Enddo;

        // Set the flexible CSV file name for Excel Export
        ExpUsrAcc = %Trim(inTabName);

        // Display the User Access Grid
        Exfmt rcdUsrAcc;

       Endsr;

     p UserAccessWdw   e

       //*** End Add *** 05/09/22 ***************************************** ****
       //*** Beg Add *** 04/10/23 *** WC2430 ****************************** ****
      *==================================================================
      * Check User Authority
      *==================================================================
       Dcl-Proc ChkUsrAut Export;
       Dcl-Pi *N Ind;
         inAppID char(10) const;
       End-Pi;

       Dcl-S strAuth char(1);
       Dcl-S inUTYP char(1);
       Dcl-S inULVL char(3);

       Monitor;
       Close sdacl;
       On-Error;
       Endmon;

       //Retrieve authority for SDACL file
       Open sdacl;
       Clear pac00;
       Chain inAppID pac00;
       // Get user sec level/type
       inULVL = %Editc(GetUsrLvl(Q1user:inUTYP):'X');
       strAuth = '0';

       //****************************************************************
       // Security Check - Calls SRCCSC
       //****************************************************************
       CheckAuth(inUTYP:inULVL:ACstp1:%Editc(ACslv1:'X'):'1':strAuth
        :ACstp2:ACstp3:ACstp4:ACstp5:ACstp6:%Editc(ACslv2:'X')
        :%Editc(ACslv3:'X'):%Editc(ACslv4:'X'):%Editc(ACslv5:'X')
        :%Editc(ACslv6:'X'):ACstp7:ACstp8:ACstp9:ACstp0:%Editc(ACslv7:'X')
        :%Editc(ACslv8:'X'):%Editc(ACslv9:'X'):%Editc(ACslv0:'X'));

       //-----------------------------------------------------------
       // If result of strAuth = '0' then Security Check is "Passed"
       // Set return indicator = on
       //-----------------------------------------------------------
       If strAuth = '0';
          Return *on; // Authority passed

       //-----------------------------------------------------------
       // If result of strAuth = '1' then Security Check is "Not Passed"
       // Set return indicator = off
       //-----------------------------------------------------------
       Else;
          Return *off; // Authority not passed
       Endif;
       Close sdacl;
       End-Proc;
       //*** End Add *** 04/10/23 *** WC2430 ****************************** ****
