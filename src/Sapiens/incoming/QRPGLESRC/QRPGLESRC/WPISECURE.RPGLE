     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
     H BNDDIR('WPISECURE')
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 08/28/18  C:33  Project: SY390                                                     *
     H*                 Allow user profile to be passed in                                 *
     H**************************************************************************************

     D/copy wPrSecure

     D/copy wPrClaims
     D/copy wPrGroup
     D/copy wPrEmplr
     D/copy wPrUsrPrms

     D psds          esds                  extname(szq1p)

     PInqUpdAccessAgt  b                   export
     P*====================================================================
     P* Return the Inquiry or Update call flag for the agent
     P*====================================================================
     DInqUpdAccessAgt  pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inAgt#                         5  0 const
     D***  Begin Add  ***  08/28/18  **************************************
     D inUser                        10    const options(*nopass)
     D***  End   Add  ***  08/28/18  **************************************

     DgrpAccess        s              1
     D***  Begin Add  ***  08/28/18  **************************************
     DstrUser          s             10
     D***  End   Add  ***  08/28/18  **************************************
      /Free
       //***  Begin Add  ***  08/28/18  ********************************
       strUser = q1User;
       If %parms >= 4 and inUser <> *blanks;
          strUser = inUser;
       Endif;
       //***  End   Add  ***  08/28/18  ********************************

       //***  Begin Chg  ***  08/28/18  ********************************
       //grpAccess = InqUpdAccessGrp(inCo#:inFnd);
       grpAccess = InqUpdAccessGrp(inCo#:inFnd:strUser);
       //***  End   Chg  ***  08/28/18  ********************************

       Select;
       When grpAccess = '*';
          // No access
          Return '*';
       Other;
          Return grpAccess;
       Endsl;

      /End-free
     PInqUpdAccessAgt  e

     PInqUpdAccessClm  b                   export
     P*====================================================================
     P* Return the Inquiry or Update call flag for the claim
     P*====================================================================
     DInqUpdAccessClm  pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const
     D inCase                         7  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D***  Begin Add  ***  08/28/18  **************************************
     D inUser                        10    const options(*nopass)
     D***  End   Add  ***  08/28/18  **************************************

     DgrpAccess        s              1
     DempAccess        s              1
     D***  Begin Add  ***  08/28/18  **************************************
     DstrUser          s             10
     D***  End   Add  ***  08/28/18  **************************************
      /Free
       //***  Begin Add  ***  08/28/18  ********************************
       strUser = q1User;
       If %parms >= 7 and inUser <> *blanks;
          strUser = inUser;
       Endif;
       //***  End   Add  ***  08/28/18  ********************************

       //***  Begin Chg  ***  08/28/18  ********************************
       //grpAccess = InqUpdAccessGrp(inCo#:inFnd);
       //empAccess = InqUpdAccessEmp(inCo#:inFnd:inEmp#:inDiv);
       grpAccess = InqUpdAccessGrp(inCo#:inFnd:strUser);
       empAccess = InqUpdAccessEmp(inCo#:inFnd:inEmp#:inDiv:strUser);
       //***  End   Chg  ***  08/28/18  ********************************

       Select;
       When grpAccess = '*' or empAccess = '*';
          // No access
          Return '*';
       When grpAccess = 'D' or empAccess = 'D';
          Return 'D';
       Other;
          Return 'C';
       Endsl;

      /End-free
     PInqUpdAccessClm  e

     PInqUpdAccessEmp  b                   export
     P*====================================================================
     P* Return the Inquiry or Update call flag for the employer
     P*====================================================================
     DInqUpdAccessEmp  pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D***  Begin Add  ***  08/28/18  **************************************
     D inUser                        10    const options(*nopass)
     D***  End   Add  ***  08/28/18  **************************************

     DgrpAccess        s              1
     DoutAccess        s              1
     DisAuthorized     s              1n
     D***  Begin Add  ***  08/28/18  **************************************
     DstrUser          s             10
     D***  End   Add  ***  08/28/18  **************************************
      /Free
       //***  Begin Add  ***  08/28/18  ********************************
       strUser = q1User;
       If %parms >= 5 and inUser <> *blanks;
          strUser = inUser;
       Endif;
       //***  End   Add  ***  08/28/18  ********************************

       //***  Begin Chg  ***  08/28/18  ********************************
       //grpAccess = InqUpdAccessGrp(inCo#:inFnd);
       //isAuthorized = EmpAuth(inCo#:inFnd:inEmp#:q1User:outAccess);
       grpAccess = InqUpdAccessGrp(inCo#:inFnd:strUser);
       isAuthorized = EmpAuth(inCo#:inFnd:inEmp#:strUser:outAccess);
       //***  Begin Chg  ***  08/28/18  ********************************

       Select;
       When grpAccess = '*';
          // No access
          Return '*';
       When not isAuthorized;
          // No access
          Return '*';
       When outAccess = 'I';
          Return 'D';
       When outAccess = 'A';
          Return 'C';
       Other;
          Return 'C';
       Endsl;

      /End-free
     PInqUpdAccessEmp  e

     PInqUpdAccessGrp  b                   export
     P*====================================================================
     P* Return the Inquiry or Update call flag for the group
     P*====================================================================
     DInqUpdAccessGrp  pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D***  Begin Add  ***  08/28/18  **************************************
     D inUser                        10    const options(*nopass)
     D***  End   Add  ***  08/28/18  **************************************

     DflgAuth          s              1
     DisAuthorized     s              1n   inz('1')
     D***  Begin Add  ***  08/28/18  **************************************
     DstrUser          s             10
     D***  End   Add  ***  08/28/18  **************************************
     DoutAccess        s              1    inz('A')

      /Free

       //***  Begin Chg  ***  08/28/18  ********************************
       //flgAuth = UsrRstFlg(q1User);
       strUser = q1User;
       If %parms >= 3 and inUser <> *blanks;
          strUser = inUser;
       Endif;
       flgAuth = UsrRstFlg(strUser);
       //***  End   Chg  ***  08/28/18  ********************************

       Select;
       When flgAuth = 'Y';
          // User parm group restriction
        //***  Begin Chg  ***  08/28/18  ********************************
        //isAuthorized = UserGrp(q1user) = inFnd;
          isAuthorized = UserGrp(strUser) = inFnd;
        //***  End   Chg  ***  08/28/18  ********************************
       When flgAuth = 'M' or flgAuth = 'S';
          // Group restriction records exists
        //***  Begin Chg  ***  08/28/18  ********************************
        //isAuthorized = GroupAuth(inCo#:inFnd:q1User:outAccess);
          isAuthorized = GroupAuth(inCo#:inFnd:strUser:outAccess);
        //***  End   Chg  ***  08/28/18  ********************************
       Endsl;

       Select;
       When not isAuthorized;
          // No access
          Return '*';
       When ExpiredGrp(inCo#:inFnd) = *on;
          Return 'D';
       When outAccess = 'I';
          Return 'D';
       When outAccess = 'A';
          Return 'C';
       Endsl;

       Return 'D';

      /End-free
     PInqUpdAccessGrp  e
