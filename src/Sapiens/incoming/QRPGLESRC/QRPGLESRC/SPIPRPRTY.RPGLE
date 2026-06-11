     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
    H*********************************************************************
    H*  Created: 10/31/12  C:33  Project: Transform
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*  07/15/13  C:65  Project:  Redmine #3415                          *
    H*                  Do not lock records in GetProperty procedure.    *
    H*                                                                   *
    H*  05/13/13  C:33  Add AddProperty                                  *
    H*********************************************************************
     Fstprp     uf a e           k disk     rename(ppr01:ppr0p)

     D/copy sPrAtrium
     D/copy sPrGenPop
     D/copy sPrPrprty

     DGetProperty      pr             1n
     D inKey                               like(PRkey) const
     D inFailNotify                   1n   const options(*nopass)
     D inPropertyArea                50    const options(*nopass)

     DSrChkPgm         pr                  extPgm('SRCHKPGM')
     D outPgmName                    10    const
     D indAjax                        1n

     DdsSTPRP          ds                  likeRec(ppr0p)

     P*=====================================================================
     P* AddProperty - Add a system property value
     P*   Parm 1 - property key
     P*   Parm 2 - property value
     P*   Parm 3 - overwrite current value, if exists {optional}
     P*   Returns = *ON-Added (or updated); *OFF-not added
     P*=====================================================================
     PAddProperty      b                   export
     DAddProperty      pi             1n
     D inKey                         25
     D inProperty                   256
     D inOverwrite                    1n   options(*nopass)

     DoverWrite        s              1n   inz

      /Free
       If %parms >= 3;
          overWrite = inOverWrite;
       Endif;

       Chain inKey stprp;
       PRkey = inKey;
       PRproperty = inProperty;

       Select;
       When %found and not overWrite;
          Unlock stprp;
          Return *off;
       When %found;
          Update ppr0p;
          Return *on;
       Other;
          Write ppr0p;
          Return *on;
       Endsl;

       Return *off;
      /End-free
     PAddProperty      e

     P*=====================================================================
     P* GetProperty - Retrieves the property value
     P*   Parm 1 - property key
     P*   Parm 2 - failure notification {optional}
     P*   Parm 3 - property area        {optional}
     P*   Returns = *on, if found; otherwise *off
     P*=====================================================================
     PGetProperty      b
     DGetProperty      pi             1n
     D inKey                               like(PRkey) const
     D inFailNotify                   1n   const options(*nopass)
     D inPropertyArea                50    const options(*nopass)

     DstrMessage       s            256
     DstrPropertyArea  s             50    inz
     DstrTitle         s             50    inz('System Properties Error')
     DisAjax           s              1n
     DisFailNotify     s              1n   inz

      /Free
       If %parms >= 2;
          isFailNotify = inFailNotify;
       Endif;
       If %parms >= 3 and inPropertyArea <> *blank;
          strPropertyArea = inPropertyArea;
       Endif;
       If strPropertyArea <> *blank;
          strTitle = strPropertyArea;
       Endif;
       Callp SrChkPgm('STPROPERTY':isAjax);

       Clear dsSTPRP;
       //*** begin chg ***  07/15/13  ********************************************
       //*****Chain inKey stprp dsSTPRP;
       Chain(n) inKey stprp dsSTPRP;
       //*** begin chg ***  07/15/13  ********************************************

       If not %found and isFailNotify;
          Select;
          When AtriumJob;
             strMessage = 'Your system is not properly configured';
             if strPropertyArea <> *blank;
                strMessage = %trim(strMessage) + ' for ' + strPropertyArea;
             Endif;
             strMessage = %trim(strMessage) + ' (' + %trim(inKey) + ').' +
              '  Please contact your system administrator.';
             Callp PopOneBtn(strMessage:strTitle:'Ok');
          When isAjax;
          // Send program message?;
          Other;
          // Send program message?;
          Endsl;
       Endif;

       Return %found(stprp);

      /End-free
     PGetProperty      e

     P*=====================================================================
     P* IAccessURL - Retrieve the URL address for iAccess
     P*   Returns = Character string with address
     P*=====================================================================
     PIAccessURL       b                   Export
     DIAccessURL       pi           256

      /Free
       Callp GetProperty('iAccessURL':*on:'iAccess');

       Return dsSTPRP.PRproperty;

      /End-free
     PIAccessURL       e

     P*=====================================================================
     P* SysProperty - Retrieve the system property values
     P*   Parm 1 - property key
     P*   Parm 2 - failure notification {optional}
     P*   Returns = property value
     P*=====================================================================
     PSysProperty      b                   Export
     DSysProperty      pi           256
     D inKey                         25    const
     D inFailNotify                   1n   const options(*nopass)

     DisFailNotify     s              1n   inz

      /Free
       If %parms >= 2;
          isFailNotify = inFailNotify;
       Endif;

       Callp GetProperty(inKey:isFailNotify);

       Return dsSTPRP.PRproperty;

      /End-free
     PSysProperty      e
