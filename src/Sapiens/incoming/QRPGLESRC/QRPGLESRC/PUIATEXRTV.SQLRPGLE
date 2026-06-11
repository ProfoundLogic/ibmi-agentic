
     H DFTACTGRP(*NO) ACTGRP(*NEW)


     D UserAuthorized  PR              N

     D PUIATEXRTV      PR                  ExtPgm('PUIATEXRTV')
     D  iUser                       128A   Const
     D  iItemCode                   128A   Const
     D  oItem                              LikeDS(Item)
     D  oError                         N
     D  oErrorMsg                   256A

     D PUIATEXRTV      PI
     D  iUser                       128A   Const
     D  iItemCode                   128A   Const
     D  oItem                              LikeDS(Item)
     D  oError                         N
     D  oErrorMsg                   256A


     D PUIINITLIB      PR                  ExtPgm('PUIINITLIB')


     D Item            DS                  Qualified
     D  Code                        128A
     D  Parent                      128A
     D  Type                          1A
     D  ActionType                    1A
     D  Action                      512A
     D  Text                        128A
     D  Icon                        128A
     D  OpenInWindow                  1A
     D  OpenOnceOnly                  1A


     D MenuItem_IN     DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D MenuItem                1     14

     D MenuItem_OUT    DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D MenuItem                1     14

     D MenuItem_Paren  DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D MenuItem                1     14

     D MMOPP         E DS                  ExtNAME(MMOPP)
     D SMUPP         E DS                  ExtNAME(SMUPP)


     D SQL             S           2048A


      /FREE


       PUIINITLIB();


       Clear oItem;

       MenuItem_IN.MenuItem = iItemCode;


       //Load User record
       EXEC SQL DECLARE CURSOR2 SCROLL CURSOR FOR
          SELECT UPLVL, UPTYPE FROM SMUPP WHERE UPUSR=:iUser AND UPSTAT='A';
       EXEC SQL OPEN CURSOR2;
       EXEC SQL FETCH CURSOR2 INTO :UPLVL, :UPTYPE;

       If SQLCOD >= 0 And SQLCOD <> 100;

       Else;
          oError    = *On;
          oErrorMsg = 'User not Valid:    (SQLCOD=' + %char(SQLCOD) + ')';
       EndIf;
       EXEC SQL close CURSOR2;


       //Load Menu Item
       If NOT oError;
       SQL='SELECT OPMNAM, OPOPTN, OPMYN, OPAPP, ' +
                  'OPEXCL, OPYNO, OPMMSC, OPMINS, ' +
                  'OPTY01, OPTY02, OPTY03, OPTY04, ' +
                  'OPTY05, OPTY06, OPMNTT ' +
           'FROM MMOPP ' +
           'WHERE OPSTAT=''A'' ' +
             'AND OPYNO<>''Y'' ' +
             'AND OPMNAM=''' + %Trim(MenuItem_IN.OPMNAM) + ''' ' +
             'AND OPOPTN ='''+ %Trim(MenuItem_IN.OPOPTN) + ''' ' +
             'AND (( OPMYN=''Y'' AND  OPMMSC<=''' + %CHAR(UPLVL) + ''') ' +
               'OR (OPMYN<>''Y'' AND  OPMINS<=''' + %Char(UPLVL) + ''')) ' +
           'ORDER BY OPOPTN';


          EXEC SQL PREPARE Dyn_Select FROM :SQL;
          EXEC SQL DECLARE cursor SCROLL CURSOR FOR Dyn_Select;

          EXEC SQL Open cursor;
          EXEC SQL FETCH CURSOR INTO :OPMNAM, :OPOPTN, :OPMYN,  :OPAPP,
                                     :OPEXCL, :OPYNO,  :OPMMSC, :OPMINS,
                                     :OPTY01, :OPTY02, :OPTY03, :OPTY04,
                                     :OPTY05, :OPTY06, :OPMNTT;


          If SQLCOD >= 0 And SQLCOD <> 100;

            If  UserAuthorized();
               ExSr LoadDataStructure;
            Else;
               oError    = *On;
               oErrorMsg = 'User not Valid does not have authority to ' +
                           'this Menu Item';
         EndIf;

          Else;
          oError    = *On;
          oErrorMsg = 'Item  not Valid:    (SQLCOD=' + %char(SQLCOD) + ')';
          EndIf;

        EndIf;


       //Get the Parent menu item
       If NOT oError;

            EXEC SQL DECLARE CURSOR3 SCROLL CURSOR FOR
            Select OPMNAM, OPOPTN FROM MMOPP
               WHERE substr(OPEXCL, 1, 10) = :MenuItem_OUT.OPMNAM
               AND OPOPTN <> '';
           EXEC SQL OPEN CURSOR3;
           EXEC SQL FETCH CURSOR3 INTO :OPMNAM, :OPOPTN;
           If SQLCOD >= 0 And SQLCOD <> 100;
               //Load Parent into oItem data structure
               MenuItem_Paren.OPMNAM = OPMNAM;
               MenuItem_Paren.OPOPTN = OPOPTN;
               oItem.Parent=MenuItem_Paren.MenuItem;
           Else;
              oError    = *On;
              oErrorMsg = 'Parent not Valid:    (SQLCOD=' + %char(SQLCOD) + ')';
           EndIf;
           EXEC SQL close CURSOR3;

       EndIf;

        If oError;
          Clear oItem;
        EndIf;


        *InLr = *On;
        Return;




        BegSr LoadDataStructure;
        //(Item Parent is no yet know.  Another SQL query will get it next)

        MenuItem_OUT.OPMNAM = OPMNAM;
        MenuItem_OUT.OPOPTN = OPOPTN;

        oItem.Code         = MenuItem_OUT.MenuItem;
        oItem.Parent       = ''; // This is set later in this program
        If OPMYN='Y';
          oItem.Type         = '0'; //NON Lead Node (a menu)
          oItem.Action       = '';
          oItem.ActionType   = '';
          oItem.OpenInWindow = '';
        oItem.OpenOnceOnly = '';
        Else;
          oItem.Type         = '1'; //LEAF node (a prgram)
          oItem.Action       = %SubSt(OPEXCL:1:10);
          oItem.ActionType   = '2';
          oItem.OpenInWindow = '0';
          oItem.OpenOnceOnly = '0';
        EndIf;

        oItem.Text         = OPMNTT;
        oItem.Icon         = '';

       EndSr;

      /END-FREE

     P UserAuthorized  B
     D UserAuthorized  PI              N

     D IsAuthorized    S               N
     D Low             S              1A
     D High            S              1A


      /Free
        IsAuthorized =*Off;

       Select;
       When UPTYPE = '*';
          IsAuthorized =*On;

       When UPTYPE = '#';
            Low  = 'A';
            High = 'M';
            ExSr CheckRange;

       When UPTYPE = '$';
            Low  = 'N';
            High = 'Z';
            ExSr CheckRange;

       When UPTYPE = '&';
            Low  = 'A';
            High = 'Z';
            Exsr CheckRange;

       When UPTYPE = '%';
            Low  = '0';
            High = '9';
            Exsr CheckRange;

       Other;
            if UPTYPE=OPTY01 or
               UPTYPE=OPTY02 or
               UPTYPE=OPTY03 or
               UPTYPE=OPTY04 or
               UPTYPE=OPTY05 or
               UPTYPE=OPTY06;
                  IsAuthorized =*On;
            EndIf;


       EndSL;

       Return IsAuthorized;


       BegSr CheckRange;
        If (OPTY01 >= Low and OPTY01 <= High) or
           (OPTY02 >= Low and OPTY02 <= High) or
           (OPTY03 >= Low and OPTY03 <= High) or
           (OPTY04 >= Low and OPTY04 <= High) or
           (OPTY05 >= Low and OPTY05 <= High) or
           (OPTY06 >= Low and OPTY06 <= High);

               IsAuthorized =*On;
        EndIf;
       EndSr;

      /End-Free

     P UserAuthorized  E
