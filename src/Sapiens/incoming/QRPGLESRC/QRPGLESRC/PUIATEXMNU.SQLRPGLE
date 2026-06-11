
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE STUSRDRY - Work with User/team diary items
     h option(*nodebugio: *srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Atrium exit program to load Menus       *
     H*                                                               *
     H* CALLED BY: Atrium                                             *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED by: Profound     PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*   11/15/12  C:191   PROJECT: Transform                   *
     H*****************************************************************

     fmmnvl     if   e           k disk    rename(pnv01:pnv00)
     fmmusl     if   e           k disk    rename(pus01:pus00)
     fsmupl     if   e           k disk    rename(pup01:pup00)

     D/copy sprUsrPrms
     D PUIINITLIB      PR                  ExtPgm('PUIINITLIB')

     D GetHomePage     PR                  ExtPgm('PUIATEXHOM')
     D  userId                       10
     D  homePageCode                  9  0

     D UserAuthorized  PR              N

     D TitleCase       PR
     D   Text                       128A

     D psds          esds                  extname(szq1p)

     dAllowDebug       pr                  extPgm('STDBGMSG')
     d outReply                       1
      *


     D PUIATEXMNU      PR                  ExtPgm('PUIATEXMNU')
     D  iUser                       128A   Const
     D  iType                         1A   Const                                N,T,H Menu  Toolbar
     D  iParent                     128A   Const
     D  ioItem                             LikeDS(Item)
     D  oError                         N
     D  oErrorMsg                   256A

     D PUIATEXMNU      PI
     D  iUser                       128A   Const
     D  iType                         1A   Const
     D  iParent                     128A   Const
     D  ioItem                             LikeDS(Item)
     D  oError                         N
     D  oErrorMsg                   256A


     D Item            DS                  Qualified
     D  Code                        128A
     D  Parent                      128A                                        blank
     D  Type                          1A                                        1 = LEAF 0 = NONLEAF
     D  ActionType                    1A                                        2
     D  Action                      512A                                        lib/pgm
     D  Text                        128A
     D  Icon                        128A
     D  OpenInWindow                  1A
     D  OpenOnceOnly                  1A

     D MMOPP         E DS                  ExtNAME(MMOPP)
     D SMUPP         E DS                  ExtNAME(SMUPP)

     D MenuItem_DS     DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D NVitemc                15     23
     D MenuItem                1     23

     D PrevItem_DS     DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D NVitemc                15     23
     D PrevItem                1     23

     D OF              S              1    DIM(50) CTDATA PERRCD(1)             OPTIONS,FUNCTIONS
     D OFD             S             35    DIM(50) ALT(OF)

     D SQL             S           2048A
     D SQLand          S            128A   Varying

     D UserFound       S               N
     D ChildFound      S               N
     D ParentMenuName  S                   like(OPMNAM)
     D CurrMenuName    S                   like(OPMNAM)
     D PrevTBItem      S                   like(NVitem)
     D homePageCode    S                   like(NVitem)
     d allowed         s               n   inz
     d userid          s                   like(upusr)
     d flgRtnCd        s              1
     d default         s             10    inz('*DEFAULT')

      /FREE

       PUIINITLIB();
       Select;
       When iType = 'N';
          Exsr ProcessN;
       When iType = 'H';
         //If q1user = 'FG02RK';
         //AllowDebug(flgRtnCd);
         //Endif;

          Exsr ProcessH;
       When iType = 'T';
          Exsr ProcessT;
       Other;
          Clear ioItem;
       Endsl;

       *InLr = *On;
       Return;
       //*********************************************************************************
       //* Process Toolbar options
       //*********************************************************************************
       Begsr ProcessT;
          If ioItem.code = ' ';
            //First request for toolbar
            ChildFound = *On;
            PrevTBItem = *loval;
          Else;
            PrevItem_DS.PrevItem = ioItem.Code;
            Monitor;
              PrevTBItem = %Int(PrevItem_DS.NVitemc);
            On-Error;
              PrevTBItem = 0;
              ChildFound = *Off;
              oError    = *On;
              oErrorMsg = 'Invalid previous item code';
              LeaveSr;
            EndMon;

            // Check if previous item exist
            Chain(n) (iType:PrevTBItem) pnv00;
            If %Found(mmnvl);
              ChildFound = *On;
            Else;
              ChildFound = *Off;
              oError    = *On;
              oErrorMsg = 'Previous item not Valid: ' + ioItem.ActionType;
              LeaveSr;
            Endif;
          Endif;

         //Load User record
          userid = iUser;
          Chain(n) userid pup00;
          If %Found(smupl);
            UserFound = *On;
          Else;
            UserFound = *Off;
            oError    = *On;
            oErrorMsg = 'User not Valid';
            LeaveSr;
          EndIf;

         //Load Toolbar Item
         If UserFound AND ChildFound;
           Setgt (iType:PrevTBitem) pnv00;
           DoU %Eof(mmnvl);
             Reade (iType) pnv00;
             If %Eof(mmnvl);
               //Process end of file
               Clear ioItem;
               Leave;
             Endif;

             //Check Allow file for additional authority
             allowed = *on;
             Chain(n) (NVitem:userid) pus00;
             If %Found(mmusl);
               If USallow ='0';
                 allowed = *Off;
               Endif;
             Else;
               //Check Group level
               Chain(n) (NVitem:UPugrp) pus00;
               If %Found(mmusl);
                 If USallow = '0';
                   allowed = *Off;
                 Endif;
               Else;
                 //Check default level
                 Chain(n) (NVitem:default) pus00;
                 If %Found(mmusl) And USallow = '0';
                   allowed = *Off;
                 Endif;
               Endif;
             EndIf;

             If allowed;
               ExSr LoadTBDS;
               Leave;
             EndIf;
           Enddo;
         EndIf;
       Endsr;
       //*********************************************************************************
       Begsr ProcessH;
          Select;
          When ioItem.code = '';
          //if this is the first request, the data structure will be empty.  Load the Home menu item
          //Load User record
          userid = iUser;
          GetHomePage(userId:homePageCode);
          If homePageCode = 0;
            Clear ioItem;
            LeaveSr;
          Else;
            PrevTBitem = homePageCode;
          Endif;

          Chain(n) (iType:PrevTBItem) pnv00;
          If %Found(mmnvl);
            MenuItem_DS.OPMNAM = NVmnam;
            MenuItem_DS.OPOPTN = NVoptn;
            MenuItem_DS.NVitemc = *Blanks;

            ioItem.Code         = MenuItem_DS.MenuItem;
            ioItem.Parent       = '';

            ioItem.Type         = '1';
            ioItem.ActionType   = NVactntype;
            ioItem.Action       = NVactn;
            ioItem.Text         = NVtext;
            ioItem.Icon         = NVicon;
            ioItem.OpenInWindow = NVwindow;
            ioItem.OpenOnceOnly = NVonce;
          Else;
            Clear ioItem;
          EndIf;
          Other;
            //if the data structure contains data, this is the second request, just return
            //an empty data structure to indicate there are no more home menu items
            Clear ioItem;
          endsl;

       Endsr;
       //*********************************************************************************
       Begsr ProcessN;

       If iParent <> '';
          MenuItem_DS.MenuItem = iParent;
          ParentMenuName =  MenuItem_DS.OPMNAM;
          EXEC SQL DECLARE CURSOR3 SCROLL CURSOR FOR
            Select OPEXCL FROM MMOPP
               WHERE OPMNAM = :MenuItem_DS.OPMNAM
               AND OPOPTN = :MenuItem_DS.OPOPTN;
           EXEC SQL OPEN CURSOR3;
           EXEC SQL FETCH CURSOR3 INTO :OPEXCL;
           If SQLCOD >= 0 And SQLCOD <> 100;
              CurrMenuName = %SubSt(OPEXCL:1:10);
              ChildFound = *On;
           Else;
              ChildFound = *Off;
              oError    = *On;
              oErrorMsg = 'Parent not Valid:    (SQLCOD=' + %char(SQLCOD) + ')';
           EndIf;
           EXEC SQL close CURSOR3;
       Else;
          ChildFound = *On;
          CurrMenuName = *blanks;
       EndIf;

        //Check for the previously returned item
        If ioItem.Code <> '';
           PrevItem_DS.PrevItem = ioItem.Code;
           SQLand = ' AND OPOPTN>'''+  %Trim(PrevItem_DS.OPOPTN) + ''' ';
        Else;
           SQLand = ' ';
        EndIf;

       //Load User record
       EXEC SQL DECLARE CURSOR2 SCROLL CURSOR FOR
         SELECT UPLVL, UPTYPE, UPIPGM FROM SMUPP WHERE UPUSR=:iUser AND
         UPSTAT='A';
       EXEC SQL OPEN CURSOR2;
       EXEC SQL FETCH CURSOR2 INTO :UPLVL, :UPTYPE, :UPIPGM;

       If SQLCOD >= 0 And SQLCOD <> 100;
          UserFound = *On;
       Else;
          UserFound = *Off;
          oError    = *On;
          oErrorMsg = 'User not Valid:    (SQLCOD=' + %char(SQLCOD) + ')';
       EndIf;
       EXEC SQL close CURSOR2;


       //Load Menu Item (if SQLand contains anything, then load NEXT Item.)
       if UserFound AND ChildFound;
         If CurrMenuName = *Blanks;
           CurrMenuName = UPIPGM;
         Endif;
         If CurrMenuName = *Blanks;
           CurrMenuName = 'MAIN';
         Endif;
       SQL='SELECT OPMNAM, OPOPTN, OPMYN, OPAPP, ' +
                  'OPEXCL, OPYNO, OPMMSC, OPMINS, ' +
                  'OPTY01, OPTY02, OPTY03, OPTY04, ' +
                  'OPTY05, OPTY06, OPMNTT ' +
           'FROM MMOPP ' +
           'WHERE OPSTAT=''A'' ' +
             'AND OPYNO<>''Y'' ' +
             'AND OPMNAM=''' + %Trim(CurrMenuName) + ''' ' +
             'AND OPOPTN <>'''' ' +
             'AND (( OPMYN=''Y'' AND  OPMMSC<=''' + %CHAR(UPLVL) + ''') ' +
               'OR (OPMYN<>''Y'' AND  OPMINS<=''' + %Char(UPLVL) + ''')) ' +
           SQLand + ' ' +
           'ORDER BY OPOPTN';


          EXEC SQL PREPARE Dyn_Select FROM :SQL;
          EXEC SQL DECLARE cursor SCROLL CURSOR FOR Dyn_Select;

          EXEC SQL Open cursor;
          EXEC SQL FETCH CURSOR INTO :OPMNAM, :OPOPTN, :OPMYN,  :OPAPP,
                                     :OPEXCL, :OPYNO,  :OPMMSC, :OPMINS,
                                     :OPTY01, :OPTY02, :OPTY03, :OPTY04,
                                     :OPTY05, :OPTY06, :OPMNTT;


         Dow SQLCOD >= 0 And SQLCOD <> 100;

         //Find First Authorized Menu Option, process and end
         If ParentMenuName <> %SubSt(OPEXCL:1:10) AND UserAuthorized();
           ExSr LoadDataStructure;
           Leave;
         EndIf;


          EXEC SQL FETCH CURSOR INTO :OPMNAM, :OPOPTN, :OPMYN,  :OPAPP,
                                     :OPEXCL, :OPYNO,  :OPMMSC, :OPMINS,
                                     :OPTY01, :OPTY02, :OPTY03, :OPTY04,
                                     :OPTY05, :OPTY06, :OPMNTT;


         EndDo;
         If SQLCOD = 100;
           Clear ioItem;
         EndIf;

         If SQLCOD < 0;
             oError    = *On;
             oErrorMsg = 'SQLCOD=' + %Char(SQLCOD);
         EndIf;

        EndIf;

       Endsr;

       //*********************************************************************************
       BegSr LoadDataStructure;

        MenuItem_DS.OPMNAM = OPMNAM;
        MenuItem_DS.OPOPTN = OPOPTN;
        MenuItem_DS.NVitemc= *blanks;

        ioItem.Code         = MenuItem_DS.MenuItem;
        ioItem.Parent       = iParent;
        If OPMYN='Y';
          ioItem.Type         = '0'; //NON Lead Node (a menu)
          ioItem.Action       = '';
          ioItem.ActionType   = '';
          ioItem.OpenInWindow = '';
        ioItem.OpenOnceOnly = '';
        Else;
          ioItem.Type         = '1'; //LEAF node (a prgram)
          ioItem.Action       = %SubSt(OPEXCL:1:10);
          ioItem.ActionType   = '2';
          ioItem.OpenInWindow = '0';
          ioItem.OpenOnceOnly = '0';
        EndIf;

        ioItem.Text         = OPMNTT;
        //TitleCase(ioItem.Text);    // Removed as part of Capitaliztion fix
        ioItem.Icon         = '';

       EndSr;

       //*********************************************************************************
       BegSr LoadTBDS;
        If NVactntype = '2';
          MenuItem_DS.OPMNAM = NVmnam;
          MenuItem_DS.OPOPTN = NVoptn;
          MenuItem_DS.NVitemc = %Editc(NVitem:'X');
          ioItem.Code  = MenuItem_DS.MenuItem;
        Else;
          MenuItem_DS.OPMNAM = *Blanks;
          MenuItem_DS.OPOPTN = *Blanks;
          MenuItem_DS.NVitemc = %Editc(NVitem:'X');
          ioItem.Code  = MenuItem_DS.MenuItem;
        Endif;
        ioItem.Parent       = %Editc(NVparent:'X');
        ioItem.Type         = NVtype;
        ioItem.Action       = NVactn;
        ioItem.ActionType   = NVactntype;
        ioItem.OpenInWindow = NVwindow;
        ioItem.OpenOnceOnly = NVonce;
        ioItem.Text         = NVtext;
        ioItem.Icon         = NVicon;
       EndSr;
      /END-FREE

      //This procedure should be replaced with call to API SRCCSC
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
        If (OPTY01 >= Low and OPTY01 <= High) or UPTYPE=OPTY01 or
           (OPTY02 >= Low and OPTY02 <= High) or UPTYPE=OPTY02 or
           (OPTY03 >= Low and OPTY03 <= High) or UPTYPE=OPTY03 or
           (OPTY04 >= Low and OPTY04 <= High) or UPTYPE=OPTY04 or
           (OPTY05 >= Low and OPTY05 <= High) or UPTYPE=OPTY05 or
           (OPTY06 >= Low and OPTY06 <= High) or UPTYPE=OPTY06;

               IsAuthorized =*On;
        EndIf;
       EndSr;

      /End-Free

     P UserAuthorized  E
