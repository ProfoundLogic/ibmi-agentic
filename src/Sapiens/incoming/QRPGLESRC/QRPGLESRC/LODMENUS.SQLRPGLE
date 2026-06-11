     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H*********************************************************************
     H/title LODMENUS - Load new menu options                             *
     H*********************************************************************
     H* SQL query to insert record in source PF:                          *
     H* From  To                                                          *
     H* 1   -  6      Release to add with                                 *
     H* 7   -  8      Application to add with                             *
     H* 9   -  11     Client ID - blank for all clients                   *
     H* 12  -  139    Menu text                                           *
     H* 140 -  651    Menu action                                         *
     H*                                                                   *
     H* Note: change release and client id as appropriate.                *
      *-------------------------------------------                        *
      * Insert into QLODMENUS (SRCDTA) values(                            *
      * 'V15R0 WC129#6a-Cove Risk Commission Check Process             '  *
      * '                                                              '  *
      * '               /profoundui/auth/start?pgm=PDSOBJ/STLAUNCH&p1=W'  *
      * '3029R    &l1=10&p2=LIV&l2=3 &p3=%28%27**COMMISSION+CHECK+PROCE'  *
      * 'SS**%27+%27W7421CFCC%27%29*End&l3=56'                            *
      *-------------------------------------------                        *
     H*                                                                   *
     H*  Created:  11/04/14    C:279   Project: 129WTQ                    *
     H*********************************************************************
     H*  Revisions:                                                       *
     H*                                                                   *
     H*  01/19/23 C:79  Support: 103038                                   *
     H*                 Correct duplicate logic so we add the library     *
     H*                 suffix                                            *
     H*                                                                   *
     H*  10/19/21 C:33  Support: 97176                                    *
     H*                 Update LIBL.                                      *
     H*                                                                   *
     H*  04/08/20 C:33  Project: SY419                                    *
     H*                 Rename 'new menu' folder                          *
     H*                                                                   *
     H*  02/23/18 C:33  Support: 64544                                    *
     H*                 Use 'S' version of company configuration          *
     H*                                                                   *
     H*  05/15/17 C:062 Support: 47008(45)                                *
     H*                 Use new menu listing                              *
     H*                                                                   *
     H*  07/09/15 C:079 Project: MC377                                    *
     H*                 Add logic to handle QLODMENUS being put into      *
     H*                 Post Load (PDSSRCxx)                              *
     H*                                                                   *
     H*  04/28/15 C:212 Project: MC377                                    *
     H*                 Added the menu option 'Import ICD-10 Codes'       *
     H*                 in QLODMENUS.                                     *
     H*                 Add suffix in library for data center clients     *
     H*                                                                   *
     H*  03/17/15 C:242 Project: 114WGU                                   *
     H*                 If Client is blank, install for all clients       *
     H*                                                                   *
     H*  01/14/15 C:282 Project: WC2334                                   *
     H*                 Added the menu option 'Work With Stat Code'       *
     H*                 in QLODMENUS.                                     *
     H*                                                                   *
     H*********************************************************************
     fqlodmenus if   e             disk    rename(qlodmenus:pmn00) UsrOpn

      * prototypes
     d CheckObj        pr                  extPgm('SRCHKOBJ')
     d outObject                     10    const
     d outObjectType                  7    const
     d inError                        1

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      *entry
     d LodMenus        pr                  extPgm('LODMENUS')
     d inApp                          2
     d inStrRls                       6
     d inEndRls                       6
     d inRlsPlus1                     1n
     d inSuffix                       3

     d LodMenus        pi
     d inApp                          2
     d inStrRls                       6
     d inEndRls                       6
     d inRlsPlus1                     1n
     d inSuffix                       3

      * procedure definitions
     d RunCmd          pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d CpyQLodMenus    pr                  extPgm('SA042C')
     d inSuffix                       3    const
     d inError                        1

     d                 ds
     d srcDta                      1988
     d Rls                            6    overlay(srcDta:1)
     d App                            2    overlay(srcDta:7)
     d CltId                          3    overlay(srcDta:9)
     d MnTxt                        128    overlay(srcDta:12)
     d MnActn                       512    overlay(srcDta:140)

     d strRls          ds             6
     d                                1    inz('V') overlay(strRls:1)
     d intRls                         2  0 overlay(strRls:2)
     d                                1    inz('R') overlay(strRls:4)
     d intRls1                        1  0 overlay(strRls:5)

     d strRlsa         ds             6
     d                                1    overlay(strRlsa:1)
     d intRlsa                        2  0 overlay(strRlsa:2)
     d                                1    overlay(strRlsa:4)
     d intRls1a                       1  0 overlay(strRlsa:5)

     d strRlsb         ds             6
     d                                1    overlay(strRlsb:1)
     d intRlsb                        2  0 overlay(strRlsb:2)
     d                                1    overlay(strRlsb:4)
     d intRls1b                       1  0 overlay(strRlsb:5)

      * work varibales
     d clientID        s              3    inz('000')
     d isCmpCfgUpd     s              1n   inz
     d isError         s              1n   inz
     d isObjExists     s              1n   inz
     d isprocess       s              1n   inz
     d isLibNtExt      s              1n   inz
     d isRecFnd        s              1n   inz
     d newNewMenu      s              1n   inz
     d oldNewMenu      s              1n   inz
     d mCount          s             10  0 inz
     d intItem         s             10  0 inz
     d intItem1        s             10  0 inz
     d intHldItm       s             10  0 inz
     d intSeq          s              9  0 inz
     d intCnt          s             10  0 inz
     d strHldLib       s             10    inz
     d strType         s              1    inz
     d xxMnActn1       s                   like(MnActn) inz
     d xxMnActn2       s                   like(MnActn) inz
     d xxPos           s              3  0 inz
     d inError         s              1    inz
     ddecRRN           s             10i 0 inz
     dstrFldr          s            128    varying inz
     dconFldr          c                   ' new menu items '
     dconFldrPrefix1   c                   'StoneRiver'
     dconFldrPrefix2   c                   'Software updated'
     dconFldrSuffix1   c                   '- !@#$%¢&*()'
     dconFldrSuffix2   c                   '- !@#$%&*()'

      /copy sPrPrprty
      /copy sPrString

      /free

       Callp CpyQLodMenus(inSuffix:inError);

       // get client ID
       GetClientID(clientID);

       // get instance library name
       strHldLib = SysProperty('InstanceName');

       isLibNtExt = *off;
       If strHldLib <> *blanks;
          // verify if library exist
          CheckObj(strHldLib:'*LIB':isLibNtExt);
       Endif;

       // check if QA env
       If isLibNtExt or strHldLib = ' ';
          isLibNtExt = *off;
          strHldLib = 'PF' + clientID + 'QA';

          // verify if library exist
          CheckObj(strHldLib:'*LIB':isLibNtExt);

          // check if PDN env
          If isLibNtExt;
             isLibNtExt = *off;
             strHldLib = 'PF' + clientID + 'PN';

             // verify if library exist
             CheckObj(strHldLib:'*LIB':isLibNtExt);

             If isLibNtExt;
                strHldLib = ' ';
             Endif;
          Endif;
       Endif;

       If strHldLib = ' ';
          Return;
       Endif;

       Monitor;
          RunCmd('RMVADDLIB LIB(' + %trim(strHldLib) +
             ') POSITION(*FIRST)':45);
          RunCmd('RMVADDLIB LIB(QTEMP) POSITION(*FIRST)':45);
       On-error;
       Endmon;

       Exsr CompanyConfig;

       // Check if High level menu already exist
       Exsr ChkHighLvl;

       If isRecFnd = *on;
          Exsr UpdHighLvl;
       Endif;
       If isRecFnd = *off;
          Exsr WrtHighLvl;
       Endif;
       intHldItm = intItem;

       Exsr GetRls;
       // write menu details
       Exsr WrtMenuDtl;

       Monitor;
          RunCmd('RMVLIBLE LIB(' + %trim(strHldLib) + ')':45);
       On-error;
       Endmon;

       eval *inlr = *on;

       //-------------------------------------------------------------------------
       // Insert new menu record
       //-------------------------------------------------------------------------
       Begsr AddNxtRec;

       Exec SQL Insert into ATNAVP (ANitem,ANseq,ANsect,ANparent,ANtype,
        ANactnType,ANactn,ANtext,ANicon,ANwindow,ANgurl,ANonce)
        values(:intItem,:intSeq,'N',:intHldItm,:strType,'0',:MNactn,:MNtxt,' ',
         '0',' ','0');

       Exec SQL Insert into ATAUTHP (AAitem,AAid,AAallow)
        values(:intItem,0,'0');

       Endsr;
       //-------------------------------------------------------------------------
       // Check High level record
       //-------------------------------------------------------------------------
       Begsr ChkHighLvl;

       isRecFnd = *off;

       // See if old new report menu exists
       //   // See if new report menu exists
       //
       //
       intItem = -1;
       // Check for original SR folder name
       strFldr = conFldrPrefix1 + conFldr + conFldrSuffix1;
       Exec sql
        select ANitem, rrn(ATNAVP) into :intItem, :decRRN
         from ATNAVP
         where ANtext = :strFldr;
       If intItem > 0;
          isRecFnd = *on;
          Leavesr;
       Endif;
       // Check for updated SR folder name
       strFldr = conFldrPrefix1 + conFldr + conFldrSuffix2;
       Exec sql
        select ANitem, rrn(ATNAVP) into :intItem, :decRRN
         from ATNAVP
         where ANtext = :strFldr;
       If intItem > 0;
          isRecFnd = *on;
          Leavesr;
       Endif;
       // Check for 'Software' folder name
       strFldr = conFldrPrefix2 + conFldr + conFldrSuffix2;
       Exec sql
        select ANitem, rrn(ATNAVP) into :intItem, :decRRN
         from ATNAVP
         where ANtext = :strFldr;
       If intItem > 0;
          isRecFnd = *on;
          Leavesr;
       Endif;

       Endsr;

       //-------------------------------------------------------------------------
       // Update company configuration records
       //-------------------------------------------------------------------------
       Begsr CompanyConfig;
        isError = *off;
        CheckObj('STCMPCFG':'*PGM':isError);
        isObjExists = not isError;
        If isObjExists;
           Exec Sql Update ATNAVP
            Set ANACTN = Replace(ANACTN, 'WTCMPCFG', 'STCMPCFG');
           isCmpCfgUpd = sqlCod = 0;
        Endif;
        If isCmpCfgUpd;
           Monitor;
              RunCmd('DLTOBJ OBJ(WTCMPCFG)   OBJTYPE(*PGM) ':40);
              RunCmd('DLTOBJ OBJ(WTCMPCFG)   OBJTYPE(*FILE)':40);
              RunCmd('DLTOBJ OBJ(WTCMPCFG01) OBJTYPE(*PGM) ':40);
              RunCmd('DLTOBJ OBJ(WTCMPCFG02) OBJTYPE(*PGM) ':40);
           On-error;
           Endmon;
        Endif;
       Endsr;

       //-------------------------------------------------------------------------
       // get next ANitem record
       //-------------------------------------------------------------------------
       Begsr GetHighLvl;

       Exec Sql Declare Cursor2 Scroll Cursor For
        Select AUINext from ATUIDSP where AUIField = 'ANITEM';

       // open cursor
       Exec SQL Open Cursor2;

       // read from cursor
       Exec SQL Fetch Cursor2 Into :intItem;

       // close cursor
       Exec SQL close Cursor2;

       Exec Sql update ATUIDSP set AUInext = (:intItem + 1)
        where AUIField = 'ANITEM';

       Endsr;
       //-------------------------------------------------------------------------
       // get next Sequence
       //-------------------------------------------------------------------------
       Begsr GetNxtSeq;

       intSeq = 0;
       Exec Sql Declare Cursor1 Scroll Cursor For
        Select Max(ANseq) from ATNAVP where ANsect = 'N' and ANparent =
        :intItem1;

       // open cursor
       Exec SQL Open Cursor1;

       // read from cursor
       Exec SQL Fetch Cursor1 Into :intSeq;

       intSeq = intSeq + 1;

       // close cursor
       Exec SQL close Cursor1;

       Endsr;
       //-------------------------------------------------------------------------
       // Get release
       //-------------------------------------------------------------------------
       Begsr GetRls;

       inStrRls = Upper(inStrRls);
       inEndRls = Upper(inEndRls);

       If inRlsPlus1 = *on;
          strRls = %trim(inStrRls);

          If intRls1 = 9;
             intRls1 = 0;
             intRls  = intRls + 1;
             inStrRls = strRls;
          Endif;
       Endif;
       clear strRls;
       strRlsa = inStrRls;
       strRlsb = inEndRls;

       Endsr;
       //-------------------------------------------------------------------------
       // Varify if application record already exist
       //-------------------------------------------------------------------------
       Begsr RecExist;

       Exec Sql Declare Cursor3 Scroll Cursor For
        Select ANitem from ATNAVP where ANsect = 'N' and ANparent =
        :intHldItm and ANactn = :MNactn and ANtext = :MNtxt;

       Exec SQL Open Cursor3;

       Exec SQL Fetch Cursor3 Into :intCnt;

       If SQLCOD >= 0 and SQLCOD <> 100 and intCnt > 0;
          isProcess = *off;
       Endif;

       Exec SQL close Cursor3;

       Endsr;
       //-------------------------------------------------------------------------
       // Update High level record
       //-------------------------------------------------------------------------
       Begsr UpdHighLvl;
        strFldr = conFldrPrefix2 + conFldr + conFldrSuffix2;
        Exec sql
         update ATNAVP
          set ANtext = :strFldr
          where rrn(ATNAVP) = :decRRN;
       Endsr;

       //-------------------------------------------------------------------------
       // Validate program
       //-------------------------------------------------------------------------
       Begsr ValidProgram;
        // By default, every program should be valid. Exception are defined here.
        Select;
        When %scan('WTCMPCFG':MnActn) <> 0;
           isProcess = *off;
        When %scan('STCMPCFG':MnActn) <> 0 and isCmpCfgUpd;
           isProcess = *off;
        Endsl;
       Endsr;

       //-------------------------------------------------------------------------
       // Validate release
       //-------------------------------------------------------------------------
       Begsr VldRls;

       strRls = Rls;

       If (intRlsa < intRls or (intRlsa = intRls and intRls1a <= intRls1)) and
        (intRlsb > intRls or (intRlsb = intRls and intRls1b >= intRls1));
          isProcess = *on;
       Endif;

       Endsr;
       //-------------------------------------------------------------------------
       // Write High level record
       //-------------------------------------------------------------------------
       Begsr WrtHighLvl;

       Exsr GetHighLvl;

       intItem1 = 0;
       Exsr GetNxtSeq;

       intHldItm = 0;
       strType   = '0';
       clear SrcDta;
       MNtxt = strFldr;
       Exsr AddNxtRec;

       Endsr;
       //-------------------------------------------------------------------------
       // Write Menu record
       //-------------------------------------------------------------------------
       Begsr WrtMenuDtl;

       Open QLODMENUS;

       Read pmn00;
       Dow not %eof;

          isProcess = *off;
          If App = inApp and
                   (CltId = clientID or CltId = '   ' or CltId = '000');
             Exsr VldRls;
             Exsr ValidProgram;
             //*** Beg Add *** 01/19/23 ************************************
             Exsr updDataCtr;
             //*** End Add *** 01/19/23 ************************************

             If isProcess = *on;

                Exsr GetHighLvl;

                intItem1 = intHldItm;
                Exsr GetNxtSeq;

                If intSeq > 1;
                   Exsr RecExist;

                   If isProcess = *off;
                      Read pmn00;
                      Iter;
                   Endif;
                Endif;

                strType = '1';
                // verify if data center client and update library if so
                //*** Beg Del *** 01/19/23 ************************************
                //***Exsr updDataCtr;
                //*** End Del *** 01/19/23 ************************************
                Exsr AddNxtRec;
             Endif;
          Endif;
          Read pmn00;
       Enddo;

       Close QLODMENUS;

       Endsr;

       //-------------------------------------------------------------------------
       // update for Date Center clients
       //-------------------------------------------------------------------------
       Begsr UpdDataCtr;

        // if its a Data Center client, suffix is passed in for its id

        If inSuffix <> *blanks;

           // replace library PDSOBJ with PDSOBJxxx where
           // xxx is client id and denotes the library for data center

           xxPos = %scan('PDSOBJ':MnActn);
           if xxPos > 0;

              // string before
              xxMnActn1 = %subst(MnActn:1:xxPos-1);

              // string after
              xxMnActn2 = %subst(MnActn:xxPos+6:513-(xxpos+6));

              // string before + PDSOBJxxx + string after
              MnActn = %trimr(xxMnActn1) + 'PDSOBJ' + inSuffix +
                       %trimr(xxMnActn2);

           endif;
        Endif;

       Endsr;

      /end-free
