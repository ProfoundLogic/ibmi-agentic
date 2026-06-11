
     H DFTACTGRP(*NO) ACTGRP(*NEW)

      /INCLUDE QSYSINC/QRPGLESRC,QUSEC

     D PUIINITLIB      PR                  ExtPgm('PUIINITLIB')

     ** External Prototypes
     D QMHSNDPM        PR                  ExtPgm('QMHSNDPM')
     D  Id                            7A   const
     D  File                         20A   const
     D  MsgData                   32000A   const options(*varsize)
     D  MsgDataLen                   10I 0 const
     D  Type                         10A   const
     D  StackEntry                   10A   const
     D  StackCounter                 10I 0 const
     D  Key                           4A
     D  ErrorCode                          LIKEDS(QUSEC)

     **   (Fields for QMHSNDPM)
     D MsgId           S              7A
     D MsgFile         S             20A
     D MsgData         S          32000A
     D MsgDataLen      S             10I 0
     D MsgType         S             10A
     D MsgStackEntry   S             10A
     D MsgStackCountr  S             10I 0
     D MsgKey          S              4A
     D MsgErrorCode    DS                  LIKEDS(QUSEC)

     D IBMQCMDEXC      PR                  ExtPgm('QCMDEXC')
     D  Command                   32000A   Const Options(*VarSize)
     D  CommandLen                   15P 5 Const

     DDevSpecificTask  pr                  ExtPgm('PUIATEXDEV')
     D task                          20    const
     D rtnCode                        1    options(*nopass)

     DDataCtrSpecific  pr                  ExtPgm('PUIATEXDCT')
     D task                          20    const
     D rtnCode                        1    options(*nopass)

     ** Internal Prototypes
     DBreakLibl        pr           275
     D inLibl                       250    const

     D UserAuthorized  PR              N

     D PUIATEXINI      PR                  ExtPgm('PUIATEXINI')
     D  iItemCode                   128A   Const

     D PUIATEXINI      PI
     D  iItemCode                   128A   Const


     ** Data structures
     D MMOPP         E DS                  ExtNAME(MMOPP)
     D SMUPP         E DS                  ExtNAME(SMUPP)
     D Parent        E DS                  ExtNAME(MMOPP)  qualified

     D MenuItem_DS     DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D MenuItem                1     14

     D PSDS           SDS
     D  CurrentUser          358    367A

     D DataArea        DS                  qualified
     D M                       1      1A   inz('M')
     D OPAPP                   2      4A
     D XA                      5      6A   inz('XA')
     D Name                    1      6A


     ** Data Area
     D LiblDataArea    S            450    dtaara(*var:DataArea.Name)


      ** Global Fields
     D Cmd             S          32000A
     D CmdLen          S             15P 5 Inz(%Size(Cmd))

     D UserFound       S               N
     D MenuItemFound   S               N

      /FREE
        PUIINITLIB();

       MsgId = 'CPF9897';
       MsgFile = 'QCPFMSG   *LIBL     ';
       MsgType = '*ESCAPE';
       MsgStackCountr = 0;
       MsgStackEntry = '*';
       QUSBPRV = 0;



        //Load User record
       EXEC SQL DECLARE CURSOR2 SCROLL CURSOR FOR
        SELECT UPLVL, UPTYPE FROM SMUPP WHERE UPUSR=:CurrentUser AND UPSTAT='A';
       EXEC SQL OPEN CURSOR2;
       EXEC SQL FETCH CURSOR2 INTO :UPLVL, :UPTYPE;

       If SQLCOD >= 0 And SQLCOD <> 100;
          UserFound = *On;
       Else;
          UserFound = *Off;
          MsgData='Invalid User ' + %CHAR(SQLCOD);
          ExSr sendEscapeMessage;
       EndIf;
       EXEC SQL close CURSOR2;




       //Load Menu Item record
       MenuItem_DS.MenuItem = iItemCode;

       EXEC SQL DECLARE CURSOR SCROLL CURSOR FOR
        SELECT OPMNAM, OPOPTN, OPMYN, OPAPP,
              OPEXCL, OPYNO, OPMMSC, OPMINS,
              OPTY01, OPTY02, OPTY03, OPTY04,
              OPTY05, OPTY06, OPMNTT
           FROM MMOPP
           WHERE OPSTAT='A'
             AND OPYNO<>'Y'
             AND OPMYN<>'Y'
             AND OPMNAM = :MenuItem_DS.OPMNAM
             AND OPOPTN = :MenuItem_DS.OPOPTN;

       EXEC SQL OPEN CURSOR;
       EXEC SQL FETCH CURSOR INTO :OPMNAM, :OPOPTN, :OPMYN,  :OPAPP,
                                     :OPEXCL, :OPYNO,  :OPMMSC, :OPMINS,
                                     :OPTY01, :OPTY02, :OPTY03, :OPTY04,
                                     :OPTY05, :OPTY06, :OPMNTT;


       If SQLCOD >= 0 And SQLCOD <> 100;
          MenuItemFound = *On;
       Else;
          MenuItemFound = *Off;


          MsgData='Invalid Menu Item ' + %CHAR(SQLCOD) ;
          ExSr sendEscapeMessage;
       EndIf;
       EXEC SQL close CURSOR;





       // Validate User has Authority to this menu item
       If UPLVL >=OPMINS AND UserAuthorized();

       Else;

          MsgData='Insufficient User Authority';
          ExSr sendEscapeMessage;

       EndIf;


       //GET Parent Menu record to get library list from data area
       EXEC SQL DECLARE CURSOR3 SCROLL CURSOR FOR
        Select OPAPP
           FROM MMOPP
           WHERE OPSTAT = 'A'
             AND OPYNO <> 'Y'
             AND OPMYN = 'Y'
             AND OPOPTN <> ''
             AND substr(OPEXCL, 1, 10) = :MenuItem_DS.OPMNAM;

       EXEC SQL OPEN CURSOR3;
       EXEC SQL FETCH CURSOR3 INTO :Parent.OPAPP;

       If SQLCOD >= 0 And SQLCOD <> 100;


       else;
          MsgData='Unable to find Parent Menu Item ' + %Char(SQLCOD) + ', ' +
           %Trim(iItemCode) +' ,' +  MenuItem_DS.MenuItem + ' , ' +
           MenuItem_DS.OPMNAM;
          ExSr sendEscapeMessage;
       EndIf;

       EXEC SQL close CURSOR3;




       //Set the Library List
       DataArea.OPAPP = OPAPP; //sets DataArea.Name used to name the LiblDataArea data area
       Monitor;
          In LiblDataArea;
       On-Error;

          MsgData='Invalid Library List Data Area - ' + %Trim(OPAPP);
          ExSr sendEscapeMessage;

       EndMon;





       Cmd = 'CHGLIBL LIBL('
        + %Trim(BreakLibl(%Subst(LiblDataArea:1:250))) + ')';
       Monitor;

          IBMQCMDEXC(Cmd:Cmdlen);

       On-Error;

          MsgData='Invalid Library List in data area ' + DataArea.Name;

          ExSr sendEscapeMessage;
       EndMon;

       // Add the user specific CLTID library to LIBL
       Callp(e) DevSpecificTask('Add CLTID library');
       // Remove QSYSASP library from system LIBL
       Callp(e) DataCtrSpecific('Remove QSYSASP');
       // Add the DTM library to system LIBL
       Callp(e) DataCtrSpecific('Add DTM library');

        Cmd = 'CALL ' + %Trim(OPEXCL);

        Monitor;

          IBMQCMDEXC(Cmd:Cmdlen);

       On-Error;

          MsgData='Invalid Program';
          ExSr sendEscapeMessage;
       EndMon;


       *InLr = *On;
       Return;




       //==============================================================================
       BegSr SendEscapeMessage;
       MsgDataLen = %size(MsgData);
       QMHSNDPM(MsgId :MsgFile :
                  MsgData : MsgDataLen :MsgType  :
                  MsgStackEntry : MsgStackCountr : MsgKey :
                  MsgErrorCode);
       EndSr;
       //==============================================================================




      /END-FREE
     P*====================================================================
     P* Add spacing between LIBL elements
     P*====================================================================
     PBreakLibl        b
     DBreakLibl        PI           275
     D inLibl                       250    const

     D outLibl         s            275
     D f1              s              3  0
     D i1              s              3  0
     D o1              s              3  0

      /Free
       For f1 = 1 to 25;
          i1 = f1 * 10 - 9;
          o1 = f1 * 11 - 10;
          %subst(outLibl:o1:10) = %trim(%subst(inLibl:i1:10));
       Endfor;
       Return outLibl;
      /End-free
     PBreakLibl        e

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
