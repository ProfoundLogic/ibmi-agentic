
     H DFTACTGRP(*NO) ACTGRP(*NEW)

     FATEXSCHP  IF A E             Disk
     F*                                    ExtFile('QTEMP/ATEXSCHP')


     D PUIINITLIB      PR                  ExtPgm('PUIINITLIB')

     D PUIATEXSCH      PR                  ExtPgm('PUIATEXSCH')
     D  iUser                       128A   Const
     D  iQuery                      128A   Const
     D  oError                         N
     D  oErrorMsg                   256A

     D PUIATEXSCH      PI
     D  iUser                       128A   Const
     D  iQuery                      128A   Const
     D  oError                         N
     D  oErrorMsg                   256A


     D UserAuthorized  PR              N




     D MMOPP         E DS                  ExtNAME(MMOPP)
     D SMUPP         E DS                  ExtNAME(SMUPP)

     D QueryStarts     S            128A   varying
     D QueryContains   S            128A   varying

     D MenuItem_OUT    DS                  qualified
     D OPMNAM                  1     10
     D OPOPTN                 11     14
     D MenuItem                1     14


      /FREE

        // Use 'starts with' and then 'contains' search using passed in query.
        // dump all results to file in qtemp
        // only show valid optios for given user



       PUIINITLIB();



           //Load User record
       EXEC SQL DECLARE CURSOR2 SCROLL CURSOR FOR
        SELECT UPLVL, UPTYPE FROM SMUPP WHERE UPUSR=:iUser AND UPSTAT='A';
       EXEC SQL OPEN CURSOR2;
       EXEC SQL FETCH CURSOR2 INTO :UPLVL, :UPTYPE;

       If SQLCOD >= 0 And SQLCOD <> 100;

       Else;
          oError = *ON;
          oErrorMsg='Invalid User ' + %CHAR(SQLCOD);

       EndIf;
       EXEC SQL close CURSOR2;

       If NOT oError;
            QueryStarts   = %Trim(iQuery) + '%';
            QueryContains = '%' + %Trim(iQuery) + '%';

          EXEC SQL DECLARE CURSOR SCROLL CURSOR FOR
             (SELECT OPMNAM, OPOPTN, OPMNTT
               FROM MMOPP
               WHERE   OPSTAT = 'A'
                   AND OPYNO <> 'Y'
                   AND OPMNTT LIKE :QueryStarts
                AND (( OPMYN='Y' AND  OPMMSC <= :UPLVL)
                  OR (OPMYN<>'Y' AND  OPMINS <= :UPLVL))
                ORDER BY OPMNTT)

             UNION

                (SELECT OPMNAM, OPOPTN, OPMNTT
               FROM MMOPP
               WHERE   OPSTAT = 'A'
                   AND OPYNO <> 'Y'
                   AND OPMNTT LIKE :QueryContains
                AND (( OPMYN='Y' AND  OPMMSC <= :UPLVL)
                  OR (OPMYN<>'Y' AND  OPMINS <= :UPLVL))
                 ORDER BY OPMNTT);


          EXEC SQL OPEN CURSOR;
          EXEC SQL FETCH CURSOR INTO :OPMNAM, :OPOPTN, :OPMNTT;


          Dow SQLCOD >= 0 And SQLCOD <> 100;

            If UserAuthorized();
            MenuItem_OUT.OPMNAM = OPMNAM;
            MenuItem_OUT.OPOPTN = OPOPTN;

            ATESCODE = MenuItem_OUT.MenuItem;
            ATESTEXT = OPMNTT;
            Write ATEXSCHR;
            EndIf;

            EXEC SQL FETCH CURSOR INTO :OPMNAM, :OPOPTN, :OPMNTT;

          EndDo;
          EXEC SQL close CURSOR;


       EndIf;



        *InLr = *On;

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
