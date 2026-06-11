
     H DFTACTGRP(*NO) ACTGRP(*NEW)

     D PUIINITLIB      PR                  ExtPgm('PUIINITLIB')

     D PUIATEXAUT      PR                  ExtPgm('PUIATEXAUT')
     D  iUser                       128A   Const
     D  oUserName                   128A
     D  oError                         N
     D  oErrorMsg                   256A

     D PUIATEXAUT      PI
     D  iUser                       128A   Const
     D  oUserName                   128A
     D  oError                         N
     D  oErrorMsg                   256A

     D Status          S              1A

      /FREE

       PUIINITLIB();

       EXEC SQL DECLARE CURSOR SCROLL CURSOR FOR
          SELECT UPUSNM,UPSTAT FROM SMUPP WHERE UPUSR = :iUser;
       EXEC SQL OPEN CURSOR;
       EXEC SQL FETCH CURSOR INTO :oUserName,:Status;


       Select;
       When SQLCOD = 100;
          oUserName = '';
          oError = *On;
          oErrorMsg = 'User profile not found.';

       When SQLCOD < 0;
          oUserName = '';
          oError = *On;
          oErrorMsg = 'SQLCOD = ' + %Char(SQLCOD);

       Other;
          If Status = 'A';
            oError = *Off;
          Else;
            oUserName = '';
            oError = *On;
            oErrorMsg = 'User Not Active';
          EndIf;


       EndSL;
       EXEC SQL close CURSOR;

       *InLr = *On;
       Return;

      /END-FREE

