     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('QC2LE':'SPISQL')
     H nomain
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*********************************************************************

     DQSQCHKS          pr                  extpgm('QSQCHKS')
     D inStmt                     32767a   const options(*varsize)
     D inStmtLen                     10i 0 const
     D in#ofRecProv                  10i 0 const
     D inLanguage                    10a   const
     D inOptions                  32767a   const options(*varsize)
     D outStmtInfo                32767a         options(*varsize)
     D inStmtInfoLen                 10i 0 const
     D out#ofRecProc                 10i 0
     D outErrorCode                        like(QUSEC)

     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     DQUSEC            DS           116    inz
     D QUSBPRV                 1      4B 0                                      Bytes Provided
     D QUSBAVL                 5      8B 0                                      Bytes Available
     D QUSEI                   9     15                                         Exception Id
     D QUSERVED               16     16                                         Reserved
     D QUSMSGDTA              17    116                                         Message Data

     D* parameters passed to CEEDOD
     DdsOperDesc       ds                  qualified
     D descType                      10I 0
     D dataType                      10I 0
     D descInfo1                     10I 0
     D descInfo2                     10I 0
     D inLen                         10I 0
     D hexLen                        10I 0
     D*
     D/copy sprmessage
     D/copy sprstring
     D/copy sprsql

     P*=====================================================================
     P* IsValidFetch - Determines if a fetch has occurred
     P*  Parameter 1 - SQL State
     P*  Returns - *on if fetch is valid, otherwise *off
     P*=====================================================================
     PIsValidFetch     b                   export
     DIsValidFetch     pi             1n
     D inSqlState                     5a   const

     D fetchOK         s               n   Inz(*Off)

      /Free

       Select;
       When %Subst(inSQlState:1:2) = '00' Or
            %Subst(inSQlState:1:2) = '01';
          fetchOK = *On;
       EndSl;

       Return fetchOK;

      /End-free
     PIsValidFetch     e

     P*=====================================================================
     P* Validate the syntax of an SQL statement using
     P*  --- Syntax Check SQL Statement (QSQCHKS) API
     P*  Parameter 1 - SQL statement
     P*  Parameter 2 - naming convention (*NONE/*SYS/*SQL)
     P*  Parameter 3 - language (*NONE/*RPG/*RPGLE/*RPGLEFREE)
     P*  Returns - *on if syntax is valid, otherwise *off
     P*=====================================================================
     PValidSQLSyntax   b                   export
     DValidSQLSyntax   pi             1n   opdesc
     D inSqlStmt                  32767a   const options(*varsize)
     D inNaming                       5    const options(*nopass)
     D inLang                        10    const options(*nopass)

     DdsStmt           DS                  likeds(myStmt) inz
     DmyStmt           DS                  inz
     D len                            4b 0
     D numRecProv                     4b 0 inz(1)
     D lang                          10A   inz('*NONE')
     D inSize                         4b 0 inz(%size(dsStmtInfo))
     D numRecProc                    10i 0

     DptrOptions       s               *
     D#ofOpt           c                   2
     DintOffset        s              5  0  inz(18)
     DdsOptions        ds                  qualified inz
     D numKeys                       10I 0
     D options                             likeds(dsOptKey) dim(#ofOpt)
     DdsOptKey         DS                  qualified based(ptrOptions)
     D key                           10I 0
     D keyLen                        10I 0
     D keyData                       10a

     DdsStmtInfo       DS                  qualified inz
     D msgfName                      10a
     D msgfLib                       10a
     D numStmts                      10i 0
     D lenStmt                       10i 0
     D rowBegin                      10i 0
     D colBegin                      10i 0
     D rowEnd                        10i 0
     D colEnd                        10i 0
     D rowErr                        10i 0
     D colErr                        10i 0
     D msgID                          7a
     D sqlState                       5a
     D lenMsg                        10i 0
     D msgData                             likeds(QUSEC)

     d isError         s              1n   inz
     d strSqlStmt      s                   like(inSqlStmt) inz
      /Free
       // Determine length of SQL statement passed in
       CEEDOD(1:dsOperDesc.descType:dsOperDesc.dataType:
        dsOperDesc.descInfo1:dsOperDesc.descInfo2:dsOperDesc.inLen:*omit);
       dsStmt.len = %len(%trimr(inSqlStmt));
       If dsStmt.len > dsOperDesc.inLen;
          dsStmt.len = dsOperDesc.inLen;
       Endif;
       // Ensure SQL does not include extraneous characters
       strSqlStmt = %subst(inSqlStmt:1:dsStmt.len);
       Select;
       When dsStmt.len <= *zero;
          SndPgmMsg('CPF9898':'*INFO':'QCPFMSG':'*LIBL':
           'The SQL statement is empty.');
          Return *on;
       When dsStmt.len < 80;
          // Statement length must be at least 80 characters
          dsStmt.len = 80;
       Endsl;

       // Define options
       dsOptions.numKeys = #ofOpt;
       ptrOptions = %addr(dsOptions.options);
       dsOptKey.key = 2;  // Check & terminate
       dsOptKey.keyLen = 10;
       dsOptKey.keyData = '2';
       ptrOptions = %addr(dsOptions.options) + intOffset;
       dsOptKey.key = 1;  // Naming
       dsOptKey.keyLen = 10;
       dsOptKey.keyData = '*SYS';
       // *SYS, *SQL or *NONE
       If %parms >=2 and inNaming <> *blanks;
          dsOptKey.keyData = inNaming;
       Endif;
       dsStmt.lang = '*NONE';
       If %parms >=3 and inLang <> *blanks;
          dsStmt.lang = inLang;
       Endif;

       dsStmt.numRecProv = 1;
       dsStmt.inSize = %size(dsStmtInfo);

       // Syntax check SQL Statement
       //--------------------------------------------------
       Clear QUSEI;
       Monitor;
         QSQCHKS(strSqlStmt:
          dsStmt.len:
          dsStmt.numRecProv:
          dsStmt.lang:
          dsOptions:
          dsStmtInfo:
          dsStmt.inSize:
          dsStmt.numRecProc:
          QUSEC);
       On-error;
         isError = *on;
       Endmon;

       // Report errors
       Select;
         // - External error
       When isError;
         SndPgmMsg('CPF9898':'*INFO':'QCPFMSG':'*LIBL':
          'Error processing the API.');
         Return *off;
         // - Api Error
       When QUSEI <> *blank;
         // ... Retrieve msg from QCPFMSG *msgf using QUSEI and QUSMSGDTA
         SndPgmMsg(QUSEI:'*INFO':'QCPFMSG':'*LIBL':QUSMSGDTA);
         Return *off;
         // - SQL Error
       When dsStmtInfo.msgID <> *blank;
         // ... Retrieve msg from QSQLMSG *msgf using
         //      dsStmtInfo.stmtOut.msgId and dsStmtInfo.stmtOut.msgData
         IF dsStmtInfo.colErr > *zero;
           // ... Here is the error position
            SndPgmMsg(dsStmtInfo.msgId:'*INFO':
             dsStmtInfo.msgfName:dsStmtInfo.msgfLib:
             dsStmtInfo.msgData);
         Endif;
         Return *off;
       Endsl;

       Return *on;
      /End-free
     PValidSQLSyntax   e
