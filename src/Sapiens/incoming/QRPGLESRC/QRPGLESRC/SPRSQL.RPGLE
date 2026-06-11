     DdsSqlDiag        ds                  qualified
     D DB2_RETURNED_SQLCODE...
     D                               10    varying
     D RETURNED_SQLSTATE...
     D                               10    varying
     D MESSAGE_TEXT...
     D                             1000    varying
     D*=====================================================================
     D* SQL handling procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* IsValidFetch - Determines if a fetch has occurred
     P*  --- Syntax Check SQL Statement (QSQCHKS) API
     P*  Parameter 1 - SQL State
     P*  Returns - *on if fetch is valid, otherwise *off
     P*=====================================================================
     DIsValidFetch     pr             1n
     D inSqlState                     5a   const
     P*=====================================================================
     P* Validate the syntax of an SQL statement using
     P*  --- Syntax Check SQL Statement (QSQCHKS) API
     P*  Parameter 1 - SQL statement
     P*  Parameter 2 - naming convention (*NONE/*SYS/*SQL)
     P*  Parameter 3 - language (*NONE/*RPG/*RPGLE/*RPGLEFREE)
     P*  Returns - *on if syntax is valid, otherwise *off
     P*=====================================================================
     DValidSQLSyntax   pr             1n   opdesc
     D inSqlStmt                  32767a   const options(*varsize)
     D inNaming                       5    const options(*nopass)
     D inLang                        10    const options(*nopass)
