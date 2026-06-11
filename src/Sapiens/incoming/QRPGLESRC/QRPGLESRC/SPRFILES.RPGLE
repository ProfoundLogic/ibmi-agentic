      *********************************************************************
      * REVISIONS:
     *
     *  08/04/21  C:180   Support: 97167
     *                   -CreateAliasSql - Optionally check on file lock.
      *
     *  02/01/19  C:33    Project: SY407
     *                   -LeaveFileRead
     *
     *  01/14/19  C:33    Project: SY406
     *                   -CreateAliasSQL
     *
     *  12/06/18  C:33    Project: SY406
     *                   -ChkFileLayouts
     *                   -DspFileLayout
     *
     *  05/10/17  C:33    Project: WC2405
     *                    Check file lock
     *
     *  04/04/17  C:33    Project: SY388
     *                    Get file/field description(s)
     *
     *  04/12/16  C:212   Project : C114MXO
     *                    Added bldMember
      *********************************************************************
     D*=====================================================================
     D* File handling procedure prototypes
     F*=====================================================================


     P*=====================================================================
     P* addFileMbr- Add a member to a file
     P*  Parameter 1 - file name   (10a)
     P*  Parameter 2 - member name (10a)
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     DaddFileMbr       PR             1
     D fileName                      10    const
     D memberName                    10    const

      //***  Begin Add  ***  04/04/17  ************************************
     P*=====================================================================
     P* Append information
     P*  Parameter 1 - string 1    (150a)
     P*  Parameter 2 - string 2    (150a)
     P*  Parameter 3 - delimiter    ( 1a)
     P*  Returns - description appended with string
     P*=====================================================================
     DAppendInfo       pr           300
     D inString1                    150    const
     D inString2                    150    const
     D inDelimiter                    1    const options(*nopass)
      //***  End   Add  ***  04/04/17  ************************************

      //***  Begin Add  ***  12/06/18  ************************************
     P*=====================================================================
     P* ChkFileLayouts - Check to see if the file layouts are the same
     P*  Parameter 1 - current file (21a) (file only or lib/file)
     P*  Parameter 2 - compare file (21a) (file only or lib/file)
     P*  Returns - *on if the file layouts are the same, otherwise *off
     P*   {This routine assumes the files exists.  If one or the other doesn't exist, returns *off}
     P*=====================================================================
     DChkFileLayouts   pr             1n
     D inFileCur                     21    const varying
     D inFileCmp                     21    const varying
      //***  End   Add  ***  12/06/18  ************************************

      //***  Begin Add  ***  05/10/17  ************************************
     P*=====================================================================
     P* ChkFileLock - Check to see if the file is locked
     P*  Parameter 1 - library name  (10a)
     P*  Parameter 2 - file name     (10a)
     P*  Parameter 3 - member name   (10a) {optional}
     P*  Returns - *on if the file(member) is locked, otherwise *off
     P*=====================================================================
     DChkFileLock      PR             1n
     D inLib                         10    const
     D inFile                        10    const
     D inMbr                         10    const options(*nopass)
      //***  End   Add  ***  05/10/17  ************************************

     P*=====================================================================
     P* checkMbr - Check to see if the member exists
     P*  Parameter 1 - object name   (10a)
     P*  Parameter 2 - object type   ( 7a)
     P*  Parameter 3 - object member (10a)
     P*  Returns - *on if the member exist, otherwise *off
     P*=====================================================================
     DchkFileMbr       PR             1
     D objectName                    10    const
     D objectType                     7    const
     D objectMbr                     10    const


     P*=====================================================================
     P* clrFileMbr - Clear file
     P*  Parameter 1 - file name   (10a)
     P*  Parameter 2 - member name (10a) {optional}
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     DclrFileMbr       PR             1
     D fileName                      10    const
     D memberName                    10    const options(*nopass)

      //***  Begin Add  ***  01/14/19  ***********************************
     P*=====================================================================
     P* CreateAliasSQL - Create an alias over physical file members to be
     P*                   used by SQL
     P*  Parameter 1 - alias (21a) (name only or lib/file)
     P*  Parameter 2 - file (21a) (file only or lib/file)
     P*  Parameter 3 - member (10a) (member name)
     P*  Parameter 4 - create member (1n) (*ON, *OFF) {optional, *ON default}
     P*  Returns - *on if the command is successful, otherwise *off
     P*=====================================================================
     DCreateAliasSQL...
     D                 pr             1n
     D inAlias                       21    const varying
     D inFile                        21    const varying
     D inMember                      10    const varying
     D inCrtMbr                       1n   const         options(*nopass)
      //***  End   Add  ***  01/14/19  ***********************************
      //***  Beg   Add  ***  08/04/21  ***********************************
     D inChkLock                      1n   const         options(*nopass)
      //***  End   Add  ***  08/04/21  ***********************************

      //***  Begin Add  ***  12/06/18  ************************************
     P*=====================================================================
     P* DspFileLayout - Display file layout (DSPFFD)
     P*  Parameter 1 - file (21a) (file only or lib/file)
     P*  Parameter 2 - output (10a) (*PRINT, *OUTFILE) {optional, *PRINT default}
     P*  Parameter 3 - output file (21a) (file only or lib/file)
     P*  Parameter 4 - output file option (10a) (*REPLACE, *ADD) {optional, *REPLACE default}
     P*  Returns - *on if the command is successful, otherwise *off
     P*=====================================================================
     DDspFileLayout    pr             1n
     D inFile                        21    const varying
     D inOutType                     10    const varying options(*nopass)
     D inOutFile                     21    const varying options(*nopass)
     D inOutFileOpt                  10    const varying options(*nopass)
      //***  End   Add  ***  12/06/18  ************************************

      //***  Begin Add  ***  04/04/17  ************************************
     P*=====================================================================
     P* Get file field description
     P*  Parameter 1 - field       (10a)
     P*  Parameter 2 - file name   (10a)
     P*  Parameter 3 - library name   (10a)
     P*  Parameter 4 - include field in description (10a) {*BEFORE,*AFTER}
     P*  Parameter 5 - delimiter (1a) {-,(,)}
     P*  Returns - description of field (with field name)
     P*=====================================================================
     DGetFieldDesc     pr           150
     D inField                       10    const
     D inFile                        10    const
     D inLib                         10    const
     D inInclude                     10    const options(*nopass)
     D inDelimiter                    1    const options(*nopass)

     P*=====================================================================
     P* Get file description
     P*  Parameter 1 - file name   (10a)
     P*  Parameter 2 - library name   (10a)
     P*  Parameter 3 - include file in description (10a) {*BEFORE,*AFTER}
     P*  Parameter 4 - delimiter (1a) {-,(,)}
     P*  Returns - description of file (with file name)
     P*=====================================================================
     DGetFileDesc      pr           150
     D inFile                        10    const
     D inLib                         10    const
     D inInclude                     10    const options(*nopass)
     D inDelimiter                    1    const options(*nopass)
      //***  End   Add  ***  04/04/17  ************************************

     P*=====================================================================
     P* dltOvr - Delete file override
     P*  Parameter 1 - file name   (10a)
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     DdltOvr           PR             1
     D fileName                      10    const


      //***  Begin Add  ***  02/01/19  ********************************
     P*=====================================================================
     P* Deterimine if the record meets criteria for exiting or EOF
     P*  Parameter 1 - eof (1n)
     P*  Parameter 2 - status (1a) {optional}
     P*  Parameter 3 - expiration date (7.0) {optional}
     P*  Parameter 4 - as of date (7.0) {optional}
     P*  Returns - *on no more records should be read
     P*=====================================================================
     DLeaveFileRead    pr             1n
     D inEof                          1n   const
     D inStatus                       1a   const options(*nopass)
     D inExpDate                      7  0 const options(*nopass)
     D inAsOfDate                     7  0 const options(*nopass)
      //***  End   Add  ***  02/01/19  ********************************


     P*=====================================================================
     P* ovrFile  - Override file
     P*  Parameter 1 - file name               (10a)
     P*  Parameter 2 - overriding member name  (10a)
     P*  Parameter 3 - overriding file name    (10a) {optional}
     P*  Parameter 4 - overriding file library (10a) {optional}
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     DovrFile          PR             1
     D fileName                      10    const
     D memberName                    10    const
     D ovrFile                       10    const options(*nopass)
     D ovrLib                        10    const options(*nopass)

     P*=====================================================================
     P* bldMember - returns name for a member
     P*  Parameter 1 - prefix                  (1a)
     P*  Parameter 2 - format                  (1a)
     P*  Parameter 3 - suffix                  (1a)
     P*  Returns - a member name of 10 char
     P*=====================================================================
     DbldMember        PR            10
     D prefix                         1    const
     D format                         1    const
     D suffix                         1    const
