     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H nomain
    H*********************************************************************
     H*  Created:
    H*           09/01/16  C:33  Project: SY371
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*  12/17/24  C:340 Seq#: 117961    Sync By: C:333                   *
    H*                  If this Program is called from SPILOCK, Do not   *
    H*                  Check Library.                                   *
    H*                                                                   *
    H*  09/21/21  C:33  Seq#: 97470                                      *
    H*                 -Only check for jobs running in the same env      *
    H*                                                                   *
    H*  08/06/19  C:33  Project: SY406                                   *
    H*                 -Add procedures for                               *
    H*                  - Retrieving the job description                 *
    H*                                                                   *
    H*  07/17/19  C:33  Project: SY406                                   *
    H*                 -Add procedure for                                *
    H*                  - Find the program that called a particular pgm  *
    H*                                                                   *
    H*  06/28/17  C:33  Project: WC2405                                  *
    H*                 -Add procedures for                               *
    H*                  - Checking for an active job                     *
    H*                  - Checking for a job on the jobq                 *
    H*********************************************************************
     D/copy sPrJobInfo

     ddsJOBI0100       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     d reserved1                      2
     d runPriority                   10i 0
     d timeSlice                     10i 0
     d defaultWait                   10i 0
     d purge                         10

     DdsJOBI0150       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     d reserved1                      2
     d runPriority                   10i 0
     d timeSlice                     10i 0
     d defaultWait                   10i 0
     d purge                         10
     d timeSliceEnd                  10

     DdsJOBI0200       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D subsystem                     10
     d runPriority                   10i 0
     d systemPool                    10i 0
     d processTime                   10i 0
     D #AuxRequests                  10i 0
     D #InteractTrans                10i 0
     D responseTime                  10i 0
     d functionType                   1
     D functionName                  10
     D actJobStatus                   4
     D #DBLockWait                   10i 0
     D #IMLockWait                   10i 0
     D #NDLockWait                   10i 0
     D dbLockWaitTime                10i 0
     D imLockWaitTime                10i 0
     D ndLockWaitTime                10i 0

     DdsJOBI0300       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1

     DdsJOBI0400       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     d jobEnteredSys                 13
     d jobBecameAct                  13
     d jobAcctingCode                15
     d jobDName                      10
     d jobDLib                       10

     DdsJOBI0500       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1

     DdsJOBI0600       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D  JOBSWI                63     70
     D  ENDSTS                71     71
     D  SBSNAM                72     81
     D  SBSDLI                82     91
     d  CURUSERPROF           92    101
     D  DBCS                 102    102
     D  EXTKEY               103    103
     D  CNLKEY               104    104
     D  PRDRC                105    108B 0
     D  USRRC                109    112B 0
     D  PGMRC                113    116B 0
     D  SPCENV               117    126
     d  DEVICENAME           127    136
     D  GRPPRF               137    146
     D  SGRPPF               147    296
     D  JUSRID               297    306
     D  JUSRIS               307    307
     D  ipaddress            308    322

     DdsJOBI0700       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D reserved1                      2A
     D NbrSysLibs                    10I 0
     D NbrPrdLibs                    10I 0
     D NbrCurLibs                    10I 0
     D NbrUsrLibs                    10I 0
     D Filler                     32685    Varying

     DdsJOBI0750       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D reserved1                      2A

     DdsJOBI0800       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D reserved1                      2A

     DdsJOBI0900       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D reserved1                      2A

     DdsJOBI1000       ds         32767    qualified
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d jobName                       10
     d userName                      10
     d jobNumber                      6
     d jobId                         16
     d jobStatus                     10
     d jobType                        1
     d jobSubtype                     1
     D reserved1                      2A

     DdsJOBL0200       ds                  qualified
     d strJobName                    10    overlay(dsJOBL0200:1)
     d strJobUser                    10    overlay(dsJOBL0200:11)
     d strJob#                        6    overlay(dsJOBL0200:21)
     d strJobID                      16    overlay(dsJOBL0200:27)
     d strJobSts                     10    overlay(dsJOBL0200:43)
     d strJobType                     1    overlay(dsJOBL0200:53)
     d strJobSubtype                  1    overlay(dsJOBL0200:54)
     d strJobInfoSts                  1    overlay(dsJOBL0200:57)
     d***  Begin Add  ***  09/21/21  *********************
     d strRsv1                        3    overlay(dsJOBL0200:58)
     d str#OfFields                  10i 0 overlay(dsJOBL0200:60)
     d***  End   Add  ***  09/21/21  *********************

     DdsError          ds            15
     D bytesProvided                 10i 0 inz(%size(dsError))
     D bytesAvail                    10i 0
     D errorId                        7

     DdsOffSets        ds                  based(ptrJobSpace)
     D binOffSetHdr          117    120b 0
     D binHeadrSize          121    124b 0
     D binOffSetList         125    128b 0
     D binJobs               133    136b 0
     D binLastEntSiz         137    140b 0

     DRtvUserSpace     pr                  ExtPgm('QUSRTVUS')
     D                               20    const
     D startPos                            const like(binOffSetList)
     D lenOfData                           const like(binLastEntSiz)
     D                            32766    options(*varsize)
     D error                               like(dsError)

     DRtvCallStack     pr                  ExtPgm('RTVCALLSTK')
     D outEntNbr                      5p 0 const
     D inStack                       20a   dim( 128 )

     DRtvUserSpacePtr  pr                  ExtPgm('QUSPTRUS')
     D                               20    const
     D                                 *

     DSrChkPgm         pr                  extPgm('SRCHKPGM')
     D inPgm                         10    const
     D outFound                       1

     d                 ds
     dbinInitSize              1      4b 0 inz(20000)
     dintNbrKeys       s             10u 0 inz(0)

     DCreateUserSpace  pr                  ExtPgm('QUSCRTUS')
     D                               20a   const
     D                               10a   const
     D                               10i 0 const
     D                                1a   const
     D                               10a   const
     D                               50a   const
     D                               10a   const options(*nopass)
     D error                               like(dsError)
     D                                     options(*nopass:*varsize)

     DdsStack          ds                  dim(128) qualified inz
     D program                       10a
     D library                       10a

     D*=====================================================================
     D* JobInfoAPI - This is internal procedure used in this module to invoke
     D*              job information API to retrieve library list of a job
     D*=====================================================================
     DJobInfoAPI       PR                  ExtPgm('QUSRJOBI')
     D RcvVar                     32767    Options(*VarSize)
     D LengthRcv                     10I 0 Const
     D FormatName                     8A   Const
     D QualJobName                   26    Const
     D InternalJobId                 16    Const
     D Error_ds                      15
     D*
     DJobListAPI       PR                  ExtPgm('QUSLJOB')
     D                               20    const
     D                                8    const
     D                               26    const
     D                               10    const
     D error                               like(dsError)
     D                                1    const
     D #OfKeys                             const like(intNbrKeys)
     D                            32766    options(*varsize)
     D*
     DRtvJobInfo       pr             1n
     D inFormatName                   8    const options(*nopass)
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
     D*
     DRtvJobList       pr             1n
     D inStatus                      10    const options(*nopass)
     D inFormatName                   8    const options(*nopass)
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     P*=====================================================================
     P* RtvJobInfo - Retrieve the needed job info                           name
     P*   Parm 1 = Format name (8A)    {optional}
     P*   Parm 2 = Job name (10A)      {optional}
     P*   Parm 3 = Job user name (10A) {optional}
     P*   Parm 4 = Job number (6A)     {optional}
     P*   Returns = *on if job is found, otherwise *off
     P*=====================================================================
     PRtvJobInfo       b
     DRtvJobInfo       pi             1n
     D inFormatName                   8    const options(*nopass)
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
      *
     DisJobFound       s              1n   inz('1')
     DstrFormat        s             10    inz('JOBI0100')
     DstrQualJob       s             26a   inz('*')
     DstrJobName       s             10    inz('*')
     DstrJobUsrName    s             10    inz
     DstrJobNumber     s              6    inz
      *
      /free
       Monitor;
        If %parms >= 1 and inFormatName <> *blank;
           strFormat = inFormatName;
        Endif;
        // Build qualified job name from input values
        If %parms >= 2 and inJobName <> *blanks;
           strJobName = inJobName;
           If %parms >= 3 and inJobUsrName <> *blanks;
              strJobUsrName = inJobUsrName;
           Endif;
           If %parms >= 4 and inJobNumber <> *blanks;
              strJobNumber = inJobNumber;
           Endif;
        Endif;

        strQualJob = strJobName + strJobUsrName + strJobNumber;

        // Call job information API to get job library list
        Select;
        When strFormat = 'JOBI0100';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0100;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0100:%Size(dsJOBI0100):'JOBI0100'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0150';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0150;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0150:%Size(dsJOBI0150):'JOBI0150'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0200';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0200;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0200:%Size(dsJOBI0200):'JOBI0200'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0300';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0300;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0300:%Size(dsJOBI0300):'JOBI0300'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0400';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0400;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0400:%Size(dsJOBI0400):'JOBI0400'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0500';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0500;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0500:%Size(dsJOBI0500):'JOBI0500'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0600';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0600;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0600:%Size(dsJOBI0600):'JOBI0600'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0700';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0700;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0700:%Size(dsJOBI0700):'JOBI0700'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0750';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0750;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0750:%Size(dsJOBI0750):'JOBI0750'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0800';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0800;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0800:%Size(dsJOBI0800):'JOBI0800'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI0900';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI0900;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI0900:%Size(dsJOBI0900):'JOBI0900'
            :strQualJob:*blanks:dsError);
        When strFormat = 'JOBI1000';
         //***  Begin Add  ***  09/21/21  *********
           Clear dsJOBI1000;
         //***  End   Add  ***  09/21/21  *********
           JobInfoAPI(dsJOBI1000:%Size(dsJOBI1000):'JOBI1000'
            :strQualJob:*blanks:dsError);
        Endsl;

       On-Error;
          Return *off;
       EndMon;

       Return *on;
      /end-free
     PRtvJobInfo       e

     P*=====================================================================
     P* RtvJobList - Retrieve the needed job list                           name
     P*   Parm 1 = Job Status (10A)    {optional}
     P*   Parm 2 = Format name (8A)    {optional}
     P*   Parm 3 = Job name (10A)      {optional}
     P*   Parm 4 = Job user name (10A) {optional}
     P*   Parm 5 = Job number (6A)     {optional}
     P*   Returns = *on if job is found, otherwise *off
     P*=====================================================================
     PRtvJobList       b
     DRtvJobList       pi             1n
     D inStatus                      10    const options(*nopass)
     D inFormatName                   8    const options(*nopass)
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
      //***  Beg Add  ***  12/17/24  ***  117961 ****************
     Di2               s             10i 0 inz
      //***  End Add  ***  12/17/24  ***  117961 ****************
      *
      //***  Begin Add  ***  09/21/21  **********
     ddsJobD           ds                  qualified inz
     d name                          10
     d lib                           10

     ddsLibl           ds          2750    inz
     d strLibl                     2750
     d aryLibl                       11    dim(250) overlay(strLibl)
      //***  End   Add  ***  09/21/21  **********

      //***  Begin Add  ***  09/21/21  **********
     DdecStart         s             10i 0 inz
      //***  End   Add  ***  09/21/21  **********
     dflgTypeOfJob     s              1    inz('*')
      //***  Begin Add  ***  09/21/21  **********
     Di1               s             10i 0 inz
      //***  End   Add  ***  09/21/21  **********
     DisJobFound       s              1n   inz('1')
     DstrFormat        s             10    inz('JOBL0200')
     DstrQualJob       s             26a   inz('*')
     DstrJobName       s             10    inz('*')
     DstrJobUsrName    s             10    inz('*ALL')
     DstrJobNumber     s              6    inz('*ALL')
     dstrKeyList       s            400
     DstrStatus        s             10    inz('*ALL')
     dstrSpace         s             20
      *
      /free
       Monitor;
        If %parms >= 1 and inStatus <> *blank;
           strStatus = inStatus;
        Endif;
        If %parms >= 2 and inFormatName <> *blank;
           strFormat = inFormatName;
        Endif;
        // Build qualified job name from input values
        If %parms >= 3 and inJobName <> *blanks;
           strJobName = inJobName;
           If %parms >= 4 and inJobUsrName <> *blanks;
              strJobUsrName = inJobUsrName;
           Endif;
           If %parms >= 5 and inJobNumber <> *blanks;
              strJobNumber = inJobNumber;
           Endif;
        Endif;

       // User space creation - Jobs
        strSpace = 'JOBLIST   QTEMP     ';
        CreateUserSpace(strSpace:'':131072:x'00':'*ALL'
         :'SPIJOBINFO':'*YES':dsError);
        RtvUserSpacePtr(strSpace:ptrJobSpace);

        strQualJob = strJobName + strJobUsrName + strJobNumber;

        // Call job list API
        Select;
        When strFormat = 'JOBL0200';
           JobListAPI(strSpace:'JOBL0200':strQualJob:strStatus:dsError:
            flgTypeOfJob:intNbrKeys:strKeyList);
        Endsl;

      //***  Beg Add  ***  12/17/24  ***  117961 ****************
        RtvCallStack(128:dsStack);
        i2 = %lookup('SPILOCK':dsStack(*).program:1);
      //***  End Add  ***  12/17/24  ***  117961 ****************

      //***  Begin Add  ***  09/21/21  **********
        strLibl = GetUserLibl();
        decStart = binOffSetList + 1;
        For i1 = 1 to binJobs;
           RtvUserSpace(strSpace:decStart:%len(dsJOBL0200):dsJOBL0200:dsError);
           dsJobD = GetJobDescription(dsJOBL0200.strJobName:
            dsJOBL0200.strJobUser:dsJOBL0200.strJob#);
      //***  Beg Chg  ***  12/17/24  ***  117961 ****************
      //***If strLibl <> *blank and %lookup(dsJobd.lib:aryLibl) > 0;
           If (strLibl <> *blank and %lookup(dsJobd.lib:aryLibl) > 0) Or
               i2 > 0;
      //***  End Chg  ***  12/17/24  ***  117961 ****************
              Return *on;
           Endif;
           decStart += %len(dsJOBL0200) + 1;
        Endfor;
      //***  End   Add  ***  09/21/21  **********

       On-Error;
          Return *off;
       EndMon;

      //***  Begin Chg  ***  09/21/21  **********
      //Return *on;
        Return *off;
      //***  End   Chg  ***  09/21/21  **********
      /end-free
     PRtvJobList       e

     P*=====================================================================
     P* ChkJobActive - Check for active job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*=====================================================================
     PChkJobActive     b                   Export
     DChkJobActive     pi             1n
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     d intIndex1       s              5  0
     D isFound         s              1n   inz(*off)
     D strJobName      s             10    inz
     D strJobNumber    s              6    inz
     D strJobUsrName   s             10    inz
      /Free
       If %parms >= 1 and inJobName <> *blanks;
          strJobName = inJobName;
       Endif;
       If %parms >= 2 and inJobUsrName <> *blanks;
          strJobUsrName = inJobUsrName;
       Endif;
       If %parms >= 3 and inJobNumber <> *blanks;
          strJobNumber = inJobNumber;
       Endif;

       Monitor;
         If RtvJobList('*ACTIVE':'JOBL0200':strJobName:strJobUsrName
          :strJobNumber);
          //***  Begin Chg  ***  09/21/21  **********
          //binOffSetList = (binOffSetList + 1);
          //If binJobs <> 0;
          //***isFound = *on;
          //Endif;
            isFound = *on;
          //***  End   Chg  ***  09/21/21  **********
         Endif;
       On-error;
       Endmon;
       Return isFound;
      /End-free
     PChkJobActive     e

     P*=====================================================================
     P* ChkJobJobQ - Check for active job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*=====================================================================
     PChkJobJobQ       b                   Export
     DChkJobJobQ       pi             1n
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     d intIndex1       s              5  0
     D isFound         s              1n   inz(*off)
     D strJobName      s             10    inz
     D strJobNumber    s              6    inz
     D strJobUsrName   s             10    inz
      /Free
       If %parms >= 1 and inJobName <> *blanks;
          strJobName = inJobName;
       Endif;
       If %parms >= 2 and inJobUsrName <> *blanks;
          strJobUsrName = inJobUsrName;
       Endif;
       If %parms >= 3 and inJobNumber <> *blanks;
          strJobNumber = inJobNumber;
       Endif;

       Monitor;
         If RtvJobList('*JOBQ':'JOBL0200':strJobName:strJobUsrName
          :strJobNumber);
          //***  Begin Chg  ***  09/21/21  **********
          //binOffSetList = (binOffSetList + 1);
          //If binJobs <> 0;
          //***isFound = *on;
          //Endif;
            isFound = *on;
          //***  End   Chg  ***  09/21/21  **********
         Endif;
       On-error;
       Endmon;
       Return isFound;
      /End-free
     PChkJobJobQ       e

     P*=====================================================================
     P* GetJobDescription - Retrieve the job descriptoin for the provided qualified job name
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = job description (name lib)  (20A)
     P*=====================================================================
     PGetJobDescription...
     P                 b                   export
     DGetJobDescription...
     D                 pi            20
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     D strJobName      s             10    inz
     D strJobNumber    s              6    inz
     D strJobUsrName   s             10    inz
      /free
       Monitor;
          If %parms >= 1 and inJobName <> *blanks;
             strJobName = inJobName;
          Endif;
          If %parms >= 2 and inJobUsrName <> *blanks;
             strJobUsrName = inJobUsrName;
          Endif;
          If %parms >= 3 and inJobNumber <> *blanks;
             strJobNumber = inJobNumber;
          Endif;
          If RtvJobInfo('JOBI0400':strJobName:strJobUsrName:strJobNumber);
             Return dsJOBI0400.jobDName + dsJOBI0400.jobDLib;
          Endif;
       On-error;
       Endmon;
       Return *blanks;
      /End-free
     PGetJobDescription...
     P                 e

     P*=====================================================================
     P* GetJobType - Retrieve the job type for provided qualified job name
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = job type  (3A)
     P*=====================================================================
     PGetJobType       B                   Export
     DGetJobType       PI             3
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
      *
     D decSkipLibs     s             10I 0 Inz(0)
     D strJobName      s             10    inz
     D strJobNumber    s              6    inz
     D strJobUsrName   s             10    inz
     D strLibl         s          32767A   Varying
     D strUserLibl     s           2750A   inz
      *
      /free
       Monitor;
        If %parms >= 1 and inJobName <> *blanks;
           strJobName = inJobName;
        Endif;
        If %parms >= 2 and inJobUsrName <> *blanks;
           strJobUsrName = inJobUsrName;
        Endif;
        If %parms >= 3 and inJobNumber <> *blanks;
           strJobNumber = inJobNumber;
        Endif;

        If RtvJobInfo('JOBI0100':strJobName:strJobUsrName:strJobNumber);
           Select;
           // ASJ (Autostart)
           When dsJOBI0100.jobType = 'A' and dsJOBI0100.jobSubtype = *blank;
              Return 'ASJ';
           // BCH (Batch)
           When dsJOBI0100.jobType = 'B' and dsJOBI0100.jobSubtype = *blank;
              Return 'BCH';
           // BCI (Batch immediate)
           When dsJOBI0100.jobType = 'B' and dsJOBI0100.jobSubtype = 'D';
              Return 'BCI';
           // EVK (Started by a program start request)
           When dsJOBI0100.jobType = 'B' and dsJOBI0100.jobSubtype = 'E';
              Return 'EVK';
           // INT (Interactive)
           When dsJOBI0100.jobType = 'I' and dsJOBI0100.jobSubtype = *blank;
              Return 'INT';
           // M36 (AS/400 Advanced 36 machine server)
           When dsJOBI0100.jobType = 'B' and dsJOBI0100.jobSubtype = 'F';
              Return 'M36';
           // MRT (Multiple requester terminal)
           When dsJOBI0100.jobType = 'B' and dsJOBI0100.jobSubtype = 'T';
              Return 'MRT';
           // PJ (Prestart job)
           When dsJOBI0100.jobType = 'B' and dsJOBI0100.jobSubtype = 'J';
              Return 'PJ ';
           // PDJ (Print driver job)
           When dsJOBI0100.jobType = 'W' and dsJOBI0100.jobSubtype = 'P';
              Return 'PDJ';
           // RDR (Reader)
           When dsJOBI0100.jobType = 'R' and dsJOBI0100.jobSubtype = *blank;
              Return 'RDR';
           // SYS (System)
           When dsJOBI0100.jobType = 'S' and dsJOBI0100.jobSubtype = *blank;
              Return 'SYS';
           // SYS (System)
           When dsJOBI0100.jobType = 'X' and dsJOBI0100.jobSubtype = *blank;
              Return 'SYS';
           // SBS (Subsystem monitor)
           When dsJOBI0100.jobType = 'M' and dsJOBI0100.jobSubtype = *blank;
              Return 'SBS';
           // WTR (Writer)
           When dsJOBI0100.jobType = 'W' and dsJOBI0100.jobSubtype = *blank;
              Return 'WTR';
           // blank (Alternative user subtype--not an active job)
           Other;
              Return *blank;
           Endsl;
        Else;
           Return *blank;
        Endif;
       On-Error;
           Return *blank;
       EndMon;

       Return *blank;
      /end-free
     PGetJobType       E

     P*=====================================================================
     P* GetUserLibl - Retrieves User library list for provided qualified jobname
     P*               Job should be active and user should have authority to the job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = Library list (32767A)
     P*=====================================================================
     PGetUserLibl      B                   Export
     DGetUserLibl      PI          2750
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
      *
     D decPos          s             10I 0 Inz(0)
     D decSkipLibs     s             10I 0 Inz(0)
     D strJobName      s             10    inz
     D strJobNumber    s              6    inz
     D strJobUsrName   s             10    inz
     D strLibl         s          32767A   Varying
     D strUserLibl     s           2750A   inz
      *
      /free
       Monitor;
        If %parms >= 1 and inJobName <> *blanks;
           strJobName = inJobName;
        Endif;
        If %parms >= 2 and inJobUsrName <> *blanks;
           strJobUsrName = inJobUsrName;
        Endif;
        If %parms >= 3 and inJobNumber <> *blanks;
           strJobNumber = inJobNumber;
        Endif;

        If RtvJobInfo('JOBI0700':strJobName:strJobUsrName:strJobNumber);
           // Position to user portion of library list
           decSkipLibs = dsJOBI0700.NbrSysLibs +
            dsJOBI0700.NbrPrdLibs + dsJOBI0700.NbrCurLibs;
           decPos = (decSkipLibs * 11) + 81;
           // Retrieve user library list
           strLibl = %subst(dsJOBI0700:decPos:dsJOBI0700.NbrUsrLibs * 11);
           strUserLibl = %Trim(strLibl);
        Else;
           strUserLibl = 'X';
        Endif;
       On-Error;
        strUserLibl = 'X';
       EndMon;

       Return strUserLibl;
      /end-free
     PGetUserLibl      E

     P*=====================================================================
     P* GetWhatCalled - Retrieve the program that called the passed in pgm
     P*  Parm 1 = Pgm Name (10A)
     P*  Returns = pgm that called it (10A)
     P*=====================================================================
     PGetWhatCalled    b                   export
     DGetWhatCalled    pi            10
     D inProgram                     10    const

     Di1               s             10i 0 inz
      /Free
       RtvCallStack(128:dsStack);
       i1 = %lookup(inProgram:dsStack(*).program:1)+1;
       Return dsStack(i1).program;
      /End-free
     PGetWhatCalled    e

     P*=====================================================================
     P* PgmInStack - Determine if program is in call stack                  name
     P*   Parm 1 = Program name (10A)
     P*   Returns = *on if program is found in stack, otherwise *off
     P*=====================================================================
     PPgmInStack       b                   export
     DPgmInStack       pi             1n
     D inPgm                         10    const
      *
     DisFound          s              1n   inz('0')
      *
      /free
       SrChkPgm(inPgm:isFound);
       Return isFound;
      /end-free
     PPgmInStack       e
