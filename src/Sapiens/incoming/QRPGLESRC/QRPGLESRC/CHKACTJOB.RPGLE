      *---------------------------------------------------------------------
      * Program: CHKACTJOB
      *---------------------------------------------------------------------
      * Programmer:    Richard Cranston
      * Description:   Calling program passes the JobName and ActiveJobFlag
      *                If JobName is active, ActiveJobFlag = 'Y'
      *---------------------------------------------------------------------

      *---------------------------------------------------------------------
      * Stand alone Fields & Data Structures
      *---------------------------------------------------------------------
     D JobNameEntry    S             10
     D JobRunning      S              1
     D StringPos       S             10I 0 Inz
     D StrLength       S             10I 0 Inz
     D SizOfSpace      S             10I 0 Inz(1024)

     D UserSpace       DS                  Inz
     D  SpaceName                    10    Inz('JOBSPACE')
     D  SpaceLib                     10    Inz('QTEMP')

     D QualJobNam      DS                  Inz
     D  JobName                      10
     D  UserName                     10
     D  JobNumber                     6

     D Error           DS                  Inz
     D  BytesProv                    10I 0
     D  BytesAvail                   10I 0
     D  MessageID                     7
     D  NotUsed                       1
     D  MsgData                     100

      * Create User Space Parms
     D ExtAtr          S             10    Inz('TEMPSPACE')
     D InitialVal      S              1    Inz
     D Authority       S             10    Inz('*ALL')
     D TextSpace       S             50    Inz
     D ReplaceSpc      S             10    Inz('*YES')

      * List Jobs API Parms
     D ListFmtNam      S              8    Inz('JOBL0100')
     D JobStatus       S             10    Inz('*ALL')

      * List Job Information API Parms
     D RecVar          DS                  Inz
     D  BytesRet                     10I 0
     D  BytesAval                    10I 0
     D  JobNam                       10
     D  UserNam                      10
     D  JobNum                        6
     D  IntJobID                     16
     D  JobSts                       10
     D  Jobtype                       1
     D  Jobsubtyp                     1
     D  Reserved                      2
     D  EndSev                       10I 0
     D  LogSev                       10I 0
     D  LogLvl                        1
     D  LogTxt                       10

      * User Space Generic Header Definition
     D GenericStc      DS                  Inz
     D  UserArea                     64
     D  SizGeneric                   10I 0
     D  RlsLevel                      4
     D  FormatName                    8
     D  APIused                      10
     D  DateTime                     13
     D  InfoStatus                    1
     D  SizUsrSpac                   10I 0
     D  InpOffset                    10I 0
     D  InpSize                      10I 0
     D  HdrOffset                    10I 0
     D  HdrSize                      10I 0
     D  LstOffset                    10I 0
     D  LstSize                      10I 0
     D  NumLstEnt                    10I 0
     D  SizeLstEnt                   10I 0
     D  temp1                        10I 0
     D  temp2                         2
     D  temp3                         3
     D  temp4                         1
     D  temp5                        42
     D  temp6                       256
     D  temp7                       128

      * List Data Section of Jobs API User Space
     D ListData        DS                  Inz
     D  Job                          10
     D  User                         10
     D  Number                        6
     D  InternalID                   16
     D  Status                       10

      *---------------------------------------------------------------------
      * Entry Parameters
      *---------------------------------------------------------------------
     C     *Entry        PList
     C                   Parm                    JobNameEntry
     C                   Parm      JobRunning    JobRunning

      *---------------------------------------------------------------------
      * Main Routine
      *---------------------------------------------------------------------
     C                   Eval      JobName = JobNameEntry
     C                   Eval      UserName = '*ALL'
     C                   Eval      JobNumber = '*ALL'

      * Create User Space API
     C                   Call      'QUSCRTUS'
     C                   Parm                    UserSpace
     C                   Parm                    ExtAtr
     C                   Parm                    SizOfSpace
     C                   Parm                    InitialVal
     C                   Parm                    Authority
     C                   Parm                    TextSpace

      * List Jobs API
     C                   Call      'QUSLJOB'
     C                   Parm                    UserSpace
     C                   Parm                    ListFmtNam
     C                   Parm                    QualJobNam
     C                   Parm                    JobStatus

      * Retrieve Generic Header Section of the User Space
     C                   Eval      StrLength = %len(GenericStc)
     C                   Call      'QUSRTVUS'
     C                   Parm                    UserSpace
     C                   Parm      1             StringPos
     C                   Parm                    StrLength
     C                   Parm                    GenericStc

      * Retrieve ListData Section
     C                   Eval      StrLength = SizeLstEnt                       Size each List Entry
     C                   Eval      StringPos = LstOffset + 1 +                  Last Record
     C                                   (StrLength * (NumLstEnt - 1))

     C                   Call      'QUSRTVUS'
     C                   Parm                    UserSpace
     C                   Parm                    StringPos
     C                   Parm                    StrLength
     C                   Parm                    ListData                       Last job information

      * Set ActiveJobFlag
     C                   If        Status = '*ACTIVE'
     C                   Eval      JobRunning = 'Y'
     C                   Else
     C                   Eval      JobRunning = 'N'
     C                   EndIF

      * Delete UserSpace
     C                   Call      'QUSDLTUS'
     C                   Parm                    UserSpace
     C                   Parm                    Error

     C                   Eval      *inlr = *On
