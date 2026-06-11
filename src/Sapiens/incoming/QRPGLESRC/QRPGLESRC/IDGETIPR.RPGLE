     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE IDGETIPR - GET IP ADDRESS OF THIS JOB
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *   Retrieves the IP address of the job that this program is running in  *
     *                                                                        *
     * CREATED   : 10/31/2006                                                 *
     * PROGRAMMER: C:002                                                      *
     * PROJECT   : DR214                                                      *
      *
      * ************************************************************************
      *  REVISIONS
      * ************************************************************************
      *
     *========================================================================*
     ***==========================================================================================**
     **  This pgm (API) is used to retrieve the current job's IP address.  This should work for inte
     **   ractive and "batch" (QZRCSRVS) jobs as well.
     **
     ** The Job info APIs can be called to retrieve information about
      * a specific job. The Qualified Job Name (Job/User/Number)
      * can be passed to the API, or the job Internal Identifier
      *  hat can be retrieved via the QUSLJOB (List Job) API,
      * or "*" to retrieve the current job details.
      *
      * Note: Ten Formats are available to retrieve
      *    job information, These are:
     ***~
      **JOBI0100 This format returns basic performance information about a job.
      **      It is faster than the JOBI0150 format and the JOBI0200 format
      **      (which also contain performance information). The reason that
      **      this format is faster is that it does not touch as many
      **      objects, causing less paging when retrieving information
      **      about the job.
      **
      * JOBI0150 This format returns additional performance information, and is
      *       slower than the JOBI0100 format. It is similar to the JOBI0200
      *       format, but is faster than that format because there is less
      *       paging involved in retrieving the information.
      *
      * JOBI0200  This format returns information equivalent to that found
      *        on the Work with Active Jobs (WRKACTJOB) command.
      *
      * JOBI0300  This format returns job queue and output queue information
      *        for a job, as well as information about the submitter's job
      *        if the job is a submitted batch job.
      *
     ** JOBI0400 This format primarily returns job attribute types of information,
     **       but has other types of information as well.
     **
     ** JOBI0500 This format returns message logging information.

     ** JOBI0600 This format returns information about active jobs only.
     **         It is intended to supplement the JOBI0400 format.
     **         It retrieves information from several additional objects
     ** associated with the job, and therefore, it causes
     ** additional paging.
     **
     ** JOBI0700 This format returns library list information
     ** for an active job.
     **
     ** JOBI0800  This format returns signal information for an
     ** active job.
     **  `
     ** JOBI0900 This format returns SQL open cursor information
     ** for an active job.
     ***==========================================================================================**

     D Inf_Len         S              4B 0

     ** The following data structure is then returned to the
     ** calling program:
     ***
     D Job_Inf         DS           256
     d  JobName                9     18
     d  JobUser               19     28
     d  JobNumber             29     34
     d  InternalId            35     50
     d  Status                51     60
     D  JobType               61     61
     D  JobSubType            62     62
     D  Subsystem             63     72
     D  JobRunPty             73     76b 0
     D  SystemPool            77     80b 0
     D  ProcessUnit           81     84b 0
     D  AuxRequests           85     88b 0
     D  InteractTrans         89     92b 0
     D  TotRespTime           93     96b 0
     d  FunctionType          97     97
     D  FunctionName          98    107
     D  ActJobStatus         108    111
     D  Reserved             112    112
     D  DBLockWait           113    116b 0
     D  NDBLockWait          117    120b 0
     D  MchLockWait          121    124b 0
     D  TimeDBLckW           125    127
     D  TimeNDBLckW          128    130
     D  TimeMchLckW          131    133
     D*
     D Job_Inf2        DS           322
     D  usenam                19     28
     d  CURUSERPROF           92    101
     d  DEVICENAME           127    136
     D  ipaddress            308    322

     ** For this program, Only IP address is retruned to the caller.
     c     *entry        plist
     c                   parm                    qqip             15
     **
     ** Optionally we could use the Internal Job ID (*char 16) which
     ** can be gathered from the QUSLJOB (list job) API. (alternative)

     ** Qualify the Job Name:
     C                   Eval      Job_Qual = '*                '                 Current job
     c                   z-add     256           Inf_Len
     **
     *** Call the API to retrieve the WRKACTJOB info for this job:               not really needed
     C                   call      'QUSRJOBI'
     C                   parm                    Job_Inf
     C                   parm                    INF_Len
     C                   parm      'JOBI0200'    Api_Format        8
     C                   parm                    Job_Qual         26
     C                   parm      *BLANKS       Job_Int          16
     **
     c                   z-add     322           Inf_Len
     **
     *** Call the API to retrieve the Additional info for this job:               Ip address is here
     C                   call      'QUSRJOBI'
     C                   parm                    Job_Inf2
     C                   parm                    INF_Len
     C                   parm      'JOBI0600'    Api_Format        8
     C                   parm                    Job_Qual         26
     C                   parm      *BLANKS       Job_Int          16
     ***
     C                   eval      qqip = ipaddress
     ***
     c                   eval      *inlr = *on
     c                   return
