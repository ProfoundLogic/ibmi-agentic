     D*=====================================================================
     D* Job Information related procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* ChkJobActive - Check for active job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*=====================================================================
     DChkJobActive     pr             1n
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)
     P*=====================================================================
     P* ChkJobJobQ - Check for job on jobq
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*=====================================================================
     DChkJobJobQ       pr             1n
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     P*=====================================================================
     P* GetJobDescription - Retrieve the job descriptoin for the provided qualified job name
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = job description (name lib)  (20A)
     P*=====================================================================
     DGetJobDescription...
     D                 pr            20
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     P*=====================================================================
     P* GetJobType - Retrieve the job type for provided qualified job name
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = job type  (3A)
     P*=====================================================================
     DGetJobType       pr             3
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     P*=====================================================================
     P* GetUserLibl - Retrieves User library list for provided qualified jobname
     P*               Job should be active and user should have authority to the job
     P*          Parm 1 = Job Name (10A)
     P*          Parm 2 = Job user name (10A)
     P*          Parm 3 = Job number (6A)
     P*          Returns = Library list (32767A)
     P*=====================================================================
     DGetUserLibl      pr          2750
     D inJobName                     10    const options(*nopass)
     D inJobUsrName                  10    const options(*nopass)
     D inJobNumber                    6    const options(*nopass)

     P***  Begin Add  ***  07/17/19  ******************************
     P*=====================================================================
     P* GetWhatCalled - Retrieve the program that called the passed in pgm
     P*  Parm 1 = Pgm Name (10A)
     P*  Returns = pgm that called it (10A)
     P*=====================================================================
     DGetWhatCalled    pr            10
     D inProgram                     10    const
     P***  End   Add  ***  07/17/19  ******************************

     P*=====================================================================
     P* PgmInStack - Determine if program is in call stack                  name
     P*   Parm 1 = Program name (10A)
     P*   Returns = *on if program is found in stack, otherwise *off
     P*=====================================================================
     DPgmInStack       pr             1n
     D inPgm                         10    const
