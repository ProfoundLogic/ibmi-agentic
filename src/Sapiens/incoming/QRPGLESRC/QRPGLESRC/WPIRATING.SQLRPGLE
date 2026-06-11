     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     ddecLogSeq#       s              9  0 inz

     ddsMaintInfo      ds                  qualified inz
     d date                           7  0
     d time                           6  0
     d user                          10
     d program                       10
     ddsMaintInz       ds                  likeds(dsMaintInfo) inz

     D psds          esds                  extname(szq1p)

     D/copy sPrDate
     D/copy sPrJobInfo

      /Free
       Exec Sql Set Option Commit = *None;
       *inlr = *on;
      /End-free

    P*====================================================================
    P* LogRatingInfo - Log rating informaiton
    P*   co#       - accepts the company #
     P*   app#      - accepts the application #
    P*   group     - accepts the group
    P*   employer  - accepts the employer
    P*   yr        - accepts the policy year
    P*   type      - accepts the information type
     P*   info      - accpets the information
    P*====================================================================
     PLogRatingInfo    b                   export
    P*
     DLogRatingInfo    pi
     D inCo#                          3  0 const
     D inApp#                         9  0 const
     D inGroup                        3  0 const
     D inEmployer                     9  0 const
     D inYr                           3  0 const
     D inType                        30    const
     D inInfo                       500    const

     dstrStatus        s              1    inz

     ddsWIRATLOG     e ds                  extname(WIRATLOG)
      /Free

       // If the maintenance information is not poupluated,
       //  this is the first log.
       If dsMaintInfo = dsMaintInz;
          // Clear prior logs
          Exec sql
           delete from WIRATLOG
            where RLco# = :inCo# and RLapp# = :inApp#
             and RLfnd = :inGroup and RLemp# = :inEmployer
             and RLfyr = :inYr;
          // We are not currently using the sequence number.
          Exec sql
           select max(RLseq#)+1 into :decLogSeq#
            from WIRATLOG
            where RLco# = :inCo# and RLapp# = :inApp#
             and RLfnd = :inGroup and RLemp# = :inEmployer
             and RLfyr = :inYr;
          dsMaintInfo.date = DateTo7(SystemDate());
          dsMaintInfo.time = TimeTo6(SystemTime());
          dsMaintInfo.user = q1User;
          dsMaintInfo.program = GetWhatCalled(q1Pgm);
       Endif;
       Exec sql
        insert into WIRATLOG
         (
          RLco#, RLapp#, RLfnd, RLemp#, RLfyr, RLseq#, RLlogType, RLlogInfo,
          RLstat, RLuser, RLpgm, RLtime, RLdt, RLiUsr, RLiPgm, RLiTim, RLiDt
         )
         values
         (
          :inCo#, :inApp#, :inGroup, :inEmployer, :inYr, :decLogSeq#,
          :inType, :inInfo,
          :strStatus, :dsMaintInfo.user, :dsMaintInfo.program,
          :dsMaintInfo.time, :dsMaintInfo.date, :dsMaintInfo.user,
          :dsMaintInfo.program, :dsMaintInfo.time, :dsMaintInfo.date
         );

     PLogRatingInfo    e
