     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Portal Configuration detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Created on 09/05/18   C:33  Project: SY390
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString
      /copy wPrRateSet
      /copy wPrTable
      /copy wPrUsrPrms

      *entry
     DGetDetail        pr                  ExtPgm('WICONFIG01')
     D                                9  0
     D                            32767
     D                                1
     DGetDetail        pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1

     Di1               s             10i 0 inz(1)
     DsizeExp          s             10i 0 inz(1000)

     DstrExpList       s           5000a   varying inz
     DstrRateSt        s             10a   varying inz
     DstrRateStName    s             50a   varying inz
     DstrStatus        s             10a   varying

     D#ofExpCodes      c                   1000
     DdsWIQUP        e ds                  extname(WIQUP)    inz qualified
     DdsWIQUPACD     e ds                  extname(WIQUPACD) inz qualified
     DdsWIQUPEXP     e ds                  extname(WIQUPEXP) inz qualified
     D                                     dim(#ofExpCodes)
     DdsWIQUPSCD     e ds                  extname(WIQUPSCD) inz qualified

      /Free
       Reset dsWIQUP;
       Exec SQL
        select *
         into :dsWIQUP
         from WIQUP
         where rrn(WIQUP) = :inRRN
         limit 1;

       Select;
       When dsWIQUP.QUstat = 'I';
          strStatus = 'Inactive';
       Other;
          strStatus = 'Active';
       Endsl;

       Clear outJSON;
       BuildJSON(outJSON :'DtlQuestion'    :%trim(dsWIQUP.QUquestion));
       BuildJSON(outJSON :'DtlOrder'       :%editc(dsWIQUP.QUseq#:'3'));
       BuildJSON(outJSON :'DtlQuestionType':%trim(dsWIQUP.QUtype));
       BuildJSON(outJSON :'DtlAnswerType'  :%trim(dsWIQUP.QUanswer));
       BuildJSON(outJSON :'DtlCode'        :%trim(dsWIQUP.QUcode));
       BuildJSON(outJSON :'DtlStatus'      :%trim(strStatus));

       Select;
       When Upper(dsWIQUP.QUtype) = 'ACORD';
          Exsr GetWIQUPACD;
       When Upper(dsWIQUP.QUtype) = 'EXPOSURE CODE';
          Exsr GetWIQUPEXP;
       When Upper(dsWIQUP.QUtype) = 'SCHEDULE RATING';
          Exsr GetWIQUPSCD;
       Endsl;

       RtnCode = ' ';
       *inlr = *on;
       Return;

       //===============================================================
       // Get ACORD information
       //===============================================================
       Begsr GetWIQUPACD;
        Reset dsWIQUPACD;
        Exec SQL
         select *
          into :dsWIQUPACD
          from WIQUPACD
          where QUco# = :dsWIQUP.QUco# and QUid# = :dsWIQUP.QUid#
          limit 1;
        BuildJSON(outJSON :'DtlAcord#'  :%editc(dsWIQUPACD.QUacord#:'X'));
        BuildJSON(outJSON :'DtlAcord#FL':%editc(dsWIQUPACD.QUacord#FL:'X'));
       Endsr;

       //===============================================================
       // Get Exposure information
       //===============================================================
       Begsr GetWIQUPEXP;
        Reset strRateSt;
        Reset strRateStName;
        Reset dsWIQUPEXP;
        Exec SQL close exp;
        Exec SQL
         declare exp cursor for
          select *
           from WIQUPEXP
           where QUco# = :dsWIQUP.QUco# and QUid# = :dsWIQUP.QUid#
           order by QUco#, QUid#, QUexpCode;
        Exec SQL open exp;
        Exec SQL
         fetch next from exp
          for :sizeExp rows
          into :dsWIQUPEXP;
        sizeExp = sqlEr3;
        For i1 = 1 to sizeExp;
           Select;
           When i1 = 1;
              strRateSt = dsWIQUPEXP(i1).QUst;
              Select;
              When dsWIQUPEXP(i1).QUst = *blank;
                 strRateSt = RatingSt(dsWIQUP.QUco#:dsWIQUPEXP(i1).QUrateSet);
              Other;
                 strRateSt = dsWIQUPEXP(i1).QUst;
              Endsl;
           Other;
              strExpList += ',';
           Endsl;
           strExpList += %editc(dsWIQUPEXP(i1).QUexpCode:'X');
        Endfor;

        Exec SQL
         select STname into :strRateStName
         from STSTP
         where STkey = :strRateSt;
        Select;
        When strRateSt = *blank;
           strRateStName = '*ALL - All States';
        Other;
           strRateStName = strRateSt + ' - ' +
            TitleCase(strRateStName);
        Endsl;
        BuildJSON(outJSON :'DtlRatingStateExposure':strRateStName);
        BuildJSON(outJSON :'DtlExposureCode':strExpList);
       Endsr;

       //===============================================================
       // Get Schedule Rating information
       //===============================================================
       Begsr GetWIQUPSCD;
        Reset strRateSt;
        Reset strRateStName;
        Reset dsWIQUPSCD;
        Exec SQL
         select *
          into :dsWIQUPSCD
          from WIQUPSCD
          where QUco# = :dsWIQUP.QUco# and QUid# = :dsWIQUP.QUid#
          limit 1;
        Exec SQL
         select STname into :strRateStName
         from STSTP
         where STkey = :dsWIQUPSCD.QUrateSt;
        Select;
        When dsWIQUPSCD.QUrateSt = *blank;
           strRateStName = '*ALL - All States';
        Other;
           strRateStName = dsWIQUPSCD.QUrateSt + ' - ' +
            TitleCase(strRateStName);
        Endsl;
        BuildJSON(outJSON :'DtlRatingStateScheduleRating':strRateStName);
        BuildJSON(outJSON :'DtlFactor':%editc(dsWIQUPSCD.QUfactor:'3'));
       Endsr;
      /End-free
