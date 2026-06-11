     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H* Program:     W8055R
     H* Description: Create EDI Update Transactions
     H*
     H* Created By:  C:079  Proj:WC2515   Date:  10/23/18
     H***************************************************************
     H* Revisions:
     H***************************************************************
     H* Indicator Usage:
     H***************************************************************
     f*
     fwmcml13   if   e           k disk
     fwmlgl     uf a e           k disk
     Fwdpal5    if   e           k disk
     Fwmfnl     if   e           k disk
      *
     d  @@Co#          s              3
     d  @@State        s              2
     d  @@ClStat       s              1
     d  @@ClTyp        s              1
     d  @@StrDate      s              8
     d  @@EndDate      s              8
     d  @@CrtBo        s              1
     d  @@TrnId        s              3
     d  @@mntcde       s              2
     d  @@Pmnts        s              1
     d  srdti          s              8  0
     d  srdto          s             10
     d  srerr          s              1
     d  srffmt         s              8
     d  srtfmt         s              8
     d  xxco#          s              3  0
     d  xxdate         s              7  0
     d  xxtime         s              6  0
     d  xxdue          s              7  0
     d  xxyear         s              4  0
      *
     d  strDate7       s              7  0
     d  endDate7       s              7  0
     d  sys_date       s               D   datfmt(*USA)
     d  sys_Time       s               T
     d                 ds
      *
     d FIDS01        E DS                  EXTNAME(SZQ2P)
     d PSDS          ESDS                  EXTNAME(SZQ1P)
     d/copy sPrDate
      *
      *  entry parm list
      *
     C     *entry        plist
     C                   parm                    @@Co#
     C                   parm                    @@State
     C                   parm                    @@ClStat
     C                   parm                    @@ClTyp
     C                   parm                    @@StrDate
     C                   parm                    @@EndDate
     C                   parm                    @@CrtBo
     C                   parm                    @@TrnId
     C                   parm                    @@MntCde
     C                   parm                    @@Pmnts
      *
      *  Key List for the Claim file
      *
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    @@state
      *
      *  Define Work Fields
      *
      /Free
         xxco# = %Dec(@@co#:3:0);
         sys_date = %date();
         sys_time = %time();
         xxDate = Dateto7(sys_date);
         xxTime = Timeto6(sys_time);
         Monitor;
         strDate7   = dtCYMD(%Dec(@@strDate:8:0));
         On-Error;
            strDate7 = 0;
         Endmon;
         If @@endDate = *zeros or @@endDate = *blanks;
            endDate7 = xxDate;
         Else;
            Monitor;
            endDate7   = dtCYMD(%Dec(@@endDate:8:0));
            On-Error;
               endDate7 = 0;
            Endmon;
         Endif;
      *
      * Main loop - read claims
      *
         Setll cmky pcm01;
         Reade cmky pcm01;
         Dow not %Eof(wmcml13);
            Chain (CMco#:CMfnd) pfn01;
            If %Found(wmfnl) and FNedi <> 'Y';
               If @@clStat = 'B' and (CMstat = 'O' or CMstat = 'C') or
                           CMstat = @@clStat;
                  If @@clTyp = 'B' and (CMltFl = 'I' or CMltFl = 'M') or
                              CMltFl = @@clTyp;
                     Select;
                     // Date received
                     When @@CrtBo = 'D';
                        If CMrcVd >= strDate7 and CMrcVd <= endDate7;
                           If @@pmnts = 'Y';
                              Exsr CheckPmnts;
                           Else;
                              Exsr CreateWMLGP;
                           Endif;
                        Endif;
                     // Accident date
                     When @@CrtBo = 'A';
                        If CMacDt >= strDate7 and CMacDt <= endDate7;
                           If @@pmnts = 'Y';
                              Exsr CheckPmnts;
                           Else;
                              Exsr CreateWMLGP;
                           Endif;
                        Endif;
                     // Report date
                     When @@CrtBo = 'R';
                        If CMdtRp >= strDate7 and CMdtRp <= endDate7;
                           If @@pmnts = 'Y';
                              Exsr CheckPmnts;
                           Else;
                              Exsr CreateWMLGP;
                           Endif;
                        Endif;
                     EndSl;
                  Endif;
               Endif;
            Endif;
            Reade cmky pcm01;
         Enddo;
         exsr PgmEnd;
      *
      *=========================================================================
      *  Check payment file
      *=========================================================================
       Begsr CheckPmnts;
          Setgt (CMco#:CMfnd:CMfyr:CMcase:strDate7) ppa01;
          Reade (CMco#:CMfnd:CMfyr:CMcase) ppa01;
          If not %Eof(wdpal5);
             If PAtrDt >= strDate7 and PAtrDt <= endDate7;
                Exsr CreateWmlgp;
             Endif;
          Endif;
       Endsr;
      *
      *=========================================================================
      *  Write EDI log record
      *=========================================================================
       Begsr createWmlgp;

          clear plg01;
          lgco# = CMCO#;
          lgfnd = CMFND;
          lgfyr = CMFYR;
          lgcase = CMCASE;
          lgtset = @@TrnID;
          lgmtc = @@mntcde;
          lgjst = @@state;
          lgsts = 'EC';
          lgdue  = xxdate;
          lgmtcd = xxdate;
          lgiusr = q1user;
          lgidt = xxdate;
          lgitm = xxtime;
          lgipgm = q1pgm;
          write     plg01;

       Endsr;
      *=========================================================================
      *  END OF PROGRAM
      *=========================================================================
       Begsr PgmEnd;

         *inlr = *on;
         return;

       Endsr;
      /End-Free
