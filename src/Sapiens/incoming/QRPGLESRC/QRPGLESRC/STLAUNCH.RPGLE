     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/TITLE STLAUNCH - Atrium link request processor
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Atrium Link request processor           *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 03/16/12  C:191 PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  03/15/18 C:33  Project: SY261                                *
     H*                 Do not save adjusted date.                    *
     H*                                                               *
     H* 10/20/16 C:308 Support: 43777                                 *
     H*                Correct inParm value & usropn the smupp file.  *
     H*                                                               *
     H* 07/01/16 C:136 Support: 42300                                 *
     H*                Clear *LDA so that downstream process (revised *
     H*                billing/policy page) can work properly         *
     H*                                                               *
     H* 04/02/14  C:302 Support: WA099 85256                            *
     H*                 Set libl earlier in the process so when it    *
     H*                 checks for the program it would be found.     *
     H*                                                               *
     H* 01/24/14  C:33  Project: SY336                                *
     H*                 Add envrionment messaging                     *
     H*                                                               *
     H*   C:191   Redmine:3068  Add user count process (SY261)        *
     H*                                                               *
     H*****************************************************************
     F*****Begin Chg  ***  10/20/16  ********************************
     F***smupl     uf   e           k disk
     Fsmupl     uf   e           k disk    usropn
     F*****EndChg  ***  10/20/16  ***********************************
     Fsmwup     uf a e           k disk    usropn
     F                                     rename(pwu01:pwu0p)
     Fstlaunch  cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     F*
     D psds          esds                  extname(szq1p)
     D/copy sPrAtrium
     D/copy sPrDate
     D*entry
     DAtriumLaunch     PR                  EXTPGM('STLAUNCH')
     D                               10
     D                               26
     D                             3250
     D                                1    options(*nopass)
     DAtriumLaunch     PI
     D inProgram                     10
     D inJobInf                      26
     D inParm                      3250
     D inMode                         1    options(*nopass)
     D*
     DRunCmd           pr                  extPgm('QCMDEXC')
     D inCmd                       2750    const
     D inCmdLen                      15  5 const
     D*
     dSetInitLibl      pr                  extPgm('M0002C')
     d inApp                          3    const
     d inMetL                        10    const
     d inOVR1                        70    const
     d inOVR2                        70    const
     d inOVR3                        70    const
     d inOVR4                        70    const
     d inOVR5                        70    const
     d inRTCD                         1    const
      *
     DWtQteLib         pr                  extPgm('WTQTELIB')

     D***  Begin Add  ***  01/24/14  *************************************
     DCheckObjInLIBL   pr                  extPgm('SRCHKOBJ')
     D outObject                     10    const
     D outObjectType                  7    const
     D inError                        1

     DCheckLibInLIBL   pr                  extPgm('SRCHKLIB')
     D outLib                        10    const
     D outType                        4    const
     D outAppSuffix                   1    const
     D inExists                       1

     DRtvObjectLib     pr                  extPgm('RTVOBJLIB')
     D outObject                     10    const
     D outObjectType                  7    const
     D inLib                         10

     DDevSpecificTask  pr                  ExtPgm('PUIATEXDEV')
     D task                          20    const
     D rtnCode                        1    options(*nopass)
     D***  End   Add  ***  01/24/14  *************************************
     D*
     D dsJobInf        ds            26
     D  strJobName                   10    overlay(dsJobInf)
     D  strUserName                  10    overlay(dsJobInf:11)
     D  strJobNumber                  6    overlay(dsJobInf:21)
     D  strEnv                        3    overlay(dsJobInf)
     D*
     D inCmd           s           2750
     D flgMode         s              1    inz
     D endPos          s              7  0 inz
     D strUserLibl     S           2750A   varying
     D pastDate        s              8  0 inz
     D day#            s              2  0 inz
     D***  Begin Add  ***  01/24/14  *************************************
     D strLiveLib      s             10    inz
     D strQuoteLib     s             10    inz
     D strProgram      s             10    inz
     D isLibInLibl     s              1n   inz
     D isOptNotAvail   s              1n   inz
     D***  End   Add  ***  01/24/14  *************************************
     D SCR             S              1    DIM(10) CTDATA PERRCD(1)             SCRAMBLE
     D***  Begin Add  ***  10/20/16  *************************************
     D saveInParm      s           3250    inz
     D***  End Add ******  10/20/16  *************************************
     D                 DS
     D  XXDATE                 1      7  0
     D  XXA                    1      1  0
     D  XXB                    2      2  0
     D  XXC                    3      3  0
     D  XXD                    4      4  0
     D  XXE                    5      5  0
     D  XXF                    6      6  0
     D  XXG                    7      7  0
     D                 DS
     D  SCRMDT                 1      7  0
     D  TMPA                   1      1  0
     D  TMPB                   2      2  0
     D  TMPC                   3      3  0
     D  TMPD                   4      4  0
     D  TMPE                   5      5  0
     D  TMPF                   6      6  0
     D  TMPG                   7      7  0
      *** beg add ***** 07/01/16 **********
     d svlda           ds          1024
     c     *dtaara       define    *lda          svlda
     c     *lock         in        *dtaara
     c                   clear                   svlda
     c                   out       *dtaara
      *** end add ***** 07/01/16 **********
      /Free
       //***  Begin Add  ***  01/24/14  ***********************************
       DevSpecificTask('Add CLTID library');
       //***  End   Add  ***  01/24/14  ***********************************
       Monitor;
         If %Parms > 2;
           dsJobInf = inJobInf;
         EndIf;
         If %Parms > 3;
           flgMode = inMode;
         EndIf;

       //flgMode = 'D';

         If flgMode = 'D';
           scJob = 'This Job(' + %char(Q1JOBN) +
            '/' + %trim(Q1USER) + '/' + %trim(Q1JOB)
            + ')';
           scStatus = 'In debug mode, press enter to run...';
           exfmt scr01;
         EndIf;
       //***  Begin Add  ***  01/24/14  ***********************************
         RtvObjectLib('MLIVXA':'*DTAARA':strLiveLib);
         RtvObjectLib('MQTEXA':'*DTAARA':strQuoteLib);
         strProgram = inProgram;
         Select;
         When strProgram = 'STDRVPGM';
            strProgram = %subst(inParm:19:10);
         Endsl;

       //***  Beg   Chg  ***  04/02/14  ***********************************
         strUserLibl = RtvUserLibl(strJobName:strUserName:strJobNumber);
         If not (strUserLibl = 'X' or strUserLibl = ' ');
           inCmd = 'CHGLIBL LIBL(' + %Trim(strUserLibl) + ')';
           RunCmd(inCmd:%len(inCmd));
         Else;
           SetInitLibl(strEnv:' ':' ':' ':' ':' ':' ':' ');
         EndIf;

         If flgMode = 'Q';
            callp WtQteLib();
         EndIf;
       //***  End   Chg  ***  04/02/14  ***********************************

         CheckObjInLIBL(inProgram:'*PGM':isOptNotAvail);
         Select;
         When strEnv = 'QTE' and strLiveLib <> strQuoteLib;
            scStatus = 'Option is not available from this environment';
            scJob = 'Quote environment is not available.';
            Exfmt scr01;
            *inlr = *On;
            Return;
         When isOptNotAvail;
            scStatus = 'Option is not available from this environment';
            scJob = 'Program is not found in LIBL.  Possible client mod.';
            Exfmt scr01;
            *inlr = *On;
            Return;
         Endsl;
       //***  End   Add  ***  01/24/14  ***********************************

       //***  Beg   Chg  ***  04/02/14  ***********************************
       //  strUserLibl = RtvUserLibl(strJobName:strUserName:strJobNumber);
       //  If not (strUserLibl = 'X' or strUserLibl = ' ');
       //    inCmd = 'CHGLIBL LIBL(' + %Trim(strUserLibl) + ')';
       //    RunCmd(inCmd:%len(inCmd));
       //  Else;
       //    SetInitLibl(strEnv:' ':' ':' ':' ':' ':' ':' ');
       //  EndIf;
       //
       //  If flgMode = 'Q';
       //     callp WtQteLib();
       //  EndIf;
       ////***  End   Chg  ***  04/02/14  ***********************************

         endPos=%Scan('*End' :inParm) - 1;
         If endPos > 0;
           inParm = %Subst(inParm:1:endPos);
         Else;
           inParm = '()';
         EndIf;
       //***  BegAdd  *****  10/20/16  ***********************************
            saveInParm = inParm;
       //***  EndAdd  *****  10/20/16  ************************************

       //***  Begin Add  ***  01/24/14  ***********************************
       // Add CLTID library back, if required
         CheckLibInLibl(strLiveLib:'*ALL':'0':isLibInLibl);
         If not isLibInLibl;
            RunCmd('ADDLIBLE ' + %trim(strLiveLib) +
             ' POSITION(*AFTER QTEMP)':45);
         Endif;
       //***  End   Add  ***  01/24/14  ***********************************

         // Update user count file before calling progarm
         Exsr UpdateUserCounts;
       //***  BegAdd ***** 10/20/16 ****************************************
         inParm = saveInParm;
       //***  EndAdd ***** 10/20/16 *   ***********************************
         inCmd = 'Call ' + %trim(inProgram) + ' ' + %trim(inParm);
         RunCmd(inCmd :%len(inCmd));

         If flgMode = 'D';
           scStatus = 'Request completed, press enter to exit...';
           exfmt scr01;
         EndIf;

         *inlr = *On;
         Return;
       On-Error;
           scJob = 'This Job(' + %char(Q1JOBN) +
            '/' + %trim(Q1USER) + '/' + %trim(Q1JOB)
            + ')';
           scStatus = 'Unexpected error occurred, check joblog';
           exfmt scr01;
         *inlr = *On;
         Return;
       EndMon;
       //************************************************
       // Update User counts
       //************************************************
       BegSr UpdateUserCounts;
       //*****Begin Chg *** 10/20/16  ***************************
         Open smupl;
       //*****End Chg  ***  10/20/16  ***************************
         Chain q1user pup01;
         If %found(smupl);
           Open smwup;
           Chain q1user pwu0p;
           day# = dow(dateTo8(systemDate()));
           pastDate = dtPast(dateTo8(systemDate()): day#);
           xxDate = dtCYMD(pastDate);
           Exsr Scram;
           WUwcdt = scrmdt;
           clear scrmdt;
           clear xxdate;
           xxdate = dateTo7(systemDate());
           Exsr Scram;
           WUisdt = scrmdt;
           WUdywk = %Int(SCR(day#));
           WUusr = q1user;
           WUuser = q1user;
           WUpgm = q1pgm;
           WUdt = dateTo7(systemDate());
           WUtime = timeTo6(systemTime());
           If %found(smwup);
             Update pwu0p;
           Else;
             WUiusr = WUuser;
             WUipgm = WUpgm;
             WUidt = WUdt;
             WUitim = WUtime;
             Write pwu0p;
           Endif;
           Close smwup;
         //***  Begin Chg  ***  03/15/18  *********************************
         //UPdt1 = dtCYMD(pastDate);
           UPdt1 = DateTo7(SystemDate());
         //***  End   Chg  ***  03/15/18  *********************************
           Update pup01;
         Endif;
       //*****Begin Chg *** 10/20/16  ***************************
         Close smupl;
       //*****End Chg *** 10/20/16  ***************************
       EndSr;
      /End-Free
     C*====================================================================
     C****Calculation of scramble date
     C*====================================================================
     C     SCRAM         BEGSR
     C***INTIALIZE
     C                   MOVE      00            X@A               2 0
     C                   MOVE      00            X@B               2 0
     C                   MOVE      00            X@C               2 0
     C                   MOVE      00            X@D               2 0
     C                   MOVE      00            X@E               2 0
     C                   MOVE      00            X@F               2 0
     C                   MOVE      00            X@G               2 0
     C***
     C                   MOVE      XXA           X@A               2 0
     C                   MOVE      XXB           X@B               2 0
     C                   MOVE      XXC           X@C               2 0
     C                   MOVE      XXD           X@D               2 0
     C                   MOVE      XXE           X@E               2 0
     C                   MOVE      XXF           X@F               2 0
     C                   MOVE      XXG           X@G               2 0
     C     X@A           IFEQ      00
     C                   MOVE      10            X@A
     C                   ENDIF
     C     X@B           IFEQ      00
     C                   MOVE      10            X@B
     C                   ENDIF
     C     X@C           IFEQ      00
     C                   MOVE      10            X@C
     C                   ENDIF
     C     X@D           IFEQ      00
     C                   MOVE      10            X@D
     C                   ENDIF
     C     X@E           IFEQ      00
     C                   MOVE      10            X@E
     C                   ENDIF
     C     X@F           IFEQ      00
     C                   MOVE      10            X@F
     C                   ENDIF
     C     X@G           IFEQ      00
     C                   MOVE      10            X@G
     C                   ENDIF
     C                   MOVE      SCR(X@A)      TMPA
     C                   MOVE      SCR(X@B)      TMPB
     C                   MOVE      SCR(X@C)      TMPC
     C                   MOVE      SCR(X@D)      TMPD
     C                   MOVE      SCR(X@E)      TMPE
     C                   MOVE      SCR(X@F)      TMPF
     C                   MOVE      SCR(X@G)      TMPG
     C                   ENDSR
**     SCR
4
7
0
3
6
9
2
5
8
1
