     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      *---------------------------------------------------------------------
     h/title STIDARTRST - iDARTS record restriction check
     h dftname(STIDARTRST)
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
     h/DEFINE   PROFOUNDUI

      *  Called from: iDARTS
      *  Created on 03/12/2015 by C:279    DR337
      *---------------------------------------------------------------------
      *  Revisions:
      *
      *  10/01/15  C:180 Support 37507
      *                  Check fund authority when dealing with agents.
      *
      *  06/01/15  C:33  Project: DR337   34884
      *                 -Add user parameter entry
      *                 -Use entry instead of q1User, if entered.
      *
      *  06/10/15  C:306 Project: DR337 Priss 17
      *                 -To fix the priss item 17
      *
      *---------------------------------------------------------------------
      *  Note: Please add comments to the top of Revisions.
      *---------------------------------------------------------------------

     fsmkdl     if   e           k disk    rename(pkd01:pkd00)
     fsmazl     if   e           k disk    rename(paz01:paz00)
     fwmupl     if   e           k disk    rename(pup01:pup00) usropn

     d psds          esDs                  extname(szq1p)

      * work variables
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
     d charApp         s              1
     d charAuth        s              1
     d charUser        s             10    inz
     d intCo           s              3  0
     d intFnd          s              3  0
     d intSub          s              5  0
     d intClaim        s              7  0
     d intFyr          s              3  0
     d intEmp          s              9  0
     d intDiv          s              5  0 inz
     d intGrpRstCnt    s              5  0 inz
     d isAuthority     s               n
     d isAuth          s               n
     d ReturnCode      s              1

      * Claims DARTS Key data structure
     d claimsKey       ds            40
     d  charCo                 1      3
     d  charFnd                4      6
     d  charYr                 7      9
     d  charClaim             10     16

      * ManagedCare DARTS Key data structure
     d mngdCrKey       ds            40
     d  charCo#                1      3
     d  charSub                4      8
     d  charFnd#               9     11
     d  charYr#               12     14
     d  charClaim#            15     21

      * Policy DARTS Key data structure
     d policyKey       ds            40
     d  charCo##               1      3
     d  charFnd##              4      6
     d  charEmp                7     15
     d  charDiv               16     20
     d  charQteFlg            40     40

      * Agency DARTS Key data structure
     d agentKey        ds            40
     d  charFein               1      9
     d  charMod               10     12

      * Proto-types

     d GetRstrct       pr                  extpgm('WA067R')
     d ##co                           3
     d ##fnd                          3
     d ##fyrc                         3
     d ##casec                        7
     d ##user                        10
     d ##rtcd                         1

     d GetAuth         pr                  extpgm('SA008R')
     d ##co#                          3  0 const
     d ##user                        10    const
     d ##appl                         1    const
     d ##rtcd                         1
     d ##auth                         1          options(*nopass)
     d ##grp                          3  0 const options(*nopass)
     d ##emp#                         9  0 const options(*nopass)
     d ##sub#                         5  0 const options(*nopass)

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

     d/copy sPrString
     d/copy wPrGroup
     d/copy WprEmpLr
     d/copy sPrAtrium
     d/copy wPrUsrPrms

      *Entry of Program.
     d stiDartRst      pr                  extpgm('STIDARTRST')
     d                               40
     d                               10
     d                               10
     d                                1n

     d stiDartRst      pi
     d @@key                         40
     d @@pgm                         10
     d @@user                        10
     d @@rtcd                         1n

      /free

       // Process - Main processing
       charUser = @@user;
       if charUser = *blank;
          charUser = q1User;
       Endif;

       //  read key definition file
       ReturnCode  = ' ';
       isAuthority = *off;
       isAuth = *off;
       @@rtcd = *off;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);

       // get the group restriction count
       Exsr GetGrpRstCnt;

       Chain @@pgm pkd00;
       If %found;
          Select;

          When KDcvtP = 'CR001R' and %subst(@@pgm:1:1) = 'W';
             claimsKey = @@key;
             Exsr processClaims;
          When KDcvtP = 'CR001R';
             mngdCrKey = @@key;
             Exsr processMngdCr;
          When KDcvtP = 'CR002R';
             policyKey = @@key;
             Exsr processPolicy;
          When KDcvtP = 'CR003R';
             agentKey = @@key;
             Exsr processAgent;
          Endsl;
       Endif;

       If isAuth = *on;
          @@rtcd = *on;
       Endif;

       If ##WCif = 'Y';
          close WMUPL;
       Endif;

       *inlr = *On;

       //===================================================================
       // processClaims
       //===================================================================
       Begsr processClaims;

       intCo = %int(charCo);
       charApp = 'W';
       charAuth = ' ';
       intFnd = %int(charFnd);

       //*** Beg chg ******** 06/10/15 *************
       // If ##WCif = 'Y' and UPxFnd = 'Y' and UPfnd = intFnd and intGrpRstCnt = 0;
       If ##WCif = 'Y' and UPxFnd = 'Y' and intGrpRstCnt = 0;
           If UPfnd = intFnd;
               isAuth = *on;
           EndIf;
       //*** End chg ******** 06/10/15 *************
       Else;
          GetAuth(intCo:charUser:charApp:isAuthority:charAuth:intFnd);
          If isAuthority = *off;
             isAuth = *on;
             GetRstrct(charCo:charFnd:charYr:charClaim:charUser:ReturnCode);

             If ReturnCode = '2';
                isAuth = *off;
             Endif;
          Endif;
       Endif;

       Endsr;
       //===================================================================
       // processMngdCr
       //===================================================================
       Begsr processMngdCr;

       intCo = %int(charCo#);
       charApp = 'M';
       charAuth = ' ';
       intFnd = %int(charFnd#);
       intSub = %int(charSub);
       intEmp = 0;

       GetAuth(intCo:charUser:charApp:isAuthority:charAuth:intFnd:
        intEmp:intSub);
       If isAuthority = *off;
          isAuth = *on;
       Endif;

       Endsr;
       //===================================================================
       // processPolicy
       //===================================================================
       Begsr processPolicy;

       intCo = %int(charCo##);
       charApp = 'W';
       charAuth = ' ';
       intFnd = %int(charFnd##);
       intEmp = %int(charEmp);

       //*** Beg chg ******** 06/10/15 *************
       // If ##WCif = 'Y' and UPxFnd = 'Y' and UPfnd = intFnd and intGrpRstCnt = 0;
       If ##WCif = 'Y' and UPxFnd = 'Y' and intGrpRstCnt = 0;
          If UPfnd = intFnd;
            isAuth = *on;
          EndIf;
       //*** End chg ******** 06/10/15 *************
       Else;
          GetAuth(intCo:charUser:charApp:isAuthority:charAuth:intFnd:intEmp);
          If isAuthority = *off;
             isAuth = *on;
          Endif;
       Endif;

       Endsr;
       //===================================================================
       // processAgent
       //===================================================================
       Begsr processAgent;
       // Begin Add  **** 10/01/15 *************************************************

       intCo = UPco#;
       charApp = 'W';
       charAuth = ' ';
       intFnd = UPfnd;
       intEmp = 0;

       If ##WCif = 'Y' and UPxFnd = 'Y' and intGrpRstCnt = 0;
          If UPfnd = intFnd;
            isAuth = *on;
          EndIf;
       Else;
          GetAuth(intCo:charUser:charApp:isAuthority:charAuth:intFnd:intEmp);
          If isAuthority = *off;
             isAuth = *on;
          Endif;
       Endif;
       // End    Add  **** 10/01/15 *************************************************

       Endsr;
       //===================================================================
       // GetGrpRstCnt
       //===================================================================
       Begsr GetGrpRstCnt;

       intGrpRstCnt = 0;

       If ##WCif = 'Y';
          Open WMUPL;

          Chain charUser pup00;
          If %found(WMUPL);

             Setll (UPco#:charUser) paz00;
             Reade (UPco#:charUser) paz00;
             Dow not %eof(SMAZL);
                If AZstat <> 'I' and AZappl = 'W';
                   intGrpRstCnt = intGrpRstCnt + 1;
                Endif;
                Reade (UPco#:charUser) paz00;
             Enddo;
          Endif;
       Endif;

       Endsr;

      /end-free

