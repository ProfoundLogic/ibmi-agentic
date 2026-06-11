     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h/title   FixTerRt
      * This program looks at policies that have billed since 07/01/13 and
      * verifies that the terrorism charge is correct. If not, it writes
      * a record to the FIXTERRT file.
      *===============================================================
      * co/fnd/emp/div/fyr/trdt
     fwdehl6    if   e           k disk
      * co/fnd/emp/div/fyr
     fwdell     if   e           k disk
      * co/fnd/st/efdt
     fwdrrl     if   e           k disk
      * co/fnd/fyr/emp/div/clas
     fwmeple    if   e           k disk
      * co/rtcd/clas/efdt
     fwmprl1    if   e           k disk
      * co/rtcd
     fwmrsl     if   e           k disk
     fwmeml     if   e           k disk
     ffixterrt  uf a e             disk
      *===============================================================
     dcalcTerrPrem     s             11  0
     dcurrTerrPrem     s                   like(ELeter)
     drateDate         s                   like(RRefdt)
      *===============================================================
     dCalcTerrChg      pr                  like(ELeter)
     dGetTerrorRates   pr
     d payRate                        7  4
     d nonPayRate                     7  4
     dGetTerrPrem      pr                  like(ELeter)
     d payRate                        7  4
     d nonPayRate                     7  4
     dGetLCM           pr             5  4
     dGetPerCapitaFlg  pr                  like(PRpcf)
     dGetRateDate      pr                  like(ELpfdt)
     dGetRateState     pr                  like(RSjrst)
     dGetDBAName       pr                  like(EMname)
     dWriteError       pr
     d/copy sPrRunCmd
      *===============================================================
      /free
       // Clear error file
       Read per01;
       dow not(%eof);
       Delete per01;
       Read per01;
       Enddo;
       // Main
       Read peh01;
       Dow not(%eof);
       // Continue reading if billed before 09/01/13
       If EHtrdt < 1130901;
          Read peh01;
          Iter;
       Endif;
       // Get terror chg from WDELP
       Chain (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pel01;
       If not(%found);
          Read peh01;
          Iter;
       Endif;
       If ELfaf = 'A';
          currTerrPrem = ELater;
       Else;
          currTerrPrem = ELeter;
       Endif;
       // If no terror premium is record in WDELP,
       //  iterate to next policy
       If currTerrPrem = 0;
          Setgt (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh01;
          Read peh01;
          Iter;
       Endif;
       // Write error if the terror chg is incorrect
       //  and iterate to next policy
       eval(h) calcTerrPrem = CalcTerrChg();
       //If %abs(calcTerrPrem - currTerrPrem) > 1;
          WriteError();
          Setgt (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh01;
       //Endif;
       Read peh01;
       Enddo;
       *inlr = *on;
       Return;
      /end-free
      *======================================================
      * Calculate Terror Charge
      *======================================================
     pCalcTerrChg      b
     dCalcTerrChg      pi                  like(ELETER)
     dsaveEfdt         s                   like(EPEFDT)
     dpayRate          s              7  4
     dnonPayRate       s              7  4
     dterrorChg        s                   like(ELeter)
      /free
       Setll (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pep01;
       Reade (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pep01;
       Dow not(%eof);
          If EPstat <> 'A';
             Reade (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pep01;
             Iter;
          Endif;
          If EPefdt <> saveEfdt;
             saveEfdt = EPefdt;
             rateDate = GetRateDate();
             GetTerrorRates(payRate:nonPayRate);
          Endif;
          terrorChg = terrorChg +
           GetTerrPrem(payRate:nonPayRate);
       Reade (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pep01;
       Enddo;
       Return terrorChg;
      /end-free
     pCalcTerrChg      e
      *======================================================
      * Get Terror Rates
      *======================================================
     pGetTerrorRates   b
     dGetTerrorRates   pi
     d payRate                        7  4
     d nonPayRate                     7  4
     drateState        s                   like(RRst)
     dlossCostMult     s              5  4
      /free
       rateState = GetRateState();
       Setgt (EPco#:EPfnd:rateState:rateDate) prr01;
       Readpe (EPco#:EPfnd:rateState) prr01;
       lossCostMult = GetLCM();
       eval(h) payRate    = RRterr * lossCostMult;
       eval(h) nonPayRate = RRter2 * lossCostMult;
       Return;
      /end-free
     pGetTerrorRates   e
      *======================================================
      * Get Terror Premium
      *======================================================
     pGetTerrPrem      b
     dGetTerrPrem      pi                  like(ELeter)
     d payRate                        7  4
     d nonPayRate                     7  4
     dterrorPrem       s                   like(ELeter)
     dperCapitaFlag    s                   like(PRpcf)
     dpayRollAmt       s                   like(EPaapr)
     dmanPremium       s                   like(EPampr)
      /free
       perCapitaFlag = GetPerCapitaFlg();
       If ELfaf = 'A';
          payRollAmt = EPaapr;
          manPremium = EPampr;
       Else;
          payRollAmt = EPeapr;
          manPremium = EPempr;
       Endif;
       If perCapitaFlag = 'P';
          payRollAmt = payRollAmt/100;
       Endif;
       If perCapitaFlag = 'O';
          terrorPrem = manPremium * nonPayRate;
       Else;
          terrorPrem = payRollAmt * payRate;
       Endif;
       Return terrorPrem/100;
      /end-free
     pGetTerrPrem      e
      *======================================================
      * Get Per Capita Flag
      *======================================================
     pGetPerCapitaFlg  b
     dGetPerCapitaFlg  pi                  like(PRpcf)
      /free
       Setgt (EPco#:ELrtcd:EPclas:rateDate) ppr01;
       Reade (EPco#:ELrtcd:EPclas:rateDate) ppr01;
       Return PRpcf;
      /end-free
     pGetPerCapitaFlg  e
      *======================================================
      * Write Error
      *======================================================
     pWriteError       b
     dWriteError       pi
      /free
       ERco#  = EPco#;
       ERfnd  = EPfnd;
       ERemp# = EPemp#;
       ERdiv  = EPdiv;
       ERfyr  = EPfyr;
       ERname = GetDBAName();
       ERcurt = currTerrPrem;
       ERcalt = calcTerrPrem;
       Write per01;
       Return;
      /end-free
     pWriteError       e
      *======================================================
      * Get Rate Date
      *======================================================
     pGetRateDate      b
     dGetRateDate      pi                  like(ELpfdt)

     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5

     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS

     C                   MOVE      ##DATE        RATEDATE
     C                   RETURN    RATEDATE
     pGetRateDate      e
      *======================================================
      * Get Rate State
      *======================================================
     pGetRateState     b
     dGetRateState     pi                  like(RRst)
      /free
       Chain (ELco#:ELrtcd) prs01;
       Return RSjrst;
      /end-free
     pGetRateState     e
      *======================================================
      * Get DBA Name
      *======================================================
     pGetDBAName       b
     dGetDBAName       pi                  like(EMname)
      /free
       Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
       Return EMname;
      /end-free
     pGetDBAName       e
      *======================================================
      * Get LCM
      *======================================================
     pGetLCM           b
     dGetLCM           pi             5  4
     C                   Z-ADD     1             XXLCM
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   RRTERC        QQCLAS
     C                   MOVE(P)   RATEDATE      QQEFDT
     C                   CALL      'SRLCM'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            3
     C                   PARM                    QQCLAS            5
     C                   PARM                    QQEFDT            7
     C                   PARM      *BLANKS       QQLCM             5
      *
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
     C                   RETURN    XXLCM
     pGetLCM           e
