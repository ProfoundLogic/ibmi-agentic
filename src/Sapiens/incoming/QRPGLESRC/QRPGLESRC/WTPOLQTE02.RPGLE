     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve billing division information - 2nd screen
     h*               billing division grid.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 06/15/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwdelp     if   e             disk    rename(pel01:pel0p)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     FWTABL     IF   E           K DISK
     FWMLDL1    IF   E           K DISK    rename(PLD01:PLD01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DQuoteGrid02      pr                  ExtPgm('WTPOLQTE02')
     D                                9  0
     D                            32767
     D                                1
      *
     DQuoteGrid02      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d x#Rrn           s              9  0
     d xxAgg           s             20
     d xxDed           s             20
     d xxPlan          s             50
     d xxFac           s             20
     d xxKey           s             10

     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C*
     C* Large Deduc File Key List
     C*
     C     keyWMLDL1     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C                   Kfld                    ELFyr
     C*
     C* Billing Schedule Code File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    xxKey

      *
      /Free

       Chain(n) inRRN pel0p;
       Chain(n) keyWMEML PEM00;

       outJSON = ' ';

       BuildJSON(outJSON :'OutDiv2DetDivision' :%editc(EMDiv: 'X'));
       BuildJSON(outJSON :'OutDiv2DetPolEffDt' :
        %EditW(dtMDYY(ELPfdt):'  /  /    '));
       BuildJSON(outJSON :'OutDiv2DetPolExpDt' :
        %EditW(dtMDYY(ELPtdt):'  /  /    '));
       BuildJSON(outJSON :'OutDiv2DetAnnivDate' :
        %EditW(dtMDYY(ELAndt):'  /  /    '));
       BuildJSON(outJSON :'OutDiv2DetInceptDate' :
        %editw(dtMDYY(GetEmIncDt(ELCo#:ELFnd:ELEmp#:ELDiv)): '  /  /    '));
       BuildJSON(outJSON :'OutDiv2DetCXDate' :
        %editw(dtMDYY(GetCXDate(ELCo#:ELFnd:ELEmp#:ELDiv)): '  /  /    '));
       BuildJSON(outJSON :'OutDiv2DetFEIN' :%editc(EMFein: 'X'));
       BuildJSON(outJSON :'OutDiv2DetRateCode' :ELRTCD);
       BuildJSON(outJSON :'OutDiv2DetRateSt' :
        GetRatingState(ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr));
       If EMSTRP = 'Y';
          BuildJSON(outJSON :'OutDiv2DetSTRP' :'Yes');
       Else;
          BuildJSON(outJSON :'OutDiv2DetSTRP' :'No');
       Endif;

       BuildJSON(outJSON :'OutDiv2DetDBAName' :
        GetEmpFullName(ELEmp#:ELDiv:x#RRN:ELFnd:ELCo#:'DBA'));
       If EMFDNM <> *blanks;
          BuildJSON(outJSON :'OutDiv2DetFedName' :
           GetEmpFullName(ELEmp#:ELDiv:x#RRN:ELFnd:ELCo#:'FED'));
       Else;
          BuildJSON(outJSON :'OutDiv2DetFedName' :' ');
       Endif;

       If EMPAD1 <> *blanks;
          BuildJSON(outJSON :'OutDiv2DetPhysAdr1' :EMPAD1);
          If EMPAD2 = *blanks;
             BuildJSON(outJSON :'OutDiv2DetPhysAdr2' :%trimr(EMPCTY) + ', ' +
              EMPST + ' ' + %editw(EMPZIP:'     -    '));
          Else;
             BuildJSON(outJSON :'OutDiv2DetPhysAdr2' :EMPAD2);
             BuildJSON(outJSON :'OutDiv2DetPhysCSZ' :%trimr(EMPCTY) + ', ' +
              EMPST + ' ' + %editw(EMPZIP:'     -    '));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDiv2DetPhysAdr1' :' ');
          BuildJSON(outJSON :'OutDiv2DetPhysAdr2' :' ');
          BuildJSON(outJSON :'OutDiv2DetPhysCSZ' :' ');
       Endif;

       If EMMAD1 <> *blanks;
          BuildJSON(outJSON :'OutDiv2DetMailAdr1' :EMMAD1);
          If EMMAD2 = *blanks;
             BuildJSON(outJSON :'OutDiv2DetMailAdr2' :%trimr(EMMCTY) + ', ' +
              EMMST + ' ' + %editw(EMMZIP:'     -    '));
          Else;
             BuildJSON(outJSON :'OutDiv2DetMailAdr2' :EMMAD2);
             BuildJSON(outJSON :'OutDiv2DetMailCSZ' :%trimr(EMMCTY) + ', ' +
              EMMST + ' ' + %editw(EMMZIP:'     -    '));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDiv2DetMailAdr1' :' ');
          BuildJSON(outJSON :'OutDiv2DetMailAdr2' :' ');
          BuildJSON(outJSON :'OutDiv2DetMailCSZ' :' ');
       Endif;

       If ELSTAT = 'Y';
          BuildJSON(outJSON :'OutDiv2DetARating' :'Yes');
       Else;
          BuildJSON(outJSON :'OutDiv2DetARating' :'No');
       Endif;

       If ELCOFL = 'Y';
          BuildJSON(outJSON :'OutDiv2DetShort' :'Yes');
       Else;
          BuildJSON(outJSON :'OutDiv2DetShort' :'No');
       Endif;

       Clear xxPlan;
       Clear xxKey;
       xxKey = %editc(ELBscd: 'X');
       Chain keyWTABL PBS01;
       If %found;
          xxPlan = %trimr(xxKey) + ' - ' + BSDesc;
       Endif;

       BuildJSON(outJSON :'OutDiv2DetPayPlan' : xxPlan );

       Clear xxDed;
       Clear xxAgg;
       Chain keyWMLDL1 PLD01;
       If %found;
          xxDed = %editc(LDAmp1:numEdtCde);
          xxAgg = %editc(LDAmg1:numEdtCde);
       Endif;

       BuildJSON(outJSON :'OutDiv2DetLrgDedAmt': xxDed);
       BuildJSON(outJSON :'OutDiv2DetAggAmt': xxAgg);

       Clear xxFac;
       If ELMnfc <> 0;
          xxFac = %editc(ELMnfc:'3');
       Endif;
       BuildJSON(outJSON :'OutDiv2DetRetroMin': xxFac );

       Clear xxFac;
       If ELMxfc <> 0;
          xxFac = %editc(ELMxfc:'3');
       Endif;
       BuildJSON(outJSON :'OutDiv2DetRetroMax': xxFac );

       Clear xxFac;
       If ELLcvf <> 0;
          xxFac = %editc(ELLcvf:'3');
       Endif;
       BuildJSON(outJSON :'OutDiv2DetRetroLossFac': xxFac );


       rtnCode = ' ';
       Return;
      /End-Free

