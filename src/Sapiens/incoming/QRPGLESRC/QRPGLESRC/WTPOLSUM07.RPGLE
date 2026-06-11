     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Current Payroll Information
     h*               detail box - TAB 3.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLSUM
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmepp     if   e             disk    rename(pep01:pep0p)
     fwdell     if   e           k disk    rename(pel01:pel01)
     fwdrrl     if   e           k disk    rename(prr01:prr01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
     d/copy wPrEmpLr

      * *ENTRY
     DPolicySumm07     pr                  ExtPgm('WTPOLSUM07')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm07     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d payEPaSt        s             10
     d ratingState     s              2
     d hdgBrokerFee    s             20
     d hdg1Assessment  s             20
     d hdg2Assessment  s             20
     d hdg3Assessment  s             20
     d hdg4Assessment  s             20
     d hdg5Assessment  s             20
     d hdg6Assessment  s             20
     d hdg7Assessment  s             20
      *
     c* Employer Detail File.
     c     keyWDELL      Klist
     c                   Kfld                    EPco#
     c                   Kfld                    EPfnd
     c                   Kfld                    EPEmp#
     c                   Kfld                    EPdiv
     c                   Kfld                    EPfyr

     c* Employer Detail File.
     c     keyWDRRL      Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ratingState
     c                   Kfld                    ELpfdt

     c     key1WDRRL     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ratingState

      /Free

       Chain(n) inRRN pep0p;

       outJSON = ' ';

       // Populate Payroll Detail.
       BuildJSON(outJSON :'OutEstManPremium' :%Editc(EPempr:numEdtCde));
       BuildJSON(outJSON :'OutEst#Employees':%Editc(EPecnt:numEdtCde));

       If EPSAFL = 'A';
          payEPaSt = 'Audited';
       Else;
          payEPaSt = 'Estimated';
       Endif;
       BuildJSON(outJSON :'OutPayrollStatus':%TrimR(payEPaSt));

       BuildJSON(outJSON :'OutAudManPremium':%Editc(EPampr:numEdtCde));
       BuildJSON(outJSON :'OutAud#Employees':%Editc(EPacnt:numEdtCde));
       BuildJSON(outJSON :'OutAddlLoad':%Editc(EPadld:numEdtCde));

       // Populate Current Assessments / Fees
       Chain(n) keyWDELL pel01;
       ratingState = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv);
       Setgt KeyWDRRL  PRR01;
       Reade Key1WDRRL PRR01;
       If RRsfhd = *Blanks;
          hdgBrokerFee = 'Broker Fee';
       Else;
          hdgBrokerFee = RRsfhd;
       Endif;

       If RR1sth = *Blanks;
          hdg1Assessment = '1st Assessment';
       Else;
          hdg1Assessment = RR1sth;
       Endif;

       If RR2ndh = *Blanks;
          hdg2Assessment = '2nd Assessment';
       Else;
          hdg2Assessment = RR2ndh;
       Endif;

       If RR3rdh = *Blanks;
          hdg3Assessment = '3rd Assessment';
       Else;
          hdg3Assessment = RR3rdh;
       Endif;

       If RR4thh = *Blanks;
          hdg4Assessment = '4th Assessment';
       Else;
          hdg4Assessment = RR4thh;
       Endif;

       If RR5thh = *Blanks;
          hdg5Assessment = '5th Assessment';
       Else;
          hdg5Assessment = RR5thh;
       Endif;

       If RR6thh = *Blanks;
          hdg6Assessment = '6th Assessment';
       Else;
          hdg6Assessment = RR6thh;
       Endif;

       If RR7thh = *Blanks;
          hdg7Assessment = '7th Assessment';
       Else;
          hdg7Assessment = RR7thh;
       Endif;

       BuildJSON(outJSON :'OutAddlLoad':%Editc(ELmfam:numEdtCde));

       BuildJSON(outJSON :'OutBrokerFeeD':%TrimR(hdgBrokerFee));
       BuildJSON(outJSON :'OutBrokerFee':%Editc(ELsfam:numEdtCde));

       BuildJSON(outJSON :'Out1stAssmntD':%TrimR(hdg1Assessment));
       BuildJSON(outJSON :'Out1stAssmnt':%Editc(EL1aam:numEdtCde));

       BuildJSON(outJSON :'Out2ndAssmntD':%TrimR(hdg2Assessment));
       BuildJSON(outJSON :'Out2ndAssmnt':%Editc(EL2aam:numEdtCde));

       BuildJSON(outJSON :'Out3rdAssmntD':%TrimR(hdg3Assessment));
       BuildJSON(outJSON :'Out3rdAssmnt':%Editc(EL3aam:numEdtCde));

       BuildJSON(outJSON :'Out4thAssmntD':%TrimR(hdg4Assessment));
       BuildJSON(outJSON :'Out4thAssmnt':%Editc(EL4aam:numEdtCde));

       BuildJSON(outJSON :'Out5thAssmntD':%TrimR(hdg5Assessment));
       BuildJSON(outJSON :'Out5thAssmnt':%Editc(EL5aam:numEdtCde));

       BuildJSON(outJSON :'Out6thAssmntD':%TrimR(hdg6Assessment));
       BuildJSON(outJSON :'Out6thAssmnt':%Editc(EL6aam:numEdtCde));

       BuildJSON(outJSON :'Out7thAssmntD':%TrimR(hdg7Assessment));
       BuildJSON(outJSON :'Out7thAssmnt':%Editc(EL7aam:numEdtCde));

       rtnCode = ' ';
       Return;
      /End-Free

