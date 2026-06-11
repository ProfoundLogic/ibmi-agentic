       CTL-OPT COPYRIGHT('(c) 2024 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  WTPOLAUDRV - Audit Reversal                                    *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This program will perform the Audit Reversal on the policy and *
       //  and Write the log records.                                     *
       //                                                                 *
       //  Created: 08/01/24 C:291  Project:  WC2976                      *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //                                                                 *
       //******************************************************************
       Dcl-Pi *N;
        inRRN packed (9:0);
       End-Pi;

       Dcl-F wdelp  Disk Usage(*Input)  Rename(pel01:pel0p) prefix(E1:2);
       Dcl-F wdell1 Disk Usage(*Input:*Update) Keyed Rename(pel01:pel01l);
       Dcl-F wdehl6 Disk Usage(*Input:*Delete) Keyed Rename(peh01:peh06);
       Dcl-F sdacl  Disk Usage(*Input) Keyed Rename(pac01:pac00);
       Dcl-F wxarvp Disk Usage(*Output) Rename(par01:par0p);
       Dcl-F wmupl  Disk Usage(*Input)  Keyed;

       //Copy Books
      /Free
      /Copy sPrDate
      /End-Free

       //Program Status Data Structure
       Dcl-Ds SZQ1P ext psds;
       End-Ds;

       Dcl-Ds dsTranDate;
        tranDate   Zoned(7:0) Pos(1);
        tranYrMnth Zoned(5:0) Pos(1);
        tranDays   Zoned(2:0) Pos(6);
       End-Ds;

       Dcl-Ds dsCurrentDate;
        currentDate Zoned(7:0) Pos(1);
        sysYrMnth   Zoned(5:0) Pos(1);
        sysDays     Zoned(2:0) Pos(6);
       End-Ds;

       Dcl-Pr Darts Extpgm('DA700R');
        ##key  Char(40);
        ##comm Char(256);
        ##cDat Char(8);
        ##dcId Char(12);
        ##pNam Char(10);
        ##secl Char(3);
        ##user Char(10);
        ##pgm  Char(10);
        ##fold Char(80);
        ##diFl Char(1);
        ##iPgm Char(10);
        ##fl01 Char(1);
        ##fl02 Char(1);
        ##rmCd Char(5);
        ##seq# Char(5);
        ##sbyl Char(1);
        ##com2 Char(19);
        ##apId Char(2);
        ##iDoc Char(12);
        ##iQue Char(10);
        ##imgF Char(1);
        ##iPth Char(10);
        ##pIdx Char(5);
        ##qPty Char(1);
        ##dcTp Char(5);
        ##csId Char(2);
        ##code Char(2);
       End-Pr;


       //Stand Alone
       Dcl-S ##key  Char(40);
       Dcl-S ##comm Char(256);
       Dcl-S ##cDat Char(8);
       Dcl-S ##dcId Char(12);
       Dcl-S ##pNam Char(10);
       Dcl-S ##secl Char(3);
       Dcl-S ##user Char(10);
       Dcl-S ##pgm  Char(10);
       Dcl-S ##fold Char(80);
       Dcl-S ##diFl Char(1);
       Dcl-S ##iPgm Char(10);
       Dcl-S ##fl01 Char(1);
       Dcl-S ##fl02 Char(1);
       Dcl-S ##rmCd Char(5);
       Dcl-S ##seq# Char(5);
       Dcl-S ##sbyl Char(1);
       Dcl-S ##com2 Char(19);
       Dcl-S ##apId Char(2);
       Dcl-S ##iDoc Char(12);
       Dcl-S ##iQue Char(10);
       Dcl-S ##imgF Char(1);
       Dcl-S ##iPth Char(10);
       Dcl-S ##pIdx Char(5);
       Dcl-S ##qPty Char(1);
       Dcl-S ##dcTp Char(5);
       Dcl-S ##csId Char(2);
       Dcl-S ##code Char(2);

       Dcl-S strKey char(10);
       Dcl-S currDate Zoned(7:0);
       Dcl-S currTime Zoned(6:0);
       Dcl-S flgEHistDltd char(1);
       Dcl-S flgEstBillRan char(1);


       // Main Procesing
       currentDate = DateTo7(systemDate());
       currTime = %Dec(%Char(%Time():*iso0):6:0);
       currDate = %Dec(%Char(%Date():*cymd0):7:0);

       Chain Q1user PUP01;
       Chain inRRN Pel0p;

       Setll (E1co#:E1fnd:E1fyr:E1emp#) pel01l;
       Reade (E1co#:E1fnd:E1fyr:E1emp#) pel01l;
       Dow Not %Eof(wdell1);
          If ELFAF = 'A';
             //Set all the Audit Premium fields to zero and ELFAF = 'E'(Estimated) or 'R'(Revised)
             Exsr SetAudFields;

             //Delete Audit records from Employer Detail History for current system Month/Year
             Exsr DltEmpDtlHist;

             //Write iDarts Remark for Audit Reversal
             Exsr WriteDART;

             //Write Audit Reversal log
             Exsr WriteAudRvsLog;

          Endif;
          Reade (E1co#:E1fnd:E1fyr:E1emp#) pel01l;
       Enddo;


       *InLr = *on;

       //*--------------------------------------------------------------------
       //* DltEmpDtlHist - Delete Audit records from Employer Detail History *
       //* for current system Month/Year                                     *
       //*--------------------------------------------------------------------
       Begsr DltEmpDtlHist;

       flgEHistDltd = 'N';
       Setgt  (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh06;
       Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh06;
       Dow not %Eof(wdehl6);
          tranDate = EHtrDt;
          If EHFAF = 'A' and tranYrMnth = sysYrMnth;
             flgEHistDltd = 'Y';
             Delete peh06;
          Endif;
          Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh06;
       Enddo;

       Endsr;

       //*----------------------------------------------------------------
       //* SetAudFields - Set all of the Audit Premium fields to zero    *
       //*----------------------------------------------------------------
       Begsr SetAudFields;

       flgEstBillRan = 'N';
       //Set ELFAF to 'E'(Estimated)
       ELFAF = 'E';
       //Set ELFAF to 'R'(Revised) if Revised Bill ran prior to running Audit bill
       Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh06;
       Dou %Eof(wdehl6);
          Readpe(n) (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh06;
          If %Eof;
             Leave;
          EndIf;
          If EHFAF <> 'A';
             If EHFAF = 'R';
                ELFAF = 'R';
             Endif;
             flgEstBillRan = 'Y';
             Leave;
          Endif;
       Enddo;
       //If Eatimated Bill was not run prior to running Audit Bill
       If flgEstBillRan = 'N';
          ELFAF = ' ';
       Endif;

       Clear ELamnp;
       Clear ELampr;
       Clear ELasbj;
       Clear ELaspr;
       Clear ELampm;
       Clear ELanmp;
       Clear ELaapr;
       Clear ELadc1;
       Clear ELadc2;
       Clear ELadc3;
       Clear ELadv2;
       Clear ELaxp1;
       Clear ELaxp2;
       Clear ELaxp3;
       Clear ELain1;
       Clear ELain2;
       Clear ELadct;
       Clear ELadcm;
       Clear ELadcs;
       Clear ELadcr;
       Clear ELadc;
       Clear ELafrs;
       Clear ELawav;
       Clear ELatca;
       Clear ELater;
       Clear ELamna;
       Clear ELadd2;
       Clear ELadtr;
       Clear ELaarc;
       Clear ELahcn;
       Clear ELapf;

       Update pel01l;

       Endsr;

       //*--------------------------------------------------------------------
       //* WriteAudRvsLog - Write Audit Reversal Log                         *
       //*--------------------------------------------------------------------
       Begsr WriteAudRvsLog;

       ARvuser = Q1user;
       ARvdt   = currDate;
       ARvtime = currTime;
       ARvco#  = ELco#;
       ARvfnd  = ELfnd;
       ARvemp# = ELemp#;
       ARvdiv  = ELdiv;
       ARvfyr   = ELfyr;
       ARvprmhst = flgEHistDltd;
       Write par0p;

       Endsr;

       //*----------------------------------------------------------------
       //* WriteDART - Write DARTS remark for Audit Reversal             *
       //*----------------------------------------------------------------
       Begsr WriteDART;

       strKey = 'AudReverse';
       Chain strKey pac00;

       ##key = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
        %EditC(ELemp#:'X') + %EditC(ELdiv:'X');
       ##comm = 'An Audit Reversal was requested by ' + %Trim(UPUSNM) +
        ' on ' + %Char(%Date():*USA) + ' for policy year ' + %Char(E1Fyr) + '.';
       ##cdat = %Editc(dtmdyy(Dateto7(SystemDate())):'X');
       ##dcid = *blanks;
       ##pnam = Q1Pgm;
       ##secl = '000';
       ##user = UPusr;
       ##pgm  = Q1Pgm;
       ##fold = *blanks;
       ##difl = 'Y';
       ##iPgm = Q1Pgm;
       ##fl01 = ' ';
       ##fl02 = ' ';
       ##rmcd = ACrmcd;
       ##seq# = '00000';
       ##sbyl = 'B';
       ##com2 = ' ';
       ##apId = ' ';
       ##iDoc = ' ';
       ##iQue = ' ';
       ##imgF = 'O';
       ##iPth = ' ';
       ##pIdx = ' ';
       ##qPty = ' ';
       ##dcTp = ' ';
       ##csId = ' ';
       ##code = ' ';

       Darts(##key:##comm:##cDat:##dcId:##pNam:##secl:##user:
        ##pgm:##fold:##diFl:##iPgm:##fl01:##fl02:##rmCd:##seq#:
        ##sbyl:##com2:##apId:##iDoc:##iQue:##imgF:##iPth:##pIdx:
        ##qPty:##dcTp:##csId:##code);

       Endsr;

