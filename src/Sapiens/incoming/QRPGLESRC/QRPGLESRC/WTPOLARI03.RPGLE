     hcopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h*StoneRiver is a trademark of StoneRiver, Inc.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve A/R Transaction Detail - TAB - 3.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLARINQ
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 05/24/23 C:342 Support: 129/108862
     h*                Added batch type on A/R inquiry screen.
     h*
     h* 04/11/22 C:340 Project: WC2663
     h*                Invoice Cloud - Add Invoice Type ID
     h*
     h* 06/03/15 Project: MC396
     h*          Load flexible heading field for Check# or EFT#
     h*
     h*    06/24/14 C:298 Redmine: 4385                               *
     h*             Added Batch# field in AR Detail Inquiry Screen    *
     h*             for posted items.                                 *
     h*
     h**********************************************************************
     fwmeap     if   e             disk    rename(pea01:pea0p)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwtavl     if   e           k disk    rename(pav01:pav01)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fwma3l     If   e           k disk    rename(pa301:pa300)
     fwtabl     if   e           k disk    include(pbs01)
     f                                     include(ptt01)
      *** beg add ***** 05/24/23 *************************
     fwdeal4    if   e           k disk    rename(pea01:pea04)
     f                                     prefix(L4_)
      *** end add ***** 05/24/23 *************************

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr
     d/copy sPrString

      * *ENTRY
     DPolicyInq03      pr                  ExtPgm('WTPOLARI03')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicyInq03      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *

     d numEdtCde       c                   const('N')
     d xxKey           s             10
     d code            s             40
     d paymentPlan     s             60
     d actstatus       s             60
     d userName        s             60
     d pgmName         s             60
     d polStatus       s             20
     d bilStatus       s             20
     d gr3Add1         s             50
     d gr3Add2         s             50
     d gr3Add3         s             50
     d gr3Add4         s             50
     d gr3Add5         s             50
     d gr3Add6         s             50
     d gr3Add7         s             50
     d ##zip           s              9  0
     d adL1            s             40    inz
     d adL2            s             40    inz
     d adL3            s             40    inz
      *** beg add ***** 06/03/15 *************************
     d hdgPayType      s             10
     d clientID        s              3    inz('000')
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3
      *** end add ***** 06/03/15 *************************

      *** beg add ***** 05/24/23 *************************
     d BtchTyp         s             10    inz
      *** end add ***** 05/24/23 *************************

     d* Load Transaction Detail detail info.
     d  LoadTrDtDtl    pr

      *** beg add ***** 05/24/23 *************************
     c     keyWdeal4     Klist
     c                   Kfld                    EAco#
     c                   Kfld                    EAbul1
     c                   Kfld                    EAfnd
     c                   Kfld                    EAemp#
     c                   Kfld                    EAdiv
     c                   Kfld                    EAfyr
     c                   Kfld                    EApodt
      *** end add ***** 05/24/23 *************************

     c* Employer Detail File
     c     keyWDELL1     Klist
     c                   Kfld                    EAco#
     c                   Kfld                    EAfnd
     c                   Kfld                    EAfyr
     c                   Kfld                    EAemp#
     c                   Kfld                    EAdiv

     c     keyTable      klist
     c                   kfld                    EAco#
     c                   kfld                    xxKey
      *
      *  Key List for the GENERIC Table file
      *
     C     keywtgtl      klist
     C                   kfld                    EAco#
     C                   kfld                    kkPgmD            4
     C                   kfld                    kkkey            10
      *
      /Free
       Chain(n) inRRN pea0p;
       Chain(n) keyWDELL1 pel01;
       LoadTrDtDtl();

       outJSON = ' ';

       BuildJSON(outJSON :'OutTrnYear' :%EditC(ELfyr:'X'));
       BuildJSON(outJSON :'OutFrmDate' :%EditW(dtMDYY(ELpfdt):'  /  /    '));
       BuildJSON(outJSON :'OutToDate'  :%EditW(dtMDYY(ELptdt):'  /  /    '));
       BuildJSON(outJSON :'OutMainDt'  :%EditW(dtMDYY(Eadt):'  /  /    '));
       BuildJSON(outJSON :'OutUser'    :%TrimR(userName));
       BuildJSON(outJSON :'OutpgmName' :%TrimR(pgmName));
       BuildJSON(outJSON :'OutpaymentPlan' :%trimR(paymentPlan));
       BuildJSON(outJSON :'OutTrnDcode'  :%trimR(code));
       BuildJSON(outJSON :'OutbilStatus' :%trimR(bilStatus));
       BuildJSON(outJSON :'OutpolStatus' :%trimR(polStatus));
       BuildJSON(outJSON :'Outactstatus' :%trimR(actstatus));
       BuildJSON(outJSON :'Outgr3Add1' :%trimR(gr3Add1));
       BuildJSON(outJSON :'Outgr3Add2' :%trimR(gr3Add2));
       BuildJSON(outJSON :'Outgr3Add3' :%trimR(gr3Add3));
       BuildJSON(outJSON :'Outgr3Add4' :%trimR(gr3Add4));
       BuildJSON(outJSON :'Outgr3Add5' :%trimR(gr3Add5));
       BuildJSON(outJSON :'Outgr3Add6' :%trimR(gr3Add6));
       BuildJSON(outJSON :'Outgr3Add7' :%trimR(gr3Add7));
       //***** Beg Add ***** 06/24/14 **************************
       BuildJSON(outJSON :'OutBatch#'  :%EditC(EAbul1:'X'));
       //***** End Add ***** 06/24/14 **************************
       //***** Beg Add ***** 06/03/15 **************************
       BuildJSON(outJSON :'OutputField156' : %trimR(hdgPayType));
       //***** End Add ***** 06/03/15 **************************
       //***** Beg Add ***** 04/11/22 **************************
       BuildJSON(outJSON :'DtlOutTDinvTypId' : %EditC(EAInvTypId:'X'));
       //***** End Add ***** 04/11/22 **************************

       //***** Beg Add ***** 05/24/23 **************************
       BtchTyp = *Blanks;
       Chain(n) keyWdeal4 pea04;
       If %found(Wdeal4);
          Select;
          When L4_EAachf = 'A';
             BtchTyp = 'EFT';
          When L4_EAachf = 'F';
             BtchTyp = 'FEE';
          When L4_EAachf = 'S';
             BtchTyp = 'SR ';
          When L4_EAachf = 'L';
             BtchTyp = 'LB ';
          When L4_EAachf = 'D';
             BtchTyp = 'DED';
          When L4_EAachf = 'R';
             BtchTyp = 'RDP';
          When L4_EAachf = 'T';
             BtchTyp = '3PY';
          When L4_EAachf = 'U';
             BtchTyp = '3BL';
          When (L4_EAachf = 'B' or L4_EAachf = ' ') and L4_EAonli = 'Y';
             BtchTyp = 'BIL';
          Endsl;
       Endif;
       BuildJSON(outJSON :'DtlOutBtchTyp' : %trimR(BtchTyp));
       //***** End Add ***** 05/24/23 **************************

       rtnCode = ' ';
       Return;
      /End-Free
       //===================================================================
       // Load Transaction Detail detail info.
       //===================================================================
     P LoadTrDtDtl     B
     D LoadTrDtDtl     PI
      /free

       //*** beg add ***** 06/03/15 *******************
       GetClientID(ClientID);
       //*** end add ***** 06/03/15 *******************
       userName  = GetUsrName(EAuser);
       kkPgmD  = 'PGMD';
       kkkey = EApgm;
       Chain(n) keywtgtl pgt00;
       If %found;
          pgmName = GTdesc;
       Else;
          pgmName = EApgm;
       Endif;

       Clear xxKey;
       xxKey = %editc(ELbscd : 'X');
       Chain keyTable PBS01;
       paymentPlan = %trimr(xxKey) + ' ' + BSdesc;

       Clear xxKey;
       xxKey = EAarcd;
       Chain keyTable PTT01;
       Code = %trimr(xxKey) + ' - ' + TTdesc;

       Select;
       When ELFAF = 'A';
         bilStatus  = 'Audited';
       When  ELFAF = 'E';
         bilStatus  = 'Estimated';
       When  ELFAF = 'R';
         bilStatus  = 'Revised';
       Endsl;

       polStatus = GetQuoteStsDesc(EAco#:EAfnd:EAemp#:EAdiv);

       Clear   AVdesc;
       Clear xxKey;
       xxKey = ELascd;
       Chain keyTable PAV01;
       actstatus  = %trimr(xxkey) + ' ' + AVdesc;

       //* Access third address file
       Clear gr3Add1;
       Clear gr3Add2;
       Clear gr3Add3;
       Clear gr3Add4;
       Chain(n) (EAco#:EAfnd:EAemp#:EAdiv) pa300;
       If not %found(wma3l);
         clear pa300;
       Else;
         If A3zip1 = *Blanks;
            A3zip1 = *Zeros;
         Endif;
         If A3zip2 = *Blanks;
            A3zip2 = *Zeros;
         Endif;
         ##zip = %Int(A3zip1)*10000 + %Int(A3zip2);
         FormatAddress(A3adr1:A3adr2:A3cty:A3st:##zip:AdL1:AdL2:AdL3);
         gr3Add1    = A3cont;
         gr3Add2    = AdL1;
         gr3Add3    = AdL2;
         gr3Add4    = AdL3;
         If A3ooc <> ' ';
            gr3Add3 = A3ooc;
         Endif;
       Endif;

       //*** beg add ***** 06/03/15 ******************************
       Select;
       When clientID = '025';
          hdgPayType = 'Statement#';
       When EAfl01 = 'E';
          hdgPayType = 'EFT#';
       Other;
          hdgPayType = 'Check#';
       Endsl;
       //*** end add ***** 06/03/15 ******************************

      /end-free
     P LoadTrDtDtl     E
