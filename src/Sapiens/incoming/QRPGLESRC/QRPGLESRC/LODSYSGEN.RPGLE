     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title LODSYSGEN - Add new values to System Generic Table File
     h dftname(LODSYSGEN)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program will add new values to System Generic  *
      *                Table File.                                         *
      *                                                                    *
      *  Called from:  SYPOSTCV                                            *
      *                                                                    *
      *  Created on 05/22/2015 by C:279 for Project: 114WKA                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  09/03/24 C:291 Project: WC2968                                    *
      *                 Add IVCNAMADDR                                     *
      *                                                                    *
      *  12/19/23 C:340 Project: WC2935                                    *
      *                 Policy Rule - Failed BIF Notification              *
      *                                                                    *
      *  10/19/21 C:33  Project: WC2663  #98816                            *
      *                 Add 3PRTPMTCX                                      *
      *                                                                    *
      *  05/04/21 C:62  Project: WC2663                                    *
      *                 Add QTE3PRTPMT and 3PRTPMTDIS                      *
      *                                                                    *
      *  05/07/20 C:33  Project: SY406                                     *
      *                -Add entries for Portal Products/Subproducts        *
      *                -****** NO LONGER USED ******                       *
      *                                                                    *
      *  04/05/17 C:136 Project: 129WUL                                    *
      *                 Add entries for Exp Mod and Policy Address         *
      *                 changes                                            *
      *                                                                    *
      *  01/31/17 C:33  Project: WC2405                                    *
      *                 Add entries for Generic File rules                 *
      *                                                                    *
      *  07/29/16 C:136 Project: 114WYQ                                    *
      *                 Add entries for Relay Messenging                   *
      *                                                                    *
      *  07/06/16  C:33  Project: WC2405                                   *
      *                  Add external roles                                *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fsmcop     if   e             disk    rename(pco01:pco0p)
     fstgip     if a e             disk    rename(pgi01:pgi0p)
     fstgil     if   e           k disk    rename(pgi01:pgi00)
     f                                     prefix(i)
     fstgtp     if a e             disk    rename(pgt01:pgt0p)
     fstgtl     if   e           k disk    rename(pgt01:pgt00)
     f                                     prefix(t)

     d psds          esds                  extname(szq1p)

      * constant
     d intSzMst        c                   10
     d***  Begin Chg  ***  05/07/20  **************************************
     d*intSzDtl        c                   75
     d intSzDtl        c                   150
     d***  End   Chg  ***  05/07/20  **************************************

     d***  Begin Chg  ***  05/07/20  **************************************
     d*TblId           s             45    dim(intSzMst) ctdata perrcd(1)
     d*TblKey          s             80    dim(intSzDtl) ctdata perrcd(1)
     d TblId           s            100    dim(intSzMst) ctdata perrcd(1)
     d TblKey          s            100    dim(intSzDtl) ctdata perrcd(1)
     d***  End   Chg  ***  05/07/20  **************************************

      * data structure
     d                 ds
     d***  Begin Chg  ***  05/07/20  **************************************
     d*dsTblId                       45
     d dsTblId                      105
     d***  End   Chg  ***  05/07/20  **************************************
     d strTbId                        4    overlay(dsTblId:1)
     d strDesc                       40    overlay(dsTblId:6)
     d***  Begin Add  ***  05/07/20  **************************************
     d str10Hdg1                     20    overlay(dsTblId:46)
     d str10Hdg2                     20    overlay(dsTblId:66)
     d str10Hdg3                     20    overlay(dsTblId:86)
     d***  End   Add  ***  05/07/20  **************************************

     d                 ds
     d***  Begin Chg  ***  05/07/20  **************************************
     d*dsTblKey                      80
     d dsTblKey                     100
     d***  End   Chg  ***  05/07/20  **************************************
     d strTbId1                       4    overlay(dsTblKey:1)
     d strKey                        10    overlay(dsTblKey:6)
     d strDesc1                      40    overlay(dsTblKey:17)
     d strFlg                         1    overlay(dsTblKey:58)
     d strFlg2                        1    overlay(dsTblKey:59)
     d strStat                        1    overlay(dsTblKey:70)
     d***  Begin Add  ***  05/07/20  **************************************
     d str10Char1                    10    overlay(dsTblKey:71)
     d str10Char2                    10    overlay(dsTblKey:81)
     d str10Char3                    10    overlay(dsTblKey:91)
     d***  End   Add  ***  05/07/20  **************************************

      * work varibales
     d intCount        s              5  0 inz
     d intCount1       s              5  0 inz

     *** Beg Add *** 12/19/23 *** WC2935 ***************************
     D/copy sPrModule
     *** End Add *** 12/19/23 *** WC2935 ***************************
      /copy sPrDate

      /free

       // read through company master file
       Setll 1 pco0p;
       Read pco0p;
       Dow not %eof(SMCOP);
          // read through generic table master file
          For intCount = 1 to intSzMst;
             If TblId(intCount) <> ' ';
                dsTblId = TblId(intCount);

                Chain (COco#:strTbId) pgi00;
                If not %found(STGIL);
                   clear pgi0p;
                   GIco#   = COco#;
                   GItbId  = strTbId;
                   GIdesc  = strDesc;
                   //***  Begin Add  ***  05/07/20  **************************************
                   GICH01  = str10Hdg1;
                   GICH02  = str10Hdg2;
                   GICH03  = str10Hdg3;
                   //***  End   Add  ***  05/07/20  **************************************
                   GIstat  = 'A';
                   GIpgm   = q1Pgm;
                   GIuser  = q1User;
                   GItime  = Timeto6(SystemTime());
                   GIdt    = Dateto7(SystemDate());
                   Write pgi0p;
                Endif;
                Exsr WriteDtl;
             Endif;
          Endfor;
          Read pco0p;
       Enddo;

       *inlr  = *on;

       //===================================================================
       // Write Generic Table value record
       //===================================================================
       BegSr WriteDtl;

       // read through generic table value file
       For intCount1 = 1 to intSzDtl;
          If TblKey(intCount1) <> ' ';
             dsTblKey = TblKey(intCount1);
             If strTbId = strTbId1;
                Chain (COco#:strTbId:strKey) pgt00;
                If not %found(STGTL);
                   clear pgt0p;
                   GTco#   = COco#;
                   GTtbId  = strTbId;
                   GTkey   = strKey;
                   GTdesc  = strDesc1;
                   GTstat  = 'A';
                   GTpgm   = q1Pgm;
                   GTuser  = q1User;
                   GTtime  = Timeto6(SystemTime());
                   GTdt    = Dateto7(SystemDate());
                   GTfg01  = strFlg;
                   GTfg02  = strFlg2;
                   //***  Begin Add  ***  05/07/20  **************************************
                   GTCF01  = str10Char1;
                   GTCF02  = str10Char2;
                   GTCF03  = str10Char3;
                   //***  End   Add  ***  05/07/20  **************************************
                   //*** Beg Chg *** 12/19/23 *** WC2935 ***************************
                   // If strStat <> *blank;
                   //    GTstat = strStat;
                   // Endif;
                   //*** Beg Chg ***** 09/03/24 ***************************
                   //***If strKey = 'BIFFAILURE' and IsModEnabled('Invoice Cloud');
                   //***   GTstat  = 'A';
                   If (strKey = 'BIFFAILURE' or strKey = 'IVCNAMADDR')
                    and IsModEnabled('Invoice Cloud');
                       GTstat = 'A';
                   //*** End Chg ***** 09/03/24 ***************************
                   Else;
                      If strStat <> *blank;
                         GTstat = strStat;
                      Endif;
                   EndIf;
                   //*** End Chg *** 12/19/23 *** WC2935 ***************************
                   Write pgt0p;
                Endif;
             Endif;
          Endif;
       Endfor;

       Endsr;

      /end-free

**
AXKY Auto Diary Identifier Key
AXDI Auto Diary Basis Date
AXOC Auto Diary Occurrence
AXRO Auto Diary Roles
RELY Relay Msg Purpose
PRD1 Products
PRD2 Subproducts                             Product             Product             Product
**
AXKY NEWCLAIM   New Claim Entry                          C
AXKY CLMCHG01   Claimant Name or Address Change          C
AXKY CLMCHG02   Indemnity/Medical/Record Only Status Chg C
AXKY CLMCHG03   Claim Indicators                         C
AXKY CLMCHG04   Claim Status                             C
AXKY LOWRSV     Low Reserves                             C
AXKY LARGERSV   Large Reserve Change                     C
AXKY LARGEPMT   Large Payment                            C
AXKY PAYMENT    Payment                                  C
AXKY CLMRETEN   Percentage of Specific Retention         C
AXKY CLMNCCI    NCCI Injury Code                         C
AXKY PAYMENT2   Total Claim Payments                     C
AXKY MULTIEMP   Multiple Employee Incident               C
AXKY CLMNPC     Nature / Part / Cause Code Trigger       C
AXKY TOTALINC   Total Incurred                           C
AXKY WEEKSPD    Compensation Weeks Paid                  C
AXKY AUDPREM$   Audited Premium $ Value Change           P
AXKY AUDPREM    Audited Premium % Change                 P
AXKY CALCPREM   Mid-term Calculated Premium % Change     P
AXKY QTEENTRY   New Business Quote Entry                 P
AXKY QTERVW     Renewal Quote Review                     P
AXKY POLPEND    Policy Issuance Pending                  P
AXKY NQTESTS    New Business Quote Status                P
AXKY RQTESTS    Renewal Business Quote Status            P
AXKY POLCONC    Policy Concentration Information         P
AXKY POLPANEL   Panel Request to Prov Relations for NB   P
AXKY FILE RULES Generic File Change Rules                X
AXKY EXPMODDATE Experience Modifier Date                 P
AXKY POLADDRCHG Policy Address Change                    P
AXKY QTE3PRTPMT Quote Third-Party Payment                P           I
AXKY 3PRTPMTDIS Third-Party Payment Disputed             P           I
AXKY 3PRTPMTCX  Third-Party Payment on CX Policy         P           I          10/19/21
AXDI SystemDt   System Date
AXDI DtRcvd     Date Received                            C
AXDI AccDate    Accident Date                            C
AXDI RptDate    Report Date                              C
AXOC One        Only one diary record allowed
AXOC OneOpen    Only one open diary record allowed
AXOC Multiple   Multiple open diary records allowed
AXRO ClmAdj     Claims Adjustor                          C
AXRO ClmSuper   Claims Supervisor                        C
AXRO AcctMgr    Account Manager                          C
AXRO ClmMgr     Claim Manager                            C
AXRO CaseNurse  Case Nurse                               C
AXRO OutAdj     Outside Adjuster                         C
AXRO SDFAdj     Second Disability Fund Adjuster          C
AXRO ExcessAdj  Excess Adjuster                          C
AXRO LossCtl    Loss Control                             P
AXRO UnderWtr   Underwriter                              P
AXRO PolSvcs    Policy Services                          P
AXRO PremAcctng Premium Accounting                       P
AXRO AuditRvwr  Audit Reviewer                           P
AXRO AuditMgr   Audit Manager                            P
AXRO PolClmCont Policy Claim Contact                     P
AXRO PolPrimary Policy Claim Contact                     P
AXRO PolCont1   Policy Contact 1                         PE
AXRO PolCont2   Policy Contact 2                         PE
AXRO PolCont3   Policy Contact 3                         PE
AXRO PolCont4   Policy Contact 4                         PE          I
AXRO ClmAttyDef Claim Defense Attorney                   CE
AXRO ClmAttyPln Claim Plaintiff Attorney                 CE
AXRO Claimant   Claimant                                 CE
RELY OW         Onboard Welcome
PRD1 Consumer   Consumer
PRD1 FROI       FROI
PRD1 Quote      Quote
PRD1 Reports    Reports
PRD1 S/R Online S/R Online
PRD1 iDARTS     iDARTS
PRD2 Billing    Billing                                               Consumer
PRD2 Claims     Claims                                                Consumer  Reports   iDARTS
PRD2 Policy     Policy                                                Consumer  Reports   iDARTS
AXKY BIFFAILURE BIF Failure                              P           I
AXKY IVCNAMADDR Name/Address Change Failure              P           I
