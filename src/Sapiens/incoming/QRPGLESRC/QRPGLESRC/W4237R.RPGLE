     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE  W4237R - BANK TAPE RECONCILIATION - UPDATE/REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTIION:                                        *
     H*  THIS PROGRAM:                                                *
     H*    1)  Reads a Bank Tape of Checks processed.                 *
     H*    2)  Check Rec file is accessed.                            *
     H*        If there is a hit on Acct#, Check#, & Amount,          *
     H*           then if the Activity Type is blank,                 *
     H*                   then the record is updated with 'X'         *
     H*                   and the Activity Date.                      *
     H*                else a report line is written detailing        *
     H*                   the non-blank Activity Type.                *
     H*        else a report line is written indicating that          *
     H*           No Hit on Bank info.                                *
     H*                                                               *
     H*****************************************************************
     H*  Called From: W4237C                                          *
     H*  Calls  to:   None                                            *
     H*****************************************************************
     H* Created on 12/05/2019 by c:62 for Project 102WID
     H*****************************************************************
     H* Revisions:
     H*
     H* 01/23/23  C:340 Project # 114WZP
     H*                 Wells Fargo Check Reconciliation File
     H*****************************************************************
      * COMPANY FILE
     FSMCOL     IF   E           K DISK    Rename(pco01:pco00)
      * Check Reconciliation file
     FWDCRL1    UF   E           K DISK
      * Bank Tape file
     ****  Beg Chg  ***  01/23/23  *********************************************   *
     F** WABRP     IF   F   80        DISK
     FWABRP     IF   E             DISK    Rename(WABRP:RecBRP)
     F                                     PreFix(F_)
     ****  End Chg  ***  01/23/23  *********************************************   *
      * Exception Report
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *

     D MSG             S             35    DIM(2) CTDATA PERRCD(1)              ERROR MESSAGES
     D TYP             S              1    DIM(4) CTDATA PERRCD(1)              Type/Description
     D DES             S              7    DIM(4) ALT(TYP)
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D xxCo#           s                   Like(COco#)
     D xxDt            s                   Like(CRacDt)
     D xxAcct          s                   Like(CRacct)
     ****  Beg Chg  ***  01/23/23  *********************************************   *
     D*xxChk#          s                   Like(CRchk#)
     D xxChk#          S             10  0
     ****  End Chg  ***  01/23/23  *********************************************   *

     D inCo#           s              3
     D xxMDYY          s              8  0
     D xxTot#          s              6  0
     D xxRec#          s              6  0
     D xxNot#          s              6  0
     D xxRecA          s             11  2
     D xxNotA          s             11  2
     D xxMsg           s             35
     D xxDes           s              7
     D page#           s              3  0
     D x               s              1  0
     D dsDate          s              8  0
     D dsTime          s              6  0

     ****  Beg Add  ***  01/23/23  *********************************************   *
     DGetCltId         Pr                  ExtPgm('RTVCLTID')
     D CltId                               Like(CltId)

     DCltId            S              3
     DxxAmnt           S             12  2
     DxxAcDt           S              6  0

     D DS@BRCWF        DS
     D  BRacctWF               1     13
     D  BRchk#WF              14     23  0
     D  BRamtWF               24     33  2
     D  BRdDtWF               34     41  0
     D  BRtrTpWF              42     42
     D  BRiDtWF               43     50  0
     D  BRfillWF              51     80

     D DS@WABRP        DS
     D  BRacct                 1     10
     D  BRchk#                15     20  0
     D  BRymd                 27     32  0
     D  BRamt                 33     43  2

     ****  End Add  ***  01/23/23  *********************************************   *

     ****  Beg Del  ***  01/23/23  *********************************************   *
     I*WABRP     NS  01
     I*                                  1   10  BRacct
     I*                                 15   20 0BRchk#
     I*                                 27   32 0BRymd
     I*                                 33   43 2BRamt
     ****  End Del  ***  01/23/23  *********************************************   *

     C     *Entry        PList
     C                   Parm                    inCo#

      /free
       xxCo#  = %Dec(inCo#:3:0);
       xxTot# = 0;
       xxMDYY = 0;
       xxTot# = 0;
       xxRec# = 0;
       xxNot# = 0;
       xxRecA = 0;
       xxNotA = 0;
       dsDate = %Dec(%Date():*usa);
       dsTime = %Dec(%Time():*hms);

       // Get the Company Record
       Chain xxCo# pco00;
       If Not %Found(smcol);
          Coname = ' ';
       EndIf;

     ****  Beg Add  ***  01/23/23  *********************************************   *
       GetCltId(CltId);
     ****  End Add  ***  01/23/23  *********************************************   *

       page# = 1;
       Except Head;

       Read WABRP;
       DoW Not %Eof(wabrp);
     ****  Beg Add  ***  01/23/23  *********************************************   *
          If %SubSt(F_WABRP:1:2) = 'HR' Or  %SubSt(F_WABRP:1:2) = 'TR';
             Iter;
          EndIf;

       If CltId = '114';
          DS@BRCWF = F_WABRP;
          xxAcct = %Trim(BRacctWF);
          xxChk# = BRchk#WF;
          xxAmnt = BRamtWF ;
          xxAcDt = %Dec(%SubSt(%Char(BRdDtWF):3:6):6:0);
       Else;
          DS@WABRP = F_WABRP;
          xxAcct = %Trim(BRacct);
          xxChk# = BRchk#;
          xxAmnt = BRamt ;
          xxAcDt = BRymd ;
       Endif;
     ****  End Add  ***  01/23/23  *********************************************   *
     ****  Beg Del  ***  01/23/23  *********************************************   *
       // xxAcct = BRacct;
       // xxChk# = BRchk#;
     ****  End Del  ***  01/23/23  *********************************************   *
          xxTot# = xxTot# + 1;
          *in62 = *On;   // Indicates it couldn't find a check rec rcd

          Chain (xxCo#:xxAcct:xxChk#) pcr01;
          If %Found(wdcrl1);
             DoU %Eof(wdcrl1);
     ****  Beg Chg  ***  01/23/23  *********************************************   *
          //    If CRckAm = BRamt;
                If CRckAm = xxAmnt;
     ****  End Chg  ***  01/23/23  *********************************************   *
                   If CRacTp = ' ';
     ****  Beg Chg  ***  01/23/23  *********************************************   *
                      xxRec# = xxRec# + 1;
                      xxRecA = xxRecA + xxAmnt;
                //    xxRecA = xxRecA + 1;
     ****  End Chg  ***  01/23/23  *********************************************   *
                      CRacTp = 'X';
     ****  Beg Chg  ***  01/23/23  *********************************************   *
                      If CltId <> '114';
                         If %Subst(%EditC(BRymd:'X'):1:2) < '36';
                            CRacDt = 1000000 + BRymd;
                         Else;
                            CRacDt = BRymd;
                         EndIf;
                      Else;
                         CRacDt = %Dec(%Char(%Date(BRdDtWF:*ISO):*CYMD0):7:0);
                      EndIf;
            //        If %Subst(%EditC(BRymd:'X'):1:2) < '36';
            //           CRacDt = 1000000 + BRymd;
            //        Else;
            //           CRacDt = BRymd;
            //        EndIf;
     ****  End Chg  ***  01/23/23  *********************************************   *

                      Update pcr01;
                   Else;
                      *in20 = *On;
                      xxDes = ' ';
                      x = %LookUp(CRacTp:typ);
                      If x <> 0;
                         xxDes = des(x);
                      EndIf;
                      xxNot# = xxNot# + 1;
     ****  Beg Chg  ***  01/23/23  *********************************************   *
               //     xxNotA = xxNotA + BRamt;
                      xxNotA = xxNotA + xxAmnt;
     ****  End Chg  ***  01/23/23  *********************************************   *
                      xxMsg  = msg(2);
                      ExSr OutMsg;
                   EndIf;
                   *in62  = *Off;
                   Leave;
                Else;
                   ReadE (xxCo#:xxAcct:xxChk#) pcr01;
                EndIf;
             EndDo;
          Else;
             *in62  = *Off;
             xxNot# = xxNot# + 1;
     ****  Beg Chg  ***  01/23/23  *********************************************   *
          // xxNotA = xxNotA + BRamt;
             xxNotA = xxNotA + xxAmnt;
     ****  End Chg  ***  01/23/23  *********************************************   *
             *in20  = *Off;
             xxMsg  = msg(1);
             ExSr OutMsg;
          EndIf;

          If *in62 = *On;
             xxNot# = xxNot# + 1;
     ****  Beg Chg  ***  01/23/23  *********************************************   *
         //  xxNotA = xxNotA + BRamt;
             xxNotA = xxNotA + xxAmnt;
     ****  End Chg  ***  01/23/23  *********************************************   *
             *in20  = *Off;
             xxMsg = msg(1);
             ExSr OutMsg;
          EndIf;
          Read wabrp;
       EndDo;

       Except Total;

       *inLR = *On;
       //==============================================================
       // Print Error Messages
       //==============================================================
       BegSR OutMsg;

           If *inOA = *On;
              page# = page# + 1;
              Except head;
              *inOA = *Off;
           EndIf;

           Except detail;

       EndSr;
      /end-free
      *****************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           10 'W4237R'
     O                       CONAME              80
     O                                           95 'PAGE'
     O                       PAGE#         Z    100
     O          E            HEAD           2
     O                                           50 'BANK TAPE RECONCILIATI'
     O                                           63 'ON EXCEPTION '
     O                                           69 'REPORT'
     O                       DSDATE        Y     12
     O                       DSTIME             100 '  :  :  '
     O          E            HEAD           2
     O                                           14 'Account#'
     ****  Beg Chg  ***  01/23/23  *********************************************   *
     O*                                          24 'Check#'
     O*                                          37 'Amount'
     O*                                          48 'Bank Date'
     O*                                          57 'Type'
     O*                                          74 'Message'
     O                                           30 'Check#'
     O                                           43 'Amount'
     O                                           54 'Bank Date'
     O                                           63 'Type'
     O                                           73 'Message'
     ****  End Chg  ***  01/23/23  *********************************************   *
     O          E            DETAIL         1
     ****  Beg Chg  ***  01/23/23  *********************************************   *
     O*                      BRACCT              14
     O*                      BRCHK#        Z     24
     O*                      BRAMT         3     38
     O*                      BRYMD               49 '  /  /  '
     O*              20      XXDES               58
     O*                      XXMSG               95
     O                       xxAcct              20
     O                       xxChk#        Z     30
     O                       xxAmnt        3     43
     O                       xxAcDt              54 '  /  /  '
     O               20      XXDES               63
     O                       XXMSG              101
     ****  End Chg  ***  01/23/23  *********************************************   *
     O          E            TOTAL       2  2
     O                                           47 'T O T A L'
     O          E            TOTAL          1
     O                                           10 'Bank Recs'
     O                                           23 'Reconciled'
     O                                           38 'Amount'
     O                                           58 'Not Recon.'
     O                                           73 'Amount'
     O          E            TOTAL          1
     O                       XXTOT#        Z     10
     O                       XXREC#        Z     23
     O                       XXRECA        3     38
     O                       XXNOT#        Z     58
     O                       XXNOTA        3     73
**
Bank Acct/Check/Amount Not Found
Check Rec Type Not Outstanding
**
XCleared
SStop
VVoid
DDelete
