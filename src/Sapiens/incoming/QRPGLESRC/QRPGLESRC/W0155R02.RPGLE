     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/Define CONVERT
      //
     H/TITLE W0155R02 - DEDUCTIBLE DISCOUNT DEFINITION
      //****************************************************************
      //                                                               *
      //  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
      //                OR ADD RECOrDS IN THE DEDUC DISCOUNT  FILE.    *
      //                                                               *
      //  CALLED FROM:                                                 *
      //    W0155R - DEDUC DISCOUNT DEFINITION MAINTENANCE             *
      //                                                               *
      //  CallS TO:                                                    *
      //    NONE                                                       *
      //                                                               *
      //  CREATED: 05/07/97  PROGRAMMER: C:65  PROJECT: WC690          *
      //****************************************************************
      //                                                               *
      //  REVISIONS:                                                   *
      //                                                               *
      //  08/08/22 C:079 Project: WC2859                               *
      //                 Add Death Benefits PV1 Codes                  *
      //                                                               *
      //  09/02/20 C:180 PROJECT WC2733                                *
      //                 Add field Include Medical/Indemnity Claims    *
      //                 field to RcdRating2 - Deductible section      *
      //                                                               *
      //  01/22/20 C:318 SUPPOrT: 86408                                *
      //                 Removed indicatOr while Chaining file         *
      //                 WTGTL(PGT01)                                  *
      //                                                               *
      //  09/15/17 C:180 PROJECT  150WBL                               *
      //                 Add Variable Aggregate Amount check box       *
      //                                                               *
      //  09/11/09 C:189 PROJECT: 139WMA                               *
      //                 ULLICO LARGE DEDUCTIBLE FOrMS.AddED FIELD     *
      //                 FOr ALLOCATED LOST ADJUSTMENT EXPENSES        *
      //                                                               *
      // 10/11/07 C:190 SUPPOrT WA099 54504                            *
      //                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      //                DISCOUNT CODE HAVING INACTIVE RECOrDS.         *
      //                                                               *
      //  09/28/06 C:132 PROJECT: WC1292/135WCH                        *
      //                 ReMove F5=Refresh when in Add mode.           *
      //                                                               *
      //  04/20/05 C:53 PROJECT: WC1292/135WCH                         *
      //                DEDUCTIBLE BILLING BY INCURRED FLAG            *
      //                                                               *
      //  05/11/01 C:65 PROJECT: SUPPOrT# 27343                        *
      //                ALLOW GROSS DEDUCTIBLE PLANS TO HAVE 0% FOr    *
      //                LARGE DEDUCTIBLE SETUPS.                       *
      //                                                               *
      //  09/20/99 C:61 PROJECT: WC949                                 *
      //                Add 'Auto renew  (N/ )' field.                 *
      //                                                               *
      //  08/24/99 C:90 PROJECT: WC902                                 *
      //                AddED Entry OF NEW FIELD 'CLAIM/OCCURRENCE     *
      //                VERBIAGE'. VALUE MUST BE 'C' Or BLANK.         *
      //                                                               *
      //  08/11/99 C:65 PROJECT: SUPPOrT# 19218                        *
      //                Do NOT REQUIRE THE FIRST DEDUC HIERARCHY If    *
      //                THE GROUP IS ADMIN ONLY.                       *
      //                                                               *
      //  08/07/98 C:87 PROJECT: WC591                                 *
      //                Added Deductible Plan field And processing.    *
      //                                                               *
      //  04/08/98 C:91 PROJECT: WC577                                 *
      //                DISPLAY ERROr MESSAGE If F10 KEY PRESSED ON    *
      //                NON SEARCHABLE FIELD (IND. 72).                *
      //                                                               *
      //  03/23/98 C:85 PROJECT: WC773                                 *
      //                DETERMINE If CLIENT IS ADMIN-ONLY (IND. 94),   *
      //                Or CLAIMS-ONLY (IND. 95).                      *
      //                                                               *
      //  01/12/98 C:85 PROJECT: WC577                                 *
      //                AddED ERROr CHECKING FOr DISCOUNT CODE         *
      //                (SCCLAS), If IN Add MODE.                      *
      //                CHANGED SEARCHES FOr SCDC01 - SCDC10.          *
      //                CHANGED FOrMAT OF EFFECTIVE DATE AFTER SEARCH. *
      //                                                               *
      //    NOTE:  PLEASE Add NEW COMMENTS AT THE TOP OF REVISIONS.    *
      //    -------------------------------------------------------    *
      //****************************************************************
      //                                                               *
      // INDICATOrS USED:                                              *
      //                                                               *
      //  09 - INQUIRY                                                 *
      // N09 - Update                                                  *
      //  40 - Add                                                     *
      // N40 - Update/INQUIRY                                          *
      //  93 - SCREEN WAS Update WHILE USER WAS IN IT                  *
      //                                                               *
      //  SCR01:                                                       *
      //  41 - (WC30005) RecOrd already exists.                        *
      //  42 - (WCR0504) The state code was not found in list.         *
      //  43 - (WCR0500) Invalid date.                                 *
      //  44 - (WCR0502) INVALID DISC CODE.                            *
      //  71 - (WC31317) Auto renewal (N/ ).                           *
      //  72 - (WCR1058) F10 KEY PRESSED ON NON SEARCHABLE FIELD       *
      //                                                               *
      // SCR02:                                                        *
      //                                                               *
      // SCR03:                                                        *
      //                                                               *
      //****************************************************************
      //  REVISIONS:                                                   *
      //
      //  04/27/00 C:53  SUPPOrT: WC065/19768                          *
      //                 SWAP FILE STABL FOr STSTL3 TO OMIT "UC" Entry.*
      //                                                               *
      //****************************************************************
     FW0155F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWTDDP     UF A E             Disk
     F                                     RENAME(PDD01:DbrcdF)
     F                                     INFDS(DBFDS)
     FWTDDL2    If   E           K Disk
     F                                     INFDS(DBFDS2)
     FWMFNL     If   E           K Disk
     FSTSTL3    If   E           K Disk
     FWTABL     If   E           K Disk
     F                                     IGNOrE(PSY01)
      //
     FSSYLF     If   E           K Disk
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      //
      //      System Table ID File
     FWTGIL     If   E           K Disk
      //      Sustem Table Value File
     FWTGTL     If   E           K Disk
      //
      //
      // Procedure prototypes
      //
     D RtvCltID        pr                  ExtPgm('RTVCLTID')
     D                                3    const

      /COPY SCOPYMT01D
      //
      //
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
      //
     D OPTNM           C                   CONST('Deductible Discoun-
     D                                     t Definition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
      //
     D                 DS
     D  SDC                    1     20
     D                                     DIM(10)
     D  SCDC01                 1      2
     D  SCDC02                 3      4
     D  SCDC03                 5      6
     D  SCDC04                 7      8
     D  SCDC05                 9     10
     D  SCDC06                11     12
     D  SCDC07                13     14
     D  SCDC08                15     16
     D  SCDC09                17     18
     D  SCDC10                19     20
       //*** Beg *** 08/08/22 *************************************************
       dcl-ds dsPv1Db;
        dcl-subf scPv1Db01 Char(2);
        dcl-subf scPv1Db02 Char(2);
        dcl-subf scPv1Db03 Char(2);
        dcl-subf scPv1Db04 Char(2);
        dcl-subf scPv1Db05 Char(2);
        dcl-subf scPv1Db06 Char(2);
        dcl-subf scPv1Db07 Char(2);
        dcl-subf scPv1Db08 Char(2);
        dcl-subf scPv1Db09 Char(2);
        dcl-subf scPv1Db10 Char(2);
        dsPv1Db2 dim(10) Pos(1) like(scPv1Db01);
       end-ds dsPv1Db;
       //*** End *** 08/08/22 *************************************************
     D FIDS01        E DS                  ExtName(SZQ2P)
      //
      // StAnd alone
       //*** Beg *** 08/08/22 *************************************************
       dcl-s strReCd Like(B2rC01);
       dcl-s str1Pv1 Char(2);
       dcl-s str2Pv1 Char(2);
       dcl-s i2 Zoned(3);
       dcl-s j1 Zoned(3);
       //*** End *** 08/08/22 *************************************************
      //
     D CltId           s              3
      //
     C     *Like         Define    DDCO#         XXCO#
     C     *Like         Define    DDFND         XXFND
     C     *Like         Define    DDST          XXST
     C     *Like         Define    DDCODE        XXCODE
     C     *Like         Define    DDEFDT        XXEFDT
     C     *Like         Define    SCPRDS        QQPRDS
     C     *Like         Define    GTTBID        XXTBID
      //
     C     COMPL         Plist
     C                   Parm                    ##CO#             3 0
     C                   Parm                    ##FND             3 0
     C                   Parm                    ##CONM           40
     C                   Parm                    ##FNAM           40
      //
      //   KEY LIST FOr THE SYSTEM CONTROL FILE
      //
     C     SYKY          Klist
     C                   Kfld                    XXSYK             2
      //
      //  TABLE FILE
      //
     C     TBKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXKEY
      //
      //  KEY LIST FOr THE SYSTEM-WIDE STATE CODE TABLE FILE
      //
     C     STKY          Klist
     C                   Kfld                    XXKEY            10
      //
      //  FUND MASTER FILE
      //
     C     FNKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
      //
      //   KEY LIST FOr THE DEDUCTIBLE DISCOUNT CODE FILE
      //
     C     DDKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    XXST
     C                   Kfld                    XXCODE
     C                   Kfld                    XXEFDT
      //
      //  SYSTEM TABLE ID FILE
     C     GIKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXTBID
      //
      //  SYSTEM TABLE VALUE FILE
     C     GTKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXTBID
     C                   Kfld                    XXKEY
      //
     C     *Entry        Plist
     C                   Parm      *InKC         @@KC              1
     C                   Parm      *InKL         @@KL              1
     C                   Parm                    @@MODE            1
     C                   Parm                    @@RRN             9 0
     C                   Parm                    @@CO#             3 0
     C                   Parm                    @@FND             3 0
      //
     C/COPY SCOPYMT01C
      //
      //===============================================================
      // ChgSts - SUBROUTINE FOr CHANGING STATUS
      //===============================================================
      //
       Begsr ChgSts;
          //
          Chain @@RRN DbrcdF;
          *In60 = Not %Found;
          //
          If *In60 = *OFF;
             //
             If DDSTAT = 'I';
     C                   Move      'A'           DDSTAT
             Else;
     C                   Move      'I'           DDSTAT
             Endif;                                                           //DPSTAT=I
             //
     C                   MoveL     Q1USER        DDUSER
     C                   MoveL     Q1PGM         DDPGM
      //
     C                   TIME                    DSDTTM
      //
     C                   Z-Add     DSDATE        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   MoveL     SRDTO         DDDT
      //
     C                   MoveL     DSTIME        DDTIME
      //
             Update DbrcdF;
             //
          Endif;                                                              //*In60=*OFF
          //
       Endsr;                                                                 //ChgSts
       //
       //===============================================================
       // DltPre - SUBROUTINE FOr PRIOr TO DELETION PROCESSING
       //===============================================================
       //
       Begsr DltPre;
       Endsr;                                                                 //DltPre
       //
       //===============================================================
       // DsplyR - SUBROUTINE FOr DISPLAYING INFOrMATION
       //===============================================================
       //
       Begsr DsplyR;
       Endsr;                                                                 //DsplyR
       //
       //===============================================================
       // ERRCK1 - SUBROUTINE FOF ERROR CHECKING SCR01
       //===============================================================
       //
       Begsr Errck1;
          //
          //  VERIfY STATE And EFFECTIVE DATE If IN Add MODE.
          //
          If *In40 = *On;
             //
             //  STATE MUST BE VALID.
             //
     C                   MoveL     SCST          XXKEY
             Chain STKY PST01;
             *In60 = Not %Found;
     C  N90
     CAN 60              Seton                                        9042
      //
      //  EFFECTIVE DATE MUST BE VALID
      //
     C                   Z-Add     SCEFDT        SRDTI
     C                   Call      'SRCVTDAT'    DATCHK
     C                   MoveL     SRDTO         SCEFDT
      //
B02  C  N90SRERR         Ifeq      '1'
                *In90 = *On;
                *In43 = *On;
E02          Endif;                                                           //SRERR=1
             //
             //  RECOrD MUST NOT ALREADY EXISTS FOr ST/CODE/EFDT
             //
     C                   Move      SCST          XXST
     C                   Move      SCCODE        XXCODE
     C                   Z-Add     SCEFDT        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   MoveL     SRDTO         XXEFDT
             Chain DDKY PDD01;
             *In60 = Not %Found;
     C  N90*In60         Ifeq      *OFF
     C     DDSTAT        Andeq     'A'
     C                   Move      *On           *In41
     C                   Move      *On           *In90
             Endif;                                                           //*In60 EQ 0
             //
             //
             //  THE DISCOUNT CODE MUST NOT BE BLANK.
             //
     C  N90SCCODE        Ifeq      *Blank
     C                   Move      *On           *In46
     C                   Move      *On           *In90
             Endif;                                                           //SCCODE=BLANK
             //
             //
          Endif;                                                              //*In40 EQ 1
          //
          //  DESCRIPTION MUST NOT BE BLANK
          //
     C  N90SCDESC        Ifeq      *Blanks
     C                   Move      '1'           *In44
     C                   Move      '1'           *In90
          Endif;                                                              //SCDESC Ifeq
          //
          //  THE GROSS/NET TYPE MUST BE 'G' Or 'N'
          //
     C  N90SCTYPE        IfNE      'G'
     C     SCTYPE        AndNE     'N'
     C                   Move      '1'           *In20
     C                   Move      '1'           *In90
          Endif;
          //
          //  THE DISCOUNT MUST BE GREATER THAN 0 FOR GROSS REPORTING.
          //
     C  N90SCTYPE        Ifeq      'G'
     C     SCPCT         AndLT     0
     C                   Move      '1'           *In21
     C                   Move      '1'           *In90
          Endif;
          //
          //  THE DISCOUNT MUST NOT BE LESS THAN 0 FOR NET REPORTING.
          //
     C  N90SCTYPE        Ifeq      'N'
     C     SCPCT         AndLT     0
     C                   Move      '1'           *In21
     C                   Move      '1'           *In90
          Endif;
          //
          //  THE AMOUNT PER OCCURRENCE CANNOT BE LESS THAN 0.
          //
     C  N90SCAMPO        IfLt      *ZERO
     C                   Move      '1'           *In22
     C                   Move      '1'           *In90
          Endif;
          //
          //  THE AMOUNT AGGREGATE CANNOT BE LESS THAN 0.
          //
          If SCAMAG < *ZERO;
     C                   Move      '1'           *In23
     C                   Move      '1'           *In90
          Endif;
          //
          //  DEDUCTIBLE PLAN
          //
          If SCPLAN <> *Blank;
             //
             //     - Must be in Table file
     C                   MoveL(P)  SCPLAN        XXKEY
     C                   Move(P)   'DT'          XXTBID
      //
      //     - First try state table
     C                   MoveL     SCST          XXTBID
             Chain GTKY PGT01;
             If Not %Found(WTGTL) And SCST <> *Blank;
     C                   MoveL     '  DT'        XXTBID
                Chain GTKY PGT01;
             Endif;
             If Not %Found(WTGTL) Or GTSTAT ='I';
     C                   Move      '1'           *In81
     C                   Move      '1'           *In90
             Endif;
             //
          Endif;                                                              //SCPLAN<>*Blank
          //
          //  ALAE can have either 'Y' Or 'N'
          //
          If SCALAE <> *Blank
          And SCALAE <> 'Y';
             *In90 = *On;
             *In73 = *On;
          Endif;                                                              //SCALAE
          //
       If SCvarA <> 'Y' And SCvara <> *Blanks;
          SCvarA = *Blanks;
       Endif;
      //
      //
      //**FIRST DEDUC CANNOT BE BLANK
      //
      //
      //
      //  THE FIRST DEDUCTIBLE MUST NOT BE BLANK.
      //
     C  N90SCDC01        Ifeq      *Blanks
     C     *In94         Andeq     *OFF
      //
      //  GET ELECTRONIC SUBMISSION FLAG
      //
     C                   MoveL     'WC'          XXSYK
             Chain SYKY SWC01;
             *In60 = Not %Found;
             //
             If WCFL03 = 'Y';
     C                   Move      *On           *In45
     C                   Move      *On           *In90
             Endif;
             //
          Endif;                                                              //SCDC01=BLANKS
          //
          //
          //  VALIDATE DEDUC HIERARCHY
          //
B02  C  N901             Do        10            I1                3 0
      //
B03          If SDC(I1) <> *Blank;
                //
     C                   Move      *Blank        XXKEY
     C                   MoveL     SDC(I1)       XXKEY
     C     I1            Add       23            X1                3 0
      //
                Chain TBKY PRT01;
                *In60 = Not %Found;
                //
B04             If *In60 = *On;
                   //
     C                   Move      *On           *In(X1)
     C                   Move      *On           *In90
                   LEAVE;
                   //
X04             Else;
                   //
B05                If RTSTAT <> 'A';
     C                   Move      *On           *In(X1)
     C                   Move      *On           *In90
                      LEAVE;
                   //*** Beg Add *** 08/08/22 ********************************
                   // Check all 10 possible reserves codes against Death Benefit
                   // 10 possible reserve codes. Give error If you get a match
                   Else;
                      If scType = 'G' And scToti <> 'Y' and *In90 <> *On;
                         ExSr srCmpRsvCodes;
                         If *In90 = *On;
                            Leave;
                         Endif;
                      Endif;
                   //*** End Add *** 08/08/22 ********************************
E05                Endif;                                                     //RTSTAT<>A
                   //
E04             Endif;                                                        //*In60=*On
                //
E03          Endif;                                                           //SDC,I1<>*Blank
             //
E02       EndDo;                                                              //I1->10
          //
       //*** Beg Add *** 08/08/22 *********************************************
       // Validate Death Benefit PV1 Codes when Gross plan
       If *In90 <> *On and scType = 'G' and scToti <> 'Y';
          FOr i1 = 1 to 10;
             xxKey = dsPv1Db2(i1);
             If xxKey <> *Blanks;
                Chain TBky pb201;
                If Not %Found(WTABL) or (%Found(WTABL) and B2stat = 'I');
                   *In90 = *On;
                   Select;
                      When i1 = 1;
                         erDbPv11 = *On;
                      When i1 = 2;
                         erDbPv12 = *On;
                      When i1 = 3;
                         erDbPv13 = *On;
                      When i1 = 4;
                         erDbPv14 = *On;
                      When i1 = 5;
                         erDbPv15 = *On;
                      When i1 = 6;
                         erDbPv16 = *On;
                      When i1 = 7;
                         erDbPv17 = *On;
                      When i1 = 8;
                         erDbPv18 = *On;
                      When i1 = 9;
                         erDbPv19 = *On;
                      When i1 = 10;
                         erDbPv110 = *On;
                   Endsl;
                Endif;
             Endif;
          EndfOr;
       Endif;
       // Check for duplicate Death Benefits PV1 codes
       If *In90 <> *On and scType = 'G' and scToti <> 'Y';
          FOr i1 = 1 to 10;
             str1Pv1 = dsPv1Db2(i1);
             If str1Pv1 <> *Blanks;
                j1 = 1;
                Dow j1 <= 10;
                   str2Pv1 = dsPv1Db2(j1);
                   If str1Pv1 = str2Pv1 And i1 <> j1;
                      *in90 = *on;
                      Select;
                         When i1 = 1;
                            erDbPv1D1 = *on;
                         When i1 = 2;
                            erDbPv1D2 = *on;
                         When i1 = 3;
                            erDbPv1D3 = *on;
                         When i1 = 4;
                            erDbPv1D4 = *on;
                         When i1 = 5;
                            erDbPv1D5 = *on;
                         When i1 = 6;
                            erDbPv1D6 = *on;
                         When i1 = 7;
                            erDbPv1D7 = *on;
                         When i1 = 8;
                            erDbPv1D8 = *on;
                         When i1 = 9;
                            erDbPv1D9 = *on;
                         When i1 = 10;
                            erDbPv1D10 = *on;
                      Endsl;
                   Endif;
                   j1 = j1 + 1;
                EndDo;
             Endif;
          EndfOr;
       Endif;
       //*** End Add *** 08/08/22 *********************************************
      //
      // VERBIAGE PER FLAG MUST BE 'C' Or BLANK
      //
          If SCVERB <> 'C'
          And SCVERB <> *Blank;
             *In47 = *On;
             *In90 = *On;
          Endif;                                                              //SCVERB
          //
          // VERBIAGE PER FLAG MUST BE 'C' Or BLANK
          //
          If SCTOTI <> 'Y'
          And SCTOTI <> *Blank;
             *In48 = *On;
             *In90 = *On;
          Endif;                                                              //SCTOTI
          //
          //  Auto renew must be 'N', If entered.
          //
          If SCRNW <> *Blank
          And SCRNW <> 'N';
             *In90 = *On;
             *In71 = *On;
          Endif;                                                              //SCRNW
          //
          //
          //  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
          //
       Endsr;                                                                 //ERRCK1
       //
       //===============================================================
       // ERRCK2 - SUBROUTINE FOr ERROr CHECKING SCR02
       //===============================================================
       //
       Begsr ErrCk2;
          //
          //
       Endsr;                                                                 //ERRCK2
       //===============================================================
       // ERRCK3 - SUBROUTINE FOr ERROr CHECKING SCR03
       //===============================================================
       Begsr ErrCk3;
          //
          //
       Endsr;                                                                 //ERRCK3
       //===============================================================
       // KjDo - SUBROUTINE FOr F10
       //===============================================================
       //
       //  F10=SEARCH
       //
       Begsr KjDo;
          //
          If Hfld = 'SCDC01'
          Or Hfld = 'SCDC02'
          Or Hfld = 'SCDC03'
          Or Hfld = 'SCDC04'
          Or Hfld = 'SCDC05'
          Or Hfld = 'SCDC06'
          Or Hfld = 'SCDC07'
          Or Hfld = 'SCDC08'
          Or Hfld = 'SCDC09'
          Or Hfld = 'SCDC10';
             //
     C                   MoveL     Hfld          YTMP              6
     C                   Move      YTMP          Y1                2 0
     C                   Move      *Blanks       SCKEY
      //
     C                   Call      'W2136R'
     C                   Parm      '2'           @@Call            1
     C     *InKC         Parm                    @@KC              1
     C     *InKL         Parm                    @@KL              1
     C                   Parm                    SCKEY            10
      //
      //
      //
      //
      //
      //
             If *InKL = *OFF
             And SCKEY <> *Blanks;
                //
     C                   MoveL     SCKEY         SDC(Y1)
     C     Y1            Add       51            Z1                3 0
      //
             Else;
     C     Y1            Add       50            Z1                3 0
             Endif;                                                           //*InKL=OFF,SCKEY
             //
             //  CHANGE FOrMAT OF EFFECTIVE DATE
             //
     C                   Z-Add     SCEFDT        SRDTI
     C                   Call      'SRCVTDAT'    DATCHK
     C                   MoveL     SRDTO         SCEFDT
      //
      //
     C                   Move      *On           *In91
     C                   Move      *On           *In(Z1)
      //
          Else;
             //
     C                   Move      *On           *In72
      //
             If Hfld = 'SCPLAN';
                ExSr KJPLAN;
     C                   Move      *OFF          *In72
             Endif;
             //
          Endif;                                                              //Hfld Ifeq
       Endsr;                                                                 //KjDo
       //
       //===============================================================
       // KJPLAN - SUBROUTINE FOr DEDUCTIBLE PLAN SEARCH
       //===============================================================
       Begsr KJPLAN;
          //
     C                   Move(P)   'DT'          XXTBID
     C                   MoveL     SCST          XXTBID
     C                   Z-Add     2             Y                 1 0
          Chain GIKY PGI01;
          *In60 = Not %Found;
          If *In60 = *On;
     C                   MoveL(P)  '  DT'        XXTBID
     C                   Z-Add     1             Y
          Endif;
          //
     C                   Move      *Blank        QQCall            1
     C                   MoveL     SCPRDS        QQPRDS
      //
      //      Try again on generic table If not found in state table.
     C                   Do        Y
     C                   Call      'W2493R'
     C                   Parm                    QQPRDS
     C                   Parm                    QQCall
     C                   Parm                    XXTBID
     C                   Parm                    SCKEY
     C                   Parm                    QQRTCD            1
             If SCKEY <> *Blank;
                LEAVE;
             Endif;                                                           //SCKEY = ' '
     C                   MoveL     '  CT'        XXTBID
          ENDDo;                                                              //Do 2
          //
          If QQRTCD = '*';
     C                   MoveL     SCKEY         SCPLAN
     C                   MoveL(P)  SCKEY         XXKEY
             Chain GTKY PGT01;
             *In60 = Not %Found;
             If *In60 = *OFF;
     C                   MoveL(P)  GTDESC        SCPNAM
     C                   Move      *On           *In51                          POS CUSR
     C                   Move      *On           *In91                          POS CUSR
     C                   Move      *OFF          *In81
             Else;
     C                   Move      *Blank        SCPNAM
             Endif;                                                           //*In60=*OFF
          Endif;                                                              //QQRTCD='*'
          //
       Endsr;                                                                 //KJPLAN
       //
       //===============================================================
       // MvDbSc - SUBROUTINE FOr MOVING DATA BASE FIELDS TO SCREEN
       //===============================================================
       //
       Begsr MvDbSc;
          //
     C                   Move      DDST          SCST
     C                   Move      DDTYPE        SCTYPE
     C  N40              Move      DDSTAT        SCSTAT
     C   40              Move      'A'           SCSTAT
     C                   Move      DDCLAS        SCCLAS
     C                   MoveL     DDDESC        SCDESC
     C                   Z-Add     DDPCT         SCPCT
     C                   Z-Add     DDAMPO        SCAMPO
     C                   Z-Add     DDAMAG        SCAMAG
     C                   MoveL     DDCODE        SCCODE
     C                   Z-Add     DDEFDT        SRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   MoveL     SRDTO         SCEFDT
     C                   Move      DDDC01        SCDC01
     C                   Move      DDDC02        SCDC02
     C                   Move      DDDC03        SCDC03
     C                   Move      DDDC04        SCDC04
     C                   Move      DDDC05        SCDC05
     C                   Move      DDDC06        SCDC06
     C                   Move      DDDC07        SCDC07
     C                   Move      DDDC08        SCDC08
     C                   Move      DDDC09        SCDC09
     C                   Move      DDDC10        SCDC10
     C                   Move      DDPLAN        SCPLAN
     C                   Move      *Blank        SCPNAM
     C                   MoveL(P)  SCST          XXTBID
     C                   Move      'DT'          XXTBID
     C                   MoveL(P)  DDPLAN        XXKEY
          Chain GTKY PGT01;
          *In60 = Not %Found;
          If *In60 = *On;
     C                   MoveL(P)  '  DT'        XXTBID
             Chain GTKY PGT01;
             *In60 = Not %Found;
          Endif;
          If *In60 = *OFF;
     C                   MoveL(P)  GTDESC        SCPNAM
          Endif;
     C                   Move      DDVERB        SCVERB
     C                   Move      DDALAE        SCALAE
      /Free
       SCvarA = DDvarA;
     C                   Move      DDTOTI        SCTOTI
     C                   Move      DDRNW         SCRNW
         scIncMdIn = DDincMdIn;
         If scIncMdIn = *Blank;
            scIncMdIn = 'B';
         Endif;
      //
       //*** Beg Add *** 08/08/22 *********************************************
       scPv1Db01 = ddPv1Db01;
       scPv1Db02 = ddPv1Db02;
       scPv1Db03 = ddPv1Db03;
       scPv1Db04 = ddPv1Db04;
       scPv1Db05 = ddPv1Db05;
       scPv1Db06 = ddPv1Db06;
       scPv1Db07 = ddPv1Db07;
       scPv1Db08 = ddPv1Db08;
       scPv1Db09 = ddPv1Db09;
       scPv1Db10 = ddPv1Db10;
       //*** End Add *** 08/08/22 *********************************************
       Endsr;                                                                 //MvDbSc
       //
       //===============================================================
       // MvScDb - SUBROUTINE FOr MOVING SCREEN FIELDS TO DATA BASE
       //===============================================================
       //
       Begsr MvScDb;
          //
     C                   Z-Add     XXCO#         DDCO#
     C                   Z-Add     XXFND         DDFND
     C                   Move      SCST          DDST
     C                   Move      SCSTAT        DDSTAT
     C                   Move      *Blanks       DDDESC
     C                   MoveL     SCDESC        DDDESC
     C                   Move      SCTYPE        DDTYPE
     C                   Move      SCCLAS        DDCLAS
     C                   Z-Add     SCPCT         DDPCT
     C                   Z-Add     SCAMPO        DDAMPO
     C                   Z-Add     SCAMAG        DDAMAG
     C                   MoveL     SCCODE        DDCODE
     C                   Z-Add     SCEFDT        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   MoveL     SRDTO         DDEFDT
     C                   Move      SCDC01        DDDC01
     C                   Move      SCDC02        DDDC02
     C                   Move      SCDC03        DDDC03
     C                   Move      SCDC04        DDDC04
     C                   Move      SCDC05        DDDC05
     C                   Move      SCDC06        DDDC06
     C                   Move      SCDC07        DDDC07
     C                   Move      SCDC08        DDDC08
     C                   Move      SCDC09        DDDC09
     C                   Move      SCDC10        DDDC10
     C                   Move      SCPLAN        DDPLAN
     C                   Move      SCALAE        DDALAE
       DDvarA  = SCvarA;
     C                   Move      SCVERB        DDVERB
     C                   Move      SCTOTI        DDTOTI
     C                   Move      SCRNW         DDRNW
          DDincMdIn = scIncMdIn;
          If DDIncMdIn = 'B';
             DDIncMdIn = *Blank;
          Endif;
      //
       //*** Beg Add *** 08/08/22 *********************************************
       // Clear all 10 fields then read through the array of fields and load to
       // the database without skipping any fields even if the screen fields skip
       ddPv1Db01 = *blanks;
       ddPv1Db02 = *blanks;
       ddPv1Db03 = *blanks;
       ddPv1Db04 = *blanks;
       ddPv1Db05 = *blanks;
       ddPv1Db06 = *blanks;
       ddPv1Db07 = *blanks;
       ddPv1Db08 = *blanks;
       ddPv1Db09 = *blanks;
       ddPv1Db10 = *blanks;
       If scType = 'G' and scToti <> 'Y';
          j1 = 1;
          For i2 = 1 to 10;
            If dsPv1Db2(i2) <> *blanks;
               Select;
                  When j1 = 1;
                     ddPv1Db01 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 2;
                     ddPv1Db02 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 3;
                     ddPv1Db03 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 4;
                     ddPv1Db04 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 5;
                     ddPv1Db05 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 6;
                     ddPv1Db06 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 7;
                     ddPv1Db07 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 8;
                     ddPv1Db08 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 9;
                     ddPv1Db09 = dsPv1Db2(i2);
                     j1 = j1 + 1;
                  When j1 = 10;
                     ddPv1Db10 = dsPv1Db2(i2);
                     j1 = j1 + 1;
               Endsl;
            Endif;
          Endfor;
       Else;
          scPv1Db01 = *blanks;
          scPv1Db02 = *blanks;
          scPv1Db03 = *blanks;
          scPv1Db04 = *blanks;
          scPv1Db05 = *blanks;
          scPv1Db06 = *blanks;
          scPv1Db07 = *blanks;
          scPv1Db08 = *blanks;
          scPv1Db09 = *blanks;
          scPv1Db10 = *blanks;
       Endif;
       //*** End Add *** 08/08/22 *********************************************

       Endsr;                                                                 //MvScDb
       //
       //===============================================================
       // OTHERR - SUBROUTINE FOr PROCESSING OTHER MODES
       //===============================================================
       //
       Begsr OthErr;
       Endsr;                                                                 // OTHERR
       //
       //===============================================================
       // OTHFKS - SUBROUTINE FOr OTHER FUNCTION KEYS
       //===============================================================
       //
       Begsr OthFks;
          //
     C     *InKj         CASEQ     *On           KjDo                     90
     C                   ENDCS
       //*** Beg Add *** 08/08/22 *********************************************
       // Initialize PV1 errOr indicatOrs
       erDbPv11  = *off;
       erDbPv12  = *off;
       erDbPv13  = *off;
       erDbPv14  = *off;
       erDbPv15  = *off;
       erDbPv16  = *off;
       erDbPv17  = *off;
       erDbPv18  = *off;
       erDbPv19  = *off;
       erDbPv110 = *off;
       erDbPv1Rt1 = *off;
       erDbPv1Rt2 = *off;
       erDbPv1Rt3 = *off;
       erDbPv1Rt4 = *off;
       erDbPv1Rt5 = *off;
       erDbPv1Rt6 = *off;
       erDbPv1Rt7 = *off;
       erDbPv1Rt8 = *off;
       erDbPv1Rt9 = *off;
       erDbPv1R10 = *off;
       erDPv1Rt1 = *off;
       erDPv1Rt2 = *off;
       erDPv1Rt3 = *off;
       erDPv1Rt4 = *off;
       erDPv1Rt5 = *off;
       erDPv1Rt6 = *off;
       erDPv1Rt7 = *off;
       erDPv1Rt8 = *off;
       erDPv1Rt9 = *off;
       erDPv1R10 = *off;
       erDbPv1D1 = *off;
       erDbPv1D2 = *off;
       erDbPv1D3 = *off;
       erDbPv1D4 = *off;
       erDbPv1D5 = *off;
       erDbPv1D6 = *off;
       erDbPv1D7 = *off;
       erDbPv1D8 = *off;
       erDbPv1D9 = *off;
       erDbPv1D10 = *off;
       //*** End Add *** 08/08/22 *********************************************
      //
       Endsr;                                                                 //OTHFKS
       //
       //===============================================================
       // PGMEND - SUBROUTINE FOr ENDING THE PROGRAM
       //===============================================================
       //
       Begsr PgmEnd;
       Endsr;
       //
       //===============================================================
       // PgmStr - SUBROUTINE FOr STARTING THE PROGRAM
       //===============================================================
       //
       Begsr PgmStr;
        //*** Beg Add *** 08/08/22 *********************************************
        msDbPv1    = 'Invalid Payment Value 1 code Entered.';
        msDedPv1Rt   = 'This reserve type is assigned to a death benefit ' +
         'PV1 code.';
        msDedRtPv1   = 'This PV1 code has a reserve type already included ' +
         'in the deductible reserve hierarchy.';
        msDbPv1Dup   = 'This PV1 code is a duplicate of another PV1 ' +
         'code that was entered.';
        //*** End Add *** 08/08/22 *********************************************
          //
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      //
      //** BEGIN Add *** 03/23/98 **************************************
      //
      //  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
      //
      //  Access System Control file 'WC' recOrd
      //
     C                   MoveL     'WC'          XSKEY             2
      //
          Chain XSKEY SWC01;
          *In60 = Not %Found;
          //
          If *In60 = *OFF;
             //
             If WCFL08 <> *Blank;
                //
                If WCFL08 = 'A';
     C                   Move      *On           *In94                          ADMIN ONLY
                Endif;                                                        //WCFL08=A
                //
                If WCFL08 = 'C';
     C                   Move      *On           *In95                          CLAIMS ONLY
                Endif;                                                        //WCFL08=C
                //
             Endif;                                                           //WCFL08<>*Blank
             //
          Endif;                                                              //*In60=*OFF
          //
          //
     C                   Move      *OFF          *In40
      //
          If @@MODE = 'A';
     C                   Move      *On           *In40
          Endif;                                                              //@@MODE EQ A
          //
     C                   MoveL(P)  OPTNM         ##DATA
      //
     C                   Call      'SRCENTER'
     C                   Parm                    ##DATA           80
     C                   Parm      40            ##LENG            3 0
     C                   MoveL     ##DATA        SCPRDS
     C                   Move      *On           DSPTYP
      //
          Chain @@RRN DbrcdF;
          *In60 = Not %Found;
     C                   Z-Add     @@CO#         XXCO#
     C                   Z-Add     @@FND         XXFND
     C                   Z-Add     @@FND         SCFND
     C                   Move      DDST          XXST
     C                   Z-Add     DDEFDT        XXEFDT
     C                   MoveL     DDCODE        XXCODE
      //
          Chain FNKY PFN01;
          *In60 = Not %Found;
          If *In60 = *OFF;
     C                   MoveL     FNNAME        SCFNAM
          Endif;                                                              //*In60 EQ 0
          //
         DisblVarA = *On;
         VisVarA = *off;
         RtvCltID(CltId);
         If CltId = '150';
            DisblVarA = *off;
            VisVarA = *On;
         Else;
           scVarA = *Blanks;
         Endif;
       If *In09;
          dspMode = *On;
          disIncMdIn = *On;
       Else;
          dspMode = *Off;
          disIncMdIn = *Off;
       Endif;
          ExSr MvDbSc;
          //
       Endsr;                                                                 //PgmStr
       //
       //===============================================================
       // PreOth - SUBROUTINE FOr OTHER MODE PRE-PROCESSING
       //===============================================================
       //
       Begsr PreOth;
          SELECT;
             //
          When @@MODE = 'S';
             ExSr ChgSts;
             Return;
             //
          Endsl;
       Endsr;
       //
       //*** Beg Add *** 08/08/22 *********************************************
       //======================================================================
       // srCmpRsvCodes - Compare PV1 Reserves Codes to Death Benefit Res Codes
       //======================================================================
       Begsr srCmpRsvCodes;

       // Cycle through the PV1 Death Benefits array for all 10 elements
       FOr i2 = 1 to 10;
          xxKey = dsPv1Db2(i2);
          Chain TBky pb201;
          // Check for duplicate reserve codes for all 10 fields
          If %Found(WTABL) And (sDc(i1) = B2rC01 Or sDc(i1) = B2rC02 Or
           sDc(i1) = B2rC03 Or sDc(i1) = B2rC04 Or sDc(i1) = B2rC05 Or
           sDc(i1) = B2rC06 Or sDc(i1) = B2rC07 Or sDc(i1) = B2rC08 Or
           sDc(i1) = B2rC09 Or sDc(i1) = B2rC10);
             // Set on the correct errOr indicator for one of the 10 Reserve
             // Code fields
             Select;
                When i1 = 1;
                   erDPv1Rt1 = *On;
                When i1 = 2;
                   erDPv1Rt2 = *On;
                When i1 = 3;
                   erDPv1Rt3 = *On;
                When i1 = 4;
                   erDPv1Rt4 = *On;
                When i1 = 5;
                   erDPv1Rt5 = *On;
                When i1 = 6;
                   erDPv1Rt6 = *On;
                When i1 = 7;
                   erDPv1Rt7 = *On;
                When i1 = 8;
                   erDPv1Rt8 = *On;
                When i1 = 9;
                   erDPv1Rt9 = *On;
                When i1 = 10;
                   erDPv1R10 = *On;
             Endsl;
             // Set on the cOrrect error indicator for one of the 10 Death
             // Benefit fields
             Select;
                When i2 = 1;
                   erDbPv1Rt1 = *On;
                When i2 = 2;
                   erDbPv1Rt2 = *On;
                When i2 = 3;
                   erDbPv1Rt3 = *On;
                When i2 = 4;
                   erDbPv1Rt4 = *On;
                When i2 = 5;
                   erDbPv1Rt5 = *On;
                When i2 = 6;
                   erDbPv1Rt6 = *On;
                When i2 = 7;
                   erDbPv1Rt7 = *On;
                When i2 = 8;
                   erDbPv1Rt8 = *On;
                When i2 = 9;
                   erDbPv1Rt9 = *On;
                When i2 = 10;
                   erDbPv1R10 = *On;
             Endsl;
             *In90 = *On;
             Leave;
          Endif;
       Endfor;

       Endsr;
       //*** End Add *** 08/08/22 *********************************************
       //===============================================================
       // Updatr - SUBROUTINE FOr UPDATING DATA BASE RECOrDS
       //===============================================================
       //
       Begsr Updatr;
          //
          Chain @@RRN DbrcdF;
          *In99 = Not %Found;
          //
B02       If *In99 = *OFF;
             //
             ExSr MvScDb;
             //
     C                   MoveL     Q1USER        DDUSER
     C                   MoveL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-Add     DSDATE        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   MoveL     SRDTO         DDDT
     C                   MoveL     DSTIME        DDTIME
      //
             Update DbrcdF;
             //
E02       Endif;                                                              //*In99=*OFF
          //
     C                   Move      *On           OUTFLG
      //
       Endsr;                                                                 //Updatr
       //
       //===============================================================
       // WriteR - SUBROUTINE FOr WriteING DATA BASE RECOrDS
       //===============================================================
       //
       Begsr WriteR;
          //
          Clear DbrcdF;
          //
          ExSr MvScDb;
          //
     C                   MoveL     Q1PGM         DDPGM
     C                   MoveL     Q1USER        DDUSER
     C                   TIME                    DSDTTM
     C                   Z-Add     DSDATE        SRDTI
     C                   Call      'SRCVTDAT'    DTCYMD
     C                   MoveL     SRDTO         DDDT
     C                   MoveL     DSTIME        DDTIME
     C                   MoveL     Q1PGM         DDIPGM
     C                   MoveL     Q1USER        DDIUSR
     C                   MoveL     SRDTO         DDIDT
     C                   MoveL     DSTIME        DDITIM
      //
          Write DbrcdF;
          //
          Feod WTDDP;
          //
     C                   MoveL     'C'           @@MODE
      //
       Endsr;                                                                 //WriteR
