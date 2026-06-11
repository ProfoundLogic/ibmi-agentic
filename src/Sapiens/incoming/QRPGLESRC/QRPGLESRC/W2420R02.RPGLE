     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W2420R01 - Bank Information Maintenance Program
     H*****************************************************************
     H*
     H*  Description:
     H*
     H*    This program maintains the WCMS Bank Information file.
     H*
     H*    F10 is used for all search pgm execution.  Cursor is placed
     H*    in field to search.  If a valid search program exists, a
     H*    search field list will display.  If no valid search program
     H*    exists, an error message will display.
     H*
     H*    Bank Account Master file is accessed to assign Bank Name/Address
     H*    fields.  The Bank Info record is updated with this information.
     H*    During Daily Batch Check Processing the Bank Info file Bank Name/
     H*    Address fields will be used for check assignments.
     H*
     H*  Called From:
     H*    W2420R
     H*
     H*  Calls To:
     H*    SRCENTER
     H*    SRCVTDAT
     H*    SY600R
     H*
     H*  Created:  02/13/97  C:30  Project:  87WBL
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
    H*                                                               *
    H* 04/19/21 C:079 Support: 90/95537                              *
    H*                Add Bank Code to API                           *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                Add E-Pay Screens                              *
     H*                                                               *
     H* 03/06/19 C:180 Project: 126WFR                                *
     H*                Increase size of "Text on check"               *
     H*                                                               *
     H* 08/22/18 C:180 Project: 126WFR                                *
     H*                Add "Text on check"                            *
     H*                                                               *
     H* 05/21/18 C:062 Project: 129WVC                                *
     H*                Add "Print 2 blank lines if over $ limit"      *
     H*                                                               *
     H* 07/30/15 C:136 Project: WC2337                                *
     H*               -Edit effective date against bank master        *
     H*               -Edit to ensure that EFT transactions           *
     H*                don't exist before allowing EFT flag to be     *
     H*                changed to 'MICR only'                         *
     H*                                                               *
     H*  05/10/13 C:283 Support: WA099 82084                          *
     H*                 EFT missing an edit.                          *
     H*                                                               *
     H*  12/21/12 C:282 Project:WC2044 Priss 52                       *
     H*                 Code for validating Bank Account              *
     H*                                                               *
     H*  04/17/12 C:282 Project:WC2044                                *
     H*                 Modified the program by replacing the EFT     *
     H*                 status with EFT Flag.                         *
     H*                                                               *
     H*  03/12/12 C:255 Project:114WDE                                *
     H*                 Modified program to display SCR04 only when   *
     H*                 CLT = 025 or any EFT module is purchased.     *
     H*                 When EFT module is purchased, processing for  *
     H*                 below fields added.                           *
     H*                 1) EFT Debit comp. ID                         *
     H*                 2) EFT Credit comp. ID                        *
     H*                                                               *
     H*  10/05/11 C:279 SUPPORT: 77992                                *
     H*                 PREVENT RETURN PREMIUM BANK FROM BEING SET    *
     H*                 UP UNLESS THE RETURN PREMIUM CODES ARE        *
     H*                 POPULATED IN BANK MASTER FOR THAT ACCOUNT.    *
     H*                                                               *
     H*  04/19/10 C:235 PROJECT: 25WRG                                *
     H*                 CLAIMS GENERAL RETROFIT                       *
     H*                                                               *
     H*  07/27/09 C:136 PROJECT: 114WUK                               *
     H*                 ALLOW CSI TO PROCESS XML TYPE TRANSACTIONS    *
     H*                                                               *
      *  06/25/09 C:136 PROJECT: WC1806                               *
      *  06/29/09 C:191 PROJECT: WC1806                               *
     H*                -MAKE APP TYPE SEARCHABLE AND TABLE DRIVEN     *
     H*                -REMOVE STATE FIELD (NOT USED)                 *
     H*                -DEFAULT ROUTING INFO FROM BANK MASTER ON ADD  *
     H*                                                               *
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 07/18/07 C:136 PROJECT: WC1491 (INDEPENDENT CASH)             *
     H*               -ADD 'IC' AS A BANK APPLICATION TYPE            *
     H*               -DISPLAY 'RP' IF RETURN PREMIUM IS ENABLED      *
     H*               -DISPLAY 'IC' IF INDEPENDENT CASH IS ENABLED    *
     H*                                                               *
     H* 03/23/07 C:15  Support: WC114/57401                           *
     H*                Added code to error route to correct issue     *
     H*                with incorrect error msg displaying.           *
     H*                                                               *
     H* 02/27/07 C:185 Project: 114WMF Back to Base.                  *
     H*                114WED Dated 07/26/02.                         *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H* 01/20/04 C:136 Project: WC1033                                *
     H*                Add return premium ('RP') as an application    *
     H*                type.                                          *
     H*                                                               *
     H* 07/25/02 C:15  Support: WC065/33554                           *
     H*                Chgd edit on effective date, to allow a new    *
     H*                bank acct to be effective on the system date.  *
     H*                                                               *
     H* 04/13/01 C:61  PROJECT WC1020                                 *
     H*              - Add entry for Bank Code.                       *
     H*              - Remove prompt for EFT.                         *
     H*
     H*  12/01/00 C:33  PROJECT: WC997
     H*                 ALLOW ENTRY OF MULTIPLE ACTIVE BANK ACCOUNTS
     H*                 FOR 'AG' TYPES TO ALLOW FOR EFT RECORDS.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H* 03/23/98  C:58 PROJECT:   SUPPORT Sequence # 10788
     H*           Increased R-T fields size SCR03: RECOMPILE ONLY.
     H*
     H* 02/02/98  47  PROJECT: 91XXX
     H*           ADDED A THIRD SCREEN TO ENTER FOUR 40A FREE FORM
     H*           FIELDS TO PRINT IN THE UPPER LEFT OF THE CHECK AND
     H*           FILECOPY.
     H*           RENAMED SCR02 TO SCR03.
     H*           MADE THIS NEW SCREEN SCR02.
     H* ***************************************************************
     H*  Indicators:
     H*
     H*   09 - Display Mode.
     H*   49 - No Search pgm for F10 field search error message.
     H*   60 - General File Access
     H*   71 - 76  Used to select error messages
     H*            71 highest order bit, values used in ascending
     H*            order.  Last value used = 24 decimal.
     H*
     H*   80 -     Voucher
     H*
     H*   81 - 86  Used to position cursor
     H*            81 highest order bit, values used in ascending
     H*            order.  Last value used =  7 decimal.
     H*
     H*   90 - Edit Error occurred.
     H*   99 - General File access.
     H*
     H*****************************************************************
      *  Bank Info Screen
     FW2420F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  WCMS Bank Information
     FWMBIP     UF A E             DISK
     F                                     RENAME(PBI01:DBRCDF)
     f*** Beg Add *** 07/20/20 *****************************************
     f                                     InfDs(DBFDS2)
     f*** End Add *** 07/20/20 *****************************************
      *
     FWMBIL6    IF   E           K DISK
     F                                     RENAME(PBI01:DBRCDF2)
      *
      *  WCMS Fund Master
     FWMFNL     IF   E           K DISK
      *
      *  WCMS Bank Account Master
     FWSCKL     IF   E           K DISK
      *
      *  WCMS Check Signature file
     FWMSGL1    IF   E           K DISK
      *
      *  User Parameters
     FWMUPL     IF   E           K DISK
      *
      *  System table file
      *
      *  Fund Auxiliary file
     FWDF2L     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     f*** Beg Add *** 07/20/20 ***************************************
     FWMEVML1   IF   E           K DISK
     FWDB2L1    IF   E           K DISK
     f*** Beg Add *** 07/20/20 *****************************************
     f                                     InfDs(DBFDS)
     f*** End Add *** 07/20/20 *****************************************
     f*** End Add *** 07/20/20 ***************************************
      *
     d CltId           s              3
     d*
     dRtvCltId         pr                  extPgm('RTVCLTID')
     d CltId                          3
     d*** Beg Add *** 07/20/20 ***************************************
     d EPayInfo        pr                  extPgm('W2420R03')
     D outKc                          1
     D outKl                          1
     D inMode                         1
     D inRrn                          9  0
     D inRrn2                         9  0
     D outKc           s              1
     D outKl           s              1
     D inMode          s              1
     D inRrn           s              9  0
     D inRrn2          s              9  0
     d isEpay          s               n
     d scrn#           s              1  0
     d ##Rrn           s              9  0
     d/Copy sprModule
     d*** End Add *** 07/20/20 ***************************************
      *
     D OPTNM           C                   CONST('Bank Information Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('WA001R')
      *
     D/copy WPRBANK
     d #co#            s              3
     d #fnd            s              3
     d ##appl          s              2
     d ##srdt          s              8
     d ##acct          s             20
     d ##call          s              1
     d ##hdcy          s              1
     d ##chkq          s             10
     d ##ftyp          s             10
     d ##rtcd          s              1
     d ##dt            s              7
     d ##cpyq          s             10
     d ##cktp          s             10
     d ##eftc          s              1
     d ##coid          s             20
     d ##case          s              7
     d ##data1         s             75
     d hasEFT          s               n
     d*** Beg Add *** 07/20/20 ***************************************
     d dbfDS           DS
     d  dbRrn                397    400B 0
     d dbfDS2          DS
     d  dbRrn2               397    400B 0
     d*** End Add *** 07/20/20 ***************************************
     C/COPY SCOPYMT05C
      *  Bank Information FILE
      *
     C     KEY1          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C                   KFLD                    XXACCT
     C                   KFLD                    XXEFDT
      *
     C     BIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C                   KFLD                    XXEFDT
      *
     C     BIKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
      *
     C     BIKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXACCT
      *
     C     BIKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXCKTP
      *
     C     BIKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
      *
     C     BIKY7A        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
      *
      *  Fund Master file
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Signature file
      *
     C     SGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKNAME
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
      *  Company Name
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     *LIKE         DEFINE    BICO#         XXCO#
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIACCT        XXACCT
     C     *LIKE         DEFINE    BIACCT        @@ACCT
     C     *LIKE         DEFINE    BIACCT        ##ACCT
     C     *LIKE         DEFINE    BICKTP        XXCKTP
     C     *LIKE         DEFINE    BIEFDT        SVEFDT
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C     *LIKE         DEFINE    BINAM1        ##NAM1
     C     *LIKE         DEFINE    BINAM2        ##NAM2
     C     *LIKE         DEFINE    BI10A1        ##10A1
     C     *LIKE         DEFINE    BI10A2        ##10A2
     C     *LIKE         DEFINE    BICMD1        ##CMD1
     C     *LIKE         DEFINE    BICMD2        ##CMD2
     C     *LIKE         DEFINE    BINAM1        KKNAME
     C     *LIKE         DEFINE    BILOGO        SVLOGO
     C     *LIKE         DEFINE    BIEFTC        SVEFTC
      *
      *  ---------------------------
      *  P r o g r a m     S t a r t
      *  ---------------------------
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FND             3 0
     C*
      *
      *  GET THE USER PARAMETER FILE
      *
      *
      *
       //*** Beg Add *** 07/20/20 ************************************
       //=============================================================
       // CallR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
       //=============================================================
       Begsr CallR;
          If scrn# = #ofScr;
        //*** Beg Chg *** 04/19/21 ***************************************
        //***Chain (BIco#:BIappl:BIfnd:BIacct:BIefDt) pb201;
             Chain (BIco#:BIappl:BIfnd:BIacct:BIckTp:BIefDt) pb201;
        //*** End Chg *** 04/19/21 ***************************************
             If %Found(WDB2L1);
                Select;
                When @@mode = 'A' or @@mode = 'C' or @@mode = 'P';
                   ##rrn = DBrrn;
                   ##mode = 'C';
                   inMode = 'C';
                Other;
                   ##rrn = DBrrn;
                   ##mode = 'D';
                   inMode = 'D';
                Endsl;
             Else;
                Select;
                When @@mode = 'A' or @@mode = 'C' or @@mode = 'P';
                   ##rrn = 0;
                   ##mode = 'A';
                   inMode = 'A';
                Other;
                   ##rrn = 0;
                   ##mode = 'D';
                   inMode = 'D';
                Endsl;
             Endif;

             inRrn  = ##rrn;
             inRrn2 = DBrrn2;
             Callp EpayInfo(outKc:outKl:inMode:inRrn:inRrn2);
             *inKc = outKc;
             *inKl = outKl;
          Endif;
       Endsr;
       //==============================================================
       // CallRt - RETURN FROM ADDITIONAL SCREEN PROGRAMS
       //==============================================================
       Begsr CallRt;
          Select;
             When @@Mode = 'C' or @@Mode = 'A' or @@Mode = 'P';
                If *Inkl = *on;
                   *in90 = *on;
                   @@mode = 'C';
                   @@rrn = ##rrn;
                   *in40 = *off;
                   *in41 = *off;
                Endif;
             When @@Mode = 'D';
                If *inkl = *on;
                   *in90 = *on;
                Endif;
                Exsr PreDsp;
             Other;
                Exsr PreOth;
          Endsl;
       Endsr;
       //*** End Add *** 07/20/20 ************************************
      ****************************************************************
      *  DLTPRE                                                      *
      ****************************************************************
     C     DLTPRE        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  DSPLYR - Indicate Screen has been displayed.                *
      ****************************************************************
     C     DSPLYR        BEGSR
      *
     C                   MOVE      *ON           OUTFLG
       //*** Beg Add *** 07/20/20 *************************************
                         If not *Inkl;
                            scrn# = scrn# + 1;
                         Endif;
                         If scrn# = #ofScr and isEpay;
                            Exsr callR;
                         Endif;
       //*** End Add *** 07/20/20 *************************************
      *
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK1 - Edit W2420F02 SCR01 fields                         *
      ****************************************************************
     C     ERRCK1        BEGSR
      *
      *  Voucher Flag
      *
     C                   MOVEL     SCACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C     CKVCHR        ANDEQ     'Y'
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   Z-ADD     3             #OFSCR
     C*
      /Free
       If cltid = '025' or WCfl17 = 'Y' or
        WCfl24 = 'Y' or WCfl35 = 'Y';
          #ofscr = 4;
       Endif;
      /End-Free
     c                   ENDIF
      *
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CKAI01        SCBKNM
     C                   MOVEL     CKAI02        SCBKAD
     C                   MOVEL     CKAI03        SCBKCS
     C                   MOVE      CKVCHR        SCVCHR
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           SCSTAT
     C                   MOVEL     CKONUS        SCONUS
     C                   MOVEL     CKCKDG        SCCKDG
     C                   MOVEL     CKBNK#        SCTRNS
     C                   MOVEL     CKTRNS        SCRTE1
     C                   MOVEL     CKAXOU        SCAUON
     C                   MOVEL     CKRTUP        SCRTE2
     C                   MOVEL     CKRTLW        SCRTE3
     C                   MOVEL     CKNAM1        SCNAM1
     C                   MOVEL     CKNAM2        SCNAM2
     C                   ENDIF
     C                   ENDIF
      *
     C                   WRITE     SCR01
      *
     C                   CLEAR                   *IN89
      *
      *  If DISPLAY mode, ignore Edit processing.
      *  If ADD mode
      /free
       If scHdCy = *blank;
          scHdCy = 'N';
       Endif;
       If scTrst = *blank;
          scTrst = 'N';
       Endif;
       If scLogo = *blank;
          scLogo = 'N';
       Endif;
      /end-free

     C     @@MODE        IFEQ      'A'
      *
      *  The Fund Number cannot be zero       (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCFND         ANDEQ     *ZERO
     C                   SETON                                        9020
     C                   ENDIF
      *
      *  The Fund Number must exist in Fund Master
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9021
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  The Account Number cannot be BLANKS  (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCACCT        ANDEQ     *BLANKS
     C                   SETON                                        90
     C                   MOVEA     '010011'      *IN(71)
     C                   ENDIF
      *
      *  The Bank Account Number must exist in Account Master
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90
     C                   MOVEA     '010100'      *IN(71)
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  If Group Aux record indicates that it is a voucher group,
      *  then the Bank Acct record must be a voucher type.
      *
     C     *IN90         IFEQ      *OFF
     C     CKVCHR        ANDNE     'Y'
     C                   Z-ADD     SCFND         XXFND
     C     FNKEY         CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C     F2CTYP        ANDEQ     'V'
     C                   SETON                                        90
     C                   MOVEA     '011100'      *IN(71)
     C                   ENDIF
     C                   ENDIF
      *
      *  The Effective Date cannot be zero       (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDEQ     *ZERO
     C                   SETON                                        9087
     C                   ENDIF
      *
      *  Effective Date must be valid Date
      *
     C     SCEFDT        IFNE      *ZERO
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C                   WRITE     SCR01
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9088
     C                   ELSE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVEFDT
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
      *
      * MAKE SURE EFT INFO IS SET UP IN BANK MASTER BEFORE ALLOWING
      * AN EFT BANK CODE
      *
      *  Validate the bank code by calling WA023R1.  This pgm has the
      *  bank codes hardcoded.
      *
     C                   MOVE      XXCO#         #CO#
     C                   MOVE      FNFND         #FND
      /Free
       If *IN90 = *Off;
           If (scCKTP = 'EFT');
              *IN61 = *On;
              *IN90 = *On;
           EndIf;
       EndIf;
       // Validate the bank code by procedure call ValidBankCode.

       ##fyr  = *Zeros;
       ##case = *Zeros;
       ##appl = SCappl;
       ##data1 = *Blanks;
       ##cktp = SCcktp;

       If *IN90 = *Off;
          If Not ValidBankCode(#co#: #fnd: ##fyr: ##case: ##appl:
             ##data1: ##cktp);
             *In25 = *On;
             *In90 = *On;
          EndIf;
       EndIf;

      /End-Free
      *
      *  Application must be one of (WC, AG, PC, GL, WP)
      *
     C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDEQ     'A'
      *
     C                   MOVE      *OFF          ERRFLG            1
     C* VALIDATE APPLICATION TYPE FROM TABLE
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCAPPL        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C     *IN60         ANDEQ     *OFF
     C                   MOVE      *ON           ERRFLG
     C                   ELSE
     C*
     C* CHECK APPLICATION FLAGS
     C                   SELECT
     C     SCAPPL        WHENEQ    'WP'
     C     WCFL24        ANDNE     'Y'
     C                   MOVE      *ON           ERRFLG
     C     SCAPPL        WHENEQ    'RP'
     C     WCFL25        ANDNE     'Y'
     C                   MOVE      *ON           ERRFLG
     C     SCAPPL        WHENEQ    'IC'
     C     WCFL36        ANDNE     'Y'
     C                   MOVE      *ON           ERRFLG
     C                   ENDSL
     C                   ENDIF
      *
     C     ERRFLG        IFEQ      *ON
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000110'      *IN(71)
     C                   ENDIF
     C*
     C                   ENDIF
      *
      /free
       IF *IN90 = *Off And
          (SCeftc <> *blanks And SCeftc <> 'E' And SCeftc <> 'B');
             *In62 = *On;
             *In90 = *On;
       Endif;

       IF *IN90 = *Off And
          (Sceftc = *blanks) And
          (CKbnk# = *blanks or CKtrns = *blanks Or
           CKonus = *blanks or CKrtup = *blanks Or
           CKrtlw = *blanks);
             *IN63 = *On;
             *IN90 = *On;
       EndIf;

       IF *IN90 = *Off And
          (Sceftc = 'E' or Sceftc = 'B') And
          (CKidsa = *zero or CKiora = *zero);
             *IN64 = *On;
             *IN90 = *On;
       EndIf;
      /end-free
      *  The CO#/ACCT/EFDT/FND can NOT already exist in Bank Info file
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCACCT        XXACCT
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   Z-ADD     SCFND         XXFND
     C                   MOVE      SCAPPL        XXAPPL
     C                   MOVEL     SCCKTP        XXCKTP
     C*
      *
      /free
       Setll key1 dbrcdf2;
       Reade key1 dbrcdf2;
       Dow Not %EOF(WMBIL6);

       // Only check flags if existing bank info is active
          If BIstat = 'A';
             Select;
             When SCeftc = *blanks and BIeftc = 'E';
                *IN26 = *On;
                *IN90 = *On;
             When SCeftc = 'E' and BIeftc = *blanks;
                *IN27 = *On;
                *IN90 = *On;
             Other;
                *IN23 = *On;
                *IN90 = *On;
                Leave;
             EndSl;
          Endif;
       Reade key1 dbrcdf2;
       EndDo;
      /end-free
      *
     C                   ENDIF
      *
      *  If screen Effective Date LE Today, then
      *  Call API WA023R to determine the "Current" Bank Acct. based
      *  on CKVCHR value using Today as Search Date.
      *  If a "Current" Bank Acct. exists, then display Error msg.
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAY            7 0
      *
     C     *IN90         IFEQ      *OFF
     C     XXEFDT        IFLT      XXTDAY
      *
     C                   SELECT
     C     CKVCHR        WHENEQ    'Y'
     C                   MOVE      'V'           ##CALL
     C     CKVCHR        WHENEQ    'N'
     C                   MOVE      'C'           ##CALL
     C                   OTHER
     C                   MOVE      CKVCHR        ##CALL
     C                   ENDSL
      *
     C                   MOVE      FNCO#         #CO#
     C                   MOVE      FNFND         #FND
     C                   MOVE      DSDATE        ##SRDT
     C                   MOVE      SCAPPL        ##APPL
      *
      *  Call API to access "Current" Bank Account based on ##CALL value
      *
      /Free
       ##acct = *Blanks;
       ##hdcy = *Blanks;
       ##chkq = *Blanks;
       ##ftyp = *Blanks;
       ##rtcd = *Blanks;
       ##dt   = *Zeros ;
       ##cpyq = *Blanks;
       ##cktp = sccktp;
       ##eftc = scEftc;

       ##acct= GetBankAcc(#co#: #fnd: ##appl: ##srdt: ##call:
                ##hdcy: ##chkq: ##ftyp: ##dt: ##cpyq:
                ##cktp: ##eftc: ##coid: ##rtcd);
      /End-Free
      *
     C     ##RTCD        IFEQ      *BLANK
      *  Bank Info found for Voucher/Check, with effective date prior to today.
      *
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '111111'      *IN(71)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *
      * ENSURE THAT THE EFF DATE IS NOT PAST THE BANK MASTER EXP DATE
      *
     C  N90XXEFDT        IFGE      CKEXDT
     C     CKEXDT        ANDGT     0
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '100000'      *IN(71)
     C                   ENDIF
     C     SCAPPL        IFEQ      'RP'
     C     CKRPCD        IFEQ      *BLANKS
     C     CKRPVS        OREQ      *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN89
     C                   ENDIF
     C                   ELSE
     C                   MOVE      '0'           *IN89
     C                   ENDIF

      /free
       // Ensure eff date isn't prior to Bank Master's (if populated)
       //  Move '110011' to *in(71)
       If not *in90 and xxEfDt < CKefdt and CKefdt <> 0;
          *in71 = *on;
          *in72 = *on;
          *in73 = *off;
          *in74 = *off;
          *in75 = *on;
          *in76 = *on;
          *in90 = *on;
       Endif;
      /end-free

     C                   ENDIF
      /free
       IF *IN90 = *Off And @@mode = 'C' And
          (SCeftc <> *blanks And SCeftc <> 'E' And SCeftc <> 'B');
             *In62 = *On;
             *In90 = *On;
       Endif;


       If *in90 = *off and @@mode = 'C' and scEftc = *blank
        and (SVeftc = 'E' or SVeftc = 'B');
          hasEft = GetEFTExist(BIco#:BIfnd:BIefdt:BIappl:BIacct:BIcktp);
          Select;
          When not(hasEFT);
          When SVeftc = 'E';
             *in65 = *on;
             *in90 = *on;
          When SVeftc = 'B';
             *in66 = *on;
             *in90 = *on;
          Endsl;
       Endif;

       IF *IN90 = *Off And @@MODE = 'C' And
          (Sceftc = 'B') And
          (CKbnk# = *blanks or CKtrns = *blanks Or
           CKonus = *blanks or CKrtup = *blanks Or
           CKrtlw = *blanks);
             *IN63 = *On;
             *IN90 = *On;
       EndIf;

       IF *IN90 = *Off And @@MODE = 'C' And
          (Sceftc = 'B' or Sceftc = 'E') And
          (CKidsa = *zero or CKiora = *zero);
             *IN64 = *On;
             *IN90 = *On;
       EndIf;

      /end-free
      *  If CHANGE mode or ADD mode
      *
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
      *
      *  Trust Y/N
      *
     C     *IN90         IFEQ      *OFF
     C     SCTRST        ANDNE     'Y'
     C     SCTRST        ANDNE     'N'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000100'      *IN(71)
     C                   ENDIF
      *
      *  Hardcopy Y/N
      *
     C     *IN90         IFEQ      *OFF
     C     SCHDCY        ANDNE     'Y'
     C     SCHDCY        ANDNE     'N'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000101'      *IN(71)
     C                   ENDIF
      *
      *  Formtype cannot be BLANK, if hardcpy = 'Y'
      *
     C     *IN90         IFEQ      *OFF
     C     SCFTYP        ANDEQ     *BLANKS
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '000111'      *IN(71)
     C                   ENDIF
      *
      *  Logo Y/N
      *
     C     *IN90         IFEQ      *OFF
     C     SCLOGO        ANDNE     'Y'
     C     SCLOGO        ANDNE     'N'
      *
     C     SCHDCY        IFEQ      'Y'
     C     SCHDCY        OREQ      'N'
     C     SCLOGO        ANDNE     *BLANK
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001000'      *IN(71)
     C                   ENDIF
     C                   ENDIF
      *
      *  Macro ID cannot be ZERO
      *
     C     *IN90         IFEQ      *OFF
     C     SCMACR        ANDEQ     *ZERO
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001001'      *IN(71)
     C                   ENDIF
      *
      *  Check OUTQ cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCOUTQ        ANDEQ     *BLANKS
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001010'      *IN(71)
     C                   ENDIF
      *
      *  File Copy OUTQ cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCCPYQ        ANDEQ     *BLANKS
     C     SCHDCY        ANDEQ     'Y'
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001011'      *IN(71)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEA     '000'         *IN(41)
     C     SCLOGO        IFEQ      'N'
     C                   MOVE      *ON           *IN41
     C                   ENDIF
     C     SCLOGO        IFEQ      'Y'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C     SCLOGO        IFEQ      *BLANK
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C                   MOVEL     SCLOGO        SVLOGO
     C                   ENDIF
       //*** Beg Add *** 07/20/20 ******************************************
                         scrn# = 1;
       //*** End Add *** 07/20/20 ******************************************
      *
     C                   ENDSR
      *
      *  ERRCK2 - NO EDITS ON SCR02                                  *
     C     ERRCK2        BEGSR
      *
       //*** Beg Add *** 07/20/20 ******************************************
                         scrn# = 2;
       //*** End Add *** 07/20/20 ******************************************
     C                   ENDSR
      *
      *  ERRCK3 - Edit W2420F02 SCR03 fields                         *
      ****************************************************************
     C     ERRCK3        BEGSR
      *
      /free
        If (SCeftc = 'E') or (SVeftc = 'E' And SCEFTC = 'B');
         LeaveSr;
        Else;
      /end-free
      *  Routing for MICR cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCRTE1        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001100'      *IN(71)
     C                   ENDIF
      *
      *  Transit Number cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCTRNS        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001101'      *IN(71)
     C                   ENDIF
      *
      *  Check Digit cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCCKDG        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001110'      *IN(71)
     C                   ENDIF
      *
      *  ON-US cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCONUS        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '001111'      *IN(71)
     C                   ENDIF
      *
      *  Routing Entry Upper cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCRTE2        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '010000'      *IN(71)
     C                   ENDIF
      *
      *  Routing Entry Lower cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     *IN80         ANDEQ     *OFF
     C     SCRTE3        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVEA     '010001'      *IN(71)
     C                   ENDIF
      *
      *  Signature 1 must exist in Check Signature file
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAM1        ANDNE     *BLANKS
     C                   MOVEL     SCNAM1        KKNAME
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90
     C                   MOVEA     '010101'      *IN(71)
     C                   ENDIF
     C                   ENDIF
      *
      *  Signature 2 must exist in Check Signature file
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAM2        ANDNE     *BLANKS
     C                   MOVEL     SCNAM2        KKNAME
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        90
     C                   MOVEA     '010110'      *IN(71)
     C                   ENDIF
     C                   ENDIF
      *
      /free
        EndIf;
      /end-free
       //*** Beg Add *** 07/20/20 ******************************************
                         scrn# = 3;
       //*** End Add *** 07/20/20 ******************************************
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK4 - Edit W2420F02 SCR04 fields                         *
      ****************************************************************
     C     ERRCK4        BEGSR
       //*** Beg Add *** 07/20/20 ******************************************
                         scrn# = 4;
       //*** End Add *** 07/20/20 ******************************************
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK5 - Edit W2420F02 SCR05 fields                         *
      ****************************************************************
     C     ERRCK5        BEGSR
       //*** Beg Add *** 07/20/20 ******************************************
                         scrn# = 5;
       //*** End Add *** 07/20/20 ******************************************
     C                   ENDSR
      *
      ****************************************************************
      *  GETNXK                                                      *
      ****************************************************************
     C     GETNXK        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  KJDO - Process F10 "Table Search" requests.                 *
      ****************************************************************
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFND'
     C     @@MODE        ANDEQ     'A'
     C     *IN55         ANDEQ     *OFF
      *
     C                   CALL      'W0010R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    QQCO#             3
     C                   PARM      *ZERO         QQFND             3
     C                   PARM                    ##FYR             3
     C                   PARM      1             ##VIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     QQFND         OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *INKL
     C                   MOVEA     '000011'      *IN(81)
     C*
     C                   ELSE
     C*
     C*                    *** Position cursor to Bank Name
     C                   MOVEA     '000010'      *IN(81)
     C                   MOVE      QQFND         SCFND
     C*
     C                   ENDIF
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCACCT'
     C     @@MODE        ANDEQ     'A'
      *
     C                   CALL      'W2422R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM                    QQCO#
     C                   PARM      *BLANKS       ##ACCT
      *
      *                    *** If account # returned
     C     ##ACCT        IFNE      *BLANKS
      *
      *                    *** Position cursor to effective date
     C                   MOVEA     '000001'      *IN(81)
     C                   MOVEL     ##ACCT        SCACCT
      *
     C                   MOVEL     ##ACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CKAI01        SCBKNM
     C                   MOVEL(P)  CKAI02        SCBKAD
     C                   MOVEL(P)  CKAI03        SCBKCS
     C                   MOVEL(P)  CKVCHR        SCVCHR
     C                   ENDIF
      *
     C                   ELSE
      *                    *** Position cursor to Bank Acct
     C                   MOVEA     '001000'      *IN(81)
     C                   ENDIF
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCNAM1'
      *
     C                   CALL      'W2424R'
     C                   PARM      '2'           ##MODE            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      XXCO#         ##CO#
     C                   PARM      *BLANKS       ##NAM1
     C                   PARM      *BLANKS       ##10A1
     C                   PARM      *BLANKS       ##CMD1
      *
     C     ##NAM1        IFNE      *BLANKS
     C                   MOVEL     ##NAM1        SCNAM1
     C                   MOVEL     ##10A1        SC10A1
     C                   MOVEL     ##CMD1        SCCMD1
     C                   ENDIF
      *
      *                    *** If Signature 1 name returned
     C     SCNAM1        IFNE      *BLANKS
      *
      *                    *** Position cursor to Signature 2 name
     C                   MOVEA     '000111'      *IN(81)
      *
     C                   ELSE
      *                    *** Position cursor to Signature 1 name
     C                   MOVEA     '000101'      *IN(81)
     C                   ENDIF
      *
      ******************************************************************
      *
     C     HFLD          WHENEQ    'SCNAM2'
      *
     C                   CALL      'W2424R'
     C                   PARM      '2'           ##MODE            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    XXCO#
     C                   PARM      *BLANKS       ##NAM2
     C                   PARM      *BLANKS       ##10A2
     C                   PARM      *BLANKS       ##CMD2
      *
     C     ##NAM2        IFNE      *BLANKS
     C                   MOVEL     ##NAM2        SCNAM2
     C                   MOVEL     ##10A2        SC10A2
     C                   MOVEL     ##CMD2        SCCMD2
     C                   ENDIF
      *
      *                    *** If Signature 2 name returned
     C     SCNAM2        IFNE      *BLANKS
      *
      *                    *** Position cursor to File Copy OUTQ
     C                   ELSE
      *
      *                    *** Position cursor to Signature 2 name
     C                   MOVEA     '000111'      *IN(81)
     C                   ENDIF
      *
      *
      ******************************************************************
     C     HFLD          WHENEQ    'SCAPPL'
     C                   MOVEL(P)  'BTYP'        QQTBID
     C                   CALL      'W2493R'
     C                   PARM      *BLANK        QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQTBID            4
     C                   PARM      *BLANK        QQKEY            10
     C                   PARM      *BLANK        QQRTCD            1
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCAPPL
     C                   MOVEL(P)  'BTYP'        KKTBID            4
     C                   MOVEL(P)  QQKEY         KKKEY            10
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCBTYD
     C                   MOVE(P)   *ON           *IN32
     C                   END
     C                   ELSE
     C                   MOVE(P)   *ON           *IN31
     C                   END
      *
     C                   OTHER
     C                   MOVE      *ON           *IN49
      *
     C                   ENDSL
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR
      *
      ****************************************************************
      *  KUDO - Process F20 ACTIVE/INACTIVE                          *
      ****************************************************************
     C     KUDO          BEGSR
      *
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR
      *
      ****************************************************************
      *  MVDBSC - Assign Bank Information fields to Screen fields   *
      ****************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVE      XXCO#         QQCO#
     C                   MOVEL     BIACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CKAI01        SCBKNM
     C                   MOVEL     CKAI02        SCBKAD
     C                   MOVEL     CKAI03        SCBKCS
     C                   MOVE      CKVCHR        SCVCHR
     C     CKVCHR        IFEQ      'N'
     C                   Z-ADD     3             #OFSCR
     C*
      /Free
       If cltid = '025' or WCfl17 = 'Y' Or
        WCfl24 = 'Y' or WCfl35 = 'Y';
          #ofscr = 4;
       Endif;
      /End-Free
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL(P)  BICKTP        SCCKTP
     C                   MOVE      BIFND         SCFND
     C                   MOVEL     BIONUS        SCONUS
     C                   MOVEL     BICKDG        SCCKDG
     C                   MOVEL     BIBNK#        SCTRNS
     C                   MOVEL     BITRNS        SCRTE1
     C                   MOVEL     BIAXOU        SCAUON
     C                   MOVEL     BISTAT        SCSTAT
     C                   MOVEL     BIRTUP        SCRTE2
     C                   MOVEL     BIRTLW        SCRTE3
     C                   MOVEL     BITRST        SCTRST
     C                   MOVEL     BI$LMT        SC$LMT
     C                   MOVEL     BI10A1        SC10A1
     C                   MOVEL     BI10A2        SC10A2
     C                   MOVEL     BIAPPL        SCAPPL
     C                   MOVEL     BIFTYP        SCFTYP
     C                   MOVEL     BINAM1        SCNAM1
     C                   MOVEL     BICMD1        SCCMD1
     C                   MOVEL     BINAM2        SCNAM2
     C                   MOVEL     BICMD2        SCCMD2
     C                   MOVEL     BIMACR        SCMACR
     C                   Z-ADD     BI50N3        SCVOID
     C                   MOVEL     BIHDCY        SCHDCY
     C                   MOVEL     BIOUTQ        SCOUTQ
     C                   MOVEL     BICPYQ        SCCPYQ
     C                   MOVEL     BIACCT        SCACCT
     C                   MOVEL     BICKPV        SCCKPV
     C                   Z-ADD     BIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     c                   MoveL     BI1A2         SC1A2
      *
      /Free
       If CltId= '126';
          SCTxtOnCk = BItxtOnCk + BItxtOnck2;
       Endif;
      /End-Free
     C     FIRST         IFEQ      *BLANK
     C                   MOVEL     BILOGO        SCLOGO
     C                   MOVEL     BILOGO        SVLOGO
     C                   MOVE      'Y'           FIRST             1
     C                   ENDIF
      *
      *  THIS CONTROLS THE MESSAGE DISPLAYED ON SCR02.
      *
     C     SCLOGO        IFNE      SVLOGO
     C     FIRST         OREQ      'Y'
     C                   MOVEA     '000'         *IN(41)
     C     SCLOGO        IFEQ      'N'
     C                   MOVE      *ON           *IN41
     C                   ENDIF
     C     SCLOGO        IFEQ      'Y'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C     SCLOGO        IFEQ      *BLANK
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C                   MOVEL     SCLOGO        SVLOGO
     C                   MOVE      'N'           FIRST
     C                   ENDIF
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF
      *
      *  "Add" Group occurring from Work With Groups
      *  Avoid pgm recursion thru F10 by assign/protec Group field.
      *
     C     *IN55         IFEQ      *ON
     C                   Z-ADD     @@FND         SCFND
     C                   ENDIF
      *
      *  Voucher Flag
      *
     C                   MOVEL     SCACCT        XXACCT
     C     CKKEY         CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C     CKVCHR        ANDEQ     'Y'
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   ENDIF
      *
      *
      *    UPDATE MODE
     C                   MOVEL(P)  BI40A1        SC40A1
     C                   MOVEL(P)  BI40A2        SC40A2
     C                   MOVEL(P)  BI40A3        SC40A3
     C                   MOVEL(P)  BI40A4        SC40A4
      *
      *  LOAD FOUR FREE FORM FIELDS WITH GROUP INFO FIRST TIME
      *    ADD MODE
     C     *IN40         IFEQ      *ON
      *
     C                   Z-ADD     @@FND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SC40A1
     C                   MOVEL(P)  FNMAD1        SC40A2
      *
     C     FNMAD2        IFNE      *BLANK
     C                   MOVEL     FNMAD2        SC40A3
      *
     C     FNMCTY        CAT(P)    ',':0         SC40A4
     C                   CAT       FNMST:1       SC40A4
     C                   MOVEL(P)  FNMZIP        XA5               5
     C                   MOVE(P)   FNMZIP        XA4               4
     C                   CAT       XA5:2         SC40A4
     C                   CAT       '-':0         SC40A4
     C                   CAT       XA4:0         SC40A4
     C                   ENDIF
      *
     C     FNMAD2        IFEQ      *BLANK
     C     FNMCTY        CAT(P)    ',':0         SC40A3
     C                   CAT       FNMST:1       SC40A3
     C                   MOVEL(P)  FNMZIP        XA5               5
     C                   MOVE(P)   FNMZIP        XA4               4
     C                   CAT       XA5:2         SC40A3
     C                   CAT       '-':0         SC40A3
     C                   CAT       XA4:0         SC40A3
      *
     C                   MOVE      *BLANK        SC40A4
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      /Free
       If *In46 = *On;
          scccid = BIccid;
          scdcid = BIdcid;
       Endif;
      /End-Free
      /free
       SCeftc = BIeftc;
       SVeftc = BIeftc;
      /end-free
       //*** Beg Add *** 07/20/20 *************************************
       If @@mode = 'D' and *Inkl = *on;
          scrn# = scrn# - 1;
       Endif;
       //*** End Add *** 07/20/20 *************************************
      //*** Beg Add *** 07/20/20 **************************************
      /Free
       // If E-Pay is installed and setup at bank account master determine
       // if the vendor is setup for the Application code for this account
       isEpay = isModEnabled('E-Pay');
       Chain(n) ckkey pck01;
       If CKePayVn = *blanks;
          isEpay = *off;
       Endif;
       If isEpay;
          isEpay = *off;
          Setll (*loval:*loval:*loval) pevm01;
          Read pevm01;
          Dow Not %Eof(WMEVML1);
             If EVMappl = BIappl and EVMVend = CKePayVn;
                isEPay = *on;
                Leave;
             Endif;
             Read pevm01;
          Enddo;
       Endif;
      //*** End Add *** 07/20/20 **************************************
     C                   ENDSR
      *
      ****************************************************************
      *  MVSCDB - Assign Screen fields to Bank Information Fields    *
      ****************************************************************
     C     MVSCDB        BEGSR
      *
     C                   MOVE      XXCO#         BICO#
     C                   MOVE      SCFND         BIFND
     C                   MOVEL(P)  SCCKTP        BICKTP
     C                   MOVEL     SCBKNM        BIAI01
     C                   MOVEL     SCBKAD        BIAI02
     C                   MOVEL     SCBKCS        BIAI03
     C                   MOVEL     SCONUS        BIONUS
     C                   MOVEL     SCCKDG        BICKDG
     C                   MOVEL     SCTRNS        BIBNK#
     C                   MOVEL     SCRTE1        BITRNS
     C                   MOVEL     SCAUON        BIAXOU
     C                   MOVEL     SCSTAT        BISTAT
     C                   MOVEL     SCRTE2        BIRTUP
     C                   MOVEL     SCRTE3        BIRTLW
     C                   MOVEL     SCTRST        BITRST
     C                   MOVEL     SCLOGO        BILOGO
     C                   MOVEL     SC$LMT        BI$LMT
     C                   MOVEL     SCAPPL        BIAPPL
     C                   MOVEL     SCFTYP        BIFTYP
      *
     C     SCNAM1        IFEQ      *BLANK
     C                   MOVE      *BLANK        BI10A1
     C                   MOVE      *BLANK        BINAM1
     C                   MOVE      *BLANK        BICMD1
      *
     C                   ELSE
      *
     C                   MOVEL(P)  SCNAM1        KKNAME
     C                   MOVEL(P)  SCNAM1        BINAM1
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SGFONT        BI10A1
     C                   MOVEL(P)  SGCMD         BICMD1
     C                   ENDIF
     C                   ENDIF
      *
     C     SCNAM2        IFEQ      *BLANK
     C                   MOVE      *BLANK        BI10A2
     C                   MOVE      *BLANK        BINAM2
     C                   MOVE      *BLANK        BICMD2
      *
     C                   ELSE
      *
     C                   MOVEL(P)  SCNAM2        KKNAME
     C                   MOVEL(P)  SCNAM2        BINAM2
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SGFONT        BI10A2
     C                   MOVEL(P)  SGCMD         BICMD2
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     SCMACR        BIMACR
     C                   Z-ADD     SCVOID        BI50N3
     C                   MOVEL     SCHDCY        BIHDCY
     C                   MOVEL     SCOUTQ        BIOUTQ
     C                   MOVEL     SCCPYQ        BICPYQ
     C                   MOVEL     SCACCT        BIACCT
     C                   MOVEL     SCCKPV        BICKPV
     C                   MOVEL     Q1USER        BIUSER
     C                   MOVEL     Q1PGM         BIPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   Z-ADD     DSTIME        BITIME
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIEFDT
     c                   MoveL     SC1A2         BI1A2
      *
     C                   MOVEL(P)  SC40A1        BI40A1
     C                   MOVEL(P)  SC40A2        BI40A2
     C                   MOVEL(P)  SC40A3        BI40A3
     C                   MOVEL(P)  SC40A4        BI40A4
      *
      /Free
       If *In46 = *On;
          BIccid = scccid;
          BIdcid = scdcid;
       Endif;

       BIeftc = SCeftc;

       If CltId= '126';
          BItxtOnCk  = %subst(SCTxtOnCk:1:20);
          BItxtOnCk2 = %subst(SCTxtOnCk:21:2);
       Endif;
      /End-Free
     C                   ENDSR
      *
      ****************************************************************
      *  OTHERR - Used for processing other Modes                    *
      ****************************************************************
     C     OTHERR        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  OTHFKS - Used for processing other Function Keys            *
      ****************************************************************
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKU         CASEQ     *ON           KUDO                     90
     C                   ENDCS
      *
     C                   ENDSR
      *
      ****************************************************************
      *  PGMEND                                                      *
      ****************************************************************
     C     PGMEND        BEGSR
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  PGMSTR                                                      *
      ****************************************************************
     C     PGMSTR        BEGSR
      *
      /Free
       CallP RtvCltId(CltId);

       Chain Upkey pup01;
       If Not %Found(WMUPL);
          upco# = *zeros;
       endif;
       xxco# = UPco#;

       clt129 = *Off;
       If cltID = '129';
          clt129 = *On;
       EndIf;

       If cltID = '126';
          VisTxtOnCk = *On;
       Else;
          VisTxtOnCk = *Off;
       EndIf;

      /End-Free
      *
     C                   Z-ADD     2             #OFSCR            1 0
      *
     C                   MOVE      *OFF          *IN55
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C     @@FND         IFGT      *ZERO
     C                   MOVE      *ON           *IN55
     C                   Z-ADD     @@FND         SCFND
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF
     C*
     C     'WC'          CHAIN     SWC01                              60
     C     WCFL17        COMP      'Y'                                    24
     C     WCFL25        COMP      'Y'                                    35
     C     WCFL36        COMP      'Y'                                    36
      /Free
       If  WCfl17 = 'Y' or WCfl24 = 'Y' or
        WCfl35 = 'Y';
          #ofscr = 4;
          *In46  = *On;
       Endif;

       If  CltId = '025';
          *In45 = *On;
       Endif;
      /End-Free
      //*** Beg Add *** 07/20/20 **************************************
       scrn# = 0;
      //*** End Add *** 07/20/20 **************************************
     C*
     C                   ENDSR
      ****************************************************************
      *  PREOTH                                                      *
      ****************************************************************
     C     PREOTH        BEGSR
      *
     C     @@MODE        IFEQ      'S'
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C     BISTAT        IFEQ      'I'
     C                   MOVE      'A'           BISTAT
     C                   ELSE
     C                   MOVE      'I'           BISTAT
     C                   ENDIF
      *
     C                   MOVE      Q1USER        BIUSER
     C                   MOVE      Q1PGM         BIPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   Z-ADD     DSTIME        BITIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      ****************************************************************
      *  UPDATR - Update Payee Demographics file                     *
      ****************************************************************
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        BIUSER
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   MOVEL     DSTIME        BITIME
      *
     C                   UPDATE    DBRCDF
     C*** Beg Add *** 07/20/20 ***************************************
       //Free
       If isEPay;
          Exsr callR;
       Endif;
       //End-Free
     C*** End Add *** 07/20/20 ***************************************
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      ****************************************************************
      *  WRITER - Write Payee Demographic file                       *
      ****************************************************************
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      GETNXK
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        BIUSER
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BIDT
      *
     C                   MOVEL     DSTIME        BITIME
     C                   MOVE      'A'           BISTAT
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      WMBIP
     C                   MOVE      *ON           OUTFLG
     C*** Beg Add *** 07/20/20 ***************************************
       //Free
                         Exsr callR;
       //End-Free
     C*** End Add *** 07/20/20 ***************************************
      *
     C                   ENDSR
