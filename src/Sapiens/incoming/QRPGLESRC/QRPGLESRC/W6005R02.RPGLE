     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W6005R02 - Daily Check Schedule Maintenance
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W6005R - WW Daily Check Schedules                  *
     H*                                                               *
     H* CALLS TO:  none                                               *
     H*                                                               *
     H* CREATED:  04/31/01     C:61          PROJECT: WC1020          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/30/15 C:136 Project: WC2337                                *
     H*               -Write separate daily check schedule records    *
     H*                if MICR and EFT bank account are different     *
     H*                                                               *
     H* 01/15/13  C:191 Redmine: 2108                                 *
     H*                 Last check # not loading correctly            *
     H*                                                               *
     H* 04/17/12  C:255 Project WC2044                                *
     H*                 Added code for the new field added on the sc- *
     H*                 reen. Replaced WA023R to GetBankAcc proc call.*
     H*                                                               *
     H* 11/16/11  C:279 Support 78503                                 *
     H*                 If daily processing data area is already in   *
     H*                 use, display notification window.             *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 05/01/09 C:220 PROJECT: WC1766                                *
     H*                Create Holiday Blackout Date entry screen      *
     H*                for mailing Claims checks                      *
     H*                                                               *
     H* 01/22/09 C:33  Project: WC1647  WA114/66866                   *
     H*                Remove message for CLT114 as it interupts CSIs *
     H*                 workflow.                                     *
     H*                                                               *
     H* 01/20/09 C:33  Project: WC1647  WA114/66823                   *
     H*               -The check for "Y" should occur after all errors*
     H*                 have been checked.                            *
     H*               -If F12 is pressed from the window msg, the flag*
     H*                 should not change.                            *
     H*                                                               *
     H* 05/01/08 C:190 Project: WC1647                                *
     H*                Check for a "Y" in position "7" of the data    *
     H*                area and if NOT "Y" display a window stating   *
     H*                warning message.                               *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                Add check mailing date field on SCR01          *
     H*                                                               *
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 06/19/02 C:15  Support: WC082/61295                           *
     H*                Chgd pgm to prevent duplicate records.         *
     H*                                                               *
     H* 11/19/01 C:15  Project: WC1020                                *
     H*                Chgd key list for logical WACPL4.              *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09       DISPLAY MODE                                        *
     H*  40       ADD MODE                                            *
     H*  45       GROUP CHECKS                                        *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  90       Error                                               *
     H*                                                               *
     H* SCR01                                                         *
     H*  31       Payment type field visibility.                      *
     H*  71       Invalid Group                                       *
     H*  72       System date >= group expiration date                *
     H*  73       Invalid Group Aux record                            *
     H*  74       Group Aux process claims flag                       *
     H*  75       Invalid process date                                *
     H*  76       Bank Code can't be blank                            *
     H*  77       Bank Account # can't be blank                       *
     H*  78       Waste checks Y / N / BLANK                          *
     H*  79       Check print  Y / N / BLANK                          *
     H*  80       Starting check # must be entered                    *
     H*  81       FROM Date valid                                     *
     H*  82       TO Date valid                                       *
     H*  83       FROM Date <= TO Date                                *
     H*  84       Duplicate record not allowed                        *
     H*            - record with same key info already exists in file *
     H*  85       Position cursor SCFND                               *
     H*  86       Position cursor SCPRDT                              *
     H*  87       Position cursor SCCKTP                              *
     H*  88       Position cursor SCCHK#                              *
     H*                                                               *
     H*****************************************************************
     FW6005F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
      *
     FWACPP     UF A E             DISK
     F                                     RENAME(PCP01:DBRCDF)
      *
      *  Group master
     FWMFNL     IF   E           K DISK
      *
      *  Group Auxiliary      key=CO#/GRP
     FWDF2L     IF   E           K DISK
      *
      *  Process Check
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
     FWACPL4    IF   E           K DISK
      *
      *  Bank Account Master  key=CO#/ACCT#
     FWSCKL     IF   E           K DISK
      *
     FWMCXL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     D*     TABLES
     D OPTN            S             40    DIM(2) CTDATA PERRCD(1)
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      *
     D                 DS
     D  DLYA                   1     80
     D  DLYA1                  7      7
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d #2eftc          s              1
     d #2rtcd          s              1
     d #2fyr           s              3
     d #2case          s              7
     d #2data          s             75
     d #2coid          s             20
     d #2chkq          s             10
     d #2ftyp          s             10
     d #2hdcy          s              1
     d #2cpyq          s             10
     d #2efdt          s              7
     d strKey          s              2
     d EFTflg          s              1  0 Inz(0)
     d SVacce          s                   like(SCacce)
      *******End Add********04/17/12**********************************
     C/COPY SCOPYMT01C
     C*
     C     *DTAARA       DEFINE    WDAILYA       DLYA
     C*
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WACPP
     C                   PARM                    @@GRCK            1            G=GROUP CHECK
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Group Master
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
      *
      *  Group Master
      *
     C     CPKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    X1PRDT
     C                   KFLD                    SCACCT
     C                   KFLD                    SCFND
     C                   KFLD                    SCCKTP
      *
      *  Bank Master
      *
     C     CKKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCACCT
      *******Beg Add********04/17/12**********************************
     C     CKKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCacce
      *******End Add********04/17/12**********************************
     C     CXKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C     WAKY          KLIST
     C                   KFLD                    CPCO#
     C                   KFLD                    CPPRDT
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    FNEXDT        SYSDAT
     C     *LIKE         DEFINE    SCPRDS        OPTNM
     C     *LIKE         DEFINE    WCKEY         XSKEY
      *
      *================================================================
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   Z-ADD     0             XXCNT             1 0
     C     WAKY          CHAIN     PCP01                              60
     C     *IN60         DOWEQ     *OFF
     C     CPSTA0        IFEQ      ' '
     C     CPSTA1        ANDEQ     ' '
     C                   ADD       1             XXCNT
     C                   ENDIF
     C     WAKY          READE     PCP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ '0'
     C*
     C     XXCNT         IFEQ      1
     C     *LOCK         IN        DLYA
     C                   MOVE      ' '           DLYA1
     C                   OUT       DLYA
     C                   ENDIF                                                  XXCNT='1'
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C     @@MODE        IFEQ      'A'
      *
      *  Group must be valid
      *
     C     *IN90         IFEQ      *OFF
     C     FNKY          CHAIN     PFN01                              60
     C   60              SETON                                        90  71
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  Group expiration date > system date
      *
     C     *IN90         IFEQ      *OFF
     C     FNEXDT        ANDGT     *ZERO                                        Group Expiration DT
     C     FNEXDT        ANDLT     SYSDAT
     C                   SETON                                        90  72
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  Verify Group Auxiliary record
      *
     C     *IN90         IFEQ      *OFF
     C     FNKY          CHAIN     PF201                              60
     C   60              SETON                                        90  73
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  Group Auxiliary Process Claims Flag check
      *
     C  N90F2CLCK        IFNE      'Y'                                          Process Claims
     C                   SETON                                        90  74
     C                   ENDIF                                                  F2CLCK NE Y
      *
      *  Process date must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90  75
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *******Beg Add********04/17/12**********************************
      /Free
       // Validate Payment type
       If SCeftc <> ' ' And SCeftc <> 'B' And
        SCeftc <> 'E';
          *In30 = *On;
          *In33 = *On;
          *In90 = *On;
       EndIf;
      /End-Free
      *******End Add********04/17/12**********************************
     C                   SETOFF                                       6569
     C     *IN90         IFEQ      *OFF
     C     *IN66         ANDEQ     *ON
      * If EDI install flag is Y and EDI mailing date is entered
      * Validate EDI mailing date
     C     SCCMDT        IFNE      0
     C                   MOVE      ' '           SRERR
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCMDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90  65
     C                   ENDIF                                                  SRERR
      * EDI mailing date should not be less than processing date
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXCMDT            7 0
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXPRDT            7 0
      *
     C     XXCMDT        IFLT      XXPRDT
     C                   SETON                                        90  69
     C                   ENDIF                                                  SCCMDT < SCPRDT
      *
     C                   ELSE
     C* If EDI mailing date is zero and it is add mode
     C* default to working date(CXEDIM+Processing date) after processing date
     C     SCPRDT        IFNE      0
     C* Get EDI mailing date increment #
     C                   Z-ADD     *ZEROS        XXEDIM            1 0
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CXEDIM        XXEDIM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   Z-ADD     XXEDIM        SRDAYS            5 0
     C                   Z-ADD     SCPRDT        SRDATE            8 0
     C* Calculate EDI mailing date
     C*
     C* Call API to find out the next working day date
     C*
     C                   MOVE      ##CO#         @@CO#
     C                   MOVE      SRDATE        @@DTI
     C                   MOVE      SRDAYS        @@SRD
     C                   MOVE      '2'           @@MOD
     C                   MOVE      *BLANKS       @@DTO
     C                   MOVE      *BLANK        @@RTCD
     C*
     C                   CALL      'WA072R'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DTI             8
     C                   PARM                    @@SRD             5
     C                   PARM                    @@MOD             1
     C                   PARM                    @@DTO             8
     C                   PARM                    @@RTCD            1
     C*
     C                   MOVEL     @@DTO         SCCMDT
      *** beg del ***** 07/30/15 *******************************************
     C***********        SETON                                        90
      *** end del ***** 07/30/15 *******************************************
     C*
     C*
     C                   ENDIF                                                  SCPRDT NE 0
     C                   ENDIF                                                  SCCMDT NE 0
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  No Duplicate Record allowed
     C**N60*****           SETON                     90  84
      *
      *  Validate the Bank account code
      *
     C                   MOVE      ##CO#         #2CO#
     C                   MOVE      SCFND         #2FND
      *******Beg Chg********04/17/12**********************************
     C*******            CALL      'WA023R1'
     C*******            PARM      '1'           #2MODE            1
     C*******            PARM                    #2CO#
     C*******            PARM                    #2FND
     C*******            PARM      *ZEROS        #2FYR             3
     C*******            PARM      *ZEROS        #2CASE            7
     C*******            PARM      'WC'          #2APPL            2
     C*******            PARM      *BLANKS       #2DATA           75
     C*******            PARM      SCCKTP        #2CKTP
     C*******            PARM      *BLANK        #2RTCD                         Return Code
      /Free
       #2fyr = *Zeros;
       #2case = *Zeros;
       #2appl = 'WC';
       #2data = *Blanks;
       #2cktp = SCcktp;
       #2eftc  = SCeftc;

       If Not ValidBankCode(#2co#: #2fnd: #2fyr: #2case: #2appl:
           #2data: #2cktp: #2eftc);
           *In76 = *On;
           *In90 = *On;
       EndIf;
      /End-Free
      *******End Chg********04/17/12**********************************
      *
      *  Retrieve the check/voucher flag from the Group Aux file.
      *
     C     FNKY          CHAIN     PF201                              60
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C   60              MOVE      *BLANK        #2CALL
     C  N60F2CTYP        IFNE      *BLANK
     C                   MOVE      F2CTYP        #2CALL
     C                   ELSE
     C                   MOVE      'C'           #2CALL
     C                   ENDIF
     C                   OTHER
     C   60              MOVE      *BLANK        #2CALL
     C  N60F2CTYP        IFEQ      'V'
     C                   MOVE      'V'           #2CALL
     C                   ELSE
     C                   MOVE      'C'           #2CALL
     C                   ENDIF
     C                   ENDSL
      *
      *  Call API to access "Current" Bank Account based on #2CKTP
      *
     C                   MOVE      SCPRDT        #2SRDT
      *******Beg Chg********04/17/12**********************************
     C******             CALL      'WA023R'
     C******             PARM                    #2CO#                          Company
     C******             PARM                    #2FND                          Group #
     C******             PARM      'WC'          #2APPL                         Applic Code
     C******             PARM                    #2SRDT                         MMDDCCYY
     C******             PARM      *BLANKS       #2ACCT                         Bank Acct #
     C******             PARM                    #2CALL                         (V)oucher/(C)heck
     C******             PARM      *BLANK        #2HDCY            1            Hardcopy Y/N
     C******             PARM      *BLANKS       #2CHKQ           10            Check/Voucher OUTQ
     C******             PARM      *BLANKS       #2FTYP           10            Form Type
     C******             PARM      *BLANK        #2RTCD            1            Return Code
     C******             PARM      *ZERO         #2EFDT            7            WMBIP Eff. DT CYYMMD
     C******             PARM      *BLANKS       #2CPYQ           10
     C******             PARM                    #2CKTP
     C*
     C******             MOVEL(P)  #2ACCT        SCACCT
      *** beg del ***** 07/30/15 *********************************
      ****/Free
      **** ExSr RetrieveBankAcc#;
      ****/End-Free
      *** end del ***** 07/30/15 *********************************
      *******End Chg********04/17/12**********************************
      *** beg add ***** 07/30/15 *********************************
      /free
       Select;
       When SCeftc = *blank;
          #2eftc = *blank;
          Exsr RetrieveBankAcc#;
          // No MICR account setup
          If #2acct = *blanks;
             *in34 = *on;
             *in90 = *on;
             scAcct = *blanks;
          Else;
             scAcct = #2acct;
             scAcce = *blanks;
             Chain ckKy pck01;
             scChk# = CKlchk;
          Endif;
       When SCeftc = 'E';
          #2eftc = 'E';
          Exsr RetrieveBankAcc#;
          // No EFT account setup
          If #2acct = *blanks;
             *in35 = *on;
             *in90 = *on;
             scAcce = *blanks;
          Else;
             scAcct = *blanks;
             scAcce = #2acct;
             scChk# = *zeros;
          Endif;
       When SCeftc = 'B';
          #2eftc = *blank;
          Exsr RetrieveBankAcc#;
          // No MICR account setup
          If #2acct = *blanks;
             *in34 = *on;
             *in90 = *on;
             scAcct = *blanks;
          Else;
             scAcct = #2acct;
             Chain ckKy pck01;
             scChk# = CKlchk;
          Endif;
          #2eftc = 'E';
          Exsr RetrieveBankAcc#;
          // No EFT account setup
          If #2acct = *blanks;
             *in35 = *on;
             *in90 = *on;
             scAcce = *blanks;
          Else;
             scAcce = #2acct;
          Endif;
       Endsl;
      /end-free
      *** end add ***** 07/30/15 *********************************
      *
      *  No Duplicate Record allowed
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1PRDT            7 0
      *
     C     CPKY          CHAIN     PCP01                              60
     C  N60              SETON                                        90  84
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  If the starting chk# is not entered, default it from the
      *    Bank Master file.
      *
      *******Beg Chg********04/17/12**********************************
     C******IN90         IFEQ      *OFF
     C*****SCCHK#        ANDEQ     *ZERO
      *****
     C*****CKKY          CHAIN     PCK01                              99
     C**N99***           MOVE      CKLCHK        SCCHK#
     C**N99***           SETON                                        8990
     C******             ENDIF
      *** beg del ***** 07/30/15 ***************************
      *****
      *****/Free
      ***** If *In90 = *Off And SCchk# = *Zeros And
      *****  EFTflg =0;
      *****    If SCacct =  ' ' ;
      *****       Chain ckky1 pck01;
      *****    Else;
      *****       Chain ckky  pck01;
      *****    EndIf;
      *****
      *****    If SCeftc = 'E';
      *****            SCchk# = *Zeros;
      *****       *in36 = *On;
      *****       *In90 = *On;
      *****       EFTflg = 1 ;
      *****    Else;
      *****    //*** BEG CHG ****** 01/15/13 ***************************
      *****    //***** If Not %Eof; *****************
      *****       If %Found(WSCKL);
      *****    //*** END CHG ****** 01/15/13 ***************************
      *****          SCchk# = CKlChk;
      *****          *In90 = *On;
      *****       EndIf;
      *****    EndIf;
      ***** EndIf;
      *****/End-Free
      *******End Chg********04/17/12**********************************
      *** end del ***** 07/30/15 ***************************
      *
      *
     C                   ENDIF                                                  @@MODE = A
      *
      *  The starting chk# cannot be 0.
      *
      *******Beg Chg********04/17/12**********************************
     C******IN90         IFEQ      *OFF
     C*****SCCHK#        ANDEQ     *ZERO
     C*****              SETON                                        80
     C*****              ENDIF
      /Free
       If *In90 = *Off And SCeftc <> 'E' And
        SCchk# = *Zeros;
          *In80 = *On ;
       EndIf;
      /End-Free
      *******End Chg********04/17/12**********************************
      * Validate EDI mailing date
     C     @@MODE        IFNE      'A'
     C     *IN90         ANDEQ     *OFF
     C     *IN66         ANDEQ     *ON
     C                   SETOFF                                       6569
     C     SCCMDT        IFNE      0
     C                   MOVE      ' '           SRERR
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCMDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90  65
     C                   ENDIF                                                  SRERR
      * EDI mailing date should not be less than processing date
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXCMDT
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXPRDT
      *
     C     XXCMDT        IFLT      XXPRDT
     C                   SETON                                        90  69
     C                   ENDIF                                                  SCCMDT < SCPRDT
     C                   ENDIF                                                  SCCMDT NE 0
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  Date range must be entered for GROUP checks
      *
     C     @@GRCK        IFEQ      'G'
      *
      *  'FROM' Date must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90  81
     C                   ENDIF
      *
     C                   ENDIF                                                  *in90 = *off
      *
      *  'TO' Date must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90  82
     C                   ENDIF
      *
     C                   ENDIF                                                  *in90 = *off
      *
      *  'FROM' Date <= 'TO' Date
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
      *
     C     XXFRDT        IFGT      XXTODT
     C                   SETON                                        90  83
     C                   ENDIF
      *
     C                   ENDIF                                                  *in90 = *off
      *
     C                   ENDIF                                                  @@GRCK = G
      *
      *  Waste checks ( Y / N / BLANK)
      *
     C     *IN90         IFEQ      *OFF
     C     SCWAST        ANDNE     *BLANK
     C     SCWAST        ANDNE     'Y'
     C     SCWAST        ANDNE     'N'
     C                   SETON                                        90  78
     C                   ENDIF                                                  SCWAST
      *
      *  Check print ( Y / N / BLANK)
      *
     C     *IN90         IFEQ      *OFF
     C     SCSTA0        ANDNE     *BLANK
     C     SCSTA0        ANDNE     'Y'
     C     SCSTA0        ANDNE     'N'
     C                   SETON                                        90  79
     C                   ENDIF                                                  SCWAST
     C*
     C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDEQ     'A'
     C*
     C                   IN        DLYA                                 91
     C                   CALL      'WA044R'
     C                   PARM                    ##RTCD
     C*
     C     ##RTCD        IFEQ      'F'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C     DLYA1         IFNE      'Y'
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C                   OTHER
     C                   EXFMT     WDW01
     C                   ENDSL
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN90
     C                   OTHER
     C                   MOVE      'Y'           DLYA1
     C                   ENDSL
     C                   MOVE      DLYA1         ##UPKL            1
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##MCKL            1
     C                   PARM                    ##UPKL
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *******Beg Add********04/17/12**********************************
      *** beg del ***** 07/30/15 ******************************
      ******/Free
      ****** If SCacct <> ' '  And SCacct <> SCacce;
      ******    If SCacce <> SVacce;
      ******       *In32 = *On;
      ******       *In90 = *On;
      ******       SVacce= SCacce;
      ******    Else;
      ******       *In32 = *Off;
      ******     //*In90 = *Off;
      ******    EndIf;
      ****** EndIf;
      ******/End-Free
      *** end del ***** 07/30/15 ******************************
      *******End Add********04/17/12**********************************
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *===============================================================
      * KJDO - Searches
      *===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * Group Search Given Company Number
      *
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVE      SCCO#         #1CO#
      *
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1FND             3
     C                   PARM      *BLANKS       #1FYR             3
     C                   PARM      1             #1VIEW            1 0
      *
     C     #1FND         IFEQ      *BLANKS
     C                   SETON                                        85
      *
     C                   ELSE
      *
     C                   MOVE      #1FND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
     C   60              MOVEL(P)  *BLANKS       SCFNAM
     C                   SETON                                        86
     C                   ENDIF
      *
      * Bank Information Search
      *
     C     HFLD          WHENEQ    'SCCKTP'
      *
     C                   MOVE      SCCO#         #2CO#
     C                   MOVE      SCFND         #2FND
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C                   MOVE      SCPRDT        #2SRDT
      *
     C                   CALL      'W2420R'
     C                   PARM      '2'           #2CALL            1
     C                   PARM      *BLANK        #2KC              1
     C                   PARM      *BLANK        #2KL              1
     C                   PARM                    #2CO#             3
     C                   PARM                    #2FND             3
     C                   PARM      *BLANKS       #2ACCT           20
     C                   PARM                    #2SRDT            8
     C                   PARM      'WC'          #2APPL            2
     C                   PARM      SCCKTP        #2CKTP           10
      *
     C     #2ACCT        IFEQ      *BLANKS
     C                   SETON                                        87
      *
     C                   ELSE
      *
     C                   MOVE      #2FND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
     C   60              MOVEL(P)  *BLANKS       SCFNAM
     C                   SETON                                        88
      *
      *  Retrieve the check/voucher flag from the Group Aux file.
      *
     C     FNKY          CHAIN     PF201                              60
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C   60              MOVE      *BLANK        #2CALL
     C  N60F2CTYP        IFNE      *BLANK
     C                   MOVE      F2CTYP        #2CALL
     C                   ELSE
     C                   MOVE      'C'           #2CALL
     C                   ENDIF
     C                   OTHER
     C   60              MOVE      *BLANK        #2CALL
     C  N60F2CTYP        IFEQ      'V'
     C                   MOVE      'V'           #2CALL
     C                   ELSE
     C                   MOVE      'C'           #2CALL
     C                   ENDIF
     C                   ENDSL
      *
      *  Call API to access "Current" Bank Account based on #2CKTP
     C                   MOVE      SCPRDT        #2SRDT
      *******Beg Chg********04/17/12**********************************
      ******
     C******             CALL      'WA023R'
     C******             PARM                    #2CO#                          Company
     C******             PARM                    #2FND                          Group #
     C******             PARM      'WC'          #2APPL                         Applic Code
     C******             PARM                    #2SRDT                         MMDDCCYY
     C******             PARM      *BLANKS       #2ACCT                         Bank Acct #
     C******             PARM                    #2CALL                         (V)oucher/(C)heck
     C******             PARM      *BLANK        #2HDCY            1            Hardcopy Y/N
     C******             PARM      *BLANKS       #2CHKQ           10            Check/Voucher OUTQ
     C******             PARM      *BLANKS       #2FTYP           10            Form Type
     C******             PARM      *BLANK        #2RTCD            1            Return Code
     C******             PARM      *ZERO         #2EFDT            7            WMBIP Eff. DT CYYMMD
     C******             PARM      *BLANKS       #2CPYQ           10
     C******             PARM                    #2CKTP
     C*
     C                   MOVEL(P)  #2CKTP        SCCKTP
     C******             MOVEL(P)  #2ACCT        SCACCT

      /Free
       ExSr RetrieveBankAcc#;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C*  Default the starting chk# from the Bank Master file.
     C*
     C     CKKY          CHAIN     PCK01                              99
     C  N99              MOVE      CKLCHK        SCCHK#
      *
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     ##CO#         SCCO#
      *
     C                   Z-ADD     CPFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
     C   60              MOVEL(P)  *BLANKS       SCFNAM
      *
     C                   Z-ADD     CPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRDT
      *
     C                   MOVEL(P)  CPCKTP        SCCKTP
     C                   MOVEL(P)  CPACCT        SCACCT
     C                   Z-ADD     CPCHK#        SCCHK#
     C                   Z-ADD     CPRSTR        SCRSTR
      *
     C                   Z-ADD     CPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
      *
     C                   Z-ADD     CPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
      *
     C                   MOVE      CPWAST        SCWAST
     C                   MOVE      CPSTA0        SCSTA0
     C                   MOVE      CPSTA1        SCSTA1
     C                   Z-ADD     CPCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCMDT
      *******Beg Add********04/17/12**********************************
      /Free
       SCeftc = CPeftc;
       //*** beg add ***** 07/30/15 *************************************
       scAcce = *blanks;
       svAcce = *blanks;
       //*** end add ***** 07/30/15 *************************************
      /End-Free
      *******End Add********04/17/12**********************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         CPCO#
     C                   Z-ADD     SCFND         CPFND
      *
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPPRDT
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPTODT
      *
     C                   MOVEL(P)  SCCKTP        CPCKTP
     C                   MOVEL(P)  SCACCT        CPACCT
     C                   Z-ADD     SCCHK#        CPCHK#
     C                   Z-ADD     SCRSTR        CPRSTR
     C                   MOVE      SCWAST        CPWAST
     C                   MOVE      SCSTA0        CPSTA0
     C                   MOVE      SCSTA1        CPSTA1
     C                   Z-ADD     SCCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPCMDT
      *******Beg Add********04/17/12**********************************
      /Free
       CPeftc = SCeftc;
      /End-Free
      *******End Add********04/17/12**********************************
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90     Search
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
      *  Set on LR so over-rides for group checks will work
      *
     C                   SETON                                        LR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     @@GRCK        IFEQ      'G'
     C                   MOVEL     OPTN(2)       OPTNM
     C                   ELSE
     C                   MOVEL     OPTN(1)       OPTNM
     C                   ENDIF
      *******Beg Add********04/17/12**********************************
      /Free
       // Payment field will be visible only for WCfl35.
       strKey = 'WC';
       Chain strKey Swc01;
       If WCfl35 = 'Y';
          *In31 = *On;
       EndIf;

      /End-Free
      *******End Add********04/17/12**********************************
     C     @@MODE        COMP      'A'                                    40     ADD MODE
     C     @@GRCK        COMP      'G'                                    45     GRP CHECKS
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT
     C                   MOVEL(P)  'WC'          XSKEY
     C                   SETOFF                                       66
     C     XSKEY         CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL09        ANDEQ     'Y'
     C                   SETON                                        66
     C                   ENDIF                                                  *IN60=*OFF
     C                   CALL      'WA044R'
     C                   PARM                    ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'F'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
      *
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        CPUSER
     C                   MOVEL     Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C                   MOVEL     DSTIME        CPTIME
      *
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        CPUSER
     C                   MOVEL     Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C                   MOVEL     DSTIME        CPTIME
      *
      *******Beg Chg********04/17/12**********************************
     C******             WRITE     DBRCDF
      /Free
       If CPacct <> ' ' and CPacct <> SCacce;
          CPeftc = ' ' ;
       EndIf;

       If CPacct <> '  ';
          Write DBRCDF;
       EndIf;

       If CPacct <>  SCacce And SCacce <> ' '  ;
          CPacct = SCacce;
       //Override check No. to 0 for E type record.
          CPeftc = 'E'   ;
          CPchk# = 0     ;
          Write DBRCDF;
       EndIf;
      /End-Free
      *******END Chg********04/17/12**********************************
     C                   FEOD      WACPP
     C                   ENDSR                                                  WRITER
      *** beg chg ***** 07/30/15 *************************************
      *******Beg Add********04/17/12**********************************
      *******
      ***/Free
      ***BegSr*RetrieveBankAcc#;
      *******
      ****#2appl = 'WC';
      ****//#2srdt = %Char(SCprdt);
      ****#2acct = *Blanks;
      ****#2hdcy = *Blanks;
      ****#2chkq = *Blanks;
      ****#2ftyp = *Blanks;
      ****#2rtcd = *Blanks;
      ****#2efdt = *Zeros ;
      ****#2cpyq = *Zeros ;
      ****#2coid = *Blanks;
      ****#2eftc = SCeftc ;
      *******
      ****#2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:
      *******      #2hdcy: #2chkq: #2ftyp: #2efdt: #2cpyq:
      *******      #2cktp: #2eftc: #2coid: #2rtcd);
      ****Select;
      *******
      ****When #2eftc = 'B';
      *******
      ****If*Account No. <> ' ' , MICR & EFT account No. will be same.
      *******If #2acct <> ' ';
      *******   SCacct = #2Acct;
      *******   SCacce = #2Acct;
      *******Else ;
      *******   #2eftc = ' ' ;
      *******   #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:
      *******            #2hdcy: #2chkq: #2ftyp: #2efdt: #2cpyq:
      *******            #2cktp: #2eftc: #2coid: #2rtcd);
      *******   If #2Acct <> ' ' ;
      *******      SCacct = #2acct;
      *******   Else;
      ****No*MICR account exist
      *******      *In34 = *On;
      *******      *In90 = *On;
      *******   EndIf;
      *******
      *******   #2eftc = 'E' ;
      *******   #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:
      *******            #2hdcy: #2chkq: #2ftyp: #2efdt: #2cpyq:
      *******            #2cktp: #2eftc: #2coid: #2rtcd);
      *******   If #2Acct <> ' ' ;
      *******      SCacce = #2acct;
      *******   Else;
      ****No*EFT Account exist
      *******      *In35 = *On;
      *******      *In90 = *On;
      *******   EndIf;
      *******
      ****If*#2Acct <> ' ' ends here.
      *******EndIf;
      *******
      ****When #2eftc = ' ';
      *******
      *******If #2Acct <> ' ' ;
      *******   SCacct = #2acct;
      *******Else;
      ****No*MICR Account exist
      *******      *In34 = *On;
      *******      *In90 = *On;
      *******EndIf;
      *******
      ****When #2eftc = 'E';
      *******
      *******If #2Acct <> ' ' ;
      *******   SCacce = #2acct;
      *******Else;
      ****No*EFT Account exist
      *******      *In35 = *On;
      *******      *In90 = *On;
      *******EndIf;
      ****EndSl;
      *******
      ***EndSr;
      ***/End-Free
      *******End Add********04/17/12**********************************
      *=====================================================
      * RetrieveBankAcc# - retrieves bank account number
      *=====================================================
      /free
       BegSr RetrieveBankAcc#;
       #2appl = 'WC';
       #2acct = *Blanks;
       #2hdcy = *Blanks;
       #2chkq = *Blanks;
       #2ftyp = *Blanks;
       #2rtcd = *Blanks;
       #2efdt = *Zeros ;
       #2cpyq = *Zeros ;
       #2coid = *Blanks;
       #2cktp = SCcktp ;
       #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
        #2chkq: #2ftyp: #2efdt: #2cpyq: #2cktp: #2eftc: #2coid: #2rtcd);
       EndSr;
      /end-free
      *** end chg ***** 07/30/15 *************************************
     C**************************************************************************
     C* SRNXDT - Calculate next working date after no. of days from current date
     C* Get processing date + EDI mailing date increment#
     C* Calculated day should not be weekend
     C* If date is saturday add 2, if sunday add 1
     C* Take base date as known saturday
     C* XXDAYS: 0=Sat, 1=Sun, 2=Mon, 3=Tue, 4=Wed, 5=Thu, 6=Fri
     C******               SELEC
     C* its Saturday, should return monday
     C******               Z-ADD2         SRDAYS
     C* its Sunday, should return monday
     C* other weekday, return next day
     C******
     C* If SRDAYS > 0 then add to get next working date
      *
      *
      *
** Pgm screen names
Daily Check Schedule Maintenance
Group Check Schedule Maintenance
