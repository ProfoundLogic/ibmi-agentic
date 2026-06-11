     H* (c) 2012 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W3021R - GENERATE REQUESTS FOR COMMISSION CHECKS
     H*
     H* NOTE: IF INSTALLING THIS PROGRAM FROM V10, INSTALL WA023R3 AND*
     H*       WMEFP/L*. IF INSTALLING AT EFT CLIENT, ENTIRE WC1806    *
     H*       MAY BE REQUIRED!                                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                1) Use GetBankAcc  instead of WA023R           *
     H*                2) Use GetEFTStatus instead of WA023R3         *
     H*                                                               *
     H* 10/01/10 C:136 SUPPORT: WA071 74371                           *
     H*                USE SYSTEM DATE TO CHECK EFT STATUS OF AGENT   *
     H*                INSTEAD OF QQDATE SO THAT CHECK REQUEST        *
     H*                EFT FLAG IS CORRECT WHEN THE PAYMENT IS        *
     H*                ACTUALLY PROCESSED                             *
     H*                                                               *
     H* 06/29/09 C:191  Project: WC1806                               *
     H*                 USE STANDARD API TO DETERMINE EFT AND PRENOTE *
     H*                 STATUS                                        *
     H*                                                               *
     H*  09/20/07 C:136 WC997                                         *
     H*                  ADD PROCESSING FOR EFT ACCOUNTS.             *
     H*                                                               *
     H*  12/02/97 C:30 Project:  WC500                                *
     H*           Re-compile for WAR2P new definition.                *
     H*                                                               *
     H*****************************************************************
     FWDF2L     IF   E           K DISK
     FWMACL5    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWAR2L1    UF A E           K DISK
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d ##co#           s              3
     d #co#            s              3
     d ##fnd           s              3
     d #fnd            s              3
     d ##Appl          s              2
     d ##call          s              1
     d ##hdcy          s              1
     d ##chkq          s             10
     d ##cpyq          s             10
     d ##ftyp          s             10
     d ##srdt          s              8
     d ##dt            s              7
     d ##key           s             30
     d ##acct          s             20
     d ##Data          s             75
     d ##Cktp          s             10
     d ##rtcd          s              1
     d ##Eftc          s              1
     d ##Pre           s              1                                         07
     d ##ptyp          s              1                                         02
     d ##type          s              1                                         02
     d #ekey           s             30                                         04
     d ##Date          s              7                                         05
     d ##coid          s             20                                         05
      *******End Add********04/17/12**********************************
     C*
     C     *LIKE         DEFINE    ACDT          XXDATE
     C     *LIKE         DEFINE    ACDT          D1DATE
     C     *LIKE         DEFINE    R2ACCT        REGACC
     C     *LIKE         DEFINE    R2ACCT        EFTACC
     C     *LIKE         DEFINE    R2AMT         SVAMT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCODE            2
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQDATE        X1DATE            8 0
     C                   MOVE      QQCODE        XXCODE            2
     C*
     C*  KEY LIST FOR REQUEST PAYMENT FILE
     C*
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND  AUXILIARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT COMMISSIONS FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ACKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DATE
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*   DELETE ALL EXISTING RECORDS IN REQUEST PMT FILE FOR CO# & FND
     C*
     C     R2KEY         CHAIN     PR201                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PR201
     C     R2KEY         READE     PR201                                  60
     C                   END
     C*
     C*   CHAIN TO THE FUND AUXILIARY FILE
     C*
     C     F2KEY         CHAIN     PF201                              60
     C*
     C*   CHAIN TO THE TABLE FILE FOR TYPE & DESCRIPTION
     C*
     C                   MOVE      XXCODE        R2COCD
     C                   MOVEL     XXCODE        XXKEY
     C     TBKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       CSTYPE
     C                   MOVE      *BLANKS       CSDESC
     C                   END
     C*
     C*   START READING THE AGENT COMMISSION FILE
     C*
     C     ACKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   CALCULATE COMMISSION OWED
     C*
     C                   Z-ADD     0             XXOWED
     C                   Z-ADD     ACAGT#        XXAGT#            5 0
     C                   MOVE      '0'           *IN50
     C     *IN50         DOWEQ     '0'
     C*
     C     ACPODT        IFLE      XXDATE
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C     ACCOTP        OREQ      'V'
     C                   ADD       ACAMT         XXOWED            9 2
     C                   ELSE
     C     ACCOTP        IFEQ      'P'
     C                   SUB       ACAMT         XXOWED
     C                   END
     C                   END
     C                   END
     C*
     C     ACKY2         READE     PAC01                                  50
     C                   END
     C*
     C*   WRITE RECORD IF AMOUNT OWED IS > OR = FUND MINIMUM
     C*
     C     XXOWED        IFGT      0
     C     XXOWED        ANDGE     F2MACC
     C*
     C                   EXSR      BNKACC
     C*
     C                   EXSR      CHKEFT
     C*
     C                   Z-ADD     XXCO#         R2CO#
     C                   Z-ADD     XXFND         R2FND
     C                   Z-ADD     XXAGT#        R2AGT#
     C                   Z-ADD     XXDATE        R2PODT
     C                   Z-ADD     XXDATE        R2TRDT
     C                   Z-ADD     XXOWED        R2AMT
     C                   Z-ADD     0             R2CHCK
     C                   MOVE      CSTYPE        R2COTP
     C                   MOVE      CSDESC        R2DESC
     C                   SELECT
     C     EFT           WHENEQ    *ON
      *******Beg Chg********04/17/12**********************************
     C*******            MOVEL(P)  'EFT'         R2CKTP
      /Free
       R2eftc = 'E';
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVEL(P)  EFTACC        R2ACCT
     C                   OTHER
      *******Beg Chg********04/17/12**********************************
     C******             CLEAR                   R2CKTP
      /Free
       Clear R2eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVEL(P)  REGACC        R2ACCT
     C                   ENDSL
     C                   MOVE      *BLANK        R2STAT
     C                   MOVE      Q1USER        R2USER
     C                   MOVE      Q1PGM         R2PGM
     C                   Z-ADD     DSTIME        R2TIME
     C                   Z-ADD     D1DATE        R2DT
     C                   MOVE      *BLANKS       R2BUL2
     C*
     C                   SELECT
     C     EFT           WHENEQ    *OFF
     C                   WRITE     PR201
     C     EFT           WHENEQ    *ON
     C     PRENOT        ANDEQ     *OFF
     C                   WRITE     PR201
     C     EFT           WHENEQ    *ON
     C     PRENOT        ANDEQ     *ON
     C                   Z-ADD     R2AMT         SVAMT
     C                   CLEAR                   R2AMT
     C                   WRITE     PR201
     C                   Z-ADD     SVAMT         R2AMT
      *******Beg Chg********04/17/12**********************************
     C******             CLEAR                   R2CKTP
      /Free
       Clear R2eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVEL     REGACC        R2ACCT
     C                   WRITE     PR201
     C                   ENDSL
     C*
     C                   END
     C*
     C     ACKY2         SETGT     PAC01
     C     ACKEY         READE     PAC01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*===============================================================
     C* BNKACC - GET BANK ACCOUNTS
     C*===============================================================
     C*
     C     BNKACC        BEGSR
     C*
     C*  GET EFT BANK ACCOUNT
     C*
     C*
     C*  GET EFT BANK ACCOUNT USING NEW PARM IN WA023R
     C*
      *******Beg Chg********04/17/12**********************************
     C******             MOVEL     *BLANKS       ZZCKTP           10
     C******             MOVEL     'EFT'         ZZCKTP
     C*
     C*****              CALL      'WA023R'
     C*****              PARM      QQCO#         #CO#              3            Company
     C*****              PARM      QQFND         #FND              3            Group #
     C*****              PARM      'AG'          ##APPL            2            Applic Code
     C*****              PARM      QQDATE        ##SRDT            8            MMDDCCYY
     C*****              PARM      *BLANKS       ##ACCT           20            Bank Acct #
     C*****              PARM      'C'           ##CALL            1            (V)oucher/(C)heck
     C*****              PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C*****              PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C*****              PARM      *BLANK        ##FTYP           10            Form Type
     C*****              PARM      *BLANK        ##RTCD            1            Return Code
     C*****              PARM      *ZERO         ##DT              7            WMBIP Eff. DT CYYMMD
     C*****              PARM      *BLANK        ##CPYQ           10
     C*****              PARM      ZZCKTP        ##CKTP           10
     C*
      /Free
       //Call the Procedure to retrieve Bank Account in subroutine.
       ##cktp = *Blanks;
       ##eftc = 'E';
       ##acct = *Blanks;

       Exsr RetrieveBankAcc#;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVEL(P)  ##ACCT        EFTACC
     C*
     C*  GET REGULAR BANK ACCOUNT
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              MOVEL     *BLANKS       ZZCKTP           10
     C*****              CALL      'WA023R'
     C*****              PARM      QQCO#         #CO#              3            Company
     C*****              PARM      QQFND         #FND              3            Group #
     C*****              PARM      'AG'          ##APPL            2            Applic Code
     C*****              PARM      QQDATE        ##SRDT            8            MMDDCCYY
     C*****              PARM                    ##ACCT                         Bank Acct #
     C*****              PARM      'C'           ##CALL            1            (V)oucher/(C)heck
     C*****              PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C*****              PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C*****              PARM      *BLANK        ##FTYP           10            Form Type
     C*****              PARM      *BLANK        ##RTCD            1            Return Code
     C*****              PARM      *ZERO         ##DT              7            WMBIP Eff. DT CYYMMD
     C*****              PARM      *BLANK        ##CPYQ           10
     C*****              PARM      ZZCKTP        ##CKTP           10
     C*
      /Free
       //Call the Procedure to retrieve Bank Account.
       ##cktp = *Blanks;
       ##eftc = ' ';

       Exsr RetrieveBankAcc#;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVEL(P)  ##ACCT        REGACC
     C*
     C     REGACC        IFEQ      *BLANKS
     C                   MOVEL(P)  EFTACC        REGACC
     C                   ENDIF                                                  REGAC=*BLANKS
     C*
     C                   ENDSR                                                  BNKACC
     C*
     C*===============================================================
     C* CHKEFT - CHECK IF AGENT IS SETUP WITH EFT PROCESSING
     C*===============================================================
     C*
     C     CHKEFT        BEGSR
     C*
     C                   MOVE      *OFF          EFT               1
     C                   MOVE      *OFF          PRENOT            1
     C*
     C                   MOVE      'A'           ##TYPE
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVEL(P)  XXAGT#        ##KEY
     C                   MOVE      D1DATE        ##DATE
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'WA023R3'
     C*****              PARM                    ##CO#             3            Company
     C*****              PARM                    ##TYPE            1            Payee Type
     C*****              PARM                    ##FND             3            Group #
     C*****              PARM                    ##KEY            30            Key
     C*****              PARM                    ##DATE            7            Date
     C*****              PARM                    EFT                            EFT flag
     C*****              PARM                    PRENOT                         PRE note flag
     C*
      /Free
       //Call the Procedure to retrieve EFT status.
         eft = GetEFTStatus(##co#: ##type: ##fnd: ##key: ##date:
                 prenot);
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   ENDSR                                                  CHKEFT
      *******Beg Add********04/17/12**********************************
      /Free
       BegSr RetrieveBankAcc#;

          #co#  =  QQco#;
          #fnd  =  QQfnd;
          ##appl = 'AG';
          ##srdt = QQdate;
          ##call = 'C';
          ##hdcy = *Blanks;
          ##chkq = *Blanks;
          ##ftyp = *Blanks;
          ##rtcd = *Blanks;
          ##dt   = *Blanks;
          ##cpyq = *Blanks;
          ##coid = *Blanks;

          ##acct= GetBankAcc(#co#: #fnd: ##appl: ##srdt: ##call:
                   ##hdcy: ##chkq: ##ftyp: ##dt: ##cpyq:
                   ##cktp: ##eftc: ##coid: ##rtcd);
       EndSr;
      /End-Free
      *******End Add********04/17/12**********************************
