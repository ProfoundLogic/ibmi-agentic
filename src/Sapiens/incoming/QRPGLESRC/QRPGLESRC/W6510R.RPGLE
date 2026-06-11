     H* (c) 2015 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     h bnddir ('WTBNDDIR')
     H*****************************************************************
     H/TITLE W6510R - PROCESS COMMISSION CHECK REQUEST MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/30/15 C:136 Project: WC2337                                *
     H*                Make sure that check schedule records are      *
     H*                written properly if EFT and MICR bank          *
     H*                acccounts are different                        *
     H*                                                               *
     H* 04/17/12  C:255 Project WC2044                                *
     H*                 Added code for the new field added on the sc- *
     H*                 reen. Replaced WA023R to GetBankAcc proc call.*
     H*                                                               *
     H* 09/20/07 C:136 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSIONS              *
     H*                R2ACCT SHOULD ALREADY BE POPULATED BY WA023R   *
     H*                                                               *
     H* 02/29/00 C:33  SEQ#: WA100/22029                              *
     H*                PASS CORRECT PARMS TO SEARCH.                  *
     H*                                                               *
     H* 07/14/99 C:118 PROJECT: WC577                                 *
     H*                Made chgs to display file so print flag entry  *
     H*                is not allowed in ADD mode and process flag    *
     H*                is required in UPDATE mode.                    *
     H*                                                               *
     H* 11/06/98 C:78 PROJ: WC577                                     *
     H*               ITS DISPLAYING SCMSG1 AND SCMSG2 EVEN IF THE    *
     H*               CURRENT DATE IS THE SAME AS THE 'DATE ENTERED'. *
     H*                                                               *
     H* 06/15/98 C:90 PROJ: WC577                                     *
     H*               CHANGED SCREENS TO CURRENT STANDARDS            *
     H*                                                               *
     H* 05/11/98 C:91 PROJ: WC577                                     *
     H*               CHANGED SCREENS TO CURRENT STANDARDS            *
     H*                                                               *
     H* 04/14/98 C:33 SEQ#: WA095/11129                               *
     H*              -ADD CHECK FOR AP/GL PRIOR TO CALLING AA022R.    *
     H*              -V3 STANDARDS.                                   *
     H*                                                               *
     H* 12/08/97 C:30 Project: WC500                                  *
     H*          Add Edit for Unfinalized AP Checks exist for Group's *
     H*          "current" Bank Account #.  Do not allow Check Sched. *
     H*          record to be written when above situation occurs.    *
     H*                                                               *
     H* 12/02/97 C:30 Project: WC500                                  *
     H*          Add 'Voucher Y/N' flag.  Call WA023R to obtain       *
     H*          "current" Check or Voucher Bank Information record   *
     H*          prior to print process.                              *
     H*           Additional Edits:                                   *
     H*           1) Group Expiration Date valid.                     *
     H*           2) Process Commission Checks ('Y').                 *
     H*           3) Bank Info Account# must exist in WSCKP.          *
     H*                                                               *
     H*   JJJ 1-16-91 - BASE ENHANCEMENT TO SEND A VERIFICATION
     H*                 MESSAGE IF ENTERED DATE DOES NOT EQUAL THE
     H*                 CURRENT SYSTEM DATE
     H*
     H*   JJJ 6-03-91 - PROJECT WC058
     H*                 BASE ENHANCEMENT : DO NOT ALLOW RESTART #
     H*                 TO BE ENTERED IN ADD MODE
     H*                                                               *
     H*****************************************************************
     FW6510F    CF   E             WORKSTN
     F                                     INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWAC2L     UF A E           K DISK
      *________________________________________________________________
      *  Group Master         key=CO#/GRP
     FWMFNL     IF   E           K DISK
      *________________________________________________________________
      *  Group Auxiliary      key=CO#/GRP
     FWDF2L     IF   E           K DISK
      *________________________________________________________________
      *  Bank Account Master  key=CO#/ACCT#
     FWSCKL     IF   E           K DISK
      *________________________________________________________________
      *******Beg Add********04/17/12**********************************
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
      *******End Add********04/17/12**********************************
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D MG              S             60    DIM(2) CTDATA PERRCD(1)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D OPTNM           C                   CONST('Commission Check -
     D                                     Schedule Maintenance')
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d AA022R          pr                  Extpgm('AA022R')
     d ##acct                        20
     d ##srdt                         8
     d ##rtcd                         1
     d*
     d ##co#           s              3
     d ##fnd           s              3
     d ##rtcd          s              1
     d ##eftc          s              1
     d xxappl          s              2
     d ##fyr           s              3
     d ##case          s              7
     d ##coid          s             20
     d ##chkq          s             10
     d ##ftyp          s             10
     d ##hdcy          s              1
     d @@cpyq          s             10
     d ##efdt          s              7
     d ##srdt          s              8
     d ##acct          s             20
     d ##call          s              1
     d @@cktp          s             10
     d strKey          s              2
      *******End Add********04/17/12**********************************
      *** beg add ***** 07/30/15 *************************
     d svEftC          s                   like(##EftC)
      *** end add ***** 07/30/15 *************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  PROCESS COMMISSION CHECKS MAINTENANCE
     C*
     C     C2KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXPRDT
     C                   KFLD                    SCFND
      *
      *
      *  Bank Account Master
      *
      *******Beg chg********04/17/12**********************************
     C*****CKKY          KLIST
     C*****              KFLD                    FNCO#
     C*****              KFLD                    ##ACCT
      *
     C     CKKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    SCACCT
      *
     C     CKKY1         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    SCacce
      *******End chg********04/17/12**********************************
     C*
     C*  FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  AGENT COMMISSION FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXCHK#            7 0
     C*
     C     *LIKE         DEFINE    @@PRDS        QQPRDS
     C     *LIKE         DEFINE    @@CALL        QQCALL
      *
     C     *LIKE         DEFINE    C2PRDT        XXDSDT
     C     *LIKE         DEFINE    C2PRDT        XXPRDT
     C     *LIKE         DEFINE    C2PRDT        XXEFDT
     C     *LIKE         DEFINE    SCPRDT        XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C*
      *******Beg Add********04/17/12**********************************
      /Free
       // Payment field will be visible only for WCfl17.
       strKey = 'WC';
       Chain strKey Swc01;
       If WCfl17 = 'Y';
          *In45 = *On;
       EndIf;
      /End-Free
      *******End Add********04/17/12**********************************
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   Z-ADD     UPFND         SCFND
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE
     C                   MOVE      XXDATE        SCPRDT
     C*
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C                   CLEAR                   QQCALL
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      @@CALL        QQCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG

     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPRDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'W6515R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      SCPRDT        QQPRDT            8 0
     C                   PARM      SCFND         QQFND             3 0
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPRDT        OREQ      *ZERO
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     QQPRDT        ANDNE     *ZERO
     C                   MOVE      QQPRDT        SCPRDT
     C                   GOTO      SCR01T
     C                   ELSE
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC=*
     C*
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *
      *  CHECK THE FUND NUMBER
      *
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VERIFY THE FUND AUXILIARY RECORD
      *
     C     FNKEY         CHAIN     PF201                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Edit 03:  Group Master Expiration Date check
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
      *
     C     FNEXDT        IFGT      *ZERO                                        Group Expiration DT
     C     FNEXDT        ANDLT     XXDT
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Edit 05:  Group Auxiliary Process Agent Commission Flag check
      *
     C     F2AGCK        IFNE      'Y'                                          Process Agents
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ENDIF
      *
     C*
     C*  CHECK THE PROCESS DATE
     C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
      *******Beg Add********04/17/12**********************************
      /Free
       //Invalid value entered.
       If SCeftc <> ' ' And SCeftc <> 'B' And
        SCeftc <> 'E';
          *In30 = *On;
      /End-Free
     C                   GOTO      STRT
     C                   EndIf
      *******End Add********04/17/12**********************************
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
      *
      *
      *  Verify valid Bank Information record exists for Group Account Type
      *
     C                   MOVE      F2CO#         ##CO#                          Company
     C                   MOVE      F2FND         ##FND                          Group #
     C                   MOVE      SCPRDT        ##SRDT                         Search DT
      *
     C                   MOVE      'C'           ##CALL
      *
      *  Call API to access "Current" Bank Account based on ##CALL value
      *
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'WA023R'
     C*****              PARM                    ##CO#             3            Company
     C*****              PARM                    ##FND             3            Group #
     C*****              PARM      'AG'          XXAPPL            2            Applic Code
     C*****              PARM                    ##SRDT            8            MMDDCCYY
     C*****              PARM                    ##ACCT           20            Bank Acct #
     C*****              PARM                    ##CALL            1            (V)oucher/(C)heck
     C*****              PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C*****              PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C*****              PARM      *BLANK        ##FTYP           10            Form Type
     C*****              PARM      *BLANK        ##RTCD            1            Return Code
     C*****              PARM      *ZERO         ##EFDT            7            WMBIP Eff. DT CYYMMD
     C*****              PARM      *BLANKS       @@CPYQ           10
     C*****              PARM      *BLANKS       @@CKTP           10
      *
      /Free
       ExSr RetrieveBankAcc#;
      /End-Free
      *******End Chg********04/17/12**********************************

      *** beg add ***** 07/30/15 *************************
      *  *IN90 will be turned on if there isn't a bank account available
      *  for the eft type selected
      *
     c                   If        *in90 = *on
     c                   eval      *in90 = *off
     c                   Goto      stRt
     c                   Endif
      *** end add ***** 07/30/15 *************************

     C     ##RTCD        IFEQ      '1'
     C                   SETON                                        27
     C                   GOTO      STRT
     C                   ENDIF                                                  ##RTCD
      *
     C                   MOVE      ##EFDT        XXEFDT                         WMBIP Eff. DT CYYMMD
      *
     C     XXEFDT        IFGT      XXPRDT
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   CALL      'SA002R'
     C                   PARM                    ##GLAP            1            GL/AP INSTALLED
     C*
     C     ##GLAP        IFEQ      'Y'
      *
      *  Edit for Un-Finalized AP Checks for group's current Bank Account #.
      *
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'AA022R'
     C*****              PARM                    ##ACCT                         Bank Account
     C*****              PARM                    ##SRDT                         Date
     C*****              PARM                    ##RTCD                         Return Code
      /Free
       If SCacct <> ' ';
          ##acct = SCacct;
          CallP AA022R(##acct :##srdt: ##rtcd);
       EndIf;

       If SCacce <> ' ' And SCacce <> SCacct;
          ##acct = SCacce;
          CallP AA022R(##acct :##srdt: ##rtcd);
       EndIf;
      /End-Free
      *******Beg Chg********04/17/12**********************************
      *
     C     ##RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  ##GLAP=Y
     C*
     C*  SEND A VERIFICATION MESSAGE IF ENTERED DATE DOES NOT EQUAL
     C*  CURRENT SYSTEM DATE
     C*
     C     SCMSG1        IFEQ      *BLANKS
     C     SCPRDT        ANDNE     XXDATE
     C                   MOVEL     MG(1)         SCMSG1
     C                   MOVEL     MG(2)         SCMSG2
     C                   GOTO      STRT
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     C2KEY         CHAIN     PC201                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PC201E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      C2RSTR        SCRSTR
     C                   MOVE      C2WAST        SCWAST
     C                   MOVE      C2STA0        SCSTA0
     C                   MOVE      C2STA1        SCSTA1
     C                   MOVE      C2CHK#        SCCHK#
     C                   MOVE      C2ACCT        SCACCT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C**N60*     ACCO#     COMP UPCO#                6060
     C**N60*     ACFND     COMP SCFND                6060
      *
      *  GET THE LAST CHECK NUMBER FROM THE CHECK SUMMARY FILE.
      *
      *******Beg Chg********04/17/12**********************************
     C*****CKKY          CHAIN     PCK01                              60
      *****
     C******IN60         IFEQ      *ON
      /Free
          If SCacct =  ' ' ;
             Chain ckky1 pck01;
          Else;
             Chain ckky  pck01;
          EndIf;

          If SCeftc = 'E';
             SCchk# = *Zeros;
          Else;
             If Not %Eof;
                SCchk# = CKlChk;
             Else;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
      *******Beg Add********04/17/12**********************************
      /Free
       //End of SCeftc = 'E'
          EndIf;
      /End-Free
      *******End Add********04/17/12**********************************
      *
      *  Edits Passed.  Process Check Summary record
      *
      *******Beg Del********04/17/12**********************************
     C*******            MOVE      CKACCT        SCACCT
     C*******            MOVE      CKLCHK        SCCHK#
      *******End Del********04/17/12**********************************
      *
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCSTA0
     C                   MOVE      *BLANK        SCSTA1
     C                   MOVE      *ZERO         SCRSTR
     C                   MOVE      *BLANK        SCWAST
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         IFEQ      '1'
     C                   GOTO      END02
     C                   END
     C*
     C*  THE CHECK NUMBER CANNOT BE ZERO
     C*
      *******Beg Chg********04/17/12**********************************
     C*****SCCHK#        IFLE      *ZERO
     C*****              MOVE      '1'           *IN20
      /Free
       If SCchk# = *Zeros And SCeftc <> 'E';
          *In20 = *On;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   GOTO      STRT
     C                   END

     C*
     C*  THE RESTART NUMBER MUST BE ZERO IN ADD MODE
     C*
     C     SCRSTR        IFNE      *ZERO
     C     *IN40         ANDEQ     '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  WASTE CHECKS FLAG MUST BE Y, ' '
     C*
     C     SCWAST        IFNE      'Y'
     C     SCWAST        ANDNE     ' '
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  PRINT CHECKS FLAG MUST BE Y, ' '
     C*
     C     SCSTA0        IFNE      'Y'
     C     SCSTA0        ANDNE     ' '
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     C2KEY         CHAIN     PC201                              40
      *
      *******Beg Chg********04/17/12**********************************
     C********           MOVE      ##ACCT        C2ACCT
      /Free
       C2acct = SCacct;
      /End-Free
      *******End Del********04/17/12**********************************
     C                   MOVE      UPCO#         C2CO#
     C                   MOVE      SCFND         C2FND
     C                   MOVE      SCRSTR        C2RSTR
     C                   MOVE      SCWAST        C2WAST
     C                   MOVE      XXPRDT        C2PRDT
     C                   MOVE      SCCHK#        C2CHK#
     C                   MOVE      SCSTA0        C2STA0
     C                   MOVE      SCSTA1        C2STA1
     C                   MOVE      Q1USER        C2USER
     C                   MOVE      Q1PGM         C2PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C2DT
     C*
     C                   Z-ADD     DSTIME        C2TIME
     C*
      *******Beg chg********04/17/12**********************************
     C******IN40         IFEQ      '0'
     C******             UPDATE    PC201
     C******             ELSE
     C********           WRITE     PC201
     C********           END

      /Free
       If *IN40 = '0';
          update PC201;
       Else;
          If C2acct <> ' ' and C2acct <> SCacce;
             C2eftc = ' ' ;
          EndIf;

       //*** beg add ***** 07/30/15 ************************
          If C2acct <> *blanks and C2acct = scAcce;
             C2eftc = 'B';
          Endif;
       //*** end add ***** 07/30/15 ************************

       //MICR check written here
          If C2acct <> '  ';
             Write PC201 ;
          EndIf;

       //EFT check written here
          If C2acct <> SCacce And SCacce <> ' ';
             C2acct = SCacce;
             C2eftc = 'E'   ;
             C2chk# = 0     ;
             Write PC201 ;
          EndIf;

       //*In40 ends here
       EndIf;
      /End-Free
      *******End Add********04/17/12**********************************
     C*
     C     END02         TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ZERO         SCPRDT
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *******Beg Add********04/17/12**********************************
      /Free
       BegSr RetrieveBankAcc#;

          xxappl = 'AG';
          ##hdcy = *Blanks;
          ##chkq = *Blanks;
          ##ftyp = *Blanks;
          ##rtcd = *Blanks;
          ##efdt = *Zeros ;
          @@cpyq = *Zeros ;
          @@cktp = *Blanks;
          ##coid = *Blanks;
          ##eftc = SCeftc ;
       //*** beg chg ***** 07/30/15 ********************************
       //***   ##acct= GetBankAcc(##co#: ##fnd: xxappl: ##srdt: ##call:
       //***            ##hdcy: ##chkq: ##ftyp: ##efdt: @@cpyq:
       //***            @@cktp: ##eftc: ##coid: ##rtcd);
       //***   Select;

       //***   When ##eftc = 'B';

       // If Account No. <> ' ' , MICR & EFT account No. will be same.
       //***      If ##acct <> ' ';
       //***         SCacct = ##Acct;
       //***         SCacce = ##Acct;
       //***      Else ;
       //***         ##eftc = ' ' ;
       //***         ##acct= GetBankAcc(##co#: ##fnd: xxappl: ##srdt: ##call:
       //***                  ##hdcy: ##chkq: ##ftyp: ##efdt: @@cpyq:
       //***                  @@cktp: ##eftc: ##coid: ##rtcd);
       //***        If ##Acct <> ' ' ;
       //***            SCacct = ##acct;
       //***         Else;
       // No MICR account exist
       //***            *In34 = *On;
       //***            *In90 = *On;
       //***         EndIf;

       //***         ##eftc = 'E' ;
       //***         ##acct= GetBankAcc(##co#: ##fnd: xxappl: ##srdt: ##call:
       //***                  ##hdcy: ##chkq: ##ftyp: ##efdt: @@cpyq:
       //***                  @@cktp: ##eftc: ##coid: ##rtcd);
       //***         If ##Acct <> ' ' ;
       //***           SCacce = ##acct;
       //***        Else;
       // No EFT Account exist
       //***            *In35 = *On;
       //***            *In90 = *On;
       //***         EndIf;

       // If ##Acct <> ' ' ends here.
       //***      EndIf;

       //***   When ##eftc = ' ';

       //***      If ##Acct <> ' ' ;
       //***         SCacct = ##acct;
       //***      Else;
       // No MICR Account exist
       //***            *In34 = *On;
       //***            *In90 = *On;
       //***      EndIf;

       //***   When ##eftc = 'E';

       //***      If ##Acct <> ' ' ;
       //***        SCacce = ##acct;
       //***      Else;
       // No EFT Account exist
       //***            *In35 = *On;
       //***            *In90 = *On;
       //***      EndIf;
       //***  EndSl;
       svEftC = scEftC;
       Dow 1=1;
          Select;
          When scEftC = 'B' and svEftC = 'B';
             ##eftC = ' ';
          When scEftC = 'B' and svEftC = ' ';
             ##eftC = 'E';
          When scEftC = 'B';
             Leave;
          Endsl;
          svEftC = ##EftC;

          ##acct= GetBankAcc(##co#:##fnd:xxappl:##srdt:##call:##hdcy:
           ##chkq:##ftyp:##efdt:@@cpyq:@@cktp:##eftc:##coid:##rtcd);

          Select;
          When svEftC = ' ';
             If ##Acct <> ' ' ;
                scAcct = ##acct;
             Else;
             // No MICR account exist
                *In34 = *On;
                *In90 = *On;
             Endif;
          When svEftC = 'E';
             If ##Acct <> ' ' ;
                scAcce = ##acct;
             Else;
             // No EFT account exist
                *In35 = *On;
                *In90 = *On;
             EndIf;
          Endsl;

          If scEftC <> 'B';
             Leave;
          Endif;
       Enddo;
       //*** end chg ***** 07/30/15 ********************************

       EndSr;
      /End-Free
      *******End Add********04/17/12**********************************
     OPC201     E            PC201E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
