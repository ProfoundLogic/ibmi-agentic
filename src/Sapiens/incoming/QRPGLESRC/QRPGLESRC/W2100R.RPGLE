     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2100R - BILL TRANSACTION FILE MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/16/25 C:65  Project: WC2901  (Phw#119230/CoveRisk)         *
     H*                Populate a 'Y' into BLFLG2 if either audit     *
     H*                checkbox has been checked.  Load the proper    *
     H*                checkbox on 2nd screen entry as well.          *
     H*                                                               *
     H* 07/07/23 C:291 Project: WC2901                                *
     H*                Add a new checkbox to the prompt screen for    *
     H*                audits to allow new vs revised to be specified.*
     H*                                                               *
     H* 07/20/21 C:318 SUPPORT:Phw#96795 (InHouse)                    *
     H*                Converted "Update A/R" and "Audit flag" to Cbx *
     H*                on both SCR01 and SCR02 in display file        *
     H*                                                               *
     H* 05/17/21 C:65  SUPPORT: Phw#95945 (InHouse)                   *
     H*                Disallow audit flag to be checked at same time *
     H*                as other two flags.                            *
     H*                                                               *
     H* 03/27/11 C:235 SUPPORT: 71062                                 *
     H*                MASS BILLING ISSUES                            *
     H*                New/Renew Policy Flag is used to populate      *
     H*                RENEWAL FLAG (NEW/RENEW FLAG) in Billing       *
     H*                Transation File if New/Renew Policy Flag       *
     H*                not Blank                                      *
     H*                                                               *
     H* 02/03/09  C:33  Project: 146WBW                               *
     H*                 Do not allow '3rd Party Billing' groups to be *
     H*                  used for this option.                        *
     H*                                                               *
     H* 11/14/07  C:190 PROJECT: 91WOV                                *
     H*           If the policy is a "POINT" policy:                  *
     H*              Do not allow a single Bill Request               *
     H*              to be processed.                                 *
     H*                                                               **
     H*  02/03/03 C:132 PROJECT: WC1017                               *
     H*                Change Scrn04 to check for function keys for   *
     H*                exit and return to prior screen                *
     H*                                                               *
     H*  01/06/03 C:65 PROJECT: SUPP#34947                            *
     H*                POLICY EFFECTIVE DATE RANGE IS VALID IF BOTH   *
     H*                DATES ARE THE SAME.                            *
     H*                                                               *
     H*  11/12/02 C:132 PROJECT: WC1017                               *
     H*                Change to Include/Exclude up to 10 States by   *
     H*                comparing each State code to the employer      *
     H*                jusridiction state.                            *
     H*                                                               *
     H*  03/08/02 C:65 PROJECT: WC1030                                *
     H*                IF THE EMPLOYER HAS A PREVIOUS EMPLR# THEN     *
     H*                CONSIDER THE POLICY AS A RENEWAL.              *
     H*                                                               *
     H*  01/28/00 C:118 PROJECT: WC965                                *
     H*                 MODIFY PGM TO CALL REWRITE OF W2101R WITH     *
     H*                 DIFFERENT PARMS.                              *
     H*                                                               *
     H*  03/13/98 C:65 PROJECT: WC577                                 *
     H*                FIXED PARM SIZE FOR EMPLOYER AND DATE.         *
     H*                                                               *
     H*  02/23/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  12/10/96 C:09 PROJECT WC540                                  *
     H*                ALLOW USER TO SET ALL FLAGS TO BLANKS ON SCR02 *
     H*                THIS IS THE ONLY WAY TO REMOVE A RECORD FROM   *
     H*                PROCESSING ONCE ESTABLISHED (BESIDES CLEARING  *
     H*                ALL RECORDS FOR THE FUND YEAR).                *
     H*                                                               *
     H*  11/21/96 C:65 PROJECT: CM023                                 *
     H*                CHANGED FYR TO POLICY DATE RANGE FOR MASS      *
     H*                BILLINGS. CHANGED WORDING IN W2100F TO REFLECT *
     H*                THIS. THE POLICY DATE RANGE IS REQUIRED NOW    *
     H*                FOR MASS BILLINGS WHILE THE EMPLOYER RANGE     *
     H*                FOR THEM HAS BECOME OPTIONAL.  THE NEW PARMS   *
     H*                ARE PASSED ALONG TO W2100C. AGT#,RFLG, AND     *
     H*                PFDT ARE BEING UPDATED IN THE BILLING FILE TOO.*
     H*                                                               *
     H*  07/25/96 C:64 PROJECT WC540                                  *
     H*                ADD BILLING FLAG EDITS TO ENSURE REQUEST FOR   *
     H*                BILLING NOT MADE WITHOUT AUDIT, BILLING OR UP- *
     H*                DATE A/R FLAG BEING SET TO 'Y'.                *
     H*                                                               *
     H*  05/24/96 C:09 PROJECT WC407                                  *
     H*                REMOVING THE DIVISION FROM SCR01 EMPLR NUMBER  *
     H*                ONLY APPLIES TO EMPLR NUMBERS IN THE EMPLR     *
     H*                RANGE NOT FOR A SINGLE EMPLOYER.               *
     H*                                                               *
     H*  01/22/96 C:48 PROJECT WC407                                  *
     H*                ADD: WHEN LOAD UPDATE AND BILLING FLAGS EXCLUDE*
     H*                AUDITED ACCOUNTS                               *
     H*                                                               *
     H*  04/06/95 C:24 PROJECT WC407                                  *
     H*                ADD PROCESSING FOR AN EMPLOYER RANGE WITHIN A  *
     H*                FUND/FYR FOR CONTROLLED MASS BILLINGS.         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   SCR01 (ADDED 04/06/95):                                     *
     H*   09  IN INQUIRY MODE                                         *
     H*   23  WCR0500 - FROM DATE IS INVALID                          *
     H*   24  WCR0538 -FROM EMPLOYER IS INVALID                       *
     H*   25  WCR0538 -TO EMPLOYER IS INVALID                         *
     H*   26  WCR0825 -EMPLOYER RANGE IS NOT ASCENDING                *
     H*   27  WCR1085 -AT LEAST 1 RECORD ALREADY EXISTS FOR MASS RUN  *
     H*   28  WCR0571 -BILLING FLAG IS INVALID                        *
     H*   29  WCR0571 -UPDATE A/R FLAG IS INVALID                     *
     H*   30  WCR0571 -AUDIT FLAG IS INVALID                          *
     H*   31  WCR0728 -CURSOR MUST BE ON AN EMP# TO SEARCH EMP#       *
     H*   32  WCR1058 -CURSOR MUST BE ON A FYR TO SEARCH FUND YEAR    *
     H*   33  POSITION CURSOR AT FUND YEAR FOR SINGLE EMPLOYER (FYR)  *
     H*   34  POSITION CURSOR AT SINGLE EMPLOYER                      *
     H*   35  WCR0500 - TO DATE IS INVALID.                           *
     H*   44  POLICY DATE RANGE IS INVALID                            *
     H*   36  POSITION CURSOR AT FROM EMPLOYER                        *
     H*   37  POSITION CURSOR AT TO EMPLOYER                          *
     H*   38  POSITION CURSOR AT BILLING FLAG                         *
     H*   39  WCR0503 -CURSOR AT BILLING FLAGS                        *
     H*  INDICATOR USAGE:                                             *
     H*   SCR02                                                       *
     H*   23  WCR0503 -CURSOR AT BILLING FLAGS                        *
     H*****************************************************************
     FW2100F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX00)
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     FWABLL     UF A E           K DISK
     FWABLL5    IF   E           K DISK
     F                                     RENAME(PBL01:PBL02)
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWABLP     IF   E             DISK
     F                                     RENAME(PBL01:PBL03)
     FWZEXL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK

     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  #CODES                 1     20
     D  SCST01                 1      2
     D  SCST02                 3      4
     D  SCST03                 5      6
     D  SCST04                 7      8
     D  SCST05                 9     10
     D  SCST06                11     12
     D  SCST07                13     14
     D  SCST08                15     16
     D  SCST09                17     18
     D  SCST10                19     20
     D OPTNM           C                   CONST('Bill Transaction -
     D                                     File Mainenance')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  BILL TRANSACTION FILE
     C*
     C     BLKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     BLKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXDT              7 0
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     ZEXKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVEL(P)  @@PRDS        QQPRDS           40
     C                   ENDIF
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  @@CALL        QQCALL            1
     C                   ENDIF
     C*
     C     QQPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  OPTNM         QQPRDS
     C                   ENDIF
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  QQPRDS        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C                   MOVEL     UPEMP#        SCEMP#
     C                   MOVEL     UPDIV         SCDIV
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   CLEAR                   EX3RDB
     C     FNKEY         CHAIN     PEX00                              60
     C     EX3RDB        IFEQ      'Y'
     C                   MOVEL(P)  'WDW01'       Q2RECF
     C                   EXFMT     WDW01
     C                   ENDIF
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
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   F5 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C                   CALL      'W2101R'
     C*
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQCALL            1
     C                   PARM      DBRRN         QQRRN             9 0
     C                   PARM                    QQRRN2            9 0
     C*
     C     QQRRN2        IFEQ      *ZEROS
     C     *INKL         OREQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRRN2=0
     C*                                                    *INKL=ON
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     QQRRN2        CHAIN     PBL03                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     BLFYR         SCFYR
     C                   Z-ADD     BLEMP#        SCEMP#
     C                   Z-ADD     BLDIV         SCDIV
     C                   GOTO      SCR01T
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKE=ON
     C*
     C*  F10 - SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*       (ERROR MESSAGE WCR1058)
     C*
     C     HFLD          COMP      'SCEFDT'                               51
     C     HFLD          COMP      'SCTODT'                               52
     C     HFLD          COMP      'SCAFL1'                               55
     C     HFLD          COMP      'SCAFL3'                               56
     C     HFLD          COMP      'SCAFL2'                               57
     C*
     C*  F10 - TABLE FILE SEARCH - POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCFYR'
     C*
     C*  EMPLOYER NUMBER EQUAL TO ZERO
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN33                          SCFYR POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  EMPLOYER NUMBER NOT EQUAL TO ZERO
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN2        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   MOVE      *ON           *IN41                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @1RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN33                          SCFYR POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C*  GROUP YEAR EQUAL TO ZERO
     C*
     C     SCFYR         IFEQ      *ZEROS
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN2        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   MOVE      *ON           *IN41                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @1RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN33                          SCFYR POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCFYR=0
     C*
     C*  GROUP YEAR NOT EQUAL TO ZERO
     C*
     C     SCFYR         IFNE      *ZEROS
     C                   MOVE      *ON           *IN41                          SCEFDT POSITION
     C                   ENDIF                                                  SCFYR<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN34                          SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C*  F10 - TABLE FILE SEARCH - "FROM" OR "TO" EMPLOYER NUMBER
     C*
     C     HFLD          IFEQ      'SCFEM#'
     C     HFLD          OREQ      'SCTEM#'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     HFLD          IFEQ      'SCFEM#'
     C                   MOVE      EMEMP#        SCFEM#
     C                   MOVE      *ON           *IN37                          SCTEM# POSITION
     C                   ENDIF                                                  HFLD=SCFEM#
     C*
     C     HFLD          IFEQ      'SCTEM#'
     C                   MOVE      EMEMP#        SCTEM#
     C                   MOVE      *ON           *IN38                          SCAFL1 POSITION
     C                   ENDIF                                                  HFLD=SCTEM#
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C*
     C     HFLD          IFEQ      'SCFEM#'
     C                   MOVE      *ON           *IN36                          SCFEM# POSITION
     C                   ENDIF                                                  HFLD=SCFEM#
     C*
     C     HFLD          IFEQ      'SCTEM#'
     C                   MOVE      *ON           *IN37                          SCTEM# POSITION
     C                   ENDIF                                                  HFLD=SCTEM#
     C*
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCFEM#
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDIF                                                  *INKJ=ON
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
     C                   WRITE     SCR03
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
     C                   WRITE     SCR03
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C* ============================= *
     C*   GOTO THE APPROPRIATE SCREEN
     C* ============================= *
     C     Q2RECF        CABEQ     'WDW01'       TERM
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C                   Clear                   erAFL2
      //*** Beg Add ***** 07/07/23 ***********************************
       Clear erAFLAR;
       Clear mgAFLAR;
       Clear erAFL2AN;
       Clear mgAFL2AN;
      //*** End Add ***** 07/07/23 ***********************************
     C*
     C* MUST HAVE AT LEAST ONE OPTION SELECTED.
     C*
     C     SCFYR         IFEQ      *ZERO
     C     SCEFDT        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFYR NE 0
     C*
     C* CANNOT HAVE BOTH SINGLE AND MULTIPLE OPTIONS SELECTED.
     C*
     C     SCFYR         IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C*
     C     SCEFDT        IFNE      *ZERO
     C     SCTODT        ORNE      *ZERO
     C     SCFEM#        ORNE      *ZERO
     C     SCTEM#        ORNE      *ZERO
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEFDT NE 0
     C                   ENDIF                                                  SCFYR NE 0
     C*
     C*   DEFAULT TO ONE EMPLOYER
     C*
     C     SCFYR         IFNE      *ZERO                                         1 EMPLR SELECTED
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C     *IN09         OREQ      '1'                                           OR IN INQUIRY MODE
     C     SCFYR         CABEQ     *ZERO         STRT                     21     FYR REQUIRED
     C     SCEMP#        CABEQ     *ZERO         STRT                     20     EMP# REQUIRED
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              21
     C     *IN21         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE EMPLOYER NUMBER AND DIVISION
     C*
     C     EMKEY         CHAIN     PEM01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C*
     C* VALIDATE THAT EMPLOYER DETAIL EXISTS.
     C*
     C     ELKY          CHAIN     PEL01                              46
     C     *IN46         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN46 EQ 1
     C     ELFCD1        IFEQ      'PT'
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   GET BILL TRANSACTION FILE RECORD, IF ANY
     C*
     C     BLKEY         CHAIN     PBL01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      MD(3)         SCMODE
     C*
     C                   MOVE      BLFLG1        SCFLG1
     C*** begin chg ***  06/16/25  *************************************
     C******             MOVE      BLFLG2        SCFLG2
     C                   Select
     C                   When      BLFLG2 = 'Y' and ELFAF <> 'A'
     C                   MOVE      BLFLG2        SCFLG2
     C                   When      BLFLG2 = 'Y' and ELFAF = 'A'
     C                   MOVE      BLFLG2        SCFLGAR
     C                   Other
     C                   MOVE      *BLANK        SCFLG2
     C                   MOVE      *BLANK        SCFLGAR
     C                   Endsl
     C*** end chg *****  06/16/25  *************************************
     C                   MOVE      BLFLG3        SCFLG3
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      MD(2)         SCMODE
     C*
     C                   MOVE      *BLANK        SCFLG1
     C                   MOVE      *BLANK        SCFLG2
     C                   MOVE      *BLANK        SCFLG3
     C*** begin add ***  06/16/25  *************************************
     C                   MOVE      *BLANK        SCFLGAR
     C*** end add *****  06/16/25  *************************************
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
      //*** Beg Add ***** 07/07/23 ***********************************
       Clear mgFLG2ME;
       Clear erFLG2ME;
       Clear mgFLGARME;
       Clear erFLGARME;
      //*** End Add ***** 07/07/23 ***********************************
     C*   CHECK FLAGS
     C*
     C     SCFLG1        IFNE      'Y'
     C     SCFLG1        ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFLG3        IFNE      'Y'
     C     SCFLG3        ANDNE     ' '
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFLG2        IFNE      'Y'
     C     SCFLG2        ANDNE     ' '
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END

      //*** Beg Add ***** 07/07/23 ******************************************
      // Audits New only and Audits Revise only fields are mutually exclusive
        If SCFLG2 = 'Y' and SCFLGAR  = 'Y';
           mgFLG2ME = 'These fields are mutually Exclusive';
           erFLG2ME = *On;
           mgFLGARME = 'These fields are mutually Exclusive';
           erFLGARME = *On;
     C                   Goto      Strt
        Endif;

        If  SCFLG2 = 'Y' and ELFAF = 'A';
           mgFLG2ME = 'This Policy is Audited. New Only Option ' +
            'is not allowed.';
           erFLG2ME = *On;
     C                   Goto      Strt
        Endif;
        If  SCFLGAR = 'Y' and ELFAF <> 'A';
          mgFLGARME = 'This Policy is not Audited. Revised Only Option is ' +
            'not allowed.';
           erFLGARME = *On;
     C                   Goto      Strt
        Endif;

      //*** End Add ***** 07/07/23 ******************************************
     C*
     C*  MOVE DATA
     C*
     C                   Z-ADD     UPCO#         BLCO#
     C                   Z-ADD     UPFND         BLFND
     C                   Z-ADD     SCFYR         BLFYR
     C                   Z-ADD     SCEMP#        BLEMP#
     C                   Z-ADD     SCDIV         BLDIV
     C                   MOVE      SCFLG1        BLFLG1
     C                   MOVE      SCFLG2        BLFLG2
     C*** begin add ***  06/16/25  *******************************************
     C                   If        SCFLGAR = 'Y'
     C                   MOVE      SCFLGAR       BLFLG2
     C                   Endif
     C*** end add *****  06/16/25  *******************************************
     C                   MOVE      SCFLG3        BLFLG3
     C                   MOVE      ' '           BLSTAT
     C                   MOVE      Q1USER        BLUSER
     C                   MOVE      Q1PGM         BLPGM
     C                   Z-ADD     DSTIME        BLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELPFDT        BLPFDT
     C     ELKY          CHAIN     PEL01                              60
     C     ELKY1         READPE    PEL01                                  60
     C   60ZEXKY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'R'           BLRFLG
     C                   ELSE
     C                   MOVE      'N'           BLRFLG
     C                   ENDIF                                                  *IN60 EQ 0
     C                   Z-ADD     *ZERO         BLAGT#
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   ENDDO                                                  *IN50 EQ 0
     C*
     C     *IN50         IFEQ      '0'
     C     UPCO#         ANDEQ     AHCO#
     C     UPFND         ANDEQ     AHFND
     C     SCEMP#        ANDEQ     AHEMP#
     C     SCDIV         ANDEQ     AHDIV
     C     SCFYR         ANDEQ     AHFYR
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ENDIF                                                  *IN50 EQ 0
     C*
     C     *IN09         CABEQ     '1'           NOEDT
     C*
     C     ELNWRW        IFNE      ' '
     C                   MOVE      ELNWRW        BLRFLG
     C                   ENDIF
     C*
     C     *IN40         IFEQ      '1'
     C                   WRITE     PBL01
     C                   ELSE
     C                   UPDATE    PBL01
     C                   END
     C     NOEDT         TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCEMP#
     C                   Z-ADD     0             SCDIV
     C                   Z-ADD     0             SCFYR
     C     *IN09         IFEQ      '0'                                          INQUIRY MODE
     C                   Z-ADD     0             SCEFDT
     C                   Z-ADD     0             SCTODT
     C                   Z-ADD     0             SCFEM#
     C                   Z-ADD     0             SCTEM#
     C                   MOVE      *BLANK        SCAFL1
     C                   MOVE      *BLANK        SCAFL2
     C                   MOVE      *BLANK        SCAFL3
     C                   END
     C*
     C                   GOTO      STRT
     C                   ELSE                                                    EMP RANGE SELECTED
     C*
     C*  VALIDATE THE POLICY DATES ENTERED.
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR EQ 1
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR EQ 1
     C*
     C* VALIDATE THE FROM AND TO DATES FORM A VALID RANGE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EFDT1             7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TODT1             7 0
     C     TODT1         IFLT      EFDT1
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF                                                  TODT <= EFDT
     C*
     C                   MOVE      SCEFDT        XXDT
     C*
     C*  DON'T ALLOW THE REQUEST IF AT LEAST ONE BILLING TRANS
     C*  RECORD WITH NON BLANK FLAGS EXISTS.
     C*
     C     BLKY1         SETLL     PBL02                                         CO/FND/PFDT
     C     BLKY1         READE     PBL02                                  60
     C     *IN60         DOWEQ     '0'
     C     BLFLG1        IFEQ      *BLANK
     C     BLFLG3        ANDEQ     *BLANK
     C     BLKY1         READE     PBL02                                  60
     C                   ELSE
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C   27              GOTO      STRT
     C*
     C*  EMPLOYER RANGE IS OPTIONAL BUT MUST BE ASCENDING.
     C*
     C     SCTEM#        CABLT     SCFEM#        STRT                   26        NOT ASCENDING
     C*
     C*  FLAGS MUST BE 'Y' OR BLANK
     C*
     C     SCAFL1        IFNE      'Y'
     C     SCAFL1        ANDNE     ' '
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     SCAFL3        IFNE      'Y'
     C     SCAFL3        ANDNE     ' '
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     SCAFL2        IFNE      'Y'
     C     SCAFL2        ANDNE     ' '
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK TO ENSURE ALL BILLING FLAGS NOT EQUAL SPACES
     C*
     C     SCAFL1        IFEQ      ' '
     C     SCAFL2        ANDEQ     ' '
     C     SCAFL3        ANDEQ     ' '
      *** Beg Add ***** 07/07/23 ****************************************
     C     SCAFLAR       ANDEQ     ' '
      *** End Add ***** 07/07/23 ****************************************
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*  Do not allow audit flag set with other flags.
     C                   If        SCAFL2 = 'Y' and
     C                             (SCAFL1 = 'Y' or SCAFL3 = 'Y')
     C                   Move      *on           erAFL2
     C                   Goto      Strt
     C                   Endif

      //*** Beg Add ***** 07/07/23 ******************************************
      // Do not allow Revised Audit Only flag to set with other flags.
        If SCAFLAR = 'Y' and
         (SCAFL1 = 'Y' or SCAFL3 = 'Y');
           mgAFLAR = 'Audit flag cannot be set at same time with Billing ' +
            'or Update A/R';
           erAFLAR = *On;
     C                   Goto      Strt
        Endif;

      // Audits New only and Audits Revise only fields are mutually exclusive
        If SCAFL2 = 'Y' and SCAFLAR = 'Y';
           mgAFLAR = 'These fields are mutually Exclusive';
           erAFLAR = *On;
           mgAFL2AN = 'These fields are mutually Exclusive';
           erAFL2AN = *On;
     C                   Goto      Strt
        Endif;
      //*** End Add ***** 07/07/23 ******************************************
      *
      * Display Screen Four for Input of State codes to include/exclude
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
      *
      * Process the Include/Exclude of State codes
      * ====================== *
     C     SCR04T        TAG
      * ====================== *
      *
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR04'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
      * Check for Valid State Selection Code
     C     SCSTCD        IFNE      'I'
     C     SCSTCD        ANDNE     'E'
     C     SCSTCD        ANDNE     ' '
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF
      * If have State Codes & SCSTCD not = 'I' or 'E'
     C     SCSTCD        IFEQ      ' '
     C     #CODES        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ENDIF
      * If have Valid State Selection Code & No State codes is error
     C     SCSTCD        IFEQ      'I'
     C     #CODES        ANDEQ     *BLANKS
     C     SCSTCD        OREQ      'E'
     C     #CODES        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
     C                   ENDIF
      * Check All State Codes against STSTL3 to be valid
     C     SCST01        IFNE      *BLANKS
     C                   MOVEL(P)  SCST01        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST02        IFNE      *BLANKS
     C                   MOVEL(P)  SCST02        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST03        IFNE      *BLANKS
     C                   MOVEL(P)  SCST03        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST04        IFNE      *BLANKS
     C                   MOVEL(P)  SCST04        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST05        IFNE      *BLANKS
     C                   MOVEL(P)  SCST05        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST06        IFNE      *BLANKS
     C                   MOVEL(P)  SCST06        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST07        IFNE      *BLANKS
     C                   MOVEL(P)  SCST07        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST08        IFNE      *BLANKS
     C                   MOVEL(P)  SCST08        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST09        IFNE      *BLANKS
     C                   MOVEL(P)  SCST09        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C     SCST10        IFNE      *BLANKS
     C                   MOVEL(P)  SCST10        XXKEY
     C     STKY          CHAIN     PST01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  ALL EDITS PASSED FOR EMPLR RANGE SELECTION.
     C*  CALL CL TO SUBMIT REQUEST.
     C*
     C                   MOVE      SCSTCD        QQSTCD
     C                   MOVEL(P)  #CODES        QQCDES
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      UPFND         QQFND
     C                   MOVE      SCEFDT        QQEFDT
     C                   MOVE      SCTODT        QQTODT
     C                   MOVE      SCFEM#        QQFEM#
     C                   MOVE      SCTEM#        QQTEM#
      //*** Beg Add ***** 07/07/23 ***************************************
      //Pass value 'R' in SCAFL2 if the Revised only check box is checked
       If SCAFLAR = 'Y';
          SCAFL2 = 'R';
       Endif;
      //*** End Add ***** 07/07/23 ***************************************
     C                   CALL      'W2100C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFEM#            9
     C                   PARM                    QQTEM#            9
     C                   PARM                    SCAFL1                         FLAG (1A)
     C                   PARM                    SCAFL2
     C                   PARM                    SCAFL3
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
      *
     C                   PARM                    QQSTCD            1
     C                   PARM                    QQCDES           20
      *
     C                   END                                                      IF FYR & EMP NE 0
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
