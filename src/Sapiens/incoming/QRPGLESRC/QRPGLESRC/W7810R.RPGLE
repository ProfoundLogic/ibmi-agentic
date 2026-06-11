     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7810R - PROMPT FOR COMMERCIAL ROLLOVER
     H*****************************************************************
     H*  PROGRAMMER: C57 - TONY CELLANA                               *
     H*  CREATED:    08/28/96  -- BASED ON CLT 068 W7810R2            *
     H*  PROJECT#:   CM022                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/23/15 C:136 Project: WC2182                                *
     H*                Single employer renewal                        *
     H*                                                               *
     H* 05/22/13 C:279 Redmine: 2960                                  *
     H*                Default checkbox with a 'N'.                   *
     H*                                                               *
     H* 12/05/12 C:65  Project: WC2116                                *
     H*                Default renew to quote checkbox with a 'N'.    *
     H*                                                               *
     H* 12/08/08 C:33  Project: WC1017  WA133/66249                   *
     H*                Do not allow for 'Renew to Quote' in test env  *
     H*                                                               *
     H* 12/11/07 C:190 PROJECT: WC1500                               *
     H*              - Add new flag for "State Reporting Division     *
     H*                Only Renew"                                    *
     H*              - Edit only if Include/Exclude flag is I/E.      *
     H*              - Add Parm for new flag to call to W7810C        *
     H*                                                               *
     H* 04/13/07 C:33  Seq#: WA099/54283                              *
     H*               -Removed F10 availablity from SCR02.            *
     H*               -Process avaiable function keys from SCR02      *
     H*                 correctly.                                    *
     H*                                                               *
     H*  04/19/07 C:189 SEQ#: WA132/56248                             *
     H*                 Invalid error message shown on the screen even*
     H*                 when  the date range is in 31 days            *
     H*                                                               *
     H* 04/18/06 C:168 PROJECT 102WDO                                 *
     H*                Added Include/Exclude states                   *
     H*                                                               *
     H*  03/23/06 C:132 PROJECT: WC1377  (71WGC)                      *
     H*               - Modify the renewal prompt to allow users to   *
     H*                 designate up to 4 different rollover options  *
     H*                 for payroll.                                  *
     H*         NOTE: - Need revised WMEPP, W7820R installed with     *
     H*                 this project.                                 *
     H*                                                               *
     H*  01/06/04 C:136 SUPPORT: WA034 40661 (WC1017)                 *
     H*                 Make renew to quote flag non-display if       *
     H*                 client is claims only.                        *
     H*                                                               *
     H*  10/10/03 C:33  PROJECT: WC1017                               *
     H*                 Add 'Renew to quote' flag.                    *
     H*                                                               *
     H*  04/17/02 C:136 SUPPORT: WA114 32128                          *
     H*                 Correct condition that displays informational *
     H*                 message when date range exceeds 31 day.  It   *
     H*                 presently doesn't check for the year value.   *
     H*                                                               *
     H*  10/26/98 C:61 PROJECT WC577                                  *
     H*                Use UPOUTQ if entered.                         *
     H*                                                               *
     H*  04/29/98  C:78  PROJ:WC745                                   *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL. WDF2L      *
     H*                 DELETED CAUSE ITS NOT BEING USED ANYMORE.     *
     H*                                                               *
     H*  04/06/98 C:65 PROJECT: WC816                                 *
     H*                REMOVE NEW FYR FROM THE PROMPT FOR COMMERCIAL. *
     H*                                                               *
     H*  02/17/98 C:80 PROJECT: WC800                                 *
     H*                CHANGED PARAMETER QQPCT FROM 3 TO 5 CHARACTERS *
     H*                                                               *
     H*  12/04/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                ADDED ASCENDING ORDER CHECK TO SCFTDT & SCTTDT.*
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 41 - 'FROM' EXPIRATION DATE INVALID                           *
     H* 42 - 'TO' EXPIRATION DATE INVALID                             *
     H* 43 - 'TO' POLICY YEAR NOT FOUND                               *
     H* 44 - PAYROLL TYPE INVALID                                     *
     H* 45 - INVALID INCREASE/DECREASE RESPONSE                       *
     H* 46 - INVALID POLICY EXPIRATION DATE RANGE                     *
     H* 47 - INVALID POLICY EXPIRATION DATE RANGE                     *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
      *
     FW7810F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
      *** BEGIN DEL ***  04/06/98  ************************************
      *****WDF1L   IF  E           K        DISK
      *** END DEL *****  04/06/98  ************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      ***** BEGIN DEL ****** 04/29/98 *********************************
      ***** WDF2L   IF  E           K        DISK
      ***** END   DEL ****** 04/29/98 *********************************
      *** BEG ADD ***** 01/06/04 **************************************
     FSWCPF     IF   E             DISK
      *** END ADD ***** 01/06/04 **************************************
      **** Begin Add **** 04/18/06 ***********************************
     FSTABL     IF   E           K DISK
      **** End   Add **** 04/18/06 ***********************************
      *** beg add ****** 06/23/15 ************************************
     fWDELP     if   e             disk    rename(pel01:pel0p)
     fWMEMP     if   e             disk    rename(pem01:pem0p)
     fWMEML     if   e           k disk    rename(pem01:pem0l)
      *** end add ****** 06/23/15 ************************************

     D*
     D***  Begin Add  ***  10/10/03  *********************************
     D OPTNM           C                   CONST('Employer Renewal Pro-
     D                                     cess')
     D***  End   Add  ***  10/10/03  *********************************
     D***  Begin Add  ***  12/08/08  **************************************
     D RTVENV          C                   CONST('RTVENVLIB')
     D***  End   Add  ***  12/08/08  **************************************
      *** beg add ***** 06/23/15 ************************************
     d/copy sprGenPop
     d/copy wprEmplr
     d elRRN           s              9  0
     d emRRN           s              9  0
     d errFlag         s              1
     d firstDispFlg    s               n
     d inkc            s              1
     d inkl            s              1
     d menuCallFlg     s               n
     d renewAllInd     s               n
      *** end add ***** 06/23/15 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXFTDT                 1      8  0
     D  FMNTHS                 1      2  0
     D  FDAYS                  3      4  0
     D  FYEARS                 5      8  0
     D                 DS
     D  XXTTDT                 1      8  0
     D  TMNTHS                 1      2  0
     D  TDAYS                  3      4  0
     D  TYEARS                 5      8  0
     D***  BEG   ADD  ***  04/18/06  *********************************
     D                 DS
     D  SCST1                  1      2
     D  SCST2                  3      4
     D  SCST3                  5      6
     D  SCST4                  7      8
     D  SCST5                  9     10
     D  SCST6                 11     12
     D  SCST7                 13     14
     D  SCST8                 15     16
     D  SCST9                 17     18
     D  SCST10                19     20
     D  SCSTS                 21     21
     D  QQSTIN                 1     21
     D***  END   ADD  ***  04/18/06  *********************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *****************************************************************
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
      *** BEGIN CHG ***  04/06/98  ************************************
      *
      *  GROUP DETAIL FILE
      *
      *****      F1KY      KLIST
      *****                KFLD           UPCO#
      *****                KFLD           XXFND   30
      *****                KFLD           XXFYR   30
      *
     C     *LIKE         DEFINE    UPFND         XXFND
      *** END CHG *****  04/06/98  ************************************
     C***  Begin Add  ***  10/10/03  *********************************
     C     *LIKE         DEFINE    QQPRDS        XXPRDS
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C***  End   Add  ***  10/10/03  *********************************
      *
      *****************************************************************
      *
      *** BEGIN ADD *** 12/04/97 **************************************
      *
      *  DEFINE FROM & TO DATES FOR ASCENDING ORDER COMPARISON
      *
     C                   Z-ADD     0             X1FTDT            7 0          FROM DATE
     C                   Z-ADD     0             X1TTDT            7 0          TO DATE
      *
      *** END   ADD *** 12/04/97 **************************************
     C***  Begin Add  ***  12/08/08  **************************************
     C     ENVPL         PLIST
     C                   PARM      '*OTHER'      ##TYPE           10
     C                   PARM      *BLANKS       ##LIB            10
     C                   PARM      *BLANKS       ##MLIB           10
     C***  End   Add  ***  12/08/08  **************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *** beg add ***** 08/05/13 *************************************
     C                   PARM                    qqRRN             9 0
      *** end add ***** 08/05/13 *************************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C***  Begin Add  ***  10/10/03  *********************************
     C                   CLEAR                   XXPRDS
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE(P)   QQPRDS        XXPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQCALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C***  End   Add  ***  10/10/03  *********************************
     C***  Begin Add  ***  12/08/08  **************************************
     C                   CALL      RTVENV        ENVPL
     C***  End   Add  ***  12/08/08  **************************************
     C***  BEG ADD ******  01/06/04  *********************************
     C                   READ      SWCPF                                  60
     C     WCFL08        COMP      'C'                                    49
     C***  Begin Add  ***  12/08/08  **************************************
     C     ##LIB         IFEQ      *BLANKS
     C     ##MLIB        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN49
     C                   ENDIF
     C***  End   Add  ***  12/08/08  **************************************
     C*** begin chg ***  12/05/12  ****************************************
     C***49****          MOVE      'N'           SCRTOQ
     C                   MOVE      'N'           SCRTOQ
     C*** end chg *****  12/05/12  ****************************************
     C***  END ADD ******  01/06/04  *********************************
     C***** beg add ***** 05/22/13 ****************************************
     C                   MOVE      'N'           SCSTRP
     C***** end add ***** 05/22/13 ****************************************
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
      *** BEGIN DEL ***  04/06/98  ************************************
      *****                MOVE UPFYR     SCTFYR
      *** END DEL *****  04/06/98  ************************************
     C***  beg add ***** 06/23/15 ************************************
      /free
       If Q1prms >= 3;
          menuCallFlg = *off;
          dsbEmpDiv = *on;
          visPolDate = *off;
          firstDispFlg = *on;
          elRRN = qqRRN;
          Chain elRRN pel0p;
          UPco#  = ELco#;
          scFnd  = ELfnd;
          scEmp# = ELemp#;
       Else;
          menuCallFlg = *on;
          visPolDate = *on;
       Endif;
      /end-free
     C***  end add ***** 06/23/15 ************************************
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
      *
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      **** BEGIN ADD ******* 04/29/98 *********************************
     C   60              MOVE      *BLANK        FNCGFL
      **** END   ADD ******* 04/29/98 *********************************
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C***  Begin Chg  ***  10/10/03  *********************************
     C**********           MOVE QQPRDS    SCPRDS
     C     XXPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  OPTNM         XXPRDS
     C                   ENDIF                                                  XXPRDS=*BLANKS
     C*
     C                   MOVEL(P)  XXPRDS        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  End   Chg  ***  10/10/03  *********************************
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF IFEQ
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *   COMMAND 03 = END OF JOB
      *
     C     *INKC         IFEQ      *ON
      ****** beg add ***** 05/22/13 ***************************
     C     *IN12         OREQ      *ON
      ****** end add ***** 05/22/13 ***************************
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKA IFEQ
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
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
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10 IFEQ
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
      *** beg del ***** 08/05/13 *************************************
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
      *** BEGIN DEL ***  04/06/98  ************************************
      *****                Z-ADDUPFYR     SCTFYR
      *** END DEL *****  04/06/98  ************************************
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM IFEQ

      *** beg add ***** 06/23/15 **************************************
      /free
       posEmp#Ind = *off;
       posRTypInd = *off;
       If *inkj = *on;
          If hfld = 'SCEMP#';
             emRRN = EmpSrch(inkc:inkl:errFlag:scFnd:UPco#);
          Endif;
          If emRRN <> *zeros;
             Chain emRRN pem0p;
             scEmp# = EMemp#;
             posRTypInd = *on;
             exsr goToStrt01;
          Else;
             posEmp#Ind = *on;
          Endif;
       Endif;
      /end-free
      *** end add ***** 06/23/15 **************************************

     C                   ENDIF                                                  *IN11 IFEQ
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
     C***  Begin Add  ***  10/10/03  *********************************
     C*
     C* Validate 'Return to quote' flag
     C*
     C     SCRTOQ        IFNE      'Y'
     C     SCRTOQ        ANDNE     'N'
     C                   MOVE      *ON           *IN48
     C                   GOTO      STRT
     C                   ENDIF
     C***  End   Add  ***  10/10/03  *********************************
      *
      * VALIDATE 'FROM' EXPIRATION DATE
      *
      *** beg add ***** 06/23/15 ************************************
      /free
       If scemp# = 0;
          exsr EditDates;
       Else;
          If scFtDt <> 0 or scTtDt <> 0;
             erExcDates = *on;
             msExcDates = 'Policy expiration date range and employer fields ' +
              'are mutually exclusive.';
             exsr goToStrt;
          Else;
             erExcDates = *off;
          Endif;
          Chain (UPco#:scFnd:scEmp#) pem0l;
          If not(%found);
             erBadEmp# = *on;
             msBadEmp# = 'Invalid employer number.';
             exsr goToStrt;
          Else;
             erBadEmp# = *off;
          Endif;
       Endif;
      /end-free
      *** end add ***** 06/23/15 ************************************

      *** beg del ***** 06/23/15 ************************************
     C*****SCFTDT        IFEQ      0
     C***********        MOVE      *ON           *IN41
     C***********        GOTO      STRT
      ***********
     C***********        ELSE
      ***********
     C***********        Z-ADD     SCFTDT        SRDTI
     C***********        CALL      'SRCVTDAT'    DATCHK
     C***********        MOVEL     SRDTO         SCFTDT
      ***********
     C*****SRERR*        IFEQ      *ON
     C***********        MOVE      *ON           *IN41
     C***********        GOTO      STRT
     C***********        ENDIF                                                  SRERR IFEQ
     C***********        ENDIF                                                  SCFTDT IFEQ
      ***********
      **VALIDATE*'TO' EXPIRATION DATE
      ***********
     C*****SCTTDT        IFEQ      0
     C***********        MOVE      *ON           *IN42
     C***********        GOTO      STRT
      ***********
     C***********        ELSE
      ***********
     C***********        Z-ADD     SCTTDT        SRDTI
     C***********        CALL      'SRCVTDAT'    DATCHK
     C***********        MOVEL     SRDTO         SCTTDT
      ***********
     C*****SRERR*        IFEQ      *ON
     C***********        MOVE      *ON           *IN42
     C***********        GOTO      STRT
     C***********        ENDIF                                                  SRERR IFEQ
     C***********        ENDIF                                                  SCTTDT IFEQ
      ***********
      ****BEGIN*ADD *** 12/04/97 **************************************
      ***********
      ***DATES*MUST BE IN ASCENDING ORDER
      ***********
     C***********        Z-ADD     SCFTDT        SRDTI
     C***********        CALL      'SRCVTDAT'    DTCYMD
     C***********        MOVEL     SRDTO         X1FTDT
      ***********
     C***********        Z-ADD     SCTTDT        SRDTI
     C***********        CALL      'SRCVTDAT'    DTCYMD
     C***********        MOVEL     SRDTO         X1TTDT
      ***********
     C*****X1FTDT        IFGT      X1TTDT
     C***********        MOVE      '1'           *IN40
     C***********        GOTO      STRT
     C***********        END
      *** end del ***** 06/23/15 ************************************
      *** END   ADD *** 12/04/97 **************************************
      *
      *  POLICY YEAR MUST BE SET UP IN GROUP DETAIL
      *
     C                   MOVE      SCFND         XXFND
      *** BEGIN DEL ***  04/06/98  ************************************
      *****                MOVE SCTFYR    XXFYR
      *
      *****      F1KY      CHAINPF101                43
      *****      *IN43     IFEQ *ON
      *****                GOTO STRT
      *****                ENDIF
      *** END DEL *****  04/06/98  ************************************
      *
      *  MUST SELECT TYPE "A", "B" OR "C"
      *
     C     SCTYPE        IFNE      'A'
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'C'
      ***** BEG ADD ***** 03/23/06 ************************************
     C     SCTYPE        ANDNE     'D'
      ***** END ADD ***** 03/23/06 ************************************
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTYPE IFNE
      *
      *  MUST FLAG AN INCREASE "I" OR A DECREASE "D" IF A PAYROLL
      *   PERCENT IS KEYED
      *
     C     SCPCT         IFNE      *ZEROS
     C     SCIDFL        ANDNE     'I'
     C     SCIDFL        ANDNE     'D'
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPCT
      *
      * CHECK THE RANGE OF DAYS FOR WARNING MESSAGE
      *
      ***** BEGIN DEL ****** 04/29/98 *********************************
      ******     FNKY      CHAINPF201                60
      ***** END   DEL ****** 04/29/98 *********************************
     C                   MOVE      SCFTDT        XXFTDT
     C                   MOVE      SCTTDT        XXTTDT
      ***** BEGIN CHG ****** 04/29/98 *********************************
      ********   F2CT01    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
      ***** END   CHG ****** 04/29/98 *********************************
      *
     C     MESS          IFNE      '1'
      *** BEG CHG **** 04/17/02 ***********************************************
     C***********TMNTHS    IFGT FMNTHS
      *** BEG CHG **** 04/19/07 ***********************************************
     C*********  TYEARS    IFGT FYEARS
     C*********  TMNTHS    ORGT FMNTHS
      *** END CHG **** 04/17/02 ***********************************************
     C*********  TDAYS     ANDGEFDAYS
      *** END CHG **** 04/19/07 ***********************************************
      *** BEG ADD **** 04/19/07 ***********************************************
     C                   MOVE      XXTTDT        XXTTD             8 0
     C                   MOVE      XXFTDT        XXFTD             8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXFTD
     C                   PARM                    XXTTD
     C                   PARM                    @DIFF             5 0
     C                   PARM                    @ERR              1
     C*
     C     @DIFF         IFGT      31
      *** END ADD **** 04/19/07 ***********************************************
     C                   MOVE      *ON           *IN46
     C                   MOVE      '1'           MESS              1
     C                   GOTO      STRT
     C                   ENDIF                                                  TMNTHS IFGT
     C                   ENDIF                                                  *IN46  IFGT
      *
     C                   ELSE
      *
     C     MESS          IFNE      '1'
     C     TYEARS        IFGT      FYEARS
     C     TMNTHS        IFGT      FMNTHS
     C     TMNTHS        OREQ      FMNTHS
     C     TDAYS         ANDGE     FDAYS
     C                   MOVE      *ON           *IN47
     C                   MOVE      '1'           MESS              1
     C                   GOTO      STRT
     C                   ENDIF                                                  TMNTHS IFGT
     C                   ENDIF                                                  TYEARS IFGT
     C                   ENDIF                                                  *IN47  IFGT
      *
     C                   ENDIF                                                  F2CT01 IFEQ
      *
      **** Begin Add **** 04/18/06 ***********************************
     C     *IN03         DOUEQ     *ON
     C     *IN12         OREQ      *ON
      *
     C                   EXFMT     SCR02
     C                   MOVE      *OFF          *IN73
      ***** BEG ADD ***** 12/11/07 **********************************
     C                   MOVE      *OFF          *IN75
     C                   MOVE      *OFF          *IN76
      ***** END END ***** 12/11/07 **********************************
     C                   MOVE      *OFF          *IN77
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79
      *
     C     *IN03         IFEQ      *ON
     C     *IN12         OREQ      *ON
     C                   LEAVE
     C                   ENDIF
      *
     C                   EXSR      VLDSC2
      *
     C     *IN73         IFEQ      *ON
      ***** BEG ADD ***** 12/11/07 **********************************
     C     *IN75         OREQ      *ON
     C     *IN76         OREQ      *ON
      ***** END END ***** 12/11/07 **********************************
     C     *IN77         OREQ      *ON
     C     *IN78         OREQ      *ON
     C     *IN79         OREQ      *ON
     C                   ITER
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      **** End   Add **** 04/18/06 ***********************************
      *
     C***  Begin Add  ***  04/13/07  **************************************
     C                   SELECT
     C     *IN03         WHENEQ    *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C     *IN12         WHENEQ    *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDSL
     C***  End   Add  ***  04/13/07  **************************************
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
      *** BEGIN CHG ***  04/06/98  ************************************
      *****                MOVE SCTFYR    QQFYR   3
     C                   MOVE      '000'         QQFYR             3
      *** END CHG *****  04/06/98  ************************************
     C                   MOVE      SCTYPE        QQTYPE            1
      *** BEGIN CHG *** 02/17/98 **************************************
      ******************** MOVE SCPCT     QQPCT   3
     C                   MOVE      SCPCT         QQPCT             5
      *** END   CHG *** 02/17/98 **************************************
     C                   MOVE      *BLANK        QQOPT             1
     C                   MOVE      SCIDFL        QQIDFL            1
     C                   MOVE      SCFTDT        QQFTDT            8
     C                   MOVE      SCTTDT        QQTTDT            8
      *** BEGIN ADD ***  05/14/97  ************************************
     C                   Z-ADD     DSTIME        QQTIM             6 0
     C                   MOVE      QQTIM         QQTIME            6
     C                   CALL      'W7812R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    QQTIM
     C                   PARM                    SCCONM
     C                   PARM                    SCPRDS
     C                   PARM                    UPCO#
     C                   PARM                    SCFND
     C                   PARM                    SCNAME
     C                   PARM                    QQFTDT
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC EQ 1
      *
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL EQ 1
      *** END ADD *****  05/14/97  ************************************
      ***** BEG ADD ******* 10/26/98 **********************************
     C                   MOVE      UPOUTQ        QQOUTQ           10
      ***** END ADD ******* 10/26/98 **********************************
      *
      ***** BEG ADD ***** 12/11/07 **********************************
     C                   MOVE      SCSTRP        QQSTRP            1
      ***** END END ***** 12/11/07 **********************************

      *** beg add ***** 06/23/15 ************************************
      /free
       qqEmp# = %editc(scEmp# : 'X');
      /end-free
      *** end add ***** 06/23/15 ************************************

     C                   CALL      'W7810C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQTYPE
     C                   PARM                    QQPCT
     C                   PARM                    QQOPT
     C                   PARM                    QQIDFL
     C                   PARM                    QQFTDT
     C                   PARM                    QQTTDT
      *** BEGIN ADD ***  05/14/97  ************************************
     C                   PARM                    QQTIME
      *** END ADD *****  05/14/97  ************************************
      ***** BEG ADD ******* 10/26/98 **********************************
     C                   PARM                    QQOUTQ
      ***** END ADD ******* 10/26/98 **********************************
     C***  Begin Add  ***  10/10/03  *********************************
     C                   PARM      SCRTOQ        QQRTOQ            1
     C***  End   Add  ***  10/10/03  *********************************
      **** Begin Add **** 04/18/06 ***********************************
     C                   PARM                    QQSTIN
      **** End   Add **** 04/18/06 ***********************************
      ***** BEG ADD ***** 12/11/07 **********************************
     C                   PARM                    QQSTRP
      ***** END END ***** 12/11/07 **********************************
      *** BEG ADD ***** 06/23/15 **************************************
     C                   PARM                    QQEMP#            9
      *** END ADD ***** 06/23/15 **************************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      **** Begin Add **** 04/18/06 ***********************************
     C     VLDSC2        BEGSR
      *
     C                   MOVE      '00000000'    *IN21
     C                   MOVE      '00'          *IN29
      *
     C     *BLANKS       IFNE      SCST1
     C                   MOVEL     SCST1         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST2
     C                   MOVEL     SCST2         STATE            10
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST3
     C                   MOVEL     SCST3         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST4
     C                   MOVEL     SCST4         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN24
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST5
     C                   MOVEL     SCST5         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST6
     C                   MOVEL     SCST6         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST7
     C                   MOVEL     SCST7         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST8
     C                   MOVEL     SCST8         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST9
     C                   MOVEL     SCST9         STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN29
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     *BLANKS       IFNE      SCST10
     C                   MOVEL     SCST10        STATE
     C     STATE         CHAIN     STABL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   ENDIF
      *
     C     SCSTS         IFEQ      'I'
     C     SCSTS         OREQ      'E'
     C     SCST1         IFEQ      *BLANKS
     C     SCST2         ANDEQ     *BLANKS
     C     SCST3         ANDEQ     *BLANKS
     C     SCST4         ANDEQ     *BLANKS
     C     SCST5         ANDEQ     *BLANKS
     C     SCST6         ANDEQ     *BLANKS
     C     SCST7         ANDEQ     *BLANKS
     C     SCST8         ANDEQ     *BLANKS
     C     SCST9         ANDEQ     *BLANKS
     C     SCST10        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN78
     C                   ENDIF
      ***** BEG ADD ***** 12/11/07 **********************************
     C  N78SCSTRP        IFEQ      ' '
     C                   MOVE      *ON           *IN76
     C                   ENDIF
      ***** END END ***** 12/11/07 **********************************
     C                   ENDIF
      *
      ***** BEG ADD ***** 12/11/07 **********************************
     C     SCSTS         IFEQ      ' '
     C     SCSTRP        IFEQ      'Y'
      ***** beg del ***** 05/22/13 **********************************
     C*****SCSTRP        OREQ      'N'
      ***** end del ***** 05/22/13 **********************************
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C                   ENDIF
      ***** END END ***** 12/11/07 **********************************
      *
     C     SCSTS         IFNE      'I'
     C     SCSTS         ANDNE     'E'
     C     SCST1         IFNE      *BLANKS
     C     SCST2         ORNE      *BLANKS
     C     SCST3         ORNE      *BLANKS
     C     SCST4         ORNE      *BLANKS
     C     SCST5         ORNE      *BLANKS
     C     SCST6         ORNE      *BLANKS
     C     SCST7         ORNE      *BLANKS
     C     SCST8         ORNE      *BLANKS
     C     SCST9         ORNE      *BLANKS
     C     SCST10        ORNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C                   ENDIF
     C     SCSTS         IFNE      'I'
     C     SCSTS         ANDNE     'E'
     C     SCSTS         ANDNE     ' '
     C                   MOVE      *ON           *IN79
     C                   ENDIF
      *
     C                   ENDSR
      **** End   Add **** 04/18/06 ***********************************
      *** beg add ***** 06/23/15 *************************************
      *===============================================================
      * Goto Strt - how to use a Goto in Free Form
      *===============================================================
     c     goToStrt      begsr
     c                   goto      strt
     c                   endsr
      *===============================================================
      * Goto Strt01 - how to use a Goto in Free Form
      *===============================================================
     c     goToStrt01    begsr
     c                   goto      strt01
     c                   endsr
      *===============================================================
      * Edit Dates - Edit dates subroutine
      *===============================================================
     C     editDates     BEGSR

     C     SCFTDT        IFEQ      0
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   Z-ADD     SCFTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFTDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
     C                   ENDIF                                                  SCFTDT IFEQ
      *
      * VALIDATE 'TO' EXPIRATION DATE
      *
     C     SCTTDT        IFEQ      0
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   Z-ADD     SCTTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTTDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
     C                   ENDIF                                                  SCTTDT IFEQ
      *
      *** BEGIN ADD *** 12/04/97 **************************************
      *
      *  DATES MUST BE IN ASCENDING ORDER
      *
     C                   Z-ADD     SCFTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FTDT
      *
     C                   Z-ADD     SCTTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TTDT
      *
     C     X1FTDT        IFGT      X1TTDT
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C                   END
     C                   ENDSR
      *** end add ***** 06/23/15 *************************************

