     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2155R02 - BILLING SCHEDULE CODE INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE BILLING SCH CODE FILE.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W2155R - WORK WITH BILLING SCHEDULE CODES                  *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/13/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/16/15 C:180 Support 37084                                  *
     H*                Qualify EFT box                                *
     H*                                                               *
     H* 02/07/14 C:136 Project: 129WSB Priss 27                       *
     H*                Hide EFT Only flag for V14R0. Re-implement     *
     H*                into V14R1.                                    *
     H*                                                               *
     H* 11/22/13 C:279 Project: 129WSB                                *
     H*                Added field for EFT only on screen.            *
     H*                                                               *
     H* 03/19/13 C:65  SUPPORT: WA099 78814  (Redmine 2522)           *
     H*                Correct the correction done on 05/07/12. The   *
     H*                program was not setting the # of screens to 2  *
     H*                for Add mode if Vendor or ACH flags are set.   *
     H*                                                               *
     H* 02/08/13 C:190 PROJECT: 140WAU                                *
     H*                Added two fields for "Estimating deposit %     *
     H*                period 1" and "period 2" to screen.            *
     H*                                                               *
     H* 11/28/12 C:283 SUPPORT: WA140 81084                           *
     H*                Only 2 periods should be visible for S/R       *
     H*                type Policies on SCR01.                        *
     H*                                                               *
     H* 05/07/12 C:136 SUPPORT: WA099 78814                           *
     H*                REMOVED FIXES FROM 01/17/11 AND 12/29/11       *
     H*                AND CONDITIONED 2ND SCREEN TO DISPLAY PROPERLY *
     H*                WHEN VENDOR PROCESSING IS NOT ENABLED AND      *
     H*                BSSEFL = 'S' AND PGM IS IN DISPLAY MODE        *
     H*                                                               *
     H* 12/29/11 C:282  SUPPORT: 78814                                *
     H*                 Added the condition CLTID = '132' which       *
     H*                 will not show second screen for self reporting*
     H*                 billing codes and for other the self reporting*
     H*                 will be shown.                                *
     H*                                                               *
     H* 01/17/11 C:246  Project: 139WMO PRISS# 40                    *
     H*                 Don't show second screen for 2 screen as blank*
     H*                 for self reporting billing codes.             *
     H*                                                               *
     H* 11/23/10 C:255  PROJECT: 114WVZ                               *
     H*                 CHECK BILLING SCHEDULE CODE FOR RETENTION BI- *
     H*                 LLING SCHEDULE PLAN FOR CLIENT 114.           *
     H*                                                               *
     H* 04/15/10 C:62   Project: WC1838  25WQW Priss# 29              *
     H*                 Don't allow plan to be cleared if code is     *
     H*                 being used.                                   *
     H*                                                               *
     H* 02/22/10 C:33   Project: WC1838  25WQW Priss# 3               *
     H*                 Allow for large deductible and retro to be    *
     H*                  processed independently or concerrently.     *
     H*                                                               *
     H* 12/30/09 C:232  PROJECT: WC1838                               *
     H*                 ADDED LARGE DEDUCTIBLE PROCESSING FIELDS.     *
     H*                                                               *
     H* 04/07/09 C:136 PROJECT: 139WLE                                *
     H*                ALLOW RETROSPECTIVE RATING ON S/R POLICIES     *
     H*                                                               *
     H* 06/06/08 C:65  PROJECT: CI019                                 *
     H*                CONDITION SECOND SCREEN TO DISPLAY FOR S/R     *
     H*                ONLINE AS WELL.                                *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                ADD A FLAG FOR ONLINE S/R PROCESSING TO SECOND *
     H*                SCREEN.  THIS NEW PROMPT WILL ONLY APPEAR IF   *
     H*                THE BSCD IS S/R AND WCFL38 SYSTEM FLAG IS 'Y'. *
     H*                                                               *
     H* 01/11/07 C:183 PROJECT: 139WGA                                *
     H*                RETRO RATING BILLING CODE MUST BE DESIGNATED   *
     H*                AS AN ESTIMATING PAY PLAN.                     *
     H*                                                               *
     H* 03/22/06 C:168 Project: 129WNU                                *
     H*                Added flag for the 15th day billing on screen 1*
     H*              - BSON15 can only be Y or blanks                 *
     H*              - if BSFREQ = M, then allow BSON15 = 'Y'         *
     H*              - if BSON15 = Y, then BSFREQ must equal 'M'      *
     H*                                                               *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                Add new entries to SCR01.                      *
     H*                {109AM}                                        *
     H*                   - Special S/R processing.             BSMSFL*
     H*                   -   if 'Y', number of months reported BS#MTH*
     H*                   -   Month deposit transferred.        BSTMTH*
     H*                {109BC}                                        *
     H*                   - S/R Frequency                       BSSRFQ*
     H*                                                               *
     H* 02/28/05 C:136 SUPPORT: WA126 45557                           *
     H*                - EDIT FIRST SCREEN TO ENSURE THAT THE TWO      *
     H*                  DEPOSIT PERIOS ARE MUTUALLY EXCLUSIVE.       *
     H*                - CONDITION THE SECOND SCREEN TO DISPLAY       *
     H*                  PROPERLY IF ACH=*ON, VENDOR=*ON, AND         *
     H*                  S/R FLAG IS 'S'                              *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                CONDITION DISPLAY ON ACH PROCESSING FLAG.      *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115                                *
     H*                CONDITION DISPLAY ON VENDOR PROCESSING FLAG.   *
     H*                                                               *
     H* 10/19/99  C:35   PROJECT: WC577                               *
     H*                  CONDITION DISPLAY OF PAID/INCURRED FLAG ON   *
     H*                  SYSTEM FLAG WCFL11.                          *
     H*                                                               *
     H*  10/27/97  C:65  PROJECT: V3                                  *
     H*                  ADDED PROMPT FOR PLAN TYPE.                  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  42 - DO NOT DISPLAY PAID/INCURRED FLAG                       *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Code must be > zero {ADD mode only}           *
     H*  21 - (WC30005) Code cannot be used more than once            *
     H*  22 - (WCR0503) Code description cannot be blank              *
     H*  23 - (WCR0516) Flag not 'S'-Self Reporting or 'E'-Estimating *
     H*  24 - (WCR0513) Rollover billing schedule code invalid        *
     H*  25 - (WCR0570) Percentages not = 100                         *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW2155F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWTBSP     UF A E             DISK
     F                                     RENAME(PBS01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWTABL     IF   E           K DISK
      *** BEG ADD ***** 12/30/09 **************************************
     FWDELLA    IF   E           K DISK
      *** END ADD ***** 12/30/09 **************************************
      **** BEGIN ADD **** 10/19/99 *********************************************
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
      **** END   ADD **** 10/19/99 *********************************************
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  SBS                    1    108  6
     D                                     DIM(12)
     D  SCBS01                 1      9  6
     D  SCBS02                10     18  6
     D  SCBS03                19     27  6
     D  SCBS04                28     36  6
     D  SCBS05                37     45  6
     D  SCBS06                46     54  6
     D  SCBS07                55     63  6
     D  SCBS08                64     72  6
     D  SCBS09                73     81  6
     D  SCBS10                82     90  6
     D  SCBS11                91     99  6
     D  SCBS12               100    108  6
     D                 DS
     D  FBS                    1    108  8
     D                                     DIM(12)
     D  BSAB01                 1      9  8
     D  BSAB02                10     18  8
     D  BSAB03                19     27  8
     D  BSAB04                28     36  8
     D  BSAB05                37     45  8
     D  BSAB06                46     54  8
     D  BSAB07                55     63  8
     D  BSAB08                64     72  8
     D  BSAB09                73     81  8
     D  BSAB10                82     90  8
     D  BSAB11                91     99  8
     D  BSAB12               100    108  8
     D*
     D OPTNM           C                   CONST('Billing Schedule Cod-
     D                                     e Information')
      *** BEG DEL ***** 02/18/03 ***********************************************
     D************* 1                     C         #OFSCR
      *** END DEL ***** 02/18/03 ***********************************************
     D COMAPI          C                   CONST('WA001R')
     D***  Begin Add  ***  02/22/10  **************************************
     D DOTS            C                   CONST('. . . . . . . . . .')
     D XHPIL           C                   CONST('(P/I/L/ ):')
     D XHPI            C                   CONST('(P/I/ ):')
     D XHL             C                   CONST('(L/ ):')
     D XMPIL           C                   CONST('P, I, L or blank.')
     D XMPI            C                   CONST('P, I, or blank.')
     D XML             C                   CONST('L or blank.')
     D****BEG*ADD ***** 11/23/10 **************************************
     D XHPILR          C                   CONST('(P/I/L/R/ ):')
     D XHPIR           C                   CONST('(P/I/R/ ):')
     D XMPILR          C                   CONST('P, I, L, R or blank.')
     D XMPIR           C                   CONST('P, I, R or blank.')
     D****END*ADD ***** 11/23/10 **************************************
     D***  End   Add  ***  02/22/10  **************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE CODE FILE
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*** BEG ADD ***** 12/30/09 **************************************
     C     BSKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXBSCD            3 0
     C*** END ADD ***** 12/30/09 **************************************
      **** BEGIN ADD **** 10/19/99 *********************************************
      *
      * KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      **** END   ADD **** 10/19/99 *********************************************
     C*
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     BSSTAT        IFEQ      'I'
     C                   MOVE      'A'           BSSTAT
     C                   ELSE
     C                   MOVE      'I'           BSSTAT
     C                   ENDIF                                                  BSSTAT=I
     C*
     C                   MOVEL     Q1USER        BSUSER
     C                   MOVEL     Q1PGM         BSPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSDT
     C*
     C                   MOVEL     DSTIME        BSTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*** BEG ADD *****  11/28/12  ********************************************
     C     SCSEFL        IFEQ      'S'
      *
     C     *IN49         IFEQ      *ON
     C                   SETOFF                                       49
     C                   SETON                                        90
     C                   ENDIF
     C                   ELSE
     C     *IN49         IFEQ      *OFF
     C                   SETON                                        49  90
     C                   ENDIF
      *
     C                   ENDIF
     C*** END ADD *****  11/28/12  ********************************************
     C*  CODE MUST BE GREATER THAN ZERO (IF IN ADD MODE)
     C*
     C   40
B02  CANN90SCBSCD        IFLE      *ZERO
     C                   SETON                                        20  90
E02  C                   ENDIF                                                  SCBSCD<=*ZERO
     C*
     C*  CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL(P)  SCBSCD        XXKEY
     C   40
B02  CANN90BSKY          CHAIN     PBS01                              60
     C   40
     CANN90
     CANN60              SETON                                        21  90
     C*
     C*  DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCDESC=*BLANKS
     C*
     C*  THE SELF REPORTING/ESTIMATING CODE MUST BE S/E.
     C*
     C  N90SCSEFL        IFNE      'S'
     C     SCSEFL        ANDNE     'E'
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCSEFL<>S/E
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C     *IN51         IFEQ      *ON
     C*
     C* If 'self-reporting' then frequency must be 'M', 'Q' or 'S'
     C  N90SCSEFL        IFEQ      'S'
     C     SCSRFQ        IFNE      'M'
     C     SCSRFQ        ANDNE     'Q'
     C     SCSRFQ        ANDNE     'S'
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCSRFQ<>'M'
     C*
     C                   ELSE
     C*
     C* If 'estimating' then frequency must be blank
     C     SCSRFQ        IFNE      *BLANK
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCSRFQ<>*BLANK
     C*
     C                   ENDIF                                                  SCSEFL='S'
     C*
     C                   ENDIF
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*  IF THE ROLLOVER CODE IS NOT EQUAL TO BLANKS THEN
     C*  THE ROLLOVER CODE MUST BE A VALID BILLING SCHEDULE CODE
     C*
     C  N90SCRLCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRLCD        XXKEY
     C*
     C     BSKY          CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     BSSTAT        OREQ      'I'
     C                   SETON                                        24  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCRLCD<>*BLANKS
     C*** BEGIN ADD ***  10/27/97  ************************************
     C*
     C*  PLAN TYPE MUST BE EITHER P,I,OR ' '.
     C*
     C***  Begin Chg  ***  02/22/10  **************************************
     C****BEG*ADD ***** 12/30/09 **************************************
     C**********           SETOF                     69
     C**********
     C********** WCFL43    IFNE 'Y'
     C****END*ADD ***** 12/30/09 **************************************
     C**N90***** SCLPGM    IFNE *BLANKS
     C**********
     C********** SCLPGM    IFNE 'P'
     C********** SCLPGM    ANDNE'I'
     C**********           MOVE *ON       *IN90
     C**********           MOVE *ON       *IN26
     C**********           ENDIF                           SCLPGM IFNE
     C****BEG*ADD ***** 12/30/09 **************************************
     C**********           ENDIF                           SCLPGM IFNE
     C**********           ENDIF                           WCFL43 IFNE
      **********
     C***PLAN*TYPE MUST BE EITHER P,I,L,OR ' '.
     C********** WCFL43    IFEQ 'Y'
     C**N90***** SCLPGM    IFNE *BLANKS
     C**********
     C********** SCLPGM    IFNE 'P'
     C********** SCLPGM    ANDNE'I'
     C********** SCLPGM    ANDNE'L'
     C**********           MOVE *ON       *IN90
     C**********           MOVE *ON       *IN69
     C**********           ENDIF                           SCLPGM IFNE
     C**********           ENDIF                           WCFL43 = 'Y'
     C****END*ADD ***** 12/30/09 **************************************
     C  N90SCLPGM        IFNE      *BLANKS
     C*
     C  N90              SELECT
     C****BEG*CHG ***** 11/23/10 **************************************
     C*******    WCFL11    WHEQ 'Y'
     C*******    WCFL43    ANDEQ'Y'
     C*******    SCLPGM    IFNE 'P'
     C*******    SCLPGM    ANDNE'I'
     C*******    SCLPGM    ANDNE'L'
     C*******              SETON                     69  90
     C*******              ENDIF
     C*******    WCFL11    WHEQ 'Y'
     C*******    SCLPGM    IFNE 'P'
     C*******    SCLPGM    ANDNE'I'
     C*******              SETON                     69  90
     C*******              ENDIF
     C*******    WCFL43    WHEQ 'Y'
     C*******    SCLPGM    IFNE 'L'
     C*******              SETON                     69  90
     C*******              ENDIF
     C*******              ENDSL
     C*
     C     WCFL11        WHENEQ    'Y'
     C     WCFL43        ANDEQ     'Y'
     C     CLTID         IFEQ      '114'
     C     SCLPGM        ANDNE     'P'
     C     SCLPGM        ANDNE     'I'
     C     SCLPGM        ANDNE     'L'
     C     SCLPGM        ANDNE     'R'
     C                   SETON                                        69  90
     C                   ELSE
     C     CLTID         IFNE      '114'
     C     SCLPGM        ANDNE     'P'
     C     SCLPGM        ANDNE     'I'
     C     SCLPGM        ANDNE     'L'
     C                   SETON                                        69  90
     C                   ENDIF
     C                   ENDIF
     C*
     C     WCFL11        WHENEQ    'Y'
     C     CLTID         IFEQ      '114'
     C     SCLPGM        ANDNE     'P'
     C     SCLPGM        ANDNE     'I'
     C     SCLPGM        ANDNE     'R'
     C                   SETON                                        69  90
     C                   ELSE
     C     CLTID         IFNE      '114'
     C     SCLPGM        ANDNE     'P'
     C     SCLPGM        ANDNE     'I'
     C                   SETON                                        69  90
     C                   ENDIF
     C                   ENDIF
     C*
     C     WCFL43        WHENEQ    'Y'
     C     CLTID         IFEQ      '114'
     C     SCLPGM        ANDNE     'L'
     C                   SETON                                        69  90
     C                   ELSE
     C     CLTID         IFNE      '114'
     C     SCLPGM        ANDNE     'L'
     C                   SETON                                        69  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C****END*CHG ***** 11/23/10 **************************************
     C***  End   Chg  ***  02/22/10  **************************************
     C*
      *** BEG ADD **** 01/11/07 ******************************************
      *** BEG DEL ***** 04/07/09 *****************************************
     C*********  SCSEFL    IFNE 'E'
     C*********            SETON                     38  90
     C*********            ENDIF                           SCSEFL IFEQ
      *** END DEL ***** 04/07/09 *****************************************
      *** END ADD **** 01/11/07 ******************************************
     C                   ENDIF                                                  SCLPGM IFNE
     C*
     C*** BEG ADD ***** 12/30/09 **************************************
     C***PLAN TYPE VALUE CAN BE CHANGED TO A NON-BLANK VALUE ONLY IF
     C***NO POLICIES ARE ASSIGNED TO THE BILLING SCHEDULE CODE
     C*
     C                   SETOFF                                       70
     C*** BEGIN CHG *** 04/15/10 **************************************
     C**N90***** SCLPGM    IFNE *BLANKS
     C********** SCLPGM    ANDNEBSLPGM
     C  N90SCLPGM        IFNE      BSLPGM
     C*** END   CHG *** 04/15/10 **************************************
     C                   MOVEL     SCBSCD        XXBSCD
     C     BSKY1         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN70
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD ***** 12/30/09 **************************************
     C*
     C*** END ADD *****  10/27/97  ************************************
     C*
     C*  IF SELF REPORTING CODE IS 'E' TOTAL PERCENTAGES
     C*
     C*** BEG CHG *****  02/18/03  ************************************
     C**N90******SCSEFL    IFEQ 'E'
     C  N90
     CANN43
     CANN45SCSEFL        IFEQ      'E'
     C*** END CHG *****  02/18/03  ************************************
     C*
     C                   XFOOT     SBS           TTLPCT            9 6
     C*
     C     TTLPCT        IFNE      100
     C                   SETON                                        25  90
     C                   ENDIF                                                  TTLPCT<>100
     C*
     C                   ENDIF                                                  SCSEFL=E
      *** BEG ADD **** 02/28/05 ******************************************
      *
      * IF SELF-REPORTING, DEPOSIT PERIODS ARE MUTUALLY EXCLUSIVE
      *
     C  N90SCSEFL        IFEQ      'S'
     C     SCBS01        ANDNE     *ZERO
     C     SCBS02        ANDNE     *ZERO
     C                   SETON                                        71  90
     C                   ENDIF
      *
      * CONDITION SECOND SCREEN FOR ACH = *ON, VENDOR = *ON, AND SELF-REPORTING
      *
     C  N90*IN45         IFEQ      *ON
     C     *IN43         OREQ      *ON
     C*** BEGIN ADD ***  06/06/08  ****************************************
     C     WCFL38        OREQ      'Y'
     C*** END ADD *****  06/06/08  ****************************************
     C     SCSEFL        IFEQ      'S'
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C                   MOVE      *OFF          *IN44
     C                   ENDIF
     C                   ENDIF                                                  *IN45 = *ON
      *** END ADD **** 02/28/05 ******************************************
     C*
      *** Beg Add *****  02/08/13  ***************************************
      /free
       //*** If Self reporting/estimating flag not 'E' then both estimating deposit
       //*** fields should be zero.
       If scSeFl <> 'E' and (scEstDep1 <> 0 or scEstDep2 <> 0);
           errEstDep3 = *On;
           *in90 = *On;
           LeaveSr;
       Endif;

       //*** If Estimating  Deposit fields should not be less than 0
       If  scSefl = 'E' and (scEstDep1 < 0 or scEstDep2 < 0);
           errEstDep1 = *On;
           *In90 = *On;
           LeaveSr;
       Endif;

       //** IF Estimating, Deposit periods are mutually exclusive
       If  scSeFl = 'E' and  scEstDep1 <> 0 and scEstDep2 <> 0;
           errEstDep2 = *On;
           *In90 = *On;
       Endif;
      /end-free
      *** End Add *****  02/08/13  ***************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C     *IN51         IFEQ      *ON
     C*
     C* Special S/R processing flag (Y/ )
     C*
     C  N90SCMSFL        IFNE      'Y'
     C     SCMSFL        ANDNE     *BLANK
     C                   SETON                                        90  71
     C                   ENDIF                                                  BSMSFL
     C*
     C* If S/R Frequency is 'Quarterly' or 'Semi-annual' then
     C* 'Special S/R processing' must be 'Y'
     C  N90SCSRFQ        IFEQ      'Q'
     C     SCSRFQ        OREQ      'S'
     C     SCMSFL        IFNE      'Y'
     C                   SETON                                        33  90
     C                   ENDIF
     C                   ENDIF
     C*
     C* If 'Special S/R processing' is 'Y'
     C     SCMSFL        IFEQ      'Y'
     C*
     C*   If frequency is 'M' then # of months must be 1 - 12
     C  N90SCSRFQ        IFEQ      'M'
     C     SC#MTH        IFLT      1
     C     SC#MTH        ORGT      12
     C                   SETON                                        29  90
     C                   ENDIF
     C                   ENDIF
     C*
     C*   If frequency is 'Q' then # of months must be 1 - 3
     C  N90SCSRFQ        IFEQ      'Q'
     C     SC#MTH        IFLT      1
     C     SC#MTH        ORGT      3
     C                   SETON                                        30  90
     C                   ENDIF
     C                   ENDIF
     C*
     C*   If frequency is 'S' then # of months must be 1
     C  N90SCSRFQ        IFEQ      'S'
     C     SC#MTH        ANDNE     1
     C                   SETON                                        31  90
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     SC#MTH        IFNE      0
     C     SCTMTH        ORNE      0
     C                   SETON                                        32  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  BSMSFL='Y'
     C*
     C* month deposit transferred must be (1 - 12 )
     C*    if Special processing flag = Y
     C*
     C  N90SCMSFL        IFEQ      'Y'
      *
     C     SCTMTH        IFLT      1
     C     SCTMTH        ORGT      12
     C                   SETON                                        90  73
     C                   ENDIF                                                  SCTMTH
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZERO         SCTMTH
     C*
     C                   ENDIF                                                  BSMSFL
     C*
     C                   ENDIF
     C***  End   Add  ***  06/24/05  **************************************
     C*** BEGIN ADD ***  03/28/08  ********************************************
      *
     C  N90SCONLI        IFNE      'Y'
     C     SCONLI        ANDNE     *BLANK
     C                   SETON                                        47  90
     C                   ENDIF
      *
     C  N90SCSEFL        IFEQ      'E'
     C     SCONLI        ANDNE     *BLANK
     C                   SETON                                        48  90
     C                   ENDIF
     C*** END ADD *****  03/28/08  ********************************************
     C*** BEG ADD **** 04/11/03 ************************************************
      *
      * CONDITION DISPLAY IF ACH PROCESSING IS ON AND VENDOR PROCESSING IF OFF
      *
     C     *IN45         IFEQ      *ON
     C     *IN43         ANDNE     *ON
      *
     C     SCSEFL        IFEQ      'S'
     C                   SETON                                        44
     C                   Z-ADD     1             #OFSCR
     C                   ELSE
     C                   SETOFF                                       44
     C                   Z-ADD     2             #OFSCR
     C                   ENDIF
      *
      *  POP UP THE PERIOD FIELDS IF SELF REPORTING FLAG IS CHANGED
      *
      *** BEG CHG **** 02/28/05 ******************************************
     C*********  SCSEFL    IFNE SVSEFL
     C  N90SCSEFL        IFNE      SVSEFL
      *** END CHG **** 02/28/05 ******************************************
     C                   MOVEL     SCSEFL        SVSEFL
      *** BEG ADD **** 02/28/05 ******************************************
     C     SCSEFL        IFEQ      'S'
      *** END ADD **** 02/28/05 ******************************************
     C                   SETON                                        90
      *** BEG ADD **** 02/28/05 ******************************************
     C                   ENDIF
      *** END ADD **** 02/28/05 ******************************************
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN45 = *ON
     C*** END ADD **** 04/11/03 ************************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90BSDT          IFGT      SAVEDT
     C     BSDT          OREQ      SAVEDT
     C     BSTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C     ERRCK2        BEGSR
     C*** BEG ADD **** 02/18/03 **************************************
     C*
     C* IF SELF-REPORTING, FREQUENCY MUST BE MONTHLY OR WEEKLY
     C*
     C     *IN44         IFEQ      *ON
     C     SCFREQ        ANDNE     'M'
     C     SCFREQ        ANDNE     'W'
     C                   SETON                                        71  90
     C                   ENDIF
      *
     C*** BEG ADD **** 04/11/03 **************************************
     C*
     C* IF ACH ENABLED AND ESTIMATING, FREQ MUST BE M,T, OR W.
     C*
     C  N90*IN45         IFEQ      *ON
     C     *IN44         ANDEQ     *OFF
      *
     C     SCEFRQ        IFNE      'M'
     C     SCEFRQ        ANDNE     'T'
     C     SCEFRQ        ANDNE     'W'
     C                   SETON                                        80  90
     C                   ENDIF                                                  SCEFRQ = 'M'
      *
     C*** BEG ADD **** 03/22/06 **************************************
      *
     C  N90*IN34         IFEQ      *ON
      *
      * Billing on the 15th Flag must be either 'Y' or blanks
      *
     C  N90SCON15        IFNE      'Y'
     C     SCON15        ANDNE     *BLANKS
     C                   SETON                                        35  90           -WC31401
     C                   ENDIF
      *
      * If billing on the 15th, then Frequency MUST BE = (M)ontly
      *
     C  N90SCON15        IFEQ      'Y'
     C     SCEFRQ        ANDNE     'M'
     C                   SETON                                        37  90       -WC40041
     C                   ENDIF                                                  SCON15 = 'Y'
      *
     C                   ENDIF                                                  CLTID
      *
     C*** END ADD **** 03/22/06 **************************************
      *
     C                   ENDIF                                                  *IN45 = *ON
     C*** END ADD **** 04/11/03 **************************************
      *
      * IF SELF-REPORTING, VALIDATE DAY OF WEEK
      *
     C  N90*IN44         IFEQ      *ON
     C     SCFREQ        ANDEQ     'W'
     C     SCDAY         ANDNE     'MO'
     C     SCDAY         ANDNE     'TU'
     C     SCDAY         ANDNE     'WE'
     C     SCDAY         ANDNE     'TH'
     C     SCDAY         ANDNE     'FR'
     C     SCDAY         ANDNE     'SA'
     C     SCDAY         ANDNE     'SU'
     C                   SETON                                        72  90
     C                   ENDIF
      *
      * IF SELF-REPORTING, DAY MUST BE BLANK IF FREQUENCY IS MONTHLY
      *
     C  N90*IN44         IFEQ      *ON
     C     SCFREQ        ANDEQ     'M'
     C     SCDAY         ANDNE     *BLANK
     C                   SETON                                        73  90
     C                   ENDIF
      *** BEG ADD **** 04/11/03 ******************************************
      *
      * IF SELF-REPORTING, DEPOSIT PERIODS ARE MUTUALLY EXCLUSIVE
      *
     C  N90*IN44         IFEQ      *ON
     C     SCDEP1        ANDNE     *ZERO
     C     SCDEP2        ANDNE     *ZERO
     C                   SETON                                        77  90
     C                   ENDIF
      *** END ADD **** 04/11/03 ******************************************
      *
      * FOR ESTIMATING, VALIDATE DAY OF WEEK
      *
     C  N90*IN44         IFEQ      *OFF
     C     *IN45         ANDEQ     *ON
     C     SCEFRQ        ANDEQ     'W'
     C     SCEDAY        ANDNE     'MO'
     C     SCEDAY        ANDNE     'TU'
     C     SCEDAY        ANDNE     'WE'
     C     SCEDAY        ANDNE     'TH'
     C     SCEDAY        ANDNE     'FR'
     C                   SETON                                        75  90
     C                   ENDIF
     C*
     C*  IF ESTIMATING, TOTAL PERCENTAGES MUST EQUAL 100
     C*
     C  N90*IN44         IFEQ      *OFF
     C                   XFOOT     SBS           TTLPCT
      *
     C     *IN45         IFEQ      *OFF
     C     SCEDAY        ANDEQ     *BLANK
     C     TTLPCT        ANDNE     100
     C     *IN45         OREQ      *ON
     C     SCEFRQ        ANDEQ     'M'
     C     TTLPCT        ANDNE     100
     C                   SETON                                        76  90
     C                   ENDIF                                                  TTLPCT<>100
     C*
     C                   ENDIF                                                  *IN44 = *OFF
      *
      * IF ESTIMATING, DAY OF WEEK AND PERIOD BUCKETS ARE MUTUALLY EXCLUSIVE
      *
     C  N90*IN44         IFEQ      *OFF
     C     SCEDAY        ANDNE     *BLANKS
     C     TTLPCT        ANDNE     *ZERO
     C                   SETON                                        74  90
     C                   ENDIF
     C*** BEG ADD **** 04/11/03 **************************************
      *
      * IF ACH ENABLED AND ESTIMATING, FREQ AND BUCKETS ARE MUTUALLY INCLUSIVE
      * IF FREQ = M, AND MUTUALLY EXCLUSIVE OTHERWISE
      *
     C  N90*IN45         IFEQ      *ON
     C     *IN44         ANDEQ     *OFF
      *
     C     SCEFRQ        IFEQ      'M'
     C     TTLPCT        ANDEQ     *ZERO
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCEFRQ = 'M'
      *
     C     SCEFRQ        IFEQ      'T'
     C     TTLPCT        ANDNE     *ZERO
     C     SCEFRQ        OREQ      'W'
     C     TTLPCT        ANDNE     *ZERO
     C                   SETON                                        79  90
     C                   ENDIF                                                  SCEFRQ = 'T'
      *
     C     SCEFRQ        IFEQ      'M'
     C     SCEDAY        ANDNE     *BLANKS
     C     SCEFRQ        OREQ      'T'
     C     SCEDAY        ANDNE     *BLANKS
     C                   SETON                                        81  90
     C                   ENDIF                                                  SCEFRQ = 'M'
      *
      * BILLING STARTING PERIOD CANNOT BE *ZERO
      *
     C     SCDBSD        IFLE      *ZERO
     C                   SETON                                        82  90
     C                   ENDIF                                                  SCDBSD =  *ZERO
      *
     C                   ENDIF                                                  *IN45=*ON
     C*** END ADD **** 04/11/03 **************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90BSDT          IFGT      SAVEDT
     C     BSDT          OREQ      SAVEDT
     C     BSTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F1DT>SAVEDT
     C*** END ADD **** 02/18/03 **************************************
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVEL     'A'           BSSTAT
     C*
     C                   MOVEL     BSKEY         SCBSCD
     C                   MOVEL     BSDESC        SCDESC
     C                   MOVEL     BSSTAT        SCSTAT
     C                   MOVEL     BSSEFL        SCSEFL
     C*** BEG ADD *****  04/11/03  ************************************
     C                   MOVEL     BSSEFL        SVSEFL            1
     C*** END ADD *****  04/11/03  ************************************
     C                   MOVE      FBS           SBS
     C                   MOVEL     BSRLCD        SCRLCD
     C*** BEGIN ADD ***  10/27/97  ************************************
     C                   MOVE      BSLPGM        SCLPGM
     C*** END ADD *****  10/27/97  ************************************
     C*** BEGIN ADD ***  03/22/06  ************************************
     C     *IN34         IFEQ      *ON
     C                   MOVE      BSON15        SCON15
     C                   ENDIF
     C*** END ADD *****  03/22/06  ************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      BSMSFL        SCMSFL
     C                   Z-ADD     BS#MTH        SC#MTH
     C                   Z-ADD     BSTMTH        SCTMTH
     C                   MOVE      BSSRFQ        SCSRFQ
     C***  End   Add  ***  06/24/05  **************************************
     C*** BEG ADD **** 02/18/03 ************************************************
      *
      * CONDITION DISPLAY IF VENDOR PROCESSING IS ON
      *
     C     *IN43         IFEQ      *ON
      *
     C     SCSEFL        COMP      'S'                                    44
      *
     C  N44              MOVEL     BSDOWK        SCEDAY
     C   44              MOVEL     BSFREQ        SCFREQ
     C   44              MOVEL     BSDOWK        SCDAY
     C   44              MOVE      BSAB01        SCDEP1
     C   44              MOVE      BSAB02        SCDEP2
      *
     C                   ENDIF                                                  *IN43 = *ON
     C*** END ADD **** 02/18/03 ************************************************
     C*** BEG ADD **** 11/28/12 ************************************************
     C     *IN43         IFEQ      *OFF
     C     SCSEFL        IFEQ      'S'
     C                   SETOFF                                           49
     C                   ENDIF
     C                   ENDIF
     C*** END ADD **** 11/28/12 ************************************************
      *
     C*** BEG ADD **** 04/11/03 ************************************************
     C     *IN45         IFEQ      *ON
     C     SCSEFL        COMP      'S'                                    44
     C  N44              MOVEL     BSFREQ        SCEFRQ
     C  N44              MOVEL     BSDOWK        SCEDAY
     C                   Z-ADD     BSDBSD        SCDBSD
     C                   ENDIF                                                  *IN45 = *ON
     C*** END ADD **** 04/11/03 ************************************************
     C*** BEGIN ADD ***  03/28/08  ********************************************
     C                   MOVE      BSONLI        SCONLI
     C*** END ADD *****  03/28/08  ********************************************
      *** Beg Add *****  02/08/13  ********************************************
      /free
       scEstDep1 = BSesdp;
       scEstDep2 = BSesd2;
      /end-free
      *** End Add *****  02/08/13  ********************************************
      ***** Beg Add ***** 11/22/13 ********************************************
      /free
       scEfTf = BSeftf;
      /end-free
      ***** Beg Add ***** 11/22/13 ********************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         BSCO#
     C                   MOVEL     SCBSCD        BSKEY
     C                   MOVEL     SCDESC        BSDESC
     C                   MOVEL     SCSTAT        BSSTAT
     C                   MOVEL     SCSEFL        BSSEFL
     C                   MOVE      SBS           FBS
     C                   MOVEL     SCRLCD        BSRLCD
     C*** BEGIN ADD ***  10/27/97  ************************************
     C                   MOVE      SCLPGM        BSLPGM
     C*** END ADD *****  10/27/97  ************************************
     C*** BEGIN ADD ***  03/22/06  ************************************
     C     *IN34         IFEQ      *ON
     C                   MOVE      SCON15        BSON15
     C                   ENDIF
     C*** END ADD *****  03/22/06  ************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      SCMSFL        BSMSFL
     C                   Z-ADD     SC#MTH        BS#MTH
     C                   Z-ADD     SCTMTH        BSTMTH
     C                   MOVE      SCSRFQ        BSSRFQ
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*** BEG ADD **** 02/18/03 ************************************************
     C     *IN43         IFEQ      *ON
     C  N44              MOVEL     SCEDAY        BSDOWK
     C   44              MOVEL     SCFREQ        BSFREQ
     C   44              MOVEL     SCDAY         BSDOWK
     C   44              MOVE      SCDEP1        BSAB01
     C   44              MOVE      SCDEP2        BSAB02
     C                   ENDIF                                                  *IN43 = *ON
     C*** END ADD **** 02/18/03 ************************************************
     C*
     C*** BEG ADD **** 04/11/03 ************************************************
     C     *IN45         IFEQ      *ON
     C  N44              MOVEL     SCEFRQ        BSFREQ
     C  N44              MOVEL     SCEDAY        BSDOWK
     C                   Z-ADD     SCDBSD        BSDBSD
     C                   ENDIF                                                  *IN45 = *ON
     C*** END ADD **** 04/11/03 ************************************************
     C*** BEGIN ADD ***  03/28/08  ********************************************
     C                   MOVE      SCONLI        BSONLI
     C     BSSEFL        IFNE      'S'
     C                   MOVE      *BLANK        BSONLI
     C                   ENDIF
     C*** END ADD *****  03/28/08  ********************************************
      *** Beg Add *****  02/08/13  ********************************************
      /free
       BSesdp = scEstDep1;
       BSesd2 = scEstDep2;
      /end-free
      *** End Add *****  02/08/13  ********************************************
      ***** Beg Add ***** 11/22/13 ********************************************
      /free
       BSeftf = scEfTf;
      /end-free
      ***** Beg Add ***** 11/22/13 ********************************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C     PGMEND        BEGSR
      *** BEG ADD **** 04/11/03 **************************************
     C                   SETON                                        LR
      *** END ADD **** 04/11/03 **************************************
     C                   ENDSR                                                  PGMEND
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      *OFF          *IN51
3/22 C                   MOVE      *OFF          *IN34
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C                   MOVE      *ON           *IN51
      ****  BEGIN  ADD 03/22/06 ***************************************
     C     CLTID         WHENEQ    '129'
     C                   MOVE      *ON           *IN34
      ****  BEGIN  ADD 03/22/06 ***************************************
     C                   OTHER
     C                   ENDSL
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
      **** BEGIN ADD **** 10/19/99 *********************************************
     C                   MOVEL     'WC'          XXSYK
      *
     C     SYKY          CHAIN     SWC01                              60
      *
     C***  Begin Chg  ***  02/22/10  **************************************
     C********** WCFL11    IFNE 'Y'
     C**********           SETON                     42
     C**********           ELSE
     C**********           SETOF                     42
     C**********           ENDIF                           WCFL11 IFNE 'Y'
      *****END** ADD **** 10/19/99 *********************************************
      **********
      ****BEG*ADD ***** 12/30/09 **************************************
     C********** WCFL43    IFNE 'Y'
     C**********           SETON                     49
     C**********           ELSE
     C**********           SETOF                     49
     C**********           ENDIF                           WCFL43 IFNE 'Y'
      ****END*ADD ***** 12/30/09 **************************************
     C                   SETON                                        42
     C                   MOVEL     DOTS          XHLPGM
     C                   SELECT
     C****BEG*CHG ***** 11/23/10 **************************************
     C********   WCFL11    WHEQ 'Y'
     C********   WCFL43    ANDEQ'Y'
     C********             SETOF                     42
     C********             MOVE XHPIL     XHLPGM
     C********             MOVELXMPIL     XMLPGM
     C********   WCFL11    WHEQ 'Y'
     C********             SETOF                     42
     C********             MOVE XHPI      XHLPGM
     C********             MOVELXMPI      XMLPGM
     C*
     C     WCFL11        WHENEQ    'Y'
     C     WCFL43        ANDEQ     'Y'
     C     CLTID         IFEQ      '114'
     C                   SETOFF                                       42
     C                   MOVE      XHPILR        XHLPGM
     C                   MOVEL     XMPILR        XMLPGM
     C                   ELSE
     C                   SETOFF                                       42
     C                   MOVE      XHPIL         XHLPGM
     C                   MOVEL     XMPIL         XMLPGM
     C                   ENDIF
     C     WCFL11        WHENEQ    'Y'
     C     CLTID         IFEQ      '114'
     C                   SETOFF                                       42
     C                   MOVE      XHPIR         XHLPGM
     C                   MOVEL     XMPIR         XMLPGM
     C                   ELSE
     C                   SETOFF                                       42
     C                   MOVE      XHPI          XHLPGM
     C                   MOVEL     XMPI          XMLPGM
     C                   ENDIF
     C****END*CHG ***** 11/23/10 **************************************
     C     WCFL43        WHENEQ    'Y'
     C                   SETOFF                                       42
     C                   MOVE      XHL           XHLPGM
     C                   MOVEL     XML           XMLPGM
     C                   ENDSL
     C***  End   Chg  ***  02/22/10  **************************************
      *
      *** BEG CHG ***** 05/07/12 ***********************************************
      ***********
      *****BEG***ADD **** 02/18/03 *********************************************
     C***********WCFL23    IFEQ 'Y'
     C***********          SETON                     43
     C***********          Z-ADD2         #OFSCR  10
     C***********          ELSE
     C***********          SETOF                     43
     C***********          Z-ADD1         #OFSCR
     C***********          ENDIF                           WCFL23 IFEQ
      *****END***ADD **** 02/18/03 *********************************************
      ***********
      *****BEG***ADD **** 04/11/03 *********************************************
     C***********WCFL24    IFEQ 'Y'
     C***********          SETON                     45
     C***********          Z-ADD2         #OFSCR
     C***********          ELSE
     C***********          SETOF                     45
      ***********
     C***********WCFL23    IFNE 'Y'
     C***********          Z-ADD1         #OFSCR
     C***********          ENDIF                           WCFL23 IFNE
      ***********
     C***********          ENDIF                           WCFL24 IFEQ
      *****END***ADD **** 04/11/03 *********************************************
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     1             #OFSCR            1 0
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C     WCFL23        IFEQ      'Y'
     c*** begin add ***  03/19/13  **********************************************
     C                   Z-ADD     2             #OFSCR
     c*** end add *****  03/19/13  **********************************************
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C     WCFL24        IFEQ      'Y'
     c*** begin add ***  03/19/13  **********************************************
     C                   Z-ADD     2             #OFSCR
     c*** end add *****  03/19/13  **********************************************
     C                   MOVE      *ON           *IN45
     C                   ENDIF
     C                   OTHER
     C     WCFL23        IFEQ      'Y'
     C                   MOVE      *ON           *IN43
     C                   Z-ADD     2             #OFSCR
     C     BSSEFL        IFEQ      'S'
     C                   MOVE      *ON           *IN44
     C                   ENDIF
     C                   ENDIF
     C     WCFL24        IFEQ      'Y'
     C                   Z-ADD     2             #OFSCR
     C                   MOVE      *ON           *IN45
     C                   ENDIF
     C     WCFL23        IFEQ      *BLANK
     C     BSSEFL        ANDEQ     'S'
     C                   Z-ADD     1             #OFSCR
     C                   ENDIF
     C                   ENDSL
      *** END CHG ***** 05/07/12 ***********************************************
     C*** BEGIN ADD ***  03/28/08  ********************************************
     C                   MOVE      *OFF          *IN46
     C     WCFL38        IFEQ      'Y'
     C                   MOVE      *ON           *IN46
     C                   ENDIF
     C*** END ADD *****  03/28/08  ********************************************
     C*** BEG ADD *****  11/28/12  ********************************************
     C     SCSEFL        IFEQ      'S'
     C                   SETOFF                                       49
     C                   ELSE
     C                   SETON                                        49
     C                   ENDIF
     C*** END ADD *****  11/28/12  ********************************************
      /Free
       // Begin Add **** 09/16/15 *********************************************
          If WCFL24 = 'Y' or WCFL38 = 'Y';
             *IN41 = *ON;
          Else;
             *IN41 = *OFF;
          Endif;
       // End   Add **** 09/16/15 *********************************************
      /End-Free
      *
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        BSUSER
     C                   MOVEL     Q1PGM         BSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSDT
     C                   MOVEL     DSTIME        BSTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        BSIUSR
     C                   MOVEL     Q1USER        BSUSER
     C                   MOVEL     Q1PGM         BSIPGM
     C                   MOVEL     Q1PGM         BSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSIDT
     C                   MOVEL     SRDTO         BSDT
     C                   MOVEL     DSTIME        BSITM
     C                   MOVEL     DSTIME        BSTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTBSP
     C*
     C                   ENDSR                                                  WRITER
