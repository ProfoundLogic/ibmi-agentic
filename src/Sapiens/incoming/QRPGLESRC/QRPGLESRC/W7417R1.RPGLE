     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
      *
      * W7417R1 - WCPOLS Submission Process
     H*****************************************************************
     H*    PROGRAM: W7417R1                                           *
     H*    DESCRIPTION: PROMPT FOR WCPOLS DATA SUBMISSION             *
     H*                     TO INITIALIZE TAPE                        *
     H*                       NOTE: INITIALIZE VALUES GOTTEN FROM     *
     H*                             SYSTEM CONTROL FILE: SDRPF IN SMNU*
     H*                             (VOL ID HARD-CODED W/ 'XXX')      *
     H*    CREATED BY:  M.GELDERT #02       DATE:  04/09/96           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/10/22 C:340 Project: WC2843                                *
     H*                -Use Carrier Specific data provide & fein      *
     H*                                                               *
     H* 03/30/22 C:062 Ticket: 101568(102)                            *
     H*                Clear data area if holds value of /holsomba/   *
     H*                                                               *
     H* 02/04/21 C:062 Project: WC2761                                *
     H*                -Expand File and Directory Parameters.         *
     H*                -Create file member in led interactively,      *
     H*                -Prevent interference with other jobs.         *
     H*                                                               *
     H* 02/04/19 C:180 Project: 150WDV                                *
     H*                Remove client-id condition for PEEP prompt.    *
     H*                                                               *
     H* 01/16/18 C:65  Project: 150WAD                                *
     H*                Clear SCER to ensure rating dropdown properly  *
     H*                displays the All Records option.               *
     H*                                                               *
     H* 02/03/17 C:65  Project: SY306                                 *
     H*                Hide the file type entry since the only option *
     H*                is File and default it to 'F'.                 *
     H*                                                               *
     H* 12/03/15 C:65  Project: 45WKP                                 *
     H*                Add checkbox to allow selection of pre-2016    *
     H*                data for LCTA which uses old Carrier #.        *
     H*                                                               *
     H*  01/06/15  C:33  Project: 129WTT                              *
     H*                  Change Experience Rating to a drop down and  *
     H*                   allow access by CLT129.                     *
     H*                                                               *
     H*  05/02/14  C:290 Support:OP114   85501                        *
     H*                  Changed the set focus(screen property) value *
     H*                  for Experience rating and Processed through  *
     H*                  PEEP fields and handled them in the program. *
     H*                  Contains Display file changes also.          *
     H*                                                               *
     H*  11/23/10  C:189 Project 139WPQ                               *
     H*                  Use the W7944R screen for group selection    *
     H*                  which have the capability of Subset on Carr# *
     H*                                                               *
     H*  05/11/09  C:61  Project: 93WHW
     H*                  Condition entry "Exerience rating" so only
     H*                  appears for clt093.
     H*                                                               *
     H*  05/20/08  C:65   Project:  114WAK                            *
     H*                   ENSURE PENDING SUBMISSION LIST ONLY INCLUDES*
     H*                   RECORDS WHICH ARE GOING TO SUBMIT FOR THE   *
     H*                   SELECTED STATE/BUREAU.  ALSO, ZERO OUT SCZIP*
     H*                   AT START OF PROGRAM TO PREVENT DECIMAL DATA *
     H*                   ERRORS.                                     *
     H*                                                               *
     H* 04/04/08 C:189 114WQC                                         *
     H*                POPULATING EXTRA FIELDS FOR WCPOLS             *
     H*                                                               *
     H* 06/11/07 C:61  PROJECT WC1504                                 *
     H*                Default SCR03 values.                          *
     H*                                                               *
     H*  02/08/07  C:65   Project:  114WLK                            *
     H*                   REMOVE THE RESTRICTION ON ALLOWING          *
     H*                   MULTIPLE GROUPS FOR NCCI SUBMISSIONS.       *
     H*                                                               *
     H*  10/19/06  C:65   Project:  114WLK/Supp# 53886                *
     H*                   DISPLAY GROUP, CARRIER #, OR VARIOUS        *
     H*                   CARRIER IDS FIELD ON TOTALS SCREEN.         *
     H*                                                               *
     H* 04/03/06 C:168 PROJECT: 114WLK                                *
     H*                - Convined the code from this program in client*
     H*                  93 and 114                                   *
     H*                - Added code to retrieve the client id since   *
     H*                  we must condition code for specific clients. **
     H*                  *IN93 allows to show code for client 93      **
     H*                                                               *
     H*                PROJECT: 114WIY                                *
     H*                DISPLAY RECORD COUNTS FOR EACH RECORD TYPE.    *
     H*                                                               *
     H*                PROJECT 93WEW                                  *
     H*                ADD GROUP SELECTION SCREEN IF THE GROUP IS     *
     H*                LEFT EMPTY.  IF THE GROUP AND THE STATE        *
     H*                RECEIVING DATA IS LEFT EMPTY FLAG AN ERROR.    *
     H*                THE GROUP SELECTION PROCESS RUNS OVER WXPGP    *
     H*                WHICH IS COPIED INTO QTEMP AT START AND        *
     H*                REMOVED AT THE END.                            *
     H*                                                               *
     H*                SUPPORT 37316                                  *
     H*                CHANGE LOGIC FOR SCREEN LOAD                   *
     H*                                                               *
     H*                PROJECT WC093 SEQ: 35092                       *
     H*                FIX ERROR ON SUBFILE LOAD WHEN NOT AT END OF   *
     H*                READE ON WMSHL9 AND SUBFILE EMPTY             *
     H*                                                               *
     H*                PROJECT 93WBZ                                  *
     H*                ADDED EXPERIENCE RATING SELECTION              *
     H*                                                               *
     H* 02/14/05 C:65  PROJECT: 114WIY                                *
     H*                DISPLAY RECORD COUNTS FOR EACH RECORD TYPE.    *
     H*                                                               *
     H* 02/01/05 C:65  PROJECT: WC1272                                *
     H*                DISPLAY THE ADDRESS FIELDS FOR CONTACT SCREEN. *
     H*                                                               *
     H* 01/18/05 C:65  PROJECT: WC1253                                *
     H*                DISPLAY THE CONTACT INFORMATION SCREEN FOR     *
     H*                ANY NON-NCCI SUBMISSION TO ALLOW CREATION OF   *
     H*                THE TRANSMITTAL HEADER RECORD.                 *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H* 01/16/04 C:65  PROJECT 114WAK/SUPP#40711                      *
     H*                DISPLAY THE CONTACT INFORMATION SCREEN FOR     *
     H*                NJ AS WELL.                                    *
     H*                                                               *
     H* 12/05/02 C:132 PROJECT WC577                                  *
     H*                ADD A SEARCH ON THE STATE ENTERED IN SCREEN    *
     H*                ONE ON COMMAND KEY 11.                         *
     H*                                                               *
     H* 10/03/02 C:65  PROJECT 114WAK                                 *
     H*                ADD A NEW SCREEN FOR CONTACT INFORMATION.      *
     H*                THIS SCREEN WILL APPEAR IF THE SUBMISSION      *
     H*                STATE IS PA OR DE.  MOVE THE CALL FOR THE      *
     H*                TAPE SCREEN TO ALLOW FOR PROPER PROGRAM FLOW.  *
     H*                                                               *
     H* 04/30/01 C:65  PROJECT 114WAK                                 *
     H*                ADD SCREEN PROMPT FOR STATE TO RECEIVE DATA.   *
     H*                ANY STATE ENTERED CANNOT BE A NCCI STATE. IF   *
     H*                NCCI STATES ARE DESIRED THEN THE FIELD MUST    *
     H*                REMAIN BLANK.                                  *
     H*                                                               *
     H* 01/05/01 C:61  PROJECT WC1011                                 *
     H*              - Remove "Method of transfer" fields.            *
     H*              - Add prompt fields:                             *
     H*                 "Type of output (F/T)"                        *
     H*                 "Output file library"                         *
     H*                 "Output file name"                            *
     H*              - Chg to use "F10=Search"                        *
     H*              - If 'F'ile is chosen for "Type of output" then  *
     H*                skip SCR02.                                    *
     H*                                                               *
     H* 08/02/96 C:64 PROJECT: WC525                                  *
     H*               CHANGED CALL TO F0001C TO W0001C TO VALIDATE    *
     H*               THE TAPE DEVICE ENTERED.                        *
     H*                                                               *
     H* 08/21/96 C:35 PROJECT: WC525                                  *
     H*               CHANGED TO DISPLAY TRANSACTION DATE             *
     H*                                                               *
     H* 09/03/96 C:35 PROJECT: WC525                                  *
     H*               CHANGED TO CALL W0006R FOR USER PARM CHG.  ALSO *
     H*               MADE VARIOUS CHANGES TO THE WAY FUNCTION KEY    *
     H*               PROGRAM CALLS WERE PROCESSED.                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  20     INVALID INITIALIZATION VALUE                          *
     H*  21     SYSTEM CONTROL FILE SDRPF IS IN ERROR                 *
     H*  22     INVALID DEVICE NAME ENTERED                           *
     H*  23     VALUE CANNOT BE EQUAL TO BLANKS.  (DEVICE)            *
     H*  24     VALUE CANNOT BE EQUAL TO BLANKS.  (VOLUME)            *
     H*  25     VALUE CANNOT BE EQUAL TO BLANKS.  (DENSITY)           *
     H*  60     GENERAL CHAIN                                         *
     H* SCR04:                                                        *
     H*                                                               *
     H*  01    SFLDSP, SFLDSPCTL                                      *
     H*  02    SFLINZ                                                 *
     H*  03    SFLDLT                                                 *
     H*  04    SFLCLR                                                 *
     H*  05    SFLEND                                                 *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY SELECT FIELDS   *
     H*  19    SFLNXTCHG                                              *
     H*  20    HIGHLIGHT RECORD IN ERROR                              *
     H*  21    INVALID OPTION SELECTED                                *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  48    AT LEAST ONE RECORD DELETION REQUESTED                 *
     H*  49    AT LEAST ONE NON-BLANK OPTION ENTERED                  *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  51    SCM0516-DOCUMENT NOT FOUND IN SPECIFIED FOLDER         *
     H*  52    SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER           *
     H*  53    SCM0518-USER NOT ENROLLED IN OFFICE 400                *
     H*  54    SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT    *
     H*  70    SHOW PENDING TRANSMISSIONS                             *
     H* N70    SHOW COMPLETED TRANSMISSIONS                           *
     H*  80    REPRINT ENABLED - DARTS AND OFFICE ARE INSTALLED       *
     H*  85    DISPLAY A SINGLE EMPLOYER ONLY                         *
     H*                                                               *
     H*****************************************************************
     FW7417F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR04S:RRN2)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMSHL9    IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     FWXPGL     IF   E           K DISK    USROPN
     FWMSHP     O  A E             DISK    USROPN
     F                                     RENAME(PSH01:PSH02)
     FWMSHL17   IF   E           K DISK
     F                                     RENAME(PSH01:PSH03)
     FWDF2L     IF   E           K DISK
      *
     F* WCSTAT Transmittal Information Default
     **** Beg Add - 03/10/22  **************************************************
     F*** WMTML     IF   E           K DISK
     FWMTML1    IF   E           K DISK
     **** End Add - 03/10/22  **************************************************
      *
      *

     D TC              S              1    DIM(4) CTDATA PERRCD(1)              TYPE CODE/DESC
     D TD              S              9    DIM(4) ALT(TC)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXPHN                  1     10  0
     D  SCPHNA                 1      3  0
     D  SCPHN1                 4      6  0
     D  SCPHN2                 7     10  0
     D                 DS
     D  XXFAX                  1     10  0
     D  SCFAXA                 1      3  0
     D  SCFAX1                 4      6  0
     D  SCFAX2                 7     10  0
     D                 DS
     D  SCZIP                  1      9  0
     D  SCZIP1                 1      5  0
     D  SCZIP2                 6      9  0
     D                 DS
     D  XXADR                  1    101
     D  XXSTRT                 1     60
     D  XXCITY                61     90
     D  XXST                  91     92
     D  XXZIP1                93     97
     D  XXZIP2                98    101
     D                 DS
     D  SST                    1     30
     D                                     DIM(15)
     D  SHST01                 1      2
     D  SHST02                 3      4
     D  SHST03                 5      6
     D  SHST04                 7      8
     D  SHST05                 9     10
     D  SHST06                11     12
     D  SHST07                13     14
     D  SHST08                15     16
     D  SHST09                17     18
     D  SHST10                19     20
     D  SHST11                21     22
     D  SHST12                23     24
     D  SHST13                25     26
     D  SHST14                27     28
     D  SHST15                29     30
     D                 DS
     D  SSS                    1     15
     D                                     DIM(15)
     D  SHSS01                 1      1
     D  SHSS02                 2      2
     D  SHSS03                 3      3
     D  SHSS04                 4      4
     D  SHSS05                 5      5
     D  SHSS06                 6      6
     D  SHSS07                 7      7
     D  SHSS08                 8      8
     D  SHSS09                 9      9
     D  SHSS10                10     10
     D  SHSS11                11     11
     D  SHSS12                12     12
     D  SHSS13                13     13
     D  SHSS14                14     14
     D  SHSS15                15     15
     D XXPPGM          C                   CONST('W7417R1B')
      //***  Begin Change *** 02/04/19 ********************************
        //***  Begin Add  ***  01/06/15  *******q********************* ***
        //DisUsePEEP        s              1n   inz(*off)
        //***  End   Add  ***  01/06/15  ***************************** ***
     DisUsePEEP        s              1n   inz(*on)
      //***  End   Change *** 02/04/19 ********************************
      //*** Begin Add *** 02/04/21 ************************************
     D ##ifsName       s            256a   inz(' ')
     D ##Error         s               n   inz(*Off)
     D #1Err           s              1a   inz('0')
     **** Beg Add - 03/10/22  **************************************************
     D curCarrier      S                   Like(F2Car#)
     **** End Add - 03/10/22  **************************************************

     d storedDir      uds           250    dtaara(W7417R1DTA)
     d directory               1    250
      //*** End   Add *** 02/04/21 ************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      * WCSTAT Transmittal Information Default
      *

     **** Beg Del - 03/10/22  **************************************************
     C***  TMKY          KLIST
     C***                KFLD                    SCCO#
     C***                KFLD                    KKFLAG            1
     **** End Del - 03/10/22  **************************************************
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the FUND Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C*
     C* Key List for the FUND SELECTION FILE
     C*
     C     PGKY          KLIST
     C                   KFLD                    SCCO#
     C*
     C* Key List for the User Parameter Master File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     SHKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXSTAT            1
     C*
     C     SHKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVSTAT            1
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVTYPE
     C                   KFLD                    SVUNID
     C*
     C     SHKY3         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C*
     C*  KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C     F2KY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    EMEMP#        SVEMP#
     C     *LIKE         DEFINE    EMEMP#        SVEMP1
     C     *LIKE         DEFINE    EMDT          SVTRDT
     C     *LIKE         DEFINE    EMDT          SVTRD1
     C     *LIKE         DEFINE    EMDT          XXTRDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQRTCD            1
      *
      /Free
       scPrDs = 'WCPOLS Data Transmission Prompt';
      /End-Free
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     c*** begin add ***  02/03/17  ************************************************
     C                   Eval      scotyp = 'F'
     c*** end add *****  02/03/17  ************************************************
     c*** begin add ***  01/16/18  ************************************************
     C                   Eval      scer = ' '
     c*** end add *****  01/16/18  ************************************************
      *
      * Checks to se the client we are using in order to display more
      * options in the different screens
      *
      *** begin add ***  12/03/15  *************************************************
     C                   Eval      clt045 = *off
     C
     C                   If        xxClt# = '045'
     C                   Eval      clt045 = *on
     C                   Endif
      *** end add *****  12/03/15  *************************************************
      //***  Begin Del  ***  01/06/15  ***********************************
     C**********         SETOFF                                       93
     C*****XXCLT#        IFEQ      '093'
     C**********         SETON                                        93
     C**********         ELSE
     C**********         SETOFF                                       93
     C**********         ENDIF
      //***  End   Del  ***  01/06/15  ***********************************
      *
      /Free
      //***  Begin Change *** 02/04/19 ********************************
        //C***                SETOFF                                     67
        //C***  XXCLT#        IFEQ      '114'
        //***  Begin Add  ***  01/06/15  *********************************
        //C***                eval      isUsePEEP = *on
        //***  End   Add  ***  01/06/15  *********************************
        //C***                SETON                                      67
        //C***                ENDIF
        *in67 = *On;
      //*** End    Change *** 02/04/19 ********************************
      /End-Free
     C*
     C     STSPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
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
     C*** Begin Chg *** 02/04/21 ***********************************
     C**********         MOVE      'C'           QQFLAG            1
     C**********         EXSR      GRPF
     C* Generate and Create File Member
     C                   Call      'W7417C5'
     C                   Parm      'C'           qqFlag            1
     C                   Parm      *blanks       qqMember         10
     C*** End   Chg *** 02/04/21 ***********************************
     C*
     C                   MOVE      *ZEROS        QQCO#             3
     C*** Begin Add *** 02/04/21 ***********************************
     C                   MOVE      *ZEROS        QQFND             3
     C*** End   Add *** 02/04/21 ***********************************
     C                   MOVE      *BLANKS       QQDEV            10
     C                   MOVE      *BLANKS       QQVOL            10
     C                   MOVE      *BLANKS       QQTAPE           10
     C                   MOVE      *BLANKS       QQINZ             3
     C                   Z-ADD     0             SCZIP
      *
     C                   MOVE      'Y'           XXDFT3            1
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C*  Access System Control file 'WC' record
     C*
     C     'WC'          CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     WCFL18        ANDEQ     'Y'
     C                   MOVE      *ON           *IN59
     C                   ELSE
     C                   MOVE      *OFF          *IN59
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXPHN
     C                   Z-ADD     0             XXFAX
     C*
     C* LOAD INITIAL SCREEN VALUES
     C* Get the tape initialization values from SMNU/SDRPF
     C*
     C     'SC'          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRTDNM        SCDEV
     C                   MOVEL     'NCCI'        SCVOL
     C     DRTDEN        IFNE      '6250    '
     C     DRTDEN        ANDNE     '1600    '
     C                   MOVEL     *BLANKS       SCDEN
     C                   MOVEL     '*QIC120 '    SCDEN
     C                   ELSE
     C                   MOVEL     DRTDEN        SCDEN
     C                   ENDIF
     C*
     C* If there is no System Control File, send message
     C*
     C                   ELSE                                                   ELSE 0 *IN60
     C                   MOVEL     *BLANKS       SCDEV
     C                   MOVEL     'NCCI'        SCVOL
     C                   MOVEL     *BLANKS       SCDEN
     C                   MOVE      '1'           *IN21
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVEL(P)  *BLANKS       SCFNAM
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         MOVEL(P)  'PDSRPT'      SCLIB
      *
     C**********         CALL      'SA030R'
     C**********         PARM                    SUFFIX            3
      *
     C**********         CAT(P)    SUFFIX:0      SCLIB
     C                   MOVEL(P)  *BLANKS       SCLIB
          In storedDir;
          Unlock storedDir;
          //*** Begin Chg *** 03/30/22 *************************************
          //*** cLib = directory;
          If directory = '/HOLSOMBA/';
             scLib = ' ';
          Else;
             scLib = directory;
          EndIf;
          //*** End   Chg *** 03/30/22 *************************************
     C*** End   Chg *** 02/04/21 *******************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET COMPANY NAME
     C*
     C*
     C     STRT01        TAG
     C                   MOVE      UPCO#         SCCO#
     C                   MOVE      UPFND         SCFND
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C*  LOAD USER NAME
     C*
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
      *
     C     XXDFT3        IFEQ      'Y'
     C                   EXSR      SRDEF3
     C                   ENDIF
      *
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04C'
     C     Q2RECF        OREQ      'SCR04S'
     C                   MOVE      *ON           *IN01
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   ELSE
     C                   MOVEL(P)  'Multiple'    SCNAME
     C                   ENDIF                                                  SCFND IFNE
     C*
     C* SET UP GROUP/CARRIER #/VARIOUS CARRIER ID HEADING FOR SCR05.
     C*
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             SVCARR            5 0
     C                   MOVE      *BLANK        XXERR             1
     C                   MOVE      *BLANK        XXFRST            1
     C                   MOVEA     '00'          *IN(67)
      *
     C                   OPEN      WXPGL
      *
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     PGFLAG        IFEQ      'I'
     C                   ADD       1             XXCNT
     C                   Z-ADD     PGGRP         XXFND
     C                   Z-ADD     0             XXCARR            5 0
     C     F2KY          CHAIN     PF201                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     XXFRST        IFEQ      *BLANK
     C                   Z-ADD     F2CAR#        SVCARR
     C                   MOVE      'Y'           XXFRST
     C                   ENDIF                                                  XXFRST IFEQ
      *
     C                   Z-ADD     F2CAR#        XXCARR
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXCARR        IFNE      SVCARR
     C                   MOVE      'Y'           XXERR
     C                   LEAVE
     C                   ENDIF                                                  XXCARR IFNE
     C*
     C                   ENDIF                                                  PGFLAG IFEQ
     C*
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO
      *
     C                   CLOSE     WXPGL
      *
      * SET INDICATORS FOR HEADING.
      *
     C     XXCNT         IFGT      1
     C                   MOVE      *ON           *IN67
     C                   MOVE      SVCARR        SCCARR
     C*
     C     XXERR         IFEQ      'Y'
     C                   MOVE      *ON           *IN68
     C                   ENDIF                                                  XXERR IFEQ
     C*
     C                   ENDIF                                                  XXCNT IFGT
     C                   WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      *OFF          *IN01
     C                   END
      *
     C     Q2RECF        IFEQ      'WDW01'
     C                   EXFMT     WDW01
     C                   END
     C*
     C******************************************************************
     C* Process All Command Keys
     C     *IN11         IFEQ      '1'
     C************************************
     C* F3 = EXIT                        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04C'
     C     Q2RECF        OREQ      'SCR04S'
     C*
     C                   CLOSE     WMSHL9
     C                   CLOSE     WMSHP
     C*
     C     SCFND         IFEQ      0
     C                   MOVE      'T'           QQFLAG            1
     C                   EXSR      GRPF
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q2RECF IFEQ
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
      * SEARCHES
      *
     C     *INKJ         IFEQ      *ON
      *
     C                   SELECT
      *
      *  Company
      *
     C     HFLD          WHENEQ    'SCCO#'
     C                   CALL      'S0501R'
     C                   PARM      QQPRDS        #1PRDS           40
     C                   PARM      *BLANK        #1CALL            1
     C                   PARM      *ZERO         #1CO#             3 0
     C                   PARM      *BLANK        #1RTC             1
      *
     C     #1CO#         IFEQ      *ZERO
     C                   SETON                                        74
      *
     C                   ELSE
      *
     C                   SETON                                        75
     C                   Z-ADD     #1CO#         SCCO#
     C                   ENDIF                                                  ##CO#
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      *  Fund
      *
     C     HFLD          WHENEQ    'SCFND'
      *
      *    Validate Company #
      *
     C     SCCO#         CHAIN     PCO01                              60
     C     *IN60         CABEQ     '1'           STRT                     22
     C                   MOVE      SCCO#         ##CO#
      *
      *    Fund Search
      *
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM      *BLANKS       ##FYR             3
     C                   PARM      1             ##VIEW            1 0
      *
     C     ##FND         IFEQ      *BLANKS
     C                   SETON                                        75
      *
     C                   ELSE
      *
     C                   MOVE      ##FND         SCFND
     C                   SETON                                        76
     C                   ENDIF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      *    State Search
      *
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'      STSPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        78
     C*
     C                   ELSE
      /free
       //***  Begin Chg  ***  01/06/15  ***********************************
       //***Beg Add***** 05/02/14 ****************
       //If XXCLT# = '093';
       //   *IN68 = '1';
       //ElseIf XXCLT# = '114';
       //   *IN69 = '1';
       //Endif;
       //***End Add***** 05/02/14 ****************
       *in68 = *on;
       Select;
       When isUsePEEP;
          *in69 = *on;
       Endsl;
       //***  End   Chg  ***  01/06/15  ***********************************
      /end-free
     C*
     C     ##RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         SCST
     C*
     C                   ENDIF
      *
     C                   ENDSL
       //***  Begin Add  ***  01/06/15  ***********************************
     C                   GOTO      STRT
       //***  End   Add  ***  01/06/15  ***********************************
      *
     C                   ENDIF                                                  *INKJ
      *
      *
     C************************************
     C* *IN10 - Process HELP Requests    *
     C************************************
     C     *IN10         IFEQ      '1'
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
     C                   END                                                    END *IN10
     C* * * F 12 PREVIOUS
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04C'
     C     Q2RECF        OREQ      'SCR04S'
     C*
     C                   CLOSE     WMSHL9
     C                   CLOSE     WMSHP
     C*
     C     SCFND         IFEQ      0
     C                   MOVE      'T'           QQFLAG            1
     C                   EXSR      GRPF
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q2RECF IFEQ
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKC
     C*
     C************************************
     C* CMD 13 - CHG USER PARMS          *
     C************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C*
     C*  *IN18 -  ROLLUP REQUEST
     C*
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C*
     C     *IN32         IFEQ      *ON
     C                   MOVE      XXLAST        HHREC#
     C                   GOTO      STRT                                         NO ROLL IF EOF
     C                   ENDIF
     C*
     C                   MOVE      XXRRN2        RRN2
     C     SHKY2         SETGT     PSH01
     C     SCFND         IFNE      *ZERO
     C     SHKEY         READE     PSH01                                  60
     C                   ELSE
     C     PGKY          READE     PSH01                                  60
     C                   ENDIF                                                  SCFND IFNE
     C     *IN60         IFEQ      *OFF
     C                   EXSR      LOAD
     C                   ENDIF
     C                   GOTO      STRT
     C                   END                                                    *IN18 ROLLUP
     C*
     C**
     C                   END                                                    END CMDKEYS
     C*********************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04C'
     C     Q2RECF        OREQ      'SCR04S'
     C                   GOTO      SCR04T
     C                   END
     C*
     C****************************
     C* LOAD TAPE & INITIALIZE   *
     C****************************
     C     SCR01T        TAG
     C*
     C* * * VALIDATE COMPANY #
     C     SCCO#         CHAIN     PCO01                              60
     C     *IN60         CABEQ     '1'           STRT                     22
     C*
     C* * * VALIDATE FUND #
     C     SCFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         CABEQ     '1'           STRT                     23
     C                   ENDIF
      *
      * Type of output
      *
     C     SCOTYP        IFNE      'F'
     C                   SETON                                        71
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVE      SCOTYP        QQBTFL            1
     C                   ENDIF
      *
      *  Tape output
      *
      *  Valid Library must be entered if output type is 'F'ile.
      *
     C     SCOTYP        IFEQ      'F'
      *
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         CALL      'SRCHKOB2'
     C**********         PARM      SCLIB         ##LIB            10
     C**********         PARM      SCFNAM        ##FIL            10
     C**********         PARM      '*FILE'       ##TYP             7
     C**********         PARM      *BLANK        ##ERR             1
     C                   Eval      ##ifsName = scLib
     C                   Call      'CHKIFSOBJ'
     C                   Parm                    ##ifsName
     C                   Parm      *Off          ##found
     C*** End   Chg *** 02/04/21 *******************************************
      *
     C*** Begin Chg *** 02/04/21 *******************************************
     C*****##ERR         IFEQ      '1'
     C     ##found       IFEQ      *Off
     C*** End   Chg *** 02/04/21 *******************************************
     C                   SETON                                        72
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDIF                                                  SCOTYP
      *
      *  Valid File name must be entered, if 'F'ile output selected
      *
     C     SCOTYP        IFEQ      'F'
      *
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         CALL      'SRCKOBJN'
     C**********         PARM      SCFNAM        #1FIL            10
     C**********         PARM      *BLANK        #1ERR             1
     C                   Eval      #1err = '0'
     C                   If        %scan('.TXT':scFnam) = 0
     C                   Eval      #1err = '1'
     C                   EndIf
     C*** End   Chg *** 02/04/21 *******************************************
      *
     C     #1ERR         IFEQ      '1'
     C                   SETON                                        73
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDIF                                                  SCOTYP
      *
      *  Info window if File already exists in specified library.
      *
     C*** Begin Add *** 02/04/21 *******************************************
     C* Does the file already exist?
     C                   Eval      ##ifsName = %Trim(scLib) + '/' +
     C                             scFnam
     C                   Call      'CHKIFSOBJ'
     C                   Parm                    ##ifsName
     C                   Parm      *Off          ##found           1
     C*** End   Add *** 02/04/21 *******************************************
     C     SCOTYP        IFEQ      'F'
     C*** Begin Chg *** 02/04/21 *******************************************
     C*****##ERR         ANDEQ     '0'
     C     ##found       AndEq     *On
     C*** End   Chg *** 02/04/21 *******************************************
     C                   EXFMT     WDW01
      *
     C     *INKL         IFEQ      *ON
     C                   SETOFF                                       KL
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL
      *
     C                   ENDIF                                                  SCOTYP
     C     SCST          IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        26
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVEL(P)  'NCCI'        XXTBID
     C                   MOVEL(P)  SCST          XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        27
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SCST IFNE
      *
     C*  Experience rating selection
     C*
     C     SCER          IFNE      '1'
     C     SCER          ANDNE     '3'
     C     SCER          ANDNE     ' '
     C                   MOVE      *ON           *IN77
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C* VALIDATE THE SCPEEP
     C*
     C     SCPEEP        IFNE      *BLANKS
     C     SCPEEP        ANDNE     'Y'
     C                   MOVE      '1'           *IN43
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
      *
     C     SCFND         IFEQ      *ZERO
     C*
     C                   MOVE      SCCO#         ##CO#
     C                   CALL      'W7944C'
     C                   PARM      XXPPGM        @@PGM            10
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     @@PGM         IFEQ      '*ON'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKC
      *
     C                   Z-ADD     0             XXCNT             3 0
     C                   Z-ADD     0             XXFND             3 0
      *
     C                   OPEN      WXPGL
      *
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
     C     PGFLAG        IFEQ      'I'
     C                   ADD       1             XXCNT
     C                   Z-ADD     PGGRP         XXFND
     C                   ENDIF
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO
      *
     C                   CLOSE     WXPGL
      *
     C                   SELECT
     C     XXCNT         WHENEQ    *ZERO
     C                   MOVE      *ON           *IN29
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C     XXCNT         WHENEQ    1
     C                   Z-ADD     XXFND         SCFND
     C                   OTHER
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*
     C* LOAD CONTACT SCREEN IF PA OR DE.
     C*
     C     SCST          IFNE      *BLANKS
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   EXSR      TEMPF
     C*
     C* PROCEED TO PROCESS SCREEN.
     C*
     C                   GOTO      PROCES
     C*
     C     SCR03T        TAG
     C*
     C* EMAIL CANNOT BE BLANK
     C*
     C     SCMAIL        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* CONTACT NAME CANNOT BE BLANK
     C*
     C     SCCNAM        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* PHONE AREA CODE CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCPHNA        IFLE      *ZERO
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* PHONE PREFIX CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCPHN1        IFLE      *ZERO
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* PHONE SUFFIX CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCPHN2        IFLE      *ZERO
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* EXTENSION CANNOT BE LESS THAN ZERO.
     C*
     C     SCEXT         IFLT      *ZERO
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* FAX AREA CODE CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCFAXA        IFLE      *ZERO
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* FAX PREFIX CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCFAX1        IFLE      *ZERO
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* FAX SUFFIX CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCFAX2        IFLE      *ZERO
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* STREET NAME CANNOT BE BLANK.
     C*
     C     SCSTRT        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* CITY NAME CANNOT BE BLANK.
     C*
     C     SCCITY        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN43
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* STATE MUST BE VALID.
     C*
     C                   MOVEL(P)  SCST2         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* ZIP CODE 1 CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C     SCZIP1        IFLE      *ZERO
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* ZIP CODE 2 CANNOT BE LESS THAN ZERO.
     C*
     C     SCZIP2        IFLT      *ZERO
     C                   MOVE      *ON           *IN46
     C                   GOTO      STRT
     C                   ENDIF
     **** Beg Add - 03/10/22  **************************************************

        If SCDtaPvdr = 'T' and scTFein = 0 ;
           *In47 = *On;
     C                   GOTO      STRT
        EndIf;

     **** End Add - 03/10/22  **************************************************
     C                   EXSR      TEMPF
     C*
     C*
     C* PROCEED TO PROCESS SCREEN
     C*
     C                   GOTO      PROCES
      *
     C*
     C     SCR02T        TAG
     C*
     C* Validate initialization value.
     C*
     C     SCINZ         IFNE      'INZ'
     C     SCINZ         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     SCINZ         QQINZ
     C                   MOVEL     SCDEV         QQDEV
     C                   MOVEL     SCVOL         QQVOL
     C                   MOVEL     SCDEN         QQTAPE
     C*
     C* If Device Name or Device Density is blank, send message
     C*
     C     QQDEV         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C* If Device Name is incorrect, send message
     C*
     C                   MOVE      ' '           QQRTC             1
     C                   CALL      'W0001C'
     C                   PARM                    QQDEV
     C                   PARM                    QQRTC
     C     QQRTC         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END 1 QQRTC
     C*
     C* IF VOLUME IS BLANK, SEND MESSAGE
     C*
     C     QQVOL         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C* IF DEVICE DENSITY IS BLANK, SEND MESSAGE
     C*
     C     QQTAPE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C                   EXSR      TEMPF
     C*
     C     PROCES        TAG
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04   '    Q2RECF
     C                   Z-ADD     0             RRN2              5 0
     C*
     C     SCEMP#        IFNE      0
     C                   Z-ADD     SCEMP#        SCEMPX
     C                   ENDIF                                                  SCEMP# IFNE
     C                   MOVE      SCEMPX        SVEMP1
     C                   MOVE      XXTRDT        SVTRD1
     C                   MOVE      XXUNID        SVUNI1           12 0
     C                   MOVE      SCEMPX        SVEMP#
     C                   MOVE      XXTRDT        SVTRDT
     C                   MOVE      XXUNID        SVUNID
     C                   SUB       1             SVUNID
     C*
     C*   CLEAR SUBFILE
     C*
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR04C
     C                   MOVE      *OFF          *IN04
     C*
      *
     C                   MOVE      'P'           XXTYPE            1
     C                   MOVE      '3'           XXSTAT            1
     C*
     C     SCFND         IFNE      *ZERO
     C     SHKEY         CHAIN     PSH01                              60
     C                   ELSE
     C     PGKY          CHAIN     PSH01                              60
     C                   ENDIF                                                  SCFND IFNE
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C                   GOTO      STRT
     C*
     C*
     C     SCR04T        TAG
     C*
     C                   MOVE      *OFF          *IN04
     C                   MOVE      *OFF          *IN48
     C*
     C                   MOVE      *OFF          *IN49
     C*
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCEMPX        IFNE      *ZERO
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID           12 0
     C                   GOTO      PROCES
     C                   END                                                                     S
     C*
     C*
     C*
     C*
     C***** CALL PROGRAM TO PROCESS THE DATA TRANSFER
     C*
     C                   MOVE      *BLANKS       QQMAIL           31
     C                   MOVE      *BLANKS       QQCNAM           25
     C                   MOVE      *BLANKS       QQPHN            10
     C                   MOVE      *BLANKS       QQEXT             6
     C                   MOVE      *BLANKS       QQFAX            10
     C*** Begin Chg *** 04/08/21 *******************************************
     C**********         MOVE      *BLANKS       QQADR           101
     C                   MOVE      *BLANKS       QQADR           102
     C*** End   Chg *** 04/08/21 *******************************************
     C*
     C     SCMAIL        IFNE      *BLANKS
     C*
     C                   MOVEL     SCMAIL        QQMAIL
     C                   MOVEL     SCCNAM        QQCNAM
     C                   MOVE      XXPHN         QQPHN
     C                   MOVE      SCEXT         QQEXT
     C                   MOVE      XXFAX         QQFAX
     C                   MOVE      SCSTRT        XXSTRT
     C                   MOVE      SCCITY        XXCITY
     C                   MOVE      SCST2         XXST
     C                   MOVE      SCZIP1        XXZIP1
     C                   MOVE      SCZIP2        XXZIP2
     C*** Begin Add *** 04/08/21 *******************************************
     C**********         MOVE      XXADR         QQADR
     C                   MOVEL     XXADR         QQADR
     C                   MOVE      '$'           QQADR
     C*** End   Add *** 04/08/21 *******************************************
     C*
     C                   ENDIF
     C                   CLOSE     WMSHL9
     C                   CLOSE     WMSHP
     C*
     C     SCFND         IFEQ      0
     C                   MOVE      'T'           QQFLAG            1
     C                   EXSR      GRPF
     C                   ENDIF
     C*** Begin Add *** 02/04/21 *******************************************
     C                   Eval      qqCo# = %EditC(scCo#:'X')
     C                   Eval      qqFnd = %EditC(scFnd:'X')
     C*
     C*** Begin Add *** 03/10/22 *******************************************
       qqFein    = %Char(ScTFein);
     C*** End   Add *** 03/10/22 *******************************************
     C* Add $ at end of directory and file string
     C                   Eval      qqLib = scLib  + '$'
     C                   Eval      qqFil = scFnam + '$'
       // Store Directory
       In *Lock storedDir;
       directory = scLib;
       Out storedDir;
     C*** End   Add *** 02/04/21 *******************************************
     C*
     C*  Call              program
     C*
     C                   CALL      'W7417C'
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         PARM                    SCCO#
     C**********         PARM                    SCFND
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C*** End   Chg *** 02/04/21 *******************************************
     C                   PARM                    QQBTFL
     C                   PARM                    QQINZ
     C                   PARM                    QQDEV
     C                   PARM                    QQVOL
     C                   PARM                    QQTAPE
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         PARM      *ZEROS        QQTOTR            9 0
     C**********         PARM      *ZEROS        QQTOTH            9 0
     C                   PARM      '000000000'   QQTOTR            9
     C                   PARM      '000000000'   QQTOTH            9
     C*** End   Chg *** 02/04/21 *******************************************
     C                   PARM                    QQRTCD
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         PARM      SCLIB         QQLIB            10
     C**********         PARM      SCFNAM        QQFIL            10
     C                   PARM                    QQLIB           251
     C                   PARM                    QQFIL           101
     C*** End   Chg *** 02/04/21 *******************************************
     C                   PARM      SCER          QQER              1
     C                   PARM      SCST          QQST              2
     C                   PARM                    QQMAIL
     C                   PARM                    QQCNAM
     C                   PARM                    QQPHN
     C                   PARM                    QQEXT
     C                   PARM                    QQFAX
     C                   PARM                    QQADR
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         PARM      *ZEROS        QQTLNP            9 0
     C**********         PARM      *ZEROS        QQTLRP            9 0
     C**********         PARM      *ZEROS        QQTLCX            9 0
     C**********         PARM      *ZEROS        QQTLRE            9 0
     C**********         PARM      *ZEROS        QQTLEN            9 0
     C                   PARM      '000000000'   QQTLNP            9
     C                   PARM      '000000000'   QQTLRP            9
     C                   PARM      '000000000'   QQTLCX            9
     C                   PARM      '000000000'   QQTLRE            9
     C                   PARM      '000000000'   QQTLEN            9
     C*** End   Chg *** 02/04/21 *******************************************
     C                   PARM      SCPEEP        QQPEEP            1
      *** begin add ***  12/03/15  *************************************************
     C                   PARM      scOldCar      QQOldCar          1
      *** end add *****  12/03/15  *************************************************
     C*** Begin Add *** 02/04/21 *******************************************
     C                   PARM                    QQmember         10
     C*** End   Add *** 02/04/21 *******************************************
     C*** Begin Add *** 03/10/22 *******************************************
     C                   PARM      SCDtaPvdr     qqDtaPvdr         1
     C                   PARM                    qqFein            9
     C*** Begin Add *** 03/10/22 *******************************************
     C*
     C**** DISPLAY THE TOT RECORDS PANEL FOR THE TRANSMITTAL LETTER.
     C                   MOVE      QQTOTR        SCTOTR
     C                   MOVE      QQTOTH        SCTOTH
     C                   MOVE      QQTLNP        SCTLNP
     C                   MOVE      QQTLRP        SCTLRP
     C                   MOVE      QQTLCX        SCTLCX
     C                   MOVE      QQTLRE        SCTLRE
     C                   MOVE      QQTLEN        SCTLEN
     C*
     C*** Begin Chg *** 02/04/21 *******************************************
     C**********         EXFMT     SCR05
     C                   EXFMT     WDW02
     C*** End   Chg *** 02/04/21 *******************************************
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C*** Begin Del 02/04/21 ***********************************************
     C**********         MOVE      'D'           QQFLAG            1
     C**********         EXSR      GRPF
     C*** End   Del 02/04/21 ***********************************************
     C*** Begin Add *** 02/04/21 ***********************************
     C* Remove file Member since single group is being passed
     C                   If        scFnd <> 0 Or *inKC = *On
     C                   Call      'W7417C5'
     C                   Parm      'R'           qqFlag
     C                   Parm                    qqMember
     C                   Endif
     C*** End   Add *** 02/04/21 ***********************************
     C                   MOVE      '1'           *INLR
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*** GET ALL POLICY SUBMISSION REQUESTS ***
     C*
     C                   MOVE      'P'           XXTYPE            1
     C                   MOVE      '3'           XXSTAT            1
     C*
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UP TO 11 LINES
     C*
     C     NEXTLD        TAG
     C                   DO        11            X                 5 0
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     NEXT          TAG
     C     EMKY          CHAIN     PEM01                              61
     C                   MOVEL     EMNAME        SCENAM
     C  N61SCER          IFNE      *BLANK
     C     SCER          IFNE      EMEXRT
     C     SCFND         IFNE      *ZERO
     C     SHKEY         READE     PSH01                                  60
     C                   ELSE
     C     PGKY          READE     PSH01                                  60
     C                   ENDIF                                                  SCFND IFNE
     C     *IN60         IFEQ      '0'
     C                   GOTO      NEXT
     C                   ELSE
     C                   GOTO      LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** begin add ***  12/03/15  *********************************************
     C*
     C* Do not display records that won't submit for LCTA
     C*
     C                   If        xxClt# = '045'
     C*
     C                   If        scOldCar = 'Y'
     C*
     C* When processing old carrier data, do not display anything effective >= 1/1/2016
     C*
     C                   If        SHPfdt >= 1160101
     C*
     C     SHKEY         READE     PSH01                                  60
     C     *IN60         IFEQ      '0'
     C                   GOTO      NEXT
     C                   ELSE
     C                   GOTO      LEAVE
     C                   ENDIF
     C*
     C                   Endif
     C*
     C                   Else
     C*
     C* When processing new carrier data, do not display anything effective < 1/1/2016
     C*
     C                   If        SHPfdt < 1160101
     C*
     C     SHKEY         READE     PSH01                                  60
     C     *IN60         IFEQ      '0'
     C                   GOTO      NEXT
     C                   ELSE
     C                   GOTO      LEAVE
     C                   ENDIF
     C*
     C                   Endif
     C*
     C                   Endif                                                  If scOldCar = 'Y'
     C*
     C                   Endif                                                  If xxClt# = '045'
     C*** end add *****  12/03/15  *********************************************
     C*
     C* CHECK IF STATE IS PENDING IF PROCESSING BY STATE.
     C*
     C     *IN59         IFEQ      *ON
     C*
     C     SCST          IFNE      *BLANKS
     C                   MOVE      SCST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
     C*
     C                   Z-ADD     1             I                 2 0
     C     XZST          LOOKUP    SST(I)                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C     SSS(I)        ORNE      '3'
     C*
     C     SCFND         IFNE      *ZERO
     C     SHKEY         READE     PSH01                                  60
     C                   ELSE
     C     PGKY          READE     PSH01                                  60
     C                   ENDIF                                                  SCFND IFNE
     C*
     C     *IN60         IFEQ      '0'
     C                   GOTO      NEXT
     C                   ELSE
     C                   GOTO      LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN59 IFEQ
     C                   ADD       1             RRN2
     C*
     C     X             IFEQ      1
     C     *IN21         ANDEQ     *OFF
     C                   Z-ADD     RRN2          HHREC#
     C                   Z-ADD     RRN2          XXLAST            5 0
     C                   ENDIF
     C*
     C                   MOVE      SHEMP#        SCEMPS
     C                   MOVE      SHEMP#        SVEMP#
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      SHSBDT        SVTRDT
     C                   MOVE      SHFND         SCFND2
     C                   MOVE      SHUNID        SCUNID
     C                   MOVE      SHTYPE        SVTYPE            1
     C                   MOVE      SHSTAT        SVSTAT            1
     C                   MOVE      SHUNID        SVUNID           12 0
     C                   MOVE      SHFND         SVFND             3 0
     C                   MOVE      *BLANK        SCSTAT
     C*
     C                   MOVEL     'READY   '    SCSTAT
     C*
     C                   Z-ADD     1             Y                 3 0
     C     SHTYPE        LOOKUP    TC(Y)                                  60
     C                   MOVEL     TD(Y)         SCTYPE
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR04S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C*
     C     SCFND         IFNE      *ZERO
     C     SHKEY         READE     PSH01                                  60
     C                   ELSE
     C     PGKY          READE     PSH01                                  60
     C                   ENDIF                                                  SCFND IFNE
     C*
     C     LEAVE         TAG
     C* If past thru once and NO subfile records process
     C* again until reach end of file.
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '0'
     C                   GOTO      NEXTLD
     C                   ENDIF
     C                   END
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *ZERO         SCEMPS
     C                   MOVEL     *BLANK        SCENAM
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *ZERO         SCTRDT
     C                   MOVE      *ZERO         SCFND2
     C                   MOVE      *ZERO         SCUNID
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             XXLAST
     C                   Z-ADD     1             HHREC#
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR04S
     C                   END
     C*
     C*  SAVE LAST WRITTEN RECORD NUMBER
     C*
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C*
     C                   MOVEL     *ZERO         SCEMPX
     C*
     C                   ENDSR
      *
      * CALL CL TO CREATE TEMPORARY VERSIONS OF FILES.
      *
     C     GRPF          BEGSR
      *
     C                   CALL      'W7417C4'
     C                   PARM                    QQFLAG
      *
     C                   ENDSR                                                  GRPF ENDSR
      *****************************************************************
     C*
     C* WRITE RECORD TO WMSHP FOR RECORD DISPLAY SCREEN.
     C*
     C     INCREC        BEGSR
     C*
     C     SHKY3         CHAIN     PSH03                              60
     C     *IN60         DOWEQ     *OFF
     C                   WRITE     PSH02
     C     SHKY3         READE     PSH03                                  60
     C                   ENDDO
     C*
     C                   ENDSR                                                  INCREC ENDSR
     C*****************************************************************
     C*
     C* PREPARE TEMPORARY FILE FOR RECORD DISPLAY SCREEN.
     C*
     C     TEMPF         BEGSR
     C*
     C     SCFND         IFEQ      0
     C                   MOVE      'S'           QQFLAG            1
     C                   EXSR      GRPF
     C                   ENDIF
     C*
     C                   OPEN      WMSHP
     C                   OPEN      WMSHL9
     C*
     C* LOAD TEMPORARY FILE WITH DATA FOR SCREEN.
     C*
     C     SCFND         IFEQ      0
     C*
     C                   OPEN      WXPGL
      *
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     PGFLAG        IFEQ      'I'
     C                   Z-ADD     PGGRP         XXFND
     C                   EXSR      INCREC
     C                   ENDIF
      *
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO
      *
     C                   CLOSE     WXPGL
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  TEMPF ENDSR
      *
      *
      *=========================================================================
      *  SRDEF3 - Default SCR03 Values
      *=========================================================================
     C     SRDEF3        BEGSR
     C***                MOVE      'N'           XXDFT3
     **** Beg Add - 03/10/22  **************************************************
     C***                MOVE      'P'           KKFLAG

           curCarrier = 0;
           Chain(n) (scCO# : scFND) WDF2L ;
           If %Found(WDF2L);
              Chain(n) (scCO# : F2CAR#) WMTML1;
              If Not %Found(WMTML1);
                 Chain(n) (scCO# : 0) WMTML1;
              EndIf;
           Else;
              Chain(n) (scCO# : 0) WMTML1;
           EndIf;

     C***  TMKY          CHAIN     PTM01                              60
      *
     C***  *IN60         IFEQ      *OFF
           If %Found(WMTML1);
     **** End Add - 03/10/22  **************************************************
     C                   MOVEL(P)  TMMAIL        SCMAIL
     C                   MOVEL(P)  TMNAME        SCCNAM
     C                   MOVE      TMPHN#        XXPHN
     C                   Z-ADD     TMEXT#        SCEXT
     C                   MOVE      TMFAX#        XXFAX
     C                   MOVEL(P)  TMADR1        SCSTRT
     C                   MOVEL(P)  TMCTY         SCCITY
     C                   MOVEL     TMST          SCST2
     C                   MOVE      TMZIP         SCZIP
     **** Beg Add - 03/10/22  **************************************************
                         scDtaPvdr = TMDType ;
                         scTFein   = TMFein ;
                         qqDtaPvdr = TMDType ;
                         qqFein    = %Char(TMFein);
     **** End Add - 03/10/22  **************************************************
     C                   ENDIF
      *
     C                   ENDSR
      *
**  TC/TD - TYPE CODE/DESCRIPTION
PPOLICY
CCANCEL
EENDORSE
RREINSTATE
