     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*
     H* W0169R25 - ISSUE MASS CANCELLATION PROMPT SCREEN
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 10/06/16  C:65  PROJECT: SY371                                *
      *                 Implement Scriptura output control window     *
      *                 when process is flagged to generate Scriptura *
      *                 output.  The settings are stored/passed via   *
      *                 a local data area which is picked up by       *
      *                 API's and the printing program.               *
     H*                                                               *
     H* 05/08/13 C:279 Redmine: 2845                                  *
     H*                Changed to default checkbox for Generate cert  *
     H*                holders letter.                                *
     H*                                                               *
     H* 05/19/11 C:180 SUPPORT  75073
     H*                Enable printing of agent labels for all clients.
     H*
     H* 09/16/09 C:227 SUPPORT: WA099 68073                           *
     H*                CHANGES OF 46WGC TO BE FLAGGED FOR CLIENT 46   *
     H*                                                               *
     H* 10/21/08 C:136 SUPPORT: WA071 65654                           *
     H*                DATES NOT DISPLAYING IN HEADER                 *
     H*                                                               *
     H* 07/24/08 C:61  PROJECT 46WGC                                  *
     H*                Enable printing agent labels.                  *
     H*                                                               *
     H* 07/03/07 C:190 PROJECT 71WIH                                  *
     H*              - Add new prompt selections(SCRT1,SCRT2)        *
     H*                                                               *
     H* 08/03/04 C:61  PROJECT WC966                                  *
     H*              - Default Mass CX Issue date to current date.    *
     H*              - Display Warning if difference between issue    *
     H*                date entered and current date is more than     *
     H*                one week.                                      *
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*              - Change header from "Minimum balance" to        *
     H*                "% of Premium".                                *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify if vendor processing is enabled.        *
     H*                CCFREQ <> blanks                               *
     H*                                                               *
     H* 12/01/00  C:61  PROJECT: WC577                                *
     H*                 Add Fund to header info.                      *
     H*                                                               *
     H* 12/02/99  C:90  PROJECT: WC912                                *
     H*                 ADD 'BALANCE TYPE' (CCEAFL) TO THE HEADER.    *
     H*                                                               *
     H* 10/26/99  C:87  PROJECT: WC577                                *
     H*                 Chg 'Balance dt' to 'Billings through' and    *
     H*                 add 'Payments through' to screen.             *
     H*                                                               *
     H* 09/28/99  C:87  PROJECT: WC757                                *
     H*                 Change processing so form type is optional.   *
     H*                 However, if label type is entered then form   *
     H*                 type must be entered.                         *
     H*                                                               *
     H* 06/12/98  C:35  PROJECT: SUPPORT WC095 12447                  *
     H*                 IF A USER HAD TWO SESSIONS RUNNING AND THEY   *
     H*                 WENT INTO WORK WITH CANCELLATION BATCHES IN   *
     H*                 ONE SESSION AND THEN CHANGED THE FUND IN      *
     H*                 THEIR USER PARAMETERS IN THE OTHER SESSION    *
     H*                 AND THEN SWITCHED BACK TO THE FIRST SESSION   *
     H*                 AND TOOK AN OPTION 25 TO ISSUE THE MASS       *
     H*                 CANCELLATION, THIS PROGRAM WAS PICKING UP THE *
     H*                 FUND FROM USER PARAMETERS (WHICH WAS THE      *
     H*                 WRONG FUND), INSTEAD OF THE FUND FROM THE     *
     H*                 CANCELLATION BATCH CONTROL RECORD.  THIS      *
     H*                 PROBLEM HAS BEEN CORRECTED.                   *
     H*                                                               *
     H*****************************************************************
      *
     FW0169F25  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWXCCP     UF   E             DISK
     F                                     RENAME(PCC01:DBRCDF)
     FSSYLF     IF   E           K DISK
      *** begin add ***  10/06/16  ************************************
     FWMEXL     IF   E           K DISK    rename(pex01:pex00)
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Cancellations')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add *****  10/06/16  ************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Issue Mass Cancellat-
     D                                     ion Batch')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
      * KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
      *
      *  VALIDATE THE CANCEL NOTICE DATE
      *
     C                   Z-ADD     SCDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         SCDATE
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR IFEQ
      *
      * VALIDATE THE CANCELLATION EFFECTIVE DATE
      *
     C  N90SCCDAT        IFNE      0
      *
     C  N90              Z-ADD     SCCDAT        SRDTI
      *
     C  N90              CALL      'SRCVTDAT'    DATCHK
      *
     C  N90              MOVEL     SRDTO         SCCDAT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCCDAT IFNE 0
      *
      *  GENERATE CERTIFICATE HOLDERS LETTERS MUST BE 'Y' OR 'N'
      *
     C  N90SYOFIF        IFEQ      'Y'
     C     SCCCRT        ANDNE     'Y'
     C     SCCCRT        ANDNE     'N'
      *
     C                   SETON                                        73  90
      *
     C                   ENDIF
      *
      * VALIDATE LABEL SELECTION.  'R'=REGULAR; 'L'=LOGO
      *
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C     SCTYPE        IFNE      *BLANK
     C***  END   ADD  ***  07/29/99  **********************************
     C  N90SCTYPE        IFNE      'R'
     C     SCTYPE        ANDNE     'L'
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCTYPE IFNE 'R'
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C                   ENDIF                                                  SCTYPE<>*BLANK
     C***  END   ADD  ***  07/29/99  **********************************
      *
      * VALIDATE FORM SELECTION.  'L'=LASER; 'P'=PINFEED
      *
     C***  BEGIN ADD  ***  09/28/99  **********************************
     C     SCFORM        IFNE      *BLANK
     C***  END   ADD  ***  09/28/99  **********************************
     C  N90SCFORM        IFNE      'L'
     C     SCFORM        ANDNE     'P'
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCFORM IFNE 'L'
     C***  BEGIN ADD  ***  09/28/99  **********************************
     C                   ENDIF                                                  SCFORM<>*BLANK
     C***  END   ADD  ***  09/28/99  **********************************
     C*
     C***  BEGIN ADD  ***  09/28/99  **********************************
      ***** BEG DEL ******* 07/24/08 **********************************
     C*********  SCTYPE    IFNE *BLANK
     C*********  SCFORM    ANDEQ*BLANK
     C*********  SCTYPE    OREQ *BLANK
     C*********  SCFORM    ANDNE*BLANK
     C*********            SETON                     77  90
     C*********            ENDIF
      ***** END DEL ******* 07/24/08 **********************************
     C***** BEG ADD ******* 09/16/09 **********************************
      ******** BEG DELETE *** 05/19/11 ********************************
     C***        CLTID     IFNE '046'
     C***        SCTYPE    IFNE *BLANK
     C***        SCFORM    ANDEQ*BLANK
     C***        SCTYPE    OREQ *BLANK
     C***        SCFORM    ANDNE*BLANK
     C***                  SETON                     77  90
     C***                  ENDIF
     C***                  ENDIF
      ******** END DELETE *** 05/19/11 ********************************
     C***** END ADD ******* 09/16/09 **********************************
     C***  END   ADD  ***  09/28/99  **********************************
      *
      * VALIDATE FORM SELECTION.  LASER NOT ALLOWED WITH LOGO FORMS
      *
     C  N90SCFORM        IFEQ      'L'
     C     SCTYPE        ANDEQ     'L'
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCFORM IFEQ 'L'
      ***** BEG ADD ******* 08/03/04 **********************************
      *
     C***  BEGIN ADD  ***  07/03/07  **********************************
      * VALIDATE PRIMARY PROCESSING SORT ORDER
      *    AN-AGENT NUMERIC
      *    AA-AGENT ALPHA
      *    EN-EMPLOYER NUMERIC
      *    EN-EMPLOYER ALPHA
     C     SCSRT1        IFEQ      *BLANK
     C                   SETON                                        65  90
     C                   ENDIF                                                              '  '
     C*
     C  N90SCSRT1        IFNE      'AN'
     C     SCSRT1        ANDNE     'AA'
     C     SCSRT1        ANDNE     'EN'
     C     SCSRT1        ANDNE     'EA'
     C                   SETON                                        94  90
     C                   ENDIF                                                  SCSRT1 IFNE 'AA'
     C****
      * VALIDATE SECONDARY PROCESSING SORT ORDER
      *    EN-EMPLOYER NUMERIC
      *    EN-EMPLOYER ALPHA
      *
     C  N90SCSRT2        IFNE      *BLANK
     C     SCSRT2        ANDNE     'EN'
     C     SCSRT2        ANDNE     'EA'
     C                   SETON                                        95  90
     C                   ENDIF                                                  SCSRT2 IFNE 'EA'
      *
     C  N90SCSRT2        IFNE      *BLANK
     C     SCSRT1        ANDNE     'AN'
     C     SCSRT1        ANDNE     'AA'
      ***** BEG ADD ******* 07/24/08 **********************************
     C***** BEG CHG ******* 09/16/09 **********************************
     C*********            SETON                     95  90
     C*********            SETON                     81  90
      ***** END ADD ******* 07/24/08 **********************************
      ***** BEGIN CHANGE ***** 05/19/11 *******************************
     C*          CLTID     IFEQ '046'
     C*                    SETON                     81  90
     C*                    ELSE
     C*                    SETON                     95  90
     C*                    ENDIF                           SCSRT2 IFNE 'EA'
      *
     C                   SETON                                        81  90
      ***** END   CHANGE ***** 05/19/11 *******************************
     C***** END CHG ******* 09/16/09 **********************************
     C*                    ENDIF                           SCSRT2 IFNE 'EA'
     C                   ENDIF                                                  SCSRT2 IFNE 'EA'
     C***  END   ADD  ***  07/03/07  **********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     C***** BEG ADD ******* 09/16/09 **********************************
     C***** BEG DELETE ***** 05/19/11 *********************************
     C****       CLTID     IFEQ '046'
     C***** END ADD ******* 09/16/09 **********************************
     C***** END DELETE **** 05/19/11 **********************************
      *
      * "Address label type" and "Print labels ..." are mutually inclusive
      *
     C  N90SCAEB         IFNE      *BLANK
     C     SCTYPE        ANDEQ     *BLANK
     C     SCAEB         OREQ      *BLANK
     C     SCTYPE        ANDNE     *BLANK
     C                   SETON                                        90  78
     C                   ENDIF
      *
      * "Print labels ..." values "A" or "B" only allowed if
      *     "Primary sort order" by agent ("AA" or "AN")
      *
     C  N90SCAEB         IFEQ      'A'
     C     SCAEB         OREQ      'B'
      *
     C     SCSRT1        IFNE      'AA'
     C     SCSRT1        ANDNE     'AN'
     C                   SETON                                        90  79
     C                   ENDIF
      *
     C***** BEG DELETE ***** 05/19/11 *********************************
     C*****                ENDIF
     C***** END DELETE **** 05/19/11 **********************************
      *
      * "Print labels ..." values "E" or "B" only allowed if
      *     "Employer address label form" value entered.
      *
     C  N90SCAEB         IFEQ      'E'
     C     SCAEB         OREQ      'B'
      *
     C     SCFORM        IFEQ      *BLANK
     C                   SETON                                        90  80
     C                   ENDIF
      *
     C                   ENDIF
      *
     C***** BEG ADD ******* 09/16/09 **********************************
     C                   ENDIF
     C***** END ADD ******* 09/16/09 **********************************
      ***** END ADD ******* 07/24/08 **********************************
      *
      * Display Warning if "CX Notice Date" differs from current date
      * by more than one week.
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     DSDATE        #1FROM
     C                   Z-ADD     SCDATE        #1TO
      *
     C                   CALL      'SRDDIF8'
     C                   PARM                    #1FROM            8 0
     C                   PARM                    #1TO              8 0
     C                   PARM      0             #1DAYS            5 0
     C                   PARM      *BLANK        #1ERR             1
      *
     C     #1DAYS        IFGT      7
     C                   EXFMT     WDW01
     C   KL              SETON                                        90
     C                   ENDIF
      *
     C                   ENDIF
       //*** begin add ***  10/06/16  ****************************************
      /free

       If isUseScript;

          xxProcType = processType;
          xxMass = 'Y';

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) (CCCo#:CCFnd) pex00;

          Select;
          When %found and EXCXCD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXCXCD;
          Other;
             xxDart = 'N';
             xxRMCD = ' ';
          Endsl;

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

          If xxSubmit = *off;
             *in90 = *on;
          Endif;

       Endif;

      /end-free
       //*** end add *****  10/06/16  ****************************************
      *
      ***** END ADD ******* 08/03/04 **********************************
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90CCDT          IFGT      SAVEDT
     C     CCDT          OREQ      SAVEDT
     C     CCTIME        ANDGT     SAVETM
      *
     C                   SETON                                        93  90
      *
     C                   ENDIF                                                  CCDT IFGT
      *
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for screen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      CCBAT#        SCBAT#
      *
     C     CCRPMY        IFNE      0
      *
      *** BEG CHG ***** 10/21/08 **************************************
     C                   CLEAR                   SRDTI
      *** END CHG ***** 10/21/08 **************************************
     C                   MOVE      CCRPMY        SRDTI
      *
     C                   CALL      'SRCVTMY'     DTMDYY
      *
     C                   MOVEL     SRDTO         SCRPMY
     C                   MOVE      *ON           *IN40
      *
     C                   ELSE
      *
      ***** BEG CHG ******* 05/02/03 **********************************
     C*************        MOVE CCMIN     SCMIN
     C                   Z-ADD     CCMIN         SCMIN
      ***** END CHG ******* 05/02/03 **********************************
      *** BEG CHG ***** 10/21/08 **************************************
     C                   CLEAR                   SRDTI
      *** END CHG ***** 10/21/08 **************************************
     C                   MOVE      CCBLDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         SCBLDT
     C*
     C***  BEGIN ADD  ***  10/26/99  **********************************
      *** BEG CHG ***** 10/21/08 **************************************
     C                   CLEAR                   SRDTI
      *** END CHG ***** 10/21/08 **************************************
     C                   MOVE      CCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C***  END   ADD  ***  10/26/99  **********************************
     C*
     C                   MOVE      *OFF          *IN40
      *
     C                   ENDIF                                                  CCRPMY IFNE
      *
     C                   MOVE      CCETYP        SCETYP
      *** BEG CHG ***** 10/21/08 **************************************
     C                   CLEAR                   SRDTI
      *** END CHG ***** 10/21/08 **************************************
     C                   MOVE      CCFRDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         SCFRDT
      *** BEG CHG ***** 10/21/08 **************************************
     C                   CLEAR                   SRDTI
      *** END CHG ***** 10/21/08 **************************************
     C                   MOVE      CCTODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         SCTODT
     C                   MOVE      CCRSN         SCRSN
      **** BEGIN ADD **** 12/02/99 ************************************
     C                   MOVE      CCEAFL        SCEAFL
      **** END   ADD **** 12/02/99 ************************************
      ***** BEG DEL ******* 08/03/04 **********************************
     C*********            Z-ADD0         SCDATE
      ***** END DEL ******* 08/03/04 **********************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
      *
      ***** END ADD ******* 08/03/04 **********************************
     C                   Z-ADD     0             SCCDAT
      *
      *** BEG ADD **** 02/18/03 ******************************************
     C     CCFREQ        COMP      *BLANKS                            4242
     C   42CCWEDT        COMP      0                                  4040
      *
     C                   Z-ADD     CCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWEDT
      *
     C   42              SELECT
     C     CCFREQ        WHENEQ    'B'
     C                   MOVEL     'Both'        SCFREQ
     C     CCFREQ        WHENEQ    'W'
     C                   MOVEL     'Weekly'      SCFREQ
     C     CCFREQ        WHENEQ    'M'
     C                   MOVEL     'Monthly'     SCFREQ
     C                   ENDSL
      *** END ADD **** 02/18/03 ******************************************
      *
     C                   ENDSR                                                  MVDBSC
      *
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      *BLANK        CCBPF
     C                   MOVE      Q1USER        CCUSER
     C                   MOVE      Q1PGM         CCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         CCDT
     C                   MOVEL     DSTIME        CCTIME
      *
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
      *
      *   CALL THE CLP TO SUBMIT THE BATCH
      *
     C     *IN41         IFEQ      *OFF
     C                   MOVE      'N'           SCCCRT
     C                   ENDIF                                                  *IN41 IFEQ
      *
      **** BEGIN CHG **** 06/12/98 *********************************************
     C******************** MOVE ##CO#     QQCO#   3
     C******************** MOVE ##FND     QQFND   3
     C                   MOVE      CCCO#         QQCO#             3
     C                   MOVE      CCFND         QQFND             3
      **** END   CHG **** 06/12/98 *********************************************
     C                   MOVE      SCDATE        QQDATE            8            NOTICE DATE
     C                   MOVE      SCCDAT        QQCDAT            8
     C                   MOVE      CCBAT#        QQBAT#            9
      *
      ***** BEG ADD ******* 07/24/08 **********************************
     C***** BEG ADD ******* 09/16/09 **********************************
      *** BEG DELETE *** 05/19/11 *************************************
     C***        CLTID     IFEQ '046'
      *** END DELETE *** 05/19/11 *************************************
     C***** END ADD ******* 09/16/09 **********************************
     C                   SELECT
      *
     C     SCTYPE        WHENEQ    'R'
     C                   MOVE      SCAEB         SCTYPE
      *
     C     SCTYPE        WHENEQ    'L'
     C     SCAEB         ANDEQ     'A'
     C                   MOVE      'F'           SCTYPE
      *
     C     SCTYPE        WHENEQ    'L'
     C     SCAEB         ANDEQ     'E'
     C                   MOVE      'G'           SCTYPE
      *
     C     SCTYPE        WHENEQ    'L'
     C     SCAEB         ANDEQ     'B'
     C                   MOVE      'H'           SCTYPE
      *
     C                   ENDSL
      *
     C***** BEG ADD ******* 09/16/09 **********************************
      *** BEG DELETE *** 05/19/11 *************************************
     C****                 ENDIF
      *** END DELETE *** 05/19/11 *************************************
     C***** END ADD ******* 09/16/09 **********************************
      ***** END ADD ******* 07/24/08 **********************************
      *
     C                   CALL      'W0163C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQDATE
     C                   PARM                    QQCDAT
     C                   PARM                    QQBAT#
     C                   PARM      SCCCRT        QQCCRT            1
     C                   PARM      SCTYPE        QQTYPE            1
     C                   PARM      SCFORM        QQFORM            1
     C                   PARM      ##OUTQ        QQOUTQ           10
     C                   PARM      'D'           QQMODE            1
      ***** BEG ADD ******* 07/03/07 **********************************
     C*
     C                   PARM      SCSRT1        QQSRT1            2
     C                   PARM      SCSRT2        QQSRT2            2
      ***** END ADD ******* 07/03/07 **********************************
      *
     C                   ENDIF                                                  *INKC IFEQ
      *
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      ***** BEG ADD ******* 05/02/03 **********************************
     C     @@RRN         CHAIN     DBRCDF                             99
     C     CCADPP        COMP      'P'                                    43
      ***** END ADD ******* 05/02/03 **********************************
      *
      *** BEG ADD **** 02/18/03 ******************************************
     C     CCFREQ        COMP      *BLANKS                            4242
     C   42CCWEDT        COMP      0                                  4040
      *
     C   42              SELECT
     C     CCFREQ        WHENEQ    'B'
     C                   MOVEL     'Both'        SCFREQ
     C     CCFREQ        WHENEQ    'W'
     C                   MOVEL     'Weekly'      SCFREQ
     C     CCFREQ        WHENEQ    'M'
     C                   MOVEL     'Monthly'     SCFREQ
     C                   ENDSL
      *** END ADD **** 02/18/03 ******************************************
      ***** BEG ADD ******* 12/01/00 **********************************
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCNAME
      ***** END ADD ******* 12/01/00 **********************************
      *
      *   PULL "OV/400 INSTALLED?" FLAG FROM APPLICATION CONTROL FILE.
      *
     C                   MOVEL     'SY'          ##KEY
     C                   MOVE      *BLANK        SYOFIF
      *
     C     SYKY          CHAIN     SSY01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SYOFIF        ANDEQ     'Y'
      *
     C                   MOVE      *ON           *IN41
      *
     C                   ELSE
      *
     C                   MOVE      *OFF          *IN41
      *
     C                   ENDIF                                                  *IN60 IFEQ
      ***** BEG ADD ******* 08/03/04 **********************************
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT            7 0
      ***** BEG ADD ******* 07/03/07 **********************************
     C     CLTID         IFEQ      '129'
     C                   MOVE      'AN'          SCSRT1
     C                   MOVE      'EN'          SCSRT2
     C                   ENDIF
      ***** BEG END ******* 07/03/07 **********************************
      *
      ***** END ADD ******* 08/03/04 **********************************
      ***** BEG ADD ******* 07/24/08 **********************************
     C                   MOVE      *BLANK        SCAEB
      ***** END ADD ******* 07/24/08 **********************************
      ***** BEG ADD ******* 05/08/13 **********************************
     C                   If        SCccrt = ' '
     C                   Eval      SCccrt = 'N'
     C                   Endif
      ***** END ADD ******* 05/08/13 **********************************
       //*** begin add ***  10/06/16  ****************************************
      /free

       isUseScript = UseScriptura(processType);

      /end-free
       //*** end add *****  10/06/16  ****************************************
      *
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
B01  C     *IN99         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   UPDATE    DBRCDF
      *
E01  C                   ENDIF                                                  *IN99 IFEQ
      *
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
