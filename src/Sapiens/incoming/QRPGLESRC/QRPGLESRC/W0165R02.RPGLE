     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0165R02 - CANCELLATION INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE CANCELLATION FILE.  A    *
     H*                STATE REQUIRED NOTIFICATION FORM IS PRINTED    *
     H*                ALONG WITH AN OPTIONAL  LETTER TO CERTIFICATE  *
     H*                HOLDERS.                                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0165R - WORK WITH CANCELLATIONS                           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W2196R - WORK WITH CANCELLATION REASON CODES               *
     H*    W7165C - CANCELLATION NOTICE SUBMITTER                     *
     H*    WA007R - 'PRINT' MESSAGE API                               *
     H*    SRCENTER                                                   *
     H*    SRCVTDAT                                                   *
     H*    WA005R - COMPRESS EMP#/NAME API                            *
     H*    WA002R - AGENT/PRODUCER INFO API                           *
     H*                                                               *
     H*  CREATED: 06/12/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/17/11 C:190  Project: WC2019                               *
     H*                 Pull the cancellation notice days and         *
     H*                 additional mailing days from the new          *
     H*                 fields for new policies.                      *
     H*                                                               *
     H* 02/17/09 C:61  Seq#: WA139/67254                              *
     H*                Correct get CX date.                           *
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA099/44610                              *
     H*                Release record lock                            *
     H*                                                               *
     H* 05/25/06 C:61  SUPPORT WA129 51399                            *
     H*                Do not write WDCXP record for renewal year if  *
     H*                F12=Cancel or F3=Exit or DELETE mode or        *
     H*                DISPLAY mode.                                  *
     H*                                                               *
     H* 07/25/05 C:61  PROJECT 90WDC                                  *
     H*              - Recalculate number of CX if not processing     *
     H*                revision or reprint.  Display '# of CX' in     *
     H*                header info.                                   *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 08/03/04 C:61  PROJECT WC966                                  *
     H*              - Add information window to indicate when        *
     H*                automatic cancellation of renewal years will   *
     H*                be performed.                                  *
     H*                                                               *
     H* 06/23/04 C:61  SUPPORT WA093 42840                            *
     H*                Verify entered CX reason code is valid for     *
     H*                policy year rating state of every employer     *
     H*                division included in cancellation processing.  *
     H*                                                               *
     H* 03/12/04 C:61  SUPPORT WA081 41582                            *
     H*                Eliminate possiblity of writing WDCXP WDXHP    *
     H*                records with incorrect policy year.            *
     H*                                                               *
     H* 02/20/04 C:33  Support: WA081/41263                           *
     H*                Update employer master cx date(s).             *
     H*                                                               *
     H* 08/28/03 C:61  PROJECT WC577                                  *
     H*                Warning edit produced if revised cancellation. *
     H*                "WCPOLS reporting does not allow revisions to  *
     H*                 cancellations."                               *
     H*                                                               *
     H* 04/07/03 C:61  PROJECT 129WAG                                 *
     H*                Load rating state to CX Detail WDCXP.          *
     H*                                                               *
     H* 12/23/02 C:61  SUPPORT WA101 35269                            *
     H*                Unlock record after chain wdxhl if not write.  *
     H*                                                               *
     H* 12/09/02 C:61  SUPPORT WA101 35024                            *
     H*                Get CX reason description using CX issue date  *
     H*                when not in ADD mode.                          *
     H*                                                               *
     H* 10/10/02 C:61  SUPPORT WA093 34433                            *
     H*                When cancel date is less than accident date,   *
     H*                show window message but condition option       *
     H*                to override on user security level.            *
     H*                                                               *
     H* 01/11/02 C:61  SUPPORT WA033 30718                            *
     H*                If CX being revised has issue date CXCDAT in   *
     H*                future, the issue date can not be changed.     *
     H*                                                               *
     H* 12/12/01 C:61  SUPPORT WA045 30139                            *
     H*                If CX being revised has issue date in the      *
     H*                future, the issue date can not be changed.     *
     H*                                                               *
     H* 11/21/01 C:90  PROJECT: SEQ#29586                             *
     H*                Load SVRSN (save reason code) field when       *
     H*                selected thru a search (F10).                  *
     H*                                                               *
     H* 10/09/01 C:90  PROJECT: SEQ#29259                             *
     H*                Moved edit to auto-load the reinstatement fee  *
     H*                when reason code is zero to not be within date *
     H*                edit.                                          *
     H*                                                               *
     H* 07/24/01 C:90  PROJECT: WC577                                 *
     H*                Automatically load the reinstatement fee when  *
     H*                the reason code is keyed and the fee is zero.  *
     H*                                                               *
     H* 01/18/01 C:61  SUPPORT WA093 25921                            *
     H*              - Check for existing claims across all divisions.*
     H*                                                               *
     H* 11/10/00  C:87  PROJECT: SUPP WA999 #24603                    *
     H*                 REVISE ALL DIVISIONS WHEN PROCESSING A        *
     H*                 REVISED CANCELLATION.                         *
     H*                                                               *
     H* 10/12/99  C:87 PROJECT: SUPP WA101 #20065                     *
     H*                Array index error caused by not looking at the *
     H*                specific emp dtl rec for each div.             *
     H*                                                               *
     H*  09/07/99 C:61 PROJECT WC927                                  *
     H*              - Default Reinstatement Fee amount from WTCRP    *
     H*                after selection from F10 reason code search.   *
     H*              - Ignore inactive cancellation reason codes.     *
     H*                                                               *
     H*  09/02/99 C:61 PROJECT WC577                                  *
     H*                Add Activity Status Description to header      *
     H*                info.                                          *
     H*                                                               *
     H* 08/10/99  C:87   PROJECT: CM046                               *
     H*                  Load "Deleted Division" to cancellation rsn  *
     H*                  desc if CXENFL='Y'.  Clear CXENFL and XHENFL *
     H*                  when writing or updating WDCXP or WDXHP.     *
     H*                                                               *
     H* 07/29/99  C:87   PROJECT: WC757                               *
     H*                  Display pol eff date and pol exp date in the *
     H*                  screen header.  If the emp being cancelled   *
     H*                  has multiple divs not cancelled display pop  *
     H*                  up window advising all divs will be cancld.  *
     H*                  Loop through all divs in PGMEND and process  *
     H*                  all divs for cancellation that have not been *
     H*                  cancelled or have a canx date > canx date    *
     H*                  entered on the screen.  Remove option 4=Dlt  *
     H*                  code.  Remove code which sets indicators 66- *
     H*                  69 off.  Add code to check that employer is  *
     H*                  still active after screen has been processed.*
     H*                  Make 'Address labe type' field optional.     *
     H*                                                               *
     H* 07/21/99  C:87   PROJECT: SUPP WA081 #18950                   *
     H*                  If the policy was cancelled because of a     *
     H*                  missing S/R month (CXRPMY <> 0) then show it *
     H*                  on the screen instead of amount due.  Add    *
     H*                  appropriate processing.                      *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H* 04/08/99  C:87   PROJECT: SUPPORT WA096 #17125                *
     H*                  Compiled pgm because error msg associated w/ *
     H*                  *in34 was chgd from WCR1061 to WC30181.      *
     H*                                                               *
     H* 02/24/99  C:87   PROJECT: SUPPORT TASB #NONE                  *
     H*                  Change to use the current date as opposed to *
     H*                  the policy eff date when reading the canx    *
     H*                  reason code table file(WTCRP).               *
     H*                                                               *
     H* 02/23/99  C:87   PROJECT: SUPPORT WA096 #16485                *
     H*                  This program was looking at the old v2       *
     H*                  security level.  Changed to look at admin/   *
     H*                  DARTS (WMEXP) file.                          *
     H*                                                               *
     H* 01/07/99  C:35   PROJECT: SUPPORT WA101 15861                 *
     H*                  RE-CHAIN TO CORRECT EMPLOYER DETAIL RECORD   *
     H*                  AFTER COUNTING DIVISIONS.                    *
     H*                                                               *
     H* 12/10/98  C:87   PROJECT: SUPPORT WA095 #15403                *
     H*                  Chain to rate set code file to get the       *
     H*                  jurisdiction state when not in add mode.     *
     H*                                                               *
     H* 12/03/98  C:35   PROJECT: SUPPORT WA081 #15363                *
     H*                  INITIALIZE CONFIRMATION FLAG, CONFIRMATION   *
     H*                  DATE, CONFIRMATION PROCESSED DATE AND        *
     H*                  CANCELLED IN ERROR FLAG TO BLANKS AND ZEROS  *
     H*                  WHEN WRITING A NEW RECORD SO THAT THESE      *
     H*                  VALUES ARE NOT CARRIED FORWARD FROM OTHER    *
     H*                  RECORDS.                                     *
     H*                                                               *
     H* 11/10/98  C:87  PROJECT: WC852                                *
     H*                 Change the prompt screen to display'Leave this*
     H*                 date blank to use the default calculation'    *
     H*                 next to the canx eff dt.  If the field is left*
     H*                 blank then calculate the canx eff dt as in    *
     H*                 Mass Canx process (W0164R) and display a pop  *
     H*                 up window for user approval of date.  Pass    *
     H*                 emp jurisdiction state for canx reason code   *
     H*                 search. Use WTCRL instead of WTABL for canx   *
     H*                 reason code file.                             *
     H*                                                               *
     H* 09/18/98  C:61  PROJECT 48WLC                                 *
     H*                 Display old Emp#/Div from Employer Cross      *
     H*                 Reference (WZEXP).                            *
     H*                                                               *
     H* 08/17/98  C:35  PROJECT: SUPPORT                              *
     H*                 USE THE POLICY EFFECTIVE DATE TO RETRIEVE THE *
     H*                 STATE CX/REIN DEFINITION RECORD INSTEAD OF    *
     H*                 THE CURRENT DATE.                             *
     H*                                                               *
     H* 07/22/98  C:108 PROJECT SUPPORT: WA093/12777                  *
     H*                 CHANGE CLAIMS-ONLY FLAG FROM "Y" TO "C".      *
     H*                                                               *
     H* 06/01/98  C:35  PROJECT: SUPPORT WA091 11873                  *
     H*                 DELETE CANCELLATION HISTORY RECORDS WHEN A    *
     H*                 CANCELLATION IS DELETED.  ALSO CORRECT CODE   *
     H*                 THAT WAS RUNNING A CANCELLATION IF OPTION 4   *
     H*                 WAS USED TO DELETE A CANCELLATION.            *
     H*                                                               *
     H* 04/03/98  C:78  PROJECT: WC825                                *
     H*                 ALLOW ACCESS TO DARTS. F14 ADDED.             *
     H*                                                               *
     H* 12/29/97  C:35  PROJECT: WC735                                *
     H*                 REMOVE CODE WHICH CHANGED MODE FROM 'CHANGE'  *
     H*                 TO 'DISPLAY' IN THE CASE OF A REINSTATED      *
     H*                 CANCELLATION.                                 *
     H*                                                               *
     H* 12/24/97  C:35  PROJECT: WC735                                *
     H*                 CONVERT CX DATE SO THAT IT DISPLAYS CORRECTLY *
     H*                 AFTER REASON CODE SEARCH.                     *
     H*                                                               *
     H* 11/06/97  C:85  PROJECT: WC764                                *
     H*                 VERSION 3 REPORT STANDARDS.                   *
     H*                 ADDED DIVISION COUNT TO SCR01.                *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - ENROLLED IN OV/400                                      *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  66 - COMMERCIAL APPLICATION INSTALLED (WCFL07)               *
     H*  67 - CLAIMS ONLY INSTALLED (WCFL08)                          *
     H*  68 - RETRO/LOSS PGM INSTALLED (WCFL10)                       *
     H*  69 - DEDUCTIBLES INSTALLED (WCFL06)                          *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H* N45 - Display reinstatement flag                              *
     H*  ERROR:                                                       *
     H*  20 - (WCR0500) Invalid cancel date                           *
     H*  21 - (WCR0501) Invalid certificate holder flag               *
     H*  22 - (WCR0513) Invalid table (Cancel Reason)                 *
     H*  23 - (WCR0501) Invalid revision flag                         *
     H*  27 -           Account has additional divisions              *
     H*  37 - (WCR0930) Date not between policy dates                 *
     H*  POSITION CURSOR:                                             *
     H*  30 - (SCRSN ) Cancel Reason                                  *
     H*  31 - (SCCOM1) Comment 1                                      *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0165F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDCXP     UF A E             DISK
     F                                     RENAME(PCX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDCXL     IF   E           K DISK
     F***  BEGIN ADD  ***  07/29/99  **********************************
     F                                     INFDS(DBFDSA)
     F***  END   ADD  ***  07/29/99  **********************************
      ***** BEG ADD ******* 07/25/05 **********************************
      *
     FWDCXL10   IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
      *
      *  Cancellation maintenance
     FWTXCL1    IF   E           K DISK
      *
      ***** END ADD ******* 07/25/05 **********************************
      *
      **** BEGIN CHG **** 06/01/98 *********************************************
     F********** WDXHL   IF  E           K        DISK                      A
     FWDXHL     UF A E           K DISK
      **** END   CHG **** 06/01/98 *********************************************
     FWXCNL     IF   E           K DISK
      ***** BEG ADD ******* 09/18/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
      ***** END ADD ******* 09/18/98 **********************************
     FWDELL1    IF   E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
      ***** BEG ADD ******* 08/03/04 **********************************
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL04)
      ***** END ADD ******* 08/03/04 **********************************
     F***  BEGIN CHG  ***  07/29/99  **********************************
     F***WMEML   UF  E           K        DISK
     FWMEML     IF   E           K DISK
     F***  END   CHG  ***  07/29/99  **********************************
     F***  Begin Add  ***  02/20/03  **************************************
     FWMEML3    UF   E           K DISK
     F                                     RENAME(PEM01:PEM03)
     F***  End   Add  ***  02/20/03  **************************************
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMRSL     IF   E           K DISK
     F***  BEGIN DEL  ***  11/10/98  **********************************
     F***WTXCL   IF  E           K        DISK
     F***  END   DEL  ***  11/10/98  **********************************
     F***  BEGIN DEL  ***  07/29/99  **********************************
     F***WMSHL13 UF  E           K        DISK
     F***  END   DEL  ***  07/29/99  **********************************
      ***** BEG CHG ******* 01/18/01 **********************************
     F********** WMCMLC  IF  E           K        DISK
      *
      * Claims master
     FWMCML11   IF   E           K DISK
      ***** END CHG ******* 01/18/01 **********************************
      *
     F***  BEGIN DEL  ***  07/29/99  **********************************
     F***WXC#L1  UF  E           K        DISK
     F***         PCR01                             KRENAMEPC#01
     F***  END   DEL  ***  07/29/99  **********************************
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTABL   IF  E           K        DISK
     F*********** PCN01                             KIGNORE
     F*********** PSY01                             KIGNORE
     F*********** PPC01                             KIGNORE
     F*********** PRC01                             KIGNORE
     F*********** PBC01                             KIGNORE
     F*********** PCS01                             KIGNORE
     F*********** PLM01                             KIGNORE
     F*********** PRS01                             KIGNORE
     F*
     FWTCRL     IF   E           K DISK
     F***  END   CHG  ***  11/10/98  **********************************
     F***  BEGIN ADD  ***  11/10/98  **********************************
     FWMALL     IF   E           K DISK
     F***  END   ADD  ***  11/10/98  **********************************
     F***  BEGIN ADD  ***  02/23/99  **********************************
     FWMEXL     IF   E           K DISK
     F***  END   ADD  ***  02/23/99  **********************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      ***** BEG ADD ******* 09/02/99 **********************************
      *
      * Activity Status Table
     FWTAVL     IF   E           K DISK
      ***** END ADD ******* 09/02/99 **********************************
      *
      *
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D***  BEGIN ADD  ***  07/29/99  **********************************
     D DBFDSA          DS
     D  X@RRN                397    400B 0
     D***  END   ADD  ***  07/29/99  **********************************
     D*
     D OPTNM           C                   CONST('Cancellation Inform-
     D                                     ation')
     D MSG1            C                   CONST('Policy already under-
     D                                      cancellation effect-
     D                                     ive')
     D MSG2            C                   CONST('Policy already cance-
     D                                     lled')
     D***  BEGIN ADD  ***  08/10/99  **********************************
     D DELDIV          C                   CONST('Deleted Division')
     D***  END   ADD  ***  08/10/99  **********************************
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      ****** BEGIN ADD ***** 04/03/98 *********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      ****** END   ADD ***** 04/03/98 *********************************
      *
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              CLAIM PMTS
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
      *
      *
      *==============================================================
      *  KLIST
      *==============================================================
      *
      *  KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  KEY LIST FOR THE GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
      *
      *  KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKYX         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      ***** BEG ADD ******* 09/18/98 **********************************
     C*
     C***  Begin Add  ***  02/20/03  **************************************
     C     EMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFND
     C***  End   Add  ***  02/20/03  **************************************
     C*
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***** END ADD ******* 09/18/98 **********************************
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      *** BEGIN ADD *** 11/06/97 **************************************
      *
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
      *
      *** END   ADD *** 11/06/97 **************************************
      ***** BEG ADD ******* 04/07/03 **********************************
      *
     C     ELKY2         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      ***** END ADD ******* 04/07/03 **********************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCFYR
      *
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      *
     C     ELKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      *
     C     ELKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      *
      ***** END ADD ******* 08/03/04 **********************************
      *
      *
      *  KEY LIST FOR THE CANCELLATION DETAIL FILE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCREQ#
      ***** BEG ADD ******* 07/25/05 **********************************
      *
     C     CXKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    X3DATE
      *
     C     CXKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      *
      *  CANCELLATION/REINSTATEMENT STATE FILE
      *
     C     XCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXJRST
     C                   KFLD                    KKEFDT
      *
     C     XCKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXJRST
      *
      ***** END ADD ******* 07/25/05 **********************************
      *
      *  KEY LIST FOR THE CANCELLATION HISTORY FILE
      *
     C     XHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCREQ#
      *
      *   KEY LIST FOR MASS CANCELLATION NOTICE WORKFILE
      *
     C     CNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      *  KEY LIST FOR THE CANCELLATION/REINSTATEMENT STATE FILE
      *
     C***  BEGIN DEL  ***  11/10/98  **********************************
     C********** XCKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           SCFND
     C**********           KFLD           XXST
     C**********           KFLD           XXEFDT
     C**********
     C********** XCKY2     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           SCFND
     C**********           KFLD           XXST
     C***  END   DEL  ***  11/10/98  **********************************
      *
      *  KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
      *
      ***** BEG ADD ******* 04/07/03 **********************************
      *
     C     RSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      ***** END ADD ******* 04/07/03 **********************************
     C***  BEGIN CHG  ***  11/10/98  **********************************
      *  TABLE FILE
      **********
     C********** TBKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXKEY
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C***  END   CHG  ***  11/10/98  **********************************
      ***** BEG ADD ******* 07/25/05 **********************************
      *
     C     CRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
      *
     C     CRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    KKKEY
      *
      ***** END ADD ******* 07/25/05 **********************************
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
     C***  BEGIN DEL  ***  07/29/99  **********************************
      * KEY LIST FOR THE WCPOLS SUBMISSION HISTORY FILE
      **********
     C********** SHKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           SCFND
     C**********           KFLD           SCEMP#
     C**********           KFLD           SCDIV
     C**********           KFLD           SCFYR
     C**********           KFLD           XXTYPE
      **********
      **KEY*LIST FOR THE WCPOLS CANCEL/REINSTATE TRANSACTION FILE
      **********
     C********** C#KY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           SCFND
     C**********           KFLD           SCEMP#
     C**********           KFLD           SCFYR
     C**********           KFLD           XXUNID
     C***  END   DEL  ***  07/29/99  **********************************
      *
      * KEY LIST FOR THE CLAIM MASTER FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      ***** BEG DEL ******* 01/18/01 **********************************
     C*************        KFLD           SCDIV
      ***** END DEL ******* 01/18/01 **********************************
     C                   KFLD                    XXACDT
      *
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      ***** BEG DEL ******* 01/18/01 **********************************
     C*************        KFLD           SCDIV
      ***** END DEL ******* 01/18/01 **********************************
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Additional entities/locations file
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C***  END   ADD  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  02/23/99  **********************************
     C*  Admin/DARTS control defn file
     C     XXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C***  END   ADD  ***  02/23/99  **********************************
      ***** BEG ADD ******* 09/02/99 **********************************
      *
      *  Activity Status
      *
     C     AVKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
      *
      ***** END ADD ******* 09/02/99 **********************************
      *
      ***** BEG ADD ******* 08/03/04 **********************************
      *
      *
      *===============================================================
      *  *LIKE
      *===============================================================
      ***** BEG ADD ******* 07/25/05 **********************************
     C     *LIKE         DEFINE    CXCFDT        X3DATE
     C     *LIKE         DEFINE    XCST          XXJRST
      ***** END ADD ******* 07/25/05 **********************************
     C     *LIKE         DEFINE    CXFYR         S1FYR
     C     *LIKE         DEFINE    CXREQ#        S1REQ#
      ***** END ADD ******* 08/03/04 **********************************
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CXCXDT        XXRNDT
     C     *LIKE         DEFINE    CRKEY         XXKEY
     C     *LIKE         DEFINE    CXREDT        XXREDT
     C     *LIKE         DEFINE    CXCXDT        XXCXDT
     C***  BEGIN DEL  ***  11/10/98  **********************************
     C********** *LIKE     DEFN XCEFDT    XXEFDT
     C********** *LIKE     DEFN XCST      XXST
     C***  END   DEL  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C***  BEGIN DEL  ***  07/29/99  **********************************
     C********** *LIKE     DEFN SHTYPE    XXTYPE
     C********** *LIKE     DEFN SHUNID    XXUNID
     C***  END   DEL  ***  07/29/99  **********************************
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CXCXDT        XXDATE
      ******** BEGIN ADD **** 04/03/98 ********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ******** END   ADD **** 04/03/98 ********************************
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    CREFDT        XXEFDT
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CRST          XXST
     C***  END   ADD  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C     *LIKE         DEFINE    EMDIV         SAVDIV
     C     *LIKE         DEFINE    CXREQ#        SVREQ#
     C***  END   ADD  ***  07/29/99  **********************************
      ***** BEG ADD ******* 09/02/99 **********************************
     C     *LIKE         DEFINE    AVKEY         KKKEY
      ***** END ADD ******* 09/02/99 **********************************
      **** BEGIN ADD **** 07/24/01 ************************************
     C     *LIKE         DEFINE    SCRSN         SVRSN
      **** END   ADD **** 07/24/01 ************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
      ***** BEG CHG ******* 08/03/04 **********************************
     C****************     PARM           ##FYR   30
     C                   PARM      ELFYR         ##FYR             3 0
      ***** END CHG ******* 08/03/04 **********************************
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
     C     SRCHPL        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      *
     C/COPY SCOPYMT01C
      *
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C*===============================================================
     C* CHKACT - Checks that employer can be cancelled.
     C*===============================================================
     C     CHKACT        BEGSR
     C*
     C*  Get Employer master record
     C     EMKY          CHAIN(N)  PEM01                              60
     C*
     C*  The employer must be effective in order to cancel,
     C*  otherwise send an error message displaying the current
     C*  cancel date
     C     @@MODE        IFEQ      'A'
     C*
     C                   MOVEA     '000'         *IN(24)
     C                   Z-ADD     0             SCCX1
     C                   Z-ADD     0             SCCX2
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C*
     C     CNKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN26
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  60
     C  N60              Z-ADD     5             X
     C   60              SUB       1             X
     C*
     C  N26X             IFNE      *ZERO
     C*
     C     CD(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCX1
     C*
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCX2
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C     CD(X)         COMP      XXRNDT                               24
     C   24              MOVEL     MSG2          SCMSG2
     C  N24              MOVEL     MSG1          SCMSG1
     C  N24              MOVE      *ON           *IN25
     C*
     C                   ENDIF                                                  CD,X<>*ZERO
     C*
     C                   ENDIF                                                  X<>*ZERO
     C*
     C     *IN24         IFEQ      *ON
     C     *IN25         OREQ      *ON
     C     *IN26         OREQ      *ON
     C*
     C                   EXFMT     WDW01
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDIF                                                  *IN24,25,26=*ON
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   ENDSR                                                  CHKACT
     C***  END   ADD  ***  07/29/99  **********************************
      ***** BEG ADD ******* 06/23/04 **********************************
      *
      *
      *
      *===============================================================
      * CHKRSN - Check CX reason code valid for other employer divisons
      *===============================================================
     C     CHKRSN        BEGSR
      *
     C                   Z-ADD     SCDIV         SAVDIV
     C                   SETOFF                                       71
      *
      *   Cycle through all divisions
      *
     C     EMKYX         CHAIN(N)  PEM01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     *IN71         ANDEQ     *OFF
      *
     C     EMDIV         IFNE      SAVDIV
     C                   Z-ADD     EMDIV         SCDIV
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRGETX
      *
     C     CD(X)         IFEQ      *ZERO
     C     CD(X)         ORGT      XXCXDT
     C     SCREV         OREQ      'Y'
     C     RSKY1         CHAIN     PRS01                              60
      *
      *  VALIDATE REASON CODE
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SCRSN         XXKEY
     C                   MOVE      RSJRST        KKST
     C                   Z-ADD     XXRNDT        KKEFDT
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *ON
     C     CRSTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  RSKY
      *
     C                   ENDIF                                                  CD,X=*ZERO
      *
     C                   ENDIF                                                  ELKY
      *
     C                   ENDIF                                                  EMDIV<>SAVDIV
      *
     C     EMKYX         READE(N)  PEM01                                  60
     C                   ENDDO
      *
      *  Reset files
     C                   Z-ADD     SAVDIV        SCDIV
     C     EMKY          CHAIN(N)  PEM01                              60
     C     ELKY          CHAIN     PEL01                              60
      *
     C                   ENDSR                                                  CHKRSN
      *
      ***** END ADD ******* 06/23/04 **********************************
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C***  BEGIN DEL  ***  07/29/99  **********************************
      **********
      **CLEAR*THE CANCELLATION DATE FROM THE EMPLOYER MASTER FILE
      **********
     C********** EMKY      CHAINPEM01                60
      **********
     C**********           Z-ADD1         X
     C********** *ZERO     LOKUPED,X                     60
      **********
     C********** *IN60     IFEQ *OFF
     C**********           Z-ADD5         X
     C**********           ELSE
     C**********           SUB  1         X
     C**********           ENDIF                           *IN60 IFEQ *OFF
      **********
     C********** X         IFNE 0
     C**********           Z-ADD0         CD,X
     C**********           ENDIF                           X IFNE 0
      **********
     C**********           UPDATPEM01
      **********
      **DELETE*THE WCPOLS TRANSACTION RECORDS
      **********
     C**********           MOVE 'C'       XXTYPE
      **********
     C********** SHKY      CHAINPSH01                60
      **********
     C********** *IN60     IFEQ *OFF
      **********
     C**********           MOVE SHUNID    XXUNID
      **********
     C********** C#KY      CHAINPC#01                60
      **********
     C********** *IN60     IFEQ *OFF
     C**********           DELETPC#01
     C**********           ENDIF                           *IN60 IFEQ *OFF
      **********
     C**********           DELETPSH01
      **********
     C**********           ENDIF                           *IN60 IFEQ *OFF
      *****BEGIN ADD **** 06/01/98 *********************************************
      **********
      **DELETE*CANCELLATION HISTORY RECORDS
      **********
     C**********           MOVE CXFND     SCFND
     C**********           MOVE CXEMP#    SCEMP#
     C**********           MOVE CXDIV     SCDIV
     C**********           MOVE CXFYR     SCFYR
     C**********           MOVE CXREQ#    SCREQ#
      **********
     C********** XHKY      CHAINPXH01                60
      **********
     C********** *IN60     DOWEQ*OFF
      **********
     C**********           DELETPXH01
     C********** XHKY      READEPXH01                    60
      **********
     C**********           ENDDO                           *IN60 DOWEQ *OFF
      **********
      **** END   ADD **** 06/01/98 *********************************************
      **********
     C***  END   DEL  ***  07/29/99  **********************************
      *
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
      ***** BEG ADD ******* 07/25/05 **********************************
     C   40              EXSR      SRCNC#
      ***** END ADD ******* 07/25/05 **********************************
     C*
     C**** BEGIN ADD **** 10/09/01 ************************************
     C     SVRSN         IFNE      SCRSN
      *
     C                   MOVEL(P)  SCRSN         XXKEY
     C                   MOVE      XXST          KKST
     C                   Z-ADD     XXRNDT        KKEFDT
      ***** BEG ADD ******* 12/09/02 **********************************
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 12/09/02 **********************************
     C                   MOVEL     SCRSN         SVRSN
      *
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
     C                   Z-ADD     CRRFEE        SCRFEE
     C                   SETON                                        31  90
     C                   ENDIF
     C**** END   ADD **** 10/09/01 ************************************
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Validate if not blank, else calculate date
     C     SCCXDT        IFNE      *ZERO
     C***  END   ADD  ***  11/10/98  **********************************
      *
      *  VALIDATE CANCEL DATE
      *
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCXDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        20  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   ELSE                                                   SCCXDT<>0
     C*
     C*  Validate the reason code
     C                   MOVEL(P)  SCRSN         XXKEY
     C                   MOVE      XXST          KKST
     C***  BEGIN CHG  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
     C                   Z-ADD     XXRNDT        KKEFDT
      ***** BEG ADD ******* 12/09/02 **********************************
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 12/09/02 **********************************
     C***  END   CHG  ***  02/24/99  **********************************
     C*
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 09/07/99 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 09/07/99 **********************************
     C     *IN60         IFEQ      *ON
     C     CRSTAT        OREQ      'I'
     C                   SETON                                        22  90
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C**** BEGIN ADD **** 07/24/01 ************************************
     C**** BEGIN DEL **** 10/09/01 ************************************
     C********** SVRSN     IFNE SCRSN
     C********** SCRFEE    ANDEQ*ZERO
     C**********           Z-ADDCRRFEE    SCRFEE
     C**********           SETON                         90
     C**********           ENDIF
     C**** END   DEL **** 10/09/01 ************************************
     C**** END   ADD **** 07/24/01 ************************************
     C*
     C  N90              EXSR      SRCALC
     C*
     C                   ENDIF                                                  SCCXDT<>0
     C***  END   ADD  ***  11/10/98  **********************************
      *
      *  ENSURE CANCELLATION DATE IS WITHIN POLICY PERIOD.
      *
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCXDT
      *
     C  N90XXCXDT        IFLT      ELPFDT
     C     XXCXDT        ORGT      ELPTDT
     C                   SETON                                        37  90
     C                   ENDIF                                                  ELPFDT>=XXCXDT>=ELPT
      *
      * CHECK FOR OPEN CLAIMS OR CLOSED CLAIMS WITH PAYMENTS, WHICH
      * HAVE AN ACCIDENT DATE GREATER THAN OR EQUAL TO THE CANCEL DATE
      *
     C  N90XXCXDT        IFLE      XXRNDT
     C     SCEOVR        ANDNE     'Y'
      *
     C                   MOVE      XXCXDT        XXACDT
      *
     C     CMKY          SETLL     PCM01
     C     CMKY1         READE     PCM01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     SCEOVR        ANDNE     'Y'
     C     *IN34         ANDEQ     *OFF
      *
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     CMSTAT        IFNE      'C'
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
      *
      *  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY
      *
     C                   MOVE      'N'           SCEOVR
      *
     C***  BEGIN CHG  ***  02/23/99  **********************************
     C********** ##LVL     IFGE WCSL03
     C**********           EXFMTWDWMSG
     C**********           ENDIF                           ##LVL IFGE WCSL03
     C*
     C     XXKY          CHAIN     PEX01                              60
      ***** BEG DEL ******* 10/10/02 **********************************
     C*****      ##LVL     IFGE EXOVRL
      ***** END DEL ******* 10/10/02 **********************************
      ***** BEG ADD ******* 10/10/02 **********************************
     C                   SETOFF                                       51
      *
     C     *IN60         IFEQ      *OFF
     C     ##LVL         ANDGE     EXOVRL
     C                   SETON                                        51
     C                   ENDIF                                                  ##LVL >= EXOVRL
      *
      ***** END ADD ******* 10/10/02 **********************************
     C                   EXFMT     WDWMSG
      *
      ***** BEG DEL ******* 10/10/02 **********************************
     C*********            ENDIF                           ##LVL >= EXOVRL
     C*****END*  CHG  ***  02/23/99  **********************************
      ***** END DEL ******* 10/10/02 **********************************
      *
     C     SCEOVR        IFEQ      'N'
     C                   SETON                                        9034
     C                   ENDIF                                                  SCEOVR IFEQ 'N'
      *
     C                   ENDIF                                                  CMSTAT IFNE 'C'
      *
     C     CMKY1         READE     PCM01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   ENDIF                                                  XXCXDT IFLE XXRNDT
      *
      *
      *  VALIDATE REASON CODE
      *
     C                   MOVEL(P)  SCRSN         XXKEY
      *
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** TBKY ***  CHAINPCR01                60
     C* N60 *************  MOVELCRDESC    SCRSND    P
     C*
     C                   MOVE      XXST          KKST
     C***  BEGIN ADD  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
     C                   Z-ADD     XXRNDT        KKEFDT
      ***** BEG ADD ******* 12/09/02 **********************************
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 12/09/02 **********************************
     C***  END   ADD  ***  02/24/99  **********************************
     C*
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C*
     C***  END   CHG  ***  11/10/98  **********************************
      ***** BEG ADD ******* 09/07/99 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 09/07/99 **********************************
      *
     C     *IN60         IFEQ      *ON
     C     CRSTAT        OREQ      'I'
     C                   SETON                                        22  90
     C                   ENDIF                                                  *IN60 IFEQ *ON
      ***** BEG ADD ******* 06/23/04 **********************************
      *
      * Check CX reason code valid for other divisons
      *
     C  N90              EXSR      CHKRSN
      ***** END ADD ******* 06/23/04 **********************************
      *
      * REINSTATEMENT FEE CANNOT BE LESS THAN ZERO
      *
     C  N90SCRFEE        IFLT      0
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCRFEE IFLT 0
      *
      * AMOUNT DUE CANNOT BE LESS THAN ZERO
      *
     C  N90SCAMTD        IFLT      0
     C                   SETON                                        32  90
     C                   ENDIF                                                  SCAMTD IFLT 0
     C*
     C***  BEGIN ADD  ***  07/21/99  **********************************
     C     *IN90         IFEQ      *OFF
     C     SCRPMY        ANDNE     *ZERO
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        35  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C***  END   ADD  ***  07/21/99  **********************************
      *
      *  GENERATE CERTIFICATE HOLDERS LETTERS MUST BE 'Y' OR 'N'
      *
     C  N90
     CAN 41SCCCRT        IFNE      'Y'
     C     SCCCRT        ANDNE     'N'
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCCCRT<>Y/N
      *
      *  REVISION FLAG MUST BE 'Y' OR 'N'
      *
     C  N90SCREV         IFNE      'Y'
     C     SCREV         ANDNE     'N'
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCREV<>Y/N
      *
      * REVISED FLAG MUST BE 'Y' IF CANCEL DATE, REASON, COMMENTS OR
      * AMOUNT DUE WAS CHANGED.
      *
     C  N90*IN40         IFEQ      *OFF
     C     SCREV         ANDEQ     'N'
      *
     C     XXCXDT        IFNE      CXCXDT
     C     SCRSN         ORNE      CXRSN
     C     SCCOM1        ORNE      CXCOM1
     C     SCCOM2        ORNE      CXCOM2
     C     SCAMTD        ORNE      CXAMTD
     C***  BEGIN ADD  ***  07/21/99  **********************************
     C     SCRPMY        ORNE      SVRPMY
     C***  END   ADD  ***  07/21/99  **********************************
      *
     C                   SETON                                        33  90
      *
     C                   ENDIF                                                  XXCXDT IFNE CXCXDT
      *
     C                   ENDIF                                                  *IN40 IFEQ *OFF
      ***** BEG ADD ******* 08/28/03 **********************************
      *
      * WARNING if Revised Cancellation
      *
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C     SCREV         ANDEQ     'Y'
     C                   EXFMT     WDW04
     C   KL              SETON                                        90
     C   KL              SETOFF                                       KL
     C                   ENDIF                                                  *IN90
      *
      ***** END ADD ******* 08/28/03 **********************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
      *  Display window if renewal year processing to be done
      *
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C     SCREV         ANDNE     'Y'
     C     ELKY3         SETGT     PEL03
     C     ELKY4         READE     PEL03                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   EXFMT     WDW05
     C   KL              SETON                                        90
     C   KL              SETOFF                                       KL
     C                   ENDIF                                                  *IN90
      *
     C     @@RRN2        CHAIN     WDELP                              60
     C                   ENDIF
      *
      ***** END ADD ******* 08/03/04 **********************************
      *
      * ADDRESS TYPE MUST BE 'R' OR 'L' (if not *blank)
      *
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C     SCTYPE        IFNE      *BLANK
     C***  END   ADD  ***  07/29/99  **********************************
     C  N90SCTYPE        IFNE      'R'
     C     SCTYPE        ANDNE     'L'
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCTYPE IFNE 'R'
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C                   ENDIF                                                  SCTYPE<>*BLANK
     C***  END   ADD  ***  07/29/99  **********************************
     C*
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C* Call subroutine to check if account can be cancelled
     C  N90              EXSR      CHKACT
     C*
     C* Check for other divs eligible for cancellation/revision
     C  N90              EXSR      SROTHD
     C***  END   ADD  ***  07/29/99  **********************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK3
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   MOVEL     *BLANKS       XXKEY
      *
     C                   SELECT
      *
      *  CANCELLATION REASON
      *
     C     HFLD          WHENEQ    'SCRSN'
      *
     C                   CALL      'W2196R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    XXKEY
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   PARM      XXST          ##ST              2
     C***  END   ADD  ***  11/10/98  **********************************
     C*
      *
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL(P)  XXKEY         SCRSN
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** TBKY      CHAINPCR01                60
     C**********           MOVELCRDESC    SCRSND    P
     C*
     C                   MOVE      XXST          KKST
     C***  BEGIN ADD  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
     C                   Z-ADD     XXRNDT        KKEFDT
      ***** BEG ADD ******* 12/09/02 **********************************
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 12/09/02 **********************************
     C***  END   ADD  ***  02/24/99  **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 09/07/99 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 09/07/99 **********************************
     C                   MOVEL(P)  CRDESC        SCRSND
     C***  END   CHG  ***  11/10/98  **********************************
      ***** BEG ADD ******* 09/07/99 **********************************
     C                   Z-ADD     CRRFEE        SCRFEE
      ***** END ADD ******* 09/07/99 **********************************
      **** BEGIN ADD **** 11/21/01 ************************************
     C                   MOVEL     SCRSN         SVRSN
      **** END   ADD **** 11/21/01 ************************************
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   ENDIF                                                  XXKEY<>*BLANKS
      *
     C                   OTHER
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      ***  BEGIN ADD  ***  04/03/98  *********************************
      *===============================================================
      * KNDO - SUBROUTINE FOR F14
      *===============================================================
      *
      *  F14=DARTS
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      ***  END   ADD  ***  04/03/98  *********************************
      *
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C  N40              MOVE      CXREQ#        SCREQ#
      *
     C     *IN40         IFEQ      *ON
      *
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     0             SCRFEE
     C                   Z-ADD     0             SCAMTD
      *
      *    GET CX/REIN BY STATE RECORD
      *
     C                   MOVE      ELRTCD        XXRTCD
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      RSJRST        XXST
     C                   TIME                    DSDTTM
      **** BEGIN CHG **** 08/17/98 *********************************************
      ******************** Z-ADDDSDATE    SRDTI
      ********************
      ******************** CALL 'SRCVTDAT'DTCYMD
      ********************
      ******************** MOVELSRDTO     XXEFDT
     C                   Z-ADD     ELPFDT        XXEFDT
      **** END   CHG **** 08/17/98 *********************************************
      *
     C***  BEGIN DEL  ***  11/10/98  **********************************
     C* ADD NOTICE DAYS AND ADDITIONAL MAILING DAYS TO CURRENT DATE TO
     C* GET DEFAULT CANCELLATION DATE.  IF COMPUTED DATE IS OUTSIDE OF
     C* THE POLICY RANGE, DO NOT DEFAULT THE DATE.
     C**********
     C********** XCKY      SETGTPXC01
     C********** XCKY2     REDPEPXC01                    60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********
     C**********           MOVE XCRFEE    SCRFEE
     C********** XCADDM    ADD  XCNOTC    QQDAYS
     C**********           Z-ADDDSDATE    SRDTI
     C**********
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********
     C**********           MOVELSRDTO     QQDATE
     C**********
     C**********           CALL 'SRFUTR8'
     C**********           PARM           QQDATE  80
     C**********           PARM           QQDAYS  50
     C**********           PARM           QQERR   1
     C**********
     C********** QQERR     IFNE '1'
     C**********
     C**********           MOVE QQDATE    SCCXDT
     C**********           Z-ADDSCCXDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXCXDT
     C**********
     C********** XXCXDT    IFLT ELPFDT
     C********** XXCXDT    ORGT ELPTDT
     C**********           Z-ADD0         SCCXDT
     C**********           ENDIF                           XXCXDT IFLT ELPFDT
     C**********
     C**********           ENDIF                           QQERR IFNE
     C**********
     C**********           ENDIF                           *IN60 IFEQ
     C***  END   DEL  ***  11/10/98  **********************************
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ELSE
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C*
      ***** BEG DEL ******* 01/11/02 **********************************
      ***** BEG ADD ******* 12/12/01 **********************************
      *********
      ***Do*not allow future CX issue date to be revised
      *********
     C*********  @@MODE    IFEQ 'C'                        CHANGE MODE
     C*********  CXCXDT    ANDGTXXRNDT
     C*********            SETON                     46
     C*********            ENDIF                           CHANGE MODE
      *********
      ***** END ADD ******* 12/12/01 **********************************
      ***** END DEL ******* 01/11/02 **********************************
     C***  BEGIN ADD  ***  12/10/98  **********************************
     C*    GET CX/REIN BY STATE RECORD
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ENDIF
     C***  END   ADD  ***  12/10/98  **********************************
      *
     C                   ENDIF                                                  *IN40 IFEQ
      *
     C                   MOVE      CXREIN        SCREIN
     C                   MOVE      CXCCRT        SCCCRT
      *
     C                   MOVE      CXRSN         SCRSN
     C                   MOVEL     CXRSN         XXKEY
     C                   MOVEL     *BLANKS       SCRSND
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** TBKY      CHAINPCR01                60
     C* N60 **********     MOVELCRDESC    SCRSND    P
     C*
     C                   MOVE      XXST          KKST
     C***  BEGIN ADD  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
     C                   Z-ADD     XXRNDT        KKEFDT
      ***** BEG ADD ******* 12/09/02 **********************************
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 12/09/02 **********************************
     C***  END   ADD  ***  02/24/99  **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 09/07/99 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 09/07/99 **********************************
     C  N60              MOVEL(P)  CRDESC        SCRSND
     C***  END   CHG  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C     CXENFL        IFEQ      'Y'
     C                   MOVEL(P)  DELDIV        SCRSND
     C                   ENDIF
     C***  END   ADD  ***  08/10/99  **********************************
      *
     C                   MOVEL     CXCOM1        SCCOM1
     C                   MOVEL     CXCOM2        SCCOM2
     C   40              MOVE      'N'           SCREV
     C  N40              MOVE      'Y'           SCREV
     C  N40              MOVE      CXRFEE        SCRFEE
     C  N40              MOVE      CXAMTD        SCAMTD
     C                   MOVE      'N'           SCEOVR
     C***  BEGIN ADD  ***  07/21/99  **********************************
     C     CXRPMY        IFNE      0
     C                   MOVE      *OFF          *IN50
     C                   Z-ADD     CXRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C                   Z-ADD     SCRPMY        SVRPMY            6 0
     C                   ELSE
     C                   MOVE      *ON           *IN50
     C                   Z-ADD     0             SCRPMY
     C                   Z-ADD     0             SVRPMY
     C                   ENDIF                                                  CXRPMY<>0
     C***  END   ADD  ***  07/21/99  **********************************
     C**** BEGIN ADD **** 07/24/01 ************************************
     C                   MOVEL     SCRSN         SVRSN
     C**** END   ADD **** 07/24/01 ************************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCXDT
      *
     C                   Z-ADD     SCAGT#        CXAGT#
     C                   MOVE      SCRSN         CXRSN
     C                   MOVE      SCCCRT        CXCCRT
     C                   MOVEL     SCCOM1        CXCOM1
     C                   MOVEL     SCCOM2        CXCOM2
     C                   MOVE      SCRFEE        CXRFEE
     C                   MOVE      SCAMTD        CXAMTD
     C                   MOVE      *BLANKS       CXSTAT
     C                   MOVE      'N'           CXREIN
     C***  BEGIN ADD  ***  07/21/99  **********************************
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         CXRPMY
     C***  END   ADD  ***  07/21/99  **********************************
      ***** BEG ADD ******* 04/07/03 **********************************
      *
      *  Get rating state
      *
     C     ELKY2         CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        XXRTCD
     C  N60RSKY1         CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        CXRST
     C   60              MOVE      *BLANKS       CXRST
      *
      ***** END ADD ******* 04/07/03 **********************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
      * Renewal Year Processing
      *
     C     XXRNEW        IFEQ      'Y'
     C                   MOVE      'Y'           CXAFLT
     C                   Z-ADD     ELPFDT        CXCXDT
     C                   MOVE      'N'           CXCCRT
     C                   Z-ADD     0             CXRFEE
     C                   ENDIF                                                  XXRNEW
      *
      ***** END ADD ******* 08/03/04 **********************************
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     SCCNC#        CXCNC#
      ***** END ADD ******* 07/25/05 **********************************
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
      **** BEGIN ADD **** 12/24/97 *********************************************
      *
      * CONVERT CANCEL DATE HERE SO THAT IT IS DISPLAYED CORRECTLY
      * AFTER REASON CODE SEARCH
      *
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCXDT
      **** END   ADD **** 12/24/97 *********************************************
     C*
     C***  BEGIN ADD  ***  07/21/99  **********************************
     C* Convert the Missing S/R Month so that it is displayed correctly
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
     C***  END   ADD  ***  07/21/99  **********************************
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
      ******* BEGIN ADD ****** 04/03/98 *******************************
     C     *INKN         CASEQ     *ON           KNDO
      ******* END   ADD ****** 04/03/98 *******************************
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
     C***  BEGIN CHG  ***  07/29/99  **********************************
      ***ONLY DO IF NOT IN INQUIRY AND ENTER WAS PRESSED
      ********
     C********   *IN09     IFEQ *OFF
     C********   *INKC     ANDEQ*OFF
     C********   *INKL     ANDEQ*OFF
      **** BEGIN ADD **** 06/01/98 *********************************************
     C********   @@MODE    ANDNE'E'
      **** END   ADD **** 06/01/98 *********************************************
      ********
      ***IF THIS IS A REVISED CANCELLATION, NEED TO RE-UPDATE THE
      ***CANCELLATION DATE IN WMEMP AFTER PRINTING
      ********
     C********   SCREV     IFEQ 'Y'
     C********             MOVE 'N'       QQREPR
     C********             ENDIF                           SCREV=Y
      ********
      ***WRITE A RECORD TO THE CANCELLATION HISTORY FILE IF THIS IS
      ***THE INITIAL ENTRY OR IF THIS IS A REVISION
      ********
     C********   XHKY      CHAINPXH01                60
      ********
     C********             TIME           DSDTTM
      ********
     C********             MOVE *BLANKS   XHSTAT
     C********             MOVE Q1USER    XHUSER
     C********             MOVE Q1PGM     XHPGM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     XHDT
     C********             Z-ADDDSTIME    XHTIME
      ********
     C********   *IN60     IFEQ *ON
      ********
     C********             Z-ADDXXCO#     XHCO#
     C********             Z-ADDSCFND     XHFND
     C********             Z-ADDSCFYR     XHFYR
     C********             Z-ADDSCEMP#    XHEMP#
     C********             Z-ADDSCDIV     XHDIV
     C********             Z-ADDSCREQ#    XHREQ#
     C********             Z-ADDSCAGT#    XHAGT#
      ********
     C********             MOVE *BLANK    XHRVFL
     C********             Z-ADDCXREDT    XHREDT
     C********             Z-ADDCXRDAT    XHRDAT
      ********
     C********             MOVE SCRSN     XHRSN
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     XHTRDT
     C********             Z-ADDCXCXDT    XHCXDT
     C********             Z-ADDCXCDAT    XHCDAT
      ********
     C********             WRITEPXH01
      ********
     C********             ELSE
      ********
     C********   SCREV     IFEQ 'Y'
      ********
     C********             MOVE 'R'       XHRVFL
      ********
     C********             MOVE SCRSN     XHRSN
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     XHTRDT
     C********             Z-ADDCXCXDT    XHCXDT
     C********             Z-ADDCXCDAT    XHCDAT
      ********
     C********             WRITEPXH01
      ********
     C********             ENDIF                            SCREV=Y
      ********
     C********             ENDIF                            *IN61=*ON
      ********
      ***SUBMIT CANCELLATION PRINT PROGRAM
      ********
     C********             MOVE XXCO#     QQCO#   3
     C********             MOVE SCFND     QQFND   3
     C********             MOVE SCFYR     QQFYR   3
     C********             MOVE SCEMP#    QQEMP#  9
     C********             MOVE SCDIV     QQDIV   5
     C********             MOVE SCREQ#    QQREQ#  5
      ********
      ***GET USER OUTQ
      ********
     C********             MOVEL##OUTQ    QQOUTQ 10 P
      ********
     C********             CALL 'W7165C'               99
     C********             PARM           QQCO#
     C********             PARM           QQFND
     C********             PARM           QQFYR
     C********             PARM           QQEMP#
     C********             PARM           QQDIV
     C********             PARM           QQREQ#
     C********             PARM           SCCCRT
     C********             PARM           QQREPR
     C********             PARM           QQOUTQ
     C********             PARM           SCTYPE
      ********
     C********             CALL 'WA007R'               99
      ********
     C********             ENDIF                           *IN09,KC,KL=*OFF
     C*
     C                   EXSR      PGMENX
     C***  END   CHG  ***  07/29/99  **********************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
      * Process Renewal years
      *
     C                   EXSR      SRRNEW
      *
      ***** END ADD ******* 08/03/04 **********************************
     C*
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C*    Process other divs if they exist
     C     *IN87         IFEQ      *ON
     C                   EXSR      SROTHW
     C                   ENDIF
     C***  END   ADD  ***  07/29/99  **********************************
      *
      ***** BEG ADD ******* 03/12/04 **********************************
     C                   MOVE      *ON           *INLR
      ***** END ADD ******* 03/12/04 **********************************
     C                   ENDSR                                                  PGMEND
      *
      *
      *
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C*===============================================================
     C* PGMENX - Processing to be done for each cancelled emp/div
     C*===============================================================
     C     PGMENX        BEGSR
     C*
     C*  Only do if not in inquiry and enter was pressed
     C     *IN09         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     @@MODE        ANDNE     'E'
     C*
     C*  If this is a revised cancellation, need to re-update the
     C*  cancellation date in WMEMP before printing
     C     SCREV         IFEQ      'Y'
     C                   MOVE      'N'           QQREPR
     C                   ENDIF                                                  SCREV=Y
     C*
     C***  Begin Add  ***  02/20/04  **************************************
     C*  Update the cx date for the employer
     C*
      ***** BEG ADD ******* 08/03/04 **********************************
     C     XXRNEW        IFEQ      'N'
      ***** END ADD ******* 08/03/04 **********************************
     C     EMKY3         CHAIN     PEM03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             XX                1 0
     C     *ZERO         LOOKUP    ED(XX)                                 60
     C*
     C     ED(XX)        IFNE      *ZERO
     C                   Z-ADD     5             XX
     C                   ELSE
     C                   SUB       1             XX
     C                   ENDIF                                                  ED,XX<>*ZERO
     C*
     C                   Z-ADD     XXCXDT        CD(XX)
     C*
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   Z-ADD     DSTIME        EMTIME
     C*
     C                   UPDATE    PEM03
     C*
     C                   ENDIF
     C***  End   Add  ***  02/20/04  **************************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
     C                   ENDIF                                                  XXRENEW
      ***** END ADD ******* 08/03/04 **********************************
     C*
     C*  Write a record to the cancellation history file if this is
     C*  the initial entry or if this is a revision
     C     XHKY          CHAIN     PXH01                              60
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHDT
     C                   Z-ADD     DSTIME        XHTIME
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C                   MOVE      *BLANKS       XHENFL
     C***  END   ADD  ***  08/10/99  **********************************
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     XXCO#         XHCO#
     C                   Z-ADD     SCFND         XHFND
     C                   Z-ADD     SCFYR         XHFYR
     C                   Z-ADD     SCEMP#        XHEMP#
     C                   Z-ADD     SCDIV         XHDIV
     C                   Z-ADD     SCREQ#        XHREQ#
     C                   Z-ADD     SCAGT#        XHAGT#
     C*
     C                   MOVE      *BLANK        XHRVFL
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C*
     C                   MOVE      SCRSN         XHRSN
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHTRDT
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C*
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     CXCNC#        XHCNC#
      ***** END ADD ******* 07/25/05 **********************************
     C                   WRITE     PXH01
     C*
     C                   ELSE
     C*
     C     SCREV         IFEQ      'Y'
     C*
     C                   MOVE      'R'           XHRVFL
     C*
     C                   MOVE      SCRSN         XHRSN
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHTRDT
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C*
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   Z-ADD     CXCNC#        XHCNC#
      ***** END ADD ******* 07/25/05 **********************************
     C                   WRITE     PXH01
     C*
     C                   ENDIF                                                   SCREV=Y
     C*
     C                   ENDIF                                                   *IN61=*ON
      *
      ***** BEG ADD ******* 12/23/02 **********************************
     C                   UNLOCK    WDXHL
      ***** END ADD ******* 12/23/02 **********************************
     C*
     C*  Submit cancellation print program
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCREQ#        QQREQ#            5
     C*
     C*  Get user outq
     C                   MOVEL(P)  ##OUTQ        QQOUTQ           10
     C*
     C                   CALL      'W7165C'                             99
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQREQ#
     C                   PARM                    SCCCRT
     C                   PARM                    QQREPR
     C                   PARM                    QQOUTQ
     C                   PARM                    SCTYPE
     C*
      ***** BEG ADD ******* 08/03/04 **********************************
     C     XXRNEW        IFEQ      'N'
      ***** END ADD ******* 08/03/04 **********************************
     C   87              CALL      'WA007R'                             99
      ***** BEG ADD ******* 08/03/04 **********************************
     C                   ENDIF                                                  XXRENEW
      ***** END ADD ******* 08/03/04 **********************************
     C*
     C                   ENDIF                                                  *IN09,KC,KL=*OFF
     C*
     C                   ENDSR                                                  PGMENX
     C***  END   ADD  ***  07/29/99  **********************************
      *
      *
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
      ***** BEG ADD ******* 08/03/04 **********************************
     C                   MOVE      'N'           XXRNEW
      ***** END ADD ******* 08/03/04 **********************************
      ***** BEG DEL ******* 01/11/02 **********************************
      ******BEG ADD ******* 12/12/01 **********************************
     C*********            SETOF                     46
      ******END ADD ******* 12/12/01 **********************************
      ***** END DEL ******* 01/11/02 **********************************
      *
      *** BEGIN ADD *** 11/06/97 **************************************
      *
      *
      *  DEFINE DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
      *
      *** END   ADD *** 11/06/97 **************************************
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C                   Z-ADD     @@RRN         X@RRN
     C***  END   ADD  ***  07/29/99  **********************************
      *
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      ***** BEG DEL ******* 03/12/04 **********************************
     C*********
     C***GET*CANCELLATION RECORD
     C*********
     C*********            CLEARDBRCDF
     C*********  @@RRN     CHAINWDCXP                60
     C*********
     C*********            Z-ADDCXCO#     XXCO#
     C*********            Z-ADDCXFND     SCFND
     C*********            Z-ADDCXFYR     SCFYR
     C*********            Z-ADDCXEMP#    SCEMP#
     C*********            Z-ADDCXDIV     SCDIV
     C*********
     C*****BEGIN DEL **** 12/29/97 *********************************************
     C********** CXREIN    IFEQ 'Y'
     C********** @@MODE    ANDEQ'C'
     C******************** MOVE 'D'       @@MODE
     C******************** MOVE *ON       *IN09
     C******************** ENDIF                           CXREIN=Y/@@MODE=C
     C*****END*  DEL **** 12/29/97 *********************************************
     C*********
     C***GET*EMPLOYER DETAIL RECORD
     C*********
     C*********  *IN40     IFEQ *OFF
     C*********  ELKY      CHAINPEL01                60
     C*********            ELSE
     C*********  @@RRN2    CHAINWDELP                60
     C*********            Z-ADDELCO#     XXCO#
     C*********            Z-ADDELFND     SCFND
     C*********            Z-ADDELFYR     SCFYR
     C*********            Z-ADDELEMP#    SCEMP#
     C*********            Z-ADDELDIV     SCDIV
     C*********            ENDIF                           *IN40=*OFF
      ***** END DEL ******* 03/12/04 **********************************
      ***** BEG ADD ******* 03/12/04 **********************************
      *
     C     @@RRN2        CHAIN     WDELP                              60
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
      *
      ***** END ADD ******* 03/12/04 **********************************
      *
      *** BEGIN ADD *** 11/06/97 **************************************
      *
      *
      *  BEGIN CALCULATION FOR DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
      *
     C     ELKY1         CHAIN     PEL01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 = OFF
      *
      *  END CALCULATION FOR DIVISION COUNT
      *
      *** END   ADD *** 11/06/97 **************************************
      **** BEGIN ADD **** 01/07/99 *********************************************
     C     ELKY          CHAIN     PEL01                              60
      **** END   ADD **** 01/07/99 *********************************************
     C*
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C***  END   ADD  ***  07/29/99  **********************************
      *
      *  CHECK FOR ADDITIONAL DIVISIONS
      *
     C                   MOVE      *OFF          *IN27
      *
     C     EMKYX         CHAIN(N)  PEM01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     *IN27         ANDEQ     *OFF
      *
     C     SCDIV         IFNE      EMDIV
     C                   MOVE      *ON           *IN27
     C                   ENDIF                                                  SCDIV<>EMDIV
      *
     C     EMKYX         READE(N)  PEM01                                  60
      *
     C                   ENDDO                                                  *IN60/27=*OFF
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Check for additional entities/locations and set indicator
     C                   MOVE      *OFF          *IN36
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN36
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C***  BEGIN CHG  ***  07/29/99  **********************************
      ***GET EMPLOYER MASTER RECORD
      **********
     C********** EMKY      CHAINPEM01               N60
      **********
      ***THE EMPLOYER MUST BE EFFECTIVE IN ORDER TO CANCEL,
      ***OTHERWISE SEND AN ERROR MESSAGE DISPLAYING THE CURRENT
      ***CANCEL DATE
      **********
     C********** @@MODE    IFEQ 'A'
      **********
     C**********           MOVEA'000'     *IN,24
     C**********           Z-ADD0         SCCX1
     C**********           Z-ADD0         SCCX2
     C**********           MOVE *BLANKS   SCMSG1
     C**********           MOVE *BLANKS   SCMSG2
      **********
     C********** CNKY      CHAINPCN01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVE *ON       *IN26
     C**********           ENDIF                           *IN60=*OFF
      **********
     C**********           Z-ADD1         X       30
     C********** *ZERO     LOKUPED,X                     60
     C**** N60             Z-ADD5         X
     C***** 60             SUB  1         X
      **********
     C* N26      X         IFNE *ZERO
      **********
     C********** CD,X      IFNE *ZERO
      **********
     C**********           Z-ADDCD,X      SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCCX1
      **********
     C**********           Z-ADDCD,X      SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCCX2
      **********
     C**********           TIME           DSDTTM
      **********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXRNDT
      **********
     C********** CD,X      COMP XXRNDT                 24
     C***** 24             MOVELMSG2      SCMSG2
     C**** N24             MOVELMSG1      SCMSG1
     C**** N24             MOVE *ON       *IN25
      **********
     C**********           ENDIF                           CD,X<>*ZERO
      **********
     C**********           ENDIF                           X<>*ZERO
      **********
     C********** *IN24     IFEQ *ON
     C********** *IN25     OREQ *ON
     C********** *IN26     OREQ *ON
      **********
     C**********           EXFMTWDW01
      **********
     C**********           MOVE *ON       *INLR
     C**********           RETRN
      **********
     C**********           ENDIF                           *IN24,25,26=*ON
      **********
     C**********           ENDIF                           @@MODE=A
     C*
     C*  Call subroutine to check if account can be cancelled
     C                   EXSR      CHKACT
     C***  END   DEL  ***  07/29/99  **********************************
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
      ***** BEG ADD ******* 09/02/99 **********************************
      *
      *  Get Activity Status Description for Header
      *
     C***  BEGIN CHG ****  07/07/05  **********************************
     C******************   MOVELEMASTS    KKKEY     P
     C                   MOVEL(P)  ELASCD        KKKEY
     C     AVKY          CHAIN     PAV01                              64
     C  N64              MOVEL(P)  AVDESC        SCACTD
      *
     C   64              MOVEL(P)  *BLANKS       SCACTD
      *
     C***  END CHG ******  07/07/05  **********************************
      ***** END ADD ******* 09/02/99 **********************************
      ***** BEG ADD ******* 09/18/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX02                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 09/18/98 **********************************
      *
      *
      *  GET GROUP INFORMATION
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C                   MOVEL     FNNAME        SCNAME
      *
      *  GET AGENT RECORDS
      *
     C                   Z-ADD     SCFYR         XXFYR
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *
     C***  BEGIN DEL  ***  07/29/99  **********************************
     C*******              MOVEL'WC'      XXSYK
     C*******    SYKY      CHAINSWC01                60
     C*******    WCFL07    COMP 'Y'                      66
      ***** BEGIN CHG **** 07/22/98 ***********
      ********** WCFL08    COMP 'Y'                      67
     C*******    WCFL08    COMP 'C'                      67
      ***** END   CHG **** 07/22/98 ***********
     C*******    WCFL10    COMP 'Y'                      68
     C*******    WCFL06    COMP 'Y'                      69
     C***  END   DEL  ***  07/29/99  **********************************
      *
      *  FLAG IF CLIENT IS ENROLLED IN OV/400
      *
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     SYOFIF        COMP      'Y'                                    41
      *
      *  IF IN ADD MODE
      *
     C     *IN40         IFEQ      *ON
      *
      *    SET REPRINT FLAG TO 'N'
      *
     C                   MOVE      'N'           QQREPR            1
      *
      *    GET THE NEXT REQUEST #
      *
     C                   MOVE      *HIVAL        SCREQ#
      *
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     CXREQ#        ADD       1             SCREQ#
     C                   ELSE
     C                   Z-ADD     1             SCREQ#
     C                   ENDIF                                                  *IN60=*OFF
      *
      *    DO NOT DISPLAY THE REINSTATEMENT FLAG
      *
     C                   MOVE      *ON           *IN45
      *
     C                   MOVE      'N'           SCCCRT
      *
     C                   ELSE
      *
      *    SET REPRINT FLAG TO 'Y'
      *
     C                   MOVE      'Y'           QQREPR            1
      *
      *    DISPLAY THE REINSTATEMENT FLAG
      *
     C                   MOVE      *OFF          *IN45
      *
     C                   ENDIF                                                  *IN40=*ON
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
      *
      ***** BEG ADD ******* 07/25/05 **********************************
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** @@RRN     CHAINDBRCDF               99
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C***  End   Chg  ***  01/03/07  **************************************
      *
     C     QQREPR        IFEQ      'Y'                                          REPRINT
     C     *IN40         OREQ      *OFF                                         REVISION
     C                   Z-ADD     CXCNC#        SCCNC#
      *
     C                   ELSE
      *
     C                   EXSR      SRCNC#                                       CALC CX COUNT
     C                   ENDIF
      *
      *
      ***** END ADD ******* 07/25/05 **********************************
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      *
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*===============================================================
     C* SRCALC - CALCULATES THE NUMBER OF DAYS TO ADD TO THE ISSUE
     C*          DATE TO DETERMINE THE CANX EFFECTIVE DATE
     C*===============================================================
     C     SRCALC        BEGSR
     C*
     C                   Z-ADD     *ZERO         ZZDAYS            4 0
     C*
     C*  Cycle through all divs for employer
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*      Get jurisdiction state
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*      Chain to canx reason code file
     C                   MOVEL     SCRSN         XXKEY
     C                   MOVE      RSJRST        KKST
     C***  BEGIN CHG  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
     C                   Z-ADD     XXRNDT        KKEFDT
      ***** BEG ADD ******* 12/09/02 **********************************
      *
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     CXCDAT        KKEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 12/09/02 **********************************
     C***  END   CHG  ***  02/24/99  **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 09/07/99 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 09/07/99 **********************************
     C  N60              MOVEL(P)  CRDESC        SCRSND
     C*
      ***** BEG ADD ******* 08/17/11 **********************************
      *
     C                   MOVE      ' '           POLNEW            1
     C     CRNDNW        IFGT      0
     c* Number of days to consider policy new
     C                   Z-ADD     CRNDNW        QQDAYS
     C*
     c** Calculate new work date
     C                   CALL      'SRPAST8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          Q@CXDT            8 0
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X@WRDT            7 0
     C                   ENDIF                                                  QQERR<>'1'
      *
     C                   Z-ADD     XXCO#         #@CO#
     C                   Z-ADD     SCFND         #@FND
     C                   Z-ADD     SCEMP#        #@EMP#
     C                   Z-ADD     SCDIV         #@DIV
     C                   Z-ADD     *ZEROS        #@DATE
     C                   CALL      'WA084R'
     C                   PARM                    #@CO#             3 0
     C                   PARM                    #@FND             3 0
     C                   PARM                    #@EMP#            9 0
     C                   PARM                    #@DIV             5 0
     C                   PARM                    #@DATE            7 0
     C*
     C                   MOVE      ' '           POLNEW            1
     C     #@DATE        IFGE      X@WRDT
     C                   MOVE      'Y'           POLNEW
     C                   ENDIF                                                  X#DATE > X@WRDT
     C                   ENDIF                                                  CRNDNW > 0
      ***** END ADD ******* 08/17/11 **********************************
     C*
     C*      Calculate number of days to add to issue date
     C                   Z-ADD     0             XXDAYS            4 0
     C*
     C     ELCXDY        IFNE      0
     C                   ADD       ELCXDY        XXDAYS
     C                   ELSE
      ***** BEG CHG ******* 08/17/11 **********************************
     C*************        ADD  CRNOTC    XXDAYS
     C     POLNEW        IFNE      'Y'
     C                   ADD       CRNOTC        XXDAYS
     C                   ELSE
     C                   ADD       CRNTNP        XXDAYS
     C                   ENDIF                                                  POLNEW = 'Y'
      ***** END CHG ******* 08/17/11 **********************************
     C                   ENDIF                                                  ELCXDY<>0
     C*
     C     ELADDM        IFNE      0
     C                   ADD       ELADDM        XXDAYS
     C                   ELSE
      ***** BEG CHG ******* 08/17/11 **********************************
     C*************        ADD  CRADDM    XXDAYS
     C     POLNEW        IFNE      'Y'
     C                   ADD       CRADDM        XXDAYS
     C                   ELSE
     C                   ADD       CRMDNP        XXDAYS
     C                   ENDIF                                                  POLNEW = 'Y'
      ***** END ADD ******* 08/17/11 **********************************
     C                   ENDIF                                                  ELADDM<>0
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     XXDAYS        IFGT      ZZDAYS
     C                   Z-ADD     XXDAYS        ZZDAYS
     C                   ENDIF                                                  XXDAYS>ZZDAYS
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  Add days to system date to get cx effective date
     C                   Z-ADD     ZZDAYS        QQDAYS
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SCCXDT
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCXDT
     C                   ENDIF                                                  QQERR<>'1'
     C*
     C*  Reset to look at correct employer detail record
     C     ELKY          CHAIN     PEL01                              60
     C*
     C*  Check that cancellation date is within policy period.
     C     XXCXDT        IFLT      ELPFDT
     C     XXCXDT        ORGT      ELPTDT
     C                   SETON                                        37  90
     C                   ENDIF                                                  ELPFDT>=XXCXDT>=ELPT
     C*
     C*  Display calculated date for confirmation
     C     *IN37         IFEQ      *OFF
     C*
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCW1DT
     C                   EXFMT     WDW02
     C*
     C     *INKL         IFEQ      *ON
     C                   CLEAR                   SCCXDT
     C                   SETON                                        90
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   ENDIF                                                  *IN37=*OFF
     C*
     C                   ENDSR                                                  SRCALC
     C***  END   ADD  ***  11/10/98  **********************************
      ***** BEG ADD ******* 07/25/05 **********************************
      *
      *
      *
      *===============================================================
      * SRCNC# - Calculate cancellation count
      *===============================================================
     C     SRCNC#        BEGSR
      *
      *  Get Rating State
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVEL(P)  RSJRST        XXJRST
     C   61              MOVEL(P)  *BLANKS       XXJRST
      *
      *  Get Cancellation limit
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C     XCKY2         SETGT     PXC01
     C     XCKY3         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             XCCXMN
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
      *  Get number of months back to look for CX
      *
     C                   Z-ADD     *ZERO         X3DATE
      *
     C     XCCXMN        IFNE      *ZERO
     C                   Z-ADD     XCCXMN        ##MTHS
      *
     C                   CALL      'SRPMTH8'
     C                   PARM      DSDATE        ##DATE            8 0
     C                   PARM                    ##MTHS            3 0
     C                   PARM      *OFF          ##ERR             1
      *
     C     ##ERR         IFEQ      *OFF
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X3DATE
     C                   ENDIF
      *
     C                   ENDIF                                                  XCCXMN
      *
      *  Count CX for reason of non-payment in number of past months
      *
     C                   Z-ADD     *ZERO         XX#CX            11 0
      *
     C     CXKY2         SETLL     PCX02
     C     CXKY3         READE     PCX02                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     CXERR         IFNE      'Y'
     C                   MOVEL(P)  CXRSN         KKKEY
     C                   EXSR      SRNCCI
      *
     C     CRNCCI        IFEQ      '05'                                         NON-PAY
     C                   ADD       1             XX#CX
     C                   ENDIF                                                  CRNCCI
      *
     C                   ENDIF                                                  CXERR
      *
     C     CXKY3         READE     PCX02                                  60
     C                   ENDDO
      *
     C                   Z-ADD     XX#CX         SCCNC#
      *
      * Include current CX in count if for reason of non-payment
      *
     C                   MOVEL(P)  SCRSN         KKKEY
     C                   EXSR      SRNCCI
      *
     C     CRNCCI        IFEQ      '05'                                         NON-PAY
     C                   ADD       1             SCCNC#
     C                   ENDIF
      *
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** @@RRN     CHAINDBRCDF               99
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C***  End   Chg  ***  01/03/07  **************************************
      *
     C                   ENDSR                                                  SRCNC#
      *
      *
      *===============================================================
      * SRNCCI - GET NCCI CX REASON CODE
      *===============================================================
     C     SRNCCI        BEGSR
      *
     C     CRKY2         SETGT     PCR01
      *
     C     *IN60         DOUEQ     *ON
     C     CRSTAT        ORNE      'I'
     C     CRKY3         READPE    PCR01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   CRNCCI
     C                   ENDIF
      *
     C                   ENDSR                                                  SRNCCI
      *
      *
      *
      *
      *
      ***** END ADD ******* 07/25/05 **********************************
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C*===============================================================
     C* SROTHD - Checks for other divs to cancel/revise cancel
     C*===============================================================
     C     SROTHD        BEGSR
     C*
     C                   Z-ADD     SCDIV         SAVDIV
     C                   Z-ADD     SCREQ#        SVREQ#
     C*
     C                   SETOFF                                       87
     C*
     C*   Cycle through all divisions
     C     EMKYX         CHAIN(N)  PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     *IN87         ANDEQ     *OFF
     C     *IN88         ANDEQ     *OFF
     C*
     C     EMDIV         IFNE      SAVDIV
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C***  BEGIN ADD  ***  10/12/99  **********************************
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C***  END   ADD  ***  10/12/99  **********************************
     C*
     C*     Set indicator if another div is found for cancellation
     C                   EXSR      SRGETX
     C     CD(X)         IFEQ      *ZERO
     C     CD(X)         ORGT      XXCXDT
     C***  BEGIN ADD  ***  11/10/00  **********************************
     C     SCREV         OREQ      'Y'
     C***  END   ADD  ***  11/10/00  **********************************
     C                   SETON                                        87
     C                   ENDIF                                                  CD,X=*ZERO
     C*
     C***  BEGIN ADD  ***  10/12/99  **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   ADD  ***  10/12/99  **********************************
     C*
     C                   ENDIF                                                  EMDIV<>SAVDIV
     C*
     C     EMKYX         READE(N)  PEM01                                  60
     C                   ENDDO
     C*
     C*   If there are multiple divs for cancellation then output msg
     C     SCREV         COMP      'Y'                                    88
     C     *IN87         IFEQ      *ON
     C                   EXFMT     WDW03
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN87=*ON
     C*
     C*  Reset emp master and canx detail files
     C                   Z-ADD     SAVDIV        SCDIV
     C                   Z-ADD     SVREQ#        SCREQ#
     C     EMKY          CHAIN(N)  PEM01                              60
     C     CXKY1         CHAIN     PCX01                              60
     C*
     C***  BEGIN ADD  ***  10/12/99  **********************************
     C*  Reset emp detail file
     C     ELKY          CHAIN     PEL01                              60
     C***  END   ADD  ***  10/12/99  **********************************
     C*
     C                   ENDSR                                                  SROTHD
     C*===============================================================
     C* SROTHW - Writes/Updates cancellation records for other divs
     C*===============================================================
     C     SROTHW        BEGSR
     C*
     C                   SETOFF                                       87
     C                   Z-ADD     SCDIV         SAVDIV
      ***** BEG ADD ******* 08/03/04 **********************************
     C                   Z-ADD     SCREQ#        SVREQ#
      ***** END ADD ******* 08/03/04 **********************************
     C*
     C*   Cycle through all divisions
     C     EMKYX         CHAIN(N)  PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMDIV         IFNE      SAVDIV
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     XXCXDT        IFGE      ELPFDT
     C     XXCXDT        ANDLE     ELPTDT
     C*
     C*     Get appropriate effective and cancel dates from emp master
     C                   EXSR      SRGETX
     C*
     C*     Update employer master with cancel date
     C     CD(X)         IFEQ      *ZERO
     C     CD(X)         ORGT      XXCXDT
     C***  BEGIN ADD  ***  11/10/00  **********************************
     C     SCREV         OREQ      'Y'
     C***  END   ADD  ***  11/10/00  **********************************
     C*
     C*     Find the latest cancellation
     C                   Z-ADD     *HIVAL        SCREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
     C*
     C*       Add cancellation record
     C     *IN60         IFEQ      *ON
     C     CXREIN        OREQ      'Y'
     C*
     C   60              Z-ADD     1             SCREQ#
     C  N60CXREQ#        ADD       1             SCREQ#
     C*
     C                   CALL      AGTAPI        AGTPL
     C                   EXSR      WRITER
     C                   EXSR      PGMENX
      ***** BEG ADD ******* 08/03/04 **********************************
      *
      *    Process Renewal years
      *
     C                   EXSR      SRRNEW
      *
     C                   ELSE
      *
      *  Revise a cancellation record
      *
      ***** END ADD ******* 08/03/04 **********************************
      ***** BEG DEL ******* 08/03/04 **********************************
     C*************
     C*************        ENDIF                           *IN60=*ON
     C*************
     C********Revise a cancellation record
     C************IN60     IFEQ *OFF
     C***********CXREIN    ANDNE'Y'
      ***** END DEL ******* 08/03/04 **********************************
     C*
     C                   Z-ADD     CXREQ#        SCREQ#
     C*
     C                   CALL      AGTAPI        AGTPL
     C                   EXSR      UPDATR
     C                   EXSR      PGMENX
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CD,X=*ZERO
     C                   ENDIF                                                  XXCXDT>=ELPFDT
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EMDIV<>SAVDIV
     C     EMKYX         READE(N)  PEM01                                  60
     C                   ENDDO
     C*
     C*  Reset emp master and canx detail files
     C                   Z-ADD     SAVDIV        SCDIV
     C                   Z-ADD     SVREQ#        SCREQ#
     C     EMKY          CHAIN(N)  PEM01                              60
     C     CXKY1         CHAIN     PCX01                              60
     C*
     C                   ENDSR                                                  SROTHW
      *
      *
      *
     C*===============================================================
     C* SRGETX - Gets appropriate emp master eff and canx dates
     C*===============================================================
     C     SRGETX        BEGSR
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             X
     C     *ZERO         LOOKUP    ED(X)                                  70
     C*
     C     ED(X)         IFNE      *ZERO
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  ED,X<>*ZERO
     C*
      ***** BEG CHG ******* 02/17/09 **********************************
     C*********  X         DOWGT*ZERO
     C     X             DOWGT     1
      ***** END CHG ******* 02/17/09 **********************************
     C     ED(X)         IFGT      ELPFDT
     C                   SUB       1             X
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  ED,X>ELPFDT
     C                   ENDDO                                                  X=*ZERO
     C*
     C*  Determine if cancellation date exists, set canx indicator
     C     X             IFEQ      *ZERO
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C     CD(X)         IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CD,X=*ZEROS
     C                   ENDIF                                                  X=*ZERO
     C*
     C                   ENDSR                                                  SRGETX
     C***  END   ADD  ***  07/29/99  **********************************
      ***** BEG ADD ******* 08/03/04 **********************************
      *
      *
      *
      *===============================================================
      * SRRNEW - Process Renewal Years
      *===============================================================
     C     SRRNEW        BEGSR
      ***** BEG ADD ******* 05/25/06 **********************************
     C*
     C*  Only do if not in inquiry and enter was pressed
     C     *IN09         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     @@MODE        ANDNE     'E'
      ***** END ADD ******* 05/25/06 **********************************
     C                   MOVE      'Y'           XXRNEW            1
      *
      *  If not revised CX or Reprint
      *
     C     SCREV         IFNE      'Y'
     C     QQREPR        ANDEQ     'N'
     C                   Z-ADD     SCFYR         S1FYR
     C                   Z-ADD     SCREQ#        S1REQ#
      *
      *  For each renewal policy year for employer division
      *
     C     ELKY5         SETGT     PEL04
     C     ELKY6         READE     PEL04                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELFYR         SCFYR
      *
      *    If not cancelled, process CX of renewal year
      *
     C                   Z-ADD     *HIVAL        SCREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *ON                                          NOT CX
     C     CXREIN        OREQ      'Y'                                          OR REINSTATED
     C   60              Z-ADD     1             SCREQ#
     C  N60CXREQ#        ADD       1             SCREQ#
     C                   CALL      AGTAPI        AGTPL
     C                   EXSR      WRITER
     C                   EXSR      PGMENX
     C                   ENDIF                                                  CXKY
      *
     C     ELKY6         READE     PEL04                                  60
     C                   ENDDO                                                  *IN90
      *
      * Reset
      *
     C                   Z-ADD     S1FYR         SCFYR
     C                   Z-ADD     S1REQ#        SCREQ#
     C     ELKY          CHAIN     PEL01                              60
     C                   CALL      AGTAPI        AGTPL
     C     CXKY1         CHAIN     PCX01                              60
     C                   ENDIF
      *
     C                   MOVE      'N'           XXRNEW            1
      ***** BEG ADD ******* 05/25/06 **********************************
      *
     C                   ENDIF
      *
      ***** END ADD ******* 05/25/06 **********************************
     C                   ENDSR                                                  SRRNEW
      *
      *
      *
      ***** END ADD ******* 08/03/04 **********************************
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
     C***  BEGIN CHG  ***  07/29/99  **********************************
     C********** @@RRN     CHAINDBRCDF               99
     C     X@RRN         CHAIN     DBRCDF                             99
     C***  END   CHG  ***  07/29/99  **********************************
      *
B02  C     *IN99         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C     SCREV         IFEQ      'Y'
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C     *IN87         OREQ      *ON
     C***  END   ADD  ***  07/29/99  **********************************
      ***** BEG ADD ******* 01/11/02 **********************************
      *
     C     CXCDAT        IFLE      XXRNDT
      ***** END ADD ******* 01/11/02 **********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCDAT
      ***** BEG ADD ******* 01/11/02 **********************************
     C                   ENDIF
      *
      ***** END ADD ******* 01/11/02 **********************************
     C                   ENDIF                                                  SCREV=Y
     C*
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C                   MOVE      *BLANKS       CXENFL
     C***  END   ADD  ***  08/10/99  **********************************
      *
      *
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXTIME
      *
     C                   UPDATE    DBRCDF
      *
E02  C                   ENDIF                                                  *IN99=*OFF
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  UPDATR
      *
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
      *
     C                   Z-ADD     XXCO#         CXCO#
     C                   Z-ADD     SCFND         CXFND
     C                   Z-ADD     SCEMP#        CXEMP#
     C                   Z-ADD     SCDIV         CXDIV
     C                   Z-ADD     SCFYR         CXFYR
     C                   Z-ADD     SCREQ#        CXREQ#
      *
     C                   EXSR      MVSCDB
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCDAT
     C                   Z-ADD     0             CXREDT
     C                   Z-ADD     0             CXRDAT
     C                   MOVE      'N'           CXRCRT
     C                   MOVE      *BLANKS       CXRDRT
      *
     C                   MOVEL     Q1USER        CXIUSR
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXIPGM
     C                   MOVEL     Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXIDT
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXITM
     C                   MOVEL     DSTIME        CXTIME
      **** BEGIN ADD **** 12/03/98 *********************************************
     C                   Z-ADD     0             CXCFDT
     C                   Z-ADD     0             CXCFPD
     C                   MOVE      *BLANK        CXCFFL
     C                   MOVE      *BLANK        CXERR
      **** END   ADD **** 12/03/98 *********************************************
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C                   MOVE      *BLANKS       CXENFL
     C***  END   ADD  ***  08/10/99  **********************************
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      WDCXP
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  WRITER
