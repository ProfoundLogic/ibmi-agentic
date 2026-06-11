     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0660R02 - EMPLOYER PAYROLL S/R INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD S/R INFO IN THE S/R PAYROLL FILE.       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0660R - WORK WITH EMPLOYER S/R PAYROLLS                   *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/02/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/15/15 C:308 SUPPORT: 32186                                 *
     H*                Prevent future date entry on S/R payroll entry *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                Use the new logical(s) to get the description  *
     H*                by extension.                                  *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 03/27/08 C:136 SUPPORT: 62792                                 *
     H*                PER NCCI, S/R PAYROLLS CAN BE MADE ON PAYROLLS *
     H*                THAT EXPIRE MIDTERM TIL END OF TERM.           *
     H*                                                               *
     H* 02/08/08 C:61  PROJECT 46WFQ                                  *
     H*                Correct screen issues 46WFL.                   *
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA126/44088                              *
     H*                Release record lock                            *
     H*                                                               *
     H* 11/15/06 C:136 SUPPORT: WA135 54711                           *
     H*                DISPLAY FILE CHANGES TO ALLOW CENTS VALUE      *
     H*                                                               *
     H* 10/11/06 C:136 SUPPORT: WA099 45522                           *
     H*                CORRECT DATE EDIT                              *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 04/26/06 C:132 PROJECT: 46WFL                                 *
     H*              - If client 48, dsplay prompt for "Late Report"  *
     H*                and allow entry.                              *
     H*              - Edit for "L" or Blank, update WASRP with flag. *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: 129WKQ                                *
     H*                ADD LOSS COST MULTIPLIER                       *
     H*                                                               *
     H* 12/16/04 C:132  PROJECT: WC1233                               *
     H*                 Display Payroll Class Code based on ARD or    *
     H*                 Polcy Effective Date when searching payroll   *
     H*                 class code.                                   *
     H*                                                               *
     H* 10/07/03 C:61  SUPPORT WA081 39048                            *
     H*                Do not allow addition of expired payroll class.*
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify display if vendor processing is         *
     H*                enabled to include report frequency and        *
     H*                report date for weekly frequency reports.      *
     H*                                                               *
     H*  11/14/02 C:65  PROJECT: WC1099                               *
     H*                 ADD CALL TO W0660R4 IF ANY CLASSES HAVE A     *
     H*                 STAT ENABLED EXTENSION FOR TERR DIFFERENTIAL. *
     H*                                                               *
     H*  11/13/02 C:65  PROJECT: WC577                                *
     H*                 REMOVE EDIT FROM SCSPEC FIELD, ANY ENTRY IS   *
     H*                 VALID.  ENTRY NOT REQUIRED.                   *
     H*                                                               *
     H* 10/01/02 C:61  SUPPORT WA099 33984                            *
     H*              - Only validate class extension for new entry.   *
     H*              - Call class extension search with correct       *
     H*                parameters.                                    *
     H*                                                               *
     H* 06/25/01 C:61  SUPPORT WA096 27958                            *
     H*                Add effective date to EPKY1.                   *
     H*                                                               *
     H* 04/24/01 C:61  PROJECT WC577                                  *
     H*              - F10=Search should only work for new entries.   *
     H*              - Correct issues with subfile display,           *
     H*                Page Up/Down.                                  *
     H*                                                               *
     H* 04/23/01 C:61  PROJECT WC577                                  *
     H*                Do not allow ADD of S/R month/year outside of  *
     H*                employer policy effective date range.          *
     H*                                                               *
     H* 06/29/00  C:33  PROJECT: WC577                                *
     H*                 ON SECOND ENTRY INTO PROGRAM POSITION CURSOR  *
     H*                 ON MONTH/YEAR.                                *
     H*                                                               *
     H* 03/14/00 C:61  SUPPORT WA045 21820                            *
     H*                Provide informational edit when user attempts  *
     H*                to add payrolls to account that has been       *
     H*                audited.  (opt 6, F6)                          *
     H*                                                               *
     H* 10/19/99 C:61  PROJECT WC944                                  *
     H*                If payroll class is added, edit to ensure      *
     H*                effective date is not greater than expiration  *
     H*                date for payroll class.                        *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 09/17/99 C:61  PROJECT WC577                                  *
     H*                When in add mode (opt 6), class can not be     *
     H*                *BLANK.                                        *
     H*                                                               *
     H* 03/02/99  C:65  PROJECT: SUPPORT                              *
     H*                 DO NOT CHAIN TO THE PAYROLL RATE FILE.        *
     H*                                                               *
     H* 01/28/99  C:33  SEQ#: WA091/12414                             *
     H*                -DO NOT ALLOW A NEW ENTRY OF A CLASS CODE THAT *
     H*                 IS CURRENTLY INACTIVE FOR THE EMPLOYER.       *
     H*                                                               *
     H* 01/25/99  C:33  SEQ#: WA081/15499                             *
     H*                -UPDATE MAINTENANCE FIELDS IN WMEPP.           *
     H*                -CLEAR SVAMT IN PGMSTR.                        *
     H*                                                               *
     H* 11/02/98  C:33  PROJECT: WC577                                *
     H*                 ADD WINDOW TO INFORM USER THAT THEY ARE ABOUT *
     H*                 TO ENTER S/R PAYROLLS FOR AN ESTIMATING EMPLR.*
     H*                                                               *
     H* 10/21/98  C:33  SEQ#: WA081/14541                             *
     H*                -DO NOT VALIDATE CLASS CODE AGAINST PAYROLL    *
     H*                 FILE BUT WHAT IS ON THE SCREEN.               *
     H*                -UPDATE SUBFILE AFTER F10 IS PRESSED FOR SCCLAS*
     H*                                                               *
     H* 08/11/98 C:65 Project: WC843                                  *
     H*               USE EXTENSION DESCRIPTION IF SPECIFIED.         *
     H*                                                               *
     H* 03/26/98  C:33  SEQ#: WA091/10840                             *
     H*                 ADD EDIT TO NOT ALLOW EFFECTIVE DATE TO BE    *
     H*                 LESS THAN THE POLCIY FROM DATE.               *
     H*                                                               *
     H* 03/25/98  C:65  PROJECT: SUPPORT                              *
     H*                 CHANGE LOGICAL FOR UPDATING PAYROLL FILE,     *
     H*                 S/R AMOUNT WAS BECOMING INCORRECT WHEN        *
     H*                 A PAYROLL CLASS WAS INACTIVE.                 *
     H*                                                               *
     H* 02/12/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMEPP*
     H*                                                               *
     H*  PLEASE ADD TO TOP OF REVISIONS                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE FOR NEW MONTH                                  *
     H* N40 - ADD MODE FOR MORE CLASS CODES                           *
     H* N41 - NEW CLASS CODE ENTRIES                                  *
     H*  85 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  94 - ROLL UP/DOWN PAST LAST RECORD                           *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  30 - (WCR0500) Invalid reporting MM/CCYY                     *
     H*  31 - (WC30005) MM/CCYY used more than once                   *
     H*  71 - (WCR0539) Class code invalid {new entry}                *
     H*  72 - (WCR0697) Class code already used {new entry}           *
     H*  73 - (WC30012) Spc Des not 1-9, 'S', 'F' or blank {new entry}*
     H*  74 - (WCR0500) Effective date invalid {new entry}            *
     H*  75 - (WCR0578) Effective date not between policy dates       *
     H*  77 - (WCR0503) CLASS can not be zero.                        *
     H*  78 - (WC31373) MM/CCYY not within employer policy date range.*
     H*  82 - (WCR0502) Amount cannot be < zero                       *
     H*  83 - (WC30018) Amount must be zero if eff dt > report mm/yy  *
     H*  84 - (WCr0528) Amount cannot be > zero if eff dt not present *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0660F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:DBRCDF2)
     F                                     INFDS(DBFDS2)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWASRP     UF A E             DISK
     F                                     RENAME(PSR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWASRL3    IF   E           K DISK
     F                                     RENAME(PSR01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*** BEG ADD ****** 02/18/03  ************************************
     F*  Logical file definition (for displaying items if freq = weekly
     FWASRL9    IF   E           K DISK
     F                                     RENAME(PSR01:DBRCDF4)
     F                                     INFDS(DBFDS4)
     F*** END ADD ****** 02/18/03  ************************************
     F*** BEGIN CHG ***  03/25/98  ************************************
     F**** WMEPL7  UF  E           K        DISK
     F*** BEG CHG *****  04/20/09  ************************************
     F*****WMEPL1  UF  E           K        DISK
     FWMEPL1E   UF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F*******WMEPL7  IF  E           K        DISK
     FWMEPL7E   IF   E           K DISK
     F*** END CHG *****  04/20/09  ************************************
     F*** END CHG *****  03/25/98  ************************************
     F                                     RENAME(PEP01:DBRCDF3)
     F                                     INFDS(DBFDS3)
     FWMEPP     UF A E             DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PPR02)
     F***  BEGIN ADD  ***  11/02/98  *********************************
     FWTABL     IF   E           K DISK
      **** BEG   ADD **** 02/18/03 *********************************************
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
      **** END   ADD **** 02/18/03 *********************************************
     F***  END   ADD  ***  11/02/98  *********************************
     F*** BEGIN ADD ***  08/11/98  ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     F****WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 10/06/99 ************************************
     FWMPXP     IF   E             DISK
     F                                     RENAME(PPX01:PPX02)
     F*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS02)
      **** END   ADD **** 10/06/99 ************************************
      **** BEG   ADD **** 02/18/03 *********************************************
     FSSYLF     IF   E           K DISK
      **** END   ADD **** 02/18/03 *********************************************
      /COPY SCOPYWW03D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D***  BEGIN ADD  ***  10/21/98  *********************************
     D***  BEGIN CHG  ***  04/20/09  *********************************
     D********            CLAS      999  6
     D CLAS            S             11    DIM(999)
     D***  END CHG  ***  04/20/09  ***********************************
     D***  END   ADD  ***  10/21/98  *********************************
     D*
     D*** BEGIN CHG ***  06/01/06  ***********************************
     D*****/COPY SCOPYWW02E
     D*
     D*****/COPY SCOPYWW02I
     D***PY SCOPYWW03E
     D*
     D***PY SCOPYWW03I
     D*** END CHG *****  06/01/06  ***********************************
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
      *** BEG ADD ******* 02/18/03 *********************************************
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
      *** END ADD ******* 02/18/03 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      **** END   ADD **** 02/12/98 *********************************************
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  XXEFDT                 1      7  0
     D  XXCYM                  1      5  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('S/R Information')
     D ACDPGM          C                   CONST('STDEFAULT')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D SF1PAG          C                   CONST(12)

      ***** Beg Add *** 12/15/15  *********************************
     D intSysdat       s              7P 0
     D intDate         s              5P 0 Inz(*zero)
     D strMsgId        s              7    inz(*Blank)
      ***** End Add *** 12/15/15  *********************************
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RPMY            6 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##MODE            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKRPMY
      *
      *** BEG ADD ******* 02/18/03 ************************************
     C     KEY1B         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKTODT            7 0
      *** BEG ADD ******* 02/18/03 ************************************
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *  PAYROLL DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCEXT
      *** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   KFLD                    XXST
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    X2EFDT
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCEXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
     C*** BEGIN ADD ***  11/14/02  ************************************
     C     PXKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SRCLAS
     C                   KFLD                    SREXT
     C                   KFLD                    XXST
     C*** END ADD *****  11/14/02  ************************************
     C*** BEGIN ADD ***  06/01/06  ************************************
     C*** BEG CHG *****  04/01/08  ************************************
     C****************     KFLD           SREFDT
     C                   KFLD                    X4EFDT            7 0
     C*** END CHG *****  04/01/08  ************************************
     C     PXKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SRCLAS
     C                   KFLD                    SREXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      **** END   ADD **** 10/06/99 ************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C*** BEG CHG *****  04/20/09  ************************************
     C                   KFLD                    SCEXT
     C*** END CHG *****  04/20/09  ************************************
     C*
     C     EPKY1         KLIST
     C                   KFLD                    SRCO#
     C                   KFLD                    SRFND
     C                   KFLD                    SRFYR
     C                   KFLD                    SREMP#
     C                   KFLD                    SRDIV
     C                   KFLD                    SRCLAS
     C                   KFLD                    SRSPEC
     C*** BEG CHG *****  04/20/09  ************************************
     C                   KFLD                    SREXT
     C*** END CHG *****  04/20/09  ************************************
      ***** BEG ADD ******* 06/25/01 **********************************
     C                   KFLD                    SREFDT
      ***** END ADD ******* 06/25/01 **********************************
     C*
     C***  BEGIN ADD  ***  01/28/99  *********************************
     C     EPKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C*** BEG CHG *****  04/20/09  ************************************
     C                   KFLD                    SCEXT
     C*** END CHG *****  04/20/09  ************************************
     C                   KFLD                    XXEFDT
     C***  END   ADD  ***  01/28/99  *********************************
     C*
     C*  PAYROLL MASTER
     C*
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C***  BEG CHG ******  04/01/08  *********************************
     C*************        KFLD           XXEFDT
     C                   KFLD                    X3EFDT            7 0
     C***  END CHG ******  04/01/08  *********************************
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C*
     C*  SELF-REPORTING PAYROLL TRANSACTION
     C*
     C     SRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXRPMY
     C*
     C*** BEG ADD *******  02/18/03  *********************************
     C     SRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXTODT            7 0
     C*** BEG ADD *******  02/18/03  *********************************
     C***  BEGIN ADD  ***  11/02/98  *********************************
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY
     C***  END   ADD  ***  11/02/98  *********************************
     C*
      **** BEG   ADD **** 02/18/03 *********************************************
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      **** END   ADD **** 02/18/03 *********************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SRRPMY        KKRPMY
      ***** BEG ADD ******* 04/23/01 **********************************
     C     *LIKE         DEFINE    SRRPMY        X1PFDT
     C     *LIKE         DEFINE    SRRPMY        X1PTDT
      ***** END ADD ******* 04/23/01 **********************************
     C     *LIKE         DEFINE    SRCLAS        KKCLAS
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    SCEFDT        X1EFDT
     C     *LIKE         DEFINE    SRRPMY        XXRPMY
     C     *LIKE         DEFINE    SCAMT         XXAMT
     C     *LIKE         DEFINE    SCRPMY        ##RPMY
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C     *LIKE         DEFINE    CLAS          XXCLAS
     C***  END   ADD  ***  10/21/98  *********************************
     C***  BEGIN ADD  ***  11/02/98  *********************************
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C***  END   ADD  ***  11/02/98  *********************************
      **** BEGIN ADD **** 10/06/99 ************************************
     C     *LIKE         DEFINE    PXST          XXST
      **** END   ADD **** 10/06/99 ************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C*** BEGIN CHG ***  06/01/06  ***********************************
     C*****/COPY SCOPYWW02C
     C/COPY SCOPYWW03C
     C*** END CHG *****  06/01/06  ***********************************
     C*
     C*================================================================
     C* DELETE - Delete requested record
     C*================================================================
     C*
     C     DELETE        BEGSR
     C*
     C     @@RRN         CHAIN     PFRCDF                             60
     C*
     C*** BEGIN CHG ***  03/25/98  ************************************
     C*****      EPKY1     CHAINDBRCDF3              60
     C     EPKY1         CHAIN     PEP02                              60
     C*** END CHG *****  03/25/98  ************************************
     C*
     C     *IN60         IFEQ      *OFF
     C                   SUB       SRAMTP        EPSAPR
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   EXSR      UPDTEP
     C***  END   ADD  ***  01/25/99  *********************************
     C*** BEGIN CHG ***  03/25/98  ************************************
     C*****                UPDATDBRCDF3
     C                   UPDATE    PEP02
     C*** END CHG *****  03/25/98  ************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     @@RRN         DELETE    PFRCDF                             60
     C*
     C                   ENDSR                                                  DELETE
     C*
     C*================================================================
     C* ERRCKA - Error check all subfile records (entire screen)
     C*================================================================
     C*
     C     ERRCKA        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCKA
     C*
     C*================================================================
     C* ERRCKC - Error check control record prior to subfile
     C*================================================================
     C*
     C     ERRCKC        BEGSR
      *** BEG ADD ***** 02/18/03 *********************************************
     C     *IN45         IFEQ      *OFF
      *** END ADD ***** 02/18/03 *********************************************
     C*
     C*  CHECK REPORTING FOR VALID MONTH/YEAR
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPMY
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        30  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  MONTH/YEAR CANNOT BE ADDED MORE THAN ONCE
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SRKY          CHAIN     DBRCDF                             60
     C  N60              SETON                                        31  90
     C                   ENDIF                                                  *IN90=*OFF
      ***** BEG ADD ******* 04/23/01 **********************************
      *
      *  Month/Year must be within employer policy effective date range
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     ELPFDT        X1PFDT
     C                   MOVEL     ELPTDT        X1PTDT
     C                   MOVE      ELPTDT        XXTMP             4 0
      *
     C     XXTMP         IFEQ      101                                          1ST OF MONTH
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DT8             8 0
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DT8
     C                   PARM      1             ##PAST            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1PTDT
     C                   ENDIF                                                  XXTMP=101
      *
      *
      ****** Beg add ******* 12/15/15 ***********************************
     C                   Eval      intSysDat = %Dec(%Date():*cymd)
     C                   Movel     intSysDat     intDate
      ****** End add ******* 12/15/15 ***********************************
      *
     C     XXRPMY        IFLT      X1PFDT
     C     XXRPMY        ORGT      X1PTDT
      ****** Beg add ******* 12/15/15 ***********************************
     C                   Move      'WC31373'     strMsgId
      ****** End add ******* 12/15/15 ***********************************
     C                   SETON                                        90  78
      ****** Beg add ******* 12/15/15 ***********************************
     C                   ElseIf    XXRPMY  > intDate
     C                   Move      'WC40162'     strMsgId
     C                   SETON                                        90  78
      ****** End add ******* 12/15/15 ***********************************
     C                   ENDIF                                                  XXRPMY
      *
     C                   ENDIF                                                  *IN90=*OFF
      *
      ***** END ADD ******* 04/23/01 **********************************
      *** BEG ADD ***** 02/18/03 **************************************
     C                   ELSE
     C*
     C*  VALID DATE IF FREQUENCY IS WEEKLY
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        30  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  REPORTING DATE CANNOT BE ADDED MORE THAN ONCE
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SRKY1         CHAIN     DBRCDF4                            60
     C  N60              SETON                                        31  90
     C                   ENDIF                                                  *IN90=*OFF
      *
      *  Report date must be within employer policy effective date range
      *
     C     *IN90         IFEQ      *OFF
      *
     C     XXTODT        IFLT      ELPFDT
     C     XXTODT        ORGE      ELPTDT
     C                   SETON                                        90  78
     C                   ENDIF                                                  XXTODT
      *
     C                   ENDIF                                                  *IN90=*OFF
      *
      *  Report date day of week must match billing schedule code day of week
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      SCTODT        ##RPDT            8 0
     C                   CALL      'SRDAY8'
     C                   PARM                    ##RPDT
     C                   PARM      *BLANK        ##SDAY            4
     C                   PARM      *BLANK        ##LDAY            9
     C                   PARM      *BLANK        ##ERR             1
     C                   MOVEL     ##SDAY        XXDOWK            2
      *
     C     XXDOWK        IFNE      SVDOWK
     C                   SETON                                        62  90
     C                   ENDIF                                                  XXDOWK
      *
     C                   ENDIF                                                  *IN90=*OFF
      *
     C                   ENDIF                                                  *IN45=*OFF
      *** END ADD ***** 02/18/03 **************************************
      *
      ***** BEG ADD ***** 04/26/06 ***********************************
     C  N90
     CAN 46SCLATE        IFNE      'L'
     C     SCLATE        ANDNE     *BLANK
     C                   SETON                                        47  90
     C                   ENDIF                                                  SCLATE<> L
      ***** END ADD ***** 04/26/06 ***********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*
     C***  Begin Add  ***  04/13/07  **************************************
     C     *LIKE         DEFINE    XXTODT        XXRPDT
     C*
     C                   SELECT
     C     USEDT8        WHENEQ    *ON
     C                   Z-ADD     XXTODT        XXRPDT
     C     USEDT6        WHENEQ    *ON
     C     XXRPMY        MULT      100           XXRPDT
     C                   ADD       1             XXRPDT
     C                   ENDSL
     C***  End   Add  ***  04/13/07  **************************************
     C*
     C                   ENDSR                                                  ERRCKC
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     SCCLAS        IFNE      *ZEROS
     C     SCCLAS        COMP      SVCLAS                                 41
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF                                                  SCCLAS<>*ZEROS
     C*
     C*  VALIDATE EFFECTIVE DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
      ***** BEG ADD ******* 10/11/06 **********************************
     C     SCCLAS        IFNE      *ZERO
      ***** END ADD ******* 10/11/06 **********************************
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C***  BEGIN ADD  ***  03/26/98  *********************************
     C  N90XXEFDT        IFLT      ELPFDT
     C     XXEFDT        ORGT      ELPTDT
     C                   SETON                                        75  90
     C                   ENDIF                                                  ELPFDT>XXEFDT>ELPTDT
     C***  END   ADD  ***  03/26/98  *********************************
      *
      ***** BEG ADD ******* 10/11/06 **********************************
     C                   ENDIF                                                  SCDATE <> 0
      ***** END ADD ******* 10/11/06 **********************************
      ***** BEG ADD ******* 09/17/99 **********************************
      *
      * When Effective Date entered, Class must also be entered.
      *
     C  N90*IN41         IFEQ      *OFF
     C     SCEFDT        ANDNE     *ZERO
     C     SCCLAS        ANDEQ     *ZERO
     C                   SETON                                        7790
     C                   ENDIF                                                  *IN41
      *
      ***** END ADD ******* 09/17/99 **********************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X3EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X3EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C***  Begin Add  ***  04/13/07  **************************************
     C  N90PRKY          SETGT     PPR01
     C  N90PRKY1         READPE    PPR01                                  60
     C***  End   Add  ***  04/13/07  **************************************
     C*
     C*  CHECK IF NEW ENTRY
     C*
     C  N90SCCLAS        IFNE      SVCLAS
     C     SCSPEC        ORNE      SVSPEC
     C*
     C*  VALIDATE CLASS CODE FOR NEW ENTRY
     C*
     C*** BEGIN CHG ***  03/02/99  ************************************
     C**N90***   PRKY1     CHAINPPR01                60
     C***  Begin Del  ***  04/13/07  **************************************
     C**N90***** PRKY      SETGTPPR01
     C**N90***** PRKY1     REDPEPPR01                    60
     C***  End   Del  ***  04/13/07  **************************************
     C*** END CHG *****  03/02/99  ************************************
     C*
     C  N90*IN60         IFEQ      *ON
     C     PRSTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON
     C*
      ***** BEG ADD ******* 10/07/03 **********************************
     C  N90PREXDT        IFNE      *ZERO
     C     PREXDT        ANDLE     XXEFDT
     C                   SETON                                        79  90
     C                   ENDIF                                                  *IN60=*ON
      ***** END ADD ******* 10/07/03 **********************************
     C*
     C***  Begin Chg  ***  04/13/07  **************************************
     C*** BEG DEL ***** 03/27/08 ******************************************
     C**N90***** PREXDT    IFNE *ZERO
     C********** PREXDT    ANDLEXXRPDT
     C**********           SETON                     343390
     C**********           ENDIF                           *IN60=*ON
     C*** END DEL ***** 03/27/08 ******************************************
     C***  End   Chg  ***  04/13/07  **************************************
     C*
     C     *IN90         IFEQ      *OFF
      *** BEG ADD ***** 08/10/05 **********************************
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   XXEFDT        QQEFDT
     C                   CALL      'SRLCM'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            3
     C                   PARM                    QQCLAS            5
     C                   PARM                    QQEFDT            7
     C                   PARM      *BLANKS       QQLCM             5
      *
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
      *
     C     PRRT          MULT(H)   XXLCM         PRRT
      *** END ADD ***** 08/10/05 **************************************
     C                   Z-ADD     PRRT          SCRATE
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2EFDT            7 0
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL     PXDESC        SCDESC
     C   60RSKY          CHAIN     PRS02                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL     PXDESC        SCDESC
     C     SCDESC        IFEQ      *BLANKS
      **** END   ADD **** 10/06/99 ************************************
     C                   MOVEL     PRDESC        SCDESC
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   ENDIF                                                  SCDESC
      **** END   ADD **** 10/06/99 ************************************
     C                   ENDIF                                                  *IN90=*OFF
      ***** BEG ADD ******* 10/19/99 **********************************
      *
      * Emp payroll eff dt < payroll expiration dt
      *
     C  N90PREXDT        IFNE      *ZERO
     C     XXEFDT        ANDGE     PREXDT
      *
     C                   Z-ADD     PREXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
      *
     C                   Z-ADD     PRXFER        SCXFER
     C     SCXFER        COMP      *ZERO                                  43
     C                   EXFMT     WDW01
     C                   SETON                                        90
     C                   ENDIF                                                  *IN60=*ON
      *
      ***** END ADD ******* 10/19/99 **********************************
     C*
     C*  THE SPECIAL DESIGNATION MUST BE 'F', 'S' OR BLANK OR '1'-'9'
     C*  FOR AIRCRAFT SURCHARGE
     C*
     C*** BEGIN DEL ***  11/13/02  ************************************
     C**N90****  SCSPEC    IFNE *BLANK
     C*
     C*********  SCCLAS    IFNE F2DCCD
     C*
     C*********  SCSPEC    IFNE 'F'
     C*********  SCSPEC    ANDNE'S'
     C*********            SETON                     73  90
     C*********            ENDIF                           SCSPEC<>F/S
     C*
     C*********            ELSE
     C*
     C*********  SCSPEC    IFLT '1'
     C*********  SCSPEC    ORGT '9'
     C*********            SETON                     73  90
     C*********            ENDIF                           1>SCSPEC>9
     C*
     C*********            ENDIF                           SCCLAS<>F2DCCD
     C*
     C*********            ENDIF                           SCSPEC<>*BLANK
     C*** END DEL *****  11/13/02  ************************************
     C*
     C*  CHECK TO BE SURE CLASS CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C***  BEGIN CHG  ***  10/21/98  *********************************
B02  C**N41N90** EPKY      CHAINDBRCDF3              60
     C**N41N90N60          SETON                     72  90
     C  N90*IN41         IFEQ      *OFF
     C*
     C                   MOVEL     SCCLAS        XXCLAS
     C                   MOVE      SCSPEC        XXCLAS
     C***  BEG ADD ******04/20/09************************************
     C                   MOVE      SCEXT         XXCLAS
     C***  END ADD ******04/20/09************************************
     C*
     C                   Z-ADD     1             X                 3 0
     C     XXCLAS        LOOKUP    CLAS(X)                                60
     C   60              SETON                                        72  90
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     EPKY          CHAIN     DBRCDF3                            60
     C*
     C  N60              Z-ADD     DBRRN3        SCRRN3
     C*
     C***  BEGIN ADD  ***  01/28/99  *********************************
     C     *IN60         IFEQ      *ON
     C     EPKY2         CHAIN(N)  PEP02                              60
     C  N60              SETON                                        76  90
     C                   ENDIF                                                  *IN60=*ON
     C***  END   ADD  ***  01/28/99  *********************************
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDIF                                                  *IN41=*OFF
     C***  END   CHG  ***  10/21/98  *********************************
     C*
      ***** BEG DEL ******* 10/01/02 **********************************
     C*********            ENDIF                           SCCLAS<>SVCLAS
      ***** END DEL ******* 10/01/02 **********************************
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *  EXTENSION MUST BE VALID.
      *
     C  N90SCEXT         IFNE      *BLANKS
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 10/06/99 ************************************
     C*** BEGIN CHG ***  06/01/06  ************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2EFDT            7 0
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     C   60RSKY          CHAIN     PRS02                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
      **** END   ADD **** 10/06/99 ************************************
     C*
     C     *IN60         IFEQ      *ON
     C     PXSTAT        OREQ      'I'
     C                   SETON                                        32  90
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SCEAPR<*ZERO
      *** END ADD *****  08/11/98  ************************************
      ***** BEG ADD ******* 10/01/02 **********************************
     C                   ENDIF                                                  SCCLAS<>SVCLAS
      ***** END ADD ******* 10/01/02 **********************************
     C*
     C***  Begin Chg  ***  04/13/07  **************************************
     C*** BEG DEL ***** 03/27/08 ******************************************
     C**N90***** PREXDT    IFNE *ZERO
     C********** PREXDT    ANDLEXXRPDT
     C********** SCAMT     ANDNE*ZERO
     C**********           SETON                     33  90
     C**********           ENDIF                           *IN60=*ON
     C*** END DEL ***** 03/27/08 ******************************************
     C***  End   Chg  ***  04/13/07  **************************************
     C*
     C*  AMOUNT CANNOT BE LESS THAN ZERO
     C*
     C  N90SCAMT         IFLT      *ZERO
     C                   SETON                                        82  90
     C                   ENDIF                                                  SCAMT<*ZERO
     C*
     C*  AMOUNT MUST BE ZERO IF EFFECTIVE DATE IS GREATER THAN RPMY
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
      ***** BEG CHG ******* 02/18/03 **********************************
     C************** N90      SCAMT     IFGT *ZERO
     C  N90
     CANN45SCAMT         IFGT      *ZERO
      ***** END CHG ******* 02/18/03 **********************************
     C     XXCYM         ANDGT     XXRPMY
     C                   SETON                                        83  90
     C                   ENDIF                                                  SCAMT>*ZERO
     C*
      ***** BEG ADD ******* 02/18/03 **********************************
     C  N90
     CAN 45SCAMT         IFGT      *ZERO
      ***** END CHG ******* 02/18/03 **********************************
     C     XXEFDT        ANDGT     XXTODT
     C                   SETON                                        83  90
     C                   ENDIF                                                  SCAMT>*ZERO
      ***** END ADD ******* 02/18/03 **********************************
     C*
     C*  AMOUNT CANNOT BE ENTERED WITHOUT AN EFFECTIVE DATE
     C*
     C  N90SCAMT         IFGT      *ZERO
     C     SCEFDT        ANDEQ     *ZERO
     C                   SETON                                        84  90
     C                   ENDIF                                                  SCAMT>*ZERO
     C*
     C  N90ELSTAT        IFEQ      'Y'
     C     SCCLAS        ANDNE     SVCLAS
     C                   EXFMT     WDWMSG
     C                   ENDIF                                                  ELSTAT<>Y
     C*
     C  N90              Z-ADD     SCCLAS        SVCLAS
     C  N90              MOVEL     SCSPEC        SVSPEC
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SCDBRR        CHAIN(N)  PFRCDF                             99
     C*
     C  N90SRDT          IFGT      SAVEDT
     C     SRDT          OREQ      SAVEDT
     C     SRTIME        ANDGT     SAVETM
     C                   SETON                                        85  90
     C                   ENDIF                                                  EPDT>SAVEDT
     C*
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     SCCLAS        XXCLAS
     C                   MOVE      SCSPEC        XXCLAS
     C***  BEG ADD ******04/20/09************************************
     C                   MOVE      SCEXT         XXCLAS
     C***  END ADD ******04/20/09************************************
     C*
     C                   Z-ADD     1             X                 3 0
     C     XXCLAS        LOOKUP    CLAS(X)                                60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    CLAS(X)                                60
     C                   MOVEL     SCCLAS        CLAS(X)
     C                   MOVE      SCSPEC        CLAS(X)
     C***  BEG ADD ******04/20/09************************************
     C                   MOVE      SCEXT         CLAS(X)
     C***  END ADD ******04/20/09************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C***  END   ADD  ***  10/21/98  *********************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********            Z-ADD*ZEROS    KKCLAS
      ***** END DEL ******* 04/24/01 **********************************
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   MOVE      *HIVAL        KKRPMY
      ***** END ADD ******* 04/24/01 **********************************
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C     KEYSET        BEGSR
     C                   ENDSR                                                  KEYSET
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C     KEYSFL        BEGSR
     C*
      ***** BEG ADD ******* 02/18/03 **********************************
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         KKTODT
      ***** END ADD ******* 02/18/03 **********************************
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         KKRPMY
     C*
      ***** BEG ADD ******* 04/24/01 **********************************
      *
      * Set Indicator for Subfile Display
      *
     C     SCCLAS        IFNE      *ZEROS
     C     SCCLAS        COMP      SVCLAS                                 41
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF                                                  SCCLAS<>*ZEROS
      *
      ***** END ADD ******* 04/24/01 **********************************
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
      ***** BEG ADD ******* 04/24/01 **********************************
      *
      *  Only allow search on new entries
      *
     C     HRRN          CHAIN     SCR01S                             60
      *
     C     SCCLAS        IFNE      *ZEROS
     C     SCCLAS        COMP      SVCLAS                                 41
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF                                                  SCCLAS<>*ZEROS
      *
     C     *IN41         IFEQ      *OFF
     C     HFLD          ANDNE     'SCRPMY'
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C                   SELECT
      ***** BEG ADD ******* 04/24/01 **********************************
     C     HFLD          WHENEQ    'SCAMT'
     C                   SETON                                        50  07
     C                   Z-ADD     HRRN          SCRRN1
     C                   WRITE     SCR01
      *
     C     HFLD          WHENEQ    'SCEFDT'
     C                   SETON                                        51  07
     C                   Z-ADD     HRRN          SCRRN1
     C                   WRITE     SCR01
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C     HFLD          WHENEQ    'SCCLAS'
     C     HFLD          OREQ      'SCSPEC'
     C*
     C     HRRN          CHAIN     SCR01S                             60
      *
      ***** BEG ADD ***** 12/16/04 ***********************************
     C*
      * If Screen date > zeros, pass into search pgm, else use  current dt
     C                   MOVE      *BLANKS       ##DATE            8
     C     SCEFDT        IFGT      *ZEROS
     C                   MOVE      SCEFDT        ##DATE
     C                   ELSE
     C                   MOVE      DSDATE        ##DATE
     C                   ENDIF
     C*
      ***** END ADD ***** 12/16/04 ***********************************
     C*
     C                   CALL      'W0630R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      ELRTCD        ##RTCD            3
     C                   PARM      2             ##VIEW            1 0
      ***** BEG ADD ***** 12/16/04 ***********************************
     C                   PARM      ##DATE        @@DATE            8
      ***** END ADD ***** 12/16/04 ***********************************
     C*
     C                   MOVE      *ON           *IN08
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   MOVE      *ON           *IN88
      *
      ***** BEG ADD ******* 04/24/01 **********************************
     C     HFLD          IFEQ      'SCCLAS'
     C                   SETON                                        52  07
     C                   ELSE
     C                   SETON                                        53  07
     C                   ENDIF
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C                   ELSE
     C*
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   SETON                                        54  07
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C     ##RRN         CHAIN     PPR02                              99
     C*
     C                   MOVE      PRCLAS        SCCLAS
     C*
     C                   MOVE      *ON           *IN89
     C*
     C                   Z-ADD     SCEFDT        X1EFDT
     C*
     C     SCEFDT        IFEQ      *ZERO
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        X1EFDT
     C                   ENDIF                                                  SCEFDT=*ZERO
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCRATE
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X3EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X3EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2EFDT            7 0
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL     PXDESC        SCDESC
     C   60RSKY          CHAIN     PRS02                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL     PXDESC        SCDESC
     C     SCDESC        IFEQ      *BLANKS
      **** END   ADD **** 10/06/99 ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     C**N60                MOVELPRDESC    SCDESC    P
     C                   MOVEL(P)  PRDESC        SCDESC
     C                   ENDIF                                                  SCDESC
      **** END   CHG **** 10/06/99 ************************************
     C  N60              Z-ADD     PRRT          SCRATE
     C*
     C                   ENDIF                                                  SCCLAS=*ZEROS
     C*
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      *OFF          *IN08
     C***  END   ADD  ***  10/21/98  *********************************
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   Z-ADD     HRRN          SCRRN1
     C                   WRITE     SCR01
      ***** END ADD ******* 04/24/01 **********************************
      *
      *
     C*_______________________________________________________________
     C*
      *** BEGIN ADD ***  08/11/98  ************************************
     C     HFLD          WHENEQ    'SCEXT'
      *
     C     HRRN          CHAIN     SCR01S                             60
     C*
     C                   CALL      'W0650R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ELCO#
     C                   PARM                    SCCLAS
      ***** BEG ADD ******* 10/01/02 **********************************
      ***** BEG CHG ******* 04/20/09 **********************************
     C************         PARM           ##ST    2
     C                   PARM                    RSJRST
      ***** END CHG ******* 04/20/09 **********************************
      ***** END ADD ******* 10/01/02 **********************************
      *
     C                   MOVE      *ON           *IN08
      *
      ***** BEG DEL ******* 04/24/01 **********************************
     C********** *INKC     IFEQ *ON
     C**********           MOVE *ON       *INKC
     C**********           ENDIF                           QQRTC=A
      ***** END DEL ******* 04/24/01 **********************************
      *
     C     ##RRN         IFEQ      *ZEROS
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PPX02                              99
      *
     C                   MOVE      PXEXT         SCEXT
     C                   MOVE      PXEXT         SVEXT
      *
      *
     C                   MOVEL(P)  PXDESC        SCDESC
     C                   ENDIF                                                  ##RRN IFNE
      *** END ADD *****  08/11/98  ************************************
     C*
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   SETON                                        54  07
      ***** END ADD ******* 04/24/01 **********************************
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      *OFF          *IN08
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   Z-ADD     HRRN          SCRRN1
     C                   WRITE     SCR01
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C                   ENDSL
     C*
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   ENDIF                                                  *IN41 = *OFF
      ***** END ADD ******* 04/24/01 **********************************
      *
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
      *
      ***** BEG ADD ***** 04/26/06 ***********************************
     C                   MOVE      SRSTAT        SCLATE
      ***** END ADD ***** 04/26/06 ***********************************
     C*
     C*** BEG CHG ****** 02/18/03 *************************************
     C******************** Z-ADDDBRRN     SCDBRR
     C  N45              Z-ADD     DBRRN         SCDBRR
     C   45              Z-ADD     DBRRN4        SCDBRR
     C*** END CHG ****** 02/18/03 *************************************
     C                   Z-ADD     *ZEROS        SCRRN3
     C*
     C                   Z-ADD     SREFDT        XXEFDT
     C                   Z-ADD     SREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     SRAMTP        SCAMT
     C                   Z-ADD     SRAMTP        SVAMT
     C                   Z-ADD     SRCLAS        SCCLAS
     C                   Z-ADD     SRCLAS        SVCLAS
     C                   MOVEL     SRSPEC        SCSPEC
     C                   MOVEL     SRSPEC        SVSPEC
     C*** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     SREXT         SCEXT
     C                   MOVEL     SREXT         SVEXT
     C*** END ADD *****  08/11/98  ************************************
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        SCDBRR
     C                   Z-ADD     DBRRN3        SCRRN3
     C*
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   Z-ADD     EPCLAS        SVCLAS
     C                   MOVEL     EPSPEC        SCSPEC
     C                   MOVEL     EPSPEC        SVSPEC
     C*** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     EPEXT         SCEXT
     C                   MOVEL     EPEXT         SVEXT
     C*** END ADD *****  08/11/98  ************************************
     C*
     C                   ENDIF                                                  *IN40=*OFF
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCRATE
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X3EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X3EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2EFDT            7 0
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL     PXDESC        SCDESC
     C   60RSKY          CHAIN     PRS02                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SCCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL     PXDESC        SCDESC
     C     SCDESC        IFEQ      *BLANKS
      **** END   ADD **** 10/06/99 ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     C**N60                MOVELPRDESC    SCDESC
     C                   MOVEL     PRDESC        SCDESC
     C                   ENDIF                                                  SCDESC
      **** END   CHG **** 10/06/99 ************************************
     C  N60              Z-ADD     PRRT          SCRATE
     C*
     C                   MOVE      *ON           *IN08
     C                   MOVE      *ON           *IN41
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
      ***** BEG ADD ***** 04/26/06 ***********************************
     C                   MOVE      SCLATE        SRSTAT
      ***** END ADD ***** 04/26/06 ***********************************
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         SRRPMY
     C*
      *** BEG ADD ***** 02/18/03 *************************************
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRTODT
      *** END ADD ***** 02/18/03 *************************************
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SREFDT
     C                   MOVEL     SRDTO         EPEFDT
     C*
     C                   Z-ADD     SCCLAS        SRCLAS
     C                   Z-ADD     SCCLAS        EPCLAS
     C                   MOVEL     SCSPEC        SRSPEC
     C                   MOVEL     SCSPEC        EPSPEC
     C*** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     SCEXT         SREXT
     C                   MOVEL     SCEXT         EPEXT
     C*** END ADD *****  08/11/98  ************************************
     C*
     C                   Z-ADD     SCAMT         SRAMTP
     C                   Z-ADD     SCAMT         EPSAPR
     C*
     C                   MOVEL     'S'           SRTYFL
     C*
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   SETOFF                                       505152
     C                   SETOFF                                       535455
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *IN09         ANDEQ     *OFF
     C*
     C*** BEGIN ADD ***  11/14/02  ************************************
     C*
     C                   MOVE      *BLANK        XXFLAG            1
     C*
     C     SRKY          CHAIN     DBRCDF                             60
     C*
     C     *IN60         DOWEQ     *OFF
     C     XXFLAG        ANDEQ     *BLANK
     C*
     C     SREXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X4EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   MOVE(P)   SREFDT        #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SRCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X4EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C********** PXKY2     CHAINPPX01                60
     C     PXKY2         SETGT     PPX01
     C     PXKY3         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *ON
     C     RSKY          CHAIN     PRS02                              60
     C                   MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X4EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   MOVE(P)   SREFDT        #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   SRCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X4EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C********** PXKY2     CHAINPPX01                60
     C     PXKY2         SETGT     PPX01
     C     PXKY3         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     *IN60         IFEQ      *OFF
     C     PXCODE        ANDNE     *ZERO
     C                   MOVE      'Y'           XXFLAG
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SREXT IFNE
     C*
     C     SRKY          READE     DBRCDF                                 60
     C                   ENDDO
     C*
     C     XXFLAG        IFNE      *BLANK
     C*
     C                   CALL      'W0660R4'                            99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C                   PARM      SCRPMY        ##RPMY
     C*
     C                   ENDIF                                                  XXFLAG IFNE
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*** END ADD *****  11/14/02  ************************************
     C*
     C                   CALL      'W0660R1'                            99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C                   PARM      SCRPMY        ##RPMY
      *** BEG ADD **** 02/18/03 ***************************************
     C                   PARM      SCTODT        ##TODT            8 0
      *** END ADD **** 02/18/03 ***************************************
     C*
     C*** BEGIN ADD ***  11/14/02  ************************************
     C                   ENDIF                                                  *INKC=*OFF
     C*** END ADD *****  11/14/02  ************************************
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   MOVEA     '00000000'    *IN(21)
     C                   EXSR      CLRSF1
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   SETOFF                                       505152
     C                   SETOFF                                       535455
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCAMT
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   Z-ADD     *ZEROS        SVAMT
     C***  END   ADD  ***  01/25/99  *********************************
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C     @@MODE        IFEQ      'E'                                          DELETE
     C                   EXSR      DELETE
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF                                                  @@MODE=E
     C*
     C     @@CALL        COMP      ' '                                    40    ADD MONTH
     C*
     C                   CLEAR                   SCRPMY
     C*** BEG ADD *******  02/18/03  *********************************
     C                   CLEAR                   SCTODT
     C*** END ADD *******  02/18/03  *********************************
     C***  BEGIN ADD  ***  06/29/00  *********************************
     C                   MOVE      *OFF          *IN06
     C***  END   ADD  ***  06/29/00  *********************************
     C*
      *** BEG ADD **** 02/18/03 **************************************
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        44
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  WCFL23 IFEQ
      *
     C                   SETOFF                                       45
     C     *IN44         IFEQ      *ON
      *
     C     @@RRN2        CHAIN     DBRCDF2                            99
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C                   MOVEL     BSDOWK        SVDOWK            2
      *
     C                   SELECT
     C     BSFREQ        WHENEQ    'W'
     C                   MOVEL(P)  'Weekly'      SCFREQ
     C                   SETON                                        45
     C     BSFREQ        WHENEQ    'M'
     C                   MOVEL(P)  'Monthly'     SCFREQ
     C                   OTHER
     C                   MOVE      *BLANKS       SCFREQ
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN44 = *ON
      *** END ADD **** 02/18/03 **************************************
     C*
     C***  Begin Add  ***  04/13/07  **************************************
     C                   MOVE      *OFF          USEDT8            1
     C                   MOVE      *OFF          USEDT6            1
     C*
     C                   SELECT
     C     *IN45         WHENEQ    *ON
     C                   MOVE      *ON           USEDT8
     C     *IN45         WHENEQ    *OFF
     C                   MOVE      *ON           USEDT6
     C                   ENDSL
     C***  End   Add  ***  04/13/07  **************************************
     C*
     C     @@CALL        IFEQ      'P'
     C*
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** @@RRN     CHAINPFRCDF               99
     C     @@RRN         CHAIN(N)  PFRCDF                             99
     C***  End   Chg  ***  01/03/07  **************************************
     C*
     C                   Z-ADD     SRRPMY        KKRPMY
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
      *** BEG ADD **** 02/18/03 **************************************
     C                   Z-ADD     SRTODT        KKTODT
     C                   Z-ADD     SRTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
      *** END ADD **** 02/18/03 **************************************
     C*
     C                   MOVE      ' '           @@CALL
     C                   ENDIF                                                  @@CALL=P
     C*
     C     @@RRN2        CHAIN     DBRCDF2                            99
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C***  BEGIN ADD  ***  11/02/98  *********************************
     C*
     C*  DISPLAY MESSAGE WINDOW IF TRYING TO ADD TO AN ESTIMATING ACCT
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     BSSEFL        ANDEQ     'E'
     C                   MOVE      'N'           SCESTF
     C                   EXFMT     WDWEST
     C*
     C     SCESTF        IFNE      'Y'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF                                                  SCESTF<>Y
     C*
     C                   ENDIF                                                  *IN60=*OFF/BSSEFL=E
     C*
     C                   ENDIF                                                  *IN40=*ON
     C***  END   ADD  ***  11/02/98  *********************************
      ***** BEG ADD ******* 03/14/00 **********************************
      *
      *  Informational edit if account is audited
      *
     C     @@MODE        IFEQ      'A'
     C     ELFAF         ANDEQ     'A'
     C                   EXFMT     WDW02
     C   KL              MOVE      *ON           *INLR
     C   KL              RETURN
     C                   ENDIF                                                  @@MODE=A/ELFAF=A
      *
      ***** END ADD ******* 03/14/00 **********************************
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                  ELFAF=A
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
      **** BEGIN DEL **** 02/12/98 *********************************************
     C******************** DO   5         I1      30
     C********************
     C********** CX,I1     IFNE *ZERO
     C******************** Z-ADDCX,I1     SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCCXDT
     C******************** ELSE
     C******************** LEAVE
     C******************** ENDIF                           CX,I1<>*ZERO
     C********************
     C******************** ENDDO                           I1->5
      **** END   DEL **** 02/12/98 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN42
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     FNKY          CHAIN     PF201                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET AGENT RECORDS
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C                   CLEAR                   CLAS
     C***  END   ADD  ***  10/21/98  *********************************
      *
      ***** BEG ADD ***** 04/26/06 ***********************************
     C                   MOVE      *OFF          *IN46
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     CLTID         IFEQ      '046'
     C                   MOVE      *ON           *IN46
     C                   ENDIF
      ***** END ADD ***** 04/26/06 ***********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
      ***** BEG CHG ******* 02/18/03 **********************************
     C**************N40      KEY1      READEDBRCDF                   95
     C  N40
     CANN45KEY1          READE     DBRCDF                                 95
     C  N40
     CAN 45KEY1          READE     DBRCDF4                                95
      ***** END CHG ******* 02/18/03 **********************************
     C   40KEY1          READE     DBRCDF3                                95
     C*
     C     *IN40         IFEQ      *OFF
      ***** BEG ADD ******* 02/18/03 **********************************
     C     *IN45         ANDEQ     *OFF
      ***** END ADD ******* 02/18/03 **********************************
     C     SRRPMY        ANDNE     KKRPMY
     C                   MOVE      *ON           *IN95
     C                   ENDIF                                                  *IN40=*OFF
     C*
      ***** BEG ADD ******* 02/18/03 **********************************
     C     *IN40         IFEQ      *OFF
     C     *IN45         ANDEQ     *ON
     C     SRTODT        ANDNE     KKTODT
     C                   MOVE      *ON           *IN95
     C                   ENDIF                                                  *IN40=*OFF
      ***** END ADD ******* 02/18/03 **********************************
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C***95*****           MOVE *OFF      *IN41
      ***** END DEL ******* 04/24/01 **********************************
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read prior record from file
     C*================================================================
     C     READP         BEGSR
     C*
      ***** BEG CHG ******* 04/24/01 **********************************
     C**N40***** KEY1      READEDBRCDF                   95
     C***40***** KEY1      READEDBRCDF3                  95
      ***** BEG CHG ******* 02/18/03 **********************************
     C**************N40      KEY1      REDPEDBRCDF                   95
     C  N40
     CANN45KEY1          READPE    DBRCDF                                 95
     C  N40
     CAN 45KEY1          READPE    DBRCDF4                                95
      ***** END CHG ******* 02/18/03 **********************************
     C   40KEY1          READPE    DBRCDF3                                95
      ***** END CHG ******* 04/24/01 **********************************
     C*
     C     *IN40         IFEQ      *OFF
      ***** BEG ADD ******* 02/18/03 **********************************
     C     *IN45         ANDEQ     *OFF
      ***** END ADD ******* 02/18/03 **********************************
     C     SRRPMY        ANDNE     KKRPMY
     C                   MOVE      *ON           *IN95
     C                   ENDIF                                                  *IN40=*OFF
     C*
      ***** BEG ADD ******* 02/18/03 **********************************
     C     *IN40         IFEQ      *OFF
     C     *IN45         ANDEQ     *OFF
     C     SRTODT        ANDNE     KKTODT
     C                   MOVE      *ON           *IN95
     C                   ENDIF                                                  *IN40=*OFF
      ***** END ADD ******* 02/18/03 **********************************
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
      ***** BEG CHG ******* 04/24/01 **********************************
     C********** KEY1A     SETLLDBRCDF               96
     C********** KEY1      SETLLDBRCDF3              96
      ***** BEG CHG ******* 02/18/03 **********************************
     C****************  N40      KEY1A     SETLLDBRCDF               96
     C  N40
     CANN45KEY1A         SETLL     DBRCDF                             96
     C  N40
     CAN 45KEY1B         SETLL     DBRCDF4                            96
      ***** END ADD ******* 02/18/03 **********************************
     C   40KEY1          SETLL     DBRCDF3                            96
      ***** END CHG ******* 04/24/01 **********************************
     C*
     C     *IN96         IFEQ      *ON
     C                   Z-ADD     *ZEROS        DBRRN
     C                   ENDIF                                                  *IN96=*ON
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C     UPDATR        BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SRUSER
     C                   MOVEL     Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVEL     DSTIME        SRTIME
     C*
     C                   UPDATE    PFRCDF
     C*
     C*** BEGIN CHG ***  03/25/98  ************************************
     C*****      EPKY1     CHAINDBRCDF3              60
     C     EPKY1         CHAIN     PEP02                              60
     C*** END CHG *****  03/25/98  ************************************
     C*
     C     *IN60         IFEQ      *OFF
     C     SCAMT         SUB       SVAMT         XXAMT
     C                   ADD       XXAMT         EPSAPR
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   EXSR      UPDTEP
     C***  END   ADD  ***  01/25/99  *********************************
     C*** BEGIN CHG ***  03/25/98  ************************************
     C*****                UPDATDBRCDF3
     C                   UPDATE    PEP02
     C*** END CHG *****  03/25/98  ************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C*===============================================================
     C* UPDTEP - SUBROUTINE FOR UPDATING RECORDS IN WMEPP
     C*===============================================================
     C     UPDTEP        BEGSR
     C*
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   MOVEL     DSTIME        EPTIME
     C*
     C                   ENDSR                                                  UPDTEP
     C***  END   ADD  ***  01/25/99  *********************************
     C*===============================================================
     C* WRITEP - SUBROUTINE FOR WRITING DATA BASE RECORDS TO WMEPP
     C*===============================================================
     C     WRITEP        BEGSR
     C*
     C                   CLEAR                   PEP01
     C*
     C                   Z-ADD     ELCO#         EPCO#
     C                   Z-ADD     ELFND         EPFND
     C                   Z-ADD     ELEMP#        EPEMP#
     C                   Z-ADD     ELDIV         EPDIV
     C                   Z-ADD     ELFYR         EPFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   Z-ADD     SCAMT         EPSAPR
     C                   MOVEL     'A'           EPSTAT
     C                   Z-ADD     SCRATE        EPARAT
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPIPGM
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPIDT
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     DSTIME        EPITM
     C                   MOVEL     DSTIME        EPTIME
     C*
     C                   WRITE     PEP01
     C*
     C                   FEOD      WMEPP
     C*
     C                   ENDSR                                                  WRITEP
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C     WRITER        BEGSR
     C*
     C     SCCLAS        IFNE      *ZERO
     C*
     C                   CLEAR                   PFRCDF
     C*
     C                   Z-ADD     ELCO#         SRCO#
     C                   Z-ADD     ELFND         SRFND
     C                   Z-ADD     ELEMP#        SREMP#
     C                   Z-ADD     ELDIV         SRDIV
     C                   Z-ADD     ELFYR         SRFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SRIUSR
     C                   MOVEL     Q1USER        SRUSER
     C                   MOVEL     Q1PGM         SRIPGM
     C                   MOVEL     Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRIDT
     C                   MOVEL     SRDTO         SRDT
     C                   MOVEL     DSTIME        SRITM
     C                   MOVEL     DSTIME        SRTIME
     C*
     C                   WRITE     PFRCDF
     C*
     C                   FEOD      WASRP
     C*
     C     SCRRN3        IFEQ      *ZEROS
     C*
     C                   EXSR      WRITEP
     C*
     C                   ELSE
     C*
     C     SCRRN3        CHAIN     WMEPP                              60
     C     SCAMT         SUB       SVAMT         XXAMT
     C                   ADD       XXAMT         EPSAPR
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   EXSR      UPDTEP
     C***  END   ADD  ***  01/25/99  *********************************
     C                   UPDATE    PEP01
     C*
     C                   ENDIF                                                  SCRRN3=*ZEROS
     C*
     C                   ENDIF                                                  SCCLAS<>*ZERO
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
     C*
** Column headings
Opt
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
210F10=Search               YNN
212F12=Cancel               YYN
214F14=DARTS                NNN
3
