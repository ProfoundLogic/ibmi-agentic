     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0060R - WORK WITH PAYROLLS
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL CLASS  *
     H*                CODES FOR A SELECTED EMPLOYER.  EACH CODE CAN  *
     H*                BE ACCESSED FOR UPDATE, DISPLAY OR SELECTION   *
     H*                DEPENDING ON THE CALLING PROGRAM.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0011R - WORK WITH POLICY YEARS                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0060R02 - EMPLOYER PAYROLL INFORMATION (OPT 2/5)          *
     H*    W0066R2  - AUDIT PAYROLL INFORMATION (F 15)                *
     H*    W0660R   - S/R PAYROLL INFORMATION (F 16)                  *
     H*    W0062R   - WORK WITH EMPLOYEES (OPT8)                      *
     H*                                                               *
     H*  CREATED: 04/24/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Add column to display the new Payroll Extension*
     H*                field and to adjust display.                   *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 03/29/06 C:132 PROJECT WA091 - 44078                          *
     H*              - Change logical WMPRL1 to WMPRL4, to show the   *
     H*                payroll records with status of "I" Inactive.   *
     H*                                                               *
     H* 04/12/05 C:61  PROJECT WC1283 135WBW                          *
     H*              - Add option "8=Employees".                      *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 01/20/04 C:09  PROJECT: CM027                                 *
     H*                We will no longer generate a separate NCCI     *
     H*                midterm endorsement form (WC890600A) if        *
     H*                payroll changes are made.  One form will       *
     H*                be created with Item 4.*Class, Rate, Other     *
     H*                (WC890615) marked or Interim Adjustment of     *
     H*                Premium (WC890616) marked or both, as appr.    *
     H*                (see W0090R, SR89600 for other changes).       *
     H*                                                               *
     H* 08/30/01 C:90  PROJECT: WC1035                                *
     H*                Expand premium fields from 9.2 to 11.2         *
     H*                                                               *
     H* 08/15/01 C:90  PROJECT: WC1035                                *
     H*                Expand rate field from 5.2 to 7.2              *
     H*                                                               *
     H* 12/05/00 C:61  PROJECT WC577                                  *
     H*                Don't allow 'F21=S/R Payroll Edit' when called *
     H*                from QUOTE.                                    *
     H*                                                               *
     H*  09/20/99 C:90 PROJECT: WC945                                 *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H*  07/28/99 C:61 PROJECT WC931                                  *
     H*                Add F17=Subset.  Active/Inactive               *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H*  01/29/99  C:33  SEQ#: WA081/15499                            *
     H*                  IF CALLED FROM REVISED BILLING, DO NOT ALLOW *
     H*                  F15=AUDITS AND ONLY ALLOW DISPLAY OF S/R     *
     H*                  PAYROLLS.                                    *
     H*                                                               *
     H* 01/14/99  C:09   PROJECT: SUPPORT WA033 13709                 *
     H*                  TO PREVENT DECIMAL DATA ERROR, FOR PAYROLL   *
     H*                  CHANGES ALSO CHECK (LDENDA) TO ENSURE        *
     H*                  PAYROLL CHANGES WERE MADE THRU MIDTERM       *
     H*                  REVISED BILLING OPTION.                      *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H*  11/17/98 C:33 SEQ#: DR101/13559                              *
     H*                ONLY LOAD 'Q' TO QCQTE.                        *
     H*                                                               *
     H*  11/05/98 C:33 SEQ#: WA100/14705                              *
     H*                ONLY CHECK FOR THE EXITENCE OF THE FIRST CLAIM,*
     H*                WITH THE SAME CLASS CODE, THAT IS NOT CLOSED OR*
     H*                HAS PAYMENTS                                   *
     H*                                                               *
     H*  11/04/98 C:33 SEQ#: WA091/14817                              *
     H*                CHANGE PAGING KEY TO INCLUDE THE SPECIAL DESC  *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H*  08/10/98  C:61 Project: 48WLC                                *
     H*                  Display old Emp#/Div from Employer Cross     *
     H*                  Reference (WZEXP).                           *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H*  08/07/98  C:78  SEQ#: DR095/12876                            *
     H*                  QCQTE NOT LOADED QITH EITHER A 'Q' OR BLANKS.*
     H*                                                               *
     H*  07/10/98  C:33  SEQ#: WA093/12013                            *
     H*                  CORRECT DISPLAY OF ADDITIONAL LOAD.          *
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WA093/12385                            *
     H*                  WRITE RECORDS TO BILL TRANSACTION FILE.      *
     H*                                                               *
     H* 05/19/98  C:33  SEQ#: WA078/11549                             *
     H*                 USE EMP RATE CODE WHEN GETTING RATE CODE DESC *
     H*                                                               *
     H* 05/14/98  C:33  SEQ#: WA091/11509                             *
     H*                 ACCOUNT FOR NEGATIVE YTD S/R PAYROLLS         *
     H*                                                               *
     H* 03/30/98  C:78  PROJECT: WC825                                *
     H*                 ADD DARTS ACCESS TO SCR01. F14=DARTS.         *
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
     H*  02/05/98  C:57  PROJECT: CM027                               *
     H*                  IDENTIFY THAT CHANGES/ADD/DELETS HAVE        *
     H*                  OCCURRED IN W0060R02 WHEN CALLED FROM EMPLYR *
     H*                  MID TERM ENDORSEMENTS.  LDPYRL WILL BE 'Y'.  *
     H*                  PROCESS THIS AT END OF PGM (SR = PGMEND).    *
     H*                                                               *
     H*  12/22/97  C:33  PROJECT: WC577                               *
     H*                  ADD F19 & F21=EDIT REQUEST (ALL & S/R)       *
     H*                                                               *
     H*  12/09/97  C:33  PROJECT: WC577                               *
     H*                  OPT 20 SHOULD READ 'CHANGE STATUS'           *
     H*                                                               *
     H*  11/12/97  C:33  PROJECT: WC577                               *
     H*                  USE DIFFERENT EDIT MASK FOR PAYROLL AMOUNTS  *
     H*                                                               *
     H*  10/30/97  C:33  PROJECT: WC577                               *
     H*                  REMOVE OPTIONS FOR QUOTE PROCESSING          *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  63 - Display ol Emp#/Div in header info                      *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0060F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
     FWMEPP     IF   E             DISK    USROPN
     F                                     RENAME(PEP01:PFRCDF)
     F***** BEG CHG ******* 04/20/09 **********************************
     F******WMEPL6  IF  E           K        DISK                     UC
     FWMEPL6E   IF   E           K DISK    USROPN
      ***** END CHG ******* 04/20/09 **********************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEP01:DBRCDF)
      *
     FWDELP     IF   E             DISK    USROPN
      *
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
      ***** END ADD ******* 08/10/98 **********************************
      *
     FWMFNL     IF   E           K DISK
      ***  BEGIN CHG  ***  05/19/98  *********************************
     F**********WMPRL   IF  E           K        DISK
      *
      ***** BEG CHG ***** 03/29/06 ***********************************
      * Chg loical to allow inactive records to show.
     F***********WMPRL1  IF  E           K        DISK
     FWMPRL4    IF   E           K DISK
      ***** END CHG ***** 03/29/06 ***********************************
     F***  END   CHG  ***  05/19/98  *********************************
      *
     F***** BEG CHG ******* 04/20/09 **********************************
     F*******WMEPL1  IF  E           K        DISK                           UC
     FWMEPL1E   IF   E           K DISK    USROPN
      ***** END CHG ******* 04/20/09 **********************************
     F*** BEGIN ADD ***  08/11/98  ************************************
      *
      **** BEGIN CHG **** 09/20/99 ************************************
     F******WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 09/20/99 ************************************
     F*** END ADD *****  08/11/98  ************************************
      *
     FWMCML     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FSSYLF     IF   E           K DISK
     F*** BEG ADD ***** 02/05/98 ***********************************************
      *
     FWTMYP     IF   E             DISK
      *
     FWMDXL     IF   E           K DISK
      *
     FWMENL3    IF A E           K DISK
     F*** END ADD ***** 02/05/98 ***********************************************
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 09/20/99 ************************************
     FWMRSL     IF   E           K DISK
      **** END   ADD **** 09/20/99 ************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              HEADERS
     D***  BEGIN CHG  ***  03/30/98  *********************************
     D***  BEGIN CHG  ***  12/22/97  *********************************
     D**********          OF      1  12  1   OFD    35   Options, Function
     D***************     OF      1  14  1   OFD    35   Options, Function
      ***** BEG CHG ******* 07/28/99 **********************************
     D***************     OF      1  15  1   OFD    35   Options, Function
      ***** BEG CHG ******* 04/12/05 **********************************
     D*********           OF      1  16  1   OFD    35   Options, Function
     D OF              S              1    DIM(17) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(17) ALT(OF)
      ***** END CHG ******* 04/12/05 **********************************
      ***** END CHG ******* 07/28/99 **********************************
     D***  END   CHG  ***  12/22/97  *********************************
     D***  END   CHG  ***  03/30/98  *********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Employer Payrolls')
     D DELETD          C                   CONST('*Deleted')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D ACDPGM          C                   CONST('W0060R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(4)
     D***  BEGIN ADD  ***  10/30/97  *********************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D***  END   ADD  ***  10/30/97  *********************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D***  BEGIN ADD  ***  12/22/97  *********************************
     D                 DS
     D  XXDATA                 1     80
     D  XXTYPE                 1      1
     D  XXCO#                  2      4
     D  XXUSER                 5     14
     D  XXTIME                15     20
     D  XXDATE                21     28
     D  ##OUTQ                29     38
     D***  END   ADD  ***  12/22/97  *********************************
     D                 DS
     D***  BEG   CHG  ***  04/20/09  *********************************
     D*********                               1  53 INFO1
     D  INFO1                  1     48
     D  PRDESC                 2     31
     D********                               33  42 XXCXDT
     D  XXCXDT                33     42
     D********                               47  47 XXSTS1
     D  XXSTS1                46     46
     D***  END   CHG  ***  04/20/09  *********************************
     D                 DS
     D***  BEG   CHG  ***  04/20/09  *********************************
     D*********                               1  53 INFO2
     D  INFO2                  1     48
     D***  END   CHG  ***  04/20/09  *********************************
     D  XXESTP                 1     14
     D***  BEGIN CHG  ***  05/14/98  *********************************
     D**********                             18  31 XXYTDP
     D***  BEG   CHG  ***  04/20/09  *********************************
     D**********                             18  32 XXYTDP
     D  XXYTDP                16     30
     D***  END   CHG  ***  04/20/09  *********************************
     D***  END   CHG  ***  05/14/98  *********************************
     D***  BEG   CHG  ***  04/20/09  *********************************
     D**********                             34  47 XXAUDP
     D**********                             50  50 XXSTS2
     D  XXAUDP                31     44
     D  XXSTS2                47     47
     D***  END   CHG  ***  04/20/09  *********************************
     D                 DS
      **** BEGIN CHG **** 08/15/01 ************************************
     D***********************                 1  53 INFO3
     D***********************                 1   6 XXRATE
     D***********************                 9  20 XXEPRM
     D***********************                23  34 XXAPRM
     D***********************                38  38 XXSTS3
      **** BEGIN CHG **** 08/30/01 ************************************
     D***  BEG   CHG  ***  04/20/09  *********************************
     D*********                               1  56 INFO3
     D  INFO3                  1     48
     D***  END   CHG  ***  04/20/09  *********************************
     D  XXRATE                 1      9
     D***  BEG   CHG  ***  04/20/09  *********************************
     D  XXEPRM                13     28
     D***********                            30  45 XXAPRM
     D  XXAPRM                29     44
     D***  END   CHG  ***  04/20/09  *********************************
     D***  BEG   CHG  ***  04/20/09  *********************************
     D*********                              49  49 XXSTS3
     D  XXSTS3                46     46
     D***  END   CHG  ***  04/20/09  *********************************
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 08/15/01 ************************************
     D                 DS
     D***  BEG   CHG  ***  04/20/09  *********************************
     D*********                               1  53 INFO4
     D  INFO4                  1     48
     D***  END   CHG  ***  04/20/09  *********************************
     D  XXECNT                 1      9
     D  XXACNT                12     20
     D***  BEG   CHG  ***  04/20/09  *********************************
     D*********                              23  37 XXLOAD
     D  XXLOAD                25     39
     D  XXSTS4                42     42
     D***  END   CHG  ***  04/20/09  *********************************
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
      **** BEGIN ADD **** 03/30/98 *********************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      **** END   ADD **** 03/30/98 *********************************************
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS                  INZ
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
     D*** BEG ADD ***** 02/05/98 ***********************************************
     D SVLDA           DS          1024
     D  LDENDA                 1      5
     D  LDEND#                 1      5  0
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26

     I*** END ADD ***** 02/05/98 ***********************************************
      *

     C***  BEGIN ADD  ***  01/29/99  *********************************
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C***  END   ADD  ***  01/29/99  *********************************
     C     *LIKE         DEFINE    EPEFDT        KKEFDT
     C     *LIKE         DEFINE    EPCLAS        KKCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C     *LIKE         DEFINE    EPSPEC        KKSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    PRDT          XXDTE
      *** BEG ADD ***** 02/05/98 ***********************************************
     C     *LIKE         DEFINE    ENREQ#        XXREQ#
      *** END ADD ***** 02/05/98 ***********************************************
      *** BEGIN ADD *** 03/30/98 ***********************************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *** END   ADD *** 03/30/98 ***********************************************
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     *LIKE         DEFINE    @@BILL        XXBILL
      ***  END   ADD  ***  06/24/98  *********************************
      **** BEGIN ADD **** 09/20/99 ************************************
     C     *LIKE         DEFINE    PXST          XXST
      **** END   ADD **** 09/20/99 ************************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    EPEXT         KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C                   PARM      XXBILL        ##BILL            1
      ***  END   ADD  ***  06/24/98  *********************************
      ***  BEGIN ADD  ***  08/07/98  *********************************
     C                   PARM      XXQTE         ##QTE             1
      ***  END   ADD  ***  08/07/98  *********************************
      *
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C***  BEGIN CHG  ***  01/29/99  *********************************
     C**********           PARM @@CALL    ##CALL  1
     C                   PARM      SVCALL        ##CALL            1
     C***  END   CHG  ***  01/29/99  *********************************
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN2        ##RRN             9 0
      *
     C     ACDPL2        PLIST
      ***  BEGIN CHG  ***  06/24/98  *********************************
      ********** *INKC     PARM           ##KC
      ********** *INKL     PARM           ##KL
      **********           PARM           ##MODE
      **********           PARM           ##CO#
      **********           PARM           SCFND
      **********           PARM           SCFYR
      **********           PARM           ##KEY  10
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C                   PARM      XXBILL        ##BILL            1
      ***  END   CHG  ***  06/24/98  *********************************
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      ***  BEGIN ADD  ***  12/22/97  *********************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
      ***  END   ADD  ***  12/22/97  *********************************
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
      *  Change Numeric to Alpha program parameter list
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
      *  EMPLOYER MASTER
      *
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *** BEGIN ADD ***  08/11/98  ************************************
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     ZEXKY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***** END ADD ******* 08/10/98 **********************************
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
      **** BEGIN ADD **** 09/20/99 ************************************
     C                   KFLD                    XXST
      **** END   ADD **** 09/20/99 ************************************
      *** END ADD *****  08/11/98  ************************************
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    EPEFDT
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
      *
      *  GROUP MASTER
      *
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      **** END   ADD **** 10/14/98 *********************************************
      *
      *  PAYROLL MASTER
      *
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
      ***  BEGIN ADD  ***  05/19/98  *********************************
     C                   KFLD                    ELRTCD
      ***  END   ADD  ***  05/19/98  *********************************
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
      *
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
      ***  BEGIN ADD  ***  05/19/98  *********************************
     C                   KFLD                    ELRTCD
      ***  END   ADD  ***  05/19/98  *********************************
     C                   KFLD                    EPCLAS
      *
      *  EMPLOYER PAYROLL FILE (MASTER)
      *
     C     EPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    CMCLAS
      ***  BEG   ADD  ***  04/20/09  *********************************
     C                   KFLD                    CMSPEC
     C                   KFLD                    CMEXT
      ***  END   ADD  ***  04/20/09  *********************************
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
      *  KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  REPOSITION KEYS
      *
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     KEY1A         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   KFLD                    KKSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   KFLD                    KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *** BEG ADD ***** 02/05/98 ***********************************************
      *
      *  ENDORS/DARTS CONTROL DEFINITION
      *
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *  ENDORSEMENT REQUEST WORKFILE
      *
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *** END ADD ***** 02/05/98 ***********************************************
      **** BEGIN ADD **** 09/20/99 ************************************
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      **** END   ADD **** 09/20/99 ************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      ***  BEGIN ADD  ***  10/30/97  *********************************
     C                   PARM                    @@QTE             1
      ***  END   ADD  ***  10/30/97  *********************************
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C                   PARM                    @@BILL            1
      *
      *  NOTE: @@BILL CONTROLS THE WRITING OF RECORDS TO THE MASS BILL
      *        WORKFILE (Y/N)
      ***  END   ADD  ***  06/24/98  *********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
      *
     C/COPY SCOPYWW01C
      *
      *===============================================================
      * CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
      *===============================================================
      *
     C     CHKSSM        BEGSR
      *
      ***** BEG DEL ******* 07/28/99 **********************************
B01  C********** SSPDSC    IFNE *BLANKS
      ***** END DEL ******* 07/28/99 **********************************
      ***** BEG ADD ******* 07/28/99 **********************************
     C     SSSTAT        IFNE      *BLANKS
      ***** END ADD ******* 07/28/99 **********************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
      *
     C                   ENDSR                                                  CHKSSM
      *
      *===============================================================
      * CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
      *===============================================================
      *
     C     CVTDT         BEGSR
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C                   ENDSR                                                  CVTDT
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
      *===============================================================
      *
     C     ERRCK1        BEGSR
      *
     C                   SELECT
      *
     C     SCSEL         WHENEQ    '20'
      *
     C     SCSTAT        IFEQ      'A'
      *
      *  IF A PAYROLL CLASS CODE IS BEING INACTIVATED THEN THERE
      *  CANNOT BE ANY CLAIMS WRITTEN UNDER THAT CLASS CODE WHOSE
      *  ACCIDENT DATE FALLS UNDER THE COVERAGE OF THE INACTIVATED CODE
      *
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      *OFF          CLMOK             1
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE
      *
     C     SCCLAS        IFEQ      CMCLAS
     C***  BEG   ADD  ***  04/20/09  *********************************
     C     SCSPEC        ANDEQ     CMSPEC
     C     SCEXT         ANDEQ     CMEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C     XXDTE         ANDLE     CMACDT
      *
      *  READ OTHER CLASS CODES TO SEE IF THE SAME CLASS CODE EXISTS
      *  WITH A PRIOR EFFECTIVE DATE.  IF SUCH A CODE EXISTS AND THAT
      *  CODE IS ACTIVE, READ THE NEXT CLAIM
      *
     C     EPKY          CHAIN     PEP01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     XXDTE         IFNE      EPEFDT
     C     EPSTAT        ANDEQ     'A'
     C     EPEFDT        ANDLE     CMACDT
      *
     C     EPCADT        IFEQ      *ZERO
     C     EPCADT        ORGT      CMACDT
     C                   MOVE      *ON           CLMOK
     C                   LEAVE
     C                   ENDIF                                                  EPCADT=*ZERO
      *
     C                   ENDIF                                                  SCCLAS=EPCLAS
      *
     C     EPKY          READE     PEP01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C     CLMOK         IFEQ      *OFF
      *
      *  ONLY SEND ERROR MESSAGE IF THE CLAIM IS OPEN OR THE CLAIM IS
      *  CLOSED AND PAYMENTS HAVE BEEN MADE ON THE CLAIM
      *
     C                   XFOOT     AR            XXPMTS           11 2
      *
     C     CMSTAT        IFNE      'C'
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
      *
      *  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY
      *
     C                   MOVEL     'WC'          XXSYK
      *
     C     SYKY          CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     UPLVL         IFGE      WCSL03
      *
     C                   MOVE      'N'           SCEOVR
      *
     C                   EXFMT     WDWMSG
      *
     C     SCEOVR        IFNE      'Y'
     C                   SETON                                        84  90
     C                   ENDIF                                                  SCEOVR<>Y
      *
     C                   ELSE
      *
     C                   SETON                                        84  90
      *
     C                   ENDIF                                                  UPLVL>=WCSL03
      *
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C***  BEGIN ADD  ***  11/05/98  *********************************
     C                   LEAVE
     C***  END   ADD  ***  11/05/98  *********************************
      *
     C                   ENDIF                                                  CMSTAT<>C
      *
     C                   ENDIF                                                  SCCLAS=CMCLAS
      *
     C                   ENDIF                                                  CLMOK=*OFF
      *
     C     CMKY          READE     PCM01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  SCSTAT=A
      *
      ***** BEG CHG ******* 07/28/99 **********************************
     C                   MOVE      *ON           AUTORF
      ***** END CHG ******* 07/28/99 **********************************
      *
      **
     C     SCSEL         WHENEQ    '2'
      *
     C     SCSTAT        IFEQ      'I'
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCSTAT=I
      ***** BEG CHG ******* 07/28/99 **********************************
      *
      * Refresh WW screen after delete
      *
     C     SCSEL         WHENEQ    '4'
     C                   MOVE      *ON           AUTORF
      *
      ***** END CHG ******* 07/28/99 **********************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
      *===============================================================
      *
     C     ERRCK2        BEGSR
      ***** BEG ADD ******* 07/28/99 **********************************
      *
      *  Status (Active, Inactive, All)
      *
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8890
     C                   ENDIF
     C*
      ***** END ADD ******* 07/28/99 **********************************
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
      *===============================================================
      *
     C     INZSST        BEGSR
      ***** BEG ADD ******* 07/28/99 **********************************
      *
      *  Record Status subset defaults to Active only
      *
     C                   MOVEL     'A'           SSSTAT
      *
      ***** END ADD ******* 07/28/99 **********************************
     C                   ENDSR                                                  INZSST
      *
      *===============================================================
      * KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
      *===============================================================
      *
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCLAS
     C                   MOVE      *HIVAL        KKEFDT
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   CLEAR                   KKSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   CLEAR                   KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   ENDSR                                                  KEYINZ
      *
      *===============================================================
      * KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
      *===============================================================
      *
     C     KEYSET        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
      *
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C                   MOVE      SCPSTA        KKCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   CLEAR                   KKSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   CLEAR                   KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KEYSET
      *
      *===============================================================
      * KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
      *===============================================================
      *
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C                   MOVE      SCCLAS        KKCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   MOVE      SCSPEC        KKSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVE      SCEXT         KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C                   ENDSR                                                  KEYSFL
      *
      ***  BEGIN ADD  ***  03/30/98  *********************************
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
     C***  BEGIN ADD  ***  11/17/98  *********************************
     C     XXQTE         IFEQ      'Q'
     C***  END   ADD  ***  11/17/98  *********************************
      ******* BEGIN ADD **** 08/07/98 *********************************
     C                   MOVE      XXQTE         QCQTE             1
      ******* END   ADD **** 08/07/98 *********************************
     C***  BEGIN ADD  ***  11/17/98  *********************************
     C                   ENDIF                                                  XXQTE=Q
     C***  END   ADD  ***  11/17/98  *********************************
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      ***  END   ADD  ***  03/30/98  *********************************
      *
      *===============================================================
      * KPDO - SUBROUTINE FOR F15
      *===============================================================
      *
      *  F15=AUDITS
      *
     C     KPDO          BEGSR
      *
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
      *
      ***  BEGIN CHG  ***  06/24/98  *********************************
      **********           CALL 'W0660R2' ACDPL1       99
     C                   CALL      'W0660R2'     ACDPL2                 99
      ***  END   CHG  ***  06/24/98  *********************************
      *
     C                   MOVE      *ON           AUTORF
      *
     C                   ENDSR                                                  KPDO
      *
      *===============================================================
      * KQDO - SUBROUTINE FOR F16
      *===============================================================
      *
      *  F16=S/R ENTRY
      *
     C     KQDO          BEGSR
      *
     C                   CALL      'W0660R'      ACDPL1                 99
      *
     C                   MOVE      *ON           AUTORF
      *
     C                   ENDSR                                                  KQDO
      *
      ***  BEGIN ADD  ***  12/22/97  *********************************
      *===============================================================
      * KTDO - SUBROUTINE FOR F19
      *===============================================================
      *
      *  F19=EDIT (ALL PAYROLLS)
      *
     C     KTDO          BEGSR
      *
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      'F'           XXTYPE
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  Q1USER        XXUSER
     C                   MOVE      DSTIME        XXTIME
     C                   MOVE      DSDATE        XXDATE
      *
     C                   CALL      'WA029R'                             99
     C     *INKL         PARM                    ##KL
      *
     C  NKL              CALL      'QSNDDTAQ'
     C                   PARM      'WEDITQ'      XXQNM
     C                   PARM      '*LIBL'       XXLIB
     C                   PARM      80            XXFLDL
     C                   PARM                    XXDATA
      *
     C                   ENDSR                                                  KTDO
      *
      *===============================================================
      * KVDO - SUBROUTINE FOR F21
      *===============================================================
      *
      *  F21=EDIT (S/R PAYROLLS)
      *
     C     KVDO          BEGSR
      *
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      'A'           XXTYPE
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      Q1USER        XXUSER
     C                   MOVE      DSTIME        XXTIME
     C                   MOVE      DSDATE        XXDATE
      *
     C                   CALL      'WA029R'                             99
     C     *INKL         PARM                    ##KL
      *
     C  NKL              CALL      'QSNDDTAQ'
     C                   PARM      'WEDITQ'      XXQNM            10
     C                   PARM      '*LIBL'       XXLIB            10
     C                   PARM      80            XXFLDL            5 0
     C                   PARM                    XXDATA
      *
     C                   ENDSR                                                  KVDO
      *
      ***  END   ADD  ***  12/22/97  *********************************
      *
      *===============================================================
      * MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     EPEFDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   MOVEL     EPSPEC        SCSPEC
      ***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVEL     EPEXT         SCEXT
      ***  END   ADD  ***  04/20/09  *********************************
     C                   MOVEL     EPSTAT        SCSTAT
      *
     C                   SELECT
      *
     C     HD#           WHENEQ    1
      *
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C   60              MOVEL     *BLANKS       PRDESC
      *
      *** BEGIN ADD ***  08/11/98  ************************************
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       PRDESC
      **** BEGIN ADD **** 09/20/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 09/20/99 ************************************
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL     PXDESC        PRDESC
      **** BEGIN ADD **** 09/20/99 ************************************
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL     PXDESC        PRDESC
      **** END   ADD **** 09/20/99 ************************************
     C                   ENDIF                                                  EPEXT IFNE
      *** END ADD *****  08/11/98  ************************************
     C                   Z-ADD     EPCADT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCXDT
      *
     C                   MOVEL     EPSTAT        XXSTS1
      *
     C                   MOVEL     INFO1         SCDATA
      *
      **
      *
     C     HD#           WHENEQ    2
      *
      ***  BEGIN ADD  ***  05/14/98  *********************************
     C                   CLEAR                   INFO2
      ***  BEGIN ADD  ***  05/14/98  *********************************
      *
     C                   MOVEL(P)  EPEAPR        XXNUM
      ***  BEGIN CHG  ***  11/12/97  *********************************
      **********           MOVEL'XXX,XXX' EDTMSK    P
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
      ***  END   CHG  ***  11/12/97  *********************************
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXESTP
      *
     C                   MOVEL(P)  EPSAPR        XXNUM
      ***  BEGIN CHG  ***  11/12/97  *********************************
      **********           MOVEL'XXX,XXX' EDTMSK    P
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
      ***  END   CHG  ***  11/12/97  *********************************
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXYTDP
      *
     C                   MOVEL(P)  EPAAPR        XXNUM
      ***  BEGIN CHG  ***  11/12/97  *********************************
      **********           MOVEL'XXX,XXX' EDTMSK    P
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
      ***  END   CHG  ***  11/12/97  *********************************
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAUDP
      *
     C                   MOVEL     EPSTAT        XXSTS2
      *
     C                   MOVEL     INFO2         SCDATA
      *
      **
      *
     C     HD#           WHENEQ    3
      *
     C                   MOVEL(P)  EPARAT        XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRATE
      *
     C                   MOVEL(P)  EPEMPR        XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEPRM
      *
     C                   MOVEL(P)  EPAMPR        XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAPRM
      *
     C                   MOVEL     EPSTAT        XXSTS3
      *
     C                   MOVEL     INFO3         SCDATA
      *
      **
      *
     C     HD#           WHENEQ    4
      *
     C                   MOVEL(P)  EPECNT        XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXECNT
      *
     C                   MOVEL(P)  EPACNT        XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXACNT
      *
     C                   MOVEL(P)  EPADLD        XXNUM
      ***  BEGIN CHG  ***  07/10/98  *********************************
      **********           MOVEL'XXX,XXX' EDTMSK    P
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
      ***  END   CHG  ***  07/10/98  *********************************
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXLOAD
      *
     C                   MOVEL     EPSTAT        XXSTS4
      *
     C                   MOVEL     INFO4         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      ***** BEG ADD ******* 04/12/05 **********************************
      *
      *
      *===============================================================
      * OPT8  - Work With Employees
      *===============================================================
     C     OPT8          BEGSR
      *
     C     *IN09         IFEQ      '1'
     C     SVCALL        OREQ      '2'
     C                   MOVE      '1'           ##CALL
     C                   ELSE
     C                   MOVE      ' '           ##CALL
     C                   ENDIF
      *
     C                   CALL      'W0062R'                             99
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0          WMEPP
     C                   PARM      @@QTE         ##QTE             1
      *
     C                   ENDSR                                                  OPT8
      *
      ***** END ADD ******* 04/12/05 **********************************
      *
      *
      *===============================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *===============================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR                                                  OPT20
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
      *
     C     *INKP         CASEQ     *ON           KPDO
     C     *INKQ         CASEQ     *ON           KQDO
      ***  BEGIN ADD  ***  12/22/97  *********************************
     C     *INKT         CASEQ     *ON           KTDO
     C     *INKV         CASEQ     *ON           KVDO
      ***  END   ADD  ***  12/22/97  *********************************
      ***  BEGIN ADD  ***  03/30/98  *********************************
     C     *INKN         CASEQ     *ON           KNDO
      ***  END   ADD  ***  03/30/98  *********************************
     C                   CAS                     ENTRDO
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * OTHOPT - SUBROUTINE FOR OTHER OPTIONS
      *===============================================================
      *
     C     OTHOPT        BEGSR
      *
      ***** BEG ADD ******* 04/12/05 **********************************
     C     SCSEL         CASEQ     '8'           OPT8
      ***** END ADD ******* 04/12/05 **********************************
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      *
      *
      *===============================================================
      * PGMEND - SUBROUTINE TO END THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
     C                   CLOSE     WMEPP
      ***** BEG CHG ******* 04/20/09 **********************************
     C************         CLOSEWMEPL1
     C************         CLOSEWMEPL6
     C                   CLOSE     WMEPL1E
     C                   CLOSE     WMEPL6E
      **** END   CHG **** 04/20/09 *********************************************
      **** BEGIN DEL **** 11/23/98 *********************************************
     C******************** CLOSEWDELP
      **** END   DEL **** 11/23/98 *********************************************
      *
      *** BEG DEL ***** 01/20/04 ***********************************************
      *** BEG ADD ***** 02/05/98 ***********************************************
      *********************
      ***WRITE*A*CLASS*RATE/OTHER ENDORSEMET RECORD IF CALLED VIA EMPLR
      *  MID TERM ENDORSEMENTS, AND ADD/CHG/DELETES HAVE OCCURRED.
      *********************
     C************NAMVAR***DEFN *LDA      SVLDA
     C************LOCK*****IN   *NAMVAR
      *********************
      ***CHECK*PAYROLL*CHANGES FLAG
      *********************
     C***********LDPYRL****IFEQ 'Y'                        CHG HAVE OCCURRED
      **** BEGIN ADD **** 01/19/99 *********************************************
      *********************
      ***BE*SURE*PROGRAM*IS BEING CALLED FROM MIDTERM REVISED BILLING
      *********************
     C***********LDENDA****ANDNE*BLANKS
      **** END ADD ****** 01/19/99 *********************************************
     C*********************MOVEL*BLANKS   LDPYRL
      *********************
      *  CHK TO SEE IF CALLED AS PART OF ADDITION OF A DIVISION
      *********************
     C*********************CALL 'SRCHKPGM'
     C*********************PARM 'W0051R19'##PGM  10
     C*********************PARM *BLANKS   ##EXST  1
      *********************
      *  ONLY WRITE A WPENP REC IF NOT FROM ADDITION OF DIVISION
      *********************
     C***********##EXST****IFEQ *OFF
     C*********************EXSR WRTPEN
     C*********************ENDIF                           ##EXST = *OFF
      *********************
     C*********************ENDIF                           LDPYRL = 'Y'
      *********************
     C*********************OUT  *NAMVAR
      *** END ADD ***** 02/05/98 ***********************************************
      *** END DEL ***** 01/20/04 ***********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   CLOSE     WDELP
      **** END   ADD **** 11/23/98 *********************************************
     C*
     C***  BEGIN ADD  ***  01/29/99  *********************************
     C     SVCALL        IFEQ      'R'
     C                   MOVE      *ON           *INLR
     C                   ENDIF                                                  SVCALL=R
     C***  END   ADD  ***  01/29/99  *********************************
      *
     C                   ENDSR
      *
      *===============================================================
      * PGMSTR - SUBROUTINE TO START THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  01/29/99  *********************************
     C                   MOVE      @@CALL        SVCALL
     C*
     C     @@CALL        IFEQ      'R'
     C*
     C                   MOVE      ' '           @@CALL
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '15'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF                                                  @@CALL=R
     C***  END   ADD  ***  01/29/99  *********************************
      *
     C                   OPEN      WMEPP
      ***** BEG CHG ******* 04/20/09 **********************************
     C************         OPEN WMEPL1
     C************         OPEN WMEPL6
     C                   OPEN      WMEPL1E
     C                   OPEN      WMEPL6E
      ***  END   CHG  ***  04/20/09  *********************************
     C                   OPEN      WDELP
      *
     C                   Z-ADD     8             SF1PAG            5 0
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    INQUIRY
      *
      ***  BEGIN ADD  ***  10/30/97  *********************************
     C     Q1PRMS        IFGT      5
      *
B01  C     @@QTE         IFEQ      'Q'
      ***** BEG DEL ******* 04/12/05 **********************************
     C*********            MOVELCLRFKY    OFD,11           F 15
     C*********            MOVELCLRFKY    OFD,12           F 16
      ***** END DEL ******* 04/12/05 **********************************
      ***** BEG ADD ******* 04/12/05 **********************************
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '15'          XXNMBR                         F15
     C                   EXSR      UPDOFA
      *
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '16'          XXNMBR                         F16
     C                   EXSR      UPDOFA
      *
      ***** END ADD ******* 04/12/05 **********************************
      ***** BEG ADD ******* 12/05/00 **********************************
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '21'          XXNMBR                         F21
     C                   EXSR      UPDOFA
      *
      ***** END ADD ******* 12/05/00 **********************************
E01  C                   ENDIF                                                  @@QTE=Q
      *
      **** BEGIN ADD ***** 08/07/98 **********************************
     C                   MOVE      @@QTE         XXQTE             1
      **** END   ADD ***** 08/07/98 **********************************
      *
E01  C                   ENDIF                                                  Q1PRMS>5
      ***  END   ADD  ***  10/30/97  *********************************
      *
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     Q1PRMS        IFGT      6
     C                   MOVE      @@BILL        XXBILL
     C                   ELSE
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF                                                  Q1PRMS>6
      ***  END   ADD  ***  06/24/98  *********************************
      *
      *  LOAD SCREEN HEADINGS
      *
      *  GET EMPLOYER DETAIL RECORD
      *
     C     @@RRN2        CHAIN     PEL01                              60
      *
     C                   Z-ADD     ELFYR         SCFYR
      *
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
      *
      *  GET EMPLOYER MASTER RECORD
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
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
      **** BEGIN DEL **** 02/12/98 *********************************************
      ******************** DO   5         I1      30
      ********************
      ********** CX,I1     IFNE *ZERO
      ******************** Z-ADDCX,I1     SRDTI
      ******************** CALL 'SRCVTDAT'DTMDYY
      ******************** MOVELSRDTO     SCCXDT
      ******************** ELSE
      ******************** LEAVE
      ******************** ENDIF                           CX,I1<>*ZERO
      ********************
      ******************** ENDDO                           I1->5
      **** END   DEL **** 02/12/98 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN41
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
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
      *
      *  GET FUND MASTER INFORMATION
      *
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              60
      **** END   ADD **** 10/14/98 *********************************************
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
      *
     C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
      *
      *  GET AGENT/PRODUCER INFORMATION
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *
      *  GET USER PARMS
      *
     C                   CLEAR                   PUP01
     C     Q1USER        CHAIN     PUP01                              60
      *
      *
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     ZEXKY         CHAIN     PEX02                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 08/10/98 **********************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * READ - SUBROUTINE TO READ THE FILE
      *===============================================================
      *
     C     READ          BEGSR
      *
     C   21KEY1          READE     DBRCDF                                 95
      *
     C                   ENDSR                                                  READ
      *
      *===============================================================
      * READP - SUBROUTINE TO READ THE FILE BACKWARDS
      *===============================================================
      *
     C     READP         BEGSR
      *
     C   21KEY1          READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
      *
      *===============================================================
      * SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
      *===============================================================
      *
     C     SETGT         BEGSR
      *
     C   21KEY1A         SETGT     DBRCDF                             96
      *
     C                   ENDSR                                                  SETGT
      *
      *===============================================================
      * SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
      *===============================================================
      *
     C     SETLL         BEGSR
      *
     C   21KEY1A         SETLL     DBRCDF                             96
      *
     C                   ENDSR                                                  SETLL
      *
      *===============================================================
      * SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
      *===============================================================
      *
     C     SF1UPD        BEGSR
      *
      *
     C                   ENDSR                                                  SF1UPD
      *
      *===============================================================
      * SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
      *===============================================================
      *
     C     SLCTRD        BEGSR
      *
     C                   Z-ADD     SCDBRR        @@RRN
      *
     C                   ENDSR
      *
      *
      *
      *===============================================================
      * SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
      *===============================================================
      *
     C     SUBSET        BEGSR
      ***** BEG ADD ******* 07/28/99 **********************************
      *
      *  Record Status
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     EPSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      ***** END ADD ******* 07/28/99 **********************************
     C                   ENDSR                                                  SUBSET
      *
      *
      *
      *** BEG ADD *****02/05/98 ************************************************
      *===============================================================
      *  WRITE A RECORD TO THE EMPLOYER MID TERM ENDORSEMENT FILE
      *===============================================================
     C     WRTPEN        BEGSR
      *
      *  GET ENDORS/DARTS INFORMATION
      *
     C     DXKY          CHAIN     PDX01                              60
      *
     C                   CALL      AGTAPI        AGTPL
      *
      *  SETUP FOR NEXT REQUEST NUMBER
      *
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         XXREQ#
     C                   ELSE
     C                   Z-ADD     ENREQ#        XXREQ#
     C                   ENDIF
      *
     C                   CLEAR                   PEN01
      *
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             XXREQ#
     C                   Z-ADD     XXREQ#        ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      *
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C                   MOVEL     EMCNTY        ENCNT1
      *
     C                   MOVE      'Y'           ENCRFL
     C                   MOVEL(P)  DXPCRO        ENRMCD
      *
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C     'Class/Ra'    CAT(P)    'te/Oth':0    ENDSC3
     C                   CAT       'er Pay':0    ENDSC3
     C                   CAT       'roll C':0    ENDSC3
     C                   CAT       'hange':0     ENDSC3
     C     ENDSC2        IFEQ      *BLANK
     C                   MOVEL     ENDSC3        ENDSC2
     C                   MOVEL     *BLANKS       ENDSC3
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      *
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVE      *BLANKS       ENALIN
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
      **** END   ADD **** 11/23/98 *********************************************
      *
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
     C                   ENDSR
      *** END ADD *****02/05/98 ************************************************
      *
      *===***===***===***===***===***===***===***===***===***===***===
      *
      * * * * I M P O R T A N T * * * *
      *
      *  WHEN ADDING F KEYS OR OPTIONS BE SURE TO CHECK PGMSTR TO BE
      *  SURE THAT ALL CONDITIONAL CLEARING OF OPTIONS/FUNCTIONS ARE
      *  STILL BEING HANDLED CORRECTLY.
      *
      *===***===***===***===***===***===***===***===***===***===***===
** Headers - HD
Opt  Eff Date  Class Spc Ext    Description                     Cancel Dt  Sts
Opt  Eff Date  Class Spc Ext      Est Payroll YTD SR Payroll  Audit Payroll St
Opt  Eff Date  Class Spc Ext        Rate      Est Man Prm     Aud Man Prm  Sts
Opt  Eff Date  Class Spc Ext    Est Empl  Aud Empl  Additional Load    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1088=Employees              YYN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYY
212F12=Cancel               YYY
214F14=Darts                YYN
215F15=Audits               YNN
216F16=S/R Entry            YNN
217F17=Subset               YYY
219F19=Edit (All payrolls)  YYN
221F21=Edit (S/R payrolls)  YYN
