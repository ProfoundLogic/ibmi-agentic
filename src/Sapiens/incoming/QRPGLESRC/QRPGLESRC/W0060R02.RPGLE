     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0060R02 - EMPLOYER PAYROLL INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EMPLOYER PAYROLL FILE.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0060R - WORK WITH EMPLOYER PAYROLL                        *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/24/96  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     F*                                                               *
     F*  04/12/12 C:282 SUPPORT: 79466                                *
     F*                 Aircraft description field to be shown at     *
     F*                 SCR02 in Display Mode.                        *
     H*                                                               *
     H* 03/27/11 C:235 SUPPORT: 71062                                 *
     H*                MASS BILLING ISSUES                            *
     H*                New/Renew Policy Flag is used to populate      *
     H*                RENEWAL FLAG (NEW/RENEW FLAG) in Billing       *
     H*                Transation File if New/Renew Policy Flag       *
     H*                not Blank                                      *
     H*                                                               *
     H* 09/09/10 C:257 PROJECT: C93WIX                                *
     H*                ADDED 7 NEW FIELDS FOR CLIENT 93.              *
     H*                FIELDS: AUDITED AVERAGE ANNUAL WAGE            *
     H*                        AGE UNDER 20,AGE 21 - 30               *
     H*                        AGE 31 - 40 ,AGE 41 - 50               *
     H*                        AGE 51 - 60 ,AGE 61 +                  *
     H*                                                               *
     H*                                                               *
     H* 01/20/09 C:136 SUPPORT: WA139 65954                           *
     H*                DO NOT WRITE A MASS BILLING RECORD IF POLICY   *
     H*                HAS BEEN PREVIOUSLY BILLED PER GW              *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                USE THE NEW LOGICAL(S) TO GET THE DESCRIPTION  *
     H*                BY EXTENSION.                                  *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*
     H* 09/05/07 C:65  PROJECT SUPP# 59907                            *
     H*                CHAIN TO PAYROLL RATE FILE USING THE PROPER    *
     H*                EFFECTIVE DATE FOR SPLIT ANNIVERSARY POLICIES. *
     H*                CLEAR XXSTDT AT START OF PROGRAM AS WELL.      *
     H*                                                               *
     H* 08/24/06 C:180 PROJECT 114WMF                                 *
     H*                 If payroll is changed from revised billing,   *
     H*                 the policy is short-term, and the override    *
     H*                 pro-rata factor flag has been set, then       *
     H*                 prorate the payroll.                          *
     H*
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 03/29/06 C:132 PROJECT WA091 - 44078                          *
     H*              - Change logical WMPRL1 to WMPRL4, to show the   *
     H*                payroll records with status of "I" Inactive.   *
     H*              - Added new message for inactive payroll class,  *
     H*                need to send WMESGG with this correction.      *
     H*                                                               *
     H* 03/17/06 C:132 SUPPORT WA132 50559                            *
     H*              - Issue Message if Class Extension is Inactive   *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: WC1136                                *
     H*                ADD LOSS COST MULTIPLIER                       *
     H*                                                               *
     H* 12/16/04 C:132  PROJECT: WC1233                               *
     H*                 Display Payroll Class Code based on ARD or    *
     H*                 Polcy Effective Date when searching payroll   *
     H*                 class code.                                   *
     H*                                                               *
     H* 11/11/04 C:65 PROJECT: SUPP#44123/WC995                       *
     H*               WHEN PULLING RATES FOR PA ON A MULTI-STATE      *
     H*               POLICY, USE THE EARLIEST EFFECTIVE DATE OF      *
     H*               THE PA DIVISIONS ONLY.                          *
     H*                                                               *
     H* 10/10/03 C:33  PROJECT: WC1017                                *
     H*                Update renewal flag.                           *
     H*                                                               *
     H* 11/02/03 C:136 SUPPORT: 39762                                 *
     H*                Since W8010R uses effective date of state      *
     H*                reporting division to rate payroll classes,    *
     H*                edit the payroll class expiration date against *
     H*                the state reporting division eff date.         *
     H*                                                               *
     H* 03/18/03 C:61  PROJECT WC1106                                 *
     H*                Only allow rate change if both employer and    *
     H*                Payroll class are "A" rated.                   *
     H*                                                               *
     H*  03/08/02 C:65 PROJECT: WC1030                                *
     H*                IF THE EMPLOYER HAS A PREVIOUS EMPLR# THEN     *
     H*                CONSIDER THE POLICY AS A RENEWAL.              *
     H*                                                               *
     H*  11/12/01 C:142 PROJECT: WC1061                               *
     H*                 If add mode and live system default to        *
     H*                 ELPFDT if no other payrolls exist for policy  *
     H*                 year. Also use ELPFT if in quote system.      *
     H*                                                               *
     H*  08/15/01 C:90  PROJECT: WC1035                               *
     H*                 Expand rate field from 5.2 to 7.2             *
     H*                                                               *
     H*  05/01/00 C:118 PROJECT: WC577                                *
     H*                 REMOVE EDIT FROM SCSPEC FIELD, ANY ENTRY IS   *
     H*                 VALID.  ENTRY NOT REQUIRED.                   *
     H*                                                               *
     H*  11/16/99 C:90  PROJECT: WC945                                *
     H*                 PASS RATING STATE FOR SEARCH ON PAYROLL CLASS *
     H*                 EXTENSION.                                    *
     H*                                                               *
     H*  10/19/99 C:61  PROJECT WC944                                 *
     H*                 Add edit to ensure employer payroll class     *
     H*                 effective date not greater than payroll       *
     H*                 expiration date (if entered).                 *
     H*                                                               *
     H*  09/20/99 C:90 PROJECT: WC945                                 *
     H*                DISPLAY PAYROLL CLASS DESCRIPTION FOR BLANK    *
     H*                STATE VALUE, IF NO HIT DISPLAY DESCRIPTION     *
     H*                USING RATING STATE.                            *
     H*                                                               *
     H*  08/06/99 C:118 PROJECT: WC932                                *
     H*                 Added Hazard group to display.  Display Hazard*
     H*                 group value in UPDATE mode.  In ADD mode      *
     H*                 display value from payroll entry or search.   *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H*  12/11/98  C:65  PROJECT: SUPPORT                             *
     H*                  CHAIN TO A FILE BEFORE DOING A READ PRIOR.   *
     H*                                                               *
     H*  11/11/98  C:65  PROJECT: WC843                               *
     H*                  ALLOW SPECIAL DESG OF 1-9 FOR ALL PAYROLLS.  *
     H*                                                               *
     H*  09/01/98 C:33 SEQ#: DR095/12876                              *
     H*                CHECK Q1PRMS PRIOR TO PROCESSING @@QTE.        *
     H*                                                               *
     H*  08/27/98  C:65  PROJECT: SUPP WA091/13495                    *
     H*                  CHANGED INDICTOR FROM 31 TO 32 FOR SCEXT.    *
     H*                                                               *
     H*  08/10/98  C:61  Project: 48WLC                               *
     H*                  Display old Emp#/Div from Employer Cross     *
     H*                  Reference (WZEXP).                           *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  ADD EXTENSION FIELD FOR DESCRIPTION AND      *
     H*                  MOVED THE CHECK(ER) TO THIS FIELD.           *
     H*                                                               *
     H*  08/07/98  C:78  SEQ#: DR095/12876                            *
     H*                  QCQTE NOT LOADED QITH EITHER A 'Q' OR BLANKS.*
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WA093/12385                            *
     H*                  WRITE RECORDS TO BILL TRANSACTION FILE.      *
     H*                                                               *
     H*  04/15/98  C:33  SEQ#: WA091/11509                            *
     H*                  CHANGE EDIT CODE FOR YTD S/R PAYROLL.        *
     H*                                                               *
     H*  04/03/98  C:78  PROJECT: WC825                               *
     H*                  ALLOW ACCESS TO DARTS. F14 ADDED.            *
     H*                                                               *
     H*  02/05/98  C:57  PROJECT: CM027                               *
     H*                  IDENTIFY THAT CHANGES/ADD/DELETS HAVE        *
     H*                  OCCURRED WHEN CALLED FROM EMPLOYER MID TERM  *
     H*                  ENDORSEMENTS.  PASS THIS BACK VIA THE LDA.   *
     H*                                                               *
     H*  12/09/97  C:33  PROJECT: CM027                               *
     H*                 -Do not clear the effective date in ADD mode. *
     H*                 -Class code validation fix.                   *
     H*                 -Close files when changing status.            *
     H*                                                               *
     H*  12/03/97  C:57  PROJECT: CM027                               *
     H*                  Spec may be 0 thru 9 for Aircraft Class Cds. *
     H*                                                               *
     H*  11/25/97  C:33  PROJECT: WC577                               *
     H*                  Load 'E' to EPSAFL in add mode.              *
     H*                                                               *
     H*  11/21/97  C:57  PROJECT: CM027                               *
     H*                  Added Aircraft Description entry to WDWENT.  *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMEPP*
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  31 - QUOTE MODE                                              *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - ESTIMATED CLASS CODE                                    *
     H* N41 - AUDITED CLASS CODE                                      *
     H*  42 - 'A' RATED EMPLOYER                                      *
     H*  50 - 'A' RATED PAYROLL CLASS                                 *
     H*  63 - Display old Emp#/Div in header info                     *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0500) Effective Date invalid                        *
     H*  21 - (WCR0539) Payroll Class invalid                         *
     H*  22 - (WCR0697) Payroll class already exists for this employer*
     H*  24 - (WCR0547) Estimated Amount < zero                       *
     H*  25 - (WCR0547) Audited Amount < zero                         *
     H*  26 - (WCR0547) Estimated employee count < zero               *
     H*  27 - (WCR0547) Audited employee count < zero                 *
     H*  28 - (WCR0547) Additional Load < zero                        *
     H*  29 - (WCR0500) Cancel date invalid                           *
     H*  30 - (WC32391) Payroll class is inactive                     *
     H*  32 - (WCR0513) EXTENSION - INVALID TABLE VALUE               *
     H*  70 - (WCR0578) Effective date not between policy dates       *
     H*  POSITION CURSOR:                                             *
     H*  71 - (SCRATE) Rate                                           *
     H*  72 - (SCEAPR) Estimated Amount                               *
     H*  73 - (SCCLAS) Class Code                                     *
     H*  74 - (SCSPEC) Special Flag                                   *
     H*  77 - (SCEXT)  EXTENSION CODE                                 *
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
     FW0060F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FWMEPP     UF A E             DISK    USROPN
     F                                     RENAME(PEP01:DBRCDF)
     F                                     INFDS(DBFDS)
      *
     FWDELP     IF   E             DISK    USROPN
      *
     FWMEML     IF   E           K DISK
      *
     FWMFNL     IF   E           K DISK
      *
     FWDF2L     IF   E           K DISK
      *
      ***** BEG CHG ***** 04/20/09 ***********************************
     F******WMEPL1  IF  E           K        DISK                           UC
     FWMEPL1E   IF   E           K DISK    USROPN
      ***** END CHG ***** 04/20/09 ***********************************
      *
      ***** BEG CHG ***** 03/29/06 ***********************************
      * Chg loical to allow inactive records to show.
     F***********WMPRL1  IF  E           K        DISK
     FWMPRL4    IF   E           K DISK
      ***** END CHG ***** 03/29/06 ***********************************
      *
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PPR02)
     F*** BEG ADD ***** 11/21/97 ***********************************************
      *
     FWMRSL     IF   E           K DISK
      *
     FWDRRL     IF   E           K DISK
     F*** END ADD ***** 11/21/97 ***********************************************
     F***  BEGIN ADD  ***  06/24/98  *********************************
      *
     FWABLL     UF A E           K DISK
      *
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      ***** END ADD ******* 08/10/98 **********************************
      *
     FWMAHL     IF   E           K DISK
      *
     FWMEPL     IF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F***  END   ADD  ***  06/24/98  *********************************
     F*** BEGIN ADD ***  08/11/98  ************************************
      *
      **** BEGIN CHG **** 09/20/99 ************************************
     F*****WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 09/20/99 ************************************
     FWMPXP     IF   E             DISK
     F                                     RENAME(PPX01:PPX02)
     F*** END ADD *****  08/11/98  ************************************
     F*** BEG ADD ***** 01/20/09 **************************************
     FWDEHLT2   IF   E           K DISK
     F*** END ADD ***** 01/20/09 **************************************
      *
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Employer Payroll Inf-
     D                                     ormation')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D*** BEG DEL ***** 11/21/97 ***********************************************
     D***           1                     C         #OFSCR
     D*** END DEL ***** 11/21/97 ***********************************************
     D*** BEG ADD ***** 02/05/98 ***********************************************
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*** END ADD ***** 02/05/98 ***********************************************
     D*** BEGIN ADD *** 04/03/98 ***********************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*** END ADD ***** 04/03/98 ***********************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     D                 DS
     D  XATODT                 1      7  0
     D  XXTOYR                 1      3  0

     I*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
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
      * EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *** BEG ADD ***** 11/02/03 **************************************
     C     EMKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      *** END ADD ***** 11/02/03 **************************************
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***** END ADD ******* 08/10/98 **********************************
      *
      *  GROUP MASTER/AUXILIARY FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      *  EMPLOYER PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
      **** BEG ADD ******04/20/09***********************************
     C                   KFLD                    SCEXT
      **** END ADD ******04/20/09***********************************
     C                   KFLD                    XXEFDT
      *
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     EPKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      ***  END   ADD  ***  06/24/98  *********************************
      *
      *  PAYROLL MASTER
      *
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C*** BEG CHG ***** 04/01/08 ***********************************************
     C*************        KFLD           XXEFDT
     C                   KFLD                    X4EFDT            7 0
     C*** END CHG ***** 04/01/08 ***********************************************
      *
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
      *** BEG ADD ***** 11/21/97 ***********************************************
      *
      *** BEG ADD ***** 11/02/03 ***********************************************
     C     PRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C*** BEGIN CHG ***  09/05/07  ************************************
     C*********            KFLD           XXSTDT
     C                   KFLD                    X2EFDT            7 0
     C*** END CHG *****  09/05/07  ************************************
      *** END ADD ***** 11/02/03 ***********************************************
      *
      *  RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *  STATISTICAL RATING/REPORTING FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    XREFDT
      *
     C     RRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
      *** END ADD ***** 11/21/97 ***********************************************
      ***  BEGIN ADD  ***  06/24/98  *********************************
      *
      *  BILLING WORK FILE
      *
     C     BLKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *** BEG ADD ***** 01/20/09 ************************************
     C     EHKYT2        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
      *** END ADD ***** 01/20/09 ************************************
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *** BEG ADD ***** 11/02/03 *************************************
     C     ELKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    XSDIV
     C                   KFLD                    XSFYR
      *** END ADD ***** 11/02/03 *************************************
      *
      *  AGENT HISTORY FILE
      *
     C     AHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      ***  END   ADD  ***  06/24/98  *********************************
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *  PAYROLL DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCEXT
      **** BEGIN ADD **** 09/20/99 ************************************
     C                   KFLD                    XXST
      **** END   ADD **** 09/20/99 ************************************
      *** END ADD *****  08/11/98  ************************************
     C*** BEGIN ADD ***  06/01/06  ************************************
     C*** BEG CHG *****  04/01/08  ************************************
     C************         KFLD           XXEFDT
     C                   KFLD                    X3EFDT            7 0
     C*** END CHG *****  04/01/08  ************************************
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCEXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
     C*** BEGIN ADD ***  03/08/02  ************************************
     C     ZEXKY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*** END ADD *****  03/08/02  ************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0          wdelp
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C                   PARM                    @@BILL            1
      ***  BEGIN ADD  ***  08/07/98  *********************************
     C                   PARM                    @@QTE             1
      ***  END   ADD  ***  08/07/98  *********************************
      *
      *  NOTE: @@BILL CONTROLS THE WRITING OF RECORDS TO THE MASS BILL
      *        WORKFILE (Y/N)
      ***  END   ADD  ***  06/24/98  *********************************
      *
     C     *LIKE         DEFINE    EPEFDT        XXEFDT
     C     *LIKE         DEFINE    SCEFDT        X1EFDT
     C     *LIKE         DEFINE    EPCLAS        SVCLAS
     C     *LIKE         DEFINE    EPSPEC        SVSPEC
      *** BEGIN ADD ***  08/11/98  ************************************
     C     *LIKE         DEFINE    EPEXT         SVEXT
      *** END ADD *****  08/11/98  ************************************
     C     *LIKE         DEFINE    SCEFDT        SVEFDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
      *** BEG ADD ***** 11/21/97 ***********************************************
     C     *LIKE         DEFINE    RREFDT        XREFDT
      *** END ADD ***** 11/21/97 ***********************************************
      ***  BEGIN ADD  ***  12/09/97  ********************************* *********
     C     *LIKE         DEFINE    *IN99         FIRST
     C     *LIKE         DEFINE    *INKJ         F10FLG
      ***  END   ADD  ***  12/09/97  ********************************* *********
      *** BEG ADD ***** 02/05/98 ***********************************************
     C     *LIKE         DEFINE    EPEFDT        S#EFDT
     C     *LIKE         DEFINE    EPCADT        S#CADT
      *** BEGIN ADD *** 04/03/98 ***********************************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *** END ADD ***** 04/03/98 ***********************************************
     C     *DTAARA       DEFINE    *LDA          SVLDA
      *** END ADD ***** 02/05/98 ***********************************************
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     *LIKE         DEFINE    *IN60         XXEXST
     C     *LIKE         DEFINE    BLRFLG        XXRFLG
     C     *LIKE         DEFINE    @@BILL        XXBILL
      ***  END   ADD  ***  06/24/98  *********************************
     C***  BEGIN ADD  ***  09/01/98  *********************************
     C     *LIKE         DEFINE    @@QTE         XXQTE
     C***  END   ADD  ***  09/01/98  *********************************
     C***** BEGIN ADD **** 08/06/99 ***********************************
     C     *LIKE         DEFINE    PRHZGR        XXHZGR
     C***** END   ADD **** 08/06/99 ***********************************
      **** BEGIN ADD **** 09/20/99 ************************************
     C     *LIKE         DEFINE    RSJRST        XXST
      **** END   ADD **** 09/20/99 ************************************
      *** BEG ADD ***** 11/02/03 ***********************************************
     C     *LIKE         DEFINE    EMDIV         XSDIV
     C     *LIKE         DEFINE    ELFYR         XSFYR
     C     *LIKE         DEFINE    EMDIV         X1DIV
     C     *LIKE         DEFINE    ELPFDT        XSPFDT
     C     *LIKE         DEFINE    ELANDT        XSANDT
     C     *LIKE         DEFINE    ELPFDT        X1PFDT
     C     *LIKE         DEFINE    ELANDT        X1ANDT
     C     *LIKE         DEFINE    ELPFDT        XXSTDT
      *** END ADD ***** 11/02/03 ***********************************************
      *
     I*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C     *LIKE         DEFINE    ELPFDT        X2PFDT
     C     *LIKE         DEFINE    ELPTDT        X2PTDT
     C     *LIKE         DEFINE    ELPROR        X2PROR
     C     *LIKE         DEFINE    ELFFL9        X2FFL9
     C     *LIKE         DEFINE    BLDT          X1FROM
     C     *LIKE         DEFINE    BLDT          X1TO
     C     *LIKE         DEFINE    EPEAPR        SVEAPR
     I*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C/COPY SCOPYMT01C
      *
      /EJECT
      ***  BEGIN ADD  ***  06/24/98  *********************************
      *===============================================================
      * BILLNG - SUBROUTINE FOR WRITING/UPDATING BILL TRANSACTION FILE
      *===============================================================
      *
     C     BILLNG        BEGSR
      *
      *  WRITE TO BILLING WORK FILE
      *
      *** BEG ADD ***** 01/20/09 *************************************
      *
      *  A PREMIUM HISTORY RECORD WILL EXIST ON ONE OF THE DIVISIONS
      *  IF THE POLICY HAS BEEN PREVIOUSLY BILLED
      *
     C     EHKYT2        CHAIN     PEH01                              60
     C     *IN60         IFEQ      *ON
      *** END ADD ***** 01/20/09 *************************************
      *
     C     BLKY          CHAIN     PBL01                              60
      *
      *  CLEAR FLAGS IF WRITING NEW RECORD
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        BLFLG1
     C                   MOVE      *BLANK        BLFLG2
     C                   MOVE      *BLANK        BLFLG3
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   Z-ADD     ELCO#         BLCO#
     C                   Z-ADD     ELFND         BLFND
     C                   Z-ADD     ELFYR         BLFYR
     C                   Z-ADD     ELEMP#        BLEMP#
     C                   Z-ADD     ELDIV         BLDIV
      *
      *  ONLY WRITE/UPDATE FLAGS IN WABLP IF NO OTHER PAYROLLS
      *  HAVE EXISTED FOR THE FYR & PAYROLL TYPE IS ESTIMATING.
      *
     C     XXEXST        IFEQ      *OFF
     C                   MOVE      'Y'           BLFLG1
     C                   MOVE      'Y'           BLFLG3
     C                   MOVE      *BLANKS       BLFLG2
     C                   ENDIF                                                  XXEXST=*OFF
      *
     C                   Z-ADD     ELPFDT        BLPFDT
     C                   MOVE      XXRFLG        BLRFLG
      *
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  61
      *
     C     *IN61         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  61
     C                   ENDDO                                                  *IN61=*OFF
      *
     C     *IN61         IFEQ      *OFF
     C     AHSTAT        ANDNE     'I'
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ELSE
     C                   Z-ADD     *ZERO         BLAGT#
     C                   ENDIF                                                  *IN61=*OFF
      *
     C                   MOVEL(P)  Q1USER        BLUSER
     C                   MOVEL(P)  Q1PGM         BLPGM
     C                   Z-ADD     DSTIME        BLTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
      *
      *** BEG ADD ***** 03/27/11 *************************************
     C*
     C     ELNWRW        IFNE      ' '
     C                   MOVE      ELNWRW        BLRFLG
     C                   ENDIF
     C*
      *** END ADD ***** 03/27/11 *************************************
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    PBL01
     C                   ELSE
     C                   WRITE     PBL01
     C                   ENDIF                                                  *IN60=*OFF
      *
      *** BEG ADD ***** 01/20/09 *************************************
     C                   ENDIF                                                  *IN60=*ON
      *** END ADD ***** 01/20/09 *************************************
      *
     C                   ENDSR                                                  BILLNG
      ***  END   ADD  ***  06/24/98  *********************************
      *===============================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS
      *===============================================================
      *
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     EPSTAT        IFEQ      'I'
     C                   MOVE      'A'           EPSTAT
     C                   ELSE
     C                   MOVE      'I'           EPSTAT
     C                   ENDIF                                                  EPSTAT=I
      *
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
      *
     C                   MOVEL     DSTIME        EPTIME
      *
     C                   UPDATE    DBRCDF
      *** BEG ADD ***** 02/05/98 ***********************************************
      *
      *  IF TRACK CHANGES IS "Y"ES, LOAD "Y" TO LDPYRL.
      *
     C     TRKCHG        IFEQ      'Y'
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     'Y'           LDPYRL
     C                   OUT       *DTAARA
     C                   ENDIF
      *** END ADD ***** 02/05/98 ***********************************************
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  CHGSTS
      *
      /EJECT
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
      *
      *** BEG ADD ***** 02/05/98 ***********************************************
      *
      *  IF TRACK CHANGES IS "Y"ES, LOAD "Y" TO LDPYRL.
      *
     C     TRKCHG        IFEQ      'Y'
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     'Y'           LDPYRL
     C                   OUT       *DTAARA
     C                   ENDIF
      *** END ADD ***** 02/05/98 ***********************************************
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
      *
      *  VALIDATE EFFECTIVE DATE
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        20  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C  N90XXEFDT        IFLT      ELPFDT
     C     XXEFDT        ORGT      ELPTDT
     C                   SETON                                        70  90
     C                   ENDIF                                                  ELPFDT>XXEFDT>ELPTDT
      *
      *  LOAD DESCRIPTION OF CLASS CODE AND DO NOT EDIT
      *
     C  N90SVCLAS        IFNE      SCCLAS
     C     SVSPEC        ORNE      SCSPEC
     C     SVEFDT        ORNE      SCEFDT
      ***  BEGIN ADD  ***  12/09/97  ********************************
     C     F10FLG        OREQ      *ON
      ***  END   ADD  ***  12/09/97  ********************************
      *** BEGIN ADD ***  08/11/98  ************************************
     C     SVEXT         ORNE      SCEXT
     C     XX1ST         OREQ      '1'
      *** END ADD *****  08/11/98  ************************************
     C                   MOVE      *ON           *IN90
     C   42              MOVE      *ON           *IN71
     C  N42              MOVE      *ON           *IN72
     C                   ENDIF                                                  SVCLAS<>SCCLAS
      *
      ***  BEGIN ADD  ***  12/09/97  ********************************
     C                   MOVE      *OFF          F10FLG
      ***  END   ADD  ***  12/09/97  ********************************
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVE      ' '           XX1ST
      *** END ADD *****  08/11/98  ************************************
      *
      *  VALIDATE CLASS CODE
      *
     C*** BEGIN ADD ***  09/05/07  ************************************
     C     XXEFDT        IFGE      X2STDT
     C     X2STDT        ANDNE     *ZERO
     C                   Z-ADD     X2STDT        X2EFDT
     C                   ELSE
     C                   Z-ADD     XXSTDT        X2EFDT
     C                   ENDIF
     C*** END ADD *****  09/05/07  ************************************
      *
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
      ***  BEGIN ADD  ***  12/09/97  *********************************
      **N90***** PRKY1     CHAINPPR01                60
      *** BEG CHG ***** 11/02/03 **************************************
     C**N90******PRKY***** SETGTPPR01
     C  N90PRKY2         SETGT     PPR01
      *** END CHG ***** 11/02/03 **************************************
     C  N90PRKY1         READPE    PPR01                                  60
      ***  END   ADD  ***  12/09/97  *********************************
      ***** BEG ADD ******* 03/18/03 **********************************
     C  N60
     CANN90PRARFL        COMP      'Y'                                    50
     C   60
     CANN90              SETOFF                                       50
      ***** END ADD ******* 03/18/03 **********************************
      *
     C  N90*IN60         IFEQ      *ON
      ***** BEG DEL ******* 03/29/06 **********************************
     C********   PRSTAT    OREQ 'I'
      ***** END DEL ******* 03/29/06 **********************************
     C                   SETON                                        21  90
     C                   ENDIF                                                  *IN60=*ON
      ***** BEG ADD ******* 10/19/99 **********************************
      *
      ***** BEG ADD ******* 03/29/06 **********************************
      * If have Payroll Class and it is inactive, is error.
     C  N90*IN60         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'I'
     C                   SETON                                        30  90
     C                   ENDIF                                                  *IN60=*ON
      ***** END ADD ******* 03/29/06 **********************************
      *
      * Emp payroll eff dt < payroll expiration dt, in ADD mode
      *
     C  N90
     CAN 40PREXDT        IFNE      *ZERO
      *** BEG CHG ***** 11/02/03 **************************************
     C*********  XXEFDT    ANDGEPREXDT
     C     XXSTDT        ANDGE     PREXDT
      *** END CHG ***** 11/02/03 **************************************
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
      *
     C***** BEGIN DEL **** 05/01/00 ***********************************
     C** THE SPECIAL DESIGNATION MUST BE 'F', 'S' OR BLANK OR '1'-'9'
     C** FOR AIRCRAFT SURCHARGE
     C**
     C**N90      SCSPEC    IFNE *BLANK
     C**
     C*** BEGIN CHG ***  08/11/98  ************************************
     C*****      SCCLAS    IFNE F2DCCD
     C********** SCCLAS    IFNE RRDCCD
     C*** END CHG *****  08/11/98  ************************************
     C*
     C********** SCSPEC    IFNE 'F'
     C********** SCSPEC    ANDNE'S'
     C*** BEGIN ADD ***  11/11/98  ************************************
     C********** SCSPEC    IFLT '0'
     C********** SCSPEC    ORGT '9'
     C*** END ADD *****  11/11/98  ************************************
     C**********           SETON                     23  90
     C*** BEGIN ADD ***  11/11/98  ************************************
     C**********           ENDIF                           SCSPEC IFLT
     C*** END ADD *****  11/11/98  ************************************
     C**********           ENDIF                           SCSPEC<>F/S
     C**********
     C**********           ELSE
     C**********
     C*** BEG CHG ***** 12/03/97 ***********************************************
     C***        SCSPEC    IFLT '1'
     C********** SCSPEC    IFLT '0'
     C*** END CHG ***** 12/03/97 ***********************************************
     C********** SCSPEC    ORGT '9'
     C**********           SETON                     23  90
     C**********           ENDIF                           1>SCSPEC>9
     C**********
     C**********           ENDIF                           SCCLAS<>F2DCCD
     C**********
     C**********           ENDIF                           SCSPEC<>*BLANK
     C***** END   DEL **** 05/01/00 ***********************************
      *
      *  CHECK TO BE SURE CLASS CODE HAS NOT BEEN USED PREVIOUSLY
      *
     C   40
B02  CANN90EPKY          CHAIN     PEP01                              60
     C   40
     CANN90
     CANN60              SETON                                        22  90
      *
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *  EXTENSION MUST BE VALID.
      *
     C  N90SCEXT         IFNE      *BLANKS
      **** BEGIN ADD **** 09/20/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 09/20/99 ************************************
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
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
      **** BEGIN ADD **** 09/20/99 ************************************
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
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
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
      **** END   ADD **** 09/20/99 ************************************
     C*
     C     *IN60         IFEQ      *ON
      ***** BEG DEL ***** 03/17/06 ************************************
     C*******    PXSTAT    OREQ 'I'
      ***** END DEL ***** 03/17/06 ************************************
     C*** BEGIN CHG ***  08/27/98  ************************************
     C*****                SETON                     31  90
     C                   SETON                                        32  90
     C*** END CHG *****  08/27/98  ************************************
     C                   ENDIF                                                  *IN60 IFEQ
      *
      ***** BEG ADD ***** 03/17/06 ************************************
     C     *IN60         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'I'
     C                   SETON                                        33  90
     C                   ENDIF                                                  *IN60 = *OFF
      ***** END ADD ***** 03/17/06 ************************************
     C*
     C                   ENDIF                                                  SCEAPR<*ZERO
      *** END ADD *****  08/11/98  ************************************
      *
      *  ESTIMATED AMOUNT CANNOT BE LESS THAN ZERO
      *
     C  N90SCEAPR        IFLT      *ZERO
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  AUDITED AMOUNT CANNOT BE LESS THAN ZERO
      *
     C  N90SCAAPR        IFLT      *ZERO
     C                   SETON                                        25  90
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  ESTIMATED # OF EMPLOYEES CANNOT BE LESS THAN ZERO
      *
     C  N90SCECNT        IFLT      *ZERO
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  AUDITED # OF EMPLOYEES CANNOT BE LESS THAN ZERO
      *
     C  N90SCACNT        IFLT      *ZERO
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  ADDITIONAL LOAD CANNOT BE LESS THAN ZERO
      *
     C  N90SCADLD        IFLT      *ZERO
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  VALIDATE CANCEL DATE, IF ENTERED
      *
     C                   Z-ADD     SCCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCADT
      *
     C  N90SRERR         IFEQ      *ON
     C     SCCADT        ANDNE     *ZEROS
     C                   SETON                                        29  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C     *IN90         IFEQ      *OFF
     C     *IN71         OREQ      *ON
     C     *IN72         OREQ      *ON
      *
     C                   MOVEL     *BLANKS       SCDESC
     C*** BEGIN ADD ***  09/05/07  ************************************
     C     XXEFDT        IFGE      X2STDT
     C     X2STDT        ANDNE     *ZERO
     C                   Z-ADD     X2STDT        X2EFDT
     C                   ELSE
     C                   Z-ADD     XXSTDT        X2EFDT
     C                   ENDIF
     C*** END ADD *****  09/05/07  ************************************
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
      *** BEG CHG ***** 11/02/03 **************************************
     C********** PRKY***** SETGTPPR01
     C     PRKY2         SETGT     PPR01
      *** END CHG ***** 11/02/03 **************************************
     C     PRKY1         READPE    PPR01                                  60
      ***** BEG ADD ******* 03/18/03 **********************************
     C  N60PRARFL        COMP      'Y'                                    50
     C   60              SETOFF                                       50
      ***** END ADD ******* 03/18/03 **********************************
      *** BEGIN ADD ***  08/11/98  ************************************
     C     SCEXT         IFNE      *BLANKS
      **** BEGIN ADD **** 09/20/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 09/20/99 ************************************
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
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                62
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  62
     C*** END CHG *****  06/01/06  ************************************
      **** BEGIN ADD **** 09/20/99 ************************************
     C   62RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 62              MOVE      RSJRST        XXST
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
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*62** PXKY      CHAINPPX01                62
     C  N61
     CAN 62PXKY          SETGT     PPX01
     C  N61
     CAN 62PXKY1         READPE    PPX01                                  62
     C*** END CHG *****  06/01/06  ************************************
      **** END   ADD **** 09/20/99 ************************************
     C  N62              MOVEL(P)  PXDESC        SCDESC
     C                   ELSE
      *** END ADD *****  08/11/98  ************************************
     C  N60              MOVEL(P)  PRDESC        SCDESC
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   ENDIF                                                  SCEXT IFNE
      *** END ADD *****  08/11/98  ************************************
      *
     C  N60SCRATE        IFEQ      *ZEROS
     C     SCCLAS        ORNE      SVCLAS
      *** BEG ADD ***** 08/10/05 **********************************
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   SCCLAS        QQCLAS
     C                   MOVE(P)   XXSTDT        QQEFDT
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
     C***** BEGIN ADD **** 08/06/99 ***********************************
     C                   MOVE      PRHZGR        XXHZGR
      *  RIGHT JUSTIFY
     C     ' '           CHECKR    XXHZGR:5      X                 5 0
     C     5             SUB       X             X1                5 0
     C                   MOVEL(P)  *BLANKS       SCHZGR
     C                   CAT(P)    XXHZGR:X1     SCHZGR
     C***** END   ADD **** 08/06/99 ***********************************
     C                   ENDIF                                                  SCRATE=*ZEROS
      *
     C                   ENDIF                                                  *IN90=*OFF
      *
     C                   Z-ADD     SCEFDT        SVEFDT
     C                   Z-ADD     SCCLAS        SVCLAS
     C                   MOVEL     SCSPEC        SVSPEC
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     SCEXT         SVEXT
      *** END ADD *****  08/11/98  ************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
      *** BEG ADD ***** 11/21/97 ***********************************************
      *
      *    IF AIRCRAFT SURCHARGE PAYROLL CLASS CODE, GET AIRCRAFT DESC
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      1             #OFSCR
      *
      *    GET THE JURISDICTION STATE FROM THE RATE SET CODE FILE
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XREFDT
      *
      *    GET THE STATISTICAL RPT/RATE FILE RECORD
      *
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
      *
      *    PROCESS THE AIRCRAFT DESCRIPTION, IF APPROPRIATE
      *
     C     SCCLAS        IFEQ      RRDCCD
     C                   MOVE      2             #OFSCR
     C*** BEGIN ADD ***  09/09/10  ************************************
     C                   MOVE      '1'           *IN34
     C*** END   ADD ***  09/09/10  ************************************
     C*** BEGIN ADD ***  08/11/98  ************************************
     C     *IN40         IFEQ      *ON
     C     SCEXT         ANDNE     *BLANKS
     C                   MOVEL(P)  SCDESC        SCACDS
     C                   ENDIF
     C*** END ADD *****  08/11/98  ************************************
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *** END ADD ***** 11/21/97 ***********************************************
     I*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C     SCEAPR        IFNE      SVEAPR
     C                   MOVE      'Y'           XXCHG             1
     C                   ELSE
     C                   MOVE      'N'           XXCHG             1
     C                   ENDIF
     I*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*** BEG ADD *****  09/09/10  ************************************
     C     CLTID         IFEQ      '093'
     C                   SETOFF                                       36
     C                   MOVE      2             #OFSCR
     C                   MOVE      '0'           *IN35
     C     SCAAAW        IFLT      *ZERO
     C                   SETON                                        36  90
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN35
     C                   ENDIF
     C*
     C*** END ADD *****  09/09/10  ************************************
      *
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
      *** BEG ADD ***** 11/21/97 ***********************************************
      *
      *  AIRCRAFT DESCRIPTION MUST BE FILLED
      *
     C     SCACDS        IFEQ      *BLANK
     C                   MOVE      *ON           *IN71
     C                   ENDIF
      *** END ADD ***** 11/21/97 ***********************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C*** BEG ADD *****  09/09/10  ************************************
     C     CLTID         IFEQ      '093'
     C                   SELECT
     C     SCUNTW        WHENLT    *ZERO
     C                   SETON                                        44  90
     C     SCUNTH        WHENLT    *ZERO
     C                   SETON                                        45  90
     C     SCUNFA        WHENLT    *ZERO
     C                   SETON                                        46  90
     C     SCUNFT        WHENLT    *ZERO
     C                   SETON                                        47  90
     C     SCUNSI        WHENLT    *ZERO
     C                   SETON                                        48  90
     C     SCGTSI        WHENLT    *ZERO
     C                   SETON                                        49  90
      *
     C                   ENDSL
     C                   ENDIF
     C*** ADD ADD *****  09/09/10  ************************************
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK3
      *
      ***** BEG ADD ***** 12/16/04 ***********************************
      *===============================================================
      * GETSDT - SUBROUTINE to get Search Date
      *===============================================================
      *
     C     GETSDT        BEGSR
      *
     C                   Z-ADD     *ZEROS        TEMPDT            7 0
     C                   Z-ADD     *ZEROS        XXDATE            7 0
     C                   Z-ADD     *ZEROS        ZZDATE            7 0
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C                   SELECT
      *
      * If no ARD, use Policy effective Date
     C     ELANDT        WHENLE    *ZEROS
     C                   Z-ADD     ELPFDT        ZZDATE
      *
      * If Have ARD, check for within one year
     C     ELANDT        WHENGT    *ZEROS
     C     ELANDT        ADD       10000         TEMPDT
      *
      * If current date is less than ARD + one Year, then display
      * the most current effective class code prior to the ARD
     C     XXDATE        IFLT      TEMPDT
     C                   Z-ADD     ELANDT        ZZDATE
     C                   ENDIF
      *
      * If current date is greater than or equal to ARD + one year, then
      * display most current effective class code prior to ARD + one year.
     C     XXDATE        IFGE      TEMPDT
     C                   Z-ADD     TEMPDT        ZZDATE
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  GETSDT
      *
      ***** END ADD ***** 12/16/04 ***********************************
      *
      /EJECT
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCCLAS'
     C     HFLD          OREQ      'SCSPEC'
      *
     C     *IN40         IFEQ      *ON
      *
      ***  BEGIN ADD  ***  12/09/97  *********************************
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *ON           F10FLG
      ***  END   ADD  ***  12/09/97  *********************************
      *
      ***** BEG ADD ***** 12/16/04 ***********************************
     C                   MOVE      *BLANKS       ##DATE            8
     C                   EXSR      GETSDT
     C*
     C                   Z-ADD     ZZDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DATE
     C***** BEG ADD ***** 04/01/08 ***********************************
     C                   MOVE      SCEFDT        ##DATE
     C***** END ADD ***** 04/01/08 ***********************************
     C*
     C***********          MOVE ZZDATE    ##DATE
      ***** END ADD ***** 12/16/04 ***********************************
      *
     C                   CALL      'W0630R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM      ELRTCD        @@RTCD            3
     C                   PARM      2             @@VIEW            1 0
      ***** BEG ADD ***** 12/16/04 ***********************************
     C                   PARM      ##DATE        @@DATE            8
      ***** END ADD ***** 12/16/04 ***********************************
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTC=A
      *
     C     @@RRN         IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN73
      *
     C                   ELSE
      *
     C     @@RRN         CHAIN     PPR02                              99
      *
     C                   MOVE      PRCLAS        SCCLAS
     C                   MOVE      PRCLAS        SVCLAS
     C                   MOVE      SCSPEC        SVSPEC
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     SCEXT         SVEXT
      *** END ADD *****  08/11/98  ************************************
      *
     C                   MOVE      *ON           *IN74
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL     SRDTO         SVEFDT
      *
     C                   MOVEL     *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCRATE
     C***** BEGIN ADD **** 08/06/99 ***********************************
     C                   MOVE      *BLANKS       SCHZGR
     C                   MOVE      *BLANKS       XXHZGR
     C***** END   ADD **** 08/06/99 ***********************************
     C*** BEGIN ADD ***  09/05/07  ************************************
     C     XXEFDT        IFGE      X2STDT
     C     X2STDT        ANDNE     *ZERO
     C                   Z-ADD     X2STDT        X2EFDT
     C                   ELSE
     C                   Z-ADD     XXSTDT        X2EFDT
     C                   ENDIF
     C*** END ADD *****  09/05/07  ************************************
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
      *** BEG CHG ***** 11/02/03 **************************************
     C********** PRKY      SETGTPPR01
     C     PRKY2         SETGT     PPR01
      *** END CHG ***** 11/02/03 **************************************
     C     PRKY1         READPE    PPR01                                  60
      ***** BEG ADD ******* 03/18/03 **********************************
     C  N60PRARFL        COMP      'Y'                                    50
     C   60              SETOFF                                       50
      ***** END ADD ******* 03/18/03 **********************************
      *
      ***  BEGIN CHG  ***  12/09/97  *********************************
      **N60*****           MOVELPRDESC    SCDESC    P
      **N60*****           Z-ADDPRRT      SCRATE
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PRDESC        SCDESC
      *** BEG ADD ***** 08/10/05 **********************************
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   SCCLAS        QQCLAS
     C                   MOVE(P)   XXSTDT        QQEFDT
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
     C***** BEGIN ADD **** 08/06/99 ***********************************
     C                   MOVE      PRHZGR        XXHZGR
     C*
      *  RIGHT JUSTIFY
     C     ' '           CHECKR    XXHZGR:5      X                 5 0
     C     5             SUB       X             X1                5 0
     C                   MOVEL(P)  *BLANKS       SCHZGR
     C                   CAT(P)    XXHZGR:X1     SCHZGR
     C***** END   ADD **** 08/06/99 ***********************************
     C                   ELSE
     C                   CLEAR                   SVCLAS
     C                   ENDIF                                                  *IN60=*OFF
      ***  END   CHG  ***  12/09/97  *********************************
      *
     C                   ENDIF                                                  SCCLAS=*ZEROS
      *
     C                   ENDIF                                                  *IN40=*ON
      *** BEGIN ADD ***  08/11/98  ************************************
     C     HFLD          WHENEQ    'SCEXT'
     C                   MOVE      *ON           F10FLG
      *
     C                   CALL      'W0650R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ELCO#
     C                   PARM                    SCCLAS
      **** BEGIN ADD **** 11/16/99 ************************************
     C                   PARM                    RSJRST
      **** END   ADD **** 11/16/99 ************************************
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTC=A
      *
     C     ##RRN         IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN77
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PPX02                              99
      *
      ***** BEG ADD ***** 03/17/06 ************************************
     C     *IN99         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'I'
     C                   SETON                                        33  90
     C                   SETON                                        73
     C                   ENDIF                                                  *IN60 = *OFF
      ***** END ADD ***** 03/17/06 ************************************
     C                   MOVE      PXEXT         SCEXT
     C                   MOVE      PXEXT         SVEXT
      *
     C   42              MOVE      *ON           *IN71
     C  N42              MOVE      *ON           *IN72
      *
     C                   MOVEL(P)  PXDESC        SCDESC
     C                   ENDIF                                                  @@RRN IFNE
      *** END ADD *****  08/11/98  ************************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      /EJECT
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
      ***** BEGIN ADD ******* 08/07/98 ********************************
     C   31              MOVE      'Q'           QCQTE
     C  N31              MOVE      *BLANKS       QCQTE
      ***** END   ADD ******* 08/07/98 ********************************
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      ******* END   ADD ****** 04/03/98 *******************************
      *
      /EJECT
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C     EPSAFL        COMP      'A'                                4141
      *
     C   40              MOVEL     'A'           SCSTAT
     C  N40              MOVEL     EPSTAT        SCSTAT
      *
     C                   Z-ADD     EPEFDT        SRDTI
      ***  BEGIN ADD  ***  11/12/01  *********************************
     C     *IN40         IFEQ      *ON
     C     XXQTE         IFEQ      'Q'
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ELSE
     C     XXEXST        IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ***  END   ADD  ***  11/12/01  *********************************
     C                   CALL      'SRCVTDAT'    DTMDYY
      ***  BEGIN ADD  ***  12/09/97  *********************************
     C     FIRST         IFEQ      *ON
     C     *IN40         ANDEQ     *ON
     C     *IN40         OREQ      *OFF
      ***  END   ADD  ***  12/09/97  *********************************
     C                   MOVEL     SRDTO         SCEFDT
      ***  BEGIN ADD  ***  12/09/97  *********************************
     C                   MOVE      *OFF          FIRST
     C                   ELSE
     C                   MOVE      *ON           *IN73
     C                   ENDIF                                                  FIRST=*ON
      ***  END   ADD  ***  12/09/97  *********************************
      *
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   MOVEL     EPSPEC        SCSPEC
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     EPEXT         SCEXT
     C                   MOVEL     SCEXT         SVEXT
      *** END ADD *****  08/11/98  ************************************
      *
     C                   Z-ADD     SCEFDT        SVEFDT
     C                   Z-ADD     EPCLAS        SVCLAS
     C                   MOVEL     EPSPEC        SVSPEC
      *
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   MOVEL     *BLANKS       SCDESC
     C***** BEGIN ADD **** 08/06/99 ***********************************
     C                   MOVE      *BLANKS       SCHZGR
     C                   MOVE      *BLANKS       XXHZGR
     C*
     C*** BEGIN ADD ***  09/05/07  ************************************
     C     XXEFDT        IFGE      X2STDT
     C     X2STDT        ANDNE     *ZERO
     C                   Z-ADD     X2STDT        X2EFDT
     C                   ELSE
     C                   Z-ADD     XXSTDT        X2EFDT
     C                   ENDIF
     C*** END ADD *****  09/05/07  ************************************
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
      *** BEG CHG ***** 11/02/03 **************************************
     C********** PRKY      SETGTPPR01
     C     PRKY2         SETGT     PPR01
      *** END CHG ***** 11/02/03 **************************************
     C     PRKY1         READPE    PPR01                                  60
      ***** BEG ADD ******* 03/18/03 **********************************
     C  N60PRARFL        COMP      'Y'                                    50
     C   60              SETOFF                                       50
      ***** END ADD ******* 03/18/03 **********************************
     C*
     C  N60              MOVE      PRHZGR        XXHZGR
     C*
      *  RIGHT JUSTIFY
     C     ' '           CHECKR    XXHZGR:5      X                 5 0
     C     5             SUB       X             X1                5 0
     C                   MOVEL(P)  *BLANKS       SCHZGR
     C                   CAT(P)    XXHZGR:X1     SCHZGR
     C*
     C***** END   ADD **** 08/06/99 ***********************************
      *** BEGIN ADD ***  08/11/98  ************************************
     C     SCEXT         IFNE      *BLANKS
      **** BEGIN ADD **** 09/20/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 09/20/99 ************************************
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
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                62
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  62
     C*** END CHG *****  06/01/06  ************************************
      **** BEGIN ADD **** 09/20/99 ************************************
     C   62RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 62              MOVE      RSJRST        XXST
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
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*62** PXKY      CHAINPPX01                62
     C  N61
     CAN 62PXKY          SETGT     PPX01
     C  N61
     CAN 62PXKY1         READPE    PPX01                                  62
     C*** END CHG *****  06/01/06  ************************************
      **** END   ADD **** 09/20/99 ************************************
     C  N62              MOVEL(P)  PXDESC        SCDESC
     C                   ELSE
      *** END ADD *****  08/11/98  ************************************
     C***** BEGIN DEL **** 08/06/99 ***********************************
     C********** PRKY      SETGTPPR01
     C********** PRKY1     REDPEPPR01                    60
     C***** END   DEL **** 08/06/99 ***********************************
     C  N60              MOVEL(P)  PRDESC        SCDESC
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   ENDIF                                                  SCEXT IFNE
      *** END ADD *****  08/11/98  ************************************
      *
     C                   Z-ADD     EPARAT        SCRATE
     C                   Z-ADD     EPEAPR        SCEAPR
     C                   Z-ADD     EPECNT        SCECNT
     C                   Z-ADD     EPADLD        SCADLD
      *
     C                   Z-ADD     EPCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCADT
      *
     C                   Z-ADD     EPAAPR        SCAAPR
     C                   Z-ADD     EPACNT        SCACNT
      *
     C                   Z-ADD     EPSAPR        SCSAPR
     C                   Z-ADD     EPEMPR        SCEMPR
     C                   Z-ADD     EPAMPR        SCAMPR
      *** BEG ADD ***** 11/21/97 ***********************************************
     C                   MOVEL     EPACDS        SCACDS
      *
      *  IF AIRCRAFT SURCHRG CLASS CODE, CHG NUMBER OF SCREENS TO TWO
      *
      *    GET THE JURISDICTION STATE FROM THE RATE SET CODE FILE
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EPEFDT        XREFDT
      *
      *    GET THE STATISTICAL RPT/RATE FILE RECORD
      *
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
      *
      *    PROCESS THE AIRCRAFT DESCRIPTION, IF APPROPRIATE
      *
     C     *IN60         IFEQ      *OFF
     C     SCCLAS        ANDEQ     RRDCCD
     C                   MOVE      2             #OFSCR
     C*** BEGIN ADD ***  04/12/12  ************************************
     C                   MOVE      '1'           *IN34
     C*** END   ADD ***  04/12/12  ************************************
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *** END ADD ***** 11/21/97 ***********************************************
      *** BEG ADD ***** 02/05/98 ***********************************************
      *
      *  SAVE DATE FIELDS FOR LATER COMPARISON FOR CHANGE TRACKING
      *
     C                   MOVE      EPEFDT        S#EFDT
     C                   MOVE      EPCADT        S#CADT
      *** END ADD ***** 02/05/98 ***********************************************
     C*** BEG ADD ***** 09/09/10 ***********************************************
     C     CLTID         IFEQ      '093'
     C                   Z-ADD     2             #OFSCR
     C                   SELECT
     C     @@MODE        WHENEQ    'D'
     C                   MOVE      '1'           *IN10
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      '0'           *IN10
     C     @@MODE        WHENEQ    'C'
     C                   MOVE      '0'           *IN10
      *
     C                   ENDSL
     C                   MOVE      EPAAAW        SCAAAW
     C                   MOVE      EPUNTW        SCUNTW
     C                   MOVE      EPUNTH        SCUNTH
     C                   MOVE      EPUNFA        SCUNFA
     C                   MOVE      EPUNFT        SCUNFT
     C                   MOVE      EPUNSI        SCUNSI
     C                   MOVE      EPGTSI        SCGTSI
     C                   ENDIF
     C*
     C*** END ADD ***** 09/09/10 ***********************************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      /EJECT
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *** BEG ADD ***** 02/05/98 ***********************************************
      *
      *  IF TRACK CHANGES IS "Y"ES, LOAD "Y" TO LDPYRL IF ADD MODE
      *  OR CHANGES HAVE OCCURRED.
      *
     C     TRKCHG        IFEQ      'Y'
      *
     C     *IN40         IFEQ      *ON
     C     S#EFDT        ORNE      EPEFDT
     C     SCSTAT        ORNE      EPSTAT
     C     SCCLAS        ORNE      EPCLAS
     C     SCSPEC        ORNE      EPSPEC
      *** BEGIN ADD ***  08/11/98  ************************************
     C     SCEXT         ORNE      EPEXT
      *** END ADD *****  08/11/98  ************************************
     C     SCRATE        ORNE      EPARAT
     C     SCEAPR        ORNE      EPEAPR
     C     SCECNT        ORNE      EPECNT
     C     SCADLD        ORNE      EPADLD
     C     S#CADT        ORNE      EPCADT
     C     SCAAPR        ORNE      EPAAPR
     C     SCACNT        ORNE      EPACNT
     C     SCACDS        ORNE      EPACDS
      *
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     'Y'           LDPYRL
     C                   OUT       *DTAARA
      *
     C                   ENDIF                                                  *IN40 = *ON
      *
     C                   ENDIF                                                  TRKCHG = 'Y'
      *** END ADD ***** 02/05/98 ***********************************************
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPEFDT
      *
     C                   MOVEL     SCSTAT        EPSTAT
     C                   Z-ADD     SCCLAS        EPCLAS
     C                   MOVEL     SCSPEC        EPSPEC
      *** BEGIN ADD ***  08/11/98  ************************************
     C                   MOVEL     SCEXT         EPEXT
      *** END ADD *****  08/11/98  ************************************
     C                   Z-ADD     SCRATE        EPARAT
     C                   Z-ADD     SCEAPR        EPEAPR
     C                   Z-ADD     SCECNT        EPECNT
     C                   Z-ADD     SCADLD        EPADLD
      *
     C                   Z-ADD     SCCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPCADT
      *
     C                   Z-ADD     SCAAPR        EPAAPR
     C                   Z-ADD     SCACNT        EPACNT
      *** BEG ADD ***** 11/21/97 ***********************************************
     C                   MOVEL     SCACDS        EPACDS
      *** END ADD ***** 11/21/97 ***********************************************
      ***  BEGIN ADD  ***  11/25/97  *********************************
     C   40              MOVE      'E'           EPSAFL
      ***  END   ADD  ***  11/25/97  *********************************
     C*** BEG ADD ***** 09/09/10 ***********************************************
     C     CLTID         IFEQ      '093'
     C                   MOVE      SCAAAW        EPAAAW
     C                   MOVE      SCUNTW        EPUNTW
     C                   MOVE      SCUNTH        EPUNTH
     C                   MOVE      SCUNFA        EPUNFA
     C                   MOVE      SCUNFT        EPUNFT
     C                   MOVE      SCUNSI        EPUNSI
     C                   MOVE      SCGTSI        EPGTSI
     C                   ENDIF
     C*
     C*** END ADD ***** 09/09/10 ***********************************************
      *
     C                   ENDSR                                                  MVSCDB
      *
      /EJECT
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
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
      ******* BEGIN ADD ****** 04/03/98 *******************************
     C     *INKN         CASEQ     *ON           KNDO                     90
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
     C                   CLOSE     WMEPP
     C                   CLOSE     WDELP
      *** BEG CHG ***** 04/20/09 ***********************************************
     C**********           CLOSEWMEPL1
     C                   CLOSE     WMEPL1E
      *** BEG CHG ***** 04/20/09 ***********************************************
      *
     C                   ENDSR
      *
      /EJECT
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
      *
      *** BEG ADD ***** 02/05/98 ***********************************************
      *
      *  IF CALLED FROM EMPLOYER MID TERM ENDORSEMENT PROCESSING, SET
      *  TRACK CHANGES FLAG TO "Y"ES.
      *
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   MOVE      'Y'           TRKCHG            1
     C                   ENDIF
      *** END ADD ***** 02/05/98 ***********************************************
      *
     C                   OPEN      WMEPP
     C                   OPEN      WDELP
     C**********BEG CHG ********** 04/20/09 ******************************
     C****************     OPEN WMEPL1
     C                   OPEN      WMEPL1E
     C**********END CHG ********** 04/20/09 ******************************
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C***  BEGIN CHG  ***  09/01/98  *********************************
     C**********BEGIN ADD ********** 08/07/98 *************************
     C********** @@QTE     COMP 'Q'                      31QUOTE
     C**********END   ADD ********** 08/07/98 *************************
     C*
     C     Q1PRMS        IFGE      7
     C                   MOVEL     @@QTE         XXQTE
     C                   ELSE
     C                   MOVEL     *BLANKS       XXQTE
     C                   ENDIF                                                  Q1PRMS>=7
     C*
     C     XXQTE         COMP      'Q'                                    31    QUOTE
     C***  END   CHG  ***  09/01/98  *********************************
      *
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     Q1PRMS        IFGT      5
     C                   MOVE      @@BILL        XXBILL
     C                   ELSE
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF                                                  Q1PRMS>5
      ***  END   ADD  ***  06/24/98  *********************************
      *
      ***  BEGIN ADD  ***  12/09/97  *********************************
     C                   MOVE      *ON           FIRST
     C                   MOVE      *OFF          F10FLG
      ***  END   ADD  ***  12/09/97  *********************************
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *** BEG DEL ***** 11/21/97 ***********************************************
     C                   Z-ADD     1             #OFSCR            1 0
      *** END DEL ***** 11/21/97 ***********************************************
      *** BEG ADD ***** 09/09/10 ***********************************************
     C     CLTID         IFEQ      '093'
     C                   Z-ADD     2             #OFSCR
     C                   SELECT
     C     @@MODE        WHENEQ    'D'
     C                   MOVE      '1'           *IN10
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      '0'           *IN10
     C                   ENDSL
     C                   ELSE
     C                   MOVE      '1'           *IN35
     C                   ENDIF
      *** END ADD ***** 09/09/10 ***********************************************
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
      *  GET EMPLOYER RECORD
      *
     C     @@RRN2        CHAIN     WDELP                              60
      *** BEG ADD ***** 11/02/03 *************************************
      *
      *  Get state reporting division's eff date or anniversary date, if appli-
      *  cable
      *
     C                   Z-ADD     ELDIV         X1DIV                          Store policy info
     C                   Z-ADD     ELPFDT        X1PFDT
     C*** BEGIN ADD ***  09/05/07  ************************************
     C                   Z-ADD     0             XXSTDT
     C                   Z-ADD     0             X2STDT            7 0
     C*** END ADD *****  09/05/07  ************************************
     C*** BEGIN ADD ***  11/11/04  ************************************
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     ELPFDT        XSPFDT
     C                   ENDIF
     C*** END ADD *****  11/11/04  ************************************
     C                   Z-ADD     ELANDT        X1ANDT
     C                   Z-ADD     ELFYR         XSFYR
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C*** BEGIN ADD ***  11/11/04  ************************************
     C     XXST          OREQ      'PA'
     C*
     C     XXST          IFEQ      'PA'
     C*
     C                   Z-ADD     EMDIV         XSDIV
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     'PA'
     C     ELPFDT        ANDLT     XSPFDT
     C                   Z-ADD     ELPFDT        XSPFDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE                                                   XXST IFEQ
     C*** END ADD *****  11/11/04  ************************************
     C                   Z-ADD     EMDIV         XSDIV
     C     ELKY1         CHAIN     PEL02                              60
     C                   Z-ADD     ELPFDT        XSPFDT                         Get state rpt div
     C                   Z-ADD     ELANDT        XSANDT                         info
     C                   LEAVE
     C*** BEGIN ADD ***  11/11/04  ************************************
     C                   ENDIF                                                  XXST IFEQ
     C*** END ADD *****  11/11/04  ************************************
     C                   ENDIF
      *
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
     C     X1DIV         IFNE      XSDIV
     C     X1ANDT        ANDEQ     *ZERO
      *
     C     XSANDT        IFNE      *ZERO
     C                   ADD       10000         XSANDT
      *
     C     XSANDT        IFLE      X1PFDT
     C                   Z-ADD     XSANDT        XXSTDT
     C                   ELSE
     C     XSANDT        SUB       10000         XXSTDT
     C*** BEGIN ADD ***  09/05/07  ************************************
     C                   Z-ADD     XSANDT        X2STDT
     C*** END ADD *****  09/05/07  ************************************
     C                   ENDIF
      *
     C                   ENDIF                                                  XSANDT
      *
     C                   ENDIF                                                  X1DIV
      *
     C     XXSTDT        IFEQ      *ZERO
      *
     C     XSPFDT        IFLE      X1PFDT
     C                   Z-ADD     XSPFDT        XXSTDT
     C                   ENDIF
      *
     C                   ENDIF                                                  XXSTDT
      *
     C     X1ANDT        IFNE      *ZERO
     C     X1ANDT        ANDLE     XXSTDT
     C                   Z-ADD     X1ANDT        XXSTDT
     C*** BEGIN ADD ***  09/05/07  ************************************
     C     X1ANDT        ADD       10000         X2STDT
     C*** END ADD *****  09/05/07  ************************************
     C                   ENDIF
      *
     C     @@RRN2        CHAIN     WDELP                              60
      *** END ADD ***** 11/02/03 *************************************
      *
     I*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XAFROM            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XATO              8 0
     C*
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XAFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XATO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   Z-ADD     XQFROM        XXLPDT            7 0
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XAFROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XAFROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C                   Z-ADD     ELPFDT        X2PFDT
     C                   Z-ADD     ELPTDT        X2PTDT
     C                   Z-ADD     ELPROR        X2PROR
     C                   MOVEL     ELFFL9        X2FFL9
     C*
     I*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      ***  BEGIN ADD  ***  06/24/98  *********************************
      *  SET FLAG IF PAYROLL HAS EXISTED BEFORE FOR YR
      *
     C                   MOVE      *OFF          XXEXST
     C     EPKY1         CHAIN     PEP02                              60
     C  N60              MOVE      *ON           XXEXST
     I*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   Z-ADD     EPEAPR        SVEAPR
     I*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
      *  SET RENEWAL FLAG
      *
     C*** BEGIN ADD ***  12/11/98  ************************************
     C     AHKY          CHAIN     PEL02                              60
     C*** END ADD *****  12/11/98  ************************************
     C     ELKY          READPE    PEL02                                  60
     C*** BEGIN ADD ***  03/08/02  ************************************
     C   60ZEXKY         CHAIN     PEX01                              60
     C*** END ADD *****  03/08/02  ************************************
      *
     C     *IN60         IFEQ      *OFF
     C***  Begin Add  ***  10/10/03  **************************************
     C     ELRQFL        OREQ      'Y'
     C***  End   Add  ***  10/10/03  **************************************
     C                   MOVE      'R'           XXRFLG
     C                   ELSE
     C                   MOVE      'N'           XXRFLG
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     @@RRN2        CHAIN     WDELP                              60
      ***  END   ADD  ***  06/24/98  *********************************
      *
     C                   Z-ADD     ELFYR         SCFYR
      *
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                  ELFAF=A
      *
     C     ELSTAT        COMP      'Y'                                    42
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
      *  GET GROUP INFORMATION
      *
     C     FNKY          CHAIN     PFN01                              60
     C     FNKY          CHAIN     PF201                              60
      *
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
      *  GET AGENT RECORDS
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *
     C     @@MODE        IFEQ      'A'
     C     ELFAF         ANDEQ     'A'
      *
     C                   MOVEL     'N'           SCEOVR
     C                   EXFMT     WDWMSG
      *
     C     SCEOVR        IFNE      'Y'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF                                                  SCEOVR<>Y
      *
     C                   ENDIF                                                  @@MODE=A/ELFAF=A
      *
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *    GET THE JURISDICTION STATE FROM THE RATE SET CODE FILE
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C                   Z-ADD     ELPFDT        XREFDT
      *
      *    GET THE STATISTICAL RPT/RATE FILE RECORD
      *
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C                   MOVE      '1'           XX1ST             1
      *** END ADD *****  08/11/98  ************************************
      *
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX01                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 08/10/98 **********************************
     C***** BEGIN ADD **** 08/06/99 ***********************************
     C*  Initialize Hazard Group
     C                   MOVE      *BLANKS       SCHZGR
     C                   MOVE      *BLANKS       XXHZGR
     C*
     C***** END   ADD **** 08/06/99 ***********************************
     C                   ENDSR                                                  PGMSTR
      *
      /EJECT
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
      ***  BEGIN CHG  ***  12/09/97  *********************************
     C                   EXSR      PGMEND
      ***  END   CHG  ***  12/09/97  *********************************
     C                   RETURN
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  PREOTH
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
B02  C     *IN99         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     DSTIME        EPTIME
     I*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*
     C                   Z-ADD     X2PFDT        XATODT
     C                   ADD       1             XXTOYR
      *
     C     XXCHG         IFEQ      'Y'
     C     X2PTDT        ANDNE     XATODT
     C     X2FFL9        ANDEQ     'Y'
     C                   Z-ADD     0             EPEMPR
     C                   ENDIF
     C*
     I*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
     C                   UPDATE    DBRCDF
      *
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     XXBILL        IFEQ      'Y'
     C                   EXSR      BILLNG
     C                   ENDIF                                                  XXBILL=Y
      ***  END   ADD  ***  06/24/98  *********************************
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
     C                   Z-ADD     ELCO#         EPCO#
     C                   Z-ADD     ELFND         EPFND
     C                   Z-ADD     ELEMP#        EPEMP#
     C                   Z-ADD     ELDIV         EPDIV
     C                   Z-ADD     ELFYR         EPFYR
      *
     C                   EXSR      MVSCDB
      *
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
      *
     C                   WRITE     DBRCDF
      *
      ***  BEGIN ADD  ***  06/24/98  *********************************
     C     XXBILL        IFEQ      'Y'
     C                   EXSR      BILLNG
     C                   ENDIF                                                  XXBILL=Y
      ***  END   ADD  ***  06/24/98  *********************************
      *
     C                   FEOD      WMEPP
      *** BEG ADD ***** 11/21/97 ***********************************************
     C                   MOVE      1             #OFSCR
      *** END ADD ***** 11/21/97 ***********************************************
      *
     C                   ENDSR                                                  WRITER
