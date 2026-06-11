     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0660R2 - EMPLOYER PAYROLL AUDIT INFORMATION             *
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD AUDIT INFO IN THE EMPLOYER PAYROLL FILE.*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0060R - WORK WITH EMPLOYER PAYROLLS                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/25/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/27/11 C:235 SUPPORT: 71062                                 *
     H*                MASS BILLING ISSUES                            *
     H*                New/Renew Policy Flag is used to populate      *
     H*                RENEWAL FLAG (NEW/RENEW FLAG) in Billing       *
     H*                Transation File if New/Renew Policy Flag       *
     H*                not Blank                                      *
     H*                                                               *
     H* 10/04/10 C:136 SUPPORT: WA139 73900                           *
     H*                PREVENT ADDING DUPLICATE PAYROLLS              *
     H*                CONCURRENTLY. VERSIONS AT V10 AND LATER        *
     H*                ALSO CHECK THE EXTENSION DESCRIPTION           *
     H*                FIELD (SCEXT)                                  *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                Add column to display/enter the                *
     H*                extension field.                               *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 04/17/07 C:191 PROJECT 93WFZ                                  *
     H*                ZERO AUDITED PAYROLL W/ CLAIMS EDIT            *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE. ADD FULL CODING FOR FILE.      *
     H*                                                               *
     H* 02/01/06 C: 35 SUPPORT SEQ#51261                              *
     H*                REMOVE EDIT FROM SCSPEC FIELD.  ANY ENTRY IS   *
     H*                VALID.                                         *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: WC1136                                *
     H*                ADD LOSS COST MULTIPLIER                       *
     H*                                                               *
     H* 04/11/05 C:136 SUPPORT: WA081 46406                           *
     H*                PREVENT WABLP (MASS BILLING) RECORD FROM       *
     H*                BEING WRITTEN UNLESS ALL ACTIVE PAYROLLS       *
     H*                ARE AUDITED.                                   *
     H*                                                               *
     H* 01/03/05 C:136 SUPPORT: WA108 44908                           *
     H*                DO NOT INVOKE 'CLAIM EXISTS' EDIT FOR ZERO     *
     H*                AUDITED PAYROLL IF CLIENT IS 'ADMIN ONLY'      *
     H*                                                               *
     H* 01/06/04 C:136 SUPPORT: WA093 40657 (WC1159)                  *
     H*                DISPLAY OVERRIDE WINDOW IF                     *
     H*                (1) CLAIM PAYMENTS EXIST AND CLASS IS BEING    *
     H*                    AUDITED FOR A ZERO PAYROLL AMT             *
     H*                (2) CLAIM PAYMENTS EXIST AND CLASS IS BEING    *
     H*                    CHANGED TO A ZERO AUDIT AMT FROM A         *
     H*                    PREVIOUS AUDIT                             *
     H*                (3) THERE IS NO CLAIMS/DART FILE. NO OVERRIDE  *
     H*                    OPTION WILL BE OFFERED                     *
     H*                                                               *
     H* 10/14/03 C:136 PROJECT: WC1159                                *
     H*                ADD SECURITY LVL/TYPE OVERRIDE ON EDIT FOR     *
     H*                ZERO PAYROLL WITH CLAIMS.                      *
     H*                                                               *
     H*  03/08/02 C:65 PROJECT: WC1030                                *
     H*                IF THE EMPLOYER HAS A PREVIOUS EMPLR# THEN     *
     H*                CONSIDER THE POLICY AS A RENEWAL.              *
     H*                                                               *
     H* 08/30/01 C:61  PROJECT WC577                                  *
     H*                Change payroll class special designation edit  *
     H*                to use WDRRP/RRDCCD instead of WDF2P/F2DCCD.   *
     H*                                                               *
     H* 04/24/01 C:61  PROJECT WC577                                  *
     H*                Correct paging issues.                         *
     H*                                                               *
     H* 04/06/01  C:35   PROJECT: SUPPORT WA112 #27019                *
     H*                  SEARCH ON CLASS WAS PUTTING CODE ON SCREEN   *
     H*                  WHEN NONE WAS SELECTED.                      *
     H*                                                               *
     H* 12/05/00 C:61  PROJECT WC577                                  *
     H*                Info edit window if claims and amt=0.          *
     H*                                                               *
     H* 07/19/00 C:61  PROJECT WC577 SEQ 23371                        *
     H*                If payroll class is added, do not allow efdt   *
     H*                equal policy TO date.                          *
     H*                                                               *
     H* 10/19/99 C:61  PROJECT WC944                                  *
     H*                If payroll class is added, edit to ensure      *
     H*                effective date is not greater than expiration  *
     H*                date for payroll class.                        *
     H*                                                               *
     H* 06/05/99  C:33  SEQ#: WA093/18333                             *
     H*                 IF AN AUDIT IS BACKED OUT, LOAD 'E' TO EPSAFL.*
     H*                                                               *
     H* 02/10/99  C:33  SEQ#: WA048/15373                             *
     H*                 ALLOW FOR AUDITS TO BE BACKED OUT.            *
     H*                                                               *
     H* 02/09/99  C:33  SEQ#: WA081/15499                             *
     H*                 DO NOT ALLOW A NEW ENTRY OF A CLASS CODE THAT *
     H*                 IS CURRENTLY INACTIVE FOR THE EMPLOYER.       *
     H*                                                               *
     H*  12/11/98  C:65  PROJECT: SUPPORT                             *
     H*                  CHAIN TO A FILE BEFORE DOING A READ PRIOR.   *
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WA093/12385                            *
     H*                  WRITE RECORDS TO BILL TRANSACTION FILE.      *
     H*                                                               *
     H*  04/22/98  C:33  PROJECT: WC577                               *
     H*                  VALIDATE THE EFFECTIVE DATE AGAINST THE      *
     H*                  POLICY DATE RANGE.                           *
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
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N40 - ADDITIONAL CLASS CODE ENTRY                             *
     H*  85 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  94 - ROLL UP/DOWN PAST LAST RECORD                           *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  70 - Option Invalid                                          *
     H*  71 - (WCR0537) Flag not 'Y' or blank                         *
     H*  72 - (WCR0547) Amount < zero                                 *
     H*  73 - (WCR0547) # of Employees < zero                         *
     H*  74 - (WCR0547) Additional load < zero                        *
     H*  75 - (WCR0500) Cancel date invalid                           *
     H*  76 - (WC30007) Flag must be 'A' to enter audit information   *
     H*  80 - (WCR0500) Effective date invalid {new entry}            *
     H*  81 - (WCR0539) Class code invalid {new entry}                *
     H*  82 - (WCR0697) Class code already used {new entry}           *
     H*  83 - (WC30012) Spc Des not 1-9, 'S', 'F' or blank {new entry}*
     H*  84 - (WC30006) Flag must be 'A' {new entry}                  *
     H*  86 - (WCR0578) Effective date not between policy dates       *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*  87 - (SCAMT)  AMT=0 and claims exist.                        *
     H*  88 - (SCCLAS) Class code                                     *
     H*  89 - (SCSAFL) Audit flag                                     *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0660F2   CF   E             WORKSTN
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
     FWMEPP     UF A E             DISK
     F                                     RENAME(PEP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F***  BEGIN CHG  ***  04/20/09  *********************************
     F********WMEPL7  IF  E           K        DISK
     FWMEPL7E   IF   E           K DISK
     F***  END   CHG  ***  04/20/09  *********************************
     F                                     RENAME(PEP01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PPR02)
     F***  BEGIN ADD  ***  06/24/98  *********************************
     FWABLL     UF A E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMAHL     IF   E           K DISK
     F***  BEG   CHG  ***  04/20/09  *********************************
     F******* WMEPL   IF  E           K        DISK
     FWMEPLE    IF   E           K DISK
     F***  END   CHG  ***  04/20/09  *********************************
     F                                     RENAME(PEP01:PEP02)
     F***  END   ADD  ***  06/24/98  *********************************
     F***  BEGIN ADD  ***  02/09/99  *********************************
     F***  BEG   CHG  ***  04/20/09  *********************************
     F********WMEPL1  IF  E           K        DISK
     FWMEPL1E   IF   E           K DISK
     F***  END   CHG  ***  04/20/09  *********************************
     F                                     RENAME(PEP01:PEP03)
     F***  END   ADD  ***  02/09/99  *********************************
      ***** BEG ADD ******* 12/05/00 **********************************
      *
      * Claims Master
      *** BEG CHG ***** 10/14/03 ***************************************
     F************WMCML   IF  E           K        DISK
     FWMCMLZ    IF   E           K DISK
     FWMCXL     IF   E           K DISK
      *** END CHG ***** 10/14/03 ***************************************
      ***** END ADD ******* 12/05/00 **********************************
      *** BEG ADD ***** 01/03/05 **************************************
     FSWCPF     IF   E             DISK
      *** END ADD ***** 01/03/05 **************************************
      ***** BEGIN DEL ***** 02/01/06 **********************************
      ***** BEG ADD ******* 08/30/01 **********************************
      *******
      **Rating Set Code Master
     F*******WMRSL   IF  E           K        DISK
      *******
      **Statisical Reporting Rating
     F*******WDRRL1  IF  E           K        DISK
      ***** END ADD ******* 08/30/01 **********************************
      ***** END   DEL ***** 02/01/06 **********************************
     F*** BEGIN ADD ***  03/08/02  ************************************
     FWZEXL     IF   E           K DISK
     F*** END ADD *****  03/08/02  ************************************
      **** BEGIN CHG **** 06/01/06 ************************************
     FWMPXL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
      **** END   CHG **** 06/01/06 ************************************
     F***  BEGIN ADD  ***  04/20/09  *********************************
     FWMPXP     IF   E             DISK
     F                                     RENAME(PPX01:PPX02)
     F***  END   ADD  ***  04/20/09  *********************************
      *
      /COPY SCOPYWW03D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
      **** BEG ADD ****** 10/14/03 *********************************************
      **** END ADD ****** 10/14/03 *********************************************
      *** BEG ADD ***** 10/04/10 ***********************************************
     D NCL             S             19    DIM(999)                             NEW CLASS
      *** END ADD ***** 10/04/10 ***********************************************
     D*
     D*** BEGIN CHG ***  06/01/06  ************************************
     D*********/COPY SCOPYWW02E
     D*
     D*********/COPY SCOPYWW02I
     D***PY SCOPYWW03E
     D*
     D***PY SCOPYWW03I
     D*** END CHG *****  06/01/06  ************************************
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
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
     D*** BEG ADD ***** 10/14/03 **************************************
     D                 DS
     D  PAY                    1     90  2
     D                                     DIM(10)
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
     D*** END ADD ***** 10/14/03 **************************************
     D*** BEG ADD ***** 10/04/10 **************************************
     D                 DS
     D  XXKEY                  1     19
     D  SCEFDT                 1      8  0
     D  SCCLAS                 9     13  0
     D  SCSPEC                14     14
     D  SCEXT                 15     19
     D*** END ADD ***** 10/04/10 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Audit Information')
     D ACDPGM          C                   CONST('STDEFAULT')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D SF1PAG          C                   CONST(12)

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
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C                   PARM                    @@BILL            1
     C*
     C*  NOTE: @@BILL CONTROLS THE WRITING OF RECORDS TO THE MASS BILL
     C*        WORKFILE (Y/N)
     C***  END   ADD  ***  06/24/98  *********************************
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
      *** BEG ADD ***** 10/14/03 *************************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
      *** END ADD ***** 10/14/03 *************************************
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
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKSPEC
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   KFLD                    KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   KFLD                    KKEFDT
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
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   KFLD                    SCEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   KFLD                    XXEFDT
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C     EPKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C*  PAYROLL MASTER
     C*
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C***  BEG CHG ******  04/01/08  *********************************
     C************         KFLD           XXEFDT
     C                   KFLD                    X2EFDT            7 0
     C***  END CHG ******  04/01/08  *********************************
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C*** BEGIN ADD ***  06/01/06  ************************************
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCEXT
     C                   KFLD                    XXST              2
     C***  BEG CHG ******  04/01/08  *********************************
     C*************        KFLD           XXEFDT
     C                   KFLD                    X2EFDT
     C***  END CHG ******  04/01/08  *********************************
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCEXT
     C                   KFLD                    XXST
      *
      *  Rate Set Code Master
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*** END ADD *****  06/01/06  ************************************
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C*
     C*  BILLING WORK FILE
     C*
     C     BLKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C***  END   ADD  ***  06/24/98  *********************************
     C*** BEGIN ADD ***  03/08/02  ************************************
     C     ZEXKY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*** END ADD *****  03/08/02  ************************************
      ***** BEG ADD ******* 12/05/00 **********************************
      *
      *  CLAIM MASTER
      *
     C     CMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      ***** BEG ADD ******* 10/14/03 **********************************
     C                   KFLD                    SCCLAS
      *
     C     CXKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
      ***** END ADD ******* 10/14/03 **********************************
      *
      ***** END ADD ******* 12/05/00 **********************************
      ***** BEGIN DEL ***** 02/01/06 **********************************
      ***** BEG ADD ******* 08/30/01 **********************************
      ********
      ***Rate Set Code Master
      ********
     C********   RSKY      KLIST
     C********             KFLD           ELCO#
     C********             KFLD           ELRTCD
      ********
      ***Statisical Reporting Rating
      ********
     C********   SRKY      KLIST
     C********             KFLD           ELCO#
     C********             KFLD           ELFND
     C********             KFLD           RSJRST
      ********
      ***** END ADD ******* 08/30/01 **********************************
      ***** END   DEL ***** 02/01/06 **********************************
     C*
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EPCLAS        KKCLAS
     C     *LIKE         DEFINE    EPSPEC        KKSPEC
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    EPEXT         KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    EPEFDT        KKEFDT
     C     *LIKE         DEFINE    PREFDT        XXEFDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    SCEFDT        X1EFDT
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C     *LIKE         DEFINE    *IN60         XXEXST
     C     *LIKE         DEFINE    BLRFLG        XXRFLG
     C     *LIKE         DEFINE    @@BILL        XXBILL
     C***  END   ADD  ***  06/24/98  *********************************
     C*
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*********/COPY SCOPYWW02C
     C/COPY SCOPYWW03C
     C*** END CHG *****  06/01/06  ************************************
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C*===============================================================
     C* BILLNG - SUBROUTINE FOR WRITING/UPDATING BILL TRANSACTION FILE
     C*===============================================================
     C*
     C     BILLNG        BEGSR
     C*
     C*  WRITE TO BILLING WORK FILE
     C*
     C     BLKY          CHAIN     PBL01                              60
     C*
     C*  CLEAR FLAGS IF WRITING NEW RECORD
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        BLFLG1
     C                   MOVE      *BLANK        BLFLG2
     C                   MOVE      *BLANK        BLFLG3
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   Z-ADD     ELCO#         BLCO#
     C                   Z-ADD     ELFND         BLFND
     C                   Z-ADD     ELFYR         BLFYR
     C                   Z-ADD     ELEMP#        BLEMP#
     C                   Z-ADD     ELDIV         BLDIV
     C*
     C*  WRITE/UPDATE BILLING FLAGS IN WABLP WHEN PAYROLL IS AUDIT TYPE
     C*
     C                   MOVE      *BLANKS       BLFLG1
     C                   MOVE      *BLANKS       BLFLG3
     C                   MOVE      'Y'           BLFLG2
     C*
     C                   Z-ADD     ELPFDT        BLPFDT
     C                   MOVE      XXRFLG        BLRFLG
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C     *IN61         IFEQ      *OFF
     C     AHSTAT        ANDNE     'I'
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ELSE
     C                   Z-ADD     *ZERO         BLAGT#
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   MOVEL(P)  Q1USER        BLUSER
     C                   MOVEL(P)  Q1PGM         BLPGM
     C                   Z-ADD     DSTIME        BLTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
      *** BEG ADD ***** 03/27/11 *************************************
     C*
     C     ELNWRW        IFNE      ' '
     C                   MOVE      ELNWRW        BLRFLG
     C                   ENDIF
     C*
      *** END ADD ***** 03/27/11 *************************************
     C*
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    PBL01
     C                   ELSE
     C                   WRITE     PBL01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  BILLNG
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C*================================================================
     C* ERRCKA - Error check all subfile records (entire screen)
     C*================================================================
     C     ERRCKA        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCKA
     C*================================================================
     C* ERRCKC - Error check all control record fields
     C*================================================================
     C*
     C     ERRCKC        BEGSR
     C                   ENDSR                                                  ERRCKC
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********  SCCLAS    IFEQ SVCLAS
     C*********  SCCLAS    ANDNE*ZERO
     C*********            MOVE *ON       *IN40
     C*********            ENDIF                           SCCLAS=SVCLAS
      ***** END DEL ******* 04/24/01 **********************************
      ***** BEG ADD ******* 04/24/01 **********************************
      *
     C     SCCLAS        IFNE      *ZEROS
     C     SCCLAS        COMP      SVCLAS                                 40
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF                                                  SCCLAS=SVCLAS
      *
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C*  CHECK IF NEW ENTRY
     C*
     C  N90SCCLAS        IFNE      SVCLAS
     C*
     C*  VALIDATE EFFECTIVE DATE FOR NEW ENTRY
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C***  BEGIN ADD  ***  04/22/98  *********************************
     C  N90XXEFDT        IFLT      ELPFDT
      ***** BEG CHG ******* 07/19/00 **********************************
     C*********  XXEFDT    ORGT ELPTDT
     C     XXEFDT        ORGE      ELPTDT
      ***** END CHG ******* 07/19/00 **********************************
     C                   SETON                                        86  90
     C                   ENDIF                                                  ELPFDT>XXEFDT>ELPTDT
     C***  END   ADD  ***  04/22/98  *********************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   SCCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C*
     C*  VALIDATE CLASS CODE FOR NEW ENTRY
     C*
      ***** BEG DEL ******* 10/19/99 **********************************
     C**N90***** PRKY1     CHAINPPR01                60
      ***** BEG DEL ******* 10/19/99 **********************************
      ***** BEG ADD ******* 10/19/99 **********************************
     C  N90PRKY          SETGT     PPR01
     C  N90PRKY1         READPE    PPR01                                  60
      ***** BEG ADD ******* 10/19/99 **********************************
     C*
     C  N90*IN60         IFEQ      *ON
     C     PRSTAT        OREQ      'I'
     C                   SETON                                        81  90
     C                   ENDIF                                                  *IN60=*ON
     C*
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
     C                   MOVEL     PRDESC        SCDESC
     C*** BEGIN ADD ***  06/01/06  ************************************
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   SCCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        SCDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   SCCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        SCDESC
     C                   ENDIF                                                  EPEXT IFNE
     C*** END ADD *****  06/01/06  ************************************
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
      ***** BEG DEL ******* 08/30/01 **********************************
     C*********
     C***THE*SPECIAL DESIGNATION MUST BE 'F', 'S' OR BLANK OR '1'-'9'
     C***FOR*AIRCRAFT SURCHARGE
     C*********
     C**N90****  SCSPEC    IFNE *BLANK
     C*********
     C*********  SCCLAS    IFNE F2DCCD
     C*********
     C*********  SCSPEC    IFNE 'F'
     C*********  SCSPEC    ANDNE'S'
     C*********            SETON                     83  90
     C*********            ENDIF                           SCSPEC<>F/S
     C*********
     C*********            ELSE
     C*********
     C*********  SCSPEC    IFLT '1'
     C*********  SCSPEC    ORGT '9'
     C*********            SETON                     83  90
     C*********            ENDIF                           1>SCSPEC>9
     C*********
     C*********            ENDIF                           SCCLAS<>F2DCCD
     C*********
     C*********            ENDIF                           SCSPEC<>*BLANK
      ***** END DEL ******* 08/30/01 **********************************
      ***** BEGIN DEL ***** 02/01/06 **********************************
      ***** BEG ADD ******* 08/30/01 **********************************
      *********
      ***THE SPECIAL DESIGNATION MUST BE 'F', 'S' OR BLANK OR '1'-'9'
      ***FOR AIRCRAFT SURCHARGE
      *********
     C**N90      SCSPEC    IFNE *BLANK
     C*********  RSKY      CHAINPRS01                60
     C**N60      SRKY      CHAINPRR01                60
      *********
     C*********  *IN60     IFEQ *OFF
      *********
     C*********  SCCLAS    IFEQ RRDCCD
      *********
     C*********  SCSPEC    IFLT '1'
     C*********  SCSPEC    ORGT '9'
     C*********            SETON                     83  90
     C*********            ENDIF                           1>SCSPEC>9
      *********
     C*********            ELSE
      *********
     C*********  SCSPEC    IFNE 'F'
     C*********  SCSPEC    ANDNE'S'
     C*********            SETON                     29  90
     C*********            ENDIF                           SCSPEC<>F/S
      *********
     C*********            ENDIF                           SCCLAS<>RRDCCD
      *********
     C*********            ENDIF                           *IN60 = *OFF
      *********
     C*********            ENDIF                           SCSPEC<>*BLANK
      *********
      ***** END ADD ******* 08/30/01 **********************************
      ***** END   DEL ***** 02/01/06 **********************************
     C*
     C*  CHECK TO BE SURE CLASS CODE HAS NOT BEEN USED PREVIOUSLY
     C*
      ***** BEG CHG ******* 04/24/01 **********************************
B02  C***40N90** EPKY      CHAINDBRCDF               60
     C***40N90N60          SETON                     82  90
B02  C  N90EPKY          CHAIN     DBRCDF                             60
     C  N90
     CANN60              SETON                                        82  90
      ***** END CHG ******* 04/24/01 **********************************
     C*
     C***  BEGIN ADD  ***  02/09/99  *********************************
     C     *IN60         IFEQ      *ON
     C     EPKY          CHAIN(N)  PEP03                              60
     C  N60              SETON                                        77  90
     C                   ENDIF                                                  *IN60=*ON
     C***  END   ADD  ***  02/09/99  *********************************
     C*
     C*  FLAG MUST BE 'A' FOR NEW ENTRY
     C*
     C  N90SCSAFL        IFNE      'A'
     C                   SETON                                        84  90
     C                   ENDIF                                                  SCSAFL<>A
     C*
     C  N90ELSTAT        IFEQ      'Y'
     C                   EXFMT     WDWMSG
     C                   ENDIF                                                  ELSTAT<>Y
     C*
     C                   MOVE      *OFF          *IN40
     C*
      *** BEG ADD ***** 10/04/10 **************************************
     C                   Z-ADD     1             IX                3 0
     C     XXKEY         LOOKUP    NCL(IX)                                60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN82
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C     *BLANKS       LOOKUP    NCL(IX)                                60
     C                   MOVEA     XXKEY         NCL(IX)
     C                   ENDIF
     C                   ELSE
     C     SCCLAS        IFNE      *ZERO
     C                   CLEAR                   NCL
     C                   ENDIF                                                  SCCLAS<>0
      *** END ADD ***** 10/04/10 **************************************
     C                   ENDIF                                                  SCCLAS<>SVCLAS
     C*
     C*  FLAG MUST BE 'A' OR BLANK
     C*
     C  N90SCSAFL        IFNE      *BLANKS
     C     SCSAFL        ANDNE     'A'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCSAFL<>*BLANKS/A
     C*
     C*  AMOUNT CANNOT BE LESS THAN ZERO
     C*
     C  N90SCAMT         IFLT      *ZERO
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCEAPR<*ZERO
     C*
     C*  # OF EMPLOYEES CANNOT BE LESS THAN ZERO
     C*
     C  N90SC#EMP        IFLT      *ZERO
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCEAPR<*ZERO
     C*
     C*  ADDITIONAL LOAD CANNOT BE LESS THAN ZERO
     C*
     C  N90SCADLD        IFLT      *ZERO
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCEAPR<*ZERO
     C*
     C*  IF AUDIT INFORMATION IS ENTERED, THE FLAG MUST BE 'A'
     C*
     C  N90SCSAFL        IFNE      'A'
     C*
     C     SCAMT         IFNE      *ZERO
     C     SC#EMP        ORNE      *ZERO
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCAMT<>*ZERO<>SCACNT
     C*
     C                   ENDIF                                                  SCSAFL<>A
      ***** BEG ADD ******* 12/05/00 **********************************
      *
      *  Display override window if amt=0 and claims with payments exist and
      *  record hasn't been audited yet. Bypass this edit if client is 'admin
      *  only'
      *
      *** BEG ADD ****** 01/03/05 **********************************************
     C  N90ADMFLG        IFNE      'Y'                                          NOT ADMIN ONLY
      *** END ADD ****** 01/03/05 **********************************************
      *** BEG ADD *** 04/17/07 *************************************************
     C     CLTID         OREQ      '093'
      *** END ADD *** 04/17/07 *************************************************
      *
      *** BEG ADD ****** 10/14/03 **********************************************
     C  N90SCDBRR        CHAIN(N)  PFRCDF                             99
      *** END ADD ****** 10/14/03 **********************************************
      *** BEG ADD ****** 01/06/04 **********************************************
     C     EPSAFL        IFNE      'A'
     C     SCAMT         OREQ      *ZERO
     C     EPAAPR        ANDNE     *ZERO
      *** END ADD ****** 01/06/04 **********************************************
      *
     C  N90SCAMT         IFEQ      *ZERO
     C     SCSAFL        ANDEQ     'A'
      *** BEG ADD ****** 10/14/03 **********************************************
      *** BEG DEL ****** 01/06/04 **********************************************
     C*********  SCAMT     ANDNEEPAAPR
      *** END DEL ****** 01/06/04 **********************************************
      *** END ADD ****** 10/14/03 **********************************************
      *
     C     CMKY          CHAIN     PCM01                              60
      *** BEG ADD ****** 01/06/04 **********************************************
     C     *IN60         DOWEQ     *OFF
      *
     C                   XFOOT     PAY           XXSUM            13 2
     C     XXSUM         IFGT      *ZERO
      *** BEG ADD *** 04/17/07 *************************************************
     C     CLTID         OREQ      '093'
      *** END ADD *** 04/17/07 *************************************************
     C                   SETON                                        89
     C                   MOVE      *BLANKS       XBAUTH            1
     C                   LEAVE
     C                   ENDIF
      *
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
      *** END ADD ****** 01/06/04 **********************************************
      *** BEG ADD *** 04/17/07 *************************************************
      * Display Different window message for client = 93
     C     CLTID         IFEQ      '093'
     C                   MOVE      *ON           *IN51
     C                   ELSE
     C                   MOVE      *OFF          *IN51
     C                   ENDIF                                                  CLTID = 93
      *** END ADD *** 04/17/07 *************************************************
      *** BEG CHG ****** 10/14/03 **********************************************
      *********
     C*********  *IN60     DOWEQ*OFF
      *********
     C*********  SCCLAS    IFEQ CMCLAS
     C*********            EXFMTWDW02
     C***KL****            SETON                     90  87
     C***KL****            SETOF                     KL
     C*********            LEAVE
     C*********            ENDIF                           SCCLAS = CMCLAS
      *********
     C*********  CMKY      READEPCM01                    60
     C*********            ENDDO
      *********
     C*********            ENDIF                           SCAMT = 0
      *
     C     *IN60         IFEQ      *OFF
      *** BEG DEL ******01/06/044 **********************************************
     C*********            XFOOTPAY       XXSUM  132
      *********
     C*********  XXSUM     IFGT *ZERO
     C*********            SETON                     89
     C*********            MOVE *BLANKS   XBAUTH  1
      *** END DEL ******01/06/044 **********************************************
      *
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ##LVL         SRULVL
     C                   MOVE      CXAUSL        SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXAUT1        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM                    SRRES             1
     C                   PARM      CXAUT2        SRCTP2            1
     C                   PARM      CXAUT3        SRCTP3            1
     C                   PARM      CXAUT4        SRCTP4            1
     C                   PARM      CXAUT5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
      *** BEG ADD ****** 01/06/04 **********************************************
     C                   ELSE
     C                   MOVE      '1'           SRRES
     C                   ENDIF
      *** END ADD ****** 01/06/04 **********************************************
      *
      *         SRRES = '0' if "Passed" Security Check
      *                 '1' if "Did Not Pass" Security Check
      *
     C     SRRES         IFEQ      '0'
     C                   MOVE      'Y'           XBAUTH                         Authorized to
     C                   ENDIF                                                   overide edit
      *
     C     XBAUTH        IFEQ      'Y'                                          IF AUTH
     C                   MOVE      'N'           SCEOVR
     C                   EXFMT     WDW02
      *
     C     SCEOVR        IFEQ      'Y'
     C                   SETOFF                                       89        OK
     C                   ELSE
     C                   SETON                                        90
     C                   ENDIF                                                  SCEOVR
      *
     C                   ELSE                                                   NOT AUTHORIZED
     C                   SETON                                        31  90
     C                   MOVE      'N'           SCEOVR
     C                   EXFMT     WDW02
     C                   SETOFF                                       31
     C                   ENDIF                                                  XBAUTH = 'Y'
      *
      *** BEG DEL ****** 01/06/04 **********************************************
     C******************   ENDIF                           *IN60 = *OFF
     C******************   ENDIF                           XXSUM <> 0
      *** END DEL ****** 01/06/04 **********************************************
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDIF                                                  SCAMT = 0
      *** END CHG ****** 10/14/03 **********************************************
      *
      *** BEG ADD ****** 01/06/04 **********************************************
     C                   ENDIF                                                  EPSAFL <> 'A'
      *** END ADD ****** 01/06/04 **********************************************
      *
      *** BEG ADD ****** 01/03/05 **********************************************
     C                   ENDIF                                                  ADMFLG <> 'Y'
      *** END ADD ****** 01/03/05 **********************************************
      ***** END ADD ******* 12/05/00 **********************************
     C*
     C*  VALIDATE CANCEL DATE
     C*
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCXDT
     C*
     C  N90SRERR         IFEQ      *ON
     C     SCCXDT        ANDNE     *ZERO
     C                   SETON                                        75  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SCDBRR        CHAIN(N)  PFRCDF                             99
     C*
     C  N90EPDT          IFGT      SAVEDT
     C     EPDT          OREQ      SAVEDT
     C     EPTIME        ANDGT     SAVETM
     C                   SETON                                        85  90
     C                   ENDIF                                                  EPDT>SAVEDT
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCCLAS
     C                   Z-ADD     SCCLAS        KKCLAS
     C                   MOVEL     *BLANKS       KKSPEC
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVEL     *BLANKS       KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   Z-ADD     *ZEROS        KKEFDT
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        KKCLAS
     C                   MOVEL     *BLANKS       KKSPEC
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVEL     *BLANKS       KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   Z-ADD     *ZEROS        KKEFDT
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCCLAS        KKCLAS
     C                   MOVEL     SCSPEC        KKSPEC
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVEL     SCSPEC        KKSPEC
     C***  END   ADD  ***  04/20/09  *********************************
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********  *IN02     IFEQ *ON
     C*********  *IN04     ANDEQ*ON
     C*********  1         CHAINSCR01S               99
     C*********            SETON                     0840
     C*********            UPDATSCR01S
     C*********            ENDIF                           *IN02=*ON=*IN04
      ***** END DEL ******* 04/24/01 **********************************
      ***** BEG ADD ******* 04/24/01 **********************************
      *
      * Set Indicator for Subfile Display
      *
     C     SCCLAS        IFNE      *ZEROS
     C     SCCLAS        COMP      SVCLAS                                 40
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF                                                  SCCLAS<>*ZEROS
      *
      ***** END ADD ******* 04/24/01 **********************************
     C*
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
     C     SCCLAS        COMP      SVCLAS                                 40
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF                                                  SCCLAS<>*ZEROS
      *
     C     *IN40         IFEQ      *OFF
     C     HFLD          ANDNE     'SCPST1'
     C     HFLD          ANDNE     *BLANKS
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCLAS'
     C     HFLD          OREQ      'SCSPEC'
     C*
     C     HRRN          CHAIN     SCR01S                             60
     C*
     C                   CALL      'W0630R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
      **** BEGIN CHG **** 04/06/01 *********************************************
     C******************** PARM           @@RRN   90
     C                   PARM      0             @@RRN             9 0
      **** END   CHG **** 04/06/01 *********************************************
     C                   PARM      ELRTCD        @@RTCD            3
     C                   PARM      2             @@VIEW            1 0
     C*
     C                   MOVE      *ON           *IN08
     C*
     C     @@RRN         IFEQ      *ZEROS
     C*
     C                   MOVE      *ON           *IN88
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     PPR02                              99
     C*
     C                   MOVE      PRCLAS        SCCLAS
     C*
      *** BEG CHG ***** 10/14/03 **********************************************
     C*******************  MOVE *ON       *IN89
     C                   MOVE      *ON           *IN30
      *** BEG CHG ***** 10/14/03 **********************************************
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
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   SCCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C  N60              MOVEL(P)  PRDESC        SCDESC
     C  N60              Z-ADD     PRRT          SCRATE
     C*** BEGIN ADD ***  06/01/06  ************************************
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   SCCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        SCDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   SCCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X2EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        SCDESC
     C                   ENDIF                                                  EPEXT IFNE
     C*** END ADD *****  06/01/06  ************************************
     C*
     C                   ENDIF                                                  SCCLAS=*ZEROS
     C*** BEGIN ADD ***  04/20/09  ************************************
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
     C                   PARM                    ##ST              2
      *
     C                   MOVE      *ON           *IN08
      *
     C     ##RRN         IFEQ      *ZEROS
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PPX02                              99
      *
     C                   MOVE      PXEXT         SCEXT
     C****                 MOVE PXEXT     SVEXT
      *
      *
     C                   MOVEL(P)  PXDESC        SCDESC
     C                   ENDIF                                                  ##RRN IFNE
     C*** END ADD *****  04/20/09  ************************************
     C*
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      *OFF          *IN08
     C*
     C                   ENDSL
     C*
     C                   Z-ADD     HRRN          SCRRN1
     C*
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   ENDIF                                                  *IN40 = *OFF
      ***** END ADD ******* 04/24/01 **********************************
      *
     C                   ENDSR                                                  KJDO
     C*
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCR01S
     C*
     C                   Z-ADD     DBRRN         SCDBRR
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
      ***** BEG ADD ******* 12/05/00 **********************************
     C                   Z-ADD     EPCO#         SCCO#
     C                   Z-ADD     EPFND         SCFND
     C                   Z-ADD     EPEMP#        SCEMP#
     C                   Z-ADD     EPDIV         SCDIV
     C                   Z-ADD     EPFYR         SCFYR
      ***** END ADD ******* 12/05/00 **********************************
     C*
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   Z-ADD     EPCLAS        SVCLAS
     C                   MOVEL     EPSPEC        SCSPEC
      ***** BEGIN ADD ***** 06/01/06 **********************************
     C                   MOVEL     EPEXT         SCEXT
      ***** END ADD ******* 06/01/06 **********************************
     C*
     C     EPSAFL        IFEQ      'A'
     C                   MOVEL     EPSAFL        SCSAFL
     C                   ENDIF                                                  EPSAFL=A
     C*
     C                   Z-ADD     EPAAPR        SCAMT
     C                   Z-ADD     EPACNT        SC#EMP
     C                   Z-ADD     EPADLD        SCADLD
     C*
     C                   Z-ADD     EPCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCADT
     C*
     C                   MOVE      *ON           *IN08
     C                   MOVE      *ON           *IN40
      *** BEG ADD ***** 04/11/05 *************************************
     C                   MOVE      *ON           XXWRIT            1
      *** END ADD ***** 04/11/05 *************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPEFDT
     C*
     C                   Z-ADD     SCCLAS        EPCLAS
     C                   MOVEL     SCSPEC        EPSPEC
      ***** BEGIN ADD ***** 06/01/06 **********************************
     C                   MOVEL     SCEXT         EPEXT
      ***** END ADD ******* 06/01/06 **********************************
     C*
     C***  BEGIN CHG  ***  06/05/99  *********************************
     C*****BEGIN DEL  ***  02/10/99  *********************************
     C********** SCSAFL    IFEQ 'A'
     C*****END** DEL  ***  02/10/99  *********************************
     C**********           MOVELSCSAFL    EPSAFL
     C*****BEGIN DEL  ***  02/10/99  *********************************
     C**********           ENDIF                           SCSAFL=A
     C*****END** DEL  ***  02/10/99  *********************************
     C     SCSAFL        IFEQ      'A'
     C                   MOVEL     SCSAFL        EPSAFL
     C                   ELSE
     C                   MOVEL     'E'           EPSAFL                         Per C:35
      *** BEG ADD ***** 04/11/05 *************************************
     C                   MOVE      *OFF          XXWRIT            1
      *** END ADD ***** 04/11/05 *************************************
     C                   ENDIF                                                  SCSAFL=A
     C***  END   CHG  ***  06/05/99  *********************************
     C*
     C                   Z-ADD     SCAMT         EPAAPR
     C                   Z-ADD     SC#EMP        EPACNT
     C                   Z-ADD     SCADLD        EPADLD
     C*
     C                   Z-ADD     SCCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPCADT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      *** BEG ADD ***** 04/11/05 *************************************
     C     XXBILL        IFEQ      'Y'
     C     XXWRIT        ANDEQ     *ON
     C                   EXSR      BILLNG
     C                   ENDIF
      *** END ADD ***** 04/11/05 *************************************
     C*
     C                   MOVEA     '00000000'    *IN(21)
     C                   EXSR      CLRSF1
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C     Q1PRMS        IFGT      5
     C                   MOVE      @@BILL        XXBILL
     C                   ELSE
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF                                                  Q1PRMS>5
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C     @@RRN         CHAIN     DBRCDF2                            99
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C*  SET FLAG IF PAYROLL HAS EXISTED BEFORE FOR YR
     C*
     C                   MOVE      *OFF          XXEXST
     C     EPKY1         CHAIN     PEP02                              60
     C  N60              MOVE      *ON           XXEXST
     C*
     C*  SET RENEWAL FLAG
     C*
     C*** BEGIN ADD ***  12/11/98  ************************************
     C     AHKY          CHAIN     PEL02                              60
     C*** END ADD *****  12/11/98  ************************************
     C     ELKY          READPE    PEL02                                  60
     C*** BEGIN ADD ***  03/08/02  ************************************
     C   60ZEXKY         CHAIN     PEX01                              60
     C*** END ADD *****  03/08/02  ************************************
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'R'           XXRFLG
     C                   ELSE
     C                   MOVE      'N'           XXRFLG
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     @@RRN         CHAIN     DBRCDF2                            99
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C                   Z-ADD     ELFYR         SCFYR
      ***** BEG ADD ******* 04/24/01 **********************************
     C                   Z-ADD     ELFND         SC1FND
     C                   Z-ADD     ELFYR         SC1FYR
      ***** END ADD ******* 04/24/01 **********************************
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
     C*** BEG ADD **** 01/03/05 **********************************
     C                   READ      SWCPF                                  60
     C     WCFL08        IFEQ      'A'
     C                   MOVE      'Y'           ADMFLG            1            ADMIN ONLY FLAG
     C                   ELSE
     C                   MOVE      *BLANK        ADMFLG
     C                   ENDIF
     C*** END ADD **** 01/03/05 **********************************
     C*
     C*** BEG ADD *** 04/17/07 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 04/17/07 **********************************
      *
     C*** BEG ADD ***** 10/04/10 ********************************
     C                   CLEAR                   NCL
     C*** END ADD ***** 10/04/10 ********************************
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********            MOVE *ON       *IN40
      ***** END DEL ******* 04/24/01 **********************************
     C*
     C     KEY1          READE     DBRCDF                                 95
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********  *IN95     IFEQ *ON
     C*********            MOVE *OFF      *IN40
     C*********            ENDIF                           *IN95=*ON
      ***** END DEL ******* 04/24/01 **********************************
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********            MOVE *ON       *IN40
      ***** END DEL ******* 04/24/01 **********************************
     C*
     C     KEY1          READPE    DBRCDF                                 95
     C*
      ***** BEG DEL ******* 04/24/01 **********************************
     C*********  *IN95     IFEQ *ON
     C*********            MOVE *OFF      *IN40
     C*********            ENDIF                           *IN95=*ON
      ***** END DEL ******* 04/24/01 **********************************
     C*
     C                   ENDSR                                                  READP
     C*
     C*
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C     KEY1A         SETLL     DBRCDF                             96
     C*
     C     *IN96         IFEQ      *ON
     C                   Z-ADD     *ZEROS        DBRRN
     C                   ENDIF                                                  *IN96=*ON
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     DSTIME        EPTIME
     C*
     C                   UPDATE    PFRCDF
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C*** BEG DEL ***** 04/11/05 **************************************
     C********** XXBILL    IFEQ 'Y'
     C**********           EXSR BILLNG
     C**********           ENDIF                           XXBILL=Y
     C*** END DEL ***** 04/11/05 **************************************
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C     SCCLAS        IFNE      *ZEROS
     C*
     C                   CLEAR                   PFRCDF
     C*
     C                   Z-ADD     ELCO#         EPCO#
     C                   Z-ADD     ELFND         EPFND
     C                   Z-ADD     ELEMP#        EPEMP#
     C                   Z-ADD     ELDIV         EPDIV
     C                   Z-ADD     ELFYR         EPFYR
     C                   Z-ADD     SCRATE        EPARAT
     C                   MOVEL     'A'           EPSTAT
     C*
     C                   EXSR      MVSCDB
     C*
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
     C                   WRITE     PFRCDF
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C*** BEG DEL ***** 04/11/05 **************************************
     C********** XXBILL    IFEQ 'Y'
     C**********           EXSR BILLNG
     C**********           ENDIF                           XXBILL=Y
     C*** END DEL ***** 04/11/05 **************************************
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C                   ENDIF                                                  SCCLAS<>*ZEROS
     C*
     C                   FEOD      WMEPP
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
