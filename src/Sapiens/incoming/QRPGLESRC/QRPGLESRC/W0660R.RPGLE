     H copyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0660R - WORK WITH S/R PAYROLLS
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL S/R    *
     H*                PAYROLLS FOR A SELECTED EMPLOYERS POLICY YEAR. *
     H*                EACH PAYROLL CAN BE SELECTED FOR DELETION OR   *
     H*                ADDING TO A SPECIFIC MONTH.                    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0060R - WORK WITH PAYROLLS                                *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0660R02 - S/R PAYROLL INFORMATION (OPT 4/6)               *
     H*    W0660R1  - S/R PAYROLL CALCULATION (OPT 15)                *
     H*                                                               *
     H*  CREATED: 05/02/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/15/15 C:308 Support 32186   Below changes removed          *
     H* 10/21/15 C:308 Support 32186                                  *
     H*                Self Reporting Entry not allow for Future Month*
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                ADD COLUMN TO DISPLAY THE "EXTENSION"          *
     H*                FIELD WASRP/SREXT.                             *
     H*                                                               *
     H* 07/29/08 C:183 PROJECT 46WFV                                  *
     H*                Add indicator in reverse image on S/R entry    *
     H*                screen if owner officer set to include         *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 11/15/05 C:61  SUPPORT WA109 49329                            *
     H*                Use Employer Rate Code when getting            *
     H*                S/R rate code description.                     *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify display if vendor processing is         *
     H*                enabled to include report frequency and        *
     H*                report date for weekly frequency reports.      *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*              - Add edit "F15=S/R Payroll calculation",        *
     H*                reporting month/year must be in policy year    *
     H*                effective date range.                          *
     H*                                                               *
     H* 08/11/99 C:61  PROJECT WC577                                  *
     H*                Add number of employer divisions to header.    *
     H*                                                               *
     H* 01/29/99  C:33  SEQ#: WA081/15499                             *
     H*                 IF CALLED FROM REVISED BILLING, DO NOT ALLOW  *
     H*                 ANY TYPE OF CHANGES TO THE FILE.              *
     H*                                                               *
     H* 01/25/99  C:33  SEQ#: WA081/16161                             *
     H*                 USE A DIFFERENT LOGICAL THAT HAS THE SPECIAL  *
     H*                 DESIGNATION CODE IN IT.                       *
     H*                                                               *
     H* 08/11/98 C:65 Project: WC843                                  *
     H*               USE EXTENSION DESCRIPTION IF SPECIFIED.         *
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
     H*****************************************************************
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
     H*  31 - Called as Quote                                         *
     H*  40 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0660F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWASRP     IF   E             DISK
     F                                     RENAME(PSR01:PFRCDF)
     F***  BEGIN CHG  ***  01/25/99  *********************************
     F**********WASRL3  IF  E           K        DISK
     F***  BEGIN CHG  ***  04/20/09  *********************************
     FWASRL6E   IF   E           K DISK
     F*******************WASRL6  IF  E           K        DISK
     F***  END   CHG  ***  04/20/09  *********************************
     F***  END   CHG  ***  01/25/99  *********************************
     F                                     RENAME(PSR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      ***  BEGIN CHG  ***  11/15/05  *********************************
     F*******WMPRL   IF  E           K        DISK
     FWMPRL1    IF   E           K DISK
     F***  END   CHG  ***  11/15/05  *********************************
     FWDELP     IF   E             DISK
     F*** BEGIN ADD ***  08/11/98  ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     F******WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 10/06/99 ************************************
     F*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     FWMRSL     IF   E           K DISK
      **** END   ADD **** 10/06/99 ************************************
      **** BEG   ADD **** 02/18/03 *********************************************
     FWASRL8    IF   E           K DISK
     F                                     RENAME(PSR01:DBRCDF2)
     F                                     INFDS(DBFDS2)
     FSSYLF     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
      **** END   ADD **** 02/18/03 *********************************************
      **** BEGIN ADD **** 07/29/08 ************************************
     FWMOOL     IF   E           K DISK
      **** END   ADD **** 07/29/08 ************************************
      /COPY SCOPYWW01D
     D*
      *** BEG CHG ******* 02/18/03 *********************************************
     D*****************   HD      1   1 78               HEADERS
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              HEADERS
      *** END CHG ******* 02/18/03 *********************************************
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D OPTNM           C                   CONST('Employer S/R Payroll-
     D                                     s')
     D DELETD          C                   CONST('* * Deleted * *')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D ACDPGM          C                   CONST('W0660R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
      ***** BEG CHG ******* 08/11/99 **********************************
     D************  9                     C         SF1PAG
     D SF1PAG          C                   CONST(7)
      ***** END CHG ******* 08/11/99 **********************************
      *** BEG CHG ******* 02/18/03 *********************************************
     D************* 1                     C         VIEWMX
     D VIEWMX          C                   CONST(2)
      *** END CHG ******* 02/18/03 *********************************************
     D INFOMX          C                   CONST(1)
      **** BEGIN ADD **** 02/12/98 *********************************************
      *** BEG ADD ******* 02/18/03 *********************************************
     D CHGNUM          C                   CONST('WA003R')
      *** END ADD ******* 02/18/03 *********************************************
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

      ***** Beg Add *** 12/15/15  *********************************
      ***** Beg Add *** 10/21/15  *********************************
     D*intSysdat       s              8P 0
     D*intDate         s              6P 0
     D*strMsgId        s              7    inz(*Blank)
      ***** End Add *** 10/21/15  *********************************
      ***** End Add *** 12/15/15  *********************************

     C*
     C***  BEGIN ADD  ***  01/29/99  *********************************
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C***  END   ADD  ***  01/29/99  *********************************
     C     *LIKE         DEFINE    SRRPMY        KKRPMY
     C     *LIKE         DEFINE    SRCLAS        KKCLAS
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C     *LIKE         DEFINE    SRSPEC        KKSPEC
     C***  END   ADD  ***  01/25/99  *********************************
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    SREXT         KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    SREFDT        KKEFDT
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    KPRPMY        ##RPMY
      **** BEGIN ADD **** 10/06/99 ************************************
     C     *LIKE         DEFINE    PXST          XXST
      **** END   ADD **** 10/06/99 ************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      ***** BEG ADD ******* 09/16/99 **********************************
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ***** END ADD ******* 09/16/99 **********************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'P'           ##CALL            1
     C                   PARM      'A'           ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
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
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
      *** BEG ADD ********* 02/18/03 **********************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *** END ADD ********* 02/18/03 **********************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      ***** BEG ADD ******* 08/11/99 **********************************
      *
     C     EMKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      *
      ***** END ADD ******* 08/11/99 **********************************
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  PAYROLL MASTER
     C*
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
      ***  BEGIN ADD  ***  11/15/05  *********************************
     C                   KFLD                    ELRTCD
      ***  END   ADD  ***  11/15/05  *********************************
     C                   KFLD                    SRCLAS
     C                   KFLD                    SREFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
      ***  BEGIN ADD  ***  11/15/05  *********************************
     C                   KFLD                    ELRTCD
      ***  END   ADD  ***  11/15/05  *********************************
     C                   KFLD                    SRCLAS
     C*
     C*  REPOSITION KEYS
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
     C                   KFLD                    KKCLAS
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   KFLD                    KKSPEC
     C***  END   ADD  ***  01/25/99  *********************************
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   KFLD                    KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   KFLD                    KKEFDT
     C*** BEGIN ADD ***  08/11/98  ************************************
      *
     C*** BEG ADD *****  02/18/03  ************************************
     C     KEY1B         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKTODT            7 0
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKSPEC
     C                   KFLD                    KKEFDT
     C*** END ADD *****  02/18/03  ************************************
     C*
     C*  PAYROLL DESCRIPTION EXTENSION FILE
     C*
     C     PXKY          KLIST
     C                   KFLD                    SRCO#
     C                   KFLD                    SRCLAS
     C                   KFLD                    SREXT
     C*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   KFLD                    XXST
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    SREFDT
     C     PXKY1         KLIST
     C                   KFLD                    SRCO#
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
      *
      **** BEG   ADD **** 02/18/03 *********************************************
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY            10
      **** END   ADD **** 02/18/03 *********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *HIVAL        KKRPMY
     C***  BEG ADD ******  02/18/03  *********************************
     C                   MOVE      *HIVAL        KKTODT
     C***  END ADD ******  02/18/03  *********************************
     C                   MOVE      *LOVAL        KKCLAS
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   MOVE      *LOVAL        KKSPEC
     C***  END   ADD  ***  01/25/99  *********************************
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVE      *LOVAL        KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   MOVE      *LOVAL        KKEFDT
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         KKRPMY
     C                   MOVE      *LOVAL        KKCLAS
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   MOVE      *LOVAL        KKSPEC
     C***  END   ADD  ***  01/25/99  *********************************
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVE      *LOVAL        KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   MOVE      *LOVAL        KKEFDT
     C*
     C***  BEG ADD ******  02/18/03  *********************************
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTODT
     C                   MOVE      *LOVAL        KKCLAS
     C                   MOVE      *LOVAL        KKSPEC
     C                   MOVE      *LOVAL        KKEFDT
     C***  END ADD ******  02/18/03  *********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C     ##MODE        IFNE      'A'
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         KKRPMY
     C***  BEG ADD ******  02/18/03  *********************************
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTODT
     C***  END ADD ******  02/18/03  *********************************
     C                   Z-ADD     SCCLAS        KKCLAS
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   MOVE      SCSPEC        KKSPEC
     C***  END   ADD  ***  01/25/99  *********************************
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVE      SCEXT         KKEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C                   Z-ADD     SCDT          KKEFDT
     C                   ELSE
     C                   MOVE      *HIVAL        KKRPMY
     C***  BEG ADD ******  02/18/03  *********************************
     C                   MOVE      *HIVAL        KKTODT
     C***  END ADD ******  02/18/03  *********************************
     C                   MOVE      *LOVAL        KKCLAS
     C***  BEGIN ADD  ***  01/25/99  *********************************
     C                   MOVE      *LOVAL        KKSPEC
     C***  END   ADD  ***  01/25/99  *********************************
     C***  BEGIN ADD  ***  04/20/09  *********************************
     C                   MOVE      *LOVAL        KKSPEC
     C***  END   ADD  ***  04/20/09  *********************************
     C                   MOVE      *LOVAL        KKEFDT
     C                   CLEAR                   ##MODE
     C                   ENDIF                                                  ##MODE<>A
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* KPDO - SUBROUTINE FOR F15
     C*===============================================================
     C*  F15=S/R PAYROLL CALC
     C*
     C     KPDO          BEGSR
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C                   EXFMT     WDWRPMY
     C*
      *** BEG ADD **** 02/18/03 ******************************************
     C     *IN42         IFEQ      *OFF
      *** END ADD **** 02/18/03 ******************************************
     C*
     C*  CHECK REPORTING FOR VALID MONTH/YEAR
     C*
     C                   Z-ADD     KPRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         KPRPMY
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        30  90
     C                   ENDIF                                                  SRERR=*ON
      ***** BEG ADD ******* 09/16/99 **********************************
      *
      *  Date entered must be within employer policy effective date range
      *
     C     KPRPMY        MULT      100.0001      X1DT              6 0
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1PFDT            6 0
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1PTDT            6 0
      *
      ****** Beg add ******* 12/15/15 ***********************************
      ****** Beg add ******* 10/21/15 ***********************************
     C*                  Eval      intSysDat = %Dec(%Date())
     C*                  Movel     intSysDat     intDate
      ****** End add ******* 10/21/15 ***********************************
      ****** End add ******* 12/15/15 ***********************************
      *
     C     X1DT          IFLT      X1PFDT
     C     X1DT          ORGT      X1PTDT
      ****** Beg add ******* 12/15/15 ***********************************
      ****** Beg add ******* 10/21/15 ***********************************
     C*                  Move      'WC31315'     strMsgId
      ****** End add ******* 10/21/15 ***********************************
      ****** End add ******* 12/15/15 ***********************************
     C                   SETON                                        9071
      ****** Beg add ******* 12/15/15 ***********************************
      ****** Beg add ******* 10/21/15 ***********************************
     C*                  ElseIf    X1DT > intDate
     C*                  Move      'WC40162'     strMsgId
     C*                  SETON                                        9071
      ****** End add ******* 10/21/15 ***********************************
      ****** End add ******* 12/15/15 ***********************************
     C                   ENDIF                                                  X1DT
      *
      ***** END ADD ******* 09/16/99 **********************************
      *** BEG ADD **** 02/18/03 ******************************************
     C                   ELSE
     C                   Z-ADD     KPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         KPTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        30  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   Z-ADD     KPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
      *
     C     XXTODT        IFLT      ELPFDT
     C     XXTODT        ORGT      ELPTDT
      ****** Beg add ******* 12/15/15 ***********************************
      ****** Beg add ******* 10/21/15 ***********************************
     C*                  Move      'WC31315'     strMsgId
      ****** End add ******* 10/21/15 ***********************************
      ****** End add ******* 12/15/15 ***********************************
     C                   SETON                                        71  90
     C                   ENDIF                                                  X1TODT
      *
     C                   ENDIF                                                  *IN43=*OFF
      *** END ADD **** 02/18/03 ******************************************
     C*
     C                   ENDDO                                                  *IN90=*OFF/*INKL=*ON
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   CALL      'W0660R1'                            99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C                   PARM      KPRPMY        ##RPMY
      *** BEG ADD **** 02/18/03 ******************************************
     C                   PARM      KPTODT        ##TODT            8 0
      *** END ADD **** 02/18/03 ******************************************
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDSR                                                  KPDO
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
      *** BEG ADD ****** 02/18/03 ************************************
     C     *IN42         IFEQ      *OFF
      *** END ADD ****** 02/18/03 ************************************
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
      *** BEG ADD ****** 02/18/03 ************************************
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'RPMY'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCDATE
      *
     C                   ELSE
     C                   Z-ADD     SRTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCDATE
      *
     C                   ENDIF
      *** END ADD ****** 02/18/03 ************************************
     C*
     C                   Z-ADD     SRAMTP        SCAMT
     C                   Z-ADD     SREFDT        SCDT
     C*
     C                   Z-ADD     SREFDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     SRCLAS        SCCLAS
     C                   MOVEL     SRSPEC        SCSPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVEL     SREXT         SCEXT
     C*** END   ADD ***  04/20/09  ************************************
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C  N60              MOVEL     PRDESC        SCDESC
     C*** BEGIN ADD ***  08/11/98  ************************************
     C     SREXT         IFNE      *BLANKS
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 10/06/99 ************************************
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'A'
     C                   MOVEL(P)  PXDESC        SCDESC
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   ELSE
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
     CANN60              MOVEL(P)  PXDESC        SCDESC
      **** END   ADD **** 10/06/99 ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  SREXT IFNE
     C*** END ADD *****  08/11/98  ************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6
     C*===============================================================
     C*
     C* OPTION 6=ADD S/R PAYROLL INFORMATION
     C*
     C     OPT6          BEGSR
     C*
     C                   CALL      ACDPGM        ACDPL1                 99
     C*
     C  NKC              MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT6
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKP         CASEQ     *ON           KPDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C                   MOVE      ##CO#         ##CO#
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C  N09              MOVE      ' '           ##CALL            1
     C   09              MOVE      '1'           ##CALL
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C***  BEGIN ADD  ***  01/29/99  *********************************
     C     SVCALL        IFEQ      'R'
     C                   MOVE      *ON           *INLR
     C                   ENDIF                                                  SVCALL=R
     C***  END   ADD  ***  01/29/99  *********************************
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  01/29/99  *********************************
     C                   MOVE      @@CALL        SVCALL
     C*
     C     @@CALL        IFEQ      'R'
     C*
     C                   MOVE      ' '           @@CALL
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '15'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF                                                  @@CALL=R
     C***  END   ADD  ***  01/29/99  *********************************
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    09    SEARCH
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN2        CHAIN     PEL01                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
      ***** BEG ADD ******* 08/11/99 **********************************
      *
      *   Get Number of Employer Divisions to display in Screen Header
      *
     C     EMKY2         CHAIN     PEM01                              60
     C                   Z-ADD     0             SC#DIV
      *
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             SC#DIV
     C     EMKY2         READE     PEM01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 08/11/99 **********************************
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
      *** BEG ADD ***** 07/29/08 **************************************
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      *OFF          *IN44
      *
     C     CLTID         IFEQ      '046'
      *
     C     EMKY          SETLL     WMOOL                                  60
     C     EMKY          READE     WMOOL                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     OOOFPC        IFGT      0
     C     OOEXFL        ANDEQ     'I'
     C                   MOVE      *ON           *IN44
     C                   LEAVE
     C                   ENDIF                                                  OOOFPC IFGT 0
      *
     C     EMKY          READE     WMOOL                                  60
      *
     C                   ENDDO
      *
     C                   ENDIF                                                  CLTID IFEQ '046'
      *** END ADD ***** 07/29/08 **************************************
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
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
      *** BEG ADD **** 02/18/03 **************************************
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  WCFL23 IFEQ
      *
     C                   SETOFF                                       42
     C   43              MOVEL(P)  ELBSCD        XXKEY
     C   43TBKY          CHAIN     PBS01                              60
     C   43              SELECT
     C     BSFREQ        WHENEQ    'W'
     C                   MOVEL(P)  'Weekly'      SCFREQ
     C                   SETON                                        42
     C                   Z-ADD     2             VIEW#
     C     BSFREQ        WHENEQ    'M'
     C                   MOVEL(P)  'Monthly'     SCFREQ
     C                   OTHER
     C                   MOVE      *BLANKS       SCFREQ
     C                   ENDSL
      *** END ADD **** 02/18/03 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
      *** BEG ADD **** 02/18/03 **************************************
     C   22KEY1          READE     DBRCDF2                                95
      *** END ADD **** 02/18/03 **************************************
     C*
     C                   ENDSR                                                  READ
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
      *** BEG ADD **** 02/18/03 **************************************
     C   22KEY1          READPE    DBRCDF2                                95
      *** END ADD **** 02/18/03 **************************************
     C*
     C                   ENDSR                                                  READP
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
      *** BEG ADD **** 02/18/03 **************************************
     C   22KEY1B         SETGT     DBRCDF2                            96
      *** END ADD **** 02/18/03 **************************************
     C*
     C                   ENDSR                                                  SETGT
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
      *** BEG ADD **** 02/18/03 **************************************
     C   22KEY1B         SETLL     DBRCDF2                            96
      *** END ADD **** 02/18/03 **************************************
     C*
     C                   ENDSR                                                  SETLL
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C     SF1UPD        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCRPMY
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   Z-ADD     *ZEROS        SCCLAS
     C                   MOVEL     *BLANKS       SCSPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVEL     *BLANKS       SCEXT
     C*** END   ADD ***  04/20/09  ************************************
     C                   MOVEL(P)  DELETD        SCDESC
     C*
     C                   ENDSR                                                  SF1UPD
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
     C*===============================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
** Headers - HD
Opt  Rpt MM/CCYY        Amount   Eff Date   Class  Spc  Ext   Description
Opt  Report date        Amount   Eff Date   Class  Spc  Ext   Description
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                NNN
1066=Add payrolls, same dateYNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add S/R report        YNN
212F12=Cancel               YYY
215F15=S/R payroll calc     YNN
