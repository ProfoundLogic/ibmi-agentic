     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0030R1 - Payee Master Maintenance Program
     H*****************************************************************
     H*
     H*  Description:
     H*
     H*    This program maintains the WCMS Payee Demographic file.
     H*
     H*    F10 is used for all search pgm execution.  Cursor is placed
     H*    in field to search.  If a valid search program exists, a
     H*    search field list will display.  If no valid search program
     H*    exists, an error message will display.
     H*    F20 will toggle Status field from 'A' to 'I' and vice versa.
     H*
     H*  Called From:
     H*    W0030R
     H*
     H*  Calls To:
     H*    FA005R
     H*    F0032R01
     H*    SA001R
     H*    SRCENTER
     H*    SRCVTDAT
     H*    SY600R
     H*    W0534R
     H*    W2111R
     H*
     H*  Created:  11/06/96  C:30  Project:  68MDI
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/22/19 C:079 Support: 152/79622                             *
     H*                Use isModEnabled for Managed Care install flags*
     H*                                                               *
     H* 10/09/14 C:212 Project: RED4660                               *
     H*                Make changes in screen to show Mod when zero   *
     H*                Set default value for 1099 flag (client flag'd)*
     H*                                                               *
     H* 07/04/14 C:300 Project: WC2109                                *
     H*                Added email address field to the Payee         *
     H*                Demographics screen                            *
     h*                                                               *
     h* 12/24/13 C:283 Support: WA090 84395                           *
     h*                Corrected the formatting of the Phone# and     *
     h*                FAX# fields on the screen. Recompiling the pgm.*
     h*                                                               *
     h* 03/18/13 C:282 Redmine: 2455                                  *
     h*                Intialized the default value of SC1099 as 'N'  *
     H*                                                               *
     H* 12/27/11 C:282 Project: WC2005                                *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H* 08/04/10 C:190 SUPPORT  73713                                 *
     H*                Made changes of 25WRG to BASE.                 *
     H*                                                               *
     H* 04/19/10 C:235 PROJECT: 25WRG                                 *
     H*                CLAIMS GENERAL RETROFIT                        *
     H*                                                               *
     H* 11/02/09 C:232 PROJECT: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*                                                               *
     H* 10/05/09 C:33  Project: WC1770                                *
     H*                Add 'Firm Name'                                *
     H*                                                               *
     H* 07/14/09 C:189 Project: 114WOJ                                *
     H*                  Change of Address Tracking/Force new Darts   *
     H*                  Remark                                       *
     H*                                                               *
     H* 12/20/07 C:33  Seq#: WA135/WC135                              *
     H*                Removed code to force the PD1099 flag to a "Y" *
     H*                 for CLT135.                                   *
     H*
     H* 12/10/07 C:198 Project: FL064                                 *
     H*                Expanded license# field to 13 characters.      *
     H*                                                               *
     H*  10/23/07 C:180 PROJECT/SUPPORT WC132 54749
     H*                 Not needed for this release.
     H*
     H*  10/04/07 C:180 SUPPORT 60139
     H*                 Do not zero-suppress FEIN# on SCR01.
     H*                 No changes to this pgm - Just to DSPF.
     H*                                                               *
     H* 02/16/06 C:132 Project: WA135 - 50615                         *
     H*              - Added code to force the PD1099 flag to a "Y"   *
     H*                on the screen and file update if client 135.   *
     H*              - Added code to load initial user, date, pgm &   *
     H*                time fields if is add record.                  *
     H*                                                               *
     H* 12/15/05 C:168 Project: 114WKX                                *
     H*                Added code to log every change for every field *
     H*                on the screen to log file WDPHP                *
     H*                                                               *
     H* 06/22/05 C:132 Project: WC129  47611                          *
     H*                Edit on out-of-Contry code field in wrong spot *
     H*                                                               *
     H* 03/21/05 C:15  Project: WC1269                                *
     H*                Added out of country address entry fields.     *
     H*                                                               *
     H* 10/25/04 C:53  Support: WC128/43225                           *
     H*                Chgd code to use WARPLW to only read needed    *
     H*                records                                        *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H*  03/03/04 C:58  PROJECT: WC1142
     H*                 If payee/provider address changes in master,
     H*                 update unprocessed check request file (WARPP).
     H*
     H*  01/18/02 C:53  Support: WC095/30740                          *
     H*                 ADD EDIT FOR 2ND ADDRESS WHEN DBA = "Y"       *
     H*                                                               *
     H*  09/04/01 C:132 PROJECT: WC1047                               *
     H*                 ADD PDSS# Field in WMPDP file for Identifying *
     H*                 ID Number is a SocSec# = 'Y' or FEIN = 'N'    *
     H*                                                               *
     H*  04/10/01 C:62  PROJECT: MC244                                *
     H*                 CONDITION OF CALL OF FA005R IS GOING TO BE    *
     H*                 BASED UPON WCMC INSTALL FLAG ONLY             *
     H*                                                               *
     H*  09/06/00 C:62  Support: MC101/24239                          *
     H*                 ADD PARM TO CALL OF F0032R01.                 *
     H*                                                               *
     H*  08/17/00 C:15  Support: WC025/23822                          *
     H*                 Removed edit on license# which requires the   *
     H*                 first 2 characters to be alpha.               *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768                          *
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.*
     H*                                                               *
     H*  02/01/00 C:118 PROJECT: WC577                                *
     H*                 Add search on provider type.                  *
     H*                                                               *
     H*  01/11/00 C:15  Project: WC823                                *
     H*                 Correct loading of PDDT.                      *
     H*                                                               *
     H*  12/09/99 C:15  Project: WC823                                *
     H*                 Correct loading of PDDT.                      *
     H*                                                               *
     H*  08/18/99 C:61  PROJECT: WC823                                *
     H*                 correct date comparison.                      *
     H*                                                               *
     H*  06/23/99 C:118 SUPPORT:  MC033/18538                         *
     H*                 Modified RPG so SCFEIN can not be less than or*
     H*                 equal to zero.                                *
     H*                                                               *
     H*  05/24/99 C:118 Project:  WC577                               *
     H*                 Changed indicators so cursor position would   *
     H*                 move from 'Specialty Code' to 'WC Certified'. *
     H*                                                               *
     H*  05/29/98 C:15 Support: WC065/11797                           *
     H*               -Fixed the payee type edit.  It was not working *
     H*                in change mode.                                *
     H*               -Fixed the 1099 sent and received date edits.   *
     H*               -Added a window which will be displayed if the  *
     H*                1099 mod entered does not match the any of the *
     H*                1099 mods for other modifiers for the entered  *
     H*                FEIN.                                          *
     H*                                                               *
     H*  04/23/98  C:33  SEQ#: WA078/11095                            *
     H*                 -REMOVE F20=CHANGE STATUS AND IMPLEMENT CHANGE*
     H*                  STATUS USING OPT 20 FROM W/W                 *
     H*                 -IMPLEMENT F5=REFRESH                         *
     H*                                                               *
     H*  01/13/97 C:30 Project:  10MAE                                *
     H*           Edit Payee Type for '1' or '4' only.                *
     H*                                                               *
     H*  03/06/97 *** C:31 *** 87MBQ ***                              *
     H*           Load BWC Providers/Anthem                           *
     H*           1. BWC cert date to screen 2                        *
     H*                                                               *
     H*****************************************************************
     H*  Indicators:
     H*
     H*   09 - Display Mode.
     H*   20 - Provide Name is blank.
     H*   21 - Address Line 1 is blank.
     H*   22 - City is blank.
     H*   23 - State is invalid.
     H*   24 - Zip is zero.
     H*   25 - BWC Cert date error.
     H*   26 - 1099 flag is invalid (Y/N).
     H*   27 - Payee Type is invalid ( 1 - 6 ).
     H*   28 - Speciality code is invlid.
     H*   29 - Mod for 1099 Mailed to Mod does not exist.
     H*   30 - Backup Withholding flag is invalid (Y/N).
     H*   31 - WC Certified flag is invalid (Y/N).
     H*   32 - W9 Send Date is invalid.
     H*   32 - W9 Receive Date is invalid.
     H*   34 - Address 1 DBA Flag is invalid (Y/N).
     H*   35 - W9 Send Date is not less than or equal W9 Receive Date
     H*   36 - ADD mode, FEIN is zero.
     H*   37 - ADD mode, FEIN/Mod already exists.
     H*   38 - ADD mode, Status is invalid (A/I).
     H*   39 - License # must contain 2 leading alpha chars
     H*   40 - ADD mode.
     H*   41 - F10 pressed for field that does not have a search pgm.
     H*        An error message displays.
     H*   43 - DSPATR(PC) on SCST
     H*   44 - DSPATR(PC) on SCZIP1
     H*   45 - DSPATR(PC) on SC109M
     H*   46 - DSPATR(PC) on SCTYPE
     H*   47 - DSPATR(PC) on SCSPEC
     H***** BEGIN CHG ***** 05/24/99 **********************************
     H*** 48 - DSPATR(PC) on SCLIC#
     H*   48 - DSPATR(PC) on SCWCCE
     H***** END CHG ******* 05/24/99 **********************************
     H*   49 - No Search pgm for F10 field search error message.
     H***** BEGIN ADD ***** 09/04/01 **********************************
     H*   51 - SS#/FEIN Flag is Invalid - Not (Y/N)
     H***** END CHG ******* 09/04/01 **********************************
     H*   60 - General File Access
     H*   67 - Read WARPP
     H*   71 - BWC Certification definition error.
     H*  N74 - DSPATR(ND) on 'Mail to Mod Name' & SCMNAM.
     H*   74 - Mail to Mod different than FEIN/Mod.
     H*   90 - Edit Error occurred.
     H*   99 - General File access.
     H*
     H*****************************************************************
      *  Payee Maint Screen
     FW0030F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  WCMS Payee Demographics
     FWMPDP     UF A E             DISK
     F                                     RENAME(PPD01:DBRCDF)
     F*** BEGIN ADD ***  12/27/11  ****************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *****  12/27/11  ****************************************
     FWMPDL     IF   E           K DISK
     F                                     RENAME(PPD01:PPD02)
     F***** BEGIN ADD **** 12/15/05 ***********************************
     FWDPHP     O    E             DISK
     F***** END   ADD **** 12/15/05 ***********************************
      *
      *  User Parameters
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
      *
      *  Table File
     FWTABL     IF   E           K DISK
     F************PSY01                             KIGNORE
      *
      *  System-Wide Table file
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      *
     F***** BEGIN ADD **** 02/01/00 ***********************************
      *  System Table Value File
     FWTGTL     IF   E           K DISK
     F***** END   ADD **** 02/01/00 ***********************************
     F***** BEGIN ADD **** 03/03/04 ***********************************
      *  Payment request file
     F*** BEG CHG *** 10/25/04 ***********************************
     F**WARPP***UF  E                    DISK
     FWARPLW    UF   E           K DISK
     F*** END CHG *** 10/25/04 ***********************************
     F***** END   ADD **** 03/03/04 ***********************************
      ***** BEG ADD ******* 07/14/09 **********************************
     FSMCOL     IF   E           K DISK
      ***** END ADD ******* 07/14/09 **********************************
     F*
      *
     D*** Beg Add *** 05/22/19 **************************************
      /Copy sprModule
     D*** End Add *** 05/22/19 **************************************
     F***** BEGIN ADD **** 12/15/05 ***********************************
      /COPY SCOPYMT01D
     D FNAM            S             40    DIM(28) CTDATA PERRCD(1)             FIELD NAMES
     F***** END   ADD **** 12/15/05 ***********************************
      *
     D***PY SCOPYMT01I
      *
     D PGMCO#          C                   CONST('FA007R    ')
     D OPTNM           C                   CONST('Payee Demographics')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D***** BEGIN ADD **** 03/03/04 ***********************************
     D*
     D*        Name and address data structures: screen vs. file.
     D*
     D SCRNA           DS                  INZ
     D  SCNAME                 1     40
     D  SCADR1                41     80
     D  SCADR2                81    120
     D  SCCTY                121    145
     D  SCST                 146    147
     D*** Begin add *** 03/21/05 **************************************
     D  SCADR3               148    187
     D*** End add ***** 03/21/05 **************************************
     D FILNA           DS                  INZ
     D  PDNAME                 1     40
     D  PDADR1                41     80
     D  PDADR2                81    120
     D  PDCTY                121    145
     D  PDST                 146    147
     D*** Begin add *** 03/21/05 **************************************
     D  PDADR3               148    187
     D*** End add ***** 03/21/05 **************************************
     D***** END   ADD **** 03/03/04 ***********************************
     D*** BEGIN ADD ***  12/27/11  ************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*** END ADD *****  12/27/11  ************************************
     D*** Beg Add *** 05/22/19 **************************************
     d isWcMc          s              1n   inz('0')
     d isWcInt         s              1n   inz('0')
     D*** End Add *** 05/22/19 **************************************
      *
      *  PAYEE DEMOGRAPHICS FILE
      *
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*** BEGIN ADD *** 05/29/98 **************************************
     C     PDKY1         KLIST
     C                   KFLD                    XXFEIN
     C*** END ADD ***** 05/29/98 **************************************
     C*** BEG ADD *** 10/25/04 ***********************************
      *
     C     RPKEY         KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
      *
     C*** END ADD *** 10/25/04 ***********************************
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  SPECIALTY CODE TABLE FILE
      *
     C     SPKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    TABKEY
     C*
     C***** BEGIN ADD **** 02/01/00 ***********************************
     C* SYSTEM TABLE VALUE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C***** END   ADD **** 02/01/00 ***********************************
      *
      ***** BEG ADD ******* 07/14/09 **********************************
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
      ***** END ADD ******* 07/14/09 **********************************
      *  CVTDAT
      *
     C*** BEGIN DLT *** 01/11/00 **************************************
     C********** DTYMD     PLIST
     C**********           PARM           SRDTI   80
     C**********           PARM '*MDY'    SRFFMT  8
     C**********           PARM '*YMD'    SRTFMT  8
     C**********           PARM           SRDTO  10
     C**********           PARM           SRERR   1
     C*** END DLT ***** 01/11/00 **************************************
      *
      *  Company Name
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  F0032R01
      *
     C     ACDPL2        PLIST
     C     *INKC         PARM      *INKC         @@KC              1
     C     *INKL         PARM      *INKL         @@KL              1
     C*** BEGIN CHG *** 03/09/98 **************************************
     C                   PARM                    ##MODE            1
     C*** END   CHG *** 03/09/98 **************************************
     C                   PARM                    QQMCPD            9 0          FMPDP RRN
     C*** BEGIN CHG *** 09/06/00 **************************************
     C                   PARM                    ##CALL            1
     C*** END   CHG *** 09/06/00 **************************************
      *
      *  Managed Care File Maint Plist
      *
     C     MCPL          PLIST
     C*** BEGIN CHG *** 03/09/98 **************************************
     C                   PARM      @@MODE        ##MODE
     C*** END   CHG *** 03/09/98 **************************************
     C                   PARM                    QQMCPD            9 0          FMPDP RRN
     C                   PARM                    QQMCP1            9 0          FMP1P RRN
     C                   PARM                    QQRTCD            1            Return Code
     C                   PARM                    PDFEIN
     C                   PARM                    PDMOD
     C                   PARM                    PDNAME
     C                   PARM                    PDFSN
     C                   PARM                    PDTITL
     C                   PARM                    PDADR1
     C                   PARM                    PDCTY
     C                   PARM                    PDST
     C                   PARM                    PDZIP
     C                   PARM                    PDAREA
     C                   PARM                    PDPHN#
     C                   PARM                    PDSHNM
     C                   PARM                    PD1099
     C                   PARM                    PDTYPE
     C                   PARM                    PDSPCD
     A*** Begin Chg *** 12/10/07 **************************************
     C******************   PARM           PDFLIC
     C                   PARM                    PDLIC#
     A*** End   Chg *** 12/10/07 **************************************
     C                   PARM                    PDVFLG
     C                   PARM                    PDADR2
     C                   PARM                    PDDBA1
     C                   PARM                    PDFXAR
     C                   PARM                    PDFAX#
     C                   PARM                    PDWCCE
     C                   PARM                    PDSTAT
      *** BEGIN ADD *** 03/26/97 ***********************************************
     C                   PARM                    PDWCDT
     C                   PARM                    PDBWC#
      *** END   ADD *** 03/26/97 ***********************************************
     C                   PARM                    PDBWCT
     C*** Begin add *** 03/21/05 **************************************
     C                   PARM                    PDCTRY
     C                   PARM                    PDADR3
     C*** End add ***** 03/21/05 **************************************
      *
      *  P r o g r a m     S t a r t
      *
     C     *ENTRY        PLIST
     C     *INKC         PARM      *INKC         @@KC              1
     C     *INKL         PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      *
     C*****      *LIKE     DEFN WAUK      NNUK
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    SPKEY         TABKEY
     C***** BEGIN ADD **** 02/01/00 ***********************************
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C***** END   ADD **** 02/01/00 ***********************************
     C***** BEGIN ADD **** 03/03/04 ***********************************
     C     *LIKE         DEFINE    PDZIP         XXZIP
     C***** END ADD ****** 03/03/04 ***********************************
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
     C/COPY SCOPYMT01C
      *
      ****************************************************************
      *  DLTPRE                                                      *
      ****************************************************************
     C     DLTPRE        BEGSR
     C                   ENDSR
      *
      ****************************************************************
      *  DSPLYR - Indicate Screen has been displayed.                *
      ****************************************************************
     C     DSPLYR        BEGSR
      *
     C     PDTYPE        IFEQ      '1'
     C     PDTYPE        OREQ      '4'
     C                   EXSR      MCINTR
     C                   ENDIF
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK1 - Edit W0030F1 SCR02 fields                          *
      ****************************************************************
     C     ERRCK1        BEGSR
      *
      *  If DISPLAY mode, ignore Edit processing.
      *
     C     @@MODE        IFNE      'D'
      *
      *  The Provider Number cannot be zero    (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     *IN40         IFEQ      *ON
     C***** BEGIN CHG **** 06/23/99 ***********************************
     C********** SCFEIN    IFEQ *ZERO
     C     SCFEIN        IFLE      *ZERO
     C***** END   CHG **** 06/23/99 ***********************************
     C                   SETON                                        9036
     C                   ENDIF
      *
      *  The Provider Number and Modifier can not exist in Payee
      *  Demographics file                     (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C     PDKEY         CHAIN     PPD02                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9037
     C                   CLEAR                   DBRCDF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  Provider Status Code must be 'A' or 'I'
      *
     C*** BEG ADD *** 11/02/09 ****************************************
     C     CLTID         IFNE      '139'
     C*** END ADD *** 11/02/09 ****************************************
     C     *IN90         IFEQ      *OFF
     C*** BEG DEL *** 11/02/09 ****************************************
     C     *IN40         ANDEQ     *ON
     C*** END DEL *** 11/02/09 ****************************************
     C     SCSTAT        ANDNE     'A'
     C     SCSTAT        ANDNE     'I'
     C                   SETON                                        9038
     C                   ENDIF
     C*** BEG ADD *** 11/02/09 ****************************************
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
      *   Provider Status Code must be 'A' or 'I' or 'P' for client 139
     C*** BEG ADD *** 11/02/09 ****************************************
     C     CLTID         IFEQ      '139'
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SCSTAT        ANDNE     'A'
     C     SCSTAT        ANDNE     'I'
     C     SCSTAT        ANDNE     'P'
     C                   SETON                                        9069
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
      *  THE PROVIDER LAST NAME CANNOT BE BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAME        ANDEQ     *BLANK
     C                   SETON                                        9020
     C                   ENDIF
      *
      *  THE ADDRESS LINE 1 CANNOT BE BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCADR1        ANDEQ     *BLANK
     C                   SETON                                        9021
     C                   ENDIF
      *
      *  ADDRESS LINE 1 Used as DBA Flag must be 'N' or 'Y'
      *
     C     *IN90         IFEQ      *OFF
     C     SCDBA1        ANDNE     'Y'
     C     SCDBA1        ANDNE     'N'
     C                   SETON                                        9034
     C                   ENDIF
     C*** BEG ADD *** 01/18/02 ****************************
      *
      *  THE ADDRESS LINE 2 CANNOT BE BLANK IF DBA = "Y"
      *
     C     *IN90         IFEQ      *OFF
     C     SCDBA1        ANDEQ     'Y'
     C     SCADR2        ANDEQ     *BLANK
     C                   SETON                                        9070
     C                   ENDIF
      *
     C*** Begin add *** 03/21/05 **************************************
      *  Either the C/S/Z must be entered or the Out of Country address.
      *
     C     *IN90         IFEQ      *OFF
     C     SCCTY         ANDEQ     *BLANK
     C     SCST          ANDEQ     *BLANK
     C     SCZIP1        ANDEQ     *ZERO
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        9082
     C                   ENDIF
      *
      *  The C/S/Z and Out of Country address are mutually exclusive.
      *
     C     *IN90         IFEQ      *OFF
     C     SCCTY         ANDNE     *BLANK
     C     SCST          ANDNE     *BLANK
     C     SCZIP1        ANDNE     *ZERO
     C     SCADR3        ANDNE     *BLANK
     C                   SETON                                        908283
     C                   ENDIF
      *
      ***** BEG ADD ***** 06/22/05 ************************************
     C                   MOVEL(P)  *BLANKS       SCCTRD
     C     *IN90         IFEQ      *OFF
     C     SCCTRY        ANDNE     *BLANK
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9083
      *
     C                   ELSE
      *
     C                   MOVEL(P)  GTDESC        SCCTRD
      *
     C     SCCTRY        IFNE      'USA'
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        908183
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60 = *ON
      *
     C                   ENDIF                                                  *IN90 = *OFF
      ***** END ADD ***** 06/22/05 ************************************
      *
     C     SCCTRY        IFEQ      'USA'
     C     SCADR3        OREQ      *BLANK
     C*** End add ***** 03/21/05 **************************************
     C*** BEG END *** 01/18/02 ****************************
      *
      *  THE CITY CANNOT BE BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCCTY         ANDEQ     *BLANK
     C                   SETON                                        9022
     C                   ENDIF
      *
      *  THE STATE MUST BE VALID
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      *BLANKS       STKEY1           10
     C                   MOVEL     SCST          STKEY1
     C     STKEY1        CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   SETON                                        9023
     C                   ENDIF
     C                   ENDIF
      *
      *  THE ZIP CODE CANNOT BE ZERO
      *
     C     *IN90         IFEQ      *OFF
     C     SCZIP1        ANDLE     *ZERO
     C                   SETON                                        9024
     C                   ENDIF
     C*
     C*** Begin add *** 03/21/05 **************************************
     C                   ELSE
      *
      *  Validate the country code.
      *********
      ***** BEG DEL ***** 06/22/05 ************************************
     C*********  *IN90     IFEQ *OFF
     C*********  SCCTRY    ANDNE*BLANK
     C*********            MOVEL'CTRY'    XXTBID    P
     C*********            MOVELSCCTRY    XXKEY     P
     C*********  GTKY      CHAINPGT01                60
     C*********  *IN60     IFEQ *ON
     C*********  GTSTAT    ORNE 'A'
     C*********            SETON                     9083
     C*********            ENDIF
     C*********            ENDIF
      ***** END DEL ***** 06/22/05 ************************************
      *
      *  Either the C/S/Z must be entered or the Out of Country address.
      *
     C     *IN90         IFEQ      *OFF
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        908183
     C                   ENDIF
      *
     C                   ENDIF
     C*** End add ***** 03/21/05 **************************************
      *
      *  W9 Sent Date and W9 Receive Date (Optional).
      *
     C*** BEGIN DLT *** 05/29/98 **************************************
     C********** *IN90     IFEQ *OFF
     C********** SCW9SE    IFNE *ZERO
     C********** SCW9RC    ORNE *ZERO
     C*** END DLT ***** 05/29/98 **************************************
      *
      *  Verify valid Dates Entered.
      *
     C*** BEGIN ADD *** 05/29/98 **************************************
     C     *IN90         IFEQ      *OFF
     C     SCW9SE        ANDNE     *ZERO
     C*** END ADD ***** 05/29/98 **************************************
     C                   Z-ADD     SCW9SE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCW9SE
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9032
     C                   ENDIF
     C*** BEGIN ADD *** 05/29/98 **************************************
     C                   ENDIF
     C*** END ADD ***** 05/29/98 **************************************
      *
     C*** BEGIN ADD *** 05/29/98 **************************************
     C     *IN90         IFEQ      *OFF
     C     SCW9RC        ANDNE     *ZERO
     C*** END ADD ***** 05/29/98 **************************************
     C                   Z-ADD     SCW9RC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCW9RC
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9033
     C                   ENDIF
     C*** BEGIN ADD *** 05/29/98 **************************************
     C                   ENDIF
     C*
     C*  Received Date must be greater than or equal to Sent Date.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCW9SE        IFNE      *ZERO
     C     SCW9RC        ANDNE     *ZERO
     C*** END ADD ***** 05/29/98 **************************************
      *
      *  Convert Dates to YYMMDD format.
      *
     C                   Z-ADD     SCW9SE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      ***** BEG CHG ******* 08/18/99 **********************************
     C****************     MOVELSRDTO     XXW9SE  60
     C                   MOVEL     SRDTO         XXW9SE            7 0
      ***** END CHG ******* 08/18/99 **********************************
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9032
     C                   ENDIF
      *
     C                   Z-ADD     SCW9RC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      ***** BEG CHG ******* 08/18/99 **********************************
     C*******************  MOVELSRDTO     XXW9RC  60
     C                   MOVEL     SRDTO         XXW9RC            7 0
      ***** END CHG ******* 08/18/99 **********************************
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9033
     C                   ENDIF
      *
      *  Compare Sent Date and Receive Date
      *
     C     *IN90         IFEQ      *OFF
     C     XXW9RC        ANDLT     XXW9SE
     C                   SETON                                        9035
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      *  Backup Withholding flag must be 'Y' or 'N'
      *
     C     *IN90         IFEQ      *OFF
     C     SCBKUP        ANDNE     'Y'
     C     SCBKUP        ANDNE     'N'
     C                   SETON                                        9030
     C                   ENDIF
      *
     C*******BEG ADD********04/19/10**********************************
      *
      *   Excluded from dup pay edit 'Y' or Blank
      *
     C*******BEG DLT********08/04/10**********************************
     C********   CLTID     IFEQ '025'
     C*******END DLT********08/04/10**********************************
     C     *IN90         IFEQ      *OFF
     C     SCXDPE        ANDNE     'Y'
     C     SCXDPE        ANDNE     ' '
     C                   SETON                                        9050
     C                   ENDIF
     C*******BEG DLT********08/04/10**********************************
     C********             ENDIF
     C*******END DLT********08/04/10**********************************
     C*******END ADD********04/19/10**********************************
     C***** BEGIN ADD ***** 09/04/01 **********************************
      *
      *  SS#/FEIN ID flag must be 'Y' or 'N'
      *
     C     *IN90         IFEQ      *OFF
     C     SCSS#         ANDNE     'Y'
     C     SCSS#         ANDNE     'N'
     C     SCSS#         ANDNE     ' '
     C                   SETON                                        9051
     C                   ENDIF
     C***** END CHG ******* 09/04/01 **********************************
      *
      *  THE 1099 FLAG MUST BE Y,N
      *
     C     *IN90         IFEQ      *OFF
     C     SC1099        ANDNE     'Y'
     C     SC1099        ANDNE     'N'
     C                   SETON                                        9026
     C                   ENDIF
      *
      *  1099'S MAILED TO MOD MUST BE VALID'Y' OR 'N' ONLY IF
      *   SCMOD IS GT 0
      *
     C     *IN90         IFEQ      *OFF
     C     SC109M        ANDNE     SCMOD
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SC109M        XXMOD
     C     PDKEY         CHAIN     PPD02                              29
     C     *IN29         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
      *
      *  THE PAYEE TYPE MUST BE VALID
      *
     C*** BEGIN DLT *** 05/29/98 **************************************
     C********** *IN40     IFEQ *ON
     C*** END DLT ***** 05/29/98 **************************************
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDNE     '1'
     C     SCTYPE        ANDNE     '2'
     C     SCTYPE        ANDNE     '3'
     C     SCTYPE        ANDNE     '4'
     C     SCTYPE        ANDNE     '5'
     C     SCTYPE        ANDNE     '6'
     C                   SETON                                        9027
     C                   ENDIF
     C*** BEGIN DLT *** 05/29/98 **************************************
     C******************** ENDIF
     C*** END DLT ***** 05/29/98 **************************************
     C*
     C***  Begin Add  ***  10/05/09  *************************************
     C*  Firm Name required if Type = '5'
     C  N90SCTYPE        IFEQ      '5'
     C     SCRFNM        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
     C***  End   Add  ***  10/05/09  *************************************
      *
      *  IF ENTERED, SPECIALTY MUST BE VALID
      *
     C     *IN90         IFEQ      *OFF
     C     SCSPEC        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     SCSPEC        TABKEY
     C     SPKY          CHAIN     PSP01                              60
     C     *IN60         IFEQ      '1'
     C                   SETON                                        9028
     C                   ENDIF                                                  END '1' *IN60
     C                   ENDIF
      *
      *  (Optional)
     C*** BEGIN DLT *** 08/17/00 **************************************
     C*  The License # must contain 2 leading Alpha chars
     C**********
     C********** *IN90     IFEQ *OFF
     C********** SCLIC#    ANDNE*BLANK
     C********** 1         SUBSTSCLIC#:1  XXCH1   1
     C********** 1         SUBSTSCLIC#:2  XXCH2   1
     C********** XXCH1     IFLT 'A'
     C********** XXCH1     ORGT 'Z'
     C********** XXCH2     ORLT 'A'
     C********** XXCH2     ORGT 'Z'
     C**********           SETON                     9039
     C**********           ENDIF
     C**********           ENDIF
     C*** END DLT ***** 08/17/00 **************************************
      *
      *  The WC Certified flag must be 'Y' or 'N'
      *
     C     *IN90         IFEQ      *OFF
     C     SCWCCE        ANDNE     'Y'
     C     SCWCCE        ANDNE     'N'
     C                   SETON                                        9031
     C                   ENDIF
     C*** BEGIN ADD ** 03/06/97 ************************************************
     C*
     C*          Validate Certification Date
     C*          ------------------------------------------------------
     C     SCWCDT        IFNE      0
     C                   Z-ADD     SCWCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWCDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9025
     C                   ENDIF                                                  ERR = On
     C                   ENDIF                                                  WCDT <> 0
     C*
     C*          Validate Certification Definition
     C*          Errors are: BWC# = blank and WCDT <> 0
     C*                      WCCE = N and BWC# <> blank
     C*                      WCCE = N and WCDT <> 0
     C*          ------------------------------------------------------
      ******
     C******     SCBWC#    IFEQ *BLANKS
     C******     SCWCDT    ANDNE0
     C******     SCWCCE    OREQ 'N'
     C******     SCBWC#    ANDNE*BLANKS
     C******     SCWCCE    OREQ 'N'
     C******     SCWCDT    ANDNE0
     C******               SETON                     9071
     C******               ENDIF                           BWC# = blank
     C*** END ADD ** 03/06/97 **************************************************
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 05/29/98 **************************************
     C*  If the 1099 modifier does not match the ones setup for the
     C*    other modifiers for this FEIN, display a window which allows
     C*    the user to determine whether or not to accept the 1099 mod.
     C*
     C     *IN90         IFEQ      *OFF
     C     SC109M        ANDNE     *ZEROS
     C                   MOVE      'N'           XXFLAG            1
     C                   MOVE      SCFEIN        XXFEIN
     C     PDKY1         CHAIN     PPD02                              99
     C     *IN99         DOWEQ     *OFF
     C     PDMOD         IFNE      SCMOD
     C     PD109M        ANDEQ     SC109M
     C                   MOVE      'N'           XXFLAG
     C                   LEAVE
     C                   ENDIF
     C     PDKY1         READE     PPD02                                  99
     C  N99              MOVE      'Y'           XXFLAG
     C                   ENDDO
     C*
     C*  Display the window if the 1099 mod is different.
     C*
     C     XXFLAG        IFEQ      'Y'
     C*
     C     *INKF         DOUEQ     *ON
     C     *INKL         OREQ      *ON
     C                   EXFMT     WDW01
     C     *INKL         COMP      *ON                                    90
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 05/29/98 **************************************
      ***** BEG ADD ******* 07/14/09 **********************************
     C     CXKY          CHAIN     PCO01                              99
      *
     C     CONMAD        IFNE      *BLANKS
     C                   EXSR      AUDIT
      *
     C     @@AFL         IFEQ      'Y'
     C     ##RTN         ANDNE     'Y'
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
      *
      ***** END ADD ******* 07/14/09 **********************************
      *
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK2 - Edit W0030F1 SCR03 fields                          *
      ****************************************************************
     C     ERRCK2        BEGSR
     C                   ENDSR
      *
      ****************************************************************
      *  ERRCK3 - Edit W0030F1 SCR04 fields                          *
      ****************************************************************
     C     ERRCK3        BEGSR
     C                   ENDSR
      *
      ****************************************************************
      *  GETNXK                                                      *
      ****************************************************************
     C     GETNXK        BEGSR
     C***                  MOVE *ZEROS    WAUK
     C***        *HIVAL    SETGTPMWAL
     C***                  READPPMWAL                    99
     C***        WAUK      ADD  1         NNUK
     C                   ENDSR
      *
      ****************************************************************
      *  KJDO - Process F10 "Table Search" requests.                 *
      ****************************************************************
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      ******************************************************************
      *     M o d     S e a r c h     ( A D D     M O D E )
      *
     C     HFLD          WHENEQ    'SCMOD'                                      Mod Search
      *
     C                   MOVE      SCFEIN        ZZFEIN            9 0
     C                   MOVE      SCMOD         ZZMOD             3 0
     C                   MOVE      *BLANK        QQTYPE            1
     C****                 MOVE *BLANKS   QQPRDS
     C****                 MOVELPTL,1     QQPRDS
     C                   CALL      'W0534R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTCD            1
      *
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
      *
     C                   OTHER
     C                   MOVE      *ON           *IN41
      *
     C                   ENDSL
      *
      ******************************************************************
      *     S t a t e     S e a r c h
      *
     C     HFLD          WHENEQ    'SCST'                                       State Search
      *
     C                   MOVE      SCST          QQST              2
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'SY600R'
     C                   PARM                    QQST
     C                   PARM                    QQRTC             1
      *
     C                   SELECT
     C     QQRTC         WHENEQ    'A'
     C                   MOVE      *ON           *INKC
      *
     C     QQRTC         WHENEQ    '*'
     C                   MOVE      QQST          SCST
     C                   MOVE      *ON           *IN44
      *
     C                   OTHER
     C                   MOVE      *ON           *IN43
      *
     C                   ENDSL
      *
      *** Begin add *** 03/21/05 *************************************
      *     Country Code  S e a r c h
      *
     C     HFLD          WHENEQ    'SCCTRY'
     C*
     C                   MOVEL(P)  'CTRY'        QQTBID
     C                   MOVE(P)   *BLANK        QQKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN81                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCCTRY
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   SETON                                        8182      POS CUSR
     C                   END
     C                   END
      *** End add ***** 03/21/05 *************************************
      *
      ******************************************************************
      *     1 0 9 9     M a i l e d     t o     M o d     S e a r c h
      *
     C     HFLD          WHENEQ    'SC109M'                                     Mod Search
      *
      *  RESET CONDITIONING INDICATOR FOR MODIFIER DIFFERENCE FOR 1099
     C                   MOVE      '0'           *IN74
      *
     C                   MOVE      SCFEIN        ZZFEIN
     C                   MOVE      SCMOD         ZZMOD
     C                   MOVE      *BLANK        QQTYPE            1
     C****                 MOVE *BLANKS   QQPRDS
     C****                 MOVELPTL,1     QQPRDS
     C                   CALL      'W0534R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTCD            1
      *
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
      *
     C     QQRTCD        WHENEQ    '*'
     C** TEMP              WRITESCR99
     C                   MOVE      *ON           *IN46
     C                   MOVE      SCFEIN        XXFEIN
      *** BEGIN CHG *** 12/05/96 ********************************************
     C*********************MOVE QQMOD     XXMOD
     C                   MOVE      ZZMOD         XXMOD
      *** END   CHG *** 12/05/96 ********************************************
     C     SCMOD         IFNE      XXMOD
     C     PDKEY         CHAIN     PPD02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        SCMNAM
     C                   ENDIF                                                  PDKEY = '0'
     C                   MOVE      '1'           *IN74
     C                   ENDIF                                                  SCMOD NE XXMOD
      *** BEGIN CHG *** 12/05/96 ******************************************
     C*********************MOVE QQMOD     SC109M
     C                   MOVE      ZZMOD         SC109M
      *** END   CHG *** 12/05/96 ******************************************
      *
     C                   OTHER
     C                   MOVE      *ON           *IN45
      *
     C                   ENDSL
     C*
     C***** BEGIN ADD **** 02/01/00 ***********************************
     C*
     C*  Provider Type Search
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C*
     C                   MOVEL     *BLANKS       QQTBID
     C*
     C                   MOVEL(P)  'PVTP'        QQTBID
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM      *BLANKS       QQKEY
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTCD=A
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN46
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCTYPE
     C                   MOVEL(P)  QQKEY         XXKEY
     C                   MOVEL(P)  QQTBID        XXTBID
     C                   MOVE      *ON           *IN47
     C                   ENDIF                                                  QQRTCD=*
     C*
     C                   MOVEL     *BLANKS       SCTDES
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTDES
     C                   ENDIF                                                  *IN60=ON
     C*
     C***** END   ADD **** 02/01/00 ***********************************
      *
      ******************************************************************
      *     S p e c i a l t y     C o d e     S e a r c h
      *
     C     HFLD          WHENEQ    'SCSPEC'                                     Mod Search
      *
     C                   MOVE      SCSPEC        SCKEY            10
     C***** BEGIN ADD **** 05/24/99 ***********************************
     C                   MOVE      *BLANKS       SCKEY
     C***** END ADD ****** 05/24/99 ***********************************
     C                   CALL      'W2111R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    SCKEY
      *
     C                   SELECT
      *
     C     *INKL         WHENEQ    *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      *ON           *IN47
     C*
     C                   OTHER
     C*
     C                   MOVE      *ON           *IN48
     C                   MOVEL     SCKEY         SCSPEC
     C*** TEMP             WRITESCR99
      *
     C                   ENDSL
      ******************************************************************
      *
     C                   OTHER                                                  FIELD NO SEARCH
     C                   MOVE      *ON           *IN49
      *
     C                   ENDSL
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR
      *
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      PDFEIN        QCFEIN
     C                   MOVE      PDMOD         QCMOD
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0030R'      XXPGM            10
     C*
     C                   ENDSR                                                  KNDO
     C*
     C***  BEGIN DEL  ***  04/23/98  *********************************
     C****************************************************************
     C***KUDO*-*Process F20 ACTIVE/INACTIVE                          *
     C****************************************************************
     C********** KUDO      BEGSR
     C**********
     C********** SCSTAT    IFEQ 'A'
     C**********           MOVE 'I'       SCSTAT
     C**********           ELSE
     C**********           MOVE 'A'       SCSTAT
     C**********           ENDIF
     C**********
     C**********           MOVE *ON       *IN90
     C**********
     C**********           ENDSR
     C***  END   DEL  ***  04/23/98  *********************************
      *
      ****************************************************************
      *  MVDBSC - Assign Payee Demographic fields to Screen fields   *
      ****************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVE      PDFEIN        SCFEIN
     C                   MOVE      PDMOD         SCMOD
     C                   MOVE      PDNAME        SCNAME
     C                   MOVE      PDFSN         SCFSN
     C***  Begin Add  ***  10/05/09  *************************************
     C                   MOVEL     PDRFNM        SCRFNM
     C***  End   Add  ***  10/05/09  *************************************
     C                   MOVE      PDTITL        SCTITL
     C                   MOVE      PDADR1        SCADR1
     C                   MOVE      PDCTY         SCCTY
     C                   MOVE      PDST          SCST
     C                   MOVEL     PDZIP         SCZIP1
     C                   MOVE      PDZIP         SCZIP2
     C                   MOVE      PDAREA        SCAREA
     C                   MOVEL     PDPHN#        SCPHN1
     C                   MOVE      PDPHN#        SCPHN2
     C                   MOVE      PDSHNM        SCSHNM
      *
     C                   MOVE      PD1099        SC1099
     C     PD1099        IFEQ      *BLANK
     C                   MOVE      'N'           SC1099
     C                   ENDIF

     C***** Beg Add *** 10/09/14 *******************************************
     C** For T.H.E client, set the default value to 'Y' when adding a vendor
      /free

       If CltID = '089' and
          *in40 = *on and
          PD1099 <> 'Y';
         SC1099 = 'Y';
       EndIf;

      /end-free
     C***** End Add *** 10/09/14 *******************************************
      *
     C***  Begin Del  ***  12/20/07  **************************************
      ******BEG*ADD ******  02/16/06  **************************************
      **If*client is PPC, force default of "Y" to 1099 flag field.
     C********** CLTID     IFEQ '135'
     C********** PD1099    ANDNE'Y'
     C**********           MOVE 'Y'       SC1099
     C**********           ENDIF
      ******END*ADD ******  02/16/06  **************************************
     C***  End   Del  ***  12/20/07  **************************************
      *
     C                   MOVE      PDTYPE        SCTYPE
     C***** BEGIN ADD **** 02/01/00 ***********************************
     C                   MOVEL(P)  PDTYPE        XXKEY
     C                   MOVEL(P)  'PVTP'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCTDES
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 02/01/00 ***********************************
     C                   MOVE      PDSPCD        SCSPEC
     C                   MOVE      PDSTAT        SCSTAT
     C                   MOVE      PD109M        SC109M                         1099'S MAILING MOD
     C                   MOVE      PDADR2        SCADR2                         Address Line 2
     C*** Begin add *** 03/21/05 **************************************
     C                   MOVE      PDCTRY        SCCTRY
     C                   MOVE      PDADR3        SCADR3
     C*** End add ***** 03/21/05 **************************************
      *
     C                   MOVE      PDDBA1        SCDBA1                         ADDR1 Used as DBA
     C     PDDBA1        IFEQ      *BLANK
     C                   MOVE      'N'           SCDBA1
     C                   ENDIF
      *
     C                   MOVE      PDFXAR        SCFXAR                         FAX# Area Code
     C                   MOVEL     PDFAX#        SCFXP1                         FAX# Phone 1 - 3
     C                   MOVE      PDFAX#        SCFXP2                         FAX# Phone 4 - 7
     C                   Z-ADD     PDW9SE        SRDTI                          W9 Sent Date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCW9SE
     C                   Z-ADD     PDW9RC        SRDTI                          W9 Received Date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCW9RC
      *
     C                   MOVE      PDBKUP        SCBKUP                         Backup Withholding
     C     PDBKUP        IFEQ      *BLANK
     C                   MOVE      'N'           SCBKUP
     C                   ENDIF
      *
     C*** BEGIN ADD ** 09/04/01 ************************************************
     C                   MOVE      PDSS#         SCSS#                          SS#/FEIN ID flag
     C     PDSS#         IFEQ      *BLANK
     C                   MOVE      *BLANK        SCSS#
     C                   ENDIF
     C*** END ADD **** 09/04/01 ************************************************
      *
     C                   MOVE      PDWCCE        SCWCCE                         WC Certified Y/N
     C     PDWCCE        IFEQ      *BLANK
     C                   MOVE      'N'           SCWCCE
     C                   ENDIF
      *
     C*** BEGIN ADD ** 03/06/97 ************************************************
     C                   Z-ADD     PDWCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWCDT
     C                   MOVE      PDBWC#        SCBWC#                         WC Certified ID
     C*** END ADD ** 03/06/97 **************************************************
     A*** Begin Chg *** 12/10/07 **************************************
     C*****************    MOVE PDFLIC    SCLIC#           WC Certified Y/N
     C                   MOVE      PDLIC#        SCLIC#                         WC Certified Y/N
     A*** End   Chg *** 12/10/07 **************************************
     C*** BEG ADD ** 04/19/10 **************************************************
     C                   MOVE      PDXDPE        SCXDPE                         Exclude dup paye
     C*** END ADD ** 04/19/10 **************************************************
      ***** Beg Add ***** 07/04/14 ***************************************
      /free
       scEMad = pdemad;
      /end-free
      ***** End Add ***** 07/04/14 ***************************************
      *
     C     *IN40         IFEQ      *ON
     C*** BEG ADD *** 11/02/09 ****************************************
     C     CLTID         IFNE      '139'
     C*** END ADD *** 11/02/09 ****************************************
     C                   MOVE      'A'           SCSTAT
     C*** BEG ADD *** 11/02/09 ****************************************
     C                   ELSE
     C                   MOVE      ' '           SCSTAT
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
     C                   ENDIF
      *
      ***** BEG ADD ******* 07/14/09 **********************************
     C                   MOVEL     SCNAME        SVNAME           40
     C                   MOVEL     SCFSN         SVFSN            40
     C                   MOVEL     SCADR1        SVADR1           40
     C                   MOVEL     SCADR2        SVADR2           40
     C                   MOVEL     SCADR3        SVADR3           40
     C                   MOVEL     SCCTY         SVCTY            25
     C                   MOVEL     SCST          SVST              2
     C                   MOVEL     SCZIP1        SVZIP1            5 0
     C                   MOVE      SCZIP2        SVZIP2            4 0
     C                   MOVE      SCCTRY        SVCTRY            3
      ***** END ADD ******* 07/14/09 **********************************
     C*** BEGIN ADD ** 12/15/05 ************************************************
     C                   EXSR      MVSTMP
     C*** END   ADD ** 12/15/05 ************************************************
      *
     C                   ENDSR
      *
     C*** BEGIN ADD ** 12/15/05 ************************************************
      ****************************************************************
      *  MVSTMP - saves the field's value to the "temp" fields       *
      ****************************************************************
     C     MVSTMP        BEGSR
      *
     C     *LIKE         DEFINE    SCFEIN        @CFEIN
     C     *LIKE         DEFINE    SCMOD         @CMOD
     C     *LIKE         DEFINE    SCNAME        @CNAME
     C     *LIKE         DEFINE    SCFSN         @CFSN
     C     *LIKE         DEFINE    SCTITL        @CTITL
     C     *LIKE         DEFINE    SCSHNM        @CSHNM
     C     *LIKE         DEFINE    SCADR1        @CADR1
     C     *LIKE         DEFINE    SCDBA1        @CDBA1
     C     *LIKE         DEFINE    SCADR2        @CADR2
     C     *LIKE         DEFINE    SCCTY         @CCTY
     C     *LIKE         DEFINE    SCST          @CST
     C     *LIKE         DEFINE    SCZIP1        @CZIP1
     C     *LIKE         DEFINE    SCZIP2        @CZIP2
     C     *LIKE         DEFINE    SCCTRY        @CCTRY
     C     *LIKE         DEFINE    SCADR3        @CADR3
     C     *LIKE         DEFINE    SCAREA        @CAREA
     C     *LIKE         DEFINE    SCPHN1        @CPHN1
     C     *LIKE         DEFINE    SCPHN2        @CPHN2
     C     *LIKE         DEFINE    PDAREA        @DAREA                         Phone Area Code
     C     *LIKE         DEFINE    PDPHN#        @DPHN#                         Phone Number
     C     *LIKE         DEFINE    PDFXAR        @DFXAR                         FAX# Area Code
     C     *LIKE         DEFINE    PDFAX#        @DFAX#                         FAX# Number
     C     *LIKE         DEFINE    SCW9SE        @CW9SE
     C     *LIKE         DEFINE    SCW9RC        @CW9RC
     C     *LIKE         DEFINE    SCBKUP        @CBKUP
     C     *LIKE         DEFINE    SCSS#         @CSS#
     C     *LIKE         DEFINE    SC1099        @C1099
     C     *LIKE         DEFINE    SC109M        @C109M
     C     *LIKE         DEFINE    SCTYPE        @CTYPE
     C     *LIKE         DEFINE    SCSPEC        @CSPEC
     C     *LIKE         DEFINE    SCWCCE        @CWCCE
     C     *LIKE         DEFINE    SCLIC#        @CLIC#
     C     *LIKE         DEFINE    SCWCDT        @CWCDT
     C     *LIKE         DEFINE    SCBWC#        @CBWC#
      *
     C                   MOVE      SCFEIN        @CFEIN
     C                   MOVEL     SCMOD         @CMOD
     C                   MOVEL(P)  SCNAME        @CNAME
     C                   MOVEL(P)  SCFSN         @CFSN
     C                   MOVEL(P)  SCTITL        @CTITL
     C                   MOVEL(P)  SCSHNM        @CSHNM
     C                   MOVEL(P)  SCADR1        @CADR1
     C                   MOVEL(P)  SCDBA1        @CDBA1
     C                   MOVEL(P)  SCADR2        @CADR2
     C                   MOVEL(P)  SCCTY         @CCTY
     C                   MOVEL(P)  SCST          @CST
     C                   MOVE      SCZIP1        @CZIP1
     C                   MOVE      SCZIP2        @CZIP2
     C                   MOVEL(P)  SCCTRY        @CCTRY
     C                   MOVEL(P)  SCADR3        @CADR3
     C                   MOVEL     PDAREA        @DAREA                         Phone Area Code
     C                   MOVEL     PDPHN#        @DPHN#                         Phone Number
     C                   MOVEL     PDFXAR        @DFXAR                         Fax Area Code
     C                   MOVEL     PDFAX#        @DFAX#                         Fax Number
     C                   MOVEL(P)  SCW9SE        @CW9SE
     C                   MOVE      SCW9RC        @CW9RC
     C                   MOVEL(P)  SCBKUP        @CBKUP
     C                   MOVEL(P)  SCSS#         @CSS#
     C                   MOVEL(P)  SC1099        @C1099
     C                   MOVE      SC109M        @C109M
     C                   MOVEL(P)  SCTYPE        @CTYPE
     C                   MOVEL(P)  SCSPEC        @CSPEC
     C                   MOVEL(P)  SCWCCE        @CWCCE
     C                   MOVEL(P)  SCLIC#        @CLIC#
     C                   MOVE      SCWCDT        @CWCDT
     C                   MOVEL(P)  SCBWC#        @CBWC#
      *
     C                   ENDSR
      *
     C*** END   ADD ** 12/15/05 ************************************************
      ****************************************************************
      *  MVSCDB - Assign Screen fields to Payee Demographics Fields  *
      ****************************************************************
     C     MVSCDB        BEGSR
      *
     C                   MOVE      SCFEIN        PDFEIN
     C                   MOVE      SCMOD         PDMOD
     C                   MOVE      SCNAME        PDNAME
     C                   MOVE      SCFSN         PDFSN
     C***  Begin Add  ***  10/05/09  *************************************
     C                   MOVEL     SCRFNM        PDRFNM
     C***  End   Add  ***  10/05/09  *************************************
     C                   MOVE      SCTITL        PDTITL
     C                   MOVE      SCADR1        PDADR1
     C                   MOVE      SCCTY         PDCTY
     C                   MOVE      SCST          PDST
     C                   MOVEL     SCZIP1        PDZIP
     C                   MOVE      SCZIP2        PDZIP
     C                   MOVE      SCAREA        PDAREA
     C                   MOVEL     SCPHN1        PDPHN#
     C                   MOVE      SCPHN2        PDPHN#
     C                   MOVE      SCSHNM        PDSHNM
      *
     C                   MOVE      SC1099        PD1099
     C     SC1099        IFEQ      *BLANK
     C                   MOVE      'N'           PD1099
     C                   ENDIF
      *
     C***  Begin Del  ***  12/20/07  **************************************
      ******BEG*ADD ******  02/16/06  **************************************
      **IF*Client is 135 and 1099 flag <> "Y", force default of a "Y".
     C********** CLTID     IFEQ '135'
     C********** SC1099    ANDNE'Y'
     C**********           MOVE 'Y'       PD1099
     C**********           ENDIF
      ******END*ADD ******  02/16/06  **************************************
     C***  End   Del  ***  12/20/07  **************************************
      *
     C                   MOVE      SCTYPE        PDTYPE
     C                   MOVE      SCSPEC        PDSPCD
     A*** Begin Chg *** 12/10/07 **************************************
     C******************   MOVE SCLIC#    PDFLIC           12/01/90 ADD
     C                   MOVE      SCLIC#        PDLIC#                         12/01/90 ADD
     A*** End   Chg *** 12/10/07 **************************************
     C                   MOVE      SCSTAT        PDSTAT
     C                   MOVE      Q1USER        PDUSER
     C                   MOVE      Q1PGM         PDPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C*** BEGIN CHG *** 01/11/00 **************************************
     C******************** CALL 'SRCVTDAT'DTYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END CHG ***** 01/11/00 **************************************
     C*** BEGIN CHG *** 12/09/99 **************************************
     C******************** MOVELSRDTO     XXDT    6
     C                   MOVEL     SRDTO         XXDT              7
     C*** END CHG ***** 12/09/99 **************************************
     C                   MOVE      XXDT          PDDT
      *
     C                   Z-ADD     DSTIME        PDTIME
     C                   MOVE      SC109M        PD109M
      *
      ***** BEG ADD ******  02/16/06  **************************************
      * IF is ADD, load initial pgm, user, time & Date fields
     C     *IN40         IFEQ      *ON
     C                   Z-ADD     DSTIME        PDITIM
     C                   MOVE      XXDT          PDIDT
     C                   MOVE      Q1USER        PDIUSR
     C                   MOVE      Q1PGM         PDIPGM
     C                   ENDIF
      ***** END ADD ******  02/16/06  **************************************
     C                   MOVE      SCADR2        PDADR2                         Address Line 2
      *
     C*** Begin add *** 03/21/05 **************************************
     C                   MOVE      SCCTRY        PDCTRY
     C                   MOVE      SCADR3        PDADR3
     C*** End add ***** 03/21/05 **************************************
      *
     C                   MOVE      SCDBA1        PDDBA1                         ADDR1 Used as DBA
     C     SCDBA1        IFEQ      *BLANK
     C                   MOVE      'N'           PDDBA1
     C                   ENDIF
      *
     C                   MOVE      SCFXAR        PDFXAR                         FAX# Area Code
     C                   MOVEL     SCFXP1        PDFAX#                         FAX# Phone 1 - 3
     C                   MOVE      SCFXP2        PDFAX#                         FAX# Phone 4 - 7
     C                   Z-ADD     SCW9SE        SRDTI                          W9 Sent Date
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDW9SE
     C                   Z-ADD     SCW9RC        SRDTI                          W9 Received Date
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDW9RC
      *
     C                   MOVE      SCBKUP        PDBKUP                         Backup Withholding
     C     SCBKUP        IFEQ      *BLANK
     C                   MOVE      'N'           PDBKUP
     C                   ENDIF
      *
     C*** BEGIN ADD ** 09/04/01 ************************************************
     C                   MOVE      SCSS#         PDSS#                          Backup Withholding
     C     SCSS#         IFEQ      *BLANK
     C     SCSS#         OREQ      'N'
     C                   MOVE      *BLANK        PDSS#
     C                   ENDIF
     C*** END ADD **** 09/04/01 ************************************************
      *
     C                   MOVE      SCWCCE        PDWCCE                         WC Certified Y/N
     C     SCWCCE        IFEQ      *BLANK
     C                   MOVE      'N'           PDWCCE
     C                   ENDIF
      *
     C*** BEGIN ADD ** 03/06/97 ************************************************
     C                   Z-ADD     SCWCDT        SRDTI                          WC CERTIFIEDDate
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDWCDT                         WC Certified Date
     C                   MOVE      SCBWC#        PDBWC#                         WC Certified ID
     C*** END ADD ** 03/06/97 **************************************************
     C*** BEG ADD ** 04/19/10 **************************************************
     C                   MOVE      SCXDPE        PDXDPE                         Exclude dup paye
     C*** END ADD ** 04/19/10 **************************************************
      ***** Beg Add ***** 07/04/14 ***************************************
      /free
       PDemad = scEMad;
      /end-free
      ***** End Add ***** 07/04/14 ***************************************
      *
     C*** BEGIN ADD *** 12/15/05 **************************************
     C                   EXSR      MVSCHG
     C*** END ADD ***** 12/15/05 **************************************
      *
     C                   ENDSR
      *
     C*** BEGIN ADD ** 12/15/05 ************************************************
      ****************************************************************
      *  MVSCHG - check if a change occurred and writes a log record *
      ****************************************************************
     C     MVSCHG        BEGSR
      *
     C                   MOVE      ##CO#         PHCO#
     C                   MOVE      PDFEIN        PHFEIN
     C                   MOVE      PDMOD         PHMOD
     C                   MOVEL(P)  PDUSER        PHUSER
     C                   MOVE      PDDT          PHDT
     C                   MOVE      PDTIME        PHTIME
      *
      * Name or last name
     C     SCNAME        IFNE      @CNAME
     C                   MOVEA(P)  FNAM(1)       PHDESC
     C                   MOVEL(P)  @CNAME        PHFVAL
     C                   MOVEL(P)  SCNAME        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * First name
     C     SCFSN         IFNE      @CFSN
     C                   MOVEA(P)  FNAM(2)       PHDESC
     C                   MOVEL(P)  @CFSN         PHFVAL
     C                   MOVEL(P)  SCFSN         PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Title
     C     SCTITL        IFNE      @CTITL
     C                   MOVEA(P)  FNAM(3)       PHDESC
     C                   MOVEL(P)  @CTITL        PHFVAL
     C                   MOVEL(P)  SCTITL        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Short Name
     C     SCSHNM        IFNE      @CSHNM
     C                   MOVEA(P)  FNAM(4)       PHDESC
     C                   MOVEL(P)  @CSHNM        PHFVAL
     C                   MOVEL(P)  SCSHNM        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Mailing address
     C     SCADR1        IFNE      @CADR1
     C                   MOVEA(P)  FNAM(5)       PHDESC
     C                   MOVEL(P)  @CADR1        PHFVAL
     C                   MOVEL(P)  SCADR1        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Used as DBA
     C     SCDBA1        IFNE      @CDBA1
     C                   MOVEA(P)  FNAM(6)       PHDESC
     C                   MOVEL(P)  @CDBA1        PHFVAL
     C                   MOVEL(P)  SCDBA1        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Mailing address 2
     C     SCADR2        IFNE      @CADR2
     C                   MOVEA(P)  FNAM(7)       PHDESC
     C                   MOVEL(P)  @CADR2        PHFVAL
     C                   MOVEL(P)  SCADR2        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * city
     C     SCCTY         IFNE      @CCTY
     C                   MOVEA(P)  FNAM(8)       PHDESC
     C                   MOVEL(P)  @CCTY         PHFVAL
     C                   MOVEL(P)  SCCTY         PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * State
     C     SCST          IFNE      @CST
     C                   MOVEA(P)  FNAM(9)       PHDESC
     C                   MOVEL(P)  @CST          PHFVAL
     C                   MOVEL(P)  SCST          PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Zip code
     C     SCZIP1        IFNE      @CZIP1
     C                   MOVEA(P)  FNAM(10)      PHDESC
     C                   MOVEL(P)  @CZIP1        PHFVAL
     C                   MOVEL(P)  SCZIP1        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Zip code +4
     C     SCZIP2        IFNE      @CZIP2
     C                   MOVEA(P)  FNAM(11)      PHDESC
     C                   MOVEL(P)  @CZIP2        PHFVAL
     C                   MOVEL(P)  SCZIP2        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Country
     C     SCCTRY        IFNE      @CCTRY
     C                   MOVEA(P)  FNAM(12)      PHDESC
     C                   MOVEL(P)  @CCTRY        PHFVAL
     C                   MOVEL(P)  SCCTRY        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Province/Postal code
     C     SCADR3        IFNE      @CADR3
     C                   MOVEA(P)  FNAM(13)      PHDESC
     C                   MOVEL(P)  @CADR3        PHFVAL
     C                   MOVEL(P)  SCADR3        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Phone number with area code
     C     PDAREA        IFNE      @DAREA
     C     PDPHN#        ORNE      @DPHN#
     C                   MOVEA(P)  FNAM(14)      PHDESC
     C                   MOVEL(P)  @DAREA        ##PHON           11
     C     ##PHON        CAT       '-'           ##PHON
     C                   MOVE      @DPHN#        ##PHON
     C                   MOVEL(P)  ##PHON        PHFVAL
     C                   MOVEL(P)  PDAREA        ##PHON           11
     C     ##PHON        CAT       '-'           ##PHON
     C                   MOVE      PDPHN#        ##PHON
     C                   MOVEL(P)  ##PHON        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Fax number with area code
     C     PDFXAR        IFNE      @DFXAR
     C     PDFAX#        ORNE      @DFAX#
     C                   MOVEA(P)  FNAM(15)      PHDESC
     C                   MOVEL(P)  @DFXAR        ##PHON           11
     C     ##PHON        CAT       '-'           ##PHON
     C                   MOVE      @DFAX#        ##PHON
     C                   MOVEL(P)  ##PHON        PHFVAL
     C                   MOVEL(P)  PDFXAR        ##PHON           11
     C     ##PHON        CAT       '-'           ##PHON
     C                   MOVE      PDFAX#        ##PHON
     C                   MOVEL(P)  ##PHON        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * W9 sent date
     C     SCW9SE        IFNE      @CW9SE
     C                   MOVEA(P)  FNAM(16)      PHDESC
     C                   MOVEL(P)  @CW9SE        XXNUM
     C                   CALL      CHGNUM
     C                   PARM                    XXNUM            80
     C                   PARM      'DATE'        EDTMSK           10
     C                   MOVEL(P)  XXNUM         PHFVAL
      *
     C                   MOVEL(P)  SCW9SE        XXNUM
     C                   CALL      CHGNUM
     C                   PARM                    XXNUM            80
     C                   PARM      'DATE'        EDTMSK           10
     C                   MOVEL(P)  XXNUM         PHTVAL
      *
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * W9 received date
     C     SCW9RC        IFNE      @CW9RC
     C                   MOVEA(P)  FNAM(17)      PHDESC
     C                   MOVEL(P)  @CW9RC        XXNUM
     C                   CALL      CHGNUM
     C                   PARM                    XXNUM            80
     C                   PARM      'DATE'        EDTMSK           10
     C                   MOVEL(P)  XXNUM         PHFVAL
      *
     C                   MOVEL(P)  SCW9RC        XXNUM
     C                   CALL      CHGNUM
     C                   PARM                    XXNUM            80
     C                   PARM      'DATE'        EDTMSK           10
     C                   MOVEL(P)  XXNUM         PHTVAL
      *
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Backup withholding
     C     SCBKUP        IFNE      @CBKUP
     C                   MOVEA(P)  FNAM(18)      PHDESC
     C                   MOVEL(P)  @CBKUP        PHFVAL
     C                   MOVEL(P)  SCBKUP        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * ID number is SS#
     C     SCSS#         IFNE      @CSS#
     C                   MOVEA(P)  FNAM(19)      PHDESC
     C                   MOVEL(P)  @CSS#         PHFVAL
     C                   MOVEL(P)  SCSS#         PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * 1099 Flag (Y/N)
     C     SC1099        IFNE      @C1099
     C                   MOVEA(P)  FNAM(20)      PHDESC
     C                   MOVEL(P)  @C1099        PHFVAL
     C                   MOVEL(P)  SC1099        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * 1099 mailed to mod
     C     SC109M        IFNE      @C109M
     C                   MOVEA(P)  FNAM(21)      PHDESC
     C                   MOVEL(P)  @C109M        PHFVAL
     C                   MOVEL(P)  SC109M        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Provider type
     C     SCTYPE        IFNE      @CTYPE
     C                   MOVEA(P)  FNAM(22)      PHDESC
     C                   MOVEL(P)  @CTYPE        PHFVAL
     C                   MOVEL(P)  SCTYPE        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * Specialty code
     C     SCSPEC        IFNE      @CSPEC
     C                   MOVEA(P)  FNAM(23)      PHDESC
     C                   MOVEL(P)  @CSPEC        PHFVAL
     C                   MOVEL(P)  SCSPEC        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * WC certified (y/n)
     C     SCWCCE        IFNE      @CWCCE
     C                   MOVEA(P)  FNAM(24)      PHDESC
     C                   MOVEL(P)  @CWCCE        PHFVAL
     C                   MOVEL(P)  SCWCCE        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * License #
     C     SCLIC#        IFNE      @CLIC#
     C                   MOVEA(P)  FNAM(25)      PHDESC
     C                   MOVEL(P)  @CLIC#        PHFVAL
     C                   MOVEL(P)  SCLIC#        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * WC date
     C     SCWCDT        IFNE      @CWCDT
     C                   MOVEA(P)  FNAM(26)      PHDESC
     C                   MOVEL(P)  @CWCDT        PHFVAL
     C                   MOVEL(P)  SCWCDT        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
      * ID
     C     SCBWC#        IFNE      @CBWC#
     C                   MOVEA(P)  FNAM(27)      PHDESC
     C                   MOVEL(P)  @CBWC#        PHFVAL
     C                   MOVEL(P)  SCBWC#        PHTVAL
     C                   WRITE     PPH01
     C                   ENDIF
      *
     C                   ENDSR
     C*** END ADD ** 12/15/05 **************************************************
      *
      ****************************************************************
      *  OTHERR - Used for processing other Modes                    *
      ****************************************************************
     C     OTHERR        BEGSR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  OTHFKS - Used for processing other Function Keys            *
      ****************************************************************
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C***  BEGIN DEL  ***  04/23/98  *********************************
     C********** *INKU     CASEQ*ON       KUDO           90
     C***  END   DEL  ***  04/23/98  *********************************
     C                   ENDCS
      *
     C                   ENDSR
      *
      ****************************************************************
      *  PGMEND                                                      *
      ****************************************************************
     C     PGMEND        BEGSR
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
      *
      ****************************************************************
      *  PGMSTR                                                      *
      ****************************************************************
     C     PGMSTR        BEGSR
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40                          Unprotect FEIN
     C                   ELSE
     C                   MOVE      *OFF          *IN40                          Protect FEIN
     C                   ENDIF
      *
      ***** BEG ADD ******  02/16/06  **************************************
      ***** END ADD ******  02/16/06  **************************************
     C*
      ***** BEG ADD ******  02/16/06  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ******  02/16/06  **************************************
      *
      ***** BEG ADD ******  04/19/10  **************************************
      ***** BEG DLT ******  08/04/10  **************************************
     C*********  CLTID     IFEQ '025'
     C*********            MOVE *ON       *IN61
     C*********            ELSE
     C*********            MOVE *OFF      *IN61
     C*********            ENDIF
      ***** END DLT ******  08/04/10  **************************************
      ***** END ADD ******  04/19/10  **************************************
      ***** BEG ADD ******  11/02/09  **************************************
     C     CLTID         IFEQ      '139'
     C     @@MODE        ANDEQ     'A'
     C                   MOVE      *ON           *IN76
     C                   ELSE
     C                   MOVE      *OFF          *IN76
     C                   ENDIF
      ***** END ADD ******  11/02/09  **************************************
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C***** BEGIN ADD **** 03/03/04 ***********************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7
     C***** END   ADD **** 03/03/04 ***********************************
     C***** Beg Add **** 03/18/13 ***********************************
      /Free
       sc1099 = 'N';
      /End-Free
     C***** End Add **** 03/18/13 ***********************************
       //*** Beg Add *** 05/22/19 *************************************
      /Free
                         isWcMc = isModEnabled('Managed Care');
                         isWcInt = isModEnabled('Work Comp Interface');
      /End-free
       //*** End Add *** 05/22/19 *************************************
     C                   ENDSR
      *
      *
      ****************************************************************
      *  PREOTH                                                      *
      ****************************************************************
     C     PREOTH        BEGSR
      *
     C     @@MODE        IFEQ      'S'
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C     PDSTAT        IFEQ      'I'
     C                   MOVE      'A'           PDSTAT
     C                   ELSE
     C                   MOVE      'I'           PDSTAT
     C                   ENDIF
      *
     C                   MOVE      Q1USER        PDUSER
     C                   MOVE      Q1PGM         PDPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C*** BEGIN CHG *** 01/11/00 **************************************
     C******************** CALL 'SRCVTDAT'DTYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END CHG ***** 01/11/00 **************************************
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      XXDT          PDDT
      *
     C                   Z-ADD     DSTIME        PDTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   EXSR      MCINTR
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
     C***** BEGIN ADD **** 03/03/04 ***********************************
      ****************************************************************
      *  PAYREX - Update Payment Requests file
      ****************************************************************
     C     PAYREX        BEGSR
     C*** BEG CHG *** 10/25/04 ***********************************
     C***********          READ PRP01                    67
     C     RPKEY         SETLL     PRP01
     C     RPKEY         READE     PRP01                                  67
     C*** END CHG *** 10/25/04 ***********************************
     C     *IN67         DOWEQ     *OFF
     C*
     C     RPFEIN        IFNE      *ZERO
     C*** BEG DEL *** 10/25/04 ***********************************
     C********** RPFEIN    ANDEQPDFEIN
     C********** RPMOD     ANDEQPDMOD
     C*** END DEL *** 10/25/04 ***********************************
     C*
     C     SCFSN         IFEQ      *BLANK
     C                   MOVEL(P)  SCNAME        RPPNAM
     C                   ELSE                                                   FSN <> ' '
     C*
     C                   MOVEL(P)  SCNAME        XXLAST           56
     C                   MOVEL(P)  SCFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL(P)  XXLAST        RPPNAM
     C                   END                                                    FSN <> ' '
     C                   MOVEL     SCADR1        RPADR1
     C                   MOVEL     SCADR2        RPADR2
     C                   MOVEL     SCCTY         RPCTY
     C                   MOVE      SCST          RPST
     C                   MOVEL     SCZIP1        RPZIP
     C                   MOVE      SCZIP2        RPZIP
     C*** Begin add *** 03/21/05 **************************************
     C                   MOVE      SCCTRY        RPCTRY
     C                   MOVE      SCADR3        RPADR3
     C*** End add ***** 03/21/05 **************************************
     C*
     C     SCDBA1        IFEQ      'Y'
     C                   MOVE      SCADR1        RPPNAM
     C                   MOVE      SCADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C*
     C                   MOVEL     Q1PGM         RPPGM
     C                   MOVEL     Q1USER        RPUSER
     C                   TIME                    DSDTTM
     C                   MOVE      XXDT          RPDT
     C                   MOVEL     DSTIME        RPTIME
     C                   UPDATE    PRP01
     C                   ENDIF                                                  FEIN = ...
     C*
     C*** BEG CHG *** 10/25/04 ***********************************
     C***********          READ PRP01                    67
     C     RPKEY         READE     PRP01                                  67
     C*** END CHG *** 10/25/04 ***********************************
     C                   ENDDO                                                  DOW IN67 OFF
     C*
     C                   ENDSR
     C***** END   ADD **** 03/03/04 ***********************************
      *
      ****************************************************************
      *  UPDATR - Update Payee Demographics file                     *
      ****************************************************************
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C***** BEGIN ADD **** 03/03/04 ***********************************
     C*   First, check for N & A changes and apply to payment requests.
     C                   MOVEL     SCZIP1        XXZIP
     C                   MOVE      SCZIP2        XXZIP
     C     SCRNA         IFNE      FILNA
     C     XXZIP         ORNE      PDZIP
     C                   EXSR      PAYREX
     C                   ENDIF                                                  nSCRNA<>FIL
     C***** END   ADD **** 03/03/04 ***********************************
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        PDUSER
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C*** BEGIN CHG *** 01/11/00 **************************************
     C******************** CALL 'SRCVTDAT'DTYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END CHG ***** 01/11/00 **************************************
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      XXDT          PDDT
      *
     C                   MOVEL     DSTIME        PDTIME
      *
     C                   UPDATE    DBRCDF
      *
     C*** BEGIN DEL *** 04/10/01 **************************************
     C********** PDTYPE    IFEQ '1'
     C********** PDTYPE    OREQ '4'
     C*** END   DEL *** 04/10/01 **************************************
     C*** BEGIN ADD ***  12/27/11  ****************************************
     C     PDNAME        IFNE      SVNAME
     C                   CALL      'WA085R'
     C                   PARM      'P'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C                   PARM      ##CO#         XXCO#             3 0
     C                   ENDIF
     C*** END ADD *****  12/27/11  ****************************************
      *
     C                   EXSR      MCINTR
      *
     C*** BEGIN DEL *** 04/10/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 04/10/01 **************************************
     C                   ENDIF
      *
     C                   ENDSR
      *
      ****************************************************************
      *  WRITER - Write Payee Demographic file                       *
      ****************************************************************
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      GETNXK
     C***                  MOVE NNUK      WAUK
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        PDUSER
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C*** BEGIN CHG *** 01/11/00 **************************************
     C******************** CALL 'SRCVTDAT'DTYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END CHG ***** 01/11/00 **************************************
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      XXDT          PDDT
      *
     C                   MOVEL     DSTIME        PDTIME
     C*** BEG ADD *** 11/02/09 ****************************************
     C     CLTID         IFEQ      '139'
     C                   MOVE      SCSTAT        PDSTAT
     C                   ELSE
     C*** END ADD *** 11/02/09 ****************************************
     C                   MOVE      'A'           PDSTAT
     C*** BEG ADD *** 11/02/09 ****************************************
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      WMPDP
     C                   MOVE      *ON           OUTFLG
      *
     C*** BEGIN DEL *** 04/10/01 **************************************
     C********** PDTYPE    IFEQ '1'
     C********** PDTYPE    OREQ '4'
     C*** END   DEL *** 04/10/01 **************************************
      *
     C                   EXSR      MCINTR
      *
     C*** BEGIN DEL *** 04/10/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 04/10/01 **************************************
      *
     C                   ENDSR
      *
      ****************************************************************
      *  MCINTR - WCMC Interface                                     *
      ****************************************************************
     C     MCINTR        BEGSR
      *
      *
      *  If Payee Type is '1' or '4', call WCMC Interface
      *
     C*** BEGIN DEL *** 04/10/01 **************************************
     C********** PDTYPE    IFEQ '1'
     C********** PDTYPE    OREQ '4'
     C*** END   DEL *** 04/10/01 **************************************
      *
      *  WCMS / WCMC Installed flags determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
      *  Maintain WCMC Provider and Provider Auxiliary files
      *  prior to executing WCMC Provider Maint.
      *
       //*** Beg Chg *** 05/22/19 *************************************
     C*****QQFBIF        IFEQ      'Y'
     C*****QQFL01        ANDEQ     'Y'
      /Free
           If isWcMc and isWcInt;
      /End-Free
       //*** End Chg *** 05/22/19 *************************************
     C                   CALL      'FA005R'      MCPL
     C*** BEGIN ADD *** 04/10/01 **************************************
     C     PDTYPE        IFEQ      '1'
     C     PDTYPE        OREQ      '4'
     C*** END   ADD *** 04/10/01 **************************************
      *
     C     @@MODE        IFNE      'S'
     C*** BEGIN CHG *** 09/06/00 **************************************
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'A'           ##CALL
     C                   ELSE
     C                   MOVEL     ' '           ##CALL
     C                   ENDIF
     C*** END   CHG *** 09/06/00 **************************************
     C                   CALL      'F0032R01'    ACDPL2
      *
     C     *INKL         IFEQ      *ON
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      *ON           OUTFLG
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
     C*** BEG ADD ***** 07/14/09 *************************************
     C*===============================================================
     C*   AUDIT  WRITE DART IF CLAIMANT INFO CHANGES
     C*===============================================================
     C*
     C     AUDIT         BEGSR
      *
     C                   MOVE      ' '           @@AFL             1
     C     SCNAME        IFNE      SVNAME
     C     SVNAME        ANDNE     *BLANK
     C     SCFSN         ORNE      SVFSN
     C     SVFSN         ANDNE     *BLANK
     C     SCADR1        ORNE      SVADR1
     C     SVADR1        ANDNE     *BLANK
     C     SCADR2        ORNE      SVADR2
     C     SVADR2        ANDNE     *BLANK
     C     SCADR3        ORNE      SVADR3
     C     SVADR3        ANDNE     *BLANK
     C     SCCTY         ORNE      SVCTY
     C     SVCTY         ANDNE     *BLANK
     C     SCST          ORNE      SVST
     C     SVST          ANDNE     *BLANK
     C     SCZIP1        ORNE      SVZIP1
     C     SVZIP1        ANDNE     *ZERO
     C     SCZIP2        ORNE      SVZIP2
     C     SVZIP2        ANDNE     *ZERO
     C     SCCTRY        ORNE      SVCTRY
     C     SVCTRY        ANDNE     *ZERO
     C*
     C                   MOVE      'Y'           @@AFL
     C                   CALL      'WA074R'
     C                   PARM      0             ##CO#             3 0
     C                   PARM      0             ##FND             3 0
     C                   PARM      0             ##FYR             3 0
     C                   PARM      0             ##CASE            7 0
     C                   PARM      PDFEIN        ##FEIN            9 0
     C                   PARM      PDMOD         ##MOD             3 0
     C                   PARM      CONMAD        ##RCOD            5
     C                   PARM      *BLANKS       ##RTN             1
     C                   ENDIF
     C*
     C                   ENDSR                                                  AUDIT
     C*** END ADD ***** 07/14/09 *************************************
      *
     C*_______________________________________________________________
** FIELD NAMES
Name or last name                                                   1
First name                                                          2
Title                                                               3
Short name                                                          4
Mailing Address                                                     5
Used as DBA                                                         6
Mailing Address                                                     7
City                                                                8
State                                                               9
Zip Code                                                           10
Zip+4                                                              11
Country code                                                       12
Province/Postal cd                                                 13
Area code, Phone #                                                 14
Area Code, FAX #                                                   15
W9 Sent Date                                                       16
W9 Received Date                                                   17
Backup Withholding                                                 18
ID Nbr is SS#                                                      19
1099 flag                                                          20
1099 Mailed to Mod                                                 21
Provider type                                                      22
Specialty code                                                     23
WC Certified                                                       24
License #                                                          25
WC Date                                                            26
ID                                                                 27
