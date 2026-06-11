     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H*****************************************************************
     H/TITLE W0810R02 - CERTIFICATE HOLDER INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  This program will allow a user to change, view *
     H*                or add records in the Certificate Holder file. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0810R - WORK WITH CERTIFICATE HOLDERS                     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/12/99  PROGRAMMER: C:113 PROJECT: CM045          *
     H*****************************************************************
     H* REVISIONS:
      *
      * 03/04/11 C:235 SUPPORT 75260                                  *
      *              - Change the edits of field "Any Officers        *
      *                Excluded (Y/ )". Edit should allow user to     *
      *                enter Y or N or Blanks                         *
      *                                                               *
      * 04/23/10 C:61  PROJECT: WC1880
      *                Add "Revision #" to SCR01.  Default to 1 in
      *                ADD mode.
      *
      * 04/07/09 C:61  PROJECT: WC1304
      *              - Convert to use two user input screens.
      *              - Add fields for "Province/Postal code" and
      *                "Country Code".
      *              - Provide "Country code" table search.
     H*                                                               *
     H* 01/23/09 C:189 Project: SUPPORT 66635                         *
     H*               -Removed DEFAULT COMMENTS FOR LCTA               *
     H*                                                               *
     H* 07/08/08 C:190 Project: 135WLK(Priss#8)                       *
     H*                         Remove condition indicator(*IN90)     *
     H*                                                               *
     H* 06/24/08 C:190 Project: 135WLK                                *
     H*                Default the "Third party" certificate flag and *
     H*                the "Rollover" flag in ADD mode on Certificate *
     H*                Holders information screen each to 'Y,  if     *
     H*                processing for PPC.                            *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Remove all processing related to project 45WFB *
     H*                                                              *
     H*  04/26/06 C:168  PROJECT: 45WFB (IMPLEMENTED THE FOLLOWING)   *
     H*                * PROJECT: 45WBS                               *
     H*                  MODIFY TO ALLOW USER TO INDICATE IF CERT.    *
     H*                  INFO BEING ENTERED SHOULD BE DUPLICATED AS   *
     H*                  A WAIVER ENTRY AS WELL.  ENTRY ONLY SHOWS    *
     H*                  IN ADD MODE.                                 *
     H*                * PROJECT: 45WBZ                               *
     H*                  APPLY CHANGES TO RECOGNIZE WAIVERS THAT WERE *
     H*                  ADDED OR CHANGED ON A MIDTERM ENDORSEMENT    *
     H*                  REQUEST.                                     *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT 126WDK                                 *
     H*                PROJECT 126WCI                                 *
     H*                ADD NEW FIELD "EXCLUDED OFFICERS FLAG (Y/ )".  *
     H*                PROJECT 126WBJ                                 *
     H*                SCR01 CHANGES.  CHANGE SIZE OF REMARK LINES    *
     H*                2 AND 4.                                       *
     H*                PROJECT 126WBQ                                 *
     H*                ADD NEW FIELD "THIRD PARTY (Y/ )".             *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 05/12/99 C:113 PROJECT: CM045                                 *
     H*                Rewrote program to remove usage for waiver of  *
     H*                subrogation.  Waiver programs are now          *
     H*                W0815R/R02.                                    *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 10/19/98  C:78   PROJECT: WC577                               *
     H*                  CERT. # DEFAULTS TO THE SAME # IT DEFAULTED  *
     H*                  LAST TIME A NEW CERT HOLDER WAS ENTERED. CERT*
     H*                  NUMBER IS OBTAINED FOR ALL FUND YEARS INSTEAD*
     H*                  OF GETTING IT FOR EVERY SINGLE FUND YEAR.    *
     H*                                                               *
     H*  05/04/98  C:87  PROJECT: SUPP#11378                          *
     H*                  PROBLEM WITH 'ACORD DEFAULT' REMARK INCORR-  *
     H*                  ECTLY APPEARING FOR SOME EMPLOYERS. CHANGED  *
     H*                  READ PREVIOUS EQUAL TO TABLE FILE WTXCL.     *
     H*                                                               *
     H*  02/12/98  C:35  PROJECT: SUPPORT                             *
     H*                  CHANGE CODE SO THAT THE CANCELLATION DATE    *
     H*                  DISPLAYED ON THE SCREEN IS THE CX DATE       *
     H*                  ASSOCIATED WITH THE MOST RECENT EFFECTIVE    *
     H*                  DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED *
     H*                  SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY*
     H*                  DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER   *
     H*                  IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF  *
     H*                  THIS FIELD WITH AN INDICATOR SO THAT IF THE  *
     H*                  EMPLOYER IS NOT CANCELLED, THE FIELD AND     *
     H*                  HEADING WILL NOT SHOW ON THE SCREEN.         *
     H*                                                               *
     H*  02/10/98  C:33  PROJECT: WC577                               *
     H*                  DEFAULT THE NEXT AVAILABLE CERT #.           *
     H*                                                               *
     H*  12/29/97  C:87  PROJECT: WC752                               *
     H*                 -MODIFY SCREEN DISPLAY VARIABLES TO LINE UP   *
     H*                  WITH OTHERS.                                 *
     H*                 -CHAIN TO THE CANCELLATION MAINTENANCE FILE   *
     H*                  TO SEE IF THERE EXISTS AN "ACORD DEFAULT     *
     H*                  REMARK1"(XCDREM). IF SO MOVE THIS TO THE SCR *
     H*                  AND PROTECT IT FROM CHG.                     *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  32 - IF *ON : ACORD DEFAULT REMARK1 NOT BLANK                *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Certificate Holder number cannot be <= zero   *
     H*  21 - (WC30005) Certificate Holder number previously used     *
     H*  22 - (WCR0503) Name must be entered                          *
     H*  23 - (WCR0503) Address 1 must be entered                     *
     H*  24 - (WCR0503) City must be entered                          *
     H*  25 - (WCR0504) State not valid                               *
     H*  26 - (WCR0502) Zip 1 must be >= zero                         *
     H*  27 - (WCR0501) Rollover flag not 'Y' or 'N'                  *
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
     FW0810F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCEP     UF A E             DISK
     F                                     RENAME(PCE01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMCEL4    IF   E           K DISK
     F***  BEGIN ADD  ***  04/26/06  *********************************
      *** BEG DEL **** 07/16/07 ************************************
     F***WMWVP   O   E           K        DISK
      *** END DEL **** 07/16/07 ************************************
     F***  END   ADD  ***  04/26/06  *********************************
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWMRSL     IF   E           K DISK
     FWTXCL     IF   E           K DISK
     FWTMYP     IF   E             DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     FWDF2L     IF   E           K DISK
      ***** BEG ADD ******* 04/07/09 **********************************
      *
      *  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      ***** END ADD ******* 04/07/09 **********************************
      *
      *
      /COPY SCOPYMT01D
     D*================================================================
     D*  Array & Table definitions
     D*================================================================
     D XXBL            S             50    DIM(5)
     D XXAL            S             50    DIM(5)
     D*================================================================
     D*  Data Structures & constants definitions
     D*================================================================
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*
     D OPTNM           C                   CONST('Certificate Holder -
     D                                     Information')
     D***  BEGIN ADD  ***  04/26/06  *********************************
     D XXREM1          C                   CONST('EXCESS CARRIER: NAT-
     D                                     IONAL UNION FIRE IN-
     D                                     SURANCE COMPANY OF')
     D XXREM2          C                   CONST('LOUISIANA')
      ***  END   ADD  ***  04/26/06  *********************************
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
      ***** BEG CHG ******* 04/07/09 **********************************
     D*********     1                     C         #OFSCR
     D #OFSCR          C                   CONST(2)
      ***** END CHG ******* 04/07/09 **********************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*================================================================
     C*  PLIST Definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
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
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL2        PLIST
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*================================================================
     C*  KLIST Definitions
     C*================================================================
     C*
     C*  CERTIFICATE HOLDER FILE
     C*
     C     CEKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCCER#
     C                   KFLD                    SCFYR
     C*
     C     CEKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
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
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C* KEY LIST FOR THE GROUP AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*
     C*  CANCELLATION MAINTENANCE FILE
     C*
     C     XCKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     XCKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      ***** BEG ADD ******* 04/07/09 **********************************
      *
      *   GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      ***** END ADD ******* 04/07/09 **********************************
     C*================================================================
     C*  *LIKE DEFN definitions
     C*================================================================
      ***** BEG ADD ******* 04/07/09 **********************************
     C     *LIKE         DEFINE    GTTBID        XXTBID
      ***** END ADD ******* 04/07/09 **********************************
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CECER#        XXCER#
     C     *LIKE         DEFINE    CECER#        S#CER#
     C     *LIKE         DEFINE    CENAME        S#NAME
     C     *LIKE         DEFINE    CEADR1        S#ADR1
     C     *LIKE         DEFINE    CEADR2        S#ADR2
     C     *LIKE         DEFINE    CECTY         S#CTY
     C     *LIKE         DEFINE    CEST          S#ST
     C     *LIKE         DEFINE    CEZIP         S#ZIP
     C     *LIKE         DEFINE    CEATTN        S#ATTN
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C** **
     C** **      *LIKE     DEFN CETYPE    @@TYPE
     C** **      *LIKE     DEFN CETYPE    XXTYPE
     C** **      *LIKE     DEFN CECER#    X2CER#
     C***  END ADD ******  04/26/06  *********************************
     C                   Z-ADD     0             I1                3 0
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C*  CKCHGS - SUBROUTINE TO CREATE MID-TERM ENDORSEMENT RECORDS
     C*===============================================================
     C     CKCHGS        BEGSR
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     CECER#        IFNE      S#CER#
     C     CENAME        ORNE      S#NAME
     C     CEADR1        ORNE      S#ADR1
     C     CEADR2        ORNE      S#ADR2
     C     CECTY         ORNE      S#CTY
     C     CEST          ORNE      S#ST
     C     CEZIP         ORNE      S#ZIP
     C     CEATTN        ORNE      S#ATTN
      *
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C*
     C*  Load flag to designate an add thru midterm waiver changes
     C*
     C     XXCLT#        IFEQ      '045'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      '0'
     C                   MOVE      'C'           CERLFL
     C                   UPDATE    DBRCDF
     C                   END
     C                   ENDIF
     C***  END ADD ******  04/26/06  *********************************
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      AGTAPI        AGTPL2
     C*
     C*  SETUP FOR NEXT REQUEST NUMBER
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
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
     C*
     C                   MOVE      S#CER#        ENOFI1                         SUBRO #
     C                   MOVEL     S#NAME        ENOEN1                         NAME
     C                   MOVEL     S#ADR1        ENOM11                         ADR1
     C                   MOVEL     S#ADR2        ENOM21                         ADR2
     C                   MOVEL     S#CTY         ENOMC1                         CTY
     C                   MOVEL     S#ST          ENOMS1                         ST
     C                   MOVE      S#ZIP         ENOMZ1                         ZIP
     C                   MOVEL     S#ATTN        ENOC11                         ATTN
     C*
     C                   MOVE      CECER#        ENOFI2                         SUBRO #
     C                   MOVEL     CENAME        ENOEN2                         NAME
     C                   MOVEL     CEADR1        ENOM12                         ADR1
     C                   MOVEL     CEADR2        ENOM22                         ADR2
     C                   MOVEL     CECTY         ENOMC2                         CTY
     C                   MOVEL     CEST          ENOMS2                         ST
     C                   MOVE      CEZIP         ENOMZ2                         ZIP
     C                   MOVEL     CEATTN        ENOC12                         ATTN
     C*
     C                   MOVE      *BLANKS       ENNCFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENFIFL
     C                   MOVE      *BLANKS       ENCNFL
     C                   MOVE      *BLANKS       ENATFL
     C                   MOVE      *BLANKS       ENC3FL
     C                   MOVE      *BLANKS       ENTBFL
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENNOBF
     C                   MOVE      *BLANKS       ENNOFL
     C*
     C                   MOVE      'S'           ENMSFL
     C*
     C                   MOVEL(P)  DXMISC        ENRMCD
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C*  BUILD UP BEFORE LINES
     C*
     C                   MOVE      *BLANKS       XXBL
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     1             I                 1 0
     C                   MOVEL     S#CER#        XXBL(I)
     C                   CAT       S#NAME:1      XXBL(I)
     C                   ADD       1             I
     C     S#ATTN        IFNE      *BLANKS
     C                   MOVEL     S#ATTN        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     S#ADR1        IFNE      *BLANKS
     C                   MOVEL     S#ADR1        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     S#ADR2        IFNE      *BLANKS
     C                   MOVEL     S#ADR2        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     S#CTY         IFNE      *BLANKS
     C     S#ST          ORNE      *BLANKS
     C     S#ZIP         ORNE      *ZEROS
     C                   MOVEL     S#ZIP         XXZIP5            5
     C                   MOVE      S#ZIP         XXZIP4            4
     C     S#CTY         CAT       ',':0         XXBL(I)
     C                   CAT       S#ST:1        XXBL(I)
     C                   CAT       XXZIP5:1      XXBL(I)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXBL(I)
     C                   CAT       XXZIP4:0      XXBL(I)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  BUILD UP AFTER LINE
     C*
     C                   MOVE      *BLANKS       XXAL
     C                   Z-ADD     1             I
     C                   MOVEL     CECER#        XXAL(I)
     C                   CAT       CENAME:1      XXAL(I)
     C                   ADD       1             I
     C     CEATTN        IFNE      *BLANKS
     C                   MOVEL     CEATTN        XXAL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     CEADR1        IFNE      *BLANKS
     C                   MOVEL     CEADR1        XXAL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     CEADR2        IFNE      *BLANKS
     C                   MOVEL     CEADR2        XXAL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     CECTY         IFNE      *BLANKS
     C     CEST          ORNE      *BLANKS
     C     CEZIP         ORNE      *ZEROS
     C                   MOVEL     CEZIP         XXZIP5            5
     C                   MOVE      CEZIP         XXZIP4            4
     C     CECTY         CAT       ',':0         XXAL(I)
     C                   CAT       CEST:1        XXAL(I)
     C                   CAT       XXZIP5:1      XXAL(I)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXAL(I)
     C                   CAT       XXZIP4:0      XXAL(I)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1
     C*
     C*  WRITE THE RECORDS
     C*
     C                   Z-ADD     1             I
     C     I             DOWLE     5
     C*
     C     XXBL(I)       IFEQ      *BLANKS
     C     XXAL(I)       ANDEQ     *BLANKS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEL     XXBL(I)       ENBLIN
     C                   MOVEL     XXAL(I)       ENALIN
     C*
     C                   WRITE     PEN01
     C*
     C*  BUMP UP NECESSARY NUMERIC FIELDS FOR NEXT WRITE IF REQUIRED
     C*
     C                   ADD       1             I
     C                   ADD       1             ENTIME
     C                   ADD       1             ENSEQ#
     C                   ENDDO
     C*
     C                   ENDIF                                                  CECER# <> S#CER#
     C*
     C                   ENDIF                                                  LDENDA <>0,' '
     C*
     C                   ENDSR
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
     C     CESTAT        IFEQ      'I'
     C                   MOVE      'A'           CESTAT
     C                   ELSE
     C                   MOVE      'I'           CESTAT
     C                   ENDIF                                                  CESTAT=I
     C*
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVEL     Q1PGM         CEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEDT
     C*
     C                   MOVEL     DSTIME        CETIME
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
     C*
     C*
     C*  SAVE THE OLD DATA BASE VALUES
     C*
     C                   MOVE      CECER#        S#CER#
     C                   MOVE      CENAME        S#NAME
     C                   MOVE      CEADR1        S#ADR1
     C                   MOVE      CEADR2        S#ADR2
     C                   MOVE      CEZIP         S#CTY
     C                   MOVE      CEST          S#ST
     C                   MOVE      CEZIP         S#ZIP
     C                   MOVE      CEATTN        S#ATTN
     C*
     C                   CLEAR                   CECER#
     C                   CLEAR                   CENAME
     C                   CLEAR                   CEADR1
     C                   CLEAR                   CEADR2
     C                   CLEAR                   CECTY
     C                   CLEAR                   CEST
     C                   CLEAR                   CEZIP
     C                   CLEAR                   CEATTN
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C                   EXSR      CKCHGS
     C*
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
     C*  CERTIFICATE HOLDER # CANNOT BE <= ZERO
     C*
     C  N90SCCER#        IFLE      *ZERO
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCCER#<=*ZERO
     C*
     C*  CHECK TO BE SURE CERT HOLDER # HAS NOT BEEN USED PREVIOUSLY
     C*
     C  N90@@MODE        IFEQ      'A'
     C*
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C** **                SELEC
     C** **      XXTYPE    WHEQ *BLANK
     C***  END   ADD  ***  04/26/06  *********************************
B02  C  N90CEKY          CHAIN     PCE01                              60
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C** **      XXTYPE    WHEQ 'S'
B02  C**N90      CEKY      CHAINPCE01S               60
     C** **                ENDSL
     C***  END   ADD  ***  04/26/06  *********************************
     C*
     C  N90
     CANN60              SETON                                        21  90
     C*
     C                   ENDIF                                                  @@MODE=A
      ***  BEGIN ADD  ***  04/07/05 ***********************************
      *
      *  THIRD PARTY MUST BE (Y/ )
      *
     C  N90SC3PTY        IFNE      'Y'
     C     SC3PTY        ANDNE     *BLANK
     C                   SETON                                        72  90
     C                   ENDIF                                                  SC3PTY
      *
      *
      *****BEG   DEL*****  03/04/11 ***********************************
      *  EXCLUDED OFFICERS MUST BE (Y/ )
      *
     C******  N90      SCEOFL    IFNE 'Y'
     C******     SCEOFL    ANDNE*BLANK
     C******               SETON                     28  90
     C******               ENDIF                           SCEOLF
      *****END   DEL*****  03/04/11 ***********************************
      ***  END   ADD  ***  04/07/05 ***********************************
     C*
     C*  THE NAME CANNOT BE BLANK
     C*
     C  N90SCCNAM        IFEQ      *BLANK
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCCNAM=*BLANK
     C*
     C*  THE ADDRESS LINE CANNOT BE BLANK
     C*
     C  N90SCADR1        IFEQ      *BLANK
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCADR1=*BLANK
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
      ***** BEG ADD ******* 04/07/09 **********************************
     C  N90SCCTRY        IFEQ      *BLANK
     C     SCCTRY        OREQ      'USA'
      ***** END ADD ******* 04/07/09 **********************************
     C  N90SCCTY         IFEQ      *BLANK
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCCTY=*BLANK
      ***** BEG ADD ******* 04/07/09 **********************************
     C                   ENDIF
      ***** END ADD ******* 04/07/09 **********************************
     C*
     C*  THE STATE MUST BE VALID
     C*
      ***** BEG ADD ******* 04/07/09 **********************************
     C  N90SCCTRY        IFEQ      *BLANK
     C     SCCTRY        OREQ      'USA'
      ***** END ADD ******* 04/07/09 **********************************
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        25  90
      ***** BEG ADD ******* 04/07/09 **********************************
     C                   ENDIF
      ***** END ADD ******* 04/07/09 **********************************
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
      ***** BEG ADD ******* 04/07/09 **********************************
     C  N90SCCTRY        IFEQ      *BLANK
     C     SCCTRY        OREQ      'USA'
      ***** END ADD ******* 04/07/09 **********************************
     C  N90SCZIP1        IFLE      *ZERO
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCZIP1<=*ZERO
      ***** BEG ADD ******* 04/07/09 **********************************
     C                   ENDIF
      *
      *  If country code other than USA has been entered.
      *
     C  N90SCCTRY        IFNE      'USA'
     C     SCCTRY        ANDNE     *BLANKS
      *
      *      Country Code must be valid
      *
     C     *IN90         IFNE      *ON
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        90  77
     C                   ENDIF
      *
     C                   ENDIF
      *
      *      USA City should not be entered
      *
     C  N90SCCTY         IFNE      *BLANKS
     C                   SETON                                        90  73
     C                   ENDIF
      *
      *      USA State should not be entered
      *
     C  N90SCST          IFNE      *BLANKS
     C                   SETON                                        90  74
     C                   ENDIF
      *
      *      USA Zip Code should not be entered
      *
     C  N90SCZIP1        IFNE      *ZERO
     C                   SETON                                        90  75
     C                   ENDIF
      *
      *      Province / Postal Code should be entered.
      *
     C  N90SCPROV        IFEQ      *BLANKS
     C                   SETON                                        90  76
     C                   ENDIF
      *
     C                   ENDIF                                                  SCCTRY <> USA
      *
      *  If country code = USA, then "Province/Postal Code" should not
      *  be entered.
      *
     C  N90SCCTRY        IFEQ      'USA'
     C     SCCTRY        OREQ      *BLANKS
      *
     C     SCPROV        IFNE      *BLANKS
     C                   SETON                                        90  78
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 04/07/09 **********************************
      ***** BEG DEL ******* 04/07/09 **********************************
     C*********
     C***ROLLOVER FLAG MUST BE "Y" OR "N"
     C*********
     C*********
     C***THIS*FIELD IS NOT LOADED IF CALLED FROM DEPARTMENT OF
     C***INSURANCE, SO NO EDIT.
     C*********
     C**N90****  SCROLL    IFNE 'Y'
     C*********  SCROLL    ANDNE'N'
     C*********            SETON                     27  90
     C*********            ENDIF                           SCROLL<>Y/N
      *****BEGIN ADD  ***  04/07/05 ***********************************
      *********
      ***Special job (Y/ )
      *********
     C**N90****  SCTYPE    IFNE 'Y'
     C*********  SCTYPE    ANDNE*BLANK
     C*********            SETON                     71  90
     C*********            ENDIF                           SCTYPE (Y/ )
      *****END*  ADD  ***  04/07/05 ***********************************
     C****BEGIN ADD *** 04/26/06 **************************************
      ****BEG*DEL **** 07/16/07 ************************************
     C*********
     C***SCWAIV MUST BE 'Y' or ' '
     C*********
     C*********  WAIVER    IFEQ *ON
     C*****N90*     SCWAIV    IFNE 'Y'
     C*********  SCWAIV    ANDNE*BLANKS
     C*********            SETON                     28  90
     C*********            ENDIF                           SCWAIV<>Y/
     C*********            ENDIF
      ****END*DEL **** 07/16/07 ************************************
     C****END** ADD *** 04/26/06 **************************************
      ***** END DEL ******* 04/07/09 **********************************
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90CEDT          IFGT      SAVEDT
     C     CEDT          OREQ      SAVEDT
     C     CETIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  CEDT>SAVEDT
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      ***** BEG ADD ******* 04/07/09 **********************************
      *
      *  ROLLOVER FLAG MUST BE "Y" OR "N"
      *
      *  THIS FIELD IS NOT LOADED IF CALLED FROM DEPARTMENT OF
      *  INSURANCE, SO NO EDIT.
      *
     C  N90SCROLL        IFNE      'Y'
     C     SCROLL        ANDNE     'N'
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCROLL<>Y/N
      ***** BEG ADD ******* 03/04/11 **********************************
      *
      *  EXCLUDED OFFICERS MUST BE (Y/N/ )
      *
     C  N90SCEOFL        IFNE      'Y'
     C     SCEOFL        ANDNE     *BLANK
     C     SCEOFL        ANDNE     'N'
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCEOLF
      ***** END ADD ******* 03/04/11 **********************************
      *
      *  Special job (Y/ )
      *
     C  N90SCTYPE        IFNE      'Y'
     C     SCTYPE        ANDNE     *BLANK
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCTYPE (Y/ )
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90CEDT          IFGT      SAVEDT
     C     CEDT          OREQ      SAVEDT
     C     CETIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  CEDT>SAVEDT
      ***** END ADD ******* 04/07/09 **********************************
      *
     C                   ENDSR                                                  ERRCK2
      *
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
      ***** BEG ADD ******* 04/07/09 **********************************
     C                   SELECT
      *
      *  Country Code
      *
     C     HFLD          WHENEQ    'SCCTRY'
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        #1PRDS           40
     C                   PARM      '2'           #1CALL            1
     C                   PARM      'CTRY'        #1TBID            4
     C                   PARM      *BLANKS       #1KEY            10
     C                   PARM      *BLANK        #1RTCD            1
      *
     C     #1KEY         IFEQ      *BLANKS
     C                   SETON                                        79
     C                   ELSE
     C                   MOVEL(P)  #1KEY         SCCTRY
     C                   SETON                                        80
     C                   ENDIF
      *
     C                   ENDSL
      *
      ***** END ADD ******* 04/07/09 **********************************
     C                   ENDSR                                                  KJDO
      *
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'A'           CESTAT
     C                   Z-ADD     XXCER#        CECER#
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C                   MOVE      *BLANKS       SCTYPE
      *** BEG DEL **** 07/16/07 ************************************
     C*****                MOVE *BLANKS   SCWAIV
      *** END DEL **** 07/16/07 ************************************
     C***  END ADD ******  04/26/06  *********************************
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   Z-ADD     CECER#        SCCER#
      ***** BEG ADD ******* 04/23/10 **********************************
     C   40              Z-ADD     1             SCREV#
     C  N40              Z-ADD     CEREV#        SCREV#
      ***** END ADD ******* 04/23/10 **********************************
      ***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   MOVEL     CE3PTY        SC3PTY
     C                   MOVEL     CEEOFL        SCEOFL
      ***  END   ADD  ***  04/07/05 ***********************************
     C                   MOVEL     CENAME        SCCNAM
     C                   MOVEL     CEADR1        SCADR1
     C                   MOVEL     CEADR2        SCADR2
     C                   MOVEL     CECTY         SCCTY
     C                   MOVEL     CEST          SCST
     C                   MOVEL     CEZIP         SCZIP1
     C                   MOVE      CEZIP         SCZIP2
     C                   MOVEL     CEATTN        SCATTN
     C                   MOVEL     CERLFL        SCROLL
      ***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   MOVEL     CETYPE        SCTYPE
      ***  END   ADD  ***  04/07/05 ***********************************
     C     *IN32         IFEQ      *ON
     C                   MOVEL     XCDREM        SCREM1
     C                   ELSE
     C                   MOVEL     CEREM1        SCREM1
     C                   ENDIF                                                  *IN32 = *ON
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     C************         MOVELCEREM2    SCREM2
     C                   MOVEL(P)  CEREM2        SCREM2
      ***  BEG   DEL  ***  01/23/09 ***********************************
      *****BEGIN ADD  ***  04/26/06 ***********************************
     C*****      XXCLT#    IFEQ '045'
     C*****      SCREM1    ANDEQ*BLANKS
     C*****                MOVELXXREM1    SCREM1    P
     C*****                MOVELXXREM2    SCREM2    P
     C*****                ENDIF
      *****END   ADD  ***  04/26/06 ***********************************
      ***  END   DEL  ***  01/23/09 ***********************************
      ***  END   CHG  ***  04/07/05 ***********************************
     C                   MOVEL     CEREM3        SCREM3
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     C************         MOVELCEREM4    SCREM4
     C                   MOVEL(P)  CEREM4        SCREM4
      ***  END   CHG  ***  04/07/05 ***********************************
     C                   MOVEL     CEREM5        SCREM5
     C                   MOVE      CESTAT        SCSTAT
     C                   ADD       1             XXCER#
      ***  BEGIN ADD  ***  06/24/08 ***********************************
      ***  BEGIN CHG  ***  07/08/08 ***********************************
     C*
     C*********N90      @@MODE    IFEQ 'A'
     C     @@MODE        IFEQ      'A'
      ***  END   CHG  ***  07/08/08 ***********************************
     C     XXCLT#        ANDEQ     '135'
     C                   MOVEL     'Y'           SC3PTY                         3RD PARTY Y
     C                   MOVEL     'Y'           SCROLL                         ROLLOVER
     C                   ENDIF                                                  @@MODE='A'
     C*
      ***  END   ADD  ***  06/24/08 ***********************************
      ***** BEG ADD ******* 04/07/09 **********************************
     C                   MOVEL(P)  CECTRY        SCCTRY
     C                   MOVEL(P)  CEPROV        SCPROV
      ***** END ADD ******* 04/07/09 **********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
      ***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   MOVEL     SC3PTY        CE3PTY
     C                   MOVEL     SCEOFL        CEEOFL
      ***  END   ADD  ***  04/07/05 ***********************************
     C                   MOVEL     SCCNAM        CENAME
     C                   MOVEL     SCADR1        CEADR1
     C                   MOVEL     SCADR2        CEADR2
     C                   MOVEL     SCCTY         CECTY
     C                   MOVEL     SCST          CEST
     C                   MOVEL     SCZIP1        CEZIP
     C                   MOVE      SCZIP2        CEZIP
     C                   MOVEL     SCATTN        CEATTN
     C                   MOVEL     SCROLL        CERLFL
      ***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   MOVEL     SCTYPE        CETYPE
      ***  END   ADD  ***  04/07/05 ***********************************
     C                   MOVEL     SCREM1        CEREM1
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     C*********            MOVELSCREM2    CEREM2
     C                   MOVEL(P)  SCREM2        CEREM2
      ***  END   CHG  ***  04/07/05 ***********************************
     C                   MOVEL     SCREM3        CEREM3
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     C*********            MOVELSCREM4    CEREM4
     C                   MOVEL(P)  SCREM4        CEREM4
      ***  END   CHG  ***  04/07/05 ***********************************
     C                   MOVEL     SCREM5        CEREM5
     C                   MOVE      SCSTAT        CESTAT
      ***** BEG ADD ******* 04/07/09 **********************************
     C                   MOVEL(P)  SCCTRY        CECTRY
     C                   MOVEL(P)  SCPROV        CEPROV
      ***** END ADD ******* 04/07/09 **********************************
      ***** BEG ADD ******* 04/23/10 **********************************
     C     SCREV#        IFEQ      0
     C                   Z-ADD     1             CEREV#
     C                   ELSE
     C                   Z-ADD     SCREV#        CEREV#
     C                   ENDIF
      ***** END ADD ******* 04/23/10 **********************************
     C*
     C                   ENDSR                                                  MVSCDB
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C*===============================================================
     C* MVSCD2 - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C** **
     C** **      MVSCD2    BEGSR
     C** **
     C** **                CLEARDBRCDF
     C** **
     C** **                Z-ADDELCO#     CECO#
     C** **                Z-ADDELFND     CEFND
     C** **                Z-ADDELEMP#    CEEMP#
     C** **                Z-ADDELDIV     CEDIV
     C** **                Z-ADDELFYR     CEFYR
     C** **                Z-ADDSCCER#    CECER#
     C** **
     C** **                MOVELSCCNAM    CENAME
     C** **                MOVELSCADR1    CEADR1
     C** **                MOVELSCADR2    CEADR2
     C** **                MOVELSCCTY     CECTY
     C** **                MOVELSCST      CEST
     C** **                MOVELSCZIP1    CEZIP
     C** **                MOVE SCZIP2    CEZIP
     C** **                MOVELSCATTN    CEATTN
     C** **                MOVEL*BLANKS   CEREM1
     C** **                MOVEL*BLANKS   CEREM2
     C** **                MOVEL*BLANKS   CEREM3
     C** **                MOVEL*BLANKS   CEREM4
     C** **                MOVEL*BLANKS   CEREM5
     C** **                MOVE SCSTAT    CESTAT
     C** **                MOVE 'A'       CERLFL
     C** **                MOVEL'S'       CETYPE    P
     C** **
     C** **                MOVELQ1USER    CEIUSR
     C** **                MOVELQ1USER    CEUSER
     C** **                MOVELQ1PGM     CEIPGM
     C** **                MOVELQ1PGM     CEPGM
     C** **
     C** **                TIME           DSDTTM
     C** **
     C** **                MOVELDSTIME    CEITM
     C** **                MOVELDSTIME    CETIME
     C** **
     C** **                Z-ADDDSDATE    SRDTI
     C** **                CALL 'SRCVTDAT'DTCYMD
     C** **                MOVELSRDTO     CEIDT
     C** **                MOVELSRDTO     CEDT
     C** **
     C** **                WRITEDBRCDF
     C** **                FEOD WMCEP
     C** **
     C** **                ENDSR                           MVSCD2
     C***  END ADD ******  04/26/06  *********************************
     C*
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
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*** BEGIN CHG *** 06/07/99 **************************************
     C                   MOVE      *ON           *INLR
     C*** END   CHG *** 06/07/99 **************************************
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
      *
     C***  BEGIN ADD  ***  04/26/06  *********************************
      *** BEG DEL **** 07/16/07 ************************************
     C*****      *LIKE     DEFN *IN55     WAIVER
     C*****                MOVE *ON       *IN55
     C*****                MOVE *OFF      WAIVER
      *** END DEL **** 07/16/07 ************************************
     C***  END ADD ******  04/26/06  *********************************
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C*** BEGIN ADD *** 04/26/06 **************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
      *
     C** **      Q1PRMS    IFGE 6
     C** **                MOVEL@@TYPE    XXTYPE    P
     C** **                ELSE
     C** **                MOVEL*BLANKS   XXTYPE
     C** **                ENDIF                           Q1PRMS>=7
     C** **
     C** **                MOVELOPTNM1    OPTNM  40 P
     C** **                MOVELHEAD1     SCCTHD    P
      *
     C                   SELECT
     C** **      XXTYPE    WHEQ 'S'
     C** **                MOVE *ON       *IN55
     C** **                MOVELOPTNMS    OPTNM     P
     C** **                MOVELHEADS     SCCTHD    P
      *
     C** **      XXTYPE    WHEQ *BLANKS
     C** **      XXCLT#    ANDEQ'045'
      *** BEG DEL **** 07/16/07 ************************************
     C*****      XXCLT#    WHEQ '045'
     C*****      *IN40     ANDEQ*ON
     C*****                MOVE *ON       WAIVER
     C*****                MOVE *OFF      *IN55
      *** END DEL **** 07/16/07 ************************************
     C                   ENDSL
     C*** END   ADD *** 04/26/06 **************************************
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
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
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     *IN09         ANDNE     *ON
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
     C*  CHAIN TO RATE SET CODE FILE
     C*
     C     RSKY          CHAIN     PRS01                              30
     C     *IN30         IFEQ      *OFF
     C*
     C     XCKY          SETGT     PXC01
     C*
     C     XCKY1         READPE    PXC01                                  31
     C*
     C     *IN31         IFEQ      *OFF
     C     XCDREM        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  XCDREM IFNE *BLANKS
     C*
     C                   ENDIF                                                  *IN30 = *OFF
     C*
     C*  DEFAULT LATEST # IN ADD MODE
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C                   MOVE      *ON           *IN60
     C*
     C***  BEGIN ADD  ***  04/26/06  *********************************
     C** **                SELEC
     C** **      XXTYPE    WHEQ *BLANKS
     C     CEKY1         SETGT     PCE01
     C     CEKY1         READPE    PCE01                                  60
     C***  END ADD ** ***  04/26/06  *********************************
     C** **      XXTYPE    WHEQ 'S'
     C** **      CEKY1     SETGTPCE01S
     C** **      CEKY1     REDPEPCE01S                   60
     C** **                ENDSL
     C***  END ADD ** ***  04/26/06  *********************************
     C*
     C     *IN60         IFEQ      *OFF
     C     CECER#        ADD       1             XXCER#
     C                   ELSE
     C                   Z-ADD     1             XXCER#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C*  SAVE THE OLD DATA BASE VALUES
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   S#CER#
     C                   CLEAR                   S#NAME
     C                   CLEAR                   S#ADR1
     C                   CLEAR                   S#ADR2
     C                   CLEAR                   S#CTY
     C                   CLEAR                   S#ST
     C                   CLEAR                   S#ZIP
     C                   CLEAR                   S#ATTN
     C*
     C                   ELSE
     C*
     C                   MOVE      CECER#        S#CER#
     C                   MOVE      CENAME        S#NAME
     C                   MOVE      CEADR1        S#ADR1
     C                   MOVE      CEADR2        S#ADR2
     C                   MOVE      CECTY         S#CTY
     C                   MOVE      CEST          S#ST
     C                   MOVE      CEZIP         S#ZIP
     C                   MOVE      CEATTN        S#ATTN
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*
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
     C                   ENDSR
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
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVEL     Q1PGM         CEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEDT
     C                   MOVEL     DSTIME        CETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C                   EXSR      CKCHGS
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
      *
     C*** BEGIN ADD *** 04/26/06 **************************************
      *** BEG DEL **** 07/16/07 ************************************
     C*
     C*  CREATE A WAIVER ENTRY
     C*
     C*****      SCWAIV    IFEQ 'Y'
      *
     C** **      CEKY1     SETGTPCE01S
     C** **      CEKY1     REDPEPCE01S                   60
     C** **      *IN60     IFEQ *OFF
     C** **      CECER#    ADD  1         X2CER#
     C** **                ELSE
     C** **                Z-ADD1         X2CER#
     C** **                ENDIF                           *IN60=*OFF
      *
     C*****                CLEARPWV01
      *
      *  SAVE CERTIFICATE HOLDER # IN WAIVER ENTRY BEING CREATED
      *
     C*****                Z-ADDELCO#     WVCO#
     C*****                Z-ADDELFND     WVFND
     C*****                Z-ADDELEMP#    WVEMP#
     C*****                Z-ADDELDIV     WVDIV
     C*****                Z-ADDELFYR     WVFYR
     C*****                Z-ADDSCCER#    WVWAV#
      *
     C*****                MOVELSCCNAM    WVNAME
     C*****                MOVELSCADR1    WVADR1
     C*****                MOVELSCADR2    WVADR2
     C*****                MOVELSCCTY     WVCTY
     C*****                MOVELSCST      WVST
     C*****                MOVELSCZIP1    WVZIP
     C*****                MOVE SCZIP2    WVZIP
     C*****                MOVE SCSTAT    WVSTAT
     C*
     C*****                MOVELQ1USER    WVIUSR
     C*****                MOVELQ1USER    WVUSER
     C*****                MOVELQ1PGM     WVIPGM
     C*****                MOVELQ1PGM     WVPGM
     C*
     C*****                TIME           DSDTTM
     C*
     C*****                MOVELDSTIME    WVITM
     C*****                MOVELDSTIME    WVTIME
     C*
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     WVIDT
     C*****                MOVELSRDTO     WVDT
     C** **                MOVELSRDTO     WVEFDT
     C** **                Z-ADDX2CER#    WVEND#
     C*
     C*****                WRITEPWV01
     C*
     C*****                FEOD WMWVP
      *
     C*****                ENDIF
      *** END DEL **** 07/16/07 ************************************
     C***  END ADD ******  04/26/06  *********************************
      *
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     ELCO#         CECO#
     C                   Z-ADD     ELFND         CEFND
     C                   Z-ADD     ELEMP#        CEEMP#
     C                   Z-ADD     ELDIV         CEDIV
     C                   Z-ADD     ELFYR         CEFYR
     C                   Z-ADD     SCCER#        CECER#
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CEIUSR
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVEL     Q1PGM         CEIPGM
     C                   MOVEL     Q1PGM         CEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        CEITM
     C                   MOVEL     DSTIME        CETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEIDT
     C                   MOVEL     SRDTO         CEDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMCEP
     C*
     C*  Create mid-term endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C                   ENDSR                                                  WRITER
