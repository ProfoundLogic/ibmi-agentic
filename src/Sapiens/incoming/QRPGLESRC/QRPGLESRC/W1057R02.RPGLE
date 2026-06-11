     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *===============================================================*
      *===============================================================*
     H/TITLE W1057R02 - OWNER/OFFICER INFORMATION
     H Bnddir('WTBNDDIR')
      *===============================================================*
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE OWNER/OFFICER FILE.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W1057R - WORK WITH OWNERS/OFFICERS                         *
     H*    W0051R11 - MID TERM NAME CHANGES                           *
     H*    W0051R14 - MID TERM LEGAL STATUS CHANGES                   *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/20/97  PROGRAMMER: C:33  PROJECT: 60AND          *
      *===============================================================*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H*  07/05/01 C:90  PROJECT: SEQ#28011                            *
     H*                 Allow edits in ADD and CHANGE modes.          *
     H*                                                               *
     H*  10/18/00 C:90  PROJECT: WC877                                *
     H*                 CHANGED EDIT ON CLASS CODE TO VALIDATE CODE   *
     H*                 FIRST BY CO#/CLASS. IF CODE DOES NOT EXIST    *
     H*                 THEN VALIDATE BASED ON CO#/RTCD/CLASS.        *
     H*                                                               *
     H*  12/03/99 C:118 PROJECT: WC577                                *
     H*                 In ADD F10 works, but not displayed,fixed.    *
     H*                                                               *
     H*  06/29/99 C:33 PROJECT: WC577  PER: C:09                      *
     H*                MOVE PERCENTAGE EDIT TO THE END OF W1057R.     *
     H*                                                               *
     H* 04/27/99 C:118   PROJECT: WC577
     H*                  MODIFIED PROGRAM SO F10=SEARCH RETURNS A
     H*                  VALUE IN ADD, CHANGE AND UPDATE MODE.
     H*                  MODIFIED F KEYS IN DISPLAY FILE TO
     H*                  CORRESPOND TO APPROPRIATE MODE.
     H*
     H* 04/13/99 C:09    PROJECT: CM027                               *
     H*                  MIDTERM OWNER/OFFICER ENDORSEMENT REWRITE    *
     H*                                                               *
     H* 02/22/99  C:90   SUPPORT: WA048/16609                         *
     H*                  ERRCK WAS USING EFFECTIVE DT. (XXEFDT)       *
     H*                  INSTEAD OF THE EXPIRATION DT. (XXEXDT).      *
     H*                                                               *
     H* 01/27/99  C:90   PROJECT: WC877                               *
     H*                  ADD NEW FIELD 'PAYROLL CLASSIFICATION CODE'  *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 11/12/98  C:78   PROJECT: WC873                               *
     H*                  CHANGE FLAG/PROMPT TO NONDISPLAY. (CORP Y/N) *
     H*                  SCCPFL.                                      *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H* 08/07/98  C:78  SUPPORT DR095/12876                           *
     H*                 LOAD VALUE 'Q' OR BLANKS FOR QUOTE MODE.      *
     H*                                                               *
     H* 06/10/98  C:77  PROJECT: SUPPORT (WA091 #12193)               *
     H*                 CHG PROGRAM TO CORRECTLY SEQUENCE OWNERS/OFF. *
     H*                                                               *
     H* 04/06/98  C:78  PROJECT: WC825                                *
     H*                 ALLOW ACCESS TO DARTS. F14 ADDED.             *
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
     H*  01/28/98  C:57  PROJECT: CM027                               *
     H*                 -SETUP FOR MID TERM ENDORSEMENT TRACKING IF   *
     H*                  CALLED FROM THAT PROCESS.                    *
     H*                                                               *
     H*  12/31/97  C:33  PROJECT: WC577                               *
     H*                 -DO NOT CHECK PERCENTAGES PER ENTRY, IN ADD   *
     H*                  MODE.  IN ADD MODE CHECK IN PGMEND.          *
     H*                 -DEFAULT CORP FLAG TO 'N' IN ADD MODE.        *
      *===============================================================*
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - ENTITY/LOCATION                                         *
     H*  42 - CHANGE INC/EXC                                          *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATED DURING USAGE                         *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  80 - (WC30015) Last name or SS# must be entered              *
     H*  71 - (WCR0500) Invalid Date of Birth                         *
     H*  72 - (WCR0589) Inc/Exc Flag must be 'I', 'E' or blank        *
     H*  73 - (WC30010) Include/Exclude flag must be 'I' or 'E'       *
     H*  74 - (WCR0500) Invalid Effective date                        *
     H*  75 - (WCR0585) Include/Exclude flag mus be 'I' or 'E' w/Date *
     H*  76 - (WCR0500) Invalid Expiration date                       *
     H*  32 - (WCR0539) Payroll class is invalid                      *
     H*  34 - (WCR1058) Cursor is not on a proper field for the search*
     H*  POSITION CURSOR:                                             *
     H*  33 - PC @ PAYROLL CLASS CODE                                 *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
      *===============================================================*
     FW1057F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMOOP     UF A E             DISK
     F                                     RENAME(POO01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWMALP     IF   E             DISK
     F*** BEG ADD ***** 01/28/98 ***********************************************
     FWTMYP     IF   E             DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     F*** END ADD ***** 01/28/98 ***********************************************
     F*** BEGIN ADD *** t6/10/98 ***********************************************
      *** BEGIN CHG *** 10/21/99 ***********************************************
     F*****WMOOL7  UF  E           K        DISK
     F*****       POO01                             KRENAMEPOO02
     FWMOOL7    IF   E           K DISK
     F                                     RENAME(POO01:POO02)
      *** END   CHG *** 10/21/99 ***********************************************
     F*** END ADD ***** 06/10/98 ***********************************************
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     FWDELL     IF   E           K DISK
      **** END   ADD **** 11/23/98 *********************************************
     F**** BEGIN ADD **** 01/27/99 ************************************
     FWMPRL1    IF   E           K DISK
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PPR02)
     F**** END   ADD **** 01/27/99 ************************************
     F**** BEGIN ADD **** 10/18/00 ************************************
     FWMPRL     IF   E           K DISK
     F                                     RENAME(PPR01:PPR03)
     F**** END   ADD **** 10/18/00 ************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
      /COPY SCOPYMT01D
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D*
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
     D*** BEG ADD ***** 01/28/98 ***********************************************
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*** END ADD ***** 01/28/98 ***********************************************
     D*** BEGIN ADD *** 04/06/98 ***********************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*** END   ADD *** 04/06/98 ***********************************************
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Owner/Officer Inform-
     D                                     ation')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*** BEG ADD ***** 01/28/98 ***********************************************
     D AGTAPI          C                   CONST('WA002R')
     D*** END ADD ***** 01/28/98 ***********************************************
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*** END ADD ***** 01/28/98 ***********************************************
     C*
     C*  OWNER/OFFICER FILE
     C*
     C     OOKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*** BEG ADD ***** 01/28/98 ***********************************************
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      **** END   ADD **** 10/14/98 *********************************************
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
     C*** END ADD ***** 01/28/98 ***********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    ENDIV
     C                   KFLD                    ENFYR
      **** END   ADD **** 11/23/98 *********************************************
     C**** BEGIN ADD **** 01/27/99 ************************************
     C     ELKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C* KEY LIST FOR THE PAYROLL CLASSIFICATION FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCPRCD
      *
     C**** END   ADD **** 01/27/99 ************************************
     C**** BEGIN ADD **** 10/18/00 ************************************
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCPRCD
     C*
     C**** END   ADD **** 10/18/00 ************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@ELFL            1
      ****** BEGIN ADD ****** 08/07/98 ********************************
     C                   PARM                    @@QTE             1
      ****** END   ADD ****** 08/07/98 ********************************
     C*
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    OOEXDT        XXEXDT
     C     *LIKE         DEFINE    OOEFDT        XXEFDT
     C     *LIKE         DEFINE    OOEFDT        XXFREF
     C     *LIKE         DEFINE    OOEXDT        SVEXDT
     C     *LIKE         DEFINE    OOEFDT        SVEFDT
     C     *LIKE         DEFINE    OOOFPC        SVOFPC
     C     *LIKE         DEFINE    OOOFPC        TTLPCT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C     *LIKE         DEFINE    OOOLSN        S#OLSN
     C     *LIKE         DEFINE    OOOFSN        S#OFSN
     C     *LIKE         DEFINE    OOOSS#        S#OSS#
     C     *LIKE         DEFINE    OOOFPC        S#OFPC
     C     *LIKE         DEFINE    OOEXFL        S#EXFL
     C     *LIKE         DEFINE    OOCPFL        S#CPFL
     C     *LIKE         DEFINE    OODOB         S#DOB
     C     *LIKE         DEFINE    OOTITL        S#TITL
     C     *LIKE         DEFINE    OOEFDT        S#EFDT
     C     *LIKE         DEFINE    OOEXDT        S#EXDT
     C*** END ADD ***** 01/28/98 ***********************************************
      ******* BEGIN ADD ****** 04/06/98 ****************************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ******* END   ADD ****** 04/06/98 ****************************************
     C*** BEGIN ADD *** 06/10/98 ***********************************************
     C     *LIKE         DEFINE    OOSEQ#        XXSEQ#
     C*** END ADD ***** 06/10/98 ***********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                2 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*** BEGIN DEL *** 04/13/99 ***********************************************
     C*
     C*  **** SUBROUTINE NO LONGER BEING USED ****
     C*===============================================================
     C*  CKCHGS - SUBROUTINE TO CREATE MID-TERM ENDORSEMENT RECORDS
     C*===============================================================
     C     CKCHGS        BEGSR
     C*
     C************NAMVAR   DEFN *LDA      SVLDA
     C************LOCK     IN   *NAMVAR
     C************         OUT  *NAMVAR
     C*** END DEL ***** 04/13/99 ***********************************************
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C*  CHK TO SEE IF CALLED AS PART OF ADDITION OF A DIVISION
     C*
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W0051R19'    ##PGM            10
     C                   PARM      *BLANKS       ##EXST            1
     C*
     C*  ONLY WRITE A WPENP REC IF NOT FROM ADDITION OF DIVISION
     C*
     C     ##EXST        IFEQ      *OFF
     C*
     C     OOOLSN        IFNE      S#OLSN
     C     OOOFSN        ORNE      S#OFSN
     C     OOOFPC        ORNE      S#OFPC
     C     OOEXFL        ORNE      S#EXFL
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     @@RRN2        CHAIN     WMEMP                              60
     C*
     C                   CALL      AGTAPI        AGTPL
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
     C*
     C                   MOVE      'Y'           ENNOFL
     C     *IN40         IFEQ      *ON
     C                   MOVEL(P)  DXOOAD        ENRMCD
     C                   ELSE
     C                   MOVEL     DXOONC        ENRMCD
     C                   ENDIF
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
     C*  BUILD UP BEFORE LINE
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVE      *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVEL(P)  S#OLSN        XXWK25           25
     C     S#OFSN        IFNE      *BLANKS
     C                   CAT       ',':0         XXWK25
     C                   CAT       S#OFSN:1      XXWK25
     C                   ENDIF
     C     XXWK25        CAT(P)    ',':0         ENBLIN
     C                   MOVEL     S#OFPC        XXWK03            3
     C                   MOVE      S#OFPC        XXWK02            2
     C                   CAT       XXWK03:1      ENBLIN
     C                   CAT       '.':0         ENBLIN
     C                   CAT       XXWK02:0      ENBLIN
     C                   CAT       ',':0         ENBLIN
     C                   CAT       S#EXFL:1      ENBLIN
     C     S#TITL        IFNE      *BLANKS
     C                   CAT       ',':0         ENBLIN
     C                   CAT       S#TITL:1      ENBLIN
     C                   ENDIF
     C                   ENDIF
     C*
     C*  BUILD UP AFTER LINE
     C*
     C                   MOVEL     *BLANKS       ENALIN
     C                   MOVEL(P)  OOOLSN        XXWK25           25
     C     OOOFSN        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    OOOFSN:1      XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         ENALIN
     C                   ENDIF
     C                   MOVEL     OOOFPC        XXWK03            3
     C                   MOVE      OOOFPC        XXWK02            2
     C     OOOFPC        IFNE      *ZERO
     C     ENALIN        ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      ENALIN
     C                   CAT       '.':0         ENALIN
     C                   CAT       XXWK02:0      ENALIN
     C                   ENDIF
     C     OOEXFL        IFNE      *BLANK
     C                   CAT(P)    ',':0         ENALIN
     C                   CAT       OOEXFL:1      ENALIN
     C                   ENDIF
     C     OOTITL        IFNE      *BLANK
     C                   CAT(P)    ',':0         ENALIN
     C                   CAT       OOTITL:1      ENALIN
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60 OFEQ *OFF
      **** END   ADD **** 11/23/98 *********************************************
     C*
     C                   WRITE     PEN01
     C*
     C                   ENDIF                                                  OOOLSN <> S#0LSN
     C*
     C                   ENDIF                                                  ##EXST = *OFF
     C*
     C                   ENDIF                                                  LDENDA <>0,' '
     C*
     C                   ENDSR
     C*** END ADD ***** 01/28/98 ***********************************************
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C*
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*
     C*  SAVE THE OLD DATA BASE VALUES
     C*
     C                   MOVE      OOOLSN        S#OLSN
     C                   MOVE      OOOFSN        S#OFSN
     C                   MOVE      OOOSS#        S#OSS#
     C                   MOVE      OOOFPC        S#OFPC
     C                   MOVE      OOEXFL        S#EXFL
     C                   MOVE      OOCPFL        S#CPFL
     C                   MOVE      OODOB         S#DOB
     C                   MOVE      OOTITL        S#TITL
     C                   MOVE      OOEFDT        S#EFDT
     C                   MOVE      OOEXDT        S#EXDT
     C*
     C                   CLEAR                   OOOLSN
     C                   CLEAR                   OOOFSN
     C                   CLEAR                   OOOSS#
     C                   CLEAR                   OOOFPC
     C                   CLEAR                   OOEXFL
     C                   CLEAR                   OOCPFL
     C                   CLEAR                   OODOB
     C                   CLEAR                   OOTITL
     C                   CLEAR                   OOEFDT
     C                   CLEAR                   OOEXDT
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C*** BEGIN DEL *** 04/13/99 ***********************************************
     C*********************EXSR CKCHGS
     C*** END DEL ***** 04/13/99 ***********************************************
     C*** END ADD ***** 01/28/98 ***********************************************
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
     C*  THE LAST NAME CANNOT BE BLANK
     C*
     C  N90SCOLNM        IFEQ      *BLANK
     C     SCOSS#        ANDEQ     *BLANK
     C                   SETON                                        80  90
     C                   ENDIF                                                  SCOLNM=*BLANK=SCOSS#
     C*
     C*  DATE OF BIRTH MUST BE VALID, IF ENTERED
     C*
     C  N90SCDOB         IFNE      *ZERO
     C*
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDOB
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCDOB<>*ZERO
     C*
     C*  CORPORATION FLAG MUST BE Y/N IF NOT BLANK.
     C*
     C  N90SCCPFL        IFNE      'Y'
     C     SCCPFL        ANDNE     'N'
     C     SCCPFL        ANDNE     ' '
     C                   SETON                                        81  90
     C                   ENDIF                                                  SCLNAM<>*BLANKS
     C*
     C*  COVERAGE FLAG MUST BE I, E, OR BLANK
     C*
     C**** BEGIN CHG **** 07/05/01 ************************************
     C**N90*40***SCOLNM    IFNE *BLANKS
     C  N90SCOLNM        IFNE      *BLANKS
     C**** END   CHG **** 07/05/01 ************************************
     C     SCEXFL        ANDNE     'I'
     C     SCEXFL        ANDNE     'E'
     C     SCEXFL        ANDNE     *BLANK
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCLNAM<>*BLANKS
     C*
     C*  IF EMPLOYER TYPE OF BUSINESS = 1 (SOLE PROPRIETORSHIP) OR
     C*  2 (PARTNERSHIP) FLAG MUST BE 'I' OR 'E'
     C*
     C**** BEGIN CHG **** 07/05/01 ************************************
     C**N90*40***EMTPOB    IFEQ '1'
     C  N90EMTPOB        IFEQ      '1'
     C**** END   CHG **** 07/05/01 ************************************
     C     EMTPOB        OREQ      '2'
     C*
     C     SCEXFL        IFEQ      *BLANK
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCEXFL=*BLANK
     C*
     C                   ENDIF                                                  EMTPOB=1/2
     C*
     C*  IF FLAG IS 'I' OR 'E', OR A CHANGE OF INC/EXC, A VALID
     C*  EFFECTIVE DATE IS REQUIRED
     C*
     C**** BEGIN CHG **** 07/05/01 ************************************
     C**N90*40***SCEXFL    IFEQ 'I'
     C  N90SCEXFL        IFEQ      'I'
     C**** END   CHG **** 07/05/01 ************************************
     C     SCEXFL        OREQ      'E'
     C     *IN42         OREQ      *ON
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  *IN42=*ON/SCEXFL=I/E
     C*
     C*  IF FLAG IS BLANK, THEN AN EFFECTIVE DATE MUST NOT BE ENTERED,
     C*  IN ADD MODE
     C*
     C  N90
     CAN 40SCEXFL        IFEQ      *BLANK
     C     SCEFDT        ANDNE     *ZERO
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCEXFL=*BLANK
     C*
     C*  EFFECTIVE DATE MUST BE ENTERED/VALID, IN OPT 10 MODE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C  N90
     CAN 42SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCEXFL=*BLANK
     C*
     C*  EXPIRATION DATE MUST BE VALID, IF ENTERED
     C*
     C  N90SCEXDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        76  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCEXDT<>*ZERO
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C*  EFFECTIVE DATE MUST BE GREATER THAN FROM EFFECTIVE DATE, OPT10
     C*
     C  N90
     CAN 42XXEFDT        IFLT      XXFREF
     C                   SETON                                        78  90
     C                   ENDIF                                                  XXEFDT<XXFREF
     C*
     C*  DATES MUST BE IN ASCENDING ORDER
     C*
     C  N90XXEXDT        IFLT      XXEFDT
     C     XXEXDT        ANDGT     *ZERO
     C                   SETON                                        77  90
     C                   ENDIF                                                  *ZERO<XXEXDT<XXEFDT
     C*
     C*  EXPIRATION DATE CANNOT BE LESS THAT POLICY INITIAL EFFECTIVE
     C*  DATE
     C*
     C**** BEGIN CHG **** 02/22/99 ************************************
     C**N90      XXEFDT    IFLT EMEFD1
     C  N90XXEXDT        IFLT      EMEFD1
     C**** END   CHG **** 02/22/99 ************************************
     C     XXEXDT        ANDGT     *ZERO
     C                   SETON                                        79  90
     C                   ENDIF                                                  *ZERO<XXEFDT<EMEFD1
     C***  BEGIN DEL  ***  06/29/99  **********************************
     C**********
     C***PERCENTAGES MUST BE BETWEEN 99-100%
     C**********
     C********** *IN90     IFEQ *OFF
     C*****BEGIN ADD  ***  12/31/97  *********************************
     C********** *IN40     ANDEQ*OFF
     C*****END** ADD  ***  12/31/97  *********************************
     C**********
     C********** SCOFPC    IFNE SVOFPC
     C**********
     C**********           CALL 'W1057R01'
     C**********           PARM           @@RRN2
     C**********           PARM           @@ELFL
     C**********           PARM           ##ERR   1
     C**********           PARM           XXEFDT
     C**********           PARM           XXEXDT
     C**********           PARM           SCOFPC
     C**********           PARM           @@RRN
     C**********
     C********** ##ERR     IFEQ *ON
     C**********           SETON                         90
     C**********           ENDIF                           ##ERR=*ON
     C**********
     C**********           Z-ADDSCOFPC    SVOFPC
     C**********
     C**********           ENDIF                           SCOFPC<>SVOFPC
     C**********
     C**********           ENDIF                           *IN90=*OFF
     C***  END   DEL  ***  06/29/99  **********************************
     C**** BEGIN ADD **** 10/18/00 ************************************
     C*
     C*  IF ENTERED, PAYROLL CLASS CODE MUST BE VALID
     C*
     C  N90SCPRCD        IFNE      *ZEROS
     C     ELRTCD        IFNE      *BLANKS
     C     PRKY          CHAIN     PPR01                              60
     C   60              SETON                                        32  90
     C                   ELSE
     C     PRKY1         CHAIN     PPR03                              60
     C   60              SETON                                        32  90
     C                   ENDIF                                                  ELRTCD
     C                   ENDIF                                                  SCPRCD
     C**** END   ADD **** 10/18/00 ************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90OODT          IFGT      SAVEDT
     C     OODT          OREQ      SAVEDT
     C     OOTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  OODT>SAVEDT
     C**** BEGIN DEL **** 10/18/00 ************************************
     C**** BEGIN ADD **** 01/27/99 ************************************
     C*
     C*  IF ENTERED, PAYROLL CLASS CODE MUST BE VALID
     C*
     C**N90      SCPRCD    IFNE *ZEROS
     C********** PRKY      CHAINPPR01                60
     C********** *IN60     IFEQ *ON
     C**********           SETON                     32  90
     C**********           ENDIF
     C**********           ENDIF
     C**** END   ADD **** 01/27/99 ************************************
     C**** END   DEL **** 10/18/00 ************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90OODT          IFGT      SAVEDT
     C     OODT          OREQ      SAVEDT
     C     OOTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  OODT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90OODT          IFGT      SAVEDT
     C     OODT          OREQ      SAVEDT
     C     OOTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  OODT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C**** BEGIN ADD **** 01/27/99 ************************************
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCPRCD'
     C***** BEGIN ADD **** 04/27/99 ***********************************
     C     *IN42         IFEQ      *OFF
     C***** END ADD ****** 04/27/99 ***********************************
      *
     C                   MOVE      *ZEROS        $$RRN
     C*
     C                   CALL      'W0630R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    $$RRN             9 0
     C                   PARM      ELRTCD        @@RTCD            3
     C                   PARM      2             @@VIEW            1 0
      *
     C***** BEGIN DEL **** 04/27/99 ***********************************
     C********** *INKC     IFEQ *ON
     C**********           MOVE *ON       *INKC
     C***** END DEL ****** 04/27/99 ***********************************
     C***** BEGIN CHG **** 04/27/99 ***********************************
     C**********           ENDIF                           QQRTC=A
     C                   ENDIF                                                  *IN42 IFEQ
     C***** END CHG ****** 04/27/99 ***********************************
     C*
     C     *INKL         IFEQ      *ON
     C     $$RRN         OREQ      *ZEROS
      *
     C                   MOVE      *ON           *IN33
      *
     C                   ELSE
      *
     C     $$RRN         CHAIN     PPR02                              99
      *
     C                   MOVE      PRCLAS        SCPRCD
     C                   MOVEL     PRDESC        SCPCDS
     C                   ENDIF
     C*
     C***** BEGIN CHG **** 04/27/99 ***********************************
     C********** HFLD      WHNE 'SCPRCD'
     C                   OTHER
     C***** END CHG ****** 04/27/99 ***********************************
     C                   SETON                                        34  90
     C*
     C                   ENDSL
     C**** END   ADD **** 01/27/99 ************************************
     C*
     C                   ENDSR                                                  KJDO
     C*
      ***  BEGIN ADD  ***  04/06/98  *********************************
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
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      ****** BEGIN ADD ****** 08/07/98 ********************************
     C                   MOVE      XXQTE         QCQTE
      ****** END   ADD ****** 08/07/98 ********************************
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C***  END   ADD  ***  04/06/98  *********************************
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C***  BEGIN ADD  ***  12/31/97  *********************************
     C   40              MOVEL     'N'           OOCPFL
     C***  END   ADD  ***  12/31/97  *********************************
     C*
     C                   MOVEL     OOOLSN        SCOLNM
     C                   MOVEL     OOOFSN        SCOFNM
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      OOOSS#        SCOSS#
      /Free
       SCOSS# = MaskData('SSN':'OOOSS#':OOOSS#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C                   MOVEL     OOCPFL        SCCPFL
     C*
     C                   Z-ADD     OODOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOB
     C*
     C                   Z-ADD     OOOFPC        SCOFPC
     C                   Z-ADD     *ZEROS        SVOFPC
     C                   MOVEL(P)  OOTITL        SCTITL
     C                   MOVEL     OOEXFL        SCEXFL
     C**** BEGIN ADD **** 01/27/99 ************************************
     C                   Z-ADD     OOCLAS        SCPRCD
     C*
     C***** BEGIN ADD **** 04/27/99 ***********************************
     C                   MOVE      *BLANKS       SCPCDS
     C***** END ADD ****** 04/27/99 ***********************************
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL(P)  PRDESC        SCPCDS
     C**** END   ADD **** 01/27/99 ************************************
     C*
     C     OOEXFL        IFEQ      'E'
     C                   MOVEL     'I'           SCTOFL
     C                   ELSE
     C                   MOVEL     'E'           SCTOFL
     C                   ENDIF                                                  OOEXFL=E
     C*
     C     *IN42         IFEQ      *OFF
     C*
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFREF
     C*
     C                   Z-ADD     OOEFDT        XXFREF
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   Z-ADD     *ZEROS        SCEXDT
     C*
     C                   ENDIF                                                  *IN42=*OFF
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCOLNM        OOOLSN
     C                   MOVEL     SCOFNM        OOOFSN
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      SCOSS#        OOOSS#
      /Free
       OOOSS# = UnMaskData('SSN':'OOOSS#':SCOSS#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C                   Z-ADD     SCOFPC        OOOFPC
     C                   MOVEL     SCCPFL        OOCPFL
     C*
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OODOB
     C*
     C                   MOVEL     SCTITL        OOTITL
     C                   MOVEL     SCEXFL        OOEXFL
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OOEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OOEXDT
     C*
     C**** BEGIN ADD **** 01/27/99 ************************************
     C                   Z-ADD     SCPRCD        OOCLAS
     C**** END   ADD **** 01/27/99 ************************************
     C**** BEGIN ADD **** 04/13/99 ************************************
      *
      *** BEGIN DLT *** 10/21/99 ***********************************************
     C********** *IN40     IFEQ *ON
     C**********           MOVE 'A'       OOSTAT
     C**********           ELSE
     C********** *IN42     IFEQ *ON
     C**********           MOVE 'T'       OOSTAT
     C**********           ELSE
     C**********           MOVE 'C'       OOSTAT
     C**********           END
     C**********           END
     C**********
     C***FLAG*ANY CHANGES MADE TO OWNERS
     C**********
     C********** OOOFPC    IFNE *ZEROS
     C**********           MOVE 'Y'       XXCGFL
     C**********           END
     C**** END ADD ****** 04/13/99 ************************************
      *** END   DLT *** 10/21/99 ***********************************************
      *
      *** BEGIN ADD *** 10/21/99 ***********************************************
     C                   MOVE      'A'           OOSTAT
      *** END   ADD *** 10/21/99 ***********************************************
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
      ********* BEGIN ADD ***** 04/06/98 ******************************
     C     *INKN         CASEQ     *ON           KNDO                     90
      ********* END   ADD ***** 04/06/98 ******************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C***  BEGIN DEL  ***  06/29/99  *********************************
     C*****BEGIN ADD  ***  12/31/97  *********************************
     C********** *IN40     IFEQ *ON
     C********** OFFCNT    ANDGT*ZERO
     C**********
     C**********           CALL 'W1057R01'
     C**********           PARM           @@RRN2
     C**********           PARM           @@ELFL
     C**********           PARM           ##ERR   1
     C**********           PARM           XXEFDT
     C**********           PARM           XXEXDT
     C**********           PARM           SCOFPC
     C**********           PARM           @@RRN
     C**********
     C**********           ENDIF                           *IN40=*ON
     C*****END** ADD  ***  12/31/97  *********************************
     C***  END   DEL  ***  06/29/99  *********************************
     C*** BEGIN CHG *** 04/13/99 ***********************************************
      *
      *** BEGIN DEL *** 10/21/99 ***********************************************
      **********
     C***IF*ANY*CHANGE WAS MADE TO AN OWNER, ALL OWNERS MUST BE LISTED
     C***ON*THE*MIDTERM ENDORSEMENT FORM
     C**********
     C********** XXCGFL    IFEQ 'Y'
     C********** LDENDA    ANDNE*ZEROS
     C********** LDENDA    ANDNE*BLANKS
     C**********
     C********** OOKY      CHAINPOO02                60
     C********** *IN60     DOWEQ*OFF
     C********** OOOFPC    IFNE *ZEROS
     C********** OOSTAT    ANDNE'T'
     C**********           MOVE 'C'       OOSTAT
     C**********           UPDATPOO02
     C**********           ENDIF
     C********** OOKY      READEPOO02                    60
     C**********           ENDDO
     C**********
     C**********           ENDIF
      *** END   DEL *** 10/21/99 ***********************************************
      *
     C*** END CHG ***** 04/13/99 ***********************************************
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD         XC
     C     @@MODE        COMP      'I'                                    42    CHANGE INC/EXC
     C*
      ****** BEGIN ADD ****** 08/07/98 ********************************
     C     Q1PRMS        IFGE      7
     C*** BEGIN CHG *** 04/13/99 ***********************************************
     C     @@QTE         IFEQ      'A'
     C     @@QTE         OREQ      'C'
     C     @@QTE         OREQ      'X'
     C                   MOVE      *BLANKS       XXQTE
     C                   ELSE
     C                   MOVE      @@QTE         XXQTE             1
     C                   END
     C*** END CHG ***** 04/13/99 ***********************************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXQTE             1
     C                   END                                                                XC
      ****** END   ADD ****** 08/07/98 ********************************
     C*
     C   42              MOVEL     'C'           @@MODE
     C*
     C     @@ELFL        IFEQ      *BLANKS
     C*
     C     @@RRN2        CHAIN     WMEMP                              99
     C*
     C                   MOVE      *OFF          *IN41
     C                   MOVEL     *BLANKS       SCELID
     C*
     C                   ELSE
     C*
     C     @@RRN2        CHAIN     WMALP                              99
     C*
     C                   MOVE      *ON           *IN41
     C                   MOVEL(P)  ALELID        SCELID
     C                   MOVEL(P)  ALNAME        SCELNM
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   ENDIF                                                  @@ELFL=*BLANKS
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
     C                   MOVE      *OFF          *IN43
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
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
     C**** BEGIN ADD **** 01/27/99 ************************************
     C     ELKY2         CHAIN     PEL01                              60
     C**** END   ADD **** 01/27/99 ************************************
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN(N)  PFN01                              60
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              60
      **** END   ADD **** 10/14/98 *********************************************
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  FORCE INQUIRY IF SYSTEM DATE IS > GROUP EXPIRATION DATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C  N09FNEXDT        IFNE      *ZEROS
     C     FNEXDT        ANDLT     SYSDT
     C                   MOVE      *ON           *IN09
     C                   MOVE      'D'           @@MODE
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C***  BEGIN ADD  ***  12/31/97  *********************************
     C                   Z-ADD     *ZERO         OFFCNT            5 0
     C***  END   ADD  ***  12/31/97  *********************************
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*
     C*  SAVE THE OLD DATA BASE VALUES
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     *IN99         IFEQ      *ON
     C     CPYFLG        OREQ      *ON
     C                   CLEAR                   S#OLSN
     C                   CLEAR                   S#OFSN
     C                   CLEAR                   S#OSS#
     C                   CLEAR                   S#OFPC
     C                   CLEAR                   S#EXFL
     C                   CLEAR                   S#CPFL
     C                   CLEAR                   S#DOB
     C                   CLEAR                   S#TITL
     C                   CLEAR                   S#EFDT
     C                   CLEAR                   S#EXDT
     C*
     C                   ELSE
     C*
     C                   MOVE      OOOLSN        S#OLSN
     C                   MOVE      OOOFSN        S#OFSN
     C                   MOVE      OOOSS#        S#OSS#
     C                   MOVE      OOOFPC        S#OFPC
     C                   MOVE      OOEXFL        S#EXFL
     C                   MOVE      OOCPFL        S#CPFL
     C                   MOVE      OODOB         S#DOB
     C                   MOVE      OOTITL        S#TITL
     C*
     C     *IN42         IFEQ      *OFF
     C                   MOVE      OOEFDT        S#EFDT
     C                   MOVE      OOEXDT        S#EXDT
     C                   ELSE
     C                   Z-ADD     *ZERO         S#EFDT
     C                   Z-ADD     *ZERO         S#EXDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 01/28/98 ***********************************************
     C*** BEGIN CHG *** 04/13/99 ***********************************************
      *** BEGIN DEL *** 10/21/99 ***********************************************
     C**********           MOVE 'N'       XXCGFL  1
      *** END   DEL *** 10/21/99 ***********************************************
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*** END CHG ***** 04/13/99 ***********************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
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
     C                   Z-ADD     OOEFDT        SVEFDT
     C                   Z-ADD     OOEXDT        SVEXDT
     C*
     C                   EXSR      MVSCDB
     C*
     C   42              Z-ADD     OOEFDT        OOEXDT
     C   42              Z-ADD     SVEFDT        OOEFDT
     C*
     C                   MOVEL     Q1USER        OOUSER
     C                   MOVEL     Q1PGM         OOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OODT
     C                   MOVEL     DSTIME        OOTIME
     C*
     C     *IN42         IFEQ      *ON
     C     OOEXDT        ANDGT     SVEXDT
     C     *IN42         OREQ      *OFF
     C                   UPDATE    DBRCDF
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C*** BEGIN DEL *** 04/13/99 ***********************************************
     C*********************EXSR CKCHGS
     C*** END DEL ***** 04/13/99 ***********************************************
     C*** END ADD ***** 01/28/98 ***********************************************
     C                   ENDIF                                                  *IN42=*OFF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C     *IN42         IFEQ      *ON
     C                   EXSR      WRITER
     C                   ENDIF                                                  *IN42=*ON
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
     C*
     C*** BEGIN ADD *** 06/10/98 **************************************
     C* GET NEXT SEQUENCE NUMBER FOR OWNER/OFFICER
     C     OOKY          SETGT     POO02
     C     OOKY          READPE(N) POO02                                  61
     C*
     C     *IN61         IFEQ      *OFF
     C     1             ADD       OOSEQ#        XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF                                                  *IN61 = '0'
     C*** END ADD ***** 06/10/98 **************************************
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     ##CO#         OOCO#
     C                   Z-ADD     SCFND         OOFND
     C                   Z-ADD     SCEMP#        OOEMP#
     C                   Z-ADD     SCDIV         OODIV
     C                   MOVE      SCELID        OOELID
     C*** BEGIN ADD *** 06/10/98 **************************************
     C                   Z-ADD     XXSEQ#        OOSEQ#
     C*** END ADD ***** 06/10/98 **************************************
     C*
     C                   EXSR      MVSCDB
     C*
     C   42              MOVEL     SCTOFL        OOEXFL
     C                   MOVEL     Q1USER        OOIUSR
     C                   MOVEL     Q1USER        OOUSER
     C                   MOVEL     Q1PGM         OOIPGM
     C                   MOVEL     Q1PGM         OOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OOIDT
     C                   MOVEL     SRDTO         OODT
     C                   MOVEL     DSTIME        OOITIM
     C                   MOVEL     DSTIME        OOTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMOOP
     C**** BEGIN ADD **** 01/27/99 ************************************
     C                   CLEAR                   SCPCDS
     C**** END   ADD **** 01/27/99 ************************************
     C*
     C***  BEGIN ADD  ***  12/31/97  *********************************
     C                   ADD       1             OFFCNT
     C***  END   ADD  ***  12/31/97  *********************************
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C*** BEGIN DEL *** 04/13/99 ***********************************************
     C*********************EXSR CKCHGS
     C*** END DEL ***** 04/13/99 ***********************************************
     C*** END ADD ***** 01/28/98 ***********************************************
     C*
     C                   ENDSR                                                  WRITER
