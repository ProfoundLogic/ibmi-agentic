     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
      /TITLE W0070R01 - ADD A CLAIM
     H Bnddir('WTBNDDIR')
      * ***************************************************************
      * DESCRIPTION:  THIS PGM PROMPTS THE USER FOR INFORMATION       *
      *      NEEDED TO ADD A CLAIM.                                   *
      * ***************************************************************
      * ***************************************************************
      *                                                               *
      * CALLED FROM: W0070C01                                         *
      * CALLS TO:    NONE                                             *
      *                                                               *
      * CREATED BY:  C15     01/10/97      60ANC                      *
      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:298 Project: SY420
      *                Mask SSN
      *                                                               *
      * 02/18/15  C:180 Support 87637                                 *
      *                 Fix aww coming from CiCenter                  *
      *                                                               *
      * 11/16/12  C:190 Project WC2116                                *
      *                 Open Claim in Transform when confirmed.       *
      *
      * 05/04/12  C:255 Project 114DET                                *
      *                 Added code to populate WMNIP field from WHFRP *
      *                 file.                                         *
      *                                                               *
      * 04/05/12  C:255 Project WC2087                                *
      *                 Recompiled only to avoid decimal data error   *
      *                 for field CMCLDT/WMCMP.                       *
      *                                                               *
      * 07/21/09 C:62  PROJECT: 114WTT                                *
      *                Transfer Devereux info to claims               *
      *                                                               *
      * 05/01/07 C:71  SUPPORT: WC071 58027                           *
      *               Warning Screen not appearing to indicate
      *               OWNER/OFFISER excluded on location selection    *
      *                                                               *
      * 10/19/06 C:15  Project: 132WDQ                                *
      *                Chgd pgm to support a "batch" mode (i.e. don't *
      *                open display file.                             *
      *                                                               *
      * 06/14/06 C:62  PROJECT: WC1400                                *
      *                Added default to CMMCON.                       *
      *                                                               *
      * 04/24/06 C:62  SUPPORT: 114DJN                                *
      *                Add additional field transfers                 *
      *                                                               *
      * 04/17/06 C:62  SUPPORT: CO132 52316                           *
      *                Populate CMFSAL from FRFSAL.                   *
      *                                                               *
      * 08/01/05 C:53  SUPPORT: #48347                                *
      *                Populate CMHOSZ from FRHOSZ.                   *
      *                                                               *
      * 07/22/05 C:61  PROJECT: 71WFY                                 *
      *                Populate field for Alabama.                    *
      *                                                               *
      * 07/14/05 C:132 PROJECT: 129WIX                                *
      *                NY FROI WCMS implementation.                   *
      *                                                               *
      * 07/07/05-2 C:132 PROJECT: 133WBQ  (07/12/05)                  *
      *                  Changes to new incoming Date Fields          *
      *                                                               *
      * 07/07/05 C:132 PROJECT: 133WBQ                                *
      *                Populate Louisiana FROI fields.                *
      *                                                               *
      * 12/23/04 C:62  PROJECT: 132DAW                                *
      *                Populate California Fields.                    *
      *                                                               *
      * 07/29/04 C:53  SUPPORT: WC114 # 42727                         *
      *                If ACDT is not within the Policy date range    *
      *                and does not fall within another policies      *
      *                range and within a year of the effective date, *
      *                assign it to that fund year.                   *
      *                                                               *
      * 03/22/04 C:136 SUPPORT: CI072 41717                           *
      *                Initial treatment code invokes an error when   *
      *                filled from CICENTER.                          *
      *                                                               *
      *****************************************************************
      **** THE FOLLOWING PROJECT WILL BE ACTIVATED WHEN CICENTER ADDS *
      **** THE NEW FIELDS TO THEIR DATABASE  04/20/04 C:53            *
      *                                                               *
      * 10/20/03 C:53  Project: WC909                                 *
      *                Added country info.                            *
      *****************************************************************
      *                                                               *
      * 04/22/03 C: 53 PROJECT: WC1121                                *
      *                Add option 30=Auto Claim letters               *
      *                                                               *
      * 04/16/03 C:15  Support: WC072/36892                           *
      *                Corrected the location delete date vs. acc dt  *
      *                edit.                                          *
      *                                                               *
      * 04/03/03 C:136 Project: WC1084                                *
      *                Add code to process CiCenter Internet Claims.  *
      *                                                               *
      * 05/15/02 C:15  Support: WC094/32506                           *
      *                Added code to release record lock on WMFNP to  *
      *                prevent errors we other user try to access the *
      *                locked record.                                 *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
      *                                                               *
      * 01/03/01 C:61  PROJECT WC819                                  *
      *              - Add prompt fields for GRP, EMP, DIV, Location. *
      *              - Searches for new prompt fields.                *
      *                                                               *
      * 09/01/00 C:15 Project: WC577                                  *
      *               Chgd call to W0563R to load '*HIVAL' into the   *
      *               accident date parm since the search is now in   *
      *               descending order (chg was made to WMCMLA).      *
      *               Without this change, no records would appear    *
      *               in the search.                                  *
      *                                                               *
      * 05/31/00 C:53  Project: WC461                                 *
      *                Add Daily batch lockout process                *
      *                                                               *
      * 01/20/00 C:15 Support: WC065/21088                            *
      *               Changed the claim FYR calculation to use the    *
      *               group detail FYR if the emp does not have a     *
      *               policy in effect for the accident date.         *
      *                                                               *
      * 07/06/99 C:15 Support: WC071/17389                            *
      *               Added the op code FEOD immediately after the    *
      *               update of WMFNP when retrieving the next claim  *
      *               number to prevent 2 claims from being created   *
      *               with the same claim #.                          *
      *                                                               *
      *  2/08/99 C:58 Support: WC071/16101                            *
      *              -Initialize IX 'Y'                               *
      *                                                               *
      * 12/16/98 C:15 Support: WC048/15572                            *
      *              -Fixed the policy coverage edit override.        *
      *                                                               *
      * 04/28/98 C:15 Project: WC577                                  *
      *              -Added 5 addl parms to SRCCSC.  These parms are  *
      *               addl security types.                            *
      *              -Changed the policy coverage edit override.      *
      *                                                               *
      *  03/10/98  C:58  PROJECT: SUPPORT: WC087  sequence # 10555
      *              Initialize entry date to zeroes.
      *
      *  02/10/98  47  PROJECT: WC577                                 *
      *            WRITE A CLAIM HISTORY RECORD FOR EACH NEW CLAIM    *
      *            MASTER RECORD.                                     *
      *                                                               *
      *  01/20/98  47  PROJECT: WC577                                 *
      *            REMOVED SS# SEARCH (W0571R1).                      *
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      * 71-89 These indicators are to used as error indicators for    *
      *       the various screens. They will set off after            *
      *       displaying each input capible screen.                   *
      *  90 - Used as general purpose error indicator                 *
      *  99 - General purpose indiactor (CHAIN, READ, LOKUP...)       *
      * ***************************************************************
     F*
     FW0070F01  CF   E             WORKSTN USROPN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     UF   E           K DISK
     F                                     INFDS(FNFDS)
     FWMCXL     IF   E           K DISK
     FWMCML1    IF A E           K DISK
     F                                     INFDS(DBFDS)
     FWMCML9    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
      *
      * Employer Master
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(EMFDS)
     F                                     RENAME(PEM01:PEM02)
      *
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      *
     FWDELL2    IF   E           K DISK
      *
      *
     FWMALP     IF   E             DISK
     FWMALL     IF   E           K DISK
     F                                     INFDS(ALFDS)
     F                                     RENAME(PAL01:PAL02)
      *
      *
      *  OWNERS/OFFICERS FILE
     FWMOOL1    IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
      *  CLAIM HISTORY
     FWMCHP     O    E             DISK
     FWDF1L1    IF   E           K DISK
     FWHFRP     UF   E             DISK
     FWMCDL     UF A E           K DISK
     FWMNIP     O    E             DISK
     FWMTPP     O    E             DISK
      *
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
      *  EMPLOYER'S EFFECTIVE AND CANCEL DATES
      *
     D PHY             S              1    DIM(40)
     D PRPH            S              1    DIM(10)
      *
      *
      *
      *
     D FNFDS           DS
     D  FNRRN                397    400B 0
      *
     D EMFDS           DS
     D  EMRRN                397    400B 0
      *
     D ALFDS           DS
     D  ALRRN                397    400B 0
      *
      *
     D*
     D OPTNM           C                   CONST('Add a Claim')
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS                  INZ
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  OOOSS#                 1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS
     D  SCSS#                  1      9
     D  SS1A                   1      3
     D  SS2A                   4      5
     D  SS3A                   6      9
      *
     D                 DS
     D  DSDT                   1      8  0
     D  DSMMDD                 1      4
     D  DSCCYY                 5      8
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D XXCONH          C                   CONST('Contact')
     D DOTS            C                   CONST(' . . . . . . . . . .')
     D XCORGO          C                   CONST('Original open of -
     D                                     claim')
      *
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
     C*
     C*  KEY LIST FOR EMPLOYER LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCTG1
      *
      *  ENTITY / LOCATION
      *
     C     ALKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
      *
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSS#
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE CLAIM/DARTS FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
     C*
     C*   KEY LIST FOR OWNERS / OFFICERS FILE
     C*
     C     OOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCSS#
     C                   KFLD                    XXACDT
     C*
     C     OOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCSS#
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQMODE            1
     C                   PARM      DBRRN         QQRRN             9 0
     C                   PARM                    QQCONT            1
     C                   PARM                    QQRRN1            9 0
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      *OFF          ##KC              1
     C     *INKL         PARM      *OFF          ##KL              1
     C                   PARM      'L'           ##MODE            1            EMP SRCH W/LOC
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C*
     C     ACDPL3        PLIST
     C                   PARM      *INKC         QQKC
     C                   PARM      *INKL         QQKL
     C                   PARM                    QQMODE
     C                   PARM      DBRRN         QQRRN
     C                   PARM                    QQCONT            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTISO         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*ISO'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTISCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*ISO'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  DEFINE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCTG1        XXCTG1
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMACDT        XXCADT
     C     *LIKE         DEFINE    CMACDT        XXDT
     C     *LIKE         DEFINE    CMACDT        XXADDT
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    ALELID        XXELID
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDCY --> CYMD
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   MOVEL(P)  OPTNM         SRDATA
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDATA           80
     C                   PARM      40            SRLENG            3 0
     C                   MOVEL     SRDATA        SCPRDS
     C*
     C*  GET THE USER PARAMETER RECORD
     C*
     C     TOP           TAG
     C                   Z-ADD     *ZERO         UPCO#
     C                   Z-ADD     *ZERO         UPFND
     C     UPKY          CHAIN     PUP01                              99
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   INITIALIZE SCR01 FIELDS
     C*
     C                   MOVE      *BLANKS       SCSS#
     C     Q1PRMS        IFGT      5
     C     QQRRN1        IFNE      0
     C     QQRRN1        CHAIN(N)  PFR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           CICFLG            1
     C                   Z-ADD     FRFND         SCFND
     C                   Z-ADD     FREMP         SCEMP#
     C                   Z-ADD     FRDIV         SCDIV
     C                   MOVE      FRSS          SCSS#
     C                   MOVE(P)   FRACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTISO
     C                   MOVEL     SRDTO         SCACDT
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  QQRNN1
     C                   ENDIF                                                  Q1PRMS
     C*
     C     QQMODE        IFEQ      'X'
      *
     C                   Z-ADD     FRFND         XXFND
     C                   Z-ADD     FREMP         XXEMP#
     C                   Z-ADD     FRDIV         XXDIV
     C                   MOVE      FRSS          SCSS#
      *
     C                   MOVE(P)   FRACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTISO
     C                   MOVEL     SRDTO         SCACDT
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
      *
     C                   EXSR      ADDCLM
     C                   GOTO      TERM
      *
     C                   ELSE
     C                   OPEN      W0070F01
     C                   ENDIF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
      *****  Begin Add  ***  06/09/20  **********************
      /Free
       SCSS# = MaskData('SSN':'FRSS':FRSS);
      /End-Free
      *****  End   Add  ***  06/09/20  **********************
     C                   EXFMT     SCR01
      *****  Begin Add  ***  06/09/20  **********************
      /Free
       SCSS# = UnMaskData('SSN':'FRSS':SCSS#);
      /End-Free
      *****  End   Add  ***  06/09/20  **********************
     C                   END
     C* ***
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   END
     C* ***
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
      *
     C                   SELECT
      *
      * Group Search Given Company Number
      *
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVE      XXCO#         ##CO#
      *
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM      *BLANKS       ##FYR             3
     C                   PARM      1             ##VIEW            1 0
      *
     C     ##FND         IFEQ      *BLANKS
     C                   SETON                                        71
      *
     C                   ELSE
      *
     C                   MOVE      ##FND         SCFND
     C                   MOVE      ##FND         XXFND
     C                   SETON                                        72
     C                   ENDIF
      *
      *  Search Employer and Division
      *
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   EXSR      EMSRCH
      *
      *  Location
      *
     C     HFLD          WHENEQ    'SCALID'
      *
     C     SCEMP#        IFNE      *ZERO
     C     SCFND         ANDNE     *ZERO
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C     EMKY          CHAIN     WMEML                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   CALL      'W0076R'                             99
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM                    #1RRN             9 0
     C                   PARM      EMRRN         #1RRN2            9 0
     C                   PARM                    #1QTE             1
      *
     C     #1RRN         IFEQ      *ZERO
     C                   SETON                                        73
      *
     C                   ELSE
      *
     C     #1RRN         CHAIN     PAL01                              60
     C                   MOVEL(P)  ALELID        SCALID
     C                   SETON                                        74
     C                   ENDIF                                                  #1RRN
      *
     C                   ELSE
      *
     C                   EXSR      EMSRCH
     C                   ENDIF                                                  IN60
      *
     C                   ELSE
      *
     C                   EXSR      EMSRCH
     C                   ENDIF                                                  SCEMP# SCFND
      *
     C                   ENDSL
      *
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C*
      *
      * UPDATE THE USER PROFILE WITH THE FUND.
      *
     C*
     C                   END                                                    *INKJ
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C                   GOTO      TERM
     C                   END
     C*  F13=CHG USER PARMS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
     C*  PROCESS REQUEST SCREEN 1
     C*
     C     SCR01T        TAG
     C*
     C                   EXSR      ERRCK1                                       EDIT SCR01
     C                   EXSR      XSCR01                                       PROCESS SCR01
     C*
     C*  IF IN 'ADD MODE', CREATE A CLAIM RECORD.
     C*
     C     QQMODE        IFEQ      'A'
     C                   EXSR      ADDCLM
     C                   END
     C*
     C                   EXSR      CALLR                                        CALL NEXT PGM
     C*
     C*  END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C                   MOVE      *ON           *INKL
     C                   RETURN
     C*
     C*
     C*
      *=========================================================================
      *  EMPLOYER SEARCH
      *=========================================================================
     C     EMSRCH        BEGSR
      *
     C     SCFND         IFNE      *ZERO
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN(N)  PFN01                              60
     C  N60              Z-ADD     FNRRN         ##RRN2
     C   60              Z-ADD     *ZERO         ##RRN2
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         ##RRN2
     C                   ENDIF
      *
     C                   CALL      'W0050R'      ACDPL
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        72
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PEM01                              60
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   SETON                                        73
      *
      * Check if Location selected
      *
     C     ##RRN2        IFNE      *ZERO
     C     ##RRN2        CHAIN     PAL01                              60
     C                   MOVEL(P)  ALELID        SCALID
     C                   SETOFF                                       73
     C                   SETON                                        74
     C                   ENDIF                                                  ##RRN2
      *
     C                   ENDIF                                                  ##RRN
      *
     C                   ENDSR                                                  EMSRCH
     C*
     C*
     C*
     C*=========================================================================
     C*  THIS SUBROUTINE PROCESSES THE CREATION OF A CLAIM
     C*=========================================================================
     C     ADDCLM        BEGSR
     C*
     C*  GET THE EMPLOYER'S CURRENT YEAR
     C*
     C                   Z-ADD     XXACDT        XXEFDT
     C     ELKY          SETGT     PEL01
     C     EMKY          READPE    PEL01                                  99
     C                   Z-ADD     ELFYR         XXFYR
     C                   MOVE      *ON           *IN46                            Policy year valid
      *
     C                   MOVE      *OFF          YRFLAG            1
      *
     C     *IN99         IFEQ      *OFF
     C     ELPTDT        ANDLE     XXACDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   MOVE      ELFYR         XWFYR             2 0
     C     XWFYR         DIV       4             XDFYR             2 0
     C                   MVR                     XRDIV             1 0
     C*
     C     XWFYR         IFEQ      *ZEROS
     C     XRDIV         OREQ      *ZEROS
     C                   Z-ADD     366           SRDAYS            5 0
     C                   ELSE
     C                   Z-ADD     365           SRDAYS
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C*
     C     XXACDT        IFLT      XXFDAT
     C                   Z-ADD     ELFYR         XXFYR
     C                   MOVE      *ON           YRFLAG
     C                   ENDIF
     C                   ENDIF
      *
     C     YRFLAG        IFEQ      *OFF
      *
     C     *IN99         IFEQ      *ON
     C     ELPTDT        ORLE      XXACDT
      *
      *
     C     F1KY          SETGT     PF101
     C     FNKY          READPE    PF101                                  60
     C     *IN60         IFEQ      '0'
     C     F1ENPO        ANDGT     XXACDT
     C                   Z-ADD     F1FYR         XXFYR
     C                   ELSE
     C     XXACDT        DIV       10000         XXFYR
     C                   END
      *
     C                   MOVE      *OFF          *IN46                            Policy yr not vali
     C                   END
     C                   ENDIF
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL     SRDTO         X8PFDT            8 0
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         X8PTDT            8 0
      *
      *  IF ACCIDENT IS NOT WITHIN THE POLICY EFF PERIOD, DSP WINDOW.
      *
     C     QQMODE        IFNE      'X'
      *
     C     ELPFDT        IFGT      XXACDT
     C     ELPTDT        ORLE      XXACDT
     C     ELPFDT        OREQ      *ZERO
     C                   MOVE      *ON           *IN78
      *
     C     XBAUTH        IFEQ      'Y'                                          CLAIM/DART CNTL
     C                   MOVE      'N'           SCEOV2
     C                   EXFMT     WDW04
      *
     C     SCEOV2        IFEQ      'Y'
     C                   MOVE      *OFF          *IN78                          OK
     C                   ELSE
     C                   MOVEL(P)  'SCR01'       Q2RECF                         SCEOV2 = N
     C                   GOTO      STRT
     C                   END
      *
     C                   ELSE                                                   NOT AUTHORIZED
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    XA/XBAUTH
      *
      *
     C                   END                                                    ELPFDT GT XXACDT
      *
     C                   EXSR      SRCONF
      *
     C                   ENDIF
     C*
     C*  GET THE NEXT AVAILABLE CLAIM #
     C*
     C                   Z-ADD     0             FNNACS
     C     FNKY          CHAIN     PFN01                              99
     C     FNNACS        IFEQ      *ZERO
     C                   Z-ADD     1             FNNACS
     C                   END
     C                   ADD       1             FNNACS
     C  N99              UPDATE    PFN01
     C  N99              FEOD      WMFNL
     C     FNNACS        SUB       1             XXCASE
     C*
     C*  CREATE THE CLAIM RECORD
     C*
     C                   CLEAR                   PCM01
     C                   Z-ADD     XXCO#         CMCO#
     C                   Z-ADD     XXFND         CMFND
     C                   Z-ADD     XXFYR         CMFYR
     C                   Z-ADD     XXCASE        CMCASE
     C                   Z-ADD     XXEMP#        CMEMP#
     C                   Z-ADD     XXDIV         CMDIV
     C                   MOVEL     XXCTG1        CMCTG1
     C                   MOVEL     SCSS#         CMSS#
     C                   Z-ADD     XXACDT        CMACDT
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   MOVEL     Q1USER        CMIUSR
     C                   MOVEL     Q1PGM         CMIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C                   Z-ADD     DSTIME        CMITM
     C                   Z-ADD     0             CMENTD
     C                   MOVEL     'O'           CMSTAT
      *
     C     CICFLG        IFEQ      *ON
      *
     C                   MOVEL     FRLSN         CMLSN
     C                   MOVEL     FRFSN         CMFSN
     C                   MOVEL     FRMID         CMMID
     C                   Z-ADD     FRCLAS        CMCLAS
     C                   Z-ADD     FRAWW         CMAWW
     C                   MOVEL     FR20A9        TIMPD             1
      ****** BEGIN DELETE  *** 02/18/15 ************************************
     C**   CMAWW         IFEQ      *ZEROS
     C**   TIMPD         IFEQ      'W'
     C**   TIMPD         OREQ      'w'
     C**                 MOVE      FR9N4         CMAWW
     C**                 ENDIF
     C**                 ENDIF
      ****** END   DELETE  *** 02/18/15 ************************************
     C*
     C                   MOVE      FRDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMDTRP
      *
     C                   MOVE      FRLTFL        CMLTFL
     C                   MOVE      FRNATR        CMNATR
     C                   MOVE      FRPART        CMPART
     C                   MOVE      FRCAUS        CMCAUS
      *
     C                   MOVE      FRRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMRCVD
      *
     C                   MOVE      FREADR        CMEADR
      *
     C                   MOVE      FRHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMHIDT
      *
     C                   MOVE      FRJRST        CMJRST
     C                   MOVE      FRCTG1        CMCTG1
     C                   Z-ADD     FRACTM        CMACTM
      *
     C                   MOVE      FRDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMDTLW
      *
     C                   MOVE      FRDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMDBDT
      *
     C                   MOVE      FRDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMDOD
      *
     C                   MOVE      FRESTS        CMESTS
     C                   MOVEL     FROCDS        CMOCDS
     C                   Z-ADD     FRPRWW        CMPRWW
      *
     C                   MOVE      FRRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CMRTW
      *
     C                   MOVE      FRPOI         CMPOI
     C                   MOVE      FRFSAL        CMFSAL
     C                   MOVEL     FR60A3        CMACDS
     C                   MOVEL     FR60A4        CMACD2
     C*
     C     CMACDS        IFEQ      *BLANKS
     C     CMACD2        ANDEQ     *BLANKS
     C                   MOVEL     FR80A1        CMACDS
     C                   MOVE      FR80A1        HOLD20           20
     C     HOLD20        CAT       FR80A2        HLD100          100
     C                   MOVEL     HLD100        CMACD2
     C                   ENDIF
     C*
     C                   MOVEL     FRHOSN        CMHOSN
     C                   MOVEL     FRHOSA        CMHOSA
     C                   MOVEL     FRHOSC        CMHOSC
     C                   MOVEL     FRHOSS        CMHOSS
     C                   MOVEL     FRHOSZ        CMHOSZ
     C                   Z-ADD     FR3N2         CMHOSR
     C                   Z-ADD     FR9N4         CMHOSP
     C                   MOVEL     FRITRT        CMITRT
     C                   Z-ADD     FRDEP         CM#DEP
     C                   Z-ADD     FRDAYS        CMDAYS
     C                   MOVEL     FRRPT         CMECS#
     C                   MOVEL(P)  FRCTG1        CMCTG1
     C                   MOVEL(P)  FRGT1         CMGT1
     C                   MOVEL(P)  FRACDR        CMACDR
     C                   MOVEL(P)  FRACTY        CMACTY
     C                   MOVEL(P)  FRAST         CMAST
     C                   MOVEL     FRACZP        CMACZ1
     C                   MOVE      FRACZP        CMACZ2
     C*
     C     CMACDR        IFEQ      *BLANKS
     C                   MOVEL(P)  FR40A1        CMACDR
     C                   MOVEL(P)  FR25A1        CMACTY
     C                   MOVEL(P)  FR2A1         CMAST
     C                   MOVEL     FR9N2         CMACZ1
     C                   MOVE      FR9N2         CMACZ2
     C                   ENDIF
     C*
     C                   MOVEL(P)  FRHOSN        CMHOSN
     C                   Z-ADD     FRHRSD        CMHRSD
     C                   MOVEL(P)  FRESTS        CMESTS
     C                   Z-ADD     FROTWK        CMOTWK
      *
     C                   Z-ADD     FRPOWW        CMPOWW                         Post Injury WW
     C                   MOVEL     'I'           CMMCON
     C                   MOVEL(P)  FRDCAU        CMDCAU
     C                   MOVEL(P)  FRDRET        CMDRET
     C                   MOVEL(P)  FRDPGM        CMDPGM
      *
      *
     C                   ENDIF                                                  CICFLG
      *
     C                   WRITE     PCM01
     C*
     C*  WRITE CLAIM HISTORY FILE
     C*
     C                   Z-ADD     XXCO#         CHCO#
     C                   Z-ADD     XXFND         CHFND
     C                   Z-ADD     XXFYR         CHFYR
     C                   Z-ADD     XXCASE        CHCASE
      *
     C                   Z-ADD     XXDT          CHEFDT
     C                   MOVEL(P)  XCORGO        CHEXPL
     C                   MOVEL     CMSTAT        CHOCST
     C                   MOVE      *BLANK        CHSTAT
     C                   MOVEL     Q1USER        CHUSER
     C                   MOVEL     Q1PGM         CHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CHDT
     C                   Z-ADD     DSTIME        CHTIME
     C                   WRITE     PCH01
      *
     C     CICFLG        IFEQ      *ON
      *
      * WRITE CLAIMS DEMOGRAPHICS FILE
      *
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      SCSS#         CDSS#
     C     FRLSN         IFNE      *BLANKS
     C                   MOVEL     FRLSN         CDLSN
     C                   ENDIF
     C     FRFSN         IFNE      *BLANKS
     C                   MOVEL     FRFSN         CDFSN
     C                   ENDIF
     C     FRMID         IFNE      *BLANKS
     C                   MOVEL     FRMID         CDMID
     C                   ENDIF
     C     FRADR1        IFNE      *BLANKS
     C                   MOVEL     FRADR1        CDADR1
     C                   ENDIF
     C     FRADR2        IFNE      *BLANKS
     C                   MOVEL     FRADR2        CDADR2
     C                   ENDIF
     C     FRCTY         IFNE      *BLANKS
     C                   MOVEL     FRCTY         CDCTY
     C                   ENDIF
     C     FRST          IFNE      *BLANKS
     C                   MOVEL     FRST          CDST
     C                   ENDIF
     C     FRZIP         IFNE      *ZEROS
     C                   MOVE      FRZIP         CDZIP
     C                   ENDIF
      *
      *****************************************************************
      **** THE FOLLOWING CODE WILL BE ACTIVATED WHEN CICENTER ADDS    *
      **** THE NEW FIELDS TO THEIR DATABASE                           *
     C**
     C**         FRCTRY    IFNE *BLANKS
     C**                   MOVELFRCTRY    CDCTRY    P
     C**                   ENDIF
     C**
     C**         FRADR3    IFNE *BLANKS
     C**                   MOVELFRADR3    CDADR3    P
     C**                   ENDIF
     C**
      *****************************************************************
      **** END CHANGES BASED ON CI CENTER
      *****************************************************************
      *
     C     FRDTOB        IFNE      *BLANKS
     C     FRDTOB        ORNE      *ZEROS
     C                   MOVE      FRDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         CDDTOB
     C                   ENDIF
      *
     C     FRSEX         IFNE      *BLANKS
     C                   MOVE      FRSEX         CDSEX
     C                   ENDIF
     C     FRAREA        IFNE      *ZEROS
     C                   MOVE      FRAREA        CDAREA
     C                   ENDIF
     C     FRPHN         IFNE      *ZEROS
     C                   MOVE      FRPHN         CDPHN#
     C                   ENDIF
     C     FRMSTS        IFNE      *BLANKS
     C                   MOVE      FRMSTS        CDMSTS
     C                   ENDIF
      *
     C                   MOVEL     Q1USER        CDUSER
     C                   MOVEL     Q1PGM         CDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CDDT
     C                   Z-ADD     DSTIME        CDTIME
      *
     C     *IN60         IFEQ      *ON
     C                   MOVEL     Q1USER        CDIUSR
     C                   MOVEL     Q1PGM         CDIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CDIDT
     C                   Z-ADD     DSTIME        CDITM
     C                   WRITE     PCD01
     C                   ELSE
     C                   UPDATE    PCD01
     C                   ENDIF
      *
      * WRITE STATE NOI FILE
      *
     C                   Z-ADD     XXCO#         NICO#
     C                   Z-ADD     XXFND         NIFND
     C                   Z-ADD     XXFYR         NIFYR
     C                   Z-ADD     XXCASE        NICASE
     C                   MOVE      FR1A1         NI1A1
     C                   MOVE      FR1A2         NI1A2
     C                   MOVEL     FR60A1        NI60A1
     C                   MOVEL     FR60A2        NI60A2
     C                   MOVEL     FR60A3        NI60A3
     C                   MOVEL     FR60A4        NI60A4
     C                   MOVEL     FR60A5        NI60A5
     C                   MOVEL     FR60A6        NI60A6
     C                   MOVEL(P)  FR80A1        NI80A1
     C                   MOVEL(P)  FR80A2        NI80A2
     C                   MOVEL(P)  FR80A3        NI80A3
     C     FRJRST        IFNE      'MA'
     C     FRJRST        ANDNE     'NY'
     C                   MOVEL     FRSTHR        NI10A2
     C                   ENDIF
     C*
     C* California Specific Fields
     C*
     C                   Z-ADD     FRTIM2        NITIM2
     C                   MOVEL     FR40A1        NI40A1
     C                   MOVEL     FR40A2        NI40A2
     C                   MOVEL     FR30A1        NI30A1
     C                   MOVEL     FR25A1        NI25A1
     C                   MOVEL     FR2A1         NI2A1
     C                   Z-ADD     FR9N2         NI9N2
     C                   MOVEL     FR30A2        NI30A2
     C                   MOVEL     FR20A1        NI20A1
     C                   MOVEL     FR1A3         NI1A3
     C                   Z-ADD     FRDT1         NIDT1
     C                   MOVEL     FR40A3        NI40A3
     C                   MOVEL     FR30A3        NI30A3
     C                   MOVEL     FR40A4        NI40A4
     C                   MOVEL     FR1A4         NI1A4
     C                   MOVEL     FR80A4        NI80A4
     C                   MOVEL     FR80A5        NI80A5
     C                   MOVEL     FR40A5        NI40A5
     C                   MOVEL     FR1A5         NI1A5
     C                   MOVEL     FR1A6         NI1A6
     C                   Z-ADD     FR3N1         NI3N1
     C                   MOVE      FR9N3         NI9N3
     C                   MOVEL     FR40A6        NI40A6
     C                   MOVEL     FRPRNM        NIPRNM
     C     FRPREP        IFNE      *BLANKS
     C     FRPREP        ORNE      *ZEROS
     C                   MOVE      FRPREP        SRDTI
     C                   CALL      'SRCVTDAT'    DTISCY
     C                   MOVEL     SRDTO         NIPREP
     C                   ENDIF
     C                   MOVEL(P)  FRCONT        NICONT
     C                   MOVE      FRCAR         NICAR
     C                   MOVE      FRCPHN        NICPHN
     C*
     C                   MOVEA     FRPRPH        PRPH
     C     1             DO        10            X
     C     PRPH(X)       IFEQ      'N'
     C     PRPH(X)       OREQ      'U'
     C     PRPH(X)       OREQ      'L'
     C                   MOVE      ' '           PRPH(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     PRPH          FRPRPH
     C                   MOVEL(P)  FRPRPH        NIPRPH
      *
      *
      * Add LOUSIANA Specific Fields - 133WBQ and New York - 129WIX
      *
     C                   MOVEL     FR1A7         NI1A7
     C                   MOVEL     FR1A8         NI1A8
     C                   MOVEL     FR1A9         NI1A9
     C                   MOVEL     FR1A10        NI1A10
      *
     C                   MOVEL     FR10A1        NI10A1
     C     FRJRST        IFEQ      'MA'
     C     FRJRST        OREQ      'NY'
     C                   MOVEL     FR10A2        NI10A2
     C                   ENDIF
     C                   MOVEL     FR10A3        NI10A3
     C                   MOVEL     FR10A4        NI10A4
     C                   MOVEL     FR10A5        NI10A5
      *
     C                   MOVEL     FR20A2        NI20A2
     C                   MOVEL     FR20A3        NI20A3
     C                   MOVEL     FR20A4        NI20A4
     C                   MOVEL     FR20A5        NI20A5
     C                   MOVEL     FR20A6        NI20A6
     C                   MOVEL     FR20A7        NI20A7
     C                   MOVEL     FR20A8        NI20A8
     C                   MOVEL     FR20A9        NI20A9
     C                   MOVEL     FR20A0        NI20A0
      *
     C                   MOVEL     FR30A4        NI30A4
     C                   MOVEL     FR30A5        NI30A5
      *
     C                   MOVEL     FR40A7        NI40A7
     C                   MOVEL     FR40A8        NI40A8
     C                   MOVEL     FR40A9        NI40A9
     C                   MOVEL     FR40A0        NI40A0
      *
     C                   MOVEL     FR60A7        NI60A7
     C                   MOVEL     FR60A8        NI60A8
     C                   MOVEL     FR60A9        NI60A9
     C                   MOVEL     FR60A0        NI60A0
      *
     C                   MOVEL     FR80A6        NI80A6
     C                   MOVEL     FR80A7        NI80A7
     C                   MOVEL     FR80A8        NI80A8
     C                   MOVEL     FR80A9        NI80A9
     C                   MOVEL     FR80A0        NI80A0
     C                   MOVEL     FR80AA        NI80AA
      *
     C                   Z-ADD     FR3N2         NI3N2
     C                   Z-ADD     FR3N3         NI3N3
     C                   Z-ADD     FR3N4         NI3N4
     C                   Z-ADD     FR3N5         NI3N5
     C                   Z-ADD     FR3N6         NI3N6
     C                   Z-ADD     FR3N7         NI3N7
     C                   Z-ADD     FR3N8         NI3N8
     C                   Z-ADD     FR3N9         NI3N9
     C                   Z-ADD     FR3N10        NI3N10
      *
     C                   Z-ADD     FR5N1         NI5N1
     C                   Z-ADD     FR5N2         NI5N2
     C                   Z-ADD     FR5N3         NI5N3
     C                   Z-ADD     FR5N4         NI5N4
     C                   Z-ADD     FR5N5         NI5N5
      *
     C                   Z-ADD     FR9N1         NI9N1
     C                   Z-ADD     FR9N4         NI9N4
     C                   Z-ADD     FR9N5         NI9N5
      *******Beg Add********05/04/12**********************************
      /Free
       NI1A8  = FR1A8;
       NI1A9  = FR1A9 ;
       NI60A7 = FR60A7;
       NI80A1 = FR80A1;
       NI80A2 = FR80A2;
       NI1A3  = FR1A3 ;
       NI1A4  = FR1A4 ;
       NI30A5  = FR30A5 ;
       NI1A5  = FR1A5 ;
       NI1A6  = FR1A6 ;
       NI1A7  = FR1A7 ;
       NI2A2  = FR2A2 ;
       NI2A3  = FR2A3 ;
       NI5N1  = FR5N1 ;
       NI92N1 = FR92N1;
       NI92N2 = FR92N2;
       NI92N3 = FR92N3;
       NI92N4 = FR92N4;
       NI92N5 = FR92N5;
       NI92N6 = FR92N6;
       NI92N7 = FR92N7;
       NI92N8 = FR92N8;
       NI92N9 = FR92N9;
       NI92N0 = FR92N0;
      /End-Free
      *******End Add********05/04/12**********************************
      *
      * Incoming Date fields, Changed from 7,0 to 8A, need to reformat
      *
      *
     C                   MOVE      FRDT2         ZZDT2             8 0
     C                   MOVE      ZZDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         NIDT2
     C                   ELSE
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         NIDT2
     C                   ENDIF
      *
     C                   MOVE      FRDT3         ZZDT3             8 0
     C                   MOVE      ZZDT3         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         NIDT3
     C                   ELSE
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         NIDT3
     C                   ENDIF
      *
     C                   MOVE      FRDT4         ZZDT4             8 0
     C                   MOVE      ZZDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         NIDT4
     C                   ELSE
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         NIDT4
     C                   ENDIF
      *
     C                   MOVE      FRDT5         ZZDT5             8 0
     C                   MOVE      ZZDT5         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         NIDT5
     C                   ELSE
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         NIDT5
     C                   ENDIF
      *
      *
     C                   MOVEL     FR2A2         NI2A2
     C                   MOVEL     FR2A3         NI2A3
     C                   MOVEL     FR2A4         NI2A4
     C                   MOVEL     FR2A5         NI2A5
      *
     C                   MOVEL     FR25A2        NI25A2
     C                   MOVEL     FR25A3        NI25A3
     C                   MOVEL     FR25A4        NI25A4
     C                   MOVEL     FR25A5        NI25A5
      *
     C                   Z-ADD     FRTIM1        NITIM1
     C                   Z-ADD     FRTIM3        NITIM3
     C                   Z-ADD     FRTIM4        NITIM4
     C                   Z-ADD     FRTIM5        NITIM5
      *
      *
     C                   MOVE      'A'           NISTAT
     C                   MOVEL     FRJRST        NIST
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          NIDT
     C                   Z-ADD     XXDT          NIIDT
     C                   Z-ADD     DSTIME        NIITM
     C                   Z-ADD     DSTIME        NITIME
     C                   MOVEL     Q1USER        NIUSER
     C                   MOVEL     Q1USER        NIIUSR
     C                   MOVEL     Q1PGM         NIPGM
     C                   WRITE     PNI01
      *
      *  WRITE TREATING PHYSICIAN FILE
      *
     C     FRPHYN        IFNE      *BLANK
     C                   Z-ADD     XXCO#         TPCO#
     C                   Z-ADD     XXFND         TPFND
     C                   Z-ADD     XXFYR         TPFYR
     C                   Z-ADD     XXCASE        TPCASE
     C                   Z-ADD     1             TPSEQ#
     C                   MOVE      'A'           TPSTAT
     C                   MOVEA     FRPHYN        PHY
     C*
     C     1             DO        40            X                 3 0
     C     PHY(X)        IFEQ      ','
     C                   MOVE      ' '           PHY(X)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     PHY           TPPHYN
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          TPDT
     C                   Z-ADD     XXDT          TPIDT
     C                   Z-ADD     DSTIME        TPTIME
     C                   MOVEL     Q1USER        TPUSER
     C                   MOVEL     Q1USER        TPIUSR
     C                   MOVEL     Q1PGM         TPPGM
     C                   MOVEL     Q1PGM         TPIPGM
     C                   MOVEL(P)  FRPHYA        TPPHYA
     C                   MOVEL(P)  FRPHYC        TPPHYC
     C                   MOVEL(P)  FRPHYS        TPPHYS
     C                   Z-ADD     FRPHYZ        TPPHYZ
     C                   Z-ADD     FRPHYR        TPPHYR
     C                   Z-ADD     FRPHYP        TPPHYP
     C                   WRITE     PTP01
     C                   ENDIF
      *
      * UPDATE CICENTER INTERNET NOI FILE
      *
     C     QQRRN1        CHAIN     PFR01                              60
     C                   MOVE      'T'           FRSTAT
     C                   Z-ADD     XXFYR         FRFYR
     C                   Z-ADD     XXCASE        FRCASE
     C                   MOVEL     Q1USER        FRUSER
     C                   MOVEL     Q1PGM         FRPGM
     C                   TIME                    DSDTTM
     C                   MOVEL     SRDTO         DSDT
     C                   MOVEL     DSCCYY        FRDATE
     C                   MOVE      DSMMDD        FRDATE
     C                   Z-ADD     DSTIME        FRTIME
     C                   UPDATE    PFR01
      *
     C                   ENDIF                                                  CICFLG
      *
     C                   ENDSR
     C*=========================================================================
     C*  THIS SUBROUTINE WILL CALL THE NEXT PROGRAM
     C*=========================================================================
     C     CALLR         BEGSR
      *
     C                   CALL      'W0070R06'    ACDPL3
      *
     C     *INKC         IFEQ      *ON
      *
     C     QQRRN1        CHAIN     PFR01                              60
     C                   MOVE      'D'           FRSTAT
     C                   MOVEL     Q1USER        FRUSER
     C                   MOVEL     Q1PGM         FRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        DSDT
     C                   MOVEL     DSCCYY        FRDATE
     C                   MOVE      DSMMDD        FRDATE
     C                   Z-ADD     DSTIME        FRTIME
     C                   UPDATE    PFR01
      *
     C                   ENDIF                                                  CICFLG
     C*
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   CALL      'WA049R'
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM                    @@RTCD            1
     C                   ENDIF
     C*
     C                   ENDSR
     C*=========================================================================
     C*  THIS SUBROUTINE EDITS THE SCR01 ENTRY FIELDS
     C*=========================================================================
     C     ERRCK1        BEGSR
     C*
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C     ##RTCD        CABEQ     'F'           TERM
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  SS# MUST BE ENTERED
     C*
     C     SCSS#         IFEQ      *BLANKS
     C                   SETON                                        8690
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ACCIDENT DATE MUST BE ENTERED
     C*
     C     SCACDT        IFEQ      *ZERO
     C                   SETON                                        85
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ACCIDENT DATE MUST BE A VALIDATE DATE
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCACDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN84
     C                   GOTO      STRT
     C                   END
     C*
     C*  ACCIDENT DATE MUST LESS THAN OR EQUAL TO SYSTEM DATE
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C     XXACDT        IFGT      XXDT
     C                   MOVE      *ON           *IN82
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCACDT        SVACDT            8 0
      *
      *
      * Group must be valid if entered.
      * Group must be valid if Employer entered.
      *
     C     SCFND         IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN(N)  PFN01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        75
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCFND
      *
      * Employer / Division must be valid if entered
      *
     C                   Z-ADD     *ZERO         ##RRN
      *
     C     SCEMP#        IFNE      *ZERO
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C     EMKY          CHAIN     PEM02                              60
     C  N60              Z-ADD     EMRRN         ##RRN
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        76
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCEMP# SCDIV
      *
      * Location must be valid if entered
      *
     C                   Z-ADD     *ZERO         ##RRN2
      *
     C     SCALID        IFNE      *BLANKS
     C     SCALID        ANDNE     *ZERO
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C                   MOVEL     SCALID        XXELID
     C     ALKY2         CHAIN     PAL02                              60
     C  N60              Z-ADD     ALRRN         ##RRN2
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        77
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCEMP# SCDIV
      *
      *
      *
     C                   ENDSR
     C*
     C*
     C*=========================================================================
     C*  THIS SUBROUTINE DISPLAYS MORE EMPLOYER INFO WINDOW
     C*=========================================================================
     C     KHDO          BEGSR
      *
      *  LOAD SCREEN FIELDS
      *
     C     EMPHN#        IFNE      *ZERO
     C                   MOVE(P)   EMAREA        XX3A              3
     C                   MOVEL(P)  EMPHN#        XX3B              3
     C                   MOVE(P)   EMPHN#        XX4A              4
     C     '('           CAT(P)    XX3A:0        SCEPH                          EMP PH#
     C     SCEPH         CAT       ')':0         SCEPH
     C     SCEPH         CAT       XX3B:1        SCEPH
     C     SCEPH         CAT       '-':0         SCEPH
     C     SCEPH         CAT       XX4A:0        SCEPH
     C                   END
      *
     C                   MOVEL(P)  EMFEIN        SCEFID
      *
     C     EMFAX#        IFNE      *ZERO
     C                   MOVE(P)   EMFAXA        XX3A
     C                   MOVEL(P)  EMFAX#        XX3B
     C                   MOVE(P)   EMFAX#        XX4A
     C     '('           CAT(P)    XX3A:0        SCEFAX                         EMP FAX#
     C     SCEFAX        CAT       ')':0         SCEFAX
     C     SCEFAX        CAT       XX3B:1        SCEFAX
     C     SCEFAX        CAT       '-':0         SCEFAX
     C     SCEFAX        CAT       XX4A:0        SCEFAX
     C                   ENDIF                                                  SCPST<>*BLANKS
      *
      *  GET COUNTY DESCRIPTION
      *
     C                   MOVE      *BLANK        ZZKEY             4
     C                   MOVE      *BLANK        XXKEY
     C     EMPST         IFNE      *BLANK
     C     EMPST         CAT       EMCNTY:0      ZZKEY
     C                   ELSE
     C     EMMST         CAT       EMCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   MOVEL(P)  EMCNTY        SCECNY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCECDE
     C                   END
      *
      *  GET CONTACT HEADING FROM CLAIM DARTS CONTROL FILE
      *
     C                   MOVE(P)   DOTS          SCCONH
     C     CXCONH        IFNE      *BLANK
     C     CXCONH        CAT       ' ':0         SCCONH
     C                   ELSE
     C     XXCONH        CAT       ' ':0         SCCONH
     C                   ENDIF
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
     C                   MOVEL(P)  EMCONT        SCCONT
      *
      * SCREEN DISPLAY RULE: YOU CAN'T HAVE ONLY ONE DOT IN PROMPT TEXT
      *
     C     ' '           CHECKR    CXCONH        XXLEN             3 0
     C     XXLEN         IFEQ      5
     C     XXLEN         OREQ      4
     C                   MOVEL(P)  CXCONH        SCCONH
     C                   END
      *
     C                   EXFMT     WDW08
      *
     C                   ENDSR
     C*
     C*=========================================================================
     C*  THIS SUBROUTINE DISPLAYS MORE LOCATION INFO WINDOW
     C*=========================================================================
     C     KIDO          BEGSR
      *
      *  LOAD SCREEN FIELDS
      *
     C                   MOVEL(P)  ALFDNM        SCANM2
     C                   MOVEL(P)  ALFEIN        SCAFID
      *
     C     ALPHN#        IFNE      *ZERO
     C                   MOVE(P)   ALAREA        XX3A
     C                   MOVEL(P)  ALPHN#        XX3B
     C                   MOVE(P)   ALPHN#        XX4A
     C     '('           CAT(P)    XX3A:0        SCAPHN                         LOC PH#
     C     SCAPHN        CAT       ')':0         SCAPHN
     C     SCAPHN        CAT       XX3B:1        SCAPHN
     C     SCAPHN        CAT       '-':0         SCAPHN
     C     SCAPHN        CAT       XX4A:0        SCAPHN
     C                   END
      *
     C     ALFAX#        IFNE      *ZERO
     C                   MOVE(P)   ALFARA        XX3A
     C                   MOVEL(P)  ALFAX#        XX3B
     C                   MOVE(P)   ALFAX#        XX4A
     C     '('           CAT(P)    XX3A:0        SCAFAX                         LOC FAX#
     C     SCAFAX        CAT       ')':0         SCAFAX
     C     SCAFAX        CAT       XX3B:1        SCAFAX
     C     SCAFAX        CAT       '-':0         SCAFAX
     C     SCAFAX        CAT       XX4A:0        SCAFAX
     C                   END
      *
      *  GET COUNTY DESCRIPTION
      *
     C                   MOVE      *BLANK        ZZKEY
     C                   MOVE      *BLANK        XXKEY
     C     ALPST         IFNE      *BLANK
     C     ALPST         CAT       ALCNTY:0      ZZKEY
     C                   ELSE
     C     ALMST         CAT       ALCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   MOVEL(P)  ALCNTY        SCACNY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCACDE
     C                   END
      *
     C                   EXFMT     WDW09
      *
     C                   ENDSR
      *
      *=========================================================================
      *  THIS SUBROUTINE DISPLAY A CONFORMATION WINDOW
      *=========================================================================
     C     SRCONF        BEGSR
      *
      *  LOAD SCREEN FIELDS
      *
     C                   Z-ADD     XXFYR         SCFYR
     C                   Z-ADD     SVACDT        SCACDT
     C                   Z-ADD     X8PFDT        SCPFDT
     C                   Z-ADD     X8PTDT        SCPTDT
      *
     C                   Z-ADD     EMFND         SCFND
     C     FNKY          CHAIN(N)  PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   END
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   MOVEL(P)  EMNAME        SCENM
     C                   MOVEL(P)  EMFDNM        SCENM2
      *
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD1        SCEADR
     C                   MOVEL(P)  EMPAD2        SCEAD2
     C     EMPCTY        CAT(P)    ',':0         SCECSZ
     C     SCECSZ        CAT       EMPST:1       SCECSZ
     C                   MOVEL(P)  EMPZIP        XXZIP5            5
     C                   MOVE(P)   EMPZIP        XXZIP4            4
     C     SCECSZ        CAT       XXZIP5:2      SCECSZ
     C     SCECSZ        CAT       '-':0         SCECSZ
     C     SCECSZ        CAT       XXZIP4:0      SCECSZ
     C                   ELSE
     C                   MOVEL(P)  EMMAD1        SCEADR                         MAILING ADDR
     C                   MOVEL(P)  EMMAD2        SCEAD2
     C     EMMCTY        CAT(P)    ',':0         SCECSZ
     C     SCECSZ        CAT       EMMST:1       SCECSZ
     C                   MOVEL(P)  EMMZIP        XXZIP5
     C                   MOVE(P)   EMMZIP        XXZIP4
     C     SCECSZ        CAT       XXZIP5:2      SCECSZ
     C     SCECSZ        CAT       '-':0         SCECSZ
     C     SCECSZ        CAT       XXZIP4:0      SCECSZ
     C                   END
      *
     C                   Z-ADD     ED(Y)         SRDTI                          EMP EFF DATE
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCEEFD
      *
     C                   MOVE      *OFF          *IN48
     C     SCECXD        IFGT      *ZERO                                        EMP CX DATE
     C                   MOVE      *ON           *IN48
     C                   END
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  CLAIMANT SS#
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1A          CAT(P)    '-':0         SCSSA
     C*****              CAT       SS2A:0        SCSSA
     C*****              CAT       '-':0         SCSSA
     C*****              CAT       SS3A:0        SCSSA
      /Free
       SCSSA = DisplaySS#(MaskData('SSN':'SCSS#':SCSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  LOAD LOCATION
      *
     C                   MOVEL(P)  ALELID        SCALID
     C                   MOVEL     ALNAME        SCANAM
      *
      * PHYSICAL CITY, STATE, ZIP
      *
     C     ALPAD1        IFNE      *BLANK
     C                   MOVEL     ALPAD1        SCAAD1
     C     ALPCTY        CAT(P)    ',':0         SCACSZ
     C                   CAT       ALPST:1       SCACSZ
     C                   MOVEL(P)  ALPZIP        XXZIP5
     C                   MOVE(P)   ALPZIP        XXZIP4
     C                   CAT       XXZIP5:2      SCACSZ
     C                   CAT       '-':0         SCACSZ
     C                   CAT       XXZIP4:0      SCACSZ
      *
      * MAILING CITY, STATE, ZIP
      *
     C                   ELSE
     C                   MOVEL     ALMAD1        SCAAD1
     C     ALMCTY        CAT(P)    ',':0         SCACSZ
     C                   CAT       ALMST:1       SCACSZ
     C                   MOVEL(P)  ALMZIP        XXZIP5
     C                   MOVE(P)   ALMZIP        XXZIP4
     C                   CAT       XXZIP5:2      SCACSZ
     C                   CAT       '-':0         SCACSZ
     C                   CAT       XXZIP4:0      SCACSZ
     C                   END
      *
      *  GET THE EFFECTIVE DATE
      *
     C     ALEFDT        IFNE      *ZERO
     C                   Z-ADD     ALEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCAEFD
     C                   END
      *
      *  CHECK FOR A CANCEL DATE
      *
     C                   MOVE      *OFF          *IN47
     C     ALDLDT        IFNE      *ZERO
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCADLD
     C                   MOVE      *ON           *IN47                          DSP CX TEXT
     C                   END
      *
     C     *INKF         DOUEQ     *ON
     C     *INKL         OREQ      *ON
      *
     C                   EXFMT     WDW03
     C* ***
     C*  F8=MORE EMPLOYER INFO
     C*
     C     *INKH         IFEQ      *ON
     C                   EXSR      KHDO
     C                   END
     C* ***
     C*  F9=MORE LOCATION INFO
     C*
     C     *INKI         IFEQ      *ON
     C                   EXSR      KIDO
     C                   END
     C* ***
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *OFF          *INKL
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDDO
     C*
     C                   ENDSR
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
     C                   MOVEL(P)  ECEMAL        SCEMAL
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   MOVEL(P)  *BLANKS       SCEMAL
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
     C*
     C*
     C*=========================================================================
     C*  THIS SUBROUTINE PROCESSES SCR01
     C*=========================================================================
     C     XSCR01        BEGSR
     C*
     C*  IF A CLAIM ALREADY EXISTS FOR THE ENTERED SS#, CALL THE
     C*  DUPLICATE SS# SEARCH.
     C*
B01  C     XXFL01        IFEQ      *BLANK
     C                   MOVE      SCSS#         XXSS#
     C     CMKY1         CHAIN     PCM02                              99
B01  C     *IN99         IFEQ      '0'
     C*
     C                   CALL      'W0563R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      SCSS#         QQSS#             9
     C                   PARM      *HIVAL        QQACDT            8
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM      *LOVAL        XXCASE
     C                   PARM                    QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   END
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF A CLAIM WAS SELECTED, CHANGE TO 'CHANGE MODE' AND GET
     C*  THE DATA BASE RELATIVE RECORD #.
     C*
     C     XXCASE        IFNE      *LOVAL
     C                   MOVE      'C'           QQMODE                         CHANGE MODE
     C                   MOVE      'Y'           QQCONT                         TYPE OF CONT
     C     CMKY          CHAIN     PCM01                              99
     C                   GOTO      ENDXSC
     C                   END
     C*
E01  C                   END
     C                   MOVE      'N'           XXFL01            1            SS# SRCH FLAG
E01  C                   END
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  CALL EMPLOYER SEARCH
     C*
      *
     C     ##RRN         IFEQ      *ZERO
     C                   EXSR      EMSRCH
      *
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   GOTO      TERM
     C                   END
      *
     C                   ENDIF                                                  SCFND SCEMP#
     C*
     C                   CLEAR                   WDW03                          CLEAR SCREEN
     C                   MOVEL     Q1PGM         SCPGM
     C*
      *
     C*  GET THE EMPLOYER & DIVISION
     C*
     C     ##RRN         CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
      *
     C                   ELSE
      *
      *  Invalid employer
      *
     C                   SETON                                        76
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *   CHECK THE EMPLOYER EFFECTIVE AND CANCELLATION DATES
      *     GET THE MOST CURRENT EFF DATE
      *
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
      *
      *  COMPARE THE ACCIDENT DATE TO THE EFFECTIVE DATE
      *  IF ACC DATE IS >= TO EFF DATE, GET OUT OF LOOP.
      *
     C     Y             DOWNE     0
     C     XXACDT        IFGE      ED(Y)
     C                   LEAVE
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C                   ENDDO
      *
      *  The accident date can't be greater than the cancel date,
      *    unless overriden by the user.
      *
     C     Y             IFEQ      *ZERO
     C                   Z-ADD     1             Y
     C                   MOVE      *ON           *IN83
     C                   ELSE
      *
     C     CD(Y)         IFLE      XXACDT                                       CANCELLED
     C     CD(Y)         ANDGT     *ZERO
     C                   MOVE      *ON           *IN83                              CANCELLED
     C                   END
      *
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY.
      *
      *
      *
      *  GET THE CLAIM DARTS CONTROL RECORD
      *
     C                   MOVE      *BLANK        XBAUTH            1
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  COMPARE SECURITY LEVEL & TYPE OF THE USER TO CLAIM/DARTS CONTROL
      *
     C                   MOVE      UPLVL         SRULVL
     C                   MOVE      CXSECC        SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXOCAT        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM                    SRRES             1
     C                   PARM      CXOCA2        SRCTP2            1
     C                   PARM      CXOCA3        SRCTP3            1
     C                   PARM      CXOCA4        SRCTP4            1
     C                   PARM      CXOCA5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
      *         SRRES = '0' if "Passed" Security Check
      *                 '1' if "Did Not Pass" Security Check
      *
     C     SRRES         IFEQ      '0'
     C                   MOVE      'Y'           XBAUTH                         Authorized to
     C                   END                                                     overide edit
      *
     C                   END
      *
     C     *IN83         IFEQ      *ON
     C     XBAUTH        IFEQ      'Y'                                          CLAIM/DART CNTL
     C                   MOVE      'N'           SCEOVR
     C                   EXFMT     WDW02
      *
     C     SCEOVR        IFEQ      'Y'
     C                   MOVE      *OFF          *IN83                          OK
     C                   ELSE
     C                   MOVEL(P)  'SCR01'       Q2RECF                         SCEOVR = N
     C                   GOTO      STRT
     C                   END
      *
     C                   ELSE                                                   NOT AUTHORIZED
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    XA/XBAUTH
      *
     C                   END                                                    *IN83
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  GET THE LOCATION ID
     C*
     C                   MOVE      *BLANK        XXCTG1
     C                   MOVE      *OFF          *IN45
     C     ##RRN2        CHAIN     PAL01                              99
      *
     C  N99              MOVEL(P)  ALELID        SCALID
     C   99              MOVEL(P)  *BLANKS       SCALID
     C   99              MOVEL(P)  *BLANKS       ALELID
      *
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   ALELID        XXCTG1
     C                   MOVE      *ON           *IN45                          DSP CX TEXT
      *
      *  LOCATION DELETION DATE MUST NOT BE LESS THAN OR EQUAL
      *  TO ACCIDENT DATE
      *
     C     XXCTG1        IFNE      *BLANK
      *
      *  SUBTRACT ONE DAY FROM LOCATION DELETION DATE TO CALC CORRECTLY
      *
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         X8ADDT            8
      *
     C                   MOVE      X8ADDT        SRCADT            8 0
     C                   Z-ADD     1             SRPAST            5 0
     C                   MOVE      *BLANKS       SRERR
     C                   MOVE      *BLANKS       SRTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    SRCADT
     C                   PARM                    SRPAST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C                   MOVE(P)   SRCADT        X8ADDT                         MDCY -1 DAY
      *
     C                   MOVE(P)   X8ADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDCY --> CYMD
     C                   MOVEL(P)  SRDTO         XXADDT
      *
      *  CHECK CANCEL DATE AGAINST ACCIDENT DATE
      *
     C     XXADDT        IFLT      XXACDT
     C     XXADDT        ANDNE     *ZERO
     C                   MOVE      *ON           *IN80
      *
     C     XBAUTH        IFEQ      'Y'                                          CLAIM/DART CNTL
     C                   MOVE      'N'           SCEOV5
     C                   EXFMT     WDW05
      *
     C     SCEOV5        IFEQ      'Y'
     C                   MOVE      *OFF          *IN80                          OK
     C                   ELSE
     C                   MOVEL(P)  'SCR01'       Q2RECF                         SCEOV5 = N
     C                   GOTO      STRT
     C                   END
      *
     C                   ELSE                                                   NOT AUTHORIZED
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    XA/XBAUTH
      *
     C                   END                                                    ALDLDT LE XACDT
     C                   END                                                    XXCTG1 NE BLANK
      *
     C                   END                                                    *IN99
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  IF SS# BELONGS TO AN OWNER, CHECK TO SEE IF OWNER IS EXCLUDED
      *    FROM COVERAGE.
      *
     C                   MOVEA     '00'          *IN(87)
     C     OOKY          SETGT     POO01
     C     OOKY1         READPE    POO01                                  60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *BLANK        SCOONM                         FIRST LAST
     C     OOOFSN        CAT       OOOLSN:1      SCOONM
     C                   MOVE      *BLANK        SCOOSS
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCOOSS
     C*****SCOOSS        CAT       SS2:0         SCOOSS
     C*****SCOOSS        CAT       '-':0         SCOOSS
     C*****SCOOSS        CAT       SS3:0         SCOOSS
      /Free
       SCOOSS = DisplaySS#(MaskData('SSN':'OOOSS#':OOOSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C*
     C     OOEXFL        IFEQ      'E'                                          EXCLUDED
     C                   MOVE      *ON           *IN87
     C                   END
      *
     C     OOEXFL        IFEQ      'I'                                          INCLUDED
     C     OOEXFL        OREQ      *BLANK
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCEFDT
      *
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCEXDT
      *
     C     OOEXDT        IFNE      *ZERO
     C     OOEXDT        ANDLE     XXACDT
     C                   MOVE      *ON           *IN88
     C                   END
      *
     C                   END                                                    INCLUDED
     C                   END                                                    *IN60
      *
      *  ALLOW OVERRIDE ON ERROR MESSAGE.
      *
     C     *IN87         IFEQ      *ON
     C     *IN88         OREQ      *ON
     C                   MOVE      'N'           SCOVR1
     C                   EXFMT     WDW01
      *
     C     SCOVR1        IFEQ      'Y'
     C                   MOVEA     '00'          *IN(87)
     C                   ELSE
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C                   END                                                    *IN87
     C*
     C     ENDXSC        ENDSR
