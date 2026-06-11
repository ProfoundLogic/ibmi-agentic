     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * ***************************************************************
      /TITLE W0070R11 - CLAIMS, BENEFIT MAINTENANCE
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY BENEFIT INFORMATION. *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R - CLAIM MASTER SEARCH                               *
      *
      *  CALLS TO:  FA003R - API to update MC claim master FMCMP
      *             SA001R - API to determine if MC installed
      *                                                               *
      *  CREATED: 2/07/97   PROGRAMMER: 47    PROJECT: 60ANC          *
      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:318 Project: SY420
      *                Mask SSN
      *
      * 09/12/11  C:255 SUPPORT WA136  77929                          *
      *                 Compiled only for the changes done in display *
      *                 file.                                         *
      *                                                               *
      * 07/27/11  C:190 PROJECT 114WXT                                *
      *                 Made 'Method of determining pre-inj wage'     *
      *                 required on particular condition.             *
      *                                                               *
      * 09/01/10  C:246 SUPPORT 74036                                 *
      *                 Remove editable option in Inq Mode            *
      *                                                               *
      * 08/23/10 C:62  Project: 74009                                 *
      *                Fix call of W4315R                             *
      *                                                               *
      * 08/12/10 C:33  Project: 139WNF  Priss #28                     *
      *                Function keys will be modified to be flexible. *
      *                                                               *
      * 07/29/10 C:190 Project: 139WNN                                *
      *                Additional Changes.                            *
      *                                                               *
      * 05/17/10 C:235 PROJRCT: 139WNF Additional changes             *
      *                All New field and function keys will be        *
      *                qualified on the WCMS EDI install flag='Y'     *
      *                                                               *
      * 03/24/10 C:232 Project: 139WNN                                *
      *                Update programs to use the new EDI Control API *
      *                WA076R instead of using WMIXP.                 *
      *                                                               *
      * 03/23/10 C:235 PROJRCT: 139WNF                                *
      *                EDI SCREEN DESIGN                              *
      *                                                               *
      * 11/03/09  C:227 Project: 135MMX                                 *
      *                 Add Apportionment Indicator/Percent.           *
      *
      * 05/21/09  C:191 Project: WC1792                                *
      *                 New Field added to SCR02 for RTW Status        *
      *
      * 03/11/09 C:53  Support: 67284 & 67448
      *                Relax edits for EDI R3.
      *
      * 05/20/08  C:53 Project: CI054                                  *
      *                Add field for OSHA form                         *
      *
      * 03/17/08 C:180 PROJECT WC1589 -  PRISS # 14
      *                Require entry for SCAWW when LT flag = 'I'
      *
      * 03/12/08 C:180 PROJECT 65WGI-  PRISS # 11
      *                Client-flag ALL changes FOR 65WGI.
      *
      * 02/27/08 C:180 PROJECT 65WGI-  PRISS # 11
      *                Client flag Full Salary, Date last worked,
      *                Date of disability.
      *
      *
      * 08/20/07 C:193 PROJECT: 65WGI                                 *
      *                EDI Rel 3.0 Impl/Setup/Training                *
      *                                                               *
      *  08/18/06 C:53  PROJECT: 114WMF
      *                 Back to base 114WDF, 114WKG
      *                              2/07/02 2/10/06
      *                                                               *
      * 05/30/06 C:15  Support: WC132/52898                           *
      *                Changed size of XXCMPR to accomodate the new   *
      *                comp rate % in WDRBP/RBCMPR.                   *
      *                                                               *
      * 03/20/06 C:15  Support: WC135/51883                           *
      *                Added code to prevent advancing to the next    *
      *                screen when the AWW is changed.                *
      *                                                               *
      *  03/06/06 C:132 Support: WC071 51160
      *                Change Code to load DSDTTM and use DSDATE
      *                instead of *DATE anywhere in the program.
      *                                                               *
      * 04/28/05  C:132 PROJECT: WC072  46448                         *
      *                 Add edit for Average Weekly Wage, must be     *
      *                 zero or positive, not negative amount.        *
      *                                                               *
      * 04/01/05  C:132 PROJECT: 132WBS                               *
      *               - Add PD Rate for CA - Benefit Information      *
      *               - Add LP Rate for CA - Benefit Information      *
      *                                                               *
      * 06/02/04  C:53  PROJECT: WC1180                               *
      *                 EXPAND FROM 2 DEC POS TO 4                    *
      *                                                               *
      * 08/16/02 C:142 Project: WC1077                                *
      *                Added field SCOTWK                             *
      *                                                               *
      * 04/26/02 C:15  Project: WC577                                 *
      *                On 'record only' and 'medical' claims, allow   *
      *                the entry of the RTW date without requiring    *
      *                the date of disability.                        *
      *                                                               *
      * 04/09/02 C:15  Support: WC065/31865                           *
      *                Chgd edit to allow RTW date to equal the date  *
      *                of disability.                                 *
      *                                                               *
      * 10/16/00 C:15  Project: WC468                                 *
      *                Added fields for full salary amt and wks/dys   *
      *                paid.                                          *
      *                                                               *
      *  08/25/00 C:062 PROJECT: MC232                                *
      *                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
      *                                                               *
      * 07/10/00 C:53  PROJECT: WC999                                 *
      *                Add USL&H calculation                          *
      *                                                               *
      * 04/03/00 C:118 PROJECT: WC854                                 *
      *                Add parm to call of FA003R to pass WMCMP/      *
      *                CMIMP.                                         *
      *                                                               *
      * 09/02/99 C:112 Support: 19256 (MC048)                         *
      *                Change QMSCL# from 10A to 25A                  *
      *                                                               *
      * 08/09/99 C:112 Support: 18832                                 *
      *         The Calculated rate field SCCCRT  is                  *
      *         retaining the value from a previous claim if it is    *
      *         otherwise supposed to be blank.  Field is now         *
      *         cleared                                               *
      *                                                               *
      * 08/03/99 C:112 Support: WC577                                 *
      *                Now allow Compensation rate to exceed the      *
      *                Maximum Compensation rate.  Issue a Warning    *
      *                screen the user can chose to bypass the warning*
      *                or back out. The edit for this occurs if it is *
      *                an Indemnity claim (CMLTFL='I') and the        *
      *                Compensation Rate > the Max Compensation Rate. *
      *                If the Compensation Rate exceeds the Max       *
      *                Compensation Rate  already in Change mode the  *
      *                edit will be bypassed.                         *
      *                                                               *
      * 07/22/99 C:112 Support: 99-WC577                              *
      *                Allow entry of 'Full Salary Continued' and     *
      *                'paid through date' on 'medical only' claims   *
      *                Modify edit to allow for Loss Time Flag = 'M'  *
      *                or 'I'                                         *
      *                                                               *
      * 06/22/99 C:61 PROJECT 48WQJ                                   *
      *                Add previous fund number to header info.       *
      *                                                               *
      * 04/27/99 C:90 Support: WC025/17309                            *
      *               Only load default compensation rate if rate     *
      *               = 0 AND in ADD mode.                            *
      *                                                               *
      * 01/07/99 C:15 Support: WC072/15841                            *
      *               Changed the pgm to display the comp rate for    *
      *               med only claims if the AWW is entered.          *
      *                                                               *
      * 11/16/98 C:15 Project: WC577                                  *
      *               Changed the bar code process.  A bar code API   *
      *               is now called to return certain parameters.     *
      *                                                               *
      * 10/27/98 C:33  SEQ#: WC048/14644                              *
      *                DO NOT DEFAULT THE FULL SALARY PAID THRU DATE  *
      *                IF THE FULL SALARY CONTIUANCE DAYS IS ZERO.    *
      *                                                               *
      * 09/24/98 C:15  Project: WC577                                 *
      *                Added processing for a new field on SCR01,     *
      *                'Agreement to compensate code'.                *
      *                                                               *
      * 09/21/98 C:112 Project: WC577                                 *
      *                Corrected misspelling (W0070F11).              *
      *                                                               *
      * 08/31/98 C:15 Support: WC100/13042                            *
      *               If IXFROI and CMJRST are blank, do not call the *
      *               state specific screen because XXPGM = 'W0070R'  *
      *               and a recursive call error will result.         *
      *                                                               *
      * 08/10/98 C:61 Project: 48WLC                                  *
      *               Display old Emp#/Div from Employer Cross        *
      *               Reference (WZEXP).                              *
      *                                                               *
      * 08/11/98 C:62 Project: MC155                                  *
      *               ADDED PARM FOR CMFEAD TO FA003R                 *
      *                                                               *
      * 08/04/98 C:15 Project: WC577                                  *
      *               Fixed the claim record lockout routine.         *
      *                                                               *
      * 07/13/98 C:15 Support: WC051/12451                            *
      *               Removed a parm from parm list for the state     *
      *               specific pgms.                                  *
      *                                                               *
      * 04/23/98 C:33 SEQ#: WC025/11254                               *
      *              -ADD CALCULATED COMP RATE TO SCREEN (OUTPUT ONLY)*
      *              -ONLY CALCLATE THE COMP RATE IF IT HAS NOT BEEN  *
      *               ENTERED                                         *
      *                                                               *
      * 04/13/98 C:15 Project: WC468                                  *
      *               Added call to the EDI Log API pgm.              *
      *                                                               *
      * 04/07/98 C:62 Project: MC155                                  *
      *               Added additional fields to FA003R parm list.    *
      *                                                               *
      * 02/10/98 C:47 Project: WC577                                  *
      *               USE CMRLW INSTEAD OF CMRLWK; USE ONLY ONE       *
      *               RELEASED TO WORK DATE FIELD.                    *
      *                                                               *
      * 02/03/98 C:47 Project: WC577                                  *
      *               Chgd pgm to prevent loop in add mode.           *
      *                                                               *
      * 11/05/97 C:15 Project: WC577                                  *
      *               Changed the format of date fields used in the   *
      *               parm lists for pgms FA003R from MDCY to CYMD.   *
      *                                                               *
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  07 - EDI INSTALLED                                           *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      *  18 - INVALID CURSOR POSITION FOR SEARCH                      *
      *  40 - ADD MODE                                                *
      * N40 - UPDATE MODE                                             *
      *  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
      *  66 - COMMERCIAL APPLICATION INSTALLED                        *
      *  67 - SECURITY LEVEL VALID                                    *
      * N67 - SECURITY LEVEL INVALID                                  *
      *  69 - EXCESS REINSURANCE DSP                                  *
      *  70 - BAR CODE INSTALLED                                      *
      * ***************************************************************
      *
     FW0070F11  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*** BEG ADD *** 11/03/09 ****************************************
     FWMC1L     UF A E           K DISK
     F*** END ADD *** 11/03/09 ****************************************
      *
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      ***** END ADD ******* 08/10/98 **********************************
      *
     FWMFNL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FWMCDL     IF   E           K DISK
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *
      *  CLAIM/DARTS CONTROL FILE
     FWMCXL     IF   E           K DISK
      *
      *  CLAIM/EDI CONTROL FILE
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
      *
      *  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      *
      *  WORK HISTORY FILE
     F** WMWHL   IF  E           K        DISK                      A
      *
      *  PAYMENT DETAIL FILE
     FWDPAL     IF   E           K DISK
      *
      *  COMPENSATION REIMBURSEMENT FILE
     FWDRBL     IF   E           K DISK
      *
      *  AVERAGE WEEKLY WAGE HISTORY FILE
     FWMAWP     O    E             DISK
      *
      /COPY SCOPYMT01D
     D***  Begin Add  ***  08/12/10  **************************************
     D SF              S              1    DIM(60) CTDATA PERRCD(1)             Screen Functions
     D SFD             S             35    DIM(60) ALT(SF)
     D FL              S             78    DIM(14)                              Function lines
     D*FL1             S             78    DIM(14)                              Function lines
     D*FL2             S             78    DIM(14)                              Function lines
     D*FL3             S             78    DIM(14)                              Function lines
     D VF              S              1    DIM(24)                              Valid functions
     D VF1             S              1    DIM(24)                              Valid functions
     D VF2             S              1    DIM(24)                              Valid functions
     D VF3             S              1    DIM(24)                              Valid functions
     D*
     D ##SFD           DS          3500
     D***  End   Add  ***  08/12/10  **************************************
      *
     D***PY SCOPYMT01I
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
     D*
     D*   Parameters for MC Claim Master API        QMxxxx
     D*
      *
     D PARMCL          DS                  INZ
     D  QMRTCD                 1      1
     D  QMSS#                  2     10
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D*************************************  11  18 QMACDT
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMACDT                11     17
     D  FILL1                 18     18
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMCO#                 19     21
     D  QMSUB#                22     26
     D  QMFND                 27     29
     D  QMFYR                 30     32
     D  QMCASE                33     39
     D  QMEMP#                40     48
     D  QMDIV                 49     53
     D  QMACTM                54     59
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D*************************************  60  67 QMDTRP
     D*************************************  68  75 QMDTLW
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMDTRP                60     66
     D  FILL2                 67     67
     D  QMDTLW                68     74
     D  FILL3                 75     75
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMACDS                76    135
     D  QMMCON               136    136
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D************************************  137 144 QMENTD
     D************************************  145 152 QMRCVD
     D************************************  153 160 QMMMI
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMENTD               137    143
     D  FILL4                144    144
     D  QMRCVD               145    151
     D  FILL5                152    152
     D  QMMMI                153    159
     D  FILL6                160    160
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QM1A1                161    161
     D  QM10A1               162    171
     D  QMLTFL               172    172
     D  QMSCL#               173    197
     D  QMMCTM               198    201
     D  QMSTWK               202    203
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D************************************* 189 196 QMDEDT
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMDEDT               204    210
     D  FILL7                211    211
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMDEEM               212    213
     D  QMCMNU               214    216
     D  QMCMSU               217    219
     D  QMCLAD               220    222
     D  QMRRN                223    231  0
     D  QMLSN                232    261
     D  QMFSN                262    276
     D  QMMID                277    277
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D************************************* 263 270 QMDTRE
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMDTRE               278    284
     D  FILL8                282    282
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMCLSU               285    287
     D  QM#DEP               288    290
      *
     D DATEDS          DS                  INZ
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
      *
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              PMT AGAINST RR
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
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)                              RESERVES
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claims, Benefit -
     D                                     Maintenance')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D*** BEGIN DLT *** 11/16/98 **************************************
     D************* 'FILE STATUS IS OUT, -C         XCBC01
     D************* 'USER - XXXXXXXXXX'
     D************* 'FILE STATUS IS IN'   C         XCBC02
     D*** END DLT ***** 11/16/98 **************************************
     D*** BEGIN ADD *** 11/16/98 **************************************
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D*** END ADD ***** 11/16/98 **************************************
     D*********     2                     C         #OFSCR
     D COMAPI          C                   CONST('WA001R')
      * ================================================================
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *

     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *   KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      ***** END ADD ******* 08/10/98 **********************************
      *
      *   KEY LIST FOR THE FUND
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY            10
      *
      *  CLAIM/EDI CONTROL FILE
      *
     C     IXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XX148             3
      *
      *   Key List for PAYMENT DETAIL file
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *   KEY LIST FOR COMP REIMB FILE
      *
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXEFDT
      *
     C     RBKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** BEG ADD *** 11/03/09 ****************************************
      *
      *  CLAIM AUXILLARY FILE
      *
     C     C1KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*** END ADD *** 11/03/09 ****************************************
      *
      **  KEY LIST FOR WORK HISTORY FILE
      **
      **         WHKY      KLIST
      **                   KFLD           XXCO#
      **                   KFLD           XXFND
      **                   KFLD           XXFYR
      **                   KFLD           XXCASE
      **                   KFLD           XXDATE
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXDT                           SYSTEM DT
     C     *LIKE         DEFINE    CMACDT        SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMACTM        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMACDT        XXACDT                         ACC DT
     C     *LIKE         DEFINE    CMACDT        XXDBDT                         DISABILITY
     C     *LIKE         DEFINE    CMACDT        XXDTLW                         LAST WORK DATE
     C     *LIKE         DEFINE    CMACDT        XXERTW                         EST RTW DATE
     C     *LIKE         DEFINE    CMACDT        XXFSDT                         FULL SAL DATE
     C     *LIKE         DEFINE    CMACDT        XXRTW                          RETURN TO WORK
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    CMACDT        XXEFDT
      **         *LIKE     DEFN WHDATE    XXDATE
     C     *LIKE         DEFINE    CMAWW         SVAWW
     C     *LIKE         DEFINE    CMAWW         SVAWW2
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    *IN60         XXAPIF                         CLAIM API FL
     C     *LIKE         DEFINE    *IN60         XXMANC                         MC INSTALLED
      *** BEGIN ADD *** 08/03/99 **************************************
     C     *LIKE         DEFINE    RBPACP        SVCRT
      *** END ADD ***** 08/03/99 **************************************
      *** BEG ADD****** 03/23/10 **************************************
     C     *LIKE         DEFINE    CMDNLT        XXDNLT
      *** END ADD ***** 03/23/10 **************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
      *
     C     ACDPL3        PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@CONT
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST
     C                   PARM                    XX148
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
     C/COPY SCOPYMT01C
      *
     C***  Begin Add  ***  08/12/10  **************************************
     C*================================================================
     C*  BLDLNS - Builds options and functions lines
     C*================================================================
     C     BLDLNS        BEGSR
     C*
     C                   SUB       FL1#1         FL1#1             3 0
     C                   SUB       FL1#2         FL1#2             3 0
     C                   SUB       FL2#1         FL2#1             3 0
     C                   SUB       FL2#2         FL2#2             3 0
     C                   SUB       FL3#1         FL3#1             3 0
     C                   SUB       FL3#2         FL3#2             3 0
     C                   MOVEA(P)  SF            ##SF
     C                   MOVEA(P)  SFD           ##SFD
     C*
     C     1             DO        #OFSCR        XXSCR#            1 0
     C*
     C                   MOVE      XXSCR#        ##SCR#
     C*
     C                   CALL      'SRMNTFNC'                           99
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM                    ##SF            100
     C                   PARM                    ##SFD
     C                   PARM                    FL
     C                   PARM                    VF
     C                   PARM                    ##SCR#            1
     C                   PARM      Q1PGM         ##PGM            10
     C*
     C                   SELECT
     C     XXSCR#        WHENEQ    1
     C                   MOVEA     FL            FL1
     C                   MOVEA     VF            VF1
     C                   MOVEL     'SCR01'       Q2RECF
     C     XXSCR#        WHENEQ    2
     C                   MOVEA     FL            FL2
     C                   MOVEA     VF            VF2
     C                   MOVEL     'SCR02'       Q2RECF
     C     XXSCR#        WHENEQ    3
     C                   MOVEA     FL            FL3
     C                   MOVEA     VF            VF3
     C                   MOVEL     'SCR03'       Q2RECF
     C                   ENDSL
      *
     C                   EXSR      KYDO
      *
     C                   ENDDO
      *
     C                   ENDSR
     C***  End   Add  ***  08/12/10  **************************************
      *
      *===============================================================
      * CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
      *===============================================================
      *
     C     CALLR         BEGSR
      *
     C     SCRN#         IFEQ      #OFSCR
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDEQ     'A'
     C                   MOVE      'A'           @@MODE
     C                   END
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVEL     'A'           QQCALL
     C     @@MODE        WHENEQ    'C'
     C                   MOVEL     'Y'           QQCALL
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL     'Z'           QQCALL
     C                   ENDSL
     C                   END
      *
     C     @@MODE        IFEQ      'A'
     C                   SELECT
     C     CXRPS         WHENEQ    'Y'
     C                   CALL      'W0070R12'    ACDPL3                 99
      *
     C     CXSTSP        WHENEQ    'Y'
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
      *** BEGIN DLT *** 07/13/98 ***********************************************
      ******************** MOVE CDZIP     QQAZIP
      *** END DLT ***** 07/13/98 ***********************************************
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
      *
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C***        *IN60     IFEQ *OFF
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C*** END CHG ****** 03/24/10 *************************************
     C     'W0070R'      CAT       IXFROI:0      XXPGM
     C                   ELSE
     C     'W0070R'      CAT       CMJRST:0      XXPGM
     C                   END
      *
     C*** BEGIN ADD *** 08/31/98 **************************************
     C     2             SUBST(P)  XXPGM:7       XXST              2
     C     XXST          IFNE      *BLANKS
     C*** END ADD ***** 08/31/98 **************************************
      *
     C                   CALL      XXPGM                                99
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
      *** BEGIN DLT *** 07/13/98 ***********************************************
      ******************** PARM           QQAZIP  90
      *** END DLT ***** 07/13/98 ***********************************************
     C                   PARM      *BLANK        QQRTCD            1
      *** BEGIN ADD *** 02/03/98 ***********************************************
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ELSE
     C                   MOVE      *ON           *IN99
     C                   ENDIF
      *
     C     *IN99         IFEQ      *ON
     C     CXDBEN        CABEQ     'Y'           TDBEN
     C     CXDART        CABEQ     'Y'           TDART
     C                   ENDIF
     C*** END ADD ***** 08/31/98 **************************************
      *
     C     CXDBEN        WHENEQ    'Y'
     C*** BEGIN ADD *** 08/31/98 **************************************
     C     TDBEN         TAG
     C*** END ADD ***** 08/31/98 **************************************
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
      *
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQRTCD
      *** BEGIN ADD *** 02/03/98 ***********************************************
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C     CXDART        WHENEQ    'Y'
     C*** BEGIN ADD *** 08/31/98 **************************************
     C     TDART         TAG
     C*** END ADD ***** 08/31/98 **************************************
     C                   Z-ADD     CMCO#         QCCO#
     C                   Z-ADD     CMFND         QCFND
     C                   Z-ADD     CMFYR         QCFYR
     C                   Z-ADD     CMCASE        QCCASE
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QAKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
     C                   END                                                    @@MODE=A
      *
     C     @@MODE        IFNE      'A'
     C     Q1PRMS        ANDGE     5
     C                   CALL      'W0070R12'    ACDPL3                 99
     C                   END
      *
     C     *INKL         IFEQ      *ON
      *** BEGIN DLT *** 02/03/98 ***********************************************
      ********** QQRTCD    OREQ 'B'
      *** END DLT ***** 02/03/98 ***********************************************
     C                   MOVE      *ON           *INLR
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   END
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   END                                                    *INKL
     C                   END                                                    SCRN#
     C                   ENDSR                                                  CALLR
      *
     C***  Begin Add  ***  08/12/10  **************************************
     C*===============================================================
     C* CHGCDA - Update values in the function array
     C*===============================================================
     C     CHGCDA        BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C*
B01  C     FOUND         DOWEQ     *OFF
     C     SF(I)         ANDNE     '*'
     C*
B02  C     SF(I)         IFEQ      XXSCRN
     C                   MOVEL     SFD(I)        DUMNUM            2
B03  C     XXNMBR        IFEQ      DUMNUM
     C                   MOVEL(P)  SFD(I)        DUMOFD           30
     C                   MOVE      XXCDA         DUMOFD
     C                   MOVEL     DUMOFD        SFD(I)
     C                   MOVE      *ON           FOUND
E03  C                   ENDIF                                                  ZZNUM=DUMNUM
E02  C                   ENDIF                                                  OF,I=XXSCRN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
     C*
E01  C                   ENDDO                                                  FOUND DOWEQ *OFF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*  CHKVFK - Check for valid function key being pressed
     C*================================================================
     C     CHKVFK        BEGSR
      *
     C                   SUB       SF#           SF#               3 0
     C                   TESTB     '1'           Q2IAID                   99
      *
B01  C     *IN99         IFEQ      *OFF                                         Function pressd
      *
     C                   TESTB     '7'           Q2IAID                   99
     C   99              ADD       1             SF#
     C                   TESTB     '6'           Q2IAID                   99
     C   99              ADD       2             SF#
     C                   TESTB     '5'           Q2IAID                   99
     C   99              ADD       4             SF#
     C                   TESTB     '4'           Q2IAID                   99
     C   99              ADD       8             SF#
     C                   TESTB     '0'           Q2IAID                   99
     C   99              ADD       12            SF#
      *
B02  C     VF(SF#)       IFEQ      *OFF
     C                   SETON                                        90  95
E02  C                   ENDIF
      *
E01  C                   ENDIF
      *
     C                   ENDSR
     C***  End   Add  ***  08/12/10  **************************************
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
      *
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
     C                   MOVEA     '00000000'    *IN(20)                        *IN20 - 27
     C                   MOVEA     '00000000'    *IN(28)                        *IN28 - 35
     C                   MOVEA     '000'         *IN(36)                        *IN36 - 38
     C                   MOVEA     '000000'      *IN(41)                        *IN41 - 46
     C                   MOVEA     '00'          *IN(50)                        *IN50 - 51
     C*** Begin add *** 08/20/07 ********************************
     C                   MOVEA     '0'           *IN(39)                           39
     C                   MOVEA     '0'           *IN(48)                           48
     C                   MOVEA     '0'           *IN(55)                           55
     C*** End add ***** 08/20/07 ********************************
      *
      *  PAID IN INJURY DATE
      *
     C     SCPOI         IFNE      *BLANKS
     C     SCPOI         ANDNE     'N'
     C     SCPOI         ANDNE     'Y'
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
      *
      *  PAID IN INJURY DATE
      *
      **         SCPOI     IFEQ 'Y'
      **         CMLTFL    ANDNE'I'
      **                   SETON                     9072
      **                   LEAVE
      **                   ENDIF
      *
      *  FULL SALARY CONTINUED
      *
     C*** Begin add *** 08/20/07 ********************************
     C     CMLTFL        IFEQ      'I'
     C     SCFSAL        IFEQ      *BLANKS
      ***** BEGIN ADD  ******* 02/27/08 ***************************
     C     CLTID         ANDEQ     '065'
      ***** END ADD    ******* 02/27/08 ***************************
     C                   MOVE      *ON           *IN55
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 08/20/07 ********************************
     C     SCFSAL        IFNE      *BLANKS
     C     SCFSAL        ANDNE     'N'
     C     SCFSAL        ANDNE     'Y'
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
      *
     C     SCFSAL        IFEQ      'Y'
     C     CMLTFL        ANDNE     'I'
      *** BEGIN ADD *** 07/22/99 **************************************
     C     CMLTFL        ANDNE     'M'
      *** END ADD ***** 07/22/99 **************************************
     C                   SETON                                        9074
     C                   LEAVE
     C                   ENDIF
      *
      *  FULL SALARY VOUCHER PROCESSING
      *
     C     SCFSVP        IFNE      *BLANKS
     C     SCFSVP        ANDNE     'N'
     C     SCFSVP        ANDNE     'Y'
     C                   SETON                                        9089
     C                   LEAVE
     C                   ENDIF
      *
      *  VALIDATE PAID THRU DATE
      *
     C     SCFSDT        IFNE      *ZERO
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFSDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9080
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  PAID THRU DATE MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C     SCFSDT        IFNE      *ZERO
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXFSDT
     C     XXFSDT        IFLT      CMACDT
     C                   SETON                                        9075
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  FULL SALARY PAID MUST BE 'Y' IF FULL SALARY DATE ENTERED
      *
     C*** BEG DEL *** 03/11/09 ************************************
      * REMOVE EDIT FOR EDI R3
     C********** SCFSAL    IFNE 'Y'
     C********** SCFSDT    ANDNE*ZERO
     C**********           SETON                     9076
     C**********           LEAVE
     C**********           ENDIF
     C*** end DEL *** 03/11/09 ************************************
     C*
     C*** BEGIN ADD *** 10/16/00 **************************************
     C*  The full salary amount cannot be less than zero.
     C*
     C     SCFSAM        IFLT      0
     C                   SETON                                        9020
     C                   LEAVE
     C                   ENDIF
     C*
     C*  The full salary wks/days cannot be less than zero.
     C*
     C     SCFSWK        IFLT      0
     C     SCFSDY        ORLT      0
     C                   SETON                                        9021
     C                   LEAVE
     C                   ENDIF
     C*
     C*  The full salary amount and wks/days are mutually inclusive.
     C*
     C     SCFSAM        IFNE      0
     C     SCFSWK        ANDEQ     0
     C     SCFSDY        ANDEQ     0
     C     SCFSAM        OREQ      0
     C     SCFSWK        ANDNE     0
     C     SCFSAM        OREQ      0
     C     SCFSDY        ANDNE     0
     C                   SETON                                        9022
     C                   LEAVE
     C                   ENDIF
     C*** END ADD ***** 10/16/00 **************************************
      *
      *  AGREE TO COMPENSATE
      *
     C     SCAGCP        IFNE      *BLANKS
     C     SCAGCP        ANDNE     'N'
     C     SCAGCP        ANDNE     'Y'
     C                   SETON                                        9077
     C                   LEAVE
     C                   ENDIF
      *
      *  IF USER TRIES TO CHG 'AGREE TO COMP' FLAG TO  N
      *    AND COMP PAYMENTS EXIST, ERROR
      *
     C     CMAGCP        IFEQ      'Y'
     C     SCAGCP        ANDEQ     'N'
     C     XXAMT         ANDNE     *ZERO
     C                   SETON                                        9050
     C                   LEAVE
     C                   ENDIF
      *
      *  IF USER TRIES TO CHG 'AGREE TO COMP' FLAG TO  N
      *    AND RESERVES EXIST, ERROR
      *
     C     CMAGCP        IFEQ      'Y'
     C     SCAGCP        ANDEQ     'N'
     C     XXRSV         ANDNE     *ZERO
     C                   SETON                                        9050
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/24/98 **************************************
      *  Validate the agreement to compensate code
      *
     C     SCAGCD        IFNE      *BLANKS
     C                   MOVEL(P)  'AGCP'        XXTBID
     C                   MOVEL(P)  SCAGCD        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 09/24/98 **************************************
      *
      *  VOCATIONAL REHAB
      *
     C     SCVORC        IFNE      *BLANKS
     C     SCVORC        ANDNE     'N'
     C     SCVORC        ANDNE     'Y'
     C                   SETON                                        9078
     C                   LEAVE
     C                   ENDIF
      *
      *  METHOD OF DETERMINING PRE-INJURY WAGE
      *
     C     SCWMTH        IFEQ      *BLANKS
     C     SCPRWW        ANDNE     *ZERO
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
      *
      *  VALIDATE METHOD OF DETERMINING PRE-INJURY WAGE
      *
      ***** BEG ADD ******* 07/27/11 **********************************
     C     CXLSRV        IFEQ      'Y'
     C     CMLTFL        ANDEQ     'I'
     C     SCWMTH        ANDEQ     ' '
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
      ***** END ADD ******* 07/27/11 **********************************
     C     SCWMTH        IFNE      *BLANKS
     C                   MOVEL(P)  'WMTH'        XXTBID
     C                   MOVEL(P)  SCWMTH        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9027
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  AVERAGE WEEKLY WAGE MUST BE ENTERED IF LOST TIME = I
      *
     C     SCAWW         IFEQ      *ZERO
     C     CMLTFL        ANDEQ     'I'
     C                   SETON                                        9081
     C                   LEAVE
     C                   ENDIF
      *
      ***** BEG ADD ***** 04/28/05 ***************************************
      *
      *  AVERAGE WEEKLY WAGE MUST NOT BE NEGATIVE
      *
     C     SCAWW         IFLT      *ZERO
     C                   SETON                                        9028
     C                   LEAVE
     C                   ENDIF
      *
      ***** END ADD ***** 04/28/05 ***************************************
      *
      *  WAGE PERIOD
      *
     C*** Begin add *** 08/20/07 ********************************
     C     CMLTFL        IFEQ      'I'
     C     SCWGP         IFEQ      *BLANK
      *** Begin Add *** 03/12/08 ********************************
     C     CLTID         ANDEQ     '065'
      *** End Add ***** 03/12/08 ********************************
     C                   MOVE      *ON           *IN39
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 08/20/07 ********************************
     C     SCWGP         IFNE      *BLANKS
     C                   MOVEL(P)  'FRWP'        XXTBID
     C                   MOVEL(P)  SCWGP         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9026
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** BEG  ADD***** 03/23/10 **************************************
      *  REDUCED BENEFIT AMOUNT CODE
      *
     C     SCRBAC        IFGT      *BLANKS
     C                   MOVEL(P)  'RBAC'        XXTBID
     C                   MOVEL(P)  SCRBAC        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9065
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 03/23/10 **************************************
      *
      *  RECALCULATE THE COMP RATE
      *
     C*** Begin add *** 08/20/07 ********************************
     C     CMLTFL        IFEQ      'I'
     C     SCAWW         IFEQ      *ZEROS
      *** Begin Add *** 03/12/08 ********************************
     C     CLTID         ANDEQ     '065'
      *** End Add ***** 03/12/08 ********************************
     C                   MOVE      *ON           *IN48
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 08/20/07 ********************************
     C     SCAWW         IFGT      *ZERO
     C     SCAWW         ANDNE     SVAWW
      *
     C*** BEGIN ADD *** 01/07/98 **************************************
     C                   MOVEL     CMJRST        XXJRST
     C                   Z-ADD     CMACDT        XXEFDT
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
     C*** BEG CHG *** 06/02/04 ****************************
     C********** RBREPC    DIV  100       XXREPC    H
     C     RBCMPR        DIV(H)    100           XXCMPR
     C*** END CHG *** 06/02/04 ****************************
     C     CMUSLH        IFNE      'Y'                                          ADD 07/10/00
     C                   Z-ADD     RBPACP        SCMCR                            MAX COMP RATE
     C*** END ADD ***** 01/07/98 **************************************
      *
      ***  BEGIN ADD  ***  04/23/98  *********************************
     C     SCCRT         IFEQ      *ZEROS
      ***  END   ADD  ***  04/23/98  *********************************
      **** BEGIN ADD **** 04/27/99 ************************************
     C     @@MODE        ANDEQ     'A'
      **** END   ADD **** 04/27/99 ************************************
      *
     C*** BEG CHG *** 06/02/04 ****************************
     C*********  SCAWW     MULT XXREPC    SCCRT     H        CALC COMP RATE
     C     SCAWW         MULT(H)   XXCMPR        SCCRT                            CALC COMP RATE
     C*** END CHG *** 06/02/04 ****************************
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCRT         IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        SCCRT
     C                   END
      *** BEGIN DELETE *** 08/03/99 ***********************************
      *********************************
      ******************* COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
      *
     C********** SCCRT     IFGT RBPACP
     C**********           Z-ADDRBPACP    SCCRT
     C**********           END
      *** END DELETE ***** 08/03/99 ***********************************
      *
      ***  BEGIN ADD  ***  04/23/98  *********************************
     C                   ENDIF                                                  SCCRT=*ZEROS
      *
     C*** BEG CHG *** 06/02/04 ****************************
     C********** SCAWW     MULT XXREPC    SCCCRT    H        CALC COMP RATE
     C     SCAWW         MULT(H)   XXCMPR        SCCCRT                           CALC COMP RATE
     C*** END CHG *** 06/02/04 ****************************
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCCRT        IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBMNRT
      *
      *  COMP RATE CAN BE GREATER THAN THE MAXIMUM
      *
     C     SCCCRT        IFGT      RBPACP
     C                   Z-ADD     RBPACP        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBPACP
      ***  END   ADD  ***  04/23/98  *********************************
      *
     C                   Z-ADD     SCAWW         SVAWW
     C                   MOVE      *ON           *IN38                          DSPATR(PC)
     C                   SETON                                        90
     C                   LEAVE
     C*** BEGIN ADD *** 01/07/98 **************************************
     C*** BEG ADD ***** 07/10/00 **************************************
     C                   ELSE
     C                   Z-ADD     RBLHMX        SCMCR                            MAX COMP RATE
     C     SCCRT         IFEQ      *ZEROS
     C     @@MODE        ANDEQ     'A'
      *
     C*** BEG CHG *** 06/02/04 ****************************
     C********** SCAWW     MULT XXREPC    SCCRT     H        CALC COMP RATE
     C     SCAWW         MULT(H)   XXCMPR        SCCRT                            CALC COMP RATE
     C*** END CHG *** 06/02/04 ****************************
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCRT         IFLT      RBLHMN
     C                   Z-ADD     RBLHMN        SCCRT
     C                   END
      *
     C                   ENDIF                                                  SCCRT=*ZEROS
      *
     C*** BEG CHG *** 06/02/04 ****************************
     C********** SCAWW     MULT XXREPC    SCCCRT    H        CALC COMP RATE
     C     SCAWW         MULT(H)   XXCMPR        SCCCRT                           CALC COMP RATE
     C*** END CHG *** 06/02/04 ****************************
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCCRT        IFLT      RBLHMN
     C                   Z-ADD     RBLHMN        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBLHMN
      *
      *  COMP RATE CAN BE GREATER THAN THE MAXIMUM
      *
     C     SCCCRT        IFGT      RBLHMX
     C                   Z-ADD     RBLHMX        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBLHMX
      *
     C                   Z-ADD     SCAWW         SVAWW
     C                   MOVE      *ON           *IN38                          DSPATR(PC)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF                                                  CMUSLH
     C*** END ADD ***** 07/10/00 **************************************
     C                   END                                                    *IN60 EQ *OFF
     C*** END ADD ***** 01/07/98 **************************************
     C*** Begin add *** 03/20/06 **************************************
     C                   Z-ADD     SCAWW         SVAWW
     C                   MOVE      *ON           *IN38                          DSPATR(PC)
     C                   SETON                                        90
     C                   LEAVE
     C*** End add ***** 03/20/06 **************************************
     C                   END                                                    SCAWW NE ZERO
      *
      * ONLY DO COMP RATE EDITS IF LOST TIME FLAG = I
      *
     C     CMLTFL        IFEQ      'I'
     C     XXRB01        ANDEQ     'Y'
     C*
     C     CMUSLH        IFNE      'Y'                                          ADD 07/10/00
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCRT         IFLT      RBMNRT
     C                   SETON                                        9036
     C                   LEAVE
     C                   END
      *
      * COMP RATE CAN BE GREATER THAN THE MAXIMUM
      *
     C     SCCRT         IFGT      RBPACP
      *** BEGIN ADD *** 08/03/99 **************************************
     C     SVCRT         ANDNE     SCCRT
     C                   EXFMT     WDWMSG
     C     *INKF         IFEQ      *ON                                          F6=*ON
     C                   MOVE      *OFF          *INKF
     C                   MOVE      SCCRT         SVCRT
     C                   ELSE
      *** END ADD ***** 08/03/99 **************************************
      *** BEGIN CHANGE *** 08/03/99 ***********************************
     C******************** SETON                     9037
     C                   SETON                                        90
      *** END CHANGE ***** 08/03/99 ***********************************
     C                   LEAVE
      *** BEGIN ADD *** 08/03/99 **************************************
     C                   ENDIF
      *** END ADD ***** 08/03/99 **************************************
     C                   END
     C*** BEG ADD ***** 07/10/00 **************************************
     C                   ELSE
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCRT         IFLT      RBLHMN
     C                   SETON                                        9036
     C                   LEAVE
     C                   END
      *
      * COMP RATE CAN BE GREATER THAN THE MAXIMUM
      *
     C     SCCRT         IFGT      RBLHMX
     C     SVCRT         ANDNE     SCCRT
     C                   EXFMT     WDWMSG
     C     *INKF         IFEQ      *ON                                          F6=*ON
     C                   MOVE      *OFF          *INKF
     C                   MOVE      SCCRT         SVCRT
     C                   ELSE
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C                   END
     C                   ENDIF                                                  CMUSLH
     C*** END ADD ***** 07/10/00 **************************************
      *
     C                   END                                                    CMLTFL = I
      *
      ***** BEG ADD ***** 04/01/05 ************************************
     C     *IN47         IFEQ      *ON                                          Is California ?
      *
      * If PD Rate is LESS than ZERO & California Rating State, is error
     C     SCPDRT        IFLT      *ZEROS
     C                   SETON                                        9032
     C                   LEAVE
     C                   ENDIF
      *
      * If LP Rate is LESS than ZERO & California Rating State, is error
     C     SCLPRT        IFLT      *ZEROS
     C                   SETON                                        9033
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN47 = *ON
      ***** END CHG ***** 04/01/05 ************************************
      *
     C                   ENDDO
      *
      *** BEGIN ADD *** 08/04/98 **************************************
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     **** BEG ADD ***** 11/03/09 **************************************
     C     CLTID         IFEQ      '135'
     C                   SETON                                        29
     C                   ENDIF
     **** END ADD ***** 11/03/09 **************************************
      *** END ADD ***** 08/04/98 **************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     1             SCRN#             1 0
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
     C                   MOVEA     '00000000'    *IN(20)                        *IN20 - 27
     C                   MOVEA     '00000000'    *IN(28)                        *IN28 - 35
     C                   MOVEA     '000'         *IN(36)                        *IN36 - 38
     C                   MOVEA     '00'          *IN(45)                        *IN45 - 46
     C                   MOVEA     '00'          *IN(50)                        *IN50 - 51
     C*** BEGIN DATE *** 08/20/07 *************************************
     C                   MOVEA     '0'           *IN(49)                        49
     C                   MOVEA     '00'          *IN(53)                        *IN53 - 54
     C*** END DATE ***** 08/20/07 *************************************
      *
      *  VALIDATE DATE LAST WORKED
      *
     C*** BEGIN DATE *** 08/20/07 *************************************
     C     CMLTFL        IFEQ      'I'
     C     SCDTLW        IFEQ      *ZERO
      ****** BEGIN ADD ****    02/27/08 *******************************
     C     CLTID         ANDEQ     '065'
      ****** END ADD ******    02/27/08 *******************************
     C                   SETON                                        9049
     C                   ENDIF
     C                   ENDIF
     C*** END DATE ***** 08/20/07 *************************************
     C     SCDTLW        IFNE      *ZERO
     C                   Z-ADD     SCDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL     SRDTO         SCDTLW
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9034
     C                   LEAVE
     C                   ENDIF
      *
      *  DATE LAST WORKED MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C                   Z-ADD     SCDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXDTLW
     C     XXDTLW        IFLT      CMACDT
     C                   SETON                                        9088
     C                   LEAVE
     C                   ENDIF
      *
      *  THE DATE LAST WORKED CANNOT BE > THE SYSTEM DATE.
      *
     C     XXDTLW        IFGT      XXDT
     C                   SETON                                        903134
     C                   LEAVE
     C                   END
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  VALIDATE DATE OF DISABILITY
      *
      *** Begin add *** 02/10/06 **************************************
     C     CMJRST        IFEQ      'PA'
     C     CMLTFL        IFEQ      'I'
     C     SCDBDT        IFEQ      *ZERO
     C                   SETON                                        9032
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *** Begin add *** 08/20/07 **************************************
     C     CMLTFL        IFEQ      'I'
     C     SCDBDT        IFEQ      *ZERO
      ****** BEGIN ADD ****    02/27/08 *******************************
     C     CLTID         ANDEQ     '065'
      ****** END ADD ******    02/27/08 *******************************
     C                   SETON                                        9053
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *** End   add *** 08/20/07 **************************************
      *
      *** End   add *** 02/10/06 **************************************
      *
     C     SCDBDT        IFNE      *ZERO
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL     SRDTO         SCDBDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9082
     C                   LEAVE
     C                   ENDIF
      *
      *  DATE OF DISABILITY MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXDBDT
     C     XXDBDT        IFLT      CMACDT
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  IF DATE OF DISABILITY CHANGES AND THE FULL SALARY PAID THRU DATE
      *  IS NOT ZERO, SEND AN INFO MSG INSTRUCTING THE USER TO VERIFY THE
      *  ACCURACY OF THE PAID THRU DATE.
      *
     C     XXOVR         IFEQ      *BLANK
     C     XXDBDT        IFNE      CMDBDT
     C     SCFSDT        ANDNE     *ZERO
     C                   EXFMT     WDW01
     C                   MOVE      'Y'           XXOVR
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  IF FULL SALARY FLAG = Y AND PAID THRU DATE = ZERO,
      *  ADD # FULL SALARY DAYS (FROM COMP REIMBURSEMENT FILE WMRBP)
      *  TO THE DATE OF DISABILITY AND LOAD THE RESUL
      *  AND LOAD THE RESULT INTO THE PAID THRU DATE.
      *
     C     SCFSAL        IFEQ      'Y'
     C     SCFSDT        ANDEQ     *ZERO
     C***  BEGIN ADD  ***  10/27/98  *********************************
     C     SC#DFS        ANDNE     *ZERO
     C***  END   ADD  ***  10/27/98  *********************************
     C                   CALL      'SRFUTR8'
     C                   PARM      SCDBDT        QQDT              8 0
     C                   PARM      SC#DFS        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCFSDT
     C                   END
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  VALIDATE ESTIMATED RETURN TO WORK DATE
      *
     C     SCERTW        IFNE      *ZERO
     C                   Z-ADD     SCERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL     SRDTO         SCERTW
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9084
     C                   LEAVE
     C                   ENDIF
      *
      *  EST RTW MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C                   Z-ADD     SCERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXERTW
     C     XXERTW        IFLT      CMACDT
     C                   SETON                                        9085
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  VALIDATE ACTUAL RETURN TO WORK DATE
      *
     C     SCRTW         IFNE      *ZERO
     C                   Z-ADD     SCRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL     SRDTO         SCRTW
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9086
     C                   LEAVE
     C                   ENDIF
      *
      *  ACTUAL RTW MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C                   Z-ADD     SCRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXRTW
     C     XXRTW         IFLT      CMACDT
     C                   SETON                                        9087
     C                   LEAVE
     C                   ENDIF
      *
      *  IF RTW IS ENTERED, DT DISABILITY MUST ALSO BE ENTERED
      *
     C     SCDBDT        IFEQ      *ZERO
     C     SCRTW         ANDNE     *ZERO
     C*** Begin add *** 04/26/02 ************************************
     C     CMLTFL        ANDEQ     'I'
     C*** End add ***** 04/26/02 ************************************
     C                   SETON                                        9082
     C                   LEAVE
     C                   ENDIF
      *
      *  ACTUAL RTW MUST BE GREATER THAN DATE OF DISABILITY
      *
     C*** BEG DEL *** 03/11/09 ************************************
      * REMOVE EDIT FOR EDI R3
     C*** Begin chg *** 04/09/02 ************************************
     C********** XXRTW     IFLE XXDBDT
     C********** XXRTW     IFLT XXDBDT
     C*** End chg ***** 04/09/02 ************************************
     C**********           SETON                     9086
     C**********           LEAVE
     C**********           ENDIF
     C*** END DEL *** 03/11/09 ************************************
     C                   ENDIF
      *
      *  RTW QUALIFIER
      *
     C     SCRTWQ        IFNE      *BLANKS
     C                   MOVEL(P)  'RTWQ'        XXTBID
     C                   MOVEL(P)  SCRTWQ        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9025
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 08/20/07 **************************************
     C     SCRTW         IFNE      *ZERO
     C     SCRTWQ        ANDEQ     *BLANKS
      *** Begin Add *** 03/12/08 ********************************
     C     CLTID         ANDEQ     '065'
      *** End Add ***** 03/12/08 ********************************
     C                   SETON                                        9054
     C                   LEAVE
     C                   ENDIF
     C*** END ADD ***** 08/20/07 **************************************
     C*** BEGIN Add ****** 05/21/09 ************************************
      *  RTW Status
      *
     C     *IN12         IFEQ      *ON
     C                   SETOFF                                         59
     C     SCRTWS        IFNE      *BLANKS
     C                   MOVEL(P)  'RTWS'        XXTBID
     C                   MOVEL(P)  SCRTWS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9059
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   Add ****** 05/21/09 ************************************
     C*** BEG   ADD ****** 03/23/10 ************************************
     C** DATE EMPLOYER NOTIFIED
     C     SCDODD        IFNE      0
     C                   MOVE      SCDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDODD
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9091
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C** CURRENT DATE LAST DAY WORKED
     C     SCCDLW        IFNE      0
     C                   MOVE      SCCDLW        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDLW
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9093
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C** CURRENT DATE DISABILITY BEGAN
     C     SCCDDB        IFNE      0
     C                   MOVE      SCCDDB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDDB
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9094
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C** EDIT FOR VALID DATE
     C     SCDNLT        IFNE      0
     C                   MOVE      SCDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDNLT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9096
     C                   LEAVE
     C                   ENDIF
     C*
     C**DATE NOTIFIED OF LT SHOULD NOT BE LESS THAN ACCIDENT DATE
     C                   Z-ADD     SCDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDNLT
     C* ACCIDENT DATE LOADED FROM CMACDT
     C     XXDNLT        IFLT      XXACDT
     C                   MOVE      *ON           *IN13
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C* RETURN TO WORK WITH SAME EMPLOYER
     C     SCSAME        IFNE      'Y'
     C     SCSAME        ANDNE     'N'
     C     SCSAME        ANDNE     ' '
     C                   SETON                                        68
     C                   LEAVE
     C                   ENDIF
     C* TAX FILING STATUS
     C     SCETFS        IFNE      ' '
     C                   MOVEL(P)  'ETFS'        XXTBID
     C                   MOVEL(P)  SCETFS        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9072
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C* NON-CONSECUTIVE PERIOD CODE
     C     SCNCPC        IFNE      ' '
     C                   MOVEL     'NCPD'        XXTBID
     C                   MOVEL     SCNCPC        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9092
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C** INITIAL DATE OF LOST TIME
     C     SCIDLT        IFNE      0
     C                   MOVE      SCIDLT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCIDLT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9014
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** END ADD********* 03/23/10 ************************************
     C*** BEG ADD ****** 11/03/09 ***********************************  *
      *  Apportionment Indicator
      *
     C     CLTID         IFEQ      '135'
     C                   SETON                                        29
     C     SCAPIN        IFNE      *BLANKS
     C     SCAPIN        ANDNE     'N'
     C     SCAPIN        ANDNE     'Y'
     C                   SETON                                        903061
     C                   LEAVE
     C                   ENDIF
      *
     C     SCAPIN        IFEQ      'Y'
     C     SCAPCT        ANDEQ     *ZEROS
     C                   SETON                                        903562
     C                   LEAVE
     C                   ENDIF
      *
     C     SCAPIN        IFNE      'Y'
     C     SCAPCT        ANDGT     0
     C                   SETON                                        905162
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
     C*** END ADD ****** 11/03/09 ***********************************  *
      *
      *  PENSION OFFSET
      *
     C     SCPEFF        IFNE      *BLANKS
     C     SCPEFF        ANDNE     'N'
     C     SCPEFF        ANDNE     'Y'
     C                   SETON                                        9020
     C                   LEAVE
     C                   ENDIF
      *
      *  UNEMPLOYMENT OFFSET
      *
     C     SCUEFF        IFNE      *BLANKS
     C     SCUEFF        ANDNE     'N'
     C     SCUEFF        ANDNE     'Y'
     C                   SETON                                        9021
     C                   LEAVE
     C                   ENDIF
      *
      *  SOC SEC OFFSET
      *
     C     SCSSFF        IFNE      *BLANKS
     C     SCSSFF        ANDNE     'N'
     C     SCSSFF        ANDNE     'Y'
     C                   SETON                                        9022
     C                   LEAVE
     C                   ENDIF
      *
      *  OTHER OFFSET
      *
     C     SCOTFF        IFNE      *BLANKS
     C     SCOTFF        ANDNE     'N'
     C     SCOTFF        ANDNE     'Y'
     C                   SETON                                        9023
     C                   LEAVE
     C                   ENDIF
      *
      *  SPECIAL DISABILITY FUND OFFSET
      *
     C     SCSFFF        IFNE      *BLANKS
     C     SCSFFF        ANDNE     'N'
     C     SCSFFF        ANDNE     'Y'
     C                   SETON                                        9024
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      *
      *** BEGIN ADD *** 08/04/98 **************************************
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** END ADD ***** 08/04/98 **************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     2             SCRN#
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *** BEGIN ADD *** 08/04/98 **************************************
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** END ADD ***** 08/04/98 **************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     3             SCRN#
     C                   ENDSR                                                  ERRCK3
      *
      *===============================================================
      * KGDO - SUBROUTINE FOR F7
      *===============================================================
      *
      *  F7=CLAIMANT FINANCIAL INFO WINDOW
      *
     C     KGDO          BEGSR
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * KHDO - SUBROUTINE FOR F8
      *===============================================================
      *
      *  F8=WORK HISTORY
      *
     C     KHDO          BEGSR
     C*
     C*** BEG CHG *** 02/07/02 ****************************
     C*
     C     CLTID         IFEQ      '114'
     C*
     C*  GET THE RRN OF THE WCMC CASE
     C*
     C                   MOVE      CMCO#         XYCO#
     C                   MOVE      CMFND         XYFND
     C                   MOVE      CMFYR         XYFYR
     C                   MOVE      CMCASE        XYCASE
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#             3
     C                   PARM                    XYFND             3
     C                   PARM                    XYFYR             3
     C                   PARM                    XYCASE            7
     C                   PARM      *ZEROS        XYRRN1            9 0
     C                   PARM      *BLANK        XYRTCD            1
     C*
     C     XYRTCD        IFNE      'F'
     C*
     C                   CALL      'F0176R'
     C     *INKC         PARM      '0'           @@KC
     C     *INKL         PARM      '0'           @@KL
     C                   PARM                    @@CALL            1
     C                   PARM      *ZEROS        @@RRN2            9 0          FAWHP
     C                   PARM      XYRRN1        @@RRN3            9 0          FMCMP
     C*
      *
      * IF F3 IS HIT, RETURN TO CLAIM MENU.
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
     C*
     C                   ENDIF                                                   XXRTCD
     C*
     C                   ELSE
     C*
     C*** END CHG *** 02/07/02 ****************************
      *
     C*** BEGIN ADD *** 08/23/10 **************************************
     C                   MOVE      '0'           QQCALL
     C*** END   ADD *** 08/23/10 **************************************
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           QQCALL
     C                   END
      *
     C                   CALL      'W4315R'
     C                   PARM                    QQPRDS           40
     C*** BEGIN CHG *** 08/23/10 **************************************
     C**********           PARM *ZERO     QQCALL  1
     C                   PARM                    QQCALL            1
     C*** END   CHG *** 08/23/10 **************************************
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    QQRTCD            1
      *** BEG ADD *** 05/20/08 ****************************
     C                   PARM      *ZEROS        XXTADL            4 0
     C                   PARM      'N'           XXUPDC            1            UPDATE CLAIM
      *
      * MOVE TOTAL DAYS LOST TO PSUEDO SCREEN FIELD TO HOLD THE VALUE
      *
     C                   Z-ADD     XXTADL        SCTADL            4 0
      *** END ADD *** 05/20/08 ****************************
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
     C*
     C*** BEG ADD *** 02/07/02 ****************************
     C                   ENDIF
     C*** END ADD *** 02/07/02 ****************************
      *
     C                   ENDSR
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     KJDO          BEGSR
     C                   MOVEA     '000000'      *IN(41)                           41 - 46
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
      *
      *  METHOD OF DETERMINING PRE-INJURY WAGE
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCWMTH'
     C                   MOVEL(P)  'WMTH'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN41                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCWMTH
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCWMTD
     C                   MOVE      *ON           *IN42                          POS CUSR
     C                   MOVE      *OFF          *IN41
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 09/24/98 **************************************
      *  Agreement to compensate code search
      *
     C     HFLD          WHENEQ    'SCAGCD'
     C                   MOVEL(P)  'AGCP'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN45                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCAGCD
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCAGDS
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   END
     C*** END ADD ***** 09/24/98 **************************************
      *
      *  WAGE PERIOD
      *
     C     HFLD          WHENEQ    'SCWGP'
     C                   MOVEL(P)  'FRWP'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCWGP
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCWGPD
     C                   MOVE      *ON           *IN44                          POS CUSR
     C                   MOVE      *OFF          *IN43
     C                   END
     C                   END
      *
      *  RTW QUALIFIER
      *
     C     HFLD          WHENEQ    'SCRTWQ'
     C                   MOVEL(P)  'RTWQ'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      *BLANK
     C                   MOVE      *ON           *IN45                          POS CUSR
     C                   MOVE      *OFF          *IN46                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCRTWQ
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRTWD
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   MOVE      *OFF          *IN45
     C                   END
     C                   END
     C*** BEGIN Add ****** 05/21/09 ************************************
      *
      *  RTW Status
      *
     C     HFLD          WHENEQ    'SCRTWS'
     C                   MOVEL(P)  'RTWS'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      *BLANK
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   MOVE      *OFF          *IN56                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCRTWS
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRTSD
     C                   MOVE      *ON           *IN56                          POS CUSR
     C                   MOVE      *OFF          *IN46
     C                   END
     C                   END
     C*** END   Add ****** 05/21/09 ************************************
      *
     C*** BEG ADD********* 03/23/10 ************************************
     C*  REDUCED BENEFIT AMOUNT CODE
     C     HFLD          WHENEQ    'SCRBAC'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'RBAC'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCRBAC
     C                   MOVE      *ON           *IN68
     C                   ENDIF
     C*
     C*   NON-CONSECUTIVE PERIOD SEARCH
     C     HFLD          WHENEQ    'SCNCPC'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'NCPD'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCNCPC
     C                   MOVE      *ON           *IN17
     C                   ELSE
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C*
     C*   TAX FILLING STATUS SEARCH
     C     HFLD          WHENEQ    'SCETFS'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'ETFS'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCETFS
     C                   MOVE      *ON           *IN17
     C                   ELSE
     C                   MOVE      *ON           *IN71
     C                   ENDIF
     C*** END ADD******* 03/23/10 **************************************
     C                   OTHER
     C                   MOVE      *ON           *IN18
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *===============================================================
      * KKDO - SUBROUTINE FOR F11
      *===============================================================
      *
      *  F11=BENEFIT ADJ
      *
     C     KKDO          BEGSR
      *
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           QQCALL
     C                   END
      *
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'W0070R9'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14 = DARTS
      *===============================================================
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      *
      *===============================================================
      * KSDO - SUBROUTINE FOR F18
      *===============================================================
      *
      *  F18=AVERAGE WEEKLY WAGE HISTORY
      *
     C     KSDO          BEGSR
      *
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           QQCALL
     C                   END
      *
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'W0070R8'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
      *
     C                   ENDSR
      *
      ****BEG ADD******03/23/10***************************************
      *===============================================================
      * KYDO - SUBROUTINE FOR F24 = MORE KEYS
      *===============================================================
     C     KYDO          BEGSR
     C*
     C***  Begin Del  ***  08/12/10  **************************************
     C********** CLTID     IFEQ '139'
     C********** *IN95     IFEQ *OFF
     C**********           MOVE *ON       *IN95
     C**********           ELSE
     C**********           MOVE *OFF      *IN95
     C**********           ENDIF
     C**********           ENDIF
     C***  End   Del  ***  08/12/10  **************************************
     C***  Begin Add  ***  08/12/10  **************************************
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C     FL1#2         ADD       1             FL1#1
B01  C     FL1#1         IFGT      12
     C     FL1(FL1#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL1#1
E01  C                   END
     C     FL1#1         ADD       1             FL1#2
     C                   MOVEL     FL1(FL1#1)    SC1FK1
     C                   MOVEL     FL1(FL1#2)    SC1FK2
     C     Q2RECF        WHENEQ    'SCR02'
     C     FL2#2         ADD       1             FL2#1
B01  C     FL2#1         IFGT      12
     C     FL2(FL2#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL2#1
E01  C                   END
     C     FL2#1         ADD       1             FL2#2
     C                   MOVEL     FL2(FL2#1)    SC2FK1
     C                   MOVEL     FL2(FL2#2)    SC2FK2
     C     Q2RECF        WHENEQ    'SCR03'
     C     FL3#2         ADD       1             FL3#1
B01  C     FL3#1         IFGT      12
     C     FL3(FL3#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL3#1
E01  C                   END
     C     FL3#1         ADD       1             FL3#2
     C                   MOVEL     FL3(FL3#1)    SC3FK1
     C                   MOVEL     FL3(FL3#2)    SC3FK2
     C                   ENDSL
     C***  End   Add  ***  08/12/10  **************************************
      /If Defined (WEB)
      /Free
       Clear sc1Fk1;
       Clear sc1Fk2;
       Clear sc2Fk1;
       Clear sc2Fk2;
       Clear sc3Fk1;
       Clear sc3Fk2;
      /End-free
      /Endif
     C*
     C                   ENDSR
      *===============================================================
      * KTDO - SUBROUTINE FOR F19 = EMPLOYEE PAID INFO
      *===============================================================
     C     KTDO          BEGSR
      *
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'W0070R3'
     C                   PARM      *BLANKS       QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      *BLANKS       QQRTCD
     C********BEG ADD ******** 09/01/10*******************************
     C                   PARM                    @@MODE
     C********END ADD ******** 09/01/10*******************************
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *
     C                   ENDSR
      ****END ADD******03/23/10***************************************
      ****BEG ADD******07/29/10***************************************
     C*===============================================================
     C* KUDO - SUBROUTINE FOR F20
     C*===============================================================
     C*
     C*  F20=Reduced Earnings
     C*
     C     KUDO          BEGSR
     C*
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C*
     C                   CALL      'W0070R1'
     C                   PARM      *BLANKS       ##PRDS           40
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
     C********BEG ADD ******** 09/01/10*******************************
     C                   PARM                    @@MODE
     C********END ADD ******** 09/01/10*******************************
     C*
     C     ##RTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     ##RTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF
     C*
     C                   ENDSR
      *===============================================================
     C*===============================================================
     C* KVDO - SUBROUTINE FOR F21
     C*===============================================================
     C*
     C*  F21=Concurrent Employer
     C*
     C     KVDO          BEGSR
     C*
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C*
     C                   CALL      'W0070R1A'
     C                   PARM      *BLANKS       ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    #@FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM      *BLANKS       ##RTCD
     C********BEG ADD ******** 09/01/10*******************************
     C                   PARM                    @@MODE
     C********END ADD ******** 09/01/10*******************************
     C*
     C     ##RTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     ##RTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF
     C                   ENDSR
      ****END ADD******07/29/10***************************************
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C     *IN40         IFEQ      *ON
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
      *
      *  LOAD KEYS
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C                   Z-ADD     CMDIV         SVDIV
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMCASE        SVCASE
     C                   Z-ADD     CMTIME        SVTIME
     C                   Z-ADD     CMDT          SVDATE
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C                   MOVE      *BLANK        SCMSG1
     C                   MOVE      *OFF          *IN69
     C     CMEXCS        IFNE      'Y'
     C     @@RRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
      *  GET THE DARTS SETUP CONTROL RECORD
      *
     C     CXKY          CHAIN     PCX01                              60
      *
      * GET SYSTEM CONTROL FILE
      *
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
      *
      * BAR CODE INSTALLED
      *
     C     SYBCIF        IFEQ      'Y'
     C*** BEGIN DLT *** 11/16/98 **************************************
     C******************** CALL 'WA027R'
     C******************** PARM           @@RRN   90
     C*** END DLT ***** 11/16/98 **************************************
     C*** BEGIN ADD *** 11/16/98 **************************************
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C*** END ADD ***** 11/16/98 **************************************
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *BLANK        SCMSG2
     C     QQBCFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN70
     C     QQTRTP        IFEQ      'I'
     C                   MOVEL(P)  XCBC02        SCMSG2
     C                   ELSE
     C                   MOVEL(P)  XCBC01        SCMSG2
     C*** BEGIN CHG *** 11/16/98 **************************************
     C******************** MOVE QQTUSR    SCMSG2
     C                   CAT       QQTUSR:1      SCMSG2
     C*** END CHG ***** 11/16/98 **************************************
     C                   END
     C                   END
     C                   END
      *
     C                   END                                                     *IN60  SSY01
      *
     C                   Z-ADD     CMFND         SCFND
     C                   MOVE      *BLANK        SCFNAM
      *
      *  GET FUND NAME & IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNAM
     C     FNEXDT        IFGT      *ZERO
     C     FNEXDT        IFLE      XXDT
     C                   MOVE      *ON           *IN09
     C                   MOVE      'D'           @@MODE
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
     C                   END
     C                   END
      *
      *** BEGIN ADD *** 04/13/98 ***********************************************
      *  Get the EDI control record to determine if the jurisdiction state
      *    for this claim uses EDI.
      *
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '148'         XX148
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C*** END CHG ****** 03/24/10 *************************************
     C  N60              MOVE      'Y'           XXEDI             1
     C   60              MOVE      'N'           XXEDI
      *** END ADD ***** 04/13/98 ***********************************************
      *
      ******               Z-ADDCMEMP#    SCEMP#
      ******               Z-ADDCMDIV     SCDIV
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      ******               MOVELEMNAME    SCENAM    P
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
      *
     C                   END                                                    WMEMP
      *
      *  CLAIMANT SS#
      *
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
      *  CLAIMANT NAME
      *
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   MOVE      *BLANK        SCPOI
     C                   MOVEL(P)  CMPOI         SCPOI
      *
     C                   MOVE(P)   *BLANK        SCFSAL
     C                   MOVEL(P)  CMFSAL        SCFSAL
     C*** BEGIN ADD *** 10/16/00 **************************************
     C                   Z-ADD     CMFSAM        SCFSAM
     C                   Z-ADD     CMFSWK        SCFSWK
     C                   Z-ADD     CMFSDY        SCFSDY
     C*** END ADD ***** 10/16/00 **************************************
      *
     C                   MOVE      *BLANK        SCFSDT
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFSDT
      *
     C                   MOVE(P)   *BLANK        SCAGCP
     C                   MOVEL(P)  CMAGCP        SCAGCP
     C     CMLTFL        IFEQ      'R'
     C     CMLTFL        OREQ      'M'
     C                   MOVEL(P)  'N'           SCAGCP
     C                   END
     C                   XFOOT     RR            XXRSV            11 2
      *
     C                   MOVEL(P)  CMVORC        SCVORC
     C                   Z-ADD     CMPRWW        SCPRWW
     C                   MOVEL(P)  CMWMTH        SCWMTH
     C                   MOVE(P)   *BLANK        SCWMTD
     C                   MOVEL(P)  'WMTH'        XXTBID
     C                   MOVEL(P)  CMWMTH        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCWMTD
     C                   ENDIF
      *
     C                   Z-ADD     CMPOWW        SCPOWW
     C*** BEGIN ADD 08/16/2002 ****************************************
     C                   Z-ADD     CMOTWK        SCOTWK
     C*** END   ADD 08/16/2002 ****************************************
      ************         Z-ADDCMFWW     SCFWW
      *
     C                   Z-ADD     CMAWW         SCAWW
     C                   Z-ADD     CMAWW         SVAWW2
      ***  BEGIN ADD ******   03/17/08 *******************************
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      *ON           *IN58
     C                   ELSE
     C                   MOVE      *OFF          *IN58
     C                   END
      ***  END ADD   ******   03/17/08 *******************************
     C*** BEGIN DLT *** 01/07/98 **************************************
     C********** CMAWW     IFEQ *ZERO
     C********** CMLTFL    ANDEQ'I'
     C**********           Z-ADD1         SCAWW
     C**********           END
     C*** END DLT ***** 01/07/98 **************************************
     C                   Z-ADD     SCAWW         SVAWW
      *** BEGIN ADD *** 08/09/99 **************************************
      *  Clear calculated comp rate.
     C                   MOVE      *ZERO         SCCCRT
      *** END ADD ***** 08/09/99 **************************************
      *
      * COMP RATE - DEFAULT FROM WDRBP ONCE LTFL = I
      *
     C                   Z-ADD     CMCRT         SCCRT                          COMP RATE
      *** BEGIN ADD *** 08/03/99 **************************************
     C                   Z-ADD     CMCRT         SVCRT                          MAX COMP RATE
      *** END ADD ***** 08/03/99 **************************************
     C                   Z-ADD     *ZERO         SCMCR                          MAX COMP RATE
     C                   Z-ADD     *ZERO         SC#DFS
     C     CMLTFL        IFEQ      'I'
     C*** BEGIN ADD *** 01/07/98 **************************************
     C     CMAWW         ORNE      *ZERO
     C*** END ADD ***** 01/07/98 **************************************
     C                   MOVE      *BLANK        XXJRST
      *
     C                   MOVEL     'N'           XXRB01            1            RECORD FLAG
     C                   MOVEL     CMJRST        XXJRST
     C                   Z-ADD     CMACDT        XXEFDT
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'Y'           XXRB01                         RECORD FLAG
     C                   Z-ADD     RBDYS         SC#DFS                           # OF DAYS F SAL
     C     SC#DFS        IFNE      *ZERO
     C                   MOVE      *ON           *IN52                            DSP THE FIELD
     C                   END
      *
     C     CMUSLH        IFNE      'Y'                                          ADD 07/10/00
     C                   Z-ADD     RBPACP        SCMCR                            MAX COMP RATE
      *** BEG ADD *** 07/10/00  ***************************
     C                   ELSE
     C                   Z-ADD     RBLHMX        SCMCR                            MAX COMP RATE
     C                   ENDIF
      *** END ADD *** 07/10/00  ***************************
      *
     C*** BEG CHG *** 06/02/04 ****************************
     C********** RBREPC    IFGT *ZERO
     C********** RBREPC    DIV  100       XXREPC  54H        CVT  COMP RATE
     C     RBCMPR        IFGT      *ZERO
     C*** Begin chg *** 05/30/06 ****************************
     C*******    RBCMPR    DIV  100       XXCMPR  74H        CVT  COMP RATE
     C     RBCMPR        DIV(H)    100           XXCMPR            7 6            CVT  COMP RATE
     C*** End chg ***** 05/30/06 ****************************
     C*** END CHG *** 06/02/04 ****************************
      ***  BEGIN ADD  ***  04/23/98  *********************************
     C     SCCRT         IFEQ      *ZEROS
      ***  END   ADD  ***  04/23/98  *********************************
      **** BEGIN ADD **** 04/27/99 ************************************
     C     @@MODE        ANDEQ     'A'
      **** END   ADD **** 04/27/99 ************************************
     C*** BEG CHG *** 06/02/04 ****************************
     C********** CMAWW     MULT XXREPC    SCCRT     H        CALC COMP RATE
     C     CMAWW         MULT(H)   XXCMPR        SCCRT                            CALC COMP RATE
     C*** END CHG *** 06/02/04 ****************************
      ***  BEGIN ADD  ***  04/23/98  *********************************
     C                   ENDIF                                                  SCCRT=*ZEROS
     C*** BEG CHG *** 06/02/04 ****************************
     C********** CMAWW     MULT XXREPC    SCCCRT    H        CALC COMP RATE
     C     CMAWW         MULT(H)   XXCMPR        SCCCRT                           CALC COMP RATE
     C*** END CHG *** 06/02/04 ****************************
      ***  END   ADD  ***  04/23/98  *********************************
     C                   END
     C     CMUSLH        IFNE      'Y'                                          ADD 07/10/00
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCRT         IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        SCCRT
     C                   END
      *** BEGIN DELETE *** 08/03/99 ***********************************
      **********************
      ******************* COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
      *
     C********** SCCRT     IFGT RBPACP
     C**********           Z-ADDRBPACP    SCCRT
     C**********           END
      *** END DELETE ***** 08/03/99 ***********************************
      *
      ***  BEGIN ADD  ***  04/23/98  *********************************
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCCRT        IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBMNRT
      *
      *  COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
      *
     C     SCCCRT        IFGT      RBPACP
     C                   Z-ADD     RBPACP        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBPACP
      *** BEG ADD *** 07/10/00  ***************************
     C                   ELSE
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCRT         IFLT      RBLHMN
     C                   Z-ADD     RBLHMN        SCCRT
     C                   END
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     SCCCRT        IFLT      RBLHMN
     C                   Z-ADD     RBLHMN        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBMNRT
      *
      *  COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
      *
     C     SCCCRT        IFGT      RBLHMX
     C     RBLHMX        ANDNE     *ZEROS
     C                   Z-ADD     RBLHMX        SCCCRT
     C                   ENDIF                                                  SCCCRT<RBPACP
     C                   ENDIF
      *** END ADD *** 07/10/00  ***************************
      ***  END   ADD  ***  04/23/98  *********************************
      *
      * FULL SALARY VOUCHER PROCESSING FLAG
      *
     C                   MOVE      'N'           SCFSVP
     C     RBVCHR        IFNE      *BLANK
     C                   MOVE      'Y'           SCFSVP
     C                   ELSE
     C                   MOVE      'N'           SCFSVP
     C                   END
      *
     C                   END                                                    *IN60=0 PRB01
      *
     C                   END                                                    CMLTFL = I
      *
     C                   MOVE      *BLANK        SCWGP
     C                   MOVE      *BLANK        SCWGPD
     C                   MOVEL(P)  CMWGPD        SCWGP
     C                   MOVEL(P)  'FRWP'        XXTBID
     C                   MOVEL(P)  SCWGP         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCWGPD
     C                   END
      *
      ***********          MOVELSCACDT    SCACD2    P
      *
     C                   MOVE      *BLANK        SCDTLW
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTLW
      *
     C                   MOVE      *BLANK        SCDBDT
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDBDT
      *
     C                   MOVE      *BLANK        SCRLWK
      *** BEG CHG *** 02/10/98 **************************************
      ***                  Z-ADDCMRLWK    SRDTI
     C                   Z-ADD     CMRLW         SRDTI
      *** END CHG *** 02/10/98 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRLWK
      *
     C                   MOVE      *BLANK        SCERTW
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCERTW
      *
     C                   MOVE      *BLANK        SCRTW
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
      *
     C                   MOVE(P)   *BLANK        SCRTWQ
     C                   MOVEL(P)  CMRTWQ        SCRTWQ
     C                   MOVE      *BLANK        SCRTWD
     C                   MOVEL(P)  'RTWQ'        XXTBID
     C                   MOVEL(P)  CMRTWQ        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCRTWD
     C                   END
     C*** BEGIN Add ****** 05/21/09 ************************************
     C     *IN12         IFEQ      *ON
     C                   MOVE(P)   *BLANK        SCRTWS
     C                   MOVEL(P)  CMRTWS        SCRTWS
     C                   MOVE      *BLANK        SCRTSD
     C                   MOVEL(P)  'RTWS'        XXTBID
     C                   MOVEL(P)  CMRTWS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCRTSD
     C                   END
     C                   ENDIF
     C*** END   Add ****** 05/21/09 ************************************
     C*** BEG ADD *** 11/03/09 ***********************************     *
     C     *IN29         IFEQ      *ON
     C     C1KY          CHAIN     PC101                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   *BLANK        SCAPIN
     C                   MOVEL(P)  C1APIN        SCAPIN
     C                   Z-ADD     C1APCT        SCAPCT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/03/09 ***********************************     *
      *
     C                   Z-ADD     CMLOST        SCLOST
      *** BEG ADD *** 05/20/08 ****************************
     C                   Z-ADD     CMDOTR        SCDOTR
     C                   Z-ADD     CMTADL        SCTADL
      *** END ADD *** 05/20/08 ****************************
      *
      *** BEG ADD *** 03/23/10 ****************************
     C                   MOVEL     CMDFBE        SCDFBE
     C                   MOVEL     CMRBAC        SCRBAC
      *** END ADD *** 03/23/10 ****************************
     C                   MOVEL(P)  CMPEFF        SCPEFF
     C                   MOVEL(P)  CMUEFF        SCUEFF
     C                   MOVEL(P)  CMSSFF        SCSSFF
     C                   MOVEL(P)  CMOTFF        SCOTFF
     C                   MOVEL(P)  CMSFFF        SCSFFF
     C*** BEGIN ADD *** 09/24/98 **************************************
     C                   MOVEL(P)  CMAGCD        SCAGCD
     C                   MOVE      *BLANKS       SCAGDS
     C                   MOVEL(P)  'AGCP'        XXTBID
     C                   MOVEL(P)  CMAGCD        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCAGDS
     C*** END ADD ***** 09/24/98 **************************************
      *
      ***** BEG ADD ***** 04/01/05 ***************************************
     C     *IN47         IFEQ      *ON
     C                   Z-ADD     CMPDRT        SCPDRT                         PD Rate for CA
     C                   Z-ADD     CMLPRT        SCLPRT                         LP Rate for CA
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCPDRT                         PD Rate for CA
     C                   Z-ADD     *ZEROS        SCLPRT                         LP Rate for CA
     C                   ENDIF                                                  *IN47 = *ON
      ***** END ADD ***** 04/01/05 ***************************************
      *
      *  GET THE LAST COMP PAYMENT INFO
      *
     C     PAKY          SETGT     PPA01
     C     *IN60         DOUEQ     *ON
     C     PAKY          READPE    PPA01                                  60
     C     *IN60         IFEQ      *OFF
      *
     C     PARETP        IFEQ      'C'
     C                   MOVE      *ON           *IN57                          DISPLAY
     C                   ADD       PAAMT         XXAMT            11 2
     C                   LEAVE
     C                   END
      *
     C                   END
     C                   ENDDO
      *
     C                   MOVE      *ON           *IN19                          DSPATR(PC)
     C**** BEG  ADD *****  03/23/10  **************************************
     C                   Z-ADD     CMDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDNLT
     C                   Z-ADD     CMDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDODD
     C                   Z-ADD     CMIDLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCIDLT
     C*
     C                   MOVE      CMCDLW        SCCDLW
     C                   Z-ADD     CMCDLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCCDLW
     C*
     C                   MOVE      CMCDDB        SCCDDB
     C                   Z-ADD     CMCDDB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCCDDB
     C                   MOVE      CMSAME        SCSAME
     C                   MOVE      CMNCPC        SCNCPC
     C                   MOVE      CMENEX        SCENEX
     C                   MOVE      CMETFS        SCETFS
     C**** END  ADD *****  03/23/10  **************************************
     C***  Begin Add  ***  02/27/08  **************************************
     C     CLTID         IFEQ      '065'
     C     CMLTFL        ANDEQ     'I'
     C**** BEG  CHG *****  05/17/10  **************************************
     C*********            MOVE *ON       *IN98
     C******               ELSE
     C*********            MOVE *OFF      *IN98
     C                   MOVE      X'26'         ATR
     C**** END  CHG *****  05/17/10  **************************************
     C                   ENDIF
     C***  End   Add  ***  02/27/08  **************************************
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
      *    Set flag to update MC if data has changed.
      *
     C                   MOVE      *OFF          XXAPIF
     C     XXMANC        IFEQ      *ON
     C     SCSCL#        IFNE      CMACL#
     C     XXDTLW        ORNE      CMDTLW
     C                   MOVE      *ON           XXAPIF
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  CLAIM MASTER FILE
     C                   MOVEL     SCSCL#        CMACL#
      *
     C                   MOVEL(P)  SCPOI         CMPOI
     C                   MOVEL(P)  SCFSAL        CMFSAL
     C*** BEGIN ADD *** 10/16/00 **************************************
     C                   Z-ADD     SCFSAM        CMFSAM
     C                   Z-ADD     SCFSWK        CMFSWK
     C                   Z-ADD     SCFSDY        CMFSDY
     C*** END ADD ***** 10/16/00 **************************************
      *
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMFSDT
     C                   MOVEL(P)  SCAGCP        CMAGCP
     C                   MOVEL(P)  SCVORC        CMVORC
      *
     C                   Z-ADD     SCPRWW        CMPRWW
     C                   MOVEL(P)  SCWMTH        CMWMTH
     C                   Z-ADD     SCPOWW        CMPOWW
     C*** BEGIN ADD 08/16/2002 ****************************************
     C                   Z-ADD     SCOTWK        CMOTWK
     C*** END   ADD 08/16/2002 ****************************************
      ************         Z-ADDSCFWW     CMFWW
      *
     C                   Z-ADD     SCAWW         CMAWW
      *
     C                   Z-ADD     SCCRT         CMCRT
     C                   MOVEL(P)  SCWGP         CMWGPD
      *
     C                   Z-ADD     SCDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDTLW
      *
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDBDT
      *
     C                   Z-ADD     SCERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMERTW
      *
     C                   Z-ADD     SCRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMRTW
      *
     C                   MOVEL(P)  SCRTWQ        CMRTWQ
     C*** BEGIN ADD ****** 05/21/09 ************************************
     C                   MOVEL(P)  SCRTWS        CMRTWS
     C*** END   ADD ****** 05/21/09 ************************************
      *
      ************         Z-ADDSCPRTW    CMPRTW
      *
     C                   MOVEL(P)  SCPEFF        CMPEFF
     C                   MOVEL(P)  SCUEFF        CMUEFF
     C                   MOVEL(P)  SCSSFF        CMSSFF
     C                   MOVEL(P)  SCOTFF        CMOTFF
     C                   MOVEL(P)  SCSFFF        CMSFFF
     C*** BEGIN ADD *** 09/24/98 **************************************
     C                   MOVEL(P)  SCAGCD        CMAGCD
     C*** END ADD ***** 09/24/98 **************************************
      *** BEG ADD *** 05/20/08 ****************************
     C                   Z-ADD     SCDOTR        CMDOTR
     C                   Z-ADD     SCTADL        CMTADL
      *** END ADD *** 05/20/08 ****************************
      *
      *** BEG ADD *** 03/23/10 ****************************************
     C                   MOVEL     SCDFBE        CMDFBE
     C                   MOVEL     SCRBAC        CMRBAC
     C                   Z-ADD     SCDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDNLT
     C                   Z-ADD     SCDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDODD
     C                   Z-ADD     SCIDLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMIDLT
     C*
     C                   Z-ADD     SCCDLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMCDLW
     C*
     C                   Z-ADD     SCCDDB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMCDDB
     C                   MOVE      SCSAME        CMSAME
     C                   MOVE      SCNCPC        CMNCPC
     C                   MOVE      SCENEX        CMENEX
     C                   MOVE      SCETFS        CMETFS
     C**** END  ADD *****  03/23/10  **************************************
      ***** BEG ADD ***** 04/01/05 ***************************************
     C     *IN47         IFEQ      *ON
     C                   Z-ADD     SCPDRT        CMPDRT                         PD Rate for CA
     C                   Z-ADD     SCLPRT        CMLPRT                         LP Rate for CA
     C                   ENDIF                                                  *IN47 = *ON
      ***** END ADD ***** 04/01/05 ***************************************
     C*** BEG ADD *** 11/03/09 ****************************************
     C     *IN29         IFEQ      *ON
      *
     C     C1KY          CHAIN     PC101                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     Q1USER        C1USER
     C                   MOVEL     Q1PGM         C1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C                   MOVEL     DSTIME        C1TIME
     C                   MOVEL(P)  SCAPIN        C1APIN
     C                   Z-ADD     SCAPCT        C1APCT
      *
     C                   UPDATE    PC101
      *
     C                   ELSE
      *
     C                   MOVE      XXCO#         C1CO#
     C                   MOVE      XXFND         C1FND
     C                   Z-ADD     XXFYR         C1FYR
     C                   MOVE      XXCASE        C1CASE
     C                   MOVEL(P)  SCAPIN        C1APIN
     C                   Z-ADD     SCAPCT        C1APCT
     C                   MOVEL     'A'           C1STAT
     C                   MOVEL     Q1USER        C1USER
     C                   MOVEL     Q1PGM         C1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C                   MOVEL     DSTIME        C1TIME
     C                   MOVEL     C1USER        C1IUSR
     C                   MOVEL     C1PGM         C1IPGM
     C                   MOVEL     C1DT          C1IDT
     C                   MOVEL     C1TIME        C1ITM
      *
     C                   WRITE     PC101
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  *IN29=*ON
     C*** END ADD *** 11/03/09 ****************************************
      *
     C                   ENDSR                                                  MVSCDB
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
      *
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
     C                   MOVEA     '00'          *IN(18)                        *IN18 - 19
     C                   MOVEA     '00000000'    *IN(20)                        *IN20 - 27
     C                   MOVEA     '00000000'    *IN(28)                        *IN28 - 35
     C                   MOVEA     '000'         *IN(36)                        *IN36 - 38
     C                   MOVEA     '00'          *IN(50)                        *IN50 - 51
     C                   SETOFF                                       59
     C*
     C***  Begin Add  ***  08/12/10  **************************************
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVEA     VF1           VF
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEA     VF2           VF
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEA     VF3           VF
     C                   ENDSL
     C*
     C                   EXSR      CHKVFK
     C*
     C     *IN90         IFEQ      *OFF
     C***  End   Add  ***  08/12/10  **************************************
      *
     C     *INKG         CASEQ     *ON           KGDO                     90      F7  FINANCIAL
     C     *INKH         CASEQ     *ON           KHDO                     90      F8  WORK HIST
     C     *INKJ         CASEQ     *ON           KJDO                     90      F10 SEARCH
     C     *INKK         CASEQ     *ON           KKDO                     90      F11 BENEFIT A
     C     *INKN         CASEQ     *ON           KNDO                     90      F14 DARTS
      ****       *INKQ     CASEQ*ON       KQDO           90  F16 WORK HIST
     C     *INKS         CASEQ     *ON           KSDO                     90      F18 AWW HIST
     C*** BEG ADD *** 03/23/10 ****************************************
     C     *INKT         CASEQ     *ON           KTDO                     90      F19 PAID INFO
     C     *INKY         CASEQ     *ON           KYDO                     90      F24 MORE KEYS
     C*** END ADD *** 03/23/10 ****************************************
     C*** BEG ADD *** 07/29/10 ****************************************
     C     *INKU         CASEQ     *ON           KUDO
     C     *INKV         CASEQ     *ON           KVDO
     C*** END ADD *** 07/29/10 ****************************************
     C                   ENDCS
      *
     C***  Begin Add  ***  08/12/10  **************************************
     C                   ENDIF
     C***  End   Add  ***  08/12/10  **************************************
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
      *  DELETE THE CLAIM RECORD IF THE USER PRESSED F3 IN ADD MODE
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
      * *******            CLOSEBATRL
     C                   RETURN
     C                   ENDSR
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
     C                   Z-ADD     2             #OFSCR            1 0          # OF SCREENS
      *
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL(P)  MODEA         SCMODE
     C                   END
      *
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
      *
     C                   MOVE      *ON           DSPTYP
      *
      * GET WORKERS COMP CONTROL FILE
      *
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
      *
      * COMMERCIAL APPLICATION INSTALLED
      *
     C                   MOVE      *OFF          *IN66
     C     WCFL07        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   END
      *
      *** BEG ADD *** 05/20/08 ****************************
      *
      * OSHA 300 MODULE INSTALLED
      *
     C                   MOVE      *OFF          *IN16
     C     WCFL39        IFEQ      'Y'
     C                   MOVE      *ON           *IN16
     C                   END
      *
      *** END ADD *** 05/20/08 ****************************
     C                   END
      *
      *     WCMC Installed determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  GET SECURITY LEVEL OF THE USER
      *
     C                   MOVE      *BLANK        XXAUTH            1
     C                   MOVE      *OFF          *IN67
     C     UPLVL         IFGE      WCSL03
     C                   MOVE      *ON           *IN67
     C                   MOVE      'Y'           XXAUTH                         AUTHORIZED
     C                   END
      *
      *  WHEN RETURNING FROM DARTS, VERIFY THAT THE USER HAS
      *  THE PROPER AUTHORITY
      *
     C                   MOVE      *OFF          *IN15
     C     UPFL2         IFEQ      'Y'
     C                   MOVE      *ON           *IN15
     C                   END
      *
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     SRDTI
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      ***** END CHG ***** 03/06/06 ************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
     C                   MOVE      *BLANK        XXOVR             1
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
      ***** BEG ADD ***** 04/01/05 ***************************************
      * Check for California for additional Rate fields to display
     C                   MOVE      *OFF          *IN47
     C  N99CMJRST        IFEQ      'CA'
     C                   MOVE      *ON           *IN47
     C                   ENDIF                                                  *IN47 = *ON
      ***** END ADD ***** 04/01/05 ***************************************
      *
     C     EXKY          CHAIN     PEX01                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      *
      ***** END ADD ******* 08/10/98 **********************************
     C***  Begin Add  ***  09/06/05  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  09/06/05  **************************************
     C*** BEG ADD *** 05/21/09 ***********************************     *
     C     CLTID         IFEQ      '114'
     C     CLTID         OREQ      '133'
     C                   MOVE      *ON           *IN12
     C                   ELSE
     C                   MOVE      *OFF          *IN12
     C                   ENDIF
     C*** END ADD *** 05/21/09 ***********************************     *
     C*** BEG ADD *** 11/03/09 ***********************************     *
     C     CLTID         IFEQ      '135'
     C                   MOVE      *ON           *IN29
     C                   ELSE
     C                   MOVE      *OFF          *IN29
     C                   ENDIF
     C*** END ADD *** 11/03/09 ***********************************     *
     C*** BEG ADD ****** 05/17/10 *************************************
      *
      *
     C                   CALL      'S6000R'
     C                   PARM                    @@WCMC            1            WCMC INSTALLED
     C                   PARM                    @@CHK             1            CHECK REC
     C                   PARM                    @@BAR             1            INTRFACD W/WCMS
     C                   PARM                    @@DED             1
     C                   PARM                    @@ADMN            1
     C                   PARM                    @@CLM             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@MICR            1
     C     @@EDI         IFEQ      'Y'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF
     C*** END ADD ****** 05/17/10 *************************************
     C*
     C***  Begin Add  ***  08/12/10  **************************************
     C                   EXSR      SETFKS
     C                   EXSR      BLDLNS
     C***  End   Add  ***  08/12/10  **************************************
     C*
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      *
     C***  Begin Add  ***  08/12/10  **************************************
      *===============================================================
      * SETFKS - Set key values
      *===============================================================
     C     SETFKS        BEGSR
      *
     C                   MOVE      *BLANKS       XXSCRN            1
     C                   MOVE      *BLANKS       XXNMBR            2
     C                   MOVE      *BLANKS       XXCDA             3
      *
     C     @@EDI         IFEQ      'Y'
     C*
     C                   MOVE      'YYY'         XXCDA
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C                   MOVE      '2'           XXSCRN
     C                   EXSR      CHGCDA
     C*
     C                   MOVE      'YYN'         XXCDA
     C                   MOVE      '20'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C                   MOVE      '2'           XXSCRN
     C                   EXSR      CHGCDA
     C*
     C                   MOVE      'YYN'         XXCDA
     C                   MOVE      '21'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C                   MOVE      '2'           XXSCRN
     C                   EXSR      CHGCDA
      *
     C                   ENDIF
      *
     C                   ENDSR
     C***  End   Add  ***  08/12/10  **************************************
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             9997
      *
     C     *IN97         IFEQ      *ON
     C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN99                          SKIP TO BOTTOM
     C                   END
      *
B02  C     *IN99         IFEQ      *OFF
      *
      *** BEGIN DLT *** 08/04/98 **************************************
      ********** SVDATE    IFNE CMDT
      ********** SVTIME    ORNE CMTIME
      **********           CALL 'WA017R'
      **********           PARM           CMUSER
      **********           PARM           QQRTCD
      **********
      **********           ELSE
      *** END DLT ***** 08/04/98 **************************************
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
      *
      *  WRITE A AWW HISTORY RECORD IF SCAWW CHANGED AND UPON INITIAL ENTRY.
      *
     C     SCAWW         IFNE      SVAWW2
     C                   MOVE      XXCO#         AWCO#
     C                   MOVE      XXFND         AWFND
     C                   Z-ADD     XXFYR         AWFYR
     C                   MOVE      XXCASE        AWCASE
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AWTRDT
     C                   MOVE      SCAWW         AWAWW
     C                   MOVE      'A'           AWSTAT
     C                   MOVEL     Q1USER        AWUSER
     C                   MOVEL     Q1PGM         AWPGM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AWDT
     C                   Z-ADD     DSTIME        AWTIME
     C                   WRITE     PAW01
     C                   END
      *
      ** WORK HISTORY FILE
      **
      **                   MOVE *BLANK    XXWH    1
      **
B02   **         XXDBDT    IFGT *ZERO                      LOOSING TIME
B02   **         XXRTW     ANDEQ*ZERO
      **                   MOVEL'L'       WHWKST
      **                   Z-ADDXXDBDT    WHDATE
      **                   Z-ADDXXDBDT    XXDATE
      **                   MOVEL'Y'       XXWH
E02   **                   ENDIF
      **
B02   **         XXDBDT    IFGT *ZERO                      RTN TO WORK
B02   **         XXRTW     ANDGTXXDBDT
      **                   MOVEL'R'       WHWKST
      **                   Z-ADDXXRTW     WHDATE
      **                   Z-ADDXXRTW     XXDATE
      **                   MOVEL'Y'       XXWH
E02   **                   ENDIF
      **
B02   **         XXWH      IFEQ 'Y'
      **         WHKY      CHAINPWH01                61
B02   **         *IN61     IFEQ *ON
      **
      **                   Z-ADDCMCO#     WHCO#
      **                   Z-ADDCMFND     WHFND
      **                   Z-ADDCMFYR     WHFYR
      **                   Z-ADDCMCASE    WHCASE
      **                   MOVE *BLANK    WHSTAT
      **
      **                   MOVELQ1USER    WHUSER
      **                   MOVELQ1PGM     WHPGM
      **                   TIME           DSDTTM
      **                   MOVELDSTIME    WHTIME
      **                   Z-ADDDSDATE    SRDTI
      **                   CALL 'SRCVTDAT'DTCYMD
      **                   MOVELSRDTO     WHDT
      **                   WRITEPWH01
E02   **                   ENDIF                           *IN99=*OFF
E02   **                   ENDIF                           XXWH = Y
      *
     C                   UPDATE    DBRCDF
      *
      *    Call API to update Managed Care Claim Master - FMCMP
      *
     C                   CLEAR                   PARMCL
      *
     C     XXAPIF        CASEQ     *ON           MCAPIC
     C                   ENDCS                                                              DATE
      *
      *** BEGIN ADD *** 04/13/98 ***********************************************
      *  If this pgm was called using option 11 and EDI is active, call
      *    the EDI Log API.
      *
     C     Q1PRMS        IFLT      5
     C     XXEDI         ANDEQ     'Y'
     C     WCFL09        ANDEQ     'Y'
     C     SCRN#         ANDEQ     #OFSCR
      *
     C                   MOVE      XXCO#         QACO#
     C                   MOVE      CMFND         QAFND
     C                   MOVE      CMFYR         QAFYR
     C                   MOVE      CMCASE        QACASE
     C                   CALL      'W0070R6'
     C                   PARM                    QACO#             3
     C                   PARM                    QAFND             3
     C                   PARM                    QAFYR             3
     C                   PARM                    QACASE            7
     C                   PARM      *BLANK        QQACTF            1
     C                   PARM      *BLANKS       QQMGID            7
     C                   PARM                    QQRTCD            1
      *
     C                   END
      *** END ADD ***** 04/13/98 ***********************************************
      *
      * CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED FROM CLM MASTER
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   EXSR      CALLR
     C                   END
      *
      *** BEGIN DLT *** 08/04/98 **************************************
E02   ******************** ENDIF                           SVDATE NE CMDATE
      *** END DLT ***** 08/04/98 **************************************
E02  C                   ENDIF                                                  *IN99=*OFF
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  UPDATR
      *
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
      *
     C                   ENDSR                                                  WRITER
      *
      *===============================================================
      * MCAPIC - DATA INTERFACE: MC CLAIM MASTER - FMCMP
      *===============================================================
      *
     C     MCAPIC        BEGSR
      *
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ******************** Z-ADDCMACDT    SRDTI
      ******************** CALL 'SRCVTDAT'DTMDYY
      ******************** MOVELSRDTO     QMACDT
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE      CMACDT        QMACDT
      *** END ADD ***** 11/05/97 ***********************************************
      *
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
      *
     C                   MOVE      XXCO#         QMCO#
     C                   MOVE      *BLANKS       QMSUB#
     C                   MOVE      XXFND         QMFND
     C                   MOVE      XXFYR         QMFYR
     C                   MOVE      XXCASE        QMCASE
      *
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ****************     Z-ADDCMDTRP    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDTRP    P      Reported
      ****************     Z-ADDCMDTLW    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDTLW    P      Last worked
      ****************     Z-ADDCMENTD    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMENTD    P      ENTERED
      ****************     Z-ADDCMRCVD    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMRCVD    P      Received
      ****************     Z-ADDCMMMI     SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMMMI     P      Max med impr
      ****************     Z-ADDCMDOD     SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDEDT    P      Death date
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
      *** END ADD ***** 11/05/97 ***********************************************
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
      *
     C                   Z-ADD     DBRRN         QMRRN
      *
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ****************     Z-ADDCMDTRE    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDTRE    P      Rehabilitatn
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
      *** END ADD ***** 11/05/97 ***********************************************
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
      *** BEGIN ADD *** 04/07/98 **************************************
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
      *** END   ADD *** 04/07/98 **************************************
      *
      *=====================================================================
     C                   CALL      'FA003R'
      *
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
      *** BEGIN CHG *** 11/05/97 ***********************************************
      ******************** PARM           QMACDT  8
     C                   PARM                    QMACDT            7
      *** END CHG ***** 11/05/97 ***********************************************
      *
     C                   PARM                    QMCO#             3
     C                   PARM      *BLANK        QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
      *
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ******************** PARM           QMDTRP  8
      ******************** PARM           QMDTLW  8
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
      *** END ADD ***** 11/05/97 ***********************************************
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ******************** PARM           QMENTD  8
      ******************** PARM           QMRCVD  8
      ******************** PARM           QMMMI   8
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
      *** END ADD ***** 11/05/97 ***********************************************
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
      *** BEGIN CHANGE *** 09/02/99 ***********************************
     C******************** PARM CMACL#    QMSCL# 10        State claim numbr
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
      *** END CHANGE ***** 09/02/99 ***********************************
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
      *** BEGIN CHG *** 11/05/97 ***********************************************
      ******************** PARM           QMDEDT  8
     C                   PARM                    QMDEDT            7
      *** END CHG ***** 11/05/97 ***********************************************
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
      *** BEGIN CHG *** 11/05/97 ***********************************************
      ******************** PARM           QMDTRE  8
     C                   PARM                    QMDTRE            7
      *** END CHG ***** 11/05/97 ***********************************************
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
      *** BEGIN ADD *** 04/07/98 **************************************
     C                   PARM      CMOCDS        QQOCDS           40            OCDS
     C                   PARM      CMHIDT        QQHIDT            7 0          HIDT
     C                   PARM      CMESTS        QQESTS            2            ESTS
     C                   PARM                    QQWKPT            2            SHFT
     C                   PARM      CMPREX        QQPREX            1            PREX
     C                   PARM      CMACD2        QQACD2           60            ACD2
     C                   PARM      CMCVFL        QQCVFL            1            CVFL
     C                   PARM      CMMCO         QQMCA             1            MCO
     C                   PARM      CMIPCT        QQMMIP            5 2          IPCT
     C                   PARM      CMODIS        QQODIS            1            ODIS
     C                   PARM      CMYRLX        QQYRLX            4 0          YRLX
     C                   PARM      CMDBDT        QQDBDT            7 0          DBDT
     C                   PARM      CMHRSD        QQHRDY            3 0          HRSD
     C                   PARM      CMDAYS        QQDYWK            1 0          DAYS
      *** END   ADD *** 04/07/98 **************************************
      *** BEGIN ADD *** 08/11/98 **************************************
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
      *** END   ADD *** 08/11/98 **************************************
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C***** END   ADD **** 04/03/00 ***********************************
     C*** BEGIN ADD *** 08/25/00 **************************************
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C*** END   ADD *** 08/25/00 **************************************
      *
     C                   ENDSR
      *===============================================================
** Description--------------CDAN                           08/12/10
103F3=Exit                  YYN
105F5=Refresh               NYN
107F7=Financial summary     YYN
108F8=Work history          YYN
110F10=Search               YNY
111F11=Benefit adj          YYY
112F12=Cancel               YYY
114F14=DARTS                YYN
118F18=AWW history          YYN
119F19=Employer-Paid Info   NNN
120F20=Reduced Earnings     NNN
121F21=Concurrent Empl      NNN
203F3=Exit                  YYN
205F5=Refresh               NYN
207F7=Financial summary     YYN
208F8=Work history          YYN
210F10=Search               YNY
211F11=Benefit adj          YYY
212F12=Cancel               YYY
214F14=DARTS                YYN
218F18=AWW history          YYN
219F19=Employer-Paid Info   NNN
220F20=Reduced Earnings     NNN
221F21=Concurrent Empl      NNN
303F3=Exit                  YYN
*
