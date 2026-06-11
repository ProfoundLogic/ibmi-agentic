     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * ***************************************************************
      /TITLE W0070R06 - CLAIMS, EMPLOYER INFORMATION                  *
      * ***************************************************************
      *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY EMPLOYER INFORMATION.*
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R - CLAIM MASTER SEARCH                               *
      *                                                               *
      *  CALLS TO:   SA001R                                           *
      *              FA003R                                           *
      *                                                               *
      *  CREATED: 01/09/97  PROGRAMMER: C:47  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H*  09/03/10 C:180  SUPPORT 74011
     H*                  QUALIFY EDI EDIT
     H*
     H*  05/17/10 C:235  PROJRCT: 139WNF  Additional Changes          *
     H*                  All New field and function keys will be      *
     H*                  qualified on the WCMS EDI install flag='Y'   *
     H*                                                               *
     H*  05/07/10 C:198  Support: 72623                               *
     H*                  Fixed the logic where only accident address  *
     H*                  or accident location narrative is mandatory. *
     H*                                                               *
     H*  04/19/10 C:235  PROJRCT: 25WRG                               *
     H*                  CLAIMS GENERAL RETROFIT                      *
     H*                                                               *
     H*  03/23/10 C:235  PROJRCT: 139WNF                              *
     H*                  EDI SCREEN DESIGN                            *
     H*                                                               *
     H*  02/10/10 C:180  SUPPORT: 71668 FOR LCTA SAME EDITS AS PHI.   *
     H*
     H*  02/01/10 C:247  PROJECT: C25WQO - New auto diary features    *
     H*                                                               *
     H*  01/25/10 C:235  SUPPORT: 69921 FOR PHI ONLY                  *
     H*                  Logic is changed for PHI if EMPLOYER PREMISES*
     H*                  is enteres "N".user should always be able to *
     H*                  enter"PLACE OF ACCIDENT" if known and user   *
     H*                  must enter"ACCIDENT LOCATION NARRATIVE" or   *
     H*                  "CITY,STATE,ZIP" either one or the other.    *
     H*                  "ADDRESS" line is not mandatory.             *
     H*                                                               *
     H*  01/15/10 C:235  SUPPORT: 71323                               *
     H*                  ACCIDENT LOCATION NARRATIVE ERROR MESSAGE    *
     H*                  INCORRECT.ONLY COMPILE PGM.                  *
     H*                                                               *
     H*  01/04/10 C:190  PROJECT 25WRE                                *
     H*                  Add Campus Code field.                       *
     H*                                                               *
     H*  09/30/09 C:180  SUPPORT  70058  FIX ACCIDENT SITE EDIT
     H*                  BOTH ACCIDENT ADDRESS AND ACCIDENT SITE
     H*                  NARRATIVE CANNOT BE REQUIRED. THIS DOES NOT
     H*                  MEAN THAT USER MUST ENTER EITHER ONE OR THE
     H*                  OTHER.
     H*
     H*                  VERIFY ACCIDENT SITE IS COMPLETE IF ENTERED.
     H*
     H*  09/10/09 C:234  SUPPORT: 69713                               *
     H*                  ACCIDENT LOCATION NARRATIVE ERROR MESSAGE    *
     H*                  INCORRECT. COMPILE PGM.                      *
     H*                                                               *
     H*  08/11/09 C:180  WC1836 - Handle  OUT-OF-STATE for GA
     H*                           accident county WHEN applicable.
     H*
     H*  07/31/09 C:062  SUPPORT 69324
     H*                  CORRECT POSITIONING
     H*                                                               *
     H*  07/29/09 C:180  Change accident site/accident narrative edit
     H*                  to handle FL without affecting other states.
     H*
     H*  07/10/09 C:180  PROJECT WC1836 - Add county field.
     H*
     H*  04/30/09 C:189  SUPPORT: 67718                               *
     H*                  CURRENT EDITS ONLY SATISFYING FL CLAIMS AND  *
     H*                  NOT THE MISSOURI CLAIMS.RELAXING ZIP CODE    *
     H*                  REQUIRED EVEN IF 'ACCIDENT NARRATIVE" IS     *
     H*                  PRESENT ZIP CODE SHOULD BE ALLOWED TO ENTER  *
     H*                                                               *
     H* 04/20/09 C:189 PROJECT: SUPP#67718                            *
     H*                FURTHER EDITS NEEDED FOR ACCIDENT NARRATIVE    *
     H*                FIELD                                          *
     H*                                                               *
     H* 03/31/09 C:65  PROJECT: SUPP#67860                            *
     H*                POPULATE ADDITIONAL LOCATION DELETE INFO AT    *
     H*                START OF ERRCK1 SUBROUTINE IN CASE LOCATION    *
     H*                FIELD CHANGES.                                 *
     H*
     H* 03/19/09 C:189 SUPPORT: 67660                                 *
     F*                  Added field for the support item to address  *
     F*                  the issue with accident location             *
     H*
     H* 08/20/07 C:193 PROJECT: 65WGI                                 *
     H*                EDI Rel 3.0 Impl/Setup/Training                *
     H*                                                               *
     H* 06/04/07 C:188 PROJECT: FL061                                 *
     H*                Add policy state parm to call of FA003R        *
     H*
     H* 04/19/07 C:191 PROJECT: 132WGP                                *
     H*                Claim Contact Field Process                    *
     H*                                                               *
     H*  04/17/06 C:062 Support: CI132 52316
     H*                 Don't override JRST if not = blanks
     H*
     H*  03/06/06 C:132 Support: WC071 51160
     H*                Change Code to load DSDTTM and use DSDATE
     H*                instead of *DATE anywhere in the program.
     H*                                                               *
     H* 02/15/05 C:61  PROJECT: WC1128                                *
     H*                Auto DARTS processing.                         *
     H*                                                               *
     H* 04/11/02 C:15  Support: WC065/31844                           *
     H*                Chgd pgm to be consistent with the other claim *
     H*                screens when displaying the emp cancel dt.     *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H*  06/18/01 C:062 PROJECT: MC082 26153                          *
     H*                 IF WCMC INSTALLED CALL DELETION FILE BUILD    *
     H*                 PROGRAM.                                      *
     H*                                                               *
     H*  08/25/00 C:062 PROJECT: MC232                                *
     H*                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
     H*                                                               *
     H* 07/21/00 C:15  Support: WC065/23644                           *
     H*                Added code to call the 148 Log API if the Jurs *
     H*                State is changed and EDI is installed.         *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 04/03/00 C:118 PROJECT: WC854                                 *
     H*                Added parm to call of FA003R to pass WMCMP/    *
     H*                CMIMP.                                         *
     H*                                                               *
     H* 09/02/99 C:112 Support: 19256 (MC048)                         *
     H*                Change QMSCL# from 10A to 25A                  *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H* 01/29/99 C:112 Project: WC827                                 *
     H*            On selection of F3=Exit, a call to program         *
     H*            'W0070R04' asks for an explanation for aborting    *
     H*            the creation of a new claim.  The called program   *
     H*            now allows an F12=Cancel.  Changes to this program *
     H*            following the return from 'W0070R04' will bring the*
     H*            first screen for Claims, Employer Information entry*
     H*                                                               *
     H*                                                               *
     H* 11/16/98 C:15 Project: WC577                                  *
     H*               Changed the bar code process.  A bar code API   *
     H*               is now called to return certain parameters.     *
     H*                                                               *
     H* 10/12/98 C:112 Project:WC577                                  *
     H*                Added code to autoload address of location     *
     H*                of accident if Location Id is blank and        *
     H*                'Employer Premise' = 'Y'                       *
     H*                                                               *
     H*                Modified code to autoload correct addresses    *
     H*                if user manually modifies Location ID.  If     *
     H*                the Location ID is blank and the user changes  *
     H*                the Accident Location Address then the address *
     H*                will be stored in the Claim file.              *
     H*                                                               *
     H* 08/11/98 C:62 Project: MC155                                  *
     H*               ADDED PARM FOR CMFEAD TO FA003R                 *
     H*                                                               *
     H* 08/10/98 C:61 Project: 48WLC                                  *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H* 08/04/98 C:15 Project: WC577                                  *
     H*               Fixed the claim record lockout routine.         *
     H*                                                               *
     H* 04/28/98 C:15 Project: WC577                                  *
     H*               Added 5 addl parms to SRCCSC.  These parms are  *
     H*               addl security types.                            *
     H*                                                               *
     H*  04/23/98 C:33 PROJECT: WC577   WC060/10621                   *
     H*                CORRECT CHANGE MADE ON 03/10/98.               *
     H*                                                               *
     H* 04/07/98 C:62 Project: MC155                                  *
     H*               Added additional fields to FA003R parm list.    *
     H*                                                               *
     H*  03/10/98 C:33 PROJECT: WC577   WC060/10621                   *
     H*                CLEAR CLAIMANT NAME IF NOT FOUND.              *
     H*                                                               *
     H* 11/05/97 C:15 Project: WC729                                  *
     H*               Changed the format of date fields used in the   *
     H*               parm lists for pgms FA003R from MDCY to CYMD.   *
     H*                                                               *
     H*****************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  07 - EDI INSTALLED                                           *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      *  20 - INVALID CURSOR POSITION FOR SEARCH                      *
      *  40 - ADD MODE                                                *
      * N40 - UPDATE MODE                                             *
      *  51 - CLIENT ID = 132                                         *
      *  66 - COMMERCIAL APPLICATION INSTALLED                        *
      *  67 - SECURITY LEVEL VALID                                    *
      * N67 - SECURITY LEVEL INVALID                                  *
      *  69 - EXCESS REINSURANCE                                      *
      *  70 - BAR CODE INSTALLED                                      *
      *                                                               *
      * ***************************************************************
      *
     FW0070F06  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
      *
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS2)
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
      *
     FWMFNL     IF   E           K DISK
      *
     FWMCDL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
      *  ADDRESS LOCATION FILE
     FWMALL     IF   E           K DISK
     FWMALP     IF   E             DISK
     F                                     RENAME(PAL01:PAL0P)
      *
      *  RATE SET CODE
     FWMRSL     IF   E           K DISK
      *
      *  EMPLOYER DETAIL
     FWDELL     IF   E           K DISK
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      ***** END ADD ******* 08/10/98 **********************************
      *
      *  OFFICE MASTER
     FWMOFL     IF   E           K DISK
      *
      *  STATE MASTER
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** BEG   ADD *** 01/04/10 ***********************************
     FWMCLP     IF   E             DISK
     F                                     RENAME(PCL01:CLPRCF)
     FWMCLL     IF   E           K DISK
     F                                     RENAME(PCL01:CLRCDF)
     F                                     INFDS(DBFDS3)
     F*** END ADD ***** 01/04/10 ***********************************
     F*** BEG ADD ***** 04/19/10 ***********************************
     FWMCZL     IF   E           K DISK
     FWTCNL1    IF   E           K DISK
     F                                     RENAME(PCN01:PCN03)
     F*** END ADD ***** 04/19/10 ***********************************
      *
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
      *
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
      *_____________________________________________________________________
      *  Bank Info file       key=CO#/APPL/GRP/CKTP/EFDT
     F*****WMBIL2  IF  E           K        DISK
      *
      *_____________________________________________________________________
      *  CLAIM/DARTS CONTROL FILE
     FWMCXL     IF   E           K DISK
      *
      ***** BEGIN ADD ***** 07/10/09 ***************************
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PCN02)
      ***** END ADD ******* 07/10/09 ***************************
      /COPY SCOPYMT01D
     D CO              S              3  0 DIM(10) ASCEND                       REIN CO#
     D LM              S             11  2 DIM(10)                              REIN LIMIT
      *
      *  EMPLOYER'S EFFECTIVE AND CANCEL DATES
      *
      *
     D***PY SCOPYMT01I
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** BEG   ADD *** 01/04/10 ***********************************************
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*** END   ADD *** 01/04/10 ***********************************************
      *
     D DATEDS          DS
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
     D*
     D*   Parameters for MC Claim Master API        QMxxxx
     D*
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
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claims, Employer -
     D                                     Information')
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
     D********      1                     C         #OFSCR
     D COMAPI          C                   CONST('WA001R')
     D XXEMM1          C                   CONST('Employer cancel date-
     D                                       . . . : ')
     D XXALM1          C                   CONST('Location deletion da-
     D                                     te :')
     D XXCONH          C                   CONST('Contact')
     D DOTS            C                   CONST(' . . . . . . . . . .')
      ***** BEG ADD ******* 01/31/02 **********************************
     D***** BEGIN ADD ****** 08/11/09 *********************************
     D OUTST           C                   CONST('OUT-OF-STATE')

     I***** END ADD   ****** 08/11/09 *********************************
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
      ***** END ADD ******* 01/31/02 **********************************
      *
      *   KEY LIST FOR THE ENTITY/LOCATION FILE
      *
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCTG1
      *
     C     ALKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      ***BANK INFORMATION KEY
      *****
      *****      BIKY      KLIST
      *****                KFLD           XXCO#
      *****                KFLD           XXAPPL  2
      *****                KFLD           XXFND
      *****                KFLD           XXCKTP
      *****                KFLD           XXEFDT
      *****
      *****      BIKY1     KLIST
      *****                KFLD           XXCO#
      *****                KFLD           XXAPPL
      *****                KFLD           XXFND
      *****                KFLD           XXCKTP
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *   CLAIMS/DARTS CONTROL KEY LIST
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
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
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *   KEY LIST FOR THE FUND
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE OFFICE MASTER FILE
      *
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXOFC#
      *
      *   KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
      *
      *   KEY LIST FOR THE STATE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C**** BEG   ADD *** 01/04/10 ***********************************
      *  CAMPUS CODE FILE
      *
     C     CLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCMPS
     C*
     C     CLKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C**** END ADD ***** 01/04/10 ***********************************
     C**** BEG ADD ***** 04/19/10 ***********************************
      *
      * ZIP CODE BY COUNTY FILE
      *
     C     CZKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCACST
     C                   KFLD                    SCACZ1
     C**** END ADD ***** 04/19/10 ***********************************
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
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
     C     *LIKE         DEFINE    CMOFF#        XXOFC#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMCTG1        XXCTG1
     C     *LIKE         DEFINE    CMACDT        XXEFDT                         EFFECTIVE DATE
     C     *LIKE         DEFINE    CMACDT        XXDT                           TODAY'S DATE
     C     *LIKE         DEFINE    CMACDT        SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMACTM        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMACDT        XXCADT                         EMP CANCEL DT
     C     *LIKE         DEFINE    CMACDT        XXADDT                         LOC DEL DT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD                         RATE SET CD
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *****      *LIKE     DEFN BICKTP    XXCKTP
      *****      *LIKE     DEFN BICKTP    @@CKTP
     C     *LIKE         DEFINE    *IN60         XXAPIF                         CLAIM API FL
     C     *LIKE         DEFINE    *IN60         XXMANC                         Managed Care
     C*** BEGIN ADD *** 07/21/00 **************************************
     C     *LIKE         DEFINE    CMJRST        SVJRST
     C*** END ADD ***** 07/21/00 **************************************
     C**** BEG   ADD *** 01/04/10 ***********************************
     C     *LIKE         DEFINE    SCCMPS        KKCMPS
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    SCCMPS        SVCMPS
     C**** END ADD ***** 01/04/10 ***********************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
      *
     C     ACDPL3        PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@CONT
      *
      **** BEGIN ADD ** **** 07/10/09 ********************************
     C     ACDPL4        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    XXKEY
     C                   PARM      ZZST          ##ST              2
     C                   PARM      2             ##VIEW            1 0
      **** END ADD **** **** 07/10/09 ********************************
     C*
      **** BEGIN CHANGE **** 01/29/99 ********************************
      ** add *INKC (F3) and *INKL (F12) as values to be passed
     C     DELPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '6'           @@MODE
     C                   PARM                    @@RRN
      **** END CHANGE ****** 01/29/99 ********************************
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C                   CLEAR                   *IN
     C                   CLEAR                   WDW01
     C                   CLEAR                   WDW02
      *
      **** BEGIN ADD **** 01/29/99 **********************************
     C     RESTR         TAG
      **** BEGIN ADD **** 01/29/99 **********************************
     C/COPY SCOPYMT01C
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
     C                   CALL      'W0070R07'    ACDPL3
     C                   END
      *
     C     *INKL         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   END
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   END
     C                   END
     C                   ENDSR                                                  CALLR
      *
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
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
      *
     C                   END
      *
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
     C                   MOVEA     '00'          *IN(19)                         POS CUSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 90 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C*** BEGIN ADD ***  03/31/09  ***********************************
      *
      *  GET THE CANCEL DATE
      *
     C     SCALID        IFNE      *BLANK
      *
     C                   MOVEL(P)  SCALID        XXCTG1
     C     ALKY          CHAIN     PAL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *BLANK        XXALCX            1            LOC DELETION
     C     ALDLDT        IFNE      *ZERO
     C                   CLEAR                   DATEDS
     C                   Z-ADD     ALDLDT        CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCADLD
     C                   MOVEL(P)  XXALM1        SCALX1
     C                   MOVE      SCADLD        SCALX1                         LOAD CX MSG
     C                   MOVE      *ON           *IN47
      *
      *  SUBTRACT ONE DAY FROM LOCATION DELETION DATE TO CALC CORRECTLY
      *
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         X8ADDT            8
      *
     C                   MOVE      X8ADDT        SRCADT
     C                   Z-ADD     1             SRPAST
     C                   MOVE      *BLANKS       SRERR
     C                   MOVE      *BLANKS       SRTERM
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
     C     XXADDT        IFLT      CMACDT
     C                   MOVE      'Y'           XXALCX                         LOC DELETION
     C                   ENDIF
      *
     C                   ENDIF                                                  ALDLDT NE ZERO
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  SCALID IFNE
      *
     C*** END ADD *****  03/31/09  ***********************************
     C                   DO
     C***  BEG   ADD  ***  01/04/10  *********************************
     C     *IN33         IFEQ      *ON
     C     SCCMPS        ANDNE     SVCMPS                                       CMPS CHANGE?
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMEMP#        KKEMP#
     C                   MOVE      CMDIV         KKDIV
     C                   MOVE      SCCMPS        KKCMPS
     C*
     C     CLKY          CHAIN     CLRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CLDESC        SCACNM
     C                   MOVEL     CLADDR        SCACDR
     C                   MOVEL     CLCCTY        SCACTY
     C                   MOVEL     CLCST         SCACST
     C                   MOVEL     CLCZIP        SCACZ1
     C                   MOVE      CLCZIP        SCACZ2
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      SCCMPS        SVCMPS                         reinitialize
     C                   SETON                                        42  90
     C                   LEAVE
     C*
     C                   ENDIF                                                  SCCMPS<>SVCMPS
      *
      *  CAMPUS CODE CANNOT BE BLANK
      *
     C     *IN33         IFEQ      *ON
     C     SCCMPS        IFEQ      *BLANK
     C                   MOVE      *ON           *IN84
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMEMP#        KKEMP#
     C                   MOVE      CMDIV         KKDIV
     C                   MOVE      SCCMPS        KKCMPS
     C     CLKY          CHAIN     CLRCDF                             60
     C     *IN60         IFEQ      *OFF
     C     CLSTAT        IFEQ      'I'
     C                   SETON                                        9089
     C                   ENDIF                                                  CLSTAT = 'I'
     C                   ELSE
     C                   SETON                                        9083      bad campus code
     C                   ENDIF                                                  *IN60 = '0'
     C                   ENDIF                                                  SCCMPS = ' '
     C     SCCMPS        IFNE      SVCMPS                                       CMPS CHANGE?
     C                   SETON                                        90
     C                   MOVE      SCCMPS        SVCMPS                         reinitialize
     C                   ENDIF
     C                   ENDIF                                                  *IN33 = '*ON'
     C**** END ADD ***** 01/04/10 *************************************
     C**** BEG ADD ***** 04/19/10 *************************************
     C*
     C     *IN33         IFEQ      *ON
     C     CZKY          CHAIN     PCZ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CZCNTY        SCACCY
     C*
     C     SCACST        CAT(P)    SCACCY:0      XXKEY
     C*
     C     TBKY          CHAIN     PCN03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNDESC        SCACYD
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN33 = *OFF
     C*
     C**** END ADD ***** 04/19/10 *************************************
      *
      *  IF EMPLOYER HAS BEEN CANCELLED AS OF THE ACCIDENT DATE,
      *    DISPLAY OVERRIDE WINDOW.
      *  IF USER HAS PROPER AUTHORITY, ALLOW OVERRIDE.
      *
     C     XXEMCX        IFEQ      'Y'                                          CANCELLED EMP
      *
      *  IF ADD MODE, DON'T DISPLAY OVERRIDE WINDOW AGAIN;
      *  IT WAS DISPLAYED BY W0070R01.
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'Y'           OVRFL1
     C                   ENDIF
      *
     C     OVRFL1        IFNE      'Y'                                           AS OF ACC DT
     C                   MOVE      'N'           SCEOVR
     C                   MOVE      *OFF          *IN49
      *
      ****       UPLVL     IFGE CXSECC                     CK USER SEC LVL
      ****       UPTYPE    ANDEQCXOCAT                     CK USER SEC TYPE
     C     XUAUTH        IFEQ      '0'                                          CK USER SECURITY
     C                   EXFMT     WDW03
      *
     C     SCEOVR        IFEQ      'Y'
     C                   MOVE      'Y'           OVRFL1
     C                   ENDIF
     C                   ENDIF
      *
     C     SCEOVR        IFNE      'Y'
     C                   SETON                                        9049
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C********** BEGIN ADD ******** 07/10/09 *************************
     C                   MOVE      *OFF          *IN62
     C     SCACST        IFEQ      *BLANK
     C     SCACCY        ANDGT     *BLANK
     C                   MOVE      *ON           *IN80
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C     WCFL09        IFEQ      'Y'
      *
      ***** BEGIN ADD **** 08/11/09 ***********************************
     C                   SELECT
     C     SCJRST        WHENEQ    'GA'
     C     SCACST        ANDEQ     'GA'
     C     SCACCY        ANDGT     *BLANK
     C                   MOVE      *BLANK        ZZKEY             4
     C                   MOVE      *BLANK        XXKEY
      *
     C     SCACST        CAT       SCACCY:0      ZZKEY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              62
     C   62              SETON                                        90
      *
     C     SCJRST        WHENEQ    'GA'
     C     SCACST        ANDNE     'GA'
     C     SCACST        ANDGT     *BLANKS
     C     SCACCY        ANDGT     *BLANK
     C                   SETON                                        62  90
      *
     C     SCJRST        WHENEQ    'GA'
     C     SCACST        ANDNE     'GA'
     C     SCACST        ANDGT     *BLANKS
     C     SCACYD        ANDGT     *BLANK
     C     SCACYD        ANDNE     OUTST
     C                   SETON                                        62  90
     C                   OTHER
      ***** END ADD   **** 08/11/09 ***********************************
     C     SCACST        IFGT      *BLANK
     C     SCACCY        ANDGT     *BLANK
     C                   MOVE      *BLANK        ZZKEY             4
     C                   MOVE      *BLANK        XXKEY
      *
     C     SCACST        CAT       SCACCY:0      ZZKEY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              62
     C   62              SETON                                        90
     C                   ENDIF                                                              KS
      *
     C********** END ADD ********** 07/10/09 *************************
      ***** BEGIN ADD **** 08/11/09 ***********************************
     C                   ENDSL
      ***** END ADD   **** 08/11/09 ***********************************
      *
     C                   ENDIF                                                              KS
      *
      *
      *  IF LOCATION HAS BEEN CANCELLED AS OF THE ACCIDENT DATE,
      *    DISPLAY OVERRIDE WINDOW.
      *  IF USER HAS PROPER AUTHORITY, ALLOW OVERRIDE.
      *
     C     XXALCX        IFEQ      'Y'                                          CANCELLED LOC
      *
      *  IF ADD MODE, DON'T DISPLAY OVERRIDE WINDOW AGAIN;
      *  IT WAS DISPLAYED BY W0070R01.
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'Y'           OVRFL2
     C                   ENDIF
      *
     C     OVRFL2        IFNE      'Y'
     C                   MOVE      'N'           SCEOV2
     C                   MOVE      *OFF          *IN50
      *
     C     XUAUTH        IFEQ      '0'                                          CK USER SECURITY
     C                   EXFMT     WDW04
      *
     C     SCEOV2        IFEQ      'Y'
     C                   MOVE      'Y'           OVRFL2
     C                   ENDIF
     C                   ENDIF
      *
     C     SCEOV2        IFNE      'Y'
     C                   SETON                                        9050
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      *  JURISDICTION STATE CANNOT BE BLANK
      *
     C     SCJRST        IFEQ      *BLANKS
     C                   SETON                                        9072
     C                   LEAVE
     C                   ENDIF
      *
     C                   MOVEL(P)  SCJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9075
     C                   LEAVE
     C                   END
      *
      *  CHECK CLAIM/DARTS CONTROL FILE TO SEE IF LOCATION IS REQUIRED
      *
     C     CXLOCT        IFEQ      'Y'
      *
     C     ALKY1         CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C     SCALID        IFEQ      *BLANK
     C                   SETON                                        9086      CANT BE BLANK
     C                   LEAVE
     C                   END
     C                   END
      *
     C                   END                                                     CXLOCT
     C     SCALID        IFNE      *BLANK
     C                   MOVEL(P)  SCALID        XXCTG1
     C     ALKY          CHAIN     PAL01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9081      INVALID LOC
     C                   LEAVE
     C                   END
      *
      *  LOCATION EFFECTIVE DATE MUST NOT BE GREATER THAN
      *  ACCIDENT DATE
      *
     C     ALEFDT        IFGT      CMACDT
     C     ALEFDT        OREQ      *ZERO
     C                   SETON                                        9076
     C                   LEAVE
     C                   END
      *
      *  LOCATION DELETION DATE MUST NOT BE LESS THAN OR EQUAL
      *  TO ACCIDENT DATE
      *
     C     ALDLDT        IFLE      CMACDT
     C     ALDLDT        ANDNE     *ZERO
     C     OVRFL2        ANDNE     'Y'
     C                   SETON                                        9082
     C                   LEAVE
     C                   END
     C                   END                                                     SCALID
      *
      *  TPA OFFICE # CANNOT BE BLANK
      *
     C     SCTPA1        IFEQ      *BLANK
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
      *
     C                   MOVEL     SCTPA1        XXOFC#
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9074
     C                   LEAVE
     C                   END
      *
      *  EMPLOYER PREMISIS MUST BE Y,N, OR BLANK
      *
     C     SCEPRM        IFNE      'Y'
     C     SCEPRM        ANDNE     'N'
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
     C**** BEG ADD ***** 04/19/10 *************************************
      *
      *  CITY STATE ZIP CANNNOT BE BLANK
      *
     C     *IN33         IFEQ      *ON
     C     CLOFFD        IFEQ      'Y'
     C     SCACDR        IFEQ      *BLANK
     C     SCACTY        OREQ      *BLANK
     C     SCACST        OREQ      *BLANK
     C     SCACZ1        OREQ      0
     C                   SETON                                        9025
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C**** END ADD ***** 04/19/10 *************************************
      *
      *  VALIDATE ACCIDENT STATE IF ENTERED
      *
     C     SCACST        IFNE      *BLANK
     C                   MOVEL(P)  SCACST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9080
     C                   LEAVE
     C                   END
     C                   ENDIF
      *
      *  PLACE OF ACCIDENT ZIP CODE CAN NOT BE BLANK
      *
      ****** BEG ADD ******  07/30/09 ********************************
     C                   SELECT
     C     SCJRST        WHENEQ    'GA'
     C     *IN65         IFEQ      *ON
     C     SCACST        ANDEQ     'GA'
     C     SCACCY        IFEQ      *BLANKS
     C     SCACYD        OREQ      *BLANKS
     C                   SETON                                        9062
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   OTHER
      ****** END ADD ******  07/30/09 ********************************
      *** BEG   ADD    *** 01/04/10 ********************************
     C     *IN33         IFEQ      *ON
     C     SCCMPS        ANDNE     'BUS'
     C     SCCMPS        ANDNE     'ODS'
     C     SCCMPS        ANDNE     *BLANK
     C     SCACZ1        IFEQ      *ZERO
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
      *** END ADD    ***** 01/04/10 ********************************
     C     SCEPRM        IFNE      'Y'
     C     SCACZ1        IFEQ      *ZERO
      ***** BEG ADD ******* 03/19/09 **********************************
     C     SCACLN        ANDEQ     ' '
      ***** END ADD ******* 03/19/09 **********************************
      ***** BEG ADD ******* 09/03/10 **********************************
     C     FNEDI         ANDEQ     *BLANKS
      ***** END ADD ******* 09/03/10 **********************************
     C*** Beg Chg *** 05/07/10 ***************************************
     C**********           SETON                     9071
     C                   SETON                                        9088
     C*** End Chg *** 05/07/10 ***************************************
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *** BEG ADD    ***** 01/04/10 ********************************
     C                   ENDIF
      *** END ADD    ***** 01/04/10 ********************************
      *
      ***** BEG ADD ******* 03/19/09 **********************************
      *  ACCIDENT NARRATIVE
      *
     C*** Beg Del *** 05/07/10 ***************************************
      ***** BEG ADD ******* 01/25/10 **********************************
     C*****      CLTID     IFNE '135'
      ***** BEG ADD ******* 02/10/10 **********************************
     C*****      CLTID     ANDNE'045'
      ***** END ADD ******* 02/10/10 **********************************
      ***** END ADD ******* 01/25/10 **********************************
     C*****      SCEPRM    IFEQ 'N'
     C*****      SCACTY    IFEQ ' '
     C*****      SCACST    OREQ ' '
     C*****      SCACZ1    OREQ 0
     C*****      SCACNM    OREQ ' '
     C*****      SCACDR    OREQ ' '
     C*****      SCACLN    IFEQ ' '
     C*****                SETON                     9088
     C*****                LEAVE
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
      ***** BEG ADD ******* 01/25/10 **********************************
     C*****                ELSE
     C*****      SCEPRM    IFEQ 'N'
     C*****      SCACTY    IFEQ ' '
     C*****      SCACST    OREQ ' '
     C*****      SCACZ1    OREQ 0
     C*****      SCACLN    IFEQ ' '
     C*****                SETON                     9088
     C*****                LEAVE
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
      ***** END ADD ******* 01/25/10 **********************************
     C*** End Del *** 05/07/10 ***************************************
      ***** BEG ADD ******* 04/20/09 **********************************
      ***** BEG DELETE ****** 09/30/09 ********************************
     C***+       SCEPRM    IFEQ 'N'
     C***+       SCACLN    ANDNE' '
     C***+       SCACTY    IFNE ' '
     C***+       SCACST    ORNE ' '
      ***** BEG ADD ******* 04/30/09 **********************************
     C*********  SCACZ1    ORNE 0
      ***** END ADD ******* 04/30/09 **********************************
     C***+       SCACNM    ORNE ' '
     C***+       SCACDR    ORNE ' '
     C***+                 SETON                     9088
     C***+                 LEAVE
     C***+                 ENDIF
     C***+                 ENDIF
      ***** END DELETE ****** 09/30/09 ********************************
      ***** END ADD ******* 04/20/09 **********************************
      ***** END ADD ******* 03/19/09 **********************************
      ****** BEG ADD ******  09/30/09 ********************************
     C*
     C* IF ADDRESS ENTERED, IT MUST BE COMPLETE.
     C*
      ****** BEG ADD ******  01/25/10 ********************************
     C     CLTID         IFNE      '135'
      ***** BEG ADD ******* 02/10/10 **********************************
     C     CLTID         ANDNE     '045'
      ***** END ADD ******* 02/10/10 **********************************
      ****** END ADD ******  01/25/10 ********************************
     C     SCEPRM        IFEQ      'N'
     C*** Beg Add *** 05/07/10 ***************************************
     C     SCACLN        ANDEQ     ' '
     C*** End Add *** 05/07/10 ***************************************
      ***** BEG ADD ******* 09/03/10 **********************************
     C     FNEDI         ANDEQ     *BLANKS
      ***** END ADD ******* 09/03/10 **********************************
     C     SCACDR        IFNE      ' '
     C     SCACTY        ORNE      ' '
     C     SCACST        ORNE      ' '
     C     SCACZ1        ORNE      0
     C     SCACDR        IFEQ      ' '
     C     SCACTY        OREQ      ' '
     C     SCACST        OREQ      ' '
     C     SCACZ1        OREQ      0
     C                   SETON                                        9088
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ****** BEG ADD ******  01/25/10 ********************************
     C                   ENDIF
      ****** END ADD ******  01/25/10 ********************************
      ****** END ADD ******  09/30/09 ********************************
      ****** BEG ADD ******  07/30/09 ********************************
     C                   ENDSL
      ****** END ADD ******  07/30/09 ********************************
      ***CHECK TYPE MUST BE VALID FOR GROUP
      *****
      *****                MOVE 'WC'      XXAPPL    P
      *****                MOVELSCCKTP    XXCKTP    P
      *****                Z-ADDCMACDT    XXEFDT
      *****
      *****
      *****      BIKY      SETLLPBI01
      *****      BIKY1     READEPBI01                    60
      *****
      *****      *IN60     DOWEQ*OFF
      *****      BISTAT    ANDEQ'I'
      *****      BIKY1     READEPBI01                    60
      *****                ENDDO
      *****
      *****      *IN60     IFEQ *ON
      *****      BICKTP    OREQ *BLANK
      *****                SETON                     9083  NO RECORD
      *****                LEAVE                           WCR0513
      *****                ELSE
      *****
      *****      BISTAT    IFNE 'A'
      *****                SETON                     9084  NOT ACTIVE
      *****                LEAVE                           WCR0690
      *****                ENDIF
      *****
      *****      BIEFDT    IFGT CMACDT
      *****      BIEFDT    OREQ *ZERO
      *****                SETON                     9085  NOT EFFECTIVE
      *****                LEAVE                           WC30030
      *****                ENDIF
      *****
      *****                ENDIF                           *IN60=*ON
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
     C                   Z-ADD     1             SCRN#             1 0
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
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
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * KHDO - SUBROUTINE FOR F8
      *===============================================================
      *
      *  F8=EMPLOYER INFORMATION WINDOW
      *
     C     KHDO          BEGSR
      *
      *  CHECK EMPLOYER CANCEL DATE
      *
      ******     XXEMCX    IFEQ 'Y'
      *                    MOVE *ON       *IN48
      ******               ENDIF
      *
     C                   EXFMT     WDW01
     C                   ENDSR
      *
      *===============================================================
      * KIDO - SUBROUTINE FOR F9
      *===============================================================
      *
      *  F9=LOCATION INFORMATION WINDOW
      *
     C     KIDO          BEGSR
      *
      *  CHECK LOCATION CANCEL DATE
      *
      *****      XXALCX    IFEQ 'Y'
      *                    MOVE *ON       *IN47
      *                    ENDIF
      *
     C                   EXFMT     WDW02
     C                   ENDSR
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     KJDO          BEGSR
     C                   MOVE      *OFF          *IN19                          RESET DSPATR(PC)
     C                   MOVE      *OFF          *IN20                          SEARCH NOT FOUND
     C                   MOVEA     '00000'       *IN(41)                        41 - 45
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
      *
     C                   SELECT
      *
      *  LOCATION SEARCH
      *
     C     HFLD          WHENEQ    'SCALID'
     C                   MOVE      *ON           *IN43
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     DBRRN2        ##RRN2
     C                   CALL      'W0076R'      ACDPL
      *
     C     ##RRN         IFNE      *ZEROS
      *
     C     ##RRN         CHAIN     PAL0P                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL(P)  ALELID        SCALID
     C                   MOVEL(P)  ALNAME        SCANAM
     C                   MOVE      *ON           *IN41
     C                   MOVE      *OFF          *IN43
      *
     C                   ENDIF
      *
     C                   ENDIF
      **** BEGIN ADD *** 10/12/98 ***********************************
      *
      * VALIDATE LOCATION AND PLACE OF ACCIDENT ADDRESS
      *
     C                   EXSR      ADRLOD
      **** END ADD ***** 10/12/98 ***********************************
      *
      *
      *  TPA OFFICE#
      *
     C     HFLD          WHENEQ    'SCTPA1'
     C                   MOVE      *BLANK        QAKC
     C                   MOVE      *BLANK        QAKL
     C                   MOVE      '2'           QACALL
     C                   MOVE      *BLANK        QATPA
     C                   CALL      'W0002R'
     C     *INKC         PARM                    QAKC              1
     C     *INKL         PARM                    QAKL              1
     C                   PARM                    QACALL            1
     C                   PARM                    QATPA             5
     C     QATPA         IFEQ      *BLANK
     C                   MOVE      *ON           *IN41                          POS CUSR
     C                   ELSE
     C                   MOVEL(P)  QATPA         SCTPA1
     C                   MOVEL(P)  QATPA         XXOFC#
     C                   MOVE      *ON           *IN42                          POS CUSR
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCTPA2
     C                   END
     C                   END
     C**** BEG   ADD *** 01/04/10 ***********************************
      *
     C     HFLD          WHENEQ    'SCCMPS'
     C                   MOVE      CMCO#         XXCO#
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'W2267R'
     C                   PARM      *BLANK        ##KC
     C                   PARM      *BLANK        ##KL
     C                   PARM      '2'           ##MODE
     C                   PARM      DBRRN2        ##RRN
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     ##RRN         IFNE      *ZEROS
     C     ##KL          ANDNE     *ON
      *
     C     ##RRN         CHAIN     CLPRCF                             60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL(P)  CLCMPS        SCCMPS
     C                   MOVE      CLCMPS        SVCMPS
     C                   MOVEL     CLDESC        SCACNM
     C                   MOVEL     CLADDR        SCACDR
     C                   MOVEL     CLCCTY        SCACTY
     C                   MOVEL     CLCST         SCACST
     C                   MOVEL     CLCZIP        SCACZ1
     C                   MOVE      CLCZIP        SCACZ2
     C**** BEG   ADD *** 04/19/10 ***********************************
     C     *IN33         IFEQ      *ON
     C     CZKY          CHAIN     PCZ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CZCNTY        SCACCY
     C*
     C     SCACST        CAT(P)    SCACCY:0      XXKEY
     C*
     C     TBKY          CHAIN     PCN03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNDESC        SCACYD
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN33 = *OFF
     C**** END   ADD *** 04/19/10 ***********************************
     C                   SETON                                        42
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        44
      *
     C                   ENDIF                                                  ##RRN <> 0
     C**** END ADD ***** 01/04/10 ************************************
      **** BEGIN ADD 07/10/09 ***************************************
     C     HFLD          WHENEQ    'SCACCY'
      *
     C     SCACST        IFNE      *BLANKS
     C                   MOVEL     SCACST        ZZST              2
      *
     C                   CALL      'W2215R'      ACDPL4
      *
     C*** BEGIN ADD *** 07/31/09 **************************************
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C*** END   ADD *** 07/31/09 **************************************
     C     ##RRN         CHAIN     WTCNP                              60
     C  N602             SUBST     CNKEY:3       SCACCY
     C  N60              MOVEL(P)  CNDESC        SCACYD
     C*** BEGIN ADD *** 07/31/09 **************************************
     C                   SETON                                        32
     C                   ENDIF
     C*** END   ADD *** 07/31/09 **************************************
     C                   ELSE
     C                   MOVE      *ON           *IN80
     C                   ENDIF                                                              KS
      **** END ADD 07/10/09   ***************************************
      *
      ***CLAIM BANK CODE/CHECK TYPE CODE
      *****
      *****      HFLD      WHEQ 'SCCKTP'
      *****
      *****                MOVE XXCO#     @@CO#
      *****                MOVE XXFND     @@FND
      *****                MOVE *ON       *IN44
      *****
      *****                CALL 'W2420R'
      *****                PARM '2'       @@CALL  1
      *****      *INKC     PARM           @@KC    1
      *****      *INKL     PARM           @@KL    1
      *****                PARM           @@CO#   3
      *****                PARM           @@FND   3
      *****                PARM *BLANKS   @@ACCT 20
      *****                PARM *BLANKS   @@EFDT  8
      *****                PARM 'WC'      @@APPL  2
      *****                PARM *BLANK    SCCKTP
      *****
      *****      SCCKTP    IFNE *BLANK
      *****                MOVE *ON       *IN45
      *****                MOVE *OFF      *IN44
      *****                ENDIF                           SCBKCD=*BLANKS
      *
     C                   OTHER
     C                   MOVE      *ON           *IN20                          SEARCH NOT FOUND
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
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
     C                   MOVE      ##CO#         QCCO#
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
      *** BEG ADD ****** 04/19/07 ****************************************
      *===============================================================
      * KPDO - SUBROUTINE FOR F15 = CONTACTS
      *===============================================================
      *
     C     KPDO          BEGSR
      * Currently this funtion is for CLTID = '132' only
      * Set call mode Inquiry/Add
     C                   SELECT
     C     *IN09         WHENEQ    *ON
      * Inquiry Mode
     C                   MOVE      '1'           ##CALL
     C     *IN40         WHENEQ    *ON
      * Add Mode
     C                   MOVE      ' '           ##CALL
     C                   ENDSL
      *
     C                   CALL      'W0077R'                             99
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN1            9 0
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
      *
      * Re-chain WMECP with same keys to access the new/changed data
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
     C                   MOVEL(P)  EMCONT        SCCONT
      *
     C                   ENDSR                                                  KPDO
      *** END ADD ****** 04/19/07 ****************************************
      *
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
      **** BEG   ADD **** 01/04/10 ************************************
     C                   MOVE      CMCMPS        SCCMPS
     C                   MOVE      CMCMPS        SVCMPS
      **** END ADD ****** 01/04/10 ************************************
      **** BEG   ADD **** 03/23/10 ************************************
     C                   MOVE      CMIIF         SCIIF
      **** END   ADD **** 03/23/10 ************************************
      *** BEG ADD ***** 07/10/09 ***********************************
      *
      * EDI INSTALLED - GET ACCIDENT COUNTY
      *
     C                   MOVE      *BLANKS       SCACYD
      *
     C     WCFL09        IFEQ      'Y'
     C                   MOVEL     CMACCY        SCACCY
     C                   MOVE      *BLANK        ZZKEY             4
     C                   MOVE      *BLANK        XXKEY
     C****** BEGIN ADD ***** 08/11/09 *********************************
     C                   SELECT
     C     CMJRST        WHENEQ    'GA'
     C     CMAST         IFNE      *BLANK
     C     CMACCY        ANDNE     OUTST
     C     CMAST         CAT       CMACCY:0      ZZKEY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCACYD
     C                   ENDIF
     C                   ENDIF                                                              KS
      *
     C     CMAST         IFNE      *BLANK
     C     CMACCY        ANDEQ     OUTST
     C                   MOVE      *BLANKS       SCACCY
     C                   MOVEL     CMACCY        SCACYD
     C                   ENDIF
      *
     C                   OTHER
     C****** END ADD   ***** 08/11/09 *********************************
      *
     C     CMAST         IFNE      *BLANK
     C     CMAST         CAT       CMACCY:0      ZZKEY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCACYD
     C                   ENDIF
     C                   ENDIF                                                              KS
      *
     C****** BEGIN ADD ***** 08/11/09 *********************************
     C                   ENDSL
     C****** END ADD   ***** 08/11/09 *********************************
     C                   ELSE
     C                   MOVE      *BLANKS       SCACCY
     C                   ENDIF
      *
      *** END ADD ***** 07/10/09 ***********************************
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
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
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
     C                   END
      *
      *  GET THE CLAIM DARTS CONTROL RECORD
      *
     C                   MOVE      *BLANK        XUAUTH            1
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  COMPARE SECURITY LEVEL & TYPE OF THE USER TO CLAIM/DARTS CONTROL
      *
     C                   MOVE      UPTYPE        SRUTYP            1
     C                   MOVE      UPLVL         SRULVL            3
     C                   MOVE      CXOCAT        SRCTYP            1
     C                   MOVE      CXSECC        SRCLVL            3
     C                   MOVE      '1'           SREXEC            1
     C                   MOVE      *ON           SRRES             1
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
      *** BEGIN ADD *** 04/28/98 **************************************
     C                   PARM      CXOCA2        SRCTP2            1
     C                   PARM      CXOCA3        SRCTP3            1
     C                   PARM      CXOCA4        SRCTP4            1
     C                   PARM      CXOCA5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *** END ADD ***** 04/28/98 **************************************
     C                   MOVE(P)   SRRES         XUAUTH
      *
     C                   END
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
      *********            Z-ADDCMEMP#    SCEMP#
      *********            Z-ADDCMDIV     SCDIV
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
      *
      ****************     MOVELEMNAME    SCENAM    P
     C                   MOVEL(P)  EMNAME        SCENM
      ****************     MOVELEMDBA     SCDBA     P
     C                   MOVEL(P)  EMFDNM        SCENM2
      *
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD1        SCEADR
     C     EMPCTY        CAT(P)    ',':0         SCECSZ
     C     SCECSZ        CAT       EMPST:1       SCECSZ
     C                   MOVEL(P)  EMPZIP        XXZIP5            5
     C                   MOVE(P)   EMPZIP        XXZIP4            4
     C     SCECSZ        CAT       XXZIP5:2      SCECSZ
     C     SCECSZ        CAT       '-':0         SCECSZ
     C     SCECSZ        CAT       XXZIP4:0      SCECSZ
     C                   ELSE
     C                   MOVEL(P)  EMMAD1        SCEADR                         MAILING ADDR
     C     EMMCTY        CAT(P)    ',':0         SCECSZ
     C     SCECSZ        CAT       EMMST:1       SCECSZ
     C                   MOVEL(P)  EMMZIP        XXZIP5
     C                   MOVE(P)   EMMZIP        XXZIP4
     C     SCECSZ        CAT       XXZIP5:2      SCECSZ
     C     SCECSZ        CAT       '-':0         SCECSZ
     C     SCECSZ        CAT       XXZIP4:0      SCECSZ
     C                   END
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
      *
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
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVEL(P)  EMCONT        SCCONT
      *
      * SCREEN DISPLAY RULE: YOU CAN'T HAVE ONLY ONE DOT IN PROMPT TEXT
      *
     C     ' '           CHECKR    CXCONH        XXLEN             3 0
     C     XXLEN         IFEQ      5
     C     XXLEN         OREQ      4
     C                   MOVEL(P)  CXCONH        SCCONH
     C                   END
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
     C*** Begin dlt *** 04/11/02 **************************************
     C*  COMPARE THE ACCIDENT DATE TO THE EFFECTIVE DATE
     C*  IF ACC DATE IS >= TO EFF DATE, GET OUT OF LOOP.
     C*
     C********** Y         DOWNE0
     C********** CMACDT    IFGE ED,Y
     C**********           LEAVE
     C**********           ELSE
     C**********           SUB  1         Y
     C**********           END
     C**********           ENDDO
     C*** End dlt ***** 04/11/02 **************************************
      *
      *  CONVERT EFFECTIVE DATE FROM CYMD TO MDCY
      *
     C     Y             IFGT      *ZERO
     C     ED(Y)         IFNE      *ZERO
     C                   CLEAR                   DATEDS
     C                   Z-ADD     ED(Y)         CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCEEFD
     C                   END
     C                   END
      *
      *  GET THE CORRESONDING CANCEL DATE
      *  CONVERT CANCEL DATE FROM CYMD TO MDCY
      *
     C                   MOVE      *BLANK        XXEMCX            1            EMP CANCEL
     C     Y             IFGT      *ZERO
     C     CD(Y)         IFNE      *ZERO
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CD(Y)         CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCECXD                         FOR WDW01
     C                   MOVEL(P)  XXEMM1        SCEMX1
     C                   MOVE      SCECXD        SCEMX1                         FOR SCR01
     C                   MOVE      *ON           *IN48                          EMP CANCEL MSG
      *
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         X8CADT            8
      *
     C                   MOVE      X8CADT        SRCADT            8 0
     C                   Z-ADD     1             SRPAST            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    SRCADT
     C                   PARM                    SRPAST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C                   MOVE(P)   SRCADT        X8CADT                         MDCY -1 DAY
      *
     C                   MOVE(P)   X8CADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDCY --> CYMD
     C                   MOVEL(P)  SRDTO         XXCADT
      *
      *  CHECK CANCEL DATE AGAINST ACCIDENT DATE
      *
     C     XXCADT        IFLT      CMACDT
     C                   MOVE      'Y'           XXEMCX                         EMP CANCEL
     C                   END
      *
     C                   END
     C                   END                                                    Y > ZERO
      *
     C                   END                                                    WMEMP
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
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
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
      ***  BEGIN DEL  ***  04/23/98  *********************************
      *****BEGIN ADD  ***  03/10/98  *********************************
      **********           ELSE
      **********           MOVEL*BLANKS   SCCNAM
      *****END** ADD  ***  03/10/98  *********************************
      ***  END   DEL  ***  04/23/98  *********************************
     C                   END
      ***  BEGIN ADD  ***  04/23/98  *********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCNAM
      ***  END   ADD  ***  04/23/98  *********************************
     C                   END
     C                   END
      *  ACCIDENT DATE
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CMACDT        CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCACDT
      *
     C     CMACDT        IFEQ      *ZERO
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
      *  AGENCY CLAIM# (OR, STATE CLAIM#)
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  LOAD LOCATION
      *
     C                   MOVE      *BLANK        XXLOC             1
     C                   MOVEL(P)  CMCTG1        SCALID
     C                   MOVEL     CMCTG1        XXCTG1
     C                   MOVE      *OFF          *IN45
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN45
     C                   MOVEL     'Y'           XXLOC
     C                   MOVEL     ALNAME        SCANAM
      *************        MOVELALDBA     SCAAD1
     C                   MOVEL     EMFDNM        SCANM2
      *
     C     ALPAD1        IFNE      *BLANK
     C                   MOVEL     ALPAD1        SCAAD1
      *
      * PHYSICAL CITY, STATE, ZIP
      *
     C     ALPCTY        CAT(P)    ',':0         SCACSZ
     C     SCACSZ        CAT       ALPST:1       SCACSZ
     C                   MOVEL(P)  ALPZIP        XXZIP5
     C                   MOVE(P)   ALPZIP        XXZIP4
     C     SCACSZ        CAT       XXZIP5:2      SCACSZ
     C     SCACSZ        CAT       '-':0         SCACSZ
     C     SCACSZ        CAT       XXZIP4:0      SCACSZ
      *
     C                   ELSE
     C                   MOVEL     ALMAD1        SCAAD1
      *
      * MAILING CITY, STATE, ZIP
      *
     C     ALMCTY        CAT(P)    ',':0         SCACSZ
     C     SCACSZ        CAT       ALMST:1       SCACSZ
     C                   MOVEL(P)  ALMZIP        XXZIP5
     C                   MOVE(P)   ALMZIP        XXZIP4
     C     SCACSZ        CAT       XXZIP5:2      SCACSZ
     C     SCACSZ        CAT       '-':0         SCACSZ
     C     SCACSZ        CAT       XXZIP4:0      SCACSZ
     C                   END
      *
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
      *
     C                   MOVEL(P)  ALCNTY        SCACNY
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCACDE
     C                   END
      *
      *  GET THE EFFECTIVE DATE
      *
     C     ALEFDT        IFNE      *ZERO
     C                   CLEAR                   DATEDS
     C                   Z-ADD     ALEFDT        CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCAEFD
     C                   END
      *
      *  GET THE CANCEL DATE
      *
     C                   MOVE      *BLANK        XXALCX            1            LOC DELETION
     C     ALDLDT        IFNE      *ZERO
     C                   CLEAR                   DATEDS
     C                   Z-ADD     ALDLDT        CYMD
     C                   EXSR      SRCDAT
     C                   MOVEL(P)  XXDT10        SCADLD
     C                   MOVEL(P)  XXALM1        SCALX1
     C                   MOVE      SCADLD        SCALX1                         LOAD CX MSG
     C                   MOVE      *ON           *IN47
      *
      *  SUBTRACT ONE DAY FROM LOCATION DELETION DATE TO CALC CORRECTLY
      *
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         X8ADDT            8
      *
     C                   MOVE      X8ADDT        SRCADT
     C                   Z-ADD     1             SRPAST
     C                   MOVE      *BLANKS       SRERR
     C                   MOVE      *BLANKS       SRTERM
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
     C     XXADDT        IFLT      CMACDT
     C                   MOVE      'Y'           XXALCX                         LOC DELETION
     C                   END
      *
     C                   END                                                    ALDLDT NE ZERO
      *
     C                   END                                                    WMALP
      *
      *  GET THE JURISDICTION STATE FROM THE RATE SET CODE IF IN ADD MODE
      *
     C                   MOVE      *BLANK        SCJRST
     C                   MOVEL(P)  CMJRST        SCJRST                         JURISDICTION ST
     C     *IN40         IFEQ      *ON
     C*** BEGIN ADD *** 04/17/06 **************************************
     C     SCJRST        ANDEQ     *BLANKS
     C*** END   ADD *** 04/17/06 **************************************
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RSJRST        SCJRST
     C                   END
     C                   END
     C                   END
     C*** BEGIN ADD *** 07/21/00 **************************************
     C                   MOVEL     SCJRST        SVJRST
     C*** END ADD ***** 07/21/00 **************************************
      *
      *  GET THE TPA OFFICE# FROM CLAIM MASTER OR EMPLOYER MASTER FILE
      *
     C                   MOVE      *BLANK        SCTPA1
     C                   MOVE      *BLANK        XXOFC#
     C     CMOFF#        IFNE      *BLANK
     C                   MOVEL(P)  CMOFF#        SCTPA1                         TPA OFFICE#
     C                   MOVEL(P)  CMOFF#        XXOFC#
     C                   ELSE
     C                   MOVEL(P)  EMOFF#        SCTPA1                         TPA OFFICE#
     C                   MOVEL(P)  EMOFF#        XXOFC#
     C                   END
      *
      *  GET THE OFFICE DESCRIPTION FROM THE OFFICE MASTER FILE
      *
     C                   MOVE      *BLANK        SCTPA2
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCTPA2                         OFFICE DESC
     C                   END
      *
     C                   MOVE      *BLANK        SCEPRM
     C                   MOVEL(P)  CMEADR        SCEPRM                         EMP PREMISIS
      *
      * PLACE OF ACCIDENT
      *
     C                   MOVE      *BLANK        SCACNM
     C                   MOVE      *BLANK        SCACDR
     C                   MOVE      *BLANK        SCACTY
      ***** BEG ADD ******* 03/19/09 **********************************
     C                   MOVE      *BLANK        SCACLN
      ***** END ADD ******* 03/19/09 **********************************
     C                   MOVE      *BLANK        SCACST
     C                   MOVE      *BLANK        SCACZ1
     C                   MOVE      *BLANK        SCACZ2
     C                   MOVEL(P)  CMACNM        SCACNM                         NAME
     C                   MOVEL(P)  CMACDR        SCACDR                         ADDERSS
     C                   MOVEL(P)  CMACTY        SCACTY                         CITY
      ***** BEG ADD ******* 03/19/09 **********************************
     C                   MOVEL(P)  CMACLN        SCACLN                         NARRATIVE
      ***** END ADD ******* 03/19/09 **********************************
     C                   MOVEL(P)  CMAST         SCACST                         STATE
     C                   MOVE(P)   CMACZ1        SCACZ1                         ZIP1
     C                   MOVE(P)   CMACZ2        SCACZ2                         ZIP2
      *
      ***GET THE CLAIM BANK CODE/CHECK TYPE
      *****
      *****                MOVE *BLANK    SCCKTP
      *****      CMCKTP    IFNE *BLANK
      *****                MOVELCMCKTP    SCCKTP
      *****                ELSE
      *****                MOVELEMCKTP    SCCKTP
      *****                ENDIF
      *****
      ***                  Z-ADDCMACDT    XXEFDT
      **         BIKY1     SETLLPBI01
      **
      **         *IN60     DOUEQ*ON
      **         BIKY1     READEPBI01                    60
      **         *IN60     IFEQ *OFF
      **
      **         BISTAT    ANDEQ'A'
      ****       BIEFDT    ANDLECMACDT
      **                   MOVELBIACCT    SCBACT
      **                   LEAVE
      **                   ENDIF
      **                   ENDDO
      *
      *********            ENDIF                           CMBACT NE BLANK
      *
     C                   MOVE      *BLANK        SCECLM
     C                   MOVE      *BLANK        SCESPV
     C                   MOVEL(P)  CMECS#        SCECLM                         EMP CLAIM#
     C                   MOVEL(P)  CMESPV        SCESPV                         SUPERVISOR
      *
     C                   MOVE      *ON           *IN19                          POS CUSR
      **** BEGIN ADD *** 10/12/98 ***********************************
      *
      * VALIDATE LOCATION AND PLACE OF ACCIDENT ADDRESS
      *
     C                   EXSR      ADRLOD
      **** END ADD ***** 10/12/98 ***********************************
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
      *    Set flag to update MC if SCL# has changed.
      *
     C                   MOVE      *OFF          XXAPIF
     C     XXMANC        IFEQ      *ON
     C     SCSCL#        IFNE      CMACL#
     C                   MOVE      *ON           XXAPIF
     C                   ENDIF
     C                   ENDIF
      *
     C*** BEGIN ADD *** 07/21/00 **************************************
      *  If this pgm was called using option 8 and EDI is active, call
      *    the EDI Log API.
      *
     C     SCJRST        IFNE      SVJRST
     C     WCFL09        ANDEQ     'Y'
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
     C                   PARM      'D'           QQACTF            1
     C                   PARM      *BLANKS       QQMGID            7
     C                   PARM                    QQRTCD            1
      *
     C                   END
     C*** END ADD ***** 07/21/00 **************************************
      *** BEG ADD ***** 07/10/09 ***********************************
      *
      * EDI INSTALLED
      *
     C     WCFL09        IFEQ      'Y'
      *
     C     SCJRST        IFEQ      'GA'
     C     SCACST        ANDNE     'GA'
     C     SCACST        ANDGT     *BLANKS
     C                   MOVEL     OUTST         CMACCY
      *
     C                   ELSE
     C                   MOVEL     SCACCY        CMACCY
     C                   ENDIF
     C                   ENDIF
      *
      *** END ADD ***** 07/10/09 ***********************************
      *
     C                   MOVEL     SCSCL#        CMACL#
     C                   MOVEL(P)  SCJRST        CMJRST                         JURISDICTON ST
     C                   MOVEL     SCTPA1        CMOFF#                         OFFICE #
      ***** BEG ADD ******* 03/23/10 **********************************
     C                   MOVEL     SCIIF         CMIIF
      ***** END ADD ******* 03/23/10 **********************************
     C                   MOVEL     SCALID        CMCTG1                         LOCATION
     C                   MOVEL     SCEPRM        CMEADR
      *
     C                   MOVEL     SCACNM        CMACNM
     C                   MOVEL     SCACDR        CMACDR
     C                   MOVEL     SCACTY        CMACTY
      ***** BEG ADD ******* 03/19/09 **********************************
     C                   MOVEL     SCACLN        CMACLN
      ***** END ADD ******* 03/19/09 **********************************
     C                   MOVEL     SCACST        CMAST
     C                   MOVE      SCACZ1        CMACZ1
     C                   MOVE      SCACZ2        CMACZ2
      *
     C     SCEPRM        IFEQ      'Y'
     C     SCACNM        ANDEQ     *BLANK
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL     EMNAME        CMACNM
     C                   MOVEL     EMPAD1        CMACDR
     C                   MOVEL     EMPCTY        CMACTY
     C                   MOVEL     EMPST         CMAST
     C                   MOVEL     EMPZIP        CMACZ1
     C                   MOVE      EMPZIP        CMACZ2
     C                   ELSE
     C                   MOVEL     EMNAME        CMACNM
     C                   MOVEL     EMMAD1        CMACDR
     C                   MOVEL     EMMCTY        CMACTY
     C                   MOVEL     EMMST         CMAST
     C                   MOVEL     EMMZIP        CMACZ1
     C                   MOVE      EMMZIP        CMACZ2
     C                   END
     C                   END
      *
      *****                MOVELSCCKTP    CMCKTP           CHECK TYPE
     C                   MOVEL     SCECLM        CMECS#                         EMP CLAIM#
     C                   MOVEL     SCESPV        CMESPV                         SUPERVISOR
      *
     C**** BEG   ADD *** 01/04/10 ***********************************
     C                   MOVE      SCCMPS        CMCMPS
     C**** END   ADD *** 01/04/10 ***********************************
     C                   ENDSR                                                  MVSCDB
      *
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
     C                   MOVE      *OFF          *IN20                          SEARCH NOT FOUND
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C                   MOVE      *OFF          *IN62
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKH         CASEQ     *ON           KHDO                     90
     C     *INKI         CASEQ     *ON           KIDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
      *** BEG ADD ****** 04/19/07 ****************************************
     C   51*INKP         CASEQ     *ON           KPDO                     90
      *** END ADD ****** 04/19/07 ****************************************
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
      *   **  IF IN ADD MODE **
      *  DELETE THE CLAIM RECORD IF THE USER PRESSED F3 OR F12
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C                   MOVE      'A'           @@MODE
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
      *** BEGIN DELETE *** 01/29/99 ***********************************
     C********   @@RRN     CHAINDBRCDF              N60
     C********   *IN60     IFEQ *OFF
      ****************     DELETDBRCDF
     C****************     UNLCKWMCMP
      *** END DELETE *** 01/29/99 *************************************
     C*** BEGIN ADD *** 06/18/01 **************************************
     C     XXMANC        IFEQ      *ON
     C*
     C*   FIRST YOU NEED TO GET THE RRN OF THE CASE YOU ARE CHANGING
     C*
     C                   MOVE      ##CO#         XYCO#             3
     C                   MOVE      CMFND         XYFND             3
     C                   MOVE      CMFYR         XYFYR             3
     C                   MOVE      CMCASE        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN1            9 0
     C                   PARM      *BLANK        XYRTCD            1
     C*
     C     XYRTCD        IFNE      'F'
     C*
     C*
     C*  NOW YOU HAVE TO CLEAR OUT AND CREATE TEMP FILES
     C*
     C                   MOVEL(P)  'FAT3PQ'      #FNAME
     C                   MOVEL(P)  'FAT3PQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C                   MOVEL(P)  'FAT3LQ'      #FNAME
     C                   MOVEL(P)  'FAT3LQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C*   NOW YOU CALL F7090R TO DO THE DELETE
     C*
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C     *INKL         PARM      *BLANKS       ##KL              1
     C                   PARM      XYRRN1        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      'D'           ##TD              1
     C                   PARM      'X'           ##FLAG            1
     C                   PARM      'Y'           ##WCMS            1
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  WCMC
     C*
     C     *INKL         IFEQ      *OFF
     C*** END   ADD *** 06/18/01 **************************************
     C                   MOVE      *ZERO         *INKC
     C                   MOVE      *ZERO         *INKL
     C                   CALL      'W0070R04'    DELPL                  99
     C*** BEGIN ADD *** 06/18/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/18/01 **************************************
      **** BEGIN ADD **** 01/29/99 **********************************
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   MOVE      *ZERO         *INKC
     C                   MOVE      *ZERO         *INKL
     C                   MOVE      '1'           *IN40
     C                   MOVE      'A'           @@MODE
     C                   GOTO      RESTR
     C                   ENDIF
      **** END ADD   **** 01/29/99 **********************************
      *
     C                   END
     C                   END
      *
      *** BEG ADD *** 02/15/05 **********************************
     C     SCJRST        IFNE      SVJRST
     C     *IN40         ANDEQ     *OFF
     C****BEG DEL *** 02/01/10 **********************************
     C****                 CALL 'WA700R'
     C****                 PARM CMCO#     #1CO#   30
     C****                 PARM CMFND     #1FND   30
     C****                 PARM CMFYR     #1FYR   30
     C****                 PARM CMCASE    #1CASE  70
     C****                 PARM 'CLMCHG02'#1KEY  10
     C****                 PARM *BLANKS   ##COMM256
     C****END DEL *** 02/01/10 **********************************
      *
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         #1CO#             3 0
     C                   PARM      CMFND         #1FND             3 0
     C                   PARM      CMFYR         #1FYR             3 0
     C                   PARM      CMCASE        #1CASE            7 0
     C                   PARM      'CLMCHG03'    #1KEY            10
     C                   PARM      *BLANKS       ##COMM          256
     C                   END
      *** BEG ADD *** 02/15/05 **********************************
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
      ** ******            CLOSEBATRL
     C                   RETURN
     C                   ENDSR
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
     C                   Z-ADD     1             #OFSCR            1 0          # OF SCREENS
     C                   MOVEL     Q1PGM         SCPGM
      *
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL     MODEA         SCMODE
     C     Q1PRMS        IFGE      5
     C                   MOVE      'A'           @@CONT
     C                   END
     C                   END
      *
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
      *
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
      * GET WORKERS COMP CONTROL FILE
      *
     C                   MOVEA     '00'          *IN(66)
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
      *
      * COMMERCIAL APPLICATION INSTALLED
      *
     C     WCFL07        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   END
      ****** BEGIN ADD ****** 07/10/09 **************************
      *
      * EDI INSTALLED
      *
     C     WCFL09        IFEQ      'Y'
     C                   MOVE      *ON           *IN65
     C                   ELSE
     C                   MOVE      *OFF          *IN65
     C                   END
      ****** END ADD ******** 07/10/09 **************************
      *
     C                   END
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
     C                   END
      *
      * MVDBSCURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     SRDTI
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      ***** END CHG ***** 03/06/06 ************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
     C                   MOVE      *BLANK        OVRFL1            1
     C                   MOVE      *BLANK        OVRFL2            1
      *
      *    Determine if Managed Care is installed
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EXKY          CHAIN     PEX01                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      *
      *** BEG ADD *** 04/19/07 **********************************
      * Retrieve Client ID
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      * Set indicator 51 for client 132 specific edits
     C     CLTID         IFEQ      '132'
     C                   MOVE      *ON           *IN51
     C                   ELSE
     C                   MOVE      *OFF          *IN51
     C                   ENDIF
      *** END ADD *** 04/19/07 **********************************
      *** BEG ADD *** 01/04/10 **********************************
     C     CLTID         IFEQ      '025'
     C                   MOVE      *ON           *IN33
     C                   ELSE
     C                   MOVE      *OFF          *IN33
     C                   ENDIF
      *** END ADD *** 01/04/10 **********************************
      *
      ***** END ADD ******* 08/10/98 **********************************
      ***** BEG ADD ******* 05/17/10 **********************************
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
     C                   MOVE      *ON           *IN35
     C                   ELSE
     C                   MOVE      *OFF          *IN35
     C                   ENDIF
      ***** END ADD ******* 05/17/10 **********************************
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name and Email Address
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
      ***** END ADD ******* 01/31/02 **********************************
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
     C     *IN99         IFEQ      *OFF
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
      **** BEGIN ADD *** 10/12/98 ***********************************
      *
      * VALIDATE LOCATION AND PLACE OF ACCIDENT ADDRESS
      *
     C                   EXSR      ADRLOD
      **** END ADD ***** 10/12/98 ***********************************
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
     C                   UPDATE    DBRCDF
      *
      *    Call API to update Managed Care Claim Master - FMCMP
      *
     C                   CLEAR                   PARMCL
      *
     C     XXAPIF        CASEQ     *ON           MCAPIC
     C                   ENDCS                                                              DATE
      *
     C*** BEGIN ADD *** 07/21/00 **************************************
      *  If this pgm was called using option 8 and EDI is active, call
      *    the EDI Log API.
      *
     C     SCJRST        IFNE      SVJRST
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
     C*** END ADD ***** 07/21/00 **************************************
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
      * ===============================================================
      *   CONVERT 7,0 DATE TO 8,0 ;   0961213 --> 19961213
      *   THEN, CYMD TO MDCY      ;  19961213 --> 12/13/1996
      * ===============================================================
     C     SRCDAT        BEGSR
      *
     C     YMD           IFNE      *ZERO
     C     CC1           IFEQ      0
     C                   MOVE      '19'          CC1
     C                   ELSE
     C                   MOVE      '20'          CC1
     C                   END
      *
     C                   MOVE(P)   CY1           CY2
     C     MM            CAT(P)    '/':0         XXDT10           10
     C     XXDT10        CAT       DD:0          XXDT10
     C     XXDT10        CAT       '/':0         XXDT10
     C     XXDT10        CAT       CY1:0         XXDT10
     C                   END
     C                   ENDSR
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
     C                   MOVEL(P)  CMSHFT        QQWKPT                         # dependents
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
     C                   PARM      SCJRST        QQJRST            2            JUR STATE
     C*** END   ADD *** 08/25/00 **************************************
     C*** BEGIN ADD *** 06/04/07 **************************************
     C                   PARM      SCJRST        QQPOST            2            POLICY STE
     C*** END   ADD *** 06/04/07 **************************************
      *
     C                   ENDSR
      *===============================================================
     C**** BEGIN ADD *** 10/12/98 **********************************************
      *===============================================================
      *=                                                             =
      *=  AUTOLOAD PLACE OF ACCIDENT ADDRESS                         =
      *=                                                             =
      *===============================================================
     C**** AUTO LOAD THE ACCIDENT ADDRESS LOCATION IF CONDITIONS APPLY *********
     C     ADRLOD        BEGSR
     C*
     C     SCALID        IFNE      *BLANKS
     C     SCEPRM        ANDEQ     'Y'                                                      ON (N81)
     C     SCACNM        ANDEQ     *BLANKS
     C     SCACDR        ANDEQ     *BLANKS
     C     SCACTY        ANDEQ     *BLANKS
     C     SCACST        ANDEQ     *BLANKS
     C*
     C                   MOVEL(P)  SCALID        XXCTG1
     C     ALKY          CHAIN     PAL01                              99
     C     *IN99         IFEQ      *OFF
     C     ALNAME        ANDNE     *BLANKS
     C     ALPAD1        ANDNE     *BLANKS
     C     ALPCTY        ANDNE     *BLANKS
     C     ALPST         ANDNE     *BLANKS
     C*
     C                   MOVEL     ALNAME        SCACNM
     C     ALPAD2        IFNE      *BLANKS                                      JOIN ADRESS
     C     ALPAD1        CAT       ALPAD2:2      SCACDR                         LINE 2 W/1
     C                   ELSE
     C                   MOVEL     ALPAD1        SCACDR
     C                   ENDIF
     C                   MOVEL     ALPCTY        SCACTY
     C                   MOVEL     ALPST         SCACST
     C                   MOVEL(P)  ALPZIP        SCACZ1
     C                   MOVE(P)   ALPZIP        SCACZ2
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                      ADRLOD
     C**** END ADD ***** 10/12/98 **********************************************
