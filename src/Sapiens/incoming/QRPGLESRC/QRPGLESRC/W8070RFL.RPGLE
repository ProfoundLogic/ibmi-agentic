     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H/TITLE W8070RFL - NOI (DWC-1) PRINT PROGRAM
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM PRINTS NOI (DWC-1) FORMS.  *
     H*                                                               *
     H* CALLED FROM:  W8070CFL                                        *
     H* CALLS TO:  NONE                                               *
     H* PROJECT: 48WLU          C:02           09/08/98               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 04/30/19 C:062 Project: 152WAB                                *
     H*                Apply Optacomp Mods.                           *
     H*                                                               *
     H* 03/21/14 C:300 Project: 147WCK                                *
     H*                FL DWC-1 Changes                               *
     H*                                                               *
     H* 06/07/13 C:282 Project: 147WAO                                *
     H*                Changing the printer file fields,              *
     H*                Penaly Amount Paid in 1st Payment              *
     H*                form (9,0) to (9,2)                            *
     H*                Interest Amount Paid in 1st Payment            *
     H*                form (9,0) to (9,2)                            *
     H*                                                               *
     H* 02/20/13 C:282 Project: 147WAO                                *
     H*                Florida WC FROI                                *
     H*                Replacing QSYSPRT  with W8070OFL.              *
     H*                Replacing QSYSPRT2 with W8070OFL1              *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 08/18/09 C:220 Support:69484                                  *
     H* 10/05/09 C:191 FIX PHONE NUMBER                               *
     H*                Change offce address and phone number.         *
     H*                                                               *
     H* 03/05/07 C:185 Support:57166                                  *
     H*                DWC-1 form corrected.                          *
     H*                                                               *
     H* 06/24/05 C:15 Project: 44WDL                                  *
     H*               Client mod to use FNANAM for the insured name.  *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT: 126WDK                                *
     H*                PROJECT: 126WBC                                *
     H*                ADDED PARMS FOR GRP/FYR/CASE TO BE ABLE TO     *
     H*                PRINT THIS FORM FROM WW CLAIMS.                *
     H*                                                               *
     H* 04/01/05 C:15  Project: WC1276                                *
     H*                Re-aligned output for new DWC-1 form.          *
     H*                                                               *
     H* 03/08/05 C:15  Support: WC126/45864                           *
     H*                Chgd pgm to print the same policy # that is    *
     H*                reported in WCSTAT.                            *
     H*                                                               *
     H* 02/17/03 C:15  Support: WC048/35917                           *
     H*                Chgd QSYSPRT2 to use EMFDNM and EMNAME the same*
     H*                as in QSYSPRT.  Removed usage of EMDBA.        *
     H*                                                               *
     H* 02/08/01 C:15  Project: WC468                                 *
     H*                Added code to prevent an error when opening    *
     H*                QSYSPRT2 when *IN73 is *ON.  If the claim      *
     H*                record was not found, the CLOSE operation      *
     H*                would not be processed causing an error the    *
     H*                next time the OPEN operation was processed.    *
     H*                                                               *
     H* 01/18/01 C:15  Support: WC094/25827                           *
     H*                Removed reference to file WMC1P.  This file is *
     H*                specific to CLT048.                            *
     H*                                                               *
     H* 05/18/00 C:53  PROJECT: WC962
     H*                Added jurisdiction state to WMNIP chain.
     H*                                                               *
     H*  04/26/00 C:61  Project: DR097                                *
     H*                 Chg ALL PDS pgms                              *
     H*                 1. Replace write to SDCMP with API DA700R     *
     H*                                                               *
     H* 08/05/99 C:15 Support: WC048/19084                            *
     H*              -Changed the code which loads the treating       *
     H*               physician name and address.                     *
     H*              -Changed output conditions for the following     *
     H*               flags to reflect a blank entry: sex, paid for   *
     H*               date of injury, and phy auth by employer.       *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 04/02/99 C:90 PROJECT: WC889                                  *
     H*               IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1.*
     H*                                                               *
     H* 02/11/99 C:15 Support: WC048/16561                            *
     H*               Changed pgm to prevent data decimal errors on   *
     H*               the SS# which should allow alpha characters.    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     FWMLGL1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     FWXNIL     IF   E           K DISK
     FWXNIL1    IF   E           K DISK
     F                                     RENAME(PNI01:PNI02)
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWMNIL     IF   E           K DISK
     F                                     RENAME(PNI01:PNI04)
     FWMCML1    IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
      *** Beg Chg ***** 02/20/13 ******************************************
     F*****QSYSPRT   O    F  127        PRINTER OFLIND(*INOA)
     F*****                                     USROPN
     Fw8070ofl  o    e             printer usropn
      *** End Chg ***** 02/20/13 ******************************************
     FQSYSPRT1  O    F   85        PRINTER OFLIND(*INOF)
     F                                     USROPN
      *** Beg Chg ***** 02/20/13 ******************************************
     F*****QSYSPRT2  O    F  127        PRINTER OFLIND(*INOG)
     F*****                                USROPN
     Fw8070ofl1 o    e             printer usropn Rename(NOI:NOI2)
      *** End Chg ***** 02/20/13 ******************************************

     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D CPY             S             21    DIM(3) CTDATA PERRCD(1)              COPY MSG
     D OVR             S              1    DIM(33)
      *** Beg Add ***** 02/20/13 ******************************************
      *** Beg Add ***** 03/21/14 ******************************************
     d zip1            s              5
     d zip2            s              5
     d xxEmPAd1        s             40
     d xxEmPAd2        s             40
      *** End Add ***** 03/21/14 ******************************************
     d xxENAM1         s             30
     d xxECSZ1         s             40
      *** End Add ***** 02/20/13 ******************************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  XXACTM                 1      6  0
     D  XXTIME                 1      4  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  XXPOL1                 1     17
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMFYR                 13     15  0
     D                 DS
     D  XXDTOB                 1      8  0
     D  XXDOBM                 1      2
     D  XXDOBD                 3      4
     D  XXDOBY                 5      8
      *** Beg Add ***** 02/20/13 ******************************************
      * Date Employed
     d                 ds
     d  XXDOH                  1      8  0
     d  XXDOHM                 1      2  0
     d  XXDOHD                 3      4  0
     d  XXDOHY                 5      8  0
      * Last Date of Employee Worked
     d                 ds
     d  XXDTLW                 1      8  0
     d  XXDTLWM                1      2  0
     d  XXDTLWD                3      4  0
     d  XXDTLWY                5      8  0
      * Returned to Work, If Yes Give Date
     d                 ds
     d  XXIRTN                 1      8  0
     d  XXIRTNM                1      2  0
     d  XXIRTND                3      4  0
     d  XXIRTNY                5      8  0
      * Date of Death
     d                 ds
     d  XXDOD                  1      8  0
     d  XXDODM                 1      2  0
     d  XXDODD                 3      4  0
     d  XXDODY                 5      8  0
      * Last Date Wages
     d                 ds
     d  XXFSDT                 1      8  0
     d  XXFSDTM                1      2  0
     d  XXFSDTD                3      4  0
     d  XXFSDTY                5      8  0
      * Day of Disability
     d                 ds
     d  XXDT1A                 1      8  0
     d  XXDT1AM                1      2  0
     d  XXDT1AD                3      4  0
     d  XXDT1AY                5      8  0
      *
     d                 ds
     d  XXDT2A                 1      8  0
     d  XXDT2AM                1      2  0
     d  XXDT2AD                3      4  0
     d  XXDT2AY                5      8  0
      *
     d                 ds
     d  XXDT1                  1      8  0
     d  XXDT1M                 1      2  0
     d  XXDT1D                 3      4  0
     d  XXDT1Y                 5      8  0
      * Full salary end date
     d                 ds
     d  XXDT2                  1      8  0
     d  XXDT2M                 1      2  0
     d  XXDT2D                 3      4  0
     d  XXDT2Y                 5      8  0
      * Date First Payment Mailed
     d                 ds
     d  XXDT4                  1      8  0
     d  XXDT4M                 1      2  0
     d  XXDT4D                 3      4  0
     d  XXDT4Y                 5      8  0
      *** End Add ***** 02/20/13 ******************************************
      *** Beg Del ***** 02/20/13 ******************************************
     D*
     D*XXOVR           C                   CONST('OVRPRTF QSYSPRT  OUT-
     D*                                    Q(          )')
     D*XXOVR1          C                   CONST('OVRPRTF QSYSPRT1 OUT-
     D*                                    Q(          )')
     D*XXOVR2          C                   CONST('OVRPRTF QSYSPRT2 OUT-
     D*                                    Q(          )')
      *** End Del ***** 02/20/13 ******************************************
      *** Beg Add ***** 02/20/13 ******************************************
     d XXHyp           C                   CONST('-')
     d XXHyp1          C                   CONST('-')
     d XXHyp2          C                   CONST('-')
     d XXHyp3          C                   CONST('-')
     d XXHyp4          C                   CONST('-')
     d XXConst         C                   CONST('X')
     d XXConst1        C                   CONST('X')
     d XXConst2        C                   CONST('X')
     d XXConst3        C                   CONST('X')
     d XXConst4        C                   CONST('X')
     d XXConst5        C                   CONST('X')
     d XXConst6        C                   CONST('X')
     d XXConst7        C                   CONST('X')
     d XXConst8        C                   CONST('X')
     d XXConst9        C                   CONST('X')
     d XXConst10       C                   CONST('X')
     d XXConst11       C                   CONST('X')
     d XXConst12       C                   CONST('X')
     d XXConst13       C                   CONST('X')
     d XXConst14       C                   CONST('X')
     d XXConst15       C                   CONST('X')
     d XXConst16       C                   CONST('X')
     d XXConst17       C                   CONST('X')
     d XXConst18       C                   CONST('X')
     d XXConst19       C                   CONST('X')
     d XXConst20       C                   CONST('X')
     d XXConst21       C                   CONST('X')
     d XXConst22       C                   CONST('X')
     d XXConst23       C                   CONST('X')
     d XXConst24       C                   CONST('X')
     d XXConst25       C                   CONST('X')
     d XXConst26       C                   CONST('X')
     d XXConst27       C                   CONST('X')
     d XXConst28       C                   CONST('X')
      *** End Add ***** 02/20/13 ******************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE NOI WORKFILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     NIKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     NIKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X2ST
     C*
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHCPY
     C                   KFLD                    XXJRST
     C                   KFLD                    XXDATE
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    LGIDT
     C                   KFLD                    LGITM
     C*
     C*  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQJRST
     C                   KFLD                    XXTSET
     C*
     C*  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PROVDER MASTER
     C*
     C     PDKY          KLIST
     C                   KFLD                    CMHOSF
     C                   KFLD                    CMHOSM
     C*
     C     PDKY1         KLIST
     C                   KFLD                    TPPHYF
     C                   KFLD                    TPPHYM
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE ADDITIONAL LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
     C*
     C*  KEY LIST FOR THE DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  Key list for the Employer Carrier Info file
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C* Key list for office information file
     C     OFKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMOFF#
     C*
     C*
     C*
     C   60
     CANN60              MOVE      *OFF          *IN60
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROI            2
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDATE            8
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    QQJRST            2
     C                   PARM                    XXTSET            3
     C                   PARM                    ##FND             3 0
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
      *
     C*
     C     *LIKE         DEFINE    LGTSET        SVTSET
     C     *LIKE         DEFINE    NICO#         XXCO#
     C     *LIKE         DEFINE    NIDT          XXDATE
     C     *LIKE         DEFINE    LGHCPY        XXHCPY
     C     *LIKE         DEFINE    NIFND         XXFND
     C     *LIKE         DEFINE    NIFND         SVFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    F2IAUD        XXIAUD
     C     *LIKE         DEFINE    F2SCTC        XXSCTC
     C     *LIKE         DEFINE    F2AREA        XXFARA
     C     *LIKE         DEFINE    F2SPH#        XXFPH#
     C     *LIKE         DEFINE    CXRSRD        XXRMCD
     C     *LIKE         DEFINE    CMDT          XXSYDT
     C     *LIKE         DEFINE    NIST          X2ST                           ADD 05/18/00
     C     *LIKE         DEFINE    NAKEY         XXKEY
     C     *LIKE         DEFINE    NADESC        XXNATR
     C     *LIKE         DEFINE    PADESC        XXPART
     C     *LIKE         DEFINE    CADESC        XXCAUS
      *** Beg Del ***** 02/20/13 ******************************************
     C******LIKE         DEFINE    CMDAYS        XXHRWK
      *** End Del ***** 02/20/13 ******************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#
     C                   MOVE      *OFF          XXOPN2
     C*
     C                   MOVE      *BLANKS       XXCPY            21
     C                   Z-ADD     0             X                 5 0
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   MOVEL     QQFROI        XXHCPY
     C                   MOVE      QQJRST        XXJRST            2
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C     @@CASE        COMP      *BLANKS                            7474
     C*
     C*  Call system API to get various system values.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@AR              1
     C                   PARM                    @@NTWK            1
     C                   PARM                    @@UR              1
     C                   PARM                    @@MED             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@WCDK            1
     C                   PARM                    @@RHAB            1
     C                   PARM                    @@MSS             1
     C                   PARM                    @@FSCH            1
     C                   PARM                    @@PRVB            1
     C                   PARM                    @@NPWC            1
     C                   PARM                    @@OS              1
     C                   PARM                    @@ARLS            1
     C                   PARM                    @@CCOD            1
     C                   PARM                    @@CRVW            1
     C                   PARM                    @@PROM            1
     C                   PARM                    @@MCRV            1
     C                   PARM                    @@FREQ            1
     C                   PARM                    @@CRAC            1
     C                   PARM                    @@PREO            1
     C                   PARM                    @@PARN            1
     C                   PARM                    @@MGRI            1
     C                   PARM                    @@KCIF            1
     C                   PARM                    @@CURS            1
     C                   PARM                    @@PCIB            1
     C                   PARM                    @@CLT#            3
     C                   PARM                    @@UCPF            1
     C                   PARM                    @@DCF             1
     C                   PARM                    @@AUTB            1
     C*
     C*   RETRIEVE COMPANY MASTER INFO
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      *BLANKS       XXOUTQ           10
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     OROUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      XXOUTQ           10
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C                   MOVEL     OROUTQ        X1OUTQ           10
     C     X1OUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      X1OUTQ
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      '2'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C                   MOVEL     OROUTQ        X2OUTQ           10
     C     X2OUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      X2OUTQ
     C                   ENDIF
     C*
     C* If called from WW Claims opt31, read WMCMP and print one copy.
     C*
     C     *IN74         IFEQ      *ON
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              OPEN      QSYSPRT2
     C                   OPEN      W8070OFL1
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *ON           XXOPN2
     C                   Z-ADD     3             XXCOPY
     C                   ELSE
     C*
     C*  IF THIS PGM WAS CALLED FROM THE DAILY BATCH, WXNIP WILL BE READ
     C*  3 TIMES TO PRODUCE 3 COPIES.  IF CALLED FROM THE EXTRACT
     C*  OPTION, 1 READ WILL OCCUR.
     C*
     C     QQFROI        IFNE      '  '
     C                   Z-ADD     3             XXCOPY            1 0
     C                   ELSE
     C                   Z-ADD     1             XXCOPY
     C                   END
     C*
     C     NEXT          TAG
     C                   Z-ADD     0             SVFND
     C                   Z-ADD     XXCOPY        X
     C                   MOVE      CPY(X)        XXCPY
     C     X             COMP      1                                      71
     C     X             COMP      2                                      72
     C     X             COMP      3                                      73
     C*
     C*   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *OFF
     C     XXCOPY        IFEQ      1
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              OPEN      QSYSPRT
     C                   OPEN      W8070OFL
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *ON           XXOPEN
     C                   END
     C     XXCOPY        IFEQ      2
     C                   OPEN      QSYSPRT1
     C                   MOVE      *ON           XXOPN1
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              OPEN      QSYSPRT2
     C                   OPEN      W8070OFL1
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *ON           XXOPN2
     C                   END
     C                   END
     C*
     C                   ENDIF
     C*
     C   71NIKY          CHAIN     PNI01                              50
     C   72NIKY          CHAIN     PNI02                              50
     C   73LGKY          CHAIN     PLG01                              50
     C   74CMKY          CHAIN     PCM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     *IN71         IFEQ      '1'
     C     NIIDT         ANDNE     XXDATE
     C     *IN72         OREQ      '1'
     C     NIIDT         ANDNE     XXDATE
     C                   GOTO      SKIP
     C                   END
     C     *IN73         IFEQ      *ON
     C     LGPRSQ        IFNE      0
     C     LGSCHP        ORNE      LGEXDT
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*
     C*   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *ON
     C     XXOPN2        ANDEQ     *OFF
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              OPEN      QSYSPRT2
     C                   OPEN      W8070OFL1
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *ON           XXOPN2
     C                   END
     C*
     C                   EXSR      SRINIT
     C*
     C*  IF THE FUND CHANGED, GET THE NEW FUND INFORMATION.
     C*
     C     XXFND         IFNE      SVFND
     C                   EXSR      FNDBRK
     C                   Z-ADD     XXFND         SVFND
     C                   END
     C*
     C*  LOAD CLAIMANT INFORMATION
     C*
     C     CMKY          CHAIN     PCM01                              60
     C   60              GOTO      SKIP
     C     CDKY          CHAIN     PCD01                              60
     C   60              GOTO      SKIP
     C                   MOVE      CMJRST        X2ST
     C     NIKY2         CHAIN     PNI04                              60
     C   60              MOVE      *BLANKS       NI1A3
     C   60              MOVE      *BLANKS       NI10A1
     C   60              MOVE      *BLANKS       NI1A5
     C   60              MOVE      *ZEROS        NIDT4
     C   60              MOVE      *BLANKS       NI80A1
     C*
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDMID         XXFRST
     C                   EXSR      SRFLNM
     C                   MOVEL     XXNAME        XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   EXSR      SRFLNM
     C                   MOVEL     XXNAME        XXCNAM
     C                   MOVEL     CDCTY         XXCCTY
     C                   MOVEL     CDZIP         XXCZIP
     C                   MOVEL     CMACTY        XXACTY
     C*
     C                   MOVEL     *BLANKS       XXPNAM
     C                   MOVEL     *BLANKS       XXPAD1
     C                   MOVEL     *BLANKS       XXPCSZ
     C                   MOVE      *ZEROS        XXZIPZ
     C                   MOVE      *BLANKS       XXAREA
     C                   MOVEL     *BLANKS       XXPPH#
     C* GET PHYSICIAN OR HOSPITAL INFORMATION
     C*
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         IFEQ      *OFF
     C     TPPHYN        IFNE      *BLANKS
     C                   MOVEL     TPPHYN        XXPNAM
     C                   MOVEL     TPPHYA        XXPAD1
     C                   MOVEL(P)  TPPHYC        XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       TPPHYS:1      XXPCSZ
     C                   MOVEL     TPPHYZ        XXZIPZ            9 0
     C                   MOVE      TPPHYZ        XXZIPZ
     C                   MOVE      TPPHYR        XXAREA
     C                   MOVEL     TPPHYP        XXPPH#
     C                   MOVE      TPPHYP        XXPPH#
     C                   ENDIF
     C*
     C     TPPHYF        IFNE      *ZEROS
     C*
     C     PDKY1         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     PDFSN         IFNE      *BLANKS
     C     PDFSN         CAT       PDNAME:1      XXPNAM
     C                   ELSE
     C                   MOVEL     PDNAME        XXPNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDADR1        XXPAD1
     C                   MOVEL(P)  PDCTY         XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       PDST:1        XXPCSZ
     C                   Z-ADD     PDZIP         XXZIPZ
     C                   Z-ADD     PDAREA        XXAREA
     C                   Z-ADD     PDPHN#        XXPPH#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     PDFSN         IFNE      *BLANKS
     C     PDFSN         CAT       PDNAME:1      XXPNAM
     C                   ELSE
     C                   MOVEL     PDNAME        XXPNAM
     C                   ENDIF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        XXPAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        XXPAD1
     C                   ENDIF
     C                   MOVEL(P)  PDCTY         XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       PDST:1        XXPCSZ
     C                   Z-ADD     PDZIP         XXZIPZ
     C                   Z-ADD     PDAREA        XXAREA
     C                   Z-ADD     PDPHN#        XXPPH#
     C*
     C                   ELSE
     C*
     C     CMHOSN        IFNE      *BLANKS
     C                   MOVEL     CMHOSN        XXPNAM
     C                   MOVEL     CMHOSA        XXPAD1
     C                   MOVEL(P)  CMHOSC        XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       CMHOSS:1      XXPCSZ
     C                   MOVEL     CMHOSZ        XXZIPZ            9 0
     C                   MOVE      CMHOSZ        XXZIPZ
     C                   MOVE      CMHOSR        XXAREA
     C                   MOVEL     CMHOSP        XXPPH#
     C                   MOVE      CMHOSP        XXPPH#
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXNATR
     C                   MOVE      *BLANKS       XXPART
     C                   MOVE      *BLANKS       XXCAUS
     C*
     C*  Retrieve the nature description.
     C*
     C                   MOVEL(P)  CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C  N60              MOVEL     NADESC        XXNATR
     C*
     C*  Retrieve the part description.
     C*
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C  N60              MOVEL     PADESC        XXPART
     C*
     C*  Retrieve the cause description.
     C*
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C  N60              MOVEL     CADESC        XXCAUS
     C*
     C*  PRINT 'MC' ON LABEL FOR MANAGED CARE CLAIMS
     C*
     C                   MOVE      *BLANKS       XXMC              2
     C     CMMCO         IFEQ      'Y'
     C                   MOVEL     'MC'          XXMC
     C                   END
     C*
     C*  LOAD THE CITY, STATE AND ZIP FOR THE LABEL
     C*
     C                   MOVE      CDCTY         XXCTY
     C                   MOVE      CDST          XXSTAT
     C                   Z-ADD     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXCCSZ           40
     C                   MOVEL     XXCSZ         XXCCSZ
     C     CDADR2        COMP      *BLANKS                                41
     C*
     C*  CONVERT ALL DATES WHICH ARE PRINTED.
     C*
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXRCVD
     C*
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXIRTN
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXACDT
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDTOB
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDTRP
     C*
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDOH
     C*
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDTLW
     C*
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXFSDT
     C*
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDOD
     C*
     C*  CONVERT THE ACCIDENT TIME
     C*
     C     CMACTM        IFGE      130000
     C     CMACTM        SUB       120000        XXACTM
     C                   ELSE
     C     CMACTM        IFLT      10000
     C                   MOVE      CMACTM        XXWK6A            6
     C                   MOVEL     '12'          XXWK6A
     C                   MOVE      XXWK6A        XXACTM
     C                   ELSE
     C                   Z-ADD     CMACTM        XXACTM
     C                   END
     C                   END
     C     CMACTM        IFGE      120000
     C     CMACTM        ANDNE     240000
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C*  GET THE EMPLOYER INFORMATION
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM
      *** Beg Add ***** 02/20/13 ******************************************
     C                   MOVEL     EMNAME        XXENAM1
      *** End Add ***** 02/20/13 ******************************************
     C                   MOVEL     EMFDNM        XXFDNM
     C                   MOVEL     EMNAOB        XXNAOB
     C*
     C     EMMAD1        IFEQ      *BLANKS
     C                   MOVE      EMPAD1        EMMAD1
     C                   MOVE      EMPAD2        EMMAD2
     C                   MOVE      EMPCTY        EMMCTY
     C                   MOVE      EMPST         EMMST
     C                   Z-ADD     EMPZIP        EMMZIP
     C                   END
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVE      EMMAD1        EMPAD1
     C                   MOVE      EMMAD2        EMPAD2
     C                   MOVE      EMMCTY        EMPCTY
     C                   MOVE      EMMST         EMPST
     C                   Z-ADD     EMMZIP        EMPZIP
     C                   END
     C*
     C                   MOVEL     EMPCTY        XXECTY
     C                   MOVEL     EMPZIP        XXEZIP
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      EMMST         XXSTAT
     C                   Z-ADD     EMMZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXECSZ
      *** Beg Add ***** 02/20/13 ******************************************
     C                   MOVEL     XXCSZ         XXECSZ1
      *** End Add ***** 02/20/13 ******************************************
     C     EMMAD2        COMP      *BLANKS                                19
     C                   END
     C*
     C*  GET THE LOCATION INFORMATION, IF ANY.
     C*
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      '0'
     C     ALPAD1        IFNE      *BLANKS
     C                   MOVEL     ALPAD1        XXLAD1
     C                   MOVEL     ALPAD2        XXLAD2
     C                   MOVEL     ALPCTY        XXLCTY
     C                   MOVEL     ALPST         XXLST
     C                   MOVEL     ALPZIP        XXLZIP
     C                   ELSE
     C                   MOVEL     ALMAD1        XXLAD1
     C                   MOVEL     ALMAD2        XXLAD2
     C                   MOVEL     ALMCTY        XXLCTY
     C                   MOVEL     ALMST         XXLST
     C                   MOVEL     ALMZIP        XXLZIP
     C                   END
     C                   END
     C*
     C*  Use the policy # from the Carrier Info file, if it exists.
     C*
     C     RPKY          CHAIN     PRP01                              60
     C   60              MOVE      *BLANKS       RPCAP#
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVEL(P)  RPCAP#        XXPOL#           18
     C                   ELSE
     C                   MOVEL(P)  XXPOL1        XXPOL#
     C                   ENDIF
     C*
     C*  SET INDICATORS FOR ALL BOXES THAT GET MARKED.
     C*
     C     CDSEX         COMP      'F'                                    21
     C     CDSEX         COMP      'M'                                    43
     C     CMPOI         COMP      'N'                                    22
     C     CMPOI         COMP      'Y'                                    44
     C     CMFSAL        COMP      'Y'                                    23
     C     CMRTW         COMP      *ZEROS                                 24
     C     CMEAGR        COMP      'N'                                    29
     C     CMDNY         COMP      'Y'                                    31
     C     NI1A3         COMP      'Y'                                    32
     C     CMFSAL        COMP      'Y'                                    33
     C                   MOVEL     NI10A1        XXCD1
     C     XXCD1         COMP      'TT'                                   34
     C     XXCD1         COMP      '80'                                   35
     C     XXCD1         COMP      'TP'                                   36
     C     XXCD1         COMP      'IB'                                   37
     C     XXCD1         COMP      'PT'                                   38
     C     XXCD1         COMP      'DT'                                   39
     C     NI1A5         COMP      'Y'                                    42
     C                   SETOFF                                           25
     C                   SETOFF                                           26
     C                   SETOFF                                           27
     C                   SETOFF                                           28
     C                   SETOFF                                           30
     C                   SETOFF                                           45
     C     NI1A6         IFEQ      'Y'
     C                   SETOFF                                           31
     C                   SETON                                            46
     C                   ENDIF
     C     XXCD1         COMP      'SO'                                   47
     C*
     C                   Z-ADD     NIDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT1A
     C*
     C                   Z-ADD     NIDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT2A
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT1
     C*
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT2
     C*
     C                   Z-ADD     NIDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT4
     C*
     C                   Z-ADD     CMAWW         XX7N1
     C                   Z-ADD     CMCRT         XX7N2
     C     NI40A1        CAT       NI40A2:0      XX80A1
     C*
     C*
      *** Beg Add ***** 06/07/13 ******************************************
      /Free
       XXpamtp = NI9n1/100;
       XXiamtp = NI9n2/100;
      /End-Free
      *** End Add ***** 06/07/13 ******************************************
     C*  Calculate the # of hours per week.
     C*
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****CMHRSD        MULT      CMDAYS        XXHRWK
      /Free
       XXhrwk  = NI3n1;
       XXrtpay = NI9n3/100;
       Select;
       When NI20A9 = 'Hour';
          *In25 = *On;
       When NI20A9 = 'Week';
          *In26 = *On;
       When NI20A9 = 'Day';
          *In27 = *On;
       When NI20A9 = 'Month';
          *In28 = *On;
       Other;
          *In25 = *Off;
          *In26 = *Off;
          *In27 = *Off;
          *In28 = *Off;
       EndSl;
      /End-Free
      *** End Chg ***** 02/20/13 ******************************************
      *** Beg Add ***** 03/21/14 ******************************************
      * For with CLTID=114 and group designation (FNGRPASC)="OPTACOMP"
      * and Employer self insured flag is "Y"
      * A. Company Name, Address and Phone# should be the Group info
      * B. put the Employer Address in the Employer's Location Address
      * C. 'Claims-Handling Entity Name, Address & Telephone' should be
      *    the Group information
      /Free
         //*** Begin Chg *** 04/30/19 *************************************
         //*** If @@Clt# = '114' and FNgrpasc = 'OPTACOMP';
         If (@@Clt# = '114' Or @@Clt# = '152') and FNgrpasc = 'OPTACOMP';
         //*** End   Chg *** 04/30/19 *************************************
            If FNsfin = 'Y';
       //** Store EMPAD1 and EMPAD2 values
               xxEmPAd1 = EMpad1;
               xxEmPAd2 = EMpad2;
       //** A. Company Name, Address and Phone#
               xxENam = xxFNam;
               If FNmad1 <> *Blanks;
                  EMpad1 = FNmad1;
                  EMpad2 = FNmad2;
                  xxECty = FNmcty;
                  EMpst  = FNmst;
                  xxEZip = %subst(%editc(FNmzip:'X'):1:5);
               Endif;
               If FNpad1 <> *Blanks;
                  EMpad1 = FNpad1;
                  EMpad2 = FNpad2;
                  xxECty = FNpcty;
                  EMpst  = FNpst;
                  xxEZip = %subst(%editc(FNpzip:'X'):1:5);
               Endif;

               EMarea = FNph1a;
               EMphn# = FNph1;

       //** B. Employer's Location Address
               If EMmad1 <> *Blanks;
                  xxLAd1 = EMmad1;
                  xxLAd2 = EMmad2;
                  xxLCty = EMmcty;
                  xxLSt  = EMmst;
                  xxLZip = %subst(%editc(EMmzip:'X'):1:5);
               Endif;
               If xxEMpad1 <> *Blanks;
                  xxLAd1 = xxEMpad1;
                  xxLAd2 = xxEMpad2;
                  xxLCty = EMpcty;
                  xxLSt  = EMpst;
                  xxLZip = %subst(%editc(EMpzip:'X'):1:5);
               Endif;
       //** C. Claims-Handling Entity Name, Address & Telephone
               xxCOnm = xxFNam;
               If FNmad1 <> *Blanks;
                  xcADr1 = FNmad1;
                  zip1   = %subst(%editc(FNmzip:'X'):1:5);
                  zip2   = %subst(%editc(FNmzip:'X'):6:4);
                  xcCSz  = %trim(FNmcty) + ',' + FNmst + ' ' + zip1
                   + '-' + zip2;
               Endif;
               If FNpad1 <> *Blanks;
                  xcADr1 = FNpad1;
                  zip1   = %subst(%editc(FNpzip:'X'):1:5);
                  zip2   = %subst(%editc(FNpzip:'X'):6:4);
                  xcCSz  = %trim(FNpcty) + ',' + FNpst + ' ' + zip1
                   + '-' + zip2;
               Endif;

               xxCAr1 = FNph1a;
               xxCPh1 = FNph1;
            Endif;
         Endif;

      /End-Free
      *** End Add ***** 03/21/14 ******************************************
     C*
     C*  GET THE INITIAL REQUESTOR NAME
     C*
     C                   MOVE      *BLANKS       XXUSER           10
     C                   MOVEL     CMIUSR        XXUSER
     C*
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****71              EXCEPT    NOI
     C****N71              EXCEPT    NOI2
     C   71              Write     NOI
     C  N71              Write     NOI2
      *** End Chg ***** 02/20/13 ******************************************
     C*
     C*  PRINT A LABEL FOR THE EMPLOYEE COPY ONLY.
     C*
     C   72              EXCEPT    LABEL
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C     *IN73         IFEQ      *ON
     C     XXOPN2        ANDEQ     *ON
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              CLOSE     QSYSPRT2
     C                   CLOSE     W8070OFL1
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C*
     C*  WRITE THE DARTS REMARK, THEN PRINT THE NOI FORM.
     C*
     C  N73              EXSR      DARTS
     C   73              EXSR      SRDART
     C*
     C     *IN74         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C*  GET THE NEXT RECORD
     C*
     C     SKIP          TAG
     C*
     C     *IN74         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C   71NIKY          READE     PNI01                                  50
     C   72NIKY          READE     PNI02                                  50
     C   73LGKY          READE     PLG01                                  50
     C                   END
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C   71XXOPEN        IFEQ      *ON
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              CLOSE     QSYSPRT
     C                   CLOSE     W8070OFL
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *OFF          XXOPEN            1
     C                   ENDIF
     C   72XXOPN1        IFEQ      *ON
     C                   CLOSE     QSYSPRT1
     C                   MOVE      *OFF          XXOPN1            1
     C                   ENDIF
     C   72XXOPN2        IFEQ      *ON
      *** Beg Chg ***** 02/20/13 ******************************************
     C*****              CLOSE     QSYSPRT2
     C                   CLOSE     W8070OFL
      *** End Chg ***** 02/20/13 ******************************************
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C*
     C*  PROCESS THE NEXT COPY
     C*
     C     XXCOPY        IFEQ      1
     C                   ADD       1             XXCOPY
     C                   GOTO      NEXT
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
     C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
     C*****************************************************************
     C     SRINIT        BEGSR
     C*
     C                   MOVE      *BLANKS       XXCNAM           40
     C                   MOVE      *BLANKS       XXCCTY           15
     C                   MOVE      *BLANKS       XXCZIP            5
     C                   MOVE      *BLANKS       XXENAM           30
      *** Beg Add ***** 02/20/13 ******************************************
     C                   MOVEL     *BLANKS       XXENAM1
      *** End Add ***** 02/20/13 ******************************************
     C                   MOVE      *BLANKS       XXFDNM           30
     C                   MOVE      *BLANKS       XXECTY           15
     C                   MOVE      *BLANKS       XXEZIP            5
     C                   MOVE      *BLANKS       XXLCTY           15
     C                   MOVE      *BLANKS       XXLZIP            5
     C                   MOVE      *BLANKS       XXACTY           15
     C                   MOVE      *BLANKS       XXNAOB           30
     C                   MOVE      *BLANKS       XXLAD1           40
     C                   MOVE      *BLANKS       XXLAD2           40
     C                   MOVE      *BLANKS       XXLST             2
     C                   MOVE      *BLANKS       XXPNAM           35
     C                   MOVE      *BLANKS       XXPAD1           35
     C                   MOVE      *BLANKS       XXPCSZ           25
     C                   MOVE      *BLANKS       XXAREA            3 0
     C                   MOVE      *BLANKS       XXPPH#            7 0
     C                   MOVE      *BLANKS       XXECSZ           40
      *** Beg Add ***** 02/20/13 ******************************************
     C                   MOVEL     *BLANKS       XXECSZ1
      *** End Add ***** 02/20/13 ******************************************
     C                   Z-ADD     0             XXRCVD            8 0
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     0             XXACTM
     C                   Z-ADD     0             XXDTOB            8 0
     C                   Z-ADD     0             XXDTRP            8 0
     C                   Z-ADD     0             XXDOH             8 0
     C                   Z-ADD     0             XXDTLW            8 0
     C                   Z-ADD     0             XXFSDT            8 0
     C                   Z-ADD     0             XXIRTN            8 0
     C                   Z-ADD     0             XXDOD             8 0
     C                   Z-ADD     0             XXDT1             8 0
     C                   Z-ADD     0             XXDT2             8 0
     C                   Z-ADD     0             XXDT4             8 0
     C                   Z-ADD     0             XX7N1             7 2
     C                   Z-ADD     0             XX7N2             7 2
     C                   Z-ADD     0             XXDT1A            8 0
     C                   Z-ADD     0             XXDT2A            8 0
     C                   MOVE      *BLANKS       XX40A1           40
     C                   MOVE      *BLANKS       XXCD1             2
     C                   MOVE      *BLANKS       XX80A1           80
     C*
     C     *IN74         IFEQ      *OFF
     C     *IN73         IFEQ      *OFF
     C                   Z-ADD     NIFND         XXFND
     C                   Z-ADD     NIFYR         XXFYR
     C                   Z-ADD     NICASE        XXCASE
     C                   ELSE
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C                   END
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* FNDBRK SUBROUTINE              ** Subroutine **   F N D B R K *
     C* THIS SUBROUTINE GETS THE FUND INFORMATION.                    *
     C*****************************************************************
     C     FNDBRK        BEGSR
     C*
     C*  GET THE CLAIMS CONTROL FILE RECORD
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C     FNKY          CHAIN     PCX01                              60
     C*
     C*   GET REMARK CODE FOR DIVISION DIARY REMARKS.
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C  N60              MOVE      CXRSRD        XXRMCD
     C*
     C*  GET THE FUND NAME AND ADDRESS
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XCADR1           40
     C                   MOVE      *BLANKS       XCCSZ            40
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     FNNAME        XXFNAM           40
     C     @@CLT#        IFEQ      '044'
     C                   MOVEL     FNANAM        XXFNAM
     C                   ENDIF
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEA     FNANAM        CNB
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    CNB(X)                             60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     CNB(X)        CN
     C                   ELSE
     C                   MOVEA     FNANAM        CN
     C                   END
     C                   MOVEA     CN            XXCONM
     C*
     C                   MOVEL     FNAAD1        XCADR1
     C                   MOVE      FNAST         XXSTAT            2
     C                   Z-ADD     FNAZIP        XXZIP             9 0
     C                   MOVEL     FNACTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCCSZ
     C*
     C                   ELSE
     C*
     C*   LOAD THE CARRIER NAME AND ADDRESS FROM THE COMPANY FILE
     C*
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    CONM(X)                            60
     C                   MOVEA     CONM(X)       XXCONM
     C*
     C                   MOVEL     COADR1        XCADR1
     C                   MOVEL     COCTY         XXCTY
     C                   MOVEL     COST          XXSTAT
     C                   Z-ADD     COZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCCSZ
     C*
     C                   ENDIF
     C*
     C     FNSFIN        COMP      'Y'                                    40
     C                   END
     C*
     C*  GET INTERNAL AUDIT NUMBER
     C*
     C                   Z-ADD     0             XXIAUD
     C                   Z-ADD     0             XXSCTC
     C                   Z-ADD     0             XXFARA
     C                   Z-ADD     0             XXFPH#
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     F2IAUD        XXIAUD
     C     @@CLT#        IFEQ      '051'
     C                   MOVEL(P)  EMSEMP        XXIAUD
     C                   ENDIF
     C                   Z-ADD     F2SCTC        XXSCTC
     C                   Z-ADD     F2AREA        XXFARA
     C                   Z-ADD     F2SPH#        XXFPH#
     C                   END
     C                   MOVE      F2AREA        XXCAR1            4 0
     C                   MOVE      F2SPH#        XXCPH1            7 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT1            3
     C     @@CLT1        IFEQ      '139'
     C     OFKY1         CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      OFCNAM        XXCONM
     C                   MOVEL     OFCAD1        XCADR1
     C                   MOVEL     OFCCTY        XXCTY
     C                   MOVEL     OFCST         XXSTAT
     C                   Z-ADD     OFCZP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCCSZ
     C     OFCARA        IFNE      *ZEROS
     C     OFCPH1        ORNE      *ZEROS
     C     OFCPH2        ORNE      *ZEROS
     C                   MOVE      OFCARA        XXCAR1
     C                   MOVEL     OFCPH1        XXCPH1
     C                   MOVE      OFCPH2        XXCPH1
     C                   ENDIF                                                    OFCPH1 <> ' '
     C                   ENDIF                                                    RECORD EXISTS
     C                   ENDIF                                                    Client 139
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
     C* THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
     C* CONTAINING THE CITY, STATE AND ZIP                            *
     C*****************************************************************
     C     SRADDR        BEGSR
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRFLNM SUBROUTINE              ** Subroutine **   S R F L N M *
     C* THIS SUBROUTINE PUTS TOGETHER A NAME, THE FIRST NAME FOLLOWED *
     C* BY THE LAST NAME (JOHN SMITH)                                 *
     C*****************************************************************
     C     SRFLNM        BEGSR
     C*
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME           40
     C*
     C                   ENDSR
     C*****************************************************************
     C* DARTS SUBROUTINE               ** Subroutine **   D A R T S   *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH NOI FORM       *
     C* PRINTED.                                                      *
     C*****************************************************************
     C     DARTS         BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
      *
      *  DO NOT WRITE A DARTS RECORD IF THE REMARK CODE WAS NOT SETUP.
      *
     C     XXRMCD        IFNE      *BLANKS
      *
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZERO         Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  XXRMCD        Q#RMCD            5
     C                   MOVE      *BLANKS       Q#FOLD           80
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVE(P)   DSDATE        Q#CDAT            8
     C                   MOVE(P)   *BLANKS       Q#COM2           19
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
      *
     C   71              MOVEL(P)  CPY(1)        Q#COMM
     C   72              MOVEL(P)  CPY(2)        Q#COMM
      *
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
     C                   ENDIF                                                  XXRMCD
      *
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
     C* IS PROCESSED.                                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C*  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
     C*  BLANK OR INVALID.
     C*
     C                   MOVE      '148'         XXTSET
     C                   MOVEL     CMFND         ##FND
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C   60              GOTO      SKIP1
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
      *
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  LGIUSR        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZERO         Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVE(P)   DSDATE        Q#CDAT            8
     C                   MOVEL(P)  IXHCPR        Q#RMCD            5
      *
     C     Q#RMCD        CHAIN     PCT01                              60
     C   60              GOTO      SKIP1
      *
      *  GET FOLDER NAME
      *
     C                   MOVE      *BLANKS       QQFND             8
     C                   MOVE      *BLANKS       QQSER             8
     C                   MOVE      *BLANKS       QQSUB             8
     C                   MOVE      *BLANKS       QQFOLD            8
     C                   MOVE      *BLANKS       QQFLDX            7
     C                   MOVE      *BLANKS       QQFPTH           40
     C                   MOVE      *BLANKS       QQTERM            1
      *
     C                   MOVEL     XXFND         QQFND                          for Fund
     C                   MOVEL     'CS'          QQSER                          for Area of Service
     C                   MOVEL     CTTYPE        QQSUB                          for Sub-Area
      *
     C                   MOVEL     'C'           QQFOLD                         means "Member"
     C                   MOVEL     CMCASE        QQFLDX                         CASE for Folder name
     C                   MOVE      QQFLDX        QQFOLD
      *
     C                   CALL      'SRFBLD'
     C                   PARM                    QQFND
     C                   PARM                    QQSER
     C                   PARM                    QQSUB
     C                   PARM                    QQFOLD
     C                   PARM                    QQFPTH                         Built, returned
     C                   PARM                    QQTERM
      *
     C                   MOVEL(P)  QQFPTH        Q#FOLD           80
      *
      *  GET DOCUMENT ID
      *
     C                   MOVE      *BLANKS       QQRT              2
     C                   MOVE      *BLANKS       QQDOC            12
     C                   MOVE      *BLANKS       QQTERM            1
      *
     C                   MOVEL     CTTYPE        QQRT                           Remark Type
     C                   CALL      'SRDBLD'
     C                   PARM                    QQFPTH                         From SRFBLD routine
     C                   PARM                    QQRT                           Rmrk Type for Doc ID
     C                   PARM                    QQDOC                          Built, returned
     C                   PARM                    QQTERM
      *
     C                   MOVEL(P)  QQDOC         Q#DCID           12
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVE(P)   *BLANKS       Q#COM2           19
     C                   MOVEL(P)  CPY(3)        Q#COMM          256
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
      *  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
      *
     C     LGKY1         CHAIN     PLG02                              60
      *
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     *ZERO         LGPRSQ
     C                   MOVE      Q#SEQ#        LGPRSQ
     C                   UPDATE    PLG02
     C                   END
      *
      *  CALL W8070C1FL TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
      *
     C                   MOVE      Q#DCID        QQDCID           12
     C                   MOVE      Q#FOLD        QQFLDR           80
      *
      *
     C                   CALL      'W8070C1F'
     C                   PARM                    Q1JOB
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C*
     C     SKIP1         TAG
     C                   ENDSR
     C**************************************************************************
      *** Beg Del ***** 02/20/13 ******************************************
     O*=========================================================================
     O*QSYSPRT   E            NOI              05
     O*******                XXRCVD              65 '  /  /    '
     O*******   E            NOI              13
     O*******                XXCNAM              45
     O*******                XXSS1               55
     O*******                                    56 '-'
     O*******                XXSS2               58
     O*******                                    59 '-'
     O*******                XXSS3               63
     O*******                XXACDT              87 '  /  /    '
     O*******                XXTIME             105 '  :  '
     O*******       N20                         107 'X'
     O*******        20                         113 'X'
     O*******   E            NOI              15
     O*******                CDADR1              51
     O*******                CMACDS             110
     O*******   E            NOI              16
     O*******                CDADR2              51
     O*******                CMACD2             110
     O*******   E            NOI              17
     O*******                XXCCTY              22
     O*******                CDST                31
     O*******                XXCZIP              45
     O*******   E            NOI              18
     O*******                XXCAUS              80
     O*******   E            NOI              20
     O*******                CDAREA        4     22
     O*******                CDPHN#              40 '0   -    '
     O*******   E            NOI              22
     O*******                CMOCDS              45
     O*******   E            NOI              23
     O*******                XXNATR              80
     O*******                XXPART             114
     O*******   E            NOI              24
     O*******                XXDOBM               9
     O*******                XXDOBD              15
     O*******                XXDOBY              23
     O*******   E            NOI              25
     O*******        43                          33 'X'
     O*******        21                          40 'X'
     O*******   E            NOI              27
     O*******                XXFDNM              45
     O*******   E            NOI              28
     O*******                XXENAM              45
     O*******                EMFEIN              70 '0   -  -    '
     O*******                XXDTRP             105 '  /  /    '
     O*******                XXENAM              45
     O*******   E            NOI              30
     O*******                EMPAD1              48
     O*******   E            NOI              31
     O*******                EMPAD2              48
     O*******                XXNAOB              80
     O*******                XXPOL#             105
     O*******   E            NOI              32
     O*******                XXECTY              22
     O*******                EMPST               31
     O*******                XXEZIP              45
     O*******   E            NOI              34
     O*******                XXDOH               67 '  /  /    '
     O*******   E            NOI              35
     O*******                EMAREA        4     22
     O*******                EMPHN#              40 '0   -    '
     O*******        44                          95 'X'
     O*******        22                         104 'X'
     O*******   E            NOI              37
     O*******        23                          97 'X'
     O*******   E            NOI              38
     O*******                XXLAD1              48
     O*******                XXDTLW              67 '  /  /    '
     O*******   E            NOI              39
     O*******                XXLAD2              48
     O*******       N24                          64 'X'
     O*******        24                          70 'X'
     O*******   E            NOI              40
     O*******                XXLCTY              22
     O*******                XXLST               31
     O*******                XXLZIP              45
     O*******   E            NOI              41
     O*******                CMCTG1              35
     O*******                XXIRTN              67 '  /  /    '
     O*******                XXFSDT             102 '  /  /    '
     O*******   E            NOI              43
     O*******        25                         105 'X'
     O*******        26                         111 'X'
     O*******   E            NOI              44
     O*******                XXDOD               71 '  /  /    '
     O*******   E            NOI              45
     O*******                CMACDR              48
     O*******        27                         105 'X'
     O*******        28                         111 'X'
     O*******   E            NOI              46
     O*******                XXACTY              22
     O*******                CMAST               31
     O*******                CMACZ1        4     45
     O*******                CMHRSD        K    115
     O*******   E            NOI              47
     O*******       N29                          60 'X'
     O*******        29                          70 'X'
     O*******   E            NOI              48
     O*******                XXHRWK        K    115
     O*******   E            NOI              49
     O*******                CMDAYS        K    115
     O*******   E            NOI              52
     O*******                XXPNAM             119
     O*******   E            NOI              53
     O*******                XXPAD1             119
     O*******   E            NOI              54
     O*******                XXPCSZ             109
     O*******                XXZIPZ             119 '     -    '
     O*******   E            NOI              55
     O*******                XXAREA        4     87
     O*******                XXPPH#              96 '   -    '
     O*******   E            NOI              57
     O*******        45                         102 'X'
     O*******        30                         107 'X'
     O*******   E            NOI              59
     O*******        31                           5 'X'
     O*******        32                          59 'X'
     O*******   E            NOI              60
     O*******                XXDT1A             100 '  /  /    '
     O*******   E            NOI              61
     O*******        46                           5 'X'
     O*******   E            NOI              62
     O*******                XXDT2A             100 '  /  /    '
     O*******   E            NOI              63
     O*******        42                           5 'X'
     O*******                XXDT1               41 '  /  /    '
     O*******        33                          74 'X'
     O*******                XXDT2              103 '  /  /    '
     O*******   E            NOI              65
     O*******                XXDT4               37 '  /  /    '
     O*******                XX7N1         K     70
     O*******                XX7N2         K    100
     O*******   E            NOI              68
     O*******        34                          10 'X'
     O*******        35                          19 'X'
     O*******        36                          32 'X'
     O*******        37                          41 'X'
     O*******        38                          50 'X'
     O*******        39                          58 'X'
     O*******        47                          69 'X'
     O*******   E            NOI              70
     O*******                NI9N1         K     42
     O*******                NI9N2         K     75
     O*******   E            NOI              72
     O*******                NI40A1              45
     O*******   E            NOI              73
     O*******                XXFNAM             115
     O*******   E            NOI              74
     O*******                NI40A2              45
     O*******   E            NOI              75
     O*******                XXCONM             115
     O*******   E            NOI              76
     O*******                F2IAUD              15
     O*******                CMCLAS              39
     O*******                EMNAIC              60
     O*******                XCADR1             115
     O*******   E            NOI              77
     O*******                XCCSZ              115
     O*******   E            NOI              78
     O*******                XXCAR1        4     78
     O*******                XXCPH1              87 '0   -    '
     O*******   E            NOI              79
     O*******                F2SCTC              15
     O*******                CMCO#               32
     O*******                                    33 '-'
     O*******                CMFND               36
     O*******                                    37 '-'
     O*******                CMFYR               40
     O*******                                    41 '-'
     O*******                CMCASE              48
     O*******   E            NOI              81
     O*******                XXCPY               62
     O*******                XXUSER             115
     O*
     O*  THIS SECTION WILL ONLY PRINT ON THE EMPLOYER COPY
     O*******   E            NOI              19
     O*******        71      XXENAM              60
     O*******   E            NOI              20
     O*******        71      EMMAD1              60
     O*******   E    71      NOI              21
     O*******          N19   EMMAD2              60
     O*******           19   XXECSZ              60
     O*******   E    71      NOI              22
     O*******          N19   XXECSZ              60
     O*******
      *** End Del ***** 02/20/13 ******************************************
     O*----------------------------------------------------------------
     OQSYSPRT1  E            LABEL          1 11
     O                       XXCNAM              45
     O          E            LABEL          1
     O                       CDADR1              45
     O          E      N41   LABEL          1
     O                       CDADR2              45
     O          E            LABEL          1
     O*----------------------------------------------------------------
      *** Beg Del ***** 02/20/13 ******************************************
     O*QSYSPRT2  E            NOI2             05
     O******                 XXRCVD              65 '  /  /    '
     O******    E            NOI2             13
     O******                 XXCNAM              45
     O******                 XXSS1               55
     O******                                     56 '-'
     O******                 XXSS2               58
     O******                                     59 '-'
     O******                 XXSS3               63
     O******                 XXACDT              87 '  /  /    '
     O******                 XXTIME             105 '  :  '
     O******        N20                         107 'X'
     O******         20                         113 'X'
     O******    E            NOI2             15
     O******                 CDADR1              51
     O******                 CMACDS             110
     O******    E            NOI2             16
     O******                 CDADR2              51
     O******                 CMACD2             110
     O******    E            NOI2             17
     O******                 XXCCTY              22
     O******                 CDST                31
     O******                 XXCZIP              45
     O******    E            NOI2             18
     O******                 XXCAUS              80
     O******    E            NOI2             20
     O******                 CDAREA        4     22
     O******                 CDPHN#              40 '0   -    '
     O******    E            NOI2             22
     O******                 CMOCDS              45
     O******    E            NOI2             23
     O******                 XXNATR              80
     O******                 XXPART             114
     O******    E            NOI2             24
     O******                 XXDOBM               9
     O******                 XXDOBD              15
     O******                 XXDOBY              23
     O******    E            NOI2             25
     O******         43                          33 'X'
     O******         21                          40 'X'
     O******    E            NOI2             27
     O******                 XXFDNM              45
     O******    E            NOI2             28
     O******                 XXENAM              45
     O******                 EMFEIN              70 '0   -  -    '
     O******                 XXDTRP             105 '  /  /    '
     O******                 XXENAM              45
     O******    E            NOI2             30
     O******                 EMPAD1              48
     O******    E            NOI2             31
     O******                 EMPAD2              48
     O******                 XXNAOB              80
     O******                 XXPOL#             105
     O******    E            NOI2             32
     O******                 XXECTY              22
     O******                 EMPST               31
     O******                 XXEZIP              45
     O******    E            NOI2             34
     O******                 XXDOH               67 '  /  /    '
     O******    E            NOI2             35
     O******                 EMAREA        4     22
     O******                 EMPHN#              40 '0   -    '
     O******         44                          95 'X'
     O******         22                         104 'X'
     O******    E            NOI2             37
     O******         23                          97 'X'
     O******    E            NOI2             38
     O******                 XXLAD1              48
     O******                 XXDTLW              67 '  /  /    '
     O******    E            NOI2             39
     O******                 XXLAD2              48
     O******        N24                          64 'X'
     O******         24                          70 'X'
     O******    E            NOI2             40
     O******                 XXLCTY              22
     O******                 XXLST               31
     O******                 XXLZIP              45
     O******    E            NOI2             41
     O******                 CMCTG1              35
     O******                 XXIRTN              67 '  /  /    '
     O******                 XXFSDT             102 '  /  /    '
     O******    E            NOI2             43
     O******         25                         105 'X'
     O******         26                         111 'X'
     O******    E            NOI2             44
     O******                 XXDOD               71 '  /  /    '
     O******    E            NOI2             45
     O******                 CMACDR              48
     O******         27                         105 'X'
     O******         28                         111 'X'
     O******    E            NOI2             46
     O******                 XXACTY              22
     O******                 CMAST               31
     O******                 CMACZ1        4     45
     O******                 CMHRSD        K    115
     O******    E            NOI2             47
     O******        N29                          60 'X'
     O******         29                          70 'X'
     O******    E            NOI2             48
     O******                 XXHRWK        K    115
     O******    E            NOI2             49
     O******                 CMDAYS        K    115
     O******    E            NOI2             52
     O******                 XXPNAM             119
     O******    E            NOI2             53
     O******                 XXPAD1             119
     O******    E            NOI2             54
     O******                 XXPCSZ             109
     O******                 XXZIPZ             119 '0     -    '
     O******    E            NOI2             55
     O******                 XXAREA        4     87
     O******                 XXPPH#              96 '0   -    '
     O******    E            NOI2             57
     O******         45                         102 'X'
     O******         30                         107 'X'
     O******    E            NOI2             59
     O******         31                           5 'X'
     O******         32                          59 'X'
     O******    E            NOI2             60
     O******                 XXDT1A             100 '  /  /    '
     O******    E            NOI2             61
     O******         46                           5 'X'
     O******    E            NOI2             62
     O******                 XXDT2A             100 '  /  /    '
     O******    E            NOI2             63
     O******         42                           5 'X'
     O******                 XXDT1               41 '  /  /    '
     O******         33                          74 'X'
     O******                 XXDT2              103 '  /  /    '
     O******    E            NOI2             65
     O******                 XXDT4               37 '  /  /    '
     O******                 XX7N1         K     70
     O******                 XX7N2         K    100
     O******    E            NOI2             68
     O******         34                          10 'X'
     O******         35                          19 'X'
     O******         36                          32 'X'
     O******         37                          41 'X'
     O******         38                          50 'X'
     O******         39                          58 'X'
     O******         47                          69 'X'
     O******    E            NOI2             70
     O******                 NI9N1         K     42
     O******                 NI9N2         K     75
     O******    E            NOI2             72
     O******                 NI40A1              45
     O******    E            NOI2             73
     O******                 XXFNAM             115
     O******    E            NOI2             74
     O******                 NI40A2              45
     O******    E            NOI2             75
     O******                 XXCONM             115
     O******    E            NOI2             76
     O******                 F2IAUD              15
     O******                 CMCLAS              39
     O******                 EMNAIC              60
     O******                 XCADR1             115
     O******    E            NOI2             77
     O******                 XCCSZ              115
     O******    E            NOI2             78
     O******                 XXCAR1        4     78
     O******                 XXCPH1              87 '0   -    '
     O******    E            NOI2             79
     O******                 F2SCTC              15
     O******                 CMCO#               32
     O******                                     33 '-'
     O******                 CMFND               36
     O******                                     37 '-'
     O******                 CMFYR               40
     O******                                     41 '-'
     O******                 CMCASE              48
     O******    E            NOI2             81
     O******                 XXCPY               62
     O******                 XXUSER             115
      *** End Del ***** 02/20/13 ******************************************
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
