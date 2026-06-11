     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      * ***************************************************************
     H dftactgrp(*no) actgrp(*caller)
      /TITLE W0070R10 - MEDICAL INFORMATION
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY MEDICAL INFORMATION. *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R - CLAIM MASTER SEARCH                               *
      *
      *  CALLS TO:  FA003R - API to update MC claim master FMCMP
      *             SA001R - API to determine if MC installed
      *                                                               *
      *  CREATED:  2/05/97  PROGRAMMER: 47    PROJECT: 60ANC          *
      * ***************************************************************
      * REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 05/15/12 C:282 Project: 133WMZ                                *
     H*                Add functionality to Clm DARTS control when    *
     H*                flags change y to blank                        *
     H*                                                               *
     H* 01/09/12 C:251 Project: 114MCX                                *
     H*                Added 'Rx claims not eligible for payment'     *
     H*                                                               *
     H* 04/18/11 C:189 WC045 SEQ76540                                 *
     H*                Do not show the screen itself if all the       *
     H*                fields on the screen are not being displayed   *
     H*                due to conditional display                     *
     H*                                                               *
     H* 01/14/11 C:235 SUPPORT: 75281                                 *
     H*                Opt.10 Medical Info (SCRO3) will appear only if*
     H*                EDI module is installed or if the jurisdiction *
     H*                state is = CO.                                 *
     H*                                                               *
     H* 01/13/11 C:062 Project: 73983                                 *
     H*                INITIALIZE CALL PARAMETER.                     *
     H*                                                               *
     H* 12/13/10 C:198 Project: WC1963                                *
     H*                Updated HIC# from WMCMP/CMHIC# to WMCDP/CDHICN *
     H*                                                               *
     H* 12/03/10 C:220 SUPPORT: 74963                                 *
     H*                Make Backward compatible - If Treating Physician
     H*                Role is blank than work as previous.           *
     H*                                                               *
     H* 06/28/10 C:220 PROJRCT: 25WPV/ PRISS 25                       *
     H*                Only most recernt treating Physician should    *
     H*                be display in medical info screen.             *
     H*                                                               *
     H* 05/18/10 C:198 Project: WC1879                                *
     H*                Added 1 field SCAINJ just below CMS date of    *
     H*                injury.                                        *
     H*                                                               *
     H* 05/17/10 C:235 PROJRCT: 139WNF Additional changes             *
     H*                All New field and function keys will be        *
     H*                qualified on the WCMS EDI install flag='Y'     *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 03/23/10 C:235 PROJRCT: 139WNF                                *
     H*                EDI SCREEN DESIGN                              *
     H*                                                               *
     H* 01/04/10 C:227 Project: 135MMX                                *
     H*                Change the workflow for CALLR                  *
     H*                                                               *
     H* 11/03/09 C:227 Project: 135MMX                                *
     H*                Change adjuster workflow to enter Primary      *
     H*                diagnosis.                                     *
     H*                                                               *
     H* 11/02/09 C:232 Project: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*                                                               *
     H* 05/20/09 C:33  Project: WC1770                                *
     H*                Add processing for Medicare Secondary Payer    *
     H*                                                               *
     H* 02/27/08 C:180 PROJECT: 65WGI - PRISS # 10                    *
     H*                Client-flag Initial treatment.
     H*
     H* 08/20/07 C:193 PROJECT: 65WGI                                 *
     H*                EDI Rel 3.0 Impl/Setup/Training                *
     H*
     H* 10/19/07 C:191 Project: WC1584                                *
     H*               EDI Release 3, Remove "Release MR Authorization"*
     H*               and "Death Result of injury fields"             *
     H*                                                               *
     H* 05/01/07 C:185Project: WC1177                                 *
     H*               EDI Release 3, Added Maintenence Type Correction*
     H*
     H*  09/06/06 C: 58 135WHF:
     H*           Add Anticipated MMI date to SCR02. Optional.
     H*
     H*  03/06/06 C:132 Support: WC071 51160
     H*                Change Code to load DSDTTM and use DSDATE
     H*                instead of *DATE anywhere in the program.
      *
      *  02/23/05 C:53  PROJECT: WC1128   Auto-DARTS
      *                Add call to WA700R
      *                                                               *
      *  03/26/04 C:058 PROJECT: WC1144                               *
      *                 ADD PROVIDER FAX # TO SCREEN
      *                                                               *
      *  08/25/00 C:062 PROJECT: MC232                                *
      *                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
      *                                                               *
      *  04/03/00 C:118 PROJECT: WC854                                *
      *                 Add new field,'Impairment determined', to     *
      *                 display file and realign screen.  Add edit    *
      *                 if '% Impairment' is not 0, then 'Impairment  *
      *                 determined', must be Y. Pass parm CMIMP to    *
      *                 FA003R.                                       *
      *                                                               *
      *  09/17/99 C:112 Project: 48WRR                                *
      *           Addition of Continuation window informing user that *
      *           the Provider is inactive.  Screen allows user to    *
      *           continue and process the screen information or to   *
      *           allow user to Cancel                                *
      *                                                               *
      * 09/02/99 C:112 Support: 19256 (MC048)                         *
      *                Change QMSCL# from 10A to 25A                  *
      *                                                               *
      *  06/22/99 C:61 PROJECT 48WQJ                                  *
      *                Add previous fund number to header info.       *
      *                                                               *
      *  04/01/99 C:90  PROJECT: WC889/#17302                         *
      *           IF DBA FLAG = 'Y', LOAD PDADR2, ELSE LOAD PDADR1.   *
      *                                                               *
      *  02/19/99 C:112 PROJECT: 25WJW                                *
      *           Add code to protect fields in Display only mode     *
      *                                                               *
      * 11/16/98 C:15 Project: WC577                                  *
      *               Changed the bar code process.  A bar code API   *
      *               is now called to return certain parameters.     *
      *                                                               *
      * 08/31/98 C:15 Support: WC100/13042                            *
      *               If IXFROI and CMJRST are blank, do not call the *
      *               state specific screen because XXPGM = 'W0070R'  *
      *               and a recursive call error will result.         *
      *                                                               *
      * 08/11/98 C:62 Project: MC155                                  *
      *               ADDED PARM FOR CMFEAD TO FA003R                 *
      *                                                               *
      * 08/10/98 C:61 Project: 48WLC                                  *
      *               Display old Emp#/Div from Employer Cross        *
      *               Reference (WZEXP).                              *
      *                                                               *
      * 08/04/98 C:15 Project: WC577                                  *
      *               Fixed the claim record lockout routine.         *
      *                                                               *
      * 07/29/98 C:78 PROJECT: WC577                                  *
      *               QQCALL field was *blank before calling W4330R,  *
      *               therefore inquiry mode was allowing change opt. *
      *                                                               *
      * 07/13/98 C:15 Support: WC051/12451                            *
      *               Removed a parm from parm list for the state     *
      *               specific pgms.                                  *
      *                                                               *
      * 04/13/98 C:15 Project: WC468                                  *
      *               Added call to the EDI Log API pgm.              *
      *                                                               *
      * 04/07/98 C:62 Project: MC155                                  *
      *               Added additional fields to FA003R parm list.    *
      *                                                               *
      * 02/03/98 C:15 Project: WC577                                  *
      *               Chgd pgm to prevent loop in add mode.           *
      *                                                               *
      * 11/05/97 C:15 Project: WC577                                  *
      *               Changed the format of date fields used in the   *
      *               parm lists for pgms FA003R from MDCY to CYMD.   *
      *                                                               *
      * 04/15/97  C:30  Project: WC607                                *
      *                 Change W0531R Search to W0030R using F10 for  *
      *                 Current Attending/Treating Physician and      *
      *                 Hospital.                                     *
      *                                                               *
      *  PLEASE ADD TO TOP                                            *
      * ***************************************************************
      *                                                               *
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  07 - EDI INSTALLED                                           *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      *  20 - INVALID CURSOR POSITION FOR SEARCH                      *
      *  40 - ADD MODE                                                *
      * N40 - UPDATE MODE                                             *
      *  63 - Display Old Emp#/Div in header info                     *
      *  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
      *  66 - COMMERCIAL APPLICATION INSTALLED                        *
      *  67 - SECURITY LEVEL VALID                                    *
      * N67 - SECURITY LEVEL INVALID                                  *
      *  69 - EXCESS REINSURANCE DSP                                  *
      *  70 - BAR CODE INSTALLED                                      *
      *                                                               *
      * ***************************************************************
      * SCR02
      *  31 - SCAMMI Not zero but not valid date
      *  32 - SCAMMI Not zero but less than accident date
      *
      *
     FW0070F10  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
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
      *
      *  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      *
      *  PAYEE DEMOGRAPHIC FILE
     FWMPDL     IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FSMDIL5    IF   E           K DISK
      *
      ***** Beg Del ***** 05/15/12 ***************************************
     D*****/COPY SCOPYMT01I
      ***** End Del ***** 05/15/12 ***************************************
      * CLAIM MASTER
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
      * TREATING PHYSICIAN
     D DBFDS2          DS
     D  FLSTTP           *STATUS
     D  RCDLEN               283    286B 0
     D  DBRRN2               397    400B 0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D*   Parameters for MC Claim Master API        QMxxxx
     D*
     D PARMCL          DS                  INZ
     D  QMRTCD                 1      1
     D  QMSS#                  2     10
     D  QMACDT                11     17
     D  FILL1                 18     18
     D  QMCO#                 19     21
     D  QMSUB#                22     26
     D  QMFND                 27     29
     D  QMFYR                 30     32
     D  QMCASE                33     39
     D  QMEMP#                40     48
     D  QMDIV                 49     53
     D  QMACTM                54     59
     D  QMDTRP                60     66
     D  FILL2                 67     67
     D  QMDTLW                68     74
     D  FILL3                 75     75
     D  QMACDS                76    135
     D  QMMCON               136    136
     D  QMENTD               137    143
     D  FILL4                144    144
     D  QMRCVD               145    151
     D  FILL5                152    152
     D  QMMMI                153    159
     D  FILL6                160    160
     D  QM1A1                161    161
     D  QM10A1               162    171
     D  QMLTFL               172    172
     D  QMSCL#               173    197
     D  QMMCTM               198    201
     D  QMSTWK               202    203
     D  QMDEDT               204    210
     D  FILL7                211    211
     D  QMDEEM               212    213
     D  QMCMNU               214    216
     D  QMCMSU               217    219
     D  QMCLAD               220    222
     D  QMRRN                223    231  0
     D  QMLSN                232    261
     D  QMFSN                262    276
     D  QMMID                277    277
     D  QMDTRE               278    284
     D  FILL8                282    282
     D  QMCLSU               285    287
     D  QM#DEP               288    290
     D*
      *  DARTS
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      ***  Beg Add  ***  05/15/12  **************************************
     d #1Co#           s              3  0
     d #1Fnd           s              3  0
     d #1Fyr           s              3  0
     d #1Case          s              7  0
     d #1Key           s             10
     d ##Comm          s            256
     d ##Codz          s             10
     d svCodz          s             10
     d svDth           s              1
      *
     d                 DS
     d dscodz                  1      9
     d uc                      1      9
     d                                     DIM(9)
     d  CMdny                  1      1
     d  CMcvfl                 2      2
     d  CMsubr                 3      3
     d  CMexcs                 4      4
     d  CMacqr                 5      5
     d  CMosha                 6      6
     d  CMsdf                  7      7
     d  CMuslh                 8      8
     d  CMdth                  9      9
     *---------------------------------------------------------------------
     * Prototype Declaration
     *---------------------------------------------------------------------
     d WrtDrtRemarks   pr                  ExtPgm('WA700R')
     d #1Co#                          3  0
     d #1Fnd                          3  0
     d #1Fyr                          3  0
     d #1Case                         7  0
     d #1Key                         10
     d #1Comm                       256
     d #1Codz                        10
      ***  End Add  ***  05/15/12  **************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Medical Information')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D COMAPI          C                   CONST('WA001R')
      * ================================================================
      ***** Beg Add ***** 05/15/12 ***************************************
     C/COPY SCOPYMT01C
      ***** End Add ***** 05/15/12 ***************************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C*  REMARK CODE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD            5
     C*
     C*     DARTS FILE
     C*
     C     C2KY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C*     DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
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
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
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
     C     *LIKE         DEFINE    CMACDT        XXSGDT                         SURGERY
     C     *LIKE         DEFINE    CMACDT        XXDTRE                         REHAB ASSIGNED
     C     *LIKE         DEFINE    CMACDT        XXRLW                          RLS TO WORK
     C     *LIKE         DEFINE    CMACDT        XXMMI                          FINAL MMI
     C     *LIKE         DEFINE    CMACDT        XXDOD                          DT OF DEATH
     C     *LIKE         DEFINE    CMACDT        X7CMDI                         CMS DOI
     C     *LIKE         DEFINE    CMACDT        X7ORMD                         ORM Term Dt
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    UPUSNM        QCUSER
     C     *LIKE         DEFINE    TPPHYF        XXFEIN                         PHY FEIN
     C     *LIKE         DEFINE    TPPHYM        XXMOD                              MOD
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    SCSGDT        SVSGDT
     C     *LIKE         DEFINE    SCDTRE        SVDTRE
     C     *LIKE         DEFINE    SCRLW         SVRLW
     C     *LIKE         DEFINE    SCMMI         SVMMI
     C     *LIKE         DEFINE    SCDOD         SVDOD
     C*          *LIKE     DEFN SCHOSM    SVHOSM
     C*          *LIKE     DEFN SCHOSN    SVHOSN
     C*          *LIKE     DEFN SCHOSA    SVHOSA
     C*          *LIKE     DEFN SCHOSC    SVHOSC
     C*          *LIKE     DEFN SCHOSS    SVHOSS
     C*          *LIKE     DEFN SCHZP1    SVHZP1
     C*          *LIKE     DEFN SCHZP2    SVHZP2
     C*          *LIKE     DEFN SCHOSR    SVHOSR
     C*          *LIKE     DEFN SCHPN1    SVHPN1
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    *IN60         XXMANC
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
     C*
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
     C*
      *******Beg Del********05/15/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********05/15/12**********************************
     C*
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     CALLR         BEGSR
     C*
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
     C     CLTID         IFEQ      '135'
     C                   SELECT
     C     *IN09         WHENEQ    *ON
     C                   MOVE      '1'           ##CALL
     C                   OTHER
     C                   MOVE      ' '           ##CALL
     C                   ENDSL
     C*
     C                   CALL      'S0275R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      'WC'          ##ENV             2
      *
     C     *INKL         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   ENDIF                                                  @@MODE='A'
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      ECALLR
      *
     C                   ENDIF                                                  *INKL=*ON
      *
     C                   ENDIF                                                  CLTID='135'
      *
     C     @@MODE        IFEQ      'A'
     C                   SELECT
     C     CXBENI        WHENEQ    'Y'
     C                   CALL      'W0070R11'    ACDPL3                 99
      *
     C     CXRPS         WHENEQ    'Y'
     C                   CALL      'W0070R12'    ACDPL3                 99
      *
     C     CXSTSP        WHENEQ    'Y'
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
      *
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C     'W0070R'      CAT       IXFROI:0      XXPGM
     C                   ELSE
     C     'W0070R'      CAT       CMJRST:0      XXPGM
     C                   END
      *
     C     2             SUBST(P)  XXPGM:7       XXST              2
     C     XXST          IFNE      *BLANKS
      *
     C                   CALL      XXPGM                                99
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANK        QQRTCD            1
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *
     C                   ELSE
     C                   MOVE      *ON           *IN99
     C                   ENDIF
      *
     C     *IN99         IFEQ      *ON
     C     CXDBEN        CABEQ     'Y'           TDBEN
     C     CXDART        CABEQ     'Y'           TDART
     C                   ENDIF
      *
     C     CXDBEN        WHENEQ    'Y'
     C     TDBEN         TAG
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
     C*
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQRTCD
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *
     C     CXDART        WHENEQ    'Y'
     C     TDART         TAG
     C                   Z-ADD     CMCO#         QCCO#
     C                   Z-ADD     CMFND         QCFND
     C                   Z-ADD     CMFYR         QCFYR
     C                   Z-ADD     CMCASE        QCCASE
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QQKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
     C                   END                                                    @@MODE = A
      *
     C     @@MODE        IFNE      'A'
     C     Q1PRMS        ANDGE     5
     C                   CALL      'W0070R11'    ACDPL3                 99
     C                   END
      *
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INKL
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
     C     ECALLR        TAG
     C                   ENDSR                                                  CALLR
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C*
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
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
      *  VALIDATE HOSPITAL FEIN
      *
     C     SCHOSF        IFNE      *ZERO
     C                   MOVE      SCHOSF        XXFEIN
     C                   MOVE      SCHOSM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *ON
     C     *IN60         OREQ      *OFF
     C     CLTID         ANDEQ     '139'
     C     PDSTAT        ANDEQ     'P'
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
      *
      *  Hospital FEIN Should be Active but if it is not put up a
      *  warning informational window and allow user to proceed or to
      *  cancel as they determine.  If the press Enter then inactive
      *  Hospital FEIN will be stored.
      *
     C     PDSTAT        IFNE      'A'
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  CAN NOT ENTER BOTH FEIN/MOD AND NAME
      *
     C     *IN46         IFEQ      *OFF
     C     SCHOSF        IFNE      *ZERO
     C     SCHOSN        ANDNE     *BLANK
     C                   SETON                                        9074
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO                                                  *IN90
     C*
     C*  If another user has updated this claim, send an error message.
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
     C*
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     1             SCRN#             1 0
     C                   ENDSR                                                  ERRCK1
      *
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
      *
      *  DO NOT PROCESS IF *IN90 IS ON AND DROP OUT OF LOOP IF IT IS SET ON
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
     C                   MOVEA     '00000'       *IN(26)                           26 - 30
     C                   MOVEA     '0'           *IN(68)                           68
     C                   MOVEA     '0'           *IN(95)                           95
      *
      *  PRE EXIST DISABILITY CANNOT BE BLANK
      *
     C     SCPREX        IFNE      *BLANKS
     C     SCPREX        ANDNE     'Y'
     C     SCPREX        ANDNE     'N'
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
      *
      *  VALIDATE INITIAL TREATMENT CODE
      *
     C     CMLTFL        IFEQ      'I'
     C     SCITRT        IFEQ      *BLANKS
     C     CLTID         ANDEQ     '065'
     C                   MOVE      *ON           *IN95
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     SCITRT        IFNE      *BLANKS
     C                   MOVEL(P)  'INIT'        XXTBID
     C                   MOVEL(P)  SCITRT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9072
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  SURGERY DATE MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C     SCSGDT        IFNE      *ZERO
     C     ERSGDT        OREQ      'Y'
     C                   Z-ADD     SCSGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCSGDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCSGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXSGDT
      *
     C     XXSGDT        IFLT      CMACDT
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  REHAB ASSIGNED MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C     SCDTRE        IFNE      *ZERO
     C     ERDTRE        OREQ      'Y'
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTRE
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXDTRE
      *
     C     XXDTRE        IFLT      CMACDT
     C                   SETON                                        9074
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*  CMS Date of Incident
     C*
     C     SCCMDI        IFNE      *ZERO
     C**  Invalid
     C                   Z-ADD     SCCMDI        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCCMDI
     C     SRERR         IFEQ      *ON
     C                   SETON                                        333990
     C                   LEAVE
     C                   ENDIF
     C                   Z-ADD     SCCMDI        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         X7CMDI
     C**  Greater than to today
     C     X7CMDI        IFGT      XXDT
     C                   SETON                                        334890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C     SCAINJ        IFNE      *BLANKS
     C                   MOVEL     SCAINJ        XXAINJ            1
     C     SCAINJ        CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C     XXAINJ        IFNE      'E'
     C     DICMS         OREQ      'E'
     C                   SETON                                        4590
     C                   LEAVE
     C                   ELSE
     C     '.'           SCAN      SCAINJ        XXLOC             2 0
     C     XXLOC         IFNE      0
     C                   SETON                                        4590
     C                   LEAVE
     C                   ENDIF                                                  XXLOC <> 0
     C                   ENDIF                                                  XXAINJ <> 'E'
     C                   ELSE
     C                   SETON                                        4590
     C                   LEAVE
     C                   ENDIF                                                  *IN60 = '0'
     C                   ENDIF                                                  SCAINJ = ' '
      *  RELEASED TO WORK MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C     SCRLW         IFNE      *ZERO
     C     ERRLW         OREQ      'Y'
     C                   Z-ADD     SCRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCRLW
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXRLW
      *
     C     XXRLW         IFLT      CMACDT
     C                   SETON                                        9081
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C*  IF ANTICIPATED MMI DATE IS ENTERED, IT MUST BE A VALID DATE,
     C*       AND IT MUST BE LESS THAN THE ACCIDENT DATE.
     C     SCAMMI        IFNE      *ZERO
     C                   Z-ADD     SCAMMI        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCAMMI
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   Z-ADD     SCAMMI        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXMMI
     C*
     C     XXMMI         IFLT      CMACDT
     C                   SETON                                        9032
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  FINAL MMI DATE MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C     SCMMI         IFNE      *ZERO
     C     ERMMI         OREQ      'Y'
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCMMI
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN29
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXMMI
      *
     C     XXMMI         IFLT      CMACDT
     C                   SETON                                        9076
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C*
     C*  IMPAIRMENT DETERMINED MUST BE Y OR BLANK
     C*    EDIT % IMPAIRMENT <> 0, IMPAIRMENT DETERMINED MUST BE Y
     C*
     C     SCIMP         IFNE      'Y'
     C     SCIMP         ANDNE     *BLANK
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF                                                  SCIMFL<>Y
     C*
     C     SCIPCT        IFNE      *ZERO
     C     SCIMP         ANDNE     'Y'
     C                   SETON                                        9084
     C                   LEAVE
     C                   ENDIF                                                  SCIPCT<>0
     C*
     C*
     C*  Date of implant
     C*
     C     SCDTIM        IFNE      *ZERO
     C                   Z-ADD     SCDTIM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTIM
     C     SRERR         IFEQ      *ON
     C                   SETON                                        34  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
      *  PERCENT IMPAIRMENT
      *
     C     SCIPCT        IFLT      *ZERO
     C     SCIPCT        ORGT      100
     C                   SETON                                        9068
     C                   LEAVE
     C                   ENDIF
      *
     C*
     C*PERMANENT IMPAIRMENT MIN. PAYMENT
     C*
     C     SCPIMP        IFNE      'Y'
     C     SCPIMP        ANDNE     ' '
     C                   SETON                                        9051
     C                   LEAVE
     C                   ENDIF
      *  OCCUPATIONAL DISEASE
      *
     C     SCODIS        IFNE      *BLANK
     C     SCODIS        ANDNE     'Y'
     C     SCODIS        ANDNE     'N'
     C                   SETON                                        9077
     C                   LEAVE
     C                   ENDIF
     C*
     C*  Date of first exposure
     C*
     C     SCDFE         IFNE      *ZERO
     C                   Z-ADD     SCDFE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDFE
     C     SRERR         IFEQ      *ON
     C                   SETON                                        35  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  YEAR LAST EXPOSED
      *
     C     SCYRLX        IFNE      *ZERO
     C     SCYRLX        IFLT      1900
     C     SCYRLX        ORGT      2100
     C                   SETON                                        9082
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Date of first ingestion
     C*
     C     SCDFI         IFNE      *ZERO
     C                   Z-ADD     SCDFI         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDFI
     C     SRERR         IFEQ      *ON
     C                   SETON                                        36  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  HANDICAP CLAIM
      *
     C     SCHCAP        IFNE      *BLANK
     C     SCHCAP        ANDNE     'Y'
     C     SCHCAP        ANDNE     'N'
     C                   SETON                                        9078
     C                   LEAVE
     C                   ENDIF
     C*
     C*  ORM Indicator
     C*
     C     SCORM         IFNE      *BLANK
     C     SCORM         ANDNE     'Y'
     C     SCORM         ANDNE     'N'
     C     SCORM         ANDNE     ' '
     C                   SETON                                        37  90
     C                   LEAVE
     C                   ENDIF
      *
      *  DEATH CLAIM
      *
     C     SCDTH         IFNE      *BLANK
     C     SCDTH         ANDNE     'Y'
     C     SCDTH         ANDNE     'N'
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
     C*
     C*  ORM Termination Date
     C*
     C     SCORMD        IFNE      *ZERO
     C**  Invalid
     C                   Z-ADD     SCORMD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCORMD
     C     SRERR         IFEQ      *ON
     C                   SETON                                        383990
     C                   LEAVE
     C                   ENDIF
     C                   Z-ADD     SCORMD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         X7ORMD
     C**  Greater than Accident Date
     C     X7ORMD        IFLT      CMACDT
     C                   SETON                                        384790
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*  SCRXEL FLAG
     C*
     C     CLTID         IFEQ      '114'
     C     SCRXEL        IFNE      *BLANK
     C     SCRXEL        ANDNE     'Y'
     C                   SETON                                        86  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  DATE OF DEATH MUST BE GREATER THAN OR EQUAL TO ACCIDENT DATE
      *
     C                   MOVE      *ZERO         XXDOD
     C     SCDOD         IFNE      *ZERO
     C                   Z-ADD     SCDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDOD
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL(P)  SRDTO         XXDOD
      *
     C     XXDOD         IFLT      CMACDT
     C                   SETON                                        9080
     C                   LEAVE
     C                   ENDIF
     C                   END                                                    SCDOD NE 0
      *
      *  DATE OF DEATH CAN NOT BE GREATER THAN TODAY'S DATE
      *
     C     XXDOD         IFGT      XXDT
     C                   SETON                                        9075
     C                   LEAVE
     C                   ENDIF
     C*
     C*
     C***DEATH RESULT OF INJURY
     C     SCIDTH        IFNE      *BLANK
     C     SCIDTH        ANDNE     'Y'
     C     SCIDTH        ANDNE     'N'
     C     SCIDTH        ANDNE     'U'
     C                   SETON                                        9055
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  If another user has updated this claim, send an error message.
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
     C*
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     2             SCRN#
     C     CMJRST        IFEQ      'CO'
     C     @@EDI         OREQ      'Y'
     C                   Z-ADD     3             #OFSCR
     C                   ELSE
     C                   Z-ADD     2             #OFSCR
     C                   ENDIF
     C     #OFSCR        IFEQ      3
     C     *IN49         ANDEQ     *OFF
     C     *IN25         ANDEQ     *OFF
     C                   Z-ADD     2             #OFSCR
     C                   ENDIF
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  If another user has updated this claim, send an error message.
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
     C*
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C     SCAURL        IFNE      *BLANKS
     C     SCAURL        ANDNE     'Y'
     C     SCAURL        ANDNE     'N'
     C                   SETON                                        525390
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     3             SCRN#
     C                   ENDSR                                                  ERRCK3
     C*
      *** Beg Add ***** 05/15/12 **************************************
      ****************************************************************
      *  ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04                *
      ****************************************************************
      /Free
       BegSr  Errck4;
       Endsr;
      /End-Free
      ****************************************************************
      *  ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR03                *
      ****************************************************************
      /Free
       BegSr  Errck5;
       Endsr;
      /End-Free
      *** End Add ***** 05/15/12 **************************************
      * =========================================================
      *  SUBROUTINE TO GET PHYSICIAN NAME AND ADDRESS
      *    IF THE PHYSICIAN WAS NOT IN THE PROVIDER MASTER,
      *    GET NAME & ADDRESS FROM TREATING PHSYICIAN FILE.
      * =========================================================
     C     GETNM         BEGSR
     C*
     C*   FORMAT THE PROVIDER LAST NAME, FIRST
     C*
     C                   MOVE      *BLANK        XXATNM           30
     C     XXFEIN        IFEQ      *ZERO
     C                   MOVEL     TPPHYN        XXATNM
     C                   MOVEL     TPPHYN        SCPHYN
     C                   MOVEL     TPPHYA        SCPHYA
     C                   MOVEL     TPPHYC        SCPHYC
     C                   MOVEL     TPPHYS        SCPHYS
     C                   MOVEL     TPPHYZ        SCPZP1
     C                   MOVE      TPPHYZ        SCPZP2
     C                   MOVE      TPPHYR        SCPHYR
     C                   MOVEL     TPPHYP        SCPPN1
     C                   MOVE      TPPHYP        SCPPN2
     C                   MOVE      TPFXAR        SCFXAR
     C                   MOVEL     TPFAX#        SCFAX1
     C                   MOVE      TPFAX#        SCFAX2
     C*
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        XXATNM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         XXATNM
     C                   CAT       PDFSN:1       XXATNM
     C                   END
     C*
     C                   MOVEL     PDNAME        SCPHYN
     C                   MOVEL     PDADR1        SCPHYA
     C                   MOVEL     PDCTY         SCPHYC
     C                   MOVEL     PDST          SCPHYS
     C                   MOVEL     PDZIP         SCPZP1
     C                   MOVE      PDZIP         SCPZP2
     C                   MOVE      PDAREA        SCPHYR
     C                   MOVEL     PDPHN#        SCPPN1
     C                   MOVE      PDPHN#        SCPPN2
     C                   MOVE      PDFXAR        SCFXAR
     C                   MOVEL     PDFAX#        SCFAX1
     C                   MOVE      PDFAX#        SCFAX2
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*===============================================================
     C* KGDO - SUBROUTINE FOR F7 = FINANCAIL SUMMMARY
     C*===============================================================
     C*
     C*  F7=CLAIMANT FINANCIAL INFO WINDOW
     C*
     C     KGDO          BEGSR
     C*
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
     C     Q2RECF        IFEQ      'SCR03'
     C                   Z-ADD     3             #OFSCR
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* KHDO - SUBROUTINE FOR F8=BODY PART IMPAIRMENT
     C*===============================================================
     C     KHDO          BEGSR
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           QQCALL
     C                   ENDIF
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   CALL      'W0070R7'                            99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     Q2RECF        IFEQ      'SCR03'
     C                   Z-ADD     3             #OFSCR
     C                   END
     C                   END
     C*
     C                   ENDSR                                                  KHDO
     C*
     C*===============================================================
     C* KIDO - SUBROUTINE FOR F9
     C*===============================================================
     C*  F9=Diag Info
     C     KIDO          BEGSR
     C*
     C                   SELECT
     C     *IN09         WHENEQ    *ON
     C                   MOVE      '1'           ##CALL
     C                   OTHER
     C                   MOVE      ' '           ##CALL
     C                   ENDSL
     C*
     C                   CALL      'S0275R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      'WC'          ##ENV             2
     C*
     C                   ENDSR                                                  KIDO
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   MOVE      *OFF          *IN20                          INVALID SEARCH
     C                   MOVEA     '0000'        *IN(41)                           41 - 44
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
     C                   MOVE      *OFF          *IN50                                      CH
     C                   MOVE      *OFF          *IN52                                      CH
      *
      *  HOSPITAL  ___________________________________________________
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCHOSF'
     C     HFLD          OREQ      'SCHOSN'
      *
     C                   MOVE      SCHOSF        QQFEIN
     C                   MOVE      SCHOSM        QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   MOVE      *ON           *IN44                          POS CUSR
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN46                          PROTECT NAME
     C                   MOVE      QQFEIN        SCHOSF
     C                   MOVE      QQMOD         SCHOSM
     C                   MOVE      SCHOSF        XXFEIN
     C                   MOVE      SCHOSM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCHOSN
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        SCHOSA
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCHOSA
     C                   ENDIF
     C                   MOVEL(P)  PDCTY         SCHOSC
     C                   MOVEL(P)  PDST          SCHOSS
     C                   MOVEL(P)  PDZIP         SCHZP1
     C                   MOVE(P)   PDZIP         SCHZP2
     C                   MOVE(P)   PDAREA        SCHOSR
     C                   MOVEL(P)  PDPHN#        SCHPN1
     C                   MOVE(P)   PDPHN#        SCHPN2
     C                   MOVE      PDFXAR        SCFXAR
     C                   MOVEL     PDFAX#        SCFAX1
     C                   MOVE      PDFAX#        SCFAX2
     C                   END
     C                   END                                                    QQRTCD = '*'
      *
      *  INITIAL TREATMENT
      *
     C     HFLD          WHENEQ    'SCITRT'
     C                   MOVEL(P)  'INIT'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
      *
     C     XXKEY         IFEQ      *BLANK
     C                   MOVE      *ON           *IN41                          POS CUSR
     C                   MOVE      *OFF          *IN42                          POS CUSR
     C                   END
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCITRT
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCITDE
     C                   MOVE      *ON           *IN42                          POS CUSR
     C                   MOVE      *OFF          *IN41
     C                   END
     C                   END
     C     HFLD          WHENEQ    'SCAINJ'
      *
     C                   MOVEL     'EC'          ##AREA
      *
     C                   CALL      'S2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANKS       ##KC              1
     C                   PARM      *BLANKS       ##KL              1
     C                   PARM      *BLANKS       ##ICD9            5
     C                   PARM      *BLANKS       ##LOST            1
     C                   PARM                    ##AREA            2
      *
     C     ##KC          IFEQ      '1'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
     C     ##ICD9        IFNE      ' '
     C                   MOVEL(P)  ##ICD9        SCAINJ
     C*** next field position cursor indicator
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C     HFLD          WHENEQ    'SCMCOF'
      *
     C                   MOVE      SCMCOF        QQFEIN
     C                   MOVE      SCMCOF        QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *IN50                          POS CUSR
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN52
     C                   MOVE      QQFEIN        SCMCOF
     C                   END
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN20                          INVALID SEARCH
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*===============================================================
     C* KTDO - SUBROUTINE FOR F19=TREATING PHYSICIAN
     C*===============================================================
     C     KTDO          BEGSR
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      XXCASE        QQCASE            7
     C                   MOVE      *ZERO         QQSEQ#            5
     C*
     C                   MOVEL     ' '           QQCALL
     C     *IN09         IFEQ      '1'
     C                   MOVEL     '1'           QQCALL
     C                   END
     C*
     C                   CALL      'W4330R'                             99
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQSEQ#
     C                   PARM                    QQRTCD
     C*
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         DOWEQ     *OFF
     C     RCDLEN        ANDGT     281
     C     TPPROL        IFEQ      'TR'
     C                   LEAVE
     C                   ENDIF
     C     TPKY          READE     PTP01                                  60
     C                   ENDDO
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   *BLANK        SCPHYF
     C                   MOVE(P)   *BLANK        SCPHYM
     C                   MOVE(P)   *BLANK        SCPHYN
     C                   MOVE(P)   *BLANK        SCPHYA
     C                   MOVE(P)   *BLANK        SCPHYC
     C                   MOVE(P)   *BLANK        SCPHYS
     C                   MOVE(P)   *ZERO         SCPZP1
     C                   MOVE(P)   *ZERO         SCPZP2
     C                   MOVE(P)   *ZERO         SCPHYR
     C                   MOVE(P)   *ZERO         SCPPN1
     C                   MOVE(P)   *ZERO         SCPPN2
     C                   MOVE      *ZERO         SCFXAR
     C                   MOVE      *ZERO         SCFAX1
     C                   MOVE      *ZERO         SCFAX2
     C                   Z-ADD     TPPHYF        SCPHYF
     C                   Z-ADD     TPPHYM        SCPHYM
     C                   MOVE      SCPHYF        XXFEIN
     C                   MOVE      SCPHYM        XXMOD
     C                   EXSR      GETNM
     C                   END
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *IN90
     C                   MOVE      @@MODE        SVMODE            1
     C                   MOVE      'D'           @@MODE
     C     Q1PRMS        IFGE      5
     C     @@CONT        IFNE      'A'                                          CHG/DSP
     C                   MOVE      *BLANK        @@CONT
     C                   END
     C     @@CONT        IFEQ      'A'                                          ADD MODE
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *ON           *IN90
     C                   MOVE      SVMODE        @@MODE
     C                   END
     C                   END                                                    Q1PRMS >= 5
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   Z-ADD     3             #OFSCR
     C                   END
     C                   END
     C*
     C                   ENDSR                                                  KTDO
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
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
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
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
     C                   CAT       QQTUSR:1      SCMSG2
     C                   END
     C                   END
     C                   END
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
     C*
     C*  Get the EDI control record to determine if the jurisdiction state
     C*    for this claim uses EDI.
     C*
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '148'         XX148
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C  N60              MOVE      'Y'           XXEDI             1
     C   60              MOVE      'N'           XXEDI
      *
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
     C                   MOVE      CMSS#         XXSS#
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
      *  SCREEN 1
      *  PHYSICIAN
      *
     C                   MOVE(P)   *BLANK        SCPHYF
     C                   MOVE(P)   *BLANK        SCPHYM
     C                   MOVE(P)   *BLANK        SCPHYN
     C                   MOVE(P)   *BLANK        SCPHYA
     C                   MOVE(P)   *BLANK        SCPHYC
     C                   MOVE(P)   *BLANK        SCPHYS
     C                   MOVE(P)   *ZERO         SCPZP1
     C                   MOVE(P)   *ZERO         SCPZP2
     C                   MOVE(P)   *ZERO         SCPHYR
     C                   MOVE(P)   *ZERO         SCPPN1
     C                   MOVE(P)   *ZERO         SCPPN2
     C                   MOVE      *ZERO         SCFXAR
     C                   MOVE      *ZERO         SCFAX1
     C                   MOVE      *ZERO         SCFAX2
      *
     C                   MOVE(P)   *BLANK        SCHOSF
     C                   MOVE(P)   *BLANK        SCHOSM
     C                   MOVE(P)   *BLANK        SCHOSN
     C                   MOVE(P)   *BLANK        SCHOSA
     C                   MOVE(P)   *BLANK        SCHOSC
     C                   MOVE(P)   *BLANK        SCHOSS
     C                   MOVE(P)   *BLANK        SCHZP1
     C                   MOVE(P)   *BLANK        SCHZP2
     C                   MOVE(P)   *BLANK        SCHOSR
     C                   MOVE(P)   *BLANK        SCHPN1
     C                   MOVE(P)   *BLANK        SCHPN2
     C                   MOVEA     '00'          *IN(45)
      *
      * PROVIDER IS IN PROVIDER MASTER FILE
      *
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         DOWEQ     *OFF
     C     RCDLEN        ANDGT     281
     C     TPPROL        IFEQ      'TR'
     C                   LEAVE
     C                   ENDIF
     C     TPKY          READE     PTP01                                  60
     C                   ENDDO
     C     *IN60         IFEQ      *OFF
     C     TPPHYF        IFNE      *ZERO
     C                   MOVE(P)   TPPHYF        SCPHYF
     C                   MOVE(P)   TPPHYM        SCPHYM
     C                   MOVE(P)   TPPHYF        XXFEIN
     C                   MOVE(P)   TPPHYM        XXMOD
     C                   EXSR      GETNM
     C                   ENDIF
      *
      * PROVIDER IS NOT IN PROVIDER MASTER FILE
      *
     C     TPPHYN        IFNE      *BLANK
     C     TPPHYF        ANDEQ     *ZERO
     C                   MOVE      *ZERO         XXFEIN
     C                   MOVE      *ZERO         SCPHYF
     C                   MOVE      *ZERO         SCPHYM
     C                   EXSR      GETNM
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60  PTP01
      *
      *  HOSPITAL
     C     CMHOSF        IFNE      *ZERO
     C                   MOVE(P)   CMHOSF        SCHOSF
     C                   MOVE(P)   CMHOSM        SCHOSM
     C                   MOVE(P)   CMHOSF        XXFEIN
     C                   MOVE(P)   CMHOSM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCHOSN
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        SCHOSA
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCHOSA
     C                   ENDIF
     C                   MOVEL(P)  PDCTY         SCHOSC
     C                   MOVEL(P)  PDST          SCHOSS
     C                   MOVEL(P)  PDZIP         SCHZP1
     C                   MOVE(P)   PDZIP         SCHZP2
     C                   MOVE(P)   PDAREA        SCHOSR
     C                   MOVEL(P)  PDPHN#        SCHPN1
     C                   MOVE(P)   PDPHN#        SCHPN2
     C                   MOVE      *ON           *IN46                          PROTECT FIELDS
     C                   END
     C                   ENDIF
      *
     C     CMHOSN        IFNE      *BLANK
     C     CMHOSF        ANDEQ     *ZERO
     C                   MOVEL(P)  CMHOSN        SCHOSN
     C                   MOVEL(P)  CMHOSA        SCHOSA
     C                   MOVEL(P)  CMHOSC        SCHOSC
     C                   MOVEL(P)  CMHOSS        SCHOSS
     C                   MOVEL(P)  CMHOSZ        SCHZP1
     C                   MOVE(P)   CMHOSZ        SCHZP2
     C                   MOVEL(P)  CMHOSR        SCHOSR
     C                   MOVEL(P)  CMHOSP        SCHPN1
     C                   MOVE(P)   CMHOSP        SCHPN2
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SCREEN 2
     C                   MOVE      *BLANK        SCPREX
     C                   MOVEL     CMPREX        SCPREX
      *
     C                   MOVE      *BLANK        SCITRT
     C                   MOVEL     CMITRT        SCITRT
     C                   MOVE      *BLANK        SCITDE
     C                   MOVEL(P)  'INIT'        XXTBID
     C                   MOVEL(P)  CMITRT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL     GTDESC        SCITDE
     C                   ENDIF
      *  SURGERY
     C                   Z-ADD     CMSGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCSGDT
      *  REHAB
     C                   Z-ADD     CMDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCDTRE
      *  RELEASED TO WORK
     C                   Z-ADD     CMRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCRLW
     C*
     C*  Anticipated MMI
     C                   Z-ADD     CMAMMI        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCAMMI
     C*
      *  MMI
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCMMI
     C*
     C*  CMS DOI
     C                   Z-ADD     CMCMDI        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCCMDI
     C*
     C                   MOVEL     CMAINJ        SCAINJ
     C*
     C*  Date of implant
     C                   Z-ADD     CMDTIM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCDTIM
     C*
     C*  Date of first exposure
     C                   Z-ADD     CMDFE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCDFE
     C*
     C*  Date of first ingestion
     C                   Z-ADD     CMDFI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCDFI
     C*
     C*  ORM Termination Date
     C                   Z-ADD     CMORMD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCORMD
     C*
     C                   MOVEL(P)  CDHICN        SCHIC#
     C                   MOVEL(P)  CMORM         SCORM
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN85
     C                   MOVE      CMRXEL        SCRXEL
     C                   ENDIF
      *
     C                   MOVEL     *BLANK        SCIMP
     C                   Z-ADD     *ZERO         SCIPCT
     C                   MOVE      *BLANK        SCODIS
     C                   Z-ADD     *ZERO         SCYRLX
     C                   MOVE      *BLANK        SCHCAP
     C                   MOVE      *BLANK        SCDTH
     C                   Z-ADD     *ZERO         SCDOD
      *
     C                   MOVEL     CMIMP         SCIMP
     C                   Z-ADD     CMIPCT        SCIPCT
     C                   MOVEL     CMODIS        SCODIS
     C                   Z-ADD     CMYRLX        SCYRLX
     C                   MOVEL     CMHCAP        SCHCAP
     C                   MOVEL     CMDTH         SCDTH
     C*** BEG CHG *** 05/15/12 *************************************
     C******             MOVEL     CMDTH         SVDTH             1
     C*** END CHG *** 05/15/12 *************************************
     C                   MOVEL     CMIDTH        SCIDTH
     C                   MOVEL     CMPIMP        SCPIMP
     C                   Z-ADD     CMDOD         SCDOD
      *  DATE OF DEATH
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         SCDOD
      *
     C                   MOVE      *ON           *IN19                          DSPATR(PC)
     C     CLTID         IFEQ      '065'
     C     CMLTFL        ANDEQ     'I'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF
     C*
     C* SCREEN 3
     C*
     C                   Z-ADD     CMCGPH        SCCGPH
     C                   Z-ADD     CMWCPH        SCWCPH
     C                   Z-ADD     CMWDPH        SCWDPH
     C                   MOVEL     CMMCNM        SCMCNM
     C                   MOVEL     CMMCOF        SCMCOF
     C                   MOVEL     CMAURL        SCAURL
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
      *
      *  CLAIM MASTER FILE
     C                   MOVEL     SCSCL#        CMACL#
      *  SCREEN 1
      *  HOSPITAL
     C     SCHOSF        IFNE      *ZERO
     C                   MOVE      SCHOSF        XXFEIN
     C                   MOVE      SCHOSM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SCHOSF        CMHOSF
     C                   MOVE      SCHOSM        CMHOSM
     C                   MOVE      *BLANK        CMHOSN
     C                   MOVE      *BLANK        CMHOSA
     C                   MOVE      *BLANK        CMHOSC
     C                   MOVE      *BLANK        CMHOSS
     C                   MOVE      *ZERO         CMHOSZ
     C                   MOVE      *ZERO         CMHOSR
     C                   MOVE      *ZERO         CMHOSP
     C                   END
     C                   END
      *
     C     SCHOSF        IFEQ      *ZERO
     C     SCHOSF        ANDEQ     CMHOSF
     C                   MOVE      *ZERO         CMHOSF
     C                   MOVE      *ZERO         CMHOSM
     C                   MOVEL(P)  SCHOSN        CMHOSN
     C                   MOVEL(P)  SCHOSA        CMHOSA
     C                   MOVEL(P)  SCHOSC        CMHOSC
     C                   MOVEL(P)  SCHOSS        CMHOSS
     C                   MOVEL(P)  SCHZP1        CMHOSZ
     C                   MOVE      SCHZP2        CMHOSZ
     C                   MOVE      SCHOSR        CMHOSR
     C                   MOVEL     SCHPN1        CMHOSP
     C                   MOVE      SCHPN2        CMHOSP
     C                   END
      *
      * IF THERE WAS A VALID FEIN FOR A HOSPITAL
      * BUT IT WAS CHANGED TO ONE THAT IS NOT IN OUR FILE,
      *   THEN REMOVE OLD NAME & ADDRESS BUT ALLOW ENTRY
      *   OF NEW NAME & ADDRESS.
      *
     C     SCHOSF        IFEQ      *ZERO
     C     SCHOSF        ANDNE     CMHOSF
      *
     C                   MOVE      *ZERO         CMHOSF
     C                   MOVE      *ZERO         CMHOSM
     C                   MOVE      *BLANK        CMHOSN
     C                   MOVE      *BLANK        CMHOSA
     C                   MOVE      *BLANK        CMHOSC
     C                   MOVE      *BLANK        CMHOSS
     C                   MOVE      *ZERO         CMHOSZ
     C                   MOVE      *ZERO         CMHOSR
     C                   MOVE      *ZERO         CMHOSP
     C                   END                                                    SCHOSF = ZERO
      *  SCREEN 2
     C                   MOVEL     SCPREX        CMPREX
      *
     C                   MOVEL     SCITRT        CMITRT
      *
      *  SURGERY
     C                   Z-ADD     SCSGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMSGDT
      *  REHAB
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMDTRE
      *  RELEASED TO WORK
     C                   Z-ADD     SCRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMRLW
     C*  Anticipated MMI
     C                   Z-ADD     SCAMMI        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMAMMI
      *  MMI
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMMMI
     C*
     C*  CMS DOI
     C                   Z-ADD     SCCMDI        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMCMDI
     C*
     C                   MOVEL     SCAINJ        CMAINJ
     C*
     C*  Date of implant
     C                   Z-ADD     SCDTIM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMDTIM
     C*
     C*  Date of first exposure
     C                   Z-ADD     SCDFE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMDFE
     C*
     C*  Date of first ingestion
     C                   Z-ADD     SCDFI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMDFI
     C*
     C*  ORM Termination Date
     C                   Z-ADD     SCORMD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMORMD
     C*
     C                   MOVEL(P)  SCORM         CMORM
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCRXEL        CMRXEL
     C                   MOVE      *ON           *IN85
     C                   ENDIF
      *
     C                   MOVEL     SCIMP         CMIMP
     C                   MOVEL     SCIDTH        CMIDTH
     C                   MOVEL     SCPIMP        CMPIMP
     C                   Z-ADD     SCIPCT        CMIPCT
     C                   MOVEL     SCODIS        CMODIS
     C                   Z-ADD     SCYRLX        CMYRLX
     C                   MOVEL     SCHCAP        CMHCAP
      *  DATE OF DEATH
     C                   Z-ADD     SCDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         YMD --> MDY
     C                   MOVEL     SRDTO         CMDOD
      *
     C                   MOVEL     SCDTH         CMDTH
     C     SCDOD         IFNE      *ZERO
     C                   MOVE      'Y'           CMDTH
     C                   END
     C*
     C* SCREEN 3
     C*
     C                   Z-ADD     SCCGPH        CMCGPH
     C                   Z-ADD     SCWCPH        CMWCPH
     C                   Z-ADD     SCWDPH        CMWDPH
     C                   MOVEL     SCMCNM        CMMCNM
     C                   MOVEL     SCMCOF        CMMCOF
     C                   MOVEL     SCAURL        CMAURL
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
      *
      * *IN19 MUST STAY ON WHILE IN SCREEN 1
      *
     C     Q2RECF        IFNE      'SCR01'
     C                   MOVE      *OFF          *IN19                          RESET DSPATR(PC)
     C                   END
      *
     C*
     C* *IN85 MUST STAY ON WHILE IN SCREEN 2 FOR CLIENT 114
     C*
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN85
     C                   ELSE
     C                   MOVE      *OFF          *IN85
     C                   ENDIF
     C                   MOVEA     '0'           *IN(20)                           20
     C                   MOVEA     '00000'       *IN(26)                           26 - 30
     C                   MOVEA     '0'           *IN(68)                           68
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKH         CASEQ     *ON           KHDO                     90
     C     *INKI         CASEQ     *ON           KIDO                     90
      * F10 = TABLE SEARCH
     C     *INKJ         CASEQ     *ON           KJDO                     90
      * F14 = DARTS
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKT         CASEQ     *ON           KTDO                     90
     C                   ENDCS
      *
      * CONVERT ALL DATES
      * SURGERY DATE
     C                   MOVE      *BLANK        ERSGDT            1
     C     SCSGDT        IFNE      *ZERO
     C                   Z-ADD     SCSGDT        SVSGDT
     C                   Z-ADD     SCSGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         MDY --> YMD
     C                   MOVEL     SRDTO         SCSGDT
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SVSGDT        SCSGDT
     C                   MOVE      'Y'           ERSGDT
     C                   END
     C                   END
      *
      * DATE REHAB ASSIGNED
     C                   MOVE      *BLANK        ERDTRE            1
     C     SCDTRE        IFNE      *ZERO
     C                   Z-ADD     SCDTRE        SVDTRE
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         MDY --> YMD
     C                   MOVEL     SRDTO         SCDTRE
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SVDTRE        SCDTRE
     C                   MOVE      'Y'           ERDTRE
     C                   END
     C                   END
      *
      * RELEASE TO WORK DATE
     C                   MOVE      *BLANK        ERRLW             1
     C     SCRLW         IFNE      *ZERO
     C                   Z-ADD     SCRLW         SVRLW
     C                   Z-ADD     SCRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         MDY --> YMD
     C                   MOVEL     SRDTO         SCRLW
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SVRLW         SCRLW
     C                   MOVE      'Y'           ERRLW
     C                   END
     C                   END
      *
      * MMI (MAXIMUM MEDICAL IMPROVEMEMNT) DATE
     C                   MOVE      *BLANK        ERMMI             1
     C     SCMMI         IFNE      *ZERO
     C                   Z-ADD     SCMMI         SVMMI
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         MDY --> YMD
     C                   MOVEL     SRDTO         SCMMI
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SVMMI         SCMMI
     C                   MOVE      'Y'           ERMMI
     C                   END
     C                   END
      *
      * DATE OF DEATH
     C                   MOVE      *BLANK        ERDOD             1
     C     SCDOD         IFNE      *ZERO
     C                   Z-ADD     SCDOD         SVDOD
     C                   Z-ADD     SCDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         MDY --> YMD
     C                   MOVEL     SRDTO         SCDOD
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SVDOD         SCDOD
     C                   MOVE      'Y'           ERDOD
     C                   END
     C                   END
      *
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *
      *  DELETE THE CLAIM RECORD IF THE USER PRESSED F3 IN ADD MODE
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   ELSE
      *
     C     SCDTH         IFNE      SVDTH
      ***  Beg Chg  ***  05/15/12  **************************************
     C*****              CALL      'WA700R'
     C*****              PARM      CMCO#         #1CO#             3 0
     C*****              PARM      CMFND         #1FND             3 0
     C*****              PARM      CMFYR         #1FYR             3 0
     C*****              PARM      CMCASE        #1CASE            7 0
     C*****              PARM      'CLMCHG03'    #1KEY            10
     C*****              PARM      *BLANKS       ##COMM          256
      /free
          #1Co#  = CMco#;
          #1Fnd  = CMfnd;
          #1Fyr  = CMfyr;
          #1Case = CMcase;
          #1Key  = 'CLMCHG03';
          ##Comm = *blanks;
          ##Codz = svCODZ;

          WrtDrtRemarks(#1Co#:#1Fnd:#1Fyr:#1Case:#1Key:
                        ##Comm:##Codz);

      /end-free
      ***  End Chg  ***  05/15/12  **************************************
     C                   ENDIF
     C                   END
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
     C                   RETURN
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C                   Z-ADD     3             #OFSCR            1 0          # OF SCREENS
      *
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL(P)  MODEA         SCMODE
     C                   END
     C*
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
     C*
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
     C                   END
     C*
     C*     WCMC Installed determination
     C*
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
     C*
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
     C                   MOVEL     UPUSNM        QCUSER
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EXKY          CHAIN     PEX01                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
     C*
     C* ACTIVATE SCREEN 3 IF JURISDICTION STATE IS CO
     C*
     C                   MOVE      '0'           *IN49
     C     CMJRST        IFEQ      'CO'
     C                   Z-ADD     3             #OFSCR            1 0          # OF SCREENS
     C                   MOVE      '1'           *IN49
     C                   ENDIF
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
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
     C                   MOVE      *ON           *IN25
     C                   ELSE
     C                   MOVE      *OFF          *IN25
     C                   ENDIF
      ***  Beg Add  ***  05/15/12  **************************************
      /free
       SVDTH  = CMDTH;
       svCODZ = dsCODZ;
      /end-free
      ***  End Add  ***  05/15/12  **************************************
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
     C     @@RRN         CHAIN     DBRCDF                             9997
      *
      *  CHECK FOR RECORD LOCK
      *
     C     *IN97         IFEQ      *ON
     C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN99                          SKIP TO BOTTOM
     C                   END
     C*
B02  C     *IN99         IFEQ      *OFF
      *
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C*    Call API to update Managed Care Claim Master - FMCMP
     C*
     C     XXMANC        IFEQ      *ON
     C                   CLEAR                   PARMCL
     C                   EXSR      MCAPIC
     C                   ENDIF                                                  MANC ON     DATE
     C*
     C*  If EDI is active, call the EDI Log API.
     C*
     C     Q1PRMS        IFLT      5
     C     WCFL09        ANDEQ     'Y'
     C     XXEDI         ANDEQ     'Y'
     C     SCRN#         ANDEQ     #OFSCR
     C*
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
     C*
     C                   END
      *
      * CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED FROM CLM MASTER
     C     CLTID         IFEQ      '135'
      *
     C     Q1PRMS        ORGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   EXSR      CALLR
     C                   END
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   ENDSR                                                  WRITER
     C*===============================================================
     C* MCAPIC - DATA INTERFACE: MC CLAIM MASTER - FMCMP
     C*===============================================================
     C*
     C     MCAPIC        BEGSR
     C*
     C                   MOVE      CMACDT        QMACDT
     C*
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
     C*
     C                   MOVE      XXCO#         QMCO#
     C                   MOVE      *BLANKS       QMSUB#
     C                   MOVE      XXFND         QMFND
     C                   MOVE      XXFYR         QMFYR
     C                   MOVE      XXCASE        QMCASE
     C*
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
     C*
     C                   Z-ADD     DBRRN         QMRRN
     C*
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
     C                   MOVEL(P)  CMSHFT        QQWKPT                         # dependents
     C*
     C*=====================================================================
     C                   CALL      'FA003R'
     C*
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
     C                   PARM                    QMACDT            7
     C*
     C                   PARM                    QMCO#             3
     C                   PARM      *BLANK        QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
     C*
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
     C                   PARM                    QMDEDT            7
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
     C                   PARM                    QMDTRE            7
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
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
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C*
     C                   ENDSR
     C*===============================================================
