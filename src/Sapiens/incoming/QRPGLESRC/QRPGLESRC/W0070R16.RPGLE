     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      * ***************************************************************
      /TITLE W0070R16 - CLAIMS, LEGAL INFORMATION
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY LEGAL INFORMATION.   *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R - CLAIM MASTER SEARCH                               *
      *                                                               *
      *  CALLS TO:  FA003R - API to update MC claim master FMCMP      *
      *             SA001R - API to determine if MC installed         *
      *                                                               *
      *  CREATED: 2/21/97   PROGRAMMER: 47    PROJECT: 60ANC          *
      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:318 Project: SY420
      *                Mask SSN
      *
      * 05/25/12 C:282 Project: WC2109                                *
      *                Added Claim Contacts email field               *
      *                                                               *
      * 02/07/11 C:198 Project: WC1980                                *
      *                Added a window to issue a soft warning when    *
      *                Rep FEIN is entered on legal screen            *
      *                                                               *
      * 02/04/11 C:198 Project: WC1963 Priss 16                       *
      *                Fixed the issue to pull phone# when Rep FEIN   *
      *                is written w/o taking F10 on FEIN.             *
      *                                                               *
      * 01/10/11 C:53  Project: 45WIE                                 *
      *                Remove project 45WEV                           *
      *                                                               *
      * 10/05/10 C:251 Project: WC1944                                *
      *                Add Edit TPOC date :Settlement date > CMS Date *
      *                of Incident                                    *
      *                                                               *
      * 01/18/10 C:247 Project: 25WPT - Litigation_Scrn_Tracking      *
      *                        Attorney Fees (Option F17)             *
      *
      * 05/20/09 C:33 Project: WC1770                                 *
     H*                Add processing for Medicare Secondary Payer    *
      *                                                               *
      * 03/09/09 C:220 SUPPORT: 135WIB / 65638                        *
      *                Search for the Method of settlement  was not   *
      *                working properly.                              *
      *                                                               *
      * 04/09/07 C:183 Project: 135WIB                                *
      *                Call W0070R18 for F16, valid only for Client   *
      *                135.                                           *
      *                                                               *
      * 02/27/07 C:185 Project: 114WMF, Back to Base.                 *
      *                114WBC dated 02/19/01.                         *
      *                                                               *
      * 04/13/06 C:53  Project: 45WEV                                 *
      *                Change field names.                            *
      *                                                               *
      *  03/06/06 C:132 Support: WC071 51160                          *
      *                Change Code to load DSDTTM and use DSDATE      *
      *                instead of *DATE anywhere in the program.      *
      *                                                               *
      * 11/23/05 C:15  Project: WC1379                                *
      *                Removed table processing for WCAB# field.      *
      *                                                               *
      * 10/31/05 C:132 Project: WC1379                                *
      *                On search of WCAB#, turn on indicator to       *
      *                 position cursor on next screen field.         *
      *                                                               *
      * 04/01/05 C:15  Project: WC1276                                *
      *                Added screen fields on SCR02.                  *
      *                 -Date indemnity settlement check mailed.      *
      *                 -Date medical settlement check mailed.        *
      *                                                               *
      *  03/31/05 C:132 PROJECT: 132WBP                               *
      *       ADD WCAB# and I&A phone Number to SCR02 if juridiction  *
      *         state is California. Update WMLLP if present.         *
      *                                                               *
      *  03/23/04 C:058 PROJECT: WC1146                               *
      *       ADD ADDRESS INFO TO SCREEN and update WMLLP for both
      *         defense and plaintiff attorneys.
      *       ALSO protect phone as well as address if WMPDP contains
      *         the data (valid FEIN/MOD is present).
      *
      *  08/25/00 C:062 PROJECT: MC232                                *
      *                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
      *                                                               *
      * 05/04/00 C:53  Support: WC048/22758
      *                Initialize fields from screen.
      *                                                               *
      * 04/03/00 C:118 PROJECT: WC854                                 *
      *                Add parm to call of FA003R to pass WMCMP/      *
      *                CMIMP.                                         *
      *                                                               *
      * 11/01/99 C:15  Project: WC577                                 *
      *               -Changed attorney names and phone numbers to be *
      *                protected in inquiry mode.                     *
      *               -Fixed payee name comparison edit.  Screen      *
      *                field is 30A and data base field is 40A.       *
      *                                                               *
      * 10/27/99 C:61  PROJECT WC577                                  *
      *                Only update phone numbers with default from    *
      *                provider demographics when selection made from *
      *                provider search.                               *
      *                                                               *
      * 09/02/99 C:112 Support: 19256 (MC048)                         *
      *                Change QMSCL# from 10A to 25A                  *
      *                                                               *
      * 08/11/99 C:15 Support: WC048/19224                            *
      *               Fixed method of settlement edit and search.     *
      *                                                               *
      * 11/16/98 C:15 Project: WC577                                  *
      *               Changed the bar code process.  A bar code API   *
      *               is now called to return certain parameters.     *
      *                                                               *
      * 10/27/98 C:112 Support: WC095 Seq# 13829                      *
      *               Changed MVDBSC routine to incorporate change    *
      *               (see '02/25/98 C:85 Project: WC577' below)      *
      *               where 'LSST' to 'xxMS', where 'xx' is           *
      *               equal to the jurisdiction state.                *
      *                                                               *
      * 08/07/98 C:87 PROJECT: WC591                                  *
      *               Added Lump sum field and processing.            *
      *                                                               *
      * 08/04/98 C:15 Project: WC577                                  *
      *               Fixed the claim record lockout routine.         *
      *                                                               *
      * 07/23/98 C:33 SEQ#: WA048/12653                               *
      *               DO NOT CLEAR THE METHOD OF SETTLEMENT DATA      *
      *               STRUCTURE UNLESS RELOADING IT.                  *
      *                                                               *
      * 06/26/98 C:15 Support: WC095/11952                            *
      *               Fixed the code which prevents the user from     *
      *               updating the files if another user has updated  *
      *               the files since coming into this pgm.           *
      *                                                               *
      * 06/16/98 C:33 SEQ#: WA091/10403                               *
      *               USE THE CORRECT TABLE ID FOR METHOD OF SETTLEMNT*
      *                                                               *
      * 05/06/98 C:33 SEQ#: WA095/11399                               *
      *               IF THE ATTORNEY(S) FEIN IS ENTERED USE THE PHONE*
      *               # FROM THE PAYEE FILE.                          *
      *                                                               *
      * 04/07/98 C:62 Project: MC155                                  *
      *               Added additional fields to FA003R parm list.    *
      *                                                               *
      * 02/25/98 C:85 Project: WC577                                  *
      *               Changed the table file called by F10            *
      *               from 'LSST' to 'xxMS', where 'xx' is            *
      *               equal to the jurisdiction state.                *
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
      *  17 - INVALID CURSOR POSITION FOR SEARCH                      *
      *  40 - ADD MODE                                                *
      * N40 - UPDATE MODE                                             *
      *  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
      *  66 - COMMERCIAL APPLICATION INSTALLED                        *
      *  67 - SECURITY LEVEL VALID                                    *
      * N67 - SECURITY LEVEL INVALID                                  *
      *  69 - EXCESS REINSURANCE                                      *
      *  70 - BAR CODE INSTALLED                                      *
      *                                                               *
      * ***************************************************************
      *
     FW0070F16  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
      *  BAR CODE
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
      *  SYSTEM TABLE FILE
     FWTGTL     IF   E           K DISK
      *  LEGAL MASTER
     FWMLLL     UF A E           K DISK
     F                                     INFDS(DBFDS2)
      *  PAYEE DEMOGRAPHICS
     FWMPDL     IF   E           K DISK
      *  EMPLOYER DETAIL
     FWDELL     IF   E           K DISK
      *  RATE SET CODE
     FWMRSL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
     F*
      *
      *  EXCESS REINSURANCE
      *
     D OL              S             80    DIM(14)                              Option lines
     D FL              S             80    DIM(14)                              Function lines
     D VO              S              1    DIM(99)                              Valid options
     D VF              S              1    DIM(24)                              Valid functions
     D VF1             S              1    DIM(24)                              Valid functions SCR0
     D VF2             S              1    DIM(24)                              Valid functions SCR0
     D VF3             S              1    DIM(24)                              Valid functions SCR0
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Functions
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D ##OFD           DS          3500
      *
      ***** Beg Del ***** 05/25/12 ***************************************
     D*****/COPY SCOPYMT01I
      ***** End Del ***** 05/25/12 ***************************************
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
      *
     D DBFDS2          DS
     D  FLSTLL           *STATUS
     D  DBRRN2               397    400B 0
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
      *
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      *
     D                 DS                  INZ
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      * METHOD/TYPE OF SETTLEMENT
     D                 DS                  INZ
     D  XXKEY1                 1     10
     D  XXJRST                 1      2
     D  XXMTST                 3      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claims, Legal Info')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D COMAPI          C                   CONST('WA001R')
      * ================================================================
      ***** Beg Add ***** 05/25/12 ***************************************
     C/COPY SCOPYMT05C
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
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
     C*   KEY LIST FOR THE SYSTEM TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE LEGAL MASTER
     C*
     C     LLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXRTCD
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*                                                                *********
     C*
     C*   KEY LIST FOR THE SYSTEM TABLE ID FILE
     C*
     C     IDKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXTBD1            4
     C*                                                                *********
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
     C     *LIKE         DEFINE    CMACDT        XXACDT                         ACC DT
     C     *LIKE         DEFINE    CMACDT        XXDEDT                         DT ASSIGNED
     C     *LIKE         DEFINE    CMACDT        XXDTAP                         DT APPOINTED
     C     *LIKE         DEFINE    CMACDT        XXLWDT                         LAWSUIT DT
     C     *LIKE         DEFINE    CMACDT        XXSTDT                         SETTLEMENT DT
     C     *LIKE         DEFINE    CMACDT        X7FDLY                         Funding Delayed Dt
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    RSRTCD        XXRTCD
     C     *LIKE         DEFINE    LLDT          SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    LLTIME        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMDT          SVDAT2                         CURRENT DATE
     C     *LIKE         DEFINE    CMTIME        SVTIM2                         CURRENT TIME
     C     *LIKE         DEFINE    *IN60         XXAPIF                         CLAIM API FL
     C     *LIKE         DEFINE    *IN60         XXMANC                         MC INSTALLED
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *******Beg Del********05/25/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********05/25/12**********************************
     C*
     C*====================================================================
     C*  BLDLNS - Builds functions lines
     C*====================================================================
     C     BLDLNS        BEGSR
     C*
     C                   SUB       OL#1          OL#1              3 0
     C                   SUB       OL#2          OL#2              3 0
     C                   SUB       FL#1          FL#1              3 0
     C                   SUB       FL#2          FL#2              3 0
     C                   MOVEA(P)  OF            ##OF
     C                   MOVEA(P)  OFD           ##OFD
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      *BLANKS       ##WWMO
     C     @@MODE        WHENEQ    'C'
     C                   MOVE      '1'           ##WWMO
     C     @@MODE        WHENEQ    'D'
     C                   MOVE      '2'           ##WWMO
     C                   OTHER
     C                   MOVE      *BLANKS       ##WWMO
E01  C                   ENDSL
     C*
     C                   CALL      'SROPTFNC'                           99
     C                   PARM                    ##WWMO            1
     C                   PARM                    ##OF            100
     C                   PARM                    ##OFD
     C                   PARM                    OL
     C                   PARM                    FL
     C                   PARM                    VO
     C                   PARM                    VF
     C                   PARM      Q1PGM         ##PGM            10
     C*
     C                   SELECT
     C     XXSCRN        WHENEQ    1
     C                   MOVEL     FL(1)         SC1FL1
     C                   MOVEL     FL(2)         SC1FL2
     C                   MOVEA     VF            VF1
     C     XXSCRN        WHENEQ    2
     C                   MOVEL     FL(1)         SC2FL1
     C                   MOVEL     FL(2)         SC2FL2
     C                   MOVEA     VF            VF2
     C     XXSCRN        WHENEQ    3
     C                   MOVEL     FL(1)         SC3FL1
     C                   MOVEL     FL(2)         SC3FL2
     C                   MOVEA     VF            VF3
     C                   ENDSL
     C*
     C                   ENDSR
      *
      *
      *===============================================================
      * CALLR - CALL NEXT PGM - WW CLAIM TRIAL INFORMATION
      *===============================================================
      *
     C     CALLR         BEGSR
      *
     C  N09              MOVE      ' '           ##CALL                         CHANGE
     C   09              MOVE      '1'           ##CALL                         INQUIRY
      *
     C                   CALL      'W9300R'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0          WMTIP
     C                   PARM      @@RRN         ##RRN1            9 0          WMCMP
      *
     C     ##KL          IFEQ      *ON
     C                   MOVE      *OFF          OUTFLG
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL
      *
     C                   ENDSR                                                  CALLR
      *
     C*================================================================
     C*  CHKVFK - Check for valid function key being pressed
     C*================================================================
     C     CHKVFK        BEGSR
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVEA     VF1           VF
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEA     VF2           VF
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEA     VF3           VF
     C                   ENDSL
     C*
     C                   SETOFF                                         94
     C                   SUB       OF#           OF#               3 0
     C                   TESTB     '1'           Q2IAID                   99
B01  C     *IN99         IFEQ      *OFF                                         Function pressd
     C                   TESTB     '7'           Q2IAID                   99
     C   99              ADD       1             OF#
     C                   TESTB     '6'           Q2IAID                   99
     C   99              ADD       2             OF#
     C                   TESTB     '5'           Q2IAID                   99
     C   99              ADD       4             OF#
     C                   TESTB     '4'           Q2IAID                   99
     C   99              ADD       8             OF#
     C                   TESTB     '0'           Q2IAID                   99
     C   99              ADD       12            OF#
B02  C     VF(OF#)       IFEQ      *OFF
     C                   SETON                                        9094
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
      *
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
     C                   EXSR      GET#SC
     C*
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
     C                   MOVE      *OFF          *IN18                          POS CUSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
      *  LITIGATION INDICATOR MUST BE Y OR N
      *
     C     SCLITG        IFNE      'Y'
     C     SCLITG        ANDNE     'N'
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
      *
      *  VALIDATE DEFENSE ATTY FEIN/MOD
      *
     C     SCDEFA        IFNE      *ZERO
     C     SCDEFM        ORNE      *ZERO
     C                   Z-ADD     SCDEFA        XXFEIN
     C                   Z-ADD     SCDEFM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9072
     C                   LEAVE
     C*   SET INDICATOR FOR PROVIDER FILE USE(VALID FEIN/MOD)
     C                   ELSE
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C                   ENDIF
      *
      *  CAN HAVE FEIN/MOD OR NAME,  NOT BOTH
      *  ONLY CHECK IF USER INPUT BOTH FEIN/MOD AND NAME;
      *
     C                   MOVEL(P)  PDNAME        XXNM30           30
     C     *IN20         IFEQ      *OFF
     C     SCDEFA        IFNE      *ZERO
     C     SCDEFN        ANDNE     *BLANK
     C     SCDEFN        ANDNE     XXNM30
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C*
      *  DATE ASSIGNED MUST BE GREATER THAN ACCIDENT DATE
      *
     C     SCDEDT        IFNE      *ZERO
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDEDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDEDT
      *
     C     XXDEDT        IFLT      XXACDT
     C                   SETON                                        9078
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  DATE ASSIGNED NOT ALLOWED IF NAME OR FEIN NOT ENTERED
      *
     C     SCDEDT        IFNE      *ZERO
     C     SCDEFA        IFEQ      *ZERO
     C     SCDEFN        ANDEQ     *BLANK
     C                   SETON                                        9050
     C                   LEAVE
     C                   ENDIF                                                  ENDIF SCDEFA
     C                   ENDIF                                                  ENDIF SCDEDT
      *
      *  AREA CODE/PHONE NUMBER NOT ALLOWED IF NAME OR FEIN NOT ENTERED
      *
     C     SCDEFR        IFNE      *ZERO
     C     SCDFP1        ORNE      *ZERO
     C     SCDFP2        ORNE      *ZERO
     C     SCDEFA        IFEQ      *ZERO
     C     SCDEFN        ANDEQ     *BLANK
     C                   SETON                                        9048
     C                   LEAVE
     C                   ENDIF                                                  ENDIF SCDEFA
     C                   ENDIF                                                  ENDIF SCDEFR
      *
      *  VALIDATE PLANTIFF ATTY FEIN/MOD
      *
     C     SCPLAA        IFNE      *ZERO
     C     SCPLAM        ORNE      *ZERO
     C                   Z-ADD     SCPLAA        XXFEIN
     C                   Z-ADD     SCPLAM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9084
     C                   LEAVE
     C*   SET INDICATOR FOR PROVIDER FILE USE   (VALID FEIN/MOD)
     C                   ELSE
     C                   MOVE      *ON           *IN21
     C                   MOVEL(P)  PDAREA        SCPLAR
     C                   MOVEL(P)  PDPHN#        SCPLP1
     C                   MOVE(P)   PDPHN#        SCPLP2
     C                   ENDIF
     C                   ENDIF
      *
      *  CAN HAVE FEIN/MOD OR NAME,  NOT BOTH
      *  ONLY CHECK IF USER INPUT BOTH FEIN/MOD AND NAME;
      *
     C                   MOVEL(P)  PDNAME        XXNM30
     C     *IN21         IFEQ      *OFF
     C     SCPLAA        IFNE      *ZERO
     C     SCPLAN        ANDNE     *BLANK
     C     SCPLAN        ANDNE     XXNM30
     C                   MOVEL     *BLANKS       SCPLAN
     C                   SETON                                        9076
     C                   LEAVE
     C                   ENDIF
     C     SCPLAA        IFNE      *ZERO
     C     SCRFNM        ANDNE     *BLANK
     C     SCRFNM        ANDNE     PDRFNM
     C                   SETON                                        81  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  DATE OF REPRESENTATION MUST BE GREATER THAN ACCIDENT DATE
      *
     C     SCDTAP        IFNE      *ZERO
     C                   Z-ADD     SCDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTAP
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDTAP
      *
     C     XXDTAP        IFLT      XXACDT
     C                   SETON                                        9075
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  DATE ASSIGNED NOT ALLOWED IF NAME OR FEIN NOT ENTERED
      *
     C     SCDTAP        IFNE      *ZERO
     C     SCPLAA        IFEQ      *ZERO
     C     SCPLAN        ANDEQ     *BLANK
     C                   SETON                                        9051
     C                   LEAVE
     C                   ENDIF                                                  ENDIF SCPLAA
     C                   ENDIF                                                  ENDIF SCDTAP
     C*
     C*  Validate Injured party rep indicator
     C*
     C     SCCLRI        IFNE      *BLANKS
     C     SCPLAA        IFEQ      *ZERO
     C     SCPLAN        ANDEQ     *BLANK
     C                   SETON                                        82  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Injured party rep indicator not allowed if name or FEIN not entered
     C*
     C     SCCLRI        IFNE      *BLANKS
     C                   MOVEL(P)  'CLRI'        XXTBID
     C                   MOVEL(P)  SCCLRI        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        83  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  AREA CODE/PHONE NUMBER NOT ALLOWED IF NAME OR FEIN NOT ENTERED
      *
     C     SCPLAR        IFNE      *ZERO
     C     SCPLP1        ORNE      *ZERO
     C     SCPLP2        ORNE      *ZERO
     C     SCPLAA        IFEQ      *ZERO
     C     SCPLAN        ANDEQ     *BLANK
     C                   SETON                                        9049
     C                   LEAVE
     C                   ENDIF                                                  ENDIF SCPLAA
     C                   ENDIF                                                  ENDIF SCPLAR
      *
     C     CMMBS         IFEQ      'Y'
     C     SCCLRI        ANDNE     ' '
     C     SCPLAA        ANDEQ     0
     C*
     C                   EXFMT     WDW01
     C                   SELECT
     C     *INKG         WHENEQ    '1'
     C                   LEAVE
     C     *INKL         WHENEQ    '1'
     C                   MOVE      '0'           *INKL
     C                   MOVE      '1'           *IN90
     C                   ITER
     C                   OTHER
     C                   MOVE      '1'           *IN90
     C                   ITER
     C                   ENDSL                                                  ENDSL SELEC
     C*
     C                   ENDIF                                                  SCPLAA = 0
     C*
     C***        === REMAINING FIELDS AND EDITS MOVED TO SCR02 ===
     C*
     C                   MOVE      *ON           *IN22                          POS CUSR
      *
     C                   ENDDO
     C*
     C     *INKG         IFEQ      *ON
     C                   MOVE      '0'           *INKG
     C                   EXSR      CALLR
     C                   ENDIF                                                            *INKG = *O
     C*  If another user has updated this claim, send an error message.
     C*
     C     LLKY          CHAIN(N)  PLL01                              99
     C*
     C  N99SVDATE        IFNE      LLDT
     C     SVTIME        ORNE      LLTIME
     C*
     C                   CALL      'WA017R'
     C                   PARM                    LLUSER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK1
      *
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C                   MOVE      *OFF          *IN18                          POS CUSR
     C*
     C                   DO
     C*
     C*****************************************************************
     C*
     C*        === THESE FIELD EDITS MOVED FROM SCR01 ===
     C*
     C*****************************************************************
      *
      *  FRAUDULENT INDICATOR MAY NOT BE BLANK
      *
     C     SCFRAU        IFEQ      *BLANK
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
      *
      *  VALIDATE FRAUDULENT INDICATOR
      *
     C                   MOVEL(P)  'FRAU'        XXTBID
     C                   MOVEL(P)  SCFRAU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9080
     C                   LEAVE
     C                   ENDIF
      *
      *  SUIT DATE MUST BE GREATER THAN ACCIDENT DATE
      *
     C*
     C     SCLWDT        IFNE      *ZERO
     C                   Z-ADD     SCLWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCLWDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCLWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXLWDT
      *
     C     XXLWDT        IFLT      XXACDT
     C                   SETON                                        9081
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  VALIDATE METHOD OF SETTLEMENT
      *
     C     SCMTST        IFNE      *BLANK
      *
      *  JURISDICTION STATE CAN NOT BE CHANGED;
      *  MUST GET THE CORRECT JURISDICTION STATE FROM THE SEARCH
      *
      *
     C                   MOVE      *BLANK        XXRTCD
     C                   MOVE      *BLANK        XXJRST
     C                   MOVE      *BLANK        XXKEY1
     C                   MOVE      *BLANK        XXKEY
     C     CMJRST        IFEQ      *BLANK
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXJRST
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     CMJRST        XXJRST
     C                   ENDIF
      *
     C                   MOVEL(P)  SCMTST        XXMTST
     C                   MOVEL(P)  SCMTST        XXKEY
     C*
     C                   MOVEL(P)  XXJRST        XXTBD1
     C                   MOVE      'MS'          XXTBD1
     C*
     C     IDKEY         CHAIN     PGI01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       XXTBD1
     C                   MOVE      'MS'          XXTBD1
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   MOVEL     XXTBD1        XXTBID
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9082
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      *  SETTLEMENT DATE MUST BE GREATER THAN ACCIDENT DATE
      *
     C     SCSTDT        IFNE      *ZERO
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCSTDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXSTDT
      *
     C     XXSTDT        IFLT      XXACDT
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF
     C*
     C*** Settlement date must be greater than CMS Date of Incident
     C*** if Medicare Secondary Payer is installed
     C*
     C     1             SUBST     SYFL12:2      XXFL12            1
     C     XXFL12        IFEQ      'Y'
     C*
     C     XXSTDT        IFLE      CMCMDI
     C                   SETON                                        9065
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*  If entered, the Date Indemnity settlement check mailed must
     C*  be > the accident date.
     C*
     C     SCISCM        IFNE      *ZERO
      *
     C                   Z-ADD     SCISCM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCISCM
     C     SRERR         IFEQ      '1'
     C                   SETON                                        3590
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCISCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXSTDT
      *
     C     XXSTDT        IFLT      XXACDT
     C                   SETON                                        3690
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  If entered, the Date Medical settlement check mailed must
     C*  be > the accident date.
     C*
     C     SCMSCM        IFNE      *ZERO
      *
     C                   Z-ADD     SCMSCM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCMSCM
     C     SRERR         IFEQ      '1'
     C                   SETON                                        3790
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     SCMSCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXSTDT
      *
     C     XXSTDT        IFLT      XXACDT
     C                   SETON                                        3890
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  LUMP SUM INDICATOR MUST BE 'Y' OR ' '
     C*
     C     SCLUMP        IFNE      'Y'
     C     SCLUMP        ANDNE     ' '
     C                   SETON                                        9089
     C                   LEAVE
     C                   ENDIF
      *
      *
      *  VALIDATE WCAB NUMBER
      *
     C     *IN45         IFEQ      *ON
     C*
     C     SCIAAR        IFGT      *ZEROS
     C     SCIAP1        ANDEQ     *ZEROS
     C                   SETON                                        9033
     C                   ENDIF                                                  SCIAAR <> ZEROS
     C     SCIAAR        IFEQ      *ZEROS
     C     SCIAP1        ANDNE     *ZEROS
     C                   SETON                                        9032
     C                   ENDIF                                                  SCIAAR <> ZEROS
     C     SCIAP1        IFGT      *ZEROS
     C     SCIAP2        ANDEQ     *ZEROS
     C                   SETON                                        9034
     C                   ENDIF                                                  SCIAAR <> ZEROS
     C  N90SCIAP1        IFEQ      *ZEROS
     C     SCIAP2        ANDGT     *ZEROS
     C                   SETON                                        9033
     C                   ENDIF                                                  SCIAAR <> ZEROS
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  *IN90 = *ON
     C                   ENDIF                                                  *IN45 = *ON
     C*
     C*  Product liability indicator not allowed if name or FEIN not entered
     C*
     C     SCPRLI        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'PRLI'        XXTBID
     C                   MOVEL(P)  SCPRLI        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        71  90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Product liability information will not be accessible on SCR03
     C*   if the value is changed
     C     SCPRGN        IFNE      *BLANKS
     C                   SELECT
     C     SVPRLI        WHENEQ    '2'
     C     SVPRLI        OREQ      '3'
     C     SCPRLI        IFNE      '2'
     C     SCPRLI        ANDNE     '3'
     C                   SETON                                        74  90
     C                   ENDIF
     C                   MOVEL(P)  SCPRLI        SVPRLI
     C                   ENDSL
     C                   ENDIF
     C*
     C*  Validate TPOC Start Date
     C*
     C     SCFDLY        IFNE      *ZERO
     C                   Z-ADD     SCFDLY        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCFDLY
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72  90
     C                   LEAVE
     C                   ENDIF
     C                   Z-ADD     SCFDLY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         X7FDLY
     C     X7FDLY        IFLT      XXACDT
     C                   SETON                                        74  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*
     C*
     C*  If another user has updated this claim, send an error message.
     C*
     C     LLKY          CHAIN(N)  PLL01                              99
     C*
     C  N99SVDATE        IFNE      LLDT
     C     SVTIME        ORNE      LLTIME
     C*
     C                   CALL      'WA017R'
     C                   PARM                    LLUSER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   EXSR      GET#SC
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C* Generic name required
     C  N90SCPRGN        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C* Brand name required
     C  N90SCPRBN        IFEQ      *BLANKS
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C* Brand name required
     C  N90SCPRMN        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C* Alleged harm required
     C  N90SCPRAH        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C* Clear the values if the liability indicator does not allow for
     C*  them
     C     SCPRLI        IFNE      '2'
     C     SCPRLI        ANDNE     '3'
     C                   CLEAR                   SCPRGN
     C                   CLEAR                   SCPRBN
     C                   CLEAR                   SCPRMN
     C                   CLEAR                   SCPRAH
     C                   ENDIF
     C*
     C*  If another user has updated this claim, send an error message.
     C*
     C     LLKY          CHAIN(N)  PLL01                              99
     C*
     C  N99SVDATE        IFNE      LLDT
     C     SVTIME        ORNE      LLTIME
     C*
     C                   CALL      'WA017R'
     C                   PARM                    LLUSER
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
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
     C*================================================================
     C* GET#SC - Get the # of screens
     C*================================================================
     C     GET#SC        BEGSR
     C*
     C                   MOVE      *OFF          *IN39
     C*
     C* Set # of Screen according to Product Liabity Indicator
     C                   SELECT
     C     SCPRLI        WHENEQ    '2'
     C     SCPRLI        OREQ      '3'
     C                   Z-ADD     3             #OFSCR
     C     SCPRGN        WHENNE    *BLANKS
     C                   Z-ADD     3             #OFSCR
     C     @@MODE        IFNE      'D'
     C                   MOVE      *ON           *IN39
     C                   ENDIF
     C                   OTHER
     C                   Z-ADD     2             #OFSCR
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* KGDO - SUBROUTINE FOR F7
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
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* KHDO - SUBROUTINE FOR F8
     C*====================================================================
     C*  F8=Multiple Settlement
     C     KHDO          BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      *BLANKS       ##CALL
     C     @@MODE        WHENEQ    'C'
     C                   MOVE      *BLANKS       ##CALL
     C     @@MODE        WHENEQ    'D'
     C                   MOVE      '1'           ##CALL
     C                   OTHER
     C                   MOVE      '1'           ##CALL
E01  C                   ENDSL
     C*
     C                   CALL      'W0716R'
     C                   PARM                    ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
      *
     C                   MOVEA     '00000000'    *IN(22)                        22 - 29
     C                   MOVEA     '0'           *IN(30)                        RESET DSPATR(PC)
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
      *
      *  DEFENSE ATTY NUMERIC SEARCH
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCDEFA'
     C                   CALL      'W0030R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *BLANKS
     C                   MOVE      *ON           *IN27                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN20                          PROTECT NAME
     C                   MOVE      *ON           *IN19                          POSITION CURSOR
     C                   MOVEL     QQFEIN        SCDEFA
     C                   MOVEL     QQMOD         SCDEFM
     C                   MOVEL     QQFEIN        XXFEIN                         MOVE FOR CHAIN
     C                   MOVEL     QQMOD         XXMOD                          MOVE FOR CHAIN
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCDEFN
     C                   MOVEL(P)  PDAREA        SCDEFR
     C                   MOVEL(P)  PDPHN#        SCDFP1
     C                   MOVE(P)   PDPHN#        SCDFP2
     C                   MOVE      *ON           *IN20
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCDEFN
     C                   MOVEL(P)  PDADR2        SCDAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCDAD1
     C                   MOVEL(P)  PDADR2        SCDAD2
     C                   ENDIF
     C                   MOVEL     PDCTY         SCDCTY
     C                   MOVEL     PDST          SCDST
     C                   MOVEL     PDZIP         SCDZP1
     C                   MOVE      PDZIP         SCDZP2
     C                   END
     C                   END
      *
      *  DEFENSE ATTY ALPHA SEARCH
      *
     C     HFLD          WHENEQ    'SCDEFN'
     C                   CALL      'W0030R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *BLANKS
     C                   MOVE      *ON           *IN28                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN19                          POSITION CURSOR
     C                   MOVEL     QQFEIN        SCDEFA
     C                   MOVEL     QQMOD         SCDEFM
     C                   MOVEL     QQFEIN        XXFEIN                         MOVE FOR CHAIN
     C                   MOVEL     QQMOD         XXMOD                          MOVE FOR CHAIN
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCDEFN
     C                   MOVEL(P)  PDAREA        SCDEFR
     C                   MOVEL(P)  PDPHN#        SCDFP1
     C                   MOVE(P)   PDPHN#        SCDFP2
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCDEFN
     C                   MOVEL(P)  PDADR2        SCDAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCDAD1
     C                   MOVEL(P)  PDADR2        SCDAD2
     C                   ENDIF
     C                   MOVEL     PDCTY         SCDCTY
     C                   MOVEL     PDST          SCDST
     C                   MOVEL     PDZIP         SCDZP1
     C                   MOVE      PDZIP         SCDZP2
     C                   END
     C                   END
      *
      *  PLANTIFF ATTY NUMERIC SEARCH
      *
     C     HFLD          WHENEQ    'SCPLAA'
     C                   CALL      'W0030R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *BLANKS
     C                   MOVE      *ON           *IN26                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN21                          PROTECT PLA NAME
     C                   MOVE      *ON           *IN30                          POSITION CURSOR
     C                   MOVEL     QQFEIN        SCPLAA
     C                   MOVEL     QQMOD         SCPLAM
     C                   MOVEL     QQFEIN        XXFEIN                         MOVE FOR CHAIN
     C                   MOVEL     QQMOD         XXMOD                          MOVE FOR CHAIN
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCPLAN
     C                   MOVEL(P)  PDRFNM        SCRFNM
     C                   MOVEL(P)  PDAREA        SCPLAR
     C                   MOVEL(P)  PDPHN#        SCPLP1
     C                   MOVE(P)   PDPHN#        SCPLP2
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCPLAN
     C                   MOVEL(P)  PDADR2        SCPAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCPAD1
     C                   MOVEL(P)  PDADR2        SCPAD2
     C                   ENDIF                                                  BA1 = Y
     C                   MOVEL     PDCTY         SCPCTY
     C                   MOVEL     PDST          SCPST
     C                   MOVEL     PDZIP         SCPZP1
     C                   MOVE      PDZIP         SCPZP2
     C                   END
     C                   END
      *
      *  PLANTIFF ATTY ALPHA SEARCH
      *
     C     HFLD          WHENEQ    'SCPLAN'
     C                   CALL      'W0030R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *BLANKS
     C                   MOVE      *ON           *IN29                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN30                          POSITION CURSOR
     C                   MOVEL     QQFEIN        SCPLAA
     C                   MOVEL     QQMOD         SCPLAM
     C                   MOVEL     QQFEIN        XXFEIN                         MOVE FOR CHAIN
     C                   MOVEL     QQMOD         XXMOD                          MOVE FOR CHAIN
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCPLAN
     C                   MOVEL(P)  PDRFNM        SCRFNM
     C                   MOVEL(P)  PDAREA        SCPLAR
     C                   MOVEL(P)  PDPHN#        SCPLP1
     C                   MOVE(P)   PDPHN#        SCPLP2
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCPLAN
     C                   MOVEL(P)  PDADR2        SCPAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCPAD1
     C                   MOVEL(P)  PDADR2        SCPAD2
     C                   ENDIF                                                  BA1 = Y
     C                   MOVEL     PDCTY         SCPCTY
     C                   MOVEL     PDST          SCPST
     C                   MOVEL     PDZIP         SCPZP1
     C                   MOVE      PDZIP         SCPZP2
     C                   END
     C                   END
      *
      *  FRAUDULENT INDICATOR
      *
     C     HFLD          WHENEQ    'SCFRAU'
     C                   MOVEL(P)  'FRAU'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN22                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCFRAU
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCFRAD
     C                   MOVE      *ON           *IN23                          POS CUSR
     C                   END
     C                   END
      *
      *  METHOD OF SETTLEMENT
      *
     C     HFLD          WHENEQ    'SCMTST'
      *
     C*
     C*
     C*
     C                   MOVEL(P)  XXJRST        XXTBD1
     C                   MOVE      'MS'          XXTBD1
     C*
     C     IDKEY         CHAIN     PGI01                              60
     C*                                                                *********
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       XXTBD1
     C                   MOVE      'MS'          XXTBD1
     C                   ENDIF                                                  *IN60=ON
     C*                                                                *********
     C                   MOVEL     XXTBD1        XXTBID
     C*                                                                *********
     C*
     C*
     C                   MOVE      *ON           *IN24                          POS CUSR
     C*** BEGIN ADD *** 08/11/99 **************************************
     C                   MOVEL     SCMTST        XXKEY
     C*** END ADD ***** 08/11/99 **************************************
     C*                                                                *********
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM      *BLANK        QQRTCD
     C*                                                                *********
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTCD=A
     C*                                                                *********
     C     QQRTCD        IFEQ      '*'
     C     XXKEY         ANDNE     *BLANK
     C                   MOVEL(P)  XXKEY         SCMTST
     C*                                                                *********
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCMTSD
     C                   MOVE      *OFF          *IN24                          POS CUSR
     C                   MOVE      *ON           *IN25                          POS CUSR
     C                   ENDIF                                                  *IN60=OFF
     C*                                                                *********
     C                   ENDIF                                                  QQRTCD=*
     C*                                                                *********
      *
      *
      *  WCAB NUMBER
      *
     C* Injured party representative indicator
     C     HFLD          WHENEQ    'SCCLRI'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'CLRI'        XXTBID
     C                   PARM      *BLANK        XXKEY
     C                   PARM                    QQRTCD
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTCD        WHENEQ    'B'
     C     XXKEY         OREQ      *BLANK
     C                   SETON                                        83  85
     C     QQRTCD        WHENEQ    '*'
     C                   SETON                                        49  85
     C                   MOVEL(P)  XXKEY         SCCLRI
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCCLRD
     C                   ENDSL
     C*
     C* Product liability indicator
     C     HFLD          WHENEQ    'SCPRLI'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'PRLI'        XXTBID
     C                   PARM      *BLANK        XXKEY
     C                   PARM                    QQRTCD
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTCD        WHENEQ    'B'
     C     XXKEY         OREQ      *BLANK
     C                   SETON                                        71  73
     C     QQRTCD        WHENEQ    '*'
     C                   SETON                                        72  73
     C                   MOVEL(P)  XXKEY         SCPRLI
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCPRLD
     C                   ENDSL
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN17                          INVALID POS
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
     C                   MOVE      ##CO#         QCCO#
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
     C*===============================================================
     C* KPDO - SUBROUTINE FOR F16 = THIRD PARTY ATTNY
     C*===============================================================
     C*
     C     KQDO          BEGSR
     C*
     C                   CALL      'W0070R18'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
      *
     C     LLKY          CHAIN(N)  PLL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LLTIME        SVTIME
     C                   Z-ADD     LLDT          SVDATE
     C                   ENDIF
      *
     C*
     C                   ENDSR                                                  KQDO
     C*===============================================================
     C* KRDO - SUBROUTINE FOR F17 = ATTORNEY ORDER
     C*===============================================================
     C*
     C     KRDO          BEGSR
     C*
     C                   MOVE      *ZERO         QQSEQ#            5
     C                   CALL      'W4343R'
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQSEQ#
     C                   PARM                    QQRTCD
     C*
     C                   ENDSR                                                  KRDO
      *
     C*====================================================================
     C* LDSCFK - Load the screen function keys
     C*====================================================================
     C     LDSCFK        BEGSR
     C*
     C                   MOVE      '2'           XXOPFN            1
     C*
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      'NYY'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '08'          XXNMBR
     C                   MOVE      'YYY'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '10'          XXNMBR
     C                   MOVE      'YYN'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '16'          XXNMBR            2
     C                   MOVE      'YYY'         XXACD             3
     C     *IN41         IFEQ      *OFF
     C                   MOVE      'NNN'         XXACD
     C                   ENDIF
     C                   EXSR      UPDOFA
     C                   MOVE      '17'          XXNMBR            2
     C                   MOVE      'YYY'         XXACD             3
     C     *IN42         IFEQ      *OFF
     C                   MOVE      'NNN'         XXACD
     C                   ENDIF
     C                   EXSR      UPDOFA
     C                   Z-ADD     1             XXSCRN            1 0
     C*
     C                   EXSR      BLDLNS
     C                   Z-ADD     2             XXSCRN
     C                   EXSR      BLDLNS
     C*
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      'NNN'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '08'          XXNMBR
     C                   MOVE      'NNN'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '10'          XXNMBR
     C                   MOVE      'NNN'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '16'          XXNMBR
     C                   MOVE      'NNN'         XXACD
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'NNN'         XXACD
     C                   EXSR      UPDOFA
     C                   Z-ADD     3             XXSCRN
     C                   EXSR      BLDLNS
     C*
     C                   ENDSR
     C*
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
     C                   Z-ADD     CMTIME        SVTIM2
     C                   Z-ADD     CMDT          SVDAT2
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
      * GET SYSTEM CONTROL FILE
      *
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
      *
      * BAR CODE INSTALLED
      *
     C*                    MOVE *BLANK    SCMSG2
     C     SYBCIF        IFEQ      'Y'
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
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
     C                   MOVE(P)   *BLANK        SCFNAM
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
      *
     C                   END                                                    CMSS# <> BLANK
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        XXACDT
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
      *
     C                   MOVEL(P)  'N'           SCLITG
     C                   Z-ADD     *ZERO         SCDEFA
     C                   Z-ADD     *ZERO         SCDEFM
     C                   MOVE      *BLANK        SCDEFN
     C                   Z-ADD     *ZERO         SCDEDT
     C                   MOVE      *BLANK        SCDEFR
     C                   MOVE      *BLANK        SCDFP1
     C                   MOVE      *BLANK        SCDFP2
     C                   Z-ADD     *ZERO         SCPLAA
     C                   Z-ADD     *ZERO         SCPLAM
     C                   MOVE      *BLANK        SCPLAN
     C                   MOVE(P)   *BLANKS       SCRFNM
     C                   Z-ADD     *ZERO         SCDTAP
     C                   MOVE      *BLANK        SCPLAR
     C                   MOVE      *BLANK        SCPLP1
     C                   MOVE      *BLANK        SCPLP2
     C                   MOVE      *BLANK        SCFRAU
     C                   MOVE      *BLANK        SCFRAD
     C                   Z-ADD     *ZERO         SCLWDT
     C                   Z-ADD     *ZERO         SCSTDT
     C                   MOVE      *BLANK        SCMTST
     C                   MOVE      *BLANK        SCMTSD
     C                   Z-ADD     *ZERO         SCSAMT
     C                   MOVE      *BLANK        SCLUMP
     C                   MOVEL     *BLANK        SCDEFN
     C                   MOVEL     *BLANK        SCDAD1
     C                   MOVEL     *BLANK        SCDAD2
     C                   MOVEL     *BLANK        SCDCTY
     C                   MOVEL     *BLANK        SCDST
     C                   MOVE      *ZERO         SCDZP1
     C                   MOVE      *ZERO         SCDZP2
     C                   MOVEL     *BLANK        SCPLAN
     C                   MOVE(P)   *BLANKS       SCRFNM
     C                   MOVEL     *BLANK        SCPAD1
     C                   MOVEL     *BLANK        SCPAD2
     C                   MOVEL     *BLANK        SCPCTY
     C                   MOVEL     *BLANK        SCPST
     C                   MOVE      *ZERO         SCPZP1
     C                   MOVE      *ZERO         SCPZP2
     C                   MOVE      '00'          SCFRAU
     C                   MOVE      *ZERO         SCISCM
     C                   MOVE      *ZERO         SCMSCM
      *
     C                   Z-ADD     0             SVTIME
     C                   Z-ADD     0             SVDATE
     C     LLKY          CHAIN(N)  PLL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LLTIME        SVTIME
     C                   Z-ADD     LLDT          SVDATE
      *
     C     LLLITG        IFNE      *BLANK
     C                   MOVEL(P)  LLLITG        SCLITG
     C                   ENDIF
      *
     C                   Z-ADD     LLDEFA        SCDEFA
     C                   Z-ADD     LLDEFM        SCDEFM
      *
      *   CHAIN TO PAYEE DEMOGRAPHICS FILE TO FIND NAME ASSOCIATED
      *   WITH DEFENSE ATTY FEIN/MOD
      *
     C*          XXFEIN    IFNE *ZERO
     C*          XXMOD     ORNE *ZERO
     C                   Z-ADD     SCDEFA        XXFEIN
     C                   Z-ADD     SCDEFM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C*                    ELSE                                  XXFEIN
     C*                    ENDIF                           ENDIF XXFEIN
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        SCDEFN
     C                   MOVE      *ON           *IN20
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCDEFN
     C                   MOVEL(P)  PDADR2        SCDAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCDAD1
     C                   MOVEL(P)  PDADR2        SCDAD2
     C                   ENDIF
     C                   MOVEL     PDCTY         SCDCTY
     C                   MOVEL     PDST          SCDST
     C                   MOVEL     PDZIP         SCDZP1
     C                   MOVE      PDZIP         SCDZP2
      *
     C                   ELSE
     C                   MOVEL(P)  LLDEFN        SCDEFN
     C                   MOVEL(P)  LLDAD1        SCDAD1
     C                   MOVEL(P)  LLDAD2        SCDAD2
     C                   MOVEL(P)  LLDCTY        SCDCTY
     C                   MOVEL     LLDST         SCDST
     C                   MOVEL     LLDZIP        SCDZP1
     C                   Z-ADD     LLDZIP        SCDZP2
     C     LLFRAU        IFEQ      *BLANK
     C                   MOVEL     '00'          LLFRAU
     C                   ENDIF
     C                   ENDIF                                                  ENDIF *IN60
      *
     C     LLDEFN        IFEQ      *BLANK
     C                   ENDIF
      *
     C                   Z-ADD     LLDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDEDT
      *
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scLema = LLemdd;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   Z-ADD     LLPLAA        SCPLAA
     C                   Z-ADD     LLPLAM        SCPLAM
      *
      *   CHAIN TO PAYEE DEMOGRAPHICS FILE TO FIND NAME ASSOCIATED
      *   WITH PLAINTIFF ATTY FEIN/MOD
      *
     C*          XXFEIN    IFNE *ZERO
     C*          XXMOD     ORNE *ZERO
     C     SCPLAA        IFEQ      *ZERO
     C     SCPLAM        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN60
     C                   ELSE                                                   ENDIF XXFEIN
     C                   Z-ADD     SCPLAA        XXFEIN
     C                   Z-ADD     SCPLAM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C*                    ENDIF                           ENDIF SCPLAA
     C                   ENDIF                                                  ENDIF XXFEIN
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        SCPLAN
     C                   MOVEL(P)  PDRFNM        SCRFNM
     C                   MOVE      *ON           *IN21
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCPLAN
     C                   MOVEL(P)  PDADR2        SCPAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCPAD1
     C                   MOVEL(P)  PDADR2        SCPAD2
     C                   ENDIF                                                  BA1 = Y
     C                   MOVEL     PDCTY         SCPCTY
     C                   MOVEL     PDST          SCPST
     C                   MOVEL     PDZIP         SCPZP1
     C                   MOVE      PDZIP         SCPZP2
      *
     C                   ELSE
     C                   MOVEL(P)  LLPLAN        SCPLAN
     C                   MOVEL(P)  LLRFNM        SCRFNM
     C                   MOVE      *OFF          *IN21
     C                   MOVEL(P)  LLPAD1        SCPAD1
     C                   MOVEL(P)  LLPAD2        SCPAD2
     C                   MOVEL(P)  LLPCTY        SCPCTY
     C                   MOVEL     LLPST         SCPST
     C                   MOVEL     LLPZIP        SCPZP1
     C                   Z-ADD     LLPZIP        SCPZP2
     C*
     C                   ENDIF                                                  ENDIF *IN60
      *
     C     LLPLAN        IFEQ      *BLANK
     C                   ENDIF
      *
     C                   Z-ADD     LLDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDTAP
      *
      *
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scLemd = LLemdt;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   MOVEL(P)  LLFRAU        SCFRAU
     C                   MOVEL(P)  'FRAU'        XXTBID
     C                   MOVEL(P)  LLFRAU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCFRAD
     C                   ENDIF
      *
     C                   Z-ADD     LLLWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCLWDT
      *
     C                   Z-ADD     LLSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCSTDT
     C                   Z-ADD     LLISCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCISCM
     C                   Z-ADD     LLMSCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCMSCM
     C                   MOVE(P)   LLLUMP        SCLUMP
      *
      * GET THE JURISDICTION STATE FOR THE METHOD OF SETTLEMENT
      *
     C                   MOVE      *BLANK        XXRTCD
     C                   MOVE      *BLANK        XXJRST
     C                   MOVE      *BLANK        XXKEY1
     C     CMJRST        IFEQ      *BLANK
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXJRST
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     CMJRST        XXJRST
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN45
     C     XXJRST        IFEQ      'CA'
     C                   MOVE      *ON           *IN45
     C                   ENDIF
      *
      * SET UP WCAB# AND PHONE #
     C     *IN45         IFEQ      *ON
      *
      *
     C                   MOVEL     LLWCAB        SCWCAB
     C                   MOVEL(P)  LLIAAR        SCIAAR
     C                   MOVEL(P)  LLIAPH        SCIAP1
     C                   MOVE(P)   LLIAPH        SCIAP2
      *
     C                   ENDIF                                                  END *IN45 = *ON
      *
     C                   MOVEL(P)  LLMTST        XXKEY
     C                   MOVEL     LLMTST        SCMTST
     C                   MOVEL(P)  XXJRST        XXTBD1
     C                   MOVE      'MS'          XXTBD1
     C*
     C     IDKEY         CHAIN     PGI01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       XXTBD1
     C                   MOVE      'MS'          XXTBD1
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   MOVEL     XXTBD1        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMTSD
     C                   ENDIF
      *
     C                   Z-ADD     LLSAMT        SCSAMT
      *
      *  Defense attorney
      *
     C                   MOVEL(P)  LLDEFR        SCDEFR
     C                   MOVEL(P)  LLDEFP        SCDFP1
     C                   MOVE(P)   LLDEFP        SCDFP2
      *
      *  Plaintiff attorney
      *
     C                   MOVEL(P)  LLPLAR        SCPLAR
     C                   MOVEL(P)  LLPLAP        SCPLP1
     C                   MOVE(P)   LLPLAP        SCPLP2
      *
     C*
     C                   MOVEL(P)  LLRFNM        SCRFNM
     C                   MOVEL(P)  LLPRGN        SCPRGN
     C                   MOVEL(P)  LLPRBN        SCPRBN
     C                   MOVEL(P)  LLPRMN        SCPRMN
     C                   MOVEL(P)  LLPRAH        SCPRAH
     C*
     C                   CLEAR                   SCCLRD
     C                   MOVEL(P)  LLCLRI        SCCLRI
     C                   MOVEL(P)  'CLRI'        XXTBID
     C                   MOVEL(P)  LLCLRI        XXKEY
     C                   CLEAR                   GTDESC
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCCLRD
     C*
     C     *LIKE         DEFINE    SCPRLI        SVPRLI
     C                   MOVEL(P)  LLPRLI        SVPRLI
     C                   MOVEL(P)  LLPRLI        SCPRLI
     C                   MOVEL(P)  'PRLI'        XXTBID
     C                   MOVEL(P)  LLPRLI        XXKEY
     C                   CLEAR                   GTDESC
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCPRLD
     C*
     C                   Z-ADD     LLFDLY        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCFDLY
     C                   ENDIF                                                  *IN60
     C*
     C                   EXSR      GET#SC
      *
     C                   MOVE      *ON           *IN18                          POS CUSR
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C*    Set flag to update MC if SCL# has changed.
     C*
     C                   MOVE      *OFF          XXAPIF
     C     XXMANC        IFEQ      *ON
     C     SCSCL#        IFNE      CMACL#
     C                   MOVE      *ON           XXAPIF
     C                   ENDIF
     C                   ENDIF
     C*
      *
      *  CLAIM MASTER FILE
     C                   MOVEL(P)  SCSCL#        CMACL#
      *
     C                   MOVEL(P)  SCLITG        LLLITG
     C                   Z-ADD     SCDEFA        LLDEFA
     C                   Z-ADD     SCDEFM        LLDEFM
     C                   MOVEL(P)  SCDEFN        LLDEFN
     C                   MOVE      SCLUMP        LLLUMP
      *
      *  DEFENSE ATTORNEY NAME CLEARED IF FEIN IS NOT ZERO
      *
     C     SCDEFA        IFNE      *ZEROS
     C                   MOVEL     *BLANKS       LLDEFN
     C*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     C*  DEFENSE ADDRESS/PHONE CLEARED IF FEIN IS NOT ZERO
     C*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     C                   ENDIF                                                  ENDIF SCDEFA
      *
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLDEDT
      *
     C                   MOVEL(P)  SCDEFR        LLDEFR
      *
      *   MOVE DEFENSE ATTY (EXTENSION) & NUMBER INTO DATABASE FIELD
      *                     EXCHANGE #
     C     SCDFP1        MULT      10000         LLDEFP
     C                   ADD       SCDFP2        LLDEFP
      *
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       LLemdd = scLema;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   Z-ADD     SCPLAA        LLPLAA
     C                   Z-ADD     SCPLAM        LLPLAM
     C                   MOVEL(P)  SCPLAN        LLPLAN
     C                   MOVEL(P)  SCRFNM        LLRFNM
      *
      *  PLAINTIFF ATTORNEY NAME CLEARED IF FEIN IS NOT ZERO
      *
     C     SCPLAA        IFNE      *ZEROS
     C                   MOVEL     *BLANKS       LLPLAN
     C                   ENDIF                                                  ENDIF SCPLAA
      *
     C                   Z-ADD     SCDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLDTAP
      *
     C                   MOVEL(P)  SCPLAR        LLPLAR
      *
      *   MOVE PLAINTIFF ATTY EXTENSION & NUMBER INTO DATABASE FIELD
      *
     C     SCPLP1        MULT      10000         LLPLAP
     C                   ADD       SCPLP2        LLPLAP
      *
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       LLemdt = scLemd ;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   MOVEL(P)  SCFRAU        LLFRAU
      *
     C                   Z-ADD     SCLWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLLWDT
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLSTDT
     C                   Z-ADD     SCISCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLISCM
     C                   Z-ADD     SCMSCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLMSCM
      *
     C                   MOVEL(P)  SCMTST        LLMTST
     C                   Z-ADD     SCSAMT        LLSAMT
     C*
     C                   MOVEL(P)  SCDAD1        LLDAD1
     C                   MOVEL(P)  SCDAD2        LLDAD2
     C                   MOVEL(P)  SCDCTY        LLDCTY
     C                   MOVEL(P)  SCDST         LLDST
     C                   MOVEL     SCDZP1        LLDZIP
     C                   MOVE      SCDZP2        LLDZIP
     C*
     C                   MOVEL(P)  SCPAD1        LLPAD1
     C                   MOVEL(P)  SCPAD2        LLPAD2
     C                   MOVEL(P)  SCPCTY        LLPCTY
     C                   MOVEL(P)  SCPST         LLPST
     C                   MOVEL     SCPZP1        LLPZIP
     C                   MOVE      SCPZP2        LLPZIP
     C*
      *
      * SET UP WCAB# AND PHONE #
     C     *IN45         IFEQ      *ON
     C                   MOVEL     SCWCAB        LLWCAB
     C                   MOVE      SCIAAR        LLIAAR
     C     SCIAP1        MULT      10000         LLIAPH
     C                   ADD       SCIAP2        LLIAPH
     C     SCWCAB        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           LLLITG
     C                   ENDIF
     C                   ENDIF                                                  END *IN45 = *ON
     C*
     C                   MOVEL(P)  SCRFNM        LLRFNM
     C                   MOVEL(P)  SCPRGN        LLPRGN
     C                   MOVEL(P)  SCPRBN        LLPRBN
     C                   MOVEL(P)  SCPRMN        LLPRMN
     C                   MOVEL(P)  SCPRAH        LLPRAH
     C                   MOVEL(P)  SCCLRI        LLCLRI
     C                   MOVEL(P)  SCPRLI        LLPRLI
     C*
     C                   Z-ADD     SCFDLY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLFDLY
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
     C*
     C                   EXSR      CHKVFK
     C*
     C     *IN90         IFEQ      *OFF
      *
     C                   MOVEA     '000'         *IN(17)                        *IN17 - 19
     C                   MOVEA     '00000000'    *IN(22)                        22 - 29
     C                   MOVEA     '0'           *IN(30)                        RESET DSPATR(PC)
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKH         CASEQ     *ON           KHDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C   41*INKQ         CASEQ     *ON           KQDO                     90
     C   42*INKR         CASEQ     *ON           KRDO                     90
     C                   ENDCS
     C*
     C                   ENDIF
      *
      * DATE ASSIGNED
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDEDT
      * DATE PLANTIFF APPOINTED
     C                   Z-ADD     SCDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTAP
      * LAWSUIT DATE
     C                   Z-ADD     SCLWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCLWDT
      * SETTLEMENT DATE
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCSTDT
      *
      * Comp check mailed date
     C                   Z-ADD     SCISCM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCISCM
      * Med check mailed date
     C                   Z-ADD     SCMSCM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCMSCM
     C*
     C                   Z-ADD     SCFDLY        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL(P)  SRDTO         SCFDLY
     C                   ENDIF
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *
     C                   CLEAR                   *IN
     C                   RETURN
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
      *
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL     MODEA         SCMODE
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
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*** CONDITION DISPLAY TITLES
     C*
     C     CLTID         IFEQ      '135'
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '025'
     C                   MOVE      *ON           *IN42
     C                   ELSE
     C                   MOVE      *OFF          *IN42
     C                   ENDIF
     C                   EXSR      LDSCFK
     C*
     C                   Z-ADD     2             #OFSCR            1 0
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
     C     LLKY          CHAIN     PLL01                              6097
     C*
     C     *IN97         IFEQ      *ON
     C     FLSTLL        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN60                          SKIP TO BOTTOM
     C                   END
     C*
     C*
     C                   EXSR      MVSCDB
     C                   TIME                    DSDTTM
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     Q1USER        LLUSER
     C                   MOVEL     Q1PGM         LLPGM
     C                   Z-ADD     XXDT          LLDT
     C                   MOVEL     DSTIME        LLTIME
     C                   UPDATE    PLL01
     C*
     C                   ELSE
     C                   Z-ADD     XXCO#         LLCO#
     C                   Z-ADD     CMFND         LLFND
     C                   Z-ADD     CMFYR         LLFYR
     C                   Z-ADD     CMCASE        LLCASE
     C                   MOVEL     Q1USER        LLIUSR
     C                   MOVEL     Q1PGM         LLIPGM
     C                   Z-ADD     XXDT          LLIDT
     C                   MOVEL     DSTIME        LLITM
     C                   WRITE     PLL01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   Z-ADD     LLTIME        SVTIME
     C                   Z-ADD     LLDT          SVDATE
     C*
     C     @@RRN         CHAIN     DBRCDF                             6097
     C*
     C     *IN97         IFEQ      *ON
     C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN60                          SKIP TO BOTTOM
     C                   END
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*
     C                   MOVEL(P)  SCSCL#        CMACL#
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C                   UPDATE    DBRCDF
     C*
     C*    Call API to update Managed Care Claim Master - FMCMP
     C*
     C                   CLEAR                   PARMCL
     C*
     C     XXAPIF        CASEQ     *ON           MCAPIC
     C                   ENDCS                                                              DATE
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*
     C                   MOVE      *ON           OUTFLG
     C*
      *
      *   Call WW Claim Trial Information
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXSR      CALLR
     C                   ENDIF
      *
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*====================================================================
     C* UPDOFA - Update values in the option/function array
     C*====================================================================
     C*  Before calling this subroutine make the following assignments:
     C*       XXOPFN = '2' if Function
     C*       XXNMBR = Option/Function number to change
     C*            NOTE: Pass option/function numbers as 2 char fields.
     C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
     C*       XXACD = 'XXX' where each 'X' is Y or N depending on change rqrd
     C*
     C*  This subroutine when called evaluates array OF for XXOPFN and checks
     C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
     C*  replaces 'XXX' with XXACD.
     C*
     C     UPDOFA        BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C*
B01  C     FOUND         DOWEQ     *OFF
     C     OF(I)         ANDNE     '3'
     C*
B02  C     OF(I)         IFEQ      XXOPFN
     C                   MOVEL     OFD(I)        DUMNUM            2
B03  C     XXNMBR        IFEQ      DUMNUM
     C                   MOVEL(P)  OFD(I)        DUMOFD           30
     C                   MOVE      XXACD         DUMOFD
     C                   MOVEL     DUMOFD        OFD(I)
     C                   MOVE      *ON           FOUND
E03  C                   ENDIF                                                  ZZNUM=DUMNUM
E02  C                   ENDIF                                                  OF,I=XXOPFN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
     C*
E01  C                   ENDDO                                                  FOUND DOWEQ *OFF
     C*
     C                   ENDSR
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
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
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
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C*
     C                   ENDSR
** Description--------------ACDN    05/20/09
203F3=Exit                  NYY
205F5=Refresh               NYN
207F7=Financial summary     NYY
208F8=Multi Settle          YYY
210F10=Search               YYN
212F12=Cancel               YYY
214F14=DARTS                NYY
216F16=Third party attny    YYY
217F17=Attorney Order       YYY
3
