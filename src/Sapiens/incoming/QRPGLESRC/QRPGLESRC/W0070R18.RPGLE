     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      *
      /TITLE W0070R18 - CLAIMS, ADDITIONAL LEGAL INFORMATION
     H Bnddir('WTBNDDIR')
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY ADDITIONAL LEGAL     *
      *                INFORMATION.                                   *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R16 - CLAIM LEGAL INFORMATION                         *
      *                                                               *
      *  CALLS TO:  FA003R - API to update MC claim master FMCMP      *
      *             SA001R - API to determine if MC installed         *
      *                                                               *
      *  CREATED:04/21/07   PROGRAMMER: 183   PROJECT: 135WIB         *
      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:298 Project: SY420
      *                Mask SSN
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
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070F18  CF   E             WORKSTN
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
     F** ********BATRL   IF  E           K        DISK                    UC
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
     F*                                                                *********
     F*** BEGIN ADD *** 02/25/98 ***********************************************
     FWTGIL     IF   E           K DISK
     F*** END   ADD *** 02/25/98 ***********************************************
     F*
      *
      *  EXCESS REINSURANCE
      *
      *
     D*/COPY SCOPYMT01I
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
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
     D*** BEGIN CHG *** 03/23/04 **************************************
     D************  1                     C         #OFSCR
     D #OFSCR          C                   CONST(1)
     D*** END   CHG *** 03/23/04 **************************************
     D COMAPI          C                   CONST('WA001R')
      * ================================================================
     C*
     C/COPY SCOPYMT01C
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
     C*** BEGIN ADD *** 02/25/98 ***********************************************
     C*
     C*   KEY LIST FOR THE SYSTEM TABLE ID FILE
     C*
     C     IDKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXTBD1            4
     C*                                                                *********
     C*** END   ADD *** 02/25/98 ***********************************************
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
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
     C                   MOVE      *OFF          *IN27                          POS CUSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
      *  VALIDATE DEFENSE ATTY FEIN/MOD
      *
     C     SCTPAA        IFNE      *ZERO
     C     SCTPAM        ORNE      *ZERO
     C                   Z-ADD     sctpaa        XXFEIN
     C                   Z-ADD     sctpam        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9072
     C                   LEAVE
     C*** BEGIN ADD *** 03/23/04 **************************************
     C*   SET INDICATOR FOR PROVIDER FILE USE(VALID FEIN/MOD)
     C                   ELSE
     C                   MOVE      *ON           *IN20
     C*** END ADD ***** 03/23/04 **************************************
     C                   ENDIF
     C                   ENDIF
      *
      *  CAN HAVE FEIN/MOD OR NAME,  NOT BOTH
      *  ONLY CHECK IF USER INPUT BOTH FEIN/MOD AND NAME;
      *
      *** BEGIN ADD *** 11/01/99 **************************************
     C                   MOVEL(P)  PDNAME        XXNM30           30
      *** END ADD ***** 11/01/99 **************************************
     C     *IN20         IFEQ      *OFF
     C     sctpaa        IFNE      *ZERO
     C     sctpan        ANDNE     *BLANK
      *** BEGIN CHG *** 11/01/99 **************************************
     C********** sctpan    ANDNEPDNAME
     C     sctpan        ANDNE     XXNM30
      *** END CHG ***** 11/01/99 **************************************
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C*
      *  DATE ASSIGNED MUST BE GREATER THAN ACCIDENT DATE
      *
     C     sctpdt        IFNE      *ZERO
     C                   Z-ADD     sctpdt        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         sctpdt
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   Z-ADD     sctpdt        SRDTI
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
     C     sctpdt        IFNE      *ZERO
     C     sctpaa        IFEQ      *ZERO
     C     sctpan        ANDEQ     *BLANK
     C                   SETON                                        9050
     C                   LEAVE
     C                   ENDIF                                                  ENDIF sctpaa
     C                   ENDIF                                                  ENDIF sctpdt
      *
      *  AREA CODE/PHONE NUMBER NOT ALLOWED IF NAME OR FEIN NOT ENTERED
      *
     C     sctpar        IFNE      *ZERO
     C     sctfp1        ORNE      *ZERO
     C     sctfp2        ORNE      *ZERO
     C     sctpaa        IFEQ      *ZERO
     C     sctpan        ANDEQ     *BLANK
     C                   SETON                                        9048
     C                   LEAVE
     C                   ENDIF                                                  ENDIF sctpaa
     C                   ENDIF                                                  ENDIF sctpar
      *
      *
      *  CAN HAVE FEIN/MOD OR NAME,  NOT BOTH
      *  ONLY CHECK IF USER INPUT BOTH FEIN/MOD AND NAME;
      *
      *** BEGIN ADD *** 11/01/99 **************************************
     C                   MOVEL(P)  PDNAME        XXNM30
      *** END ADD ***** 11/01/99 **************************************
      *
      *
     C*** BEGIN CHG *** 03/23/04 **************************************
     C*
     C***        === REMAINING FIELDS AND EDITS MOVED TO SCR02 ===
     C*
     C                   MOVE      *ON           *IN22                          POS CUSR
     C*** END CHG   *** 03/23/04 **************************************
      *
     C                   ENDDO
     C*
     C*** BEGIN ADD *** 08/04/98 **************************************
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
     C*** END ADD ***** 08/04/98 **************************************
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
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
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
     C     HFLD          WHENEQ    'SCTPAA'
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
     C                   MOVEL     QQFEIN        sctpaa
     C                   MOVEL     QQMOD         sctpam
     C                   MOVEL     QQFEIN        XXFEIN                         MOVE FOR CHAIN
     C                   MOVEL     QQMOD         XXMOD                          MOVE FOR CHAIN
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        sctpan
     C                   MOVEL(P)  PDAREA        sctpar
     C                   MOVEL(P)  PDPHN#        sctfp1
     C                   MOVE(P)   PDPHN#        sctfp2
     C*** BEGIN ADD *** 03/23/04 **************************************
     C                   MOVE      *ON           *IN20
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        sctpan
     C                   MOVEL(P)  PDADR2        sctad1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        sctad1
     C                   MOVEL(P)  PDADR2        sctad2
     C                   ENDIF
     C                   MOVEL     PDCTY         sctcty
     C                   MOVEL     PDST          sctst
     C                   MOVEL     PDZIP         sctzp1
     C                   MOVE      PDZIP         sctzp2
     C*** END ADD ***** 03/23/04 **************************************
     C                   END
     C                   END
      *
      *  DEFENSE ATTY ALPHA SEARCH
      *
     C     HFLD          WHENEQ    'SCTPAN'
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
     C                   MOVEL     QQFEIN        sctpaa
     C                   MOVEL     QQMOD         sctpam
     C                   MOVEL     QQFEIN        XXFEIN                         MOVE FOR CHAIN
     C                   MOVEL     QQMOD         XXMOD                          MOVE FOR CHAIN
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        sctpan
     C                   MOVEL(P)  PDAREA        sctpar
     C                   MOVEL(P)  PDPHN#        sctfp1
     C                   MOVE(P)   PDPHN#        sctfp2
     C*** BEGIN ADD *** 03/23/04 **************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        sctpan
     C                   MOVEL(P)  PDADR2        sctad1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        sctad1
     C                   MOVEL(P)  PDADR2        sctad2
     C                   ENDIF
     C                   MOVEL     PDCTY         sctcty
     C                   MOVEL     PDST          sctst
     C                   MOVEL     PDZIP         sctzp1
     C                   MOVE      PDZIP         sctzp2
     C*** END ADD ***** 03/23/04 **************************************
     C                   END
     C                   END
      *
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
     C*******              Z-ADDCMEMP#    SCEMP#
     C*******              Z-ADDCMDIV     SCDIV
      *
     C*******              MOVE *BLANK    SCENAM    P
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*******              MOVELEMNAME    SCENAM    P
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
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
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
     C                   Z-ADD     *ZERO         sctpaa
     C                   Z-ADD     *ZERO         sctpam
     C                   MOVE      *BLANK        sctpan
     C                   Z-ADD     *ZERO         sctpdt
      ***** BEG DEL ******* 10/27/99 **********************************
      ***** BEG CHG ******* 05/04/00 **********************************
     C                   MOVE      *BLANK        sctpar
     C                   MOVE      *BLANK        sctfp1
     C                   MOVE      *BLANK        sctfp2
     C**************       MOVE *BLANK    sctpar
     C**************       MOVE *BLANK    sctfp1
     C**************       MOVE *BLANK    sctfp2
      ***** END CHG ******* 05/04/00 **********************************
      ***** END DEL ******* 10/27/99 **********************************
     C*** BEGIN ADD *** 03/23/04 **************************************
     C                   MOVEL     *BLANK        sctpan
     C                   MOVEL     *BLANK        sctad1
     C                   MOVEL     *BLANK        sctad2
     C                   MOVEL     *BLANK        sctcty
     C                   MOVEL     *BLANK        sctst
     C                   MOVE      *ZERO         sctzp1
     C                   MOVE      *ZERO         sctzp2
     C*** End add ***** 04/01/05 **************************************
      *
     C*** BEGIN ADD *** 06/26/98 **************************************
     C                   Z-ADD     0             SVTIME
     C                   Z-ADD     0             SVDATE
     C*** END ADD ***** 06/26/98 **************************************
     C     LLKY          CHAIN(N)  PLL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LLTIME        SVTIME
     C                   Z-ADD     LLDT          SVDATE
      *
      *
     C                   Z-ADD     LLTPAA        sctpaa
     C                   Z-ADD     LLTPAM        sctpam
      *
      *   CHAIN TO PAYEE DEMOGRAPHICS FILE TO FIND NAME ASSOCIATED
      *   WITH DEFENSE ATTY FEIN/MOD
      *
     C                   Z-ADD     sctpaa        XXFEIN
     C                   Z-ADD     sctpam        XXMOD
     C     PDKY          CHAIN     PPD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        sctpan
     C*** BEGIN ADD *** 03/23/04 **************************************
     C                   MOVE      *ON           *IN20
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        sctpan
     C                   MOVEL(P)  PDADR2        sctad1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        sctad1
     C                   MOVEL(P)  PDADR2        sctad2
     C                   ENDIF
     C                   MOVEL     PDCTY         sctcty
     C                   MOVEL     PDST          sctst
     C                   MOVEL     PDZIP         sctzp1
     C                   MOVE      PDZIP         sctzp2
     C*** END ADD ***** 03/23/04 **************************************
      *
     C                   ELSE
     C                   MOVEL(P)  LLTPAN        sctpan
     C***** BEGIN ADD ***** 03/23/04 **********************************
     C                   MOVEL(P)  LLTAD1        sctad1
     C                   MOVEL(P)  LLTAD2        sctad2
     C                   MOVEL(P)  LLTCTY        sctcty
     C                   MOVEL     LLTST         sctst
     C                   MOVEL     LLDZIP        sctzp1
     C                   Z-ADD     LLDZIP        sctzp2
     C     LLFRAU        IFEQ      *BLANK
     C                   MOVEL     '00'          LLFRAU
     C                   ENDIF
     C***** END ADD ******* 03/23/04 **********************************
     C                   ENDIF                                                  ENDIF *IN60
      *
     C                   Z-ADD     LLTPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         sctpdt
      *
      ***** END ADD ***** 03/31/05 ************************************
      *
      ***** BEG ADD ******* 10/27/99 **********************************
      *
      *  Defense attorney
      *
     C                   MOVEL(P)  LLTPAR        sctpar
     C                   MOVEL(P)  LLTPAP        sctfp1
     C                   MOVE(P)   LLTPAP        sctfp2
      *
      *
      ***** END ADD ******* 10/27/99 **********************************
     C                   ENDIF                                                  *IN60
      *
     C                   MOVE      *ON           *IN27                          POS CUSR
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
     C                   Z-ADD     sctpaa        LLTPAA
     C                   Z-ADD     sctpam        LLTPAM
     C                   MOVEL(P)  sctpan        LLTPAN
      *
      *  DEFENSE ATTORNEY NAME CLEARED IF FEIN IS NOT ZERO
      *
     C     sctpaa        IFNE      *ZEROS
     C                   MOVEL     *BLANKS       LLTPAN
     C*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     C*** BEGIN ADD *** 03/23/04 **************************************
     C*  DEFENSE ADDRESS/PHONE CLEARED IF FEIN IS NOT ZERO
     C*                    MOVEL*BLANKS   LLTAD1
     C*                    MOVEL*BLANKS   LLTAD2
     C*                    MOVEL*BLANKS   LLTCTY
     C*                    MOVEL*BLANKS   LLTST
     C*                    Z-ADD*ZERO     LLDZIP
     C*                    Z-ADD*ZERO     LLTPAR
     C*                    Z-ADD*ZERO     LLTPAP
     C*** END ADD ***** 03/23/04 **************************************
     C*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     C                   ENDIF                                                  ENDIF sctpaa
      *
     C                   Z-ADD     sctpdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         LLTPDT
      *
     C                   MOVEL(P)  sctpar        LLTPAR
      *
      *   MOVE DEFENSE ATTY (EXTENSION) & NUMBER INTO DATABASE FIELD
      *                     EXCHANGE #
     C     sctfp1        MULT      10000         LLTPAP
     C                   ADD       sctfp2        LLTPAP
     C*
     C                   MOVEL(P)  SCTAD1        LLTAD1
     C                   MOVEL(P)  SCTAD2        LLTAD2
     C                   MOVEL(P)  SCTCTY        LLTCTY
     C                   MOVEL(P)  SCTST         LLTST
     C                   MOVEL     SCTZP1        LLTZIP
     C                   MOVE      SCTZP2        LLTZIP
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
     C                   MOVEA     '000'         *IN(17)                        *IN17 - 19
     C                   MOVEA     '00000000'    *IN(22)                        22 - 29
     C                   MOVEA     '0'           *IN(30)                        RESET DSPATR(PC)
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
      *
      * DATE ASSIGNED
     C                   Z-ADD     sctpdt        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         sctpdt
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *
     C* *******            CLOSEBATRL
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
     C*PUIMOD
     C*                  Z-ADD     *DATE         SRDTI
     C                   EVAL      SRDTI=%Int(%Char(%Date():*USA0))
     C
     C
     C
     C
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
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
     C*** BEGIN DLT *** 08/04/98 **************************************
B02  C********** SVDATE    IFNE LLDT
     C*** BEGIN ADD *** 06/26/98 **************************************
     C********** *IN60     ANDEQ'0'
     C*** END ADD ***** 06/26/98 **************************************
B02  C********** SVTIME    ORNE LLTIME
     C*** BEGIN ADD *** 06/26/98 **************************************
     C********** *IN60     ANDEQ'0'
     C*** END ADD ***** 06/26/98 **************************************
     C******************** CALL 'WA017R'
     C******************** PARM           LLUSER
     C******************** PARM           QQRTCD
     C********************
     C******************** ELSE
     C*** END DLT ***** 08/04/98 **************************************
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
     C*** BEGIN DLT *** 08/04/98 **************************************
B02  C********** SVDAT2    IFNE CMDT
B02  C********** SVTIM2    ORNE CMTIME
     C**********           CALL 'WA017R'
     C**********           PARM           CMUSER
     C**********           PARM           QQRTCD
     C**********
     C**********           ELSE
     C*** END DLT ***** 08/04/98 **************************************
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
     C*** BEGIN DLT *** 08/04/98 **************************************
     C******************** ENDIF                           SVDAT2 NE CMDT
     C*** END DLT ***** 08/04/98 **************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** BEGIN DLT *** 08/04/98 **************************************
     C******************** ENDIF                           SVDATE NE LLDT
     C*** END DLT ***** 08/04/98 **************************************
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
     C*** BEGIN DLT *** 11/05/97 ***********************************************
     C******************** Z-ADDCMACDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     QMACDT
     C*** END DLT ***** 11/05/97 ***********************************************
     C*** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE      CMACDT        QMACDT
     C*** END ADD ***** 11/05/97 ***********************************************
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
     C*** BEGIN DLT *** 11/05/97 ***********************************************
     C****************     Z-ADDCMDTRP    SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMDTRP    P      Reported
     C****************     Z-ADDCMDTLW    SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMDTLW    P      Last worked
     C****************     Z-ADDCMENTD    SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMENTD    P      ENTERED
     C****************     Z-ADDCMRCVD    SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMRCVD    P      Received
     C****************     Z-ADDCMMMI     SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMMMI     P      Max med impr
     C****************     Z-ADDCMDOD     SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMDEDT    P      Death date
     C*** END DLT ***** 11/05/97 ***********************************************
     C*** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
     C*** END ADD ***** 11/05/97 ***********************************************
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
     C*
     C                   Z-ADD     DBRRN         QMRRN
     C*
     C*** BEGIN DLT *** 11/05/97 ***********************************************
     C****************     Z-ADDCMDTRE    SRDTI              Date
     C****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
     C****************     MOVELSRDTO     QMDTRE    P      Rehabilitatn
     C*** END DLT ***** 11/05/97 ***********************************************
     C*** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
     C*** END ADD ***** 11/05/97 ***********************************************
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
     C*** BEGIN ADD *** 04/07/98 **************************************
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
     C*** END   ADD *** 04/07/98 **************************************
     C*
     C*=====================================================================
     C                   CALL      'FA003R'
     C*
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
     C*** BEGIN CHG *** 11/05/97 ***********************************************
     C******************** PARM           QMACDT  8
     C                   PARM                    QMACDT            7
     C*** END CHG ***** 11/05/97 ***********************************************
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
     C*** BEGIN DLT *** 11/05/97 ***********************************************
     C******************** PARM           QMDTRP  8
     C******************** PARM           QMDTLW  8
     C*** END DLT ***** 11/05/97 ***********************************************
     C*** BEGIN ADD *** 11/05/97 ***********************************************
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
     C*** END ADD ***** 11/05/97 ***********************************************
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
     C*** BEGIN DLT *** 11/05/97 ***********************************************
     C******************** PARM           QMENTD  8
     C******************** PARM           QMRCVD  8
     C******************** PARM           QMMMI   8
     C*** END DLT ***** 11/05/97 ***********************************************
     C*** BEGIN ADD *** 11/05/97 ***********************************************
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
     C*** END ADD ***** 11/05/97 ***********************************************
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
      *** BEGIN CHANGE *** 09/02/99 ***********************************
     C******************** PARM CMACL#    QMSCL# 10        State claim numbr
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
      *** END CHANGE ***** 09/02/99 ***********************************
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
     C*** BEGIN CHG *** 11/05/97 ***********************************************
     C******************** PARM           QMDEDT  8
     C                   PARM                    QMDEDT            7
     C*** END CHG ***** 11/05/97 ***********************************************
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
     C*** BEGIN CHG *** 11/05/97 ***********************************************
     C******************** PARM           QMDTRE  8
     C                   PARM                    QMDTRE            7
     C*** END CHG ***** 11/05/97 ***********************************************
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
     C*** BEGIN ADD *** 04/07/98 **************************************
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
     C*** END   ADD *** 04/07/98 **************************************
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C***** END   ADD **** 04/03/00 ***********************************
     C*** BEGIN ADD *** 08/25/00 **************************************
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C*** END   ADD *** 08/25/00 **************************************
     C*
     C                   ENDSR
