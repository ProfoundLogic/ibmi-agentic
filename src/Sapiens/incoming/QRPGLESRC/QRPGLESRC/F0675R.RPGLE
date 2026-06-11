     H*****************************************************************
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0675R - BILL ENTRY MESSAGE DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program presents the user with a subfile list of   **
     H**     messages associated with a bill detail line. The sub-   **
     H**     file contains a short description and allows the option **
     H**     to display an extended description. When this option is **
     H**     taken, a window that contains up to 5 lines of extended **
     H**     description is presented.                               **
     H**                                                             **
     H**   PROGRAMMER: C:56  ---  BASED ON F0670R FROM V3            **
     H**   CREATED: 09/03/98         PROJECT: MC152                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F0390R  - Work with Provider Bill Detail                **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     HPRBMSA - HPR CodeReview Message Description Retrevial  **
     H**                                                             **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  06/11/12 C:277 Project: 114MER                              **
     H*                Add Message for Claim Check                   **
     H*                                                              **
     H*  12/21/09 C:198 Support: 71042                               **
     H*                 Added processing to display 6th line of      **
     H*                 error message.                               **
     H*                                                              **
     H*  06/10/02 C:62 Project: 114MDC                               **
     H*                ADD CALL PROCESSING TO CODEREVIEW MESSAGES.   **
     H*                                                              **
     H*  01/16/02 C:79 Project: 114MAO                               **
     H*                1. ADD MGSRC = C FOR CORRECT CODING           **
     H*                2. CHANGE CODE REVIEW SOURCE TO R.            **
     H*                                                              **
     H*  08/15/00 C:62 PROJECT: MC215                                **
     H*                REPLACE SFBPF REFERENCES WITH APIS.           **
     H*                                                              **
     H*  01/13/00 C:62 Project: NW012                                **
     H*                ALLOW SOURCE TYPES 'N'.                       **
     H*                                                              **
     H*  07/19/99 C:62 Project: MC208                                **
     H*                -FIX AMOUNT VALUES.                           **
     H*                -ADD EOMB LINE.                               **
     H*                                                              **
     H*  12/15/97 C:56 Project: FS139                                **
     H*             1. Correct field names from file FDMGP           **
     H*                                                              **
     H*             2. Add condition to open of HPR files based on   **
     H*                FLFL04 in SFBPF                               **
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      11       Screen Valid command key.                     **
     H**      18       Screen ROLLUP command key.                    **
     H**      20       Screen 1 - Invalid option.                    **
     H**      31       Use Bill Message File key with DCS#           **
     H**      32       Use Bill Message File key with EOB#           **
     H**      50       General usage.                                **
     H**      80       Subfile 1 clear.                              **
     H**      81       Subfile 1 display.                            **
     H**      89       Subfile 1 end.                                **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**   FDMGL    (PMG01): Bill Entry Message                      **
     H**                     (CO#/SUB#/FND/FYR/CASE/DCS#/SEQ#/MSEQ)  **
     H**   FDMGL1   (PMG02): Bill Entry Message                      **
     H**                     (CO#/SUB#/FND/FYR/CASE/EOB#/SEQ#/MSEQ)  **
     H**   FMRCL    (PRC01): Reasonable/Customary File               **
     H**                     (CO#/ZIPA/CPT4/EFDT)                    **
     H**   FTABL           : Logical over Table Files (CO#/KEY)      **
     H**            (PEB01): EOMB Description Table                  **
     H**   FTMIL    (PMI01): Bill Entry Message ID (CO#/MSG#/SEQ#)   **
     H**   SMCOL    (PC001): System Company Master File (CO#)        **
     H*****************************************************************
     H*
     FF0675F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:SFNR1)
     FFDMGL     IF   E           K DISK
     FFDMGL1    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     FFMRCL     IF   E           K DISK
     FFTMIL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFDCAP     IF   E             DISK
     FFAEOP     IF   E             DISK
     FFDDHL     IF   E           K DISK
     FFDDAL2    IF   E           K DISK
     FFDCHL1    IF   E           K DISK
     F*** Beg Add *** 06/11/12 ****************************************
     FFDKHL2    IF   E           K DISK
     F*** End Add *** 06/11/12 ****************************************
     D*
     D* ---------------------------------------------------------------
     D* Tables and arrays
     D* ---------------------------------------------------------------
     D*
     D* ---------------------------------------------------------------
     D* External System Information Data Structure
     D* ---------------------------------------------------------------
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*----------------------------------------------------------------
     D* System date and time
     D*----------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*----------------------------------------------------------------
     D* Help request work area
     D*----------------------------------------------------------------
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*----------------------------------------------------------------
     D* Message text table work area
     D*----------------------------------------------------------------
     D                 DS
     D  WL                     1    420
     D                                     DIM(6)                               Message text
     D  SCWL0                  1     70
     D  SCWL1                 71    140
     D  SCWL2                141    210
     D  SCWL3                211    280
     D  SCWL4                281    350
     D  SCWL5                351    420
     D*
     D*----------------------------------------------------------------
     D* HPR Message interface area
     D*----------------------------------------------------------------
     D HPRMSG          DS
     D  CLOSE                  1      1
     D  AFLAG                  2      2
     D  APGMID                 3     10
     D  APARID                11     14
     D  AFILID                15     21
     D  FILL1                 22     30
     D  AFILES                31     32
     D  FILL2                 33     34
     D  AFILEF                35     46
     D  AARECK                47     86
     D  EOB#                  87    128
     D  DETAIL               129    131
     D  CODE                 132    136
     D  MOD                  137    138
     D  KEY                  139    143
     D  STATUS               144    144
     D  MSG1A                145    154
     D  MSG1B                155    220
     D  MSG2                 221    286
     D  MSG3                 287    352
     D  MSG4                 353    418
     D  MSG5                 419    484
     D  BLANK1               485    600
     D  BLANK2               601    800
     D  BLANK3               801   1000
     D  BLANK4              1001   1200
     D*
     D*----------------------------------------------------------------
     D* Named constant defines
     D*----------------------------------------------------------------
     D COM1            C                   CONST('MESSAGE NOT FOUND')
     D CIDR            C                   CONST('HPR CodeReview')
     D CIDP            C                   CONST('Pre-Certification')
     D CIDS            C                   CONST('State Adjudication')
     D CIDN            C                   CONST('Network')
     D CIDC            C                   CONST('Correct Coding')
     D TITLE           C                   CONST('Work with Provider B-
     D                                     ill Messages')
     D*** Beg Add *** 06/11/12 ****************************************
     D CIDK            C                   CONST('ClaimCheck')
     D xxflxx          C                   const('XXXXX')
     D xxflyy          C                   const('YYYYY')
     D xxloc           s              3  0
     D xxdata          s                   like(midesc)
     D*** End Add *** 06/11/12 ****************************************
      /EJECT
     C*----------------------------------------------------------------
     C*        Entry parm list
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN1            9 0          fdcap
     C                   PARM                    @@RRN2            9 0          faeop
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
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C*
     C*----------------------------------------------------------------
     C*        Key lists
     C*----------------------------------------------------------------
     C*          PMG01 - Bill Entry Message
     C     MGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDCS#
     C                   KFLD                    XXSEQ#
     C*
     C     DHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     CHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     CHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXCRB#
     C*
     C     DAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXRCN
     C*
     C*** Beg Add *** 06/11/12 ****************************************
     C     KHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C                   KFLD                    XXDSEQ
     C*** End Add *** 06/11/12 ****************************************
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    MGCO#         XXCO#
     C     *LIKE         DEFINE    MGSUB#        XXSUB#
     C     *LIKE         DEFINE    MGFND         XXFND
     C     *LIKE         DEFINE    MGFYR         XXFYR
     C     *LIKE         DEFINE    MGCASE        XXCASE
     C     *LIKE         DEFINE    MGDCS#        XXDCS#
     C     *LIKE         DEFINE    MGDSEQ        XXSEQ#
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    CHCRB#        XXCRB#
     C*
     C     MGKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXSEQ#
     C*
     C     *LIKE         DEFINE    MGEOB#        XXEOB#
     C*
     C*          PMI01 - Bill Entry Message ID
     C     MIKY          KLIST
     C                   KFLD                    XXMSG#
     C     *LIKE         DEFINE    MIMSG#        XXMSG#
     C*
     C*          PRC01 - Reasonable/Customary File
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C     RCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C*
     C*          *LIKE     DEFN SCFDAT    XXFDAT
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C     *LIKE         DEFINE    RCCPT4        XXCPT4
     C     *LIKE         DEFINE    RCEFDT        XXEFDT
     C*
     C*          FTABL - Logical over Table Files
     C*          PEB01 - EOMB Description Table
     C*
     C*          SSYPL - Logical over Fee Schedule Control File
     C*----------------------------------------------------------------
     C*          Defines
     C*----------------------------------------------------------------
     C     *LIKE         DEFINE    SHRNO1        SFNR1
     C     *LIKE         DEFINE    SHRNO1        SFNXT1
      /EJECT
     C*================================================================
     C*================================================================
     C*          M A I N L I N E
     C*================================================================
     C*================================================================
     C*
     C     @@RRN1        CHAIN     PCA01                              99
     C     @@RRN2        CHAIN     PEO01                              99
     C*
     C* GET STANDARD SCREEN HEADINGS
     C*
     C                   MOVE      EOCO#         ##CO#
     C                   MOVE      EOSUB#        ##SUB#
     C                   MOVE      EOFND         ##FND
     C                   MOVE      EOFYR         ##FYR
     C                   MOVE      EOCASE        ##CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C* CREATE EOMB LINE
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   Z-ADD     CALIN#        SCLIN#
     C                   Z-ADD     CADSEQ        SCDSEQ
     C*
     C     EONTWK        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  EONTWK        SCNTWK
     C                   SETON                                        68
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       68
     C*
     C                   ENDIF
     C*
     C*
     C*          Execute SR00 for initialization
     C*          ------------------------------------------------------
     C                   EXSR      SR00
     C*
     C*          Loop until F12 entered
     C*          ------------------------------------------------------
     C*
     C     *INKL         DOWEQ     *OFF
     C     *INLR         ANDEQ     *OFF
     C*
     C*             Write/read subfile 1
     C*             ---------------------------------------------------
     C                   EXFMT     SCR01C
     C*
     C*             Read changed subfile records
     C*             ---------------------------------------------------
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C*
     C*                Option 1 - Display window with extended detail
     C*                ------------------------------------------------
     C                   SETOFF                                       20
     C     SCOPT         IFEQ      '5'
     C                   EXFMT     WDW01
     C                   MOVEL(P)  'WDW01'       HDFRMT            5
     C     SFNR1         CHAIN     SCR01S                             50
     C                   MOVE      *BLANK        SCOPT
     C                   UPDATE    SCR01S
     C                   Z-ADD     SFNR1         SHRNO1
     C                   WRITE     SCR01R
     C*
     C*                Invalid option - error MCR0475
     C*                ------------------------------------------------
     C                   ELSE                                                   OPT=1
     C     SCOPT         IFNE      ' '
     C                   SETON                                        20
     C                   Z-ADD     SHRNO1        SFNR1
     C                   UPDATE    SCR01S
     C                   LEAVE
     C                   ENDIF                                                  SCOPT = Blank
     C                   UPDATE    SCR01S
     C                   ENDIF                                                  OPT=1
     C*
     C*                Read next changed record
     C*                ------------------------------------------------
     C                   READC     SCR01S                                 50
     C                   ENDDO                                                  50 = Off
     C*
     C*             If page down, no error, and not LR: fill next page
     C*             ---------------------------------------------------
     C     *IN18         IFEQ      *ON
     C     *IN20         ANDEQ     *OFF
     C     *INLR         ANDEQ     *OFF
     C                   EXSR      SR12
     C                   ENDIF                                                  18 = On
     C*
     C     HDFRMT        IFNE      'WDW01'
     C     *IN20         ANDEQ     *OFF
     C                   LEAVE
     C                   ENDIF                                                  HDFRMT IFEQ WDW01
     C                   MOVE      *BLANKS       HDFRMT
     C*
     C*             End of F3 loop
     C*             ---------------------------------------------------
     C                   ENDDO                                                  F3 = Off
     C*
     C*          Close HPR files
     C*          ------------------------------------------------------
     C     HPRFLG        IFEQ      '1'
     C     'FILE'        CAT       'CLOSE'       HPRCAL            9
     C                   CALL      HPRCAL
     C                   ENDIF                                                  HPRFLG EQ 1
     C*
     C*          End of program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
     C*
     C*================================================================
     C*================================================================
     C*          S U B R O U T I N E S
     C*================================================================
     C*================================================================
     C*  SR00    Initialization Subroutine
     C*================================================================
     C*
     C*          Move parameters to key fields
     C*          ------------------------------------------------------
     C     SR00          BEGSR
     C                   Z-ADD     CACO#         XXCO#
     C                   Z-ADD     CASUB#        XXSUB#
     C                   Z-ADD     CAFND         XXFND
     C                   Z-ADD     CAFYR         XXFYR
     C                   Z-ADD     CACASE        XXCASE
     C                   Z-ADD     CAEOB#        XXEOB#
     C                   MOVEL     CAZIPA        XXZIPA
     C                   Z-ADD     CADSEQ        XXSEQ#
     C                   MOVE      CACPT4        XXCPT4
     C                   Z-ADD     CAFDAT        XXEFDT
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Get company name from Company Master
     C*          ------------------------------------------------------
     C     XXCO#         CHAIN     PCO01                              50
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCCONM
     C*
     C*          Get CPT4 Description from Reasonable/Customary File
     C*          ------------------------------------------------------
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  50
     C                   MOVEL(P)  RCDSC1        SCDSC1
     C*
     C*          Get EOB Description from EOMB Description Table
     C*          ------------------------------------------------------
     C*
     C*          Get time
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C*
     C*          Move user name and parameters to screen fields
     C*          ------------------------------------------------------
     C                   MOVE      Q1USER        SCUSER
     C*----------------------------------------------------------------
     C*
     C                   MOVE      CACPT4        SCCODE
     C                   MOVE      CAPMOD        SCMOD
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C                   MOVE      CAPART        SCPART
     C                   MOVE      CADSEQ        SCDSEQ
     C                   Z-ADD     CACHRG        SCTOTC
     C*
     C                   Z-ADD     CANDIS        SCTOTD
     C                   ADD       CAOTHR        SCTOTD
     C                   ADD       CAOTH2        SCTOTD
     C                   ADD       CAMRA         SCTOTD
     C*
     C                   Z-ADD     CAPAID        SCTOTP
     C                   MOVE      CATOTD        SCAMT1                         MRA
     C                   MOVE      CANDIS        SCAMT2                         NDIS
     C                   MOVE      CAOTHR        SCAMT3                         OTHER
     C*
     C* GET PLACE OF SERVICE
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   DHPOS         SCPOS
     C                   MOVE(P)   DHTOS         SCTOS3
     C                   ENDIF                                                  *IN99 = *OFF
     C                   Z-ADD     CARCN         XXRCN
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       DACRST
     C                   ENDIF
     C*----------------------------------------------------------------
     C                   MOVEL(P)  TITLE         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*          Determine message key to use
     C*             31 = Use key with DCS#
     C*             32 = Use key with EOB#
     C*          ------------------------------------------------------
     C     XXDCS#        COMP      0                                  3131
     C     XXEOB#        COMP      0                                  3232
     C  N31
     CANN32              SETON                                        LR
     C*
     C*          Initialize first page of subfile
     C*          ------------------------------------------------------
     C                   EXSR      SR11
     C                   EXSR      SR12
     C     SFNR1         COMP      0                                      LR
     C*
     C*          Open HPR files
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       HPRFLG            1
     C     ##CLED        IFNE      *BLANKS
     C                   MOVE      '1'           HPRFLG                         HPR INSTALLED
     C     'FILE'        CAT       'OPEN '       HPRCAL
     C                   CALL      HPRCAL
     C                   ENDIF                                                  IN99 EQ 0
     C*
     C*          End of subroutine SR00
     C*          ------------------------------------------------------
     C     SR00X         ENDSR
     C*
     C*================================================================
     C*  SR11    Clear Subfile 1
     C*================================================================
     C*
     C*          Clear subfile
     C*          ------------------------------------------------------
     C     SR11          BEGSR
     C                   SETON                                        80
     C                   WRITE     SCR01C
     C                   SETOFF                                       8081
     C                   Z-ADD     0             SFNR1
     C                   Z-ADD     0             SFNXT1
     C                   WRITE     SCR01R
     C*
     C*          End of subroutine SR11
     C*          ------------------------------------------------------
     C     SR11X         ENDSR
     C*
     C*================================================================
     C*  SR12    Fill subfile 1 page
     C*================================================================
     C*          Read First MDV record
     C*             31 = Bill Message File key with DCS#
     C*             32 = Bill Message File key with EOB#
     C*          -----------------------------------------------------
     C     SR12          BEGSR
     C                   MOVE      SFNXT1        SFNR1
     C     SFNR1         IFEQ      0
     C   31MGKY1         CHAIN     PMG01                              89
     C   32MGKY2         CHAIN     PMG02                              89
     C                   ENDIF                                                  SFNR1 = 0
     C                   Z-ADD     0             XXNR1             3 0
     C*
     C*          Load 10 records to subfile
     C*             89 = End of read, End of Subfile
     C*          -----------------------------------------------------
     C     XXNR1         DOWLT     10
     C     *IN89         ANDEQ     *OFF
     C*
     C*             If source code = R: Is error on input or item
     C*             --------------------------------------------------
     C     MGSRC         IFEQ      'R'
     C                   SUBST     MGMSFX:6      HIMM1             2
     C                   CLEAR                   WL
     C*
     C*                Move in appropriate message source text
     C*                -----------------------------------------------
     C     MGSRC         IFEQ      'R'
     C                   MOVEL(P)  CIDR          SCID
     C                   ENDIF                                                  MGSRC = R
     C*
     C*                Error on input to HPR: get message from array
     C*                -----------------------------------------------
     C     HIMM1         IFEQ      'IN'
     C*
     C*                   Initialize key and message area
     C*                   --------------------------------------------
     C                   MOVEL     MGMSG#        XXMSG#
     C                   MOVEL     MGMSG#        SCMSG#
     C                   CLEAR                   WL
     C                   MOVEL(P)  COM1          SCMSG
     C                   MOVEL(P)  COM1          WL(1)
     C*
     C*                   Get first message record
     C*                   --------------------------------------------
     C     MIKY          CHAIN     PMI01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        SCMSG
     C                   Z-ADD     1             X                 3 0
     C*
     C*                      Get additional message records
     C*                      -----------------------------------------
     C     *IN50         DOWEQ     *OFF
     C     X             ANDLE     6
     C                   MOVEL(P)  MIDESC        WL(X)
     C     MIKY          READE     PMI01                                  50
     C                   ADD       1             X
     C                   ENDDO                                                  50 = Off
     C                   ENDIF                                                  50 = Off
     C*
     C*                Error on item from HPR:
     C*                Initialize key to get message text from HPR
     C*                -----------------------------------------------
     C                   ELSE                                                   HIMM1 = IN
     C*
     C                   CLEAR                   HPRMSG
     C                   MOVEL     'N'           CLOSE
     C                   MOVEL     'N'           AFLAG
     C                   MOVEL     DHPRCD        CODE
     C*
     C                   MOVE      *HIVAL        XXCRB#
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CHCRPC        CODE
     C                   ENDIF
     C*
     C                   MOVEL     DHMOD1        MOD
     C                   MOVE      MGMSG#        KEY
     C                   MOVEL     DACRST        STATUS
     C                   MOVEL     MGMSG#        SCMSG#
     C*
     C*                   CALL CodeReview
     C*                   --------------------------------------------
     C                   CALL      'HPRBMSA'
     C                   PARM                    HPRMSG
     C*
     C*                   Move CodeReview messages to screen
     C*                   --------------------------------------------
     C                   MOVEL(P)  MSG1B         SCMSG
     C                   MOVEL(P)  MSG1B         WL(1)
     C                   MOVEL(P)  MSG2          WL(2)
     C                   MOVEL(P)  MSG3          WL(3)
     C                   MOVEL(P)  MSG4          WL(4)
     C                   MOVEL(P)  MSG5          WL(5)
     C                   ENDIF                                                  MGSRC = R
     C                   ENDIF                                                  HIMM1 = IN
     C*
     C*             If source code P or S:
     C*             Get Message from Bill Entry Message ID table
     C*             --------------------------------------------------
     C     MGSRC         IFEQ      'P'
     C     MGSRC         OREQ      'S'
     C     MGSRC         OREQ      'N'
     C     MGSRC         OREQ      'C'
     C*
     C*                Move in appropriate message source text
     C*                -----------------------------------------------
     C     MGSRC         IFEQ      'P'
     C                   MOVEL(P)  CIDP          SCID
     C                   ENDIF                                                  MGSRC = P
     C     MGSRC         IFEQ      'S'
     C                   MOVEL(P)  CIDS          SCID
     C                   ENDIF                                                  MGSRC = S
     C     MGSRC         IFEQ      'N'
     C                   MOVEL(P)  CIDN          SCID
     C                   ENDIF                                                  MGSRC = S
     C     MGSRC         IFEQ      'C'
     C                   MOVEL(P)  CIDC          SCID
     C                   ENDIF                                                  MGSRC = C
     C*
     C*                Initialize key and message area
     C*                -----------------------------------------------
     C                   MOVEL     MGMSG#        XXMSG#
     C                   MOVEL     MGMSG#        SCMSG#
     C                   CLEAR                   WL
     C                   MOVEL(P)  COM1          SCMSG
     C                   MOVEL(P)  COM1          WL(1)
     C*
     C*                Get first message record
     C*                -----------------------------------------------
     C     MIKY          CHAIN     PMI01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        SCMSG
     C                   Z-ADD     1             X                 3 0
     C*
     C*                   Get additional message records
     C*                   --------------------------------------------
     C     *IN50         DOWEQ     *OFF
     C     X             ANDLE     6
     C                   MOVEL(P)  MIDESC        WL(X)
     C     MIKY          READE     PMI01                                  50
     C                   ADD       1             X
     C                   ENDDO                                                  50 = Off
     C                   ENDIF                                                  50 = Off
     C                   ENDIF                                                  MGSRC = P,S
     C*** Beg Add *** 06/11/12 ****************************************
      /Free

          //If source code K:
          //Get Message from Bill Entry Message ID table
          //--------------------------------------------------
        If mgsrc = 'K';
          Scid = cidk;
        Endif;

          //Initialize key and message area
          //-----------------------------------------------
        xxmsg# = mgmsg#;
        wl = *Blanks;
        Scmsg  = com1;
        wl(1)  = com1;

          //Get first message record
          //-----------------------------------------------
        Chain miky pmi01;
        If %Found(ftmil);

          xxdata= midesc;
          exsr bldmsg;
          scmsg = xxdata;
          x = 1;

          //Get additional message records
          //--------------------------------------------
          Dow not %Eof(ftmil) and x <= 6;

            xxdata= miDesc;
            exsr bldmsg;
            wl(x) = xxdata;
            Reade miky pmi01;
            x = x+1;

          Enddo;

          scmsg# = xxmsg#;

        Endif;
      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C*
     C*             Update count and write subfile record
     C*             --------------------------------------------------
     C                   ADD       1             XXNR1
     C     XXNR1         IFEQ      1
     C     SFNR1         ADD       1             SHRNO1
     C                   ENDIF                                                  XXNR1 = 1
     C                   ADD       1             SFNR1
     C                   WRITE     SCR01S
     C*
     C*             Read next record
     C*             --------------------------------------------------
     C   31MGKY1         READE     PMG01                                  89
     C   32MGKY2         READE     PMG02                                  89
     C                   ENDDO                                                  XXNR1 > 10
     C*
     C*          If data in subfile; display it
     C*             else error
     C*          -----------------------------------------------------
     C                   MOVE      SFNR1         SFNXT1
     C     SFNR1         IFNE      0
     C                   SETON                                        81
     C                   ELSE                                                   SFNR1 /= 0
     C                   EXFMT     WDW02
     C                   SETON                                        LR
     C                   ENDIF                                                  SFNR1 <> 0
     C*
     C*          End of subroutine SR12
     C*          ------------------------------------------------------
     C     SR12X         ENDSR
     C*** Beg Add *** 06/11/12 ****************************************
      /Free

       Begsr bldmsg;

          Chain khky pkh01;
             If %found(fdkhl2);
             xxloc = 0;
             xxloc = %scan(xxflxx:xxdata);
                If xxloc <> 0;
                  xxdata = %replace(khprcd:xxdata: %scan(xxflxx:xxdata));
                Endif;
             xxloc = 0;
             xxloc = %scan(xxflyy:xxdata);
                If xxloc <> 0;
                  xxdata = %replace(khmcod:xxdata: %scan(xxflyy:xxdata));
                Endif;
             Endif;

       Endsr;
      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C*
