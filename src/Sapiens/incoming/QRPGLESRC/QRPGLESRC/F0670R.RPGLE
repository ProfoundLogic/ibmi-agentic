     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0670R - BILL ENTRY MESSAGE DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program presents the user with a subfile list of   **
     H**     messages associated with a bill detail line. The sub-   **
     H**     file contains a short description and allows the option **
     H**     to display an extended description. When this option is **
     H**     taken, a window that contains up to 5 lines of extended **
     H**     description is presented.                               **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED: 12/10/96         PROJECT: MC119                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F0070R  - Bill Entry                                    **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     HPRBMSA - HPR CodeReview Message Description Retrevial  **
     H**                                                             **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                  **
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
     FF0670F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:SFNR1)
     FFDMGL     IF   E           K DISK
     FFDMGL1    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     FFMRCL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     FFTMIL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*** BEGIN ADD *** 12/15/97 **************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** END ADD *** 12/15/97 ****************************************

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
     D HPRM            DS
     D  HMCFF                  1      1
     D  HMKB                   2      3
     D  HMEDMM                 4      5
     D  HFIL12                 6      6
     D  HMEDDD                 7      8
     D  HFIL13                 9      9
     D  HMEDCC                10     11
     D  HMEDYY                12     13
     D  HMKEF                 14     14
     D  HMDEF                 15     15
     D  HMAF                  16     16
     D  HMPI                  17     24
     D  HMPARI                25     28
     D  HMFI                  29     35
     D  HFIL14                36     42
     D  HMFS                  43     44
     D  HIM2CN                45     86
     D  HIM2CD                87     89
     D  HIMC1                 90     94
     D  HIMM1                 95     96
     D  HIMK1                 97    101
     D  HICSI                102    102
     D  HOM1A                103    112
     D  HOM1B                113    178
     D  HOM12                179    244
     D  HOM13                245    310
     D  HOM14                311    376
     D  HOM15                377    442
     D  HIMC2                443    447
     D  HIMM2                448    449
     D  HIMK2                450    454
     D  HICS2                455    455
     D  HOM2A                456    465
     D  HOM2B                466    531
     D  HOM22                532    597
     D  HOM23                598    663
     D  HOM24                664    729
     D  HOM25                730    795
     D  HIMC3                796    800
     D  HIMM3                801    802
     D  HIMK3                803    807
     D  HICS3                808    808
     D  HOM3A                809    818
     D  HOM3B                819    884
     D  HOM32                885    950
     D  HOM33                951   1016
     D  HOM34               1017   1082
     D  HOM35               1083   1148
     D  HFIL15              1149   1200
     D*
     D*----------------------------------------------------------------
     D* Named constant defines
     D*----------------------------------------------------------------
     D COM1            C                   CONST('MESSAGE NOT FOUND')
     D CIDC            C                   CONST('HPR CodeReview')
     D CIDP            C                   CONST('Pre-Certification')
     D CIDS            C                   CONST('State Adjudication')
     D*** BEGIN ADD *** 12/15/97 **************************************
     D TITLE           C                   CONST('Work with Bill Entry-
     D                                      Error Messages')

     I*** END ADD *** 12/15/97 ****************************************
      /EJECT

     C*----------------------------------------------------------------
     C*        Entry parm list
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQDCS#            5
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQCODE           11
     C                   PARM                    QQPMOD            2
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQPART            2
     C                   PARM                    QQIPOP            2
     C                   PARM                    QQTOS3            3
     C                   PARM                    QQSEQ#            5
     C                   PARM                    QQTOTC            9
     C                   PARM                    QQTOTP            9
     C                   PARM                    QQAMT1            9
     C                   PARM                    QQAMT2            9
     C                   PARM                    QQAMT3            9
     C                   PARM                    QQEOMB            3
     C                   PARM                    QQZIPA            3
     C                   PARM                    QQDSEQ            5
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
     C     *LIKE         DEFINE    MGCO#         XXCO#
     C     *LIKE         DEFINE    MGSUB#        XXSUB#
     C     *LIKE         DEFINE    MGFND         XXFND
     C     *LIKE         DEFINE    MGFYR         XXFYR
     C     *LIKE         DEFINE    MGCASE        XXCASE
     C     *LIKE         DEFINE    MGDCS#        XXDCS#
     C*** BEGIN CHG *** 12/15/97 **************************************
     C********** *LIKE     DEFN MGSEQ#    XXSEQ#
     C     *LIKE         DEFINE    MGDSEQ        XXSEQ#
     C*** END CHG *** 12/15/97 ****************************************
     C*
     C     MGKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXSEQ#
     C     *LIKE         DEFINE    MGEOB#        XXEOB#
     C*
     C*          PMI01 - Bill Entry Message ID
     C     MIKY          KLIST
     C*** BEGIN DEL *** 12/15/97 **************************************
     C**********           KFLD           XXCO#
     C*** END DEL *** 12/15/97 ****************************************
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
     C*** BEGIN ADD *** 12/15/97 **************************************
     C     RCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C*          *LIKE     DEFN SCFDAT    XXFDAT
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C     *LIKE         DEFINE    RCCPT4        XXCPT4
     C     *LIKE         DEFINE    RCEFDT        XXEFDT
     C*
     C*          FTABL - Logical over Table Files
     C*          PEB01 - EOMB Description Table
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C     *LIKE         DEFINE    EBKEY         XXKEY
     C*
     C*** BEGIN ADD *** 12/15/97 **************************************
     C*          SSYPL - Logical over Fee Schedule Control File
     C     BPKEY         KLIST
     C                   KFLD                    XBKEY             2
     C*** END ADD *** 12/15/97 ****************************************
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
     C*          Execute SR00 for initialization
     C*          ------------------------------------------------------
     C                   EXSR      SR00
     C*
     C*          Loop until F12 entered
     C*          ------------------------------------------------------
     C*
     C*** BEGIN CHG *** 12/15/97 **************************************
     C     *INKL         DOWEQ     *OFF
     C     *INLR         ANDEQ     *OFF
     C*** END CHG *** 12/15/97 ****************************************
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
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   MOVEL(P)  'WDW01'       HDFRMT            5
     C*** END ADD *** 12/15/97 ****************************************
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
     C*** BEGIN ADD *** 12/15/97 **************************************
     C     HDFRMT        IFNE      'WDW01'
     C     *IN20         ANDEQ     *OFF
     C                   LEAVE
     C                   ENDIF                                                  HDFRMT IFEQ WDW01
     C                   MOVE      *BLANKS       HDFRMT
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C*             End of F3 loop
     C*             ---------------------------------------------------
     C                   ENDDO                                                  F3 = Off
     C*
     C*          Close HPR files
     C*          ------------------------------------------------------
     C*** BEGIN ADD *** 12/15/97 **************************************
     C     HPRFLG        IFEQ      '1'
     C*** END ADDD *** 12/15/97 ***************************************
     C     'FILE'        CAT       'CLOSE'       HPRCAL            9
     C                   CALL      HPRCAL
     C*** BEGIN ADD 12/15/97 ******************************************
     C                   ENDIF                                                  HPRFLG EQ 1
     C*** END ADD *** 12/15/97 ****************************************
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQDCS#        XXDCS#
     C                   MOVE      QQEOB#        XXEOB#
     C                   MOVE      QQZIPA        XXZIPA
     C                   MOVE      QQDSEQ        XXSEQ#
     C                   MOVEL     QQCODE        XXCPT4
     C                   MOVE      QQFDAT        XXEFDT
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   MOVEL(P)  QQEOMB        XXKEY
     C*
     C*          Get company name from Company Master
     C*          ------------------------------------------------------
     C     XXCO#         CHAIN     PCO01                              50
     C*** BEGIN DEL *** 12/15/97 **************************************
     C**********           MOVELCONAME    SCCONM    P
     C*** END DEL *** 12/15/97 ****************************************
     C*
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCCONM
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C*          Get CPT4 Description from Reasonable/Customary File
     C*          ------------------------------------------------------
     C*** BEGIN CHG *** 12/15/97 **************************************
     C********** RCKY      CHAINPRC01                50
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  50
     C*** END CHG *** 12/15/97 ****************************************
     C                   MOVEL(P)  RCDSC1        SCDSC1
     C*
     C*          Get EOB Description from EOMB Description Table
     C*          ------------------------------------------------------
     C     TBKY          CHAIN     PEB01                              50
     C                   MOVEL(P)  EBDESC        SCEOD1
     C*
     C*          Get time
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C*
     C*          Move user name and parameters to screen fields
     C*          ------------------------------------------------------
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      QQPMOD        SCMOD
     C                   MOVEL     QQFDAT        XXFDAT            6 0
     C*** BEG ADD *** 07/09/98 ***************************************
     C                   Z-ADD     XXFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDAT
     C*** END ADD *** 07/09/98 ***************************************
     C                   MOVE      QQPART        SCPART
     C                   MOVE      QQIPOP        SCIPOP
     C                   MOVE      QQTOS3        SCTOS3
     C                   MOVE      QQDSEQ        SCDSEQ
     C                   MOVE      QQTOTC        SCTOTC
     C                   MOVE      QQTOTP        SCTOTP
     C                   MOVE      QQAMT1        SCAMT1
     C                   MOVE      QQAMT2        SCAMT2
     C                   MOVE      QQAMT3        SCAMT3
     C                   MOVE      QQEOMB        SCEOB1
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   MOVEL(P)  TITLE         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL     ##DATA        SCPRDS
     C*** END ADD *** 12/15/97 ****************************************
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
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   MOVE      *BLANKS       HPRFLG            1
     C                   MOVE      'FB'          XBKEY
     C     BPKEY         CHAIN     SFB01                              99
     C     *IN99         IFEQ      '0'
     C     FBFL04        ANDNE     *BLANKS
     C                   MOVE      '1'           HPRFLG                         HPR INSTALLED
     C*** END ADD *** 12/15/97 ****************************************
     C     'FILE'        CAT       'OPEN '       HPRCAL
     C                   CALL      HPRCAL
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   ENDIF                                                  IN99 EQ 0
     C*** END ADD *** 12/15/97 ****************************************
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
     C*             If source code = C: Is error on input or item
     C*             --------------------------------------------------
     C     MGSRC         IFEQ      'C'
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           SUBSTMGMSGH:1  HIMC1
     C**********           SUBSTMGMSGH:6  HIMM1
     C**********           SUBSTMGMSGH:8  HIMK1
     C                   SUBST     MGMSFX:1      HIMC1
     C                   SUBST     MGMSFX:6      HIMM1
     C                   SUBST     MGMSFX:8      HIMK1
     C*** END CHG *** 12/15/97 ****************************************
     C                   CLEAR                   WL
     C*
     C*                Move in appropriate message source text
     C*                -----------------------------------------------
     C     MGSRC         IFEQ      'C'
     C                   MOVEL(P)  CIDC          SCID
     C                   ENDIF                                                  MGSRC = C
     C*
     C*                Error on input to HPR: get message from array
     C*                -----------------------------------------------
     C     HIMM1         IFEQ      'IN'
     C*
     C*                   Initialize key and message area
     C*                   --------------------------------------------
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           MOVELMGMSGP    XXMSG#
     C                   MOVEL     MGMSG#        XXMSG#
     C*** END CHG *** 12/15/97 ****************************************
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
     C     X             ANDLE     5
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
     C                   MOVE      '99'          HMKB
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           MOVE MGTRDT    XXTRDT  6
     C                   MOVE      QQFDAT        XXTRDT            8
     C*** END CHG *** 12/15/97 ****************************************
     C*
     C                   SUBST     XXTRDT:3      HMEDMM
     C                   SUBST     XXTRDT:1      HMEDDD
     C                   SUBST     XXTRDT:5      HMEDCC
     C                   SUBST     XXTRDT:7      HMEDYY
     C                   MOVE      MGCSI         HICSI
     C*
     C*                   CALL CodeReview
     C*                   --------------------------------------------
     C                   CALL      'HPRBMSA'
     C                   PARM                    HPRM
     C*
     C*                   Move CodeReview messages to screen
     C*                   --------------------------------------------
     C                   MOVEL(P)  HOM1B         SCMSG
     C                   MOVEL(P)  HOM1B         WL(1)
     C                   MOVEL(P)  HOM12         WL(2)
     C                   MOVEL(P)  HOM13         WL(3)
     C                   MOVEL(P)  HOM14         WL(4)
     C                   MOVEL(P)  HOM15         WL(5)
     C                   ENDIF                                                  MGSRC = C
     C                   ENDIF                                                  HIMM1 = IN
     C*
     C*             If source code P or S:
     C*             Get Message from Bill Entry Message ID table
     C*             --------------------------------------------------
     C     MGSRC         IFEQ      'P'
     C     MGSRC         OREQ      'S'
     C*
     C*                Move in appropriate message source text
     C*                -----------------------------------------------
     C     MGSRC         IFEQ      'P'
     C                   MOVEL(P)  CIDP          SCID
     C                   ENDIF                                                  MGSRC = P
     C     MGSRC         IFEQ      'S'
     C                   MOVEL(P)  CIDS          SCID
     C                   ENDIF                                                  MGSRC = S
     C*
     C*                Initialize key and message area
     C*                -----------------------------------------------
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           MOVELMGMSGP    XXMSG#
     C                   MOVEL     MGMSG#        XXMSG#
     C*** END CHG *** 12/15/97 ****************************************
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
     C     X             ANDLE     5
     C                   MOVEL(P)  MIDESC        WL(X)
     C     MIKY          READE     PMI01                                  50
     C                   ADD       1             X
     C                   ENDDO                                                  50 = Off
     C                   ENDIF                                                  50 = Off
     C                   ENDIF                                                  MGSRC = P,S
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
     C*
