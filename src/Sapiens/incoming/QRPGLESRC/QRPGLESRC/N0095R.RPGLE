     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0095R - GENERIC PROVIDER ASSIGNMENT DISPLAY
     H*****************************************************************
     H**                                                             **
     H**   *------------------------------------------------------*  **
     H**   |  COPY THIS PROGRAM TO THE CLIENT'S LIBRARY & COMPILE |  **
     H**   |  REASON: It uses FMPDP and FMP1P and these files can |  **
     H**   |          be client unique.                           |  **
     H**   *------------------------------------------------------*  **
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program presents the user with a subfile list of   **
     H**     all vendor records that could not be assigned directly  **
     H**     into the PDS data base. The input records are read from **
     H**     a generic vendor file (NAGPP) and are selected by vendor**
     H**     group. The group selected is determined by an input     **
     H**     parm. The assignment function is done by a called,      **
     H**     vendor specific, program.                               **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     Nvvvvv -  Where 'vvvvv' is the vendor group             **
     H**                                                             **
     H**   PROGRAMMER: 31                                            **
     H**   CREATED: 03/06/97         PROJECT: 87MBQ                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*
     H*  06/17/99  C:53 MC145                                           *
     H*            QUALIFY WCMS FILE W/NON-PDS WCMS FLAG.               *
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      10       Screen Help key.                              **
     H**      11       Screen Valid command key.                     **
     H**      18       Screen ROLLUP command key.                    **
     H**      20       Screen 1 - Invalid option.                    **
     H**               Screen 2 - Error has occured.                 **
     H**      21       Screen 2 - Invalid option.                    **
     H**      22       Screen 2 - Provider has no assigned contract. **
     H**      23       Screen 2 - Missing Auxiliary record.          **
     H**      25       Screen 2 - 10 Contracts already assigned.     **
     H**      31       Vendor input is not for a PPO                 **
     H**      39       Process WC Demographics file (WMPDP).         **
     H**      50       General usage.                                **
     H**      61       Current contract was assigned.                **
     H**      80       Subfile 1 clear.                              **
     H**      81       Subfile 1 display.                            **
     H**      89       Subfile 1 end.                                **
     H**      90       Subfile 2 clear.                              **
     H**      91       Subfile 2 display.                            **
     H**      99       Subfile 2 end.                                **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**   FMPDL    (PPD01): Payee Demographics File (FEIN/MOD)      **
     H**   FMP1L    (PP101): Provider Auxiliary File (FEIN/MOD)      **
     H**   NAGPL1   (PGP01): Generic Provider File (VNCD/FEIN/IDNR)  **
     H**   SMCOL    (PC001): System Company Master File (CO#)        **
     H**   SMUPL    (PUP01): User Parameter File (USR)               **
     H**   SSYLF           : System & Application Control File (KEY) **
     H**            (SSY01): System Control File                     **
     H**   WMPDL    (PPD02): Payee Demographics File (FEIN/MOD)      **
     H*****************************************************************
     H*
     FN0095F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:SFNR1)
     F                                     SFILE(SCR02S:SFNR2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNAGPL1    IF   E           K DISK
     FFMPDL     UF A E           K DISK
     FFMP1L     UF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWMPDL     UF A E           K DISK    USROPN
     F                                     RENAME(PPD01:PPD02)

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
     D* FMP1P array entries
     D*----------------------------------------------------------------
     D                 DS
     D  N#                     1    150
     D                                     DIM(10)                              Network number
     D  P1N#01                 1     15
     D  P1N#02                16     30
     D  P1N#03                31     45
     D  P1N#04                46     60
     D  P1N#05                61     75
     D  P1N#06                76     90
     D  P1N#07                91    105
     D  P1N#08               106    120
     D  P1N#09               121    135
     D  P1N#10               136    150
     D*
     D*----------------------------------------------------------------
     D* Zip code work area
     D*----------------------------------------------------------------
     D                 DS
     D  XXZIP                  1     10
     D  XXZIP1                 1      5
     D  XXZIPX                 6      6    INZ('-')
     D  XXZIP2                 7     10
     D*
     D*----------------------------------------------------------------
     D* Help request work area
     D*----------------------------------------------------------------
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      /EJECT

     C*----------------------------------------------------------------
     C*        Entry parm list
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQVEND            5
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
     C*          PPD01 - Payee Demographic Mstr File (Managed Care)
     C*          PPD02 - Payee Demographic Mstr File (Worker's Comp)
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PDMOD         XHMOD
     C*
     C     PDKY1         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*          PP101 - Provider Auxiliary File
     C     P1KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*          PGP01 - Generic Provider File
     C     GPKY          KLIST
     C                   KFLD                    X1VNCD
     C                   KFLD                    X1FEIN
     C     *LIKE         DEFINE    GPVNCD        X1VNCD
     C     *LIKE         DEFINE    GPFEIN        X1FEIN
     C*
     C*----------------------------------------------------------------
     C*          Defines
     C*----------------------------------------------------------------
     C     *LIKE         DEFINE    SHRNO1        SFNR1
     C     *LIKE         DEFINE    SHRNO1        SFNXT1
     C     *LIKE         DEFINE    SHRNO2        SFNR2
     C     *LIKE         DEFINE    SHRNO2        SFNXT2
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
     C*          Loop until F3 entered
     C*          ------------------------------------------------------
     C     *INKC         DOWEQ     *OFF
     C     *INLR         ANDEQ     *OFF
     C*
     C*             Write/read subfile 1
     C*             ---------------------------------------------------
     C                   EXFMT     SCR01C
     C*
     C*             Refresh screen 1 if F5
     C*                or FEIN search is requested
     C*             ---------------------------------------------------
     C     *INKE         IFEQ      *ON
     C     SCNEXT        ORNE      *BLANKS
     C                   EXSR      SR11
     C                   EXSR      SR12
     C                   ITER
     C                   ENDIF                                                  F5 = *ON
     C*
     C*             Read changed subfile records
     C*             ---------------------------------------------------
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C*
     C*                Option 1 - (SR21) Clear subfile 2
     C*                           (SR22) Fill  subfile 2 page
     C*                           (SR23) Process subfile 2
     C*                   61 = Contract assigned in screen 2
     C*                ------------------------------------------------
     C     SCOPT1        IFEQ      '1'
     C                   SETOFF                                       61
     C                   EXSR      SR21
     C                   EXSR      SR22
     C                   EXSR      SR23
     C     SFNR1         CHAIN     SCR01S                             50
     C   61              MOVE(P)   'Assigned'    SC1MS1
     C                   MOVE      *BLANK        SCOPT1
     C                   UPDATE    SCR01S
     C                   Z-ADD     SFNR1         SHRNO1
     C                   WRITE     SCR01R
     C*
     C*                   Terminate if F3 on screen 2
     C*                   ---------------------------------------------
     C   LR              LEAVE
     C*
     C*                Invalid option - error MCR0475
     C*                ------------------------------------------------
     C                   ELSE                                                   OPT=1
     C     SCOPT1        COMP      *BLANK                             2020
     C                   UPDATE    SCR01S
     C                   Z-ADD     SFNR1         SHRNO1
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
     C*             End of F3 loop
     C*             ---------------------------------------------------
     C                   ENDDO                                                  F3 = Off
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
     C*          Get company # from User Parameter file
     C*          ------------------------------------------------------
     C     SR00          BEGSR
     C     Q1USER        CHAIN     PUP01                              50
     C   50              MOVE      *ZERO         UPCO#
     C*
     C*          Get company name from Company Master
     C*          ------------------------------------------------------
     C     UPCO#         CHAIN     PCO01                              50
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*          Get time
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C*
     C*          Move system values and parameters to screen fields
     C*          ------------------------------------------------------
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQVEND        SCVNDR
     C                   MOVEL(P)  QQVEND        X1VNCD
     C*
     C*          Determine if interface with Worker's Comp
     C*             39: Process WC Demographics file WMPDP
     C*          ------------------------------------------------------
     C     'SY'          CHAIN     SSY01                              50
     C     *IN50         IFEQ      *OFF
     C     SYWCIF        ANDEQ     'Y'
     C     SYFL05        ANDNE     'Y'                                          ADD 06/17/99
     C                   SETON                                        39
     C                   OPEN      WMPDL
     C                   ENDIF                                                  IN50 = Off
     C*
     C*          Create vendor program name
     C*          ------------------------------------------------------
     C     'N'           CAT(P)    QQVEND        XXPGM             8
     C*
     C*          Move in value for first pass
     C*          ------------------------------------------------------
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*          Initialize first page of subfile 1
     C*          ------------------------------------------------------
     C                   EXSR      SR11
     C                   EXSR      SR12
     C     SFNR1         COMP      0                                      LR
     C*
     C*          End of subroutine SR00
     C*          ------------------------------------------------------
     C     SR00X         ENDSR
     C*
     C*================================================================
     C*  SR11    Clear Subfile 1
     C*================================================================
     C*          Position first record in the file
     C*             Start of file if no FEIN entered
     C*             otherwise start with FEIN entered
     C*          ------------------------------------------------------
     C     SR11          BEGSR
     C     SCNEXT        IFEQ      *BLANKS
     C                   MOVE      *LOVAL        X1FEIN
     C                   ELSE                                                   SCNEXT = Blanks
     C                   MOVEL(P)  SCNEXT        X1FEIN
     C                   MOVE      *BLANKS       SCNEXT
     C                   END                                                    SCNEXT = Blanks
     C     GPKY          SETLL     PGP01                              50
     C*
     C*          Clear subfile
     C*          ------------------------------------------------------
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
     C*          Read First record
     C*          -----------------------------------------------------
     C     SR12          BEGSR
     C                   MOVE      SFNXT1        SFNR1
     C     SFNR1         IFEQ      0
     C     QQVEND        READE     PGP01                                  89
     C                   ENDIF                                                  SFNR1 = 0
     C                   Z-ADD     0             XXNR1             3 0
     C*
     C*          Load 13 records to subfile
     C*             89 = End of read, End of Subfile
     C*          -----------------------------------------------------
     C     XXNR1         DOWLT     13
     C     *IN89         ANDEQ     *OFF
     C     GPDTYP        COMP      ' '                                3131
     C                   MOVEL(P)  GPFEIN        SCFEIN
     C                   MOVEL(P)  GPIDNR        SHIDNR
     C                   MOVEL(P)  GPIDNR        SCIDNR
     C                   MOVEL(P)  GPFNME        SCFNME
     C                   MOVEL(P)  GPNAME        SHPNAM
     C                   MOVEL(P)  GPTYPE        SHFTYP
     C                   MOVEL(P)  GPSPEC        SHSPC1
     C                   MOVEL(P)  GPADR1        SHPAD1
     C                   MOVEL(P)  GPCITY        SHPCTY
     C                   MOVEL(P)  GPST          SHPST
     C                   MOVEL(P)  GPZIP         SHPZIP
     C                   MOVEL(P)  GPBILL        SHBADR
     C                   MOVEL(P)  GPGSEQ        SHGSEQ
     C*
     C*             Update count and write subfile record
     C*             --------------------------------------------------
     C                   MOVE      *BLANKS       SC1MS1
     C                   ADD       1             XXNR1
     C     XXNR1         IFEQ      1
     C     SFNR1         ADD       1             SHRNO1
     C                   ENDIF                                                  XXNR1 = 1
     C                   ADD       1             SFNR1
     C                   WRITE     SCR01S
     C*
     C*             Read next record
     C*             --------------------------------------------------
     C     QQVEND        READE     PGP01                                  89
     C                   ENDDO                                                  XXNR1 > 13
     C*
     C*          If data in subfile; display it
     C*             else error
     C*          -----------------------------------------------------
     C                   MOVE      SFNR1         SFNXT1
     C     SFNR1         IFNE      0
     C                   SETON                                        81
     C                   ELSE                                                   SFNR1 /= 0
     C                   EXFMT     WDW01
     C                   SETON                                        LR
     C                   ENDIF                                                  SFNR1 /= 0
     C*
     C*          End of subroutine SR12
     C*          ------------------------------------------------------
     C     SR12X         ENDSR
     C*
     C*================================================================
     C*  SR21    Clear Subfile 2
     C*================================================================
     C*
     C     SR21          BEGSR
     C                   SETON                                        90
     C                   WRITE     SCR02C
     C                   SETOFF                                       9091
     C                   Z-ADD     0             SFNR2
     C                   Z-ADD     0             SFNXT2
     C                   WRITE     SCR02R
     C*
     C*          Fill heading data fields
     C*          -----------------------------------------------------
     C                   MOVEL     SHFTYP        SCFTYP
     C                   MOVEL     SHSPC1        SCSPC1
     C                   MOVEL     SHPNAM        SCPNAM
     C                   MOVEL     SHPAD1        SCPAD1
     C                   MOVEL     SHPCTY        SCPCTY
     C                   MOVEL     SHPST         SCPST
     C                   MOVEL     SHPZIP        SCPZIP
     C                   MOVEL     SHBADR        SCBADR
     C*
     C*          End of subroutine SR21
     C*          ------------------------------------------------------
     C     SR21X         ENDSR
     C*
     C*================================================================
     C*  SR22    Fill subfile 2 page
     C*================================================================
     C*          Read next MDV record
     C*             31 = MDV History Provider File
     C*             32 = MDV New Provider File
     C*          -----------------------------------------------------
     C     SR22          BEGSR
     C                   MOVE      SFNXT2        SFNR2
     C     SFNR2         IFEQ      0
     C                   MOVE      SCFEIN        XXFEIN
     C     PDKY          CHAIN     PPD01                              99
     C                   ELSE
     C                   MOVE      XHMOD         XXMOD
     C  N99PDKY1         SETGT     PPD01                              99
     C  N99              READ      PPD01                                  99
     C  N99XXFEIN        COMP      PDFEIN                             9999
     C                   ENDIF                                                  SFNR2 = 0
     C                   Z-ADD     0             XXNR2             3 0
     C  N99              EXCEPT    PPD01E
     C*
     C*          Load 2 records to subfile
     C*             99 = End of read, End of Subfile
     C*          -----------------------------------------------------
     C     XXNR2         DOWLT     2
     C     *IN99         ANDEQ     *OFF
     C*
     C*             Load data from Payee Demographics File
     C*             --------------------------------------------------
     C                   MOVE      PDMOD         SCMOD
     C                   MOVE      PDMOD         XHMOD
     C                   MOVE      PDSTAT        SCSTAT
     C   31              MOVEL     PDPARN        SCPARN
     C                   MOVEL     PDPARN        SHPARN
     C                   MOVEL     PDNAME        SC1NAM
     C                   MOVEL     PDTYPE        SCTYPE
     C                   MOVEL     PDZIP         XXZIP1
     C                   MOVE      PDZIP         XXZIP2
     C                   MOVEL     *BLANKS       SC1ADR
     C                   MOVEL     *BLANKS       XXWRK1
     C     PDADR1        CAT       ',':0         SC1ADR
     C     SC1ADR        CAT(P)    PDCTY:1       XXWRK1           60
     C     XXWRK1        CAT       ',':0         SC1ADR
     C     SC1ADR        CAT(P)    PDST:1        XXWRK1
     C     XXWRK1        CAT(P)    XXZIP:1       SC1ADR
     C*
     C*             Load data from Provider Auxiliary File
     C*             --------------------------------------------------
     C                   MOVE      PDFEIN        XXFEIN
     C                   MOVE      PDMOD         XXMOD
     C     P1KY          CHAIN     PP101                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     P1NAME        SC2NAM
     C                   MOVEL     P1ZIP         XXZIP1
     C                   MOVE      P1ZIP         XXZIP2
     C                   MOVEL     *BLANKS       SC2ADR
     C                   MOVEL     *BLANKS       XXWRK1
     C     P1ADR1        CAT       ',':0         SC2ADR
     C     SC2ADR        CAT       P1CTY:1       XXWRK1
     C     XXWRK1        CAT       ',':0         SC2ADR
     C     SC2ADR        CAT       P1ST:1        XXWRK1
     C     XXWRK1        CAT       XXZIP:1       SC2ADR
     C                   MOVE      P1ST          SCST
     C     SC1NAM        COMP      SC2NAM                                 50
     C   50              MOVEL(P)  'SAME'        SC2NAM
     C     SC1ADR        COMP      SC2ADR                                 50
     C   50              MOVEL(P)  'SAME'        SC2ADR
     C*
     C*                Insure status and network match Demographics
     C*                -----------------------------------------------
     C     PDSTAT        IFNE      P1STAT
     C     PDPARN        ORNE      P1PARN
     C                   MOVE      PDSTAT        P1STAT
     C                   MOVE      PDPARN        P1PARN
     C                   MOVEL     Q1USER        P1USER
     C                   MOVEL     Q1PGM         P1PGM
     C                   Z-ADD     DSTIME        P1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C                   UPDATE    PP101
     C                   ELSE                                                   PDSTAT /= P1STAT
     C                   EXCEPT    PP101E
     C                   ENDIF                                                  PDSTAT /= P1STAT
     C                   ELSE                                                   50 = Off
     C                   MOVE      *BLANKS       SC2NAM
     C                   MOVE      *BLANKS       SC2ADR
     C                   MOVE      *BLANKS       SCST
     C                   ENDIF                                                  50 = Off
     C*
     C*             Update count and write subfile record
     C*             --------------------------------------------------
     C                   MOVE      *BLANKS       SC2MS1
     C                   MOVE      *BLANKS       SC2MS2
     C                   ADD       1             XXNR2
     C     XXNR2         IFEQ      1
     C     SFNR2         ADD       1             SHRNO2
     C                   ENDIF                                                  XXNR2 = 1
     C                   ADD       1             SFNR2
     C                   WRITE     SCR02S
     C*
     C*             Read next record
     C*             --------------------------------------------------
     C     PDKY          READE     PPD01                                  99
     C  N99              EXCEPT    PPD01E
     C  N99XXFEIN        COMP      PDFEIN                             9999
     C                   ENDDO                                                  XXNR2 < 2
     C*
     C*          Records in subfile: Set it to current page
     C*          -----------------------------------------------------
     C                   MOVE      SFNR2         SFNXT2
     C     SFNR2         IFNE      0
     C                   SETON                                        91
     C                   ELSE                                                   SFNR2 /= 0
     C*
     C*          No records in subfile: Display message
     C*          -----------------------------------------------------
     C                   WRITE     SCR02I
     C                   ENDIF                                                  SFNR2 /= 0
     C*
     C*          End of subroutine SR22
     C*          ------------------------------------------------------
     C     SR22X         ENDSR
     C*
     C*================================================================
     C*  SR23    Process subfile 2
     C*================================================================
     C*          Loop until F3 or F12
     C*          ------------------------------------------------------
     C     SR23          BEGSR
     C     *INKC         DOWEQ     *OFF
     C                   EXFMT     SCR02C
     C   KL              LEAVE
     C*
     C*             If F6: Add FEIN/MOD to existing structures
     C*                Find highest MOD number and add 1
     C*                39 = Use WMPDP to find highest number
     C*             ---------------------------------------------------
     C     *INKF         IFEQ      *ON
     C                   MOVE      *HIVAL        XXMOD
     C                   MOVE      SCFEIN        XXFEIN
     C   39PDKY          SETGT     PPD02                              50
     C   39              READP     PPD02                                  50
     C   39
     CANN50              EXCEPT    PPD02E
     C  N39PDKY          SETGT     PPD01                              50
     C  N39              READP     PPD01                                  50
     C  N39
     CANN50              EXCEPT    PPD01E
     C     PDFEIN        IFEQ      XXFEIN
     C     PDMOD         ADD       1             SCMOD
     C                   ELSE                                                   FEIN = FEIN
     C                   Z-ADD     0             SCMOD
     C                   ENDIF                                                  FEIN = FEIN
     C*
     C*                If new MOD = 0: # could not be assumed error
     C*                ------------------------------------------------
     C     SCMOD         COMP      0                                      22
     C*
     C*                Loop until F12 or Assignment made
     C*                ------------------------------------------------
     C     *INKL         DOWEQ     *OFF
     C                   EXFMT     WDW03
     C   KL              LEAVE
     C                   MOVE      SCMOD         XXMOD
     C   39PDKY1         CHAIN     PPD02                              50
     C  N39PDKY1         CHAIN     PPD01                              50
     C*
     C*                   If MOD used: Duplicate MOD error
     C*                   ---------------------------------------------
     C     *IN50         IFEQ      *OFF
     C   39              EXCEPT    PPD02E
     C  N39              EXCEPT    PPD01E
     C                   SETON                                        21
     C                   ITER
     C                   ELSE                                                   IN50 = Off
     C*
     C*                   MOD available:
     C*                      Write basic record to insure MOD # is not
     C*                      used before the assignment can be made
     C*                   ---------------------------------------------
     C                   MOVE      SCFEIN        PDFEIN
     C                   MOVE      SCMOD         PDMOD
     C                   MOVEL     Q1USER        PDUSER
     C                   MOVEL     Q1PGM         PDPGM
     C                   Z-ADD     DSTIME        PDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C   39              WRITE     PPD02
     C                   WRITE     PPD01
     C*
     C*                      Call Vendor Processing to add provider
     C*                   ---------------------------------------------
     C                   MOVE      '2'           QQTRNS            1
     C                   MOVEL(P)  SHGSEQ        QQGSEQ           15
     C                   MOVEL(P)  SCFEIN        QQFEIN            9
     C                   MOVEL(P)  SCMOD         QQMOD             3
     C                   MOVE(P)   *BLANKS       QQCODE            2
     C                   CALL      XXPGM
     C                   PARM                    QQTRNS
     C                   PARM                    QQGSEQ
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQCODE
     C     QQCODE        IFEQ      '01'
     C                   MOVE(P)   'Assigned'    SC2MS2
     C                   SETON                                        61
     C                   ENDIF                                                  QQCODE = 01
     C                   LEAVE
     C                   ENDIF                                                  IN50 = Off
     C                   ENDDO                                                  INKL = Off
     C*
     C*                Refresh screen
     C*                ------------------------------------------------
     C                   EXSR      SR21
     C                   EXSR      SR22
     C*
     C                   ITER
     C                   ENDIF                                                  INKF = On
     C*
     C*             If F12: Leave subroutine
     C*             If no subfile records go to end of do loop
     C*             ---------------------------------------------------
     C  N91              ITER
     C*
     C*             Read changed subfile records
     C*             ---------------------------------------------------
     C                   READC     SCR02S                                 50
     C     *IN50         DOWEQ     *OFF
     C*
     C*                Process option 1: Assign Status
     C*                ------------------------------------------------
     C     SCOPT2        IFEQ      ' 1'
     C     SCOPT2        OREQ      '1 '
     C                   MOVE      *BLANKS       SCOPT2
     C*
     C*                   Call Vendor program to assign status
     C*                   ---------------------------------------------
     C                   MOVE      '3'           QQTRNS            1
     C                   MOVEL(P)  SHGSEQ        QQGSEQ           15
     C                   MOVEL(P)  SCFEIN        QQFEIN            9
     C                   MOVEL(P)  SCMOD         QQMOD             3
     C                   MOVE(P)   *BLANKS       QQCODE            2
     C                   CALL      XXPGM
     C                   PARM                    QQTRNS
     C                   PARM                    QQGSEQ
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQCODE
     C*
     C*                Return code 01: Assignment was made
     C*                ------------------------------------------------
     C     QQCODE        IFEQ      '01'
     C                   MOVE(P)   'Assigned'    SC2MS2
     C                   SETON                                        61
     C                   ENDIF                                                  QQCODE = 01
     C*
     C*                Return code 02: Already assigned
     C*                ------------------------------------------------
     C     QQCODE        IFEQ      '02'
     C                   MOVE(P)   'Already'     SC2MS1
     C                   MOVE(P)   'Assigned'    SC2MS2
     C                   SETON                                        61
     C                   ENDIF                                                  QQCODE = 02
     C*
     C*                Return code 03: Missing auxiliary record
     C*                ------------------------------------------------
     C     QQCODE        IFEQ      '03'
     C                   SETON                                        23
     C                   LEAVE                                                  >>>>> LEAVE
     C                   ENDIF                                                  QQCODE = 03
     C*
     C*                Return code 04: Provider type mismatch
     C*                ------------------------------------------------
     C     QQCODE        IFEQ      '04'
     C                   SETON                                        24
     C                   LEAVE                                                  >>>>> LEAVE
     C                   ENDIF                                                  QQCODE = 04
     C*
     C*                Return code 05: 10 contracts already assigned
     C*                ------------------------------------------------
     C     QQCODE        IFEQ      '05'
     C                   SETON                                        25
     C                   LEAVE                                                  >>>>> LEAVE
     C                   ENDIF                                                  QQCODE = 05
     C                   ENDIF                                                  SCOPT2 = 1
     C*
     C*                Process option 20: Toggle Status
     C*                ------------------------------------------------
     C     SCOPT2        IFEQ      '20'
     C                   MOVE      *BLANKS       SCOPT2
     C*
     C*                   Flip status: A > I  or  I > A
     C*                   ---------------------------------------------
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE                                                   SCSTAT = A
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF                                                  SCSTAT = A
     C*
     C*                   Update Demographics and Provider Auxiliary
     C*                   ---------------------------------------------
     C                   MOVE      SCMOD         XXMOD
     C     PDKY1         CHAIN     PPD01                              50
     C                   MOVE      SCSTAT        PDSTAT
     C                   MOVEL     Q1USER        PDUSER
     C                   MOVEL     Q1PGM         PDPGM
     C                   Z-ADD     DSTIME        PDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C  N50              UPDATE    PPD01
     C                   MOVE      PDFEIN        XXFEIN
     C                   MOVE      PDMOD         XXMOD
     C     P1KY          CHAIN     PP101                              50
     C                   MOVE      SCSTAT        P1STAT
     C                   MOVEL     Q1USER        P1USER
     C                   MOVEL     Q1PGM         P1PGM
     C                   Z-ADD     DSTIME        P1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C  N50              UPDATE    PP101
     C                   ENDIF                                                  SCOPT2 = 20
     C*
     C*                Process option 5: Toggle Network
     C*                ------------------------------------------------
     C     SCOPT2        IFEQ      '21'
     C     *IN31         ANDEQ     *ON
     C                   MOVE      *BLANKS       SCOPT2
     C*
     C*                   Flip network N > P  or  P > N
     C*                     If switch to P: Aux file must have contract
     C*                   ---------------------------------------------
     C     SHPARN        IFEQ      'N'
     C                   MOVE      PDFEIN        XXFEIN
     C                   MOVE      PDMOD         XXMOD
     C     P1KY          CHAIN     PP101                              50
     C  N50              EXCEPT    PP101E
     C                   SORTA     N#
     C*
     C*                      No contract: Error message
     C*                      ------------------------------------------
     C     N#(10)        IFEQ      *BLANKS
     C                   SETON                                        2022
     C                   LEAVE
     C                   END                                                    N#,10 = blanks
     C                   MOVE      'P'           SCPARN
     C                   MOVE      'P'           SHPARN
     C                   ELSE                                                   SHPARN = N
     C                   MOVE      'N'           SCPARN
     C                   MOVE      'N'           SHPARN
     C                   ENDIF                                                  SHPARN = N
     C*
     C*                   Update Demographics and Provider Auxiliary
     C*                   ---------------------------------------------
     C                   MOVE      SCMOD         XXMOD
     C     PDKY1         CHAIN     PPD01                              50
     C                   MOVE      SHPARN        PDPARN
     C                   MOVEL     Q1USER        PDUSER
     C                   MOVEL     Q1PGM         PDPGM
     C                   Z-ADD     DSTIME        PDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C  N50              UPDATE    PPD01
     C                   MOVE      PDFEIN        XXFEIN
     C                   MOVE      PDMOD         XXMOD
     C     P1KY          CHAIN     PP101                              50
     C                   MOVE      SHPARN        P1PARN
     C                   MOVEL     Q1USER        P1USER
     C                   MOVEL     Q1PGM         P1PGM
     C                   Z-ADD     DSTIME        P1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C  N50              UPDATE    PP101
     C                   ENDIF                                                  SCOPT2 = 21
     C*
     C*                Update screen
     C*                ------------------------------------------------
     C     SCOPT2        COMP      *BLANKS                            2020
     C   20              SETON                                        21
     C                   UPDATE    SCR02S
     C                   Z-ADD     SFNR2         SHRNO2
     C*
     C*                Read next changed record
     C*                ------------------------------------------------
     C                   READC     SCR02S                                 50
     C                   ENDDO                                                  50 = Off
     C*
     C*             If page down, no error, and not LR: Fill next page
     C*             ---------------------------------------------------
     C     *IN18         IFEQ      *ON
     C     *IN20         ANDEQ     *OFF
     C     *INLR         ANDEQ     *OFF
     C                   EXSR      SR22
     C                   ENDIF                                                  IN18 = On
     C                   ENDDO                                                  F3 = Off
     C*
     C*          If F3: Set on LR
     C*          ------------------------------------------------------
     C   KC              SETON                                        LR
     C*
     C*          End of subroutine SR23
     C*          ------------------------------------------------------
     C     SR23X         ENDSR
     O*
     O*================================================================
     O*  Output record releases
     O*================================================================
     O*
     OPPD01     E            PPD01E
     OPPD02     E            PPD02E
     OPP101     E            PP101E
