     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4099R - DWC13 MAINTENANCE
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H**                                                              *
     H** PROGRAM DESCRIPTION:                                         *
     H**                                                              *
     H**    THIS PROGRAM ALLOWS THE USER TO ENTER DWC13 TOTALS AS OF  *
     H**    AN ENTERED DATE FOR A CLAIM.                              *
     H**                                                              *
     H** CALLED FROM: NONE                                            *
     H**                                                              *
     H** CALLS TO: NONE                                               *
     H**                                                              *
     H** PROGRAMMER: LARRY KORZON                                     *
     H** CREATED: 03/24/92    PROJECT: ANM                            *
     H*****************************************************************
     H**                                                              *
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H**                                                              *
     H**  06/28/99 C:61 PROJECT: WC577                                *
     H**               - V3 screen standards applied.                 *
     H**                                                              *
     H**  02/13/95 C:15 PROJECT: WC294                                *
     H**               -CHANGED CODE TO ONLY PROCESS ONE SUMMARY      *
     H**                ENTRY FOR EACH CLAIM.                         *
     H**               -ADDED NEW ENTRIES TO THE SUMMARY SCREEN.      *
     H**                                                              *
     H**  PLEASE ADD TO TOP                                           *
     H*****************************************************************
     H**                                                              *
     H** INDICATOR USAGE:                                             *
     H**   INDICATOR        FUNCTION DESCRIPTION                      *
     H**                                                              *
     H**  SCR01:                                                      *
     H**      20        WCR0517 - INVALID FUND YEAR                   *
     H**      21        WCR0505 - RECORD NOT FOUND                    *
     H**      22        WCR0500 - INVALID DATE                        *
     H**      71        Position cursor to Case #                     *
     H**      72        Position cursor to Run Date                   *
     H**      73        Position cursor to Policy Year                *
     H**      74        Run Date invalid field for search             *
     H**                                                              *
     H**  SCR02:                                                      *
     H**      20        WCR0500 - INVALID DATE                        *
     H**      21        WCR0500 - INVALID DATE                        *
     H**      22        WCR0500 - INVALID DATE                        *
     H**      26        'ON' IF ACC DT < 1/1/94                       *
     H**                                                              *
     H**  GLOBAL:                                                     *
     H**      40        WRITE OR UPDATE FILE                          *
     H**      60        GENERAL USAGE (CHAIN)                         *
     H**                                                              *
     H*****************************************************************
      *
      *
     FW4099F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FWSFRL     UF A E           K DISK
      *
      *
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
      *
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*
     C*  KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR DWC13 SUMMARY FILE
     C*
     C     FRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTYPE
     C*
     C     *LIKE         DEFINE    FRDT          XXDSDT
     C     *LIKE         DEFINE    FRDT          XXRNDT
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    FRTYPE        XXTYPE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*  SETUP AND INITIALIZE FIELDS
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVEL     FNNAME        SCFDNM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
      ***** BEG DEL ******* 06/28/99 **********************************
     C**********
     C***COMMAND 05 = CASE # SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR01'
     C**********           CALL 'W0070R'
     C**********           PARM '2'       @@CALL  1
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM           @@MODE  1
     C**********           PARM *ZEROS    @@RRN   90
     C**********           PARM 'CN'      @@SORT  2
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM SCFYR     @@FYR   30
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZERO
     C**********           GOTO STRT
     C**********           ELSE
     C**********
     C********** @@RRN     CHAINWMCMP                60
     C**********
     C**********           MOVE CMFYR     SCFYR
     C**********           MOVE CMCASE    SCCASE
     C**********           MOVE CMSS#     SCSS#
     C**********
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
     C**********
     C***COMMAND 07 = ALPHA SEARCH
     C**********
     C********** *INKG     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR01'
     C**********           CALL 'W0070R'
     C**********           PARM '2'       @@CALL  1
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM           @@MODE  1
     C**********           PARM *ZEROS    @@RRN   90
     C**********           PARM 'CA'      @@SORT  2
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM SCFYR     @@FYR   30
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZERO
     C**********           GOTO STRT
     C**********           ELSE
     C**********
     C********** @@RRN     CHAINWMCMP                60
     C**********
     C**********           MOVE CMFYR     SCFYR
     C**********           MOVE CMCASE    SCCASE
     C**********           MOVE CMSS#     SCSS#
     C**********
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
     C**********
     C***COMMAND 09 = SS# SEARCH
     C**********
     C********** *INKI     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR01'
     C**********           CALL 'W0070R'
     C**********           PARM '2'       @@CALL  1
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM           @@MODE  1
     C**********           PARM *ZEROS    @@RRN   90
     C**********           PARM 'SS'      @@SORT  2
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM SCFYR     @@FYR   30
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZEROS
     C**********           GOTO STRT
     C**********           ELSE
     C**********
     C********** @@RRN     CHAINWMCMP                60
     C**********
     C**********           MOVE CMFYR     SCFYR
     C**********           MOVE CMCASE    SCCASE
     C**********           MOVE CMSS#     SCSS#
     C**********
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
      ***** END DEL ******* 06/28/99 **********************************
      ***** BEG ADD ******* 06/28/99 **********************************
      *
      *  F10=Search
      *
     C   KJ              EXSR      KJDO                                         F10=Search
      *
      ***** END ADD ******* 06/28/99 **********************************
     C*
     C*  COMMAND 11 = DWC13 SEARCH
     C*
     C     *INKK         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   CALL      'W0569R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFYR         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*****************************
     C*  PROCESS REQUEST SCREEN 1
     C*****************************
     C     SCR01T        TAG
     C*
     C                   MOVE      *BLANKS       XXTYPE
     C*
     C*  VALIDATE FUND YEAR
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  VALIDATE FUND YEAR AND CASE #
     C*
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              21
     C     *IN21         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         #@FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  VALIDATE THE DATE OF TOTALS
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      '1'
     C     XXRNDT        ORGT      XXDSDT
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  LOAD INFORMATION FOR SCR02
     C*
     C     CDKEY         CHAIN     PCD01                              40
     C                   MOVE      *BLANKS       QQLAST
     C     CDFSN         IFEQ      *BLANKS
     C                   MOVEL     CDLSN         SCCNAM
     C                   ELSE
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   END
     C*
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       SCSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   EXSR      SRTYPE
     C*
     C*
     C*  LOAD FIELDS FOR SCREEN 3
     C*
     C     FRKEY         CHAIN     PFR01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PFR01E
     C                   MOVE      UA(2)         SCMODE
     C                   Z-ADD     FRTTWK        SCTTWK
     C                   Z-ADD     FRTTDY        SCTTDY
     C                   Z-ADD     FRTT          SCTT
     C                   Z-ADD     FRTRWK        SCTRWK
     C                   Z-ADD     FRTRDY        SCTRDY
     C                   Z-ADD     FRTR          SCTR
     C                   Z-ADD     FRCAWK        SCCAWK
     C                   Z-ADD     FRCADY        SCCADY
     C                   Z-ADD     FRCA          SCCA
     C                   Z-ADD     FRPTWK        SCPTWK
     C                   Z-ADD     FRPTDY        SCPTDY
     C                   Z-ADD     FRPT          SCPT
     C*
     C                   Z-ADD     FRPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C                   Z-ADD     FRPSWK        SCPSWK
     C                   Z-ADD     FRPSDY        SCPSDY
     C                   Z-ADD     FRPS          SCPS
     C                   Z-ADD     FRDBWK        SCDBWK
     C                   Z-ADD     FRDBDY        SCDBDY
     C                   Z-ADD     FRDB          SCDB
     C                   Z-ADD     FRTP          SCTP
     C                   Z-ADD     FRWL          SCWL
     C                   Z-ADD     FRPI          SCPI
     C                   Z-ADD     FRPE          SCPE
     C                   Z-ADD     FRAV          SCAV
     C                   Z-ADD     FRMD          SCMD
     C                   Z-ADD     FRHP          SCHP
     C                   Z-ADD     FRMT          SCMT
     C                   Z-ADD     FRDG          SCDG
     C                   Z-ADD     FRFR          SCFR
     C                   Z-ADD     FRAC          SCAC
     C                   Z-ADD     FRRH          SCRH
     C                   Z-ADD     FRNS          SCNS
     C                   Z-ADD     FRMC          SCMC
     C*
     C                   Z-ADD     FRWODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWODT
     C*
     C                   Z-ADD     FRWO          SCWO
     C                   Z-ADD     FRTH          SCTH
     C                   Z-ADD     FRSD          SCSD
     C                   Z-ADD     FRTPWK        SCTPWK
     C                   Z-ADD     FRTPDY        SCTPDY
     C                   Z-ADD     FRPIWK        SCPIWK
     C                   Z-ADD     FRPIDY        SCPIDY
     C                   Z-ADD     FRWLWK        SCWLWK
     C                   Z-ADD     FRWLDY        SCWLDY
     C                   Z-ADD     FRMS          SCMS
     C*
     C                   Z-ADD     FRMSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMSDT
     C*
     C                   Z-ADD     FROR          SCOR
     C                   Z-ADD     FRINT         SCINT
     C                   ELSE
     C                   MOVE      UA(1)         SCMODE
     C                   Z-ADD     0             SCTTWK
     C                   Z-ADD     0             SCTTDY
     C                   Z-ADD     0             SCTT
     C                   Z-ADD     0             SCTRWK
     C                   Z-ADD     0             SCTRDY
     C                   Z-ADD     0             SCTR
     C                   Z-ADD     0             SCCAWK
     C                   Z-ADD     0             SCCADY
     C                   Z-ADD     0             SCCA
     C                   Z-ADD     0             SCPTWK
     C                   Z-ADD     0             SCPTDY
     C                   Z-ADD     0             SCPT
     C                   Z-ADD     0             SCPSWK
     C                   Z-ADD     0             SCPSDY
     C                   Z-ADD     0             SCPS
     C                   Z-ADD     0             SCDBWK
     C                   Z-ADD     0             SCDBDY
     C                   Z-ADD     0             SCDB
     C                   Z-ADD     0             SCPTDT
     C                   Z-ADD     0             SCTP
     C                   Z-ADD     0             SCWL
     C                   Z-ADD     0             SCPI
     C                   Z-ADD     0             SCPE
     C                   Z-ADD     0             SCAV
     C                   Z-ADD     0             SCMD
     C                   Z-ADD     0             SCHP
     C                   Z-ADD     0             SCMT
     C                   Z-ADD     0             SCDG
     C                   Z-ADD     0             SCFR
     C                   Z-ADD     0             SCAC
     C                   Z-ADD     0             SCRH
     C                   Z-ADD     0             SCNS
     C                   Z-ADD     0             SCMC
     C                   Z-ADD     0             SCWODT
     C                   Z-ADD     0             SCWO
     C                   Z-ADD     0             SCTH
     C                   Z-ADD     0             SCSD
     C                   Z-ADD     0             SCTPWK
     C                   Z-ADD     0             SCTPDY
     C                   Z-ADD     0             SCPIWK
     C                   Z-ADD     0             SCPIDY
     C                   Z-ADD     0             SCWLWK
     C                   Z-ADD     0             SCWLDY
     C                   Z-ADD     0             SCMS
     C                   Z-ADD     0             SCMSDT
     C                   Z-ADD     0             SCOR
     C                   Z-ADD     0             SCINT
     C                   END
     C*
     C*  SETON *IN26 IF THE ACCIDENT DATE IS LESS THAN 1/1/94.
     C*
     C     CMACDT        COMP      940101                               26
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************
     C*  PROCESS REQUEST SCREEN 3
     C*****************************
     C     SCR03T        TAG
     C*
     C*  VALIDATE PERMANENT TOTAL DATE
     C*
     C     SCPTDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE SETTLEMENT DATE
     C*
     C     SCWODT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCWODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE MEDICAL SETTLEMENT DATE
     C*
     C     SCMSDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCMSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMSDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED, LOAD FIELDS INTO FILE
     C*
     C     FRKEY         CHAIN     PFR01                              40
     C                   Z-ADD     UPCO#         FRCO#
     C                   Z-ADD     SCFND         FRFND
     C                   Z-ADD     SCFYR         FRFYR
     C                   Z-ADD     SCCASE        FRCASE
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRACDT
     C*
     C                   MOVE      XXTYPE        FRTYPE
     C     XXTYPE        IFEQ      'P'
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRDATE
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRDATE
     C*
     C                   END
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRTDT
     C*
     C                   Z-ADD     SCTTWK        FRTTWK
     C                   Z-ADD     SCTTDY        FRTTDY
     C                   Z-ADD     SCTT          FRTT
     C                   Z-ADD     SCTRWK        FRTRWK
     C                   Z-ADD     SCTRDY        FRTRDY
     C                   Z-ADD     SCTR          FRTR
     C                   Z-ADD     SCCAWK        FRCAWK
     C                   Z-ADD     SCCADY        FRCADY
     C                   Z-ADD     SCCA          FRCA
     C                   Z-ADD     SCPTWK        FRPTWK
     C                   Z-ADD     SCPTDY        FRPTDY
     C                   Z-ADD     SCPT          FRPT
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRPTDT
     C*
     C                   Z-ADD     SCPSWK        FRPSWK
     C                   Z-ADD     SCPSDY        FRPSDY
     C                   Z-ADD     SCPS          FRPS
     C                   Z-ADD     SCDBWK        FRDBWK
     C                   Z-ADD     SCDBDY        FRDBDY
     C                   Z-ADD     SCDB          FRDB
     C                   Z-ADD     SCTP          FRTP
     C                   Z-ADD     SCWL          FRWL
     C                   Z-ADD     SCPI          FRPI
     C                   Z-ADD     SCPE          FRPE
     C                   Z-ADD     SCAV          FRAV
     C                   Z-ADD     SCMD          FRMD
     C                   Z-ADD     SCHP          FRHP
     C                   Z-ADD     SCMT          FRMT
     C                   Z-ADD     SCDG          FRDG
     C                   Z-ADD     SCFR          FRFR
     C                   Z-ADD     SCAC          FRAC
     C                   Z-ADD     SCRH          FRRH
     C                   Z-ADD     SCNS          FRNS
     C                   Z-ADD     SCMC          FRMC
     C*
     C                   Z-ADD     SCWODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRWODT
     C*
     C                   Z-ADD     SCWO          FRWO
     C                   Z-ADD     SCTH          FRTH
     C                   Z-ADD     SCSD          FRSD
     C                   Z-ADD     0             FRCL
     C                   Z-ADD     0             FRDA
     C                   MOVEL     Q1USER        FRUSER
     C                   MOVEL     Q1PGM         FRPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRDT
     C*
     C                   Z-ADD     DSTIME        FRTIME
     C                   Z-ADD     SCTPWK        FRTPWK
     C                   Z-ADD     SCTPDY        FRTPDY
     C                   Z-ADD     SCPIWK        FRPIWK
     C                   Z-ADD     SCPIDY        FRPIDY
     C                   Z-ADD     SCWLWK        FRWLWK
     C                   Z-ADD     SCWLDY        FRWLDY
     C                   Z-ADD     SCMS          FRMS
     C*
     C                   Z-ADD     SCMSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRMSDT
     C*
     C                   Z-ADD     SCOR          FROR
     C                   Z-ADD     SCINT         FRINT
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PFR01
     C                   ELSE
     C                   WRITE     PFR01
     C                   END
     C*
     C*  RETURN TO SCREEN ONE
     C*
     C                   Z-ADD     0             SCFYR
     C                   Z-ADD     0             SCCASE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C                   MOVE      *BLANK        XXTYPE
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C**
     C*****************************************************************
     C*  THIS SUBROUTINE CALCULATES WHICH TYPE OF TOTALS ARE ENTERED  *
     C*  ON SCREEN 3.                                                 *
     C*  THIS SUBROUTINE NOW CALCULATES ONLY ONE SUMMARY ENTRY.       *
     C*****************************************************************
     C     SRTYPE        BEGSR
     C*
     C*
     C                   MOVE      'T'           XXTYPE
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDT
     C*
     C                   Z-ADD     SCRNDT        SCTDT
     C                   MOVE      '1'           *IN25
     C*
     C                   ENDSR
      ***** BEG ADD ******* 06/28/99 **********************************
      *
      *
      *
      *===============================================================
      * KJDO - SEARCH
      *===============================================================
      *
     C     KJDO          BEGSR
      *
     C                   MOVE(P)   Q2RECF        XXRECF           10
     C                   SELECT
      *
      *  POLICY YEAR
      *
     C     HFLD          WHENEQ    'SCFYR'
     C                   CALL      'W0011R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZERO         ##YR              3 0
      *
     C     ##YR          IFEQ      *ZERO
     C                   SETON                                        73
      *
     C                   ELSE
      *
     C                   Z-ADD     ##YR          SCFYR
     C                   SETON                                        71
     C                   ENDIF                                                  ##YR
      *
     C                   WRITE     SCR99
      *
      *  CASE SEARCH
      *
     C     HFLD          WHENEQ    'SCCASE'
     C                   CALL      'W0070R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANK        #3MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CN'          ##SORT            2
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      SCFYR         ##FYR             3 0
      *
     C                   WRITE     SCR99
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        71
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     WMCMP                              60
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       SCSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   SETON                                        72
     C                   ENDIF
      *
      * RUN DATE
      *
     C     HFLD          WHENEQ    'SCRNDT'
     C                   SETON                                        74
      * ZZZ
      *
     C                   OTHER
     C                   SETON                                        74
      *
      *
     C                   ENDSL
      *
     C                   MOVE(P)   XXRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      *
      ***** END ADD ******* 06/28/99 **********************************
     C*****************************************************************
     OPFR01     E            PFR01E
**  TABLE UA
-- ADD --
- UPDATE -
