     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4303R1 - CLAIM STAFF CODE EMPLOYER DEFINITION
     H* ============================================================= *
     H*  (c) Copyright Fiserv, Inc., 2002 - 2007
     H* ============================================================= *
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ASSIGN SPECIFIC *
     H*   SUPERVISORS AND ADJUSTORS TO AND EMPLOYER IN THE EXCPT      *
     H*   CODE FILE (WMSEP)                                           *
     H* CALLED FROM:  CLAIMS MENU                                     *
     H* CALLS TO:     NONE                                            *
     H* PROGRAMMER:   C:132  PROJECT: 71WEJ                           *
     H* ============================================================= *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H* ============================================================= *
     H* INDICATOR USAGE:                                              *
     H*    *IN60  -  GENERAL USAGE                                    *
     H*    *IN65  -  GENERAL USAGE                                    *
     H*                                                               *
     H*   SCR01:                                                      *
     H*    *IN20  -  INVALID EMPLOYER ENTERED                         *
     H*    *IN21  -  BLANK EMPLOYER NUMBER ENTERED                    *
     H*    *IN23  -  POSITION CURSOR ON 'EMPLOYER'                    *
     H*                                                               *
     H*   SCR02:                                                      *
     H*    *IN21  -  INVALID STAFF CODE ENTERED (SUPERVISOR)          *
     H*    *IN22  -  INVALID STAFF CODE ENTERED (ADJUSTOR)            *
     H*                                                               *
     H* ============================================================= *
     FW4303F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWMSEL     UF A E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXLET                  1      2
     D  XXLT1                  1      1
     D  XXLT2                  2      2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D OPTNM           C                   CONST('Claims Staff Code Em-
     D                                     ployer Definition')

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*
     C*  KEY LIST FOR THE EXCEPTION STAFF CODE FILE
     C*
     C     SEKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
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
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C*
     C*  GET THE USER PARAMETER FILE
     C* ==================== *
     C     TOP           TAG
     C* ==================== *
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPCO#         XXCO#
     C  N60              Z-ADD     UPFND         XXFND
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C  N60              Z-ADD     UPFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  OPTNM         SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C* ==================== *
     C     STRT          TAG
     C* ==================== *
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C* PROCESS ALL COMMAND KEYS
     C* ==================== *
     C     STRT1         TAG
     C* ==================== *
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F10 = TABLE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*       SCREEN 1 SEARCHES
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C*  NOTE: -@@RRN WILL RETURN THE RELETIVE RECORD NUMBER OF WMEMP
     C*        -@@RRN2 WILL RETURN THE RELETIVE RECORD NUMBER OF
     C*         WDELP OR WMALP DEPENDING ON @@CALL (Y/L)
     C*        -@@RRN2 IS USED FOR DISPLAYING A PARTICULAR GROUP
     C*        -@@RRN3 IS USED FOR DISPLAYING A PARTICULAR EMPLOYER
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN23                          SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C                   ENDIF                                                  Q2RECF EQ 'SCR0
     C*
     C*
     C*
     C*       SCREEN 2 SEARCHES
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*                STAFF CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCSUPR'
     C     HFLD          OREQ      'SCCLEX'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C     *INKL         IFEQ      *OFF
     C*
     C     ##RRN         IFGT      *ZEROS
     C     ##RRN         CHAIN     PUP03                              60
     C     *IN60         IFEQ      '0'
     C*
     C     HFLD          IFEQ      'SCSUPR'
     C                   MOVEL     UPSTAF        SCSUPR
     C                   MOVE      *OFF          *IN23
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF                                                  END SCSUPR
     C*
     C     HFLD          IFEQ      'SCCLEX'
     C                   MOVEL     UPSTAF        SCCLEX
     C                   MOVE      *OFF          *IN22
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF                                                  END SCCLEX
     C                   ELSE                                                   *IN60 NE '0'
     C                   MOVE      *ON           *IN21
     C                   ENDIF                                                  END *IN60
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ##RRN > '0'
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKL = 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      'B'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL = 'B'
     C*
     C                   ENDIF                                                  HFLD = SCUPR,SCCLEX
     C*
     C                   ENDIF                                                  Q2RECF EQ 'SCR0
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ EQ '1'
     C*
     C*   F12=PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR02
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  SCR01
     C*
     C                   ENDIF                                                  *INKL EQ '1'
     C*
     C*   F13=CHANGE USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   ENDIF                                                  *INKM EQ '1'
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   ENDIF                                                  *IN10 = '1'
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C* PROCESS REQUEST SCREEN
     C* ==================== *
     C     SCR01T        TAG
     C* ==================== *
     C*
     C* If Screen one, No emp# and not Search, is error
     C     Q2RECF        IFEQ      'SCR01'
     C     *INKJ         ANDEQ     *OFF
     C     SCEMP#        ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN21                          SCEMP# POSITION
     C                   MOVE      *ON           *IN23                          SCEMP# POSITION
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C*   VALIDATE THE STAFF CODE
     C*
     C     EMKY1         CHAIN(N)  PEM02                              20
     C*
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   MOVEL     EMNAME        SCEMNM
     C     SEKY1         CHAIN(N)  PSE01                              65
     C     *IN65         IFEQ      '0'
     C                   MOVEL     SELTSP        SCSUPR
     C                   MOVEL     SELTAD        SCCLEX
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSUPR
     C                   MOVEL     *BLANKS       SCCLEX
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C* PROCESS REQUEST SCREEN
     C* ==================== *
     C     SCR02T        TAG
     C* ==================== *
     C*
     C*  VALIDATE STAFF CODES
     C*
     C     SCSUPR        IFNE      *BLANKS
     C     SCSUPR        CHAIN     PUP02                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C     SCCLEX        IFNE      *BLANKS
     C     SCCLEX        CHAIN     PUP02                              22
     C     *IN22         CABEQ     '1'           STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C* Clear the Exception File Fields before chain to Reset
     C                   CLEAR                   PSE01
     C*
     C     SEKY1         CHAIN     PSE01                              60
     C*
     C                   MOVE      XXCO#         SECO#
     C                   MOVE      XXFND         SEFND
     C                   MOVE      SCEMP#        SEEMP#
     C                   MOVEL     SCSUPR        SELTSP
     C                   MOVEL     SCCLEX        SELTAD
     C*
     C                   MOVE      'A'           SESTAT
     C*
     C                   MOVE      Q1USER        SEUSER
     C                   MOVE      Q1PGM         SEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEDT
     C                   Z-ADD     DSTIME        SETIME
     C*
     C     *IN60         IFEQ      '1'
     C*
     C* If both Supr AND Adjustor codes blank don't write new Record
     C     SCSUPR        IFEQ      *BLANKS
     C     SCCLEX        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   END                                                    SC EQ *BLANK
     C*
     C* If have either Supr or Adjustor code write new Record
     C     SCSUPR        IFNE      *BLANKS
     C     SCCLEX        ORNE      *BLANKS
     C                   WRITE     PSE01
     C                   END                                                    SC NE *BLANK
     C*
     C                   ELSE
     C                   UPDATE    PSE01
     C                   END                                                    *IN60
     C*
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*  =================== *
     C     TERM          TAG
     C* ==================== *
     C                   MOVE      '1'           *INLR
     C* ================================================================ *
