     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API710R - MAINTAIN HISTORY DATA
     H******************************************************************
     H******************************************************************
     FAPI710F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPCHL1    UF A E           K DISK
     FAPVML1    IF   E           K DISK
     FBKSYL1    IF   E           K DISK
     FBKCFL1    IF   E           K DISK
     FAPBKL1    IF   E           K DISK

     D UA              S             10    DIM(3) CTDATA PERRCD(1)
     D SCF             S              1    DIM(24)                              FUNCTION FLAGS
0076 D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
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
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFAC#            3 0
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '4'           X@TYPE            1
     C                   MOVE      ' '           X@APP             1
     C     'BK'          CHAIN     PSY01                              54
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C***                                                              *****
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           X@CODE
     C                   CALL      'APD010R'
     C                   PARM                    SCVDR
     C                   PARM                    X@CODE            1
     C                   PARM      '1'           QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SCR01T
     C*
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCVDR         CHAIN     PVM01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     VMNAME        SCNAME
     C                   MOVE      SCVDR         XXVDR             7 0
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   CALL      'APD710R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    SCVDR
     C                   PARM                    SCSEQ1            5 0
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01 '      Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   WRITE     SCR99
     C*
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFAC#        XXFAC#
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C*
     C                   MOVE      'C'           SMAINT            1
     C                   MOVE      CHCO#         SCCO#
     C                   MOVE      CHFAC#        SCFAC#
     C                   MOVE      CHBTCH        SCBTCH
     C                   MOVE      CHINV#        SCINV#
     C                   MOVE      CHBNK#        SCBNK#
     C                   MOVE      CHCHK#        SCCHK#
     C*
     C                   Z-ADD     CHINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINVD
     C*
     C                   Z-ADD     CHDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUED
     C*
     C                   Z-ADD     CHIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCHKD
     C*
     C                   MOVE      CHACMO        SCACMO
     C                   MOVE      CHACYR        SCACYR
     C                   MOVE      CHPONO        SCPONO
     C                   MOVE      CHPODS        SCPODS
     C                   MOVE      CHAMT         SCAMT
     C                   Z-ADD     CHDISC        SCDISC
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'A'           SMAINT            1
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFAC#        XXFAC#
     C                   MOVE      XXVDR         SCVDR
     C                   MOVE      *BLANKS       SCBTCH
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCBNK#
     C                   MOVE      *BLANKS       SCCHK#
     C                   MOVE      *BLANKS       SCCHKD
     C                   MOVE      *BLANKS       SCINVD
     C                   MOVE      *BLANKS       SCDUED
     C                   MOVE      *BLANKS       SCACMO
     C                   MOVE      *BLANKS       SCACYR
     C                   MOVE      *BLANKS       SCPONO
     C                   MOVE      *BLANKS       SCPODS
     C                   MOVE      *ZEROS        SCAMT
     C                   MOVE      *ZEROS        SCDISC
     C                   MOVE      *ZEROS        SCSEQ1
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02 '      Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  VALID COMPANY/FACILITY
     C*
     C     CFKEY         CHAIN     PCF01                              41
     C     *IN41         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALID BANK
     C*
     C     SCBNK#        CHAIN     PBK01                              42
     C     *IN42         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHK DATE MUST BE A VALID DATE.
     C*
     C     SCCHKD        IFEQ      *ZERO
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCHKD
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  INVOICE DATE MUST BE A VALID DATE.
     C*
     C     SCINVD        IFEQ      *ZERO
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCINVD
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  DUE DATE MUST BE A VALID DATE.
     C*
     C     SCDUED        IFEQ      *ZERO
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUED
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALID ACCOUNTING MONTH
     C*
     C     SCACMO        IFLT      1
     C     SCACMO        ORGT      12
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C*
     C*  GET NEW SEQUENCE NUMBER FOR ADD
     C*
     C     SMAINT        IFEQ      'A'
     C                   MOVE      '1'           *IN40
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFAC#        XXFAC#
     C                   Z-ADD     99999         SCSEQ1
     C     CHKEY         SETLL     PCH01
     C                   READP     PCH01                                  41
     C  N41SCCO#         COMP      CHCO#                              4141
     C  N41SCFAC#        COMP      CHFAC#                             4141
     C  N41SCVDR         COMP      CHVDR                              4141
     C   41              Z-ADD     1             SCSEQ1
     C  N41CHSEQ1        ADD       1             SCSEQ1            5 0
     C                   MOVE      SCSEQ1        CHSEQ1
     C                   ELSE
     C*
     C*  RETRIEVE RECORD FOR CHANGE
     C*
     C     CHKEY         CHAIN     PCH01                              40
     C                   END
     C*
     C                   MOVE      SCCO#         CHCO#
     C                   MOVE      SCFAC#        CHFAC#
     C                   MOVE      SCFAC#        CHFAC2
     C                   MOVE      SCBTCH        CHBTCH
     C                   MOVE      SCVDR         CHVDR
     C                   MOVE      SCINV#        CHINV#
     C                   MOVE      SCBNK#        CHBNK#
     C                   MOVE      SCCHK#        CHCHK#
     C*
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHIDT
     C*
     C                   Z-ADD     SCINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHINVD
     C*
     C                   Z-ADD     SCDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHDUED
     C*
     C                   MOVE      SCACMO        CHACMO
     C                   MOVE      SCACYR        CHACYR
     C                   MOVE      SCPONO        CHPONO
     C                   MOVE      SCPODS        CHPODS
     C                   MOVE      SCAMT         CHAMT
     C                   Z-ADD     SCDISC        CHDISC
     C*
     C                   MOVE      Q1USER        CHCUSR
     C                   MOVE      Q1PGM         CHIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHCDT
     C*
     C                   Z-ADD     DSTIME        CHCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCH01
     C                   ELSE
     C                   MOVE      Q1USER        CHIUSR
     C                   MOVE      *BLANKS       CHCODE
     C                   MOVE      *BLANKS       CHSTAT
     C                   Z-ADD     *ZERO         CHSEQ2
     C                   Z-ADD     *ZERO         CHBUL1
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CHITIM
     C                   WRITE     PCH01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01 '      Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
